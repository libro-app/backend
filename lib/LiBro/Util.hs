-- |  Helper functions and types that can be useful
--    in more than one place.
module LiBro.Util
  (
  -- * Tree building
    ParentList(..)
  , readForest
  -- * Counting monad transformer
  , CountingT(..)
  , next
  , runCountingT
  -- * XLSX as data backend
  , storeAsXlsx
  , loadFromXlsx
  -- * Other helper functions
  , guarded
  ) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Tuple
import Data.List as L
import qualified Data.Map as M
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as V
import Data.Tree
import Data.Maybe
import Data.Csv
import Data.Bifunctor
import Control.Monad.State
import Control.Applicative
import System.FilePath
import System.Directory
import System.IO.Temp
import System.Process

-- |  A 'Tree'/'Forest' representation as a linear list.
--    All entries point to their parent.
type ParentList a = [(a, Maybe a)]

-- |  Reads a forest from a given 'ParentList', sorting each 'Node's children.
readForest :: Ord a => ParentList a -> Forest a
readForest pairs =
  let (rs, is)  = L.partition (isNothing . snd) pairs
      roots     = fst <$> rs
      inners    = second fromJust <$> is
      children  = M.fromListWith (++) $ L.map (second (:[]) . swap) inners
  in  fill children <$> roots
  where fill cs n = Node n $ case M.lookup n cs of
                              Nothing -> []; Just [] -> []
                              Just xs -> fill cs <$> sort xs

-- |  Simple monad transformer that allows to read an increasing 'Int'.
type CountingT m = StateT Int m

-- |  Grabs the next 'Int'.
next :: Monad m => CountingT m Int
next = do
  val <- get
  modify succ
  return val

-- |  Evaluate the given action with counting from the given initial value.
runCountingT :: Monad m => CountingT m a -> Int -> m a
runCountingT = evalStateT

-- |  Create an 'Alternative' value based on a predicate.
guarded :: Alternative f => (a -> Bool) -> a -> f a
guarded p x = if p x then pure x else empty

--  Internal newtype wrapper to mask special cell values
--  like references ("=foo") by prepending '%'
newtype Wrap a = Wrap {unWrap :: a} deriving Show
instance ToNamedRecord a => ToNamedRecord (Wrap a) where
  toNamedRecord = post . toNamedRecord . unWrap
    where post  = HM.fromList . map (bimap wrap wrap) . HM.toList
          wrap  = TE.encodeUtf8 . T.cons '%' . TE.decodeUtf8Lenient
instance FromNamedRecord a => FromNamedRecord (Wrap a) where
  parseNamedRecord  = fmap Wrap . parseNamedRecord . pre
    where pre       = HM.fromList . map (bimap unwrap unwrap) . HM.toList
          unwrap    = TE.encodeUtf8 . T.tail . TE.decodeUtf8Lenient
instance DefaultOrdered a => DefaultOrdered (Wrap a) where
  headerOrder   = fmap wrap . headerOrder . unWrap
    where wrap  = TE.encodeUtf8 . T.cons '%' . TE.decodeUtf8Lenient

-- |  Store a list of (CSV-transformable) data to a XLSX file.
--    CAVEAT: round-trips don't work with unsafe text values.
--    Use "LiBro.Data.SafeText".
storeAsXlsx :: (DefaultOrdered a, ToNamedRecord a) => FilePath -> [a] -> IO ()
storeAsXlsx fp records = do
  withSystemTempDirectory "xlsx-export" $ \tdir -> do
    let csvFile = tdir </> "export.csv"
    LBS.writeFile csvFile $ encodeDefaultOrderedByName (Wrap <$> records)
    callCommand $ unwords
      [ "libreoffice --calc --convert-to xlsx"
      , "--outdir", tdir, csvFile
      , "> /dev/null"
      ]
    renameFile (csvFile -<.> "xlsx") fp

-- |  Load a list of (CSV-transformable) data from a XLSX file.
--    CAVEAT: round-trips don't work with unsafe text values.
--    Use "LiBro.Data.SafeText".
loadFromXlsx :: FromNamedRecord a => FilePath -> IO (Either String [a])
loadFromXlsx fp = do
  withSystemTempDirectory "xlsx-import" $ \tdir -> do
    let xlsxFile = tdir </> "import.xlsx"
    copyFile fp xlsxFile
    callCommand $ unwords
      [ "libreoffice --calc --convert-to csv"
      , "--outdir", tdir, xlsxFile
      , "> /dev/null"
      ]
    csv <- LBS.readFile (xlsxFile -<.> "csv")
    return $ map unWrap . V.toList . snd <$> decodeByName csv
