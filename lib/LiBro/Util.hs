-- |  Helper functions and types that can be useful
--    in more than one place.
module LiBro.Util where

import Data.Tuple
import Data.List
import qualified Data.Map as M
import Data.Tree
import Data.Maybe
import Data.Bifunctor

-- |  A 'Tree'/'Forest' representation as a linear list.
--    All entries point to their parent.
type ParentList a = [(a, Maybe a)]

-- |  Reads a forest from a given 'ParentList', sorting each 'Node's children.
readForest :: Ord a => ParentList a -> Forest a
readForest pairs =
  let (rs, is)  = partition (isNothing . snd) pairs
      roots     = fst <$> rs
      inners    = second fromJust <$> is
      children  = M.fromListWith (++) $ map (second (:[]) . swap) inners
  in  fill children <$> roots
  where fill cs n = Node n $ case M.lookup n cs of
                              Nothing -> []; Just [] -> []
                              Just xs -> fill cs <$> sort xs
