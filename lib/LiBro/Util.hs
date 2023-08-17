-- |  Helper functions and types that can be useful
--    in more than one place.
module LiBro.Util where

import Data.Tuple
import Data.List as L
import qualified Data.Map as M
import Data.Tree
import Data.Maybe
import Data.Bifunctor
import Control.Monad.State
import Control.Applicative

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
