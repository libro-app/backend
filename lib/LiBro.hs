-- |  liBro App Backend
module LiBro where

-- |  The constant answer to life, the universe and everything.
answer  :: Num a
        => String
        -- ^  Technically, any type would work here,
        --    but a concrete type is easier to test for QuickCheck
        -> a
answer _ = 42
