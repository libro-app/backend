-- |  Dummy module to test API doc generation and testing
module Answer where

-- |  The constant answer to life, the universe and everything.
answer  :: Num a
        => String
        -- ^  Technically, any type would work here,
        --    but a concrete type is easier to test for QuickCheck
        -> a
answer _ = 42
