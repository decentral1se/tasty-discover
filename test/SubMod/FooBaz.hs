module FooBaz where

prop_addition_is_commutative :: Int -> Int -> Bool
prop_addition_is_commutative a b = a + b == b + a
