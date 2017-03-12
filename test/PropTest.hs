module PropTest where

prop_Addition_is_commutative :: Int -> Int -> Bool
prop_Addition_is_commutative a b = a + b == b + a
