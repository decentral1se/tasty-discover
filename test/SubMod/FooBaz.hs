module SubMod.FooBaz where

prop_additionCommutative :: Int -> Int -> Bool
prop_additionCommutative a b = a + b == b + a

prop_multiplationDistributiveOverAddition :: Integer -> Integer -> Integer -> Bool
prop_multiplationDistributiveOverAddition a b c = a * (b + c) == a * b + a * c
