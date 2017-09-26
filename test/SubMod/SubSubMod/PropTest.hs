module SubMod.SubSubMod.PropTest where

prop_additionCommutative :: Int -> Int -> Bool
prop_additionCommutative a b = a + b == b + a
