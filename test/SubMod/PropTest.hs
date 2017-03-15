module SubMod.PropTest where

prop_addition_is_associative :: Int -> Int -> Int -> Bool
prop_addition_is_associative a b c = (a + b) + c == a + (b + c)
