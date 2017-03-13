module SubMod.PropTest where

prop_Addition_is_associative :: Int -> Int -> Int -> Bool
prop_Addition_is_associative a b c = (a + b) + c == a + (b + c)
