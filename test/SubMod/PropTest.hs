module SubMod.PropTest where

prop_additionAssociative :: Int -> Int -> Int -> Bool
prop_additionAssociative a b c = (a + b) + c == a + (b + c)
