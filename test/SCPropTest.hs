module SCPropTest where

import Data.List (sort)

scprop_sortReverse :: [Int] -> Bool
scprop_sortReverse list = sort list == sort (reverse list)
