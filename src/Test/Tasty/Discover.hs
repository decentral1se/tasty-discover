-- | Automatic test discovery and runner for the tasty framework.

module Test.Tasty.Discover (module X) where

-- 3rd party
import Test.Tasty as X
import Test.Tasty.HUnit as X
import Test.Tasty.QuickCheck as X
import Test.Tasty.TH as X

-- `tasty-discover` modules
import Test.Tasty.Run as X
import Test.Tasty.Parse as X
import Test.Tasty.Type as X
