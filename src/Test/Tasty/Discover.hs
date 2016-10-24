-- | Automatic test discovery and runner for the tasty framework.

module Test.Tasty.Discover (module Discover) where

-- 3rd party
import Test.Tasty as Discover
import Test.Tasty.HUnit as Discover
import Test.Tasty.QuickCheck as Discover
import Test.Tasty.TH as Discover

-- `tasty-discover` modules
import Test.Tasty.Run as Discover
import Test.Tasty.Parse as Discover
import Test.Tasty.Type as Discover
