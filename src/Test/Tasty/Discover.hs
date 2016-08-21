-- | Automatic test discovery and runner for the tasty framework.

module Test.Tasty.Discover (
  -- `tasty` core modules
  module Test.Tasty

  -- 3rd party modules
, module Test.Tasty.HUnit
, module Test.Tasty.QuickCheck
, module Test.Tasty.TH

  -- `tasty-discover` modules
, module Test.Tasty.Run
, module Test.Tasty.Parse
, module Test.Tasty.Type
) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.TH

import Test.Tasty.Run
import Test.Tasty.Parse
import Test.Tasty.Type
