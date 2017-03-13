-- HLint configuration file

module HLint.HLint where

import "hint" HLint.Default
import "hint" HLint.Builtin.All

-- Because we need underscored names for test discovery
-- this hint isn't much use any more.
ignore "Use camelCase"
