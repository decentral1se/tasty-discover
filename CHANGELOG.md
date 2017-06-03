# Change Log

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog] and this project adheres to
[Semantic Versioning].

[Keep a Changelog]: http://keepachangelog.com/
[Semantic Versioning]: http://semver.org/

# 3.0.0 [UNRELEASED]

### Added
- [#103]: Added `--tree-display` configuration option.

### Changed
- [#97]: `case_` is deprecated in favour of `unit_` for HUnit test cases.

### Fixed
- [#102]: `--no-module-suffix` incorrectly handling directories.

[#97]: https://github.com/lwm/tasty-discover/pull/97
[#102]: https://github.com/lwm/tasty-discover/pull/102
[#103]: https://github.com/lwm/tasty-discover/pull/103

# 2.0.3 [2017-04-13]

### Fixed
- Make the Cabal description more clear for Hackage.

# 2.0.2 [2017-04-13]

### Added
- Change log is in `extra-source-files` now.
- [#96]: README is in `extra-source-files` now.

### Fixed
- [#88]: stylish-haskell automated checking.

[#88]: https://github.com/lwm/tasty-discover/pull/88
[#96]: https://github.com/lwm/tasty-discover/pull/96

## 2.0.1 [2017-03-18]

### Fixed
- [#86]: Flaky test comparison.

[#86]: https://github.com/lwm/tasty-discover/pull/86

### Removed
- [#83]: The `Test.Tasty.Type` module.

[#83]: https://github.com/lwm/tasty-discover/pull/83

## 2.0.0 [2017-03-15]

### Added
- Use hpack format.
- Use generator style test discovery from tasty-auto.
- New configuration options: debug, ingredients and module name.
- Unit tests for all functionality.

### Fixed
- Re-license to MIT.

### Removed
- RTD documentation.
- TemplateHaskell dependency
- Example project and integration test project.

### Changed
- Move all tests into test folder.

## 1.1.0 [2017-01-19]

### Added
- `--ignore-module` configuration option.

## 1.0.1 [2017-11-13]

### Added
- Cabal testing on Travis CI.
- Documentation testing on Travis CI.

### Fixed
- Include missing `extra-source-files`.
- Slim down LICENSE.md and mark as GPL-3 in Cabal file.

## 1.0.0 [2016-11-04]

### Added
- Documentation to RTD.
- Release on Hackage and Stackage.

## 0.0.3 [2016-09-20]

### Added
- `--no-module-suffix` configuration option.

## 0.0.2 [2016-02-20]

### Added
- `--module-suffix` configuration option.

## 0.0.1 [2016-02-13]
- tasty-discover initial release.
