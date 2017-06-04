# Change Log

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog] and this project adheres to
[Semantic Versioning].

[Keep a Changelog]: http://keepachangelog.com/
[Semantic Versioning]: http://semver.org/

# 3.0.1 [2017-06-04]

### Fixed
- Fixed CHANGELOG.md rendering for Hackage (see pull request [#106]).

### Added
- Add missing --tree-display documentation note (see pull request [#107]).

[#107]: https://github.com/lwm/tasty-discover/pull/107
[#106]: https://github.com/lwm/tasty-discover/pull/106

# 3.0.0 [2017-06-03]

### Added
- Add --tree-display configuration option (see pull request [#103]).

### Changed
- Deprecate `case_` in favour of `unit_` for HUnit test cases (see pull request [#97]).

### Fixed
- Correctly handle sub-directories when using --no-module-suffix (see pull request [#102]).

[#97]: https://github.com/lwm/tasty-discover/pull/97
[#102]: https://github.com/lwm/tasty-discover/pull/102
[#103]: https://github.com/lwm/tasty-discover/pull/103

# 2.0.3 [2017-04-13]

### Fixed
- Make the Cabal description more clear for Hackage.

# 2.0.2 [2017-04-13]

### Added
- README.md and CHANGELOG.md included for Hackage (see pull request [#96]).
- Re-add stylish-haskell automated checking (see pull request [#88]).

[#88]: https://github.com/lwm/tasty-discover/pull/88
[#96]: https://github.com/lwm/tasty-discover/pull/96

## 2.0.1 [2017-03-18]

### Fixed
- Fix flaky test comparison (see pull request [#86]).

[#86]: https://github.com/lwm/tasty-discover/pull/86

### Removed
- Remove the Test.Tasty.Type module (see pull request [#83]).

[#83]: https://github.com/lwm/tasty-discover/pull/83

## 2.0.0 [2017-03-15]

### Added
- Add new hpack format.
- Add generator style test discovery from tasty-auto.
- Add new configuration options: debug, ingredients and module name.
- Add unit tests for all functionality.

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
- Add --ignore-module configuration option.

## 1.0.1 [2017-11-13]

### Added
- Add Cabal and Documentation testing on Travis CI.

### Fixed
- Include missing extra-source-files.
- Slim down LICENSE.md and mark as GPL-3 in Cabal file.

## 1.0.0 [2016-11-04]

### Added
- Add documentation on RTD.
- Release on Hackage and Stackage.

## 0.0.3 [2016-09-20]

### Added
- --no-module-suffix configuration option.

## 0.0.2 [2016-02-20]

### Added
- --module-suffix configuration option.

## 0.0.1 [2016-02-13]

- tasty-discover initial release.
