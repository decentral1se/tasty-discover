# Change Log

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog] and this project adheres to [Semantic
Versioning].

[Keep a Changelog]: http://keepachangelog.com/
[Semantic Versioning]: http://semver.org/).

## 2.0.1 [UNRELEASED]

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
