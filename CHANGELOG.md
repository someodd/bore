# Changelog

## [Unreleased]

## [0.5.0.0] - 2024-10-07

### Fixed

* Was attempting to parse directories as files just because they matched the parseable file whitelist

## [0.4.0.0] - 2024-10-07

### Removed/Changed

* `.parseable.gophermap` removed, because `.gophermap`s are now parseable by default

### Fixed

* Old behavior leftover which used to generate `.gophermap` to `.gophermap/.gophermap`

## [0.3.0.0] - 2024-10-07

Gearing up for server daemon.

### Added

* output directory gets wiped before building to output
* special assets directory which doesn't get wiped

### Removed

* No more `root` specification in the `bore.toml`, this is
  simply derived from either `--output` or the default.

## [0.2.0.0] - 2024-10-07

### Added

* CLI options to just build or watch and serve. You can also override the source and
  output directories.

## [0.1.0.0] - 2024-10-06

### Added

* Initial release

[unreleased]: https://github.com/someodd/bore/compare/v0.5.0.0...HEAD
[0.5.0.0]: https://github.com/someodd/bore/compare/v0.4.0.0...v0.5.0.0
[0.4.0.0]: https://github.com/someodd/bore/compare/v0.3.0.1...v0.4.0.0
[0.3.0.0]: https://github.com/someodd/bore/compare/v0.2.0.0...v0.3.0.0
[0.2.0.0]: https://github.com/someodd/bore/compare/v0.1.0.0...v0.2.0.0
[0.1.0.0]: https://github.com/someodd/bore/releases/tag/v0.1.0.0
