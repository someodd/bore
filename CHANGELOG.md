# Changelog

## [Unreleased]

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

[unreleased]: https://github.com/someodd/bore/compare/v0.3.0.0...HEAD
[0.3.0.0]: https://github.com/someodd/bore/compare/v0.2.0.0...v0.3.0.0
[0.2.0.0]: https://github.com/someodd/bore/compare/v0.1.0.0...v0.2.0.0
[0.1.0.0]: https://github.com/someodd/bore/releases/tag/v0.1.0.0
