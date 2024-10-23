# Changelog

## [Unreleased]

## [0.15.0.0] - 2024-10-22

Phlogging!

### Changed

* Phlog post order on menu indexes is now descending (older as you go on)
* Phlog post entries on menu indexes reformatted

## [0.14.0.0] - 2024-10-22

### Fixed

* Search result paths

## [0.13.0.0] - 2024-10-22

### Fixed

* Search functionality now uses the correct paths for source and output directories

## [0.12.0.0] - 2024-10-22

### Added

* Search functionality: search on /search with the watchServe daemon

## [0.11.0.0] - 2024-10-11

### Fixed

* You can now reference 'tags' frontmatter in template (as a list)

## [0.10.0.0] - 2024-10-09

### Fixed

* Tag link errors for indexes. I'm being lazy and not testing.

## [0.9.0.0] - 2024-10-09

### Fixed

* Minor phlog index link errors

## [0.8.0.0] - 2024-10-09

### Fixed

* Fixed phlog index links

## [0.7.0.0] - 2024-10-09

### Added

* Buffed-up the indexes so they actually link to the tag indexes, atom feed, and main index.

### Changed

* You can no longer mark a post as a phlog post with FrontMatter. Now you must place it in
  the phlog/ directory. All indexes and phlog posts are also output to the phlog/ directory.
  Tag indexes will go in phlog/tags.

## [0.6.0.0] - 2024-10-07

### Fixed

* Fix creating directories that have the name of the file name it's copying for every file
  to simply be copied, not parsed (and fix up the directory creation logic in general for
  some related things)

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

[unreleased]: https://github.com/someodd/bore/compare/v0.15.0.0...HEAD
[0.15.0.0]: https://github.com/someodd/bore/compare/v0.14.0.0...v0.15.0.0
[0.14.0.0]: https://github.com/someodd/bore/compare/v0.13.0.0...v0.14.0.0
[0.13.0.0]: https://github.com/someodd/bore/compare/v0.12.0.0...v0.13.0.0
[0.12.0.0]: https://github.com/someodd/bore/compare/v0.11.0.0...v0.12.0.0
[0.11.0.0]: https://github.com/someodd/bore/compare/v0.10.0.0...v0.11.0.0
[0.10.0.0]: https://github.com/someodd/bore/compare/v0.9.0.0...v0.10.0.0
[0.9.0.0]: https://github.com/someodd/bore/compare/v0.8.0.0...v0.9.0.0
[0.8.0.0]: https://github.com/someodd/bore/compare/v0.7.0.0...v0.8.0.0
[0.7.0.0]: https://github.com/someodd/bore/compare/v0.6.0.0...v0.7.0.0
[0.6.0.0]: https://github.com/someodd/bore/compare/v0.5.0.0...v0.6.0.0
[0.5.0.0]: https://github.com/someodd/bore/compare/v0.4.0.0...v0.5.0.0
[0.4.0.0]: https://github.com/someodd/bore/compare/v0.3.0.1...v0.4.0.0
[0.3.0.0]: https://github.com/someodd/bore/compare/v0.2.0.0...v0.3.0.0
[0.2.0.0]: https://github.com/someodd/bore/compare/v0.1.0.0...v0.2.0.0
[0.1.0.0]: https://github.com/someodd/bore/releases/tag/v0.1.0.0
