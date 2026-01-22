# Changelog

I try to only note changes you may actually notice as a
user in the changelog.

## [Unreleased]

### Changed

* Behavior of `jekyll` to hopefully extract paths more reliably

### Added

* `--skip-assets` to `jekyll` so can skip copying assets
* `--assets-dir` so can define source assets directory to check/copy from
  when using the `jekyll` command

## [0.57.0.0] - 2026-01-15

### Fixed

* Fix wrapping: don't wrap gopherlines

## [0.56.0.0] - 2025-09-13

### Changed

* The behavior of deleting the contents of the output directory (except for assets/) before building the new contents is no longer the default. To re-engage this behavior use the `--reset` flag with the `build` command.

## [0.55.0.0] - 2025-08-27

### Removed

* Everything to do with hosting a Gopher server, basically, as I have separated those responsibilities to [Venusia](https://github.com/someodd/venusia)

## [0.53.0.0] - 2025-04-17

### Fixed

* By upgrading a dependency (Venusia), fix README.txt link in directory listings

## [0.52.0.0] - 2025-04-14

### Fixed

* By upgrading a dependency, fix gateway parsing

## [0.51.0.0] - 2025-04-10

### Fixed

* Simply upgraded `Venusia` to fix wildcard matching

## [0.50.0.0] - 2025-04-10

### Added

* Upgraded `Venusia` for gateway support (configurable!)

## [0.49.0.0] - 2025-04-10

This is a dummy release designed to include `example/`. I should phase this out soon.

## [0.48.0.0] - 2025-04-10

### Changed

* Updated `Venusia` dependency, so now I think directory listings should look nicer

## [0.47.0.0] - 2025-04-10

### Added

* Utilizing built-in file browser `Venusia` provides!

### Changed

* Since I'm starting to use `Ryvm` as a dependency, the search has changed a bit. It's not
  optimimal at the moment, because I need to strip gophermap syntax from the results and
  when searching, but it'll get there.

## [0.46.0.0] - 2025-04-07

### Changed

* Start migrating to my `Venusia` dependency for gopher servers!
  This includes adding a `systemd` command.

## [0.45.0.0] - 2025-04-07

## Added

* `--wait` flag for `watchServe` so when you copy files over via `sftp`, `bore` won't
  spawn a rebuild for every single file changed/copied/deleted

## [0.44.0.0] - 2025-04-07

## Fixed

* Wordwrapping blockquotes
* Vertical spacing of gopher menu links and not grouping them into paragraphs

## [0.43.0.0] - 2025-04-02

#### Fixed

* If `draft` is `true` then don't write out! FrontMatter.

## [0.42.0.0] - 2025-03-14

#### Fixed

* No more HTML entity escaping!

## [0.41.0.0] - 2025-03-10

### Changed

* Figlet font rendering now gives each word its own "line"

## [0.40.0.0] - 2025-03-09

### Added

* `--dev-mode` for building/testing locally. this will override the hostname (localhost)
  and user (don't switch)

## [0.39.0.0] - 2025-03-02

### Fixed

* for archiving jekyll (phlog), fix/add actual links to original phlog post

## [0.38.0.0] - 2025-02-24

### Added

* for archiving jekyll (phlog), copy over any referenced assets with special rules

## [0.37.0.0] - 2025-02-24

### Added

* for archiving jekyll (phlog), add `--after`, so only posts
  with a newer date will be archived

## [0.36.0.0] - 2025-02-24

### Added

* for archiving jekyll (phlog), convert gophermap links to markdown links

## [0.35.0.0] - 2025-02-17

### Changed

* for archiving to jekyll (phlog), put "original content" link at bottom of post instead

### Fixed

* URI for the "original content in gopherspace" for porting to jekyll

## [0.34.0.0] - 2025-02-17

### Added

* `draft` rule is now adhered to for skipping phlog posts in certain contexts
* command to archive phlog posts to jekyll format

## [0.29.0.0] - 2024-12-30

### Changed

* Phlog index links to display LESS info so it's more readable

## [0.28.0.0] - 2024-12-30

### Added

* Markdown link footnotes and images are now transformed into the appropriate kind of
  gopher menu item type if the article is being rendered as a menu

## [0.27.0.0] - 2024-12-20

### Added

* Markdown links (for wrapping) are now transformed into footnotes

## [0.26.0.0] - 2024-12-18

### Changed

* Prepping for Markdown/phlog post system (the word wrapping now uses this system, you may
  notice some changes)

## [0.25.0.0] - 2024-12-17

* Kind of less hacky Markdown wordwrapping in templating, less buggy

## [0.24.0.0] - 2024-12-17

### Added

* Hacky markdown wordwrapping in templating

## [0.23.0.0] - 2024-12-13

### Added

* You can now inherit frontmatter from parent templates
* Wordwrapping lambdas for mustache
* `templates/post.txt` is now default for templates!

### Fixed

* Some edge case(s) of runtime error due to unsafe `maximum` when using some things like
  figlet or containerize

## [0.22.0.0] - 2024-11-19

### Fixed

* Selector highlighting in search

## [0.21.0.0] - 2024-11-04

Lots of behind-the-scenes stuff.

### Changed/Fixed

* Search results are more picky and penalize against large distances for the keyword
  proximity score

## [0.20.0.0] - 2024-10-29

### Changed

* New under-the-hood tweaks that I consider significant, like formal verification of part
  of the search algo

## [0.19.0.0] - 2024-10-25

### Changed

* Search algo now "better" ranks proximity and also is stricter about fuzzy matches
  influencing score and some other stuff I forget, could maybe even be considered a
  "fix"

## [0.18.0.0] - 2024-10-23

### Changed

* Search results/file ranking algorithm should hopefully be *way* better now

## [0.17.0.0] - 2024-10-23

### Changed

* (hopefully) enhanced file ranking algorithm for search

## [0.16.0.0] - 2024-10-23

### Fixed

* Fix search result determination of item type

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

[unreleased]: https://github.com/someodd/bore/compare/v0.57.0.0...HEAD
[0.57.0.0]: https://github.com/someodd/bore/compare/v0.56.0.0...v0.57.0.0
[0.56.0.0]: https://github.com/someodd/bore/compare/v0.55.0.2...v0.56.0.0
[0.55.0.0]: https://github.com/someodd/bore/compare/v0.54.0.0...v0.55.0.0
[0.54.0.0]: https://github.com/someodd/bore/compare/v0.53.0.0...v0.54.0.0
[0.53.0.0]: https://github.com/someodd/bore/compare/v0.52.0.0...v0.53.0.0
[0.52.0.0]: https://github.com/someodd/bore/compare/v0.51.0.0...v0.52.0.0
[0.51.0.0]: https://github.com/someodd/bore/compare/v0.50.0.0...v0.51.0.0
[0.50.0.0]: https://github.com/someodd/bore/compare/v0.49.0.0...v0.50.0.0
[0.49.0.0]: https://github.com/someodd/bore/compare/v0.48.0.0...v0.49.0.0
[0.48.0.0]: https://github.com/someodd/bore/compare/v0.47.0.0...v0.48.0.0
[0.47.0.0]: https://github.com/someodd/bore/compare/v0.46.0.0...v0.47.0.0
[0.46.0.0]: https://github.com/someodd/bore/compare/v0.45.0.0...v0.46.0.0
[0.45.0.0]: https://github.com/someodd/bore/compare/v0.44.0.0...v0.45.0.0
[0.44.0.0]: https://github.com/someodd/bore/compare/v0.43.0.0...v0.44.0.0
[0.43.0.0]: https://github.com/someodd/bore/compare/v0.42.0.0...v0.43.0.0
[0.42.0.0]: https://github.com/someodd/bore/compare/v0.41.0.0...v0.42.0.0
[0.41.0.0]: https://github.com/someodd/bore/compare/v0.40.0.0...v0.41.0.0
[0.40.0.0]: https://github.com/someodd/bore/compare/v0.39.0.0...v0.40.0.0
[0.39.0.0]: https://github.com/someodd/bore/compare/v0.38.0.0...v0.39.0.0
[0.38.0.0]: https://github.com/someodd/bore/compare/v0.37.0.0...v0.38.0.0
[0.37.0.0]: https://github.com/someodd/bore/compare/v0.36.0.0...v0.37.0.0
[0.36.0.0]: https://github.com/someodd/bore/compare/v0.35.0.0...v0.36.0.0
[0.35.0.0]: https://github.com/someodd/bore/compare/v0.34.0.0...v0.35.0.0
[0.34.0.0]: https://github.com/someodd/bore/compare/v0.33.0.0...v0.34.0.0
[0.29.0.0]: https://github.com/someodd/bore/compare/v0.28.0.0...v0.29.0.0
[0.28.0.0]: https://github.com/someodd/bore/compare/v0.27.0.0...v0.28.0.0
[0.27.0.0]: https://github.com/someodd/bore/compare/v0.26.0.0...v0.27.0.0
[0.26.0.0]: https://github.com/someodd/bore/compare/v0.25.0.0...v0.26.0.0
[0.25.0.0]: https://github.com/someodd/bore/compare/v0.24.0.0...v0.25.0.0
[0.24.0.0]: https://github.com/someodd/bore/compare/v0.23.0.0...v0.24.0.0
[0.23.0.0]: https://github.com/someodd/bore/compare/v0.22.0.0...v0.23.0.0
[0.22.0.0]: https://github.com/someodd/bore/compare/v0.21.0.0...v0.22.0.0
[0.21.0.0]: https://github.com/someodd/bore/compare/v0.20.0.0...v0.21.0.0
[0.20.0.0]: https://github.com/someodd/bore/compare/v0.19.0.0...v0.20.0.0
[0.19.0.0]: https://github.com/someodd/bore/compare/v0.18.0.0...v0.19.0.0
[0.18.0.0]: https://github.com/someodd/bore/compare/v0.17.0.0...v0.18.0.0
[0.17.0.0]: https://github.com/someodd/bore/compare/v0.16.0.0...v0.17.0.0
[0.16.0.0]: https://github.com/someodd/bore/compare/v0.15.0.0...v0.16.0.0
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
