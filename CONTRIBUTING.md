# Contributing

## Releases

Releases are managed through GitHub Actions. Please see `.github/workflows/release.yml`.

It works like this:

1. Make sure the `CHANGELOG.md` is up-to-date (don't worry about setting the version or footnote)
1. Use `./reposcripts/prep-release.sh` which will tag with the next release, it'll suggest the release number, but you can review first.

  * double-check the `CHANGELOG.md`...

1. check the github action for release and then check the subsequent release cut and the packages attached

## Tests

Use doctests for simple unit tests (you can even use doctests for property tests), but use
QuickCheck tests in `test/` to test a module's public functions/exports.

The purpose of the actual tests in `tests/` is not to test the minutia of how a goal is
accomplished (all the private functions and such behind a public interface/function), but
instead to test that the public interface actually accomplishes what is desirable.  For a
good example of this, see the test(s) for the search functionality.