# Contributing

## LiquidHaskell

`stack install ghcid`

`ghcid` (wait), should default to stack.

## Tests

Use doctests for simple unit tests (you can even use doctests for property tests), but use
QuickCheck tests in `test/` to test a module's public functions/exports.

The purpose of the actual tests in `tests/` is not to test the minutia of how a goal is
accomplished (all the private functions and such behind a public interface/function), but
instead to test that the public interface actually accomplishes what is desirable.  For a
good example of this, see the test(s) for the search functionality.

## Releases & CI

Releases are managed through GitHub Actions. Please see `.github/workflows/release.yml`.

It works like this:

1. Make sure the `CHANGELOG.md` is up-to-date (don't worry about setting the version or footnote)
1. Use `./reposcripts/release.sh` which will tag with the next release, it'll suggest the release number, but you can review first.

  * double-check the `CHANGELOG.md`...

1. check the github action for release and then check the subsequent release cut and the packages attached

## GitHub Actions

This project is using GitHub Actions. You can test this locally with a tool like Act. Something I like to do to test the release build GitHub will make before actually pushing...

Prereqs: Docker, act.

One-time:
    sudo docker pull catthehacker/ubuntu:full-24.04

Build (no publish):

```
# choose a host dir for the output
sudo mkdir -p /tmp/bore-act-out
sudo chmod 1777 /tmp/bore-act-out

# event payload once
printf '{"inputs":{"version":"1.0.0-test","skip_publish":"true"}}' > /tmp/act_event_release.json

# run it
sudo act workflow_dispatch -j release \ 
  -C /home/tilde/Projects/bore \
  --pull=false \
  -P ubuntu-latest=catthehacker/ubuntu:full-24.04 \
  --container-options 'stack -v /tmp/bore-act-out:/var/tmp/deb-pkg' \
  -e /tmp/act_event_release.json
```


Result will be like `/tmp/bore-act-out/*.deb`.

Install:
    sudo dpkg -i /tmp/bore-act-out/*.deb || sudo apt -f install
