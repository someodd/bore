#!/bin/bash

# Prepare a commit/tag for release

# Stop the script if a command fails.
set -e

# Suggest a version.
#
# Get the current version from package.yaml and then suggest a new version which simply
# bumps the minor version.
CURRENT_VERSION=$(awk '/^version:/ {print $2}' package.yaml)
SUGGESTED_VERSION=$(echo $CURRENT_VERSION | awk -F. '{print $1"."$2+1".0.0"}')
# Ask the user for the version, defaulting to the suggestion.
read -p "Version [was: $CURRENT_VERSION -> suggested: $SUGGESTED_VERSION]: " VERSION

# If the user didn't enter a version, use the suggested version.
if [ -z "$VERSION" ]; then
    VERSION=$SUGGESTED_VERSION
fi

# Update the version in package.yaml
sed -i -E "s/^version: [0-9]+(\.[0-9]+)*$/version: $VERSION/" package.yaml

# Update the version in bore.cabal
sed -i -E "s/^version:[ \t]+[0-9]+(\.[0-9]+)*$/version: $VERSION/" bore.cabal

# Update the `CHANGELOG.md``
./reposcripts/update-changelog.sh "$VERSION"

echo "Please look over the changes, prepare for the release, then make the commit, and finally tag the release."

# Suggest the commit command
echo "git commit -m \"Prepare for release v$VERSION\""

# Suggest the tag command
echo "git tag -a v$VERSION -m \"release v$VERSION\""