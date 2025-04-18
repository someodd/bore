#!/bin/bash

# As of right now simply creates the Debian package. Plans for the future include
# creating packages for other distributions and maybe BSD as well.
#
# `fpm` (packaging made simple) needs to be installed.

# Stop the script if a command fails.
set -e

# Information about the system the package is built on.
KERNEL="$(uname -r)"
LIBC="$(ldd --version | awk '/ldd/{print $NF}')"
DISTRO="$(lsb_release -si)"
# Extract the version from package.yaml
VERSION=$(awk '/^version:/ {print $2}' package.yaml)

OUTPUT_PATH="bore_${VERSION}_amd64_${DISTRO}_kernel${KERNEL}_libc${LIBC}.deb"

# Set the temporary package directory variable
TEMPORARY_PKG_DIR=package

# Build the Haskell project and copy the binaries
stack build --copy-bins --local-bin-path ./bin

# Create necessary directories in the temporary package directory
mkdir -p $TEMPORARY_PKG_DIR/usr/local/bin
mkdir -p $TEMPORARY_PKG_DIR/etc/systemd/system
mkdir -p $TEMPORARY_PKG_DIR/var/gopher/output

# Copy the built binary to the temporary package directory
cp ./bin/bore $TEMPORARY_PKG_DIR/usr/local/bin/bore
# Copy the example gopherhole to the temporary package directory
cp -r ./example $TEMPORARY_PKG_DIR/var/gopher/source
# Copy this/overwrite for the daemon
cp ./reposcripts/bore.toml $TEMPORARY_PKG_DIR/var/gopher/source/bore.toml

# Run fpm to create the Debian package.
fpm -s dir -t deb -n bore -v ${VERSION} \
    --description "Bore gopherhole builder. Built on ${DISTRO}, Kernel ${KERNEL}, libc ${LIBC}" \
    --depends "libc6" \
    --maintainer "someodd <someodd@pm.me>" \
    --url "http://www.someodd.zip/showcase/bore" \
    --license "GPL" \
    --after-install ./reposcripts/post-install.sh \
    -p "${OUTPUT_PATH}" \
    -C $TEMPORARY_PKG_DIR \
    usr/local/bin/bore \
    var/gopher/output \
    var/gopher/source

# Clean up the temporary package directory
rm -rf $TEMPORARY_PKG_DIR
rm -rf ./bin

echo "${OUTPUT_PATH}"