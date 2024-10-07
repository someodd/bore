#!/bin/bash
# Create the `bore` user without a home directory
if ! id -u bore &>/dev/null; then
    useradd --system --no-create-home --shell /usr/sbin/nologin bore
fi

# Set ownership and permissions for the gopher directories
chown -R bore:bore /var/gopher/output /var/gopher/source
chmod -R 755 /var/gopher/output /var/gopher/source