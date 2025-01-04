# I made this to make hosting a bunch of user's gopherholes easier and more secure.
#
# How to use:
#
# docker build -t bore:latest .
#
# Run containers with something like:
#
# docker run -d --name user1_bore \
#   --hostname user1.bore \
#   --network gopher_net \
#   bore:latest
#
# Be sure to set up the Docker network and host system properly for SFTP and Gopher routing.
# This includes configuring `inetd` for dynamic selector mapping and using shared group
# permissions for data directories.
#
# Host Setup:
#
# 1. **Create Docker Network**:
#    docker network create gopher_net
#
# 2. **Create a User and Data Directory**:
#    sudo useradd -m -d /sftp/$username -s /usr/sbin/nologin $username
#    sudo passwd $username
#    sudo mkdir -p /sftp/$username/data
#
# 3. **Assign Shared Group Permissions**:
#    sudo groupadd guestholes
#    sudo usermod -aG guestholes $username
#    sudo chown root:root /sftp/$username
#    sudo chmod 755 /sftp/$username
#    sudo chown $username:guestholes /sftp/$username/data
#    sudo chmod 770 /sftp/$username/data
#
# 4. **Add Shared Group to Docker**:
#    Add the `guestholes` group inside the Docker container for the `bore` user.
#
# Gopher Routing Setup:
#
# 1. **Host Mapping for Docker Containers**:
#    Assign each container a hostname:
#      docker run -d --name user1_bore --hostname user1.bore --network gopher_net bore:latest
#
#    Add host mappings (or configure internal DNS):
#      echo "172.18.0.2 user1.bore" | sudo tee -a /etc/hosts
#      echo "172.18.0.3 user2.bore" | sudo tee -a /etc/hosts
#
# 2. **inetd Configuration**:
#    Install `inetd` if not already installed:
#      sudo apt update && sudo apt install -y openbsd-inetd
#
#    Create a router script `/usr/local/bin/gopher-router`:
#      #!/bin/bash
#      username=$(echo $1 | awk -F'/' '{print $2}' | sed 's/^~//')
#      selector=$(echo $1 | sed "s|^/~/[^/]*||")
#      exec docker exec -i ${username}_bore bore serve "${selector:-/}"
#
#      chmod +x /usr/local/bin/gopher-router
#
#    Add the following to `/etc/inetd.conf`:
#      70 stream tcp nowait nobody /usr/local/bin/gopher-router gopher-router
#
#    Restart `inetd`:
#      sudo systemctl restart inetd
#
# Quotas:
#
# - Enable and configure disk quotas for `/sftp`:
#    sudo apt install quota
#    sudo mount -o remount,usrquota /sftp
#    sudo quotacheck -cug /sftp
#    sudo quotaon /sftp
#
# - Set quotas for users:
#    sudo setquota -u $username 100M 120M 0 0 /sftp
#
# Backups/Restoring:
#
# - Backup all user data:
#    tar -czf /backups/sftp_data_$(date +%F).tar.gz /sftp
#
# - Backup individual user data:
#    tar -czf /backups/user1_data_$(date +%F).tar.gz /sftp/user1
#
# Restore by untarring to /sftp and reapplying permissions if needed.





# Define the pinned version as a build argument
ARG BORE_VERSION=0.33.0.0

# Use Debian Slim as the base image
FROM debian:bullseye-slim

# Install dependencies, convert to merged-/usr, and upgrade libc6
RUN apt-get update && apt-get install -y \
    curl \
    git \
    ca-certificates \
    libgmp-dev \
    apt-transport-https \
    && apt-get clean

# Clone the Bore repository to retrieve the example directory
RUN git clone --depth=1 https://github.com/someodd/bore.git /tmp/bore \
    && mkdir -p /var/gopher/source \
    && mv /tmp/bore/example/* /var/gopher/source/ \
    && rm -rf /tmp/bore

# Install Bore using the specified version
RUN curl -L -o bore.deb https://github.com/someodd/bore/releases/download/v0.33.0.0/bore_0.33.0.0_amd64_Ubuntu_kernel6.5.0-1025-azure_libc2.35.deb \
    && dpkg -i bore.deb \
    && rm bore.deb

# Configure Bore
RUN echo 'user = "bore"' >> /var/gopher/source/bore.toml

# Set permissions
RUN chown -R bore:bore /var/gopher/

# Add guestholes group and ensure bore user is part of it
RUN groupadd -g 1001 guestholes && \
    usermod -aG guestholes bore

# Set permissions for Bore directory
RUN mkdir -p /var/gopher && \
    chown -R bore:guestholes /var/gopher && \
    chmod -R 770 /var/gopher

# Expose Gopher port
EXPOSE 7070

# Start Bore using watchServe
CMD ["/usr/local/bin/bore", "watchServe", "--source", "/var/gopher/source", "--output", "/var/gopher/output"]
