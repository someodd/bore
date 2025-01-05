# How to use (although should probably give it the envvars), also not using volumes yet:
#
# docker build -t bore:latest .
#
# Host Setup for SFTP Forwarding:
#
# 1. **Create Docker Network**:
#    docker network create gopher_net
#
# 2. **Add Host Forwarding Rules**:
#    Set up SSH to forward connections to the correct container based on the username.
#
# Edit `/etc/ssh/sshd_config` to forward SFTP connections for usernames starting with `boreguest_`:
#
#    Match User boreguest_*
#        ForceCommand ssh -q -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null \
#            -p 22 %u@%u
#
#    Restart SSH:
#      sudo systemctl restart ssh
#
# 3. **Run the Containers**:
# docker run -d --name boreguest_user1 \
#   --hostname boreguest_user1 \
#   --network gopher_net \
#   -e SFTP_USERNAME=boreguest_user1 \
#   -e SFTP_PASSWORD=password \
#   bore:latest
#
# 4. Finally make the container hostname resolvable:
#
# CONTAINER="boreguest_user1"
# IP=$(sudo docker inspect -f '{{range.NetworkSettings.Networks}}{{.IPAddress}}{{end}}' "$CONTAINER")
# sudo sed -i "/[[:space:]]$CONTAINER$/d" /etc/hosts && \
# echo "$IP $CONTAINER" | sudo tee -a /etc/hosts
#
# Gopher Routing:
#
# You should actually use Dockerfile_inetd for this!
#
# Quotas:
#
# You'll need to use volumes.
#
# - Enable and configure disk quotas for `/var/gopher/guests`:
#    sudo apt install quota
#    sudo mount -o remount,usrquota /var/gopher/guests
#    sudo quotacheck -cug /var/gopher/guests
#    sudo quotaon /var/gopher/guests
#
# - Set quotas for users:
#    sudo setquota -u user1 100M 120M 0 0 /var/gopher/guests
#
# Backups:
#
# - Backup all user data:
#    tar -czf /backups/gopher_guests_data_$(date +%F).tar.gz /var/gopher/guests
#
# - Backup individual user data:
#    tar -czf /backups/user1_data_$(date +%F).tar.gz /var/gopher/guests/user1


# Use Debian Slim as the base image
FROM debian:bookworm-slim

# Define the pinned version
ENV BORE_VERSION=0.33.0.0

#RUN mkdir -p /var/gopher/source /var/gopher/output

# if i don't do this hgetfilecontents or whatever will fail
# Ensure locale is set to UTF-8
RUN apt-get update && apt-get install -y locales
RUN echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen
RUN locale-gen
ENV LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8

# Install dependencies
RUN apt-get update && apt-get install -y \
    curl \
    git \
    openssh-server \
    ca-certificates \
    libgmp-dev \
    apt-transport-https \
    && apt-get clean

# Clone the Bore repository to retrieve the example directory
RUN git clone --depth=1 https://github.com/someodd/bore.git /tmp/bore \
    && mkdir -p /var/gopher/source \
    && mv /tmp/bore/example/* /var/gopher/source/ \
    && rm -rf /tmp/bore

# FIXME
# Install Bore using the specified version
RUN curl -L -o bore.deb https://github.com/someodd/bore/releases/download/v${BORE_VERSION}/bore_${BORE_VERSION}_amd64_Ubuntu_kernel6.5.0-1025-azure_libc2.35.deb \
    && dpkg -i bore.deb \
    && rm bore.deb

# Configure Bore
#RUN echo 'user = "bore"' >> /var/gopher/source/bore.toml

# Set permissions
RUN chown -R bore:bore /var/gopher/

# Add guestholes group and ensure bore user is part of it
RUN groupadd -g 1001 guestholes && \
    usermod -aG guestholes bore

# Install and configure SSH
# Install and configure SSH
RUN mkdir /var/run/sshd && \
    echo 'PermitRootLogin no' >> /etc/ssh/sshd_config && \
    echo 'PasswordAuthentication yes' >> /etc/ssh/sshd_config && \
    echo 'Match User boreguest_*' >> /etc/ssh/sshd_config && \
    echo '    ChrootDirectory /var/gopher' >> /etc/ssh/sshd_config && \
    echo '    ForceCommand internal-sftp' >> /etc/ssh/sshd_config && \
    echo '    AllowTcpForwarding no' >> /etc/ssh/sshd_config

# Accept SFTP username and password as environment variables
ENV SFTP_USERNAME=boreguest_user1 SFTP_PASSWORD=password

# Add SFTP user dynamically at container runtime
RUN useradd -m -d /var/gopher -s /usr/sbin/nologin $SFTP_USERNAME && \
    echo "$SFTP_USERNAME:$SFTP_PASSWORD" | chpasswd && \
    chown -R $SFTP_USERNAME:guestholes /var/gopher

# Expose ports for SSH and Gopher
EXPOSE 22 7071

# Start SSHD and Bore simultaneously
CMD service ssh start && /usr/local/bin/bore watchServe --source /var/gopher/source --output /var/gopher/output
