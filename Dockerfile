# How to use:
#
# docker build -t bore:latest .
# ...

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

# Expose Gopher port
EXPOSE 7070

# Start Bore using watchServe
CMD ["/usr/local/bin/bore", "watchServe", "--source", "/var/gopher/source", "--output", "/var/gopher/output"]
