FROM ubuntu:22.04

# Set non-interactive frontend for apt
ENV DEBIAN_FRONTEND=noninteractive

# Install system dependencies
RUN apt-get update && \
    apt-get install -y \
    curl \
    git \
    cmake \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

# Create non-root user
RUN useradd -m -s /bin/bash leanuser

# Switch to non-root user
USER leanuser
WORKDIR /home/leanuser

# Install elan (Lean version manager)
RUN curl https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh -sSf | sh -s -- -y --default-toolchain none

# Add elan to PATH
ENV PATH="/home/leanuser/.elan/bin:${PATH}"

# Copy project files
COPY --chown=leanuser:leanuser . /home/leanuser/app
WORKDIR /home/leanuser/app

# Build the project
RUN lake build

CMD ["./.lake/build/bin/bowtie"]
