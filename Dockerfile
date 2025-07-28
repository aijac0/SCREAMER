# -------------------------
# Stage 1: Builder
# -------------------------
FROM ubuntu:24.04 AS builder

# Install build tools
RUN apt-get update && apt-get install -y build-essential gfortran make \
    && rm -rf /var/lib/apt/lists/*

# Set the working directory
WORKDIR /app

# Copy source code into builder image
COPY . /app/

# Compile the code (debug target)
RUN make screamer64_debug

# -------------------------
# Stage 2: Runtime
# -------------------------
FROM ubuntu:24.04

# Install only runtime libraries
RUN apt-get update && apt-get install -y libgfortran5 \
    && rm -rf /var/lib/apt/lists/*
    
# Set the working directory
WORKDIR /app

# Copy the compiled binary and input decks
COPY --from=builder /app/screamer64_debug .
COPY run_decks/ /app/run_decks/

# Command to run the app
ENTRYPOINT ["/bin/bash"]