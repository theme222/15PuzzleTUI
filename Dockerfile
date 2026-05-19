# Dockerfile for building a truly static binary (I'm sorry what the actual fuck)
 
FROM alpine:3.19 AS builder

# Install dependencies required for static linking
RUN apk update && apk add --no-cache \
    curl gcc g++ git gmp-dev libc-dev libffi-dev make \
    musl-dev ncurses-dev ncurses-static perl tar xz \
    zlib-dev zlib-static \
    bash linux-headers

ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

ENV PATH="/root/.ghcup/bin:${PATH}"
    
WORKDIR /app
# This layer caches the dependencies so they don't need to be rebuilt every time
COPY *.cabal ./
RUN cabal update && \
    cabal build --enable-executable-static \
                --ghc-options="-optl-static -optl-pthread -fPIC" \
                --only-dependencies
                
# build the actual executable statically
COPY . .
RUN cabal build --enable-executable-static \
                --ghc-options="-optl-static -optl-pthread -fPIC"
RUN cp `cabal list-bin 15p` .