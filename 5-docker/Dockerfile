FROM haskell:9-slim as builder
RUN echo "============ This will take a few minutes.... ============"

# install haskell packages and dependencies
RUN apt-get update && apt-get install -y libpcre3 libpcre3-dev pkg-config
RUN cabal update && cabal install --lib --global command MissingH firefly Unixutils

# install node and npm
RUN curl -sL https://deb.nodesource.com/setup_16.x | bash - && apt-get install -y nodejs
RUN npm install -g serve

RUN node --version && npm --version && g++ --version && ghc --version

# install app
WORKDIR /app
# temporary working directory for requests
RUN mkdir tmp bin

# install npm dependencies
RUN mkdir 4-scratchpad
COPY 4-scratchpad/package*.json ./4-scratchpad/
RUN (cd 4-scratchpad && npm ci)

# copy each stage and compile/build it

# compile MiniLISP and haskell
COPY 0-src 0-src
COPY 1-interpreter 1-interpreter
RUN 1-interpreter/compile-cpp.sh
COPY 3-HTTP 3-HTTP
RUN ghc -O3 3-HTTP/HTTP.hs -o bin/httpServer -package wai -package http-types -package mtl

COPY 4-scratchpad 4-scratchpad
# frontend
RUN (cd 4-scratchpad && npm run build)

# for internal testing
COPY Makefile ./
COPY 2-readme-generator ./2-readme-generator



FROM node:16-slim as final
WORKDIR /app
RUN npm install -g serve

COPY --from=builder /app/bin /app/bin
COPY --from=builder /app/1-interpreter /app/1-interpreter
COPY --from=builder /app/3-HTTP /app/3-HTTP
COPY --from=builder /app/4-scratchpad/dist /app/4-scratchpad/dist

# build instructions for internal testing
COPY Makefile ./