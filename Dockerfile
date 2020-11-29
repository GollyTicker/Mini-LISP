FROM gcc:10.2
# start with gcc
RUN echo "============ This will take a few minutes.... ============"

# install haskell
RUN apt-get update && apt-get install -y haskell-platform
RUN cabal update && cabal install command MissingH firefly Unixutils

RUN g++ --version
RUN ghc --version

# install app
RUN mkdir /app
WORKDIR /app
# temporary working directory for requests
RUN mkdir tmp

# copy files
COPY bin bin
COPY 0-src 0-src
COPY 1-interpreter 1-interpreter
COPY 2-readme-generator 2-readme-generator
COPY 3-HTTP 3-HTTP
COPY Makefile ./
COPY [1-9]* ./

# compile MiniLISP and haskell
RUN 1-interpreter/compile-cpp.sh

# REPL as entrypoint
CMD bin/MiniLISP
