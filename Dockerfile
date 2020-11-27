FROM gcc:10.2
# start with gcc
RUN echo "============ This will take a few minutes.... ============"

# install haskell
RUN apt-get update && apt-get install -y haskell-platform
RUN cabal update && cabal install command MissingH firefly

RUN g++ --version
RUN ghc --version

# install app
RUN mkdir /app
WORKDIR /app

# copy files
COPY [0-9]* ./

# compile MiniLISP and haskell
RUN ./2-compile-cpp.sh

# TODO: compile HTTP rest endpoint

# REPL as entrypoint
CMD ./MiniLISP
