FROM gcc:10.2
# start with gcc
RUN echo "============ This will take a few minutes.... ============"

# install haskell
RUN apt-get update && apt-get install -y haskell-platform
RUN cabal update && cabal install command MissingH firefly Unixutils

# install node and npm
RUN curl -sL https://deb.nodesource.com/setup_12.x | bash - && apt-get install -y nodejs
RUN npm install -g serve

RUN nodejs --version
RUN npm --version
RUN g++ --version
RUN ghc --version

# install app
RUN mkdir /app
WORKDIR /app
# temporary working directory for requests
RUN mkdir tmp bin

# install npm dependencies
RUN mkdir 4-scratchpad
COPY 4-scratchpad/package*.json ./4-scratchpad/
RUN (cd 4-scratchpad && npm install)

# copy each stage and compile/build it

# compile MiniLISP and haskell
COPY 0-src 0-src
COPY 1-interpreter 1-interpreter
RUN 1-interpreter/compile-cpp.sh

COPY 2-readme-generator 2-readme-generator
COPY 3-HTTP 3-HTTP
COPY 4-scratchpad 4-scratchpad
COPY Makefile ./

# frontend
RUN (cd 4-scratchpad && npm run build)
