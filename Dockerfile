FROM gcc:10.2
# start with gcc
RUN echo "============ This will take a few minutes.... ============"

# install haskell
RUN apt-get update && apt-get install -y haskell-platform
RUN cabal update && cabal install command MissingH

RUN g++ --version
RUN ghc --version

# install app
RUN mkdir /app
WORKDIR /app
COPY *.cpp *.hs examples* README.md.template *.lisp *.sh ./

RUN pwd
RUN ls

RUN ./generate-readme.sh
