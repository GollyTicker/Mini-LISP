# two-stage Dockerfile. builder and app

# ==== builder ======
FROM gcc:10.2 as builder
# start with gcc
RUN echo "============ This will take a few minutes.... ============"

# install haskell
RUN apt-get update && apt-get install -y haskell-platform
RUN cabal update && cabal install command MissingH

# GHC dynamic linking: https://gist.github.com/TimWSpence/269ab6943fbbaaf4b66374364f0051cd
# RUN apt-get update && apt-get download libgmp10
# RUN mv libgmp*.deb libgmp.deb
# RUN ls *.deb

RUN g++ --version
RUN ghc --version

# install app
RUN mkdir /app
WORKDIR /app

# copy files
COPY [0-4]* ./

# compile MiniLISP
RUN ./2-compile-cpp.sh
RUN ./2-compile-hs.sh

# RUN echo "Dynamic linked library dependencies:" && \
#     echo "MiniLISP" && ldd ./MiniLISP && \
#     echo "2-run-examples" && ldd ./2-run-examples

# TODO: compile HTTP rest endpoint
#CMD /bin/bash

# ==== app ======
# FROM builder as app
# RUN mkdir /app
WORKDIR /app

# GHC dynamic libraries
# COPY --from=builder /libgmp.deb /tmp
# RUN dpkg -i /tmp/libgmp.deb && rm /tmp/libgmp.deb
#
# COPY --from=builder /usr/local/lib64 /usr/local/lib64
# COPY --from=builder /lib64 /lib64
# COPY --from=builder /lib/x86_64-linux-gnu /lib/x86_64-linux-gnu
# RUN ls /usr/local/lib64

# COPY --from=builder  /app/[0-4]* /app/MiniLISP ./
RUN ls .

# REPL as entrypoint
CMD ./MiniLISP
