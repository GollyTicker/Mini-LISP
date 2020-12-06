export IMG := minilisp:v1
export TESTER := minilisp-tester
export LISP_HTTPER := minilisp-http
export SCRATCHPADER := minilisp-scratchpad
export LISP_HTTP_PORT = $(shell cat 3-HTTP/port.txt)
export SCRATCHPAD_PORT = $(shell cat 4-scratchpad/port.txt)
export EXPOSE_LISP := -p ${LISP_HTTP_PORT}:${LISP_HTTP_PORT}
export EXPOSE_SCRATCHPAD := -p ${SCRATCHPAD_PORT}:${SCRATCHPAD_PORT}
export DEFAULT_ARGS_NO_TTY := ${EXPOSE_LISP} ${IMG}
export DEFAULT_ARGS := -it ${DEFAULT_ARGS_NO_TTY}
export SCRATCHPAD_ARGS := ${EXPOSE_SCRATCHPAD} ${IMG}

build:
	docker build -t ${IMG} .

docker-bash: build
	docker run --rm ${DEFAULT_ARGS} bash

# for manual server
docker-server: build
	docker run --rm ${DEFAULT_ARGS} runhaskell 3-HTTP/HTTP.hs

# production docker server. find logs via $ docker logs minilisp-http
docker-server-prod: build
	docker rm -f ${LISP_HTTPER} 2> /dev/null > /dev/null || true
	docker run --name ${LISP_HTTPER} ${DEFAULT_ARGS_NO_TTY} runhaskell 3-HTTP/HTTP.hs

# used in test.sh script
docker-server-no-tty:
	docker run --rm ${DEFAULT_ARGS_NO_TTY} runhaskell 3-HTTP/HTTP.hs

repl: build
	docker run --rm ${DEFAULT_ARGS} bin/MiniLISP

docker-scratchpad: build
	docker run --rm --name ${SCRATCHPADER} ${SCRATCHPAD_ARGS} serve -s -l 8080 4-scratchpad/dist

test: build test-0-reset
	docker run --name ${TESTER} ${DEFAULT_ARGS} bash 2-readme-generator/generate-readme.sh && docker cp ${TESTER}:/app/README.md README.md
	make test-0-reset
	make test-server

test-server:
	./3-HTTP/test.sh

test-0-reset:
	docker rm -f ${TESTER} > /dev/null 2> /dev/null || true
