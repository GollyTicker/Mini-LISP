export IMG := minilisp:v1
export TESTER := minilisp-tester
export LISP_HTTP_PORT = $(shell cat 3-HTTP/port.txt)
export SCRATCHPAD_PORT = $(shell cat 4-scratchpad/port.txt)
export RUN_DEFAULT_ARGS_NO_TTY := -p ${LISP_HTTP_PORT}:${LISP_HTTP_PORT} -p ${SCRATCHPAD_PORT}:${SCRATCHPAD_PORT} ${IMG}
export RUN_DEFAULT_ARGS := -it ${RUN_DEFAULT_ARGS_NO_TTY}

build:
	docker build -t ${IMG} .

docker-bash: build
	docker run --rm ${RUN_DEFAULT_ARGS} bash

# for manual server
docker-server: build
	docker run --rm ${RUN_DEFAULT_ARGS} runhaskell 3-HTTP/HTTP.hs

# production docker server. find logs via $ docker logs minilisp-server
docker-server-prod: build
	docker rm -f minilisp-server 2> /dev/null > /dev/null || true
	docker run --name minilisp-server ${RUN_DEFAULT_ARGS_NO_TTY} runhaskell 3-HTTP/HTTP.hs

# used in test.sh script
docker-server-no-tty:
	docker run --rm ${RUN_DEFAULT_ARGS_NO_TTY} runhaskell 3-HTTP/HTTP.hs

repl: build
	docker run --rm ${RUN_DEFAULT_ARGS} bin/MiniLISP

docker-scratchpad: build
	docker run --rm ${RUN_DEFAULT_ARGS_NO_TTY} serve -s -l 8080 4-scratchpad/dist

test: build test-0-reset
	docker run --name ${TESTER} ${RUN_DEFAULT_ARGS} bash 2-readme-generator/generate-readme.sh && docker cp ${TESTER}:/app/README.md README.md
	make test-0-reset
	make test-server

test-server:
	./3-HTTP/test.sh

test-0-reset:
	docker rm -f ${TESTER} > /dev/null 2> /dev/null || true
