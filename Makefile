export IMG := minilisp:v1
export TESTER := minilisp-tester
export HTTP_PORT = $(shell cat 3-HTTP/port.txt)
export RUN_DEFAULT_ARGS_NO_TTY := -p ${HTTP_PORT}:${HTTP_PORT} ${IMG}
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
	docker run --rm ${RUN_DEFAULT_ARGS}

test: build test-0-reset
	docker run --name ${TESTER} ${RUN_DEFAULT_ARGS} bash 2-readme-generator/generate-readme.sh && docker cp ${TESTER}:/app/README.md README.md
	make test-0-reset
	make test-server

test-server:
	./3-HTTP/test.sh

test-0-reset:
	docker rm -f ${TESTER} > /dev/null 2> /dev/null || true
