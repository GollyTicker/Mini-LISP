export IMG := minilisp:v1
export TESTER := minilisp-tester
export HTTP_PORT = $(shell cat 5-HTTP-port.txt)
export RUN_DEFAULT_ARGS := -p ${HTTP_PORT}:${HTTP_PORT} -it ${IMG}

build:
	docker build -t ${IMG} .

docker-bash: build
	docker run --rm ${RUN_DEFAULT_ARGS} bash

docker-server:build
	docker run --rm ${RUN_DEFAULT_ARGS} runhaskell 5-HTTP.hs

repl: build
	docker run --rm ${RUN_DEFAULT_ARGS}

test: build test-0-reset
	docker run --name ${TESTER} ${RUN_DEFAULT_ARGS} bash ./3-generate-readme.sh \
		&& docker cp ${TESTER}:/app/README.md README.md
	make test-0-reset

test-0-reset:
	docker rm -f ${TESTER}  > /dev/null 2> /dev/null || true
