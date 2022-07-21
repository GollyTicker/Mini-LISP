export IMG := minilisp:v1
export IMG_DEV := minilisp:dev
export IMG_FILE := minilisp_v1.tar.gz
export TESTER := minilisp-tester
export LISP_HTTPER := minilisp-http
export SCRATCHPADER := minilisp-scratchpad
export LISP_HTTP_PORT := $(shell cat 3-HTTP/port.txt)
export SCRATCHPAD_PORT := $(shell cat 4-scratchpad/port.txt)
export EXPOSE_LISP := -p ${LISP_HTTP_PORT}:${LISP_HTTP_PORT}
export EXPOSE_SCRATCHPAD := -p ${SCRATCHPAD_PORT}:${SCRATCHPAD_PORT}
export DEFAULT_ARGS_NO_TTY := ${EXPOSE_LISP} ${IMG}
export DEFAULT_ARGS := -it ${DEFAULT_ARGS_NO_TTY}
export DEFAULT_ARGS_DEV := -it ${EXPOSE_LISP} ${IMG_DEV}
export SCRATCHPAD_ARGS := ${EXPOSE_SCRATCHPAD} ${IMG}
export LISP_HTTP_CMD := bin/httpServer
export SCRATCHPAD_CMD := nginx -c /app/4-scratchpad/nginx.conf
export SCRATCHPAD_DEV_CMD := serve -s -l ${SCRATCHPAD_PORT} 4-scratchpad/dist
export USE_COMPOSE := -f 5-docker/docker-compose.yml
export USE_COMPOSE_DEV := -f 5-docker/docker-compose-dev.yml

export BACKEND_DOMAIN_NAME := swaneet.eu

# Steps to deploy onto server
# 0. write <user>@<server-hostname> into private-login.txt
# 1. locally build image and save image to disk: make save-image-to-disk
# 2. deploy image to server: make deploy-image-to-server
# 3. on server load image from disk: make load-image-from-disk
# 4. on server start app: ./restart-service.sh
# OR simply: make build-deploy-to-server

build-deploy-to-server:
	make save-image-to-disk
	make deploy-image-to-server
	ssh $$(cat private-login.txt) "cd ~/Mini-LISP && make load-image-from-disk && ./restart-service.sh"

readme:
	./2-readme-generator/generate-readme.sh

build:
	docker build -f 5-docker/Dockerfile --target builder -t ${IMG_DEV} .
	docker build -f 5-docker/Dockerfile --target final   -t ${IMG}     .

save-image-to-disk: build
	@echo "Saving image to disk.... (takes around a minute)"
	docker save ${IMG} | gzip > ${IMG_FILE}
	@echo "Done."

deploy-image-to-server: # save-image-to-disk
	scp ${IMG_FILE} $$(cat private-login.txt):~/Mini-LISP/

load-image-from-disk:
	@echo "Loading image from disk.... (takes around a minute)"
	gzip -d --stdout ${IMG_FILE} | docker load
	@echo "Done."

docker-compose:
	docker-compose ${USE_COMPOSE} rm -f
	docker-compose ${USE_COMPOSE} up --build -d

docker-compose-dev:
	docker-compose ${USE_COMPOSE} ${USE_COMPOSE_DEV} rm -f
	docker-compose ${USE_COMPOSE} ${USE_COMPOSE_DEV} up --build -d

update-npm-dependencies:
	docker run -v ${PWD}/4-scratchpad:/web -it minilisp:v1 /bin/sh -c "cd /web && npm update"
	docker run -v ${PWD}/4-scratchpad:/web -it minilisp:v1 /bin/sh -c "cd /web && npm audit fix"

shutdown:
	docker-compose -f 5-docker/docker-compose.yml down

docker-bash: build
	docker run --rm ${DEFAULT_ARGS} bash

# for manual server
docker-server: build
	docker run --rm ${DEFAULT_ARGS} bin/httpServer

# production docker server. find logs via $ docker logs minilisp-http
docker-server-prod:
	docker rm -f ${LISP_HTTPER} 2> /dev/null > /dev/null || true
	docker run --name ${LISP_HTTPER} ${DEFAULT_ARGS_NO_TTY} ${LISP_HTTP_CMD}

# used in test.sh script
docker-server-no-tty: build docker-stop-server-no-tty
	docker run -d --name server-no-tty --rm ${DEFAULT_ARGS_NO_TTY} bin/httpServer

docker-stop-server-no-tty:
	docker stop server-no-tty || true
	docker rm -f server-no-tty || true
	
repl: build
	docker run --rm ${DEFAULT_ARGS} bin/MiniLISP

docker-scratchpad: build
	docker run --rm --name ${SCRATCHPADER} ${SCRATCHPAD_ARGS} ${SCRATCHPAD_CMD}

start-dev:
	make build
	./restart-service.sh --dev	

stop:
	./stop-services.sh

# TODO: if any test fails, then make test should also fail
test: build test-0-reset
	docker run \
		-v $$PWD/1-interpreter:/app/1-interpreter \
		-v $$PWD/2-readme-generator:/app/2-readme-generator \
		--name ${TESTER} ${DEFAULT_ARGS_DEV} bash -c \
		"cabal update && cabal install --lib --global command split && 2-readme-generator/generate-readme.sh" \
	docker cp ${TESTER}:/app/README.md README.md
	make test-0-reset
	make test-server

test-server:
	./3-HTTP/test.sh

test-0-reset:
	docker rm -f ${TESTER} > /dev/null 2> /dev/null || true
