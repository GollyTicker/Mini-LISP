docker rm -f minilisp-tester

docker build -t minilisp:v1 . \
  && docker run --name minilisp-tester -it minilisp:v1 bash ./3-generate-readme.sh \
  && docker cp minilisp-tester:/app/README.md README.md

docker rm -f minilisp-tester > /dev/null
