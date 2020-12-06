docker stop minilisp-scratchpad minilisp-http 2> /dev/null > /dev/null
nohup make docker-server-prod > /dev/null &
nohup make docker-scratchpad > /dev/null &
