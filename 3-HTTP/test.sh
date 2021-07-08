#/bin/bash

make build

make docker-server-no-tty 2> /dev/null > /dev/null &
PID=$!
sleep 3 # wait for sever startup

PORT="$(cat 3-HTTP/public-port.txt)"
URL="http://localhost:$PORT/file"

TOTAL=""

RES="$(curl -G --max-time 10 --data-urlencode "value='hi" "$URL")"
[ $RES = hi ] || TOTAL="failed"

RES="$(curl -G --max-time 10 --data-urlencode "value=(eval '(cond ('() 'b) ('t 'a)) '())" "$URL")"
[ $RES = a ] || TOTAL="failed"

RES="$(curl -G --max-time 10 --data-urlencode "value=(eval '((lambda (f n ig) (cond ((eq n '0) '0) ('t (+ n (f (decr n) ))))) '+ '2 '#ignore-embed-eval#) (environment))" "$URL")"
(echo "$RES" | grep "Time Limit" > /dev/null ) || TOTAL="failed"

kill $PID 2> /dev/null
sleep 1 # let server fully finish

echo "Testing REST API finished."

test -z "$TOTAL" # returns exitcode 0 if all passed
