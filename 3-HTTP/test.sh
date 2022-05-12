#/bin/bash

make docker-server-no-tty
sleep 3 # wait for sever startup

PORT="$(cat 3-HTTP/port.txt)"
URL="http://localhost:$PORT/file"

TOTAL=""

RES="$(curl -G --max-time 10 --data-urlencode "value='hi" "$URL")"
[ $RES = hi ] || TOTAL="failed"

RES="$(curl -G --max-time 10 --data-urlencode "value=(eval '(cond ('() 'b) ('t 'a)) '())" "$URL")"
[ $RES = a ] || TOTAL="failed"

RES="$(curl -G --max-time 10 --data-urlencode "value=(eval '((lambda (f n ig) (cond ((eq n '0) '0) ('t (+ n (f (decr n) ))))) '+ '2 '#ignore-embed-eval#) (environment))" "$URL")"
(echo "$RES" | grep "Time Limit" >/dev/null) || TOTAL="failed"

make docker-stop-server-no-tty

echo "Testing REST API finished."

test -z "$TOTAL" # returns exitcode 0 if all passed
