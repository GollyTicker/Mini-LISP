#/bin/bash
s="$(cat README.md.template)"

PREFIX=$(echo "$s" | grep -B 1000 '^%%%' | sed '$d')
CMD="$(echo "$s" | grep '^%%%' | cut -c 4-)"

$CMD | tee cmd-out

echo "$PREFIX" > "README.md"
RES="$(cat cmd-out | sed -e 's/^/\* /' | sed 's/| / `/' | sed -e 's/$/`/')"
echo "$RES" >> "README.md"
