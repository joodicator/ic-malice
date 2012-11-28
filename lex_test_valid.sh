#!/bin/bash

DEF=`echo ex/{archaeology,valid}/*.alice`

for F in ${*:-$DEF}; do
    echo -e "\n======= $F ======="
    ./AliceLexerTest < $F
    EC=$?
    if [[ $EC != 0 ]]; then
        echo -e "\nTest '$F' failed with exit code $EC." >&2
        exit $EC;
    fi
done
echo -e "\nAll tests passed." >&2
