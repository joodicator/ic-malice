DEFAULT_TESTS=`echo ex/{archaeology,valid}/*.alice`
TESTS=${*:-$DEFAULT_TESTS}
FAILED=0
COUNT=0

for FILE in $TESTS; do
    echo "======= $FILE ======="
    $PROGRAM < $FILE; EXIT_CODE=$?
    if [[ $EXIT_CODE != 0 ]]; then
        echo "==> Test '$FILE' failed with exit code $EXIT_CODE." >&2
        (( ++FAILED ))
    fi
    (( ++COUNT ))
    echo
done

if [[ $FAILED == 0 ]]; then
    echo "==> All tests passed." >&2
else
    echo "==> $FAILED/$COUNT tests failed." >&2
    exit 1
fi
