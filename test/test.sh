#!/usr/bin/env bash

pwd

cd .. && stack install && stack install hasktags

cd ./test/test-project && codex set tagger hasktags && codex set format emacs && codex update

cd ..

tagsHash=$(sha1sum test-project/TAGS | awk '{print $1}')

echo "$tagsHash"

# This is a dumb canary until better tests can be written
if [ "$tagsHash" != "5f5c498838e51ba086952a0a99e48c9532d2bbb4" ]
then
    echo "TAGS SHA1 hash didn't match expected value, was: "
    echo "$tagsHash"
    exit 1;
fi

someFuncCount=$(grep -c someFunc test-project/TAGS)

if [ "$someFuncCount" -lt 1 ]
then
    echo "Grepping for someFunc in TAGS was less than 1"
    echo "$someFuncCount"
    exit 1;
fi
