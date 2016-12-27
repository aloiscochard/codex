#!/usr/bin/env bash

pwd

cd .. && stack install && stack install hasktags

export STACK_YAML="stack.yaml"

cd ./test/test-project && codex set tagger hasktags && codex set format emacs && codex update

cd ..

# test-project/TAGS
tagsHash=$(sha1sum test-project/codex.tags | awk '{print $1}')

echo "$tagsHash"

# This is a dumb canary until better tests can be written
if [ "$tagsHash" != "9127ac957615834ab7c95b16d1043e9cadd300c0" ]
then
    echo "TAGS SHA1 hash didn't match expected value, was: "
    echo "$tagsHash"
    ls test-project/
    exit 1;
fi

someFuncCount=$(grep -c someFunc test-project/TAGS)

if [ "$someFuncCount" -lt 1 ]
then
    echo "Grepping for someFunc in TAGS was less than 1"
    echo "$someFuncCount"
    exit 1;
fi
