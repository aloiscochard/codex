#!/usr/bin/env bash

pwd


cd .. && stack install && stack install hasktags

export STACK_YAML="stack.yaml"

rm -f ~/.codex
rm -f ./test/test-project/codex.tags
rm -f ./test/test-project/TAGS

cd ./test/test-project && codex set tagger hasktags && codex set format emacs && stack exec -- codex update

cd ..

tagsFile=test-project/codex.tags
tagsHash=$(sha1sum "$tagsFile" | awk '{print $1}')

echo "$tagsHash"

# This is a dumb canary until better tests can be written
if [ "$tagsHash" != "71594f46fc81822371c48516048e301f98467781" ]
then
    echo "TAGS SHA1 hash didn't match expected value, was: "
    echo "$tagsHash"
    ls test-project/
    exit 1;
fi

someFuncCount=$(grep -c someFunc "$tagsFile")

if [ "$someFuncCount" -lt 1 ]
then
    echo "Grepping for someFunc in TAGS was less than 1"
    echo "$someFuncCount"
    exit 1;
fi
