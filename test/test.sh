#!/usr/bin/env bash
set -eo pipefail

cd "$(dirname "$0")/.."

rm -f ~/.codex
rm -f ./test/test-project/codex.tags
rm -f ./test/test-project/TAGS

export STACK_YAML="stack.yaml"
cd ./test/test-project
which cabal || stack install cabal-install
stack exec --no-ghc-package-path -- cabal update
stack exec --no-ghc-package-path -- cabal install hasktags
codex set tagger hasktags
codex set format emacs
codex update

tagsFile=codex.tags

# This is a dumb canary until better tests can be written
## This is disabled because the SHA1 isn't deterministic on TravisCI. No idea why.
# cd ..
# tagsHash=$(sha1sum "$tagsFile" | awk '{print $1}')

# echo "$tagsHash"

# if [ "$tagsHash" != "71594f46fc81822371c48516048e301f98467781" ]
# then
#     echo "TAGS SHA1 hash didn't match expected value, was: "
#     echo "$tagsHash"
#     ls test-project/
#     exit 1;
# fi

someFuncCount=$(grep -c someFunc "$tagsFile")

if [ "$someFuncCount" -lt 1 ]
then
    echo "Grepping for someFunc in TAGS was less than 1"
    echo "$someFuncCount"
    exit 1;
fi
