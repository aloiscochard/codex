#!/usr/bin/env sh
set -e


# https://github.com/travis-ci/travis-build/blob/master/lib/travis/build/templates/header.sh
travis_retry() {
  local result=0
  local count=1
  while [ $count -le 3 ]; do
    [ $result -ne 0 ] && {
      echo -e "\n${ANSI_RED}The command \"$@\" failed. Retrying, $count of 3.${ANSI_RESET}\n" >&2
    }
    # ! { } ignores set -e, see https://stackoverflow.com/a/4073372
    ! { "$@"; result=$?; }
    [ $result -eq 0 ] && break
    count=$(($count + 1))
    sleep 1
  done

  [ $count -gt 3 ] && {
    echo -e "\n${ANSI_RED}The command \"$@\" failed 3 times.${ANSI_RESET}\n" >&2
  }

  return $result
}

cd "$(dirname "$0")/.."

rm -f ~/.codex
rm -f ./test/test-project/codex.tags
rm -f ./test/test-project/TAGS

# export STACK_YAML="stack.yaml"
cd ./test/test-project
travis_retry cabal update
cabal install hasktags
cabal install .
codex set tagger hasktags
codex set format emacs

codex update

tagsFile=TAGS

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
