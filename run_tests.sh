#!/usr/bin/env bash

## Abort on error
set -e

if [[ $(uname) == *"CYGWIN"* ]]; then
  EBIN=$(cygpath -w $PWD/ebin)
else
  EBIN=$PWD/ebin
fi

erl -make
ct_run -noshell -pa $EBIN -include ./include -dir ./test -logdir ./var/logs -cover ./cover.spec
