#!/usr/bin/env bash

## Abort on error
set -e

if [[ $(uname) == *"CYGWIN"* ]]; then
  EBIN=$(cygpath -w $PWD/ebin)
else
  EBIN=$PWD/ebin
fi

erl -make
ct_run -noshell -warnings_as_errors -pa $EBIN -include ./include -dir ./test -logdir ./var/logs -cover ./cover.spec
