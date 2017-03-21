#!/usr/bin/env bash

if [[ $(uname) == *"CYGWIN"* ]]; then
  EBIN=$(cygpath -w $PWD/ebin)
else
  EBIN=$PWD/ebin
fi

ct_run -noshell -pa $EBIN -include ./include -dir ./test -logdir ./var/logs -cover ./cover.spec