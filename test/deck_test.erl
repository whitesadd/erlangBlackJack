
-module(deck_test).

-include_lib("eunit/include/eunit.hrl").

init_stop_test() ->
    Pid = deck:init(),
    ok = deck:stop(Pid).
