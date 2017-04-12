%%%-------------------------------------------------------------------
%%% File    : dealer.erl
%%% Author  : Civing
%%% Description : Dealer representation in Black Jack
%%%
%%% Created : 2017-04-12
%%%-------------------------------------------------------------------
-module(dealer).
-export([init/0, stop/1]).

-include("blackjack.hrl").

init() ->
    spawn(fun() -> dealer() end).


send({Pid, Command}) ->
    send({Pid, Command, none});
send({Pid, Command, Args}) ->
    Pid ! {self(), Command, Args},

    receive
        {ok, Result} ->
            case Result of
                none ->
                    ok;
                _ ->
                    {ok, Result}
            end;
        error ->
            error;
        Msg ->
            io:format("Received unexpected response ~w~n", [Msg]),
            error
    after
        100 ->
            timeout
    end.

stop(Pid) ->
    io:format("Calling stop for dealer ~w~n", [Pid]),
    send({Pid, die}).

dealer() ->
    io:format("Dealer alive ~w~n", [self()]),

    loop([]).

loop(State) ->
    receive
        {From, die, _} ->
            io:format("Dealer ~w dying~n", [self()]),
            From ! {ok, none};
        Msg ->
            io:format("Unexpected message ~w~n", [Msg]),
            element(1, Msg) ! error
    end.
