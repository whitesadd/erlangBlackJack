%%%-------------------------------------------------------------------
%%% File    : player.erl
%%% Author  : Civing
%%% Description : Player representation of in Black Jack
%%%
%%% Created : 2017-03-21
%%%-------------------------------------------------------------------
-module(player).
-export([init/0, stop/1, deal_card/2, count_hand/1, reset/1]).

-include("blackjack.hrl").

init() ->
    spawn(fun() -> player() end).


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
    io:format("Calling stop for player ~w~n", [Pid]),
    send({Pid, die}).

deal_card(Pid, Card) ->
    send({Pid, deal_card, Card}).

count_hand(Pid) ->
    send({Pid, count_hand}).

reset(Pid) ->
    send({Pid, reset}).

player() ->
    io:format("Player alive ~w~n", [self()]),

    loop([]).

loop(Cards) ->
    receive
        {From, deal_card, Card} ->
            From ! {ok, none},
            loop([Card | Cards]);
        {From, count_hand, _} ->
            From ! {ok, count(Cards)},
            loop(Cards);
        {From, reset, _} ->
            From ! {ok, none},
            loop([]);
        {From, die, _} ->
            io:format("Player ~w dying~n", [self()]),
            From ! {ok, none};
        Msg ->
            io:format("Unexpected message ~w~n", [Msg]),
            element(1, Msg) ! error
    end.

count([]) ->
    0;
count([Card | Cards]) ->
    Card#card.value + count(Cards).

