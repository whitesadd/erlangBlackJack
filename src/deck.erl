%%%-------------------------------------------------------------------
%%% File    : deck.erl
%%% Author  : Civing
%%% Description : Deck representation in Black Jack
%%%
%%% Created : 2017-02-01
%%%-------------------------------------------------------------------
-module(deck).
-export([init/0, stop/1, draw/1, count/1, shuffle/1]).

-include("blackjack.hrl").

-define(NUM_DECKS, 8).

init() ->
    spawn(fun() -> deck() end).

send(Pid, Command) ->
    send(Pid, Command, none).
send(Pid, Command, Args) ->
    io:format("Sending ~w to ~w~n", [Command, Pid]),
    Pid ! {self(), Command, Args},

    receive
        {ok, Result} ->
            case Result of
                none ->
                    ok;
                _ ->
                    Result
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

draw(Pid) ->
    send(Pid, draw).

stop(Pid) ->
    send(Pid, die).

count(Pid) ->
    send(Pid, count).

shuffle(Pid) ->
    send(Pid, shuffle).

deck() ->
    io:format("I'm alive!~n"),

    loop(generate_deck()).

loop(Deck) ->
    receive
        {From, draw, _} ->
            case length(Deck) of
                0 ->
                    From ! {ok, empty},
                    loop(Deck);
                _ ->
                    From ! {ok, hd(Deck)},
                    loop(tl(Deck))
            end;
        {From, count, _} ->
            From ! {ok, length(Deck)},
            loop(Deck);
        {From, shuffle, _} ->
            From ! ok,
            loop(generate_deck());
        {From, die, _} ->
            From ! {ok, none},
            io:format("Bye bye.. *wawing*~n"),
            ok;
        Msg ->
            io:format("Unexpected message ~w~n", [Msg]),
            element(1, Msg) ! error
    end.

generate_deck() ->
    Deck = lists:flatten(
             lists:duplicate(?NUM_DECKS, [#card{value=X, suite=Y} ||
                                          X <- lists:seq(2, 14),
                                          Y <- [club, heart, diamond, spade]])),
    %% Add black shuffle deck card and shuffle the deck
    shuffle_deck([#card{value=0, suite=black} | Deck]).

shuffle_deck(Deck) ->
    Random = lists:sort(
               [{rand:uniform()*100*100*100, Y} || Y <- Deck]),
    [X || {_,X} <- Random].
