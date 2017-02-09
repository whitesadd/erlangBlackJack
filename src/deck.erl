-module(deck).
-export([init/0, stop/1, draw/1, count/1]).

-record(card, {value, suite}).

init() ->
    spawn(fun() -> deck() end).

draw(Pid) ->
    Pid ! {self(), draw},

    receive
        {Value, Suite} ->
            #card{value=Value, suite=Suite};
        empty ->
            empty;
        _ ->
            ct:pal("Received unexpected response in draw()"),
            error
    after 100 ->
              timeout
    end.

stop(Pid) ->
    Pid ! {self(), die},

    receive
        ok ->
            ok;
        _ ->
            ct:pal("Received unexpected response in stop()"),
            error
    after 100 ->
              timeout
    end.

count(Pid) ->
    Pid ! {self(), count},

    receive
        X when is_number(X) ->
            X;
        _ ->
            ct:pal("Received unexpected response in count()"),
            error
    after 100 ->
              timeout
    end.

shuffle(Pid) ->
    Pid ! {self(), count},

    receive
        X when is_number(X) ->
            X;
        _ ->
            ct:pal("Received unexpected response in count()"),
            error
    after 100 ->
              timeout
    end.

deck() ->
    ct:pal("I'm alive!"),
    Deck= lists:flatten(lists:duplicate(8, generate_deck())),

    loop(shuffle_deck(Deck)).

loop(Deck) ->
    receive
        {From, draw} ->
            case length(Deck) of
                0 ->
                    From ! empty,
                    loop(Deck);
                _ ->
                    From ! hd(Deck),
                    loop(tl(Deck))
            end;
        {From, count} ->
            From ! length(Deck),
            loop(Deck);
        {From, shuffle} ->
            shuffle_deck(Deck),
            From ! ok,
            loop(Deck);
        {From, die} ->
            From ! ok,
            ct:pal("Bye bye.. *wawing*"),
            ok
    end.

generate_deck() ->
    [ {X, Y} ||
      X <- lists:seq(2, 14),
      Y <- [club, heart, diamond, spade] ].

shuffle_deck(Deck) ->
    %% TBD
    Deck.
