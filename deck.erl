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

deck() ->
    ct:pal("I'm alive!"),
    Deck = generate_deck(),

    loop(Deck, []).

loop(Deck, Trash) ->
    receive
        {From, draw} ->
            case length(Deck) of
                0 ->
                    From ! empty,
                    loop(Deck, Trash);
                _ ->
                    From ! hd(Deck),
                    loop(tl(Deck), [hd(Deck) | Trash])
            end;
        {From, count} ->
            From ! length(Deck),
            loop(Deck, Trash);
        {From, die} ->
            From ! ok,
            ct:pal("Bye bye.. *wawing*"),
            ok
    end.

generate_deck() ->
    Clubs = generate_suite_cards(club),
    Spades = generate_suite_cards(spade),
    Hearts = generate_suite_cards(heart),
    Diamonds = generate_suite_cards(diamond),

    lists:merge([Clubs, Spades, Hearts, Diamonds]).

generate_suite_cards(Suite) ->
    Numbers = lists:seq(2,14),
    Suites = lists:duplicate(13, Suite),
    lists:zip(Numbers, Suites).
