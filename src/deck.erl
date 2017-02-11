-module(deck).
-export([init/0, stop/1, draw/1, count/1, shuffle/1]).

-record(card, {value, suite}).

-define(NUM_DECKS, 8).

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
    Pid ! {self(), shuffle},

    receive
        ok ->
            ok;
        _ ->
            ct:pal("Received unexpected response in shuffle()"),
            error
    after 100 ->
              timeout
    end.

deck() ->
    ct:pal("I'm alive!"),

    loop(generate_deck()).

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
            From ! ok,
            loop(generate_deck());
        {From, die} ->
            From ! ok,
            ct:pal("Bye bye.. *wawing*"),
            ok
    end.

generate_deck() ->
    Deck = lists:flatten(
             lists:duplicate(?NUM_DECKS, [{X, Y} ||
                                          X <- lists:seq(2, 14),
                                          Y <- [club, heart, diamond, spade]])),
    %% Add black shuffle deck card and shuffle the deck
    shuffle_deck([{0, black} | Deck]).

shuffle_deck(Deck) ->
    Random = lists:sort(
               [{rand:uniform()*100*100*100, Y} || Y <- Deck]),
    [X || {_,X} <- Random].
