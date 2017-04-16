%%%-------------------------------------------------------------------
%%% File    : dealer.erl
%%% Author  : Civing
%%% Description : Dealer representation in Black Jack
%%%
%%% Created : 2017-04-12
%%%-------------------------------------------------------------------
-module(dealer).
-export([init/0, stop/1, new_game/3]).

-record(state, {deck = [], hand = [], players = []}).

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

new_game(Pid, Deck, Players) ->
    send({Pid, new_game, [Deck, Players]}).

dealer() ->
    io:format("Dealer alive ~w~n", [self()]),

    loop(#state{}).

loop(_State) ->
    receive
        {From, die, _} ->
            io:format("Dealer ~w dying~n", [self()]),
            From ! {ok, none};
        {From, new_game, [Deck, Players]} ->
            io:format("Starting new game with deck ~w and Players ~w~n",
                      [Deck, Players]),
            From ! {ok, none},
            lists:foreach(fun(P) -> player:deal_card(P, deck:draw(Deck)) end, Players),
            Hand = [deck:draw(Deck)],
            lists:foreach(fun(P) -> player:deal_card(P, deck:draw(Deck)) end, Players),

            io:format("Ask players ~w for actions~n", [Players]),
            ask_for_action(Players, Deck),

            FinalHand = draw_cards(Deck, Hand),

            loop(#state{deck = Deck, players = Players, hand = FinalHand});
        Msg ->
            io:format("Unexpected message ~w~n", [Msg]),
            element(1, Msg) ! error
    end.

ask_for_action([], _Deck) ->
    ok;
ask_for_action([Player | Players], Deck) ->
    {ok, Action} = player:action(Player),
    io:format("Player ~w action ~w~n", [Player, Action]),
    case Action of
        draw ->
            player:deal_card(Player, deck:draw(Deck)),
            ask_for_action([Player | Players], Deck);
        stop ->
            ask_for_action(Players, Deck)
    end.

draw_cards(Deck, Cards) ->
    draw_cards(Deck, Cards, sum_cards(Cards)).
draw_cards(_Deck, Cards, Sum) when Sum >= 17 ->
    Cards;
draw_cards(Deck, Cards, _Sum) ->
    NewHand = [deck:draw(Deck) | Cards],
    draw_cards(Deck, NewHand, sum_cards(NewHand)).

sum_cards(Cards) ->
    sum_cards(Cards, 0, 0).
sum_cards(Cards, Sum, Aces) when Sum > 21, Aces > 0 ->
    %% Reduce one ace to value 1
    sum_cards(Cards, Sum - 10, Aces - 1);
sum_cards([Card | Cards], Sum, Aces) when Card#card.value > 10 ->
    %% Reduce court card to value 10
    sum_cards(Cards, Sum + 10, Aces);
sum_cards([Card | Cards], Sum, Aces) when Card#card.value =:= 1 ->
    %% Ace, add 11 to sum and increment aces by one
    sum_cards(Cards, Sum + 11, Aces + 1);
sum_cards([Card | Cards], Sum, Aces) ->
    sum_cards(Cards, Sum + Card#card.value, Aces);
sum_cards([], Sum, _Aces) ->
    Sum.

