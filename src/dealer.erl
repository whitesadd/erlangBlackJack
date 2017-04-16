%%%-------------------------------------------------------------------
%%% File    : dealer.erl
%%% Author  : Civing
%%% Description : Dealer representation in Black Jack
%%%
%%% Created : 2017-04-12
%%%-------------------------------------------------------------------
-module(dealer).
-behavior(gen_server).
-export([init/0, stop/1, new_game/3]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2,
         code_change/3, handle_info/2]).

-record(state, {deck = [], hand = [], players = []}).

-include("blackjack.hrl").

%% Dealer API

init() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    Pid.

stop(Pid) ->
    io:format("Calling stop for dealer ~w~n", [Pid]),
    gen_server:call(Pid, terminate).

new_game(Pid, Deck, Players) ->
    gen_server:call(Pid, {new_game, Deck, Players}).

%% Dealer Server Implementation

init(_Args) ->
    io:format("Dealer alive ~w~n", [self()]),
    {ok, #state{}}.

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State};

handle_call({new_game, Deck, Players}, From, _State) ->
    io:format("Starting new game with deck ~w and Players ~w~n",
              [Deck, Players]),
    gen_server:reply(From, ok),
    lists:foreach(fun(P) -> player:deal_card(P, deck:draw(Deck)) end, Players),
    Hand = [deck:draw(Deck)],
    lists:foreach(fun(P) -> player:deal_card(P, deck:draw(Deck)) end, Players),

    io:format("Ask players ~w for actions~n", [Players]),
    ask_for_action(Players, Deck),

    FinalHand = draw_cards(Deck, Hand),

    {noreply, #state{deck=Deck, players=Players, hand=FinalHand}}.

terminate(Reason, _State) ->
    io:format("Dealer ~w dying, Reason ~w~n", [self(), Reason]),
    ok.

handle_cast(Request, State) ->
    io:format("Unexpected cast Request: ~p~n",[Request]),
    {noreply, State}.

handle_info(Msg, _State) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, Msg}.

code_change(_OldVsn, State, _Extra) ->
    %% No change planned. The function is there for the behaviour
    {ok, State}.

%% Util. functions

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

