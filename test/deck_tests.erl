
-module(deck_tests).

-include_lib("eunit/include/eunit.hrl").

-define(NO_OF_CARDS, (52 * 8 + 1)).

init_stop_test_() ->
    ?_assert(ok =:= deck:stop(deck:init())).

count_test_() ->
    ?_assertEqual(?NO_OF_CARDS, deck:count(deck:init())).

%% Auxiliary Functions

draw_test_() ->
    Pid = deck:init(),
    Count = deck:count(Pid),
    ?_assertEqual(Count, length(draw_all_cards(Pid))).

draw_all_cards(Pid) ->
    draw_all_cards(Pid, []).

draw_all_cards(Pid, DrawnCardList) ->
    case deck:draw(Pid) of
        empty ->
            lists:reverse(DrawnCardList);
        Card ->
            draw_all_cards(Pid, [Card | DrawnCardList])
    end.
