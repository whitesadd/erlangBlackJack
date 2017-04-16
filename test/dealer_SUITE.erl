%%%-------------------------------------------------------------------
%%% File    : dealer_SUITE.erl
%%% Author  : Civing
%%% Description : Test dealer module
%%%
%%% Created : 2017-04-12
%%%-------------------------------------------------------------------
-module(dealer_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-include("blackjack.hrl").

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%%--------------------------------------------------------------------
all() ->
    [tc001_init_stop,
     tc002_new_game,
     tc003_two_players,
     tc004_aces,
     tc005_court_cards
    ].

tc001_init_stop(_Config) ->
    ok = dealer:stop(dealer:init()).

tc002_new_game(_Config) ->
    Pid = dealer:init(),
    ok = dealer:new_game(Pid, self(), [self()]),
    ok = expect_draw(1, heart),
    ok = expect_dealt_card(),
    ok = expect_draw(2, heart),
    ok = expect_draw(3, heart),
    ok = expect_dealt_card(),

    %% Standard cards given, expect actions
    ok = expect_action(draw),
    ok = expect_draw(4, heart),
    ok = expect_dealt_card(),
    ok = expect_action(stop),

    %% Dealer last card
    ok = expect_draw(13, clubs),
    ok = expect_draw(10, clubs),

    %% No more actions
    ok = expect_nothing().

tc003_two_players(_Config) ->
    Pid = dealer:init(),
    ok = dealer:new_game(Pid, self(), [self(), self()]),

    %% Player one, first card
    ok = expect_draw(1, heart),
    ok = expect_dealt_card(),

    %% Player two, first card
    ok = expect_draw(1, heart),
    ok = expect_dealt_card(),

    %% Dealer, first card
    ok = expect_draw(2, heart),

    %% Player one, second card
    ok = expect_draw(3, heart),
    ok = expect_dealt_card(),

    %% Player two, second card
    ok = expect_draw(3, heart),
    ok = expect_dealt_card(),

    %% Player one actions
    ok = expect_action(draw),
    ok = expect_draw(4, heart),
    ok = expect_dealt_card(),
    ok = expect_action(stop),

    %% Player two actions
    ok = expect_action(draw),
    ok = expect_draw(6, heart),
    ok = expect_dealt_card(),
    ok = expect_action(draw),
    ok = expect_draw(7, heart),
    ok = expect_dealt_card(),
    ok = expect_action(draw),
    ok = expect_draw(8, heart),
    ok = expect_dealt_card(),
    ok = expect_action(stop),

    %% Dealer last card
    ok = expect_draw(13, clubs),
    ok = expect_draw(1, clubs),
    ok = expect_draw(10, clubs),

    %% No more actions
    ok = expect_nothing().

tc004_aces(_Config) ->
    Pid = dealer:init(),
    ok = dealer:new_game(Pid, self(), [self()]),
    ok = expect_draw(1, heart),
    ok = expect_dealt_card(),
    ok = expect_draw(1, heart),
    ok = expect_draw(3, heart),
    ok = expect_dealt_card(),

    % Don't care about player
    ok = expect_action(stop),

    %% Dealer last card, previous sum = 11
    ok = expect_draw(1, clubs),
    ok = expect_draw(1, clubs),
    ok = expect_draw(1, clubs),
    ok = expect_draw(1, clubs),

    %% Dealer shoud know have 14
    ok = expect_draw(3, heart),

    %% No more actions (3 + 14 = 17)
    ok = expect_nothing().

tc005_court_cards(_Config) ->
    Pid = dealer:init(),
    ok = dealer:new_game(Pid, self(), [self()]),
    ok = expect_draw(7, heart),
    ok = expect_dealt_card(),
    ok = expect_draw(1, heart),
    ok = expect_draw(3, heart),
    ok = expect_dealt_card(),

    % Don't care about player
    ok = expect_action(stop),

    %% Dealer last card, previous sum = 7
    ok = expect_draw(11, clubs),

    %% No more actions (7 + 10 = 17)
    ok = expect_nothing().

expect_draw(Value, Suite) ->
    receive
        {From, draw, none} ->
            From ! {ok, #card{value=Value, suite=Suite}},
            ok;
        Msg ->
            io:format("Received unexpected response ~w~n", [Msg]),
            fail
    after
        100 ->
            timeout
    end.

expect_dealt_card() ->
    receive
        {From, deal_card, _Card} ->
            From ! {ok, none},
            ok;
        Msg ->
            io:format("Received unexpected response ~w~n", [Msg]),
            error
    after
        100 ->
            timeout
    end.

expect_action(ActionToTake) ->
    receive
        {From, action, none} ->
            From ! {ok, ActionToTake},
            ok;
        Msg ->
            io:format("Received unexpected response ~w~n", [Msg]),
            error
    after
        100 ->
            timeout
    end.

expect_nothing() ->
    receive
        Msg ->
            io:format("Received unexpected message ~w~n", [Msg]),
            error
    after
        100 ->
            ok
    end.

