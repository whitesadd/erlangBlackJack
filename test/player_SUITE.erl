%%%-------------------------------------------------------------------
%%% File    : player_SUITE.erl
%%% Author  : Civing
%%% Description : Test player module
%%%
%%% Created : 2017-03-21
%%%-------------------------------------------------------------------
-module(player_SUITE).

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
     tc002_deal_card,
     tc003_count_cards,
     tc004_reset,
     tc005_action
    ].

tc001_init_stop(_Config) ->
    ok = player:stop(player:init()).

tc002_deal_card(_Config) ->
    Pid = player:init(),
    ok = player:deal_card(Pid, #card{value=10, suite=clubs}),
    ok = player:deal_card(Pid, #card{value=2, suite=heart}).

tc003_count_cards(_Config) ->
    Pid = player:init(),
    ok = player:deal_card(Pid, #card{value=10, suite=spade}),
    ok = player:deal_card(Pid, #card{value=10, suite=clubs}),
    {ok, 20} = player:count_hand(Pid).

tc004_reset(_Config) ->
    Pid = player:init(),
    ok = player:deal_card(Pid, #card{value=9, suite=diamond}),
    ok = player:deal_card(Pid, #card{value=5, suite=heart}),
    {ok, 14} = player:count_hand(Pid),

    % Reset players hand, give cards and count again
    ok = player:reset(Pid),
    ok = player:deal_card(Pid, #card{value=2, suite=clubs}),
    ok = player:deal_card(Pid, #card{value=9, suite=clubs}),
    {ok, 11} = player:count_hand(Pid).

tc005_action(_Config) ->
    Pid = player:init(),
    ok = player:deal_card(Pid, #card{value=10, suite=spade}),
    ok = player:deal_card(Pid, #card{value=10, suite=clubs}),
    {ok, draw} = player:action(Pid),
    ok = player:deal_card(Pid, #card{value=10, suite=clubs}),
    {ok, draw} = player:action(Pid),
    ok = player:deal_card(Pid, #card{value=10, suite=clubs}),
    {ok, draw} = player:action(Pid),
    ok = player:deal_card(Pid, #card{value=10, suite=clubs}),
    {ok, stop} = player:action(Pid).

