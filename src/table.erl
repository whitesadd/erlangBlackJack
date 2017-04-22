%%%-------------------------------------------------------------------
%%% File    : table.erl
%%% Author  : Civing
%%% Description : Table representation in Black Jack
%%%
%%% Created : 2017-04-22
%%%-------------------------------------------------------------------
-module(table).
-behavior(gen_server).
-export([init/0, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2,
         code_change/3, handle_info/2]).

-record(state, {}).

-include("blackjack.hrl").

%% Table API

init() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    Pid.

stop(Pid) ->
    io:format("Calling stop for table ~w~n", [Pid]),
    gen_server:call(Pid, terminate).

%% Table Server Implementation

init(_Args) ->
    io:format("Table alive ~w~n", [self()]),
    {ok, #state{}}.

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(Request, State) ->
    io:format("Unexpected cast Request: ~p~n",[Request]),
    {noreply, State}.

handle_info(Msg, _State) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, Msg}.

terminate(Reason, _State) ->
    io:format("Table ~w dying, Reason ~w~n", [self(), Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    %% No change planned. The function is there for the behaviour
    {ok, State}.
