%%%-------------------------------------------------------------------
%%% @author WangWeiNing
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. 八月 2017 11:27
%%%-------------------------------------------------------------------
-module(tiny_log_recorder_sup).
-author("Wang").
-behavior(supervisor).

%% API
-export([
    start_link/0,
    start_recorder/2
]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_recorder(Name, Args) ->
    supervisor:start_child(?MODULE, [Name, Args]).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    ChildSpec = {tiny_log_recorder, {tiny_log_recorder, start_link, []}, temporary, 5, worker, [tiny_log_recorder]},
    {ok, { {simple_one_for_one, 0, 1}, [ChildSpec]} }.

