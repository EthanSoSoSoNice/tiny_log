%%%-------------------------------------------------------------------
%%% @author WangWeiNing
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. 八月 2017 19:59
%%%-------------------------------------------------------------------
-module(tiny_log_config).
-author("WangWeiNing").

%% API
-export([
  init_db/0,
  get_config/2,
  get_config/3,
  set_config/3
]).

init_db() ->
  ets:new(?MODULE, [named_table, public, {read_concurrency, true}]).

get_config(RecorderRef, Key) ->
  get_config(RecorderRef, Key, undefined).

get_config(RecorderRef, Key, Default) ->
  case ets:lookup(?MODULE, {sure_pid(RecorderRef), Key}) of
    [] ->
      Default;
    [{_, V}] ->
      V
  end.

set_config(RecorderRef, Key, Default) ->
  ets:insert(?MODULE, {{sure_pid(RecorderRef), Key}, Default}).

%%% internal

sure_pid(Ref) when is_atom(Ref) ->
  whereis(Ref);
sure_pid(Ref) ->
  Ref.
