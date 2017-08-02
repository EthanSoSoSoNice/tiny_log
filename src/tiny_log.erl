%%%-------------------------------------------------------------------
%%% @author WangWeiNing
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. 八月 2017 19:54
%%%-------------------------------------------------------------------
-module(tiny_log).
-author("WangWeiNing").
-include("tiny_log.hrl").

%% API
-export([
  start/2,
  write_log/2
]).

%% test
-export([
  test/1,
  test_write_log/1
]).

%%%===========================================
%%% API
%%%===========================================

-spec start(tiny_log_recorder:recorder_ref(), [tiny_log_recorder:tiny_log_opt()]) -> {ok, pid()} | {error, any()}.
start(Name, Opts) ->
  tiny_log_recorder_sup:start_recorder(Name, Opts).


-spec write_log(tiny_log_recorder:recorder_ref(), binary()) -> ok.
write_log(Name, Content) ->
  tiny_log_recorder:write_log(Name, Content).


%%%===========================================
%%% Test
%%%===========================================

test(N) ->
  {ok, _} = start(test, [{rotate, hour}]),
  [
    write_log(test, integer_to_binary(I))
    || I <- lists:seq(1, N)
  ].

test_write_log(N) ->
  [write_log(test, integer_to_binary(I)) || I <- lists:seq(1, N)].
