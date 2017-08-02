%%%-------------------------------------------------------------------
%%% @author WangWeiNing
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. 八月 2017 19:57
%%%-------------------------------------------------------------------
-module(tiny_log_util).
-author("Wang").

%% API
-export([
  timestamp/0,
  datetime_to_timestamp/1,
  timestamp_to_datetime/1
]).


-type ts() :: integer().

%%%  DateTime
-spec timestamp()->non_neg_integer().
timestamp()->
  {M, S, _} = os:timestamp(),
  M * 1000000 + S.

-spec datetime_to_timestamp(calendar:datetme()) -> ts().
datetime_to_timestamp(DateTime) ->
  calendar:datetime_to_gregorian_seconds(DateTime) - calendar:datetime_to_gregorian_seconds(calendar:universal_time_to_local_time({{1970, 1, 1}, {0, 0, 0}})).

-spec timestamp_to_datetime(integer()) -> calendar:datetime().
timestamp_to_datetime(Seconds) ->
  I = calendar:datetime_to_gregorian_seconds(calendar:universal_time_to_local_time({{1970, 1, 1}, {0, 0, 0}})),
  calendar:gregorian_seconds_to_datetime(I + Seconds).
