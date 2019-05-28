%%%-------------------------------------------------------------------
%%% @author WangWeiNing
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. 八月 2017 20:10
%%%-------------------------------------------------------------------
-module(tiny_log_recorder).
-author("WangWeiNing").
-behavior(gen_server).
-define(DEFAULT_FORMATTER, tiny_log_default_formatter).
-define(DEFAULT_WRITER, tiny_log_file_writer).
-define(DEFAULT_PROTECTION, 300).
-define(DEFAULT_COUNT, 0).
-define(DEFAULT_ROTATE, day).
-include("tiny_log.hrl").

-type recorder_ref() :: atom() | pid().
-type rotate() :: day | hour. %% rotation period
-type writer() :: atom(). %% MODULE
-type formatter() :: atom() | function(). %% MFA | MODULE | Function
-type tiny_log_opt() :: {formatter, formatter()} | {writer, writer()} | {rotate, rotate()} | {protection, integer()} |{writer_args, any()} | {count, 0}.

-record(state, {
  writer,
  writer_state,
  protection,
  rotate,
  count,
  cache_len,
  cache
}).

%% API
-export([
  start_link/0,
  start_link/1,
  start_link/2,
  write_log/2
]).

%%  GenServer Callback
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  code_change/3,
  terminate/2
]).


%% Test
-export([
  test/0
]).


%%%=========================================================
%%% API
%%%=========================================================

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
  gen_server:start_link(?MODULE, [], []).

-spec start_link(atom()) -> {ok, pid()} | {error, any()}.
start_link(Name) when is_atom(Name) ->
  gen_server:start_link({local, Name}, ?MODULE, [], []).

-spec start_link(atom(), [tiny_log_opt()]) -> {ok, pid()} | {error, any()}.
start_link(Name, Opts) ->
  gen_server:start_link({local, Name}, ?MODULE, Opts, []).

-spec write_log(recorder_ref(), any()) -> ok.
write_log(RecorderRef, Content) ->
  LogMessage = #log_message{ ts = tiny_log_util:timestamp(), content = Content },
  Formatter = tiny_log_config:get_config(RecorderRef, formatter, undefined),
  FormattedLog = case Formatter of
                  undefined ->
                    Content;
                  _ when is_atom(Formatter) ->
                    Formatter:format(LogMessage);
                  _ ->
                    Formatter(LogMessage)
                 end,
  case tiny_log_config:get_config(RecorderRef, is_async, false) of
    false ->
      gen_server:cast(RecorderRef, {log, FormattedLog});
    true ->
      gen_server:call(RecorderRef, {log, FormattedLog})
  end.



%%%=========================================================
%%% GenServer Callback
%%%=========================================================
init(Opts) ->
  process_flag(trap_exit, true),
  Formatter = proplists:get_value(formatter, Opts, undefined),
  tiny_log_config:set_config(self(), formatter, Formatter),
  Writer  = proplists:get_value(writer, Opts, ?DEFAULT_WRITER),
  WriterArgs = proplists:get_value(writer_args, Opts, []),
  Rotate = proplists:get_value(rotate, Opts, ?DEFAULT_ROTATE),
  WriterState = Writer:start(Rotate, WriterArgs),
  Protection = proplists:get_value(protection, Opts, ?DEFAULT_PROTECTION),
  Count = proplists:get_value(count, Opts, ?DEFAULT_COUNT),
  start_rotate(Rotate),
  {ok, #state{
    writer = Writer,
    writer_state = WriterState,
    protection = Protection,
    rotate = Rotate,
    count = Count,
    cache_len = 0,
    cache = []
  }}.

handle_call({log, Log}, _From, State) ->
  NewState = handle_log(Log, State),
  {reply, ok, NewState};
handle_call(_Msg, _From, State) ->
  {noreply, State}.

handle_cast({log, Log}, State) ->
  NewState = handle_log(Log, State),
  {noreply, NewState};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({timeout, _, rotate}, State) ->
  #state{
    writer = Writer,
    rotate = Rotate
  } = State,
  State1 = write_cache(State),
  WriterState = State1#state.writer_state,
  NewWriterState = Writer:rotate(Rotate, WriterState),
  start_rotate(Rotate),
  NewState = State1#state{ writer_state = NewWriterState },
  {noreply, NewState};
handle_info(_Msg, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  #state{ writer = Writer, writer_state = WriterState } = State,
  Writer:stop(WriterState),
  ok.

code_change(_Old, State, _Extra) ->
  {ok, State}.

%%%=========================================================
%%% Internal Function
%%%=========================================================

handle_log(Log, State) ->
  {message_queue_len, Len} = erlang:process_info(self(), message_queue_len),
  #state{
    protection = Protection,
    count = Count,
    cache_len = CacheLen
    } = State,
  IsAsync = tiny_log_config:get_config(self(), is_async, false),
  if
    Len > Protection andalso not IsAsync ->
      tiny_log_config:set_config(self(), is_async, true);
    Len < Protection andalso IsAsync ->
      tiny_log_config:set_config(self(), is_async, false);
    true ->
      ok
  end,
  case Count of
    0 -> %% no cache
      write(Log, State);
    _ ->
      NewCacheLen = CacheLen + 1,
      Cache = State#state.cache,
      NewCache = [Log|Cache],
      State1 = State#state{
        cache = NewCache,
        cache_len = NewCacheLen
      },
      if
        Count =:= NewCacheLen ->
          write_cache(State1);
        true ->
          State1
      end
  end.

write_cache(#state{ cache = [] } = State) ->
  State;
write_cache(State) ->
  Cache = State#state.cache,
  State1 = lists:foldl(
    fun(Log, StateTemp) ->
      write(Log, StateTemp)
    end,
    State,
    lists:reverse(Cache)
  ),
  State1#state{ cache = [], cache_len = 0 }.

write(Log, State) ->
  Writer = State#state.writer,
  NewWriterState = Writer:write(Log, State#state.writer_state),
  State#state{ writer_state = NewWriterState }.

start_rotate(day) ->
  TS = tiny_log_util:timestamp(),
  {Date, _Time} = tiny_log_util:timestamp_to_datetime(TS + (24 * 60 * 60)),
  RotateTS = tiny_log_util:datetime_to_timestamp({Date, {0, 0, 0}}) - TS,
  erlang:start_timer(RotateTS * 1000, self(), rotate);
start_rotate(hour) ->
  TS = tiny_log_util:timestamp(),
  {Date, {H, _S, _M}} = tiny_log_util:timestamp_to_datetime(TS + 60 * 60),
  RotateTS = tiny_log_util:datetime_to_timestamp({Date, {H, 0, 0}}) - TS,
  erlang:start_timer(RotateTS * 1000, self(), rotate).


%%%==========================================================
%%% Test Function
%%%==========================================================
test() ->
  Opts = [
    {count, 10},
    {writer, tiny_log_port_writer},
    {writer_args, [
      {port_command, {spawn, "python ../test.py"}}, 
      {port_settings, [{packet, 4}]
    }]}
  ],
  {ok, _Pid} = start_link(recorder_test, Opts),
  lists:foreach(
    fun(I) ->
      tiny_log_recorder:write_log(recorder_test, <<"{\"id\":", (integer_to_binary(I))/binary, "}">>)
    end,
    lists:seq(1, 9)
  ),
  timer:sleep(5000),
  tiny_log_recorder:write_log(recorder_test, <<"{\"id\": 10}">>).