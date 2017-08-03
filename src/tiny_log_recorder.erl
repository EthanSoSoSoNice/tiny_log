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
-define(DEFAULT_ROTATE, day).
-include("tiny_log.hrl").

-type recorder_ref() :: atom() | pid().
-type rotate() :: day | hour. %% rotation period
-type writer() :: atom(). %% MODULE
-type formatter() :: atom() | function(). %% MFA | MODULE | Function
-type tiny_log_opt() :: {formatter, formatter()} | {writer, writer()} | {rotate, rotate()} | {protection, integer()} |{writer_args, any()}.

-record(state, {
  writer,
  writer_state,
  protection,
  rotate
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
  start_rotate(Rotate),
  {ok, #state{
    writer = Writer,
    writer_state = WriterState,
    protection = Protection,
    rotate = Rotate
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
    writer_state = WriterState,
    rotate = Rotate
  } = State,
  NewWriterState = Writer:rotate(Rotate, WriterState),
  start_rotate(Rotate),
  NewState = State#state{ writer_state = NewWriterState },
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
    writer = Writer,
    writer_state = WriterState
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
  NewWriterState = Writer:write(Log, WriterState),
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
