%%%-------------------------------------------------------------------
%%% @author WangWeiNing
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. 八月 2017 20:42
%%%-------------------------------------------------------------------
-module(tiny_log_port_writer).
-author("Wang").
-include("tiny_log.hrl").

-type port_writer_opt() :: 
{port_command, ErlangPortCommand :: tuple()} | 
{port_settings, ErlangPortSettings :: list()} |
{flush_period, integer()}|
{cache_upper_limit, integer()}.

-record(state, {
  port
}).

-record(port_state, {
  cache,
  cache_count,
  cache_upper_limit = 0,
  flush_period = 0,
  port
}).

%% API
-export([
  start/2,
  stop/1,
  write/2,
  rotate/2
]).


%% internal 
-export([
  port_init/4
  ]).

%% Test
-export([
    test_port/0
  ]).

-spec start(tiny_log_recorder:roate(), [port_writer_opt()]) -> #state{}.
start(_Rotate, Opts) ->
  PortCommand = proplists:get_value(port_command, Opts),
  PortSettings = proplists:get_value(port_settings, Opts, [{packet, 2}]),
  CacheUpperLimit = proplists:get_value(cache_upper_limit, Opts, 500),
  FlushPeriod = proplists:get_value(flush_period, Opts, 5000),
  Port = start_port_proc(PortCommand, PortSettings, CacheUpperLimit, FlushPeriod),
  #state{
    port = Port
  }.

-spec stop(#state{}) -> ok.
stop(State) ->
  #state{
    port = Port
  } = State,
  Port ! {self(), shutdown},
  receive
    {Port, terminate} ->
      ok
  end.

-spec write(binary(), #state{}) -> #state{}.
write(Log, State) ->
  #state{
    port = Port
  } = State,
  Port ! {write, Log},
  State.

-spec rotate(tiny_log_recorder:rotate(), #state{}) -> #state{}.
rotate(_Rotate, State) ->
  State.



%%%============================================
%%% Port Process
%%%============================================

start_port_proc(PortCommand, PortSettings, CacheUpperLimit, FlushPeriod) ->
  proc_lib:spawn_link(
    ?MODULE,
    port_init,
    [PortCommand, PortSettings, CacheUpperLimit, FlushPeriod] %% sure binary setting
  ).

port_init(PortCommand, PortSettings, CacheUpperLimit, FlushPeriod) ->
  put(count, 0),
  process_flag(trap_exit, true),
  put(port_command, PortCommand),
  put(port_settings, [binary|PortSettings]),
  Port = open_port(),
  PortState = #port_state{
    cache = <<>>,
    cache_count = 0,
    cache_upper_limit = CacheUpperLimit,
    flush_period = FlushPeriod,
    port = Port
  },
  start_flush_timer(PortState),
  port_loop(PortState).

port_loop(PortState) ->
  #port_state{
    port = Port,
    cache_count = CacheCount
  } = PortState,
  receive
    {write, Bin} when is_binary(Bin) ->
      {ok, NewPortState} =  write_log(Bin, PortState),
      port_loop(NewPortState);
    
    {Port, closed} ->
      error_logger:error_msg("[port_closed] port:~p~n", [Port]),
      port_loop(restart_port(PortState));
    
    {timeout, _, flush_cache} when CacheCount > 0 ->
      start_flush_timer(PortState),
      port_loop(flush_cache(PortState));
    
    {timeout, _, flush_cache} ->
      start_flush_timer(PortState),
      port_loop(PortState);

    {'EXIT', Port, Reason} ->
      error_logger:error_msg("[port_exit] port:~p, reason:~p~n", [Port, Reason]),
      port_loop(restart_port(PortState));

    {Port, {data, _Data}} ->
      port_loop(PortState);
    
    {Parent, shutdown} ->
      flush_cache(PortState),
      close_port(Port),
      Parent ! {self(), terminate}
  end.

start_flush_timer(PortState) ->
  FlushPeriod = PortState#port_state.flush_period,
  erlang:start_timer(FlushPeriod, self(), flush_cache).


close_port(Port) ->
  Port ! {self(), close},
  receive
    {Port, closed} ->
      ok;
    {'EXIT', Port, Reason} ->
      error_logger:error_msg("[port_exit] port:~p, reason:~p~n", [Port, Reason])
  end.

open_port() ->
  PortCommand = get(port_command),
  PortSettings = get(port_settings),
  open_port(PortCommand, PortSettings).

write_log(Bin, PortState) ->
  #port_state{
    cache_count = CacheCount,
    cache_upper_limit = CacheUpperLimit,
    cache = Cache
  } = PortState,
  NewCacheCount = CacheCount + 1,
  BinSize = byte_size(Bin),
  NewCache = <<BinSize:32/big, Bin/binary, Cache/binary>>,
  NewPortState = PortState#port_state{
    cache = NewCache,
    cache_count = NewCacheCount
  },
  if
    NewCacheCount =:= CacheUpperLimit ->
      {ok, flush_cache(NewPortState)};
    true ->
      {ok, NewPortState}
  end.

flush_cache(PortState) ->
  flush_cache(PortState, true).

flush_cache(PortState, Retry) ->
  Port = PortState#port_state.port,
  Cache = PortState#port_state.cache,
  CacheCount = PortState#port_state.cache_count,
  StartTS = gs_time:millisecond(),
  Bin = <<CacheCount:32/big, Cache/binary>>,
  PortInfo = erlang:port_info(Port, input),
  Port ! {self(), {command, Bin}},
  EndTS = gs_time:millisecond(),
  put(count, get(count) + CacheCount),
  error_logger:error_msg("[flush_cache] cache_count:~p, ts:~p, count:~p, bin_size:~p, port_info:~p~n", [CacheCount, EndTS - StartTS, get(count), byte_size(Bin), PortInfo]),
  case wait_reply(PortState) of
    port_closed when Retry ->
      flush_cache(restart_port(PortState), false);
    
    Other when Other =:= {ok, <<"ok">>} orelse Other =:= port_closed ->
      PortState#port_state{
        cache_count = 0,
        cache = <<>>
      }
  end.

wait_reply(PortState) ->
  Port = PortState#port_state.port,
  receive
    {Port, {data, Data}} ->
      {ok, Data};
    
    {Port, closed} ->
      port_closed;
    
    {'EXIT', Port, Reason} ->
      error_logger:error_msg("[port_exit] port:~p, reason:~p~n", [Port, Reason]),
      port_closed
  end.
    
    
restart_port(PortState) ->
  OldPort = PortState#port_state.port,
  NewPort = open_port(),
  error_logger:error_msg("[restart port] old_port:~p, new_port:~p~n", [OldPort, NewPort]),
  PortState#port_state{
    port = NewPort
  }.
    
      
%%%==============================================
%%% Test Function
%%%==============================================

test_port() ->
  PortCommand = {spawn, "python ../test.py"},
  PortSettings = [{packet, 4}],
  Port = start_port_proc(PortCommand, PortSettings, 1, 1000),
  Port ! {write, <<"hello port!">>},
  Port ! shutdown.