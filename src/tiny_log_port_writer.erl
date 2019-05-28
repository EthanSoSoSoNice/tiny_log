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

-type port_writer_opt() :: {port_command, ErlangPortCommand :: tuple()} | 
  {port_settings, ErlangPortSettings :: list()}.

-record(state, {
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
  port_init/2
  ]).

%% Test
-export([
    test_port/0
  ]).

-spec start(tiny_log_recorder:roate(), [port_writer_opt()]) -> #state{}.
start(_Rotate, Opts) ->
  PortCommand = proplists:get_value(port_command, Opts),
  PortSettings = proplists:get_value(port_settings, Opts, [{packet, 2}]),
  Port = start_port_proc(PortCommand, PortSettings),
  #state{
    port = Port
  }.

-spec stop(#state{}) -> ok.
stop(State) ->
  #state{
    port = Port
  } = State,
  Port ! shutdown,
  ok.

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

start_port_proc(PortCommand, PortSettings) ->
  proc_lib:spawn_link(
    ?MODULE,
    port_init,
    [PortCommand, PortSettings] %% sure binary setting
  ).

port_init(PortCommand, PortSettings) ->
  process_flag(trap_exit, true),
  Port = erlang:open_port(PortCommand, [binary|PortSettings]),
  port_loop(Port).

port_loop(Port) ->
  receive
    {write, Bin} when is_binary(Bin) ->
      Port ! {self(), {command, Bin}},
      port_loop(Port);

    {Port, closed} ->
      ok;

    {'EXIT', Port, Reason} ->
      error_logger:error_msg("port:~p exit reason:~p~n", [Port, Reason]),
      exit(port_terminate);

    {Port, {data, Data}} ->
      error_logger:error_msg("data:~p~n", [Data]);

    shutdown ->
      close_port(Port),
      port_loop(Port) %% waiting for exit or closed message
  end.

close_port(Port) ->
  Port ! {self(), close}.


%%%==============================================
%%% Test Function
%%%==============================================

test_port() ->
  PortCommand = {spawn, "python ../test.py"},
  PortSettings = [{packet, 4}],
  Port = start_port_proc(PortCommand, PortSettings),
  Port ! {write, <<"hello port!">>},
  Port ! shutdown.