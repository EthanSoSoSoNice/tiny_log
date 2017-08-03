%%%-------------------------------------------------------------------
%%% @author WangWeiNing
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. 八月 2017 20:42
%%%-------------------------------------------------------------------
-module(tiny_log_file_writer).
-author("Wang").
-include("tiny_log.hrl").

-record(state, {
  io_handle,
  root,
  prefix,
  suffix
}).
%% API
-export([
  start/2,
  stop/1,
  write/2,
  rotate/2
]).

-type file_writer_opt() :: {rotate, tiny_log_recorder:rotate()} | {file_prefix, string()}  | {root, string()} | {file_suffix, string()}.

-spec start(tiny_log_recorder:roate(), [file_writer_opt()]) -> #state{}.
start(Rotate, Opts) ->
  RootPath = proplists:get_value(root, Opts, "./"),
  Prefix = proplists:get_value(file_prefix, Opts, ""),
  Suffix = proplists:get_value(file_suffix, Opts, ""),
  FileName = filename(Rotate, Prefix, Suffix),
  FilePath = filename:join(RootPath, FileName),
  filelib:ensure_dir(FilePath),
  {ok, IoHandle} = file:open(FilePath, [append]),
  #state{
    io_handle = IoHandle,
    root = RootPath,
    prefix = Prefix,
    suffix = Suffix
  }.


-spec write(binary(), #state{}) -> #state{}.
write(Log, State) ->
  #state{ io_handle = IoHandle } = State,
  ok = file:write(IoHandle, Log),
  State.

-spec stop(#state{}) -> ok.
stop(State) ->
  #state{
    io_handle = IoHandle
  } = State,
  file:close(IoHandle).

-spec rotate(tiny_log_recorder:rotate(), #state{}) -> #state{}.
rotate(Rotate, State) ->
  #state{
    io_handle = IoHandle,
    prefix = Prefix,
    root = Root,
    suffix = Suffix
  } = State,
  ok = file:close(IoHandle),
  NewFileName = filename(Rotate, Prefix, Suffix),
  FilePath = filename:join(Root, NewFileName),
  {ok, NewIOHandle} = file:open(FilePath, [append]),
  State#state{
    io_handle = NewIOHandle
  }.

filename(day, Prefix, Suffix) ->
  {{YY, MM, DD}, _} = calendar:local_time(),
  lists:concat([Prefix, conv(YY), conv(MM), conv(DD), Suffix]);
filename(hour, Prefix, Suffix) ->
  {{YY, MM, DD}, {HH, _, _}} = calendar:local_time(),
  lists:concat([Prefix, conv(YY), conv(MM), conv(DD), conv(HH), Suffix]).

conv(N) when N < 10 ->
  [$0, $0 + N];
conv(N) ->
  integer_to_list(N).

