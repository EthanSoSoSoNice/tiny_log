%%%-------------------------------------------------------------------
%%% @author WangWeiNing
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. 八月 2017 20:35
%%%-------------------------------------------------------------------
-module(tiny_log_default_formatter).
-author("Wang").
-include("tiny_log.hrl").

%% API
-export([
  format/1
]).

-spec format(#log_message{}) -> binary().
format(LogMessage) ->
  #log_message{ content = Content } = LogMessage,
  <<Content/binary, "\n">>.
