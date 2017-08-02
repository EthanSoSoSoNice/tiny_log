-ifndef(TINY_LOG).
-define(TINY_LOG, true).

-record(log_message, {
  ts = 0 :: integer(),
  content = "" :: string()
}).

-endif.