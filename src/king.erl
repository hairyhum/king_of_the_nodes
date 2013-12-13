-module(king).

-export([start/0, who_is_the_king/0]).

start() ->
  application:start(king).

who_is_the_king() ->
  ok = application:ensure_started(king),
  king_fsm:who_is_the_king().