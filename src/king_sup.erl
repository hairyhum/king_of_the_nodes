-module (king_sup).

-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link(?MODULE, []).

init([]) ->
  {ok, Timeout} = application:get_env(king, timeout),
  Node = {
    king_fsm,
    {king_fsm, start_link, [kingwaiter, Timeout]},
    transient,
    2000,
    worker,
    [king_fsm]},

  {ok, { {one_for_one, 1000, 10}, [Node] }}.