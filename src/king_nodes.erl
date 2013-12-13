-module(king_nodes).

-export([node_name/1, node_number/1, self_name/0, self_number/0, numbers/0]).

-export([send_to/2, sync_send_to/3]).

-spec send_to(integer() | atom(), term()) -> {error, no_such_node} | ok.
send_to(Num, Message) when is_integer(Num) ->
  case node_name(Num) of
    {ok, Name} -> send_to(Name, Message);
    {error, Err} -> {error, Err}
  end;
send_to(Name, Message) when is_atom(Name) ->
  gen_fsm:send_event({king_fsm, Name}, Message).

-spec sync_send_to(integer() | atom(), term(), integer()) -> {error, no_such_node} | {ok, term()}.
sync_send_to(Num, Message, Timeout) when is_integer(Num) ->
  case node_name(Num) of
    {ok, Name} -> sync_send_to(Name, Message, Timeout);
    {error, Err} -> {error, Err}
  end;
sync_send_to(Name, Message, Timeout) when is_atom(Name) ->
  {ok, gen_fsm:sync_send_event({king_fsm, Name}, Message, Timeout)}.

-spec node_name(integer()) -> {error, no_such_node} | {ok, atom()}.
node_name(Num) ->
  case proplists:get_value(Num, node_config()) of
    undefined -> {error, no_such_node};
    Node -> {ok, Node}
  end.

-spec node_number(atom()) -> {error, unknown_node} | {ok, integer()}.
node_number(Node) ->
  case lists:keyfind(Node, 2, node_config()) of
    false -> {error, unknown_node};
    {Num, Node} -> {ok, Num}
  end.

-spec self_name() -> {ok, atom()}.
self_name() -> {ok, node()}.

-spec self_number() -> {ok, integer()}.
self_number() -> node_number(node()).

-spec node_config() -> [{integer(), atom()}].
node_config() ->
  {ok, Env} = application:get_env(king, nodes),
  Env.

-spec numbers() -> [integer()].
numbers() ->
  {Numbers, _} = lists:unzip(node_config()),
  Numbers.