#!/usr/bin/env escript
%%! -setcookie king_node -name status_checker
main([NodeName]) ->
  start_distribution(),
  Node = list_to_atom(NodeName),
  show_king(Node),
  ok;
main([]) ->
  main(["king_node1@127.0.0.1"]);
main(_)->
  io:format("Show current king").

show_king(Node) ->
  case get_king(Node) of
    node_is_not_running ->
      io:format("Error: node is not running ~n");
    Res ->
      io:format("~p is the king! ~n", [Res])
  end.

get_king(Node) ->
  case net_adm:ping(Node) of
    pang -> node_is_not_running;
    pong -> rpc:call(Node, king, who_is_the_king, [])
  end.

start_distribution() ->
  net_kernel:start([node(), shortnames]).


