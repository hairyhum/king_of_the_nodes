-module(king_fsm).
-behaviour(gen_fsm).

-export([start_link/2]).
-export([init/1, vassal/2, king/2, king/3, pretender/2, kingwaiter/2]).
-export([terminate/3]).

-export([code_change/4, handle_event/3, handle_info/3, handle_sync_event/4]).

-export([who_is_the_king/0]).

-record(state, {
  king_num :: node_num(),
  timeout :: non_neg_integer(),
  ping_timeout :: non_neg_integer(),
  num :: node_num(),
  max_num :: non_neg_integer()
  }).

-type king_status() :: king | vassal | pretender | kingwaiter.
-type node_num() :: non_neg_integer().
-type vassal_message() :: {'alive?', node_num()} | {imtheking, node_num()} | finethanks | timeout.
-type vassal_next() :: vassal | pretender.

-type kingwaiter_message() :: {'alive?', node_num()} | {imtheking, node_num()} | timeout.
-type kingwaiter_next() :: vassal | pretender | kingwaiter.

-type pretender_message() :: {'alive?', node_num()} |  {imtheking, node_num()} | finethanks | timeout.
-type pretender_next() :: kingwaiter | vassal | pretender.

-type king_message() :: {'alive?', node_num()} | {imtheking, node_num()}.

-spec who_is_the_king() -> atom().
who_is_the_king() ->
  case gen_fsm:sync_send_all_state_event(king_fsm, 'who_is_the_king?') of
    {ok, NodeName} -> NodeName;
    {ask_later, Timeout} ->
      receive
        after trunc(Timeout) -> who_is_the_king()
      end
  end.

-spec start_link(king_status(), non_neg_integer()) -> {ok,pid()} | ignore | {error,term()}.
start_link(KingStatus, Timeout) ->
  gen_fsm:start_link({local, king_fsm}, ?MODULE, {KingStatus, Timeout}, []).

-spec init({king_status(), non_neg_integer()}) -> {ok, king_status(), #state{}, non_neg_integer()}.
init({KingStatus, Timeout}) when is_integer(Timeout) ->
  MaxNum = lists:max(king_nodes:numbers()),
  {ok, Num} = king_nodes:self_number(),
  State = #state{timeout = Timeout, ping_timeout = Timeout * 4, num = Num, max_num = MaxNum},
  io:format("Hi there! ~p~n", [Num]),
  {ok, KingStatus, State, 0}.

terminate(_, _StateName, _StateData) ->  ok.

-spec vassal(vassal_message(), #state{}) -> {next_state, vassal_next(), #state{}, non_neg_integer()}.
vassal({'alive?', FromNum}, State) ->
  king_nodes:send_to(FromNum, finethanks),
  start_elections(State);
vassal({imtheking, Num}, State) ->
  long_live_the_king(Num, State);
vassal(finethanks, State) ->
  {next_state, vassal, State, State#state.timeout};
vassal(timeout, #state{king_num = KingNum, ping_timeout = PingTimeout, timeout = Timeout} = State) ->
  case ping_the_king(KingNum, PingTimeout) of
    ok ->
      {next_state, vassal, State, Timeout};
    no_king ->
      start_elections(State)
  end.

-spec kingwaiter(kingwaiter_message(), #state{}) -> {next_state, kingwaiter_next(), #state{}, non_neg_integer()}.
kingwaiter({'alive?', FromNum}, State) ->
  king_nodes:send_to(FromNum, finethanks),
  {next_state, kingwaiter, State, State#state.timeout};
kingwaiter({imtheking, Num}, State) ->
  long_live_the_king(Num, State);
kingwaiter(finethanks, #state{timeout = Timeout} = State) ->
  {next_state, kingwaiter, State, Timeout};
kingwaiter(timeout, State) ->
  start_elections(State).

-spec pretender(pretender_message(), #state{}) -> {next_state, king, #state{}} | {next_state, pretender_next(), #state{}, non_neg_integer()}.
pretender({'alive?', FromNum}, State) ->
  king_nodes:send_to(FromNum, finethanks),
  {next_state, pretender, State, State#state.timeout};
pretender({imtheking, Num}, State) ->
  long_live_the_king(Num, State);
pretender(finethanks, #state{timeout = Timeout} = State) ->
  {next_state, kingwaiter, State, Timeout};
pretender(timeout, #state{num = Num} = State) ->
  coronate(Num, State).

  -spec king(king_message(), #state{}) -> {next_state, king, #state{}} | {stop, {overthrown, node_num()}, #state{}}.
king({'alive?', FromNum}, #state{num = Num} = State) ->
  king_nodes:send_to(FromNum, {imtheking, Num}),
  {next_state, king, State};
king({imtheking, KingNum}, State) ->
  {stop, {overthrown, KingNum}, State}.

-spec king(ping, {pid(),term()}, #state{}) -> {reply, pong, king, #state{}}.
king(ping, _From, #state{num = _Num} = State) ->
  {reply, pong, king, State}.

-spec ping_the_king(node_num()|undefined, non_neg_integer()) -> ok | no_king.
ping_the_king(undefined, _) ->
  no_king;
ping_the_king(KingNum, PingTimeout) when is_integer(KingNum)  ->
  io:format("Hey king! ~p~n", [KingNum]),
  try king_nodes:sync_send_to(KingNum, ping, PingTimeout) of
    {ok, pong} ->
      io:format("King found! ~p~n", [KingNum]),
      ok
  catch
    _:_ ->
      io:format("King not found! ~p~n", [KingNum]),
      no_king
  end.

-spec start_elections(#state{}) -> {next_state, pretender, #state{}, non_neg_integer()}.
start_elections(#state{num = Num, timeout = Timeout, max_num = MaxNum} = State) ->
  io:format("Who is the king? ~n"),
  case Num of
    MaxNum ->
      coronate(Num, State);
    _ ->
      check_alive(Num, MaxNum),
      {next_state, pretender, State, Timeout}
  end.

-spec long_live_the_king(node_num(), #state{}) -> {next_state, vassal, #state{}, non_neg_integer()}.
long_live_the_king(KingNum, State) ->
  io:format("Long live the king! ~p~n", [KingNum]),
  NewState = State#state{king_num = KingNum},
  {next_state, vassal, NewState, State#state.timeout}.

-spec check_alive(node_num(), node_num()) -> list(ok).
check_alive(Num, MaxNum) ->
  [ king_nodes:send_to(NodeNum, {'alive?', Num})
    || NodeNum <- lists:seq(Num+1, MaxNum) ].

coronate(Num, #state{max_num = MaxNum} = State) ->
  NewState = State#state{king_num = Num},
  declare_king_status(Num, MaxNum),
  {next_state, king, NewState}.

-spec declare_king_status(node_num(), node_num()) -> list(ok).
declare_king_status(Num, MaxNum) ->
  io:format("I'm the king ! ~p~n", [Num]),
  [ king_nodes:send_to(NodeNum, {imtheking, Num})
    || NodeNum <- lists:seq(1, MaxNum),
    NodeNum =/= Num ].

code_change(_OldVsn, StateName, StateData, _Extra) -> {ok, StateName, StateData}.
handle_event(_Event, StateName, #state{timeout = Timeout} = StateData) -> {next_state,StateName,StateData,Timeout}.
handle_info(_Info, StateName, #state{timeout = Timeout} = StateData) -> {next_state,StateName,StateData,Timeout}.

handle_sync_event(
  'who_is_the_king?',
  _From,
  StateName,
  #state{
    king_num = KingNum,
    timeout = Timeout
  } = StateData) ->
  Result = case KingNum of
    undefined -> {ask_later, Timeout/2};
    Num when is_integer(Num) -> king_nodes:node_name(KingNum)
  end,
  {reply, Result, StateName, StateData, Timeout};
handle_sync_event(_Event, _From, StateName, #state{timeout = Timeout} = StateData) -> {next_state,StateName,StateData,Timeout}.

