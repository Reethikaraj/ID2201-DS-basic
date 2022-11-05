%%%-------------------------------------------------------------------
%%% @author reeth
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Oct 2022 14:38
%%%-------------------------------------------------------------------
-module(gms2).
-author("reeth").

%% API
-export([start/1, start/2]).

-define(timeout, 1000).
-define(arghh, 100).

start(Id) ->
  Rnd = rand:uniform(1000),
  Self = self(),
  {ok, spawn_link(fun() -> leaderinit(Id, Rnd, Self) end)}.

leaderinit(Id, Rnd, Master) ->
  random:seed(Rnd, Rnd, Rnd),
  leader(Id, Master, [], [Master]).

start(Id, Grp) ->
  Self = self(),
  {ok, spawn_link(fun()->init(Id, Grp, Self) end)}.

init(Id, Grp, Master) ->
  Self = self(),
  Grp ! {join, Master, Self},
  receive
    {view, [Leader|Slaves], Group} ->
      Master ! {view, Group},
      erlang:monitor(process, Leader),
      slave(Id, Master, Leader, Slaves, Group)
  after ?timeout ->
    Master ! {error, "no reply from leader"}
  end.

leader(Id, Master, Slaves, Group) ->
  receive
    {mcast, Msg} ->
      io:format("gms leader ~w: received {mcast, ~w}~n", [Id, Msg]),
      bcast(Id, {msg, Msg}, Slaves),
      Master ! Msg,
      io:format("gms leader ~w: sent ~w to Master ~w~n", [Id, Msg, Master]),
      leader(Id, Master, Slaves, Group);
    {join, Wrk, Peer} ->
      io:format("gms leader ~w: joining request from ~w to master~n", [Id, Peer]),
      Slaves2 = lists:append(Slaves, [Peer]),
      Group2 = lists:append(Group, [Wrk]),
      bcast(Id, {view, [self()|Slaves2], Group2}, Slaves2),
      io:format("gms leader ~w: broadcasted new view ~n", [Id]),
      Master ! {view, Group2},
      leader(Id, Master, Slaves2, Group2);
    stop ->
      ok;
    Error ->
      io:format("gms ~w: leader, strange message ~w~n",[Id, Error])
  end.

slave(Id, Master, Leader, Slaves, Group) ->
  receive
    {mcast, Msg} ->
      io:format("gms slave ~w: recieved {mcast,~w} from master ~n",[Id,Msg]),
      Leader ! {mcast, Msg},
      io:format("gms slave ~w: sent {mcast,~w} to Leader~n",[Id, Msg]),
      slave(Id, Master, Leader, Slaves, Group);
    {join, Wrk, Peer} ->
      io:format("gms slave ~w: recieved joining request from ~w ~n", [Id, Peer]),
      Leader ! {join, Wrk, Peer},
      io:format("gms slave ~w: sent joining request to leader~n", [Id]),
      slave(Id, Master, Leader, Slaves, Group);
    {msg, Msg} ->
      io:format("gms slave ~w: recieved {msg, ~w}~n", [Id, Msg]),
      Master ! Msg,
      io:format("gms slave ~w: sent {msg, ~w} to Master~n", [Id, Msg]),
      slave(Id, Master, Leader, Slaves, Group);
    {view, [Leader|Slaves2], Group2} ->
      io:format("gms slave ~w: received view ~w~n", [Id,[Leader|Slaves2]]),
      Master ! {view, Group2},
      io:format("gms slave ~w: sent view ~w to Master ~n", [Id,[Leader|Slaves2]]),
      slave(Id, Master, Leader, Slaves2,Group2);
    {'DOWN', _Ref, process, Leader, _Reason} ->
      io:format("gms slave ~w: leader, went down ~w~n", [Id, Leader]),
      election(Id, Master, Slaves, Group);
    stop ->
      ok;
    Error ->
      io:format("gms ~w:slave, strange message ~w~n",[Id, Error])
  end.

election(Id, Master, Slaves, [_|Group]) ->
  Self = self(),
  case Slaves of
    [Self|Rest] ->
      bcast(Id, {view, Slaves, Group}, Rest),
      Master ! {view, Group},
      io:format("gms election ~w: Electing self to leader~n", [Id]),
      leader(Id, Master, Rest, Group);
    [Leader|Rest] ->
      erlang:monitor(process, Leader),
      io:format("gms election ~w: Electing ~w: to leader~n", [Id, Leader]),
      slave(Id, Master, Leader, Rest, Group)
  end.

bcast(Id, Msg, Nodes) ->
  io:format("gms leader ~w : Broadcasting msg ~w to ~w~n",[Id,Msg,Nodes]),
  lists:foreach(fun(Node) -> Node ! Msg, crash(Id) end, Nodes).

crash(Id) ->
  case random:uniform(?arghh) of
    ?arghh ->
      io:format("leader ~w: crash~n", [Id]),
      exit(no_luck);
    _ ->
      ok
  end.

