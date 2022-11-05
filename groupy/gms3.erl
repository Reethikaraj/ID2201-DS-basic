%%%-------------------------------------------------------------------
%%% @author reeth
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Oct 2022 11:09
%%%-------------------------------------------------------------------
-module(gms3).
-author("reeth").

%% API
-export([start/1, start/2]).

-define(timeout, 1000).
-define(arghh, 100).

start(Id) ->
  Rnd = random:uniform(1000),
  Self = self(),
  {ok, spawn_link(fun()-> init(Id, Rnd, Self) end)}.

init(Id, Rnd, Master) ->
  random:seed(Rnd, Rnd, Rnd),
  leader(Id, Master, [], 0, [Master]).

start(Id, Grp) ->
  Rnd = random:uniform(1000),
  Self = self(),
  {ok, spawn_link(fun()-> init(Id, Rnd, Grp, Self) end)}.

init(Id, Rnd, Grp, Master) ->
  random:seed(Rnd, Rnd, Rnd),
  Self = self(),
  Grp ! {join, Master, Self},
  receive
    {view, N, [Leader|Slaves], Group} ->
      Master ! {view, Group},
      erlang:monitor(process, Leader),
      slave(Id, Master, Leader, N+1, {view, N, [Leader|Slaves], Group}, Slaves, Group)
  after ?timeout ->
    Master ! {error, "no reply from leader"}
  end.

leader(Id, Master, Slaves, N, Group) ->
  receive
    {mcast, Msg} ->
      io:format("gms leader ~w: received {mcast, ~w} in state ~w~n", [Id, Msg, N]),
      bcast(Id, {msg, N, Msg}, Slaves),
      Master ! Msg,
      leader(Id, Master, Slaves, N+1, Group);
    {join, Wrk, Peer} ->
      io:format("gms leader ~w: recieved joining request from ~w to master~n", [Id, Peer]),
      Slaves2 = lists:append(Slaves, [Peer]),
      Group2 = lists:append(Group, [Wrk]),
      bcast(Id, {view, N, [self()|Slaves2],Group2}, Slaves2),
      Master ! {view, Group2},
      leader(Id, Master, Slaves2, N+1, Group2);
    stop ->
      ok;
    Error ->
      io:format("gms leader ~w: strange message ~w~n", [Id, Error])
  end.

slave(Id, Master, Leader, N, Last, Slaves, Group) ->
  receive
    {mcast, Msg} ->
      io:format("gms  slave ~w: received {mcast, ~w} in state ~w~n", [Id, Msg, N]),
      Leader ! {mcast, Msg},
      io:format("gms  slave ~w: sent {mcast, ~w} in state ~w to leader~n", [Id, Msg, N]),
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {join, Wrk, Peer} ->
      io:format("gms slave ~w: send joining request from ~w to leader~n", [Id, Peer]),
      Leader ! {join, Wrk, Peer},
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {msg, N, Msg} ->
      io:format("gms  slave ~w: received msg ~w ~w ~n", [Id,N,Msg]),
      Master ! Msg,
      slave(Id, Master, Leader, N+1, {msg, N, Msg}, Slaves, Group);
    {view, N, [Leader|Slaves2], Group2} ->
      io:format("gms slave ~w: received view ~w ~w~n", [Id, N, [Leader|Slaves2]]),
      Master ! {view, Group2},
      slave(Id, Master, Leader, N+1, {view, [Leader|Slaves2], N, Group2}, Slaves2, Group2);
    {msg, I, _} when I < N ->
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {'DOWN', _Ref, process, Leader, _Reason} ->
      io:format("gms slave ~w: leader, went down ~w~n", [Id, Leader]),
      election(Id, Master, N, Last, Slaves, Group);
    stop ->
      ok;
    Error ->
      io:format("gms slave ~w: strange message ~w~n", [Id, Error])
  end.

election(Id, Master, N, Last, Slaves, [_|Group]) ->
  Self = self(),
  case Slaves of
    [Self|Rest] ->
      bcast(Id, Last, Rest),
      bcast(Id, {view, N, Slaves, Group}, Rest),
      Master ! {view, Group},
      io:format("gms election ~w: Electing self to leader~n", [Id]),
      leader(Id, Master, Rest, N+1, Group);
    [Leader|Rest] ->
      erlang:monitor(process, Leader),
      io:format("gms election ~w: Electing ~w: to leader~n", [Id, Leader]),
      slave(Id, Master, Leader, N, Last, Rest, Group)
  end.

bcast(Id, Msg, Nodes) ->
  io:format("gms leader ~w : Broadcasting msg ~w to ~w~n",[Id,Msg,Nodes]),
  lists:foreach(fun(Node) -> Node ! Msg, crash(Id) end, Nodes).

crash(Id) ->
  case random:uniform(?arghh) of
    ?arghh ->
      io:format("leader ~w: crash~n",
        [Id]),
      exit(no_luck);
    _ ->
      ok
  end.
