%%%-------------------------------------------------------------------
%%% @author reeth
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Sep 2022 11:49
%%%-------------------------------------------------------------------
-module(routy).
-author("reeth").

%% API
-export([start/2,stop/1,get_status/1]).

start(Reg, Name) ->
  register(Reg, spawn(fun() -> init(Name) end)).

stop(Node) ->
  Node ! stop,
  unregister(Node).

init(Name) ->
  Intf = intf:new(),
  Map = map:new(),
  Table = dijkstra:table(Intf, Map),
  Hist = hist:new(Name),
  router(Name, 0, Hist, Intf, Table, Map).

router(Name, N, Hist, Intf, Table, Map) ->
  receive

    {add, Node, Pid} ->
      io:format("~w Received add signal Node:~w~n  Pid:~w~n", [Name, Node, Pid]),
      Ref = erlang:monitor(process, Pid),
      Intf1 = intf:add(Node, Ref, Pid, Intf),
      router(Name, N, Hist, Intf1, Table, Map);

    {remove, Node} ->
      {ok, Ref} = intf:ref(Node, Intf),
      erlang:demonitor(Ref),
      Intf1 = intf:remove(Node, Intf),
      router(Name, N, Hist, Intf1, Table, Map);

    {'DOWN', Ref, process, _, _} ->
      {ok, Down} = intf:name(Ref, Intf),
      io:format("~w exit received from ~w~n", [Name, Down]),
      Intf1 = intf:remove(Down, Intf),
      router(Name, N, Hist, Intf1, Table, Map);

    {status, From} ->
      From ! {status, {Name, N, Hist, Intf, Table, Map}},
      router(Name, N, Hist, Intf, Table, Map);

    {links, Node, R, Links} ->
      case hist:update(Node, R, Hist) of
        {new, Hist1} ->
          intf:broadcast({links, Node, R, Links}, Intf),
          Map1 = map:update(Node, Links, Map),
          router(Name, N, Hist1, Intf, Table, Map1);
        old ->
          router(Name, N, Hist, Intf, Table, Map)
      end;

    update ->
      Table1 = dijkstra:table(intf:list(Intf), Map),
      router(Name, N, Hist, Intf, Table1, Map);

    broadcast ->
      Message = {links, Name, N, intf:list(Intf)},
      intf:broadcast(Message, Intf),
      router(Name, N+1, Hist, Intf, Table, Map);

    {route, Name, From, Message} ->
      io:format("~w received ~w message from ~w~n", [Name, Message,From]),
      router(Name, N, Hist, Intf, Table, Map);

    {route, To, From, Message} ->
      io:format("~w routing ~w message from ~w to ~w ~n", [Name,Message, From, To]),

      case dijkstra:route(To, Table) of
        {ok, Gw} ->
          case intf:lookup(Gw, Intf) of
            {ok, Pid} ->
              io:format("~w Found pid ~w ~n", [Name, Pid]),
              Pid ! {route, To, From, Message};
            notfound ->
              ok
          end;
        notfound ->
          ok
      end,
      router(Name, N, Hist, Intf, Table, Map);

    {send, To, Message} ->
      self() ! {route, To, Name, Message},
      router(Name, N, Hist, Intf, Table, Map);

    stat ->
      io:format("Intf ~n", []),
      even_print(Intf),
      io:format("Table ~n", []),
      even_print(Table),
      io:format("Map ~n", []),
      even_print(Map),
      router(Name, N, Hist, Intf, Table, Map);

    stop ->
      io:format("~w> Received stop signal.~n", [Name]),
      ok
  end.

even_print([])-> [];
even_print([H|T]) when H rem 2 /= 0 ->
  even_print(T);
even_print([H|T]) ->
  io:format("printing: ~p~n", [H]),
  [H|even_print(T)].

get_status(Pid) ->
	Pid ! {status, self()},
  receive
		{status, {Name, N, Hist, Intf, Table, Map}} ->
      io:format("~n Name:~w~n  ", [Name]),
      io:format("N:~w~n  ", [N]),
      io:format("Hist:~w~n  ", [Hist]),
      io:format("Intf:~w~n  ", [Intf]),
      io:format("Table:~w~n  ", [Table]),
      io:format("Map:~w~n  ", [Map])
  end.