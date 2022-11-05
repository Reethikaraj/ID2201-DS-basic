%%%-------------------------------------------------------------------
%%% @author reeth
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Sep 2022 10:30
%%%-------------------------------------------------------------------
-module(dijkstra).
-author("reeth").

%% API
-export([table/2, route/2, update/4, replace/4, iterate/3, table/2]).

% Returns the length of the shortest path to the Node or 0 if the node is not found.
entry(Node, Sorted) ->
  case lists:keyfind(Node, 1, Sorted) of
    false -> 0;
    {_, Length, _} -> Length
  end.

% Replaces the entry for Node in Sorted with a new entry having a new length N and Gateway. The resulting list be sorted.
replace(Node, N, Gateway, Sorted) ->
  AuxList = lists:keydelete(Node,N, Sorted),
  lists:keysort(2, [{Node, N, Gateway} | AuxList]).

% Update the list Sorted given the information that Node can be reached in N hops using Gateway.
% If no entry is found then no new entry is added. Only if we have a better (shorter) path should we replace the existing entry.
update(Node, N, Gateway, Sorted) ->
  Length = entry(Node, Sorted),
  if
    N < Length ->
      replace(Node, N, Gateway, Sorted);
    true ->
      Sorted
  end.


% Construct a table given a sorted list of nodes, a map and a table constructed so far.
iterate(Sorted, Map, Table) ->
  case Sorted of
    [] ->
      Table;
    [{_, inf, _} | _] ->
      Table;
    [Entry | Tail] ->
      {Node, Length, Gateway} = Entry,
      case lists:keyfind(Node, 1, Map) of
        {_, Reachables} ->
          NewList = lists:foldl(
            fun(Element, Sorted) -> update(Element, Length + 1, Gateway, Sorted) end,
            Tail, Reachables);
        false ->
          NewList = Tail
      end,
      iterate(NewList, Map, [{Node, Gateway} | Table])
  end.


% Construct a routing table given the gateways and a map.
table(Gateways, Map) ->
  Nodes = map:all_nodes(Map),
  MockedNodes = lists:map(fun(Node) ->
    case lists:member(Node, Gateways) of
      true ->
        {Node, 0, Node};
      false ->
        {Node, inf, unknown}
    end
                          end, Nodes),
  InitialSortedList = lists:keysort(2, MockedNodes),
  iterate(InitialSortedList, Map, []).


% Search the routing table and return the gateway suitable to route messages to a node.
% If a gateway is found we should return {ok, Gateway} otherwise we return notfound.
route(Node, Table) ->
  case lists:keyfind(Node, 1, Table) of
    {Dest, Gateway} ->
      {ok, Gateway};
    false ->
      notfound
  end.
