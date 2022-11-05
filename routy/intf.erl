%%%-------------------------------------------------------------------
%%% @author reeth
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Sep 2022 11:43
%%%-------------------------------------------------------------------
-module(intf).
-author("reeth").

%% API
-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).

% Return an empty set of interfaces
new() ->
  [].

% Add a new entry to the set and return the new set of interfaces.
add(Name, Ref, Pid, Intf) ->
  case lists:member({Name, Ref, Pid}, Intf) of
    true ->
      Intf;
    false ->
      [{Name, Ref, Pid} | Intf]
  end.

% Remove an entry given a name of an interface,return a new set of interfaces.
remove(Name, Intf) ->
  lists:keydelete(Name, 1, Intf).

% Find the process identifier given a name, return {ok, Pid} if found otherwise notfound.
lookup(Name, Intf) ->
  case lists:keyfind(Name, 1, Intf) of
    {Name, _, Pid} ->
      {ok, Pid};
    false ->
      notfound
  end.

% Find the reference given a name and return {ok,Ref} or notfound.
ref(Name, Intf) ->
  case lists:keyfind(Name, 1, Intf) of
    {Name, Ref, _} ->
      {ok, Ref};
    false ->
      notfound
  end.

% Find the name of an entry given a reference and return {ok, Name} or notfound.
name(Ref, Intf) ->
  case lists:keyfind(Ref, 2, Intf) of
    {Name, Ref, _} ->
      {ok, Name};
    false ->
      notfound
  end.


% Return a list with all names.
list(Intf) ->
  lists:map(fun({Name, Ref, Pid}) -> Name end, Intf).

% Send the message to all interface processes
broadcast(Message, Intf) ->
  lists:foreach(fun({_ ,_, Pid}) -> Pid ! Message end, Intf).
