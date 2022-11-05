%%%-------------------------------------------------------------------
%%% @author reeth
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Oct 2022 12:39
%%%-------------------------------------------------------------------
-module(storage).
-author("reeth").

%% API
-compile(export_all).

% create a new store
create() ->
  [].

% add a Key-Value pair to store
add(Key, Value, Store) ->
  [{Key, Value} | Store].

% see if a Key is in the Store
lookup(Key, Store) ->
  lists:keyfind(Key, 1, Store).

split(From, To, Store) ->
  lists:partition(fun({Key,Value})-> key:between(Key, From, To) end, Store).

% add a list of key-value pairs to a store
merge(Entries, Store) ->
  Store ++ Entries.
