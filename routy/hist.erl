%%%-------------------------------------------------------------------
%%% @author reeth
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Sep 2022 11:45
%%%-------------------------------------------------------------------
-module(hist).
-author("reeth").

%% API
-export([new/1, update/3]).

% Return a new history, where messages from Name will always be seen as old.
new(Name) ->
  Dict = dict:new(),
  dict:append(Name, inf, Dict).


% Check if message number N from the Node is old or new.
% If it is old then return old but if it new return {new, Updated}
% where Updated is the updated history.
update(Node, N, History) ->
  case dict:find(Node, History) of
    {ok, [Value | _ ]} ->
      if
        N > Value ->
          AuxDict = dict:erase(Node, History),
          Updated = dict:append(Node, N, AuxDict),
          {new, Updated};
        true ->
          old
      end;
    error ->
      {new, dict:append(Node, N, History)}
  end.
