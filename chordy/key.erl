%%%-------------------------------------------------------------------
%%% @author reeth
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Oct 2022 14:07
%%%-------------------------------------------------------------------
-module(key).
-author("reeth").

%% API
-compile(export_all).

% generate a random number for the key
generate() ->
  random:uniform(1000000000).

% check if Key is in between From and To or equal to To
% denoted ]From, To]
between(Key, From, To) ->
  if
  % key is in between From and To
    (From < To) and (Key > From) and (Key =< To) ->
      true;
  % From can be larger than To in a ring structure
    (From > To) and ((Key > From) or (Key =< To)) ->
      true;
  %From and To are the same node
    From == To ->
      true;
    true ->
      false
  end.
