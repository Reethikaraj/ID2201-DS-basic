%%%-------------------------------------------------------------------
%%% @author reeth
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Sep 2022 20:30
%%%-------------------------------------------------------------------
-module(time).
-author("reeth").

%% API
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).

zero() ->
  0.

inc(Name, T) ->
  T + 1.

merge(Ti, Tj) ->
  if Ti < Tj ->
    Tj;
    true ->
      Ti
  end.

leq(Ti, Tj) ->
  if Ti > Tj ->
    false;
    true ->
      true
  end.

clock([]) ->
  [];
clock([H|T]) ->
  [{H,0}|clock(T)].

update(Node, Time, Clock) ->
  List = lists:keyreplace(Node, 1, Clock, {Node, Time}),
  List.

safe(_, []) ->
  true;
safe(Time, [{_, MsgTime}|T]) ->
  if
    Time =< MsgTime -> safe(Time, T);
    true -> false
  end.


