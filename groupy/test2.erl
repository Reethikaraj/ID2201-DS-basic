%%%-------------------------------------------------------------------
%%% @author reeth
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Oct 2022 15:10
%%%-------------------------------------------------------------------
-module(test2).
-author("reeth").

%% API
-export([run/0,run1/0]).

run() ->
  Worker = worker:start(1, gms2, 1, 5000),
  timer:sleep(2000),
  worker:start(2, gms2, 2, Worker, 5000),
  timer:sleep(2000),
  worker:start(3, gms2, 3, Worker, 5000),
  timer:sleep(2000),
  worker:start(4, gms2, 4, Worker, 5000),
  timer:sleep(2000),
  worker:start(5, gms2, 5, Worker, 5000),
  timer:sleep(2000),
  worker:start(6, gms2, 6, Worker, 5000),
  timer:sleep(2000),
  worker:start(7, gms2, 7, Worker, 5000).

run1() ->
  Worker = worker:start(1, gms3, 1, 5000),
  timer:sleep(2000),
  worker:start(2, gms3, 2, Worker, 5000),
  timer:sleep(2000),
  worker:start(3, gms3, 3, Worker, 5000),
  timer:sleep(2000),
  worker:start(4, gms3, 4, Worker, 5000),
  timer:sleep(2000),
  worker:start(5, gms3, 5, Worker, 5000),
  timer:sleep(2000),
  worker:start(6, gms3, 6, Worker, 5000),
  timer:sleep(2000),
  worker:start(7, gms3, 7, Worker, 5000).
