%%%-------------------------------------------------------------------
%%% @author reeth
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Sep 2022 20:30
%%%-------------------------------------------------------------------
-module(test).
-author("reeth").

%% API
-export([run/2]).
run(Sleep, Jitter) ->
Log = loggy:start([john, paul, ringo, george]),
A = worker:start(john, Log, 13, Sleep, Jitter),
B = worker:start(paul, Log, 23, Sleep, Jitter),
C = worker:start(ringo, Log, 36, Sleep, Jitter),
D = worker:start(george, Log, 49, Sleep, Jitter),
worker:peers(A, [B, C, D]),
worker:peers(B, [A, C, D]),
worker:peers(C, [A, B, D]),
worker:peers(D, [A, B, C]),
timer:sleep(1000),
loggy:stop(Log),
worker:stop(A),
worker:stop(B),
worker:stop(C),
worker:stop(D).
