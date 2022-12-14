%%%-------------------------------------------------------------------
%%% @author reeth
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Sep 2022 20:29
%%%-------------------------------------------------------------------
-module(worker).
-author("reeth").

%% API
-export([start/5, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter) ->
  spawn_link(fun()->init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
  Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter) ->
  random:seed(Seed, Seed, Seed),
  MsgClock = 0,
  receive
    {peers, Peers} ->
      loop(Name, Log, Peers, Sleep, Jitter, MsgClock);
    stop ->
      ok
  end.

peers(Wrk, Peers) ->
  Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter, MsgClock) ->
  Wait = random:uniform(Sleep),
  receive
    {msg, Time, Msg} ->
      TempValue = time:merge(Time, MsgClock),
      NewValue = time:inc(Name, TempValue),
      Log ! {log, Name, NewValue, {received, Msg}},
      loop(Name, Log, Peers, Sleep, Jitter, NewValue);
    stop ->
      ok;
    Error ->
      Log ! {log, Name, time, {error, Error}}
  after Wait ->
    Selected = select(Peers),
    Time = time:inc(Name, MsgClock),
    Message = {hello, random:uniform(100)},
    Selected ! {msg, Time, Message},
    jitter(Jitter),
    Log ! {log, Name, Time, {sending, Message}},
    loop(Name, Log, Peers, Sleep, Jitter, Time)
  end.

select(Peers) ->
  lists:nth(random:uniform(length(Peers)), Peers).

jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).