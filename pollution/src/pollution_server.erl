%%%-------------------------------------------------------------------
%%% @author Grzegorz
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. kwi 2018 17:33
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("Grzegorz").


%% API
-export([start/0, stop/0, addStation/2, addValue/4, removeValue/3,
  getOneValue/3, getDailyMean/2, getStationMean/2, init/0, getMonitor/0, getMinimumPollutionStation/1, crash/0]).

start() ->
  register (pollutionServer, spawn_link(pollution_server, init, [])),
  'Server started'.

stop() ->
  pollutionServer ! {request, self(), stop},
  'Server stoped'.

init() ->
  loop(pollution:createMonitor()).

%server loop
loop(Monitor) ->
  receive
    {crash, Pid, _} ->
      Pid ! {reply, "crash"},
      1/0;

    {getMonitor, Pid, _} ->
      Pid ! {reply, Monitor},
      loop(Monitor);

    {addStation, Pid, {Name, {X, Y}}} ->
      UpdatedMonitor=pollution:addStation(Monitor, Name, {X,Y}),
      Pid ! {reply, ok},
      loop(UpdatedMonitor);

    {addValue, Pid, {Name, Date, Type, Value}} ->
      UpdatedMonitor=pollution:addValue(Monitor, Name, Date, Type, Value),
      Pid ! {reply, ok},
      loop(UpdatedMonitor);

    {removeValue, Pid, {Name, Date, Type}}->
      UpdatedMonitor=pollution:removeValue(Monitor, Name, Date, Type),
      Pid ! {reply, ok},
      loop(UpdatedMonitor);

    {getOneValue, Pid, {Name, Date, Type}}->
      Result=pollution:getOneValue(Monitor, Name, Date, Type),
      Pid ! {reply, Result},
      loop(Monitor);

    {getStationMean, Pid, {Name, Type}}->
      Result=pollution:getStationMean(Monitor, Name, Type),
      Pid ! {reply, Result},
      loop(Monitor);

    {getDailyMean, Pid, {{Date, Time}, Type}} ->
      Result=pollution:getDailyMean(Monitor, {Date, Time}, Type),
      Pid ! {reply, Result},
      loop(Monitor);

    {getMinimumPollutionStation,Pid, {Type}} ->
      Reply=pollution:getMinimumPollutionStation(Monitor, Type),
      Pid ! {reply, Reply},
      loop(Monitor)
  end.

%client
call(Request, Args) ->
  pollutionServer ! {Request, self(), Args},
  receive
    {reply, Reply} -> Reply
  end.

crash() ->
  call(crash, {}).

addStation(Name, {X, Y}) ->
  call(addStation, {Name, {X,Y}}).

addValue(Name, Date, Type, Value) ->
  call(addValue, {Name, Date, Type, Value}).

removeValue(Name, Date, Type)->
  call(removeValue, {Name, Date, Type}).

getOneValue(Name, Date, Type) ->
  call(getOneValue, {Name, Date, Type}).

getStationMean(Name, Type) ->
  call(getOneValue, {Name, Type}).

getDailyMean(Date, Type)->
  call(getOneValue, {{Date,0}, Type}).

getMinimumPollutionStation(Type) ->
  call(getMinimumPollutionStation, {Type}).

getMonitor() ->
  call(getMonitor, {}).

