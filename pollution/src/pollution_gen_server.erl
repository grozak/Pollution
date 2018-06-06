%%%-------------------------------------------------------------------
%%% @author Grzegorz
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. kwi 2018 12:10
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-author("Grzegorz").

-behaviour(gen_server).
%% API
-export([start/0, stop/0, crash/0, terminate/2]).
-export([start_link/0, init/1, addStation/2, addValue/4, getOneValue/3, handle_call/3, handle_cast/2]).

init(InitValue) ->
  {ok, pollution:createMonitor()}.

start_link() ->
  gen_server:start_link({local,server}, ?MODULE, 0, []).


start() ->
  start_link().

stop() ->
  gen_server:cast(server, stop).

crash() ->
  gen_server:cast(server, crash).

addStation(Name, Coord) ->
  gen_server:call(server, {addStation, Name, Coord}).

addValue(Name, Date, Type, Value) ->
  gen_server:call(server, {addValue, Name, Date, Type, Value}).

getOneValue(Name, Date, Type) ->
  gen_server:call(server, {getOneValue, Name, Date, Type}).


handle_call({addStation, Name, Coords}, From, Monitor) ->
  New=pollution:addStation(Monitor, Name, Coords),
  {reply, New, New};
handle_call({addValue, Name, Date, Type, Value}, From, Monitor) ->
  New=pollution:addValue(Monitor, Name, Date, Type, Value),
  {reply, New, New};
handle_call({getOneValue, Name, Date, Type}, From, Monitor) ->
  New=pollution:getOneValue(Monitor, Name, Date, Type),
  {reply, New, New}.


handle_cast(stop, Monitor) ->
  {stop, normal, Monitor};
handle_cast(crash, Monitor) ->
  1/0,
  {noreply, Monitor}.

terminate(Reason, State) ->
  ok.
