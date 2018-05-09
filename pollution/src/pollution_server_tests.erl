%%%-------------------------------------------------------------------
%%% @author Grzegorz
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. kwi 2018 10:19
%%%-------------------------------------------------------------------
-module(pollution_server_tests).
-author("Grzegorz").

-include_lib("eunit/include/eunit.hrl").

%% API
-export([addAndRemove_test/0, getMinimumPollutionStation_test/0]).

start_test() ->
  ?assertEqual('Server started', pollution_server:start()).

addAndRemove_test() ->
  pollution_server:addStation("Okocim", {10, 20}),
  pollution_server:addValue("Okocim", {{2017, 01, 13}, {10, 21, 30}}, "typ1", 105),
  pollution_server:addValue("Okocim", {{2017, 01, 14}, {10, 20, 30}}, "typ1", 105),
  pollution_server:addValue("Okocim", {{2017, 01, 15}, {10, 20, 30}}, "typ2", 10),
  pollution_server:addStation("Jadowniki", {70, 80}),
  pollution_server:addValue("Jadowniki", {{2017, 01, 13}, {10, 20, 30}}, "typ1", 35),
  pollution_server:addValue("Jadowniki", {{2017, 01, 13}, {10, 20, 30}}, "typ5", 35),
  pollution_server:removeValue("Okocim", {{2017, 01, 17}, {10, 20, 30}}, "typ1"),
  pollution_server:addValue("Okocim", {{2017, 01, 13}, {10, 20, 30}}, "typ2", 17),
  pollution_server:addValue("Okocim", {{2017, 01, 14}, {10, 20, 30}}, "typ2", 1),
  pollution_server:addValue("Okocim", {{2017, 01, 13}, {10, 20, 30}}, "typ5", 3),
  pollution_server:addStation("Krakow", {100, 350}),
  pollution_server:removeValue("Okocim", {{2017, 01, 13}, {10, 20, 30}}, "typ2"),
  ?assertEqual([{station, "Krakow", {100, 350}, []},
    {station, "Jadowniki",
      {70, 80},
      [{measurement, {{2017, 1, 13}, {10, 20, 30}}, "typ1", 35},
        {measurement, {{2017, 1, 13}, {10, 20, 30}}, "typ5", 35}]},
    {station, "Okocim",
      {10, 20},
      [{measurement, {{2017, 1, 13}, {10, 21, 30}}, "typ1", 105},
        {measurement, {{2017, 1, 14}, {10, 20, 30}}, "typ1", 105},
        {measurement, {{2017, 1, 15}, {10, 20, 30}}, "typ2", 10},
        {measurement, {{2017, 1, 14}, {10, 20, 30}}, "typ2", 1},
        {measurement, {{2017, 1, 13}, {10, 20, 30}}, "typ5", 3}]}], pollution_server:getMonitor()).

getMinimumPollutionStation_test() ->
  ?assertEqual(pollution_server:getMinimumPollutionStation("typ1"), {"Jadowniki", {70, 80}}),
  ?assertEqual(pollution_server:getMinimumPollutionStation("typ2"), {"Okocim", {10, 20}}),
  ?assertEqual(pollution_server:getMinimumPollutionStation("typ3"), {"NoStation", {"-", "-"}}).

stop_test() ->
  ?assertEqual('Server stoped', pollution_server:stop()).