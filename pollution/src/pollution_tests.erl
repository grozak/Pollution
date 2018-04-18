%%%-------------------------------------------------------------------
%%% @author Grzegorz
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. kwi 2018 10:13
%%%-------------------------------------------------------------------
-module(pollution_tests).
-author("Grzegorz").

-include_lib("eunit/include/eunit.hrl").

%% API
-export([runAll_test/0, addAndRemove_test/0, exMonitor/0, getMinimumPollutionStation_test/0, getDailyMean_test/0]).

runAll_test() ->
  addAndRemove_test(),
  getMinimumPollutionStation_test(),
  getDailyMean_test().

exMonitor() ->
  M = pollution:createMonitor(),
  M2 = pollution:addStation(M, "Okocim", {10, 20}),
  M3 = pollution:addValue(M2, "Okocim", {{2017, 01, 13}, {10, 21, 30}}, "typ1", 105),
  M4 = pollution:addValue(M3, "Okocim", {{2017, 01, 14}, {10, 20, 30}}, "typ1", 105),
  M5 = pollution:addValue(M4, "Okocim", {{2017, 01, 15}, {10, 20, 30}}, "typ2", 10),
  M6 = pollution:addStation(M5, "Jadowniki", {70, 80}),
  M7 = pollution:addValue(M6, "Jadowniki", {{2017, 01, 13}, {10, 20, 30}}, "typ1", 35),
  M8 = pollution:addValue(M7, "Jadowniki", {{2017, 01, 13}, {10, 20, 30}}, "typ5", 35),
  M9 = pollution:removeValue(M8, "Okocim", {{2017, 01, 17}, {10, 20, 30}}, "typ1"),
  M10 = pollution:addValue(M9, "Okocim", {{2017, 01, 13}, {10, 20, 30}}, "typ2", 17),
  M11 = pollution:addValue(M10, "Okocim", {{2017, 01, 14}, {10, 20, 30}}, "typ2", 1),
  M12 = pollution:addValue(M11, "Okocim", {{2017, 01, 13}, {10, 20, 30}}, "typ5", 3),
  M13 = pollution:addStation(M12, "Krakow", {100, 350}),
  pollution:removeValue(M13, "Okocim", {{2017, 01, 13}, {10, 20, 30}}, "typ2").

addAndRemove_test() ->
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
        {measurement, {{2017, 1, 13}, {10, 20, 30}}, "typ5", 3}]}], exMonitor()).

getMinimumPollutionStation_test() ->
  ?assertEqual(pollution:getMinimumPollutionStation(exMonitor(), "typ1"), {"Jadowniki", {70, 80}}),
  ?assertEqual(pollution:getMinimumPollutionStation(exMonitor(), "typ2"), {"Okocim", {10, 20}}),
  ?assertEqual(pollution:getMinimumPollutionStation(exMonitor(), "typ3"), {"NoStation", {"-", "-"}}).

getDailyMean_test() ->
  ?assertEqual(pollution:getDailyMean(exMonitor(), {{2017, 1, 13}, 0}, "typ1"), 70.0).