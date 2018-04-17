%%%-------------------------------------------------------------------
%%% @author mdronski
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Apr 2018 15:25
%%%-------------------------------------------------------------------
-module(pollution_test).
-author("mdronski").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-record(station_info, {geo_cord, name}).
-record(monitor, {stations_map = #{}}).


createMonitor_test() ->
  ?assertEqual(#monitor{stations_map = #{}}, pollution:createMonitor()).

addStation_test() ->

  P = pollution:addStation("Aleja Słowackiego", {50.2345,18.3445},  pollution:createMonitor()),
  ?assertEqual({monitor,#{{station_info,{50.2345,18.3445}, "Aleja Słowackiego"} => #{}}}, P),

  P2 = pollution:addStation("Aleja Słowackiego", {50.2345,18.3445},  P),
  ?assertEqual(stationAlreadyExists, P2).

checkStationExists_test() ->
  P = pollution:addStation("Aleja Słowackiego", {50.2345,18.3445},  pollution:createMonitor()),
  P2 = pollution:addStation("Aleja Mickiewicza", {12, 22},  P),
  P3 = pollution:addStation("Czarnowiejska", {99, 45.78},  P2),

  ?assertEqual(true, pollution:checkStationExists("Czarnowiejska", {99, 999},  P3)),
  ?assertEqual(true, pollution:checkStationExists("To_nie_Czarnowiejska_prank", {99, 45.78},  P3)),
  ?assertEqual(false, pollution:checkStationExists("Wonderland", {3.14, 3.14},  P3)).

getFullName_test() ->
  P = pollution:addStation("Aleja Słowackiego", {50.2345,18.3445},  pollution:createMonitor()),
  P2 = pollution:addStation("Aleja Mickiewicza", {12, 22},  P),
  P3 = pollution:addStation("Czarnowiejska", {99, 45.78},  P2),

  ?assertEqual({station_info, {12, 22}, "Aleja Mickiewicza"}, pollution:getFullName({12, 22}, P3)),
  ?assertEqual({station_info, {12, 22}, "Aleja Mickiewicza"}, pollution:getFullName("Aleja Mickiewicza", P3)),
  ?assertEqual(stationNotExistsError, pollution:getFullName("Wonderland", P3)).

addValue_test() ->
  P = pollution:addStation("Aleja Słowackiego", {50.2345,18.3445},  pollution:createMonitor()),
  Date = calendar:local_time(),

  P2 = pollution:addValue("Aleja Słowackiego", Date, "PM10", 999, P),
  ?assertEqual({monitor,#{{station_info,{50.2345,18.3445}, "Aleja Słowackiego"} => #{{"PM10",Date} => 999}}}, P2),

  P3 = pollution:addValue("Aleja Słowackiego", Date, "PM10", 10, P2),
  ?assertEqual(measurementAlreadyExistsError, P3).

removeValue_test() ->
  Date = calendar:local_time(),
  P = pollution:addStation("Aleja Słowackiego", {50.2345,18.3445},  pollution:createMonitor()),
  P2 = pollution:addValue("Aleja Słowackiego", Date, "PM10", 999, P),
  P3 = pollution:addValue("Aleja Słowackiego", Date, "PM2,5", 250, P2),

  P4 = pollution:removeValue("Aleja Słowackiego", Date, "PM10", P3),
  ?assertEqual({monitor,#{{station_info,{50.2345,18.3445}, "Aleja Słowackiego"} =>
  #{{"PM2,5", Date} => 250}}}, P4),

  P5 = pollution:removeValue("Aleja Słowackiego", Date, "foo", P3),
  ?assertEqual(valueNotExistsError, P5).

getOneValue_test() ->
  Date = calendar:local_time(),
  P = pollution:addStation("Aleja Słowackiego", {50.2345,18.3445},  pollution:createMonitor()),
  P2 = pollution:addValue("Aleja Słowackiego", Date, "PM10", 999, P),
  P3 = pollution:addValue("Aleja Słowackiego", Date, "PM2,5", 250, P2),

  P4 = pollution:getOneValue("Aleja Słowackiego", Date, "PM10", P3),
  ?assertEqual(P4, 999),

  P5 = pollution:getOneValue("Aleja Słowackiego", Date, "foo", P3),
  ?assertEqual(valueNotExistsError, P5).

getStationMean_test() ->
  Date = calendar:local_time(),
  P = pollution:addStation("Aleja Słowackiego", {50.2345,18.3445},  pollution:createMonitor()),
  P2 = pollution:addValue("Aleja Słowackiego", Date, "PM10", 1000, P),
  P3 = pollution:addValue("Aleja Słowackiego", {{1,2,3}, {4,5,6}}, "PM10", 500, P2),

  P4 = pollution:getStationMean("Aleja Słowackiego", "PM10", P3),
  ?assertEqual(750.0, P4).

getDailyMean_test() ->
  P = pollution:addStation("Aleja Słowackiego", {50.2345,18.3445},  pollution:createMonitor()),
  P2 = pollution:addValue("Aleja Słowackiego", {{2018,04,3}, {4,5,6}}, "PM10", 1000, P),
  P3 = pollution:addValue("Aleja Słowackiego", {{2018,04,3}, {14,15,6}}, "PM10", 500, P2),
  P4 = pollution:addValue("Aleja Słowackiego", {{2018,04,3}, {14,5,7}}, "PM10", 1000, P3),
  P5 = pollution:addValue("Aleja Słowackiego", {{2018,04,3}, {18,22,30}}, "PM10", 500, P4),
  P6 = pollution:addValue("Aleja Słowackiego", {{2018,04,14}, {18,22,30}}, "PM10", 9999999, P5),

  P7 = pollution:getDailyMean("Aleja Słowackiego", {2018,04,3}, "PM10", P6),
  ?assertEqual(750.0, P7).







