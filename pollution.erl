%%%-------------------------------------------------------------------
%%% @author mdronski
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Apr 2018 19:58
%%%-------------------------------------------------------------------
-module(pollution).
-author("mdronski").

%% API
-export([createMonitor/0, addStation/3, getMap/1, getFullInfo/2, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/4]).

-record(station_info, {geo_cord, name}).
-record(measurements, {type, value, date}).
-record(monitor, {stations_map = #{}}).

createMonitor() -> #monitor{}.

addStation(Monitor, Name, Coord) ->
  Monitor#monitor{stations_map = maps:put(#station_info{geo_cord = Coord, name = Name}, #{},
    Monitor#monitor.stations_map)}.

getFullInfo(Info, Monitor) ->
  Keys = maps:keys(Monitor#monitor.stations_map),
  case Info of
    {X, Y} ->
        case lists:keyfind({X, Y}, 2, Keys) of
          false -> error;
          {_, _, Name} -> {station_info, {X, Y}, Name}
        end;
    Name ->
      case lists:keyfind(Name, 3, Keys) of
        false -> error;
        {_, {X, Y}, _} -> {station_info, {X, Y}, Name}
      end
  end.

addValue(Monitor, Station, Date, Type, Value) ->
  case getFullInfo(Station, Monitor) of
    error -> stationNotExistsError;
    FullName ->
      NewMap = maps:put({Type, Date}, Value, maps:get(FullName, Monitor#monitor.stations_map)),
      Monitor#monitor {stations_map = maps:update(FullName, NewMap, Monitor#monitor.stations_map)}
  end.

removeValue(Monitor, Station, Date, Type) ->
  case getFullInfo(Station, Monitor) of
    error -> stationNotExistsError;
    FullName ->
      NewMap = maps:remove({Type, Date}, maps:get(FullName, Monitor#monitor.stations_map)),
      Monitor#monitor {stations_map = maps:update(FullName, NewMap, Monitor#monitor.stations_map)}
  end.

getOneValue(Monitor, Station, Date, Type) ->
  case getFullInfo(Station, Monitor) of
    error -> stationNotExistsError;
    FullName ->
      maps:get({Type, Date}, maps:get(FullName, Monitor#monitor.stations_map))
  end.

getStationMean(Monitor, Station, Type) ->
  case getFullInfo(Station, Monitor) of
    error -> stationNotExistsError;
    FullName ->
      Map = maps:get(FullName, Monitor#monitor.stations_map),
      FilteredMap = maps:filter(fun ({T, _}, _) -> T == Type end, Map),
      {Sum, Count} = maps:fold(fun (_, V, {A, B}) -> {A+V, B+1} end, {0, 0}, FilteredMap),
      case Count of
        0 -> 0;
        C -> Sum / C
      end
  end.

getDailyMean(Monitor, Station, Date, Type) ->
  case getFullInfo(Station, Monitor) of
    error -> stationNotExistsError;
    FullName ->
      Map = maps:get(FullName, Monitor#monitor.stations_map),
      FilteredMap = maps:filter(fun ({T, {D, _}}, _) -> (T == Type andalso D == Date) end, Map),
      {Sum, Count} = maps:fold(fun (_, V, {A, B}) -> {A+V, B+1} end, {0, 0}, FilteredMap),
      case Count of
        0 -> 0;
        C -> Sum / C
      end
  end.

getMap(M) -> M#monitor.stations_map.




