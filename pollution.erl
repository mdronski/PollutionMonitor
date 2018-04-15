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
-export([createMonitor/0, addStation/3, getMap/1, getFullInfo/2, addValue/5, removeValue/4, getOneValue/4]).

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



getMap(M) -> M#monitor.stations_map.

%%findStationMeasurements(Monitor, Info) ->
%%  case Info of
%%    {X, Y} -> maps:get(#station_info{{X, Y}, }, Monitor#monitor.stations_map);
%%  end

%%addMeasurement(Monitor, Info, Date, Type, Value) ->
%%  L = Monitor#monitor
%%  Monitor#monitor{stations_map = }
%%




