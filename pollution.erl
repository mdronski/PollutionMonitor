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
-export([createMonitor/0, addStation/3, getFullName/2, addValue/5, removeValue/4, getOneValue/4,
  getStationMean/3, getDailyMean/4, checkStationExists/3, checkValueExists/4, writeStation/3, writeMeasurement/5,
  exportToCsv/2]).

-record(station_info, {geo_cord, name = ""}).
-record(monitor, {stations_map = #{}}).

createMonitor() -> #monitor{}.

addStation(Name, Cords, Monitor) ->
  case checkStationExists(Name, Cords, Monitor) of
    true -> stationAlreadyExists;
    false -> Monitor#monitor{stations_map = maps:put(#station_info{geo_cord = Cords, name = Name}, #{},
            Monitor#monitor.stations_map)}
end.

checkStationExists(Name, Cords, Monitor) ->
  NameCheck = getFullName(Name, Monitor),
  CordsTest = getFullName(Cords, Monitor),
  case {NameCheck, CordsTest} of
    {stationNotExistsError, stationNotExistsError} -> false;
    {_, _} -> true
  end.

getFullName(Info, Monitor) ->
  Keys = maps:keys(Monitor#monitor.stations_map),
  case Info of
    {X, Y} ->
        case lists:keyfind({X, Y}, 2, Keys) of
          false -> stationNotExistsError;
          {_, _, Name} -> {station_info, {X, Y}, Name}
        end;
    Name ->
      case lists:keyfind(Name, 3, Keys) of
        false -> stationNotExistsError;
        {_, {X, Y}, _} -> {station_info, {X, Y}, Name}
      end
  end.

checkValueExists(FullName, Date, Type, Monitor) ->
   maps:is_key({Type, Date}, maps:get(FullName, Monitor#monitor.stations_map)).

addValue(Station, Date, Type, Value, Monitor) ->
  case getFullName(Station, Monitor) of
    stationNotExistsError -> stationNotExistsError;
    FullName ->
      case checkValueExists(FullName, Date, Type, Monitor) of
        true -> measurementAlreadyExistsError;
        false ->
          NewMap = maps:put({Type, Date}, Value, maps:get(FullName, Monitor#monitor.stations_map)),
          Monitor#monitor {stations_map = maps:update(FullName, NewMap, Monitor#monitor.stations_map)}
      end
  end.

removeValue(Station, Date, Type, Monitor ) ->
  case getFullName(Station, Monitor) of
    stationNotExistsError -> stationNotExistsError;
    FullName ->
      case checkValueExists(FullName, Date, Type, Monitor) of
      true -> NewMap = maps:remove({Type, Date}, maps:get(FullName, Monitor#monitor.stations_map)),
        Monitor#monitor {stations_map = maps:update(FullName, NewMap, Monitor#monitor.stations_map)};
      false -> valueNotExistsError
      end
  end.

getOneValue(Station, Date, Type, Monitor) ->
  case getFullName(Station, Monitor) of
    stationNotExistsError -> stationNotExistsError;
    FullName ->
      case checkValueExists(FullName, Date, Type, Monitor) of
        true -> maps:get({Type, Date}, maps:get(FullName, Monitor#monitor.stations_map));
        false ->  valueNotExistsError
      end
  end.

getStationMean(Station, Type, Monitor) ->
  case getFullName(Station, Monitor) of
    stationNotExistsError -> stationNotExistsError;
    FullName ->
      Map = maps:get(FullName, Monitor#monitor.stations_map),
      FilteredMap = maps:filter(fun ({T, _}, _) -> T == Type end, Map),
      {Sum, Count} = maps:fold(fun (_, V, {A, B}) -> {A+V, B+1} end, {0, 0}, FilteredMap),
      case Count of
        0 -> 0;
        C -> Sum / C
      end
  end.

getDailyMean(Station, Date, Type, Monitor) ->
  case getFullName(Station, Monitor) of
    stationNotExistsError -> stationNotExistsError;
    FullName ->
      Map = maps:get(FullName, Monitor#monitor.stations_map),
      FilteredMap = maps:filter(fun ({T, {D, _}}, _) -> (T == Type andalso D == Date) end, Map),
      {Sum, Count} = maps:fold(fun (_, V, {A, B}) -> {A+V, B+1} end, {0, 0}, FilteredMap),
      case Count of
        0 -> 0;
        C -> Sum / C
      end
  end.

writeMeasurement(FileName, Type, Date1, Date2, Value) ->
  {D11, D12, D13} = Date1,
  {D21, D22, D23} = Date2,
  file:write_file(FileName, io_lib:fwrite(",~s", [Type]), [write,append]),
  file:write_file(FileName, io_lib:fwrite(",~w", [D11]), [write,append]),
  file:write_file(FileName, io_lib:fwrite(",~w", [D12]), [write,append]),
  file:write_file(FileName, io_lib:fwrite(",~w", [D13]), [write,append]),
  file:write_file(FileName, io_lib:fwrite(",~w", [D21]), [write,append]),
  file:write_file(FileName, io_lib:fwrite(",~w", [D22]), [write,append]),
  file:write_file(FileName, io_lib:fwrite(",~w", [D23]), [write,append]),
  file:write_file(FileName, io_lib:fwrite(",~w", [Value]), [write,append]).



writeStation(FileName, FullName, Monitor) ->
  {_, {X, Y}, Name} = FullName,
  file:write_file(FileName, io_lib:fwrite("~w,", [X]), [write,append]),
  file:write_file(FileName, io_lib:fwrite("~w", [Y]), [write,append]),
  file:write_file(FileName, io_lib:fwrite(",~s", [Name]), [write,append]),
  MeasurementsMap = maps:get(FullName, Monitor#monitor.stations_map),
  maps:fold(fun ({T, {D1, D2}}, V, _) ->  writeMeasurement(FileName, T, D1, D2, V) end, 0, MeasurementsMap),
  file:write_file(FileName, io_lib:fwrite("\n", []), [write,append]).

exportToCsv(FileName, Monitor) ->
  maps:fold(fun (FullName, _, _) -> writeStation(FileName, FullName, Monitor) end, 0, Monitor#monitor.stations_map).


%%getMap(M) -> M#monitor.stations_map.




