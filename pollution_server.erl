-module(pollution_server).
-author("mdronski").

%% API
-export([start/0, stop/0,
        addStation/2, addValue/4, removeValue/3, getValue/3, getStationMean/2, getDailyMean/3, exportToCsv/1, getMonitor/0]).
-record(monitor, {stations_map = #{}}).

start() ->
  io:format("starting server~n"),
  register(pollution_server, spawn(fun init/0)),
  ok.

init() ->
  loop(pollution:createMonitor()).

stop() ->
  pollution_server ! {stop, self()},
  receive
    Msg -> io:format("~p~n", [Msg])
  end,
  ok.

loop(Monitor) ->
  io:format("Actual monitor: ~p~n", [Monitor]),
  receive
    {stop, Sender} ->
      Sender ! stopped,
      ok;
    {Request, Sender} ->
      {Status, UpdatedMonitor} = handleRequest(Request, Monitor),
      Sender ! Status,
      loop(UpdatedMonitor);
    _ ->
      io:format("Server is not able to handle this request!~n"),
      loop(Monitor)
  end.

addStation(Name, Cords) ->
  pollution_server ! {{addStation, Name, Cords}, self()},
  receive
    Msg -> io:format("~p~n", [Msg])
  end.

addValue(Station, Date, Type, Value) ->
  pollution_server ! {{addValue, Station, Date, Type, Value}, self()},
  receive
    Msg -> io:format("~p~n", [Msg])
  end.

removeValue(Station, Date, Type) ->
  pollution_server ! {{removeValue, Station, Date, Type}, self()},
  receive
    Msg -> io:format("~p~n", [Msg])
  end.

getValue(Station, Date, Type) ->
  pollution_server ! {{getValue, Station, Date, Type}, self()},
  receive
    Msg -> Msg
  end.

getStationMean(Station, Type) ->
  pollution_server ! {{getStationMean, Station, Type}, self()},
  receive
    Msg -> Msg
  end.

getDailyMean(Station, Date, Type) ->
  pollution_server ! {{getDailyMean, Station, Date, Type}, self()},
  receive
    Msg -> Msg
  end.

exportToCsv(FileName) ->
  pollution_server ! {{csv, FileName}, self()},
  receive
    Msg -> Msg
  end.

getMonitor() ->
  pollution_server ! {getMonitor, self()},
  receive
    #monitor{} = Monitor -> Monitor;
    _ -> error
  end.

handleRequest({addStation, Name, Cords}, Monitor) ->
  case pollution:addStation(Name, Cords, Monitor) of
    #monitor{} = NewMonitor -> {ok, NewMonitor};
    _ -> {error, Monitor}
  end;

handleRequest({addValue, Station, Date, Type, Value}, Monitor) ->
  case Date of
    {{_, _, _}, {_, _, _}} ->
    case pollution:addValue(Station, Date, Type, Value, Monitor) of
      #monitor{} = NewMonitor -> {ok, NewMonitor};
      _ -> {error, Monitor}
    end;
    _ -> {error, Monitor}
  end;

handleRequest({removeValue, Station, Date, Type}, Monitor) ->
  case pollution:removeValue(Station, Date, Type, Monitor) of
    #monitor{} = NewMonitor -> {ok, NewMonitor};
    _ -> {error, Monitor}
  end;

handleRequest({getValue, Station, Date, Type}, Monitor) ->
  case pollution:getOneValue(Station, Date, Type, Monitor) of
    Error when is_atom(Error) -> {error, Monitor};
    Value -> {Value, Monitor}
  end;

handleRequest({getStationMean, Station, Type}, Monitor) ->
  case pollution:getStationMean(Station, Type, Monitor) of
    Error when is_atom(Error) -> {error, Monitor};
    Value -> {Value, Monitor}
  end;

handleRequest({getDailyMean, Station, Date, Type}, Monitor) ->
  case Date of
    {_, _, _} ->
      case pollution:getDailyMean(Station, Date, Type, Monitor) of
        Error when is_atom(Error) -> {error, Monitor};
        Value -> {Value, Monitor}
      end;
    _ -> {error, Monitor}
  end;

handleRequest({csv, FileName}, Monitor) ->
  pollution:exportToCsv(FileName, Monitor),
  {ok, Monitor};

handleRequest(getMonitor, Monitor) ->
  {Monitor, Monitor};

handleRequest(_, Monitor) -> {error, Monitor}.



