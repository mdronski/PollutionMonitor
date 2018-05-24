%%%-------------------------------------------------------------------
%%% @author mdronski
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. May 2018 21:24
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-author("mdronski").

-behaviour(gen_server).

%% API
-export([start_link/0, addStation/2, addValue/4, removeValue/3, getValue/3,
  getStationMean/2, getDailyMean/3, exportToCsv/1, getMonitor/0, crash/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {monitor}).
-record(monitor, {stations_map = #{}}).

%%%===================================================================
%%% API
%%%===================================================================

addStation(Name, Cords) ->
  gen_server:cast(?SERVER, {addStation, Name, Cords}).

addValue(Station, Date, Type, Value) ->
  gen_server:cast(?SERVER, {addValue, Station, Date, Type, Value}).

removeValue(Station, Date, Type) ->
  gen_server:cast(?SERVER, {removeValue, Station, Date, Type}).

getValue(Station, Date, Type) ->
  gen_server:call(?SERVER, {getValue, Station, Date, Type}).

getStationMean(Station, Type) ->
  gen_server:call(?SERVER, {getStationMean, Station, Type}).

getDailyMean(Station, Date, Type) ->
  gen_server:call(?SERVER, {getDailyMean, Station, Date, Type}).

exportToCsv(FileName) ->
  gen_server:cast(?SERVER, {csv, FileName}).

getMonitor() ->
  gen_server:call(?SERVER, {getMonitor}).

crash() ->
  gen_server:cast(?SERVER, {crash}).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).

init([]) ->
  {ok, #state{monitor = pollution:createMonitor()}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_call({getValue, Station, Date, Type}, _From, State) ->
  case pollution:getOneValue(Station, Date, Type, State#state.monitor) of
    Error when is_atom(Error) -> {reply, error, State};
    Value -> {reply, Value, State}
  end;

handle_call({getStationMean, Station, Type}, _From, State) ->
  case pollution:getStationMean(Station, Type, State#state.monitor) of
    Error when is_atom(Error) -> {reply, error, State};
    Value -> {reply, Value, State}
  end;

handle_call({getDailyMean, Station, Date, Type}, _From, State) ->
    case pollution:getDailyMean(Station, Date, Type, State#state.monitor) of
      Error when is_atom(Error) -> {reply, error, State};
      Value -> {reply, Value, State}
    end;


handle_call({getMonitor}, _From, State) ->
  {reply, State#state.monitor, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_cast({addStation, Name, Cords}, State) ->
  case pollution:addStation(Name, Cords, State#state.monitor) of
    #monitor{} = NewMonitor -> {noreply, State#state{monitor = NewMonitor}};
    _ -> {noreply, State}
  end;

handle_cast({addValue, Station, Date, Type, Value}, State) ->
    case pollution:addValue(Station, Date, Type, Value, State#state.monitor) of
      #monitor{} = NewMonitor -> {noreply, State#state{monitor = NewMonitor}};
      _ -> {noreply, State}
    end;


handle_cast({removeValue, Station, Date, Type}, State) ->
  case pollution:removeValue(Station, Date, Type, State#state.monitor) of
    #monitor{} = NewMonitor -> {noreply, State#state{monitor = NewMonitor}};
    _ -> {noreply, State}
  end;

handle_cast({csv, FileName}, State) ->
  pollution:exportToCsv(FileName, State#state.monitor),
  {noreply, State};

handle_cast({crash}, State) ->
  X = 1/0,
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
