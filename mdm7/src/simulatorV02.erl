%%% Copyright 2011 Richard Alexander Green
%%% Description: Call event_manager module as intermediary with sink module.
%%% - This module can be a server.        One instance is needed.
%%% - event_mgr may also be a server. One instance is needed.
%%% - sink module is an event handler.    Many instances are generated via calls to event_mgr.

-module(simulatorV02). 

-behaviour(gen_server).


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-define( SYSTEM_MONITOR, {system, 0} ).

-define( NOTIMPLEMENTED, exit("function not implemented")  ).

-define( TEST, true ).
-define( DEBUG, true ).


% Set log level
% ALL=0 < TRACE=1 < DEBUG=2 < INFO=3 < WARN=4 < ERROR=5 < FATAL=6 < OFF=7
% Level semantics are defined in the org.apache.log4j.Level class.
-define( LOGLEVEL, 3 ).             % 3 means log info level and above. -- See log/2.

-include_lib("eunit/include/eunit.hrl").

-ifdef( DEBUG ).
-compile( export_all ).
-endif.


%% --------------------------------------------------------------------
%% External exports
-export([]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
% --------------------------------------------------------------------------------------------------------------------
% Spawn N circuits with M services each and transformer fanout of F.
spawn_circuits( N_circuits, M_services_per_circuit, TransfomerFanOut ) ->
		Circuit_I = N_circuits,
		SystemMonitorID = {system, 0},
		spawn_sink( SystemMonitorID, infinity, 'n/a', 'n/a' ),    % TODO: DOES THIS MAKE SENSE? ***
		spawn_circuit( Circuit_I, M_services_per_circuit, TransfomerFanOut, SystemMonitorID ),
		ok.

spawn_circuit( 0, _M_services_per_circuit, _TransfomerFanOut, _SystemMonitor ) ->
		done;
spawn_circuit( Circuit_I, M_services_per_circuit, TransfomerFanOut, SystemMonitor ) ->
	 % Spawn a sink process with this circuit ID.
	 ReportingThreshold = M_services_per_circuit * 1000 * 32,
	 CircuitID = { circuit, Circuit_I },
	 spawn_sink( CircuitID, ReportingThreshold, SystemMonitor, SystemMonitor ),
	 % TODO: Spawn a bunch of transformers.
	 spawn_transformers( TransfomerFanOut, M_services_per_circuit, CircuitID, SystemMonitor ),
   spawn_circuit( Circuit_I - 1, M_services_per_circuit, TransfomerFanOut, SystemMonitor ).
% --------------------------------------------------------------------------------------------------------------------
% Spawn transformers within a circuit context
spawn_transformers( TransfomerFanOut, M_services_per_circuit, CircuitID, SystemMonitor ) ->
		{ circuit, Circuit_I } = CircuitID,
		TransformerN = M_services_per_circuit div TransfomerFanOut,
		M_services_per_transformer = TransfomerFanOut,
		spawn_transformer( {Circuit_I,TransformerN}, M_services_per_transformer, CircuitID, SystemMonitor ),
		ok.

spawn_transformer( {_,0}, _M_service_per_transformer, CircuitID, SystemMonitor ) ->
		done;
spawn_transformer( {Circuit_I,TransformerN}, M_service_per_transformer, CircuitID, SystemMonitor ) ->
		% Spawn a sink process with this transformer ID.
		ReportingThreshold = M_service_per_transformer * 1000 * 32,
		TransformerID = {transformer,Circuit_I,TransformerN},
		spawn_sink( TransformerID, ReportingThreshold, CircuitID, SystemMonitor ),
		spawn_services( M_service_per_transformer, TransformerID, SystemMonitor ),
		spawn_transformer( {Circuit_I,TransformerN - 1}, M_service_per_transformer, CircuitID, SystemMonitor ).
% --------------------------------------------------------------------------------------------------------------------
% Spawn services within a transformer context
spawn_services( M_service_per_transformer, TransformerID, SystemMonitor ) ->
		{ transformer, Circuit_I, TransformerN } = TransformerID,
		spawn_service( {Circuit_I,TransformerN,M_service_per_transformer }, TransformerID, SystemMonitor ),
		ok.

spawn_service( {_Circuit_I,_TransformerN, 0 }, _TransformerID, _SystemMonitor ) ->
		done;
spawn_service( { Circuit_I, TransformerN, ServiceN }, TransformerID, SystemMonitor ) ->
		Sink_ID = { service, Circuit_I, TransformerN, ServiceN },
		ReportingThreshold = 1000 * 32,
		spawn_sink( Sink_ID, ReportingThreshold, TransformerID, SystemMonitor ),
		spawn_service( { Circuit_I, TransformerN, ServiceN - 1 }, TransformerID, SystemMonitor ).
% --------------------------------------------------------------------------------------------------------------------

% Spawn a sink (service | transformer | circuit | system monitor )
spawn_sink( Sink_ID, ReportingThreshold, AggregatorID, SystemMonitor ) ->
		?debugVal({spawn_sink, Sink_ID, ReportingThreshold, AggregatorID, SystemMonitor }),
		Handler = { sink, Sink_ID },
		Arguments = [Sink_ID, ReportingThreshold, AggregatorID, SystemMonitor],
 		Result = event_mgr:add_stand_alone_event_handler(Handler, Arguments),	
		?assertEqual( ok, Result ).

% --------------------------------------------------------------------------------------------------------------------
		
% Send reset event to circuits.
% Each circuit will send reset to its services and zero all histories.
% Each service will will reset its zero its register and history.
reset_all_circuits() ->
		?NOTIMPLEMENTED,
		ok.

% Send interval tick event to all listeners.
% Services will calculate interval usage and record in their interval history.
% Services will forward interval usage to their Transformer.
% Transformers will add interval usage into their interval,
%   check for over-load,
%   and forward interval usage to their circuit.
% Circuits will mirror service and transformer history and add into own.
% Circuits will forward service usage (with path) to System-Monitor.
% Circuits will also check for over-load.
% System-Monitor will mirror service & transformer & circuit history.

send_interval_tick() ->
		?NOTIMPLEMENTED,
		ok.

% --------------------------------------------------------------------------------------------------------------------

spawn_test() ->
		N_circuits = 1,
		M_services_per_circuit = 5,
		TransfomerFanOut = 2,
		spawn_circuits( N_circuits, M_services_per_circuit, TransfomerFanOut ),
		% Check to see if we have the result we expect.
		List = event_mgr:get_handlers(),
		%?debugVal( List ),
		?assertEqual( 8, length(List) ).

tick_test() ->
		Seconds = calendar:datetime_to_gregorian_seconds({ {2011,12,31}, {13,59,00} }),
		Event = { tick, Seconds },
		event_mgr:send_event_to_all_handlers_and_wait( Event ).




