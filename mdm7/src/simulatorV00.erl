-module(simulatorV00).  % Version 00 is not functional -- code study

-behaviour(gen_server).

-define( NOTIMPLEMENTED, exit("function not implemented")  ).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

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
		SystemMonitor = spawn_sink( {system_monitor, 0}, infinity, 'n/a', 'n/a' ),    % TODO: DOES THIS MAKE SENSE? ***
		spawn_circuit( Circuit_I, M_services_per_circuit, TransfomerFanOut, SystemMonitor ),
		ok.

spawn_circuit( 0, _M_services_per_circuit, _TransfomerFanOut, _SystemMonitor ) ->
		done;
spawn_circuit( Circuit_I, M_services_per_circuit, TransfomerFanOut, SystemMonitor ) ->
	 % Spawn a sink process with this circuit ID.
	 ReportingThreshold = M_services_per_circuit * 1000 * 32,
	 PID = spawn_sink( Circuit_I, ReportingThreshold, SystemMonitor, SystemMonitor ),
	 CircuitAggregator = { circuit, Circuit_I, PID },
	 % TODO: Spawn a bunch of transformers.
	 spawn_transformers( TransfomerFanOut, M_services_per_circuit, CircuitAggregator, SystemMonitor ),
   spawn_circuit( Circuit_I - 1, M_services_per_circuit, TransfomerFanOut, SystemMonitor ).
% --------------------------------------------------------------------------------------------------------------------
% Spawn transformers within a circuit context
spawn_transformers( TransfomerFanOut, M_services_per_circuit, CircuitAggregator, SystemMonitor ) ->
		{ circuit, Circuit_I, _PID } = CircuitAggregator,
		TransformerN = M_services_per_circuit div TransfomerFanOut,
		M_services_per_transformer = TransfomerFanOut,
		spawn_transformer( {Circuit_I,TransformerN}, M_services_per_transformer, CircuitAggregator, SystemMonitor ),
		ok.

spawn_transformer( {_,0}, _M_service_per_transformer, CircuitAggregator, SystemMonitor ) ->
		done;
spawn_transformer( {Circuit_I,TransformerN}, M_service_per_transformer, CircuitAggregator, SystemMonitor ) ->
		% Spawn a sink process with this transformer ID.
		ReportingThreshold = M_service_per_transformer * 1000 * 32,
		PID = spawn_sink( {Circuit_I,TransformerN}, ReportingThreshold, CircuitAggregator, SystemMonitor ),
		TransformerAggregator = { transformer, {Circuit_I,TransformerN}, PID },
		spawn_services( M_service_per_transformer, TransformerAggregator, SystemMonitor ),
		spawn_transformer( {Circuit_I,TransformerN - 1}, M_service_per_transformer, CircuitAggregator, SystemMonitor ).
% --------------------------------------------------------------------------------------------------------------------
% Spawn services within a transformer context
spawn_services( M_service_per_transformer, TransformerAggregator, SystemMonitor ) ->
		{ transformer, {Circuit_I,TransformerN}, PID } = TransformerAggregator,
		spawn_service( {Circuit_I,TransformerN,M_service_per_transformer }, TransformerAggregator, SystemMonitor ),
		ok.

spawn_service( {_Circuit_I,_TransformerN, 0 }, _TransformerAggregator, _SystemMonitor ) ->
		done;
spawn_service( { Circuit_I, TransformerN, ServiceN }, TransformerAggregator, SystemMonitor ) ->
		Sink_ID = { Circuit_I, TransformerN, ServiceN },
		ReportingThreshold = 1000 * 32,
		_PID = spawn_sink( Sink_ID, ReportingThreshold, TransformerAggregator, SystemMonitor ),
		spawn_service( { Circuit_I, TransformerN, ServiceN - 1 }, TransformerAggregator, SystemMonitor ).
% --------------------------------------------------------------------------------------------------------------------

% Spawn a sink (service | transformer | circuit | system monitor )
spawn_sink( Sink_ID, ReportingThreshold, Aggregator, SystemMonitor ) ->
		?debugVal({spawn_sink, Sink_ID, ReportingThreshold, Aggregator, SystemMonitor }),
		PID = gen_event:add_handler( self(), sink, [Sink_ID, ReportingThreshold, Aggregator, SystemMonitor]),
		PID.

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
		M_services_per_circuit = 3,
		TransfomerFanOut = 2,
		spawn_circuits( N_circuits, M_services_per_circuit, TransfomerFanOut ).




