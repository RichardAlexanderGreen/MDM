%%% Copyright Richard Alexander Green 2011
%%% Description: Merge code from simulatorV00 and event_mgr modules.

%-------------------
-module(simulator).

%%
%% Include files
%%
-define( NOTIMPLEMENTED, exit("This function is not implemented yet") ).


-define( TEST, true ).
-define( NODEBUG, true ).

% Set log level
% ALL=0 < TRACE=1 < DEBUG=2 < INFO=3 < WARN=4 < ERROR=5 < FATAL=6 < OFF=7
% Level semantics are defined in the org.apache.log4j.Level class.
-define( LOGLEVEL, 3 ).             % 3 means log info level and above. -- See log/2.

-include_lib("eunit/include/eunit.hrl").

-ifdef( DEBUG ).
-compile( export_all ).
-endif.

-define( EVENT_MANAGER, { global, ?MODULE} ).

%%
%% Exported Functions
%%
-export([ start_alone/0
				, start_supervised/0
				, add_stand_alone_event_handler/2
				, add_supervised_event_handler/2 ]).

%%
%% API Functions
%%

% Start a stand-alone event manager (me).
start_alone() ->
		gen_event:start( ?EVENT_MANAGER ).         % Choose global / local instance

% Start a supervised event manager (me) -- should be called by a supervisor.
start_supervised() ->
		gen_event:start_link( ?EVENT_MANAGER ).    % Choose global / local instance

% Start a stand-alone event handler in the context of an event manager instance (me).
% This event-handler's existence will be independent of the caller's.
% Handler = Module | { Module, Id }
% Arguments = (Whatever the handler init/1 needs)
add_stand_alone_event_handler( Handler, Arguments ) ->
		Result = gen_event:add_handler( ?EVENT_MANAGER, Handler, Arguments ),
	  ?debugVal( Result ),
	  Result.

% Start a supervised event handler in the context of an event manager instance (me).
% This event-handler will terminate when the caller terminates.
add_supervised_event_handler( Handler, Arguments ) ->
		Result = gen_event:add_sup_handler( ?EVENT_MANAGER, Handler, Arguments ),
		%?debugVal( Result ),
	  Result.

% Remove an event handler
remove_event_handler( Handler ) ->
		TerminateArguments = 'Someone called event_mgr:remove_event_handler ',
		Result = gen_event:delete_handler( ?EVENT_MANAGER, Handler, TerminateArguments ),
		?debugVal( Result ),
		Result.

% Send event to all event handlers.  ( asynchronous - does not wait )
send_event_to_all_handlers( Event ) ->
		gen_event:notify( ?EVENT_MANAGER, Event).       % Will return  ok  regardless.

% Send event to all event handlers and wait until it is processed by all of them.
send_event_to_all_handlers_and_wait( Event ) ->
		gen_event:sync_notify( ?EVENT_MANAGER, Event).  % Will return  ok  regardless.

stop() ->
		gen_event:stop( ?EVENT_MANAGER ).


%%
%% Local Functions
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  TESTS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_supervised_test() ->
		Result = start_supervised(),
		{ok, PID } = Result,
		?assert( is_pid( PID ) ),
		gen_event:stop( PID ).        % Stop so that  start_alone_test  can run.

start_alone_test() ->
		Result = start_alone(),
		{ok, PID } = Result,
		?assert( is_pid( PID ) ).

add_stand_alone_event_handler_test() ->
		Handler = { event_handler, instance_1 },
		Arguments = [ instance_1_arguments ],
		Result = add_stand_alone_event_handler( Handler, Arguments ),
		?assertEqual( ok, Result ).

add_supervised_event_handler_test() ->
		Handler = { event_handler, instance_2 },
		Arguments = [ instance_2_arguments ],
		Result = add_supervised_event_handler( Handler, Arguments ),
		?assertEqual( ok, Result ).

send_event_to_all_handlers_test() ->
		Event = { event, 'This is only a test ', send_event_to_all_handlers_test },
		Result = send_event_to_all_handlers( Event ),
		?assertEqual( ok, Result ).

send_event_to_all_handlers_and_wait_test() ->
		Event = { event, 'This too is only a test ', send_event_to_all_handlers_and_wait },
		Result = send_event_to_all_handlers_and_wait( Event ),
		?assertEqual( ok, Result ).

remove_event_handler_test() ->
		Handler = { event_handler, instance_1 },
		Result = remove_event_handler( Handler ),
		?assertEqual( ok, Result ).
		
stop_test_suppressed() ->
		% Test termination behavior --- Will kill event-manager too !!!
		stop().

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
		Handler = { sink, Sink_ID },
		PIDmaybe = add_stand_alone_event_handler( Handler, [Sink_ID, ReportingThreshold, Aggregator, SystemMonitor]),
		?debugVal( PIDmaybe ),
		%PID = gen_event:add_handler( self(), sink, [Sink_ID, ReportingThreshold, Aggregator, SystemMonitor]),
		PIDmaybe.

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




