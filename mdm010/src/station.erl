%%% Copyright 2011 Richard Alexander Green
%%% Description : Simulate a station (owns circuits, feeds energy to circuits).
%%% 
%%%
%%% Created : Jun 17, 2011
%%% -------------------------------------------------------------------
-module(station).

-behaviour(gen_fsm).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-define( NOT_IMPLEMENTED, exit( {?MODULE,'has not implemented this function'} )  ).


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

% A circuit is an asset.
-record( asset, { id           % entity ID   { <enity_type>, [] }
	              , location   = 'undefined geo-location'  
	              , parent     = 'undefined parent'
	              , components = []   % A station`s components are circuits. Members must include PID.
								}).

% A circuit is an energy sink.
-record( sink, { id           % entity ID
							 , aggregator   = 'undefined aggregator'
	             , load_profile = 'undefined load_profile'
							 , load_history = [] 
							 } ).

-define( DEFAULT_SERVICES, 1000 ).
-define( DEFAULT_FANOUT, 10 ).
-record( state, { id                  % entity ID
								, asset = #asset{}    % A station is an asset
	              , sink  = #sink{}     % A station is also an energy sink
								, monitor             % A station has a monitor (PID). A Monitor may monitor several station
								, services_per_circuit = ?DEFAULT_SERVICES
								, transformer_fanout   = ?DEFAULT_FANOUT
								} ).
-define( ME, StateData#state.id ).

%% --------------------------------------------------------------------
%% External exports
-export([]).

%% gen_fsm callbacks
-export([init/1, state_name/2, state_name/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% ====================================================================
%% External functions
%% ====================================================================


%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% --------------------------------------------------------------------
init( [ { id, Identifier}
			, { services_per_circuit, ServicesPerCircuit}
			, { transformer_fanout, TransformerFanout }
			]) ->
		process_flag( trap_exit, true ),
		State = #state { id = Identifier                 % entity ID
								   , asset = #asset{ id = Identifier}     % A station is an asset
	                 , sink  = #sink{ id = Identifier }     % A station is also an energy sink
								   , services_per_circuit = ServicesPerCircuit
									 , transformer_fanout = TransformerFanout
								   },
		{ok, planned, State };   % Initial state is 'planned'
init( Args ) ->
		exit({"This argument pattern is not expected:" , Args } ),
		ignore;

init([]) ->
		exit("Should not initialize a station without an argument list."),
    {ok, state_name, #state{}}.

% Setup standard arguments for initialization of a station.
% Note: Entity_ID should be { station, [X] }
setup( { station, [ Identifier ] } ) ->
		Entity_ID = { station, [ Identifier ] },
		ServicesPerCircuit = 1000,
		TransformerFanout = 10,
		InitializationArgs =[ { id, Identifier} 
												, { services_per_circuit, ServicesPerCircuit}
												, { transformer_fanout, TransformerFanout } 
												],
		InitializationArgs.

%% --------------------------------------------------------------------
%% Func: StateName/2   <<< Respond to gen_fsm:send_event(FsmRef, Event)
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
state_name(Event, StateData) ->
		exit( "should never be called"),
    {next_state, state_name, StateData}.


planned( {install, NCircuits }, StateData) ->
		MyID = StateData#state.id, ?debugVal( { install, NCircuits, on, MyID } ),
		#state{ id = CircuitID
					, services_per_circuit = NServicesPlanned
					, transformer_fanout = TransformerFanout } = StateData,
		NewState = install_circuits( NCircuits, StateData ),
		propagate( install, NewState ),                              % Tell stations to install services
    {next_state, running, NewState};
planned( Event, StateData ) ->
		exit( {"Event not expected in planned mode:", Event }),
		ignore.


running( {tick, DateTime }, StateData) ->
		?debugVal( {tick, DateTime, ?ME} ),
		NewStateData = propagate( {tick, DateTime }, StateData ),
    {next_state, running, StateData}.

%% --------------------------------------------------------------------
%% Func: StateName/3     <<< Respond to gen_fsm:sync_send_event(FsmRef, Event)
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
running( ping, From, StateData ) ->
		?debugVal( { ping, From, ?ME } ),
    Reply = ok,
    {reply, Reply, state_name, StateData}.

state_name(Event, From, StateData) ->
		exit( "should never be called"),
    Reply = ok,
    {reply, Reply, state_name, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_event/3  <<< Respond to gen_fsm:send_all_state_event(FsmRef, Event)
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_sync_event/4  <<< Respond to gen_fsm:sync_send_all_state_event(FsmRef, Event)
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
handle_sync_event(Event, From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_info/3   <<< Respond to PID ! Info messages.
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_info(Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: terminate/3    <<< Respond to shutdown sequence
%% Purpose: Shutdown the fsm
%% Returns: any
%% --------------------------------------------------------------------
terminate(Reason, StateName, StatData) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% --------------------------------------------------------------------
code_change(OldVsn, StateName, StateData, Extra) ->
    {ok, StateName, StateData}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

% Install circuits on this station
% NewState = install_circuits( CircuitSequence, State ).
install_circuits( 0, State ) ->
    State;	
install_circuits( CircuitSequence, State ) ->
		#state{ id = StationID
					, services_per_circuit = ServicesPerCircuit
					, transformer_fanout = TransformerFanout
					, asset = Asset } = State,
		#asset{ components = List } = Asset,
		
		% Create a circuit connected to this station.
		?debugVal( StationID ),
		{ station, StationSequence } = StationID,
		CircuitPath = StationSequence ++ [CircuitSequence],
		?assert( is_list( CircuitPath ) ),
		
		NewCircuit = circuit:create( CircuitPath, ServicesPerCircuit, TransformerFanout ),
		
		
		UpdatedList = List ++ [ NewCircuit ],
		NewAsset = Asset#asset{ components = UpdatedList },
		NewState = State#state{ asset = NewAsset },
    install_circuits( CircuitSequence - 1 , NewState ).

% Propagate event (example: tick) to my circuits (who will propagate to services)
propagate( Event, StateData ) ->
		#state{asset = Asset } = StateData,
		#asset{components = Circuits } = Asset,
		send_event( Event, Circuits ),
		StateData.

send_event( _Event, [] ) ->
		done;
send_event( Event, Circuits ) ->
		[ Circuit | RemainingCircuits ] = Circuits,
		?debugVal( { send_event, Circuit, Event } ),
		{ circuit, _CircuitID, CircuitPID } = Circuit,
		%CircuitPID = Circuit,                               % *** HACK ***
		ok = gen_fsm:send_event( CircuitPID, Event ),
		send_event( Event, RemainingCircuits ).

create( StationNumber, ServicesPerCircuit, TransformerFanout ) ->
		Args =  [ { id, StationNumber }
			      , { services_per_circuit, ServicesPerCircuit}
			      , { transformer_fanout, TransformerFanout }
			      ],
		 { ok, StationPID } = gen_fsm:start( ?MODULE, Args, [] ),
		 StationPID.

% //////////////////////////////////// TESTS //////////////////////////////////////////////////////

init_test() ->
		?debugMsg(init_test),
		StationID = {station, [1] },
		ServicesPerCircuit = 1000,
		TransformerFanout = 9,
		Init_Result = init( [ { id, StationID } 
												, { services_per_circuit, ServicesPerCircuit} 
												, { transformer_fanout, TransformerFanout } 
												]),
		{ok, planned, StateData } = Init_Result,
		#state{ id = CircuitID
					, services_per_circuit = ServicesPerCircuit
					, transformer_fanout = TransformerFanout } = StateData,
		StateData.

install_circuits_test() ->
		?debugMsg(install_circuits_test),
		StateData = init_test(),
		CircuitSequence = 1,
		NewState = install_circuits( CircuitSequence, StateData ),
		#state{ asset = Asset } = NewState,
		#asset{ components = Circuits } = Asset,
		?assertEqual( 1, length( Circuits ) ),
		NewState.

planned_install_test() ->
		?debugMsg(planned_install_test),
		StationID = { station, [1] },
		StationPID = create( StationID, 11, 3 ),         % Keep the number down to avoid huge trace output
		NCircuits = 2,
		ok = gen_fsm:send_event( StationPID, { install, NCircuits } ),
		StationPID.

running_tick_test() ->     % See tick run.   Run tick, run!
		?debugMsg(running_tick_test),
		% First we need to create a station and install its circuits.
		StationID = { station, [1] },
		StationPID = create( StationID, 11, 3 ),         % Keep the number down to avoid huge trace output
		NCircuits = 2,
		ok = gen_fsm:send_event( StationPID, { install, NCircuits } ),
		%timer:sleep(1234),
		
		DateTime = { {2011,12,31}, { 13, 59, 00 } },
		Event = {tick, DateTime },
		ok = gen_fsm:send_event( StationPID, Event ),
		?debugMsg("Should see tick event debug trace in station and circuit"),
    timer:sleep(1234),
		ok.

		


