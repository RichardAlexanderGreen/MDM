%%% Copyright 2011 Richard Alexander Green
%%% Description : A circuit owns transformers and feeds energy to transformers.
%%%
%%%
%%% Created : Jun 17, 2011
%%% -------------------------------------------------------------------
-module(circuit).

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
	              , components = []   % A circuit`s components are transformers. Members must be PID.
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
								, asset = #asset{}    % A circuit is an asset
	              , sink  = #sink{}     % A circuit is also an energy sink
								, monitor             % A circuit has a monitor (PID). A Monitor may monitor several circuits
								, number_services_planned = ?DEFAULT_SERVICES
								, transformer_fanout = ?DEFAULT_FANOUT
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
			, { number_services, NServices}
			, { transformer_fanout, NServicesPerTransformer }
			]) ->
		process_flag( trap_exit, true ),
		State = #state { id = Identifier                 % entity ID
								   , asset = #asset{ id = Identifier}     % A circuit is an asset
	                 , sink  = #sink{ id = Identifier }     % A circuit is also an energy sink
								   , number_services_planned = NServices
									 , transformer_fanout = NServicesPerTransformer
								   },
		{ok, planned, State };   % Initial state is 'planned'

init([]) ->
		exit("Should not initialize a circuit without an argument list."),
    {ok, state_name, #state{}}.

% Setup standard arguments for initialization of a circuit.
% Note: Entity_ID should be { circuit, [X] }
setup( { circuit, [ Identifier ] } ) ->
		Entity_ID = { circuit, [ Identifier ] },
		NServices = 1000,
		NServicesPerTransformer = 10,
		InitializationArgs = [ { id, Entity_ID }
			                   , { number_services, NServices}
			                   , { transformer_fanout, NServicesPerTransformer }
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

planned( install, StateData) ->
		
		#state{ id = CircuitID
					, number_services_planned = NServices
					, transformer_fanout = NServicesPerTransformer } = StateData,
		NTransformers = 1 + ( NServices div NServicesPerTransformer),
		?debugVal( {installing, NTransformers}),
		NewState = install_transformers( NTransformers, StateData ),
		propagate( install, NewState ),                              % Tell transformers to install services
    {next_state, running, NewState};

planned( Event, StateData ) ->
		exit( {"circuit does not expect event in planned mode:", Event } ),
		ignore.

running( {tick, DateTime }, StateData) ->
		?debugVal( {tick, DateTime, ?ME} ),
		NewStateData = propagate( {tick, DateTime }, StateData ),
    {next_state, running, StateData};

running( Event, StateData ) ->
		exit( {"circuit does not expect event in running mode:", Event } ),
		ignore.

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

% Install transformers on this circuit
% NewState = install_transformers( TransformerSequence, State ).
install_transformers( 0, State ) ->
    State;	
install_transformers( TransformerSequence, State ) ->
		?debugVal( { install_transformers, TransformerSequence } ),
		
		#state{ id = CircuitID, transformer_fanout = Fanout, asset = Asset } = State,
		#asset{ components = List } = Asset,
		
		% Create a transformer connected to this circuit.
		?debugVal( CircuitID ),
		{ circuit, CircuitPath } = CircuitID,
		TransformerPath = CircuitPath ++ [TransformerSequence],
		?assert( is_list( TransformerPath ) ),
		%TransformerID = { transformer, TransformerPath },
		
		NewTransformer = transformer:create( TransformerPath, Fanout, self() ),
		?debugVal( NewTransformer ),
		
		UpdatedList = List ++ [ NewTransformer ],
		NewAsset = Asset#asset{ components = UpdatedList },
		NewState = State#state{ asset = NewAsset },
    install_transformers( TransformerSequence - 1 , NewState ).

% Propagate event (example: tick) to my transformers (who will propagate to services)
propagate( Event, StateData ) ->
		#state{asset = Asset } = StateData,
		#asset{components = Transformers } = Asset,
		send_event( Event, Transformers ),
		StateData.

send_event( _Event, [] ) ->
		done;
send_event( Event, Transformers ) ->
		[ Transformer | RemainingTransformers ] = Transformers,
		%?debugVal( Transformer ),
		{ transformer, _TransformerID, TransformerPID } = Transformer,
		ok = gen_fsm:send_event( TransformerPID, Event ),
		send_event( Event, RemainingTransformers ).

create( CircuitNumber, NServices, TransformerFanout ) ->
		Args = [ { id, CircuitNumber}
			     , { number_services, NServices}
			     , { transformer_fanout, TransformerFanout }
			     ],
		 { ok, CircuitPID } = gen_fsm:start( ?MODULE, Args, [] ),
		 { circuit, CircuitNumber, CircuitPID }.

% //////////////////////////////////// TESTS //////////////////////////////////////////////////////

init_test() ->
		?debugMsg(init_test),
		CircuitID = {circuit, [1] },
		NServices = 1000,
		NServicesPerTransformer = 9,
		Init_Result = init( [ { id, CircuitID }
												, { number_services, NServices} 
												, { transformer_fanout, NServicesPerTransformer } 
												]),
		{ok, planned, StateData } = Init_Result,
		#state{ id = CircuitID
					, number_services_planned = NServices
					, transformer_fanout = NServicesPerTransformer } = StateData,
		StateData.

install_transformers_test() ->
		?debugMsg(install_transformers_test),
		StateData = init_test(),
		TransformerSequence = 1,
		NewState = install_transformers( TransformerSequence, StateData ),
		#state{ asset = Asset } = NewState,
		#asset{ components = Transformers } = Asset,
		?assertEqual( 1, length(Transformers) ),
		NewState.

planned_install_test() ->
		?debugMsg(planned_install_test),
		CircuitID = { circuit, [1] },
		{ circuit, CircuitNumber, CircuitPID } = create( CircuitID, 11, 3 ),
		ok = gen_fsm:send_event( CircuitPID, install ),
		CircuitPID.

running_tick_test() ->     % See tick run.   Run tick, run!
		?debugMsg(running_tick_test),
		% First we need to create a circuit and install its transformers.
		CircuitID = { circuit, [1] },
		{ circuit, _CircuitNumber, CircuitPID } = create( CircuitID, 11, 3 ),
		ok = gen_fsm:send_event( CircuitPID, install ),
		%timer:sleep(1234),
		
		DateTime = { {2011,12,31}, { 13, 59, 00 } },
		Event = {tick, DateTime },
		ok = gen_fsm:send_event( CircuitPID, Event ),
		?debugMsg("Should see tick event debug trace in circuit and transformer"),
    timer:sleep(1234),
		ok.

		


