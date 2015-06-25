%%% -------------------------------------------------------------------
%%% Author  : admin
%%% Description :
%%%
%%% Created : Jun 17, 2011
%%% -------------------------------------------------------------------
-module(transformer).

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

% A transformer is an asset.
-record( asset, { id           % entity ID   { <enity_type>, [] }
	              , location   = 'undefined geo-location'  
	              , parent     = 'undefined parent'
	              , components = []   % A transformer`s components are services
								}).

% A transformer is an energy sink.
-record( sink, { id           % entity ID
							 , aggregator   = 'undefined aggregator'
	             , load_profile = 'undefined load_profile'
							 , load_history = [] 
							 } ).

-define( DEFAULT_MAX_FANOUT, 10 ).
-define( DEFAULT_FANOUT, 5 ).
-record( state, { id                  % entity ID
								, asset = #asset{}    % A transformer is an asset
	              , sink  = #sink{}     % A transformer is also an energy sink
								, monitor             % A transformer has a monitor (PID). A Monitor may monitor several transformers
								, max_fanout = ?DEFAULT_MAX_FANOUT
								, service_fanout = ?DEFAULT_FANOUT
								} ).

%% --------------------------------------------------------------------
%% External exports
-export([ create/3 
				, setup/2      % TODO: Do I really need to expose setup/2 ????
				]).

%% gen_fsm callbacks
-export([init/1, state_name/2, state_name/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% ====================================================================
%% External functions
%% ====================================================================
% Setup standard arguments for initialization of a transformer.
% Note: Entity_ID should be { transformer, [X] }
setup( { transformer, Identifier }, CircuitPID ) ->
		?assert( is_list(Identifier ) ),

		Entity_ID = { transformer, Identifier },
		MaxFanout = 10,
		NServicesPerTransformer = 5,
		InitializationArgs = [ { id, Entity_ID }
			                   , { max_fanout, MaxFanout}
			                   , { service_fanout, NServicesPerTransformer }
												 , { circuit_PID, CircuitPID }
			                   ],
		InitializationArgs.

% Create a transformer instance
% Returns { transformer, TransformerID, TransformerPID }
create( TransformerID, Fanout, CircuitPID ) ->
		?debugVal( {create, TransformerID, Fanout, CircuitPID} ),
		?assert( is_list( TransformerID ) ),
		?assert( is_pid( CircuitPID ) ),
		Options = [],
		InitializationArgs = setup( {transformer, TransformerID}, CircuitPID ),		
		{ok, TransformerPID } = gen_fsm:start( ?MODULE, InitializationArgs, Options),
		{ transformer, TransformerID, TransformerPID }.


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
			, { max_fanout, MaxFanout}
			, { service_fanout, NServicesPerService }
			, { circuit_PID, CircuitPID }
			]) ->
		process_flag( trap_exit, true ),
		State = #state { id = Identifier                      % entity ID
								   , asset = #asset{ id = Identifier}     % A transformer is an asset
	                 , sink  = #sink{ id = Identifier }     % A transformer is also an energy sink
								   , max_fanout = MaxFanout
									 , service_fanout = NServicesPerService
									 , monitor = CircuitPID
								   },
		{ok, planned, State };   % Initial state is 'planned'
init( Args ) ->
		?debugVal( Args ),
		ignore;


init([]) ->
		exit("Should not initialize a transformer without an argument list."),
    {ok, state_name, #state{}}.


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
		#state{ id = TransformerID
					, service_fanout = NServicesPerTransformer } = StateData,
		CountDown = NServicesPerTransformer,
		NewState = install_services( CountDown, StateData ),
		propagate( install, NewState ),                       % Tell services to install meters.
    {next_state, running, NewState};
planned( Event, StateData ) ->
		exit({"Event not expected when transformer is merely planned:", Event}),
		ignore.

running( {tick, DateTime }, StateData) ->
		MyID = StateData#state.id, ?debugVal( {tick, DateTime, MyID }),
		propagate( {tick, DateTime }, StateData ),
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
		?debugVal( {ping, From, StateData#state.id} ),
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

% Install services on this transformer
% NewState = install_services( ServiceSequence, State ).
install_services( 0, State ) ->
    State;	
install_services( ServiceSequence, State ) ->
		#state{ id = TransformerID, service_fanout = Fanout, asset = Asset } = State,
		#asset{ components = List } = Asset,
		
		% Create a service connected to this transformer.
		{transformer, TransformerSequence } = TransformerID,
		ServicePath = TransformerSequence ++ [ServiceSequence],
		NewService = service:create( ServicePath , self() ),
		
		UpdatedList = List ++ [ NewService ],
		NewAsset = Asset#asset{ components = UpdatedList },
		NewState = State#state{ asset = NewAsset },
    install_services( ServiceSequence - 1 , NewState ).

% Propagate event (example: tick) to my services
propagate( Event, StateData ) ->
		#state{ asset = Asset } = StateData,
		#asset{ components = Services } = Asset,
		send_event( Event, Services ).

send_event( _Event, [] ) ->
		done;
send_event( Event, Services ) ->
		[ Service | RemainingServices ] = Services,
		{service, _ServiceID, ServicePID } = Service,
		ok = gen_fsm:send_event( ServicePID, Event ),
		send_event( Event, RemainingServices ).

% ////////////////////////////////// TESTS ///////////////////////////////////////////

setup_test() ->
		TransformerID = [1,2],
		CircuitPID = self(), 
		Args = setup( { transformer, TransformerID }, CircuitPID ),
		%?debugVal( Args ),
		?assertEqual( [ {id,{transformer,[1,2]}}
									, {max_fanout,10}
									, {service_fanout,5}
									, {circuit_PID, self() }
									], Args ).

init_test() ->
		TransformerID = [1,2],
		CircuitPID = self(), 
		Args = setup( { transformer, TransformerID }, CircuitPID ), 
		Init_Return = init( Args ),
		{ ok, planned, _ } = Init_Return.

create_test() ->
		TransformerID = [1,2],
		CircuitPID = self(), 
		FanOut = 11,
		Result = create( TransformerID, FanOut, CircuitPID ),
		{ transformer, TransformerID, TransformerPID } = Result.

send_event_test() ->
		Event = "This is only a test of transformer:send_event/2 ",
		{ok, MockServicePID } = gen_fsm:start( mock_fsm, [], [] ),
		MockServiceID = [1,2,3],
		MockService = {service, MockServiceID, MockServicePID },
		Services = [MockService],
		send_event( Event, Services ).

propagate_test() ->
		Event = "This is only a test of transformer:propagate/2 ",
		{ok, MockServicePID } = gen_fsm:start( mock_fsm, [], [] ),
		MockServiceID = [1,2,3],
		MockService = {service, MockServiceID, MockServicePID },
		Asset = #asset{ components = [MockService] },
		StateData = #state{ asset = Asset },	
		propagate( Event, StateData ).

install_services_test() ->
		TransformerID = [1,2],
		CircuitPID = self(), 
		Args = setup( { transformer, TransformerID }, CircuitPID ), 
		Init_Return = init( Args ),
		{ ok, planned, StateData } = Init_Return,
		CountDown = 1,
		NewState = install_services( CountDown, StateData ),
		NewState.


planned_install_test() ->
		TransformerID = [1,2],
		CircuitPID = self(), 
		Args = setup( { transformer, TransformerID }, CircuitPID ), 
		Init_Return = init( Args ),
		{ ok, planned, StateData } = Init_Return,
		planned( install, StateData ),
		StateData.

running_tick_test() ->     % See tick run.   Run tick, run!
		StateData = planned_install_test(),
		DateTime = { {2011,12,31}, { 13, 59, 00 } },
		running( {tick, DateTime }, StateData).

		
		
		



