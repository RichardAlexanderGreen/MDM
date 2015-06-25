%%% Copyright 2011 Richard Alexander Green
%%% Description : Simulate an electric service monitored by a meter.
%%%
%%% Created : Jun 17, 2011
%%% -------------------------------------------------------------------
-module(service).

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

% A service is an asset.
-record( asset, { id           % entity ID   { <enity_type>, [] }
	              , location   = 'undefined geo-location'  
	              , parent     = 'undefined parent'
	              , components = []   % A service`s components are meters
								}).

% A service is an energy sink.
-record( sink, { id           % entity ID
							 , aggregator   = 'undefined aggregator'
	             , load_profile = 'undefined load_profile'
							 , load_history = [] 
							 } ).

-define( DEFAULT_SERVICES, 1000 ).
-record( state, { id                  % entity ID
								, asset = #asset{}    % A service is an asset
	              , sink  = #sink{}     % A service is also an energy sink
								, monitor             % A service has a monitor (PID). A Monitor may monitor several services
								} ).

-define( ME, StateData#state.id ).

%% --------------------------------------------------------------------
%% External exports
-export([ create/2
				, setup/2      % TODO: Do I really need to expose setup/2 ????
				]).

%% gen_fsm callbacks
-export([init/1, state_name/2, state_name/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% ====================================================================
%% External functions
%% ====================================================================
% Setup standard arguments for initialization of a service.
% Note: Entity_ID should be { service, [X] }
setup( { service, Identifier }, TransformerPID ) ->
    Entity_ID = { service, Identifier  },
		NServices = 1000,
		NServicesPerService = 10,
		InitializationArgs = [ { id, Entity_ID }
			                   , { transformerPID, TransformerPID }
			                   ],
		InitializationArgs.

% Create a service instance
% Returns { service, ServiceID, ServicePID }
create( ServicePath, TransformerPID ) ->
		Options = [],
		InitializationArgs = setup( {service, ServicePath }, TransformerPID ),		
		Result = gen_fsm:start( ?MODULE, InitializationArgs, Options),
		{ok, ServicePID } = Result,
		{ service, ServicePath, ServicePID }.


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
			, { transformerPID, TransformerPID }
			]) ->
		process_flag( trap_exit, true ),
		StateData = #state { id = Identifier                 % entity ID
								   , asset = #asset{ id = Identifier}     % A service is an asset
	                 , sink  = #sink{ id = Identifier }     % A service is also an energy sink
								   , monitor =  TransformerPID
								   },
		{ok, planned, StateData };   % Initial state is 'planned'
init( Args ) ->
		exit( {"init args not matching pattern", Args }),
   {ok, state_name, #state{}};


init([]) ->
		exit("Empty initialization list"),
    {ok, state_name, #state{}}.


%% --------------------------------------------------------------------
%% Func: StateName/2   <<< Respond to gen_fsm:send_event(FsmRef, Event)
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
state_name( Event, StateData ) ->
		exit( "should never be called"),
    {next_state, state_name, StateData}.

planned( install, StateData ) ->
		NewState = install_meter( StateData ),
    {next_state, running, NewState};
planned( Event, StateData ) ->
		exit( {"Event not valid in service:planned mode", Event} ),
    ignore.

running( { tick, DateTime }, StateData) ->
		% Generate usage based on my load profile
		Usage = generate_usage( DateTime, StateData ),
		% Inform my meter
		inform_meter( DateTime, Usage, StateData ),
    {next_state, running, StateData };

running( { usage, DateTime, Usage }, StateData ) ->
	  % Store usage in my usage history
		inform_transformer( DateTime, Usage, StateData ),
		NewState = store_usage( Usage, StateData ),
		{next_state, running, NewState }.
 

%% --------------------------------------------------------------------
%% Func: StateName/3     <<< Respond to gen_fsm:sync_send_event(FsmRef, Event)
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
state_name( Event, From, StateData) ->
    Reply = ok,
    {reply, Reply, state_name, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_event/3  <<< Respond to gen_fsm:send_all_state_event(FsmRef, Event)
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->
		?NOT_IMPLEMENTED,
		
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
		?debugVal( { terminate, Reason, StateName, StatData} ),
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

% Install meters on this service
% NewState = install_meters( ServiceSequence, State ).
install_meter( StateData ) ->
		#state{ id = ServiceID,  asset = Asset } = StateData,
		#asset{ components = List } = Asset,
		
		% Create a meter connected to this service.
		ServicePID = self(),
		NewMeter = meter:create( ServiceID , ServicePID ),
		
		UpdatedList = List ++ [ NewMeter ],
		NewAsset = Asset#asset{ components = UpdatedList },
		NewState = StateData#state{ asset = NewAsset },
		NewState.

% Generate usage depending on time of week and load-profile.
% Usage = generate_usage( DateTime, StateData ),
generate_usage( DateTime, StateData ) ->
		#state{ sink = Sink } = StateData,
		#sink{ load_profile = LoadProfile } = Sink,
		Usage = 1234.

store_usage( Usage, StateData ) ->
		#state{ sink = Sink } = StateData,
		#sink{ load_history = History } = Sink,
		NewHistory = History ++ [Usage],
		NewSink = Sink#sink{ load_history = NewHistory },
		NewState = StateData#state{ sink = NewSink },
		NewState.
		
inform_meter( DateTime, Usage, StateData )->
		#state{ asset = Asset } = StateData,
		#asset{ components = Components } = Asset,
		[ MeterPID ] = Components,
		ok = gen_fsm:send_event( MeterPID, { usage, DateTime, Usage } ) .

inform_transformer( DateTime, Usage, StateData ) ->
		#state{ monitor = TransformerPID } = StateData,
		ok = gen_fsm:send_event( TransformerPID, {usage, DateTime, Usage } ),
		ok.

% //////////////////////////////////// TESTS ////////////////////////////////////////
generate_usage_test() ->
		?debugMsg( "generate_usage_test" ),
		StateData = #state{},
		DateTime = { {2011,12,31}, { 13, 59, 00 } },
		Usage = generate_usage( DateTime, StateData ),
		?assert( is_integer( Usage ) ).

setup_test() ->
		?debugMsg( "setup_test" ),
		ServiceID = [1,2,3],
		TransformerPID = self(),  % mock
		Args = setup( { service, ServiceID }, TransformerPID ),
		Entity_ID = { service, ServiceID },
		?assertEqual([ { id, Entity_ID } , { transformerPID, TransformerPID }  ], Args ).

init_test() ->
		?debugMsg( "init_test" ),
		ServiceID = [1,2,3],
		TransformerPID = self(),  % mock
		Args = setup( { service, ServiceID }, TransformerPID ), 
		Init_Return = init( Args ),
		{ ok, planned, _MyPID } = Init_Return.

create_test() ->
		?debugMsg( "create_test" ),
		ServiceID = [ 1,2,3 ],
		TransformerPID = self(),
		Entity_ID = { service, ServiceID },
		{ service, ServiceID, ServicePID } = create( ServiceID,  TransformerPID ),
		?assert( is_pid( ServicePID ) ),
		ServicePID.

inform_transformer_test() ->
		?debugMsg( "inform_transformer_test" ),
		{ok, MockTransformerPID } = gen_fsm:start( mock_fsm, [], [] ),
		StateData = #state{ monitor = MockTransformerPID },
		DateTime = { {2011,12,31}, { 13, 59, 00 } },
		Usage = 1234,
		inform_transformer( DateTime, Usage, StateData ).

inform_meter_test() ->
		?debugMsg( "inform_meter_test" ),
		
		{ok, MockMeterPID } = gen_fsm:start( mock_fsm, [], [] ),
		Asset = #asset{components = [MockMeterPID]},
		StateData = #state{ asset = Asset },
		DateTime = { {2011,12,31}, { 13, 59, 00 } },
		Usage = 1234,
		inform_meter( DateTime, Usage, StateData ).

install_meter_test() ->
		?debugMsg( "install_meter_test" ),
		StateData = #state{ id = {service, [1,2,3] } },
		NewStateData = install_meter( StateData ),
		NewStateData.

install_meter_event_test() ->
		?debugMsg( "install_meter_event_test" ),
		ServicePID = create_test(),
		Event = install,
		ok = gen_fsm:send_event( ServicePID, Event ).


running_tick_test() ->
		?debugMsg( "running_tick_test" ),
		ServicePID = create_test(),
		DateTime = { {2011,12,31}, { 13, 59, 00 } },
		ok = gen_fsm:send_event( ServicePID, install ),              % Must install before we tick
		ok = gen_fsm:send_event( ServicePID, { tick, DateTime }).


