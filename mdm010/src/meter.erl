%%% Copyright 2011 Richard Alexander Green
%%% Description : A meter is an asset and it monitors a service.
%%% - Meter stores service usage history.
%%%
%%% Created : Jun 17, 2011
%%% -------------------------------------------------------------------
-module(meter).

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

%% --------------------------------------------------------------------
%% External exports
-export([]).

%% gen_fsm callbacks
-export([init/1, running/2, state_name/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record( state, { id
	              , service
	              , history = [] 
								}).

%% ====================================================================
%% External functions
%% ====================================================================

% Create a meter and return its PID
create( ServiceID , ServicePID ) ->
		Args = [ { id, ServiceID}
			     , { servicePID, ServicePID }
			     ],
		Options = [],
		{ ok, MeterPID } = gen_fsm:start( ?MODULE, Args, Options ),
		MeterPID.

		

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
init( [ { id, ServiceID }
			, { servicePID, ServicePID }
			]) ->
		{ service, Path } = ServiceID,
		MeterID = { meter, Path },
		StateData = #state{ id = MeterID, service = ServicePID },	
		{ok, running, StateData};

init([]) ->
		exit( "empty init"),
    {ok, state_name, #state{}};

init( Args ) ->
		exit( {"init arguments pattern not expected ", Args} ),
		ignore.


%% --------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
running( { usage, DateTime, Usage }, StateData) ->
		% Generate usage based on my load profile
		store_usage( DateTime, Usage, StateData ),
		{next_state, running, StateData };

running(Event, StateData) ->
		exit({ "meter (running) does not handle event:", Event, StateData }),
    {next_state, state_name, StateData}.

%% --------------------------------------------------------------------
%% Func: StateName/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
state_name(Event, From, StateData) ->
		exit("Call not expected here"),
    Reply = ok,
    {reply, Reply, state_name, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_sync_event/4
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
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_info(Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: terminate/3
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

store_usage( DateTime, Usage, StateData ) ->
		#state{ history = History } = StateData,
		NewHistory = History ++ [Usage],
		NewState = StateData#state{ history = NewHistory },
		NewState.

% //////////////////////////////// TESTS ////////////////////////////////

init_test() ->
		ServiceID = {service, [1,2,3] },
		ServicePID = self(),
		Init_Result = init( [ { id, ServiceID } , { servicePID, ServicePID } ]  ),
		{ ok, running, StateData } = Init_Result.

store_usage_test() ->
		DateTime = { {2011,12,31}, {23,59,00} },
		Usage = 1234,
		StateData = #state{},
		NewState = store_usage( DateTime, Usage, StateData ),
		NewState.
		
create_test() ->
		ServiceID = {service, [1,2,3] },
		ServicePID = self(),
		MeterPID = create( ServiceID , ServicePID ),
		MeterPID.

running_usage_test() ->
		MeterPID = create_test(),
		DateTime = { {2011,12,31}, {23,59,00} },
		Usage = 1234,
		Event = {usage, DateTime, Usage },
		ok = gen_fsm:send_event( MeterPID, Event ).
		