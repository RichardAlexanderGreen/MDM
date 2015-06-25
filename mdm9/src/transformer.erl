%%% Copyright 2011 Richard Alexander Green
%%% Description : Simulates a transformer.
%%% Each transformer: 
%%% Events:
%%% - Acts as an event-manager for its services.
%%% - On {install - - }: Install services.
%%% - On {tick - -}: Forward the tick to services.
%%% - On {tick - -}: When date turns over, send day`s load history to logger.
%%% Messages:
%%% - {PoD_usage - - - } : Sum usage into local load history.
%%%
%%% Created : Jun 13, 2011
%%% -------------------------------------------------------------------
-module(transformer).

-behaviour(gen_event).

-define( EVENT_MANAGER, self() ).

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

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

% This is a helper function -- State must match circuit / transformer / service.
% Use the same -record( state, - - - ) in service / transformer / circuit and load_history modules.
-include("../include/load_point_state.hrl").


%% ====================================================================
%% External functions
%% ====================================================================


%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          Other
%% --------------------------------------------------------------------
init(  [ { id, TransformerID }, { services_per_transformer, NServicesPerTransformer }, {circuit = CircuitID } ] ) ->
		State = #state{ id = TransformerID, aggregator = CircuitID },
		ServiceSequence = NServicesPerTransformer,
		create_services( ServiceSequence, State ),
		{ok, State };

init([]) ->
		?debugMsg("transformer:init([])"),
		
    {ok, #state{}}.

%% --------------------------------------------------------------------
%% Func: handle_event/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler
%% --------------------------------------------------------------------

% Simulate installation
handle_event( { install_services, NServicesPerTransformer }, State ) ->
		% Create services as event-handlers where I am event-manager.
		create_services( NServicesPerTransformer, State ),
		NewState = State#state{},
		NewState;
	
% Simulate clock tick
% - On {tick - -}: Forward the tick to services / transformers.
% - On {tick - -}: When date turns over, send day`s load history to logger.
handle_event( { tick, CurrentDateTime }, State1) ->
		gen_event:notify( ?EVENT_MANAGER, { tick, CurrentDateTime } ),
		State2 = load_histry:when_interval_turns_over( CurrentDateTime, State1 ),
		State3 = load_history:when_date_turns_over( CurrentDateTime, State2 ),
		State3;

handle_event(Event, State) ->
    {ok, State}.

%% --------------------------------------------------------------------
%% Func: handle_call/2
%% Returns: {ok, Reply, State}                                |
%%          {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%          {remove_handler, Reply}
%% --------------------------------------------------------------------
handle_call( {install_one_service, ServiceSequence }, State ) ->
		?debugVal( { handle_call, install_one_service, ServiceSequence , State} ),
		
		create_one_service( ServiceSequence, State ),
		Reply = ok,
    {ok, Reply, State};

handle_call(Request, State) ->
		exit( {"call not expected -- Request:",Request} ),
    Reply = ok,
    {ok, Reply, State}.

%% --------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler
%% --------------------------------------------------------------------

% Messages:
% - {usage - - - } : Sum usage into local load history.
%                  : Forward usage to my aggregator (a Circuit).
handle_info( {usage, ServiceID, DateTime, Usage }, State ) ->
		#state{ id = MyID, aggregator = CircuitAddress, history = History } = State,   % Parse my state.
		
		% Forward usage to my aggregator (circuit).
		gen_server:cast( CircuitAddress, {usage, MyID, DateTime, Usage } ),
		
		% Sum usage into local load history.
		{ Date, Time } = DateTime,
		{ Sum, NewHistory } = load_history:sum_usage_for_interval( Usage, Date, Time, History ),
		NewState = State#state{ history = NewHistory },
		NewState;

handle_info(Info, State) ->
		exit( {"transformer does not understand: ", Info } ),
    {ok, State}.

%% --------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any
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

% Create services as event-handlers where I am event-manager.
create_services( 0, State ) ->
		done;

create_services( NServices, State ) ->
		create_one_service( NServices, State ),
		create_services( NServices - 1, State ).

create_one_service( ServiceSequence, State ) ->
		% Create the service ID by appending Sequence to transformer`s ID.
		TransformerID = State#state.id,
		ServiceID = TransformerID ++ [ ServiceSequence ],
		Handler = { service, ServiceID },
		% *** Args must match init/1 in service module. ***
		Args = [ { id, ServiceID }, { transformer, TransformerID } ] ,  
		gen_event:add_sup_handler( self(), Handler, Args ).

%% ///////////////////////// TESTS ///////////////////////// TESTS ///////////////////////// 
init_test() ->
		EventMgrName = { global, list_to_atom([01,02]) },
		Start_Result = gen_event:start( EventMgrName ),
		?debugVal( Start_Result ),
		
		{ ok, ProcessID } = Start_Result,

		ProcessID.
		
create_one_service_test() ->
		EventMgrRef = { global, list_to_atom([01,02]) },
		ServiceSequence = 1,
		%Handler = { ?MODULE, [01,02] },
		Handler = { ?MODULE, 'transformer create_one_service_test' },
		TransformerID = 'test transformer',
		NServicesPerTransformer = 1,
		CircuitID = ['test circuit'],
		Args = [ { id, TransformerID }, { services_per_transformer, NServicesPerTransformer }, {circuit, CircuitID } ],
		Result_add_handler = gen_event:add_handler(EventMgrRef, Handler, [] ),
		%?debugVal( Result_add_handler ),
		?assertEqual( ok, Result_add_handler ),
		
		Request = {install_one_service, ServiceSequence },
		Result_of_call = gen_event:call( EventMgrRef, Handler, Request),
		?debugVal( Result_of_call ).
