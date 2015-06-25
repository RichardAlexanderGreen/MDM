%%% -------------------------------------------------------------------
% Copyright 2011 Richard Alexander Green
%%% Description : Simulate a bunch of meters sending usage data messages.
%%%
%%% Created : Apr 15, 2011
%%% -------------------------------------------------------------------
-module(meters).

-behaviour(gen_server).
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

-include( "../include/objects.hrl" ).


-import( object_factory, [make/1] ).


%% --------------------------------------------------------------------
%% External exports
-export([ send_usage_to_service/2 ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, { meters = [] }).

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
init( Meters ) ->
		process_flag( trap_exit, true ),
    {ok, #state{ meters = Meters }}.


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
		?debugVal( {Request, From, State} ),
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast( Msg, State ) ->
		?debugVal( { Msg, State} ),
		NewState = do( Msg, State ),
    {noreply, NewState}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info( Info, State ) ->
		?debugVal( { Info, State} ),
		NewState = do( Info, State ),
    {noreply, NewState}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
		?debugVal( {terminate, Reason, State } ),
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

do( {tick, GregSeconds }, State ) ->
		% Send usage for all my meters
		Meters = State#state.meters,
		send_usage( Meters ),
		State.

send_usage( [] ) ->
		done;
send_usage( Meters ) ->
		[ Meter | RemainingMeters ] = Meters,
		ServiceID = Meter#meter.service_ID,
		Usage = simulate_demand( ServiceID ),
		send_usage_to_service( Usage, ServiceID ),
		send_usage( RemainingMeters ).

simulate_demand( ServiceID ) ->
		Start = calendar:datetime_to_gregorian_seconds( calendar:universal_time() ),
		Stop = Start + (60*60),
		Usage = 1234,              % TODO -- Calculate usage based on service's load-profile
		#usage{ quantity = Usage
					, unit_of_measure = 'Watt hours'
					, time_period = #time_period{ start_seconds = Start, stop_seconds = Stop} 
					}.

send_usage_to_service( Usage, ServiceID ) ->
		%?debugVal( {send_usage_to_service, Usage, ServiceID } ),
		
		%service() ! #service_usage{ service_ID = ServiceID, usage = Usage },
		%DirectResult = services:usage( #pod_usage{ pod_ID = ServiceID, usage = Usage } ),
		%?debugVal( DirectResult ),
		
		gen_server:cast( { global, services }, #pod_usage{ pod_ID = ServiceID, usage = Usage } ),
		ok.

service() ->
		{ global, services }.


% ========================== TESTS ===================================
set_up_test() ->
		Circuit1     = object_factory:make( circuit ),
		Transformer1 = object_factory:transformer_on_circuit( Circuit1 ),
		Service1     = object_factory:service_on_transformer( Transformer1 ),
		Meter1       = object_factory:meter_on_service( Service1 ),
		#state{ meters = [Meter1] }.

tick_test() ->
		State = set_up_test(),
		GregSeconds = calendar:datetime_to_gregorian_seconds( calendar:universal_time() ),
		NewState = do( {tick, GregSeconds} , State ).

