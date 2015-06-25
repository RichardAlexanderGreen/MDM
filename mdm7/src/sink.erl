%%% Copyright 2011 Richard Alexander Green
%%% Description: Sink is an abstract type.
%%% - A sink might be a service, transformer, or circuit.
%%% - Sink attributes include upstream-aggregator, reporting-threshold, and usage-history.
%%% On each tick:
%%% - Add given usage into my history.
%%% - If usage exceeds my reporting threshold,
%%% -  report the over-load to System Monitor.
%%% - Forward usage to my upstream aggregator.
%%% 
%%% Created : Jun 3, 2011
%%% -------------------------------------------------------------------
-module(sink).

-behaviour(gen_event).    % This module is an event handler -- See handle_event/2

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

% TODO: Figure out how to remove this hard-coded system-monitor reference.

-define( SYSTEM_MONITOR, {system, 0} ).  % Must match define in simulator

-define( NOTIMPLEMENTED, exit("function is not implemented yet.") ).

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

-record( state, { id
                , aggregator
	              , reporting_threshold = 32*1000
	              , history = orddict:new()
	              } ).

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
init( [Sink_ID, ReportingThreshold, Aggregator, SystemMonitor] ) ->
		%?debugVal([Sink_ID, ReportingThreshold, Aggregator, SystemMonitor]),
		
		{ok, #state{ id = Sink_ID
							 , reporting_threshold = ReportingThreshold
							 , aggregator = Aggregator 
							 }};

init([]) ->
		exit("Should not initialize without parameters"),
    {ok, #state{}}.

%% --------------------------------------------------------------------
%% Func: handle_event/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler
%% --------------------------------------------------------------------
handle_event( {tick,Seconds}, State) ->
		SinkID = State#state.id, ?debugVal( {tick, SinkID ,Seconds} ),
		NewState = on_tick( Seconds, State ),
    {ok, NewState};

handle_event( {usage, Seconds, Usage}, State ) ->
		SinkID = State#state.id,	?debugVal( {usage, Seconds, Usage, SinkID}),
		respond_to_usage( Seconds, Usage, State ),
		
		{ok, State };

		
handle_event(Event, State) ->
		SinkID = State#state.id,	
		exit( {"sink:handle_event does not expect", Event, SinkID } ),
    {ok, State}.

%% --------------------------------------------------------------------
%% Func: handle_call/2
%% Returns: {ok, Reply, State}                                |
%%          {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%          {remove_handler, Reply}
%% --------------------------------------------------------------------
handle_call_suppressed( {usage, Seconds, Usage }, State ) ->
		SinkID = State#state.id,	?debugVal( {usage, Seconds, Usage, SinkID }),
		
		{ Sum, NewState } = sink:respond_to_usage( Seconds, Usage, State ),
		{ ok, Sum, NewState }.


handle_call( { over_load_at, SinkID }, State ) ->
		?debugVal( { over_load_at, SinkID } ),
		Reply = ok,
		{ ok, Reply, State };


handle_call(Request, State) ->
		SinkID = State#state.id,	
		?debugVal( { handle_call, Request, SinkID }),
		exit( {"sink:handle_call does not expect", Request, SinkID } ),
    Reply = ok,
    {ok, Reply, State}.

%% --------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler
%% --------------------------------------------------------------------
handle_info(Info, State) ->
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
% On tick:
% - If I am a service (look in ID):
% - - Generate randomized usage and call my sink:rerespond_to_usage(Usage, State).
% - In any case:
% - - Generate random glitches.
on_tick( Seconds, State ) ->
		Type = get_type( State ),
		State2 = case Type of 
								 service -> 
										 Usage = generate_randomized_usage(State), 
										 %respond_to_usage( Seconds, Usage, State );
										 event_mgr:send_event_to_all_handlers({usage,Seconds,Usage}),
										 State;
								 
								 _ ->
										 generate_random_glitch( State )
						 end,
		NewState = State2.

get_type( State ) ->
		SinkID = State#state.id,
		Type = element(1,SinkID),
		Type.

generate_randomized_usage( State ) ->
		random:uniform(31*1000).

generate_random_glitch( State ) ->
		%?NOTIMPLEMENTED,                   % TODO: Implement randomized glitches after stuff is working.
		State.


% Add given usage into my history.
% If usage exceeds my reporting threshold,
%   report the over-load to System Monitor.
% Forward usage to my upstream aggregator.
respond_to_usage( Seconds, Usage, State ) ->
		{ Sum, State2 } = add_usage_into_history( Seconds, Usage, State ),
		check_for_over_load( Sum, State2 ),
		%forward_usage_to_my_aggregator( Seconds, Usage, State ),
		{ Sum, State2 }.

% Add given usage into my history.
add_usage_into_history( Seconds, Usage, State ) ->
		% Helper module sinkHistorian separates out the persistence logic
		{ Sum, NewState } = sink_historian:add( State, Seconds, Usage ),
		{ Sum, NewState }.


check_for_over_load( Sum, State ) when Sum < State#state.reporting_threshold ->
		ok;

check_for_over_load( Sum, State ) when Sum >= State#state.reporting_threshold ->
		% Notify System Monitor
		SinkID = State#state.id,
		Notice = { over_load_at, SinkID },
		%Result = event_mgr:call_handler( { sink, ?SYSTEM_MONITOR }, Notice ),
		Result = 'not implemented',
		Result.
		
forward_usage_to_my_aggregator( Seconds, Usage, State ) ->
		Aggregator = State#state.aggregator,
		Result = event_mgr:call_handler( {sink,Aggregator}, { usage, Seconds, Usage }),
		%Result = 'not implemented',
		
		Result.

