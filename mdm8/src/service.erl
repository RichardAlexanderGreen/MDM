%%% Copyright 2011 Richard Alexander Green
%%% Description: TODO: Add description to service
%%% - Service:
%%% - - Handle tick: 
%%% - - - Send service_usage event to transformer.
%%% - - - Insert into service (PoD) usage history.
%%% - - - Periodically (when date changes) write usage history to persistent store. 
%%% - - - (This is an attempt to reduce the overhead associated with persistence.)
%%% Created: Jun 10, 2011
%%% ------------------------------------------
-module(service).

-behavior( gen_event ).  % Event handler

%%
%% Include files
%%
-define( SYSTEM_MONITOR, {system_monitor, 0} ).


-define( NOTEST, true ).
-define( NODEBUG, true ).
-define( LOGLEVEL, 3 ).  % LOG info level and above. -- See log/2.
% ALL=0 < TRACE=1 < DEBUG=2 < INFO=3 < WARN=4 < ERROR=5 < FATAL=6 < OFF=7

-include_lib("eunit/include/eunit.hrl").

-include( "../include/actor.hrl").

-record( state, { id
	              , transformer
                , capacity = 120*200*1.1  % Watts
	              , history = time_series:new('Watts-hours/hour', 'Watt-hour', 3600)
	              }).

%%
%% API Functions
%%


%%
%% Local Functions
%%

% On tick:
% Send service_usage event to my transformer.
% Insert into service (PoD) usage history.
do( { tick, DateTime }, State ) ->
		Usage = generate_service_usage_for_interval( DateTime, State ),
		check_for_overload( Usage, State ),
		send_service_usage_to_my_transformer( Usage, State ),
		NewState = insert_usage_into_my_usage_history( Usage, State ),
		NewState;

%
do( Action, State ) ->
		State.

answer( Request, _From, _State ) ->
		?debugVal( {answer, Request, _From, _State } ),
		
		Reply = {ok},
		Reply.


custom_init( ArgList )->
		?debugVal( {trace, custom_init} ),
		
		State = #state{},
		State.

custom_shutdown( State ) ->
		?debugVal( {trace, custom_shutdown}),
		
		ok.

% ////////////////////////////////////////

% Return Usage = { Date, Time, Value }
generate_service_usage_for_interval( DateTime, State ) ->
	 MyCapacity = State#state.capacity,
	 { Date, Time } = DateTime,
	 Usage = { Date, Time, random:uniform( MyCapacity * 1.01 ) },   % Should produce service overload about 1% of time.
   Usage.

check_for_overload( Usage, State ) ->
		if Usage > State#state.capacity ->
					 ServiceID = State#state.id,
					 messenger:send_message_to_actor( { service_S_reports_overload_usage_U, ServiceID,  Usage }, ?SYSTEM_MONITOR );
			 true ->
					 otherwise_do_nothing
			 end,
		ok.

insert_usage_into_my_usage_history( Usage, State ) ->
		History = State#state.history,
		{ Date, Time, Value } = Usage,
		NewHistory = time_series:put( Date, Time, Value, History ),
		NewState = State#state{ history = NewHistory },	
		NewState.

send_service_usage_to_my_transformer( Usage, State ) ->
		ServiceID = State#state.id,
		TransformerID = State#state.transformer,
		messenger:send_message_to_actor( { service_S_reports_usage_U, ServiceID, Usage }, TransformerID ),
		ok.
