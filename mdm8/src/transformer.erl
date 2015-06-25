%% Copyright 2011 Richard Alexander Green
%% Description: TODO: Add description to transformer
%% Created: Jun 11, 2011
%% ------------------------------------------
-module(transformer).

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
	              , circuit
                , capacity = 120*200*1.1  % Watts
	              , history = time_series:new('Watts-hours/hour', 'Watt-hour', 3600)
	              }).



%%
%% Exported Functions
%%

%%
%% API Functions
%%



%%
%% Local Functions
%%

% Respond to service-usage notice.
do( { service_S_reports_usage_U, ServiceID, Usage }, State ) ->
		check_for_overload( Usage, State ),
		send_service_usage_to_my_circuit( { service_S_reports_usage_U, ServiceID, Usage }, State ),
		NewState = add_usage_into_my_usage_history(  { service_S_reports_usage_U, ServiceID, Usage }, State ),
		NewState;

%
do( Action, State ) ->
		exit( {'does not expect Action', Action}),
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


check_for_overload( {load, Date, Time, Sum}, State ) ->
		if Sum > State#state.capacity ->
					 TransformerID = State#state.id,
					 Message = { transformer_T_reports_excess_load_L, TransformerID,  {load, Date, Time, Sum} },
					 messenger:send_message_to_actor( Message, ?SYSTEM_MONITOR );
			 true ->
					 otherwise_do_nothing
			 end,
		ok.

add_usage_into_my_usage_history(  { service_S_reports_usage_U, ServiceID, Usage }, State ) ->
		History = State#state.history,
		{ Date, Time, Value } = Usage,
		NewHistory = time_series:put( Date, Time, Value, History ),
		NewState = State#state{ history = NewHistory },	
		NewState.

send_service_usage_to_my_circuit( { service_S_reports_usage_U, ServiceID, Usage }, State ) ->
		ServiceID = State#state.id,
		CircuitID = State#state.circuit,
		messenger:send_message_to_actor( { service_S_reports_usage_U, ServiceID, Usage }, CircuitID ),
		ok.
