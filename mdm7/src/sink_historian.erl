%% Copyright 2011 Richard Alexander Green
%% Description: TODO: Add description to sink_historian
%% Created: Jun 5, 2011
%% ------------------------------------------
-module(sink_historian).

%%
%% Include files
%%

-define( NOTIMPLEMENTED, exit("function not implemented")  ).

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


% Following must match sink`s state definition.
-record( state, { id
                , aggregator
	              , reporting_threshold = 32*1000
	              , history = orddict:new()
	              } ).

%%
%% Exported Functions
%%
-export([]).

%%
%% API Functions
%%

% Helper function -- add the usage into the sink`s usage history.
add( State, Seconds, Usage ) ->
		SinkID = State#state.id,
		?debugVal( { SinkID, Seconds, Usage } ),
		% TODO: Smarter version must interact with file storage
		History = State#state.history,
		{ NewHistory, Sum } = add_usage_into_slot( History, Seconds, Usage ),
		NewState = State#state{ history = NewHistory },
		{ Sum, NewState }.


%%
%% Local Functions
%%

add_usage_into_slot( History, Seconds, Usage ) ->
		% Create slot-key.
		{ Date, Time } = calendar:gregorian_seconds_to_datetime(Seconds),
		DayOfWeek = calendar:day_of_the_week(Date),
		{ Hour, _Minute, _Second } = Time,
		HourOfWeek = Hour + ( DayOfWeek * 7 ) - 6,
		
		% Get prior usage (if any) from the slot
    Result = orddict:find( HourOfWeek, History ),
		Sum = case Result of
							{ok,Value} ->
									Value + Usage;
							error ->
									Usage;
							_ ->
									exit("Unexpected result from call to orddict")
					end,
		% Now update the series.
		NewHistory = orddict:store(HourOfWeek, Sum, History),
		% Return updated history and sum
		{ NewHistory, Sum }.