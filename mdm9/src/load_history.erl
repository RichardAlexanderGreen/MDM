%%% Copyright 2011 Richard Alexander Green
%%% Description: Record load-history at a point in the network.
%%% Created: Jun 13, 2011
%%% ------------------------------------------
-module(load_history).

%%
%% Include files
%%
-define( TEST, true ).
-define( NODEBUG, true ).

% Set log level
% ALL=0 < TRACE=1 < DEBUG=2 < INFO=3 < WARN=4 < ERROR=5 < FATAL=6 < OFF=7
% Level semantics are defined in the org.apache.log4j.Level class.
-define( LOGLEVEL, 3 ).             % 3 means log info level and above. -- See log/2.

-include_lib("eunit/include/eunit.hrl").

-ifdef( DEBUG ).
-compile( export_all ).
-endif.

%%
%% Exported Functions
%%
-export([]).

% This is a helper function that holds functionality used by various "load_point" components
%% --record( state, - - -) declaration must match in circuit / transformer / service.
% Use the same -record( state, - - - ) in service / transformer / circuit and load_history modules.
-include("../include/load_point_state.hrl").

%%
%% API Functions
%%

sum_usage_for_interval( Usage, Date, Time, History ) ->
		MyHistory = case History of
										undefined ->
												orddict:new();
										_ ->
												History
								end,				
		Key = { Date, Time },
		PriorValue = case orddict:find(Key, MyHistory) of
										 error ->
												 0;
										 X ->
												 X
								 end,
		Sum = PriorValue + Usage,
		NewHistory = orddict:store( Key, Sum, MyHistory ),
		{ Sum, NewHistory }.
		
put_usage_for_interval( Usage, Date, Time, History ) ->
		MyHistory = case History of
										undefined ->
												orddict:new();
										_ ->
												History
								end,			
		Key = { Date, Time },
		Value = Usage,
		NewHistory = orddict:store( Key, Value, MyHistory ),
		NewHistory.

get_usage_for_interval( Date, Time, History ) ->
		Key = { Date, Time },
		Usage = case orddict:find(Key, History) of
								{ ok, Value } ->
										Value;
								error ->
										undefined
						end,
		Usage.

		
tick( { tick, CurrentDateTime }, State ) ->
		NewState = when_date_turns_over( CurrentDateTime, State ),
		NewState.
		
% When interval turns over, send sum from prior interval to my aggregator.
when_interval_turns_over( CurrentDateTime, State ) when CurrentDateTime =/= State#state.priorDateTime ->
		exit("not implemented yet").

% When date turns over, send day`s history to usage_logger.
when_date_turns_over( CurrentDateTime, State ) ->
		{ CurrentDate, CurrentTime } = CurrentDateTime,
		#state{ priorDateTime = { PriorDate, PriorTime } } = State,
		when_date_turns_over(  CurrentDate, CurrentTime, PriorDate, PriorTime, State ).

when_date_turns_over(  CurrentDate, CurrentTime,  PriorDate, PriorTime, State ) when CurrentDate =/= PriorDate ->
		% Send history to the logger.
		#state{ id = MyID, history = History } = State,
		PriorDateTime = { PriorDate, PriorTime },
		Key = { MyID, PriorDateTime },
		Value = History,
		usage_logger:log_usage( Key, Value ),
		NewState = State#state{ history = undefined, priorDateTime = { CurrentDate, CurrentTime } },
		NewState;

when_date_turns_over(  Date, Time,  PriorDate, PriorTime, State ) ->
	  %when CurrentDateTime == State#state.priorDateTime ->
		State.

% //////////////////////////////////// TESTS //////////////////////////////////// 
put_usage_for_interval_test() ->
		{ Date, Time } = { {2011,12,31}, {23,30,00} },
		History = undefined,
		Usage = 1234,
		NewHistory = put_usage_for_interval( Usage, Date, Time, History ),
		?debugVal( NewHistory ),
		NewHistory.

get_usage_for_interval_test() ->
		{ Date, Time } = { {2011,12,31}, {23,30,00} },
		History = put_usage_for_interval_test(),
		Usage = 1234,
		Result = get_usage_for_interval( Date, Time, History ),
		?assertEqual( Usage, Result ).
		
