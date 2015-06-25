% Copyright Richard Alexander Green 2011
% Created: Mar 2, 2011
% Description: Multiple actors can reference the same simlated date-time.
% - Typically, another module will set a starting time using clock_set( Date, Time )
% and then that module will advance the clock using clock_advance( day ) or clock_advance( Seconds )
% and send a tick event to other simulated actors.
% The tick event might be {tick, Seconds } and the receiver can call clock_set( Seconds ) to set their local time.
%
-module(sim_clock).

% -------------------------------------------------------------------------------------
% Include files
%

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

% -------------------------------------------------------------------------------------
% Exported Functions
%
-export([ clock_advance/1
				, clock_datetime/0
				, clock_day_type/0
				, clock_seconds/0
				, clock_set/1
				, clock_set/2
				, clock_yyyymmdd/0 ]).

% -------------------------------------------------------------------------------------
% API Functions
%

clock_set( Date, Time ) ->
		Seconds = calendar:datetime_to_gregorian_seconds( {Date, Time} ),
		clock_set( Seconds ).

clock_set( Seconds ) ->
		put( seconds, Seconds ).
		

clock_seconds() ->
		get( seconds ).

clock_yyyymmdd() ->
		{Date,_Time} = calendar:gregorian_seconds_to_datetime( get(seconds) ),
		{Y,M,D} = Date,
		YMD = D + 100*M + 10000*Y,
		YMD.


clock_datetime() ->
		{Date,Time} = calendar:gregorian_seconds_to_datetime( get(seconds) ),
		{Date,Time}.
		

clock_advance( day ) ->
		put( seconds, get(seconds) + 24*60*60 );
clock_advance( Seconds ) ->
		put( seconds, get(seconds) + Seconds ).

clock_day_type() ->
		{Date,Time} = clock_datetime(),
		case calendar:day_of_the_week(Date) of
				1 -> weekday;
				2 -> weekday;
				3 -> weekday;
				4 -> weekday;
				5 -> weekday;
				6 -> weekend;
				7 -> weekend;
				_ ->
						error_logger:error_report({logic_error, 'bad day of week number'})
		end.

% -------------------------------------------------------------------------------------
% Local Functions
%
				

% ==============================  TESTS  ============================== 

clock_set_test() ->
		Date = {2001,01,01},
		Time = {0,0,0},
		clock_set( Date, Time ),
		Seconds = calendar:datetime_to_gregorian_seconds({Date, Time}),
		?assertEqual( Seconds, clock_seconds() ).
 
clock_datetime_test() ->
		Date = {2001,01,01},
		Time = {0,0,0},
		?assertEqual( {Date,Time}, clock_datetime() ).

clock_day_type_test() ->
		?assertEqual( weekday, clock_day_type() ).

clock_advance_test() ->
		clock_advance( day ),
		?assertEqual( {{2001,01,02},{00,00,00}}, clock_datetime() ).

clock_weekend_test() ->
		clock_set( {2001,01,06}, {00,00,00} ),
		?assertEqual( weekend, clock_day_type() ).
