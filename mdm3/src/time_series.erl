%%% Copyright Richard Alexander Green 2011
%%% Created: Apr 25, 2011  (version 0.3 of this module)
%%% Description: A time-series is a X-Y curve where the X values map to points in time.
%%% - The strategy for this implementation is to represent X values as gregorian-seconds.
%%% - A time-series also has a semantic-type.
%%% The semantic-type defines the unit-of-measure and the method for generating or collecting the data.
%%% The time-series curve is organized as a simple orddict.

-module(time_series).

%%%
%%% Include files
%%%

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

%%%
%%% Exported Functions
%%%
-export([add/2, subtract/2, put_into/2, multiply/4, scale/4, divide/4, new/2, new/3, put/3, put/4, put_day/3, extract/3, extract/5]).

-record( series, { series_type               % Type must be same for add / subtract
								 , interval_seconds = 3600   % interval seconds must be same for add / substract / multiple / divide
								 , unit_of_measure           % Unit of measure must be same for add / subtract
								 , curve = orddict:new() 
								 } ).

%%%
%%% API Functions
%%%

%% Defaults interval seconds to 3600 (hourly intervals)
new( SeriesType, UnitOfMeasure ) ->
		#series{ series_type = SeriesType
           , unit_of_measure = UnitOfMeasure 
           , curve = orddict:new() }.
new( SeriesType, UnitOfMeasure, IntervalSeconds ) ->
		#series{ series_type = SeriesType
					 , interval_seconds = IntervalSeconds
           , unit_of_measure = UnitOfMeasure 
           , curve = orddict:new() }.

%% Insert the given {date-time, value} and return updated series.
%% If a value pre-exists at that time-ordinate, it is replaced.
put( Date, Time, Value, Series ) ->
		GregorianSeconds = calendar:datetime_to_gregorian_seconds({Date, Time}),
		NewSeries = put( GregorianSeconds, Value, Series ),
		NewSeries.

%% Insert the given { gregorian-seconds, value} and return updated series.
%% If a value pre-exists at that time-ordinate, it is replaced.
put( GregorianSeconds, Value, Series ) ->
		?assertEqual( 0, GregorianSeconds rem Series#series.interval_seconds ),
		0 = GregorianSeconds rem Series#series.interval_seconds,                   % Force exception if we get past macro
		
		OldCurve = Series#series.curve,
		NewCurve = orddict:store( GregorianSeconds, Value, OldCurve ),
		NewSeries = Series#series{ curve = NewCurve },
		NewSeries.   

%% Insert values corresponding for the given date's intervals.
put_day( Date, Values, Series ) ->
		IntervalSize = Series#series.interval_seconds,
		SecondsFirst = calendar:datetime_to_gregorian_seconds({Date, {00,00,IntervalSize} } ),
		SecondsLast = SecondsFirst + (24*60*60) - IntervalSize,
		SeriesOut = put_from_to( SecondsFirst, SecondsLast, Values, Series ),
		SeriesOut.


put_from_to( SecondsLast, SecondsLast, Values, SeriesIn ) when length(Values) == 1-> % One last insert.
		[Value] = Values,
		SeriesOut = put( SecondsLast, Value, SeriesIn ),
		SeriesOut;

put_from_to( SecondsLast, SecondsLast, Values, _SeriesIn ) when length(Values) > 1-> % Error
		exit( "Too many values in call to put_from_to/4");

put_from_to( _SecondsFirst, _SecondsLast, [], _SeriesIn  ) ->
		exit( "Too few values in call to put_from_to/4");

put_from_to( SecondsFirst, SecondsLast, Values, SeriesIn ) ->
		%?debugVal({put_from_to, SecondsFirst, SecondsLast, length(Values), length(SeriesIn#series.curve) }),
		
		[Value | RemainingValues ] = Values,
		SeriesOut = put( SecondsFirst, Value, SeriesIn ),
		IntervalSize = SeriesIn#series.interval_seconds,
		put_from_to( SecondsFirst+IntervalSize, SecondsLast, RemainingValues, SeriesOut ).

		

%% Extract the curve between [Start, End] inclusive.
extract( StartSeconds, EndSeconds, Series) ->
		SubSeries = orddict:filter(fun( Seconds, _Value ) -> ( Seconds >= StartSeconds) and (Seconds =< EndSeconds ) end
																		, Series ),
		SubSeries.

extract( Date1, Time1, Date2, Time2, Series ) ->
		Seconds1 = calendar:datetime_to_gregorian_seconds( Date1, Time1 ),
		Seconds2 = calendar:datetime_to_gregorian_seconds( Date2, Time2 ),
		SubSeries = extract( Seconds1, Seconds2, Series ),
		SubSeries.
		
%% Add SeriesB to SeriesA giving a series of the same type and unit-of-measure.
%% If keys do not match, the value is simply merged into the result - which is what we want in this case.
add( SeriesA, SeriesB ) ->
  true = check_compatibility( SeriesA, SeriesB ),         % Assert
	CurveC = orddict:merge( fun(_Key,A,B) -> A + B end
                         , SeriesA#series.curve
                         , SeriesB#series.curve
                         ),
	SeriesC = #series{ series_type     = SeriesA#series.series_type
									 , unit_of_measure = SeriesA#series.unit_of_measure 
									 , curve = CurveC },
	SeriesC.
		
%% Subtract SeriesB from SeriesA giving a series of the same type and unit-of-measure.
subtract( SeriesA, SeriesB ) ->
	true = check_compatibility( SeriesA, SeriesB ),         % Assert
	SeriesNegated = negate( SeriesB ),
	SeriesC = add( SeriesA, SeriesNegated ),
	SeriesC.

%% Put SeriesA into SeriesB - replacing any prior entries
put_into( SeriesA, SeriesB ) ->
   true = check_compatibility( SeriesA, SeriesB ),
   CurveC = orddict:merge( fun(_Key, A, _B ) -> A end
                         , SeriesA#series.curve
                         , SeriesB#series.curve
                         ),
   SeriesC = #series{ series_type     = SeriesA#series.series_type
									 , unit_of_measure = SeriesA#series.unit_of_measure 
									 , curve = CurveC },
	 SeriesC.


%% Negate Series - Flip the curve up-side-down.
%% (Subtract uses this to subtract one series from another by adding the negated series.)
negate( SeriesIn ) ->
  CurveOut = orddict:map( fun(_Key,Value) -> -Value end
                         , SeriesIn#series.curve
                         ),
	SeriesOut = #series{ series_type     = SeriesIn#series.series_type
									   , unit_of_measure = SeriesIn#series.unit_of_measure 
									   , curve = CurveOut },
	SeriesOut.

%% Multiply SeriesA by SeriesB giving a new series with a new SeriesType and UnitOfMeasure
multiply( SeriesA, SeriesB, SeriesType, UnitOfMeasure )  ->
	?assert( SeriesA#series.interval_seconds == SeriesB#series.interval_seconds  ),  % Interval length must match
	true =	( SeriesA#series.interval_seconds == SeriesB#series.interval_seconds ),  % Exit if it does not

	CurveA = SeriesA#series.curve,
	CurveB = SeriesB#series.curve,
	CurveC = orddict:fold(fun(Key, Value, AccIn) -> 
									           case orddict:find(Key, CurveB) of
													      error ->
															     AccIn;
													      { ok, Value2 } ->
															     AccIn ++ [{ Key, Value * Value2 }]
													 
												     end 
                        end
				          , [], CurveA ),   % SeriesA drives the output -- if A is empty or B has no matches, result is empty.
	%?debugVal( CurveA ),
	%?debugVal( CurveB ),
	%?debugVal( CurveC ),
	SeriesC = #series{ series_type = SeriesType
									 , unit_of_measure = UnitOfMeasure 
									 , curve = CurveC },
	SeriesC.
	
%% Divide SeriesA by SeriesB giving a new series with a new SeriesType and UnitOfMeasure
divide( SeriesA, SeriesB, SeriesType, UnitOfMeasure ) ->
	?assert( SeriesA#series.interval_seconds == SeriesB#series.interval_seconds  ),  % Interval length must match
	true =	( SeriesA#series.interval_seconds == SeriesB#series.interval_seconds ),  % Exit if it does not

	CurveA = SeriesA#series.curve,
	CurveB = SeriesB#series.curve,
	CurveC = orddict:fold(fun(Key, Value, AccIn) -> 
									           case orddict:find(Key, CurveB) of
													      error ->
															     AccIn;
													      { ok, Value2 } ->
															     AccIn ++ [{ Key, Value / Value2 }]
													 
												     end 
                        end
				          , [], CurveA ),   % SeriesA drives the output -- if A is empty or B has no matches, result is empty.
	%?debugVal( CurveA ),
	%?debugVal( CurveB ),
	%?debugVal( CurveC ),
	SeriesC = #series{ series_type = SeriesType
									 , unit_of_measure = UnitOfMeasure 
									 , curve = CurveC },
	SeriesC.
			

%% Multiply SeriesA by scalar-factor giving a new series with a new SeriesType and UnitOfMeasure
scale( SeriesA, Factor, SeriesType, UnitOfMeasure ) ->
  CurveC = orddict:map( fun(_Key,Value) -> Value * Factor end
                         , SeriesA#series.curve
                         ),
	SeriesC = #series{ series_type = SeriesType
									 , unit_of_measure = UnitOfMeasure 
									 , curve = CurveC },
	SeriesC.


%% Check that two series can be added or subtracted without conversion.
check_compatibility( SeriesA, SeriesB ) ->
		?assertEqual( SeriesA#series.unit_of_measure
		            , SeriesB#series.unit_of_measure ),
		?assertEqual( SeriesA#series.series_type
		            , SeriesB#series.series_type ),
		?assertEqual( SeriesA#series.interval_seconds
		            , SeriesB#series.interval_seconds ),
		( SeriesA#series.unit_of_measure == SeriesB#series.unit_of_measure )
		and ( SeriesA#series.interval_seconds == SeriesB#series.interval_seconds )
		and ( SeriesA#series.series_type == SeriesB#series.series_type ).

%% Convert series to a new series with a different unit of measure.
convert( Series, SeriesType, UnitOfMeasure ) ->
	Factor = conversion_factor( Series#series.unit_of_measure, UnitOfMeasure ),
	SeriesOut = scale( Series, Factor, SeriesType, UnitOfMeasure ),
	SeriesOut.

conversion_factor( UnitOfMeasureA, UnitOfMeasureB ) ->
		Factor = orddict:fetch( { UnitOfMeasureA, UnitOfMeasureB }, factors() ),
		Factor.
		
%%%
%%% Local Functions
%%%

factors() ->
		Table = orddict:from_list(
		[ { {'Watt-hour','kiloWatt-hour'}, 1.0 / 1000.0 }
		, { {'kiloWatt-hour','Watt-hour'}, 1000.0 / 1.0 }
		, { {'Watt-hour','Watt-second'}, 3600.0 / 1.0 }
		, { {'Watt-second','Watt-hour'}, 1.0 / 3600.0 }
		, { {'Watt','kiloWatt'}, 1.0 / 1000.0 }
		, { {'kiloWatt','Watt'}, 1000.0 / 1.0 } 
		] ),
		Table.

%%% ===============================  TESTS  ===================================

new_test() ->
		SeriesType = 'Daily Watt-hour Register Values',
		UnitOfMeasure = 'Watt-hour',
		Result = new( SeriesType, UnitOfMeasure ),
		?assertEqual( #series{ series_type = SeriesType
												 , unit_of_measure = UnitOfMeasure
												 , curve = orddict:new() }
		            , Result ),
		ok.

new_15_minute_test() ->
		SeriesType = 'Daily Watt-hour Register Values',
		UnitOfMeasure = 'Watt-hour',
		Result = new( SeriesType, UnitOfMeasure, 900 ),
		?assertEqual( #series{ series_type = SeriesType
												 , interval_seconds = 900
												 , unit_of_measure = UnitOfMeasure
												 , curve = orddict:new() }
		            , Result ),
		ok.

put_test() ->
		SeriesType = 'Daily Watt-hour Register Values',
		UnitOfMeasure = 'Watt-hour',
		Series = new( SeriesType, UnitOfMeasure ),
		Date = {2011,12,31},
		Time = {24,00,00},
		Value = 54321,
		NewSeries = put( Date, Time, Value, Series ),
		%?debugVal( NewSeries ),
		?assertEqual( {series,'Daily Watt-hour Register Values',3600,'Watt-hour', [{63492595200,54321}]}
		            , NewSeries ),
		NewSeries.

put_fails_when_bad_time_test() ->
		SeriesType = 'Daily Watt-hour Register Values',
		UnitOfMeasure = 'Watt-hour',
		Series = new( SeriesType, UnitOfMeasure ),
		Date = {2011,12,31},
		Time = {23,59,59},          % One second short of the 3600 interval mark.
		Value = 54321,
		?assertError( {_,_}, put( Date, Time, Value, Series ) ), % As long as it fails, I don't care how! 
		
		IntervalSeconds = 900, 
		Series2 = new( SeriesType, UnitOfMeasure, IntervalSeconds ),
		?assertError( {_,_}, put( Date, Time, Value, Series2 ) ), % As long as it fails, I don't care how!
		ok.
		
scale_test() ->
		SeriesA = put_test(),
		Factor = 3,
		SeriesType = 'Scaled By 3',
		UnitOfMeasure = 'Funky Units',
		ScaledSeries = scale( SeriesA, Factor, SeriesType, UnitOfMeasure ),
		%?debugVal( ScaledSeries ),
		?assertEqual( {series,'Scaled By 3',3600,'Funky Units',[{63492595200,162963}]}
                 , ScaledSeries ),
		ok.

add_test() ->
		SeriesA = put_test(),
		SeriesB = put_test(),
		SeriesC = add( SeriesA, SeriesB ),
		%?debugVal( SeriesC ),
		?assertEqual( {series,'Daily Watt-hour Register Values',3600,'Watt-hour', [{63492595200,54321+54321}] }
                , SeriesC ),
		ok.

add_fails_mismatched_interval_test() ->
		SeriesA = put_test(),
		SeriesX = put_test(),	
    SeriesB = SeriesX#series{ interval_seconds = 900 },
		?assertError( {_,_}, add( SeriesA, SeriesB ) ).

add_fails_mismatched_units_test() ->
		SeriesA = put_test(),
		SeriesX = put_test(),	
    SeriesB = SeriesX#series{ unit_of_measure = none },
		?assertError( {_,_}, add( SeriesA, SeriesB ) ).

add_fails_mismatched_type_test() ->
		SeriesA = put_test(),
		SeriesX = put_test(),	
    SeriesB = SeriesX#series{ series_type = none },
		?assertError( {_,_}, add( SeriesA, SeriesB ) ).
	

subtract_test() ->
		SeriesA = put_test(),
		SeriesB = put_test(),
		SeriesC = subtract( SeriesA, SeriesB ),
		%?debugVal( SeriesC ),
		?assertEqual( {series,'Daily Watt-hour Register Values',3600,'Watt-hour', [{63492595200,0}] }
                , SeriesC ),
		ok.

subtract_fails_mismatched_interval_test() ->
		SeriesA = put_test(),
		SeriesX = put_test(),	
    SeriesB = SeriesX#series{ interval_seconds = 900 },
		?assertError( {_,_}, subtract( SeriesA, SeriesB ) ).

subtract_fails_mismatched_units_test() ->
		SeriesA = put_test(),
		SeriesX = put_test(),	
    SeriesB = SeriesX#series{ unit_of_measure = none },
		?assertError( {_,_}, subtract( SeriesA, SeriesB ) ).

subtract_fails_mismatched_type_test() ->
		SeriesA = put_test(),
		SeriesX = put_test(),	
    SeriesB = SeriesX#series{ series_type = none },
		?assertError( {_,_}, subtract( SeriesA, SeriesB ) ).

subtract_when_SeriesB_extends_test() ->
		SeriesType = 'Daily Watt-hour Register Values',
		UnitOfMeasure = 'Watt-hour',
		SeriesA = time_series:put( {2011,12,30}, {24,00,00}, 1234, new( SeriesType, UnitOfMeasure ) ),
		SeriesB = time_series:put( {2011,12,31}, {24,00,00}, 1234, new( SeriesType, UnitOfMeasure ) ),
		SeriesC = time_series:subtract(SeriesA, SeriesB),
		?assertEqual({series,'Daily Watt-hour Register Values',3600,'Watt-hour', [{63492508800,1234},{63492595200,-1234}] }
                , SeriesC ),
		ok.
		

multiply_test() ->
		VoltSeries = put( {2011,12,31}, {24,00,00}, 120, new( 'RMS Voltage', 'Volts' ) ),
		AmpSeries  = put( {2011,12,31}, {24,00,00}, 50, new( 'RMS Amperage', 'Amps' ) ),
		WattSeries = multiply( VoltSeries, AmpSeries, 'RMS Watts', 'Watt' ),
		%?debugVal( WattSeries ),
		?assertEqual( {series,'RMS Watts',3600,'Watt',[{63492595200,6000}]}, WattSeries ),
		ok.

multiply_mismatch_test() ->
		VoltSeries = put( {2011,12,30}, {24,00,00}, 120, new( 'RMS Voltage', 'Volts' ) ),
		AmpSeries  = put( {2011,12,31}, {24,00,00},  50, new( 'RMS Amperage', 'Amps' ) ),
		WattSeries = multiply( VoltSeries, AmpSeries, 'RMS Watts', 'Watt' ),
		%?debugVal( WattSeries ),
		?assert( {series,'RMS Watts',3600,'Watt',[{63492595200,6000}]} =/= WattSeries ),
		ok.

multiply_fails_mismatched_interval_test() ->
		VoltSeries = put( {2011,12,30}, {24,00,00}, 120, new( 'RMS Voltage', 'Volts' ) ),
		AmpSeries  = put( {2011,12,31}, {24,00,00},  50, new( 'RMS Amperage', 'Amps', 900 ) ),
		?assertError( {_,_}, multiply( VoltSeries, AmpSeries, 'RMS Watts', 'Watt' ) ).

divide_test() ->
		WattSeries = put( {2011,12,31}, {24,00,00}, 6000, new( 'RMS Watts', 'Watts' ) ),
		VoltSeries = put( {2011,12,31}, {24,00,00},  120, new( 'RMS Voltage', 'Volts' ) ),
		AmpSeries = divide( WattSeries, VoltSeries, 'RMS Amperage', 'Amps' ),
		%?debugVal( AmpSeries ),
		?assertEqual( {series,'RMS Amperage',3600, 'Amps',[{63492595200,50.0}]}, AmpSeries ),
		ok.

divide_mismatch_test() ->
		WattSeries = put( {2011,12,30}, {24,00,00}, 6000, new( 'RMS Watts', 'Watts' ) ),
		VoltSeries = put( {2011,12,31}, {24,00,00},  120, new( 'RMS Voltage', 'Volts' ) ),
		AmpSeries = divide( WattSeries, VoltSeries, 'RMS Amperage', 'Amps' ),
		%?debugVal( AmpSeries ),
		?assert( {series,'RMS Amperage',3600, 'Amps',[{63492595200,50.0}]} =/= AmpSeries ),
		ok.

divide_fails_mismatched_interval_test() ->
		WattSeries = put( {2011,12,31}, {24,00,00}, 6000, new( 'RMS Watts', 'Watts' ) ),
		VoltSeries = put( {2011,12,31}, {24,00,00},  120, new( 'RMS Voltage', 'Volts', 900 ) ),
		?assertError( {_,_}, divide( WattSeries, VoltSeries, 'RMS Amperage', 'Amps' ) ).		

conversion_factor_test() ->
		?assertEqual( 1000.0, conversion_factor( 'kiloWatt-hour','Watt-hour' ) ),
		?assertEqual( 1000.0, conversion_factor( 'kiloWatt','Watt' ) ),
		?assertEqual( 1/3600, conversion_factor( 'Watt-second','Watt-hour' ) ),
		?assertEqual( 3600.0, conversion_factor( 'Watt-hour','Watt-second' ) ),
		?assertEqual( 1/1000, conversion_factor( 'Watt-hour','kiloWatt-hour' ) ),
		?assertEqual( 1/1000, conversion_factor( 'Watt','kiloWatt' ) ),
		ok.

put_day_test() ->
		Series = new( 'test series', 'Watt-hour' ),
		Date = {2011,12,31},
		Values = [    1, 2, 3, 4, 5, 6, 7, 8, 9
						 ,10,11,12,13,14,15,16,17,18,19
						 ,20,21,22,23,24 ],
		NewSeries = put_day( Date, Values, Series ),
		FirstHourSeconds = calendar:datetime_to_gregorian_seconds( { Date, {01,00,00} } ),
		?assertMatch( {series, 'test series', 3600, 'Watt-hour', [{FirstHourSeconds, 1} | _ ] }, NewSeries ).

% TODO - Write a test for convert/3

		

		
		
		
		
