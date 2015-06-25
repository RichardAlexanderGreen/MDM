%% Copyright Richard Alexander Green 2011
%% Created: Apr 4, 2011
%% Description: A time-series is a X-Y curve where the X values map to points in time.
%% - The strategy for this implementation is to represent X values as gregorian-seconds.
%% - A time-series also has a semantic-type.
%% The semantic-type defines the unit-of-measure and the method for generating or collecting the data.
%% The time-series curve is organized as a simple orddict.

-module(time_series).

%%
%% Include files
%%

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

%%
%% Exported Functions
%%
-export([add/2, subtract/2, multiply/4, scale/4, divide/4, new/2, put/3, put/4, extract/3, extract/5]).

-record( series, { series_type               % Arithmetic requires that two operands have same type and units.
								 , unit_of_measure
								 , curve = orddict:new() 
								 } ).

%%
%% API Functions
%%

new( SeriesType, UnitOfMeasure ) ->
		#series{ series_type = SeriesType
           , unit_of_measure = UnitOfMeasure 
           , curve = orddict:new() }.

% Insert the given {date-time, value} and return updated series.
% If a value pre-exists at that time-ordinate, it is replaced.
put( Date, Time, Value, Series ) ->
		GregorianSeconds = calendar:datetime_to_gregorian_seconds({Date, Time}),
		NewSeries = put( GregorianSeconds, Value, Series ),
		NewSeries.

% Insert the given { gregorian-seconds, value} and return updated series.
% If a value pre-exists at that time-ordinate, it is replaced.
put( GregorianSeconds, Value, Series ) ->
		OldCurve = Series#series.curve,
		NewCurve = orddict:store( GregorianSeconds, Value, OldCurve ),
		NewSeries = Series#series{ curve = NewCurve },
		NewSeries.

% Extract the curve between [Start, End] inclusive.
extract( StartSeconds, EndSeconds, Series) ->
		SubSeries = orddict:filter(fun( Seconds, Value ) -> ( Seconds >= StartSeconds) and (Seconds =< EndSeconds ) end
																		, Series ),
		SubSeries.

extract( Date1, Time1, Date2, Time2, Series ) ->
		Seconds1 = calendar:datetime_to_gregorian_seconds( Date1, Time1 ),
		Seconds2 = calendar:datetime_to_gregorian_seconds( Date2, Time2 ),
		SubSeries = extract( Seconds1, Seconds2, Series ),
		SubSeries.
		
% Add SeriesB to SeriesA giving a series of the same type and unit-of-measure.
add( SeriesA, SeriesB ) ->
  check_compatibility( SeriesA, SeriesB ),
	CurveC = orddict:merge( fun(_Key,A,B) -> A + B end
                         , SeriesA#series.curve
                         , SeriesB#series.curve
                         ),
	SeriesC = #series{ series_type     = SeriesA#series.series_type
									 , unit_of_measure = SeriesA#series.unit_of_measure 
									 , curve = CurveC },
	SeriesC.

% Subtract SeriesB from SeriesA giving a series of the same type and unit-of-measure.
subtract( SeriesA, SeriesB ) ->
  check_compatibility( SeriesA, SeriesB ),
	CurveC = orddict:merge( fun(_Key,A,B) -> A - B end
                         , SeriesA#series.curve
                         , SeriesB#series.curve
                         ),
	SeriesC = #series{ series_type     = SeriesA#series.series_type
									 , unit_of_measure = SeriesA#series.unit_of_measure 
									 , curve = CurveC },
	SeriesC.

% Multiply SeriesA by SeriesB giving a new series of the SeriesType and UnitOfMeasure

multiply( SeriesA, SeriesB, SeriesType, UnitOfMeasure )  ->
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
	
% Divide SeriesA by SeriesB giving a new series of the SeriesType and UnitOfMeasure
divide( SeriesA, SeriesB, SeriesType, UnitOfMeasure ) ->
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
			

% Multiply SeriesA by scalar-factor giving a new series of the SeriesType and UnitOfMeasure
scale( SeriesA, Factor, SeriesType, UnitOfMeasure ) ->
  CurveC = orddict:map( fun(_Key,Value) -> Value * Factor end
                         , SeriesA#series.curve
                         ),
	SeriesC = #series{ series_type = SeriesType
									 , unit_of_measure = UnitOfMeasure 
									 , curve = CurveC },
	SeriesC.


% Check that two series can be added or subtracted without conversion.
check_compatibility( SeriesA, SeriesB ) ->
		?assertEqual( SeriesA#series.unit_of_measure
		            , SeriesB#series.unit_of_measure ),
		?assertEqual( SeriesA#series.series_type
		            , SeriesB#series.series_type ).

% Convert series to a new series with a different unit of measure.
convert( Series, SeriesType, UnitOfMeasure ) ->
	Factor = conversion_factor( Series#series.unit_of_measure, UnitOfMeasure ),
	SeriesOut = scale( Series, Factor, SeriesType, UnitOfMeasure ),
	SeriesOut.

conversion_factor( UnitOfMeasureA, UnitOfMeasureB ) ->
		{{ UnitOfMeasureA, UnitOfMeasureB }, Factor} = orddict:lookup( { UnitOfMeasureA, UnitOfMeasureB }, factors() ),
		Factor.
		
%%
%% Local Functions
%%

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

% ===============================  TESTS  ==================================================

new_test() ->
		SeriesType = 'Daily Watt-hour Register Values',
		UnitOfMeasure = 'Watt-hour',
		Result = new( SeriesType, UnitOfMeasure ),
		?assertEqual( #series{ series_type = SeriesType, unit_of_measure = UnitOfMeasure, curve = orddict:new() }
		            , Result ),
		ok.

put_test() ->
		SeriesType = 'Daily Watt-hour Register Values',
		UnitOfMeasure = 'Watt-hour',
		Series = new( SeriesType, UnitOfMeasure ),
		Date = {2011,12,31},
		Time = {23,59,59},
		Value = 54321,
		NewSeries = put( Date, Time, Value, Series ),
		%?debugVal( NewSeries ),
		?assertEqual( {series,'Daily Watt-hour Register Values','Watt-hour', [{63492595199,54321}]}
		            , NewSeries ),
		NewSeries.

scale_test() ->
		SeriesA = put_test(),
		Factor = 3,
		SeriesType = 'Scaled By 3',
		UnitOfMeasure = 'Funky Units',
		ScaledSeries = scale( SeriesA, Factor, SeriesType, UnitOfMeasure ),
		%?debugVal( ScaledSeries ),
		?assertEqual( {series,'Scaled By 3','Funky Units',[{63492595199,162963}]}
                 , ScaledSeries ),
		ok.

add_test() ->
		SeriesA = put_test(),
		SeriesB = put_test(),
		SeriesC = add( SeriesA, SeriesB ),
		%?debugVal( SeriesC ),
		?assertEqual( {series,'Daily Watt-hour Register Values','Watt-hour', [{63492595199,54321+54321}] }
                , SeriesC ),
		ok.

subtract_test() ->
		SeriesA = put_test(),
		SeriesB = put_test(),
		SeriesC = subtract( SeriesA, SeriesB ),
		%?debugVal( SeriesC ),
		?assertEqual( {series,'Daily Watt-hour Register Values','Watt-hour', [{63492595199,54321-54321}] }
                , SeriesC ),
		ok.

multiply_test() ->
		VoltSeries = put( {2011,12,31}, {23,59,59}, 120, new( 'RMS Voltage', 'Volts' ) ),
		AmpSeries  = put( {2011,12,31}, {23,59,59}, 50, new( 'RMS Amperage', 'Amps' ) ),
		WattSeries = multiply( VoltSeries, AmpSeries, 'RMS Watts', 'Watt' ),
		?debugVal( WattSeries ),
		?assertEqual( {series,'RMS Watts','Watt',[{63492595199,6000}]}, WattSeries ),
		ok.

multiply_mismatch_test() ->
		VoltSeries = put( {2011,12,30}, {23,59,59}, 120, new( 'RMS Voltage', 'Volts' ) ),
		AmpSeries  = put( {2011,12,31}, {23,59,59},  50, new( 'RMS Amperage', 'Amps' ) ),
		WattSeries = multiply( VoltSeries, AmpSeries, 'RMS Watts', 'Watt' ),
		?debugVal( WattSeries ),
		?assert( {series,'RMS Watts','Watt',[{63492595199,6000}]} =/= WattSeries ),
		ok.

divide_test() ->
		WattSeries = put( {2011,12,31}, {23,59,59}, 6000, new( 'RMS Watts', 'Watts' ) ),
		VoltSeries = put( {2011,12,31}, {23,59,59},  120, new( 'RMS Voltage', 'Volts' ) ),
		AmpSeries = divide( WattSeries, VoltSeries, 'RMS Amperage', 'Amps' ),
		?debugVal( AmpSeries ),
		?assertEqual( {series,'RMS Amperage', 'Amps',[{63492595199,50.0}]}, AmpSeries ),
		ok.

divide_mismatch_test() ->
		WattSeries = put( {2011,12,30}, {23,59,59}, 6000, new( 'RMS Watts', 'Watts' ) ),
		VoltSeries = put( {2011,12,31}, {23,59,59},  120, new( 'RMS Voltage', 'Volts' ) ),
		AmpSeries = divide( WattSeries, VoltSeries, 'RMS Amperage', 'Amps' ),
		?debugVal( AmpSeries ),
		?assert( {series,'RMS Amperage', 'Amps',[{63492595199,50.0}]} =/= AmpSeries ),
		ok.

		

		
		
