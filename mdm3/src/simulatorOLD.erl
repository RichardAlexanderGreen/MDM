%% Copyright 2011 Richard Alexander Green
%% Created: Apr 25, 2011
%% Description: Simulate the installations and/or load over some time period.
%% ----------------------------------------------------------------------------
-module(simulatorOLD).

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

-include( "../include/objects.hrl").

-import( object_factory, [make/1]).

-record( service_profile, { meter_ID, service_ID , load_profile } ).


%%
%% Exported Functions
%%
-export([]).

%%
%% API Functions
%%
% Install N end_points belonging to the given circuit.
install_circuit( Circuit, N_Endpoints, Transformer_Fanout ) ->
		%
		N_Transformers = N_Endpoints div Transformer_Fanout,
		install_transformers( N_Transformers, Transformer_Fanout, Circuit ),
		ok.


install_transformers( 0, _Transformer_Fanout, _Circuit ) ->
		done;
install_transformers( N_Transformers, Transformer_Fanout, Circuit ) ->
		Transformer = object_factory:transformer_on_circuit( Circuit ),
		N_Services = Transformer_Fanout,
		install_services( N_Services, Transformer ),
		install_transformers( N_Transformers - 1, Transformer_Fanout, Circuit ).

install_services( 0, _Transformer ) ->
		done;
install_services( N_Services, Transformer ) ->
		Service = object_factory:service_on_transformer( Transformer ),
		_Meter = object_factory:meter_on_service( Service ),	
		install_services( N_Services - 1, Transformer ).


simulate_reading( ThisDate, StopDate ) when ThisDate > StopDate ->
		done;
simulate_reading( ThisDate, StopDate ) ->
		% Simulate this day's demand
		simulate_days_demand( ThisDate ),
		% Advance to next day
		Days = calendar:date_to_gregorian_days( ThisDate ),
		NextDate = calendar:gregorian_days_to_date( Days + 1 ),		
		simulate_reading( NextDate, StopDate ).

simulate_days_demand( ThisDate ) ->
		Meters = assets:get_type( meter ),   % Do daily to pick up meters installed previous day.
		MapFunction = fun(Meter) -> 
													MeterID = Meter#meter.meter_ID,
													ServiceID = Meter#meter.service_ID,
													[PoD] = assets:get( pod,  ServiceID ),
													%?debugVal( { MeterID, ServiceID, PoD } ),
													
													LoadServiceProfile = PoD#pod.load_profile,
													#service_profile{ meter_ID = MeterID
																					, service_ID = ServiceID
																					, load_profile = LoadServiceProfile 
																					}
													end,
		
		ServiceProfiles = lists:map( MapFunction, Meters ),

		simulate_service_day( ServiceProfiles, ThisDate ).

simulate_service_day( [], _ThisDate ) ->
		done;
simulate_service_day( ServiceProfiles, ThisDate ) ->
		[ ServiceProfile | RemainingServiceProfiles ] = ServiceProfiles,
		simulate_service_hours( ServiceProfile, ThisDate, 0000 ),
		simulate_service_day( RemainingServiceProfiles, ThisDate ).

simulate_service_hours( _ServiceProfile, _ThisDate, Hour ) when Hour >= 2400 ->
		done;
simulate_service_hours( ServiceProfile, ThisDate, Hour ) ->
		
		% Get usage for this hour.
		UsageTable = ServiceProfile#service_profile.load_profile,
		DayOfWeek = calendar:day_of_the_week(ThisDate),
		HourUsage = case UsageTable of
										undefined -> 3000 + Hour;
										_ ->
												orddict:fetch({DayOfWeek,Hour}, UsageTable )
								end,
		% Send usage to service listener
		ServiceID = ServiceProfile#service_profile.service_ID,
		StopSeconds = calendar:datetime_to_gregorian_seconds( {ThisDate,{Hour div 100,00,00} } ),
		StartSeconds = StopSeconds - (60*60),							
		TimePeriod = #time_period{ start_seconds = StartSeconds, stop_seconds = StopSeconds },
		
		UsageRecord = #usage{ quantity = HourUsage, unit_of_measure = 'Watt Hour', time_period = TimePeriod },
		
		meters:send_usage_to_service( UsageRecord, ServiceID ),
		simulate_service_hours( ServiceProfile, ThisDate, Hour + 100 ).



%%
%% Local Functions
%%


% ============================ TESTS ==============================

install_services_test() ->
		?debugMsg( "install_services_test"),
		Circuit = object_factory:make( circuit ),
		Transformer = object_factory:transformer_on_circuit( Circuit ),
		N_Services = 3,
		install_services( N_Services, Transformer ).

install_transformers_test() ->
		?debugMsg("install_transformers_test"),
		Circuit = make( circuit ),
		N_Transformers = 3,
		Transformer_Fanout = 4,
		install_transformers( N_Transformers, Transformer_Fanout, Circuit ).
	
install_circuit_test() ->
		?debugMsg("install_circuit_test"),
		% Install a circuit
		Circuit = object_factory:make( circuit ),
		N_Endpoints = 12,
		Transformer_Fanout = 4,
		install_circuit( Circuit, N_Endpoints, Transformer_Fanout ),
		ok.

simulate_week_test() ->
		% Simulate a week of meter reading operation starting on a Sunday.
		StartDate = {2011,04,24},
		StopDate  = {2011,04,30},
		simulate_reading( StartDate, StopDate ).

