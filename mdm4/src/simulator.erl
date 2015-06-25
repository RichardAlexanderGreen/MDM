%% Copyright 2011 Richard Alexander Green
%% Created: Apr 25, 2011
%% Description: Simulate the installations and/or load over some time period.
%% ----------------------------------------------------------------------------
-module(simulator).

%%
%% Include files
%%

-define( TEST, true ).
-define( DEBUG, true ).
-define( RUN_ONLY_ONE_DAY, false ).

% Set log level
% ALL=0 < TRACE=1 < DEBUG=2 < INFO=3 < WARN=4 < ERROR=5 < FATAL=6 < OFF=7
% Level semantics are defined in the org.apache.log4j.Level class.
-define( LOGLEVEL, 3 ).             % 3 means log info level and above. -- See log/2.

-include_lib("eunit/include/eunit.hrl").

-ifdef( DEBUG ).
-compile( export_all ).
-endif.


-record( service_profile, { meter_ID, service_ID , load_profile = default_profile() } ).

-import( object_factory, [make/1]).

-include( "../include/objects.hrl").


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
		%?debugVal( Service ),
		_Meter = object_factory:meter_on_service( Service ),	
		install_services( N_Services - 1, Transformer ).


simulate_reading( ThisDate, StopDate ) when ThisDate > StopDate ->
		done;
simulate_reading( ThisDate, StopDate ) ->
		%?debugVal( {simulate_reading, ThisDate, StopDate, calendar:local_time()}),
		
		% Simulate this day's demand
		simulate_days_demand( ThisDate ),
		% Advance to next day
		Days = calendar:date_to_gregorian_days( ThisDate ),
		NextDate = calendar:gregorian_days_to_date( Days + 1 ),		
		simulate_reading( NextDate, StopDate ).


simulate_read_and_install( ThisDate, StopDate ) when ThisDate > StopDate ->
		done;
simulate_read_and_install( ThisDate, StopDate ) ->
		%?debugVal( {simulate_read_and_install, ThisDate, StopDate, calendar:local_time() }),
		%io:format( "Simulate read and install for: ~p stop at: ~p  current time: ~p "
		%	    		 , [ ThisDate, StopDate, calendar:local_time() ] ),
		 
		
		% Simulate this day's demand
		simulate_days_demand( ThisDate ),
		
		% Simulate a day's installation (a circuit a day)
		Circuit = object_factory:make( circuit ),
		N_Endpoints = 1000,
		Transformer_Fanout = 10,
		install_circuit( Circuit, N_Endpoints, Transformer_Fanout ),
		
		flush_tables(),
		
		% Advance to next day
		Days = calendar:date_to_gregorian_days( ThisDate ),
		NextDate = calendar:gregorian_days_to_date( Days + 1 ),		
		simulate_read_and_install( NextDate, StopDate ).


simulate_days_demand( ThisDate ) ->				
		Meters = assets:get_type( meter ),   % Do daily to pick up meters installed previous day.
		
		%?debugVal( Meters ),
		
		%?debugVal( {simulate_days_demand, ThisDate, length( Meters)  } ),
		N_Meters = length( Meters ),
		T_Start = now(),
		
		io:format( "Simulate day: ~p ~p meters start: ~p "
						 , [ ThisDate, N_Meters,T_Start] ),
		
		MapFunction = fun(Meter) -> 
													MeterID = Meter#meter.meter_ID,
													ServiceID = Meter#meter.service_ID,
													PodList = assets:fetch( pod, ServiceID ),
													case PodList of
															[] ->
																	exit( {"no PoD for service: ", ServiceID });
															#pod{} -> 
																	PoD = PodList,
																	
																	%?debugVal( { MeterID, ServiceID, PoD } ), 
																	LoadServiceProfile = simulate_profile( PoD ), 
																	#service_profile{ meter_ID = MeterID 
																									, service_ID = ServiceID 
																									, load_profile = LoadServiceProfile 
																									};
															Other ->
																	exit( {"PodList not in expected format. See other: ", Other})
													end
									end,
		
		ServiceProfiles = lists:map( MapFunction, Meters ),
		T_Stop = now(),
		ElapsedMicroseconds = timer:now_diff( T_Stop, T_Start ),
		io:format( " stop: ~p ==> ~p meters per second ~n", [T_Stop,( N_Meters / ( ElapsedMicroseconds/1000000) ) ]),

		simulate_service_day( ServiceProfiles, ThisDate ).

simulate_profile( PoD ) ->
		LoadPodProfile = PoD#pod.load_profile,
		ThisProfile = case LoadPodProfile of
				[] ->
						default_profile();
				
				undefined ->
						% Return default residential profile.
						default_profile();
											
				#load_profile{ hours = [] } ->
            default_profile();
											
				SomeProfile ->
						SomeProfile
		end,
		?assertEqual( 24*7, orddict:size( ThisProfile#load_profile.hours) ),
		ThisProfile.

		

default_profile() ->
		Owner = 'simulator',
		Profile0 = load_profile:new( Owner ),  % Owner is generally a PoD -- But it could be a generic type.
		Usage = [ 3000, 3100, 3200, 3300, 3400, 3500, 3600, 3700
						, 3800, 3900, 1000, 1111, 1200, 1300, 1400, 1500
						, 1600, 1700, 1800, 1900, 2000, 2100, 2200, 2300 ],  % Not a realistic profile -- just for test
		Profile1 = load_profile:put_day( Profile0, 'Sunday', Usage ),
    Profile2 = load_profile:put_day( Profile1, 'Monday', Usage ),
		Profile3 = load_profile:put_day( Profile2, 'Tuesday', Usage ),
    Profile4 = load_profile:put_day( Profile3, 'Wednesday', Usage ),
		Profile5 = load_profile:put_day( Profile4, 'Thursday', Usage ),
    Profile6 = load_profile:put_day( Profile5, 'Friday', Usage ),
		Profile7 = load_profile:put_day( Profile6, 'Saturday', Usage ),
		?assertEqual( 24*7, orddict:size( Profile7#load_profile.hours) ),
		Profile7.


simulate_service_day( [], _ThisDate ) ->
		done;
simulate_service_day( ServiceProfiles, ThisDate ) ->
		[ ServiceProfile | RemainingServiceProfiles ] = ServiceProfiles,
    send_days_demand( ServiceProfile, ThisDate ),
		case ?RUN_ONLY_ONE_DAY of
				true ->
						done;
				false -> 
					 simulate_service_day( RemainingServiceProfiles, ThisDate )
		end.

send_days_demand( ServiceProfile, ThisDate ) ->
		%?debugVal( { send_days_demand, ThisDate } ),
		LoadProfile = ServiceProfile#service_profile.load_profile,
		?assert( LoadProfile =/= undefined ),
		
		%?debugVal( { ServiceProfile, LoadProfile } ),

		DaysTypicalUsage = load_profile:map_profile_for_day_of_week_matching( ThisDate, LoadProfile ),
		%?debugVal( { length(DaysTypicalUsage), DaysTypicalUsage } ),

		Series = time_series:put_day( ThisDate, DaysTypicalUsage, time_series:new( 'simulated hourly usage', 'Watt-hour') ),
		[ FirstEntry | _ ] = Series#series.curve,
		%?debugVal( FirstEntry ),
		PoD_Series = #pod_series{ pod_ID = ServiceProfile#service_profile.service_ID
													 , series = Series },
		gen_server:cast( {global, services}, PoD_Series ).



%%
%% Local Functions
%%

flush_tables() ->
		% Send flush messages to assets and services.
		gen_server:cast( { global, assets   }, flush ),
		gen_server:cast( { global, services }, flush ).


% ============================ TESTS ==============================

flush_tables_test() ->
		?debugMsg("............................. flush_tables_test ............................. flush_tables_test "),
		 flush_tables().

install_services_test() ->
		?debugMsg( ".............................. install_services_test .............................. install_services_test "),
		Circuit = object_factory:make( circuit ),
		Transformer = object_factory:transformer_on_circuit( Circuit ),
		N_Services = 3,
		install_services( N_Services, Transformer ).

install_transformers_test() ->
		?debugMsg(".............................. install_transformers_test .............................. install_transformers_test "),
		Circuit = make( circuit ),
		N_Transformers = 3,
		Transformer_Fanout = 4,
		install_transformers( N_Transformers, Transformer_Fanout, Circuit ).
	
install_circuit_test() ->
		?debugMsg(".............................. install_circuit_test .............................. install_circuit_test"),
		
		% Install a circuit
		Circuit = object_factory:make( circuit ),
		N_Endpoints = 12,
		Transformer_Fanout = 4,
		install_circuit( Circuit, N_Endpoints, Transformer_Fanout ),
		ok.

simulate_week_test() ->
		?debugMsg("............................. simulate_week_test ............................. simulate_week_test "),
		
		% Simulate a week of meter reading operation starting on a Sunday.
		StartDate = {2011,04,24},
		StopDate  = {2011,04,30},
		simulate_reading( StartDate, StopDate ).

simulate_month_test() ->
		?debugMsg("............................. simulate_month_test ............................. simulate_month_test "),
		
		% Simulate a month of meter reading and installation.
		StartDate = {2011,04,01},
		StopDate  = {2011,04,30},
		simulate_read_and_install( StartDate, StopDate ).






