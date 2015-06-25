%%% -------------------------------------------------------------------
%%% Author  : admin
%%% Description :
%%%
%%% Created : Nov 14, 2010
%%% -------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Use Cases:
%%
%% - Install electric meters on C circuits.
%% - - Installed-Device has attributes device_ID, device_type, service.
%% - - Electric-Meter has attributes interval_length, last_register_value.
%% - - Service has attributes service_ID, location, circuit, end-use, average weekly load.
%% - - End-Use-Profile has attributes end-use-name and hourly-fraction.
%% - - A typical circuit has roughly 1000 services. (Make this a parameter of simulation.)
%% - - Circuit has attributes circuit-ID, number-of-services, weather-zone, location (centriod).
%%
%% - Simulate real-time data flowing from N meters to the usage_monitor.
%% - - Meters send data every M = 15 minutes (900 seconds = 900000 milliseconds) in simulated time.
%% - - - Balance the load: Meter sends at milliseconds = serial modulo 90000.
%% - - Data simulates usage which varies as f(end-use, hour-of-week).
%% - - Run the simulation for T seconds.
%%
%% - Simulate a storm outage.
%% - - P percentage of the meters on circuit C send a Power-Off alarm.
%%
%% --------------------------------------------------------------------

-module(network_simulator).
-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([test_install_circuit/1]).

-define(debug,true).
%-undef(debug).

-ifdef(debug).
-compile(export_all).   % For debug only.
-endif.


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record( state, {} ).

%% - - Installed-Device has attributes device_ID, device_type, service_ID.
-record( installed_device, { device_ID, device_type, service_ID }).

%% - - Electric-Meter has (additional) attributes interval_length, last_register_value.
-record( electric_meter, { device_ID, interval_length, last_register_value } ).

%% - - Service has attributes service_ID, location, circuit, end-use, average weekly load.
-record( service, { service_ID, location, circuit_ID, end_use, average_weekly_load }).
% Location is the GPS coordinates { latitude, longitude } in decimal notation.
% One degree of latitude = 111,319.9 meter.
% One degree of longitude = 111,319.9 meter * cosine( latitude ) * correction( latititude ).
% The correction factor (The earth is slightly elipsoid ) can be ignored for our simulation.
% Close to the north pole the correction is 0.99664719 - so our error is < 0.00335281.
% Anyway - To record GPS degrees with nearest meter accuracy requires 5 decimal places.
-define( DEGREES_PER_METER, (1/111319.0) ).


%% - - End-Use-Profile defines the fraction of weekly-load as a f(end-use, day-of-week, hour-of-day).
-record( end_use_profile, { end_use, day_of_week, hour_of_day, fraction }).

%% - - A typical circuit has roughly 1000 services. (Make this a parameter of simulation.)
%% - - Circuit has attributes circuit-ID, number-of-services, weather-zone, location (centriod).
-record( circuit, { circuit_ID, n_services, weather_zone, centroid } ).

% Layout: Detroit has an average population density around 2500 / square-kilometer.
% The urban area average desity is about 1200 / square-kilometer
% With 2 people per house-hold ==> ~ 1250 households / square-kilometer.
% Implies a meter spacing of 10-40 meters ==> every 0.0001 - 0.0004 degrees.


%% ====================================================================
%% External functions
%% ====================================================================


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
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
install_circuit( Circuit_ID, N_services, Centroid ) ->
		
		% First line is at Centroid and South 500 meters.
		{ location, Latitude, Longitude } = Centroid,
	  First_line_latitude = Latitude - (500 * ?DEGREES_PER_METER),
		% Lay down the lines. (lay_line calls itself recurvively.)
		Circuit_Services = build_circuit( [], Circuit_ID, First_line_latitude, Longitude, N_services ),
    
		% Adjust the centroid.
		%Adjusted_Centroid = Centroid,
		
		% Finish up.
		%Weather_Zone = "South East Michigan",
		%{ circuit, Circuit_ID, N_services, Weather_Zone, Adjusted_Centroid },
		
		Circuit_Services.

build_circuit( List_of_services, Circuit_ID, First_line_Latitude, Center_Longitude, N_services ) ->
		N_per_line = trunc( math:sqrt( N_services )  ),
    lay_line( List_of_services, Circuit_ID, First_line_Latitude, Center_Longitude - (500 * ?DEGREES_PER_METER), N_per_line, N_services ).

lay_line( List_of_services, _, _, _, _, N_remaining ) when ( N_remaining < 1 ) ->
		List_of_services;
lay_line( List_of_services, Circuit_ID, Line_Latitude, Line_Longitude, N_per_line, N_remaining ) ->
		Next_list = install_services( List_of_services, Circuit_ID, Line_Latitude, Line_Longitude - ( 500 * ?DEGREES_PER_METER), N_per_line, min( N_per_line, N_remaining) ),
		lay_line( Next_list, Circuit_ID, Line_Latitude + ?DEGREES_PER_METER * ( 1000 / N_per_line ), Line_Longitude, N_per_line, N_remaining - N_per_line ).

install_services (List_of_services, Circuit_ID, Line_Latitude, Line_Longitude, N_per_line, N_remaining ) when ( N_remaining > 0 ) ->
		Service_sequence = 1 + length( List_of_services ),
		Service_ID = Circuit_ID ++"-"++ integer_to_list( Service_sequence ),
		Service_record = {service_location, Circuit_ID, Service_ID, Line_Latitude, Line_Longitude },	
		record_service_attributes( Service_record ),
		Next_List = [ Service_record | List_of_services ],
		install_services( Next_List, Circuit_ID, Line_Latitude, Line_Longitude + ?DEGREES_PER_METER * ( 1000 / N_per_line ), N_per_line, N_remaining - 1 );
install_services( List_of_services, _, _, _,  _, _ ) ->
		List_of_services.

record_service_attributes( Service_record ) ->
		{service_location, Circuit_ID, Service_ID, Latitude, Longitude } = Service_record,
		Dictionary = orddict:from_list([{circuit, Circuit_ID}, {location,[Latitude, Longitude]}]),
		PID_asset_db = whereis( asset_db ),
		if PID_asset_db /= undefined ->
			PID_asset_db ! { remember_multiple_attributes, Service_ID, Dictionary };  % remote call
      PID_asset_db == undefined ->
					ok = asset_db:remember_multiple_attributes( Service_ID, Dictionary ); % local call
			true ->
					how_did_I_get_here
		end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% --------------------------------------------------------------------
%% Tests -- Test components, run small simulations.
%% --------------------------------------------------------------------
%% Use Cases:
%%
%% - Install electric meters on C circuits.
%% - - Installed-Device has attributes device_ID, device_type, service.
%% - - Electric-Meter has attributes interval_length, last_register_value.
%% - - Service has attributes service_ID, location, circuit, end-use, average weekly load.
%% - - End-Use-Profile has attributes end-use-name and hourly-fraction.
%% - - A typical circuit has roughly 1000 services. (Make this a parameter of simulation.)
%% - - Circuit has attributes circuit-ID, number-of-services, weather-zone, location (centriod).
%%
%% - Simulate real-time data flowing from N meters to the usage_monitor.
%% - - Meters send data every M = 15 (900 seconds = 900000 milliseconds) minutes in simulated time.
%% - - - Balance the load: Meter sends at milliseconds = serial modulo 90000.
%% - - Data simulates usage which varies as f(end-use, hour-of-week).
%% - - Run the simulation for T seconds.
%%
%% - Simulate a storm outage.
%% - - P percentage of the meters on circuit C send a Power-Off alarm.
%%
%% --------------------------------------------------------------------

% Test: Install a circuit's meters scattered over a square kilometer of area.
test_install_circuit(N_services) ->
		Circuit_ID = "West Side Division 1",
		Centroid = { location, Latitude = 43.0, Longitude = -83.0 },	
		%{ circuit, Circuit_ID, N_services, _, Centroid } = 
		List = install_circuit( Circuit_ID, N_services, Centroid ),
		N_services = length(List),
		io:format("Installed ~p services.~n", [N_services]).




		

