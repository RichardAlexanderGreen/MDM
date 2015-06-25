%%% Copyright Richard Alexander Green 2011
%%% Description : Sim_Install actor creates a landscape and calendar scenario
%%% by installing services and meters allocated to transformers and circuits.
%%% Meters are installed at a daily rate over a time period defined in the simulation setup.
%%% Installation events normally come from work-order completion notices.
%%% In simulation / test mode, this module creates work-order completions.
%%% In either mode, the asset manager subscribes.
%%% In simulation mode, the sim_demand module also subscribes and takes the place of a collection engine.
%%%
%%% Created : Feb 2, 2011
%%% -------------------------------------------------------------------
-module(sim_install).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include( "mdm_objects.hrl" ).

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

-import( sim_clock
			 , [ clock_advance/1
				, clock_datetime/0
				, clock_day_type/0
				, clock_seconds/0
				, clock_set/1
				, clock_set/2
				, clock_yyyymmdd/0 ] ).

%% --------------------------------------------------------------------
%% External exports
-export([]).

%% gen_server callbacks
-export( [init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3] ).

-record( state, {} ).

-record( w5, {who=sim_install, what_sim_install, why=simulation, whenUDT, where = sim_install} ).

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
		put( audit_list, [] ),
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
		error_logger:error_report( info, {'sim_install sent unexpected message: ', Info } ),
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
install_days( _ServicesPerDay, 0 ) -> 
		audit( check ),
		done;
install_days( ServicesPerDay, N_Days ) ->
		?assert( messenger:is_running() ),
		
		clock_advance(day),
		
		case clock_day_type() of
				weekday ->
						N_Services = ServicesPerDay,
						install_services( ServicesPerDay, N_Services );
				weekend ->
						%?debugVal({ clock_yyyymmdd(), 'Take day off'}),
						'Take day off'
		end,
		install_days( ServicesPerDay, N_Days-1 ).

install_services( _ServicesPerDay, 0 ) -> done;               % TODO --- ???
install_services( ServicesPerDay, N_Services ) ->
		clock_advance( 500*60 div ServicesPerDay ),               % TODO --- ???
		install_service_and_meter_on_circuit( N_Services ).

% This module generates 4 events for each service installed.
% The throughput seen on my Mac is approximately 400,000 events / 16 seconds = 25,000 events / second
install_service_and_meter_on_circuit( 0 ) -> done;
install_service_and_meter_on_circuit( Index ) ->

    % Simulate the job-ticket
    What = sim_install,
    Why = simulation,
    W5 = quad:w5( What, Why ),

		YMD = clock_yyyymmdd(),
		Circuit = {'circuit:',YMD},                                % TODO -- Need a better way to allocate to circuit.
		Transformer = { 'transformer:', YMD, ( Index div 10 ) },   % Crude: transformer fan-out is 10.
		Service = { 'service:', YMD, Index },
		% TODO - generate additional service attributes ( coordinates, profile )

		% Define the service we intend to install.
		publish( { new_service, Service, record, 
							 #service{ oid = Service
											 , transformer = Transformer
											 , circuit = Circuit 
											 , coordinates = { 43.001001, -83.001001 }
											 , premise = {'premise:', YMD, Index }
											 }  
						 , W5 }
					 , new_service ),
		
		% AssetMgr : Define asset and connect as indicated.
		% circuit : Connect service to circuit	
		publish( { service, Service, installed_on_circuit, Circuit, W5 }, service_installed_on_circuit ),
		
		% circuit: Connect service to transformer 		
		publish( { service, Service, draws_on_transformer, Transformer, W5 },  service_draws_on_transformer),
		
		Inventory = "Installation Inventory",
		MeterType = "Smart Meter",
		
		Meter = request( assets, { withdraw_asset_of_type, MeterType, from_inventory, Inventory } ),
		% TODO - THINK : Should we move the meter from inventory to inventory?

		publish( { meter, Meter, record, 
							 #meter{ oid = Meter
										 , type_of_meter = MeterType
										 , configuration = "RSS1.1"
										 }  
						 , W5 }
					 ,  meter_configured ),

		% Install the meter (service is turned-on)
		publish( { meter, Meter, installed_on_service, Service, W5 }, meter_installed ),
		
		% sim_demand : Start demand simulation
		
		install_service_and_meter_on_circuit( Index-1 ).

publish( Notice, OnTopic ) ->
		%?debugVal( Notice ),
		%EventID = clock_seconds(),     % Use the simulation seconds.
    % What = sim_install,
    % Why = simulation,
    % EventID = quad:w5( What, Why ),
		EventID = now(),
		Actor = self(),	
		_PublishResult = messenger:publish( EventID, Notice, OnTopic, Actor ),
		%?debugVal( _PublishResult ),
		audit( Notice ),
		ok.

% Write some of these at random onto a audit-list to check that repository has equivalent of each item.
audit( check ) ->
		AuditList = get( audit_list ),
		?debugVal( AuditList ),
		check_audit_list( AuditList ),
		done;

audit( Notice ) ->
		Rand = random:uniform( 1000 ),
		if ( Rand < 3 ) ->                  % Sample 3 per 10000      
		   write_audit_list( Notice );      % Write notice to audit-list.
			 true ->
					 do_nothing
		end,
		ok.
write_audit_list( Notice ) ->
		% Try the simplest thing: Just add it to a list.
		AuditList = get( audit_list ),
		NewAuditList =   lists:append( AuditList, [Notice] ),
		put( audit_list, NewAuditList ),
		ok.

check_audit_list( undefined ) ->
		?debugVal( { check_audit_list, undefined } ),
		done;
check_audit_list( [ ] ) ->
		done;
check_audit_list( AuditList ) ->
		[ ListEntry | Remainder ] = AuditList,
		?debugVal( { check_audit_list, ListEntry } ),
		check_audit_list( Remainder ).
		

request( assets, {withdraw_asset_of_type,"Smart Meter",from_inventory, "Installation Inventory"} ) ->  %%% MOCK %%%
		Meter = get( meter_counter ) + 1,
		put( meter_counter, Meter );

request( Actor, Action ) ->
		?debugVal( Action ),
		result.

w5() ->
		#w5{ whenUDT = clock_datetime() }.


				


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  TESTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% ------------------ Install Tests ------------------ 

install_reset_test() ->
		clock_set( {2001,01,01}, {00,00,00} ),
		put( meter_counter, 1000000 ).

install_service_and_meter_on_circuit_test() ->
		init([]),
		put( audit_list, [] ),
		?debugMsg( install_service_and_meter_on_circuit_test ),
		% Just see if it runs.
		install_service_and_meter_on_circuit(1).

install_services_test() ->
		init([]),
		put( audit_list, [] ),
		?debugMsg( install_services_test ),
		%ServicesPerDay = 2,
		%N_Services = 4,
		install_services( ServicesPerDay = 1, N_Services = 1 ),
		check_audit_list( get( audit_list ) ).

install_days_test() ->
		init([]),
		put( audit_list, [] ),
		?debugMsg( install_days_test ),
		clock_set( {2011,11,22},{00,00,00}),
		ServicesPerDay = 2,
		put( meter_counter, 1000000 ),
		install_days( ServicesPerDay, 7 ).

% Generate some number of installations 
load_test( ServicesPerDay, NDays ) ->
		init([]),
		put( audit_list, [] ),
		?assert( messenger:is_running() ),
		
		?debugVal( { load_test, ServicesPerDay, NDays } ),
		clock_set( {2011,11,22},{00,00,00}),
		?debugVal( { start_seconds, clock_seconds() } ),
		
		%put( services_per_day, ServicesPerDay ),
		put( meter_counter, 100000 ),
		{ MicroSeconds, done } = timer:tc( ?MODULE, install_days, [ ServicesPerDay, NDays ] ),
		?debugVal( { stop_seconds, clock_seconds() } ),
		{ MicroSeconds/1.0e6, ' seconds of run  time'}.
		

  
% ------------------------------------------------------


		





		