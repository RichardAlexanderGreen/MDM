% Copyright Richard Alexander Green 2011
% Description: This module runs the simulated clock. It publishes clock events.
%
% Subscribers respond to clock events.
% sim_installer responds by installing new meters - and starting the corresponding sim_service.
% sim_service responds by generating usage events.
%
% We use gen_event:sync_notify/2 (synchronized notification) for each tick.
% A pontential hazard with any approach is that event generators can generally run faster than the disk_log.
% However, the goal of my simulators are to generate semi-realistic event streams to test the MDM system.
% The simulation is not the goal. The event stream is the goal.
% That means that the sequence and mix of events is more important than the actual timing.
% 
% Clock ticks will occur at a rate of one per simulated minute.
%
% The installer will respond by sending install events between 8 am and 5 pm. (9 hours)
% The average number of events sent in a minutes will be (installations per day) / (minutes in day).
% If the install rate is 1000 per day, each minute will typically spawn 1 to 3 install events.
% 
% The demand simulator will respond by advancing the meter register according to the service's load profile.
% The meter will send usage events whenever the register passes the next-report-due threshold and increment the threshold.
% (We will try 1 kWh increments for the first simulations.)
% If a service is operating at a load of 10kW, it will send a 1 kWh usage event every 6 minutes.

% -------------------------------------------------------------------
-module(sim_world).

-behaviour(gen_event).

-define( EVENT_MANAGER, { local, ?MODULE } ).
% --------------------------------------------------------------------
% Include files
% --------------------------------------------------------------------


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


-import( sim_clock, [ clock_advance/1
										, clock_datetime/0
										, clock_day_type/0
										, clock_seconds/0
										, clock_set/2
										, clock_yyyymmdd/0]
			 ).


% --------------------------------------------------------------------
% External exports
-export([]).

% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

% ====================================================================
% External functions
% ====================================================================

run( StartDate, DaysToRun, InstallsPerDay ) ->
		
		MetersPerCircuit = 1500,
		MetersPerTransformer = 10,
		sim_install:initialize( MetersPerCircuit, MetersPerTransformer, InstallsPerDay ),    % Set fan-out ratios
		
		WattsBeforeUsageEvent = 1000,		           % Set usage reporting increment (1000 => 1 kWhr increment )
		sim_demand:initialize( WattsBeforeUsageEvent ),  

		% Make sure the actors are registered and ready to run.
		Handlers = gen_event:which_handlers(?EVENT_MANAGER),
		?assert( lists:member( {local, sim_install }, Handlers )  ),
		?assert( lists:member( {local, sim_demand  }, Handlers )  ),

		% Run clock
		clock_set( StartDate, {00,00,00} ),        % Set the initial clock value.
		minute_by_minute( DaysToRun * 24 * 60 ).   % Run the clock for that many minutes

minute_by_minute( 0 ) ->
		ok;

minute_by_minute( Minutes ) ->
		clock_advance( 60 ),                % Increment the clock by 60 seconds
		tick(),                             % Send event to event-handlers (actors)
		minute_by_minute( Minutes - 1).     % Loop until we are done.

% tick/0 creates a synchrounous event - So, actions for each tick must complete before next is sent.
tick() ->
		gen_event:sync_notify( ?EVENT_MANAGER, {tick, clock_seconds() } ).  % This is synchronous call.

% ====================================================================
% Server functions
% ====================================================================
% --------------------------------------------------------------------
% Func: init/1
% Returns: {ok, State}          |
%          Other
% --------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

% --------------------------------------------------------------------
% Func: handle_event/2
% Returns: {ok, State}                                |
%          {swap_handler, Args1, State1, Mod2, Args2} |
%          remove_handler
% --------------------------------------------------------------------
handle_event(Event, State) ->
    {ok, State}.

% --------------------------------------------------------------------
% Func: handle_call/2
% Returns: {ok, Reply, State}                                |
%          {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%          {remove_handler, Reply}
% --------------------------------------------------------------------
handle_call(Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

% --------------------------------------------------------------------
% Func: handle_info/2
% Returns: {ok, State}                                |
%          {swap_handler, Args1, State1, Mod2, Args2} |
%          remove_handler
% --------------------------------------------------------------------
handle_info(Info, State) ->
    {ok, State}.

% --------------------------------------------------------------------
% Func: terminate/2
% Purpose: Shutdown the server
% Returns: any
% --------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

% --------------------------------------------------------------------
% Func: code_change/3
% Purpose: Convert process state when code is changed
% Returns: {ok, NewState}
% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

% --------------------------------------------------------------------
% Internal functions
% --------------------------------------------------------------------

% ============================= TESTS ============================= 



