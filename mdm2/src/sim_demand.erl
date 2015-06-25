% -------------------------------------------------------------------
% Copyright Richard Alexander Green 2011
% Description : This module simulates usage events from the metering network.
% It takes the place of a collection engine in the architecture.
%
% Approach:
% - This demand simulator receives installation notices from the sim_install actor.
% - When a meter is installed, it starts sending usage events.
% - This module is an event manager (gen_event behavior).
% - Simulated meters provide event_handler semantics and respond to tick events.
% - Meters publish usage events and other events.
% - The network sends published events to subscribers.
% - Circuits are data managers and subscribe to meter events.
%
% Simulation Data Flow:
% - The sim_install actor publishes installation events.
% - The network forwards events to subscribers.
% - The assets actor and sim_demand actor subscribe to installation events.
% - The sim_demand actor creates (simulated) meter actors and initiates circuit actors as needed.
% - The sim_demand actor sends tick events to meters.
% - Meters respond to tick events by publishing usage events.
% - The system responds as it normally would to meter events.
% - The network forwards usage events to circuits.
% - Circuits update service, transformer, and circuit load histories.
%
% Meter State:
% - The initial version does not attempt to simulate meter states.
% - Meter state includes: (power on/off, switch open/closed, load-side voltage).
% -------------------------------------------------------------------
-module(sim_demand).

-behaviour(gen_event).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include( "../include/records.hrl" ).


%% --------------------------------------------------------------------
%% External exports
-export([]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================


%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          Other
%% --------------------------------------------------------------------
init([]) ->
		% TODO - Subscribe to installation events.
    {ok, #state{}}.

%% --------------------------------------------------------------------
%% Func: handle_event/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler
%% --------------------------------------------------------------------
% When the installation event is the installation of a meter on a service,
%  create a demand-event generator for that service > meter > transformer > circuit.
% Elsewhere a circuit coordinator (event handler) will generate circuit listener.
% The circuit listener will respond to demand-events by updating the service usage.

handle_event( { event,  NoticeID, meter_installed, From, Notice } , State ) ->
		% TODO - Create a demand-event generator for the service.
		{ meter, Meter, installed_on_service, Service } = Notice,
		create_demand_generator_for_service( Service, Meter ),
		
		{ok, State };

handle_event( Event, State ) ->
    {ok, State}.

%% --------------------------------------------------------------------
%% Func: handle_call/2
%% Returns: {ok, Reply, State}                                |
%%          {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%          {remove_handler, Reply}
%% --------------------------------------------------------------------
handle_call(Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%% --------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    {ok, State}.

%% --------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any
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

create_demand_generator_for_service( Service, Meter ) ->
		% TODO -- Create a demand generator for given service and meter.
		ok.


