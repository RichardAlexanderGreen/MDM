%%% Copyright 2011 Richard Alexander Green
%%% Description : Simulates an electric service.
%%% Each service: 
%%% Events:
%%% - Acts as an event-manager for its services.  **** MAYBE NOT *** 
%%% - On {install - - }: Install my self. 
%%% - - TODO: In future, install a meter too.
%%% - On {tick - -}: Generate semi-random usage according to the load-profile
%%% - On {tick - -}: When date turns over, send day`s load history to logger.
%%% Messages:
%%% - {PoD_usage - - - } : Sum usage into local load history.
%%%
%%% Created : Jun 13, 2011
%%% -------------------------------------------------------------------
-module(service).  % Represents an electric service

-behaviour(gen_event).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
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

%% --------------------------------------------------------------------
%% External exports
-export([]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

% This is a helper function -- State must match circuit / transformer / service.
% Use the same -record( state, - - - ) in service / transformer / circuit and load_history modules.
-include("../include/load_point_state.hrl").

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
init( [ { id, ServiceID }, { transformer, TransformerID } ] ) ->
		LoadProfile = generate_load_profile_for_week(),
		State = #state{ id = ServiceID, aggregator = TransformerID, load_profile = LoadProfile },
		{ ok, State };


init([]) ->
    {ok, #state{}}.

%% --------------------------------------------------------------------
%% Func: handle_event/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler
%% --------------------------------------------------------------------

% Simulate installation
handle_event( { install_services, NServicesPerTransformer }, State ) ->
		% Create services as event-handlers where I am event-manager.
		create_services( NServicesPerTransformer, State ),
		NewState = State#state{},
		NewState;
	
% Simulate clock tick
% - On {tick - -}: Generate semi-random usage according to the load-profile
% - On {tick - -}: When date turns over, send day`s load history to logger.
handle_event( { tick, CurrentDateTime }, State) ->
		#state{ id = MyID, history = History, load_profile = LoadProfile, aggregator = TransformerAddress  } = State,
		
		% Send prior day`s usage to logger when date turns over.
		State2 = load_history:when_date_turns_over( CurrentDateTime, State ),
		
		% Simulate usage according to the load-profile (See load_point_state.hrl).
		Usage = simulate_usage( CurrentDateTime, LoadProfile ),
		
		% Send usage message to my transformer (transformer will forward to circuit)
		gen_server:cast( TransformerAddress, {usage, MyID, CurrentDateTime, Usage } ),
		
		% Put usage into load-history.
		{ Date, Time } = CurrentDateTime,
		NewHistory = load_history:put_usage_for_interval( Usage, Date, Time, History ),
		State3 = State2#state{ history = NewHistory },
		State3;

handle_event(Event, State) ->
    {ok, State}.

%% --------------------------------------------------------------------
%% Func: handle_call/2
%% Returns: {ok, Reply, State}                                |
%%          {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%          {remove_handler, Reply}
%% --------------------------------------------------------------------
handle_call(Request, State) ->
		exit( "call not expected"),
    Reply = ok,
    {ok, Reply, State}.

%% --------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler
%% --------------------------------------------------------------------

% Messages:

handle_info(Info, State) ->
		exit( {"service does not understand: ", Info } ),
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

% TODO: Create services as event-handlers where I am event-manager.
create_services( NServicesPerTransformer, State ) ->
		exit("not implementeted yet").

% TODO: Simulate usage based on service`s load-profile.
simulate_usage( CurrentDateTime, LoadProfile ) ->
		{ Date, Time } = CurrentDateTime,
		{ Hour, Minute, Second } = Time,
		DayOfWeek = calendar:day_of_the_week( Date ),
		Usage = load_profile:get_hour( LoadProfile, DayOfWeek, Hour ),
		Usage.

generate_load_profile_for_week() ->
		Profile = load_profile:week_test().     % *** DANGER ***

%% ///////////////////////////// TESTS ///////////////////////////// 
generate_load_profile_for_week_test() ->
		Profile = generate_load_profile_for_week(),
		Profile.

simulate_usage_test() ->
		Profile = generate_load_profile_for_week_test(),
		CurrentDateTime = calendar:local_time(),
    Usage = simulate_usage( CurrentDateTime, Profile ).

		



