%%% -------------------------------------------------------------------
%%% Copyright 2011 Richard Alexander Green
%%% Description : Simulate the action of an electric meter (trigger on tick event)
%%%
%%% Created : Apr 14, 2011
%%% -------------------------------------------------------------------
-module(sim_meter).

-behaviour(gen_event).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record( state,{ last_seconds
							 , accumulated_watt_hours
							 , last_reported_watt_hours
							 , report_on_delta 
							 , meter_attributes
							 } ).

-record( meter, { meter_ID
							  , service_ID
							  , transformer_ID
							  , circuit_ID
								} ).

-record( usage_increment, { meter_attributes 
													, seconds
		                      , increment  
													, register 
													} ).

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
		% TODO -- Trap exit.
		
    {ok, #state{}}.

%% --------------------------------------------------------------------
%% Func: handle_event/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler
%% --------------------------------------------------------------------

handle_event({tick, Seconds}, State) ->
		NewState = do( {tick, Seconds}, State ),
		{ok, NewState};

handle_event(Event, State) ->
		error_logger:error_report({'sim_meter does not expect event: ', Event } ),
    {error, State}.

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

do( {tick, Seconds}, State ) ->
		% Calculate usage since last tick.
		LastSeconds = State#state.last_seconds,     % TODO - Assure that last_seconds is properly intialized.
		ElapsedSeconds = Seconds - LastSeconds,
		Watts = usage_rate( Seconds ),           % Usage rate at this hour of week
		WattSeconds = Watts * ElapsedSeconds,
		WattHourIncrement = WattSeconds / (60*60),
		AccumulatedWattHours = WattHourIncrement + State#state.accumulated_watt_hours,
		% Decide to report or not.
		State2 = report_per_rule( AccumulatedWattHours, Seconds, State ),
		% Update state
		State3 = State2#state{ last_seconds = Seconds
													, accumulated_watt_hours = AccumulatedWattHours 
													},	
		State3.

usage_rate( Seconds ) ->
		% TODO - 	Lookup the rate based on the hour of week.
		HourOfWeek = hour_of_week( Seconds ),
		Watts = 1000,
		Watts.

hour_of_week( Seconds ) ->
		% TODO -- Translate Gregorian seconds into hour of week (for lookup).
		HourOfWeek = { sunday, 00 }.

report_per_rule( AccumulatedWattHours, Seconds, State ) ->
		% TODO -- Report when we have accumlated X Watt-hours since last report.
		LastReportedWattHours = State#state.last_reported_watt_hours,
    Delta = AccumulatedWattHours - LastReportedWattHours,
    if ( Delta >= State#state.report_on_delta ) ->
					 State2 = report_increment( Delta, Seconds, State );
			 true ->
					 State2 = State
		end,
		State2.

report_increment( Delta, Seconds, State ) ->
		% TODO -- Publish an 'incremental report'
		Attributes = State#state.meter_attributes,
		Register = Delta + State#state.last_reported_watt_hours,	
		
		network() ! #usage_increment{ meter_attributes = Attributes
																, seconds = Seconds
		                            , increment = Delta
																, register = Register },
		State2 = State#state{ last_reported_watt_hours = Register },
		State2.

network() ->
		{ local, networkNode }.


