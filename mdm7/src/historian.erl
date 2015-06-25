%%% Copyright 2011 Richard Alexander Green
%%% Description : maintains time-series data
%%% - This version uses a cheap trick by indexing into a tuple.
%%% - A more space-efficient method would use an array of integers.
%%% - 16-bit integers may be used because residential meters are rated < 64 kilowatts.
%%% - A more sophisticated scheme would vary the storage depending on the meter ratings.
%%%
%%% Created : Jun 5, 2011
%%% -------------------------------------------------------------------
-module(historian).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

-record( historyV00, { sinkID
	                , week
									, hrMon00,hrMon01,hrMon02,hrMon03,hrMon04,hrMon05,hrMon06,hrMon07,hrMon08,hrMon09,hrMon10,hrMon11,hrMon12,hrMon13,hrMon14,hrMon15,hrMon16,hrMon17,hrMon18,hrMon19,hrMon20,hrMon21,hrMon22,hrMon23
									, hrTue00,hrTue01,hrTue02,hrTue03,hrTue04,hrTue05,hrTue06,hrTue07,hrTue08,hrTue09,hrTue10,hrTue11,hrTue12,hrTue13,hrTue14,hrTue15,hrTue16,hrTue17,hrTue18,hrTue19,hrTue20,hrTue21,hrTue22,hrTue23
									, hrWed00,hrWed01,hrWed02,hrWed03,hrWed04,hrWed05,hrWed06,hrWed07,hrWed08,hrWed09,hrWed10,hrWed11,hrWed12,hrWed13,hrWed14,hrWed15,hrWed16,hrWed17,hrWed18,hrWed19,hrWed20,hrWed21,hrWed22,hrWed23
									, hrThu00,hrThu01,hrThu02,hrThu03,hrThu04,hrThu05,hrThu06,hrThu07,hrThu08,hrThu09,hrThu10,hrThu11,hrThu12,hrThu13,hrThu14,hrThu15,hrThu16,hrThu17,hrThu18,hrThu19,hrThu20,hrThu21,hrThu22,hrThu23
									, hrFri00,hrFri01,hrFri02,hrFri03,hrFri04,hrFri05,hrFri06,hrFri07,hrFri08,hrFri09,hrFri10,hrFri11,hrFri12,hrFri13,hrFri14,hrFri15,hrFri16,hrFri17,hrFri18,hrFri19,hrFri20,hrFri21,hrFri22,hrFri23
									, hrSat00,hrSat01,hrSat02,hrSat03,hrSat04,hrSat05,hrSat06,hrSat07,hrSat08,hrSat09,hrSat10,hrSat11,hrSat12,hrSat13,hrSat14,hrSat15,hrSat16,hrSat17,hrSat18,hrSat19,hrSat20,hrSat21,hrSat22,hrSat23
									, hrSun00,hrSun01,hrSun02,hrSun03,hrSun04,hrSun05,hrSun06,hrSun07,hrSun08,hrSun09,hrSun10,hrSun11,hrSun12,hrSun13,hrSun14,hrSun15,hrSun16,hrSun17,hrSun18,hrSun19,hrSun20,hrSun21,hrSun22,hrSun23
									} ).
-record( history, { sinkID 
									, week
									, series = orddict:new() 
									}).



%% ====================================================================
%% External functions
%% ====================================================================
% Add usage into the load-history for the sink at the slot indicated.
add( SinkID, Seconds, Usage ) ->
		% Figure out the week from the Gregorian-seconds.
		Week = seconds_to_week( Seconds ),
		% Retrieve the week`s history from file / cache.
		History = retrieve_week( SinkID, Week ),
		% Add the usage into appropriate interval slot.
		{ NewHistory, Sum } = add_usage_into_slot( History, Seconds, Usage ),
		% Return the sum for that slot (caller may check for over-loaded sink).
		{ NewHistory, Sum }.		
		

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

% Convert gregorian-seconds to week
% Week = seconds_to_week( Seconds )
seconds_to_week( Seconds ) ->
		{ Date, Time } = calendar:gregorian_seconds_to_datetime(Seconds),
		{ Year, Month, Day } = Date,
		Days = calendar:date_to_gregorian_days(Year, Month, Day),
		Week = (Days - 2) div 7,
		WeekDate = calendar:gregorian_days_to_date( Week * 7 ),
		WeekDate.
		
% Retrieve the week`s history from file / cache.
%		#history{} = retrieve_week( SinkID, Week )
retrieve_week( SinkID, Week ) ->
		#history{ sinkID = SinkID, week = Week }.   % TODO: Replace this MOCK.

% Add the usage into appropriate interval slot.
% Return the sum for that slot (caller may check for over-loaded sink).
% Sum = add_usage_into_slot( History, Seconds, Usage )
add_usage_into_slot( History, Seconds, Usage ) ->
		% Create slot-key.
		{ Date, Time } = calendar:gregorian_seconds_to_datetime(Seconds),
		DayOfWeek = calendar:day_of_the_week(Date),
		{ Hour, _Minute, _Second } = Time,
		HourOfWeek = Hour + ( DayOfWeek * 7 ) - 6,
		
		% Get prior usage (if any) from the slot
    Result = orddict:find( HourOfWeek, History#history.series ),
		Sum = case Result of
							{ok,Value} ->
									Value + Usage;
							error ->
									Usage;
							_ ->
									exit("Unexpected result from call to orddict")
					end,
		% Now update the series.
		NewHistory = orddict:store(HourOfWeek, Sum, History),
		% Return updated history and sum
		{ NewHistory, Sum }.

add_usage_into_slotV00( History, Seconds, Usage ) ->
		{ Date, Time } = calendar:gregorian_seconds_to_datetime(Seconds),
		DayOfWeek = calendar:day_of_the_week(Date),
		{ Hour, _Minute, _Second } = Time,
		HourOfWeek = Hour + ( DayOfWeek * 7 ) - 6,
		Element = element( HourOfWeek + 3, History ),
		Sum = case Element of
							undefined ->
									Usage;
							Prior ->
									Prior + Usage
					end,
		% And how will I update the element?
		Sum.
