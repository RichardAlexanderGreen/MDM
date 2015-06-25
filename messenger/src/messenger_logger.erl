%%% Copyright Richard Alexander Green 2011
%%% Description: The messenger_logger is designed to work in conjunction with the messenger framework.
%%% - It subscribes to all topics and writes their event notices to a log file.
%%% - Each notice is assigned a unique notice_ID.
%%% - Notice IDs are monotonically increasing - (currently implemented with now() )
%%% - The messenger_logger also implements a replay feature.
%%% NOTE: The functionality here could also be implemented using
%%%   the log_mf_h module from stdlib (an event handler that logs events) with
%%%   the rb module from the sasl library (designed to scan log_mf_h files).
%%%   I wrote this before discovering those utilities
%%%     and before I gained much facility with Erlang.
%%%   The rb module provides more filtering capability
%%%     than I have here.
%%% -------------------------------------------------------------------
-module(messenger_logger).

-behaviour(gen_server).
-define( SERVER, ?MODULE ).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define( TEST, true ).
-define( DEBUG, true ).

% Set log level
% ALL=0 < TRACE=1 < DEBUG=2 < INFO=3 < WARN=4 < ERROR=5 < FATAL=6 < OFF=7
% Level semantics are defined in the org.apache.log4j.Level class.
-define( LOGLEVEL, 3 ).             % 3 means log info level and above. -- See log/2.

-define( CHECKPOINT_EVERY, 5000 ).  % Default value -- How often do we flush tables?

-include_lib("eunit/include/eunit.hrl").

-ifdef( DEBUG ).
-compile( export_all ).
-endif.

-include( "records.hrl" ).

%% --------------------------------------------------------------------
%% External exports
-export([start/0, stop/0, log/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, { log, log_file_name, last_notice_ID }).

%% ====================================================================
%% External functions
%% ====================================================================
start_server() ->
		?debugMsg( start_server),
		
		gen_server:start_link(                       % Start server instance.
                           { global, ?SERVER },   % Give it a GLOBAL name.
													 ?MODULE, []           % The module containing and arguments for init/1.
													 , []                  % options
                           ).  

start() ->                         % TODO - Open the disk_log
		?debugMsg( start ),
		Args = [],
		Options = [],
		Result = gen_server:start( {global, messenger_logger}, ?MODULE, Args, Options ),
		%?debugVal( {'start', Result }),
		Result.

stop() ->
		?debugMsg( stop ),
		gen_server:call( {global, messenger_logger}, stop ).

log( Event ) ->
		gen_server:call( {global, messenger_logger}, Event ).


%handle_notice( Event ) ->          % TODO - Write event to disk_log
%		?debugVal( Event ),
%		ok.

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
		?debugMsg( init ),
		State = #state{ log_file_name = "messenger_log"},
		NewState = open_log_file( State ),
    {ok, NewState }.

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
handle_call( stop, From, State ) ->
		terminate( {'Received "stop" message from: ', From}, State ),
		{reply, 'Closing messenger_log', State };

handle_call( {replay, Topic, Since }, From, State ) ->
		Result = replay( Topic, Since, State ),
		{reply, Result, State };

handle_call(Event, From, State) ->
		%?debugVal( {handle_call, Event, State } ),
		log_event( Event, State ),
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast( Event, State) ->
		%?debugVal( {handle_cast, Event, State } ),
		log_event( Event, State ),
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
		close_log_file( State ),
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

open_log_file( State ) ->		
		?debugVal( {'open_log_file( State )', State } ),
		LogFileName = State#state.log_file_name,
		MaxNoBytes = 1024 * 1024 * 1024,
		OpenLogResult = disk_log:open([ { name, messenger_log }
														      , { file, LogFileName }
													        , { type, wrap }
													        , { size, { MaxNoBytes, 32 }  }   % { MaxNoBytes, MaxNoFiles }
														      ]),
		%?debugVal( OpenLogResult ),
		case OpenLogResult of
				{ ok, Log } ->
						Log;
				{repaired, Log, {recovered, Rec}, {badbytes, Bad} } ->
						error_logger:warning_report({ 'log file repaired'}),
						Log;
				Surprise ->
						error_logger:error_report({'disk_log:open produced surprise: ', Surprise }),
						Log = log_not_opened
		end,
		%?debugVal( { open_log_file, Log } ),
		NewState = State#state{ log = Log },
		NewState.



log_event( Event, State  ) ->
		Log = State#state.log,
		Notice_ID = Event#event.notice_ID,
		LogResult = disk_log:log( Log, { Notice_ID, Event } ),
		%?debugVal( LogResult ),
		NewState = State#state{ last_notice_ID = Notice_ID },
		NewState.

replay( Topic, SinceNoticeID ) ->
		List = gen_server:call( { global, messenger_logger }, {replay, Topic, SinceNoticeID } ),
		List.


replay( Topic, SinceNoticeID, State ) ->
		Log = State#state.log,
		{ _Continuation2, List } = disk_log:chunk(Log, start),
		% TODO -- Need to filter the list on SinceNoticeID
		%?debugVal( { _Continuation2, List } ),
		Pred = fun( LogEntry ) ->
									 { Notice_ID, Event } = LogEntry,
									 
									 ( Notice_ID > SinceNoticeID ) and ( Event#event.topic == Topic ) 
					 end,
		
		FilteredList = lists:filter(Pred, List),
		%?debugVal( FilteredList ),
		FilteredList.


close_log_file( State ) ->
		Log = State#state.log,
		disk_log:sync( Log ),
		%?debugVal( disk_log:info(Log) ),
		
		disk_log:close(Log),
    ok.
		

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  TESTS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

open_log_file_test() ->
		State = #state{ log_file_name = "test_log"},
		NewState = open_log_file( State ),
		%?debugVal( NewState ),
		NewState.

log_event_test() ->
		Event = #event{},
		State = #state{ log_file_name = "test_log"},
		NewState = open_log_file( State ),		
		NewState2 = log_event( Event, NewState  ),
		%?debugVal( NewState2 ),
		NewState2.

close_log_file_test() ->
		State = log_event_test(),
		close_log_file( State ).

start_test() ->
		start().

log_test() ->
		T = now(),
		put( t, T ),
		Event = #event{ notice_ID = T, topic = 'test log/1', notice = 'This is only a messenger_logger test of log/1'},
		put( event, Event ),
		log( Event ).

replay_test() ->
		Topic = 'test log/1',
		%Topic = messenger_test_topic,
		T = get( t ),
		{ A, B, C } = T,
		Since = { A, B, C-1 },
		List = replay( Topic, Since ),
		%?debugVal( List ),
		?assert( length( List ) == 1 ),
		Event = get(event ),
		?assertEqual( [ {T, Event} ], List ).


stop_test() ->
		stop().




																	


