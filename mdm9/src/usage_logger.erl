%%% Copyright Richard Alexander Green 2011
%%% Description: The usage_logger is designed to work in conjunction with the MDM framework.
%%% Uses disk_log utility -- 
%%% TODO: Consider date-stamping or circuit-stampting the filenames.
%%% -------------------------------------------------------------------
-module(usage_logger).

-behaviour(gen_server).
-define( SERVER, ?MODULE ).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define( TEST, true ).
-define( NODEBUG, true ).

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
-export([start/0, stop/0, log_usage/2]).

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

start() ->                        
		?debugMsg( start ),
		Args = [],
		Options = [],
		Result = gen_server:start( { global, usage_logger}, ?MODULE, Args, Options ),
		%?debugVal( {'start', Result }),
		Result.

stop() ->
		?debugMsg( stop ),
		gen_server:call( { global, usage_logger}, stop ).

log_usage( Key, Value ) ->
		gen_server:call( { global, usage_logger}, { Key, Value } ).


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
		State = #state{ log_file_name = "usage_log"},
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
		{reply, 'Closing usage_log', State };


handle_call( { Key, Value }, From, State) ->
		%?debugVal( {handle_call, Event, State } ),
		log( Key, Value, State ),
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast( { Key, Value }, State) ->
		%?debugVal( { handle_cast, { Key, Value }, State } ),
		log( Key, Value, State ),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info( { Key, Value }, State) ->
		log( Key, Value, State ),
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
		OpenLogResult = disk_log:open([ { name, usage_log }
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



log( Key, Value, State  ) ->
		Log = State#state.log,
		LogResult = disk_log:log( Log, { Key, Value } ).

		%?debugVal( LogResult ).
		

close_log_file( State ) ->
		Log = State#state.log,
		disk_log:sync( Log ),
		%?debugVal( disk_log:info(Log) ),
		
		disk_log:close(Log),
    ok.
		

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  TESTS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

open_and_close_log_file_test() ->
		State = #state{ log_file_name = "test_log"},
		NewState = open_log_file( State ),
		?debugVal( NewState ),
		close_log_file( NewState ).

start_test() ->
		start().

log_usage_test() ->
		MyID = 'usage_logger:test()',
		PriorDateTime = calendar:local_time(),
		Key = { MyID, PriorDateTime },
		{ Date, Time } = PriorDateTime,
		Usage = 1234,
		History = load_history:put_usage_for_interval( Usage, Date, Time, undefined ),
		Value = History,
		log_usage( Key, Value ).

stop_test() ->
		stop().




																	


