% Copyright Richard Alexander Green 2011
% Description : This module is the local proxy for the messenger framework.
% - It sends notices to all subscribers using gen_event:notify(EventMgrRef, Event).
% - Notices are written to a local log with a serial number to enable replay.

-module( messenger ).

-behaviour(gen_server).

-define( SERVER, ?MODULE ).
-define( EVENT_MGR, messenger_event_mgr ).
-define( SUBSCRIPTIONS, subscription_table ).   % Internal name of subscription table

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
-export([is_running/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, { last_serial, log_file } ).

%% ====================================================================
%% External functions
%% ====================================================================

is_running() ->
		P = global:whereis_name( ?MODULE ),
		is_pid( P ).

subscribe( Actor, ToTopic ) ->
		?debugVal( { subscribe, Actor, ToTopic } ),
		%?debugVal( ets:info( ?SUBSCRIPTIONS ) ),
		ets:insert( ?SUBSCRIPTIONS, { ToTopic, Actor } ),
		ok.

unsubscribe( Actor, ToTopic ) ->
		?debugVal( { unsubscribe, Actor, ToTopic }  ),
		Count = ets:match_delete( ?SUBSCRIPTIONS, {ToTopic, Actor}),
		?assertEqual( true, Count ),
		ok.

publish( Notice, OnTopic, FromActorName ) ->
		%?debugVal( {publish, Notice, OnTopic, FromActorName } ),
		Result = gen_server:call( ?SERVER, {notice, OnTopic, Notice } ),
		Result.





%% ====================================================================
%% Server functions
%% ====================================================================
start_server( Arguments ) ->
		?debugVal( { start_server, Arguments } ),
		
		Result = gen_server:start_link(              % Start server instance.
                           { local, ?SERVER }    % Give it a LOCAL name -- enable use of !
													 ,?MODULE              % The module containing init/1
												   , Arguments           % parameters for init/1.
													 , []                  % options
                           ),
		?debugVal( { start_link_result, Result } ),
		Result.
		
%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init( Args ) ->
		?debugVal({init, Args } ),
		
		process_flag( trap_exit, true ),   % Enable terminate -- So we can flush tables on shutdown
		global:register_name( messenger, self() ),    % Enable remote nodes to use global:send( quad, {Action} )

		% Initialize table of subscriptions
		NewTable = ets:new( ?SUBSCRIPTIONS, [public, named_table, bag] ),
		%?debugVal( NewTable ),
		%?debugVal( ets:info(NewTable) ),

		
		% Get log_file
		[ {log_file, LogFileName } ] = Args,
		%?debugVal( LogFileName ),
		MaxNoBytes = 1024 * 1024,
		OpenLogResult = disk_log:open([ { name, messenger_log }
														      , { file, LogFileName }
													        , { type, wrap }
													        , { size, { MaxNoBytes, 32 }  }   % { MaxNoBytes, MaxNoFiles }
														      ]),
		%?debugVal( OpenLogResult ),
		
		% TODO - Get the last term from the log file and extract the serial number.
		LastSerialNumber = 0,   % TODO - READ THE SERIAL NUMBER FROM THE LAST TERM IN THE LOGFILE
    {ok, #state{ log_file = messenger_log, last_serial = LastSerialNumber }}.

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
handle_call({notice, OnTopic, Notice }, From, State) ->
		% Assume the Request is a Notice
		NewState = log_and_send( Notice, OnTopic, State ),
    Reply = NewState#state.last_serial,
    {reply, Reply, NewState }.

% log_and_send/3 logs the notice on the log-file and then sends a gen_event:notify.
% TODO - We might decouple the disk_log function by making it an eventy handler also.
% -- But the disk_log is essential to the replay feature.

log_and_send( Notice, OnTopic, State ) ->
		SerialNumber = 1 + State#state.last_serial,
		LogFile = State#state.log_file,
		disk_log:log( LogFile, { SerialNumber, Notice } ),
		NewState = State#state{ last_serial = SerialNumber },
		%?debugVal( { log_and_send, SerialNumber, OnTopic, Notice  } ),
		
		gen_event:notify( messenger_event_mgr, { notice, SerialNumber, OnTopic, Notice } ),  
    NewState.

% Make me an Event Manager.  Subscribers are event handlers.
start_link() ->
    gen_event:start_link( {local, ?SERVER} ).


%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
		error_logger:error_report(logic_error, { ?MODULE, 'does not expect handle_cast message', Msg } ),
		
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
		error_logger:error_report(logic_error, { ?MODULE, 'does not expect handle_info message', Info } ),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
		% Close the log file.
		LogRef = State#state.log_file,
		?debugVal( LogRef ),
		CloseResult = disk_log:close( LogRef ),
		?debugVal( CloseResult ),
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TESTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_event_manager_test() ->
		LogFileName = "publisher_test_logfile",
		Args = [{ log_file = LogFileName }],
		init( Args ).


