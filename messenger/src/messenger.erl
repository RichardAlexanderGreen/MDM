%%% @doc This is a server template.
%%% Notices:
%%%
%%% Queries:
%%%
%%% Requests:
%%%
%%% @end
%%% @Author: RichardAlexanderGreen@gmail.com
%%% @copyright 2010 Richard Alexander Green

-module( messenger ).
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

-include_lib("eunit/include/eunit.hrl").

-ifdef( DEBUG ).
-compile( export_all ).
-endif.

-define( EVENT_MANAGER, { global, ?MODULE } ).

-include( "records.hrl" ).

-record( state, {  } ).

%% --------------------------------------------------------------------
%% External exports
-export( [ start_server/0
				 , is_running/0
				 , publish/3
				 , publish/4
				 , subscribe/2
				 , unsubscribe/2
				 ] ).

%% gen_server callbacks
-export( [init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3] ).

%% ====================================================================
%% External functions
%% ====================================================================
%%% Queries:


%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% @doc Starts the server.
%%
%% @spec start_server() -> {ok, Pid}
%% where
%% Pid = pid()
%% @end
%%--------------------------------------------------------------------
start_event_manager() ->
		?debugMsg( start_event_manager ),
		start_supervised_messenger().

start_server() ->
		?debugMsg( start_server),
		
		gen_server:start_link(                       % Start server instance.
                           { global, ?SERVER },   % Give it a GLOBAL name.
													 ?MODULE, []           % The module containing and arguments for init/1.
													 , []                  % options
                           ).  

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
				
		process_flag( trap_exit, true ),   % Enable terminate -- So we can flush tables on shutdown

		start_supervised_messenger(),
		
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
handle_call( Request, From, State) ->     % Synchronous -- Caller is waiting for reply.
		?debugVal( { handle_call, Request, From} ),	
    Reply = answer( Request ),
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast( Action, State ) ->           % Asynchronous -- No value is returned.
		?debugVal( { handle_cast, Action } ),	
		ok = do( Action ),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info( Action, State) ->
    ?debugVal( { handle_info, Action } ),	
		ok = do( Action ),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate( _Reason, _State) ->
		?debugVal( {terminate, _Reason, _State }),
		stop_logger(),
		
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change( _OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

do( Action ) ->
		{ ?MODULE, does_not_understand_action, Action }.

answer( Request ) ->
		{ ?MODULE, does_not_understand_request, Request }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Following pasted from prior messenger %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ---------------------------------------------------------------------------------------
%% API Functions
% ---------------------------------------------------------------------------------------

% Start a stand-alone event manager (me).
start_stand_alone_messenger() ->
		?debugMsg( start_stand_alone_messengers ),
		start_logger(), 
		gen_event:start( ?EVENT_MANAGER ),         % Choose global / local instance
		
		% Message_logger subscribes to all events.
		Logger = {global, messenger_logger},
		%Arguments = #subscriber_attributes{ module = messenger_logger, topic = all },
		%add_stand_alone_subscriber_proxy( Handler, Arguments ).
		subscribe( Logger, all ).

% Start a supervised event manager (me) -- should be called by a supervisor.
start_link(Args) ->
		?debugVal( {'start_link(Args)',Args} ),
		start_supervised_messenger().

start_supervised_messenger() ->
		?debugMsg( start_supervised_messenger ),
		% start_logger(),  % REMOVED BECAUSE SUPERVISOR WILL START IT	
		
    Result = gen_event:start_link( ?EVENT_MANAGER ),     % Choose global / local instance
		?debugVal( {'gen_event:start_link( ?EVENT_MANAGER )', Result} ),
		
		% Message_logger subscribes to all events.
		Logger = {global, messenger_logger},
		
		%add_stand_alone_subscriber_proxy( Handler, Arguments ).
		subscribe( Logger, all ),
		
    Result.

start_logger() ->
		?debugMsg( start_logger ),
		messenger_logger:start().

stop_logger() ->
		?debugMsg( stop_logger ),
		messenger_logger:stop().

% Is an instance of me running as an event manager?
is_running() ->
		% which_handlers(EventMgrRef) -> [Handler]
		Result_which_handlers = gen_event:which_handlers( ?EVENT_MANAGER ),
		%?assertEqual( [{subscriber_proxy,{{global,messenger_logger},all}}], Result_which_handlers ),
		%[{subscriber_proxy,{{global,messenger_logger},all}}] == Result_which_handlers.
		
		?assert( lists:any( fun(Element) -> {subscriber_proxy,{{global,messenger_logger},all}} == Element end, Result_which_handlers )),
		
		lists:any( fun(Element) -> {subscriber_proxy,{{global,messenger_logger},all}} == Element end, Result_which_handlers ).
											 

subscribe( Subscriber, Topic ) ->
		?debugVal( {subscribe, Subscriber, Topic }),
		
		Handler = { subscriber_proxy, { Subscriber, Topic } },
		Arguments = #subscriber_attributes{ module = Subscriber, topic = Topic },
		Result = add_stand_alone_subscriber_proxy( Handler, Arguments ),              %% TODO -- IS THIS CORRECT ???
		%?debugVal( {start, Result } ),
		Result.

publish( Notice, OnTopic, FromActorPID ) ->
		NoticeID = now(),
		publish( NoticeID, Notice, OnTopic, FromActorPID ).


publish( NoticeID, Notice, OnTopic, FromActorPID ) ->
		Event = #event{ notice = Notice, topic = OnTopic, from = FromActorPID, notice_ID = NoticeID },
		send_event_to_all_handlers( Event ),  %% The subscriber_proxy simply ignores events it is not subscribed to.
		NoticeID.

unsubscribe( Subscriber, ToTopic ) ->
		%?debugVal( { unsubscribe, Subscriber, ToTopic } ),
		Handler = { subscriber_proxy, { Subscriber, ToTopic } },
		UnsubscribeResult = remove_subscriber_proxy( Handler ),
		%?debugVal( UnsubscribeResult ),
		UnsubscribeResult.
		

% ---------------------------------------------------------------------------------------
%% Local Functions
% ---------------------------------------------------------------------------------------

% Start a stand-alone event handler in the context of an event manager instance (me).
% This event-handler's existence will be independent of the caller's.
% Handler = Module | { Module, Id }
% Arguments = (Whatever the handler init/1 needs)
add_stand_alone_subscriber_proxy( Handler, Arguments ) ->
		Result = gen_event:add_handler( ?EVENT_MANAGER, Handler, Arguments ),
	  %?debugVal( Result ),
	  Result.

% Start a supervised event handler in the context of an event manager instance (me).
% This event-handler will terminate when the caller terminates.
add_supervised_subscriber_proxy( Handler, Arguments ) ->
		Result = gen_event:add_sup_handler( ?EVENT_MANAGER, Handler, Arguments ),
		%?debugVal( Result ),
	  Result.

% Remove an event handler
remove_subscriber_proxy( Handler ) ->
		?debugVal( { remove_subscriber_proxy, Handler } ),
		TerminateArguments = 'Someone called event_mgr:remove_subscriber_proxy ',
		Result = gen_event:delete_handler( ?EVENT_MANAGER, Handler, TerminateArguments ),
		?debugVal( Result ),
		Result.

% Send event to all event handlers.  ( asynchronous - does not wait )
send_event_to_all_handlers( Event ) ->
		% TODO - THINK: Should we 'warn once' if there are no subscribers?
		% Missing or future subscribers may be able to replay (if messenger log does not roll-over first.)		
		gen_event:notify( ?EVENT_MANAGER, Event ).       % Will return  ok  regardless.

% Send event to all event handlers and wait until it is processed by all of them.
send_event_to_all_handlers_and_wait( Event ) ->
		NumberedEvent = Event#event{ notice_ID = now() },
		gen_event:sync_notify( ?EVENT_MANAGER, NumberedEvent).  % Will return  ok  regardless.

stop() ->
		?debugMsg( stop ),
		stop_logger(),
		gen_event:stop( ?EVENT_MANAGER ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  TESTS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_supervised_messenger_test() ->
		Result = start_supervised_messenger(),
		{ok, PID } = Result,
		?assert( is_pid( PID ) ),
		gen_event:stop( PID ).        % Stop so that  start_stand_alone_messenger_test  can run.

start_stand_alone_messenger_test() ->
		Result = start_stand_alone_messenger(),
		{ok, PID } = Result,
		?assert( is_pid( PID ) ).

add_stand_alone_subscriber_proxy_test() ->
		Handler = { subscriber_proxy, instance_1 },
		Arguments = [ instance_1_arguments ],
		Result = add_stand_alone_subscriber_proxy( Handler, Arguments ),
		?assertEqual( ok, Result ).

add_supervised_subscriber_proxy_test() ->
		Handler = { subscriber_proxy, instance_2 },
		Arguments = [ instance_2_arguments ],
		Result = add_supervised_subscriber_proxy( Handler, Arguments ),
		?assertEqual( ok, Result ).

send_event_to_all_handlers_test() ->
		Event = #event{ notice ='This is only a test ', topic = send_event_to_all_handlers_test, from = self() },
		Result = send_event_to_all_handlers( Event ),
		?assertEqual( ok, Result ).

send_event_to_all_handlers_and_wait_test() ->
		Event = #event{ notice ='This too is only a test ', topic = send_event_to_all_handlers_and_wait_test, from = self() },
		Result = send_event_to_all_handlers_and_wait( Event ),
		?assertEqual( ok, Result ).

remove_subscriber_proxy_test() ->
		Handler = { subscriber_proxy, instance_1 },
		Result = remove_subscriber_proxy( Handler ),
		?assertEqual( ok, Result ).
		
stop_test() ->
		% Test termination behavior --- Will kill event-manager too !!!
		stop().




