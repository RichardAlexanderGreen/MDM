% Copyright Richard Alexander Green 2011
% Description: This is a skeleton -- I wrote it to obtain a better understanding of gen_event behavior.
%-------------------
-module(event_mgr).

%%
%% Include files
%%
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

-define( EVENT_MANAGER, { global, ?MODULE} ).

%%
%% Exported Functions
%%
-export([ start_alone/0
				, start_supervised/0
				, add_stand_alone_event_handler/2
				, add_supervised_event_handler/2
				, call_handler/2 
				, send_event_to_all_handlers/1 ]).

%%
%% API Functions
%%

% Start a stand-alone event manager (me).
start_alone() ->
		gen_event:start( ?EVENT_MANAGER ).         % Choose global / local instance

% Start a supervised event manager (me) -- should be called by a supervisor.
start_supervised() ->
		gen_event:start_link( ?EVENT_MANAGER ).    % Choose global / local instance

% Start a stand-alone event handler in the context of an event manager instance (me).
% This event-handler's existence will be independent of the caller's.
% Handler = Module | { Module, Id }
% Arguments = (Whatever the handler init/1 needs)
add_stand_alone_event_handler( Handler, Arguments ) ->
		Result = gen_event:add_handler( ?EVENT_MANAGER, Handler, Arguments ),
	  %?debugVal( Result ),
	  Result.

% Start a supervised event handler in the context of an event manager instance (me).
% This event-handler will terminate when the caller terminates.
add_supervised_event_handler( Handler, Arguments ) ->
		Result = gen_event:add_sup_handler( ?EVENT_MANAGER, Handler, Arguments ),
		%?debugVal( Result ),
	  Result.

get_handlers() ->
		List = gen_event:which_handlers(?EVENT_MANAGER),
		List.

% Remove an event handler
remove_event_handler( Handler ) ->
		TerminateArguments = 'Someone called event_mgr:remove_event_handler ',
		Result = gen_event:delete_handler( ?EVENT_MANAGER, Handler, TerminateArguments ),
		?debugVal( Result ),
		Result.

% Send event to all event handlers.  ( asynchronous - does not wait )
send_event_to_all_handlers( Event ) ->
		gen_event:notify( ?EVENT_MANAGER, Event).       % Will return  ok  regardless.

% Send event to all event handlers and wait until it is processed by all of them.
send_event_to_all_handlers_and_wait( Event ) ->
		gen_event:sync_notify( ?EVENT_MANAGER, Event).  % Will return  ok  regardless.

% Call a specific handler
call_handler( 'n/a', Request ) ->
		?debugVal( {call_handler, 'n/a', Request });

call_handler( Handler, Request ) ->
		?debugVal( {call_handler, Handler, Request }),
		
		Result = gen_event:call(?EVENT_MANAGER, Handler, Request, 9000),
		%Result = 'not implemented',
		
		?debugVal( Result ).

stop() ->
		gen_event:stop( ?EVENT_MANAGER ).


%%
%% Local Functions
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  TESTS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_supervised_test() ->
		Result = start_supervised(),
		{ok, PID } = Result,
		?assert( is_pid( PID ) ),
		gen_event:stop( PID ).        % Stop so that  start_alone_test  can run.

start_alone_test() ->
		Result = start_alone(),
		{ok, PID } = Result,
		?assert( is_pid( PID ) ).

add_stand_alone_event_handler_test() ->
		Handler = { event_handler, instance_1 },
		Arguments = [ instance_1_arguments ],
		Result = add_stand_alone_event_handler( Handler, Arguments ),
		?assertEqual( ok, Result ).

add_supervised_event_handler_test() ->
		Handler = { event_handler, instance_2 },
		Arguments = [ instance_2_arguments ],
		Result = add_supervised_event_handler( Handler, Arguments ),
		?assertEqual( ok, Result ).

get_handlers_test() ->
		List = get_handlers(),
		?assertEqual( 2, length(List) ).

send_event_to_all_handlers_test() ->
		Event = { event, 'This is only a test ', send_event_to_all_handlers_test },
		Result = send_event_to_all_handlers( Event ),
		?assertEqual( ok, Result ).

send_event_to_all_handlers_and_wait_test() ->
		Event = { event, 'This too is only a test ', send_event_to_all_handlers_and_wait },
		Result = send_event_to_all_handlers_and_wait( Event ),
		?assertEqual( ok, Result ).

remove_event_handler_test() ->
		Handler = { event_handler, instance_1 },
		Result = remove_event_handler( Handler ),
		?assertEqual( ok, Result ).

call_one_handler_test() ->
		Handler =  { event_handler, instance_2 },
		Request = 'Test call from event_mgr',
		Result = call_handler( Handler, Request ),
		?assertEqual( ok, Result ).
		
stop_test() ->
		% Test termination behavior --- Will kill event-manager too !!!
		stop().



