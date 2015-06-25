%% Copyright: Richard Alexander Green 2011
%% Description: The primary purpose is to test actor.hrl
-module(actor_test).

%%
%% Include files
%%
-define( NOTEST, true ).
-define( NODEBUG, true ).
-define( LOGLEVEL, 3 ).  % LOG info level and above. -- See log/2.
% ALL=0 < TRACE=1 < DEBUG=2 < INFO=3 < WARN=4 < ERROR=5 < FATAL=6 < OFF=7

-include_lib("eunit/include/eunit.hrl").

-include( "../include/actor.hrl").

-record( state, {} ).


%%
%% API Functions
%%



%%
%% Local Functions
%%
do( Action, State ) ->
		State.

answer( Request, _From, _State ) ->
		?debugVal( {answer, Request, _From, _State } ),
		
		Reply = {ok},
		Reply.


custom_init( ArgList )->
		?debugVal( {trace, custom_init} ),
		
		State = #state{},
		State.

custom_shutdown( State ) ->
		?debugVal( {trace, custom_shutdown}),
		
		ok.

%%
%% //////////////////////////// Test Functions -- /////////////////////////////////////////
%%

% Test service initialization.

% test_init() ->
%		State = init( [] ), % This calls custom_init( ArgList )
%		ok.

% Test logging
log_open_test() ->
		log_open( "actor_test_log.txt"),
		ok.


log_test() ->
		log( debug, {'This is only a test of the logging'} ),
		ok.

% Test local send.
local_send_test() ->
		actor_test ! {test,test_local_send},
		ok.


% Test global send.

global_send_test() ->
		global:send( actor_test, {test, test_global_send } ),
		ok.

% Test normal termination. 

terminate_shutdown_test() ->
		State = #state{},
		
		terminate( shutdown, State ),  % This calls custom_shutdown( State ) and log_close.
		ok.
