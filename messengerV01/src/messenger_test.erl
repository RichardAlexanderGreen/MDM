% Copyright 2011 Richard Alexander Green
% Description : Practice Test Driven Design (and Test Driven Development).
% - This module tests the Messenger API
% -------------------------------------------------------------------
-module(messenger_test).

-export([handle_notice/3]).

-compile([export_all]).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%-include( "mdm_objects.hrl" ).

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

% SUBSCRIBER MUST IMPLEMENT handle_notice/3 INTERFACE
% This version is onlyl a helper for this test module.

handle_notice( messenger_test_topic, Notice_ID, Notice ) ->
		?debugVal( { handle_notice, Notice_ID, Notice } ),
		ets:insert(notices_seen_so_far, { Notice_ID, Notice }),
		ok;

handle_notice( Topic, Notice_ID, Notice ) ->
		error_logger:error_report(warning, { 'Did not expect this notice in test:', Topic, Notice_ID, Notice}),
		not_ok. 



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TESTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Messenger API
% --------- : ---------------------------------------------------------
% Actor     : Request
% --------- : ---------------------------------------------------------
% Publisher : Publish notice N on topic T.
% Subscriber: Send me a copy of all future notices published on topic T.
% Subscriber: Replay all notices on topic T since notice id I.
% Subscriber: Stop sending me notices on topic T.

% Precondition: Messenger service is started. ""application:start( messenger )."" in test script.

% Check that messenger service is running / accessible on this node.
is_running_test() ->
		Result = messenger:is_running(),
		?assert( Result ).


% Subscriber: Send me a copy of all future notices published on topic T.
start_subscription_test() ->
		ToTopic = messenger_test_topic,
		messenger:subscribe( self(), ToTopic ).

setup_publish_test() ->
		% Create the table used to check receipt of what we publish.
		ets:new( notices_seen_so_far, [named_table] ).

% Publisher : Publish notice N on topic T.
publish_1_test() ->
		Notice = { "Notice 0 from ", ?MODULE, " for test"},
		OnTopic = messenger_test_topic,
		FromActorName = "messenger test",
		ID1 = messenger:publish( Notice, OnTopic, FromActorName ),
		?debugVal( ID1 ),
		?assert( is_integer( ID1 ) ).

% Publisher : Publish notice N on topic T.
publish_2_test() ->
		Notice = { "Notice 1 from ", ?MODULE, " for test"},
		OnTopic = messenger_test_topic,
		FromActorName = "messenger test",
		ID1 = messenger:publish( Notice, OnTopic, FromActorName ),
		Notice2 = { "Notice 2 from ", ?MODULE, " for test"},
		ID2 = messenger:publish( Notice2, OnTopic, FromActorName ),
		?assert( ID1 < ID2 ).

% TODO - Figure out how to check that this module also receives notices via handle_notice/3.
% Suggestion: have handle_notice/3 put items into a table -- query the table for expected entries.

% TODO - Figure out how to check that notices are written into the messenger's disk_log.
% Suggestion: Since replay relies on disk_log, replay_test (next) may be sufficient.

% Subscriber: Replay all notices on topic T since notice id I.
replay_test() ->
		?assertEqual( replay_test, not_implemented ),
		dummy.  % TODO - Figure out how to test the replay feature.

% Subscriber: Stop sending me notices on topic T.
stop_subscription_test() ->
		ToTopic = messenger_test_topic,
		messenger:unsubscribe( self(), ToTopic ).











