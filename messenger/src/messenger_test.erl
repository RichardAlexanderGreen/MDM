% Copyright 2011 Richard Alexander Green
% Description : Practice Test Driven Design (and Test Driven Development).
% - This module tests the Messenger API
% -------------------------------------------------------------------

-module(messenger_test).

-compile([export_all]).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-define( TEST, true ).
-define( DEBUG, true ).
-define( RUNNING_STANDALONE, false ).

% Set log level
% ALL=0 < TRACE=1 < DEBUG=2 < INFO=3 < WARN=4 < ERROR=5 < FATAL=6 < OFF=7
% Level semantics are defined in the org.apache.log4j.Level class.
-define( LOGLEVEL, 3 ).             % 3 means log info level and above. -- See log/2.

-include_lib("eunit/include/eunit.hrl").

-ifdef( DEBUG ).
-compile( export_all ).
-endif.

-include( "records.hrl").

%% --------------------------------------------------------------------




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


start_stand_alone_test() when ?RUNNING_STANDALONE ->
		messenger:start_stand_alone_messenger();
start_stand_alone_test() ->
		ok.

% Check that messenger service is running / accessible on this node.
is_running_test() ->
		timer:sleep(100),
		Result = messenger:is_running(),
		?assert( Result ).

% Subscriber: Send me a copy of all future notices published on topic T.
start_subscription_test() ->
		ToTopic = messenger_test_topic,
		Subscriber = {global, subscriber_mock},
		gen_server:start( Subscriber, subscriber_mock, [], [] ),
		messenger:subscribe( Subscriber , ToTopic ).

setup_publish_test() ->
		% Create the table used to check receipt of what we publish.
		ets:new( notices_seen_so_far, [named_table] ).

% Publisher : Publish notice N on topic T.
publish_1_test() ->
		Notice = { "Notice 0 from ", ?MODULE, " for test"},
		OnTopic = messenger_test_topic,
		FromActorPID = self(),
		ID1 = messenger:publish( Notice, OnTopic, FromActorPID ),
		?debugVal( ID1 ).

% Publisher : Publish notice N on topic T.
publish_2_test() ->
		Notice = { "Notice 1 from ", ?MODULE, " for test"},
		OnTopic = messenger_test_topic,
		FromActorPID = self(),
		ID1 = messenger:publish( Notice, OnTopic, FromActorPID ),
		Notice2 = { "Notice 2 from ", ?MODULE, " for test"},
		ID2 = messenger:publish( Notice2, OnTopic, FromActorPID ),
		?assert( ID1 < ID2 ).

% TODO - Figure out how to check that this module also receives notices via handle_notice/3.
% Suggestion: have handle_notice/3 put items into a table -- query the table for expected entries.

% TODO - Figure out how to check that notices are written into the messenger's disk_log.
% Suggestion: Since replay relies on disk_log, replay_test (next) may be sufficient.

% Subscriber: Replay all notices on topic T since notice id I.
%replay_test() ->
%		?assertEqual( replay_test, not_implemented ),
%		dummy.  % TODO - Figure out how to test the replay feature.

% Subscriber: Stop sending me notices on topic T.
stop_subscription_test() ->
		ToTopic = messenger_test_topic,
		Subscriber = {global, subscriber_mock},
		UnsubscribeResult = messenger:unsubscribe( Subscriber, ToTopic ),
		%?debugVal( UnsubscribeResult ),
		?assertMatch( ok, UnsubscribeResult ).

%stop_messenger_test() ->
%		messenger:stop().













