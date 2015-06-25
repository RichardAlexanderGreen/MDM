% Copyright Richard Alexander Green 2011
% Description: subscriber_proxy is an event handler in the messenger framework.
% Strategy:
% - One messenger is a gen_event event manager.
% - Each messenger_subscriber is a gen_event event handler.
% - Each subscriber is represented by a subscriber_proxy.
% - Each subscriber_proxy passes notices for its assigned topic on to the subscriber.
% - To provide replay, one subscriber writes all notices to a log file.

%%% -------------------------------------------------------------------
-module(subscriber_proxy).

-behaviour(gen_event).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define( TEST, true ).
-define( NODEBUG, true ).

% Set log level
% ALL=0 < TRACE=1 < DEBUG=2 < INFO=3 < WARN=4 < ERROR=5 < FATAL=6 < OFF=7
% Level semantics are defined in the org.apache.log4j.Level class.
-define( LOGLEVEL, 3 ).             % 3 means log info level and above. -- See log/2.

-define( CHECKPOINT_EVERY, 2000 ).  % Default value -- How often do we flush tables?

-include_lib("eunit/include/eunit.hrl").

-ifdef( DEBUG ).
-compile( export_all ).
-endif.

%% --------------------------------------------------------------------
%% External exports
-export([]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).


-include( "records.hrl").

-record( state, { module, topic }).



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
init( Arguments ) ->
		?debugVal( { init, Arguments } ),
		#subscriber_attributes{ module = Module, topic = Topic } = Arguments,
    {ok, #state{  module = Module, topic = Topic }}.

%% --------------------------------------------------------------------
%% Func: handle_event/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler
%% --------------------------------------------------------------------
handle_event( Event, State ) when State#state.topic == all ->
		%?debugVal( { handle_event, topic_all, Event, State } ),
		
		forward_event( Event, State ),
		{ok, State };

handle_event( Event, State) when State#state.topic == Event#event.topic ->
		?debugVal( { handle_event, topic_match, Event, State } ),
		forward_event( Event, State ),
    {ok, State};

handle_event( Event, State ) ->
		%?debugVal(  { handle_event, no_topic_match, Event, State }  ),
		{ok, State }.


forward_event( Event, State ) ->
		#event{ topic = Topic, notice_ID = Notice_ID, from = From, notice = Notice } = Event,
		?debugVal( { Topic, Notice, From, State } ),
		Subscriber = State#state.module,
		?debugVal( { forward_event, Subscriber, Event, State } ),
		Result_forward_event = gen_server:cast( Subscriber, Event ),
		?debugVal( Result_forward_event ),
		Result_forward_event.

%% --------------------------------------------------------------------
%% Func: handle_call/2
%% Returns: {ok, Reply, State}                                |
%%          {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%          {remove_handler, Reply}
%% --------------------------------------------------------------------
handle_call( remove_handler, _State ) ->
		{remove_handler, 'from handle_call/2'};

handle_call(Request, State) ->
		?debugVal( {handle_call, Request, State }),
    Reply = ok,
    {ok, Reply, State}.

%% --------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    ?debugVal( {handle_info, Info, State }),
    {ok, State}.

%% --------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any
%% --------------------------------------------------------------------
terminate(Reason, State) ->
		?debugVal( { 'event_handler:terminate/2', Reason, State } ),
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

