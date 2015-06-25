%%% -------------------------------------------------------------------
%%% Author  : admin
%%% Description :
%%%
%%% Created : Mar 17, 2011
%%% -------------------------------------------------------------------
-module(asset_mgr).

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


-include( "../include/mdm_objects.hrl" ).

-import( quad, [quad/0] ).


%% --------------------------------------------------------------------
%% External exports
-export([]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, { counters = orddict:new() }).

-record( event, {  notice_ID = 0, topic, from, notice}).


%% ====================================================================
%% External functions
%% ====================================================================


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
		process_flag(trap_exit, true),
		
		subscribe_to_asset_events(),
    {ok, #state{}}.

subscribe_to_asset_events() ->
		?debugVal( {subscribe_to_asset_events} ),
		
		messenger:subscribe( self(), new_service ),
		messenger:subscribe( self(), service_installed_on_circuit ),
		messenger:subscribe( self(), service_draws_on_transformer ),
		messenger:subscribe( self(), meter_configured ),
		messenger:subscribe( self(), meter_installed ),
		% messenger:subscribe( self(), _ ),
		
		ok.

start_server() ->
		?debugVal( {start_server_0, no_args } ),
		process_flag(trap_exit, true),
		
		gen_server:start_link(                       % Start server instance.
                           { global, ?SERVER },  % Give it a global name.
													 ?MODULE, []           % The module containing and arguments for init/1.
													 , []                  % options
                           ).  

start_server(Args) ->
		?debugVal( {start_server_1, Args } ),
		process_flag(trap_exit, true),
		
		gen_server:start_link(                       % Start server instance.
                           { global, ?SERVER },  % Give it a global name.
													 ?MODULE, Args         % The module containing and arguments for init/1.
													 , []                  % options
                           ).  

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
    ?debugVal( { handle_call, Request, From }),
		Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast( { event, Event }, State) -> 
		?debugVal( { 'handle_cast event:', { event, Event } }),
		{ NoticeID, Notice } = Event,
		NewState = do( Notice, State ),
    {noreply, NewState};

handle_cast( #event{ notice_ID = NoticeID, topic = Topic, from = From, notice = Notice }, State) -> 
		%?debugVal( { 'handle_cast #event.notice: ', Notice  }),
		NewState = do( Notice, State ),
    {noreply, NewState};


handle_cast( Msg, State) -> 
		?debugVal( { 'handle_cast Msg:', Msg }),
		error_logger:error_report( error, {'asset_mgr given unexpected message: ', Msg}),
		
		{noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    ?debugVal( { handle_info, Info }),
		{noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
		?debugVal( { terminate, Reason, State } ),
		
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
																		 
count( Category, State ) ->
		%?debugVal( {count, Category, State } ),
		
		Counters = State#state.counters,
		UpdatedCounters = orddict:update_counter( Category, 1, Counters ),
		NewState = State#state{ counters = UpdatedCounters },
		%?debugVal( {count, Category, NewState } ),	
		NewState.

do( { service, Service, installed_on_circuit, Circuit, W5 }, State ) ->
		%?debugVal( { service, Service, installed_on_circuit, Circuit } ),
		quad() !  { add_relationship, Service, installed_on_circuit, Circuit, W5 },
		NewState = count( installed_on_circuit, State ),
		NewState;

do( { service, Service, draws_on_transformer, Transformer, W5 }, State ) ->
		%?debugVal( { service, Service, draws_on_transformer, Transformer } ),
		quad() ! {  add_relationship, Service, draws_on_transformer, Transformer, W5 },
		NewState = count( draws_on_transformer, State ),
		NewState;

do(  { new_service, Service, record, ServiceRecord, W5 }, State ) ->
		%?debugVal( { service, Service, record, ServiceRecord } ),
		#service{ oid = Service
						, transformer = Transformer
						, circuit = Circuit 
					  , coordinates = Coordinates
						, premise = Premise
						}  = ServiceRecord,
    Entity = Service,
    NameValueList = [ { transformer, Transformer }
                    , { circuit, Circuit }
                    , { coordinates, Coordinates }
                    , { premise, Premise } ],
		quad() ! { set_attribute_values, Entity, NameValueList, W5 },
		NewState = count( new_service, State ),
		NewState;

do( { meter, Meter, record, MeterRecord, W5 }, State ) ->
		%?debugVal( { meter, Meter, record, MeterRecord } ),
		#meter{ oid = Meter
					, type_of_meter = MeterType
					, configuration = Configuration
					} = MeterRecord,

    Entity = Meter,
    NameValueList = [ { type_of_meter, MeterType }
                    , { configuration, Configuration }
                    ],
		quad() ! { set_attribute_values, Entity, NameValueList, W5 },
		
		NewState = count( meter, State ),
		NewState;

do(  { meter, Meter, installed_on_service, Service, W5 }, State ) ->
		%?debugVal( { meter, Meter, installed_on_service, Service } ),
		
		quad() ! {  add_relationship, Meter, installed_on_service, Service, W5 },
		NewState = count( installed_on_service, State ),
		NewState;

%do( _, State ) ->
%		State;

do( UnknownMessage, State ) ->
		error_logger:error_report( logic_error, {'asset_mgr does not recognize request:', UnknownMessage }),
		
		NewState = count( unknown_message, State ),
		NewState.


