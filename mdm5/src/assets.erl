%%% -------------------------------------------------------------------
%%% Author  : admin
%%% Description :
%%%
%%% Created : Apr 15, 2011
%%% -------------------------------------------------------------------
-module(assets).

-behaviour(gen_server).
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

-include( "../include/objects.hrl" ).

-import( object_factory, [ make/1, make/2 ]).


%% --------------------------------------------------------------------
%% External exports
-export([ fetch/2, store/3 ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================
fetch( Type, undefined ) ->
		?debugVal( {fetch, Type, undefined}),
		exit( "assets:fetch called with undefined ID" );

fetch( Type, ID ) ->
		%gen_server:call( {global, assets}, {get, Type, ID} ).
		Reply = answer( {get, Type, ID } ),
		Reply.
		


%% ====================================================================
%% Server functions
%% ====================================================================

assets() ->
		Node = 'mdmNode@RichardGreenMacBookPro.local',
		{ assets, Node }.

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
		%?debugMsg( init ),
		process_flag( trap_exit, true ),	
		
		%open_table( object , [ {type, set}, { keypos,1 }, { estimated_no_objects, 100*1000 } ] ),
		NodeList = [node()],
		mnesia:create_schema( NodeList ),
		mnesia:start(),
		Create = mnesia:create_table( object, [ {disc_only_copies, NodeList }
																					, { attributes, record_info( fields, object ) } ]  ),
		?debugVal( Create ),
		ok = mnesia:wait_for_tables( [object], 4000 ),
		
		
		% TODO -- THINK: Should the application (context) open these tables so that clients don't need to?
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
handle_call( Request, From, State) ->
		%?debugVal( {Request, From, State} ),
    Reply = answer( Request ),
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast( Msg, State) ->
   %?debugVal( { Msg, State } ),
	  Result = do( Msg, State ),
		case Result of
				ok ->
						ok;
				Trouble ->
						exit( {"Trouble in asset handling of Msg:", Msg, Trouble })
		end,
		
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
		?debugVal( { Info, State } ),
		NewState = do( Info, State ),
    {noreply, NewState}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
		?debugVal( {terminate, Reason}),
		flush(),
		
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
do( Object = #asset{ asset_ID = ID }       , _State ) 
	 -> store( asset, ID, Object );

do( Object = #circuit{ circuit_ID = ID }   , _State ) 
	  -> store( circuit, ID, Object );

do( Object = #device{ device_ID = ID }     , _State ) 
	  -> store( device, ID, Object );

do( Object = #meter{ meter_ID = ID }       , _State )
	  -> store( meter, ID, Object );

do( Object = #pod{ pod_ID = ID }           , _State )    % 
	  -> store( pod, ID, Object );

do( Object = #service{ service_ID = ID}    , _State ) 
	  -> store( service, ID, Object );

do( Object = #site{ site_ID = ID }         , _State ) 
	  -> store( site, ID, Object );

do( Object = #transformer{ transformer_ID = ID } , _State ) 
	  -> store( transformer, ID, Object );

do( flush, _State ) ->
	  flush();

do( Msg, State ) ->
		?debugVal( {?MODULE, 'does not handle: ', Msg } ),
		error_logger:error_report({?MODULE, 'does not handle: ', Msg }),
		
		State.

%store( Object ) -> do( Object, #state{} ).


store( Type, ID, Object ) ->
		StoreOnMnesia = fun() ->
														mnesia:write( #object{ key = {Type,ID}, object_record = Object } ) 
										end,
    StoreResult = mnesia:transaction( StoreOnMnesia ),
		
		?assertEqual( { atomic, ok }, StoreResult ).

flush() ->
		%mnesia:(dets)sync( object ),
		% TODO: Does Mnesia need something like a flush function? (Assume not)
		ok.

get_ID( undefined ) ->
	  exit( "asset ID is 'undefined' ");

% Get list of objects of this ID (subtypes / supertypes)
get_ID( ID ) ->
		Wild = mnesia:table_info( object, wild_pattern ),
		MatchHead = Wild#object{ key = { '_', ID } },
		SelectID = fun() -> mnesia:match_object( MatchHead ) end,						 

		{ atomic, Output } = mnesia:transaction( SelectID ),
		%?debugVal( { get_ID, ID, MatchHead, Output } ),
		
		Objects = [ X || #object{ key = Key, object_record = X } <- Output],
		Objects.
		

% Get list of objects of this type.
get_type( Type ) ->
		Wild = mnesia:table_info(object,wild_pattern),
		MatchHead = Wild#object{ key = { Type, '_' } },
		SelectType = fun() ->  mnesia:match_object( MatchHead )   end,
		
		{ atomic, Output } = mnesia:transaction( SelectType ),
		%?debugVal( { get_type, Type, MatchHead, Output } ),
		
		Objects = [ X || #object{ key = Key, object_record = X } <- Output],
		Objects.

% Get object of given type and ID
get( Type, undefined ) ->
		exit( "asset ID is 'undefined' ");
get( Type, ID ) ->
		Wild = mnesia:table_info(object,wild_pattern),
		MatchHead = Wild#object{ key = {Type,ID} },
		SelectByTypeID = fun() -> mnesia:match_object( MatchHead ) end,
		
		MnesiaResult = mnesia:transaction( SelectByTypeID ),
		%?debugVal( MnesiaResult ),
		{ atomic, Output } = MnesiaResult,
		%?debugVal( Output ),
		Object = case Output of 
									 [] -> 
											 exit( {"assets could not find:", Type, ID } );
									 SomeOutput ->
											 [Match] = [ X || #object{ key = Key, object_record = X } <- SomeOutput ],
											 %?debugVal( Match ),
											 Match
											 end,
		%?debugVal( Object ),
		Object.

% Remote Procedure (synchronous) Calls come here:

% Get next ID by using the table count -- DO NOT USE IN A CONCURRENT ENVIRONEMNT !!!!!
answer( next_ID ) ->
	 %Serial = mnesia:(dets)info( object, size ) + 1,
		Serial = mnesia:table_info( object, size ) + 1,
		Serial;
% RPC: Get object of given type and ID 
answer( { get, Type, ID } ) -> get( Type, ID );
answer( UnknownRequest) ->
		%exit( {"assets does not know how to handle call: ", UnknownRequest } ).
		do( UnknownRequest, #state{} ),
		ok.

% =========================== TESTS ================================

% Make raw objects (no attributes are populated)

who_am_i_test() ->
		?debugVal( {who_am_i, self() }).

make_id_test()          -> 
		ID = object_factory:make( id, site ),
		?assert( ID > 0 ).

get_asset_test()       -> 
	  Object = object_factory:make( asset, capacitor ),
		%store( Object ),
		
		#asset{ asset_ID = ID } = Object,
		?assert( length( get_type( asset ) ) > 0 ),
		?assertEqual( length([asset]), length( get_ID( ID ) ) ),
		?assertEqual( Object, get( asset, ID ) ).

get_device_test()      -> 
    Object = object_factory:make( device, capacitor ),
		%store( Object ),
		#device{ device_ID = ID } = Object,
		?assert( length( get_type( device ) ) > 0 ),
		?assertEqual( length([device,asset]), length( get_ID( ID ) ) ), % a device >> asset
		?assertEqual( Object, get( device, ID ) ).

get_pod_test()         -> 
		Object = object_factory:make( pod, capacitor ),
		%store( Object ),
		#pod{ pod_ID = ID } = Object,
		?assert( length( get_type( pod ) ) > 0 ),
		?assertEqual( length([pod,device,asset]), length( get_ID( ID ) )  ), % a pod >> device >> asset
		?assertEqual( Object, get( pod, ID ) ).

get_meter_test()       -> 
		Object =  object_factory:make( meter ),
		%store( Object ),
		#meter{ meter_ID = ID } = Object,
		?assert( length( get_type( meter ) ) > 0 ),
		?assertEqual( length([meter,device,asset]), length( get_ID( ID ) ) ), % meter >> device >> asset
		?assertEqual( Object, get( meter, ID ) ).

get_service_test()     -> 
		Object = object_factory:make( service ),
		%store( Object ),
		#service{ service_ID = ID } = Object,
		?assert( length( get_type( service ) ) > 0 ),
		?assertEqual( length([service,pod,device,asset]), length( get_ID( ID )  )  ), % service >> pod >> device >> asset
		?assertEqual( Object, get( service, ID ) ).

get_transformer_test() -> 
    Object = object_factory:make( transformer ),
		%store( Object ),
		#transformer{ transformer_ID = ID } = Object,
		?assert( length( get_type( transformer ) ) > 0 ),
		?assertEqual( length([transformer,pod,device,asset]), length( get_ID( ID )  ) ), % transformer >> pod >> device >> asset
		?assertEqual( Object, get( transformer, ID ) ).

get_circuit_test()     -> 
    Object = object_factory:make( circuit ),
		%store( Object ),
		#circuit{ circuit_ID = ID } = Object,
		?assert( length( get_type( circuit ) ) > 0 ),
		?assertEqual( length([circuit,pod,device,asset]), length( get_ID( ID )  ) ), % circuit >> pod >> device >> asset
		?assertEqual( Object, get( circuit, ID ) ).


% Make PoD objects with upstream connections.

meter_on_service_test() ->
		Meter = object_factory:meter_on_service( object_factory:make(service) ),
		%?debugVal( Meter ),
		#meter{ meter_ID = {meter,MeterNumber}
					, service_ID = {service,ServiceNumber}
				  } = Meter,
		#meter{ meter_ID = ID } = Meter,
		?assert( length( get_type( meter ) ) > 0 ),
		?assertEqual( length([meter,device,asset]), length( get_ID( ID )  )  ),
		?assertEqual( Meter, get( meter, ID ) ),
		?assert( MeterNumber =/= ServiceNumber ),
		Meter.

service_on_transformer_test() ->
		Service = object_factory:service_on_transformer( object_factory:make( transformer) ),
		%?debugVal( Service ),		
		#service{ service_ID = {service,ServiceNumber} 
						, transformer_ID = {transformer,TransformerNumber} 
						} = Service,
		#service{ service_ID = ID } = Service,
		?assert( length( get_type( service ) ) > 0 ),
		?assertEqual( length([service,pod,device,asset]), length( get_ID( ID )  )  ),
		?assertEqual( Service, get( service, ID ) ),
		?assert( ServiceNumber =/= TransformerNumber ),
		Service.

transformer_on_circuit_test() ->
		Transformer = object_factory:transformer_on_circuit( object_factory:make( circuit ) ),
		%?debugVal( Transformer ),
		#transformer{ transformer_ID = {transformer,TransformerNumber}
								, circuit_ID = {circuit,CircuitNumber}           
								} = Transformer,
		#transformer{ transformer_ID = ID } = Transformer,
		?assert( length( get_type( transformer ) ) > 0 ),
		?assertEqual( length([transformer,pod,device,asset]), length( get_ID( ID ) )  ),
		?assertEqual( Transformer, get( transformer, ID ) ),
		?assert( TransformerNumber =/= CircuitNumber ),
		Transformer.

dump_table_test_suppressed() ->
		Wild = 		Wild = mnesia:table_info(object,wild_pattern),
		Dump = fun() -> mnesia:match_object( Wild )
									 end,
		{ atomic, Output } = mnesia:transaction(Dump),
		%?debugVal( { self(), Output } ),

		io:format("~n~p~n", [Output] ).
