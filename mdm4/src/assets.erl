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
fetch( Type, ID ) ->
		gen_server:call( {global, assets}, {get, Type, ID} ).


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
		
		open_table( assets_table
							, [ {type, set}, { keypos,1 }, { estimated_no_objects, 100*1000 } ] ),
		
		% TODO -- THINK: Should the application (context) open these tables so that clients don't need to?
    {ok, #state{}}.

open_table( TableTerm, Options ) ->
		case dets:info(TableTerm) of
				undefined -> 
						TableReadingResult = dets:open_file( TableTerm, Options ),
						case TableReadingResult of
								{ok, TableTerm } ->
										good;
								{error, Reason } -> 
										case Reason of
												{read_error,{file_error,FileName,enoent}} ->         % No such file
														 TableOpeningResult = dets:open_file( TableTerm, Options );   % Create a new table
												_ ->
														?debugVal( {"problem opening table", TableTerm, Reason }),
														
														error_logger:error_report({"problem opening table", TableTerm, Reason})
										end
						end;
				
				Report -> 
						?debugVal( "Some other process owns the asset table!" ),
						"Assets table already open"
		end,				
		?debugVal( dets:info( TableTerm )  ),
		done.


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
do( Object = #asset{ asset_ID = ID }       , State ) -> 
		   store( asset, ID, Object );

do( Object = #circuit{ circuit_ID = ID }   , State ) 
	  -> store( circuit, ID, Object );

do( Object = #device{ device_ID = ID }     , State ) 
	  -> store( device, ID, Object );

do( Object = #meter{ meter_ID = ID }       , State )
	  -> store( meter, ID, Object );

do( Object = #pod{ pod_ID = ID }           , State )    % 
	  -> store( pod, ID, Object );

do( Object = #service{ service_ID = ID}    , State ) 
	  -> store( service, ID, Object );

do( Object = #site{ site_ID = ID }         , State ) 
	  -> store( site, ID, Object );

do( Object = #transformer{ transformer_ID = ID } , State ) 
	  -> store( transformer, ID, Object );

do( flush, State ) ->
	  flush();

do( Msg, State ) ->
		?debugVal( {?MODULE, 'does not handle: ', Msg } ),
		error_logger:error_report({?MODULE, 'does not handle: ', Msg }),
		
		State.


store( Type, ID, Object ) ->
		dets:insert( assets_table, { {Type,ID}, Object } ).

flush() ->
		dets:sync( assets_table ),
		ok.

get_ID( undefined ) ->
	  exit( "asset ID is 'undefined' ");

% Get list of objects of this ID (subtypes / supertypes)
get_ID( ID ) ->
		[ X || [X] <- dets:match( assets_table, { {'_',   ID } ,'$1'} )  ].

% Get list of objects of this type.
get_type( Type ) ->
		[ X || [X] <- dets:match( assets_table, { { Type, '_'} ,'$1'} )  ].

% Get object of given type and ID
get( Type, undefined ) ->
		exit( "asset ID is 'undefined' ");
get( Type, ID ) ->
		[Match] = [ X || [X] <- dets:match( assets_table, { { Type, ID},'$1'} )  ],
		Match.



% Remote Procedure (synchronous) Calls come here:

answer( next_ID ) ->
	  Serial = dets:info( assets_table, size ) + 1,
		Serial;
% RPC: Get object of given type and ID 
answer( { get, Type, ID } ) -> get( Type, ID ).




% =========================== TESTS ================================

% Make raw objects (no attributes are populated)

make_id_test()          -> 
		ID = object_factory:make( id, site ),
		?assert( ID > 0 ).


make_asset_test()       -> 
	  Object = make( asset, capacitor ),
		#asset{ asset_ID = ID } = Object,
		?assert( length( get_type( asset ) ) > 0 ),
		?assertEqual( Object, get( asset, ID ) ).

make_device_test()      -> 
    Object = make( device, capacitor ),
		#device{ device_ID = ID } = Object,
		?assert( length( get_type( device ) ) > 0 ),
		?assertEqual( Object, get( device, ID ) ).

make_pod_test()         -> 
		Object = make( pod, capacitor ),
		#pod{ pod_ID = ID } = Object,
		?assert( length( get_type( pod ) ) > 0 ),
		?assertEqual( Object, get( pod, ID ) ).

make_meter_test()       -> 
		Object =  make( meter ),
		#meter{ meter_ID = ID } = Object,
		?assert( length( get_type( meter ) ) > 0 ),
		?assertEqual( Object, get( meter, ID ) ).

make_service_test()     -> 
		Object = make( service ),
		#service{ service_ID = ID } = Object,
		?assert( length( get_type( service ) ) > 0 ),
		?assertEqual( Object, get( service, ID ) ).

make_transformer_test() -> 
    Object = make( transformer ),
		#transformer{ transformer_ID = ID } = Object,
		?assert( length( get_type( transformer ) ) > 0 ),
		?assertEqual( Object, get( transformer, ID ) ).

make_circuit_test()     -> 
    Object = make( circuit ),
		#circuit{ circuit_ID = ID } = Object,
		?assert( length( get_type( circuit ) ) > 0 ),
		?assertEqual( Object, get( circuit, ID ) ).


% Make PoD objects with upstream connections.

meter_on_service_test() ->
		Meter = object_factory:meter_on_service( make(service) ),
		%?debugVal( Meter ),
		{meter,{meter,MeterNumber},{service,ServiceNumber},undefined} = Meter,
		?assert( MeterNumber =/= ServiceNumber ).

service_on_transformer_test() ->
		Service = object_factory:service_on_transformer( make( transformer) ),
		%?debugVal( Service ),
		{service, {service,ServiceNumber} , {transformer,TransformerNumber}, undefined, undefined} = Service,
		?assert( ServiceNumber =/= TransformerNumber ).

transformer_on_circuit_test() ->
		Transformer = object_factory:transformer_on_circuit( make( circuit ) ),
		%?debugVal( Transformer ),
		{transformer,{transformer,TransformerNumber},{circuit,CircuitNumber}} = Transformer,
		?assert( TransformerNumber =/= CircuitNumber ).




