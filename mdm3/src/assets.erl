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

-define( NOTEST, true ).
-define( NODEBUG, true ).


% Set log level
% ALL=0 < TRACE=1 < DEBUG=2 < INFO=3 < WARN=4 < ERROR=5 < FATAL=6 < OFF=7
% Level semantics are defined in the org.apache.log4j.Level class.
-define( LOGLEVEL, 3 ).             % 3 means log info level and above. -- See log/2.

-include_lib("eunit/include/eunit.hrl").

-ifdef( DEBUG ).
-compile( export_all ).
-endif.

-include( "../include/objects.hrl" ).

%% --------------------------------------------------------------------
%% External exports
-export([get_ID/1, get_type/1, get/2, store/3 ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================


%% ====================================================================
%% Server functions
%% ====================================================================

assets() ->
		Node = 'mdm3Node@RichardGreenMacBookPro.local',
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
		?debugMsg( init ),
		process_flag( trap_exit, true ),	
		
		open_table( assets, "assets_table", [ named_table, set, {keypos,1}, protected ] ),

		% TODO -- THINK: Should the application (context) open this table so that clients don't need to?

    {ok, #state{}}.


open_table( TableTerm, FileName, Options ) ->
		case ets:info(TableTerm) of
				undefined -> 
						TableReadingResult = ets:file2tab( FileName ),
						case TableReadingResult of
								{ok, TableTerm } ->
										good;
								{error, Reason } -> 
										case Reason of
												{read_error,{file_error,FileName,enoent}} ->         % No such file
														 TableOpeningResult = ets:new( TableTerm, Options );   % Create a new table
												_ ->
														?debugVal( {"problem opening table", TableTerm, FileName, Reason }),
														
														error_logger:error_report({"problem opening table", TableTerm, FileName, Reason })
										end
						end;
				
				Report -> 
						?debugVal( "Some other process owns the asset table!" ),
						"Assets table already open"
		end,				
		?debugVal( ets:info( TableTerm )  ),
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
		?debugVal( {Request, From, State} ),
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
  %  ?debugVal( { Msg, State } ),
	  NewState = do( Msg, State ),
    {noreply, NewState}.

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
	  flush(),
	
		State;


do( Msg, State ) ->
		?debugVal( {?MODULE, 'does not handle: ', Msg } ),
		error_logger:error_report({?MODULE, 'does not handle: ', Msg }),
		
		State.

store( Object ) ->
		%?debugVal( Object ),
		
		ets:insert( assets, Object ),
		
		dummy.

store( Type, ID, Object ) ->
		ets:insert( assets, { {Type,ID}, Object } ).

flush() ->
		ets:tab2file(assets, "assets_table", [ {extended_info, [object_count] } ]  ),
		ok.


get_ID( ID ) ->
		[ X || [X] <- ets:match( assets, { {'_',{'_', ID } },'$1'} )  ].

get_type( Type ) ->
		[ X || [X] <- ets:match( assets, {{ Type,{ Type,'_'}},'$1'} )  ].

get( Type, ID ) ->
		[ X || [X] <- ets:match( assets, {{ Type,ID},'$1'} )  ].

% Remote Procedure (synchronous) Calls come here:

answer( next_ID ) ->
	  Serial = ets:info( assets, size ) + 1,
		Serial.






