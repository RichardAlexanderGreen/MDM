%% Author: Richard Alexander Green
%% Created: Nov 12, 2010
%% Description: Maintain a mnesia database table of asset attributes.
%% This version (V02) implements the gen_server pattern.

-module(asset_db).
-behaviour(gen_server).

%%
%% Include files
%%
%-import(lists, [foreach/2]).
%-include_lib("stdlib/include/qlc.hrl").

-include_lib("eunit/include/eunit.hrl").

%%
%% Exported Functions
%%
%-export([ start/0, remember_asset_attribute/3, recall_asset_attribute/2, list_asset_attributes/1 ]).

% Export gen_server interface.
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, start_link/0, terminate/2, code_change/3 ]).

-define(debug,true).
%-undef(debug).

-ifdef(debug).
-compile(export_all).   % For debug only.
-endif.

-record( asset, { asset_ID, attribute_dictionary  } ).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
		% If the database has not been created, create it.
		%Tables = mnesia:system_info(tables),
		
		% Following will produce exception if there are additional tables.
		% When that happens, we need a more sophisticated test.
		%[asset,schema] = Tables,

		% Start this server and the database. Terminates if there is a problem.
		ok = start(),
		
		% What is my state? What should be passed to handle_call/3 ?
		% For now, I guess this will do.
    { ok, running }.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call( Request, _From, State) ->
		case Request of
				{ remember_asset_attribute, Asset_ID, Attribute, Value} ->
						ok = remember_asset_attribute( Asset_ID, Attribute, Value ),
						Reply = ok;
				
				{ remember_multiple_attributes, Asset_ID, OrdDict } ->
						ok = remember_multiple_attributes( Asset_ID, OrdDict ),
						Reply = ok;
						
				{ recall_asset_attribute, Asset_ID, Attribute } ->
						Reply = recall_asset_attribute( Asset_ID, Attribute );
				
				{ list_asset_attributes, Asset_ID } ->
						List = list_asset_attributes( Asset_ID ),
				    Reply = List;

				_ ->
						Reply = [ request_not_recognized, Request ]					
				end,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% API Functions
%%

% Create the database.
% Only allow this to be run in debug mode.
-ifdef(debug).

create_database() ->
		_ = mnesia:create_schema([node()]),
		mnesia:start(),
		TableConfigs = [ { disc_copies, [node()]  }
									 , { type, set } 
									 ],
		_ = mnesia:delete_table( asset ),
		{atomic, ok} = mnesia:create_table( asset, [ {attributes, record_info( fields, asset )} ]  
																			%++ TableConfigs 
																			),
		%mnesia:stop(),
		mnesia:table_info( asset, all ).
%,ok.

-endif.


% Start this service.
start() ->
		% Start the database.  
		mnesia:start(),
		% (Time-out in 20 seconds.)
		mnesia:wait_for_tables([ asset ], 20000).


start_link() ->
	gen_server:start_link( { global, ?MODULE }, ?MODULE, [], [] ).


% Get a asset's attribute set.
% The attribute set is a list of tuples {attribute, value}.
get_asset_dictionary( Asset_ID ) ->
		GetDictionary = fun() ->
													 mnesia:read( asset, Asset_ID )
													 end,
		Result = mnesia:transaction(GetDictionary),
		case Result of
				{atomic,[]} -> [];
				{atomic, [{asset, Asset_ID, Dictionary}] } ->  Dictionary
		end.		


% Remember a asset attribute.
remember_asset_attribute( Asset_ID, Attribute, Value ) ->
		Dictionary = get_asset_dictionary( Asset_ID ),
		Dictionary2 = orddict:store( Attribute, Value, Dictionary ),
		PutAttribute = fun() -> 
												Asset_Record = { asset, Asset_ID, Dictionary2 },
												mnesia:write( Asset_Record )
												end,
		{atomic, ok} = mnesia:transaction(PutAttribute),
		ok.

% Merge a set of attributes into existing set.
remember_multiple_attributes( Asset_ID, OrdDict ) ->
		Dictionary = get_asset_dictionary( Asset_ID ),
		MergeFunction = fun( _, _, ValueNew ) -> ValueNew end,
		Dictionary2 = orddict:merge( MergeFunction, Dictionary, OrdDict ),
		PutAttribute = fun() -> 
												Asset_Record = { asset, Asset_ID, Dictionary2 },
												mnesia:write( Asset_Record )
												end,
		{atomic, ok} = mnesia:transaction(PutAttribute),
		ok.

% Recall a asset attribute.
recall_asset_attribute( Asset_ID, Attribute ) ->
		Dictionary = get_asset_dictionary( Asset_ID ),
		{ok, Value} = orddict:find( Attribute, Dictionary ),
		Value.
	
% List the attributes of a asset.
list_asset_attributes( Asset_ID ) ->
		Dictionary = get_asset_dictionary( Asset_ID ),
		Dictionary.
%%
%% Local Functions
%%


%%
%% TESTS - DEBUG
%%
-ifdef(debug).

%% Note: test_get_asset_dictionary/0 IS HISTORY DEPENDENT - IT RETURNS CURRENT STATE OF DICTIONARY.
test_get_asset_dictionary() ->
	  Dictionary = get_asset_dictionary("Itron:123.456.789.123456"),
		Dictionary.


%% Test: Remember asset attribute.
test_remember_asset_attribute() ->
		?assertEqual( ok, remember_asset_attribute( "Itron:123.456.789.123456", asset_configuration, 'Residential Standard') ).
		%remember_asset_attribute( "Itron:123.456.789.123456", asset_configuration, 'Residential Standard').

%% Test: Recall a asset attribute that was recorded previously.
test_recall_asset_attribute() ->
		remember_asset_attribute( "Itron:123.456.789.123456", asset_configuration, 'Residential Standard'),
		?assertEqual( 'Residential Standard', recall_asset_attribute("Itron:123.456.789.123456", asset_configuration )).
	  %recall_asset_attribute("Itron:123.456.789.123456", asset_configuration ).

%% Test: List asset attributes.
test_list_asset_attributes() ->
		remember_asset_attribute( "Itron:123.456.789.123456", asset_configuration, 'Residential Standard'),
		remember_asset_attribute( "Itron:123.456.789.123456", service, "48103-4215:1234"),	
		?assertEqual( [{asset_configuration,'Residential Standard'}, {service,"48103-4215:1234"}], 
									list_asset_attributes("Itron:123.456.789.123456") ).
	 % list_asset_attributes("Itron:123.456.789.123456").

%% Note: test_local/0 is DESTRUCTIVE -- IT WILL CLEAR THE DATABASE.
test_local() ->
		create_database(),
		start(),
		[] = test_get_asset_dictionary(),
		ok = test_remember_asset_attribute(),
		ok = test_recall_asset_attribute(),
		ok = test_list_asset_attributes(),
		okay_all_tests_passed.

test_remember_multiple_attributes() ->
		Location = [43.0, 83.0],	
		Dictionary = orddict:from_list( [ {circuit, "Circuit_ID"}, {location, Location} ] ),
		PID_asset_db = whereis( asset_db ),
		case PID_asset_db of
				undefined ->
						remember_multiple_attributes( "Service_key", Dictionary );
				
				_ ->
					PID_asset_db ! { remember_multiple_attributes, "Service_ID", Dictionary }
			end.



test_server() ->
		% Test init/1.
		{ ok, running } = init([]),
		% Set up test values.
		Asset_ID = "TestAsset:1234567",
		Attribute1 = attribute_1,
		Value1 = "test value 1",
		% Put data into database.
		{ reply, ok, running } = handle_call( {remember_asset_attribute, Asset_ID, Attribute1, Value1 }, test, running ),
		% Now see if we can get data back.
		{ reply, Value1, running } = handle_call( {recall_asset_attribute, Asset_ID, Attribute1 }, test, running ),
		% Add another attribute to the same asset.
		Attribute2 = attribute_2,
		Value2 = "test value 2",
		{ reply, ok, running } = handle_call( {remember_asset_attribute, Asset_ID, Attribute2, Value2 }, test, running ),
		% check.
		{ reply, Value2, running } = handle_call( {recall_asset_attribute, Asset_ID, Attribute2 }, test, running ),
		% Now see if we can get expected list of attributes.
		{ reply, List, running } = handle_call( {list_asset_attributes, Asset_ID }, test, running ),
	  [{Attribute1, Value1},{Attribute2, Value2}] = List,
		% Test storing multiple attributes.
		ok = test_remember_multiple_attributes(),
		
		okay_server_tests_passed.
-endif.

