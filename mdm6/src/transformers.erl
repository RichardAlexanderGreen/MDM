%%% Copyright Richard Alexander Green 2011
%%% Description : Server updates the load history at transformer level
%%%   in the Point-of-Delivery hierarchy.
%%% Created : Apr 19, 2011
%%% -------------------------------------------------------------------
-module(transformers).

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


%% --------------------------------------------------------------------
%% External exports
-export([]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export( [do/1] ).

-record( state, {} ).

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
-define( TableRecordName, transformer_series ).
 
create_mnesia_table( ?TableRecordName ) ->
		mnesia:start(),   % Make sure mnesia is started
		NodeList = [node()],
		Create = mnesia:create_table( ?TableRecordName
																, [ {disc_only_copies, NodeList }
																	, { attributes, record_info( fields, ?TableRecordName ) } 
																	]  
																),
		?debugVal( Create ),
		ok = mnesia:wait_for_tables( [?TableRecordName], 4000 ) .

init([]) ->
		%?debugMsg( init ),
		process_flag( trap_exit, true ),	
		
		create_mnesia_table( ?TableRecordName ),
		
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
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
		%?debugVal( { Msg, State} ),
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
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
		?debugVal( {terminate, Reason, State} ),
		flush(),
		%dets:close( transformer_series_table ),
		
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
do( Message ) ->
		do( Message, #state{} ).

do( PoD_Series = #pod_series{}, State ) ->
		add_series_into_PoD_history( PoD_Series ), % Insert this day`s demand into service history.
		send_series_to_circuit( PoD_Series ),
		State;

do( flush, State ) ->
		flush(),
		State.

add_series_into_PoD_history( PoD_Series ) ->
		% Get the matching history record or make one if there is none.
		MatchingServiceSeriesRecord = get_matching_history_record( PoD_Series ),
		
		% Insert the incoming series into the series on record
		#?TableRecordName{ series = SeriesOnRecord } = MatchingServiceSeriesRecord,			
		#pod_series{ series = IncomingSeries } = PoD_Series,		
		Updated_TimeSeries = time_series:add( IncomingSeries, SeriesOnRecord ), 
		
		% Update the history record
		UpdatedRecord = MatchingServiceSeriesRecord#?TableRecordName{ series = Updated_TimeSeries },
		PutFunction = fun() ->  mnesia:write( UpdatedRecord ) end,
		mnesia:transaction( PutFunction ),
		ok.

get_matching_history_record( PoD_Series ) ->
		#pod_series{ pod_ID=ServiceID, series = IncomingSeries  } = PoD_Series,
		Wild = mnesia:table_info( ?TableRecordName, wild_pattern ),
		MatchHead = Wild#?TableRecordName{ pod_ID = ServiceID },
		LookupFunction = fun() -> mnesia:match_object( MatchHead ) end, 
		{atomic, MatchingServiceSeriesRecords } = mnesia:transaction( LookupFunction ),
		%?debugVal( MatchingServiceSeriesRecords ),
    ReturnRecord = case MatchingServiceSeriesRecords of
											 [] ->
													 Record = #?TableRecordName{ pod_ID = ServiceID
																		                 , series = IncomingSeries#series{ curve = [] } 
																		                 },
													 Record;
											 [Something] ->
													 Something
									 end,
		%?debugVal( ReturnRecord ),
		ReturnRecord.

send_series_to_circuit( PoD_Series ) ->
		#pod_series{ pod_ID = TransformerID, series = IncomingSeries } = PoD_Series,
		
		TransformerRecord = assets:get( transformer, TransformerID ),
		%?debugVal( TransformerRecord ),
		
		CircuitID = TransformerRecord#transformer.circuit_ID,
		Circuit_PoD_Series = #pod_series{ pod_ID = CircuitID, series = IncomingSeries },
		%?debugVal( { transformer, TransformerID, is_sending_series_to_circuit, CircuitID }),
		%gen_server:cast( {global, circuits}, Circuit_PoD_Series ).
		circuits:do( Circuit_PoD_Series ).

flush() ->
		%mnesia:(dets)sync( transformer_series_table ),
		% Tell circuits to flush too.
		gen_server:cast( {global, circuits}, flush ),
		ok.

% ========================== TESTS ============================================

add_series_into_PoD_history_test() ->
		#transformer{ transformer_ID = Transformer_ID } = assets:transformer_on_circuit_test(),
		PoD_Series = #pod_series{ pod_ID = Transformer_ID, series = time_series:make_test_day() },
		add_series_into_PoD_history( PoD_Series ).

send_series_to_circuit_test() ->
		#transformer{ transformer_ID = Transformer_ID } = assets:transformer_on_circuit_test(),
		PoD_Series = #pod_series{ pod_ID = Transformer_ID, series = time_series:make_test_day() },
		send_series_to_circuit( PoD_Series ) .

