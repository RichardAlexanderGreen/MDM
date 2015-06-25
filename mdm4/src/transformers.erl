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
init([]) ->
		%?debugMsg( init ),
		process_flag( trap_exit, true ),	
		
		open_table( transformer_series_table
							, [ { keypos,2 }, { estimated_no_objects, 100*1000 } ] ),
		
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
		dets:close( transformer_series_table ),
		
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
do( PoD_Series = #pod_series{}, State ) ->
		add_series_into_PoD_history( PoD_Series ),
		State;

do( flush, State ) ->
		flush(),
		State.

add_series_into_PoD_history( PoD_Series ) ->
		%?debugVal( PoD_Series ),
		TransformerID  = PoD_Series#pod_series.pod_ID,
		PoD_TimeSeries = PoD_Series#pod_series.series,
		
		Transformer_PoD_Series = 
				        case dets:lookup( transformer_series_table, TransformerID ) of 
										[] -> % If there is no prior pod_series record, build one that matches the incoming type
												#pod_series{ pod_ID = TransformerID
																	 , series = #series{ series_type      = PoD_TimeSeries#series.series_type
																	                   , unit_of_measure  = PoD_TimeSeries#series.unit_of_measure
																										 , interval_seconds = PoD_TimeSeries#series.interval_seconds
																										 } 
																	 }; 
										[TransformerSeries] -> 
												TransformerSeries 
								end,
		%?debugVal( Transformer_PoD_Series ),
		
		Transformer_TimeSeries = Transformer_PoD_Series#pod_series.series,
		Updated_TimeSeries = time_series:add( PoD_TimeSeries, Transformer_TimeSeries ),  
		%Updated_TimeSeries = time_series:put_into( PoD_TimeSeries, Transformer_TimeSeries ), % Services simply put - other PoD's add
		
		%?debugVal( Updated_TimeSeries ),
		
		Updated_PoD_Series = Transformer_PoD_Series#pod_series{ series = Updated_TimeSeries },
		%?debugVal( { TransformerID, Updated_PoD_Series#pod_series.pod_ID, length( Updated_PoD_Series#pod_series.series#series.curve)} ),
	
		dets:insert( transformer_series_table, Updated_PoD_Series ),

		send_series_to_circuit( PoD_Series ),
		ok.

send_series_to_circuit( PoD_Series ) ->
		TransformerID      = PoD_Series#pod_series.pod_ID,
		PoD_TimeSeries = PoD_Series#pod_series.series,
		
		TransformerRecord = assets:get( transformer, TransformerID ),
		%?debugVal( TransformerRecord ),
		
		CircuitID = TransformerRecord#transformer.circuit_ID,
		Circuit_PoD_Series = #pod_series{ pod_ID = CircuitID, series = PoD_TimeSeries },
		%?debugVal( { transformer, TransformerID, is_sending_series_to_circuit, CircuitID }),
		gen_server:cast({global, circuits}, Circuit_PoD_Series ).

flush() ->
		dets:sync( transformer_series_table ),
		% Tell circuits to flush too.
		gen_server:cast( {global, circuits}, flush ),
		ok.

