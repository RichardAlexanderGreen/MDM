%%% Copyright Richard Alexander Green 2011
%%% Description : Server updates the usage history at service level
%%%   in the Point-of-Delivery hierarchy.
%%% Created : Apr 19, 2011
%%% -------------------------------------------------------------------
-module(services).

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
		
		open_table( service_series, "service_series_table", [named_table, set, {keypos,2} ] ),
		
		% TODO -- THINK: Should the application (context) open these tables so that clients don't need to?
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
														
														error_logger:error_report({"problem opening table", TableTerm, FileName, Reason})
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
		ServiceID      = PoD_Series#pod_series.pod_ID,
		PoD_TimeSeries = PoD_Series#pod_series.series,
		
		Service_PoD_Series = 
				        case ets:lookup( service_series, ServiceID ) of 
										[] -> % If there is no prior pod_series record, build one that matches the incoming type
												#pod_series{ pod_ID = ServiceID
																	 , series = #series{ series_type      = PoD_TimeSeries#series.series_type
																	                   , unit_of_measure  = PoD_TimeSeries#series.unit_of_measure
																										 , interval_seconds = PoD_TimeSeries#series.interval_seconds
																										 } 
																	 }; 
										[ServiceSeries] -> 
												ServiceSeries 
								end,
		%?debugVal( Service_PoD_Series ),
		
		Service_TimeSeries = Service_PoD_Series#pod_series.series,
		%Updated_TimeSeries = time_series:add( PoD_TimeSeries, Service_TimeSeries ),  % Services simply put - other PoD's add
		Updated_TimeSeries = time_series:put_into( PoD_TimeSeries, Service_TimeSeries ), 
		%?debugVal( Updated_TimeSeries ),
		
		Updated_PoD_Series = Service_PoD_Series#pod_series{ series = Updated_TimeSeries },
		ets:insert( service_series, Updated_PoD_Series ),
		
		send_series_to_transformer( PoD_Series ),
		ok.

send_series_to_transformer( PoD_Series ) ->
		ServiceID      = PoD_Series#pod_series.pod_ID,
		PoD_TimeSeries = PoD_Series#pod_series.series,
		
		[ServiceRecord] = assets:get( service, ServiceID ),
		%?debugVal( ServiceRecord ),
		
		TransformerID = ServiceRecord#service.transformer_ID,
		Transformer_PoD_Series = #pod_series{ pod_ID = TransformerID, series = PoD_TimeSeries },
		%?debugVal( { service, ServiceID, is_sending_series_to_transformer, TransformerID }),
		gen_server:cast({global, transformers}, Transformer_PoD_Series ).

flush() ->
		ets:tab2file( service_series, "service_series_table", [ {extended_info, [object_count] } ]  ),
		% Tell transformers to flush too.
		gen_server:cast({global, transformers}, flush ),
		ok.

		
		




