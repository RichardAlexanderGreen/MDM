%%% -------------------------------------------------------------------
%%% Author  : admin
%%% Description :
%%%
%%% Created : Apr 19, 2011
%%% -------------------------------------------------------------------
-module(servicesOLD).

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
		?debugMsg( init ),
		process_flag( trap_exit, true ),	
		
		open_table( service_usage, "service_usage_table", [named_table, bag, {keypos,2} ] ),
		
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
		add_series_to_PoD_history( PoD_Series ),
		State;

do( Usage = #pod_usage{}, State ) ->
		usage( Usage ),
		State.

usage( ServiceUsage = #pod_usage{} ) ->
		%?debugVal( ServiceUsage ),
		% Record the usage in the service's history
		_Result = ets:insert( service_usage, ServiceUsage ),
		%{service, ServiceID} = ServiceUsage#pod_usage.pod_ID,
	  ServiceID = ServiceUsage#pod_usage.pod_ID,
		[ServiceRec] = assets:get( service, ServiceID ),
		%?debugVal( ServiceRec ),
		TransformerID = ServiceRec#service.transformer_ID,
		%?debugVal( TransformerID ),
		Usage = ServiceUsage#pod_usage.usage,
		send_usage_to_transformer( Usage, TransformerID ).

send_usage_to_transformer( Usage, TransformerID ) ->
		%?debugVal( {send_usage_to_transformer, Usage, TransformerID } ),
		
		gen_server:cast( { global, transformers }, #pod_usage{ pod_ID = TransformerID, usage = Usage } ),
		ok.

add_series_to_PoD_history( PoD_Series ) ->
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
		%Updated_TimeSeries = time_series:add( PoD_TimeSeries, Service_TimeSeries ),  % TODO - WRONG - NEED TO MERGE NOT ADD
		Updated_TimeSeries = time_series:put_into( PoD_TimeSeries, Service_TimeSeries ), 
		%?debugVal( Updated_TimeSeries ),
		
		Updated_PoD_Series = Service_PoD_Series#pod_series{ series = Updated_TimeSeries },
		ets:insert( service_series, Updated_PoD_Series ),
		ok.





