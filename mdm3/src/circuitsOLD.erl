% Copyright 2011 Richard Alexander Green
% Description : Aggregate usage at the circuit level.
%
% Created : Apr 19, 2011
% -------------------------------------------------------------------
-module( circuitsOLD ).

-behaviour( gen_server ).
% --------------------------------------------------------------------
% Include files
% --------------------------------------------------------------------


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

% --------------------------------------------------------------------
% External exports
-export([]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

% ====================================================================
% External functions
% ====================================================================


% ====================================================================
% Server functions
% ====================================================================

% --------------------------------------------------------------------
% Function: init/1
% Description: Initiates the server
% Returns: {ok, State}          |
%          {ok, State, Timeout} |
%          ignore               |
%          {stop, Reason}
% --------------------------------------------------------------------
init([]) ->
?debugMsg( init ),
		process_flag( trap_exit, true ),	
		
		open_table( circuit_usage, "circuit_usage_table", [named_table, bag, {keypos,2} ] ),
		
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
														
														error_logger:error_report({"problem opening table",  TableTerm, FileName, Reason})
										end
						end;
				
				Report -> 
						?debugVal( {"Some other process owns the table!", TableTerm}  ),
						"Assets table already open"
		end,				
		?debugVal( ets:info( TableTerm )  ),
		done.



% --------------------------------------------------------------------
% Function: handle_call/3
% Description: Handling call messages
% Returns: {reply, Reply, State}          |
%          {reply, Reply, State, Timeout} |
%          {noreply, State}               |
%          {noreply, State, Timeout}      |
%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%          {stop, Reason, State}            (terminate/2 is called)
% --------------------------------------------------------------------
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

% --------------------------------------------------------------------
% Function: handle_cast/2
% Description: Handling cast messages
% Returns: {noreply, State}          |
%          {noreply, State, Timeout} |
%          {stop, Reason, State}            (terminate/2 is called)
% --------------------------------------------------------------------
handle_cast(Msg, State) ->
		%?debugVal( { Msg, State} ),
		NewState = do( Msg, State ),
    {noreply, NewState}.

% --------------------------------------------------------------------
% Function: handle_info/2
% Description: Handling all non call/cast messages
% Returns: {noreply, State}          |
%          {noreply, State, Timeout} |
%          {stop, Reason, State}            (terminate/2 is called)
% --------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

% --------------------------------------------------------------------
% Function: terminate/2
% Description: Shutdown the server
% Returns: any (ignored by gen_server)
% --------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

% --------------------------------------------------------------------
% Func: code_change/3
% Purpose: Convert process state when code is changed
% Returns: {ok, NewState}
% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

% --------------------------------------------------------------------
% Internal functions
% --------------------------------------------------------------------
do( Usage = #pod_usage{}, State ) ->
		usage( Usage ),
		State.

usage( CircuitUsage = #pod_usage{} ) ->
		?debugVal( CircuitUsage ),
		% Add the usage into the PoD's history
		%Result = ets:insert( circuit_usage, CircuitUsage ),
		PoD_Record = assets:get( pod, CircuitUsage#pod_usage.pod_ID ),
		PoD_LoadHistory = PoD_Record#pod.load_history,
%		time_series:add(SeriesA, SeriesB),

		ok.

send_usage_to_others( Usage, CircuitID ) ->
		?debugVal( {send_usage_to_others, Usage, CircuitID } ),
		gen_server:cast( { global, circuits }, #pod_usage{ pod_ID = CircuitID, usage = Usage } ),
		ok.

