%% Copyright 2011 Richard Alexander Green
%% Description: TODO: Add description to meter
%% Created: Jul 4, 2011
%% ------------------------------------------
-module(metersV01).

%%
%% Include files
%%

-define( NOT_IMPLEMENTED, exit( {?MODULE,'has not implemented this function'} )  ).

-define( TEST, true ).
-define( DEBUG, true ).

-include_lib("eunit/include/eunit.hrl").

-ifdef( DEBUG ).
-compile( export_all ).
-endif.


%%
%% Exported Functions
%%
-export([]).

-record( meter, { id, register = 0 } ).

%%
%% API Functions
%%
initialize_table() ->
		case ets:info( meters ) of
				undefined ->
						ets:new( meters, [named_table, public]);
				_ ->
						meters
		end,
    initialize_channels().

-ifdef( BIG_MEMORY ).

clear_all_meters() ->
    ?debugVal("Clearing meters and channels tables."),
		ets:delete_all_objects( meters ),
		ets:delete_all_objects( channels ),
		ok.

-else.

clear_all_meters() ->
		?debugMsg("Clearing Meters"),
		ets:delete_all_objects( meters ),
		%ets:delete_all_objects( channels ),
		ok.

-endif.



initialize_meter( MeterID ) ->
		Result = ets:insert( meters, { MeterID, 0 } ),
		?assertEqual( true, Result ),
    initialize_channel( MeterID ),
    Result.


record_usage( MeterID, Usage, DateTime ) ->
		Result = ets:update_counter( meters, MeterID, Usage ),
		?assert( Result >= Usage ),
		ChannelID = MeterID,
		update_interval_history( ChannelID, Usage, DateTime ),
    Result.


%%
%% Local Functions
%%
-ifdef( BIG_MEMORY ).


update_interval_history( ChannelID, Usage, DateTime ) ->
		[History] = ets:lookup( channels, ChannelID ),
		{ ChannelID, List } = History,
		UpdatedHistory = { ChannelID, List ++ [ { DateTime, Usage } ]  },
		ets:insert(channels, UpdatedHistory ).

		
     
initialize_channel( ChannelID ) ->
		Result = ets:insert( channels, { ChannelID, [] } ),
		?assertEqual( true, Result ),
    Result.

initialize_channels() ->	
		?debugMsg( "initializing channels table "),
	
		case ets:info( channels ) of
				undefined ->
						ets:new( channels, [named_table, public]);
				_ ->
						channels
		end.

close() ->
		ets:tab2file( channels, "channels.tab" ),
		ok.

-else.   % IF WE DON'T HAVE A REALLY BIG MEMORY -- USE A DISK LOG -- WE CAN PROCESS LATER MAYBE

initialize_channels() ->
		% Use a disk_log.
		WrapSpecs = [ {name, channels}, {type, wrap}, {size,{ 20*1000*1000, 100 } }],
		HaltSpecs = [ {name, channels}, {type, halt}, { format, external }, { head, none   } ],
		Result = disk_log:open( HaltSpecs ),
		?debugVal( Result ),
		Result.

initialize_channel( _ChannelID ) ->
		dummy.

		
update_interval_history( ChannelID, Usage, DateTime )  ->
		Result = disk_log:blog( channels, term_to_binary({ ChannelID, DateTime, Usage })    ),
		Result.

close() ->
		disk_log:sync( channels ),
		disk_log:close( channels ),
		?assertCmd( "ls -lah"),
		?debugVal( disk_log:open( [ {name, channels}
															, {type, halt}
															, { format, external }
															, { head, none }
															, {mode, read_only} ] )
		         ),
		SortResult = ?debugTime("sorting channel file",  file_sorter:keysort( [1,2], ["channels.LOG"], "sorted.LOG", [{ format, term }] ) ),
		%SortResult = ?debugTime("sorting channel file", sort( channels )    ),
		%SortResult = "sort suppressed",
		?debugVal( SortResult ),
		ok.

-endif.


sort(Log) ->
    {ok, _} = disk_log:open([ {name,Log}, {mode,read_only}]),
		{ok, _} = disk_log:open([ {name, sorted}, {repair,truncate}, {mode,read_write}]),

    Input = input(Log, start),
    Output = output([]),
    Reply = file_sorter:sort( Input, Output, {format,term}),
    ok = disk_log:close(Log),
		ok = disk_log:close(sorted),
		
    Reply.

input(Log, Cont) ->
    fun(close) ->
            ok;
       (read) ->
            case disk_log:chunk(Log, Cont) of
                {error, Reason} ->
                    {error, Reason};
                {Cont2, Terms} ->
                    {Terms, input(Log, Cont2)};
                {Cont2, Terms, _Badbytes} ->
                    {Terms, input(Log, Cont2)};
                eof ->
                    end_of_input
            end
    end.

output( L ) ->
		Result = disk_log:alog_terms( sorted, L ),
		Result.


outputX(L) ->
    fun(close) ->
            lists:append(lists:reverse(L));
       (Terms) ->
            output([Terms | L])
    end.    