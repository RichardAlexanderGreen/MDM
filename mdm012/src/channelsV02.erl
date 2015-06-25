%% Copyright 2011 Richard Alexander Green
%% Description: Record usage history (interval data) at channel level.
%%
%% Created: Jul 6, 2011
%% ------------------------------------------
-module(channelsV02).

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

%%
%% API Functions
%%

initialize_storage() ->
		% Use an ordinary file.
		FileName = "channels.log",
		Modes = [ write , binary ],
		FileOpenResult = file:open( FileName, Modes ),
		?debugVal( FileOpenResult ),
    { ok, IODevice } = FileOpenResult,
		put( file, IODevice ),
		FileOpenResult.

clear_all_channel_history() ->
		initialize_storage().


initialize_channel( _ChannelID ) ->
		dummy.
		
update_interval_history( ChannelID, Usage, DateTime )  ->
		IODevice = get( file ),
		%Result = file:write( IODevice, { ChannelID, DateTime, Usage }   ),
		Result = file:write( IODevice, term_to_binary( { ChannelID, DateTime, Usage } )   ),
		Result.

close() ->
		IODevice = get( file ),
		FileCloseResult = file:close( IODevice ),
		?assertEqual( ok, FileCloseResult ).

sort_channel_data() ->
		% TODO: I've not been able to get this to work with file module -- Going back to disk_log / different approach.
		SortResult = file_sorter:keysort( [1,2], ["channels.log"], "sorted.log" ),
		?assertEqual( ok, SortResult ).

		

%%
%% Local Functions
%%

		