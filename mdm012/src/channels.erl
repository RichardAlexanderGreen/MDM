%% Copyright 2011 Richard Alexander Green
%% Description: Record usage history (interval data) at channel level.
%%
%% Created: Jul 6, 2011
%% Modified: July 9, 2011 
%% - Use disk_log instead of file for read and write.
%% - Eliminate sort function. I was never able to get it to work.
%% 
%% ------------------------------------------
-module(channels).

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
		% Use a disk_log
		{MaxNoBytes, MaxNoFiles} = {1024*1024*1024, 32},
		Options = [ { name, channels } 
							, { type, wrap }, { size, {MaxNoBytes, MaxNoFiles}  } 
							],
		FileOpenResult = disk_log:open( Options ),
		?debugVal( FileOpenResult ),
    case FileOpenResult of
				{ok, channels} ->
						ok;
				{repair, _ } ->
						ok;
				Trouble ->
						exit( {error, "trouble opening interval history log:", Trouble} )
		end,
		FileOpenResult.

clear_all_channel_history() ->
		initialize_storage().


initialize_channel( _ChannelID ) ->
		dummy.
		
update_interval_history( ChannelID, Usage, DateTime )  ->
		LogResult = disk_log:log(channels, { ChannelID, DateTime, Usage }   ),
		?assertEqual(ok, LogResult ),
		LogResult.

close() ->
		ok = disk_log:sync( channels ),
		io:format("~n~p~n", [disk_log:info( channels )] ),
		
		FileCloseResult = disk_log:close( channels ),
		?assertEqual( ok, FileCloseResult ),
		FileCloseResult.

% Select channels in the given cycle
% and price their usage according to the price-schedule (maps hours to prices)
market_price( Cycle, PriceSchedule ) ->
		% Open read-only
		open_read_only(),
		open_price_log(),
		
		% Select channels in given cycle.
		price_chunks( Cycle, PriceSchedule, start ),
		disk_log:close( channels ).

%% Internal Functions

open_read_only() ->		
		Options = [ { name, channels }
							, { type, wrap }
							, { mode, read_only } 
							],
		FileOpenForReadResult = disk_log:open( Options ),
		?debugVal( FileOpenForReadResult ),
    case FileOpenForReadResult of
				{ok, channels} ->
						ok;
				{repair, _ } ->
						ok;
				Trouble ->
						exit( {error, "trouble opening interval history log:", Trouble} )
		end,
		ok.

price_chunks( Cycle, PriceSchedule, Continuation ) ->
		
		NextResult = disk_log:chunk( channels, Continuation),
		case NextResult of
				eof ->
						done;
				{error, Reason} ->
						exit( {error, "Trouble in price_chunks:", Reason} );
				{Continuation2, Terms} ->
						price_terms( Terms, Cycle, PriceSchedule ),
						price_chunks( Cycle, PriceSchedule, Continuation2 )
		end.

price_terms( [], Cycle, PriceSchedule ) ->
		done;
price_terms( Intervals, Cycle, PriceSchedule ) ->									 
		[ IntervalUsage | RemainingIntervals ] = Intervals,
		{ ChannelID, DateTime, Usage } = IntervalUsage,
		
		price( ChannelID, DateTime, Usage, Cycle, PriceSchedule ),
		
		price_terms( RemainingIntervals, Cycle, PriceSchedule ).


price( ChannelID, DateTime, Usage, Cycle, PriceSchedule ) ->
		case channel_cycle( ChannelID ) of
				( Cycle ) ->
						Rate = rate_for_datetime( DateTime, PriceSchedule ),
						PricedUsage = Usage * Rate,
						PricedRecord = { ChannelID, DateTime, Usage, PricedUsage },
						disk_log:log( priced_usage, PricedRecord );
				_ ->
						ignore
		end.

rate_for_datetime( DateTime, PriceSchedule ) ->
		Rate = 0.11 / 1000.0,  % TODO: REPLACE TIME OF DAY PRICE
		Rate.

channel_cycle( ChannelID ) ->
		{ _Type, Path } = ChannelID,
    ReversedPath = lists:reverse(Path),
		Return = case ReversedPath of 
								 [ 1, Service, Transformer, Circuit | Remaider ] ->
										 Cycle = Circuit rem 20, 
										 %?debugVal( { channel_cycle, ChannelID, Cycle } ),
										 Cycle;
								 _ ->
										 -1111   % Ignore other Short (test) Path patterns
						 end,
		Return.

open_price_log() ->
	  % Use a disk_log
		{MaxNoBytes, MaxNoFiles} = {1024*1024*1024, 32},
		Options = [ { name, priced_usage } 
							, { type, wrap }, { size, {MaxNoBytes, MaxNoFiles}  } 
							],
		FileOpenResult = disk_log:open( Options ),
		?debugVal( FileOpenResult ),
    case FileOpenResult of
				{ok, priced_usage } ->
						ok;
				{repair, _ } ->
						ok;
				Trouble ->
						exit( {error, "trouble opening interval history log:", Trouble} )
		end,
		FileOpenResult.


%% ////////////////////////////// TESTS /////////////////////////////////////////		

open_price_log_test() ->
		open_price_log().

channel_cycle_test() ->
		Region = 888,
		Station = 777,
		Circuit = 97,
		Transformer = 73,
		Service = 8,
		Meter = 1,
		ChannelID = { channel, [ Region, Station, Circuit, Transformer, Service, Meter ] },
		Cycle = channel_cycle( ChannelID ),
		?assertEqual( 97 rem 20, Cycle ).

price_test() ->
		ChannelID = { channel, [ 888, 1, 111, 97, 4, 3, 2, 1 ] },
		Cycle = channel_cycle( ChannelID ),
		DateTime = now(),
		PriceSchedule = dummy,
		Usage = 1000,
		Result = price( ChannelID, DateTime, Usage, Cycle, PriceSchedule ),
		?assertEqual( ok, Result ).


market_price_test() ->
		Cycle = 1,
		PriceSchedule = dummy,
		market_price( Cycle, PriceSchedule ).

