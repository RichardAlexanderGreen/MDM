%% Copyright 2011 Richard Alexander Green
%% Description: Record usage history (interval data) at channel level.
%% Also includes functions that price interval usage (time of day and/or market price).
%%
%% Created: Jul 6, 2011
%% Modified: July 9, 2011 
%% - Use disk_log instead of file module for read and write.
%% - Eliminate sort function. I was never able to get it to work.
%% - Pricing simply writes a priced version of the interval to a priced usage log.
%% - The idea is that invoicing will scan the priced usage to assemble an invoice
%%
%% Comment: Pricing probably belongs in another module.
%% - I put it here because it has to know how the channels log is written.
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

% Initialize disk-log for interval history
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

% Clear interval history
clear_all_channel_history() ->
		initialize_storage().     % TODO - WARNING, THIS DOES NOT REALLY CLEAR HISTORY

% Initialize an individual channel store
initialize_channel( _ChannelID ) ->
		dummy.   % In this implementation, there is nothing to do.

% Record the usage for the given date-time
update_interval_history( ChannelID, Usage, DateTime )  ->
		% Write usage to disk for processing later
		LogResult = disk_log:log(channels, { ChannelID, DateTime, Usage }   ),
		?assertEqual(ok, LogResult ),
		LogResult.

% Close the interval history log
close() ->
		ok = disk_log:sync( channels ),                         % Flush any queued data to disk
		io:format("~n~p~n", [ disk_log:info( channels ) ]  ),   % Report 
		
		FileCloseResult = disk_log:close( channels ),
		?assertEqual( ok, FileCloseResult ),
		FileCloseResult.

% Select channels in the given cycle
% and price their usage according to the price-schedule ( maps hours to prices )
market_price( Cycle, PriceSchedule ) ->
		?debugVal( {"pricing intervals for cycle:", Cycle} ),
		
		% Open read-only
		open_read_only(),
		open_price_log(),
		
		% Select channels in given cycle.
		price_chunks( Cycle, PriceSchedule, start ),
		disk_log:close( channels ).

%% -------------------------------------------------------------------------------------------
%% Internal Functions

% Open the interval history file in read-only mode
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

% Price the intervals in the interval history for a given cycle
% This function unpacks the the data
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

% Price the intervals in the interval history for a given cycle
% This function prices a block of intervals
price_terms( [], Cycle, PriceSchedule ) ->
		done;
price_terms( Intervals, Cycle, PriceSchedule ) ->									 
		[ IntervalUsage | RemainingIntervals ] = Intervals,
		{ ChannelID, DateTime, Usage } = IntervalUsage,
		
		price( ChannelID, DateTime, Usage, Cycle, PriceSchedule ),
		
		price_terms( RemainingIntervals, Cycle, PriceSchedule ).

% Price the intervals in the interval history for a given cycle
% This function prices individual intervals
price( ChannelID, DateTime, Usage, Cycle, PriceSchedule ) ->
		
		case channel_cycle( ChannelID ) of
				
				( {ignore, _, _ } ) ->
						"Test data ignored - not priced";	
				
				( Cycle ) ->
						Rate = rate_for_datetime( DateTime, PriceSchedule ),
						PricedUsage = Usage * Rate,
						PricedRecord = { ChannelID, DateTime, Usage, PricedUsage },
						% Write priced usage record to priced_usage log
						disk_log:log( priced_usage, PricedRecord );  
				
				_OtherCycle ->
						"ignore other cycles"
		end.

% Look-up the rate in effect for the given date-time
rate_for_datetime( DateTime, PriceSchedule ) ->
		Rate = 0.11 / 1000.0,  % TODO: Replace time-of-day pricing dummy
		Rate.

% What cycle does this channel (service) belong to ?
% WARNING: This implementation uses a cheap trick -- It calculates 'Cycle = circuit-ID modulo 20'
% This will work okay as long as needed data is collected prior to the pricing.
% In the context of this implementation,
%  the data is collected hourly,
%  therefore this trick will work to partition the system into 20 billing-days
channel_cycle( ChannelID ) ->
		{ _Type, Path } = ChannelID,
    ReversedPath = lists:reverse( Path ),
		Return = case ReversedPath of 
								 % NOTE: PATTERN MUST BE CORRELATED WITH HOW CHANNELS ARE IDENTIFIED
								 % PATTERN ASSUMES CHANNEL IS ONE LEVEL BELOW SERVICE
								 [ 1, Service, Transformer, Circuit | Remaider ] ->
										 Cycle = Circuit rem 20,                           % Cycle = circuit-ID modulo 20
										 %?debugVal( { channel_cycle, ChannelID, Cycle } ),
										 Cycle;
								 _ ->
										 ?debugVal( { ignore, "ignore test data path:", Path } ),
										 { ignore, "ignore test data path:", Path }   % Ignore other Short (test) Path patterns
						 end,
		Return.

% This file contains the priced usage
% Each record is like an interval history record but with price included
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
						exit( {error, "trouble opening priced usage log:", Trouble} )
		end,
		FileOpenResult.

% TODO: Open a price log whose name includes the cycle being priced.


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
		% WARNING: Assumes that the channel ID matches the meter ID
		ChannelID = { channel, [ Region, Station, Circuit, Transformer, Service, Meter ] },
		Cycle = channel_cycle( ChannelID ),
		?assertEqual( 97 rem 20, Cycle ).

price_test() ->
		% WARNING: Assumes that the channel ID matches the meter ID
		ChannelID = { channel, [ 888, 1, 111, 97, 4, 3, 2, 1 ] },
		Cycle = channel_cycle( ChannelID ),
		DateTime = now(),
		PriceSchedule = dummy,
		Usage = 1000,
		Result = price( ChannelID, DateTime, Usage, Cycle, PriceSchedule ),
		?assertEqual( ok, Result ).


market_price_test() ->
		Cycle = 1,
		PriceSchedule = dummy,                  % TODO: Replace dummy price schedule
		market_price( Cycle, PriceSchedule ).

