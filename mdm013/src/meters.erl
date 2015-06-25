%% Copyright 2011 Richard Alexander Green
%% Description: Simulate a collection of meters
%% 
%% Created: Jul 4, 2011
%% ------------------------------------------
-module(meters).

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
-export([ initialize_table/0, initialize_meter/1, record_usage/3, price_mtd_usage/1] ).

-record( meter, { id
	              , register = 0
								, mtd_usage = 0
								, mtd_peak_load = 0 
								, mtd_dollars = 0.000
								} ).

%%
%% API Functions
%%

% Initialize the meters table.
initialize_table() ->
		case ets:info( meters ) of
				undefined ->
						ets:new( meters, [ named_table, public, {keypos,2} ] );  % Create an empty table
				_ ->
						meters
		end,
    channels:initialize_storage().

% TODO -- Initialize table from disk

% Close the meters table and write contents to disk.
close()  ->
		Tab2FileResult = ets:tab2file( meters, "meters.table"),
		?assertEqual( ok, Tab2FileResult ),
		channels:close().

% Clear the meters table (Called from simulation)
clear_all_meters() ->
    ?debugVal("Clearing meters and channels tables."),
		ets:delete_all_objects( meters ),
		channels:clear_all_channel_history(),
		ok.

initialize_meter( MeterID ) ->
		Result = ets:insert( meters, #meter{ id = MeterID } ),
		?assertEqual( true, Result ),
    channels:initialize_channel( MeterID ),
    Result.


record_usage( MeterID, Usage, DateTime ) ->
		%Result = ets:update_counter( meters, MeterID, [{2,Usage}, {3,Usage} ]  ),
		LookupResult = ets:lookup( meters, MeterID ),
		%?debugVal( LookupResult ),
		[ MeterRecord ] = LookupResult,
		
		#meter{ register = Register, mtd_usage = MTD_Usage, mtd_peak_load = MTD_Peak }  = MeterRecord,
		UpdatedMeterRecord = MeterRecord#meter{ register = Register + Usage
																					, mtd_usage = MTD_Usage + Usage
																					, mtd_peak_load = max( MTD_Peak, Usage ) },
		ets:insert( meters, UpdatedMeterRecord ),
		
		% Update my interval history
		ChannelID = MeterID,
		channels:update_interval_history( ChannelID, Usage, DateTime ),
    ok.


price_mtd_usage( MeterID ) ->
		[RecordToBePriced] = ets:lookup( meters, MeterID ),
		%?debugVal( RecordToBePriced ),
		#meter{ id = MeterID, register = Register, mtd_usage = UsageMTD } = RecordToBePriced,
		UpdatedPrice = flat_price( MeterID, UsageMTD ),
		UpdatedRecord = { MeterID, Register, UsageMTD, UpdatedPrice },
		true = ets:insert( meters, UpdatedRecord ).

%%
%% Local Functions
%%

flat_price( MeterID, Usage ) ->
		Rate = 0.10 / 1000.0,       % Rate is 10 cents per kilowatt-hour
		PricedUsage = Usage * Rate,
		PricedUsage.
