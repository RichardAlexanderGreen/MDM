%% Copyright 2011 Richard Alexander Green
%% Description: Simulate a meter
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

-record( meter, { id, register = 0, mtd_usage = 0, mtd_dollars = 0.000 } ).

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
    channels:initialize_storage().

close()  ->
		Tab2FileResult = ets:tab2file( meters, "meters.table"),
		?assertEqual( ok, Tab2FileResult ),
		channels:close().


clear_all_meters() ->
    ?debugVal("Clearing meters and channels tables."),
		ets:delete_all_objects( meters ),
		channels:clear_all_channel_history(),
		ok.

initialize_meter( MeterID ) ->
		Result = ets:insert( meters, { MeterID, 0, 0, 0.00 } ),
		?assertEqual( true, Result ),
    channels:initialize_channel( MeterID ),
    Result.


record_usage( MeterID, Usage, DateTime ) ->
		Result = ets:update_counter( meters, MeterID, [{2,Usage}, {3,Usage} ]  ),
		?assert( Result >= Usage ),
		ChannelID = MeterID,
		channels:update_interval_history( ChannelID, Usage, DateTime ),
    Result.

price_mtd_usage( MeterID ) ->
		[RecordToBePriced] = ets:lookup( meters, MeterID ),
		%?debugVal( RecordToBePriced ),
		{ MeterID, Register, UsageMTD, _PriorPrice } = RecordToBePriced,
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
