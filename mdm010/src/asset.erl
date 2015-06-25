%% Copyright 2011 Richard Alexander Green
%% Description: TODO: Add description to asset
%% Created: Jun 22, 2011
%% ------------------------------------------
-module(asset).

%%
%% Include files
%%
-define( NOT_IMPLEMENTED, exit( {?MODULE,'has not implemented this function'} )  ).


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

%%
%% Exported Functions
%%
-export([]).

-record( asset, { id
	              , location
	              , components = []
	              }).


%%
%% API Functions
%%

make_asset( AssetType, AssetID ) ->
		make_asset( AssetType, AssetID, undefined ).

make_asset( AssetType, AssetID, Location ) ->
		?assert( is_list( AssetID ) ),
		#asset{ id = {AssetType, AssetID}, location = Location }.

add_component_to_asset( Component, Asset ) ->
		?assertMatch( {Type, ID} when is_list(ID), Component ),
		Components = get_asset_components( Asset ),
		NewAsset = Asset#asset{ components = Components ++ [Component] },
		NewAsset.

get_asset_components( Asset ) ->
		#asset{ components = Components } = Asset,
		Components.

%%
%% Local Functions
%%



% /////////////////////////////// TESTS ////////////////////////////////////

make_asset_test() ->
		AssetType = test_asset,
		AssetID = [1,2,3],
		
		#asset{ id = { AssetType, AssetID} } = make_asset( AssetType, AssetID ).

make_asset_embeded_type_test() ->
		AssetType = test_asset,
		AssetID = {test_type, [1,2,3] },	   % ID should be a list
		?assertError( { assertion_failed, _ }, make_asset(  AssetType, AssetID ) ). 

make_asset_args_reversed_test() ->
		AssetType = test_asset,
		AssetID = [1,2,3],	
		?assertError( { assertion_failed, _ }, make_asset(  AssetID, AssetType ) ).  % Arguments reversed.

add_component_to_asset_test() ->
		AssetType = test_asset,
		AssetID = [1,2,3],		
		Asset = make_asset( AssetType, AssetID ),
		
		Component = { test_type, [1,2,3] },
		NewAsset = add_component_to_asset( Component, Asset ),
		Components = get_asset_components( NewAsset ),
		?assertEqual( [Component], Components ).

add_bad_component_1_to_asset_test() ->
		AssetType = test_asset,
		AssetID = [1,2,3],		
		Asset = make_asset( AssetType, AssetID ),
		
		Component = [1,2,3,4],  % Bad component ID
		?assertError( { assertMatch_failed, _ }, add_component_to_asset( Component, Asset )  ).

add_bad_component_2_to_asset_test() ->
		AssetType = test_asset,
		AssetID = [1,2,3],		
		Asset = make_asset( AssetType, AssetID ),
		
		Component = { test_type, {test_type, [1,2,3,4]} },
		
    ?assertError( { assertMatch_failed, _ }, add_component_to_asset( Component, Asset )  ).

		





		






