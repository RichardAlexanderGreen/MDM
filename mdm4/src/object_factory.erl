%% Copyright 2011 Richard Alexander Green
%% Created: Apr 15, 2011
%% Description: 
%% - Creates partially initialized objects for mdm3 architecture.

-module(object_factory).

%%
%% Include files
%%

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

%%
%% Exported Functions
%%
-export([ make/1, service_on_transformer/1, transformer_on_circuit/1, meter_on_service/1 ]).

%%
%% API Functions
%%
make( meter ) ->
		% A meter is a device
		Device = make( device, meter ),
		%assets() ! #meter{ meter_ID = Device#device.device_ID };
		record_asset(  #meter{ meter_ID = Device#device.device_ID } );

make( service ) ->
		% A service is a PoD
		PoD = make( pod, service ), 
		%assets() ! #service{ service_ID = PoD#pod.pod_ID};
		record_asset(  #service{ service_ID = PoD#pod.pod_ID } );

make( circuit ) ->
		% A circuit is a PoD
		PoD = make(pod, circuit),
		%assets() ! #circuit{ circuit_ID = PoD#pod.pod_ID};
		record_asset(  #circuit{ circuit_ID = PoD#pod.pod_ID} );

make( transformer ) ->
		% A transformer is a PoD
		PoD = make(pod, transformer),
		%assets() ! #transformer{ transformer_ID = PoD#pod.pod_ID};
		record_asset(  #transformer{ transformer_ID = PoD#pod.pod_ID} ).


make( pod, SubType ) ->
		% A PoD is a device
		Device = make( device, SubType ),
		%assets() ! #pod{ pod_ID = Device#device.device_ID };
		record_asset(  #pod{ pod_ID = Device#device.device_ID } );

make( device, SubType ) ->
		% A device is an asset
		Asset = make( asset, SubType ),
		%assets() ! #device{ device_ID = Asset#asset.asset_ID };
		%ID = Asset#asset.asset_ID,
		%?debugVal( ID ),
		
		record_asset(  #device{ device_ID =  Asset#asset.asset_ID } );

make( asset, SubType ) ->
		% An asset has a unique-ID
		%assets() ! #asset{ asset_ID = make( id ) };
		record_asset(  #asset{ asset_ID = make( id, SubType ) } );

		

make( id, Subtype ) ->
		% now() produces a (locally unique) identifier
		% TODO -- Improve this to produce a globally unique identifier
		Serial = gen_server:call({global,assets}, next_ID ),
    { Subtype,  Serial }.
		
meter_on_service( Service ) ->
		Meter = make( meter ),
		%assets() ! Meter#meter{ service_ID = Service#service.service_ID }.
    record_asset( UpdatedMeter = Meter#meter{ service_ID = Service#service.service_ID } ),
    UpdatedMeter.

service_on_transformer( Transformer ) ->
		Service = make( service ),
		%assets() ! Service#service{ transformer_ID = Transformer#transformer.transformer_ID }.
    record_asset(  UpdatedService = Service#service{ transformer_ID = Transformer#transformer.transformer_ID } ),
		UpdatedService.

transformer_on_circuit( Circuit ) ->
		Transformer = make( transformer ),
		%assets() ! Transformer#transformer{ circuit_ID = Circuit#circuit.circuit_ID }.
    record_asset( UpdatedTransformer = Transformer#transformer{ circuit_ID = Circuit#circuit.circuit_ID } ),
    UpdatedTransformer.

%%
%% Local Functions
%%

removed_assets() ->
		Node = 'mdm3Node@RichardGreenMacBookPro.local',
		%{ assets, Node }.
		{ global, assets }.

record_asset( Asset ) -> 
		%?debugVal( Asset ),	
		gen_server:cast( {global, assets}, Asset ),
		Asset.




% =========================== TESTS ================================

% Make raw objects (no attributes are populated)

make_id_test()          -> 
		?debugVal( ets:info( assets, size ) ),
		
		make( id, site ).

make_asset_test()       -> make( asset, capacitor ).
make_device_test()      -> make( device, capacitor ).
make_pod_test()         -> make( pod, capacitor ).

make_meter_test()       -> make( meter ).
make_service_test()     -> make( service ).
make_transformer_test() -> make( transformer ).
make_circuit_test()     -> make( circuit ).

% Make PoD objects with upstream connections.

meter_on_service_test() ->
		Meter = meter_on_service( make(service) ),
		?debugVal( Meter ),
		
		?assert( undefined =/= Meter#meter.service_ID ).

service_on_transformer_test() ->
		Service = service_on_transformer( make( transformer) ),
		?debugVal( Service ),
		?assert( undefined =/= Service#service.transformer_ID ).

transformer_on_circuit_test() ->
		Transformer = transformer_on_circuit( make( circuit ) ),
		?debugVal( Transformer ),
		?assert( undefined =/= Transformer#transformer.circuit_ID ).


	
	
	


		