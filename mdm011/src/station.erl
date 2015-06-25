%% Copyright 2011 Richard Alexander Green
%% Description: TODO: Add description to station
%% Created: Jun 25, 2011
%% ------------------------------------------
-module(station).

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

-record( station, { id
	                , planned_circuits = 100
	                , components = []
	                } ).

%%
%% API Functions
%%

install_planned_circuits( Station ) ->
		#station{ planned_circuits = N }  = Station,
		UpdatedStation = install_circuits( N, Station ),
		UpdatedStation.

%%
%% Local Functions
%%

install_circuits( 0, UpdatedStation ) ->
		UpdatedStation;
install_circuits( N, StationWIP ) ->
		#station{ id = { station, StationPath }, components = Components } = StationWIP,
		Station = circuit:create(  N, StationPath ),
		UpdatedStation = StationWIP#station{components = ( Components ++ [Station] )} ,
		install_circuits( N - 1 , UpdatedStation ).

install_services( Station ) ->
		#station{ components = Circuits } = Station,
		UpdatedCircuits = tell_circuits( Circuits, install_services, [] ),
		UpdatedStation = Station#station{ components = UpdatedCircuits },
		UpdatedStation.

install_transformersX( Station ) ->
		#station{ components = Circuits } = Station,
		UpdatedCircuits = tell_circuits( Circuits, install_transformers, [] ),
		UpdatedStation = Station#station{ components = UpdatedCircuits },
		UpdatedStation.


tell_circuits( [], _FunctionName, UpdatedCircuits ) ->
		UpdatedCircuits;
tell_circuits( Circuits, FunctionName, CircuitsWIP ) ->
		[ Circuit | RemainingCircuits ] = Circuits,		
		UpdatedCircuit = circuit:FunctionName( Circuit ),
	  UpdatedCircuits =	CircuitsWIP ++ [UpdatedCircuit],
		tell_circuits( RemainingCircuits, install_services, UpdatedCircuits ).


%% ///////////////////////////////// TESTS //////////////////////////////////////////


install_planned_circuits_test() ->
		StationPath = [1001],
		Station = #station{ id = { station, StationPath } },
		UpdatedStation = install_planned_circuits( Station ),
		?debugVal( UpdatedStation ),
		UpdatedStation.

install_transformers_testX() ->
		Station = install_planned_circuits_test(),
		UpdatedStation = install_transformersX( Station ),
		?debugVal( UpdatedStation ),
		UpdatedStation.


install_services_testX() ->
		Station = install_transformers_testX(),
		UpdatedStation = install_services( Station ),
		?debugVal( UpdatedStation ),
		UpdatedStation.
