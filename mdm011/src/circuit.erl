%% Copyright 2011 Richard Alexander Green
%% Description: TODO: Add description to circuit
%% Created: Jun 25, 2011
%% ------------------------------------------
-module(circuit).

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

-record( circuit, { id
	                , planned_services = 1000
									, services_per_transformer = 10
	                , components = []
	                } ).

%%
%% API Functions
%%
create(  N, StationPath ) ->
		CircuitPath = StationPath ++ [N],
		Circuit = #circuit{ id = { circuit,CircuitPath }  },
		Circuit.


install_planned_services( Circuit ) ->
		#circuit{ planned_services = NServices, services_per_transformer = Fanout }  = Circuit,
		NTransformers = 1 + ( NServices div Fanout),
		NewCircuit = install_transformers( NTransformers, Circuit ),
		UpdatedCircuit = install_services( NewCircuit ),
		?debugVal( UpdatedCircuit ),
		UpdatedCircuit.

%%
%% Local Functions
%%

install_transformers( 0, UpdatedCircuit ) ->
		UpdatedCircuit;
install_transformers( N, CircuitWIP ) ->
		#circuit{ id = { circuit, CircuitPath }, components = Components } = CircuitWIP,
		Transformer = transformer:create(  N, CircuitPath ),
		UpdatedCircuit = CircuitWIP#circuit{components = ( Components ++ [Transformer] )} ,
		install_transformers( N - 1 , UpdatedCircuit ).

install_services( Circuit ) ->
		#circuit{ components = Transformers } = Circuit,
		NewTransformers = tell_transformers( Transformers, install_services, [] ),
		UpdatedCircuit = Circuit#circuit{ components = NewTransformers },
		UpdatedCircuit.

tell_transformers( [], install_services, UpdatedTransformers ) ->
		UpdatedTransformers;
tell_transformers( Transformers, install_services, TransformersWIP ) ->
		[ Transformer | RemainingTransformers ] = Transformers,		
		UpdatedTransformer = transformer:install_services( Transformer ),
	  UpdatedTransformers =	TransformersWIP ++ [UpdatedTransformer],
		tell_transformers( RemainingTransformers, install_services, UpdatedTransformers ).

%% /////////////////////////////////////// TESTS //////////////////////////////////////////

install_transformers_test() ->
		StationPath = [1001],
		Circuit = create( 7, StationPath ),
		N = 17,
		UpdatedCircuit = install_transformers( N, Circuit ),
		%?debugVal( UpdatedCircuit ),
		UpdatedCircuit.

install_services_test() ->
		Circuit = install_transformers_test(),
		UpdatedCircuit = install_services( Circuit ),
		?debugVal( UpdatedCircuit ),
		UpdatedCircuit.



		
		
