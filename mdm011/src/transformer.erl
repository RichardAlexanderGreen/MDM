%% Copyright 2011 Richard Alexander Green
%% Description: TODO: Add description to transformer
%% Created: Jun 25, 2011
%% ------------------------------------------
-module(transformer).

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
-export([install_services/1]).

-record( transformer, { id
	                    , components = []
											, fanout = 10
	                    } ).
%%
%% API Functions
%%

create(  N, CircuitPath ) ->
		% Create an instance.
		TransformerPath = CircuitPath ++ [N],
		Transformer = #transformer{ id = { transformer, TransformerPath } },															
		Transformer.

install_services( Transformer ) ->
		%?debugVal( { install_services, Transformer }  ),
		#transformer{ fanout = NServices } = Transformer,
		UpdatedTransformer = install_service( NServices, Transformer ),
		UpdatedTransformer.
		

%%
%% Local Functions
%%
install_service( 0, UpdatedTransformer ) ->
		UpdatedTransformer;

install_service( N, TransformerWIP ) ->
		#transformer{ id = {transformer, TransformerPath }, components = Components } = TransformerWIP,
		Service = service:create( N, TransformerPath ),
		UpdatedTransformer = TransformerWIP#transformer{ components = Components ++ [Service] },
		install_service( N - 1, UpdatedTransformer ).

		
%% /////////////////////////////////////// TESTS //////////////////////////////////////////////////

install_services_test() ->
		CircuitPath = [1001,7],
		Transformer = create( 17, CircuitPath ),
		UpdatedTransformer = install_services( Transformer ),
		?debugVal( UpdatedTransformer ),
		UpdatedTransformer.


