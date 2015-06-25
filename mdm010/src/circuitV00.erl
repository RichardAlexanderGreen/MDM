%% Copyright 2011 Richard Alexander Green
%% Description: TODO: Add description to circuit
%% Created: Jun 17, 2011
%% ------------------------------------------
-module(circuitV00).

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

% A circuit is an asset.
-record( asset, { id           % entity ID   { <enity_type>, [] }
	              , location   = 'undefined geo-location'  
	              , parent     = 'undefined parent'
	              , components = []   % A circuit`s components are transformers
								}).

% A circuit is an energy sink.
-record( sink, { id           % entity ID
							 , aggregator   = 'undefined aggregator'
	             , load_profile = 'undefined load_profile'
							 , load_history = [] 
							 } ).


-record( state, { id                  % entity ID
								, asset = #asset{}    % A circuit is an asset
	              , sink  = #sink{}     % A circuit is also an energy sink
								, monitor             % A circuit has a monitor (PID). A Monitor may monitor several circuits
								} ).

%%
%% Exported Functions
%%
-export([init/1,setup/1]).

%%
%% API Functions
%%

% Create a circuit module via a simple_one_for_one supervisor 
% The supervisor may call circuit:setup() to get standard / randomized values
init( [ { id, Identifier}
			, { number_services, NServices}
			, { transformer_fanout, NServicesPerTransformer }
			]) ->
		{ ok, self() }.

% Setup standard arguments for initialization of a circuit.
% Note: Entity_ID should be { circuit, [X] }
setup( { circuit, [ Identifier ] } ) ->
		Entity_ID = { circuit, [ Identifier ] },
		NServices = 1000,
		NServicesPerTransformer = 10,
		InitializationArgs = [ { id, Entity_ID }
			                   , { number_services, NServices}
			                   , { transformer_fanout, NServicesPerTransformer }
			                   ],
		InitializationArgs.


%%
%% Local Functions
%%



%% //////////////////////////// TESTS ///////////////////////////////////////
