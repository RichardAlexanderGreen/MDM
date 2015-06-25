%% Copyright 2011 Richard Alexander Green
%% Description: TODO: Add description to service
%% Created: Jun 25, 2011
%% ------------------------------------------
-module(service).

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

-record( service, { id
	                , meter
	                } ).


%%
%% API Functions
%%
create( N, TransformerPath ) ->
		ServicePath = TransformerPath ++ [N],
		Service = #service{ id = { service,ServicePath }  }.

		


%%
%% Local Functions
%%

