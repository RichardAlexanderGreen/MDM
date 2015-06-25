%%% Copyright 2011 Richard Alexander Green
%%% Created: June 1, 2011
%%% Description: This is version 6 of MDM application architecture.
%%% It differs from mdm4 in that it uses mnesia instead of dets as its storage mechanism.
%%% It differs from mdm5 in that it makes *direct calls* between layers 
%%%   instead of using gen_server:call/3 or gen_server:cast/2
%%%
%%% - This version forwards usage data from layer to layer:
%%% Simulated meter sends usage to service (a listener).
%%% Service records usage and forwards to transformer.
%%% Transformer aggregates usage and forwards to circuit.
%%% Circuit aggregates usage.
%%% Futures:
%%% - Devices know their downstream connections 
%%%   so that they can broadcast messages downstream.
%%% - - A circuit object knows which transformers are connected.
%%% - - A transformer object know which services are connected.
%%% - - A service object knows its metering device/s.

-module(mdm6_app).

-behaviour(application).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

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

%% --------------------------------------------------------------------
%% Behavioural exports
%% --------------------------------------------------------------------
-export([
	 start/2,
	 stop/1
        ]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------


%% ====================================================================!
%% External functions
%% ====================================================================!
%% --------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%% --------------------------------------------------------------------
start(Type, StartArgs) ->
    case 'TopSupervisor':start_link(StartArgs) of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    Error
    end.

%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop(State) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

