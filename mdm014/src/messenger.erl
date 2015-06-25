%% Copyright 2011 Richard Alexander Green
%% Description: Defines send_notice_to_actor( Notice, ActorID )
%% This is prototype messenger. It has minimal functionality.
%% Created: Jun 27, 2011
%% ------------------------------------------
-module(messenger).

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
-export([send_notice_to_actor/2]).

%%
%% API Functions
%%

% Send notice to actor identified by ID.
send_notice_to_actor( Notice, ActorID ) ->
		%?debugVal( { send_notice_to_actor, Notice, ActorID } ),
		% TODO: Add a log / replay functions -- Current function is just a pass through
		actors:notify( ActorID, Notice ).

% Call the notify method of the given type.
% Type : a module name
% ActorRecord : Contains the actors state
% Notice is the message passed
local( test, ActorRecord, Notice ) ->
		?debugVal( {test, ActorRecord, Notice} ),
		"Test notice received.";

local( Module, ActorRecord, Notice ) ->
		Module:notify( ActorRecord, Notice ).

%%
%% Local Functions
%%

