%% Copyright 2011 Richard Alexander Green
%% Description: This module tracks actors and provides utilities for activating / storing / notifying
%% Created: Jun 27, 2011
%% ------------------------------------------
-module(actors).

%%
%% Include files
%%
-define( NOT_IMPLEMENTED, exit( {?MODULE,'has not implemented this function'} )  ).

-define( TEST, true ).
-define( NODEBUG, true ).

-include_lib("eunit/include/eunit.hrl").

-ifdef( DEBUG ).
-compile( export_all ).
-endif.

%%
%% Exported Functions
%%
-export([add_actor/1, get_actor/1, init_world/0, notify/2, update_actor/1]).


%%
%% API Functions
%%

init_world() ->
		case ets:info( world ) of
				undefined ->
						ets:new( world, [named_table, public]);
				_ ->
						world
		end.


% Add a new object's record to the world.
add_actor( ActorRecord ) ->
		ActorID = element( 2, ActorRecord ), % First element is record type. Second is id / key
		?debugVal( { add_actor, ActorID } ),
		Result = ets:insert_new( world, { ActorID, ActorRecord } ),
		case Result of
				false -> exit( {"Attempt to add same actor a second time: ", ActorID } );
				true ->
						ok
		end,
		ok.

update_actor( ActorRecord ) ->
		ActorID = element( 2, ActorRecord ), % First element is record type. Second is id / key
		?debugVal( { update_actor, ActorID } ),
		ets:insert( world, { ActorID, ActorRecord } ),
		ok.

% Get the object's record from the world.
get_actor( ActorID ) ->
		Result = ets:lookup( world, ActorID),
		case Result of
				[] ->
						exit( {"no such actor: ", ActorID } ),
						Key = ActorID,
						ActorRecord = {ActorID, "no such actor: ", ActorID };
				_ ->
						[ {Key, ActorRecord} ] = Result
		end,
		ActorRecord.

% Assume actor's record-type identifies its module and call type:notify( ActorRecord, Notice )'
notify( ActorID, Notice ) ->
		%?debugVal( { notify, ActorID, Notice }  ),
		ActorRecord = get_actor( ActorID ),
		Type = element( 1, ActorRecord ),
		?debugVal( { notify, ActorRecord, Type} ),
		Result = messenger:local( Type, ActorRecord, Notice ),
		Result.

%%
%% Local Functions
%%


%% ///////////////////////////////// TESTS /////////////////////////////////////

init_world_test() ->
		?assertEqual( world, init_world() ),
		ets:info(world).

add_actor_test() ->
		ActorID =  { actor_type, [1,2] },
		ActorRecord = {record_type,ActorID, data },
		ok = add_actor( ActorRecord ),
		[Result] = ets:lookup( world, { actor_type, [1,2] } ),
		?assertEqual( {ActorID,ActorRecord}, Result ).

get_actor_test() ->
		ActorID = { actor_type, [1,2] },
		ActorRecord = get_actor( ActorID ),
		?assertEqual( {record_type,{actor_type,[1,2]},data} , ActorRecord ).

add_actor_twice_test() ->
		ActorID =  { actor_type, [1,2] },
		ActorRecord = {record_type,ActorID, data },
		?assertExit( { "Attempt to add same actor a second time: ", {actor_type,[1,2]} } 
		           , add_actor( ActorRecord ) ).


update_actor_test() ->
		ActorID =  { actor_type, [1,2] },
		ActorRecord = {record_type,ActorID, data2 },
		ok = update_actor( ActorRecord ),
		[Result] = ets:lookup( world, { actor_type, [1,2] } ),
		?assertEqual( {ActorID,ActorRecord}, Result ).



notify_test() ->
		ActorID = { actor_type, [1,3] },
		ActorRecord = { test, ActorID, data },
		ok = add_actor( ActorRecord ),
	  Notice = {dummy, "This is only a test"},
		Result = notify( ActorID, Notice ),		
		?assertEqual( "Test notice received.", Result ).



		

