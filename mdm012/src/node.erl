%% Copyright 2011 Richard Alexander Green
%% Description: Represents a node/device in the electric distribution network.
%% - Station >> Circuit >> Transformer >> Service >> Meter >> Channel
%% Created: Jun 27, 2011
%% ------------------------------------------
-module(node).

%% ========================================================================================
%% Include files
%%
-define( NOT_IMPLEMENTED, exit( {?MODULE,'has not implemented this function'} )  ).

-define( TEST, true ).
-define( DEBUG, true ).

-include_lib("eunit/include/eunit.hrl").

-ifdef( DEBUG ).
-compile( export_all ).
-endif.
%% ========================================================================================
%% Exported Functions
%%
-export([ make/2, notify/2, notify_components/2 ]).

-record( node, { id
	             , parent_id
	             , planned_fan_out
	             , attributes = orddict:new()     % optional attributes (depends on type of node) 
	             , components = []                % set of component ID
							 }).

%% ========================================================================================
%% API Functions
%%

% Send notice to a list of actor-ID.
notify_components( Components, Notice ) ->
		%?debugVal( { notify_components, Components, Notice }),	
		% The messenger module sends notices, requests, and queries to actor instances.
		lists:foreach( fun(Component) -> messenger:send_notice_to_actor( Notice, Component ) end, Components ).

% Create an instance with default fan-out and add to parent node.
make( station    , ParentNode ) -> make( station, 100, ParentNode );   % Station has 100 circuits   ==> 100,000 services
make( circuit    , ParentNode ) -> make( circuit, 100, ParentNode );   % circuit has 100 transformers ==> 1,000 services
make( transformer, ParentNode ) -> make( transformer, 10, ParentNode); % transformer has 10 services.
make( service    , ParentNode ) -> make( service, 1, ParentNode );     % service has 1 meter.
make( meter      , ParentNode ) -> make( meter,   1, ParentNode );     % meter has 1 channel.
make( channel    , ParentNode ) -> make( channel, 0, ParentNode ).     % channel has no components.

notify( ActorRecord, Notice ) ->
		do( ActorRecord, Notice ).

%% ========================================================================================
%% Local Functions
%%


% Create an instance with given fan-out and add it to parent components.
make( NodeType, FanOut, ParentNode ) ->
		Node = create( NodeType, FanOut, ParentNode ),
		UpdatedParentNode = add_component_to_parent( Node, ParentNode ),
		UpdatedParentNode.

create( NodeType, Fanout, ParentNode ) ->
		
		% Create the node`s identifier
		#node{ id = ParentID, components = Components } = ParentNode,
		{ _ParentType, ParentPath } = ParentID,
		NComponents = length(Components),
		NodePath = ParentPath ++ [NComponents + 1], 
		NodeID = { NodeType, NodePath },
		
		% Create node`s record
		NewNodeRecord = #node{ id = NodeID, planned_fan_out = Fanout },
		
		% Add the record to the actors table
		actors:add_actor( NewNodeRecord ),
		
		% Perform any additional type-specific initialization
		case NodeType of
				meter -> meters:initialize_meter( NodeID );
				%channel -> channels:inialize_channel( NodeID );
				_ -> do_nothing
		end,
		NewNodeRecord.

add_component_to_parent( Node, ParentNode ) ->
		#node{ components = Components } = ParentNode,
		#node{ id = NodeID } = Node,
		UpdatedComponents = Components ++ [ NodeID ],
		UpdatedParentNode = ParentNode#node{ components = UpdatedComponents },
		actors:update_actor(UpdatedParentNode),
		UpdatedParentNode.

% Expand node to the planned-fan-out
expand( UpdatedNode ) when length( UpdatedNode#node.components ) >= UpdatedNode#node.planned_fan_out ->
    UpdatedNode;
expand( Node ) when length( Node#node.components ) < Node#node.planned_fan_out ->
		UpdatedNode = case Node#node.id of
				{ net, _ } -> make( station, Node );
				{ station, _ } -> make( circuit, Node );
				{ circuit, _ } -> make( transformer, Node );
				{ transformer, _ } -> make( service, Node );
				{ service, _ } -> make( meter, Node );
				{ meter, _ } -> make( channel, Node );
											
				_Other ->
						exit({"not ready to expand other types of nodes yet:", Node#node.id } )
		end,
		expand( UpdatedNode ).

% Expand a node`s tree
do( Node, cascade_expand ) ->
		ExpandedNode = expand( Node ),       % Expand to the planned fan out
		actors:update_actor(ExpandedNode),   % Updated node in database now has components
		
		% Cascade the expand action to the components (expand the tree all the way down)
		#node{ components = Components } = ExpandedNode,
		lists:foreach( fun( Component ) -> 
													messenger:send_notice_to_actor( cascade_expand, Component) end
								 , Components );

% Cascade an action (such as tick)
do( Node, { cascade, Action } ) ->
		do( Node, Action ),
		Components = Node#node.components,
		lists:foreach( fun( Component ) -> 
													messenger:send_notice_to_actor( { cascade, Action }, Component) end
								 , Components ),	
		ok;

% Respond to tick event on a service									
do( #node{ id = { service, _ } } = Node, tick ) ->   % tick is only meaningful to service nodes at this time
		ID = Node#node.id,
		%?debugVal( {tick, ID } )
		Usage = random:uniform( 4000 ),
		%DateTime = calendar:local_time(),     % TODO: SET-UP A SIMULATED CLOCK
		DateTime = now(),
		Meters = Node#node.components,
		case Meters of
				[] -> exit( {"service has no meters: ", ID } );		
		    _ -> proceed
		end,
	  [ MeterID ] = Meters,
		messenger:send_notice_to_actor( { usage, Usage, DateTime }, MeterID ),
		ok;

% Ignore a tick event on other nodes
do( #node{ id = { _, _ } }, tick ) ->   % tick is only meaningful to service nodes at this time
		%?debugVal( {ignore, tick, Node }),
		ignore;

% Respond to a usage event on a meter
do( #node{ id = { meter, _ } } = Node, { usage, Usage, DateTime } ) ->
		MeterID = Node#node.id,
		meters:record_usage( MeterID, Usage, DateTime ),
		%?debugVal( { usage, Usage, DateTime, MeterID } ),
		ok;

do( #node{ id = { channel, _ } } = Node, { usage, Usage, DateTime } ) ->
		ChannelID = Node#node.id,
		?debugVal( { usage, Usage, DateTime, ChannelID } ),
		ok;

% Respond to a price event on a meter (Price the month-to-date usage)
do( #node{ id = { meter, _ } } = Node , price ) ->  % price the MTD usage at each meter
		MeterID = Node#node.id,
		%?debugVal( {pricing, MeterID } ),
		meters:price_mtd_usage( MeterID ),
		ok;

% Other nodes ignore price event
do( #node{ id = { _, _ }  } = Node, price ) ->  % price message is just cascaded to other nodes
		%?debugVal( {ignore, price, Node }),
		ignore;
		
% Ignore dummy event -- used for testing
do( _, {dummy,_}) ->
		ignore;

% Exit if an unexpected event is sent
do( Node, Notice ) ->
		exit({Notice,"- not implemented yet for node: ", Node }).


%% ////////////////////////////////////// TESTS ///////////////////////////////////////////////////

		
setup_meter_table_test() ->
		?debugMsg(">>>> setup_meter_table_test"),
		Result = meters:initialize_table(),
		?debugVal( Result ).

create_test() ->
		ParentNode = #node{ id = {parent, [1002]} },
    NodeType = child,
	  Fanout = 1,
    NewNode = create( NodeType, Fanout, ParentNode ),
		%?debugVal( NewNode ),
		?assertEqual( {node,{child,[1002,1]},undefined,1,[],[]}, NewNode ),
		NewNode.

add_component_to_parent_test() ->
		ParentNode = #node{ id = {parent, [1003]} },
		NodeType = child,
	  Fanout = 1,
    NewNode = create( NodeType, Fanout, ParentNode ),
		UpdatedParentNode = add_component_to_parent( NewNode, ParentNode ),
		%?debugVal( UpdatedParentNode ),
		?assertEqual( {node,{parent,[1003]},undefined,undefined,[],[{child,[1003,1]}]}, UpdatedParentNode ),
		UpdatedParentNode.

make_test() -> 
		ParentNode = #node{ id = {parent, [1004]} },
		NodeType = child,
	  Fanout = 1,
    NewNode = create( NodeType, Fanout, ParentNode ),
		UpdatedParentNode = add_component_to_parent( NewNode, ParentNode ),
		UpdatedParentNode2 = make( NodeType, Fanout, UpdatedParentNode ),
		%?debugVal( UpdatedParentNode ),
		?assertEqual(  {node,{parent,[1004]},undefined,undefined,[],[{child,[1004,1]},{child,[1004,2]}]}, UpdatedParentNode2 ),
		UpdatedParentNode2.

notify_components_test() ->
		% Send notice to a list of actor-ID.
		MotherNode = actors:get_actor({parent,[1004]}),
		#node{ components = Components } = MotherNode,
		Notice = { dummy, "This is only a test" },
		notify_components( Components, Notice ).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
-define( DO_EXPAND_TESTING, true ).
-ifdef( DO_EXPAND_TESTING ).


expand_1_test() ->
		?debugMsg(">>>> expand_1_test"),
		Fanout = 1,
		Net = make( station, 1, #node{ id =  {net, [1] }, planned_fan_out = Fanout  }),
		ExpandedNet = expand( Net ),
		?assertEqual( Fanout, length( ExpandedNet#node.components )  ).

expand_2_test() ->
		?debugMsg(">>>> expand_2_test"),
		Fanout = 2,
		Net = make( station, 1, #node{ id =  {net, [2] }, planned_fan_out = Fanout  }),
		ExpandedNet = expand( Net ),
		?assertEqual( Fanout, length( ExpandedNet#node.components )  ).

expand_station_test() ->
		?debugMsg(">>>> expand_station_test"),
		Fanout = 100,
		Station = create( station, Fanout, #node{ id =  {net,[3] } } ),
		ExpandedStation = expand( Station ),
		?assertEqual( Fanout, length( ExpandedStation#node.components )  ).

expand_circuit_test() ->
		?debugMsg(">>>> expand_circuit_test"),
		Fanout = 100,
		Circuit = create( circuit, Fanout, #node{ id =  {net,[4] } } ),
		ExpandedCircuit = expand( Circuit ),
		?assertEqual( Fanout, length( ExpandedCircuit#node.components )  ).

expand_transformer_test() ->
		?debugMsg(">>>> expand_transformer_test"),
		Fanout = 10,
		Transformer = create( transformer, Fanout, #node{ id =  {net,[5] } } ),
		ExpandedTransformer = expand( Transformer ),
		?assertEqual( Fanout, length( ExpandedTransformer#node.components )  ).

expand_service_test() ->
		?debugMsg(">>>> expand_service_test"),
		Fanout = 1,
		Service = create( service, Fanout, #node{ id =  {net,[6] } } ),
		ExpandedService = expand( Service ),
		?assertEqual( Fanout, length( ExpandedService#node.components )  ).

expand_meter_test() ->
		?debugMsg(">>>> expand_meter_test"),
		Fanout = 1,
		Meter = create( meter, Fanout, #node{ id =  {net,[7] } } ),
		ExpandedMeter = expand( Meter ),
		?assertEqual( Fanout, length( ExpandedMeter#node.components )  ).

expand_channel_test() ->
		?debugMsg(">>>> expand_channel_test"),
		Fanout = 0,
		Channel = create( channel, Fanout, #node{ id =  {net,[8] } } ),
		ExpandedChannel = expand( Channel ),
		?assertEqual( Fanout, length( ExpandedChannel#node.components )  ).

-endif.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
-define( DO_CASCADE_TESTING, true ).
-ifdef( DO_CASCADE_TESTING ).


cascade_expand_test() ->
		Fanout = 2,
		Transformer = create( transformer, Fanout, #node{ id =  {net,[11] } }),
		NodeID = Transformer#node.id,
		messenger:send_notice_to_actor( cascade_expand, NodeID ).

cascade_expand_2_levels_test() ->
		?debugMsg(">>>> cascade_expand_2_levels_test"),
		Fanout = 2,
		Circuit = create( circuit, Fanout, #node{ id =  {net,[12] } }),
		NodeID = Circuit#node.id,
		messenger:send_notice_to_actor( cascade_expand, NodeID ).

cascade_expand_3_levels_test() ->
		?debugMsg(">>>> cascade_expand_3_levels_test"),
		Fanout = 2,
		Station = create( station, Fanout, #node{ id =  {net,[13] } }),
		NodeID = Station#node.id,
		messenger:send_notice_to_actor( cascade_expand, NodeID ).

cascade_expand_4_levels_test() ->
		?debugMsg(">>>> cascade_expand_4_levels_test"),
		Fanout = 1,
		Net = create( net, Fanout, #node{ id =  {net,[14] } }),
		NodeID = Net#node.id,
		messenger:send_notice_to_actor( cascade_expand, NodeID ).

-endif.

tick_test() ->
		?debugMsg(">>>> tick_test"),
		Fanout = 1,
		Service = create( service, Fanout, #node{ id =  {net,[25] } } ),
		ExpandedService = expand( Service ),
		NodeID = Service#node.id,
		messenger:send_notice_to_actor( tick, NodeID ).

tick_cascade_test() ->
		?debugMsg(">>>> tick_cascade_test"),
		Fanout = 2,
		Transformer = create( transformer, Fanout, #node{ id =  {net,[26] } }),
		NodeID = Transformer#node.id,
		messenger:send_notice_to_actor( cascade_expand, NodeID ),
		ExpandedTransformer = actors:get_actor(NodeID),
		%?debugVal( ExpandedTransformer ),
		messenger:send_notice_to_actor( { cascade, tick }, NodeID ),
		?assert( true ).

price_cascade_test() ->
		?debugMsg(">>>> price_cascade_test"),
		Fanout = 2,
		Transformer = create( transformer, Fanout, #node{ id =  {net,[27] } }),
		NodeID = Transformer#node.id,
		messenger:send_notice_to_actor( cascade_expand, NodeID ),
		messenger:send_notice_to_actor( { cascade, price }, NodeID ),
		?assert( true ).


tick_load_test( Fanout ) ->
		?debugVal({ tick_load_test, Fanout, stations }),
		ets:delete_all_objects( world ),
		meters:clear_all_meters(),
		Net = create( net, Fanout, #node{ id =  {net,[777] } }),
		NodeID = Net#node.id,
		?debugTime("cascade expand: ", messenger:send_notice_to_actor( cascade_expand, NodeID )  ),
		?debugTime("cascade tick  1: ", messenger:send_notice_to_actor( { cascade, tick }, NodeID ) ),
		?debugTime("cascade tick  2: ", messenger:send_notice_to_actor( { cascade, tick }, NodeID ) ),
		?debugTime("cascade tick  3: ", messenger:send_notice_to_actor( { cascade, tick }, NodeID ) ),
		?debugTime("cascade tick  4: ", messenger:send_notice_to_actor( { cascade, tick }, NodeID ) ),
		?debugTime("cascade tick  5: ", messenger:send_notice_to_actor( { cascade, tick }, NodeID ) ),
		?debugTime("cascade tick  6: ", messenger:send_notice_to_actor( { cascade, tick }, NodeID ) ),
		?debugTime("cascade tick  7: ", messenger:send_notice_to_actor( { cascade, tick }, NodeID ) ),
		?debugTime("cascade tick  8: ", messenger:send_notice_to_actor( { cascade, tick }, NodeID ) ),
		?debugTime("cascade tick  9: ", messenger:send_notice_to_actor( { cascade, tick }, NodeID ) ),
		?debugTime("cascade tick 10: ", messenger:send_notice_to_actor( { cascade, tick }, NodeID ) ),
		?debugTime("cascade tick 11: ", messenger:send_notice_to_actor( { cascade, tick }, NodeID ) ),
		?debugTime("cascade tick 12: ", messenger:send_notice_to_actor( { cascade, tick }, NodeID ) ),
		?debugTime("cascade tick 13: ", messenger:send_notice_to_actor( { cascade, tick }, NodeID ) ),
		?debugTime("cascade tick 14: ", messenger:send_notice_to_actor( { cascade, tick }, NodeID ) ),
		?debugTime("cascade tick 15: ", messenger:send_notice_to_actor( { cascade, tick }, NodeID ) ),
		?debugTime("cascade tick 16: ", messenger:send_notice_to_actor( { cascade, tick }, NodeID ) ),
		?debugTime("cascade tick 17: ", messenger:send_notice_to_actor( { cascade, tick }, NodeID ) ),
		?debugTime("cascade tick 18: ", messenger:send_notice_to_actor( { cascade, tick }, NodeID ) ),
		?debugTime("cascade tick 19: ", messenger:send_notice_to_actor( { cascade, tick }, NodeID ) ),
		?debugTime("cascade tick 20: ", messenger:send_notice_to_actor( { cascade, tick }, NodeID ) ),
		?debugTime("cascade tick 21: ", messenger:send_notice_to_actor( { cascade, tick }, NodeID ) ),
		?debugTime("cascade tick 22: ", messenger:send_notice_to_actor( { cascade, tick }, NodeID ) ),
		?debugTime("cascade tick 23: ", messenger:send_notice_to_actor( { cascade, tick }, NodeID ) ),
		?debugTime("cascade tick 24: ", messenger:send_notice_to_actor( { cascade, tick }, NodeID ) ),
		
		?debugTime("price day's usage", messenger:send_notice_to_actor( { cascade, price }, NodeID ) ),
		
		meters:close(),
		
		?debugTime( "market_price intervals", channels:market_price( 1, dummyPriceSchedule ) ),

		
		ok.

		
