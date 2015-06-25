%%% Copyright 2011 Richard Alexander Green
%%% Description : Simulates a circuit.
%%% Each circuit: 
%%% Events:
%%% - Acts as an event-manager for its transformers / services.
%%% - On {install - - }: Install transformers. Transformers will install services.
%%% - On {tick - -}: Forward the tick to transformers. Transformers will forward to services.
%%% - On {tick - -}: When date turns over, send day`s load history to logger.
%%% Messages:
%%% - {PoD_usage - - - } : Sum usage into local load history.
%%%
%%% Created : Jun 13, 2011
%%% -------------------------------------------------------------------
-module(circuit).

-behaviour(gen_event).

-define( EVENT_MANAGER, self() ).

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
%% External exports
-export([]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

% This is a helper function -- State must match circuit / transformer / service.
% Use the same -record( state, - - - ) in service / transformer / circuit and load_history modules.
-include("../include/load_point_state.hrl").

%% ====================================================================
%% External functions
%% ====================================================================


%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          Other
%% --------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%% --------------------------------------------------------------------
%% Func: handle_event/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler
%% --------------------------------------------------------------------

% Simulate installation
handle_event( { install, NServices, NServicesPerTransformer }, State) ->
		% Create transformers as event-handlers where I am event-manager.
		TransformerSequence = NServices div NServicesPerTransformer,		
		create_transformers( TransformerSequence, NServicesPerTransformer, State),
		NewState = State#state{},
		NewState;
	
% Simulate clock tick
% - On {tick - -}: Forward the tick to services / transformers.
% - On {tick - -}: When date turns over, send day`s load history to logger.
handle_event( { tick, CurrentDateTime }, State) ->
		% Forward the tick to transformers.
		% (Transformers will forward to their services.)
		gen_event:notify( ?EVENT_MANAGER, { tick, CurrentDateTime } ),
		% When date turns over, send day`s load history to logger.
		NewState = load_history:tick( { tick, CurrentDateTime }, State ),
		NewState;

handle_event(Event, State) ->
    {ok, State}.

%% --------------------------------------------------------------------
%% Func: handle_call/2
%% Returns: {ok, Reply, State}                                |
%%          {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%          {remove_handler, Reply}
%% --------------------------------------------------------------------
handle_call(Request, State) ->
		exit( "call not expected"),
    Reply = ok,
    {ok, Reply, State}.

%% --------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler
%% --------------------------------------------------------------------

% Messages:
% - {PoD_usage - - - } : Sum usage into local load history.
handle_info( {usage, PoDID, DateTime, Usage }, State ) ->
		{ Date, Time } = DateTime,
		History = State#state.history,
		{ Sum, NewHistory } = load_history:sum_usage_for_interval( Usage, Date, Time, History ),
		NewState = State#state{ history = NewHistory },
		NewState;

handle_info(Info, State) ->
		exit( {"circuit does not understand: ", Info } ),
    {ok, State}.

%% --------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any
%% --------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

% TODO: Create transformers as event-handlers where I am event-manager.
create_transformers( 0, _NServicesPerTransformer, _State ) ->
		done;

create_transformers( TransformerSequence, NServicesPerTransformer, State ) ->
		create_one_transformer( TransformerSequence, NServicesPerTransformer, State ),
		create_transformers( TransformerSequence - 1, NServicesPerTransformer, State ).

create_one_transformer( TransformerSequence, NServicesPerTransformer, State ) ->
		% Create the service ID by appending Sequence to transformer`s ID.
		CircuitID = State#state.id,
		TransformerID = CircuitID ++ [ TransformerSequence ],
		Handler = { service, TransformerID },
		Args =[ { id, TransformerID }, { services_per_transformer, NServicesPerTransformer }, {circuit = CircuitID } ],
		gen_event:add_sup_handler( self(), Handler, Args ).

% TODO: Figure out the protocol for installing services. ****





