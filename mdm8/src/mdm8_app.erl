%%% Copyright 2011 Richard Alexander Green
%%% Description: This version makes extensive use of the gen_event design patterns.
%%% Actors (Roles and Responsibilities):
%%% - Installer:
%%% - - Generate installation events for service / transformer / circuit.
%%% - Clock:
%%% - - Generate tick event.
%%% - Service:
%%% - - Handle tick: 
%%% - - - Send service_usage event to transformer.
%%% - - - Insert into service (PoD) usage history.
%%% - - - Periodically (when date changes) write usage history to persistent store. 
%%% - - - (This is an attempt to reduce the overhead associated with persistence.)
%%% - Transformer:
%%% - - Handle service_usage message: Add into transformer (PoD) load history. Check for over-load.
%%% - - Handle tick event: 
%%% - - - Send usage-sum resulting from prior period slot to the circuit.
%%% - - - Periodically (when date changes) write load history to persistent store. 
%%% - Circuit:
%%% - - Handle usage-sum message: Add into circuit (PoD) load history. Check for overload.
%%% - - Handle tick: 
%%% - - - Send usage-sum from prior slot to SystemMonitor.
%%% - - - Periodically (when date changes) write load history to persistent store. 
%%% - SystemMonitor:
%%% - - Handle usage-sum message: Add into system (PoD) load history. Check for overload? 
%%% - - Handle overload event: Just log for now. (Send to dashboard TBD ??)
%%% - - Handle tick: 
%%% - - - Periodically (when date changes) write load history to persistent store. 
%%% Middleware:
%%% - This version does not attempt to distribute processes across multiple nodes.
%%% - An actor has a PID on the local node.
%%% - An actor sends a message to another actor using pattern: actors:pid( ActorID ) ! Message.
%%% - The actors module maintains a mapping from ActorID to PID.
%%% - Actors are created by the simulator (in this case).
%%% - Actors self-report their PID to the actors module.  -- actors:register( ActorID, ProcessID ).
%%% - Messages can be broadcast using actors:broadcast( Message ).
%%% - Actors may register their attributes -- actors:attributes( ActorID, [ {Attribute,Value} ] ).
%%% - Messages can be broadcast to a group of actors according to their attributes -- actors:broadcast( Message, [{A,V}]).
%%% Created: Jun 8, 2011
%%% ------------------------------------------
-module(mdm8_app).

-behaviour(application).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

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

