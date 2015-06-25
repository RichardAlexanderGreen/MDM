%% Copyright 2011 Richard Alexander Green
%% Description: This version of the mdm application simulates a real-time system 
%%   with active components distributed in the field.
%% Each sink will:
%% - aggregate usage
%% - keep own interval history
%% - check for over-load
%% - forward usage increment to upstream aggregator
%% A simulator sends tick events to sinks.
%% A service (sink) will generate randomized usage on each tick and initiate forwarding.
%% Some components will generate random system glitches to test robustness.

%% Created: Jun 3, 2011
%% ------------------------------------------
-module(mdm7_app).

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

