%% Author: admin
%% Created: Feb 19, 2011
%% Description: TODO: Add description to messenger_app
-module(messenger_app).

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

-define( EVENT_MANAGER, { global, ?MODULE } ).

-include( "records.hrl" ).

-record( state, {} ).


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

start( _Type, StartArgs ) ->
	?debugMsg( start ),
  case messenger_sup:start_supervisor( StartArgs ) of    % Start root supervisor.
	  {ok, Pid} ->
	    {ok, Pid};   % Return the root supervisor's PID.
  Error ->
	    { start_supervisor_not_okay, Error }
  end.


%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop(State) ->
		?debugVal( {'stop(State)', State }),
  
    ok.


prep_stop(State) -> 
	?debugVal( {'prep_stop(State)', State }),
  messenger:stop(),
		NewState = State.


%% ====================================================================
%% Internal functions
%% ====================================================================

