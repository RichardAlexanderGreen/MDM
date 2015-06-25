%%% -------------------------------------------------------------------
%%% Author  : admin
%%% Description :
%%%
%%% Created : Feb 9, 2011
%%% -------------------------------------------------------------------
-module(messenger_root_sup).

-behaviour(supervisor).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define( NOTEST, true ).
-define( NODEBUG, true ).

-include_lib("eunit/include/eunit.hrl").


%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([
	 init/1
        ]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(SERVER, ?MODULE).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================

start_supervisor( Arguments ) ->
		?debugMsg( start_supervisor ),
		
		supervisor:start_link(                        % Start the supervisor instance.
                           %{ local, ?SERVER },   % Give it a local name.
													 ?MODULE, Arguments     % The module containing and arguments for init/1.
												   ).  

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init( Arguments ) ->
		?debugVal( {init, Arguments} ),

		% Assemble specifications for child 1
    ID1 = messenger,                                  % 1 Identify this child
		Startup1 = { ID1, start_server, Arguments },      % 2 { Module, Function, Arguments } to start process
	  RestartSpec = permanent,                          % 3 permanent | temporary | transient
		ShutDownMilliseconds = 8000,                      % 4 # milliseconds allowed for this child to shutdown
		TypeOfProcess = worker,                           % 5 type of child = supervisor | worker
		DependsOn = [  ],                              % 6 modules this child depends on (used for upgrades)
		ChildSpec1 = { ID1, Startup1, RestartSpec, ShutDownMilliseconds, TypeOfProcess, DependsOn },

		% Assemble specifications for child 2
    ID2 = messenger_event_mgr,                                  % 1 Identify this child
		Startup2 = { ID2, start_event_mgr, Arguments },   % 2 { Module, Function, Arguments } to start process
	  RestartSpec = permanent,                          % 3 permanent | temporary | transient
		ShutDownMilliseconds = 8000,                      % 4 # milliseconds allowed for this child to shutdown
		TypeOfProcess = worker,                           % 5 type of child = supervisor | worker
		DependsOn = [ ],                                 % 6 modules this child depends on (used for upgrades)
		ChildSpec2 = { ID2, Startup2, RestartSpec, ShutDownMilliseconds, TypeOfProcess, DependsOn },

		Children = [ ChildSpec1, ChildSpec2 ],    % List of child specifications
	  RestartStrategy = { one_for_one, 0, 1 },          % one_for_one | one_for_all, max restarts in T seconds
		{ ok, { RestartStrategy, Children }  }.           % Return supervisor specification.



%% ====================================================================
%% Internal functions
%% ====================================================================

