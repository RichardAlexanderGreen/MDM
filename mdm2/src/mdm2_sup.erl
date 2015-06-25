%%% @doc This is the root supervisor module of the MDM application.
%%%      The mdm_server module contains a detailed description of the application.
%%% @end
%%% @Author: RichardAlexanderGreen@gmail.com
%%% @copyright 2010 Richard Alexander Green

-module( mdm2_sup ).

-behaviour(supervisor).
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
%% API exports
%% --------------------------------------------------------------------
-export( [ start_supervisor/1 ] ).

%% --------------------------------------------------------------------
%% Supervisor callbacks exports
%% --------------------------------------------------------------------
-export( [ init/1 ] ).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define( SERVER, ?MODULE ).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================



%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: start_link/0
%% -- Calls supervisor:start_link/3 --
%%% start_link( Name, Mod, Args ) where:
%%%    Name ::= {local, atom()} | {global, atom()}
%%%    Mod  ::= atom(), callback module implementing the 'real' server
%%%    Args ::= term(), init arguments (to Mod:init/1)
%%% Returns: {ok, Pid} |
%%%          {error, {already_started, Pid}} |
%%%          {error, Reason}
%% --------------------------------------------------------------------
start_supervisor( StartArgs ) ->
		?debugVal( { start_supervisor, StartArgs } ),
		
		supervisor:start_link(                       % Start the supervisor instance.
                           { local, ?SERVER },   % Give it a local name.
													 ?MODULE, []           % The module containing and arguments for init/1.
												   ).  
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: { ok,  {SupFlags,  [ChildSpec] } } 
%%          | ignore                             
%%          | { error, Reason }
%% --------------------------------------------------------------------
init([]) ->
		?debugMsg( init ),

		% Assemble specifications for child 1
    ID1 = asset_mgr,                                  % 1 Identify this child
		Startup1 = { ID1, start_server, [] },             % 2 { Module, Function, Arguments } to start process
	  RestartSpec = permanent,                          % 3 permanent | temporary | transient
		ShutDownMilliseconds = 2000,                      % 4 milliseconds allowed for this child to shutdown
		TypeOfProcess = worker,                           % 5 type of child = supervisor | worker
		DependsOn = [  ],                                 % 6 modules this child depends on (used for upgrades)
		ChildSpec1 = { ID1, Startup1, RestartSpec, ShutDownMilliseconds, TypeOfProcess, DependsOn },
		
				
		Children = [ ChildSpec1  ],    % List of child specifications
	  RestartStrategy = { one_for_one, 0, 1 },          % one_for_one | one_for_all, max restarts in T seconds
		{ ok, { RestartStrategy, Children }  }.           % Return supervisor specification.

%% ====================================================================
%% Internal functions
%% ====================================================================

