%%% -------------------------------------------------------------------
%%% Author  : admin
%%% Description :
%%%
%%% Created : Feb 10, 2011
%%% -------------------------------------------------------------------
-module(messenger_event_handler_sup).

-behaviour(supervisor).

-export([start_link/0,
         start_child/2
        ]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child( Value, LeaseTime ) ->
    supervisor:start_child(?SERVER, [Value, LeaseTime]).    % Add the child to supervision tree

init([]) ->
    Handler = { messenger_subscriber, { messenger_subscriber, start_link, []},
               temporary, brutal_kill, worker, [ messenger_subscriber ] },
    Children = [Handler],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.
