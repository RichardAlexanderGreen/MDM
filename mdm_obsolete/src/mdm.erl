%% Author: admin
%% Created: Dec 3, 2010
%% Description: TODO: Add description to mdm
-module(mdm).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/0]).

%%
%% API Functions
%%

% Start the receive loop and register me.
start() ->
		register( mdm, spawn( mdm, server, [ {dashboard} ] ) ).
	
server( Dashboard ) ->
		receive
				{dashboard} ->
					 WhenStarted = { date(), time() },
					 NCalls = 0,
					 server( {dashboard, WhenStarted, NCalls });
				
					
				Other ->
						io:format("Not expecting: ~n~p~n", Other )
		end.

						




%%
%% Local Functions
%%

