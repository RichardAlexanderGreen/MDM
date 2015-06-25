%%% Copyright Richard Alexander Green 2011
%%% Description : Messenger -- provides a store and forward messenging service, but no logging.
%%% This is a lighter-weight version of messenger with no logging.
%%% Created : June 12, 2011
%%% Functions:
%%% . Actor online --€“ Remember actor's address.
%%% . Actor offline --€“ Please queue requests.
%%% . Record message in the log.  (Internal function)
%%% . Transmit message to actor.
%%% . Transmit messages that were held for actor.
%%% API: global:send( hum_messenger, Message ) -- 
%%% 	Message = { actor_online, Actor_Name, Actor_URL }     %%% Actor online --€“ Remember actor's PID
%%% 	Message = { actor_offline, Actor_Name }               %%% Actor offline --€“ Please queue requests. [*]
%%% 	Message = { send_message, Message, To_Actor }					%%% Transmit message to actor.
%%% 	Message = { send_held_messages, To_Actor } 						%%% Transmit messages that were held for actor. [*]
%%%   [*] -- These features will not be implemented in version 0.1.
%% -------------------------------------------------------------------
-module(messenger).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define( SERVER, { global, ?MODULE } ).

%% --------------------------------------------------------------------
%% External exports
-export([]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, { actor_addresses = [] }).

%% ====================================================================
%% External functions
%% ====================================================================


%% ====================================================================
%% Server functions
%% ====================================================================

% --------------------------------------------------------------------
% Function: init/1
% Description: Initiates the server
% Returns: {ok, State}          |
%          {ok, State, Timeout} |
%          ignore               |
%          {stop, Reason}
% --------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

% --------------------------------------------------------------------
% Function: handle_call/3
% Description: Handling call messages
% Returns: {reply, Reply, State}          |
%          {reply, Reply, State, Timeout} |
%          {noreply, State}               |
%          {noreply, State, Timeout}      |
%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%          {stop, Reason, State}            (terminate/2 is called)
% --------------------------------------------------------------------
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

% --------------------------------------------------------------------
% Function: handle_cast/2
% Description: Handling cast messages
% Returns: {noreply, State}          |
%          {noreply, State, Timeout} |
%          {stop, Reason, State}            (terminate/2 is called)
% --------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

% --------------------------------------------------------------------
% Function: handle_info/2
% Description: Handling all non call/cast messages
% Returns: {noreply, State}          |
%          {noreply, State, Timeout} |
%          {stop, Reason, State}            (terminate/2 is called)
% --------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

% --------------------------------------------------------------------
% Function: terminate/2
% Description: Shutdown the server
% Returns: any (ignored by gen_server)
% --------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

% --------------------------------------------------------------------
% Func: code_change/3
% Purpose: Convert process state when code is changed
% Returns: {ok, NewState}
% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

% API: global:send( hum_messenger, Message ) -- 
% 	Message = { actor_online, Actor_Name, Actor_URL }     % Actor online --€“ Remember actor's PID
% 	Message = { actor_offline, Actor_Name }               % Actor offline --€“ Please queue requests. [*]
% 	Message = { send_message, Message, To_Actor }					% Transmit message to actor.
% 	Message = { send_held_messages, To_Actor } 						% Transmit messages that were held for actor. [*]
% [*] -- These features will not be implemented in version 0.1.

do( { actor_online, Actor_Name, Actor_URL }, State ) ->
		% Remember the actor's address.
		Addresses = State#state.actor_addresses,
		UpdatedAddresses = orddict:append(Key = Actor_Name, Value = Actor_URL, Orddict1 = Addresses ),
		NewState = State#state{ actor_addresses = UpdatedAddresses },
		{ok, NewState };
do( { actor_offline, Actor_Name }, State ) ->
		error_logger:error_report(not_implemented, {'do( { actor_offline, Actor_Name }, State )' } ),
		{ok, State };
do( { send_message, Message, To_Actor }, State ) ->
		% Send the message to the named actor.
		Addresses = State#state.actor_addresses,
		Actor_Address = orddict:fetch( To_Actor,  Addresses ),
		% TODO: SEND
		send_message_to_address( Message, Actor_Address ),
		% TODO: LOG
		{ok, State };
do( { send_held_messages, To_Actor }, State ) ->
		{ok, State };
do( UnknownMessage, State ) ->
		{ok, State }.

% If the address is a PID - we can simply send. 		
send_message_to_address( Message, Actor_Address )   when is_pid( Actor_Address ) ->
		      Actor_Address ! Message,
					ok;
% If the address is anything else - we have not implemented yet
send_message_to_address( _Message, Actor_Address )  when is_port( Actor_Address ) ->
					% TODO: SEND VIA URL.
					error_logger:error_report(not_implemented, {'send_message_to_address( Message, Actor_Address ) when is_URL( Actor_Address )'}),
		 ok.

send_message_to_actor( Message, ActorID ) ->
		gen_server:cast( ?SERVER, { send_message, Message, ActorID } ).


		


