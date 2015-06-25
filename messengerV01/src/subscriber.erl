% Copyright Richard Alexander Green
% Description : This module is the local proxy for a subscriber.
% - A subscriber receives notices, it logs the serial number.
% - If the subscriber is off-line while notices are published, 
% : : it can request a replay from the publisher.
% -------------------------------------------------------------------
-module(subscriber).

-behaviour(gen_event).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record(state, { client, last_serial }).

%% ====================================================================
%% External functions
%% ====================================================================
start_subscription( ForClient ) ->
		% Get the last serial number the client processed.
		Last_Serial = 0,  % TODO - Get serial number for restart
		% Tell the event manager that we are subscriber (event handler)
		Args = [{client, ForClient}, {last_serial, Last_Serial}],
		Result = gen_event:add_sup_handler( {global, publisher }, { ?MODULE, ForClient }, Args ),
		% Use add_sup_handler/3 so that event-handler terminate will be called if Client terminates.
		Result.

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          Other
%% --------------------------------------------------------------------
init( Args ) ->
		% Get the name of my client
		[{client, ForClient}, { last_serial, Last_Serial }, { next_serial = Next_Serial }] = Args,
		% TODO -- DO SOMETHING IF THERE IS A GAP IN THE SERIAL NUMBER
		% The client might care / might not
		% If the client cares, we need to ask the publisher for a replay.
		
    if ( Next_Serial > Last_Serial ) ->
					 error_logger:error_report( oh_my
																		, {'There is a gap in the serial numbers. Client: '
																			, ForClient
																			, ' has missed some notices' } 
																		)
		        % TODO - Figure out how to run a replay of missed notices
			 end,
    InitialState = #state{ client = ForClient
							           , last_serial = Next_Serial    % Ignore gap in serial number for now! *** DANGER ***
							           },
		{ ok, InitialState }.

%% --------------------------------------------------------------------
%% Func: handle_event/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler
%% --------------------------------------------------------------------
handle_event( NoticeMessage, State ) ->
		{ notice, SerialNumber, Notice } = NoticeMessage,
		% I am proxy for the client
		Client = State#state.client,
		% Pass the Notice to the client
		gen_server:cast( Client, Notice ),
		% Log the serial number
		log_serial_number( SerialNumber ),
		NewState = State#state{ last_serial = SerialNumber },
    { ok, NewState }.

log_serial_number( SerialNumber ) ->
		% TODO - Write SerialNumber to local log.
		ok.

%% --------------------------------------------------------------------
%% Func: handle_call/2
%% Returns: {ok, Reply, State}                                |
%%          {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%          {remove_handler, Reply}
%% --------------------------------------------------------------------
handle_call( Request, State ) ->
		error_logger:error_report( logic_error, { ?MODULE, 'does not expect handle_call request', Request } ),
    Reply = ok,
    {ok, Reply, State}.

%% --------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler
%% --------------------------------------------------------------------
handle_info( Info, State ) ->
		error_logger:error_report( logic_error, { ?MODULE, 'does not expect handle_info message', Info } ),
    {ok, State}.

%% --------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any
%% --------------------------------------------------------------------
terminate( Reason, State ) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change( OldVsn, State, Extra ) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

