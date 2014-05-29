%%% -------------------------------------------------------------------
%%% Author  : cb1224
%%% Description :
%%%
%%% Created : 2014-4-14
%%% -------------------------------------------------------------------
-module(player_session_processor).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("network_define.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {socket}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link([]) ->
	%% Don't register server name, because so many process will be created when socket created.
	gen_server:start_link(?MODULE, [], []).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
	process_flag(trap_exit, true),
	io:format(">>>>>>>>>>>>>>> A processor be created ~p~n", [{?MODULE, ?LINE}]),
    {ok, #state{}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({scoket_ready, Sock}, State) ->
%% 	Res = gen_tcp:recv(Sock, 4, 180000),
%% 	io:format(">>>>>>>>>>>>>>>> ~p~n", [{?MODULE, ?LINE, Sock, Res}]),
%% 	inet:setopts(Sock, ?TCP_CLIENT_SOCKET_OPTIONS),
	inet:setopts(Sock, [{active, once}, {packet, 2}]),
	{noreply, State#state{socket = Sock}};
handle_info({tcp, Port, BinData}, #state{socket = Socket}=State) ->
	io:format(">>>>>>>>>>>>>>>> ~p~n", [{?MODULE, ?LINE, binary_to_term(BinData)}]),
	socket_callback:on_receive_client_packet(self(), BinData, self()),
    %% Make the packet can be unpacket
	inet:setopts(Socket, [{active, once}, {packet, 2}]),
    {noreply, State};

handle_info({send_data, Data}, #state{socket = Socket}=State) ->
    io:format(">>>>>>>>>>>>>>>> ~p~n", [{?MODULE, ?LINE, Data}]),
    BinData = erlang:term_to_binary(Data),
	erlang:port_command(Socket, BinData, [force]),
%%     gen_tcp:send(Socket, BinData),
    {noreply, State};

handle_info({tcp_closed, Port}, #state{socket = Socket}=State) ->
    io:format("Tcp closed Socket:~p Port:~p~n", [Socket, Port]),
    gen_tcp:close(Socket),
    {stop, normal, State};

handle_info(Info, #state{socket = Socket}=State) ->
	io:format(">>>>>>>>>>>>>>>> ~p~n", [{?MODULE, ?LINE, Info}]),
	inet:setopts(Socket, [{active, once}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
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

