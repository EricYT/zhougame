%%% -------------------------------------------------------------------
%%% Author  : Administrator
%%% Description :
%%%
%%% Created : 2014-4-14
%%% -------------------------------------------------------------------
-module(client_test).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/1, send_data/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {client_id, socket}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link([ClientId]) when is_list(ClientId) ->
    start_link([list_to_atom(ClientId)]);
start_link([ClientId]) when is_atom(ClientId) ->
    gen_server:start_link({local, ClientId}, ?MODULE, [ClientId], []).


-spec send_data(ClientId, Msg) -> 'ok' | {error, Error} when
                                                          ClientId :: integer(),
                                                          Msg :: term(),
                                                          Error :: atom().
send_data(ClientId, Msg) when is_atom(ClientId) ->
    erlang:send(ClientId, {send_data, Msg}).

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
init([ClientId]) ->
    {ok, Socket} = gen_tcp:connect("127.0.0.1", 8089, [{packet, 2}, binary]),
    io:format(">>>>>>>>>>>> ~p~n", [{?MODULE, ?LINE, Socket}]),
    {ok, #state{client_id = ClientId, socket = Socket}}.

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
handle_info({tcp, Port, BinData}, State) ->
	io:format("<<<<<<<<<<<<< handle data from server:~p~n", [{?MODULE, ?LINE, binary_to_term(BinData)}]),
    {noreply, State};

handle_info({send_data, Data}, #state{socket = Socket}=State) ->
    io:format(">>>>>>>>>>>>>>>> send data to server:~p~n", [{?MODULE, ?LINE, Data}]),
    BinData = erlang:term_to_binary(Data),
	erlang:port_command(Socket, BinData, [force]),
    {noreply, State};

handle_info({inet_reply, _Socket, _Res}, State) ->
    {noreply, State};

handle_info(Info, State) ->
	io:format(">>>>>>>>>>>>>> handle data from server:~p~n", [{?MODULE, ?LINE, Info}]),
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
