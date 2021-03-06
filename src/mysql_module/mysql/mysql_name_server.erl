%%% -------------------------------------------------------------------
%%% Author  : Eric.yutao
%%% Description :
%%%
%%% Created : 2013-12-31
%%% -------------------------------------------------------------------
-module(mysql_name_server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(DEFAULT_NAME, "_mysql_").

-define(SERVER_NAME, server_name).              %% Server Name


%% --------------------------------------------------------------------
%% External exports
-export([
		 start_link/0,
         create_name/0,
         get_client/0,
         get_all_servers/0
		 ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {server_names = [], server_size, server_last}).

%% ====================================================================
%% External functions
%% ====================================================================
-spec start_link() -> ignore.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ====================================================================
%% Server functions
%% ====================================================================
get_client() ->
    gen_server:call(?MODULE, {get_name}).

get_all_servers() ->
    gen_server:call(?MODULE, {get_all_servers}).


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
    Servers = create_name(),
    Size = erlang:length(Servers),
    io:format(">>>>>>>>>>>>>> ~p~n", [nodes()]),
    {ok, #state{server_names	= Servers,
                server_size		= Size,
                server_last		= 1}}.

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
handle_call({get_name}, _From, State) ->
    {{ServerName, Node}, NewState} = get_client(State),
    {reply, {list_to_atom(ServerName), Node}, NewState};
handle_call({get_all_servers}, _From, State) ->
    Reply = State#state.server_names,
    {reply, Reply, State};
handle_call({stop}, _From, State) ->
    {stop, shutdown, stopped, State};
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
handle_info(Info, State) ->
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
%%
%%@doc 
%% -spec get_client(record()) -> {string(), record()}.
get_client(#state{server_size = Size, server_names = Servers, server_last = Last}=State) when Last =< Size ->
    {lists:nth(Last, Servers), State#state{server_last=Last+1}};
get_client(#state{server_names = Servers}=State) ->
    {lists:nth(1, Servers), State#state{server_last=2}}.


%%@doc 读取配置的client size，运行mysql的节点，判断当前节点是否为运行mysql的node，将node名字存入#state中
-spec create_name() -> [string(), ...].
create_name() ->
    RClientSize = mysql_util:get_client_size(),
    AppRunNode = mysql_util:get_app_run_node(),
    case node_util:check_run_node(AppRunNode) of
        true ->
            NodeNames = [{node_util:get_node_sname(node()), node()}];
        false ->
            NodeNames = [{node_util:get_node_sname(Node), Node}||Node<-node_util:get_all_nodes_by_appnodes([db])]
    end,
    [{lists:concat([NodeS, ?DEFAULT_NAME, integer_to_list(Client)]), NodeN}
    ||Client<-lists:seq(1, RClientSize), {NodeS, NodeN}<-NodeNames].

