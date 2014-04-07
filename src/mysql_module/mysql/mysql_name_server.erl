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
-define(DEFAULT_READ_NAME, "mysql_read_").
-define(DEFAULT_WRITE_NAME, "mysql_write_").
-define(DEFAULT_LOG_NAME, "mysql_log_").

-define(SERVER_NAME, server_name).              %% Server Name


%% --------------------------------------------------------------------
%% External exports
-export([
		 start_link/1,
         create_name/1,
         get_client/0,
         get_all_clients/0
		 ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {server_names = [], server_node, server_size, server_last, server_index}).

%% ====================================================================
%% External functions
%% ====================================================================
-spec start_link(Type::atom()) -> ignore.
start_link(Type) ->
    ServerName = erlang:list_to_atom(erlang:atom_to_list(?MODULE)++"_"++erlang:atom_to_list(Type)),
    put(?SERVER_NAME, ServerName),
	gen_server:start_link({local, ServerName}, ?MODULE, [Type], []).

%% ====================================================================
%% Server functions
%% ====================================================================
get_client() ->
    ServerName = get(?SERVER_NAME),
    gen_server:call(ServerName, {get_name}).

get_all_clients() ->
    ServerName = get(?SERVER_NAME),
    gen_server:call(ServerName, {get_all_servers}).


%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([Type]) ->
	process_flag(trap_exit, true),
	Servers = create_name(Type),
	Size = erlang:length(Servers),
	ServerNode = node_util:get_node_sname(node()),
    {ok, #state{server_names	= Servers,
				server_node		= ServerNode,
				server_size		= Size,
				server_last		= 1,
                server_index    = 1}}.

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
    {Reply, NewState} = get_client(State),
    {reply, Reply, NewState};
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
-spec get_client(record()) -> {string(), record()}.
get_client(#state{server_size = Size, server_names = Servers, server_last = Last}=State) when Last =< Size ->
    {list_to_atom(lists:nth(Last, Servers)), State#state{server_last=Last+1}};
get_client(#state{server_names = Servers}=State) ->
    {list_to_atom(lists:nth(1, Servers)), State#state{server_last=2}}.


%%@doc 读取配置的client size，运行mysql的节点，判断当前节点是否为运行mysql的node，将node名字存入#state中
-spec create_name(Type::atom()) -> [string(), ...].
create_name(read) ->
    RClientSize = mysql_util:get_read_client_size(),
    AppRunNode = mysql_util:get_app_run_node(),
    [?DEFAULT_READ_NAME++integer_to_list(Client)||Client<-lists:seq(1, RClientSize)];
create_name(write) ->
    WClientSize = mysql_util:get_write_client_size(),
    AppRunNode = mysql_util:get_app_run_node(),
    [?DEFAULT_WRITE_NAME++integer_to_list(Client)||Client<-lists:seq(1, WClientSize)];
create_name(log) ->
    LClientSize = mysql_util:get_log_client_size(),
    AppRunNode = mysql_util:get_app_run_node(),
    [?DEFAULT_LOG_NAME++integer_to_list(Client)||Client<-lists:seq(1, LClientSize)].
