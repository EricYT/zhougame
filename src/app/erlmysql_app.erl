%% Author: Eric.yutao
%% Created: 2013-12-7
%% Description: TODO: Add description to gate_app
-module(erlmysql_app).

-behaviour(application).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Behavioural exports
%% --------------------------------------------------------------------
-export([
	 start/2,
     start/0,
	 stop/1
        ]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------


%% ====================================================================!
%% External functions
%% ====================================================================!
%% --------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%% --------------------------------------------------------------------
start(Type, StartArgs) ->
    debug:info("************** erlmysql_app app ~p~n", [""]),
    debug:log_file("../log/line.log"),
    debug:error("Test for log file~n"),
    ping_center:wait_all_nodes_connect(true),
    %% MySQL need be treated as application
    {ok, Pid} = erlmysql_sup:start_link(),
    AppRunNode = mysql_util:get_app_run_node(),
    case node_util:check_run_node(AppRunNode) of
        true ->
            start_mysql_all();
        false ->
            nothing
    end,
    {ok, Pid}.

start() ->
    applicationex:start(?MODULE).

%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop(State) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================
start_mysql_all() ->
    AllServers = mysql_name_server:get_all_servers(),
    start_mysql(AllServers, []).

start_mysql([ServerName|Tail], AccPid) ->
    {ok, Pid} = erlmysql_sup:start_mysql(list_to_atom(ServerName)),
    start_mysql(Tail, [Pid|AccPid]);
start_mysql([], AccPid) ->
    AccPid.
        




