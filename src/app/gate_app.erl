%% Author: Eric.yutao
%% Created: 2013-12-7
%% Description: TODO: Add description to gate_app
-module(gate_app).

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
    case app_util:get_argument("-line") of
        [] -> slogger:msg("Error in Gate app ~p~n", [?MODULE]);
        [_Center|Rest] ->
            debug:info("************** Gate app ~p~n", [""]),
            debug:log_file("../log/gate.log"),
            debug:error("Test for log file~n"),
            ping_center:wait_all_nodes_connect(true),
            %% MySQL need be treated as application
            erlmysql_app:start(),
			case boot_listener_sup() of
				{ok, _ListenerSupPid} ->
					ListennerState = true;
				{error, _Error} ->
					ListennerState = false
			end,
			boot_session_manager_sup(),
			if
				not ListennerState ->
					server_control_op:stop_server();
				true ->
					{ok, self()}
			end
    end.

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

boot_listener_sup() ->
	SName = node_util:get_match_snode(gate, node()),
	Port = env:get2(gateport, SName, 0),
	case Port of
		0 -> slogger:msg("Error gate port ~~~~~~~~~~~~~~");
		Port ->
			AcceptorCount = env:get2(gate, acceptor_count, 1),
			OnStartup = {?MODULE, tcp_litener_started, []},
			OnShutdown = {?MODULE, tcp_litener_stopped, []},
			AcceptCallback = {?MODULE, start_client, []},
			case tcp_listener_sup:start_link(Port, OnStartup, OnShutdown, AcceptCallback, AcceptorCount) of
				{ok, Pid} ->
					{ok, Pid};
				Error ->
					slogger:msg("tcp_listener_sup start error ~p~n", [Error]),
					{error, Error}
			end
	end.

