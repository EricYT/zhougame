%%% -------------------------------------------------------------------
%%% Author  : eric
%%% Description :
%%%
%%% Created : 2013-11-29
%%% -------------------------------------------------------------------
-module(applicationex).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("base.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([]).
-compile(export_all).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).


%% ====================================================================
%% External functions
%% ====================================================================
start(Application) ->
    force_start(),
    Cookie = env:get(cookie, ?ERLNULL),
    erlang:set_cookie(node(), Cookie),
    debug:info("********* applicationex Application ~p~n", [Application]),
    application:start(Application),
    ok.

start(Application, Type) ->
	force_start(),
	Cookie = env:get(cookie, ?ERLNULL),
	erlang:set_cookie(node(), Cookie),
	application:start(Application, Type).

%% ====================================================================
%% Server functions
%% ====================================================================
force_start() ->
	case erlang:whereis(?MODULE) of
		?ERLNULL -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []);
		_ -> ignore
	end.


%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    debug:info("applicationex start ~p ~n", [node()]),
%%     OptionName = "../options/option",
    env:init([]),
    version_up:init(),
    debug:info("applicationex ########################## ~p~n", [{"End"}]),
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

