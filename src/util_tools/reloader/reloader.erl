%%% -------------------------------------------------------------------
%%% Author  : eric.yutao
%%% Description :This module just likes version_up,but it's automated
%%%  Question one: This is no need to use a send_after,gen_server has
%%%  a way to handle a timeout. handle_info(cast, ****)
%%% Created : 2013-12-16
%%% -------------------------------------------------------------------
-module(reloader).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("kernel/include/file.hrl").

%% Macro
-define(SECONDS_RELOADER, 10).

%% --------------------------------------------------------------------
%% External exports
-export([
		 start_link/0,
		 start/0,
		 stop/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {last}).

-type(timestamp()::{{integer(), integer(), integer()}, {integer(), integer(), integer()}}).

%% ====================================================================
%% External functions
%% ====================================================================
-spec start_link() -> ignore.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec start() -> ignore.
start() ->
	gen_server:start({local, ?MODULE}, ?MODULE, [], []).


-spec stop() -> ok.
stop() ->
	gen_server:call(?MODULE, stop).


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
	erlang:send_after(timer:seconds(?SECONDS_RELOADER), ?MODULE, doit),
    {ok, #state{last = timestamp()}}.

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
handle_call(stop, From, State) ->
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
handle_info(doit, State) ->
	NowTimestamp = timestamp(),
	_ = doit(State#state.last, NowTimestamp),
	erlang:send_after(timer:seconds(?SECONDS_RELOADER), ?MODULE, doit),
	{noreply, State#state{last = NowTimestamp}};
handle_info(_Info, State) ->
	debug:error("Error handle error message ~p~n", [{?MODULE, _Info, State}]),
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
timestamp() ->
	erlang:localtime().


-spec doit(From, To) -> ok|{error, Reason} when
											 From :: timestamp(),
											 To :: timestamp(),
											 Reason :: atom().
doit(From, To) ->
	[begin
		 case file:read_file_info(Filename) of
			 {ok, #file_info{mtime = MTime}} when MTime > From, MTime < To ->
				 reload(Module);
			 {ok, _} -> unmodified;
			 {error, enoent} -> gone;
			 {error, Reason} ->
				 debug:error("Read file error in ~p ~p~n", [?MODULE, Reason]),
				 error
		 end
	 end||{Module, Filename}<-code:all_loaded(), is_list(Filename)].


-spec reload(Module) -> ok|error when
								   Module :: atom().
reload(Module) when is_atom(Module) ->
	io:format("Reloading ~p~n", [Module]),
	code:purge(Module),
	case code:load_file(Module) of
		{module, _} -> io:format("Reloading ok~n");
		{error, Reason} -> debug:error("Loading error ~p~n", [Reason]), error
	end.


-spec load_modules(Modules) -> list() when
										Modules :: [Module, ...],
										Module :: atom().
load_modules(Modules) when is_list(Modules) ->
	[begin code:purge(Module), code:load_file(Module) end || Module <- Modules].


-spec all_changed() -> list().
all_changed() ->
	[M || {M, Fn} <- code:all_loaded(), is_list(Fn), is_change(Fn)].


-spec is_change(M) -> boolean() when
								  M :: atom().
is_change(M) ->
	try
		module_vsn(M:module_info()) =/= module_vsn(code:get_object_code(M))
	catch _:_ -> false
	end.


module_vsn({Moudle, Binary, Filename}) ->
	{ok, {Moudle, Vsn}} = beam_lib:version(Filename),
	Vsn;
module_vsn(L) when is_list(L) ->
	{_, Attrs} = lists:keyfind(attributes, 1, L),
	{_, Vsn} = lists:keyfind(vsn, 1, Attrs),
	Vsn.



