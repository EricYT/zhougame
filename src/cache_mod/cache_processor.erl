%%% -------------------------------------------------------------------
%%% Author  : Eric.yu
%%% Description : The processor for control the ets.That is to say,you can create a processor to
%%% make sure the ets is in your control.
%%%
%%% Created : 2013-9-8
%%% -------------------------------------------------------------------
-module(cache_processor).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("base_def.hrl").

%%
%% Macro
-define(CACHE_TABLE, '$cache_table').
-define(CACHE_TABLE_REFRESH_TIME, '$cache_table_refresh_time').
-define(REFRESH_REF, 'refresh_ref').
-define(LAST_REFRESH_TIME, '$last_refresh_time').

%% --------------------------------------------------------------------
%% External exports
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {start_time, lease_time}).

%% ====================================================================
%% External functions
%% ====================================================================
-spec start_link(CacheProcessor, TableName, TimeRefresh) -> ok when
									CacheProcessor :: atom(),
									TableName :: atom(),
									TimeRefresh :: non_neg_integer().
start_link(CacheProcessor, TableName, TimeRefresh) ->
	ProcessorName = cache_util:make_cache_processor_name(CacheProcessor),
	gen_server:start_link({local, ProcessorName}, ?MODULE, [TableName, TimeRefresh], []).

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
init([TableName, TimeRefresh]) ->
	Now = erlang:now(),
	put(?CACHE_TABLE, TableName),
	TimeResidue = cal_lease_time(Now, TimeRefresh),
    {ok, #state{start_time = Now, lease_time = TimeRefresh}, TimeResidue}.

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
handle_call({sync_get, Key}, _From, #state{start_time = StartTime, lease_time = LeaseTime}=State) ->
    Reply = case ets:lookup(?CACHE_TABLE, Key) of
				[] ->
					[];
				Res ->
					Res
			end,
	ResidueTime = cal_lease_time(StartTime, LeaseTime),
	{reply, Reply, State, ResidueTime};
handle_call(Request, From, #state{start_time = StartTime}=State) ->
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
handle_info({set, KeyValue, Time}, #state{start_time = StartTime, lease_time = LeaseTime}=State) ->
	ObjectOrObjects = erlang:append_element(KeyValue, Time),
	ets:insert(?CACHE_TABLE, ObjectOrObjects),
	ResidueTime = cal_lease_time(StartTime, LeaseTime),
	{noreply, State, ResidueTime};

handle_info({del, Key}, #state{start_time = StartTime, lease_time = LeaseTime}=State) ->
	ets:delete(?CACHE_TABLE, Key),
	ResidueTime = cal_lease_time(StartTime, LeaseTime),
	{noreply, State, ResidueTime};

handle_info(timeout, #state{start_time = _StartTime, lease_time = LeaseTime}=State) ->
	%% 在遍历表的时候，将表锁起来。防止过程中因为表的变化引起遍历错误
	ets:safe_fixtable(?CACHE_TABLE, true),
	refresh_table_opt(),
	ets:safe_fixtable(?CACHE_TABLE, false),
	{noreply, State, LeaseTime};
handle_info(_Info, State) ->
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
-spec refresh_table_opt() -> no_return().
refresh_table_opt() ->
	First = ets:first(?CACHE_TABLE),
	refresh_table_opt1(First).

%%@doc 使用first/next来遍历表，是为了防止在遍历表的时候，将整张表载入内存
refresh_table_opt1('$end_of_table') ->
	refresh_end;
refresh_table_opt1({Key, _Value, infinity}) ->
	Next = ets:next(?CACHE_TABLE, Key),
	refresh_table_opt1(Next);
refresh_table_opt1({Key, _Value, Time}) ->
	UsedTime = time_util:now_diff(erlang:now(), Time),
	DefaultTimeRefresh = get(?CACHE_TABLE_REFRESH_TIME),
	if
		UsedTime >= DefaultTimeRefresh ->
			ets:delete(?CACHE_TABLE, Key);
		true ->
			nothing
	end,
	Next = ets:next(?CACHE_TABLE, Key),
	refresh_table_opt1(Next).


cal_lease_time(_, infinity) ->
	infinity;
cal_lease_time({_, _, _}=StartTime, LeaseTime) ->
	Now = erlang:now(),
	TimeDiff = time_util:now_diff(Now, StartTime),
	TimeUsed = TimeDiff rem LeaseTime,
	if
		TimeUsed >= LeaseTime ->
			0;
		true ->
			(LeaseTime - TimeUsed) * 1000
	end;
cal_lease_time(_, _) ->
	0.
