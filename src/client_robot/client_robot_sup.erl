%% @author eric.yutao
%% @doc @todo Add description to client_robot_sup.


-module(client_robot_sup).
-behaviour(supervisor).
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1, start_robot/1]).

start_link([]) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).


start_robot(ID) ->
	supervisor:start_child(?MODULE, [ID]).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1">supervisor:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, {SupervisionPolicy, [ChildSpec]}} | ignore,
	SupervisionPolicy :: {RestartStrategy, MaxR :: non_neg_integer(), MaxT :: pos_integer()},
	RestartStrategy :: one_for_all
					 | one_for_one
					 | rest_for_one
					 | simple_one_for_one,
	ChildSpec :: {Id :: term(), StartFunc, RestartPolicy, Type :: worker | supervisor, Modules},
	StartFunc :: {M :: module(), F :: atom(), A :: [term()] | undefined},
	RestartPolicy :: permanent
				   | transient
				   | temporary,
	Modules :: [module()] | dynamic.
%% ====================================================================
init([]) ->
    AChild = {'client_test',{'client_test',start_link,[]},
	      temporary,brutal_kill,worker,['client_test']},
    {ok,{{simple_one_for_one,5,60}, [AChild]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================


