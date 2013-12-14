%% Author: Administrator
%% Created: 2013-12-1
%% Description: TODO: Add description to env
-module(env).

%%
%% Include files
%%
-include("server_option.hrl").

%%
%% Exported Functions
%%
-export([]).
-compile(export_all).

%%
%% API Functions
%%
-spec init(OptionName) -> 'ok' when
                                 OptionName :: string().
init([OptionName]) ->
	ets:new(test, [public, set, named_table]),
    try
        ets:new(?SERVER_OPTION_ETS, [public, set, named_table])
    catch
        E:R ->
            debug:error("Error ~p ~p~n", [?MODULE, {E, R}]),
            ignore
    end,
    load_env(OptionName).


-spec load_env(FilePath) -> ignore when
                            FilePath :: string().
load_env(FilePath) ->
    case file:consult(FilePath) of
        {ok, [Options]} ->
%% 			slogger:msg("Load env ~p~n", [Options]),
            ets:delete_all_objects(?SERVER_OPTION_ETS),
            ets:insert(?SERVER_OPTION_ETS, Options),
            ok;
        {error, Reason} ->
            debug:error("~p ~p~n", [?MODULE, Reason]),
            []
    end.

-spec get(Key, Default) -> Value|Default when
                                           Key :: any(),
                                           Default :: any(),
                                           Value :: any().
get(Key, Default) ->
    case ets:lookup(?SERVER_OPTION_ETS, Key) of
        [] -> Default;
        [{_, Value}] -> Value
    end.

-spec put(Key, Value) -> no_return() when
                                       Key :: any(),
                                       Value :: any().
put(Key, Value) ->
    ets:insert(?SERVER_OPTION_ETS, {Key, Value}).




%%
%% Local Functions
%%

