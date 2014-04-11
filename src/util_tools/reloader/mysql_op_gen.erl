%% Author: Administrator
%% Created: 2014-4-11
%% Description: TODO: 通过模式替换来生成mysql操作代码
-module(mysql_op_gen).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-compile(export_all).
-export([]).

%%
%% API Functions
%%



%%
%% Local Functions
%%

-spec key_value_replace(KVList, Code) -> Code1 when
                                             KVList :: [KV, ...],
                                             KV :: tuple(),
                                             Code :: string(),
                                             Code1 :: string().
key_value_replace([{Key, Value}|Tail], Code) ->
    [Part1, Part2] = binary:split(list_to_binary(Code), [list_to_binary(Key)], [global]),
    key_value_replace(Tail, string:join([binary_to_list(Part1), binary_to_list(Part2)], Value));
key_value_replace([], Code) ->
    Code.
