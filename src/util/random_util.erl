%% Author: Eric.yu
%% Created: 2013-9-7
%% Description: TODO: Add description to random_util
-module(random_util).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([random_by_weight/1, random_list/1, random_list_nor/1]).

%%
%% API Functions
%%
-spec random_by_weight(WeightList) -> RandomList when
												   WeightList :: [{Term, Value}, ...],
												   RandomList :: [{Term, Value}, ...],
												   Term :: term(),
												   Value :: non_neg_integer().
random_by_weight(WeightList) when erlang:is_list(WeightList) ->
	SumWeight = lists:sum([Value1 || {_, Value1} <- WeightList]),
	RandomValue = random:uniform(SumWeight),
	{TermRes, _} = lists:foldl(fun({Term, Value}, {AccTerm, AccValue}) ->
									if
										AccTerm =/= [] ->
											{AccTerm, AccValue};
										true ->
											if
												AccValue + Value >= RandomValue -> %% get it
													{Term, Value};
												true ->
													{AccTerm, AccValue+Value}
											end
									end
							end, {[], 0}, WeightList),
	TermRes;
random_by_weight(_) ->
	error.


%% 这个版本的随机乱序一个list， 但是如果是非数字(原子)的时候，不管用。会按字段顺序排列
-spec random_list(ValueList::[any(), ...]) -> List::[any(), ...].
random_list([]) ->
	[];
random_list(List) when erlang:is_list(List) ->
	Temp = sets:from_list(List),
	sets:to_list(Temp).

-spec random_list_nor(List::[any(), ...]) -> List::[any(), ...].
random_list_nor(List) when erlang:is_list(List) ->
	random_list(List, []).


random_list([], RandomInfo) ->
	RandomInfo;
random_list(Info, RandomInfo) ->
	Nth = random:uniform(erlang:length(Info)),
	Elem = lists:nth(Nth, Info),
	%% 如果有重复的，这里就可能删除的不是滴n个。但是这似乎与随机不会有什么负面的效果
	random_list(lists:delete(Elem, Info), [Elem|RandomInfo]).

%%
%% Local Functions
%%

