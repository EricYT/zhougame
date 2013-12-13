%% Author: Eric.yu
%% Created: 2013-9-7
%% Description: TODO: Add description to behaviour_util
-module(behaviour_util).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([is_behaviour/2]).

%%
%% API Functions
%%
-spec is_behaviour(Module, Behaviours) -> boolean() when
														Module :: atom(),
														Behaviours :: [atom(), ...].
is_behaviour(Module, Behaviours) when erlang:is_list(Module) ->
	is_behaviour(erlang:list_to_atom(Module), Behaviours);
is_behaviour(Module, Behaviours) ->
	try
		Attributes = Module:module_info(attributes),
		is_behaviour_opt(Attributes, Behaviours)
	catch
		_E:_R -> debug:info("Module Error ~p ~p ~p~n", [Module, _E, _R]), false
	end.


%%
%% Local Functions
%%
-spec is_behaviour_opt(Attributes, Behaviours) -> boolean() when
														  Attributes :: [tuple(), ...],
														  Behaviours :: [atom(), ...].
is_behaviour_opt(_, []) -> true;
is_behaviour_opt([], _) -> false;
is_behaviour_opt([{behaviour, [Behaviour]}|Tail], Behaviours) ->
	LeaseBehaviours = lists:delete(Behaviour, Behaviours),
	is_behaviour_opt(Tail, LeaseBehaviours);
is_behaviour_opt([_|Tail], Behaviours) ->
	is_behaviour_opt(Tail, Behaviours);
is_behaviour_opt(_, _) -> false.