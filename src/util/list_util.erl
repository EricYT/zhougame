%% Author: Eric.yu
%% Created: 2013-9-21
%% Description: TODO: The tool mod for list
-module(list_util).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([lrec/2, rrec/2, is_subset/2]).

%%
%% API Functions
%%
-spec lrec(String, Char) -> String1 when
									  String :: string(),
									  Char :: char(),
									  String1 :: string().
lrec(S, _) when not erlang:is_list(S) ->
	error(badarg);
lrec(S, C) when C =< 0, C >= 255 ->
	S;
lrec([], _) ->
	[];
lrec([C|Tail], C) -> lrec(Tail, C);
lrec(S, _) -> S.


-spec rrec(String::string(), Char::char()) -> String1::string().
rrec(S, C) ->
	lists:reverse(lrec(lists:reverse(S), C)).



-spec is_subset(List1, List2) -> boolean() when
											 List1 :: [any(), ...],
											 List2 :: [any(), ...].
is_subset([], _) -> false;
is_subset(L1, L2) when erlang:is_list(L1), erlang:is_list(L2) ->
	Temp = lists:subtract(L2, L1),
	if
		Temp =/= L2 -> true;
		true -> false
	end;
is_subset(_, _) ->
	error(bad_arg).
%%
%% Local Functions
%%

