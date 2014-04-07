%% Author: eric.yutao
%% Created: 2013-12-2
%% Description: TODO: Add description to uitl
-module(util).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([]).
-compile(export_all).

%%
%% API Functions
%%
string_to_term(String) ->
	case erl_scan:string(String++".") of
		{ok, Tokens, _} ->
			case erl_parse:parse_term(Tokens) of
				{ok, Term} ->
					{ok, Term};
				Reason ->
					io:format("error string_to_term ~p~n", [{?MODULE, Reason}]),
					parse_error
			end;
		Reason ->
			io:format("error string_to_term ~p~n", [{?MODULE, Reason}]),
			scan_error
	end.

term_to_string(Term) ->
	lists:flatten(io_lib:format("~w", [Term])).

%% Ye olde uniq function
-spec uniq(List::list()) -> List1::list().
uniq([X, X|Xs]) -> uniq([X|Xs]);
uniq([X|Xs]) -> [X|uniq(Xs)];
uniq([]) -> [].


%%
%% Local Functions
%%

