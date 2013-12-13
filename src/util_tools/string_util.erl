%% Author: cb1224
%% Created: 2013-11-28
%% Description: TODO: Add description to string_util
-module(string_util).

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
-spec replace(S1, TS, RS) -> SR when
								  S1 :: string(),
								  TS :: string(),
								  RS :: string(),
								  SR :: string().
replace(String, Tokens, RepString) ->
	String;
replace(String, Tokens, RepString) ->
	TokensLen = string:len(Tokens),
	TokenString = token_string(String, Tokens, TokensLen),
	string:join(TokenString, RepString).
	


%%
%% Local Functions
%%
-spec token_string(String, Tokens, Replace) -> Res when
													 String :: string(),
													 Tokens :: string(),
													 Replace:: string(),
													 Res	:: string().
token_string(OriString, _, 0) ->
	OriString;
token_string(OriString, Tokens, TokenLen) ->
	case string:str(OriString, Tokens) of
		0 -> [OriString];
		1 ->
			LeftString = string:sub_string(OriString, TokenLen+1),
			token_string(LeftString, Tokens, TokenLen);
		I ->
			LeftString = string:sub_string(OriString, 1, I-1),
			RightString = string:sub_string(OriString, I+TokenLen),
			[LeftString|token_string(RightString, Tokens, TokenLen)]
	end.




