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
-export([replace/3, replace_by_re/3]).

%%
%% API Functions
%%
-spec replace(S1, TS, RS) -> SR when
								  S1 :: string(),
								  TS :: string(),
								  RS :: string(),
								  SR :: string().
%% replace(String, Tokens, RepString) ->
%% 	String;
replace(String, Tokens, RepString) ->
	TokensLen = string:len(Tokens),
	TokenString = token_string(String, Tokens, TokensLen),
	string:join(TokenString, RepString).
	

replace_by_re(String, Tokens, RepString) ->
	{ok, MP} = re:compile(Tokens),
	re:replace(String, MP, RepString, [global, {return,list}]).


%% 7> F1 = fun() -> string_util:replace("222abc22222 abc 2332abc2222", "abc", "ddd") end.
%% #Fun<erl_eval.20.90072148>
%% 8> F2 = fun() -> string_util:replace_by_re("222abc22222 abc 2332abc2222", "abc", "ddd") end.
%% #Fun<erl_eval.20.90072148>
%% 9> timer:tc(F1).
%% {28,"222ddd22222 ddd 2332ddd2222"}
%% 10> timer:tc(F2).
%% {61,"222ddd22222 ddd 2332ddd2222"}
%% 11> timer:tc(F1).
%% {17,"222ddd22222 ddd 2332ddd2222"}
%% 12> timer:tc(F2).
%% {47,"222ddd22222 ddd 2332ddd2222"}
%% 13> timer:tc(F1).
%% {16,"222ddd22222 ddd 2332ddd2222"}
%% 14> timer:tc(F2).
%% {50,"222ddd22222 ddd 2332ddd2222"}

%% 24> R1 = fun() -> string_util:replace(A, "replace", "123") end.
%% #Fun<erl_eval.20.90072148>
%% 25> R2 = fun() -> string_util:replace_by_re(A, "replace", "123") end.
%% #Fun<erl_eval.20.90072148>
%% 26> timer:t
%% tc/1         tc/2         tc/3         terminate/2  
%% 26> timer:tc(R1).
%% {146,
%%  "%% Author: cb1224\n%% Created: 2013-11-28\n%% Description: TODO: Add description to string_util\n-module(string_util).\n\n%%\n%% Include files\n%%\n\n%%\n%% Exported Functions\n%%\n-export([123/3, 123_by_re/3]).\n\n%%\n%% API Functions\n%%\n-spec 123(S1, TS, RS) -> SR when\n\t\t\t\t\t\t\t\t  S1 :: string(),\n\t\t\t\t\t\t\t\t  TS :: string(),\n\t\t\t\t\t\t\t\t  RS :: string(),\n\t\t\t\t\t\t\t\t  SR :: string().\n%% 123(String, Tokens, RepString) ->\n%% \tString;\n123(String, Tokens, RepString) ->\n\tTokensLen = string:len(Tokens),\n\tTokenString = token_string(String, Tokens, TokensLen),\n\tstring:join(TokenString, RepString).\n\t\n\n123_by_re(String, Tokens, RepString) ->\n\t{ok, MP} = re:compile(Tokens),\n\tre:123(String, MP, RepString, [global, {return,list}]).\n\n\n%% 7> F1 = fun() -> string_util:123(\"222abc22222 abc 2332abc2222\", \"abc\", \"ddd\") end.\n%% #Fun<erl_eval.20.90072148>\n%% 8> F2 = fun() -> string_util:123_by_re(\"222abc22222 abc 2332abc2222\", \"abc\", \"ddd\") end.\n%% #Fun<erl_eval.20.90072148>\n%% 9> timer:tc(F1).\n%% {28,\"222ddd22222 ddd 2332ddd2222\"}\n%% 10> timer:tc(F2).\n%% {61,\"222ddd22222 ddd 2332ddd2222\"}\n%% 11> timer:tc(F1).\n%% {17,\"222ddd22222 ddd 2332ddd2222\"}\n%% 12> timer:tc(F2).\n%% {47,\"222ddd22222 ddd 2332ddd2222\"}\n%% 13> timer:tc(F1).\n%% {16,\"222ddd22222 ddd 2332ddd2222\"}\n%% 14> timer:tc(F2).\n%% {50,\"222ddd22222 ddd 2332ddd2222\"}\n\n\n%%\n%% Local Functions\n%%\n-spec token_string(String, Tokens, Replace) -> Res when\n\t\t\t\t\t\t\t\t\t\t\t\t\t String :: string(),\n\t\t\t\t\t\t\t\t\t\t\t\t\t Tokens :: string(),\n\t\t\t\t\t\t\t\t\t\t\t\t\t Replace:: string(),\n\t\t\t\t\t\t\t\t\t\t\t\t\t Res\t:: string().\ntoken_string(OriString, _, 0) ->\n\tOriString;\ntoken_string(OriString, Tokens, TokenLen) ->\n\tcase string:str(OriString, Tokens) of\n\t\t0 -> [OriString];\n\t\t1 ->\n\t\t\tLeftString = string:sub_string(OriString, TokenLen+1),\n\t\t\t[\"\"|token_string(LeftString, Tokens, TokenLen)];\n\t\tI ->\n\t\t\tLeftString = string:sub_string(OriString, 1, I-1),\n\t\t\tRightString = string:sub_string(OriString, I+TokenLen),\n\t\t\t[LeftString|token_string(RightString, Tokens, TokenLen)]\n\tend.\n\n\n\n\n"}
%% 27> timer:tc(R2).
%% {137,
%%  "%% Author: cb1224\n%% Created: 2013-11-28\n%% Description: TODO: Add description to string_util\n-module(string_util).\n\n%%\n%% Include files\n%%\n\n%%\n%% Exported Functions\n%%\n-export([123/3, 123_by_re/3]).\n\n%%\n%% API Functions\n%%\n-spec 123(S1, TS, RS) -> SR when\n\t\t\t\t\t\t\t\t  S1 :: string(),\n\t\t\t\t\t\t\t\t  TS :: string(),\n\t\t\t\t\t\t\t\t  RS :: string(),\n\t\t\t\t\t\t\t\t  SR :: string().\n%% 123(String, Tokens, RepString) ->\n%% \tString;\n123(String, Tokens, RepString) ->\n\tTokensLen = string:len(Tokens),\n\tTokenString = token_string(String, Tokens, TokensLen),\n\tstring:join(TokenString, RepString).\n\t\n\n123_by_re(String, Tokens, RepString) ->\n\t{ok, MP} = re:compile(Tokens),\n\tre:123(String, MP, RepString, [global, {return,list}]).\n\n\n%% 7> F1 = fun() -> string_util:123(\"222abc22222 abc 2332abc2222\", \"abc\", \"ddd\") end.\n%% #Fun<erl_eval.20.90072148>\n%% 8> F2 = fun() -> string_util:123_by_re(\"222abc22222 abc 2332abc2222\", \"abc\", \"ddd\") end.\n%% #Fun<erl_eval.20.90072148>\n%% 9> timer:tc(F1).\n%% {28,\"222ddd22222 ddd 2332ddd2222\"}\n%% 10> timer:tc(F2).\n%% {61,\"222ddd22222 ddd 2332ddd2222\"}\n%% 11> timer:tc(F1).\n%% {17,\"222ddd22222 ddd 2332ddd2222\"}\n%% 12> timer:tc(F2).\n%% {47,\"222ddd22222 ddd 2332ddd2222\"}\n%% 13> timer:tc(F1).\n%% {16,\"222ddd22222 ddd 2332ddd2222\"}\n%% 14> timer:tc(F2).\n%% {50,\"222ddd22222 ddd 2332ddd2222\"}\n\n\n%%\n%% Local Functions\n%%\n-spec token_string(String, Tokens, Replace) -> Res when\n\t\t\t\t\t\t\t\t\t\t\t\t\t String :: string(),\n\t\t\t\t\t\t\t\t\t\t\t\t\t Tokens :: string(),\n\t\t\t\t\t\t\t\t\t\t\t\t\t Replace:: string(),\n\t\t\t\t\t\t\t\t\t\t\t\t\t Res\t:: string().\ntoken_string(OriString, _, 0) ->\n\tOriString;\ntoken_string(OriString, Tokens, TokenLen) ->\n\tcase string:str(OriString, Tokens) of\n\t\t0 -> [OriString];\n\t\t1 ->\n\t\t\tLeftString = string:sub_string(OriString, TokenLen+1),\n\t\t\t[\"\"|token_string(LeftString, Tokens, TokenLen)];\n\t\tI ->\n\t\t\tLeftString = string:sub_string(OriString, 1, I-1),\n\t\t\tRightString = string:sub_string(OriString, I+TokenLen),\n\t\t\t[LeftString|token_string(RightString, Tokens, TokenLen)]\n\tend.\n\n\n\n\n"}
%% 28> 
%% 

%% compare these methods, when string is complex, replace_by_re/3 is more effictive otherwise replace/3

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
			[""|token_string(LeftString, Tokens, TokenLen)];
		I ->
			LeftString = string:sub_string(OriString, 1, I-1),
			RightString = string:sub_string(OriString, I+TokenLen),
			[LeftString|token_string(RightString, Tokens, TokenLen)]
	end.




