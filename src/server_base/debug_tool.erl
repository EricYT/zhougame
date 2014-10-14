%% @author eric.yutao
%% @doc @todo Add description to debug_tool.


-module(debug_tool).

-compile(export_all).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

auto_etop(Node) ->
	rpc:call(Node, etop, start, [[{output,text},{interval,1},{lines,20},{sort,msg_q}]]).

etop(Node) ->
	io:format("rpc:call(~p, etop, start, [[{output,text},{interval,1},{lines,20},{sort,msg_q}]])~n", [Node]).

