%% @author eric.yutao
%% @doc @todo Add description to parse_proto_data.


-module(parse_proto_data).

-include("login_pb_define.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([]).
-compile(export_all).

-define(PROTO_FILE, "../src/proto/login_pb.proto").

parse_proto_data() ->
    case file:consult(?PROTO_FILE) of
        {ok, Messages} ->
            io:format("~p~n", [Messages]);
        Error ->
            io:format(">>>>>> Open file faild:~p~n", [{?MODULE, ?LINE, Error}])
    end.

