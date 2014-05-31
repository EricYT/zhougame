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
            convert_message_to_record(Messages, [], [], []),
            ok;
        Error ->
            io:format(">>>>>> Open file faild:~p~n", [{?MODULE, ?LINE, Error}])
    end.


convert_message_to_record([], Msgs, MsgDefines, MsgTypes) ->
    {Msgs, MsgDefines, MsgTypes};
convert_message_to_record([#message_heads{}=Record|Tail],
                          Msgs, MsgDefines, MsgTypes) ->
    io:format("message_heads ~p~n", [Record]),
    convert_message_to_record(Tail, Msgs, MsgDefines, MsgTypes);
convert_message_to_record([#msg_normal{}=Record|Tail],
                          Msgs, MsgDefines, MsgTypes) ->
    io:format("msg_normal ~p~n", [Record]),
    convert_message_to_record(Tail, Msgs, MsgDefines, MsgTypes);
convert_message_to_record([#type_private{}=Record|Tail],
                          Msgs, MsgDefines, MsgTypes) ->
    io:format("type_private ~p~n", [Record]),
    convert_message_to_record(Tail, Msgs, MsgDefines, MsgTypes).

