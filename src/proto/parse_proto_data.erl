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

-define(MESSAGE_HEAD_ETS, 'ets_message_head').
-define(MESSAGE_DEFINE_ETS, 'ets_message_define').
-define(MESSAGE_TYPE_ETS, 'ets_message_type').

parse_proto_data() ->
    case file:consult(?PROTO_FILE) of
        {ok, Messages} ->
            {Msgs, MsgDefines, MsgTypes} =
                message_validate(Messages, [], [], []),
            init_message_ets(Msgs, MsgDefines, MsgTypes),
            io:format("Message : ~p~n", [{Msgs, MsgDefines, MsgTypes}]),
            ok;
        Error ->
            io:format(">>>>>> Open file faild:~p~n", [{?MODULE, ?LINE, Error}])
    end.


message_validate([], Msgs, MsgDefines, MsgTypes) ->
    {Msgs, MsgDefines, MsgTypes};
message_validate([#message_heads{messages=HeadAttrs}=_Record|Tail],
                          _Msgs, MsgDefines, MsgTypes) ->
    Messages = [Head||Head<-HeadAttrs, erlang:is_record(Head, head_attr)],
    message_validate(Tail, Messages, MsgDefines, MsgTypes);
message_validate([#msg_normal{}=Record|Tail],
                          Msgs, MsgDefines, MsgTypes) ->
    message_validate(Tail, Msgs, [Record|MsgDefines], MsgTypes);
message_validate([#type_private{}=Record|Tail],
                          Msgs, MsgDefines, MsgTypes) ->
    message_validate(Tail, Msgs, MsgDefines, [Record|MsgTypes]).


init_message_ets(MsgHeads, MsgDefines, MsgType) ->
    try
        ets:new(?MESSAGE_HEAD_ETS, [named_table, public, set, {keypos, 2}]),
        ets:new(?MESSAGE_DEFINE_ETS, [named_table, public, set, {keypos, 2}]),
        ets:new(?MESSAGE_TYPE_ETS, [named_table, public, set, {keypos, 2}])
    catch
        E:R ->
            io:format("create ets error :~p~n", [{E, R}]),
            erlang:exit("Proto Ets Exists")
    end,
    %% If the ets exists, exit 
    ets:insert(?MESSAGE_HEAD_ETS, MsgHeads),
    ets:insert(?MESSAGE_DEFINE_ETS, MsgDefines),
    ets:insert(?MESSAGE_TYPE_ETS, MsgType),
    ok.