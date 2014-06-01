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
-define(PROTO_HRL_FILE, "../include/login_pb.hrl").
-define(PROTO_OP_FILE, "../src/proto/login_pb.erl").

-define(MESSAGE_HEAD_ETS, '$ets_message_head$').
-define(MESSAGE_DEFINE_ETS, '$ets_message_define$').
-define(MESSAGE_TYPE_ETS, '$ets_message_type$').

-define(FROAMT_RECORD_HEAD, "%%This file is auto generate,do not modify it.\n").
-define(FORMAT_RECORD, "-record($RECORD_NAME, {$RECORD_COLS}).\n").

parse_proto_data() ->
    case file:consult(?PROTO_FILE) of
        {ok, Messages} ->
            {Msgs, MsgDefines, MsgTypes} =
                message_validate(Messages, [], [], []),
            init_message_ets(Msgs, MsgDefines, MsgTypes),
            gen_proto_hrl_file(),
%%             io:format("Message : ~p~n", [{Msgs, MsgDefines, MsgTypes}]),
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


gen_proto_hrl_file() ->
    case file:open(?PROTO_HRL_FILE, [write]) of
        {ok, FileHandle} ->
            ConvertRecord =
                fun(#head_attr{msg_name = MsgName}=MsgRecord, AccRecord) ->
                        MsgNameString = atom_to_list(MsgName),
                        MsgCols = convert_message_record(MsgRecord),
                        Record =
                            mysql_op_gen:key_value_replace([{"$RECORD_NAME", MsgNameString},
                                                            {"$RECORD_COLS", MsgCols}],
                                                           ?FORMAT_RECORD), 
                        [Record|AccRecord]
                end,
            AllMsgHeads = ets:tab2list(?MESSAGE_HEAD_ETS),
            SortAllMsgHeads =
                lists:sort(fun(#head_attr{id = Id1}, #head_attr{id = Id2}) ->
                                   Id1 < Id2
                           end, AllMsgHeads),
            Res = lists:foldl(ConvertRecord, [], SortAllMsgHeads),
            io:format("~p~n", [SortAllMsgHeads]),
            file:write(FileHandle, ?FROAMT_RECORD_HEAD++lists:reverse(Res)),
            file:close(FileHandle),
            ok;
        {error, Reason} ->
            io:fomate("Gen proto hrl file error ~p~n", [Reason])
    end.


convert_message_record(#head_attr{id = Id, msg_name = MsgName}) ->
    case ets:lookup(?MESSAGE_DEFINE_ETS, MsgName) of
        [] ->
            io:format("Message formate error, no message ~p define~n", [MsgName]);
        [#msg_normal{msg_attrs = MsgCols}] ->
            SortMsgColsById =
                lists:sort(fun(#msg_attr{id = Id1}, #msg_attr{id = Id2}) ->
                                   Id1 < Id2
                           end, MsgCols),
            Temp = [atom_to_list(MsgAttr#msg_attr.msg_name)
                   ||MsgAttr<-SortMsgColsById, MsgAttr#msg_attr.msg_name =/= msgid],
            AddMsgId = ["msgid="++integer_to_list(Id)]++Temp,
            string:join(AddMsgId, ", ")
    end.



