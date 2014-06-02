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


%% login_pb.erl template

'login_pb_template'() ->
"%% Author: cb1224
%% Created: 2014-5-21
%% Description: TODO: Add description to login_pb
-module(login_pb).

-include(\"login_pb.hrl\").
-define(NEED_COMPRESS_SIZE, 64).

-export([]).
-compile(export_all).

create_ets_and_init() ->
    create(),
    init().

create() ->
    ets:new(msg_id_map_record_encode_login_pb, [set, named_table, public]),    %% {msgid, msgName, encode_fun, decode_fun}
    ets:new(msg_id_map_record_decode_login_pb, [set, named_table, public]).    %% {msgid, msgName, mode, decode_fun}

get_record_info(MsgId) ->
    case ets:lookup(msg_id_map_record_decode_login_pb, MsgId) of
        [] ->
            false;
        [{MsgId, MsgName, Mode, _DecodeFun}] ->
            {MsgName, Mode}
    end.

get_encode_fun(MsgId) ->
    case ets:lookup(msg_id_map_record_encode_login_pb, MsgId) of
        [] ->
            false;
        [{MsgId, _MsgName, EncodeFun, _DecodeFun}] ->
            EncodeFun
    end.

get_decode_fun(MsgId) ->
    case ets:lookup(msg_id_map_record_decode_login_pb, MsgId) of
        true ->
            false;
        [{_Msgid, _MsgName, _Mode, Decode_fun}] ->
            Decode_fun
    end.

decode(Input) ->
    <<ZipFlag:8/unsigned, Left/binary>> = Input,
    OriBinary = if
                    ZipFlag =/= 0 ->
                        zlib:uncompress(Left);
                    true ->
                        Left
                end,
    <<MsgId:16/unsigned, _LeftBinary/binary>> = OriBinary,
    case get_decode_fun(MsgId) of
        [] ->
            [];
        Func ->
            login_pb:Func(OriBinary)
    end.

encode(Input) ->
    MsgId = element(2, Input),
    case get_encode_fun(MsgId) of
        [] ->
            <<>>;
        Func ->
            Out = apply(login_pb, Func, [Input]),
            OS = size(Out),
            {ZFlag, Payload} = if
                                   OS >= ?NEED_COMPRESS_SIZE ->
                                       {1, zlib:compress(Out)};
                                   true ->
                                       {0, Out}
                               end,
            PackSize = size(Payload) + 1,
            if
                PackSize > 65535 ->
                    <<>>;
                true ->
                    <<ZFlag:8/unsigned, Payload/binary>>
            end
    end.

%% encode fun
encode_string(Input) when is_list(Input) ->
    InputBin = list_to_binary(Input),
    encode_string(InputBin);
encode_string(Input) when is_binary(Input) ->
    Len = size(Input),
    <<Len:16/unsigned, Input/binary>>.

decode_string(BinInput) when is_binary(BinInput) ->
    <<_Len:16/unsigned, _Rest/binary>> = BinInput,
    _Str = binary:part(_Rest, 0, _Len),
    _Size = size(_Rest),
    _Rest1 = binary:part(_Rest, {_Size, _Len - _Size}),
    {binary_to_list(_Str), _Rest1}.

encode_string_list(Input) when is_list(Input) ->
    Len = length(Input),
    Out = lists:foldr(fun(Str, Acc) -> Bin = encode_string(Str), <<Bin/binary, Acc/binary>> end, <<>>, Input),
    <<Len:16/unsigned, Out/binary>>.

decode_string_list(BinInput) when is_binary(BinInput) ->
    <<_Len:16/unsigned, _Rest/binary>> = BinInput,
    {_List, _Tail} =
        lists:foldl(fun(_Index, {_AccList, _AccTail}) ->
                            {_StrTemp, _NewTail} = decode_string(_AccTail),
                            {[_StrTemp|_AccList], _NewTail}
                    end, {[], _Rest}, lists:seq(1, _Len)),
    {lists:reverse(_List), _Tail}.

ecnode_int8_list(Input) when is_list(Input) ->
    Len = length(Input),
    Out = lists:foldr(fun(Int, Acc) -> <<Int:8/signed, Acc/binary>> end, <<>>, Input),
    <<Len:16/unsigned, Out/binary>>.

decode_int8_list(Input) when is_binary(Input) ->
    <<_Len:16/unsigned, _Rest/binary>> = Input,
    {_List, _Tail} =
        lists:foldr(fun(_Index, {_AccList, _AccTail}) ->
                            <<_C:8/signed, _NewAccTail>> = _AccTail,
                            {[_C|_AccList], _NewAccTail}
                    end, {[], _Rest}, lists:seq(1, _Len)),
    {lists:reverse(_List), _Tail}.

ecnode_uint8_list(Input) when is_list(Input) ->
    Len = length(Input),
    Out = lists:foldr(fun(Int, Acc) -> <<Int:8/unsigned, Acc/binary>> end, <<>>, Input),
    <<Len:16/unsigned, Out/binary>>.

decode_uint8_list(Input) when is_binary(Input) ->
    <<_Len:16/unsigned, _Rest/binary>> = Input,
    {_List, _Tail} =
        lists:foldr(fun(_Index, {_AccList, _AccTail}) ->
                            <<_C:8/unsigned, _NewAccTail>> = _AccTail,
                            {[_C|_AccList], _NewAccTail}
                    end, {[], _Rest}, lists:seq(1, _Len)),
    {lists:reverse(_List), _Tail}.

ecnode_int16_list(Input) when is_list(Input) ->
    Len = length(Input),
    Out = lists:foldr(fun(Int, Acc) -> <<Int:16/signed, Acc/binary>> end, <<>>, Input),
    <<Len:16/unsigned, Out/binary>>.

decode_int16_list(Input) when is_binary(Input) ->
    <<_Len:16/unsigned, _Rest/binary>> = Input,
    {_List, _Tail} =
        lists:foldr(fun(_Index, {_AccList, _AccTail}) ->
                            <<_C:16/signed, _NewAccTail>> = _AccTail,
                            {[_C|_AccList], _NewAccTail}
                    end, {[], _Rest}, lists:seq(1, _Len)),
    {lists:reverse(_List), _Tail}.

ecnode_int32_list(Input) when is_list(Input) ->
    Len = length(Input),
    Out = lists:foldr(fun(Int, Acc) -> <<Int:32/signed, Acc/binary>> end, <<>>, Input),
    <<Len:16/unsigned, Out/binary>>.

decode_int32_list(Input) when is_binary(Input) ->
    <<_Len:16/unsigned, _Rest/binary>> = Input,
    {_List, _Tail} =
        lists:foldr(fun(_Index, {_AccList, _AccTail}) ->
                            <<_C:32/signed, _NewAccTail>> = _AccTail,
                            {[_C|_AccList], _NewAccTail}
                    end, {[], _Rest}, lists:seq(1, _Len)),
    {lists:reverse(_List), _Tail}.".

