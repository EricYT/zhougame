%% Author: eric.yutao
%% Description: This file is auto generated by login_pb.proto
-module(login_pb_test).

-include("login_pb.hrl").
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
    {lists:reverse(_List), _Tail}.

%% init opeate
init() ->
	ets:insert(msg_id_map_record_encode_login_pb, [{1, login_c2s, encode_login_c2s, decode_login_c2s}]),
	ets:insert(msg_id_map_record_decode_login_pb, [{1, login_c2s, login_pb, decode_login_c2s}]),
	ets:insert(msg_id_map_record_encode_login_pb, [{2, login_s2c, encode_login_s2c, decode_login_s2c}]),
	ets:insert(msg_id_map_record_decode_login_pb, [{2, login_s2c, lua, decode_login_s2c}]),
	ets:insert(msg_id_map_record_encode_login_pb, [{3, type_test, encode_type_test, decode_type_test}]),
	ets:insert(msg_id_map_record_decode_login_pb, [{3, type_test, login_pb, decode_type_test}]).

encode_type2(Input) ->
	_col1 = <<(Input#type2.col1):16/unsigned>>,
	_col2 = <<(Input#type2.col2):32/signed>>,
	<<
	_col1/binary,
	_col2/binary
	>>.

decode_type2(Input) ->
	_LastBinary0 = Input,
	<<_col1:16/unsigned, _LastBinary1/binary>> = _LastBinary0,
	<<_col2:32/signed, _LastBinary2/binary>> = _LastBinary1,
	{#type2{
		col1 = _col1,
		col2 = _col2}, _LastBinary2}.

encode_type1(Input) ->
	_col1 = <<(Input#type1.col1):16/unsigned>>,
	_col2_count = length(Input#type1.col2),
	_col2 = lists:foldl(fun(_cls_list_col2, _cls_bin_col2) ->
		_new_cls_bin_col2 = encode_type2(_cls_list_col2),
		<<_cls_bin_col2/binary, _new_cls_bin_col2/binary>>
	end,<<_col2_count:16/unsigned>>, Input#type1.col2),
	<<
	_col1/binary,
	_col2/binary
	>>.

decode_type1(Input) ->
	_LastBinary0 = Input,
	<<_col1:16/unsigned, _LastBinary1/binary>> = _LastBinary0,
	<<_col2_count:16/unsigned, _LastBinary2_temp/binary>> = _LastBinary1,
	{_col2, _LastBinary2} = lists:foldl(fun(_, {_cls_list_col2, _cls_bin_col2}) ->
		{_new_cls_col2, _new_cls_bin_col2} = decode_type2(_cls_bin_col2),
		{[_cls_list_col2|_new_cls_col2], _new_cls_bin_col2}
	end,{[], _LastBinary2_temp}, lists:seq(1, _col2_count)),
	{#type1{
		col1 = _col1,
		col2 = _col2}, _LastBinary2}.


encode_login_c2s(Input) ->
	_msgid = <<(Input#login_c2s.msgid):16/unsigned>>,
	_id = <<(Input#login_c2s.id):64/signed>>,
	_name = encode_string_list(Input#login_c2s.name),
	<<
	_msgid/binary,
	_id/binary,
	_name/binary
	>>.

decode_login_c2s(Input) ->
	_LastBinary0 = Input,
	<<_msgid:16/unsigned, _LastBinary1/binary>> = _LastBinary0,
	<<_id:64/signed, _LastBinary2/binary>> = _LastBinary1,
	{_name, _LastBinary3} = decode_string_list(_LastBinary2),
	#login_c2s{
		msgid = _msgid,
		id = _id,
		name = _name}.

encode_login_s2c(Input) ->
	_msgid = <<(Input#login_s2c.msgid):16/unsigned>>,
	_res = <<(Input#login_s2c.res):32/signed>>,
	<<
	_msgid/binary,
	_res/binary
	>>.

decode_login_s2c(Input) ->
	_LastBinary0 = Input,
	<<_msgid:16/unsigned, _LastBinary1/binary>> = _LastBinary0,
	<<_res:32/signed, _LastBinary2/binary>> = _LastBinary1,
	#login_s2c{
		msgid = _msgid,
		res = _res}.

encode_type_test(Input) ->
	_msgid = <<(Input#type_test.msgid):16/unsigned>>,
	_res_count = length(Input#type_test.res),
	_res = lists:foldl(fun(_cls_list_res, _cls_bin_res) ->
		_new_cls_bin_res = encode_type1(_cls_list_res),
		<<_cls_bin_res/binary, _new_cls_bin_res/binary>>
	end,<<_res_count:16/unsigned>>, Input#type_test.res),
	<<
	_msgid/binary,
	_res/binary
	>>.

decode_type_test(Input) ->
	_LastBinary0 = Input,
	<<_msgid:16/unsigned, _LastBinary1/binary>> = _LastBinary0,
	<<_res_count:16/unsigned, _LastBinary2_temp/binary>> = _LastBinary1,
	{_res, _LastBinary2} = lists:foldl(fun(_, {_cls_list_res, _cls_bin_res}) ->
		{_new_cls_res, _new_cls_bin_res} = decode_type1(_cls_bin_res),
		{[_cls_list_res|_new_cls_res], _new_cls_bin_res}
	end,{[], _LastBinary2_temp}, lists:seq(1, _res_count)),
	#type_test{
		msgid = _msgid,
		res = _res}.

