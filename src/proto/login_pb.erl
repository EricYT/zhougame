%% Author: cb1224
%% Created: 2014-5-21
%% Description: TODO: Add description to login_pb
-module(login_pb).

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


init() ->
	%%Parse msg proto and generated insert code
	todo.


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
