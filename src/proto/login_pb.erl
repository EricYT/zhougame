%% Author: cb1224
%% Created: 2014-5-21
%% Description: TODO: Add description to login_pb
-module(login_pb).

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


init() ->
    ets:insert(msg_id_map_record_encode_login_pb, [{1, 'login_request_c2s', encode_login_request_c2s, decode_login_request_c2s}]),
    ets:insert(msg_id_map_record_decode_login_pb, [{1, 'login_request_s2c', login_pb, decode_login_request_c2s}]).


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


%% For test msg proto funcs

encode_login_c2s(Input) ->
    todo.

decode_login_c2s([MsgBin]) ->
	io:format(">>>>>>>>>> decode_login_c2s:~p~n", [{?MODULE, ?LINE, MsgBin}]),
	todo.

encode_login_s2c([MsgBin]) ->
    todo.

decode_login_s2c([MsgBin]) ->
	todo.
