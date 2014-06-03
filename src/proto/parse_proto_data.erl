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
-define(PROTO_OP_FILE, "../src/proto/login_pb_test.erl").

-define(MESSAGE_HEAD_ETS, '$ets_message_head$').
-define(MESSAGE_DEFINE_ETS, '$ets_message_define$').
-define(MESSAGE_TYPE_ETS, '$ets_message_type$').

-define(NORMAL_TYPE, [{"uint", "unsigned"}, {"int", "signed"}]).
-define(SPECIAL_TYPE, ['string']).

-define(FROAMT_RECORD_HEAD, "%%This file is auto generate,do not modify it.\n").
-define(FORMAT_RECORD, "-record($RECORD_NAME, {$RECORD_COLS}).\n").

-define(FORMAT_ETS_INSERT, "\tets:insert(msg_id_map_record_encode_login_pb, [{$ENCODE_INSERT}]),\n\tets:insert(msg_id_map_record_decode_login_pb, [{$DECODE_INSERT}])").

parse_proto_data() ->
    case file:consult(?PROTO_FILE) of
        {ok, Messages} ->
            {Msgs, MsgDefines, MsgTypes} =
                message_validate(Messages, [], [], []),
            init_message_ets(Msgs, MsgDefines, MsgTypes),
            gen_proto_hrl_file(),
            gen_proto_operate_file(),
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
			%% normal message define
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
                                   Id1 > Id2
                           end, AllMsgHeads),
            Res1 = lists:foldl(ConvertRecord, [], SortAllMsgHeads),
			
			%% private type define
			ConvertTypeRecord =
                fun(#type_private{type_name = TypeName}=TypeRecord, AccTypes) ->
						TypeNameString = atom_to_list(TypeName),
                        TypeCols = convert_type_message_record(TypeRecord),
                        Record =
                            mysql_op_gen:key_value_replace([{"$RECORD_NAME", TypeNameString},
                                                            {"$RECORD_COLS", TypeCols}],
                                                           ?FORMAT_RECORD), 
                        [Record|AccTypes]
                end,
            AllMsgTypes = ets:tab2list(?MESSAGE_TYPE_ETS),
            Res2 = lists:foldl(ConvertTypeRecord, [], AllMsgTypes),
            file:write(FileHandle, Res1++Res2),
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
            Temp = [atom_to_list(MsgAttr#msg_attr.field_name)
                   ||MsgAttr<-SortMsgColsById, MsgAttr#msg_attr.field_name =/= msgid],
            AddMsgId = ["msgid="++integer_to_list(Id)]++Temp,
            string:join(AddMsgId, ", ")
    end.

convert_type_message_record(#type_private{attrs = Cols}) ->
	SortColsById =
		lists:sort(fun(#msg_attr{id = Id1}, #msg_attr{id = Id2}) ->
						   Id1 < Id2
				   end, Cols),
	Temp = [atom_to_list(MsgAttr#msg_attr.field_name)
			  ||MsgAttr<-SortColsById],
	string:join(Temp, ", ").


gen_proto_operate_file() ->
    case file:open(?PROTO_OP_FILE, [write]) of
        {ok, FileHandle} ->
            InsertLan = convert_msg_insert(),
            CodeLan = convert_msg_encode_or_decode(),
			PrivateTypeLan = convert_msg_private_type(),
            GengerateCon =
                mysql_op_gen:key_value_replace([{"$INSERT_LANS", InsertLan},
												{"$MSG_CODE", CodeLan},
												{"$MSG_TYPE_CODE", PrivateTypeLan}
                                               ],
                                                'login_pb_template'()),
            file:write(FileHandle, GengerateCon),
            file:close(FileHandle),
            ok;
        {error, Reason} ->
            io:fomate("gen_proto_operate_file error ~p~n", [Reason])
    end.

convert_msg_insert() ->
    ConvertRecord =
        fun(#head_attr{msg_name = MsgName}=MsgRecord, AccRecord) ->
                {EncodeInsert, DecodeInsert} = convert_msg_insert(MsgRecord),
                Record =
                    mysql_op_gen:key_value_replace([{"$ENCODE_INSERT", EncodeInsert},
                                                    {"$DECODE_INSERT", DecodeInsert}],
                                                   ?FORMAT_ETS_INSERT), 
                [Record|AccRecord]
        end,
    AllMsgHeads = ets:tab2list(?MESSAGE_HEAD_ETS),
    SortAllMsgHeads =
        lists:sort(fun(#head_attr{id = Id1}, #head_attr{id = Id2}) ->
                           Id1 > Id2
                   end, AllMsgHeads),
    Res = lists:foldl(ConvertRecord, [], SortAllMsgHeads),
    string:join(Res, ",\n").

convert_msg_insert(#head_attr{id = Id, msg_name = MsgName, mod = Mod}) ->
    MsgNameString = atom_to_list(MsgName),
    IdString = integer_to_list(Id),
    ModString = atom_to_list(Mod),
    EncodeString =
        string:join([IdString, MsgNameString, "encode_"++MsgNameString, "decode_"++MsgNameString], ", "),
    DecodeString =
        string:join([IdString, MsgNameString, ModString, "decode_"++MsgNameString], ", "),
    {EncodeString, DecodeString}.


convert_msg_encode_or_decode() ->
    ConvertRecord =
        fun(#head_attr{}=MsgRecord, AccRecord) ->
                {MsgEncode, MsgDecode} = convert_encode_or_decode_part(MsgRecord),
				[MsgDecode]++[MsgEncode]++AccRecord
        end,
    AllMsgHeads = ets:tab2list(?MESSAGE_HEAD_ETS),
    SortAllMsgHeads =
        lists:sort(fun(#head_attr{id = Id1}, #head_attr{id = Id2}) ->
                           Id1 < Id2
                   end, AllMsgHeads),
    Res = lists:foldl(ConvertRecord, [], SortAllMsgHeads),
    string:join(lists:reverse(Res), "\n").


convert_msg_private_type() ->
    ConvertRecord =
        fun(#type_private{}=TypeRecord, AccRecord) ->
                {MsgEncode, MsgDecode} =
					convert_private_type_encode_or_decode_part(TypeRecord),
				[MsgDecode]++[MsgEncode]++AccRecord
        end,
    AllMsgHeads = ets:tab2list(?MESSAGE_TYPE_ETS),
    Res = lists:foldl(ConvertRecord, [], AllMsgHeads),
    string:join(lists:reverse(Res), "\n").


convert_encode_or_decode_part(#head_attr{msg_name = MsgName}) ->
    case ets:lookup(?MESSAGE_DEFINE_ETS, MsgName) of
        [] ->
            io:format("Message formate error, no message ~p define~n", [MsgName]),
			exit("Message formate error, no message define");
        [#msg_normal{msg_attrs = MsgCols}] ->
			SortMsgColsById =
				lists:sort(fun(#msg_attr{id = Id1}, #msg_attr{id = Id2}) ->
								   Id1 < Id2
						   end, MsgCols),
			EncodePart = convert_encode_part(MsgName, SortMsgColsById),
			DecodePart = convert_decode_part(MsgName, SortMsgColsById),
			{EncodePart, DecodePart}
    end.

convert_private_type_encode_or_decode_part(#type_private{type_name = MsgName,
														 attrs = Cols}) ->
	SortColsById =
		lists:sort(fun(#msg_attr{id = Id1}, #msg_attr{id = Id2}) ->
						   Id1 < Id2
				   end, Cols),
	EncodePart = convert_encode_part(MsgName, SortColsById),
	DecodePart = convert_type_decode_part(MsgName, SortColsById),
	{EncodePart, DecodePart}.

convert_encode_part(MsgName, MsgCols) ->
	MsgNameString = atom_to_list(MsgName),
	MsgEncodeFun = "encode_"++MsgNameString++"(Input) ->\n",
	MsgBody = convert_encode_body(MsgCols, MsgNameString, []),
	MsgTail = convert_encode_tail(MsgCols),
	MsgEncodeFun++MsgBody++MsgTail.

convert_decode_part(MsgName, MsgCols) ->
	MsgNameString = atom_to_list(MsgName),
	MsgEncodeFun = "decode_"++MsgNameString++"(Input) ->\n"++
					   "\t_LastBinary0 = Input,\n",
	MsgBody = convert_decode_body(MsgCols, MsgNameString, 0, []),
	MsgTail = convert_decode_tail(MsgCols, MsgNameString),
	MsgEncodeFun++MsgBody++MsgTail.

convert_type_decode_part(MsgName, MsgCols) ->
	MsgNameString = atom_to_list(MsgName),
	MsgEncodeFun = "decode_"++MsgNameString++"(Input) ->\n"++
					   "\t_LastBinary0 = Input,\n",
	MsgBody = convert_decode_body(MsgCols, MsgNameString, 0, []),
	MsgTail = convert_type_decode_tail(MsgCols, MsgNameString),
	MsgEncodeFun++MsgBody++MsgTail.


%% encode part
convert_encode_body([#msg_attr{field_name = FieldName, base_type = 'int',
							   len = Len, type = Type}|Tail], MsgNameString, AccInfo) ->
	FieldNameString = atom_to_list(FieldName),
	FieldHead = "\t_"++FieldNameString++" = ",
	if
		Type =:= required ->
			%%  _msgid = <<(Input#login_c2s.msgid):16/signed>>
			Lan = FieldHead++"<<(Input#"++MsgNameString++"."++
					  FieldNameString++"):"++integer_to_list(Len)++
					  "/"++"signed"++">>",
			convert_encode_body(Tail, MsgNameString, [Lan|AccInfo]);
		Type =:= repeated ->
			%% _role_list = ecnode_int32_list(Input#test.role_list)
			Lan = FieldHead++"encode_"++atom_to_list('int')++integer_to_list(Len)++
					  "_list(Input#"++MsgNameString++"."++
					  FieldNameString,
			convert_encode_body(Tail, MsgNameString, [Lan|AccInfo]);
		true ->
			exit("Bad type required or repeated")
	end;
convert_encode_body([#msg_attr{field_name = FieldName, base_type = 'uint',
							   len = Len, type = Type}|Tail], MsgNameString, AccInfo) ->
	FieldNameString = atom_to_list(FieldName),
	FieldHead = "\t_"++FieldNameString++" = ",
	if
		Type =:= required ->
			%%  _msgid = <<(Input#login_c2s.msgid):16/unsigned>>
			Lan = FieldHead++"<<(Input#"++MsgNameString++"."++
					  FieldNameString++"):"++integer_to_list(Len)++
					  "/"++"unsigned"++">>",
			convert_encode_body(Tail, MsgNameString, [Lan|AccInfo]);
		Type =:= repeated ->
			%% _role_list = ecnode_uint32_list(Input#test.role_list)
			Lan = FieldHead++"encode_"++atom_to_list('uint')++integer_to_list(Len)++
					  "_list(Input#"++MsgNameString++"."++
					  FieldNameString,
			convert_encode_body(Tail, MsgNameString, [Lan|AccInfo]);
		true ->
			exit("Bad type required or repeated")
	end;
convert_encode_body([#msg_attr{field_name = FieldName, base_type = 'string',
							   len = _Len, type = Type}|Tail], MsgNameString, AccInfo) ->
	FieldNameString = atom_to_list(FieldName),
	FieldHead = "\t_"++FieldNameString++" = ",
	if
		Type =:= required ->
			%%  _test_string = encode_string(Input#test.test)
			Lan = FieldHead++"encode_string(Input#"++MsgNameString++
					  "."++FieldNameString++")",
			convert_encode_body(Tail, MsgNameString, [Lan|AccInfo]);
		Type =:= repeated ->
			%% _role_list = ecnode_string_list(Input#test.role_list)
			Lan = FieldHead++"encode_string_list(Input#"++MsgNameString++
					  "."++FieldNameString++")",
			convert_encode_body(Tail, MsgNameString, [Lan|AccInfo]);
		true ->
			exit("Bad type required or repeated")
	end;
convert_encode_body([#msg_attr{field_name = FieldName, base_type = PrivType,
							   len = _Len, type = Type}|Tail], MsgNameString, AccInfo) ->
	FieldNameString = atom_to_list(FieldName),
	FieldHead = "\t_"++FieldNameString++" = ",
	PrivTypeString = atom_to_list(PrivType),
	if
		Type =:= required ->
			%%  _priv_type = encode_privType(#Input#test.field)
			Lan = FieldHead++"encode_"++PrivTypeString++"(Input#"++
					  MsgNameString++"."++FieldNameString++")",
			convert_encode_body(Tail, MsgNameString, [Lan|AccInfo]);
		Type =:= repeated ->
			%% _field_count = length(Input#test.field),
			%% _field = lists:foldl(_cls_list_field, _cls_bin_field) ->
			%%			_new_cls_bin_field = encode_privType(_cls_list_field),
			%%			<<_cls_bin_field/binary, _new_cls_bin_field/binary>>
			%%			end,<<_field_count:16/unsigned>>, Input#test.field)
			CountString = "\t_"++FieldNameString++"_count = "++"length(Input#"++
							  MsgNameString++"."++FieldNameString++"),\n",
			Lan = CountString++FieldHead++"lists:foldl(fun(_cls_list_"++FieldNameString++
					  ", _cls_bin_"++FieldNameString++") ->\n\t\t"++
					  "_new_cls_bin_"++FieldNameString++" = encode_"++
					  PrivTypeString++"(_cls_list_"++FieldNameString++
					  "),\n\t\t"++"<<_cls_bin_"++FieldNameString++
					  "/binary, _new_cls_bin_"++FieldNameString++
					  "/binary>>\n"++"\tend,<<_"++FieldNameString++"_count:16/unsigned"++
					  ">>, Input#"++MsgNameString++"."++FieldNameString++")",
			convert_encode_body(Tail, MsgNameString, [Lan|AccInfo]);
		true ->
			exit("Bad type required or repeated")
	end;
convert_encode_body([], _MsgNameString, AccInfo) ->
	string:join(lists:reverse([""|AccInfo]), ",\n").

convert_encode_tail(SortMsgColsById) ->
	Cols = ["_"++atom_to_list(Field)++"/binary"
		   ||#msg_attr{field_name = Field}<-SortMsgColsById],
	ColsStringWithComma = string:join(Cols, ",\n\t"),
	"\t<<\n\t"++ColsStringWithComma++"\n\t>>.\n".


%% decode part
convert_decode_body([#msg_attr{field_name = FieldName, base_type = 'int',
							   len = Len, type = Type}|Tail], MsgNameString, AccIndex, AccInfo) ->
	FieldNameString = atom_to_list(FieldName),
	if
		Type =:= required ->
			%%  <<_field:16/signed, _LastBinary1/binary>> = _LastBinary0
			Lan = "\t<<_"++FieldNameString++":"++integer_to_list(Len)++
					  "/signed, _LastBinary"++integer_to_list(AccIndex+1)++
					  "/binary>> = _LastBinary"++integer_to_list(AccIndex),
			convert_decode_body(Tail, MsgNameString, AccIndex+1, [Lan|AccInfo]);
		Type =:= repeated ->
			%% {_field, _LastBinary1} = decode_int32_list(_LastBinary0)
			Lan = "\t{_"++FieldNameString++", _LastBinary"++integer_to_list(AccIndex+1)++
					  "} = decode_int"++integer_to_list(Len)++"_list(_LastBinary"++
					  integer_to_list(AccIndex)++")",
			convert_decode_body(Tail, MsgNameString, AccIndex+1, [Lan|AccInfo]);
		true ->
			exit("Bad type required or repeated")
	end;
convert_decode_body([#msg_attr{field_name = FieldName, base_type = 'uint',
							   len = Len, type = Type}|Tail], MsgNameString, AccIndex, AccInfo) ->
	FieldNameString = atom_to_list(FieldName),
	if
		Type =:= required ->
			%%  <<_field:16/unsigned, _LastBinary1/binary>> = _LastBinary0
			Lan = "\t<<_"++FieldNameString++":"++integer_to_list(Len)++
					  "/unsigned, _LastBinary"++integer_to_list(AccIndex+1)++
					  "/binary>> = _LastBinary"++integer_to_list(AccIndex),
			convert_decode_body(Tail, MsgNameString, AccIndex+1, [Lan|AccInfo]);
		Type =:= repeated ->
			%% {_field, _LastBinary1} = decode_uint32_list(_LastBinary0)
			Lan = "\t{_"++FieldNameString++", _LastBinary"++integer_to_list(AccIndex+1)++
					  "} = decode_uint"++integer_to_list(Len)++"_list(_LastBinary"++
					  integer_to_list(AccIndex)++")",
			convert_decode_body(Tail, MsgNameString, AccIndex+1, [Lan|AccInfo]);
		true ->
			exit("Bad type required or repeated")
	end;
convert_decode_body([#msg_attr{field_name = FieldName, base_type = 'string',
							   len = _Len, type = Type}|Tail], MsgNameString, AccIndex, AccInfo) ->
	FieldNameString = atom_to_list(FieldName),
	if
		Type =:= required ->
			%% {_field, _LastBinary1} = decode_string(LastBinary0)
			Lan = "\t{_"++FieldNameString++", _LastBinary"++integer_to_list(AccIndex+1)++
					  "} = decode_string(_LastBinary"++integer_to_list(AccIndex)++
					  ")",
			convert_decode_body(Tail, MsgNameString, AccIndex+1, [Lan|AccInfo]);
		Type =:= repeated ->
			%% {_field, _LastBinary1} = decode_string_list(LastBinary0)
			Lan = "\t{_"++FieldNameString++", _LastBinary"++integer_to_list(AccIndex+1)++
					  "} = decode_string_list(_LastBinary"++integer_to_list(AccIndex)++
					  ")",
			convert_decode_body(Tail, MsgNameString, AccIndex+1, [Lan|AccInfo]);
		true ->
			exit("Bad type required or repeated")
	end;
convert_decode_body([#msg_attr{field_name = FieldName, base_type = PrivType,
							   len = _Len, type = Type}|Tail], MsgNameString, AccIndex, AccInfo) ->
	FieldNameString = atom_to_list(FieldName),
	PrivTypeString = atom_to_list(PrivType),
	if
		Type =:= required ->
			%%  {_field, _LastBinary1} = decode_privType(_LastBinary0)
			Lan = "\t{_"++FieldNameString++", _LastBinary"++integer_to_list(AccIndex+1)++
					  "} = decode_"++PrivTypeString++"(_LastBinary"++
					  integer_to_list(AccIndex)++")",
			convert_decode_body(Tail, MsgNameString, AccIndex+1, [Lan|AccInfo]);
		Type =:= repeated ->
			%% <<_field_count:16/unisgned, _LastBinary1_temp/binary>> = _LastBinary0,
			%% {_field, _LastBinary1} = lists:foldl(_, {_cls_list_field, _cls_bin_field}) ->
			%%			{_new_cls_field, _new_cls_bin_field} = decode_privType(_cls_list_field),
			%%			{[_cls_list_field|_new_cls_field], _new_cls_bin_field}
			%%			end, {[], _LastBinary1_temp}, lists:seq(1, _field_count))
			CountString = "\t<<_"++FieldNameString++"_count:16/unsigned, _LastBinary"++
							  integer_to_list(AccIndex+1)++"_temp/binary>> = _LastBinary"++
							  integer_to_list(AccIndex)++",\n\t",
			Lan = CountString++"{_"++FieldNameString++", _LastBinary"++
					  integer_to_list(AccIndex+1)++"} = "++"lists:foldl(fun(_, {_cls_list_"++
					  FieldNameString++", _cls_bin_"++FieldNameString++"}) ->\n\t\t"++
					  "{_new_cls_"++FieldNameString++", _new_cls_bin_"++FieldNameString++
					  "} = decode_"++PrivTypeString++"(_cls_bin_"++FieldNameString++
					  "),\n\t\t"++"{[_cls_list_"++FieldNameString++
					  "|_new_cls_"++FieldNameString++"], _new_cls_bin_"++
					  FieldNameString++"}\n"++"\tend,{[], _LastBinary"++
					  integer_to_list(AccIndex+1)++"_temp}"++
					  ", lists:seq(1, "++"_"++FieldNameString++"_count))",
			convert_decode_body(Tail, MsgNameString, AccIndex+1, [Lan|AccInfo]);
		true ->
			exit("Bad type required or repeated")
	end;
convert_decode_body([], _MsgNameString, _AccIndex, AccInfo) ->
	string:join(lists:reverse([""|AccInfo]), ",\n").


convert_decode_tail(SortMsgColsById, MsgNameString) ->
	ColsTemp = [atom_to_list(Field)++" = _"++atom_to_list(Field)
			   ||#msg_attr{field_name=Field}<-SortMsgColsById],
	"\t#"++MsgNameString++"{\n\t\t"++string:join(ColsTemp, ",\n\t\t")++"}.\n".


convert_type_decode_tail(SortMsgColsById, MsgNameString) ->
	ColsTemp = [atom_to_list(Field)++" = _"++atom_to_list(Field)
			   ||#msg_attr{field_name=Field}<-SortMsgColsById],
	Length = erlang:length(SortMsgColsById),
	"\t{#"++MsgNameString++"{\n\t\t"++string:join(ColsTemp, ",\n\t\t")++
		"}, _LastBinary"++integer_to_list(Length)++"}.\n".


%% login_pb.erl template

-compile({inline, [login_pb_template/0]}).
'login_pb_template'() ->
"%% Author: eric.yutao
%% Description: This file is auto generated by login_pb.proto
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
    {lists:reverse(_List), _Tail}.

%% init opeate
init() ->
$INSERT_LANS.


$MSG_TYPE_CODE


$MSG_CODE
".

