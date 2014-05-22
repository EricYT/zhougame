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
	ets:new(msg_id_map_record_encode_login_pb, [set, named_table, public]),		%% {msgid, msgName, encode_fun, decode_fun}
	ets:new(msg_id_map_record_decode_login_pb, [set, named_table, public]).


get_encode_fun(MsgId) ->
	case ets:lookup(msg_id_map_record_encode_login_pb, MsgId) of
		[] ->
			false;
		[{MsgId, }]


init() ->
	%%Parse msg proto and generated insert code
	todo.

