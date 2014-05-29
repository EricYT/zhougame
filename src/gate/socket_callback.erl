%% @author eric.yutao
%% @doc @todo Add description to socket_callback.


-module(socket_callback).

-include("login_pb.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([on_receive_client_packet/3]).



%% ====================================================================
%% Internal functions
%% ====================================================================
on_receive_client_packet(GatePid, Binary, Pid) ->
	try
		Term = login_pb:decode(Binary),
		MsgId = erlang:element(2, Term),
		case login_pb:get_record_info(MsgId) of
			false ->
				io:format(">>>>>>>>>>>> error msgid: ~p~n", [{?MODULE, ?LINE, MsgId}]);
			{ReceName, Mod} ->
				Mod:dispath(erlang:setelement(1,Term,ReceName),GatePid,Pid)
		end
	catch E:R ->
			io:format(">>>>>>>>>>>> on_receive_client_packet: ~p~n", [{?MODULE, ?LINE, E, R}])
	end.

