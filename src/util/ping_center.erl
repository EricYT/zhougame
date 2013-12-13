-module(ping_center).

-export([
	wait_all_nodes_connect/0,
	wait_all_nodes_connect/1,
	ping/1,
	wait_node_connect/1
	]).


wait_all_nodes_connect() ->
	AllNodes = env:get(pre_connect_nodes, []),
    AllFullNodes = convert_node_name(AllNodes, []),
    debug:info("Ping_center ~p~n", [{AllNodes, AllFullNodes, erlang:node()}]),
	wait_nodes(AllFullNodes).


wait_all_nodes_connect(Flag) ->
    AllNodes = env:get(pre_connect_nodes, []),
    AllFullNodes = convert_node_name(AllNodes, []),
    debug:info("Ping_center ~p~n", [{AllNodes, Flag}]),
	wait_nodes(AllFullNodes, Flag).

wait_node_connect(Type) ->
	NeedConNodes = lists:filter(fun(Node) ->
					node_util:check_snode_match(Type, Node) end
			, env:get(pre_connect_nodes, [])),
	wait_nodes(NeedConNodes).


wait_nodes([]) ->
    io:format("************** ping ok ****************** ~n"),
	ok;
wait_nodes([Node|Tail]) when Node =/= node() ->
	debug:info("Need wait nodes ~p~n", [Node]),
	ping(Node),
	wait_nodes(Tail);
wait_nodes([_|Rest]) ->
    wait_nodes(Rest).


wait_nodes([], _) ->
	ok;
wait_nodes([Node|Tail], Flag) when Flag ->
	case string:str(erlang:atom_to_list(Node), "control") of
		0 -> ping(Node);
		_ -> ping(Node, 3)
	end,
	wait_nodes(Tail, Flag);
wait_nodes([Node|Tail], Flag) ->
	debug:info("Need wait nodes ~p~n", [Node]),
	ping(Node, 3),
	wait_nodes(Tail, Flag).



ping(Node) ->
	ping_loop(Node).


ping(Node, Count) ->
	ping_loop(Node, Count).


ping_loop(Node) ->
    debug:info("Ping_center ~p~n", [{Node}]),
    %% create link with other node
    case net_adm:ping(Node) of
        pong -> debug:info("Ping_center ok  ~p~n", [{Node}]), ok;
        pang ->
            receive
                after 1000 -> ping_loop(Node)
            end
    end.


ping_loop(_, 0) ->
	ok;
ping_loop(Node, Count) ->
	%% create link with other node
	case net_adm:ping(Node) of
		pong -> ok;
		_ ->
			receive
			after 1000 -> ping_loop(Node, Count-1)
			end
	end.


convert_node_name([], AccNodes) ->
    lists:reverse(AccNodes);
convert_node_name([Node|Rest], AccNodes) ->
    convert_node_name(Rest, [node_util:make_node_full_name(Node)|AccNodes]).


