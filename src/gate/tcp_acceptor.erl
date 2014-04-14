%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is RabbitMQ.
%%
%% The Initial Developer of the Original Code is GoPivotal, Inc.
%% Copyright (c) 2007-2014 GoPivotal, Inc.  All rights reserved.
%%

-module(tcp_acceptor).

-behaviour(gen_server).

-export([start_link/3, get_proc_name/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {callback, sock, ref}).

%%--------------------------------------------------------------------

start_link(Callback, LSock, AcceptorIndex) ->
    gen_server:start_link(?MODULE, {Callback, LSock, AcceptorIndex}, []).

get_proc_name(AcceptorIndex) ->
	list_to_atom("acceptor_"++integer_to_list(AcceptorIndex)).

%%--------------------------------------------------------------------

init({Callback, LSock, Port}) ->
	erlang:register(get_proc_name(Port), self()),
    gen_server:cast(self(), accept),
    {ok, #state{callback=Callback, sock=LSock}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(accept, State) ->
%%     ok = file_handle_cache:obtain(),
    accept(State);

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({inet_async, LSock, Ref, {ok, Sock}},
			State = #state{callback={M,F,A}, sock=LSock, ref=Ref}) ->
	
	%% patch up the socket so it looks like one we got from
	%% gen_tcp:accept/1
	{ok, Mod} = inet_db:lookup_socket(LSock),
	inet_db:register_socket(Sock, Mod),
	
	%% handle
	try
		{Address, Port} = inet_op(fun() -> inet:sockname(LSock) end),
		{PeerAddress, PeerPort} = inet_op(fun() -> inet:peername(LSock) end),
		{ok, ChildPid} = supervisor:start_child(player_session_sup, []),
		ok = gen_tcp:controlling_process(Sock, ChildPid),
		apply(M, F, A++[Sock, ChildPid])
	catch
		{inet_error, Reason} ->
			gen_tcp:close(Sock),
			error_logger:error_msg("Unable to accept TCP connection:~p~n", [Reason]);
		Exp ->
			error_logger:error_msg("Unable to accept TCP connection:~p~n", [Exp])
	end,
	%% accept more
	accept(State);

handle_info({inet_async, LSock, Ref, {error, Reason}},
            State=#state{sock=LSock, ref=Ref}) ->
    case Reason of
        closed       -> {stop, normal, State}; %% listening socket closed
        econnaborted -> accept(State); %% client sent RST before we accepted
        _            -> {stop, {accept_failed, Reason}, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------

accept(State = #state{sock=LSock}) -> 
    case prim_inet:async_accept(LSock, -1) of
        {ok, Ref} -> {noreply, State#state{ref=Ref}};
        Error     -> {stop, {cannot_accept, Error}, State}
    end.

tune_buffer_size(Sock) ->
    case inet:getopts(Sock, [sndbuf, recbuf, buffer]) of
        {ok, BufSizes} -> BufSz = lists:max([Sz || {_Opt, Sz} <- BufSizes]),
                          inet:setopts(Sock, [{buffer, BufSz}]);
        Error          -> Error
    end.

throw_on_error(E, Trunk) ->
	case Trunk() of
		{error, Error} -> throw({E, Error});
		{ok, Res} -> Res
	end.

inet_op(F) -> throw_on_error(inet_error, F).
