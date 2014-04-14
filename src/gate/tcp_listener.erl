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

-module(tcp_listener).

-behaviour(gen_server).

-include("network_define.hrl").

-export([start_link/4]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {sock, on_startup, on_shutdown, acceptors}).

%%----------------------------------------------------------------------------

%%--------------------------------------------------------------------

start_link(Port, ConcurrentAcceptorCount, OnStartup, OnShutdown) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {Port, ConcurrentAcceptorCount, OnStartup, OnShutdown}, []).

%%--------------------------------------------------------------------

init({Port, ConcurrentAcceptorCount, {M,F,A} = OnStartup, OnShutdown}) ->
    process_flag(trap_exit, true),
	Opts = ?TCP_OPTIONS,
	io:format(">>>>>>>>>>>>>> ~p~n", [{?MODULE, ?LINE, Port, ConcurrentAcceptorCount, {M,F,A} = OnStartup, OnShutdown}]),
    case gen_tcp:listen(Port, Opts) of
        {ok, LSock} ->
            Fun = fun (AccIndex, Acc) ->
                                  {ok, _APid} = supervisor:start_child(tcp_acceptor_sup, [LSock, AccIndex]),
								  AcceptorName = tcp_acceptor:get_proc_name(AccIndex),
								  [AcceptorName|Acc]
                          end,
			AccProcs = lists:foldl(Fun, [], lists:seq(1, ConcurrentAcceptorCount)),
            apply(M, F, A ++ [Port, LSock]),
            {ok, #state{sock = LSock, on_startup = OnStartup, on_shutdown = OnShutdown,
                        acceptors = AccProcs}};
        {error, Reason} ->
            error_logger:error_msg(
              "failed to start ~s on ~s:~p - ~p (~s)~n",
              [Reason]),
            {stop, {cannot_listen, Port, Reason}}
    end.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{sock=LSock, on_shutdown = {M,F,A}}) ->
    {ok, {IPAddress, Port}} = inet:sockname(LSock),
    gen_tcp:close(LSock),
    error_logger:info_msg("stopped ~s on ~s:~p~n", [Port]),
    apply(M, F, A ++ [IPAddress, Port]).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


