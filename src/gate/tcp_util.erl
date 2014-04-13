%% Author: Eric.yutao
%% Created: 2014-4-13
%% Description: TODO: Add description to tcp_util
-module(tcp_util).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([]).
-compile(export_all).

%%
%% API Functions
%%
%%@doc
ntoa({0,0,0,0,0,16#ffff,AB,CD}) ->
    inet_parse:ntoa({AB bsr 8, AB rem 256, CD bsr 8, CD rem 256});
ntoa(IP) ->
    inet_parse:ntoa(IP).

ntoab(IP) ->
    Str = ntoa(IP),
    case string:str(Str, ":") of
        0 -> Str;
        _ -> "[" ++ Str ++ "]"
    end.


tcp_name(Prefix, IPAddress, Port)
  when is_atom(Prefix) andalso is_number(Port) ->
    list_to_atom(
      format("~w_~s:~w", [Prefix, inet_parse:ntoa(IPAddress), Port])).

format_inet_error(E) -> format("~w (~s)", [E, format_inet_error0(E)]).

format_inet_error0(address) -> "cannot connect to host/port";
format_inet_error0(timeout) -> "timed out";
format_inet_error0(Error)   -> inet:format_error(Error).


%%
%% Local Functions
%%

