%% Author: Eric.yu
%% Created: 2013-9-7
%% Description: TODO: Add description to time_util
-module(time_util).

%%
%% Include files
%%
-define(SECONDS_PER_MINUTE, 60).
-define(SECONDS_PER_HOUR, 3600).
-define(SECONDS_PER_DAY, 86400).
-define(DAYS_PER_YEAR, 365).
-define(DAYS_PER_LEAP_YEAR, 366).
-define(DAYS_PER_4YEARS, 1461).
-define(DAYS_PER_100YEARS, 36524).
-define(DAYS_PER_400YEARS, 146097).
-define(DAYS_FROM_0_TO_1970, 719528).


%%
%% type define
-type now_type() :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}.



%%
%% Exported Functions
%%
-export([
		 check_same_day/2,
		 check_same_week/2,
		 now_to_s/1,
		 s_to_now/1,
		 now_diff/2,
         date_time_to_now/1
		 ]).

%%
%% API Functions
%%
-spec check_same_day(Now1, Now2) -> boolean() when
												Now1 :: now_type(),
												Now2 :: now_type().
check_same_day({_, _, _}=N1, {_, _, _}=N2) ->
	{D1, _} = calendar:now_to_local_time(N1),
	{D2, _} = calendar:now_to_local_time(N2),
	D1 =:= D2.
	


-spec check_same_week(Now1, Now2) -> boolean() when
												 Now1 :: now_type(),
												 Now2 :: now_type().
check_same_week({_, _, _}=N1, {_, _, _}=N2) ->
	{{Y1, M1, D1}, _} = calendar:now_to_local_time(N1),
	{{Y2, M2, D2}, _} = calendar:now_to_local_time(N2),
	calendar:day_of_the_week(Y1, M1, D1) =:= calendar:day_of_the_week(Y2, M2, D2).


-spec now_to_s(Now) -> non_neg_integer() when
										   Now :: now_type().
now_to_s({MS, S, _}) ->
	MS*1000000 + S.


-spec s_to_now(S::integer()) -> Now when
									  Now :: now_type().
s_to_now(S) when erlang:is_integer(S), S >= 0 ->
	{S div 1000000, S - (S div 1000000)*1000000, 0}.


-spec now_diff(T1, T2) -> integer() when
									  T1 :: now_type(),
									  T2 :: now_type().
now_diff({_, _, _}=T1, {_, _, _}=T2) ->
	timer:now_diff(T1, T2) div 1000000;
now_diff(_, _) ->
	error(badarg).

-spec date_time_to_now(DateTime) -> {integer(), integer(), integer()} when
                                                                        DateTime :: tuple().
date_time_to_now({{_, _, _}, {_, _, _}}=DateTime) ->
%%     DateT = calendar:now_to_datetime(now()),
    Seconds = calendar:datetime_to_gregorian_seconds(DateTime),
    RightSeconds = Seconds - ?DAYS_FROM_0_TO_1970*?SECONDS_PER_DAY,
%%     io:format("date time ~p~n", [{DateTime, now(), {RightSeconds div 1000000, RightSeconds rem 1000000, 0}}]),
    {RightSeconds div 1000000, RightSeconds rem 1000000, 0}.




%%
%% Local Functions
%%

