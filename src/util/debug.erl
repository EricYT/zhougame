%% Author: Eric.yu
%% Created: 2013-9-21
%% Description: TODO: Add description to debug
-module(debug).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
		 info/2,
		 info/1,
		 error/1,
		 error/2,
		 log_file/1
		 ]).

-define(MF_FMT(FMAT), "(~p ~p) " ++ FMAT).
-define(MF_DATA(DATA), [?MODULE, ?LINE] ++ DATA).

%%
%% API Functions
%%
%% -define(debug, ok).
-ifdef(debug).
	info(Format, Data) ->
		io:format(?MF_FMT(Format), ?MF_DATA(Data)),
		error_logger:info_msg(?MF_FMT(Format), ?MF_DATA(Data)).

	info(Format) ->
		io:format(?MF_FMT(Format), ?MF_DATA([])),
		error_logger:info_msg(?MF_FMT(Format), ?MF_DATA([])).

	error(Format, Data) ->
		io:format(?MF_FMT(Format), ?MF_DATA(Data)),
		error_logger:error_msg(?MF_FMT(Format), ?MF_DATA(Data)).

	error(Format) ->
		io:format(?MF_FMT(Format), ?MF_DATA([])),
		error_logger:error_msg(?MF_FMT(Format), ?MF_DATA([])).
	
	log_file(Filename) ->
		try
			%% filelib:ensure just create thd dir like: filename :: /dir/dir/dir/file  --> /dir/dir/dir
			%% filelib:ensure just create thd dir like: filename :: /dir/dir/dir/dir/  --> /dir/dir/dir/dir
			filelib:ensure_dir(Filename),
			error_logger:logfile({open, Filename})
		catch
			E:R ->
				io:format("Debug Error ~p Reason ~p~n", [E, R])
		end.

-else.
	info(_Format, _Data) ->
		ok.

	info(_Format) ->
		ok.

	error(Format, Data) ->
		error_logger:error_msg(?MF_FMT(Format), ?MF_DATA(Data)).

	error(Format) ->
		error_logger:error_msg(?MF_FMT(Format), ?MF_DATA([])).

	log_file(Filename) ->
		try
			%% filelib:ensure just create thd dir like: filename :: /dir/dir/dir/file  --> /dir/dir/dir
			%% filelib:ensure just create thd dir like: filename :: /dir/dir/dir/dir/  --> /dir/dir/dir/dir
			filelib:ensure_dir(Filename),
			error_logger:logfile({open, Filename}),
			gen_event:delete_handler(error_logger, error_logger_tty_h, [])
		catch
			E:R ->
				io:format("Debug Error ~p Reason ~p~n", [E, R])
		end.

-endif.
