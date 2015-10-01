%%%-------------------------------------------------------------------
%%% @author Pedro Marques da Silva <posilva@gmail.com>
%%% @copyright (C) 2015, Pedro Marques da Silva
%%% @doc
%%%
%%% @end
%%% Created : 28. Sep 2015 00:34
%%%-------------------------------------------------------------------
-module(econfd_file_handler).
-author("Pedro Marques da Silva <posilva@gmail.com>").
%% Callbacks
-callback on_error(Error :: any()) -> ok.
-callback on_file_changed(FileHandle :: file:filename_all()) -> ok.
%% API
-export([parse_options/1, get_file_path/1, check_modified/1]).
%% Records
-record(file_handler_state, {
    path :: file:filename_all(),
    last_ts = 0 :: non_neg_integer()
}).
-opaque state() :: #file_handler_state{}.
%% Types
-export_type([state/0, check_modified_ret/0]).
-type (check_modified_ret() :: file:filename_all()).

%%%===================================================================
%%% API
%%%===================================================================
-spec check_modified(state()) -> false | {true, check_modified_ret(), state()} | {error, any()}.
check_modified(FileHandlerState) ->
    Path = econfd_file_handler:get_file_path(FileHandlerState),
    AbsPath = filename:absname(Path),
    lager:debug("Checking source for file handler ~p ", [AbsPath]),
    case filelib:last_modified(AbsPath) of
        0 ->
            lager:error("File path does not exist"),
            {error, missing_file};
        DateTime when is_tuple(DateTime) ->
            LastUnixTs = qdate:to_unixtime(qdate:to_date(DateTime)),
            case LastUnixTs =/= FileHandlerState#file_handler_state.last_ts of
                true ->
                    {true, FileHandlerState#file_handler_state.path, FileHandlerState#file_handler_state{last_ts = LastUnixTs}};
                false ->
                    false
            end
    end.

-spec parse_options(econfd:handler_options()) -> state().
parse_options(Options) ->
    #file_handler_state{
        path = get_file_path_option(Options)
    }.
%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec get_file_path(state()) -> file:filename_all().
get_file_path(FileHandlerState) ->
    FileHandlerState#file_handler_state.path.

-spec get_file_path_option(econfd:handler_options()) -> binary() | maybe_improper_list().
get_file_path_option(Options) ->
    case proplists:get_value(file_path, Options) of
        FilePath when is_binary(FilePath) orelse is_list(FilePath) ->
            FilePath;
        undefined ->
            error(missing_opt__file_path);
        _ ->
            error(invalid_opt__file_path)
    end.