%%%-------------------------------------------------------------------
%%% @author Pedro Marques da Silva <posilva@gmail.com>
%%% @copyright (C) 2015, Pedro Marques da Silva <posilva@gmail.com>
%%% @doc
%%%
%%% @end
%%% Created : 30. Sep 2015 00:14
%%%-------------------------------------------------------------------
-module(file_example_handler).
-author("Pedro Marques da Silva <posilva@gmail.com>").

-behavior(econfd_file_handler).
%% API
-export([on_file_changed/1, on_error/1]).

-spec on_error(Error::any()) -> ok.
on_error(Error) ->
    lager:error("Error on file handling: ~p", [Error]),
    ok.
-spec on_file_changed(FileHandle::file:filename_all()) -> ok.
on_file_changed(FileHandle) ->
    Content = readlines(FileHandle),
    lager:debug("Content: ~p", [Content]),
    ok.

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    Data.

