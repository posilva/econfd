%%%-------------------------------------------------------------------
%%% @author Pedro Marques da Silva <posilva@gmail.com>
%%% @copyright (C) 2015, Pedro Marques da Silva
%%% @doc
%%%
%%% @end
%%% Created : 30. Sep 2015 00:14
%%%-------------------------------------------------------------------
-module(s3_example_handler).
-author("Pedro Marques da Silva <posilva@gmail.com>").

-behavior(econfd_s3_handler).
%% API
-export([on_s3_changed/1, on_error/1]).

-spec on_error(Error::any()) -> ok.
on_error(Error) ->
    lager:error("Error on file handling: ~p", [Error]),
    ok.
-spec on_s3_changed(Content::binary()) -> ok.
on_s3_changed(Content) ->
    lager:debug("Content: ~p", [Content]),
    ok.

