%%%-------------------------------------------------------------------
%%% @author Pedro Marques da Silva <posilva@gmail.com>
%%% @copyright (C) 2015, Pedro Marques da Silva
%%% @doc
%%%
%%% @end
%%% Created : 30. Sep 2015 00:14
%%%-------------------------------------------------------------------
-module(http_example_handler).
-author("Pedro Marques da Silva <posilva@gmail.com>").

-behavior(econfd_http_handler).
%% API
-export([on_http_changed/1, on_error/1]).

-spec on_error(Error::any()) -> ok.
on_error(Error) ->
    lager:error("Error on file handling: ~p", [Error]),
    ok.
-spec on_http_changed(Content::binary()) -> ok.
on_http_changed(Content) ->
    lager:debug("Content: ~p", [Content]),
    ok.

