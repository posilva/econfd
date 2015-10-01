%%%-------------------------------------------------------------------
%%% @author Pedro Marques da Silva <posilva@gmail.com>
%%% @copyright (C) 2015, Pedro Marques da Silva
%%% @doc
%%%
%%% @end
%%% Created : 27. Sep 2015 22:53
%%%-------------------------------------------------------------------
-module(econfd_utils).
-author("Pedro Marques da Silva <posilva@gmail.com>").

%% API
-export([ts_changed/2, md5/1, get_base_handler/1]).


ts_changed(PreviousTS, CurrentTS) ->
    calendar:datetime_to_gregorian_seconds(CurrentTS) > calendar:datetime_to_gregorian_seconds(PreviousTS).

-spec md5(Data :: iodata()) -> string().
md5(Data) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(Data))]).

-spec get_base_handler(econfd:handler_type()) -> econfd_file_handler | econfd_http_handler | econfd_s3_handler.
get_base_handler(file) ->
    econfd_file_handler;
get_base_handler(http) ->
    econfd_http_handler;
get_base_handler(s3) ->
    econfd_s3_handler.
