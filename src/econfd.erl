%%%-------------------------------------------------------------------
%%% @author Pedro Marques da Silva <posilva@gmail.com>
%%% @copyright (C) 2015, Pedro Marques da Silva
%%% @doc
%%%
%%% @end
%%% Created : 27. Sep 2015 21:49
%%%-------------------------------------------------------------------
-module(econfd).
-author("Pedro Marques da Silva <posilva@gmail.com>").

-include_lib("kernel/include/file.hrl").


-export_type([handler_name/0, handler_type/0, handler_options/0, opt_file_path/0, opt_url/0]).

%% Common options
-type(opt_interval() :: {interval_ms, non_neg_integer()}).
-type(common_option() :: opt_interval()).
%% File options
-type(opt_file_path() :: {file_path, file:filename_all()}).
-type(handler_file_option() :: opt_file_path()).
%% HTTP options
-type(opt_url() :: {url, string() | binary()}).
-type(handler_http_option() :: opt_url()).
%% S3 options
-type(opt_s3_object() :: {s3_object, string()}).
-type(handler_s3_option() :: opt_s3_object()).

-type(handler_options() :: list(common_option() | handler_http_option() | handler_s3_option() | handler_file_option())).
-type(handler_name() :: string() | atom() | binary()).
-type(handler_type() :: http | s3 | file).

%% API
-export([start/0, add_watcher/4]).


-spec start() -> {'error',{atom(),_}} | {'ok',[atom()]}.
start() ->
    application:ensure_all_started(econfd).

-spec add_watcher(econfd:handler_name(),module(),econfd:handler_type(),econfd:handler_options()) -> {'ok',Pid::pid()} | {'error',Reason::term()}.
add_watcher(Name, Type, Callback, Options) ->
    econfd_watchdog_sup:start_whatcher(Name, Callback, Type, Options).

