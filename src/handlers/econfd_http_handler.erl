%%%-------------------------------------------------------------------
%%% @author Pedro Marques da Silva <posilva@gmail.com>
%%% @copyright (C) 2015, Pedro Marques da Silva
%%% @doc
%%%
%%% @end
%%% Created : 28. Sep 2015 00:38
%%%-------------------------------------------------------------------
-module(econfd_http_handler).
-author("Pedro Marques da Silva <posilva@gmail.com>").

%% API
-export([parse_options/1, check_modified/1]).

-callback on_error(Error :: any()) -> ok.
-callback on_http_changed(Content :: binary()) -> ok.

-record(http_handler_state, {
    url = "" :: string() | binary(),
    md5 = "" :: string()
}).
-opaque state() :: #http_handler_state{}.

-export_type([state/0, check_modified_ret/0]).


-type (check_modified_ret() :: binary()).

%%%===================================================================
%%% API
%%%===================================================================
-spec parse_options(econfd:handler_options()) -> state().
parse_options(Options) ->
    #http_handler_state{
        url = get_url_option(Options)
    }.

-spec check_modified(state()) -> false | {true, check_modified_ret(), state()} | {error, any()}.
check_modified(HttpState) ->
    case http_get(HttpState) of
        {ok, Content} ->
            case content_changed(HttpState, Content) of
                {true, NewMD5} ->
                    {true, term_to_binary(Content),
                     HttpState#http_handler_state{
                         md5 = NewMD5
                     }};
                {false, _} ->
                    false
            end;
        {error, Why} ->
            {error, Why}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec get_url_option(econfd:handler_options()) -> binary() | maybe_improper_list().
get_url_option(Options) ->
    case proplists:get_value(url, Options) of
        FilePath when is_binary(FilePath) orelse is_list(FilePath) ->
            FilePath;
        undefined ->
            error(missing_opt__url);
        _ ->
            error(invalid_opt__url)
    end.

-spec content_changed(state(), binary()) -> {boolean(), string()}.
content_changed(HttpState, Content) ->
    CurrentMd5 = HttpState#http_handler_state.md5,
    NewMd5 = econfd_utils:md5(Content),
    {term_to_binary(CurrentMd5) =/= term_to_binary(NewMd5), NewMd5}.

-spec http_get(state()) -> {ok, binary()}  | {error, any()}.
http_get(HttpState) ->
    lager:debug("Getting http request: ~p", [HttpState#http_handler_state.url]),
    case catch httpc:request(get, {HttpState#http_handler_state.url, []}, [],
                             []) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
            {ok, term_to_binary(Body)};
        {error, Reason} ->
            {error, Reason};
        R ->
            {error, R}
    end.
