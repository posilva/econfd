%%%-------------------------------------------------------------------
%%% @author Pedro Marques da Silva <posilva@gmail.com>
%%% @copyright (C) 2015, Pedro Marques da Silva
%%% @doc
%%%
%%% @end
%%% Created : 28. Sep 2015 12:52
%%%-------------------------------------------------------------------
-module(econfd_config).
-author("Pedro Marques da Silva <posilva@gmail.com>").

-export([
    get/1,
    get/2,
    set/2
]).

-define(APPLICATION_ATOM, econfd).

-ignore_xref([{get, 2},
              {set, 2}]).

-spec get(K :: atom()) -> V :: any().
get(K) ->
    case application:get_env(?APPLICATION_ATOM, K) of
        {ok, V} -> V;
        _ -> exit({config_key_missing, K})
    end.

-spec get(K :: atom(), Default :: any()) -> V :: any().
get(K, Default) ->
    case application:get_env(?APPLICATION_ATOM, K) of
        {ok, V} -> V;
        _ -> Default
    end.

-spec set(atom(), any()) -> ok.
set(K, V) ->
    application:set_env(?APPLICATION_ATOM, K, V).