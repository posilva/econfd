%%%-------------------------------------------------------------------
%%% @author Pedro Marques da Silva <posilva@gmail.com>
%%% @copyright (C) 2015, Pedro Marques da Silva
%%% @doc
%%%
%%% @end
%%% Created : 28. Sep 2015 00:37
%%%-------------------------------------------------------------------
-module(econfd_s3_handler).
-author("Pedro Marques da Silva <posilva@gmail.com>").

%% API
-export([parse_options/1, check_modified/1]).

-callback on_error(Error :: any()) -> ok.
-callback on_s3_changed(S3ObjecData :: any()) -> ok.

-include_lib("erlcloud/include/erlcloud_aws.hrl").
-record(s3_handler_state, {
    s3_bucket = "" :: string() | binary(),
    s3_key = "" :: string() | binary(),
    last_ts = 0 :: integer(),
    access_key = "" :: string(),
    secret_key = "" :: string(),
    region = "us-west-2" :: string(),
    s3_scheme = "https://" :: string(),
    s3_host = "s3-us-west-2.amazonaws.com" :: string(),
    s3_port = 80 :: non_neg_integer()
}).
-opaque state() :: #s3_handler_state{}.
-export_type([state/0, check_modified_ret/0]).

-type (check_modified_ret() :: binary() ).
%%%===================================================================
%%% API
%%%===================================================================
-spec parse_options(econfd:handler_options()) -> state().
parse_options(Options) ->
    Region = get_region_option(Options),
    #s3_handler_state{
        s3_bucket  = get_s3_bucket_option(Options),
        s3_key     = get_s3_key_option(Options),
        s3_scheme  = "https://",
        s3_host    = "s3-" ++ Region ++ ".amazonaws.com",
        access_key = get_access_key_option(Options),
        secret_key = get_secret_key_option(Options),
        region     = Region
    }.

-spec check_modified(state()) -> false | {true , check_modified_ret(), state()} | {error, any()}.
check_modified(S3HandlerState) ->
    case catch get_s3_metadata(S3HandlerState) of
        {'EXIT', Reason} ->
            {error, Reason};
        [] ->
            false;
        Metadata ->
            lager:debug("S3 metadata: ~p", [Metadata]),
            LastModifiedUnixTS  = extract_timestamp(Metadata),
            case LastModifiedUnixTS >= S3HandlerState#s3_handler_state.last_ts of
                true ->
                    case catch get_s3_object(S3HandlerState) of
                        {'EXIT', Reason} ->
                            {error, Reason};
                        [] ->
                            {error, "failed to get s3 object"};
                        ObjectData ->
                            Content = proplists:get_value(content, ObjectData),
                            {true, Content, S3HandlerState#s3_handler_state{
                                last_ts = LastModifiedUnixTS
                            }}
                    end;
                false ->
                    false
            end
    end.

-spec extract_timestamp(proplists:proplist()) -> integer().
extract_timestamp(Metadata) ->
    case proplists:get_value('last_modified', Metadata) of
        undefined ->
            0;
        Other ->
            qdate:to_unixtime(qdate:to_date(Other))

end .
%%%===================================================================
%%% API
%%%===================================================================
-spec get_region_option(econfd:handler_options()) -> any().
get_region_option(Options) ->
    proplists:get_value(aws_region, Options, "us-west-2").

-spec get_secret_key_option(econfd:handler_options()) -> any().
get_secret_key_option(Options) ->
    proplists:get_value(aws_secret_key, Options).

-spec get_access_key_option(econfd:handler_options()) -> any().
get_access_key_option(Options) ->
    proplists:get_value(aws_access_key, Options).

-spec get_s3_bucket_option(econfd:handler_options()) -> any().
get_s3_bucket_option(Options) ->
    case proplists:get_value(s3_bucket, Options) of
        undefined -> error(invalid_option__missing_s3_bucket);
        P -> P
    end.

-spec get_s3_key_option(econfd:handler_options()) -> any().
get_s3_key_option(Options) ->
    case proplists:get_value(s3_key, Options) of
        undefined -> error(invalid_option__missing_s3_key);
        P -> P
    end.

-spec get_s3_metadata(state()) -> proplists:proplist().
get_s3_metadata(S3HandlerState) ->
    erlcloud_s3:get_object_metadata(
        S3HandlerState#s3_handler_state.s3_bucket,
        S3HandlerState#s3_handler_state.s3_key,
        get_config(S3HandlerState)).

-spec get_s3_object(state()) -> proplists:proplist().
get_s3_object(S3HandlerState) ->
    erlcloud_s3:get_object(
        S3HandlerState#s3_handler_state.s3_bucket,
        S3HandlerState#s3_handler_state.s3_key,
        get_config(S3HandlerState)).

-spec get_config(state()) -> aws_config().
get_config(S3HandlerState) ->
    #aws_config{
        access_key_id     = S3HandlerState#s3_handler_state.access_key,
        secret_access_key = S3HandlerState#s3_handler_state.secret_key,
        s3_host           = S3HandlerState#s3_handler_state.s3_host,
        s3_scheme         = S3HandlerState#s3_handler_state.s3_scheme,
        s3_port           = S3HandlerState#s3_handler_state.s3_port
    }.