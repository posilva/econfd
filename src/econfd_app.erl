%%%-------------------------------------------------------------------
%%% @author Pedro Marques da Silva <posilva@gmail.com>
%%% @copyright (C) 2015, Pedro Marques da Silva
%%% @doc
%%%
%%% @end
%%% Created : 28. Sep 2015 00:25
%%%-------------------------------------------------------------------
-module(econfd_app).
-author("Pedro Marques da Silva <posilva@gmail.com>").

-behaviour(application).

%% Application callbacks
-export([start/2,
         stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
            StartArgs :: term()) ->
    {'error',_} | {'ok',pid()}).
start(_StartType, _StartArgs) ->
    %% Read Configurations
    case econfd_sup:start_link() of
        {ok, Pid} ->
            Handlers = econfd_config:get(handlers),
            start_handlers(Handlers),
            {ok, Pid};
        Error ->
            Error
    end.

-spec start_handlers([{econfd:handler_name(), econfd:handler_type(), module(), econfd:handler_options()}]) -> ok.
start_handlers(undefined) ->
    error(missing_handlers);
start_handlers([]) ->
    ok;
start_handlers([{Name, Type, ModuleHandler, Options}| T]) ->
    {ok, _} = econfd_watchdog_sup:start_whatcher(Name, ModuleHandler, Type, Options),
    ok = start_handlers(T).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop(State :: any()) -> ok).
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
