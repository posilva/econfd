%%%-------------------------------------------------------------------
%%% @author Pedro Marques da Silva <posilva@gmail.com>
%%% @copyright (C) 2015, Pedro Marques da Silva
%%% @doc
%%%
%%% @end
%%% Created : 28. Sep 2015 12:40
%%%-------------------------------------------------------------------
-module(econfd_watchdog_sup).
-author("Pedro Marques da Silva <posilva@gmail.com>").

-behaviour(supervisor).

%% API
-export([start_link/0, start_whatcher/4]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(HANDLERS_TABLE, handlers_table).
%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
                       MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
          [ChildSpec :: supervisor:child_spec()]
    }}).
init([]) ->
    create_table(),

    {ok, {{simple_one_for_one, 10, 60},
          [
              {econfd_watchdog, {econfd_watchdog, start_link, []},
               temporary, 5000, worker, [econfd_watchdog]}
          ]}}.

-spec(start_whatcher(econfd:handler_name(), module(), econfd:handler_type(), econfd:handler_options()) ->
    {ok, Pid :: pid()} | {error, Reason :: term()}).
start_whatcher(Name, Module, Type, Options) ->
    maybe_start_handler(handler_exists(Name), Name, Module, Type, Options).

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec maybe_start_handler(boolean(), econfd:handler_name(), module(), econfd:handler_type(), econfd:handler_options()) -> supervisor:startchild_ret().
maybe_start_handler(false, Name, Module, Type, Options) ->
    insert_handler(Name, Module, Type),
    supervisor:start_child(?MODULE, [Name, Module, Type, Options]);
maybe_start_handler(true, _Name, _Module, _Type, _Options) ->
    error(<<"Handler already exists with the same name">>).

-spec insert_handler(econfd:handler_name(), module(), econfd:handler_type()) -> boolean().
insert_handler(Name, Module, Type) ->
    ets:insert_new(?HANDLERS_TABLE, {{handler_name, Name}, {Module, Type}}).

-spec handler_exists(econfd:handler_name()) -> boolean().
handler_exists(Name) ->
    case ets:lookup(?HANDLERS_TABLE, {handler_name, Name}) of
        [] ->
            false;
        [{{handler_name, Name}, _Res}] ->
            true
    end.

-spec create_table() -> ok.
create_table() ->
    _ = try ets:new(?HANDLERS_TABLE,
                    [named_table, public, set, {keypos, 1}, {read_concurrency, true}]) of
            _Result ->
                ok
        catch
            error:badarg ->
                lager:warning("Table ~p already exists", [?HANDLERS_TABLE])
        end.
