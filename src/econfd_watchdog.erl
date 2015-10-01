%%%-------------------------------------------------------------------
%%% @author Pedro Marques da Silva <posilva@gmail.com>
%%% @copyright (C) 2015, Pedro Marques da Silva
%%% @doc
%%%
%%% @end
%%% Created : 28. Sep 2015 07:21
%%%-------------------------------------------------------------------
-module(econfd_watchdog).
-author("Pedro Marques da Silva <posilva@gmail.com>").

-behaviour(gen_server).
-include("econfd.hrl").
%% API
-export([start_link/4]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(DEFAULT_INTERVAL_MS, 60000).
-define(SERVER, ?MODULE).

-record(watchdog_state, {
    init_delay = 0 :: non_neg_integer(),
    timer_ref :: reference(),
    callback :: module(),
    last_check_ts = 0 :: non_neg_integer(),
    name :: econfd:handler_name(),
    type :: econfd:handler_type(),
    interval_ms :: non_neg_integer(),
    state = init :: init| ready | error | throttle,
    %%% Specific fields for handler
    handler_state :: econfd_file_handler:state() | econfd_http_handler:state()
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(econfd:handler_name(), module(), econfd:handler_type(), econfd:handler_options()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Name, Module, Type, Options) ->
    gen_server:start_link({local, Name}, ?MODULE,
                          [Name, Module, Type, Options], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: [any(),...]) ->
    {ok, State :: #watchdog_state{}} | {stop, Reason :: any()}).

init([Name, Module, Type, Options]) ->
    Return = init_watchdog(Name, Module, Type, Options),
    gen_server:cast(self(), schedule_watch),
    Return.

init_watchdog(Name, Module, Type, Options) ->
    case catch begin
                   lager:debug("Starting watchdog: ~p",
                               [{Name, Module, file, Options}]),
                   #watchdog_state{
                       init_delay    = get_init_delay(Options),
                       interval_ms   = get_interval(Options),
                       callback      = Module,
                       type          = Type,
                       name          = Name,
                       handler_state = get_handler_state(Type, Options)
                   }
               end of
        {'EXIT', Why} ->
            lager:critical("Failed to init {~p, ~p , ~p } Watchdog: ~p",
                           [file, Name, Options, Why]),
            {stop, Why};
        #watchdog_state{init_delay = Delay} = State ->
            start_watch(Delay),
            {ok, State}
    end.

get_handler_state(Type, Options) ->
    Handler = econfd_utils:get_base_handler(Type),
    Handler:parse_options(Options).

start_watch(Delay) ->
    lager:debug("Starting watch in ~p ms", [Delay]),
    do_watch(Delay).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #watchdog_state{}) ->
                     {reply, Reply :: term(), NewState :: #watchdog_state{}}).
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #watchdog_state{}) ->
    {noreply, NewState :: #watchdog_state{}}).

handle_cast(_Request, State) ->
    {noreply, State}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #watchdog_state{}) ->
    {noreply, NewState :: #watchdog_state{}}).
handle_info(schedule_watch, #watchdog_state{type        = Type,
                                            interval_ms = TS} = State) ->
    NewState = case check_source(State) of
                   ok ->
                       State;
                   {notify, Data, S} ->
                       S2 = State#watchdog_state{
                           handler_state = S
                       },
                       notify_handlers(Type, Data, S2),
                       S2;
                   {error, Why} ->
                       notify_handlers_about_error(State, Why),
                       State
               end,
    {noreply, NewState#watchdog_state{
        timer_ref     = do_watch(TS),
        last_check_ts = qdate:to_unixtime(os:timestamp())
    }};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #watchdog_state{}) -> ok).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #watchdog_state{},
                  Extra :: term()) ->
                     {ok, NewState :: #watchdog_state{}}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec do_watch(non_neg_integer()) -> reference().
do_watch(TS) ->
    lager:debug("Scheduling watch: ~p", [TS]),
    erlang:send_after(TS, self(), schedule_watch).

-spec notify_handlers(econfd:handler_type(), binary(), #watchdog_state{}) -> ok.
notify_handlers(file, Data, #watchdog_state{callback = CallbackHandler} =
                             State) ->
    lager:info("Notify file handlers ~p ", [State]),
    CallbackHandler:on_file_changed(Data);
notify_handlers(http, Data, #watchdog_state{callback = CallbackHandler} =
                            State) ->
    lager:info("Notify http handlers ~p ", [State]),
    CallbackHandler:on_http_changed(Data);
notify_handlers(s3, Data, #watchdog_state{callback = CallbackHandler} =
                          State) ->
    lager:info("Notify s3 handlers ~p ", [State]),
    CallbackHandler:on_s3_changed(Data).

-spec check_source(#watchdog_state{}) -> ok | {notify, any(), any()} | {error, any()}.
check_source(#watchdog_state{handler_state = FileHandlerState, type = Type} =
             _State) ->
    Handler = econfd_utils:get_base_handler(Type),
    case Handler:check_modified(FileHandlerState) of
        {true, ResponseData, NewState} ->
            {notify, ResponseData, NewState};
        false ->
            ok;
        {error, Why} ->
            {error, Why}
    end.

-spec notify_handlers_about_error(#watchdog_state{}, any()) -> any().
notify_handlers_about_error(#watchdog_state{callback = CallbackHandler} =
                            _State, Why) ->
    lager:info("Notify handlers about error ~p ", [Why]),
    CallbackHandler:on_error(Why).

-spec get_interval([econfd:handler_options()]) -> non_neg_integer().
get_interval(Options) ->
    proplists:get_value(interval_ms, Options, ?DEFAULT_INTERVAL_MS).

-spec get_init_delay([econfd:handler_options()]) -> non_neg_integer().
get_init_delay(Options) ->
    proplists:get_value(init_delay, Options, 0).
