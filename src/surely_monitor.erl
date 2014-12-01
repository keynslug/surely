-module(surely_monitor).
-behaviour(gen_server).

%% API

-export([start_link/1]).
-export([subscribe/2]).
-export([unsubscribe/2]).

%% gen_server

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% API

-spec start_link(node()) -> {ok, pid()}.

start_link(Node) ->
    Name = construct_name(Node),
    gen_server:start_link({local, Name}, ?MODULE, Node, []).

subscribe(Node, SubscriberPid) ->
    gen_server:call(construct_name(Node), {subscribe, SubscriberPid}, infinity).

unsubscribe(Node, SubscriberPid) ->
    gen_server:call(construct_name(Node), {unsubscribe, SubscriberPid}, infinity).

construct_name(Node) ->
    list_to_atom(?MODULE_STRING ++ "_" ++ atom_to_list(Node)).

%% gen_server

-record(state, {node, subs, pid, last}).

init(Node) ->
    lager:info("starting up for: ~p ...", [Node]),
    _ = process_flag(trap_exit, true),
    {ok, #state{node = Node, subs = [], pid = gather_stats()}}.

handle_call({subscribe, Pid}, _From, State) ->
    reply(do_subscribe(Pid, State));

handle_call({unsubscribe, Pid}, _From, State) ->
    reply(do_unsubscribe(Pid, State));

handle_call(Request, _From, State) ->
    lager:warning("unexpected call: ~p", [Request]),
    reply({error, State}).

handle_cast(Msg, State) ->
    lager:warning("unexpected cast: ~p", [Msg]),
    noreply({ok, State}).

handle_info({'EXIT', Pid, {stats, Stats}}, State = #state{pid = Pid}) ->
    noreply(do_broadcast(Stats, State#state{pid = gather_stats(), last = Stats}));

handle_info({'EXIT', Pid, Error}, State = #state{pid = Pid}) ->
    {stop, Error, State};

handle_info({'EXIT', Pid, _Reason}, State) ->
    noreply(do_unsubscribe(Pid, State));

handle_info(Info, State) ->
    lager:warning("unexpected info: ~p", [Info]),
    noreply({ok, State}).

terminate(_Reason, _State) ->
    lager:info("no more subscribers, shutting down ...").

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%

reply({Result, State}) ->
    {reply, Result, State}.

noreply({_, State = #state{subs = []}}) ->
    {stop, shutdown, State};

noreply({_, State}) ->
    {noreply, State}.

%%

do_subscribe(Pid, State = #state{subs = Subs, last = Last}) ->
    lager:debug("subscribe: ~p", [Pid]),
    case lists:member(Pid, Subs) of
        false ->
            true = link(Pid),
            ok = case Last of
                L when L =/= undefined ->
                    lager:debug("sending cached stats ..."),
                    do_unicast(Last, Pid);
                _ ->
                    ok
            end,
            {ok, State#state{subs = [Pid | Subs]}};
        true ->
            {{error, exists}, State}
    end.

do_unsubscribe(Pid, State = #state{subs = Subs}) ->
    lager:debug("unsubscribe: ~p", [Pid]),
    case lists:member(Pid, Subs) of
        true ->
            _ = unlink(Pid),
            {ok, State#state{subs = lists:delete(Pid, Subs)}};
        false ->
            {{error, notfound}, State}
    end.

do_broadcast(Stats, State = #state{subs = Subs}) ->
    ok = lists:foreach(fun (Pid) -> do_unicast(Stats, Pid) end, Subs),
    {ok, State}.

do_unicast(Stats, Pid) ->
    _ = Pid ! {stats, Stats}, ok.

%%

gather_stats() ->
    Interval = genlib_app:env(surely, interval),
    spawn_link(fun () -> gather_stats(Interval) end).

gather_stats(Interval) ->
    {Abs, Rel} = recon:node_stats(1, Interval, fun (S, _) -> S end, []),
    Stats = Abs ++ Rel ++ gather_additional_stats(),
    exit({stats, Stats}).

gather_additional_stats() ->
    {Wallclock, WallclockDiff} = erlang:statistics(wall_clock),
    {_Runtime, RuntimeDiff} = erlang:statistics(runtime),
    Procs = erlang:system_info(logical_processors_online),
    [
        {wall_clock  , Wallclock},
        {utilization , RuntimeDiff / (WallclockDiff + 1) / Procs}
    ].
