-module(surely_monitor).
-behaviour(gen_server).

%% API

-export([start_link/0]).
-export([subscribe/0]).
-export([unsubscribe/0]).

%% gen_server

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% API

-spec start_link() -> {ok, pid()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

subscribe() ->
    gen_server:call(?MODULE, {subscribe, self()}, infinity).

unsubscribe() ->
    gen_server:call(?MODULE, {unsubscribe, self()}, infinity).

%% gen_server.

init([]) ->
    {ok, [], 0}.

handle_call({subscribe, Pid}, _From, State) ->
    case lists:keymember(Pid, 1, State) of
        true ->
            {reply, ok, State, 0};
        false ->
            {reply, ok, [{Pid, erlang:monitor(process, Pid)} | State], 0}
    end;

handle_call({unsubscribe, Pid}, _From, State) ->
    case lists:keyfind(Pid, 1, State) of
        {Pid, MRef} ->
            _ = erlang:demonitor(MRef, [flush]),
            {reply, ok, lists:keydelete(Pid, 1, State), 0};
        false ->
            {reply, error, State, 0}
    end;

handle_call(Request, _From, State) ->
    lager:warning("unexpected call: ~p", [Request]),
    {reply, error, State, 0}.

handle_cast(Msg, State) ->
    lager:warning("unexpected cast: ~p", [Msg]),
    {noreply, State, 0}.

handle_info(timeout, State) ->
    Interval = 2000,
    Stats = gather_stats(Interval),
    _ = [Pid ! {stats, Stats} || {Pid, _} <- State],
    {noreply, State, 0};

handle_info({'DOWN', _, process, Pid, _}, State) ->
    {noreply, lists:keydelete(Pid, 1, State), 0};

handle_info(Info, State) ->
    lager:warning("unexpected info: ~p", [Info]),
    {noreply, State, 0}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%

gather_stats(Interval) ->
    {Abs, Rel} = recon:node_stats(1, Interval, fun (S, _) -> S end, []),
    Abs ++ Rel ++ gather_additional_stats().

gather_additional_stats() ->
    {Wallclock, WallclockDiff} = erlang:statistics(wall_clock),
    {_Runtime, RuntimeDiff} = erlang:statistics(runtime),
    Procs = erlang:system_info(logical_processors_online),
    [
        {wall_clock  , Wallclock},
        {utilization , RuntimeDiff / (WallclockDiff + 1) / Procs}
    ].
