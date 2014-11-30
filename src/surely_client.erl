-module(surely_client).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init(_, _, _) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_, Req, _Opts) ->
    lager:info("new client connected"),
    Req2 = cowboy_req:compact(Req),
    ok = surely_monitor:subscribe(),
    {ok, Req2, []}.

websocket_handle({text, Data}, Req, State) ->
    lager:warning("unexpected text frame: ~s", [Data]),
    {reply, {text, Data}, Req, State};

websocket_handle({binary, Data}, Req, State) ->
    lager:warning("unexpected binary frame: ~s", [Data]),
    {reply, {binary, Data}, Req, State};

websocket_handle(_Frame, Req, State) ->
    {ok, Req, State}.

websocket_info({stats, Stats}, Req, State) ->
    Values = prepare_stats(Stats),
    Payload = jiffy:encode(Values),
    {reply, {text, Payload}, Req, State};

websocket_info(Info, Req, State) ->
    lager:warning("unexpected info: ~p", [Info]),
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    lager:info("client disconnected"),
    ok = surely_monitor:unsubscribe(),
    ok.

%%

prepare_stats(Stats) ->
    {lists:map(fun (S) -> format_stat(S) end, Stats)}.

format_stat({scheduler_usage, V}) ->
    {_, Vs} = lists:unzip(V),
    {scheduler_usage, Vs};

format_stat(S) ->
    S.
