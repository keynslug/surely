-module(surely_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Procs = [get_monitor_spec(), get_listener_spec()],
    {ok, {{rest_for_one, 1, 5}, Procs}}.

get_monitor_spec() ->
    {surely_monitor, {surely_monitor, start_link, []}, permanent, brutal_kill, worker, [surely_monitor]}.

get_listener_spec() ->
    Mime = {mimetypes, cow_mimetypes, all},
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/listen" , surely_client, []},
            {"/"       , cowboy_static, {priv_file , surely, "www/index.html", [Mime]}},
            {"/[...]"  , cowboy_static, {priv_dir  , surely, "www", [Mime]}}
        ]}
    ]),
    TransOpts = [{port, get_env(port, 8000)}],
    ProtoOpts = [{env, [{dispatch, Dispatch}]}],
    ranch:child_spec(surely_http, 8, ranch_tcp, TransOpts, cowboy_protocol, ProtoOpts).

get_env(Key, Def) ->
    application:get_env(surely, Key, Def).
