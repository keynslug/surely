-module(surely_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([start_monitor/1]).

-export([init/1]).

start_link(SupName = {_, Name}) ->
    supervisor:start_link(SupName, ?MODULE, Name).

start_monitor(Node) ->
    supervisor:start_child(surely_monitor_sup, [Node]).

%%

init(surely_sup) ->
    {ok, {{rest_for_one, 5, 10}, [get_monitor_sup_spec(), get_listener_spec()]}};

init(surely_monitor_sup) ->
    {ok, {{simple_one_for_one, 10, 30}, [get_monitor_spec()]}}.

%%

get_monitor_sup_spec() ->
    genlib_app:supervisor({surely_monitor_sup, ?MODULE}, local, []).

get_monitor_spec() ->
    genlib_app:temporary(surely_monitor, none, []).

get_listener_spec() ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/listen" , surely_client, []},
            {"/"       , cowboy_static, {priv_file , surely, "www/index.html", [{mimetypes, cow_mimetypes, all}]}},
            {"/[...]"  , cowboy_static, {priv_dir  , surely, "www", [{mimetypes, cow_mimetypes, all}]}}
        ]}
    ]),
    TransOpts = [{port, genlib_app:env(surely, port)}],
    ProtoOpts = [{env, [{dispatch, Dispatch}]}],
    ranch:child_spec(surely_http, 8, ranch_tcp, TransOpts, cowboy_protocol, ProtoOpts).
