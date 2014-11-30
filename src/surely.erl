-module(surely).
-behaviour(application).

-export([start/0]).

-export([start/2]).
-export([stop/1]).

start() ->
    application:ensure_all_started(?MODULE).

start(_Type, _Args) ->
	surely_sup:start_link().

stop(_State) ->
	ok.
