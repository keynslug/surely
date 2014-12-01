-module(surely).
-behaviour(application).

-export([start/0]).

-export([subscribe/1]).

-export([start/2]).
-export([stop/1]).

%%

start() ->
    application:ensure_all_started(?MODULE).

subscribe(Node) ->
    try surely_monitor:subscribe(Node, self()) catch
        exit:{Reason, _} when Reason =:= noproc; Reason =:= shutdown ->
            case surely_sup:start_monitor(Node) of
                {ok, _} ->
                    subscribe(Node);
                {error, {already_started, _}} ->
                    subscribe(Node)
            end
    end.

start(_Type, _Args) ->
	surely_sup:start_link({local, surely_sup}).

stop(_State) ->
	ok.
