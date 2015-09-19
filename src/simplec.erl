%% -*- erlang -*-

-module(simplec).
-export([start/0, stop/0]).

start() ->
    application:start(simplec).

stop() ->
    applictation:stop(simplec).
