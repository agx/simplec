-module(simplec_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("simplec.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Uri} = application:get_env(simplec, uri),
    {ok, Dir} = application:get_env(simplec, dir),
    C = #config{url=Uri, dir=Dir},
    simplec_sup:start_link(C).

stop(_State) ->
    ok.
