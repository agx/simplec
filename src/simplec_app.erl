-module(simplec_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("simplec.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Uris} = application:get_env(simplec, uris),
    {ok, Dir} = application:get_env(simplec, dir),
    C = #config{uris=Uris, dir=Dir},
    simplec_sup:start_link(C).

stop(_State) ->
    ok.
