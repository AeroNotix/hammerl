-module(hammerl).

%% API.
-export([start/0, dispatchers/0, reload_dispatchers/0]).

%% API.

start() ->
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    ok = application:start(hammerl).

dispatchers() ->
    cowboy_router:compile([
        {'_', [
        ]}
    ]).

reload_dispatchers() ->
    cowboy:set_env(http, dispatch,
                   dispatchers()).
