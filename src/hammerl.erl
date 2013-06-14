-module(hammerl).

%% API.
-export([start/0, dispatchers/0, reload_dispatchers/0]).

%% API.

start() ->
    ok = application:start(hammerl).

dispatchers() ->
    cowboy_router:compile([
        {'_', [
            {"/blog/[:blogname]", blog, []},
            {"/", index, []}
        ]}
    ]).

reload_dispatchers() ->
    cowboy:set_env(http, dispatch,
                   dispatchers()).
