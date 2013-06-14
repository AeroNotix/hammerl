-module(hammerl_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
	Dispatch = hammerl:dispatchers(),
	{ok, Port} = application:get_env(port),
	{ok, _} = cowboy:start_http(http, 100, [{port, Port}], [
		{env, [{dispatch, Dispatch}]}
	]),
    hammerl_sup:start_link().

stop(_State) ->
	ok.
