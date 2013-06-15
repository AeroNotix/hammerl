-module(hammerl).

-include("blog.hrl").

%% API.
-export([start/0, dispatchers/0, reload_dispatchers/0, blog/1]).

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

blog(Name) ->
	emysql:execute(blog_pool, blog_stmt_get, [Name]).
