-module(hammerl).

%% API.
-export([start/0, dispatchers/0, reload_dispatchers/0, blog/1]).

%% API.

start() ->
	ok = application:start(hammerl).

dispatchers() ->
	emysql:add_pool(blog_pool,
					1,
					connection:username(),
					connection:password(),
					connection:host(),
					connection:port(),
					"blog",
					utf8),
	emysql:prepare(blog_stmt_get,
				   <<"SELECT * from blog_entry WHERE blog_title=?">>),
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
