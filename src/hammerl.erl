-module(hammerl).

-include("blog.hrl").

%% API.
-export([start/0, dispatchers/0, reload_dispatchers/0, blog/1, bloglist/0]).

%% API.
start() ->
	ok = application:start(hammerl).

dispatchers() ->
	cowboy_router:compile([
		{'_', [
			{"/blog/[:blogname]", blog, []},
            {"/static/[...]", cowboy_static, [
                {directory, {priv_dir, hammerl, [<<"static">>]}},
                {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
            ]},
			{"/about", about, []},
			{"/", index, []}
		]}
	]).

reload_dispatchers() ->
	cowboy:set_env(http, dispatch,
				   dispatchers()).

%% @doc
%% blog/1 will return the associated blog entry for the URL that you
%% provide.
%% @spec blog(Name::string()) -> blog() | {error, not_found}
blog(Name) ->
	{_, _, _, Blog, _} = emysql:execute(blog_pool, blog_stmt_get, [Name]),
	case length(Blog) of
		0 ->
			{error, not_found};
		_N ->
			{ID, Date, URL, Title, Content} = list_to_tuple(lists:nth(1, Blog)),
			#blog{id = ID, date = Date, url = URL, title = Title, content = Content}
	end.

%% @doc
%% bloglist/0 returns a proplist containing all the blogs in the
%% database.
%% @spec bloglist() -> list()
bloglist() ->
	{_, _, _, Blogs, _} = emysql:execute(blog_pool, <<"SELECT * FROM blog_entry">>),
	create_blog_list(Blogs).

%% @doc
%% create_blog_list/1 takes a list of blog entries taken from the
%% database and creates a list of proplists out of them so they can be
%% interpolated into a template easily.
%% @spec create_blog_list(L::list()) -> list()
create_blog_list([]) ->
	[];
create_blog_list(L) ->
	create_blog_list(L, []).
create_blog_list([], BlogEntries) ->
	BlogEntries;
create_blog_list([H|T], BlogEntries) ->
	{ID, Date, URL, Title, Content} = list_to_tuple(H),
	create_blog_list(T, [#blog{id = ID, date = Date, url = URL, title = Title, content = Content}|BlogEntries]).
