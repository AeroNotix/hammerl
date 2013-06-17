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
                                  %% This route will display the list
                                  %% of blog entries or a specific
                                  %% blog depending on whether
                                  %% :blogname is given.
                                  {"/blog/[:blogname]", blog, []},

                                  %% Static handler route, it serves
                                  %% the priv/* directory.
                                  {"/static/[...]", cowboy_static,
                                   [
                                    {directory, {priv_dir, hammerl, [<<"static">>]}},
                                    {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
                                   ]
                                  },

                                  %% The about page is a simple
                                  %% redirect from /about =>
                                  %% /blog/about.
                                  {"/about", about, []},

                                  %% The index page with a specialized
                                  %% template.
                                  {"/", index, []}
                                 ]
                           }
                          ]).

reload_dispatchers() ->
    cowboy:set_env(http, dispatch,
                   dispatchers()).

%% @doc
%% blog/1 will return the associated blog entry for the URL that you
%% provide.
%% @spec blog(Name::string()) -> blog() | {error, not_found}
blog(Name) ->
    case simple_cache:lookup(Name) of
	{error, not_found} ->
	    {_, _, _, Blog, _} = emysql:execute(blog_pool, blog_stmt_get, [Name]),
	    case length(Blog) of
		0 ->
		    {error, not_found};
		_N ->
		    {ID, Date, URL, Title, Content} = list_to_tuple(lists:nth(1, Blog)),
		    NewBlog = #blog{id = ID, date = Date, url = URL, title = Title, content = Content},
		    simple_cache:insert(Name, NewBlog),
		    NewBlog
	    end;
	{ok, Blog} ->
	    Blog
    end.

%% @doc
%% bloglist/0 returns a proplist containing all the blogs in the
%% database.
%% @spec bloglist() -> list()
bloglist() ->
    case simple_cache:lookup(bloglist) of
	{error, not_found} ->
	    {_, _, _, Blogs, _} = emysql:execute(blog_pool, <<"SELECT * FROM blog_entry">>),
	    BlogLists = create_blog_list(Blogs),
	    simple_cache:insert(bloglist, BlogLists),
	    BlogLists;
	{ok, Blogs} ->
	    Blogs
    end.

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
