-module(hammerl_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.
start(_Type, _Args) ->
    %% compile our templates
    {ok, Dir} = application:get_env(hammerl, template_dir),
    erlydtl:compile_dir(Dir, templates),
    %% Add our pool using our connection details.
    emysql:add_pool(blog_pool,
		    1,
		    connection:username(),
		    connection:password(),
		    connection:host(),
		    connection:port(),
		    "blog",
		    utf8),
    %% Prepare the statement the we will use quite a lot.
    emysql:prepare(blog_stmt_get,
		   <<"SELECT * from blog_entry WHERE blog_url=?">>),
    Dispatch = hammerl:dispatchers(),
    {ok, Port} = application:get_env(port),
    {ok, _} = cowboy:start_http(
                http, 100, [{port, Port}],
                [{env, [{dispatch, Dispatch}]}]
               ).

stop(_State) ->
    ok.
