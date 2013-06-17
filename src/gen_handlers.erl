-module(gen_handlers).
-export([do404/0, redirect/2]).

do404() ->
    templates:blog([{blog, [{blog_title, "Not found"},
			    {blog_post, "Sorry, that page cannot be found!"}]}]
		   ++ context:ctx()).

redirect(Where, Req) ->
    cowboy_req:reply(301, [{"Location", Where}], <<"">>, Req).
