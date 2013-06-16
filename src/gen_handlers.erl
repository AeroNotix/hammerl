-module(gen_handlers).
-export([do404/0]).

do404() ->
	templates:blog([{blog, [{blog_title, "Not found"},
							{blog_post, "Sorry, that page cannot be found!"}]}]
				   ++ context:ctx()).
