-module(blog).

-export([init/3, handle/2, terminate/3]).

-include("blog.hrl").

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
	{BlogName, Req2} = cowboy_req:binding(blogname, Req),
	case BlogName of
		undefined ->
			Blogs = [[{blog_title, Blog#blog.title},
					  {blog_url, Blog#blog.url},
					  {date, strdate(Blog#blog.date)}] || Blog <- hammerl:bloglist()],
			cowboy_req:reply(200, [], templates:blogmode([{blog, Blogs}] ++ ctx()), Req2);
		_Else ->
			Blog = hammerl:blog(binary_to_list(BlogName)),
			io:format(binary_to_list(BlogName)),
			case Blog of
				{error, not_found} ->
					cowboy_req:reply(404, [], do404(), Req2);
				_Else2 ->
					cowboy_req:reply(200, [], templates:blog([
															  {blog, [{blog_title, Blog#blog.title},
																	  {blog_post, Blog#blog.content}]}
															 ] ++ ctx()), Req2)
			end
	end,
    {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
	ok.

do404() ->
	<<"404">>.
