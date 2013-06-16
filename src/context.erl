-module(context).
-export([ctx/0, strdate/1]).

ctx() ->
	[
	 {"STATIC_URL", "/static/"},
	 {githuburl, "https://s3.amazonaws.com/github/ribbons/forkme_right_darkblue_121621.png"}
	].

strdate(Date) ->
	{date, {Y, M, D}} = Date,
	io_lib:format("~p/~p/~p", [D, M, Y]).
