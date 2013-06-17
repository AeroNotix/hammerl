-module(context).
-export([ctx/0, strdate/1]).

%% @doc
%% Since erlydtl doesn't provide the regular context processor
%% middleware that Django does, we need to supply it ourselves.
%% @spec
%% ctx() -> list()
ctx() ->
	[
	 {"STATIC_URL", "/static/"},
	 {githuburl, "https://s3.amazonaws.com/github/ribbons/forkme_right_darkblue_121621.png"}
	].

%% @doc
%% strdate/1 takes a date in the format {date, {Y, M, D}} and returns
%% a date in the form: D/M/Y.
%% @spec
%% strdate(Date::tuple()) -> iolist()
strdate(Date) ->
	{date, {Y, M, D}} = Date,
	io_lib:format("~p/~p/~p", [D, M, Y]).
