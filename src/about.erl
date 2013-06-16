-module(about).

-export([init/3, handle/2, terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    cowboy_req:reply(301, [{"Location", "/blog/about"}], <<"">>, Req),
    {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
	ok.
