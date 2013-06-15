-module(index).

-export([init/3, handle/2, terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    cowboy_req:reply(200, [], <<"OK">>, Req),
    {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
	ok.
