-module(about).

-export([init/3, handle/2, terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    Req2 = gen_handlers:redirect("/blog/about", Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.
