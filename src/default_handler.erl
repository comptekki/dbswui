-module(default_handler).
-export([init/3, handle/2, terminate/2]).

init(_Transport, Req, []) ->
       {ok, Req, undefined}.

handle(Req, State) ->
	Req2 = cowboy_req:set_resp_header(<<"Location">>, <<"/db">>, Req),
	{ok, Req3} = cowboy_req:reply(302, [], <<>>, Req2),
	{ok, Req3, State}.

terminate(_Req, _State) ->
       ok.
