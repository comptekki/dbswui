-module(default_handler).
-export([init/3, handle/2, terminate/2]).

init(_Transport, Req, []) ->
       {ok, Req, undefined}.

handle(Req, State) ->
	Req1 = cowboy_req:set_resp_header(<<"'Location'">>, <<"/db">>, Req),
	{ok, Req2} = cowboy_req:reply(302, [], <<>>, Req1),
	{ok, Req2, State}.

terminate(_Req, _State) ->
       ok.
