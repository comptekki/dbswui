-module(default_handler).
-export([init/3, handle/2, terminate/3]).

init(_Transport, Req, []) ->
       {ok, Req, undefined}.

handle(Req, State) ->
	Req2 = cowboy_req:set_resp_header(<<"Location">>, <<"/">>, Req),
	{ok, Req3} = cowboy_req:reply(302, [], <<>>, Req2),
	{ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
       ok.
