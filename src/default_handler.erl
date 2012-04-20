-module(default_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

init({_Any, http}, Req, []) ->
       {ok, Req, undefined}.

handle(Req, State) ->
	{ok,Req1} = cowboy_http_req:set_resp_header('Location', <<"db1">>, Req),
	{ok, Req2} = cowboy_http_req:reply(302, [], <<>>, Req1),
	{ok, Req2, State}.

terminate(_Req, _State) ->
       ok.
