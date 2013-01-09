-module(dbswui_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, Args) ->
	[HTTP_Port,HTTPS_Port]=
		case Args of
			[] -> [7080,7443];
			Any -> Any
		end,
	Dispatch =
		[
		 {'_',
		  [
		   {[<<"db">>], db_handler, []},
		   {[<<"db">>, <<"edit">>], db_edit_handler, []},
		   {[<<"db">>, <<"logout">>], logout_handler, []},
		   {[<<"static">>, '...'], cowboy_static, [{directory, "static/"}]},
		   {'_', default_handler, []}
		  ]
		 }
		],
	{ok, _} = 
		cowboy:start_http(
		  http,
		  100,
		  [{port, HTTP_Port}],
		  [{env, [{dispatch, Dispatch}]}]
		 ),
	{ok, _} = 
		cowboy:start_https(
		  https,
		  100,
		  [{port, HTTPS_Port},
		   {certfile, "priv/ssl/cert.pem"},
		   {keyfile, "priv/ssl/key.pem"},
		   {password, ""}],
		  [{env, [{dispatch, Dispatch}]}]
		 ),
	dbswui_sup:start_link().

stop(_State) ->
	ok.
