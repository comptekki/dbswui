-module(dbswui).
-behaviour(application).
-export([start/0, start/2, stop/1]).

start() ->
	application:start(crypto),
	application:start(public_key),
	application:start(ssl),
	application:start(cowboy),
	application:start(dbswui).

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
		   {[<<"static">>, '...'], cowboy_http_static, [{directory, "static/"}]},
		   {'_', default_handler, []}
		  ]
		 }
		],
	cowboy:start_listener(
	  my_http_listener, 100,
	  cowboy_tcp_transport, [{port, HTTP_Port}],
	  cowboy_http_protocol, [{dispatch, Dispatch}]
	 ),
	cowboy:start_listener(
	  my_https_listener, 100,
	  cowboy_ssl_transport, [{port, HTTPS_Port}, {certfile, "priv/ssl/cert.pem"}, {keyfile, "priv/ssl/key.pem"}, {password, ""}],
	  cowboy_http_protocol, [{dispatch, Dispatch}]
	 ),
	dbswui_sup:start_link().

stop(_State) ->
	ok.
