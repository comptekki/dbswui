-module(db_handler).

-export([init/3, handle/2, terminate/3]).

-import(dbswui_lib, [return_top_page/2, select_fields/1, select_pattern/1, select_pattern/3, table/7]).

-include("db.hrl").

%im().
%ii(db_handler).
%iaa([init]).

%

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

%

handle(Req0, State) ->
	{ServerPath, Req1} = cowboy_req:path(Req0),
	{Client_IP, Req} = cowboy_req:peer_addr(Req1),
	io:format("~nIP: ~p - Date/Time: ~p~n",[Client_IP, calendar:local_time()]),
	{S, _Req} = cowboy_req:qs_val(<<"s">>,Req),
	{Table, _Req} = cowboy_req:qs_val(<<"tablename">>,Req),
	{ok, Req2} =
		cowboy_req:
		reply(200, [],
			  case Table of
				  ?DB ->
					  {Rpp, _Req} = cowboy_req:qs_val(<<"rpp">>, Req),
					  {Offset, _Req} = cowboy_req:qs_val(<<"offset">>, Req),
					  {N, _Req} = cowboy_req:qs_val(<<"n">>, Req),
					  {FieldsAll, _Req} = cowboy_req:qs_vals(Req),
					  [_,_,_,_,_|RawFields] = FieldsAll,
					  Fields=select_fields(RawFields),
					  Sp =
						  case Fields of
							  [] -> (catch select_pattern(Table));
							  _  -> (catch select_pattern(Table, Fields, S))
						  end,
					  SpOffset = <<Sp/binary, " offset ", Offset/binary, " limit ", Rpp/binary>>,
					  <<(table(Sp, SpOffset, Rpp, ServerPath, Fields, S, N))/binary>>;
				  _ ->
					  case S of
						  <<"1">> ->
							  <<(return_top_page(ServerPath, <<"1">>))/binary>>;
						  _ ->
							  <<(return_top_page(ServerPath, <<"0">>))/binary>>
					  end
			  end,
			  Req),
	{ok, Req2, State}.

%
	
terminate(_Reason, _Req, _State) ->
	ok.
