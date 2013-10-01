-module(db_edit_handler).
-export([init/3, handle/2, terminate/3]).

-import(dbswui_lib, [return_top_page/2, select_fields/1, select_pattern/1, select_pattern/3, table/7, escape0/1]).

-include("db.hrl").

%im().
%ii(db_edit_handler).
%iaa([init]).

%

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

fire_wall(Req) ->	
	{{PeerAddress, _Port}, _Req}=cowboy_req:peer(Req),
	{ok, [_, {FireWallOnOff, IPAddresses}, _]} = file:consult(?CONF),
	case FireWallOnOff of
		on ->
			case lists:member(PeerAddress, IPAddresses) of
				true ->
					allow;
				false ->
					deny
			end;
		off -> allow
	end.

%

fwDenyMessage(Req, State) ->
	{ok, Req2} = cowboy_req:reply(200, [{"Content-Type", <<"text/html">>}],
<<"<html>
<head> 
<title>", ?TITLE/binary, "</title>
<style>
body {background-color:black; color:yellow}
</style>
</head>
<body>
Access Denied!
</body>
</html>">>, Req),
{ok, Req2, State}.

%

login_is() ->
	{ok, [_,_,{UPOnOff,UnamePasswds}]}=file:consult(?CONF),
	case UPOnOff of
		on -> UnamePasswds;
		off -> off
	end.

%

checkCreds(UnamePasswds, Req, _State) ->
	[{Uname,_}] = UnamePasswds,
	{C, Req1} = cowboy_req:cookie(Uname, Req),
    case (C == undefined) or (C == <<>>) of
		true ->
			checkPost(UnamePasswds, Req1);
		false  ->
			CookieVal = get_cookie_val(), 
			Req2 = cowboy_req:set_resp_cookie(Uname, CookieVal, [{max_age, ?MAXAGE}, {path, "/"}, {secure, true}, {http_only, true}], Req1),
			{pass, Req2}
	end.

%

checkCreds([{Uname,Passwd}|UnamePasswds], Uarg, Parg, Req) ->
    case Uname of
		Uarg ->
			case Passwd of
				Parg ->
					CookieVal = get_cookie_val(), 
					Req0 = cowboy_req:set_resp_cookie(Uname, CookieVal, [{max_age, ?MAXAGE}, {path, "/"}, {secure, true}, {http_only, true}], Req),
					{pass, Req0};
				_ ->
					checkCreds(UnamePasswds,Uarg,Parg,Req)
			end;
		_ ->
			checkCreds(UnamePasswds, Uarg, Parg, Req)
	end;
checkCreds([], _Uarg, _Parg, Req) ->
	{fail, Req}.

%

checkPost(UnamePasswds,Req) ->
	case cowboy_req:method(Req) of
		{<<"POST">>, Req0} ->
			{ok, FormData, Req1} = cowboy_req:body_qs(Req0),
			case FormData of
				[{_UnameVar,UnameVal},{_PasswdVar,PasswdVal},_Login] ->
					checkCreds(UnamePasswds,UnameVal,PasswdVal,Req1);
				_ ->
					{fail,Req}
			end;
		_ ->
			{fail,Req}
	end.

%

get_cookie_val() ->
	list_to_binary(
	  integer_to_list(
		calendar:datetime_to_gregorian_seconds({date(), time()})
	   )).

%

app_login(Req, State) ->
	case fire_wall(Req) of
		allow ->
			case is_list(login_is()) of
				true ->
					{ok, Req2} = cowboy_req:reply(200, [{"Content-Type", <<"text/html">>}],
<<"<html>
<head> 
<title>", ?TITLE/binary, "</title>

<meta Http-Equiv='Cache-Control' Content='no-cache'>
<meta Http-Equiv='Pragma' Content='no-cache'>
<meta Http-Equiv='Expires' Content='0'>
<META HTTP-EQUIV='EXPIRES' CONTENT='Mon, 30 Apr 2012 00:00:01 GMT'>

<link rel='icon' href='/static/favicon.ico' type='image/x-icon' />
<link rel=\"stylesheet\" href=\"", ?CSS, "?", (now_bin())/binary, "\" type=\"text/css\" media=\"screen\" />
<script type='text/javascript' src='", ?JQUERY, "'></script>
<script>
$(document).ready(function(){

$('#uname').focus();

});
</script>
</head>
<body>
<form action='/edit' method='post'>
<div>
  <h3>", ?TITLE/binary, " Login</h3>
</div>
<div class='unamed'>
  <div class='unamed-t'>Username: </div><div><input id='uname' type='text' name='uname'></div>
</div>
<div class='passwdd'>
  <div class='passwdd-t'>Password: </div><div><input id='passwd' type='password' name='passwd'></div>
</div>
<div class='logind'>
  <div class='fl'><input type='submit' name='login' value='Login'></div>
</div>
</form>
</body>
</html>">>, Req);
                false ->
					{ok, Req2} = cowboy_req:reply(200, [{"Content-Type", <<"text/html">>}],
<<"<html>
<head> 
<title>", ?TITLE/binary, " Login</title>
</head>
<body>
hi
</body>
</html>">>, Req)
            end,
    {ok, Req2, State};
        deny ->
            fwDenyMessage(Req, State)
    end.

%

handle(Req, State) ->
	case fire_wall(Req) of
		allow ->
			case cowboy_req:get([socket, transport], Req) of
				[_Socket, ranch_ssl] ->
					Creds=login_is(),
					case is_list(Creds) of
						true ->
							{Cred, Req0}=checkCreds(Creds, Req, State),
							case Cred of
								fail ->
									app_login(Req0, State);
								pass ->
									app_front_end(Req0, State)
							end;
						false -> 
							case Creds of
								off ->
									app_front_end(Req, State);
								_  ->
									app_login(Req, State)
							end
					end;
				_ ->
					Req1 = cowboy_req:set_resp_header(<<"Location">>, [<<"/">>], Req),
					{ok, Req2} = cowboy_req:reply(302, [], <<>>, Req1),
					{ok, Req2, State}
			end;
		deny ->
			fwDenyMessage(Req, State)
	end.

%

app_front_end(Req0, State) ->
	{ServerPath, Req1} = cowboy_req:path(Req0),
	{{Client_IP, _Port}, Req2} = cowboy_req:peer(Req1),
	io:format("~nIP: ~p - Date/Time: ~p~n",[Client_IP, calendar:local_time()]),
	{S, Req3} = cowboy_req:qs_val(<<"s">>, Req2),
	{Table, Req4} = cowboy_req:qs_val(<<"tablename">>, Req3),

	Val = case Table of
			  ?DB ->
				  {Rpp, Req5} = cowboy_req:qs_val(<<"rpp">>, Req4),
				  {Offset, Req6} = cowboy_req:qs_val(<<"offset">>, Req5),
				  {N, Req66} = cowboy_req:qs_val(<<"n">>, Req6),
				  {FieldsAll, Req7} = cowboy_req:qs_vals(Req66),
				  [_,_,_,_,_|RawFields] = FieldsAll,
				  Fields=select_fields(RawFields),
				  case S of
					  <<"4">> -> 
						  Ep=delete_pattern(Table, Fields),
						  case do_delete(Ep) of
							  {_, error} ->
								  <<"Error Deleting Record!">>;
							  _ ->
								  <<"Record Deleted!">>
						  end;
					  <<"3">> -> 
						  Ep=insert_pattern(Table, Fields),
						  case do_insert(Ep) of
							  {_, error} ->
								  <<"Error Adding Data!">>;
							  _ ->
								  <<"Data Added!">>
						  end;
					  <<"2">> ->
						  Ep=update_pattern(Table, Fields),
						  case do_update(Ep) of
							  {_, error} ->
								  <<"Error Saving Data!">>;
							  _ ->
								  <<"Data Updated!">>
						  end;
					  _ ->
						  Sp =
							  case Fields of
								  [] -> (catch select_pattern(Table));
								  _  -> (catch select_pattern(Table, Fields, S))
							  end,
						  SpOffset = <<Sp/binary, " offset ", Offset/binary, " limit ", Rpp/binary>>,
						  <<(table(Sp, SpOffset, Rpp, ServerPath, Fields, S, N))/binary>>
				  end;
			  _ ->
				  Req7 = Req4,
				  case S of
					  <<"1">> ->
						  <<(return_top_page(ServerPath, <<"1">>))/binary>>;
					  _ ->
						  <<(return_top_page(ServerPath, <<"0">>))/binary>>
				  end
		  end,
	{ok, Req8} = cowboy_req:reply(200, [], Val, Req7),
	{ok, Req8, State}.

%
	
terminate(_Reason, _Req, _State) ->
	ok.
%

delete_pattern(Table, [{Field,Val}|_]) ->
	<<"delete from ", Table/binary, " where ", Field/binary, "=", Val/binary>>.

%

do_delete(S) ->
io:format("~p~n",[S]),
	case pgsql:connect(?DBHOST, ?USERNAME, ?PASSWORD, [{database, ?DB}, {port, ?PORT}]) of
		{error,_} ->
			{S, error};
		{ok, Db} -> 
			case pgsql:squery(Db, S) of
				{error,Error} ->
					io:format("delete error: ~p~n", [Error]),
					{S, error};
				{_,Res} ->
					pgsql:close(Db),
					{S, Res}
			end
	end.

%

insert_pattern(Table, [{_,_}|Fields]) ->
	{Rfields, Rvals} = insert_pattern(Fields),
	<<"insert into ", Table/binary, "(", Rfields/binary, ") values (", Rvals/binary, ")">>.

%

insert_pattern([{Field, Val}|Fields]) ->
	insert_pattern(Fields, <<Field/binary>>, <<"E'", (trim_bin(escape0(Val)))/binary, "'">>).

insert_pattern([{Field, Val}|Fields], Accf, Accv) ->
	case Val of
		<<>> ->
			insert_pattern(Fields, <<Accf/binary, ",", Field/binary>>, <<Accv/binary, ", ''">>);
		_ ->
			insert_pattern(Fields, <<Accf/binary, ",", Field/binary>>, <<Accv/binary, ", E'", (trim_bin(escape0(Val)))/binary, "'">>)
	end;
insert_pattern([], Accf, Accv) ->
	{Accf, Accv}.

do_insert(S) ->
	io:format("~p~n",[S]),
	case pgsql:connect(?DBHOST, ?USERNAME, ?PASSWORD, [{database, ?DB}, {port, ?PORT}]) of
		{error,_} ->
			{S, error};
		{ok, Db} -> 
			case pgsql:squery(Db, S) of
				{error,Error} ->
					io:format("insert error: ~p~n", [Error]),
					{S, error};
				{_,Res} ->
					pgsql:close(Db),
					{S, Res}
			end
	end.

%

update_pattern(Table, [{Field, Val}|Fields]) ->
	{Rfields, Rvals} = update_pattern(Fields),
	<<"update ", Table/binary, " set (", Rfields/binary, ") = (", Rvals/binary, ") where ", Field/binary, "=", Val/binary>>.

%

update_pattern([{Field, Val}|Fields]) ->
	update_pattern(Fields, <<Field/binary>>, <<"E'", (trim_bin(escape0(Val)))/binary, "'">>).

update_pattern([{Field, Val}|Fields], Accf, Accv) ->
	case Val of
		<<>> ->
			update_pattern(Fields, <<Accf/binary, ",", Field/binary>>, <<Accv/binary, ", ''">>);
		_ ->
			update_pattern(Fields, <<Accf/binary, ",", Field/binary>>, <<Accv/binary, ", E'", (trim_bin(escape0(Val)))/binary, "'">>)
	end;
update_pattern([], Accf, Accv) ->
	{Accf, Accv}.

do_update(S) ->
	io:format("~p~n",[S]),
	case pgsql:connect(?DBHOST, ?USERNAME, ?PASSWORD, [{database, ?DB}, {port, ?PORT}]) of
		{error,_} ->
			{S, error};
		{ok, Db} -> 
			case pgsql:squery(Db, S) of
				{error,Error} ->
					io:format("update error: ~p~n", [Error]),
					{S, error};
				{_,Res} ->
					pgsql:close(Db),
					{S, Res}
			end
	end.

%

now_bin() ->
	{N1,N2,N3}=now(),
	list_to_binary(integer_to_list(N1)++integer_to_list(N2)++integer_to_list(N3)).

%

trim_bin(Bin) ->
	re:replace(Bin, "^\\s+|\\s+$", "", [{return, binary}, global]).
