-module(db1_edit_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

-include("http.hrl").
-include("db.hrl").

%

init({_Any, http}, Req, []) ->
	{ok, Req, undefined}.

fire_wall(Req) ->	
	{PeerAddress, _Req}=cowboy_http_req:peer_addr(Req),
	{ok, [_,{FireWallOnOff,IPAddresses},_,_,_]}=file:consult(?CONF),
	case FireWallOnOff of
		on ->
			case lists:member(PeerAddress,IPAddresses) of
				true ->
					allow;
				false ->
					deny
			end;
		off -> allow
	end.

%

fwDenyMessage(Req, State) ->
	{ok, Req2} = cowboy_http_req:reply(200, [{'Content-Type', <<"text/html">>}],
<<"<html>
<head> 
<title>DB1 Login</title>
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
	{ok, [_,_,{UPOnOff,UnamePasswds},_,_]}=file:consult(?CONF),
	case UPOnOff of
		on -> UnamePasswds;
		off -> off
	end.

%

checkCreds(UnamePasswds, Req, _State) ->
	{ok, [_,_,_,_,{CookieName}]}=file:consult(?CONF),
	{C,_Req} = cowboy_http_req:cookie(CookieName, Req),
    case C of
		undefined ->
			checkPost(UnamePasswds,Req);
		<<"true">> ->
			{pass,Req}
	end.

%

checkPost(UnamePasswds,Req) ->
	case Req#http_req.method of
		'POST' ->
			{FormData, Req0}=cowboy_http_req:body_qs(Req),
			case FormData of
				[{_UnameVar,UnameVal},{_PasswdVar,PasswdVal},_Login] ->
					checkCreds(UnamePasswds,UnameVal,PasswdVal,Req0);
				_ ->
					{fail,Req}
			end;
		_ ->
			{fail,Req}
	end.

%

checkCreds([{Uname,Passwd}|UnamePasswds],Uarg,Parg,Req) ->
    case Uname of
		Uarg ->
			case Passwd of
				Parg ->
					{ok, [_,_,_,{MaxAge},{CookieName}]}=file:consult(?CONF),
					{ok,Req0}=cowboy_http_req:set_resp_cookie(CookieName,<<"true">>,[{max_age,MaxAge},{path,"/"}],Req),
					{pass,Req0};
				_ ->
					checkCreds(UnamePasswds,Uarg,Parg,Req)
			end;
		_ ->
			checkCreds(UnamePasswds,Uarg,Parg,Req)
	end;
checkCreds([],_Uarg,_Parg,Req) ->
	{fail,Req}.

%

app_login(Req, State) ->
	case fire_wall(Req) of
		allow ->
			case is_list(login_is()) of
				true ->
					{ok, Req2} = cowboy_http_req:reply(200, [{'Content-Type', <<"text/html">>}],
<<"<html>
<head> 
<title>DB1 Login</title>

<meta Http-Equiv='Cache-Control' Content='no-cache'>
<meta Http-Equiv='Pragma' Content='no-cache'>
<meta Http-Equiv='Expires' Content='0'>
<META HTTP-EQUIV='EXPIRES' CONTENT='Mon, 30 Apr 2012 00:00:01 GMT'>

<link rel='icon' href='/static/favicon.ico' type='image/x-icon' />
<link href='/static/db.css' media='screen' rel='stylesheet' type='text/css' />
<script type='text/javascript' src='/static/jquery.js'></script>
<script>
$(document).ready(function(){

$('#uname').focus();

});
</script>
</head>
<body>
<form action='/db1/edit' method='post'>
<div>
  <h3>DB1 Login</h3>
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
</html>">>, Req),
					{ok, Req2, State};
                false ->
					{ok, Req2} = cowboy_http_req:reply(200, [{'Content-Type', <<"text/html">>}],
<<"<html>
<head> 
<title>DB1 Login</title>
</head>
<body>
hi
</body>
</html>">>, Req),
{ok, Req2, State}
            end;
        deny ->
            fwDenyMessage(Req, State)
    end.

%

handle(Req, State) ->
	case fire_wall(Req) of
		allow ->
			Creds=login_is(),
			case is_list(Creds) of
				true ->
					{Cred,Req0}=checkCreds(Creds, Req, State),
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
		deny ->
			fwDenyMessage(Req, State)
	end.

%

app_front_end(Req0, State) ->
	{[ServerPath1, ServerPath2], Req1} = cowboy_http_req:path(Req0),
	ServerPath= <<ServerPath1/binary, "/",ServerPath2/binary>>,
	{Client_IP, Req} = cowboy_http_req:peer_addr(Req1),
	io:format("~nIP: ~p - Date/Time: ~p~n",[Client_IP, calendar:local_time()]),
	{S, _Req} = cowboy_http_req:qs_val(<<"s">>,Req),
	{Table, _Req} = cowboy_http_req:qs_val(<<"tablename">>,Req),
	{ok, Req2} =
		cowboy_http_req:reply(200, [],
			  case Table of
				  ?DB ->
					  {Rpp, _Req} = cowboy_http_req:qs_val(<<"rpp">>,Req),
					  {Offset, _Req} = cowboy_http_req:qs_val(<<"offset">>,Req),
					  {FieldsAll, _Req} = cowboy_http_req:qs_vals(Req),
					  [_,_,_,_|RawFields] = FieldsAll,
					  Fields=select_fields(RawFields),
					  case S of
						  <<"2">> ->
							  Ep=update_pattern(Table, Fields),
							  case do_update(Ep) of
								  {_, error} ->
									  <<"Error Saving Data!">>;
								  _ ->
									  <<"Data Saved!">>
							  end;
						  _ ->
							  Sp =
								  case Fields of
									  [] -> (catch select_pattern(Table));
									  _  -> (catch select_pattern(Table, Fields, S))
								  end,
							  SpOffset = <<Sp/binary, " offset ", Offset/binary, " limit ", Rpp/binary>>,
							  <<(table(Sp, SpOffset, Rpp, ServerPath, Fields, S))/binary>>
					  end;
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
	
terminate(_Req, _State) ->
	ok.

%

update_pattern(Table, [{Field, Val}|Fields]) ->
	{Rfields, Rvals} = update_pattern(Fields),
	<<"update ", Table/binary, " set (", Rfields/binary, ") = (", Rvals/binary, ") where ", Field/binary, "=", Val/binary>>.

%

update_pattern([{Field, Val}|Fields]) ->
	update_pattern(Fields, <<Field/binary>>, <<"E'", (escape0(Val))/binary, "'">>).

update_pattern([{Field, Val}|Fields], Accf, Accv) ->
	case Val of
		<<>> ->
			update_pattern(Fields, <<Accf/binary, ",", Field/binary>>, <<Accv/binary, ", ''">>);
		_ ->
			update_pattern(Fields, <<Accf/binary, ",", Field/binary>>, <<Accv/binary, ", E'", (escape0(Val))/binary, "'">>)
	end;
update_pattern([], Accf, Accv) ->
	{Accf, Accv}.

do_update(S) ->
	io:format("~p~n",[S]),
	case pgsql:connect(?HOST, ?USERNAME, ?PASSWORD, [{database, ?DB}]) of
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

escape0(S) ->
	S1=re:replace(S,"\\\\","\\\\\\\\",[{return,binary}, global]),
	re:replace(S1,"'","\\\\'",[{return,binary}, global]).

% Get the fields to be part of the select

select_fields([H|T]) ->
	{Field, Value} = H,
	H2 =
		case size(Value) =< ?MAX_LEN of
			true ->
				{Field, Value};
			_ ->
				%% truncate value to MAX_LEN chars
				{Field, binary:part(Value, {0, ?MAX_LEN})}
		end,
	[H2|select_fields(T)];
select_fields([]) ->
	[].

%

select_pattern(Name) ->
	S= <<"select * from ", Name/binary, ?ORDER_BY>>,
	io:format("~p~n",[S]),
	S.

% postgresql commands:	
% ~~ is equiv to like
% ~~* is equiv to ilike (case insensitive)
% use ! for not

select_pattern(Name, Ls, S) ->
	case S of
		<<"0">> -> Op = <<" and ">>;
		<<"1">> -> Op = <<" or ">>
	end,
	S2=expand_cols(Ls, Op),
	S3= <<"select * from ", Name/binary, " where ", (sandor(S2))/binary, ?ORDER_BY>>,
	io:format("~p~n",[S3]),
	S3.

expand_cols([Col|Rest], Op) ->
	{Field,Value} = Col,
	<<(fieldq(Op, Field, Value))/binary,(expand_cols(Rest, Op))/binary>>;
expand_cols([],_Op) ->
	<<>>.

%

fieldq(Op, Field, Value) ->
	Sval = sanitize(Value),
	<<(fdq(Sval, Field, Op))/binary, "%'">>.

%

fdq(<<"---", Rest/binary>>, Field, Op) ->
	<<Op/binary, Field/binary, "::text ~~* '%-", Rest/binary>>;	
fdq(<<"-", Rest/binary>>, Field, _) ->
	<<" and ", Field/binary, "::text !~~* '%", Rest/binary>>;
fdq(Text, Field, Op) ->
	<<Op/binary, Field/binary, "::text ~~* '%", Text/binary>>.	

%

sandor(<<" and ", Rest/binary>>) ->
	Rest;	
sandor(<<" or ",Rest/binary>>) ->
	Rest.

%
	
sanitize(S) ->
	S2=re:replace(S,"'","''",[{return,binary}, global]),
	S3=re:replace(S2,"\\\\","\\\\\\\\",[{return,binary}, global]),
	S5=re:replace(S3,"%","\\\\%",[{return,binary}, global]),
	re:replace(S5,"_","\\\\_",[{return,binary}, global]).

% 1: inside jQuery-desktop
% _: db stand-alone

return_top_page(ServerPath, Hdr) ->
	case Hdr of
		<<"1">> ->
			rttp_main(ServerPath, Hdr);
		_ ->
			<<(get_top())/binary,
			   (rttp_main(ServerPath, Hdr))/binary>>
	end.
%

get_top() ->
<<"<html>
<head>
<title>test</title>

<meta Http-Equiv='Cache-Control' Content='no-cache'>
<meta Http-Equiv='Pragma' Content='no-cache'>
<meta Http-Equiv='Expires' Content='0'>

<meta http-equiv='Content-Type' content='text/html;charset=utf-8'/> 
<link rel='icon' href='/static/favicon.ico' type='image/x-icon' />
<link href='/static/db.css' media='screen' rel='stylesheet' type='text/css' />
">>.

%

rttp_main(ServerPath, Hdr) ->
	  <<"
<script type='text/javascript' src='",?JQUERY,"'></script>
<script type='text/javascript' src='/static/jquery.simplemodal.1.4.2.min.js'></script>
<script type='text/javascript'>

var view=true;

$(document).ready(function() {

    $('#click_fview').click(function() {
        $('#s').val(0);
        $('#fview').show('slow');
        $('#click_qsview').show('slow');
        $('#qsview').hide('slow');
        $('#click_fview').hide('slow');
        $('#title').focus();
        ajfun0();
        view=false
    }); 

    $('#click_qsview').click(function() {
        $('#s').val(1);
        $('#fview').hide('slow');
        $('#click_qsview').hide('slow');
        $('#qsview').show('slow');
        $('#click_fview').show('slow');
        $('#single_input_db').focus();
        ajfun1();
        view=true
    });

    $('#single_input_db').focus();
    ajfun1()
})
</script>

</head>
<body>
<div id='wrapper'>
",
(mk_table_tab(<<"10">>, <<"0">>, ServerPath, Hdr))/binary,
"
</div>
</body>
</html>">>.

%

setfields() ->
	<<"' + '&rpp=' + $('#range_input').val() + '&offset=' + $('#offset').val() + ",
    (setf(?TABLE))/binary>>.

setf([Col|Cols]) ->
	<<" '&",Col/binary,"=' + encodeURIComponent($('#",Col/binary,"').val()) + ",
	(setf(Cols))/binary>>;
setf([]) ->
	<<"''">>.

%

setfields_single() ->
	<<"' + '&rpp=' + $('#range_input').val() + '&offset=' + $('#offset').val() + ",
	  "'&title=' + encodeURIComponent($('#single_input_db').val()) + ",
	  "'&author_editor=' + encodeURIComponent($('#single_input_db').val()) + "
	  "'&date_of_publication=' + encodeURIComponent($('#single_input_db').val()) + "
	  "'&publisher=' + encodeURIComponent($('#single_input_db').val()) + "
	  "'&key_words=' + encodeURIComponent($('#single_input_db').val()) + "
	  "'&notes=' + encodeURIComponent($('#single_input_db').val()) + "
	  "'&valuation=' + encodeURIComponent($('#single_input_db').val()) + "
	  "'&purchase_price=' + encodeURIComponent($('#single_input_db').val())">>.

%

js3a(ServerPath) ->
<<"
<script type='text/javascript'>
ajfun0 = function() {
	$('#offset').val(0);
	$.ajax({
		url: '/",ServerPath/binary,"',
		type: 'GET',
		data: 'tablename=", ?DB/binary, "&s=0",(setfields())/binary,",
		success: function(data) {
			    $('#data').html(arguments[2].responseText);
		},
		error:function(XMLHttpRequest, textStatus, errorThrown) {
			alert(XMLHttpRequest + ' - ' + textStatus + ' - ' + errorThrown);
		}
  	});
}
</script>
">>.

%

js3b() ->
<<"
<script type='text/javascript'>",
(js3b2(?TABLE))/binary,
"</script>">>.

js3b2([Col|Rest]) ->
	<<"
$(document).ready(
function() {
	$('#", Col/binary, "').keyup(function(event) {
           if (!(event.keyCode in {0:0,13:0,16:0,17:0,18:0,91:0,224:0})) {
			   ajfun0();
		} // if
	});
});					
",(js3b2(Rest))/binary>>;
js3b2([]) ->
	<<>>.

%
		
js4(ServerPath) ->
	<<"
<script type='text/javascript'>
    ajfun1 = function() {
		$('#offset').val(0);
		$.ajax({
			url: '/",ServerPath/binary, "',
			type: 'GET',
			data: 'tablename=", ?DB/binary, "&s=1", (setfields_single())/binary, ",
			success: function(data) {
			    $('#data').html(arguments[2].responseText);
		    },
		    error:function(XMLHttpRequest, textStatus, errorThrown) {
	 		    alert(XMLHttpRequest + ' - ' + textStatus + ' - ' + errorThrown);
		    }
	    });
	}
    $(document).ready(
		function() {
			$('#single_input_db').keyup(function(event) {
// ignore windows menu key 0, return 13, shift 16,  ctr 17, alt 18,
// windows flag 91 on chrome 224 on mozilla - removed from list - in case paste text on mac
// js 'in' operator requires key:value pair so all keys just have value 0
				if (!(event.keyCode in {0:0,13:0,16:0,17:0,18:0})) {
				   ajfun1();
				} // if
			});
	});
</script>">>.
		
% Build the result page.

table(Sp, SpOffset, RowsPerPage, ServerPath, Fields, S) ->
	case do_query(SpOffset) of
		{_, error} ->
				<<"
<table>
<tr><td>Error!  Please try back later.</td></tr>
</table>
">>;
        {_, Result} ->
			case do_query(Sp) of
				{_, error} ->
				<<"
<table>
<tr><td>Error!  Please try back later.</td></tr>
</table>
">>;
			{_, Res2} ->
					table2(RowsPerPage, ServerPath, Fields, S, Result, Res2)
			end
	end.
%

table2(RowsPerPage, ServerPath, Fields, S, Result, Res2) ->
	Count=list_to_binary(integer_to_list(length(Res2))),
	case Count of
		<<"0">> ->
			<<"<br /><table style='background-color:black; color:yellow;'><tr><td>No matches found...</td></tr></table>">>;
		_ ->
			Headers = ?TABLE,
			Nav= <<"
<div class='nav'>
<table>
<tr>",
(mk_nav(Count, RowsPerPage, ServerPath, S))/binary,
"
</tr>
</table>
</div>
">>, % end of Nav
<<"

<br />
<div id='riv'>
<table>
<tr>
<td>
<select id='range_input_view_s'>
<option value='10'>10</option>
<option value='20'>20</option>
<option value='30'>30</option>
<option value='40'>40</option>
<option value='50'>50</option>
<option value='60'>60</option>
<option value='70'>70</option>
<option value='80'>80</option>
<option value='90'>90</option>
<option value='100'>100</option>
</select>
<input id='range_input_view' type='range' name='range_input_view' min='10' max='100' value='",RowsPerPage/binary,"' step='5'>
</td>
<td class='rows'>
<span>Show <span id='range_val'>10</span> items per page</span>
</td>
</tr>
</table>
</div>
<script type='text/javascript'>

$(document).ready(function() {

	if ($.browser.mozilla)
        $('#range_input_view').hide()
    else 
        $('#range_input_view_s').hide();

    $('#range_input_view_s').change(function() {
        $('#range_input_view').val($('#range_input_view_s').val());
        $('#range_input_view').change();
        $('#range_input_view').click().mouseup()
    });

    $('#range_input_view_s').val($('#range_input_view').val());
    $('#range_val').html($('#range_input_view').val());
	$('#range_input_view').change(function() {
		$('#range_val').html($('#range_input_view').val());
		$('#range_input').val($('#range_input_view').val());
		$('#offset').val(0)
	});
	$('#range_input_view').click().mouseup(function() {
		$.ajax({
			url: '/",ServerPath/binary,"',
			type: 'GET',
			data: 'tablename=", ?DB/binary, "&s=", (s_fields(S))/binary, ",
			success: function(data) {
			    $('#data').html(arguments[2].responseText);
//				$('#data').html(data)
			},
			error:function(XMLHttpRequest, textStatus, errorThrown) {
				alert(XMLHttpRequest + ' - ' + textStatus + ' - ' + errorThrown)
			}
		})
	})
})
</script>

<table>
<tr>
<td>
<p>Items Found: ", Count/binary,"</p>
</td>
</tr>
</table>
<div>",
Nav/binary,
(mk_tab(Headers, Result, Fields, ServerPath))/binary,
Nav/binary,
"

">>
	end.	

s_fields(<<"0">>) ->
	<<"0", (setfields())/binary>>;
s_fields(<<"1">>) ->
	<<"1",(setfields_single())/binary>>.

mk_nav(CountB, RowsPerPageB, ServerPath, S) ->
	Count=list_to_integer(binary_to_list(CountB)),
	RowsPerPage=list_to_integer(binary_to_list(RowsPerPageB)),
	Ni=Count div RowsPerPage,
	Nd=Count rem RowsPerPage,
	
	case Nd > 0 of
		true -> Nii = Ni + 1;
		_ -> Nii = Ni
	end,
	case Nii > 10 of
		true -> NavL = 10;
		_ -> NavL = Nii
	end,
	build_nav(1, NavL, RowsPerPage, ServerPath, S).

build_nav(Start, End, RowsPerPage, ServerPath, S) ->
	StartB=list_to_binary(integer_to_list(Start)),
	OffSet = list_to_binary(integer_to_list((Start-1)*RowsPerPage)),
	case Start==End of
		false -> 
			<<"<td><a href='javascript:void(0)' id='", StartB/binary, "'
				onclick=\"$('#offset').val(", OffSet/binary,");
			$.ajax({
				 url: '/", ServerPath/binary, "',
				 type: 'GET',
				 data: 'tablename=", ?DB/binary, "&s=", (s_fields(S))/binary, ",
				 success: function(data) {
  			        $('#data').html(arguments[2].responseText);
				 },
				 error:function(XMLHttpRequest, textStatus, errorThrown) {
				  alert(XMLHttpRequest + ' - ' + textStatus + ' - ' + errorThrown);
				 }
		   });$(':input:text:first').focus();\">
			", StartB/binary,"</a></td>", (build_nav(Start+1,End, RowsPerPage, ServerPath, S))/binary>>;
		_ ->
			<<"<td><a href='javascript:void(0)' id='", StartB/binary, "'
				onclick=\"$('#offset').val(", OffSet/binary,");
			$.ajax({
				 url: '/", ServerPath/binary, "',
				 type: 'GET',
				 data: 'tablename=", ?DB/binary, "&s=", (s_fields(S))/binary, ",
				 success: function(data) {
  			        $('#data').html(arguments[2].responseText);
				 },
				 error:function(XMLHttpRequest, textStatus, errorThrown) {
				  alert(XMLHttpRequest + ' - ' + textStatus + ' - ' + errorThrown);
				 }
		   });$(':input:text:first').focus();\">
			", StartB/binary,"</a></td>">>
	end.
	
%	

do_query(Sp) ->	
	case pgsql:connect(?HOST, ?USERNAME, ?PASSWORD, [{database, ?DB}]) of
		{error,_} ->
			{Sp, error};
		{ok, Db} -> 
			case pgsql:squery(Db, Sp) of
				{error,_} ->
					{Sp, error};
				{_,_,Res} ->
					pgsql:close(Db),
					{Sp, Res}
			end
	end.

%

add_rec(Fields, ServerPath) ->
	[Field|_]=Fields,
	Hdrs= [<<"<th style='width:175px; text-align:right; vertical-align:top;'>", (title(X))/binary, "</th>">> || X <- Fields],
	<<"
<script>
$(document).ready(function(){
    $('#add_rec').click(function() {
        $('#t_add_rec').modal({escClose:false, closeClass:'modal-cancel', focus:false, opacity:80, overlayCss: {backgroundColor:'#555'}});
        $('#i_add_rec_", Field/binary, "').focus()
    });

    $('#s_add_rec').click(function() {

	$.ajax({
		url: '/",ServerPath/binary,"',
		type: 'GET',
		data: 'tablename=", ?DB/binary, "&s=3",
 (setfields2a(Fields))/binary,
",
   success: function(data) {
            if (view)
                ajfun1()
            else 
                ajfun0();

//            alert(arguments[2].responseText);

//$('#data').html(arguments[2].responseText);

		},
		error:function(XMLHttpRequest, textStatus, errorThrown) {
			alert(XMLHttpRequest + ' - ' + textStatus + ' - ' + errorThrown);
		}
  	});

    $('#c_add_rec').click();
  });

})
</script>

<table id='t_add_rec' class='record'>",
	  (add_rec(1,Hdrs,Fields))/binary,
"</table>
">>.

%

add_rec(First,[Hdr|RestHdrs],[Field|Fields]) ->
	<<
"
<tr>
",
	  (case First of
		  1 -> <<"
<th style='width:50px;' rowspan='8'>
  <input id='s_add_rec' type='button' name='s_add_rec' value='Save' class=''><br />
  <input id='c_add_rec' type='button' name='c_add_rec' value='Cancel' class='modal-cancel'>
</th>
">>;
		  _ -> <<>>
	  end)/binary,
	  Hdr/binary,
	  "
<td>
<input id='i_add_rec_", Field/binary, "' class='dbinput2' name='' maxlength='", ?MAX_LENB/binary, "' value=''>
</td>
</tr>",
	  (add_rec(First+1,RestHdrs,Fields))/binary>>;
add_rec(_,[],_Fields) ->
	<<>>.

%

setfields2a(Fields) ->
	<<"&rpp=0&offset=0&id=add_rec' + ",
    (setf2a(Fields))/binary>>.

setf2a([Field|Fields]) ->
	<<" '&",Field/binary,"=' + encodeURIComponent($('#i_add_rec_", Field/binary, "').val()) + ",
	(setf2a(Fields))/binary>>;
setf2a([]) ->
	<<"''">>.

%

mk_table_tab(RowsPerPage, Offset, ServerPath, Hdr) ->
    <<"
<input id='s' type='hidden' value='0'>
<input id='range_input' type='hidden' value='",RowsPerPage/binary,"'>
<input id='offset' type='hidden' value='",Offset/binary,"'>",
	  (case Hdr of
			<<"1">> -> <<>>;
			_ ->
				<<"
<a href='logout' id='logout'>logout</a>
<a href='javascript:void(0)' id='add_rec'>add</a>
",
(add_rec(?TABLE, ServerPath))/binary,
"
<table>
<tr>
<td colspan='9'> 
<p style='text-align: center; text-transform: uppercase;color: #949610; font-size:1.5em'>", ?DBTITLE/binary, "</p>
</td>
</tr>
</table>
">>
	    end)/binary,
		"<br />",
(js3a(ServerPath))/binary,
(js3b())/binary,
"
<div id='click_fview' class='click'>
	<a href='javascript:void(0)'>Click this line to use the Field Search</a>
</div>",
(js4(ServerPath))/binary,
"
<div id='click_qsview' class='click'>
    <a href='javascript:void(0)'>Click this line to use the Quick Search</a>
</div>
<div id='fview'>
<table>
<tr>
<td class='srch'>Field Search</td>
",
(mk_input_fields(?TABLE))/binary,
"
</tr>
</table>
</div>
<div id='qsview'>
<table>
<tr>
<td class='srch'>Quick Search</td>
<td colspan='8'>
<input id='single_input_db' name='single_input_db' style='width:500px;' maxlength='", ?MAX_LENB/binary,"'>
</td>
</tr>
</table>
</div>
<div id='data'></div>">>.

% create each table cell; consisting of the attribute name and an input field.

mk_input_fields([Col|Cols]) ->
                <<"<td><span class='attribute'>", (title(Col))/binary,"</span>
<input id='",Col/binary,"' type='text' name='", Col/binary, "' maxlength='30'></td>",(mk_input_fields(Cols))/binary>>;
mk_input_fields([]) ->
	<<>>.
    
% Build the result table.

mk_tab(Headers, Rows, Fields, ServerPath) ->
	Hdrs= [<<"<th style='width:175px; text-align:right; vertical-align:top;'>", (title(X))/binary, "</th>">> || X <- Headers],
    <<"<div>",
%      <table class='data'>
%     ",
         (mk_tab2(Rows,Hdrs,Fields, ServerPath))/binary,
%        "
%      </table>
   "</div>">>.

mk_tab2([RowTuple|Rows],Hdrs,Fields, ServerPath) ->
	[Id|Row]=tuple_to_list(RowTuple),
%	[{Field,_Srch}|_Rest]=Fields,
	<<"
<script>
$(document).ready(function(){
    $('#h", Id/binary, "').click(function() {
        $('#t", Id/binary, "').modal({escClose:false, closeClass:'modal-cancel', focus:false, opacity:80, overlayCss: {backgroundColor:'#555'}, persist:true});
        $('#s", Id/binary, "').show();
        $('#c", Id/binary, "').show();
",
(jsedit(Id,Fields))/binary,
"
    });

    $('#s", Id/binary, "').click(function() {

        $('#s", Id/binary, "').hide();
        $('#c", Id/binary, "').hide();
",
(jsedit2(Id, Fields))/binary,
"
	$.ajax({
		url: '/",ServerPath/binary,"',
		type: 'GET',
		data: 'tablename=", ?DB/binary, "&s=2", (setfields2(Id, Fields))/binary, ",
		success: function(data) {
            if (view)
                ajfun1()
            else 
                ajfun0();

//            alert(arguments[2].responseText);

//$('#data').html(arguments[2].responseText);

		},
		error:function(XMLHttpRequest, textStatus, errorThrown) {
			alert(XMLHttpRequest + ' - ' + textStatus + ' - ' + errorThrown);
		}
  	});


        $('#c", Id/binary, "').click();
    });

    $('#c", Id/binary, "').click(function() {
        $('#s", Id/binary, "').hide();
        $('#c", Id/binary, "').hide();
",
(jsedit3(Id,Fields))/binary,
"
//        $('#c", Id/binary, "').click();
    })

})
</script>

<table id='t", Id/binary, "' class='record datat'>",
	  (mk_tab3(1,Id,Row,Hdrs,Fields))/binary,
	  "</table>
",
	  (mk_tab2(Rows,Hdrs,Fields, ServerPath))/binary>>;
mk_tab2([],_Hdrs,_Fields, _ServerPath) ->
	<<>>.

%

setfields2(Id, Fields) ->
	<<"&rpp=0&offset=0&id=", Id/binary, "' + ",
    (setf2(Id, Fields))/binary>>.

setf2(Id, [{Field,_Srch}|Fields]) ->
	<<" '&",Field/binary,"=' + encodeURIComponent($('#i_", Field/binary, "_", Id/binary, "').val()) + ",
	(setf2(Id, Fields))/binary>>;
setf2(_Id, []) ->
	<<"''">>.

%

jsedit(Id, [{Field,_Srch}|Fields]) ->
	<<"
       $('#d_", Field/binary, "_", Id/binary, "').hide();
       $('#i_", Field/binary, "_", Id/binary, "').show();
",
	  (jsedit(Id,Fields))/binary>>;
jsedit(_Id,[]) ->
	<<>>.

%

jsedit2(Id,[{Field,_Srch}|Fields]) ->
	<<"
       $('#i_", Field/binary, "_", Id/binary, "').hide();
       $('#d_", Field/binary, "_", Id/binary, "').show();

       $('#d_", Field/binary, "_", Id/binary, "').html($('#i_", Field/binary, "_", Id/binary, "').val());
       $('#ib_", Field/binary, "_", Id/binary, "').val($('#i_", Field/binary, "_", Id/binary, "').val());
",
	  (jsedit2(Id,Fields))/binary>>;
jsedit2(_Id,[]) ->
	<<>>.
%

jsedit3(Id,[{Field,_Srch}|Fields]) ->
	<<"
       $('#i_", Field/binary, "_", Id/binary, "').hide();
       $('#i_", Field/binary, "_", Id/binary, "').val($('#ib_", Field/binary, "_", Id/binary, "').val());
       $('#d_", Field/binary, "_", Id/binary, "').show();

",
	  (jsedit3(Id,Fields))/binary>>;
jsedit3(_Id,[]) ->
	<<>>.

%

mk_tab3(First,Id,[Item|RestRow],[Hdr|RestHdrs],[{Field,Srch}|Fields]) ->
	<<
"
<tr>
",
	  (case First of
		  1 -> <<"
<th style='width:50px;' rowspan='8'>
  <a href='javascript:void(0)' id='h", Id/binary, "'>", Id/binary, "</a>
  <input id='s", Id/binary, "' type='button' name='s", Id/binary, "' value='Save' class='ebutton'><br />
  <input id='c", Id/binary, "' type='button' name='c", Id/binary, "' value='Cancel' class='ebutton modal-cancel'>
</th>
">>;
		  _ -> <<>>
	  end)/binary,
	  Hdr/binary,
	  "
<td>
<div id='d_", Field/binary, "_", Id/binary, "'>",
	  (hl(Item,Srch))/binary,
"
</div>
<input id='ib_", Field/binary, "_", Id/binary, "' class='dbinput' name='' maxlength='", ?MAX_LENB/binary, "' value='", (list_to_binary(htmlize(binary_to_list(Item))))/binary, "'>
<input id='i_", Field/binary, "_", Id/binary, "' class='dbinput' name='' maxlength='", ?MAX_LENB/binary, "' value='", (list_to_binary(htmlize(binary_to_list(Item))))/binary, "'>
</td>
</tr>",
	  (mk_tab3(First+1,Id,RestRow,RestHdrs,Fields))/binary>>;
mk_tab3(_,_Id,[],_Hdrs,_Fields) ->
	<<>>.

htmlize([H|T]) ->
	[case H of
		 38 -> "&amp;"; % & to &amp;
		 60 -> "&lt;";  % < to &lt;
		 62 -> "&gt;";  % > to &gt;
		 39 -> "&#39;"; % ' to &#39;
		 _ -> H
	 end|htmlize(T)];
htmlize([]) ->
	[].

hl(Item,FsrchData) ->
	Clean =
		case chk_dash(FsrchData) of
			{"---",Rest} ->
				"-"++escape(Rest);
			_ ->
				escape(FsrchData)
		end,
	Rv =
		case length(Clean) of
			0 ->
				Item;
			_ ->
				{ok, CaselessClean}=re:compile(Clean,[caseless]),
				re:replace(Item,CaselessClean,"<span class='hl'>"++string:to_upper(Clean)++"</span>",[{return,binary},global])
		end,
	<<Rv/binary>>.

chk_dash(<<"---",Rest/binary>>) ->
	{"---",Rest};
chk_dash(Any) ->
	Any.

escape(Data) ->
	re:replace(escape2(Data),"\\#","\\\\#",[{return,list},global]).

escape2(Data) ->
	re:replace(escape3(Data),"\\$","\\\\$",[{return,list},global]).

escape3(Data) ->
	re:replace(escape4(Data),"\\&","\\\\&",[{return,list},global]).

escape4(Data) ->
	re:replace(escape5(Data),"\\(","\\\\(",[{return,list},global]).

escape5(Data) ->
	re:replace(escape6(Data),"\\)","\\\\)",[{return,list},global]).

escape6(Data) ->
	re:replace(escape7(Data),"\\*","\\\\*",[{return,list},global]).

escape7(Data) ->
	re:replace(escape8(Data),"\\+","\\\\+",[{return,list},global]).

escape8(Data) ->
	re:replace(escape9(Data),"\\.","\\\\.",[{return,list},global]).

escape9(Data) ->
	re:replace(escape10(Data),"\\?","\\\\?",[{return,list},global]).

escape10(Data) ->
	re:replace(escape11(Data),"\\[","\\\\[",[{return,list},global]).

escape11(Data) ->
	re:replace(escape12(Data),"\\^","\\\\^",[{return,list},global]).

escape12(Data) ->
	re:replace(escape13(Data),"\\|","\\\\|",[{return,list},global]).

escape13(Data) ->
	re:replace(Data,"\\\\","\\\\\\\\",[{return,list},global]).

%

title(Title) ->
	?TITLES.