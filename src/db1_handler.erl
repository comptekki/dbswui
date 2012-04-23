-module(db1_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

-include("http.hrl").
-include("db.hrl").

%

init({_Any, http}, Req, []) ->
	{ok, Req, undefined}.

%

handle(Req0, State) ->
	{[ServerPath], Req1} = cowboy_http_req:path(Req0),
	{Client_IP, Req} = cowboy_http_req:peer_addr(Req1),
	io:format("~n~nIP: ~p - Date/Time: ~p~n",[Client_IP, calendar:local_time()]),
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
					  Sp =
						  case Fields of
							  [] -> (catch select_pattern(Table));
							  _  -> (catch select_pattern(Table, Fields, S))
						  end,
					  SpOffset = <<Sp/binary, " offset ", Offset/binary, " limit ", Rpp/binary>>,
					  <<(table(Sp, SpOffset, Rpp, ServerPath, Fields, S))/binary>>;
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
			   (rttp_css())/binary,
			   (rttp_main(ServerPath, Hdr))/binary>>
	end.
%

get_top() ->
<<"<html>
<head>
<title>test</title>
  <meta http-equiv='Content-Type' content='text/html;charset=utf-8'/> 
  <link rel='icon' href='/static/favicon.ico' type='image/x-icon' />
">>.

%

rttp_css() ->
	<<"
<style type='text/css'>
body {background-color: #aaaaaa;}
table {border-collapse: collapse; border: solid black 1px; background-color: #cccccc;}
p {padding: 5px; font-weight: bold;}
input[type=text] {vertical-align: bottom; width: 100%; font-size: 80%;}
input[type=checkbox] {vertical-align: top; font-size: 80%;}
span.attribute {vertical-align: top; font-size: 80%;}
th {padding: 5px; border: solid black 1px;}
td {padding: 5px; border: solid black 1px;}
span.hl {padding: 0 2px 0 2px; /* margin: 0 2px 0 2px; */ background-color:yellow; /*color:white;*/}
#fview, #click_qsview {display:none;}
.click {padding: 3px 3px 3px 3px; background-color: #dddddd; color:green; /*width: 245px;*/ }
.click A:link, A:visited {text-decoration: none}
.click A:hover {padding: 0 2px 0 2px; background-color: #eeffee;}
.rows {background-color: #cccccc;}
.record {width:700px; margin:5px; background-color: #dddddd;}
.srch {width:30px;}
</style>
">>.

%

rttp_main(ServerPath, Hdr) ->
	  <<"
<script type='text/javascript' src='",?JQUERY,"'></script>

<script type='text/javascript'>

$(document).ready(function() {
    $('#click_fview').click(function() {
        $('#s').val(0);
        $('#fview').show('slow');
        $('#click_qsview').show('slow');
        $('#qsview').hide('slow');
        $('#click_fview').hide('slow');
        $('#title').focus();
        ajfun0();
    }); 

    $('#click_qsview').click(function() {
        $('#s').val(1);
        $('#fview').hide('slow');
        $('#click_qsview').hide('slow');
        $('#qsview').show('slow');
        $('#click_fview').show('slow');
        $('#single_input_db').focus();
        ajfun1();
    });

    $('#single_input_db').focus();
    ajfun1();
})
</script>

</head>
<body>  
",	  
(mk_table_tab(<<"10">>, <<"0">>, ServerPath, Hdr))/binary,
"</body>
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
//			$('#data').html(data) 
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
	<<"">>.

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
(mk_tab(Headers, Result, Fields))/binary,
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
			<<"<td><a href='javascript:void(0);' id='", StartB/binary, "'
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
			<<"<td><a href='javascript:void(0);' id='", StartB/binary, "'
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

mk_table_tab(RowsPerPage, Offset, ServerPath, Hdr) ->
    <<"
<input id='s' type='hidden' value='0'>
<input id='range_input' type='hidden' value='",RowsPerPage/binary,"'>
<input id='offset' type='hidden' value='",Offset/binary,"'>",
	  (case Hdr of
			<<"1">> -> <<"">>;
			_ ->
				<<"
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
	<a href='javascript:void(0);'>Click this line to use the Field Search</a>
</div>",
(js4(ServerPath))/binary,
"
<div id='click_qsview' class='click'>
    <a href='javascript:void(0);'>Click this line to use the Quick Search</a>
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
<input id='single_input_db' name='single_input_db' style='width:500px;' maxlength='",?MAX_LENB/binary,"'>
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
	<<"">>.
    
% Build the result table.

mk_tab(Headers, Rows, Fields) ->
	Hdrs= [<<"<th style='width:175px; text-align:right; vertical-align:top;'>", (title(X))/binary, "</th>">> || X <- Headers],
    <<"<div>",
%      <table class='data'>
%     ",
         (mk_tab2(Rows,Hdrs,Fields))/binary,
%        "
%      </table>
   "</div>">>.

mk_tab2([RowTuple|Rows],Hdrs,Fields) ->
	[_|Row]=tuple_to_list(RowTuple),
	<<"
<table class='record datat'>",
	  (mk_tab3(Row,Hdrs,Fields))/binary,
	  "</table>
",
	  (mk_tab2(Rows,Hdrs,Fields))/binary>>;
mk_tab2([],_Hdrs,_Fields) ->
	<<"">>.

mk_tab3([Item|RestRow],[Hdr|RestHdrs],[FieldSrch|Fields]) ->
	<<"<tr>",Hdr/binary,"<td>", (hl(Item,FieldSrch))/binary,"</td></tr>",(mk_tab3(RestRow,RestHdrs,Fields))/binary>>;
mk_tab3([],_Hdrs,_Fields) ->
	<<"">>.

hl(Item,{_FieldName,FsrchData}) ->
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

title(<<"title">>) ->
	<<"Title">>;
title(<<"author_editor">>) ->
	<<"Author Editor">>;
title(<<"date_of_publication">>) ->
	<<"Date of Publication">>;
title(<<"publisher">>) ->
	<<"Publisher">>;
title(<<"key_words">>) ->
	<<"Key Words">>;
title(<<"notes">>) ->
	<<"Notes">>;
title(<<"valuation">>) ->
	<<"Valuation">>;
title(<<"purchase_price">>) ->
	<<"Purchase Price">>.

