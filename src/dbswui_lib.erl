-module(dbswui_lib).

-export([return_top_page/2, select_fields/1, select_pattern/1, select_pattern/3, table/7, escape0/1]).

-include("db.hrl").

%

escape0(S) ->
	S1 = re:replace(S, "\\\\", "\\\\\\\\", [{return,binary}, global]),
	re:replace(S1, "'", "\\\\'", [{return, binary}, global]).

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

now_bin() ->
	{N1,N2,N3}=now(),
	list_to_binary(integer_to_list(N1)++integer_to_list(N2)++integer_to_list(N3)).

%

get_top() ->
<<"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">
<html lang=\"en\">
<head>
<title>DBSWUI</title>

<meta Http-Equiv=\"Cache-Control\" Content=\"no-cache\" />
<meta Http-Equiv=\"Pragma\" Content=\"no-cache\" />
<meta Http-Equiv=\"Expires\" Content=\"0\" />

<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" /> 
<link rel=\"icon\" href=\"/static/favicon.ico\" type=\"image/x-icon\" />
<link rel=\"stylesheet\" href=\"", ?CSS, "?", (now_bin())/binary, "\" type=\"text/css\" media=\"screen\" />
">>.

%

rttp_main(ServerPath, Hdr) ->
	  <<"
<script type=\"text/javascript\" src=\"",?JQUERY,"\"></script>
<script type=\"text/javascript\" src=\"/static/jquery.simplemodal.1.4.2.min.js\"></script>
<script type=\"text/javascript\">

var da = false;
var delclicked = false;
var view = true;
var activeElement = null;

$(document).ready(function() {

    $('#click_fview').click(function() {
        $('#s').val(0);
        $('#fview').show('slow');
        $('#click_qsview').show('slow');
        $('#qsview').hide('slow');
        $('#click_fview').hide('slow');
        $('#title').focus();
        ajfun0();
        view = false
    }); 

    $('#click_qsview').click(function() {
        $('#s').val(1);
        $('#fview').hide('slow');
        $('#click_qsview').hide('slow');
        $('#qsview').show('slow');
        $('#click_fview').show('slow');
        $('#single_input_db').focus();
        ajfun1();
        view = true
    });

    $('#single_input_db').focus();
    ajfun1();

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
	activeElement=document.activeElement;
	$('#offset').val(0);
	$.ajax({
		url: '",ServerPath/binary,"',
		type: 'GET',
		data: 'tablename=", ?DB/binary,"&n=1&s=0",(setfields())/binary,",
		success: function(data) {

            if (arguments[2].responseText.indexOf('", ?TITLE/binary, " Login') > -1 && arguments[2].responseText.indexOf('html') == 1) {
                alert('Login Expired - Please Re-Login...');
                location.href='",ServerPath/binary,"'
            }
            else {
                $('#data').html(arguments[2].responseText);
                if (activeElement != null)
                    activeElement.focus()
            }

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
			url: '",ServerPath/binary, "',
			type: 'GET',
			data: 'tablename=", ?DB/binary, "&n=1&s=1", (setfields_single())/binary, ",
			success: function(data) {

                if ((arguments[2].responseText.indexOf('", ?TITLE/binary, " Login') > -1) && (arguments[2].responseText.indexOf('html') == 1)) {
                    alert('Login Expired - Please Re-Login...');
                    location.href='",ServerPath/binary,"'
                }
                else {

                    $('#data').html(arguments[2].responseText);
                    if (activeElement != null)
                        activeElement.focus()
                }

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

table(Sp, SpOffset, RowsPerPage, ServerPath, Fields, S, N) ->
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
					table2(RowsPerPage, ServerPath, Fields, S, Result, Res2, N)
			end
	end.
%

table2(RowsPerPage, ServerPath, Fields, S, Result, Res2, N) ->

    {ok, Re} = re:compile("/edit",[caseless]),
    MatchVal = re:run(ServerPath, Re),

	Count=list_to_binary(integer_to_list(length(Res2))),
	case Count of
		<<"0">> ->
			<<"<br /><table style='background-color:black; color:red;'><tr><td>No matches found...</td></tr></table>">>;
		_ ->
			Headers = ?TABLE,
			NavT= <<"

<div class='navt'>
",
(mk_nav(Count, RowsPerPage, ServerPath, S, <<"t">>))/binary,
(case MatchVal of
	{match, _} -> 
		 <<"
<br><br>
<a href='javascript:void(0);' id='selectall'>Select All</a>
<a href='javascript:void(0);' id='unselectall'>Un-Select All</a>
<a href='javascript:void(0);' id='deleteall'>Delete All</a>">>;
	 _ -> <<>>
end)/binary,
"
</div>
">>,
 % end of NavT

			NavB= <<"

<div class='navb'>
",
(mk_nav(Count, RowsPerPage, ServerPath, S, <<"b">>))/binary,
"
</div>
">>, % end of NavB


<<"
<script type='text/javascript'>

var view = true;
var activeElement = null;

$(document).ready(function() {
    $('#nt", N/binary, "').removeClass('dhln');
    $('#nt", N/binary, "').addClass('dhl')
    $('#nb", N/binary, "').removeClass('dhln');
    $('#nb", N/binary, "').addClass('dhl')

})
</script>

<div id='riv' class='brk'>
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
<td>
Items Found: ", Count/binary,"
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
			url: '",ServerPath/binary,"',
			type: 'GET',
			data: 'tablename=", ?DB/binary, "&n=1&s=", (s_fields(S))/binary, ",
			success: function(data) {

                   if (arguments[2].responseText.indexOf('", ?TITLE/binary, " Login') > -1 && arguments[2].responseText.indexOf('html') == 1) {
                       alert('Login Expired - Please Re-Login...');
                       location.href='",ServerPath/binary,"'
                   }
                   else {
                       $('#data').html(arguments[2].responseText);
                       if (activeElement != null)
                           activeElement.focus()
                   }

			},
			error:function(XMLHttpRequest, textStatus, errorThrown) {
				alert(XMLHttpRequest + ' - ' + textStatus + ' - ' + errorThrown)
			}
		})
	})

$('.rheadg').click(function(){
    if(delclicked)
        delclicked = false;
    else
        $(this).toggleClass('rheadr rheadg');

//  if ($(this).attr('class') == 'rheadr'){
//    $(this).addClass('rheadg');
//    $(this).removeClass('rheadr');
//  }
//  else {
//    $(this).removeClass('rheadg');
//    $(this).addClass('rheadr');
//  }
});

$('.rheadr').click(function(){
    $(this).toggleClass('rheadr rheadg');

//  $(this).removeClass('rheadr');
//  $(this).addClass('rheadg');
});

$(function () {
 $('#selectall').click(function () {
  $('.rheadg').addClass('rheadr');
  $('.rheadg').removeClass('rheadg');
 });
});

$(function () {
 $('#unselectall').click(function () {
  $('.rheadr').addClass('rheadg');
  $('.rheadr').removeClass('rheadr');
 });
});

$(function () {
    $('#deleteall').click(function () {
        if ($('.rheadr').length>1) {
            deleted = $('.rheadr').length;
            var ans = confirm('Are you sure you want to delete these [ ' + deleted +' ] selected records?');
            if (ans) {
                da = true;
                $('.rheadr').each(
                   function() {
                       $('#d'+$(this).attr('id').substr(2)).click();
                   }
                );
                alert('[ ' + deleted +' ] records were deleted...');
            }
        }
        else {
            alert('At least -- 2 -- records must be selected...');
        }
   }).promise().done(function() { da = false; });
});

})
</script>

<div>",
NavT/binary,
(mk_tab(Headers, Result, Fields, ServerPath))/binary,
NavB/binary,
"

">>
	end.	

s_fields(<<"0">>) ->
	<<"0", (setfields())/binary>>;
s_fields(<<"1">>) ->
	<<"1",(setfields_single())/binary>>.

mk_nav(CountB, RowsPerPageB, ServerPath, S, TB) ->
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
	build_nav(1, NavL, RowsPerPage, ServerPath, S, TB).

build_nav(Start, End, RowsPerPage, ServerPath, S, TB) ->
	StartB=list_to_binary(integer_to_list(Start)),

	OffSet = list_to_binary(integer_to_list((Start-1)*RowsPerPage)),
	case Start==End of
		false -> 
			<<"<div id='n", TB/binary, StartB/binary, "' class='dhln'><a href='javascript:void(0)' id='", StartB/binary, "'
				onclick=\"$('#offset').val(", OffSet/binary,");
			$.ajax({
				 url: '", ServerPath/binary, "',
				 type: 'GET',
				 data: 'tablename=", ?DB/binary, "&n=", StartB/binary, "&s=", (s_fields(S))/binary, ",
				 success: function(data) {

                   if (arguments[2].responseText.indexOf('", ?TITLE/binary, " Login') > -1 && arguments[2].responseText.indexOf('html') == 1) {
                       alert('Login Expired - Please Re-Login...');
                       location.href='",ServerPath/binary,"'
                   }
                   else {
                       $('#data').html(arguments[2].responseText);
                       if (activeElement != null)
                           activeElement.focus()
                   }

				 },
				 error:function(XMLHttpRequest, textStatus, errorThrown) {
				  alert(XMLHttpRequest + ' - ' + textStatus + ' - ' + errorThrown);
				 }
		   });

$('#n", TB/binary, StartB/binary, "').removeClass('dhln');
$('#n", TB/binary, StartB/binary, "').addClass('dhl');
",
(case TB of
	 <<"b">> ->
		 <<"$('html, body').animate({scrollTop: $('html').height()}, 800);">>;
	 _ ->
		 <<>>
end)/binary,
"
\">
			", StartB/binary,"</a></div>", 
			  (build_nav(Start+1,End, RowsPerPage, ServerPath, S, TB))/binary>>;
		_ ->
			<<"<div id='n", TB/binary, StartB/binary, "' class='dhln'><a href='javascript:void(0)' id='", StartB/binary, "'
				onclick=\"$('#offset').val(", OffSet/binary,");
			$.ajax({
				 url: '", ServerPath/binary, "',
				 type: 'GET',
				 data: 'tablename=", ?DB/binary, "&n=", StartB/binary, "&s=", (s_fields(S))/binary, ",
				 success: function(data) {

                   if (arguments[2].responseText.indexOf('", ?TITLE/binary, " Login') > -1 && arguments[2].responseText.indexOf('html') == 1) {
                       alert('Login Expired - Please Re-Login...');
                       location.href='",ServerPath/binary,"'
                   }
                   else {
                       $('#data').html(arguments[2].responseText);
                       if (activeElement != null)
                           activeElement.focus()
                   }

				 },
				 error:function(XMLHttpRequest, textStatus, errorThrown) {
				  alert(XMLHttpRequest + ' - ' + textStatus + ' - ' + errorThrown);
				 }
		   });

$('#n", TB/binary, StartB/binary, "').removeClass('dhln');
$('#n", TB/binary, StartB/binary, "').addClass('dhl');
",
(case TB of
	 <<"b">> ->
		 <<"$('html, body').animate({scrollTop: $('html').height()}, 800);">>;
	 _ ->
		 <<>>
end)/binary,
"
\">
			", StartB/binary,"</a></div>">>
	end.
	
%	

do_query(Sp) ->	
	case pgsql:connect(?DBHOST, ?USERNAME, ?PASSWORD, [{database, ?DB}, {port, ?PORT}]) of
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
		url: '",ServerPath/binary,"',
		type: 'GET',
		data: 'tablename=", ?DB/binary, "&n=1&s=3",
 (setfields2a(Fields))/binary,
",
   success: function(data) {
            if (view)
                ajfun1()
            else 
                ajfun0();

            if (!(arguments[2].responseText.indexOf('", ?TITLE/binary, " Login') > -1 && arguments[2].responseText.indexOf('html') == 1))
                alert(arguments[2].responseText)

		},
		error:function(XMLHttpRequest, textStatus, errorThrown) {
			alert(XMLHttpRequest + ' - ' + textStatus + ' - ' + errorThrown);
		}
  	});

    $('#c_add_rec').click();
  });
1
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
    {ok, Re} = re:compile("/edit",[caseless]),
    MatchVal = re:run(ServerPath, Re),
    <<"
<input id='s' type='hidden' value='0'>
<input id='range_input' type='hidden' value='",RowsPerPage/binary,"'>
<input id='offset' type='hidden' value='",Offset/binary,"'>",
	  (case Hdr of
			<<"1">> -> 
			   <<>>;
			_ ->
				<<(case MatchVal of
					   {match, _} -> <<"
<a href='logout' id='logout'>logout</a>
<a href='javascript:void(0)' id='add_rec'>add</a>
",
									   (add_rec(?TABLE, ServerPath))/binary>>;
					   _  -> 
					<<"<a href='https://", ?HOST, ":7443/db/edit' id='edit'>edit</a>">>
				   end)/binary,
"
<div class='brk spc'>
<table>
<tr>
<td colspan='9'> 
<p style='text-align: center; text-transorm: uppercase;color: #949610; font-size:1.5em'>", ?TITLE/binary, "</p>
</td>
</tr>
</table>
</div>
">>
	    end)/binary,
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
<div class='brk spc5'></div>
<div id='fview'>
<table>
<tr>
<td class='srch'>Field Search</td>
<td>
",
(mk_input_fields(?TABLE))/binary,
"
</td>
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
                <<
"<div class='brk'>
<input id='",Col/binary,"' type='text' name='", Col/binary, "' maxlength='30'>
<span class='attribute'>", (title(Col))/binary,"</span>
</div>
",(mk_input_fields(Cols))/binary>>;
mk_input_fields([]) ->
	<<>>.
    
% Build the result table.

mk_tab(Headers, Rows, Fields, ServerPath) ->
	Hdrs= [<<"<th style='width:175px; text-align:right; vertical-align:top;'>", (title(X))/binary, "</th>">> || X <- Headers],
    <<"<div>",
         (mk_tab2(Rows,Hdrs,Fields, ServerPath))/binary,
   "</div>">>.

mk_tab2([RowTuple|Rows],Hdrs,Fields, ServerPath) ->
	[Id|Row]=tuple_to_list(RowTuple),
%	[{Field,_Srch}|_Rest]=Fields,
	{ok, Re} = re:compile("/edit",[caseless]),
	MatchVal = re:run(ServerPath, Re),
	SPath=case MatchVal of
			  {match, _} ->
				  <<"edit">>;
			  _  -> 
				  <<>>
		  end,
	Mtj=mk_tab2_js(Id, Fields, SPath, ServerPath),
	<<Mtj/binary,
"

<table id='t", Id/binary, "' class='record datat'>",
	  (mk_tab3(1, Id, Row, Hdrs, Fields, SPath))/binary,
	  "</table>
",
	  (mk_tab2(Rows, Hdrs, Fields, ServerPath))/binary>>;
mk_tab2([], _Hdrs, _Fields, _ServerPath) ->
	<<>>.

%
mk_tab2_js(Id, Fields, SPath, ServerPath) ->
case SPath of
	<<"edit">> ->
		<<"
<script type='text/javascript'>

$(document).ready(function(){
    $('#h", Id/binary,"').click(function(){
        $('#t", Id/binary, "').modal({escClose:false, closeClass:'modal-cancel', focus:false, opacity:80, overlayCss: {backgroundColor:'#555'}, persist:true});
        $('#s", Id/binary, "').show();
        $('#c", Id/binary, "').show();
",
(jsedit(Id,Fields))/binary,
"
    });
    $('#d", Id/binary, "').click(function() {
        delclicked = true;
        if (da == true)
            var ans=true;
        else
            var ans = confirm ('Are you sure you want to delete this record');
        if (ans) {
            $.ajax({
		       url: '", ServerPath/binary, "',
		       type: 'GET',
               data: 'tablename=", ?DB/binary, "&n=1&s=4&rpp=0&offset=0&id=", Id/binary, "',
               success: function(data) {
                   if (view)
                       ajfun1();
                   else 
                       ajfun0();
                   if (da == false)
                       if (!(arguments[2].responseText.indexOf('", ?TITLE/binary, " Login') > -1 && arguments[2].responseText.indexOf('html') == 1))
                           alert(arguments[2].responseText);
		       },
               error:function(XMLHttpRequest, textStatus, errorThrown) {
			       alert(XMLHttpRequest + ' - ' + textStatus + ' - ' + errorThrown);
		       }
  	        });
        
            $('#c", Id/binary, "').click()
        
        }
    });

    $('#s", Id/binary, "').click(function() {

        $('#s", Id/binary, "').hide();
        $('#c", Id/binary, "').hide();
",
(jsedit2(Id, Fields))/binary,
"

	$.ajax({
		url: '", ServerPath/binary, "',
		type: 'GET',
		data: 'tablename=", ?DB/binary, "&n=1&s=2", (setfields2(Id, Fields))/binary, ",
		success: function(data) {
            if (view)
                ajfun1();
            else 
                ajfun0();

            if (!(arguments[2].responseText.indexOf('", ?TITLE/binary, " Login') > -1 && arguments[2].responseText.indexOf('html') == 1))
                alert(arguments[2].responseText);
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
    });

});
</script>">>;
	_ ->
		 <<>>
end.
	
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

mk_tab3(First, Id, [Item|RestRow], [Hdr|RestHdrs], [{Field,Srch}|Fields], SPath) ->
	<<
"
<tr>
",
	  (case SPath of
		  <<"edit">> ->
			   case First of
				   1 -> <<"
<th id='th", Id/binary, "' style='width:50px;' rowspan='8' class='rheadg'>
  <a href='javascript:void(0)' id='h", Id/binary, "'>", Id/binary, "</a>
  <input id='d", Id/binary, "' type='button' name='d", Id/binary, "' value='Delete' class=''><br />
  <input id='s", Id/binary, "' type='button' name='s", Id/binary, "' value='Save' class='ebutton'><br />
  <input id='c", Id/binary, "' type='button' name='c", Id/binary, "' value='Cancel' class='ebutton modal-cancel'>
</th>
">>;
				   _ -> 
					   <<>>
			   end;
		   _ -> <<>>
	   end)/binary,
	  Hdr/binary,
	  "
<td>
<div id='d_", Field/binary, "_", Id/binary, "'>",
	  (hl(Item,Srch))/binary,
"</div>",
(case SPath of
	 <<"edit">> -> 
<<"
<input id='ib_", Field/binary, "_", Id/binary, "' class='dbinput' name='' maxlength='", ?MAX_LENB/binary, "' value='", (list_to_binary(htmlize(binary_to_list(Item))))/binary, "'>
<input id='i_", Field/binary, "_", Id/binary, "' class='dbinput' name='' maxlength='", ?MAX_LENB/binary, "' value='", (list_to_binary(htmlize(binary_to_list(Item))))/binary, "'>
">>;
	 _ ->
		 <<>>				 
end)/binary,
"
</td>
</tr>",
	  (mk_tab3(First+1, Id, RestRow, RestHdrs, Fields, SPath))/binary>>;
mk_tab3(_, _Id, [], _Hdrs, _Fields, _SPath) ->
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
