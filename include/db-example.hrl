-define(HOST, "localhost").
-define(DB, <<"dbname">>).
-define(USERNAME, "username").
-define(PASSWORD, "password").
-define(JQUERY, "/static/jquery.js").
-define(DBTITLE, <<"DB Title">>).
-define(MAX_LENB, <<"300">>).
-define(MAX_LEN, 300).
-define(TABLE, 	[<<"field1">>, <<"field2">>, <<"field3">>]).
-define(ORDER_BY, " order by some_field").
-define(TITLES,
		case Title of
			<<"field1">> -> <<"Field1">>;
			<<"field2">> -> <<"Field2">>;
			<<"field3">> -> <<"Field3">>
		end).
-define(CONF,"/path/to//db1.conf").
