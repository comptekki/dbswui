-define(DBHOST, "localhost").
-define(HOST, "localhost").
-define(DB, <<"dbooks">>).
-define(USERNAME, "postgres").
-define(PASSWORD, "pguser").
-define(PORT, 5432).
-define(JQUERY, "/static/jquery-1.7.2.min.js").
-define(DBTITLE, <<"Duplicate Books">>).
-define(MAX_LENB, <<"300">>).
-define(MAX_LEN, 300).
-define(TABLE, 	[<<"title">>, <<"author_editor">>, <<"date_of_publication">>, <<"publisher">>, <<"key_words">>, <<"notes">>, <<"valuation">>, <<"purchase_price">>]).
-define(ORDER_BY, " order by title").
-define(TITLES,
		case Title of
			<<"title">> -> <<"Title">>;
			<<"author_editor">> -> <<"Author Editor">>;
			<<"date_of_publication">> -> <<"Date of Publication">>;
			<<"publisher">> -> <<"Publisher">>;
			<<"key_words">> -> <<"Key Words">>;
			<<"notes">> -> <<"Notes">>;
			<<"valuation">> -> <<"Valuation">>;
			<<"purchase_price">> -> <<"Purchase Price">>
		end).
-define(CONF,"/usr/local/src/dbswui/src/db.conf").
-define(MAXAGE, 500).
