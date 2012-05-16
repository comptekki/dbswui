#!/bin/sh
erl -sname dbswui -pa ebin -pa deps/*/ebin -s dbswui \
	-eval "io:format(\"~n~nThe following sites are available:~n\")." \
	-eval "io:format(\"* db1 : http://localhost:9080/db1~n\")." \
	-eval "io:format(\"* db1 : http://localhost:9443/db1~n\")."