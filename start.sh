#!/bin/sh
erl -sname dbswui -pa ebin -pa deps/*/ebin -s dbswui \
	-eval "io:format(\"~n~nThe following examples are available:~n\")." \
	-eval "io:format(\"* db1 : http://localhost:8080/db1~n\")."
