#!/bin/sh
erl -sname dbswui -pa ebin -pa deps/*/ebin -s dbswui \
	-eval "io:format(\"~n~nThe following sites are available:~n\")." \
	-eval "io:format(\"* db : http://localhost:7080/db~n\")." \
	-eval "io:format(\"* db : https://localhost:7443/db~n\")."
	-eval "io:format(\"~n~nOr to Edit/Delete/Update Records:~n\")." \
	-eval "io:format(\"* db : http://localhost:7443/db/edit~n\")."
