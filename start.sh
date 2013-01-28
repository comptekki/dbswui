#!/bin/sh

# starup script

erl -sname dbswui -pa ebin -pa deps/*/ebin -s dbswui \
	-eval "io:format(\"~n~nThe following sites are available:~n\
	* db : http://localhost:9080/db~n\
	* db : https://localhost:9443/db~n\
	~n~nOr to Edit/Delete/Update Records:~n\
	* db : https://localhost:9443/db/edit~n~n\")."
