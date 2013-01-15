#!/bin/sh

# starup script

erl -sname dbswui -pa ebin -pa deps/*/ebin -s dbswui \
	-eval "io:format(\"~n~nThe following sites are available:~n\
	* db : http://localhost:7080/db~n\
	* db : https://localhost:7443/db~n\
	~n~nOr to Edit/Delete/Update Records:~n\
	* db : https://localhost:7443/db/edit~n~n\")."
