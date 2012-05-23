all: app

app: get-deps
	@./rebar compile
	@./mkcert.bash

get-deps:
	@./rebar get-deps

clean:
	@./rebar clean
	rm -f erl_crash.dump

dist-clean: clean
