REBAR = ./rebar3

compile:
	$(verbose) $(REBAR) compile

compile-dev:
	$(verbose) $(REBAR) as dev compile

distclean:
	$(verbose) rm -rf _build rebar.lock

dev: compile-dev
	$(verbose) erl -pa _build/dev/lib/*/ebin _build/dev/lib/*/include

