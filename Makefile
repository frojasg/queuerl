REBAR = $(shell pwd)/rebar3

.PHONY: test

test:
	REBAR_PROFILE=test $(REBAR) compile && erl -noshell -pa _build/test/lib/queuerl/ebin -pa _build/test/lib/proper/ebin -s queuerl_test -s erlang halt
