ERL ?= erl
APP := simplec

.PHONY: deps

all: compile

compile: deps
	@./rebar compile

deps:
	@./rebar get-deps

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

test: all
	@(./rebar skip_deps=true eunit)

tags:
	find . -name "*.[he]rl" -print | etags -

DEPSOLVER_PLT=$(CURDIR)/.depsolver_plt
$(DEPSOLVER_PLT):
	dialyzer --output_plt $(DEPSOLVER_PLT) --build_plt \
	    --apps erts kernel stdlib crypto http_uri -r deps

dialyzer: $(DEPSOLVER_PLT)
	dialyzer --plt $(DEPSOLVER_PLT) -Wrace_conditions --src src

