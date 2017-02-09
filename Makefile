ERL ?= erl
APP := simplec
USER_UNITS := ~/.config/systemd/user/
SERVICE := $(APP).service

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

install_user: test
	cp priv/$(SERVICE) $(USER_UNITS)/$(SERVICE)
	mkdir -p ~/bin/
	ln -s $$(realpath .)/$(APP) ~/bin/$(APP)
	systemctl --user enable $(SERVICE)

uninstall_user:
	systemctl --user stop $(SERVICE)
	systemctl --user disable $(SERVICE)
	rm -f $(USER_UNITS)/$(SERVICE) ~/bin/$(APP)

DEPSOLVER_PLT=$(CURDIR)/.depsolver_plt
$(DEPSOLVER_PLT):
	dialyzer --output_plt $(DEPSOLVER_PLT) --build_plt \
	    --apps erts kernel stdlib crypto http_uri -r deps

dialyzer: $(DEPSOLVER_PLT)
	dialyzer --plt $(DEPSOLVER_PLT) -Wrace_conditions --src src

