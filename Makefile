
APP := tr69
VSN = $(shell sed -n 's/.*{vsn,.*"\(.*\)"}.*/\1/p' src/$(APP).app.src)

REBAR='./rebar'

all: compile

compile: deps
	$(REBAR) -v compile

app:
	@./rebar compile skip_deps=true

deps:
	$(REBAR) check-deps || $(REBAR) get-deps

clean:
	$(REBAR) clean
	@rm -rf .eunit/*.beam

distclean:
	$(REBAR) clean delete-deps

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

utest:
	$(REBAR) -v eunit skip_deps=true ## suite=tr_soap_types

ctest: 
	$(REBAR) -v compile ct skip_deps=true suites=tr69_trace case=app_loging_tc

webstart: app
	exec erl -pa $(PWD)/ebin -pa $(PWD)/deps/*/ebin 	\
		-config priv/sys.config				\
		-boot start_sasl -s lager -s tr69		\
		|| echo Exit

.PHONY: ctest

