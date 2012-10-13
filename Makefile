
APP := TR069
REBAR='./rebar'

all: compile

compile:
	$(REBAR) -v compile

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
	$(REBAR) -v eunit skip_deps=true suite=tr_soap_types


.PHONY: ctest

