
APP := tr69
VSN = $(shell sed -n 's/.*{vsn,.*"\(.*\)"}.*/\1/p' src/$(APP).app.src)

REBAR='./rebar'

.PHONY: deps ctest

all: compile

compile: deps 
	$(REBAR) -v compile

app:
	@./rebar compile skip_deps=true

deps:
	$(REBAR) check-deps || $(REBAR) get-deps

clean:
	$(REBAR) clean
	-@rm -rf .eunit/*.beam erl_crash.dump
	-@rm *_fsm.beam *_fsm.dot *_fsm.erl *_fsm.jpeg
	-@rm *_eqc.beam *_eqc.dot *_eqc.erl *_eqc.jpg


distclean:
	$(REBAR) clean delete-deps

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

SUT=creature
SUITE=$(SUT)_tests
utest: app
	$(REBAR) -v eunit skip_deps=true suite=$(SUITE)

ut-shell:
	exec erl -pa $(PWD)/apps/*/ebin -pa $(PWD)/deps/*/ebin -pa $(PWD)/.eunit -boot start_sasl

vztest: utest
	VIEWER=firefox fsm_dynamic test/$(SUT).erl $(PWD)/ebin

eqctest: utest
	fsm_eqc test/$(SUT) $(PWD)/ebin
	firefox $(SUT)_eqc.jpg

eqcunit:
	EQC_VIEWER=firefox $(REBAR) -v eunit skip_deps=true suite=$(SUT)_eqc

ctest: 
	$(REBAR) -v compile ct skip_deps=true suites=tr69_trace case=app_loging_tc

webstart: app
	exec erl -pa $(PWD)/ebin -pa $(PWD)/deps/*/ebin 	\
		-config priv/sys.config				\
		-boot start_sasl -s lager -s tr69		\
		|| echo Exit



