REBAR3 = rebar3

all:
	@$(REBAR3) do clean, compile, dialyzer
#	@$(REBAR3) do clean, compile, ct, dialyzer

compile:
	@$(REBAR3) compile

dialyze:
	@$(REBAR3) dialyzer

deps:
	@$(REBAR3) get-deps

rel: all
	@$(REBAR3) release

run:
	@$(REBAR3) shell

doc:
	@$(REBAR3) edoc

tests:
	@$(REBAR3) ct --logdir logs/ct

set-version:
	@sed -i "s/{rinseweb, \"[[:digit:]]\+\.[[:digit:]]\+\.[[:digit:]]\+\"}/{rinseweb, \"$(version)\"}/" rebar.config
	@sed -i "s/{vsn, \"[[:digit:]]\+\.[[:digit:]]\+\.[[:digit:]]\+\"}/{vsn, \"$(version)\"}/" src/rinseweb.app.src

.PHONY: all compile dialyze deps rel run doc tests set-version
