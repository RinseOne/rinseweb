REBAR3 = rebar3
filter =

all:
	@$(REBAR3) do clean, compile, dialyzer

compile:
	@$(REBAR3) compile

cover:
	@$(REBAR3) ct --cover --cover_export_name=all
	@$(REBAR3) cover --verbose

deps:
	@$(REBAR3) get-deps

dialyze:
	@$(REBAR3) dialyzer

doc:
	@$(REBAR3) edoc

rel: all
	@$(REBAR3) release

release: set-version
	git commit -a -m "Update version to $(version)"
	git tag $(version)
	git push --atomic origin main $(version)

run:
	@$(REBAR3) shell

set-version:
	@sed -i "s/{rinseweb, \"[[:digit:]]\+\.[[:digit:]]\+\.[[:digit:]]\+\"}/{rinseweb, \"$(version)\"}/" rebar.config
	@sed -i "s/{vsn, \"[[:digit:]]\+\.[[:digit:]]\+\.[[:digit:]]\+\"}/{vsn, \"$(version)\"}/" src/rinseweb.app.src

tests:
ifeq ($(filter),)
	@$(REBAR3) ct --logdir logs/ct
else
	@$(REBAR3) ct --logdir logs/ct --suite=$(filter)
endif

.PHONY: all compile cover deps dialyze doc rel release run set-version tests
