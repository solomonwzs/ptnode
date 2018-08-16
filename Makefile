REBAR = rebar

all:
	@$(REBAR) get-deps compile

dev:
	@$(REBAR) -C rebar.dev.config get-deps compile

clean:
	@[ ! -d ebin ] || rm -r ebin

edoc:
	@$(REBAR) doc

build_plt:
	@$(REBAR) build-plt

dialyzer: all
	@$(REBAR) dialyze
