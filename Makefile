REBAR = rebar

all:
	@$(REBAR) get-deps compile

dev:
	@$(REBAR) -C rebar.dev.config get-deps compile

edoc:
	@$(REBAR) doc

build_plt:
	@$(REBAR) build-plt

dialyzer:
	@$(REBAR) dialyze
