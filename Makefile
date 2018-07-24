REBAR = rebar

all:
	@$(REBAR) get-deps compile

edoc:
	@$(REBAR) doc

build_plt:
	@$(REBAR) build-plt

dialyzer:
	@$(REBAR) dialyze
