PROJECT = survival

DIALYZER = dialyzer
REBAR := ./rebar

.PHONY: all build-plt clean-docs deps dialyze doc test clean release

all: deps
	$(REBAR) compile

deps:
	$(REBAR) get-deps

doc: clean-docs
	$(REBAR) doc skip_deps=true
	
clean-docs:
	rm -f doc/*.css
	rm -f doc/*.html
	rm -f doc/*.png
	rm -f doc/edoc-info

test:
	$(REBAR) eunit skip_deps=true

clean:
	$(REBAR) clean
	rm -f erl_crash.dump
    
release: all test
	dialyzer --src src/*.erl
	
# Dialyzer.

build-plt:
	@$(DIALYZER) --build_plt --output_plt .$(PROJECT).plt \
		--apps kernel stdlib sasl inets deps/*

dialyze:
	@$(DIALYZER) --src src --plt .$(PROJECT).plt --no_native \
		-Werror_handling -Wrace_conditions -Wunmatched_returns # -Wunderspecs