.PHONY: deps

all: compile

compile:
	@./rebar compile

app:
	@./rebar compile skip_deps=true

deps:
	@./rebar get-deps

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

start: app
	erl -pa $(PWD)/istanbus_core/ebin -pa $(PWD)/istanbus_thrift/ebin -pa $(PWD)/istanbus_web/ebin -pa $(PWD)/deps/*/ebin -boot start_sasl -s reloader -s istanbus_core -s istanbus_web -s istanbus_thrift

# detached start
dstart: app
	erl -pa $(PWD)/istanbus_core/ebin -pa $(PWD)/istanbus_thrift/ebin -pa $(PWD)/istanbus_web/ebin -pa $(PWD)/deps/*/ebin -boot start_sasl -s reloader -s istanbus_core -s istanbus_web -s istanbus_thrift -detached
