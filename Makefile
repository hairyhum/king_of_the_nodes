#!/usr/bin/env sh
APP_NAME = king

dep:
	./rebar get-deps

rebuild: clean compile

clean: 
	./rebar clean
compile:
	./rebar compile

start_test_nodes:
	./test_nodes.sh

who:
	./who_is_the_king.escript

start:
	./start.sh ${NAME}

attach:
	to_erl log${NODE}/

build_plt:
	dialyzer --build_plt --output_plt $(APP_NAME).plt --apps erts kernel stdlib crypto public_key ssl edoc 
analyze: compile
	dialyzer --plt $(APP_NAME).plt -r ebin 
