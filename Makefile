PREFIX    ?= /usr
ERL_ROOT  := $(PREFIX)/lib/erlang

all: build

build:
	rebar -C rebar.conf compile

clean:
	rebar -C rebar.conf clean

test:
	rebar -C rebar.conf compile eunit

install: build
	mkdir -p ${ERL_ROOT}/lib/p2p/ebin
	mkdir -p ${ERL_ROOT}/lib/p2p/include
	install -m0644 ebin/* ${ERL_ROOT}/lib/p2p/ebin
	install -m0644 include/* ${ERL_ROOT}/lib/p2p/include

uninstall:
	rm -rf ${ERL_ROOT}/lib/p2p

.PHONY: all build test install uninstall
