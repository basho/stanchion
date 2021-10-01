REPO		?= stanchion
HEAD_REVISION   ?= $(shell git describe --tags --exact-match HEAD 2>/dev/null)
PKG_REVISION    ?= $(shell git describe --tags 2>/dev/null)
PKG_BUILD        = 1
BASE_DIR         = $(shell pwd)
ERLANG_BIN       = $(shell dirname $(shell which erl 2>/dev/null) 2>/dev/null)
OTP_VER          = $(shell echo $(ERLANG_BIN) | rev | cut -d "/" -f 2 | rev)
REBAR           ?= $(BASE_DIR)/rebar3
OVERLAY_VARS    ?=

.PHONY: rel deps test

all: deps compile

compile: deps
	@($(REBAR) compile)

deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean

distclean: clean
	@$(REBAR) delete-deps
	@rm -rf $(PKG_ID).tar.gz

parity-test:
	@python test/prototype_parity.py -v

##
## Release targets
##
rel: deps compile
	@$(REBAR) compile
	@$(REBAR) as rel release
	@cp -a _build/rel/rel/stanchion rel/

relclean:
	rm -rf rel/stanchion

##
## Developer targets
##
stage : rel
	$(foreach dep,$(wildcard deps/*), rm -rf rel/stanchion/lib/$(shell basename $(dep))-* && ln -sf $(abspath $(dep)) rel/stanchion/lib;)
	$(foreach app,$(wildcard apps/*), rm -rf rel/stanchion/lib/$(shell basename $(app))-* && ln -sf $(abspath $(app)) rel/stanchion/lib;)

devrel: all
	mkdir -p dev
	@$(REBAR) as rel release -o dev --overlay_vars rel/dev_vars.config

stagedevrel: devrel

devclean: clean
	rm -rf dev

##
## Doc targets
##
orgs: orgs-doc orgs-README

orgs-doc:
	@emacs -l orgbatch.el -batch --eval="(riak-export-doc-dir \"doc\" 'html)"

orgs-README:
	@emacs -l orgbatch.el -batch --eval="(riak-export-doc-file \"README.org\" 'ascii)"
	@mv README.txt README

DIALYZER_APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	xmerl webtool eunit syntax_tools compiler
PLT ?= $(HOME)/.stanchion_dialyzer_plt

##
## Packaging targets
##
.PHONY: package
export PKG_VERSION PKG_ID PKG_BUILD BASE_DIR ERLANG_BIN REBAR OVERLAY_VARS RELEASE

package.src: deps
	mkdir -p package
	rm -rf package/$(PKG_ID)
	git archive --format=tar --prefix=$(PKG_ID)/ $(PKG_REVISION)| (cd package && tar -xf -)
	${MAKE} -C package/$(PKG_ID) deps
	mkdir -p package/$(PKG_ID)/priv
	git --git-dir=.git describe --tags >package/$(PKG_ID)/priv/vsn.git
	for dep in package/$(PKG_ID)/deps/*; do \
             echo "Processing dep: $${dep}"; \
             mkdir -p $${dep}/priv; \
             git --git-dir=$${dep}/.git describe --tags >$${dep}/priv/vsn.git; \
        done
	find package/$(PKG_ID) -depth -name ".git" -exec rm -rf {} \;
	tar -C package -czf package/$(PKG_ID).tar.gz $(PKG_ID)

dist: package.src
	cp package/$(PKG_ID).tar.gz .

package: package.src
	${MAKE} -C package -f $(PKG_ID)/deps/node_package/Makefile

pkgclean: distclean
	rm -rf package
