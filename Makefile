REPO		?= stanchion
HEAD_REVISION   ?= $(shell git describe --tags --exact-match HEAD 2>/dev/null)
PKG_REVISION    ?= $(shell git describe --tags 2>/dev/null)
PKG_BUILD        = 1
BASE_DIR         = $(shell pwd)
ERLANG_BIN       = $(shell dirname $(shell which erl 2>/dev/null) 2>/dev/null)
OTP_VER          = $(shell erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().')
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

rel-rpm: compile
	$(REBAR) as rpm release
	cp -a _build/rpm/rel/stanchion rel/

rel-deb: compile
	$(REBAR) as deb release
	cp -a _build/deb/rel/stanchion rel/

relclean:
	rm -rf $(REL_DIR)
	rm -rf rel/stanchion

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
## Version and naming variables for distribution and packaging
##

# Tag from git with style <tagname>-<commits_since_tag>-<current_commit_hash>
# Ex: When on a tag:            riak-1.0.3   (no commits since tag)
#     For most normal Commits:  riak-1.1.0pre1-27-g1170096
#                                 Last tag:          riak-1.1.0pre1
#                                 Commits since tag: 27
#                                 Hash of commit:    g1170096
REPO_TAG 	:= $(shell git describe --tags)

# Split off repo name
# Changes to 1.0.3 or 1.1.0pre1-27-g1170096 from example above
REVISION = $(shell echo $(REPO_TAG) | sed -e 's/^$(REPO)-//')

# Primary version identifier, strip off commmit information
# Changes to 1.0.3 or 1.1.0pre1 from example above
MAJOR_VERSION	?= $(shell echo $(REVISION) | sed -e 's/\([0-9.]*\)-.*/\1/')

# Name resulting directory & tar file based on current status of the git tag
# If it is a tagged release (PKG_VERSION == MAJOR_VERSION), use the toplevel
#   tag as the package name, otherwise generate a unique hash of all the
#   dependencies revisions to make the package name unique.
#   This enables the toplevel repository package to change names
#   when underlying dependencies change.
NAME_HASH = $(shell git hash-object distdir/$(CLONEDIR)/$(MANIFEST_FILE) 2>/dev/null | cut -c 1-8)
PKG_ID := "$(REPO_TAG)-OTP$(OTP_VER)"

##
## Packaging targets
##

# Yes another variable, this one is repo-<generatedhash
# which differs from $REVISION that is repo-<commitcount>-<commitsha>
PKG_VERSION = $(shell echo $(PKG_ID) | sed -e 's/^$(REPO)-//')

package:
	mkdir -p rel/pkg/out/stanchion-$(PKG_ID)
	git archive --format=tar HEAD | gzip >rel/pkg/out/$(PKG_ID).tar.gz
	$(MAKE) -f rel/pkg/Makefile

packageclean:
	rm -rf rel/pkg/out/*


.PHONY: package
export PKG_VERSION PKG_ID PKG_BUILD BASE_DIR ERLANG_BIN REBAR OVERLAY_VARS RELEASE

# Package up a devrel to save time later rebuilding it
pkg-devrel: devrel
	echo -n $(PKG_REVISION) > VERSION
	tar -czf $(PKG_ID)-devrel.tar.gz dev/ VERSION
	rm -rf VERSION

pkg-rel: rel
	tar -czf $(PKG_ID)-rel.tar.gz -C rel/ .
