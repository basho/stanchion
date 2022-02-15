REPO		?= stanchion
HEAD_REVISION   ?= $(shell git describe --tags --exact-match HEAD 2>/dev/null)
PKG_REVISION    ?= $(shell git describe --tags 2>/dev/null)
PKG_BUILD        = 1
BASE_DIR         = $(shell pwd)
ERLANG_BIN       = $(shell dirname $(shell which erl 2>/dev/null) 2>/dev/null)
OTP_VER          = $(shell erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell)
REBAR           ?= $(BASE_DIR)/rebar3
OVERLAY_VARS    ?=

.PHONY: rel deps test

all: deps compile

compile: deps
	@($(REBAR) compile)

deps:
	@$(REBAR) upgrade --all

clean:
	@$(REBAR) clean

distclean: clean devclean relclean
	@rm -rf _build

##
## Release targets
##
rel: compile
	@$(REBAR) as rel release
	@cp -a _build/rel/rel/stanchion rel/

rel-rpm: compile relclean
	@$(REBAR) as rpm release
	@cp -a _build/rpm/rel/stanchion rel/

rel-deb: compile relclean
	@$(REBAR) as deb release
	@cp -a _build/deb/rel/stanchion rel/

rel-fbsdng: compile relclean
	@$(REBAR) as fbsdng release
	@cp -a _build/fbsdng/rel/stanchion rel/

rel-osx: compile relclean
	@$(REBAR) as osx release
	@cp -a _build/osx/rel/stanchion rel/

rel-docker: compile relclean
	@REBAR_CONFIG=rebar.docker.config $(REBAR) release
	@cp -a _build/default/rel/stanchion rel/

relclean:
	@rm -rf _build/rel rel/stanchion

test:
	@$(REBAR) eunit
	@$(REBAR) dialyzer

##
## Developer targets
##
devrel: all
	@mkdir -p dev
	@$(REBAR) as rel release -o dev --overlay_vars rel/dev_vars.config

stagedevrel: devrel

devclean: clean
	rm -rf dev

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
