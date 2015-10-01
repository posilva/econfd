# Copyright 2012 Erlware, LLC. All Rights Reserved.
#
# This file is provided to you under the Apache License,
# Version 2.0 (the "License"); you may not use this file
# except in compliance with the License.  You may obtain
# a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#

APP=econfd

DEPS_PLT=./.deps_plt
DEPS=erts kernel stdlib inets crypto mnesia public_key ssl

# =============================================================================
# Verify that the programs we need to run are installed on this system
# =============================================================================
# REBAR=$(shell which rebar)
REBAR=rebar
ifeq ($(REBAR),)
$(error "Rebar not available on this system")
endif

.PHONY: all compile clean dialyze typer distclean \
  deps rebuild test help

all: deps compile dialyze
jenkins: deps compile tar

# =============================================================================
# Rules to build the system
# =============================================================================

deps:
	- $(REBAR) get-deps
	- $(REBAR) compile


compile:
	- $(REBAR) skip_deps=true compile

$(DEPS_PLT):
	@echo Building $(DEPS_PLT)
	- dialyzer --build_plt \
	   -r deps \
	   --output_plt $(DEPS_PLT) \
	   --apps $(DEPS)

dialyze: $(DEPS_PLT) compile
	- dialyzer -r ebin \
	    --fullpath \
		-Wunmatched_returns \
		-Werror_handling \
		-Wrace_conditions \
		-Wunderspecs \
        --plt $(DEPS_PLT) 2>&1 | grep -v -f .dialyzerignore

typer:
	typer --plt $(DEPS_PLT) \
		  -r ./src

test:
	$(REBAR) eunit

release: compile
	/bin/bash -c "pushd rel && \
	rebar generate && \
	popd"

console: release
### > Next line  is for helping dev
	cp ./etc/dev.app.config ./rel/$(APP)/etc/app.config
	./rel/$(APP)/bin/$(APP) console

clean:
	- $(REBAR) skip_deps=true clean

distclean: clear-log clean
	- rm -rf $(DEPS_PLT)
	- rm -rvf deps
	- rm -rvf rel/$(APP)

rebuild: distclean deps compile dialyze

xref: compile
	rebar xref 2>&1 | grep -v -f .xrefignore

checks: xref dialyze

ct: compile
	rebar -v ct

###-----
###	This is a helper rule that allow to l(module_name) when running make console
###-----
reload:	compile
	cp ./ebin/*.beam ./rel/$(APP)/lib/$(shell ls -lrt ./rel/$(APP)/lib/ | grep $(APP) | tr -s ' ' | cut -f9  -d' ')/ebin/.

clear-log:
	rm -rf *.log*
	rm -rf *.dump
	rm -rf Mnesia.*
	rm -rf leader_*

