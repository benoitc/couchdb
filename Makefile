## Copyright 2010 Benoît Chesneau
## 
## Licensed under the Apache License, Version 2.0 (the "License"); you may not
## use this file except in compliance with the License. You may obtain a copy of
## the License at
##
##   http://www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
## WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
## License for the specific language governing permissions and limitations under
## the License.

ACLOCAL_AMFLAGS = -I m4

all: compile

compile:
	@echo "==> couchjs (compile)"
	@cd couchjs && python scons/scons.py
	@./rebar compile

clean:
	@echo "==> couchjs (clean)"
	@cd couchjs && python scons/scons.py --clean
	@./rebar clean


check: dev
	@./rebar eunit
	@ERL_FLAGS="-pa `pwd`/apps/couch/ebin `pwd`/apps/couch/test/etap" \
		prove apps/couch/test/etap/*.t

dist: compile
	@rm -rf rel/couchdb
	@./rebar generate

distclean: clean
	@rm -rf rel/couchdb
	@rm -rf rel/dev*
	@rm -f rel/couchdb.config
	@rm -rf deps
	@rm -rf rel/tmpdata


include install.mk
install: dist
	@mkdir -p $(prefix)
	@cp -R rel/rcouchdb/* $(prefix)
	@mkdir -p $(data_dir)
	@chown $(user) $(data_dir)
	@mkdir -p $(view_dir)
	@chown $(user) $(view_dir)
	@touch $(prefix)/var/log/couchdb.log
	@chown $(user) $(prefix)/var/log/couchdb.log

dev: compile
	@rm -rf rel/tmpdata
	@rm -rf rel/dev
	@echo "==> Building development node (ports 15984/15986)"
	@./rebar generate target_dir=dev overlay_vars=dev.config
	@echo "\n\
Development node is built, and can be started using ./rel/dev/bin/couchdb.\n" 
