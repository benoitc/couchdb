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

couch_changeslibdir = $(localerlanglibdir)/couch_changes-0.1
couch_changesincludedir = $(couch_changeslibdir)/include
couch_changesebindir = $(couch_changeslibdir)/ebin

couch_changesinclude_DATA = $(include_files)
couch_changesebin_DATA = $(compiled_files)

include_files = \
    include/couch_changes.hrl

source_files = \
    src/couch_changes.app.src \
    src/couch_changes.erl \
    src/couch_changes_httpd.erl

test_files = \
    test/01-load.t \
    test/02-changes.t

compiled_files = \
    ebin/couch_changes.app \
    ebin/couch_changes.beam \
    ebin/couch_changes_httpd.beam

EXTRA_DIST = $(include_files) $(source_files) $(test_files)
CLEANFILES = $(compiled_files)

check:
	$(abs_top_builddir)/test/etap/run $(abs_top_srcdir)/src/couch_changes/test

ebin/%.app: src/%.app.src
	@mkdir -p ebin/
	sed -e "s|%version%|@version@|g" \
	< $< > $@

ebin/%.beam: src/%.erl $(include_files)
	@mkdir -p ebin/
	$(ERLC) -Wall -I$(top_srcdir)/src -I$(top_srcdir)/src/couchdb \
        -o ebin/ $(ERLC_FLAGS) ${TEST} $<;

