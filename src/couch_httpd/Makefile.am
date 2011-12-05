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

couch_httpdlibdir = $(localerlanglibdir)/couch_httpd-0.1
couch_httpdincludedir = $(couch_httpdlibdir)/include
couch_httpdebindir = $(couch_httpdlibdir)/ebin

couch_httpdinclude_DATA = $(include_files)
couch_httpdebin_DATA = $(compiled_files)

include_files = 

source_files = \
    src/couch_httpd.app.src \
    src/couch_httpd.erl \
    src/couch_httpd_auth.erl \
    src/couch_httpd_db.erl \
    src/couch_httpd_external.erl \
    src/couch_httpd_misc_handlers.erl \
    src/couch_httpd_oauth.erl \
    src/couch_httpd_proxy.erl \
    src/couch_httpd_rewrite.erl \
    src/couch_httpd_stats_handlers.erl \
    src/couch_httpd_vhost.erl

test_files = \
    test/01-load.t \
    test/02-vhosts.t \
    test/03-http-proxy.t

compiled_files = \
    ebin/couch_httpd.app \
    ebin/couch_httpd.beam \
    ebin/couch_httpd_auth.beam \
    ebin/couch_httpd_db.beam \
    ebin/couch_httpd_external.beam \
    ebin/couch_httpd_misc_handlers.beam \
    ebin/couch_httpd_oauth.beam \
    ebin/couch_httpd_proxy.beam \
    ebin/couch_httpd_rewrite.beam \
    ebin/couch_httpd_stats_handlers.beam \
    ebin/couch_httpd_vhost.beam

EXTRA_DIST = $(include_files) $(source_files) $(test_files)
CLEANFILES = $(compiled_files)

check:
	$(abs_top_builddir)/test/etap/run $(abs_top_srcdir)/src/couch_httpd/test

ebin/%.app: src/%.app.src
	@mkdir -p ebin/
	sed -e "s|%version%|@version@|g" \
	< $< > $@

ebin/%.beam: src/%.erl $(include_files)
	@mkdir -p ebin/
	$(ERLC) -Wall -I$(top_srcdir)/src -I$(top_srcdir)/src/couchdb \
        -o ebin/ $(ERLC_FLAGS) ${TEST} $<;

