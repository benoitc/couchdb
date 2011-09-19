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

jiffyebindir = $(localerlanglibdir)/jiffy-0.1.0/ebin
jiffyprivdir = $(localerlanglibdir)/jiffy-0.1.0/priv

EXTRA_DIST =  \
    c_src/jiffy.h \
	$(jiffy_file_collection)

CLEANFILES = \
    $(jiffyebin_make_generated_file_list) \
    $(jiffypriv_make_generated_file_list) \
    priv/jiffy.so

JIFFY_C_SRCS = \
    c_src/jiffy.c \
    c_src/decoder.c \
    c_src/encoder.c \
    c_src/utf8.c \
    c_src/util.c

jiffy_file_collection = \
    src/jiffy.app.src \
    src/jiffy.erl \
	src/jiffy_test_util.erl \
	test/002-literals.t \
	test/003-numbers.t \
	test/004-strings.t \
	test/005-arrays.t \
	test/006-maps.t \
	test/007-compound.t \
	test/008-halfword.t \
	$(JSON_C_SRCS)

jiffyebin_make_generated_file_list = \
    ebin/jiffy.app \
    ebin/jiffy.beam \
	ebin/jiffy_test_util.beam


if USE_JIFFY
jiffyebin_DATA = \
    $(jiffyebin_make_generated_file_list)

jiffypriv_LTLIBRARIES = jiffy.la

jiffy_la_SOURCES = $(JIFFY_C_SRCS)
jiffy_la_CFLAGS = $(ERLANG_FLAGS)
jiffy_la_LDFLAGS = -module -avoid-version

if WINDOWS
jiffy_la_LDFLAGS += -no-undefined
endif

priv/jiffy.so: jiffy.la
	@mkdir -p ./priv
	cp .libs/jiffy.so $@

all: priv/jiffy.so

check:
	$(abs_top_builddir)/test/etap/run $(abs_top_srcdir)/src/jiffy/test

endif

ebin/%.app: src/%.app.src
	@mkdir -p ebin/
	cp $< $@

ebin/%.beam: src/%.erl
	@mkdir -p ebin/
	$(ERLC) $(ERLC_FLAGS) -o ebin/ $<
