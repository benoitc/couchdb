## Licensed under the Apache License, Version 2.0 (the "License"); you may not
## use this file except in compliance with the License. You may obtain a copy of
## the License at
##
## http://www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
## WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
## License for the specific language governing permissions and limitations under
## the License

erlzmqlibdir = $(localerlanglibdir)/erlzmq-$(version)
erlzmqbindir = $(erlzmqlibdir)/ebin
erlzmqprivdir = $(erlzmqlibdir)/priv

erlzmqbin_DATA = $(beam_files) $(include_files)
CLEANFILES = $(beam_files) ebin/oauth.app
EXTRA_DIST = $(erl_files) $(include_files) src/oauth.app.in


EI_CXXFLAGS = -I$(ERLANG_ERTS_DIR)/include				\
	      -I$(ERLANG_LIB_DIR_erl_interface)/include
EI_LDFLAGS = -L$(ERLANG_LIB_DIR_erl_interface)/lib
EI_LIBS = -lei_st



erlzmqpriv_LTLIBRARIES = libzmq_drv.la
libzmq_drv_la_SOURCES = $(c_files)
libzmq_drv_la_LDFLAGS = -module -avoid-version \
						$(ERL_DRIVER_LDFLAGS) \
						$(EI_LDFLAGS) $(EI_LIBS) 
libzmq_drv_la_LIBADD = -lzmq
libzmq_drv_la_CXXFLAGS = $(EI_CXXFLAGS) $(CXXFLAGS)
libzmq_drv_la_CPPFLAGS =  $(EI_CXXFLAGS) $(CXXFLAGS)

priv/zmq_drv.so: libzmq_drv.la 
	@mkdir -p priv
	@rm -f $@
	$(LN_S) $(abs_top_builddir)/src/erlzmq/.libs/zmq_drv.so $@

all: priv/zmq_drv.so

include_files = \
			include/zmq.hrl

erl_file = \
		   src/zmq.erl \
		   src/zmq_pubserver.erl \
		   src/zmq_repserver.erl \
		   src/zmq_reqclient.erl \
		   src/zmq_subclient.erl

beam_files = \
	ebin/zmq.beam \
	ebin/zmq_pubserver.beam \
	ebin/zmq_repserver.beam \
	ebin/zmq_reqclient.beam \
	ebin/zmq_subclient.beam


c_files = \
		  c_src/zmq_drv.cpp

%.app: %.app.in
	cp $< $@

ebin/%.beam: src/%.erl include/zmq.hrl
	$(ERLC) $(ERLC_FLAGS) -I include/ -o `dirname $@` $<
