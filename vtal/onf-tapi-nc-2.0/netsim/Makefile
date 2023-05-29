include $(NCS_DIR)/src/ncs/build/include.netsim.mk


## Basic erlc flags
## APPSCRIPT creates oncnotif.app file.
ERLC        ?= erlc
DEBUG_FLAGS  = $(patsubst debug,-Ddebug,$(TYPE))
ERLC_FLAGS  += -W +debug_info $(DEBUG_FLAGS) -I $(NCS_DIR)/erlang/econfd/include
APPSCRIPT   ?= '$$vsn=shift; $$mods=""; while(@ARGV){ $$_=shift; s/^([A-Z].*)$$/\'\''$$1\'\''/; $$mods.=", " if $$mods; $$mods .= $$_; } while(<>) { s/%VSN%/$$vsn/; s/%MODULES%/$$mods/; print; }'
PACKAGE_MK_TMP_LOCAL := .package.mk

## If you need additional init xml files, put them in
## the current directory, and they will be picked up here.

XML = $(wildcard *.xml)
XML += $(NCS_DIR)/netsim/confd/var/confd/cdb/aaa_init.xml

EXTRA_FILES = confd.conf.netsim

YANG := $(wildcard ../src/yang/tapi-*.yang)
SUBMODULES := $(shell grep -l belongs-to ../src/yang/*.yang \
                        | xargs grep -l submodule)
YANG_FILES := $(filter-out $(SUBMODULES),$(YANG))

FXS := $(YANG_FILES:../src/yang/%.yang=%.fxs)
FXS += $(patsubst %.yang,%.fxs,$(wildcard *.yang))

HRL := $(FXS:tapi-common%.fxs=tapi-common%.hrl)
HRL += onc-netsim-settings.hrl

FLAGS = --yangpath ../src/yang

NETSIM_ERL_FILES := $(wildcard *.erl)
ERL_OBJECTS := $(patsubst %.erl,%.beam,$(NETSIM_ERL_FILES))
ERL_OBJECTS += $(patsubst %.app.src,%.app,$(wildcard *.app.src))


all: $(FXS) $(ERL_OBJECTS)

%.fxs:  ../src/yang/%.yang
	$(CONFDC) $(FLAGS) -c -o $@ $<

%.fxs:  %.yang
	$(CONFDC) $(FLAGS) -c -o $@ $<

%.hrl: %.fxs
	$(CONFDC) --emit-hrl $@ $<

$(PACKAGE_MK_TMP_LOCAL)/onc%.Pbeam:

$(PACKAGE_MK_TMP_LOCAL):
	mkdir -p $@

%.app: %.app.src
	perl -e $(APPSCRIPT) 1.0 $(patsubst %.erl,%,$(wildcard *.erl)) < $< > $@

%.beam: %.erl $(HRL) | $(PACKAGE_MK_TMP_LOCAL)
	$(ERLC) $(ERLC_FLAGS) -I ./ -M -MP -MG -MQ $(patsubst %.erl,%.beam,$<) -MF $(patsubst %.erl,$(PACKAGE_MK_TMP_LOCAL)/onc%.Pbeam,$<) $<
	$(ERLC) $(ERLC_FLAGS) -I ./ -o ./ $<

include $(wildcard $(NETSIM_ERL_FILES:%.erl=$(PACKAGE_MK_TMP_LOCAL)/onc%.Pbeam))

# Invoked by netsim, who will sed substitute the %var% PORT variables
# in the confd.conf.netsim file
# The install target here will be invoked multiple times by ncs-netsim,
# once for each device in the simulation network
# The following env variable will be set when ncs-netsim invokes this
# install target
# - DEST_DIR this is where all the files shall go, it's the directory
#   that will be used as execution environment for this ConfD instance
# - NAME this is the name of the managed device
# - COUNTER this is the number of the managed device

install:
	@if [ -z "$${DEST_DIR}" ]; then echo "No DEST_DIR  var"; exit 1; fi
	@if [ ! -d "$${DEST_DIR}" ]; then "echo No DEST_DIR"; exit 1; fi
	mkdir $${DEST_DIR}/cdb 2>/dev/null || true
	mkdir $${DEST_DIR}/logs 2>/dev/null || true
	for i in $(XML); do \
		sed -e 's/%NAME%/$(NAME)/g' -e 's/%COUNTER%/$(COUNTER)/g' -e 's/%TWO_DIGIT_COUNTER%/$(TWO_DIGIT_COUNTER)/g' \
		  $$i > $${DEST_DIR}/cdb/`basename $$i`; \
	done
	for f in $(FXS); do cp $$f $${DEST_DIR}; done
	cp -f $(EXTRA_FILES) $${DEST_DIR}
	cp -af $(NCS_DIR)/netsim/confd/etc/confd/ssh $${DEST_DIR}
ifneq ($(wildcard *.app),)
	@cp -f *.app $${DEST_DIR}
endif
ifneq ($(wildcard *.beam),)
	@cp -f *.beam $${DEST_DIR}
endif

clean:
	@rm -f $(FXS)
	@rm -f $(HRL)
	@rm -f $(ERL_OBJECTS)
	@rm -rf $(PACKAGE_MK_TMP_LOCAL)

.PHONY: clean