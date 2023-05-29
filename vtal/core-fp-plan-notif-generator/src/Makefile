all: fxs build-meta-data
.PHONY: all

# Include standard NCS examples build definitions and rules
include $(NCS_DIR)/src/ncs/build/include.ncs.mk

# Below data is used to generate build-meta-data.xml
PACKAGE_NAME    := $(shell xmllint --xpath "string(//*[local-name()='ncs-package']/*[local-name()='name'])" ../package-meta-data.xml)
$(info VAR="$(PACKAGE_NAME)")
PACKAGE_VERSION := $(shell xmllint --xpath "string(//*[local-name()='ncs-package']/*[local-name()='package-version'])" ../package-meta-data.xml)
$(info VAR="$(PACKAGE_VERSION)")
TAR_BUILD_NAME ?= "$(PACKAGE_NAME)"
NCS_VERSION := $(shell ncs --version)
GIT_HASH        := $(shell  git rev-parse HEAD 2>/dev/null || echo unknown)
GIT_HASH_SHORT  := $(shell  git log --pretty=format:'%h' -n 1 2>/dev/null || echo unknown)
JAVA_VERSION ?= $(shell java -version 2>&1 | head -n 1)
PYTHON_VERSION ?= $(shell python --version)


SRC = $(wildcard yang/*.yang)
DIRS = ../load-dir
FXS = $(SRC:yang/%.yang=../load-dir/%.fxs)

## Uncomment and patch the line below if you have a dependency to a NED
## or to other YANG files
# YANGPATH += ../../<ned-name>/src/ncsc-out/modules/yang \
# 	../../<pkt-name>/src/yang

NCSCPATH   = $(YANGPATH:%=--yangpath %)
YANGERPATH = $(YANGPATH:%=--path %)

fxs: $(DIRS) $(FXS)

$(DIRS):
	mkdir -p $@

../load-dir/%.fxs: yang/%.yang
	$(NCSC)  `ls $*-ann.yang  > /dev/null 2>&1 && echo "-a $*-ann.yang"` \
             $(NCSCPATH) -c -o $@ $<

clean:
	rm -rf $(DIRS)

build-meta-data:
	@echo "<ncs-pkg-build xmlns='http://tail-f.com/ns/ncs-pkg-build'> \
			   <date>$(shell date +'%Y-%m-%d %H:%M:%S')</date> \
			   <file>$(TAR_BUILD_NAME).tar.gz</file> \
			   <python>$(PYTHON_VERSION)</python> \
			   <package> \
			   <name>$(PACKAGE_NAME)</name> \
			   <version>$(PACKAGE_VERSION)</version> \
			   <sha1>$(GIT_HASH_SHORT)</sha1> \
			   </package> \
			   <ncs> \
			   <version>$(NCS_VERSION)</version> \
			   </ncs> \
			   </ncs-pkg-build>" > ../build-meta-data.xml

.PHONY: clean


