all: javac

.PHONY: all

# Include standard NCS examples build definitions and rules
include $(NCS_DIR)/src/ncs/build/include.ncs.mk
# Below data is used to generate build-meta-data.xml
PACKAGE_NAME    := $(shell xmllint --xpath "string(//*[local-name()='ncs-package']/*[local-name()='name'])" ../package-meta-data.xml)
$(info VAR="$(PACKAGE_NAME)")
PACKAGE_VERSION := $(shell xmllint --xpath "string(//*[local-name()='ncs-package']/*[local-name()='package-version'])" ../package-meta-data.xml)
$(info VAR="$(PACKAGE_VERSION)")
TAR_BUILD_NAME ?= core-fp-common
NCS_VERSION := $(shell ncs --version)
GIT_HASH        := $(shell  git rev-parse HEAD 2>/dev/null || echo unknown)
GIT_HASH_SHORT  := $(shell  git log --pretty=format:'%h' -n 1 2>/dev/null || echo unknown)
JAVA_VERSION ?= $(shell java -version 2>&1 | head -n 1)
PYTHON_VERSION ?= $(shell python --version)
JAVA_PACKAGE = com.cisco.corefpcommon
JDIR = $(shell echo $(JAVA_PACKAGE) | sed 's/\./\//g')

NS  = namespaces

JFLAGS = --java-disable-prefix \
         --exclude-enums \
         --fail-on-warnings \
         --java-package $(JAVA_PACKAGE).$(NS) \
         --emit-java java/src/$(JDIR)/namespaces

SRC = $(wildcard yang/*.yang)
DIRS = ../load-dir ../shared-jar ../private-jar ncsc-out java/src/$(JDIR)/$(NS)
FXS = $(SRC:yang/%.yang=../load-dir/%.fxs)

## Uncomment and patch the line below if you have a dependency to a NED
## or to other YANG files
# YANGPATH += ../../<ned-name>/src/ncsc-out/modules  ../../<pkt-name>/src/yang

NCSCPATH   = $(YANGPATH:%=--yangpath %)
YANGERPATH = $(YANGPATH:%=--path %)

build-meta-data:
	@echo "<ncs-pkg-build xmlns='http://tail-f.com/ns/ncs-pkg-build'> \
			   <date>$(shell date +'%Y-%m-%d %H:%M:%S')</date> \
			   <file>$(TAR_BUILD_NAME).tar.gz</file> \
			   <java>$(JAVA_VERSION)</java> \
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

javac: $(DIRS) fxs build-meta-data
	cd java && ant -q all

.PHONY: javac

fxs: $(FXS)
		for f in `echo ../load-dir/*.fxs`; do \
	    $(NCSC) --fail-on-warnings                                 \
	        --emit-python ../python/core_fp_common $$f  || \
	    exit 1;                                                      \
	   true; \
		done
.PHONY: fxs

$(DIRS):
	mkdir -p $@

../load-dir/%.fxs: yang/%.yang
	$(NCSC)  `ls $*-ann.yang  > /dev/null 2>&1 && echo "-a $*-ann.yang"` \
	     $(NCSCPATH) -c -o $@ $<
	$(NCSC) $(JFLAGS)/$*.java $@

clean:
	rm -rf $(DIRS)
	cd java && ant -q clean || true
.PHONY: clean