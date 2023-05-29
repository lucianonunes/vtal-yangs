
JAVA_PACKAGE = com.crossworks.dlm
JDIR = $(shell echo $(JAVA_PACKAGE) | sed 's/\./\//g')
GENSOURCES_DIR = 'java/build/generated-sources/main'
TEST_GENSOURCES_DIR = 'java/build/generated-sources/test'
NS  = namespaces

JFLAGS = --java-disable-prefix \
         --exclude-enums \
         --fail-on-warnings \
         --java-package $(JAVA_PACKAGE).$(NS) \
         --emit-java $(GENSOURCES_DIR)/$(JDIR)/namespaces

TEST_JFLAGS = --java-disable-prefix \
         --exclude-enums \
         --fail-on-warnings \
         --java-package $(JAVA_PACKAGE).$(NS) \
         --emit-java $(TEST_GENSOURCES_DIR)/$(JDIR)/namespaces

NOSA ?= true 

all:	fxs javac

# Include standard NCS examples build definitions and rules
include $(NCS_DIR)/src/ncs/build/include.ncs.mk

SRC = $(wildcard yang/*.yang )
FXS = $(SRC:yang/%.yang=../load-dir/%.fxs)
YANGPATH = --yangpath yang

fxs:	$(FXS)

../load-dir/%.fxs:	yang/%.yang
	mkdir -p $(GENSOURCES_DIR)/$(JDIR)/namespaces
	mkdir -p ../load-dir
	$(NCSC)  `ls $*-ann.yang  > /dev/null 2>&1 && echo "-a $*-ann.yang"` \
             $(YANGPATH) -c -o $@ $<
	$(NCSC) $(JFLAGS)/$*.java $@

javac:
	cd java && ant -q all -Dnosa=$(NOSA)

clean:
	rm -rf  ../load-dir/*
	rm -f ../*jar/*.jar
	cd java && ant -q clean || true
	rm -f $(GENSOURCES_DIR)/$(JDIR)/$(NS)/*.java

# make all should be called before make testc
testc: test-fxs test-javac

TEST-YANG = $(wildcard java/test/resources/yang/*.yang )
TEST-FXS = $(TEST-YANG:java/test/resources/yang/%.yang=../load-dir/%.fxs)

test-fxs:	 $(TEST-FXS)

../load-dir/%.fxs:	java/test/resources/yang/%.yang
	mkdir -p $(TEST_GENSOURCES_DIR)/$(JDIR)/namespaces
	mkdir -p ../load-dir
	$(NCSC)  `ls $*-ann.yang  > /dev/null 2>&1 && echo "-a $*-ann.yang"` \
             $(YANGPATH) -c -o $@ $<
	$(NCSC) $(TEST_JFLAGS)/$*.java $@


test-javac:
	cd java && ant -q test-compile -Dnosa=$(NOSA)
	cd java && ant -q functional-test-compile -Dnosa=$(NOSA)

# compiles and runs tests without coverage
tests: testc
	cd java && ant -q tests -Dnosa=$(NOSA)

# compiles, runs all unit tests and generates coverage report.
coverage: testc
	cd java && ant -q test-report -Dnosa=$(NOSA)

