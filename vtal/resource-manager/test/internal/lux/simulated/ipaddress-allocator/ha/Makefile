DIR           := ../../../../../..
LUX_TEST_DIRS := $(shell pwd)

TEST_NCS_DIR := $(DIR)/test/ncs

include $(DIR)/Makefile

all: test

.PHONY: clean hasetup stop_nodes

# init target to be called from within the lux test file.
#
init: hasetup

clean:: stop_nodes init-cleaning
	rm -fr ./lux_logs
	rm -f ./ncs.conf


# Borrowed from ncs HA basic test.
# But skip using dedicated ncs directories for the single HA node used.
# Instead using the normal /ncs directory under /test.
# With ncs.conf.tmpl the ncs.conf is to generated and copied to /ncs directory.

export NNODES = 1
HAPORT       = 4564
NETCONF_BASE = 2023
IPC_BASE     = 4569
CLI_BASE     = 2122
OPER_ALWAYS  = <operational><enabled>true</enabled><replication>always</replication></operational>
OPER_SYNC    = <operational><enabled>true</enabled><replicationMode>sync</replicationMode></operational>
OPERATIONAL  = <operational><enabled>true</enabled></operational>
EXTENA       = false
REPLICATION  = sync		# async
HATIMEOUT    = PT20S
STARTUP      = false
CANDIDATE    = true
REPLAY	     = false
AES256	     =  		#no AES256 key for older versions than 5.3
RESTENABLED  = <rest><enabled>true</enabled></rest>	
CLI_SHOW_LOGS= <show-log-directory>./logs</show-log-directory>
SYSLOG_UDP   = <udp><enabled>false</enabled><host>syslogsrv.example.com</host></udp>
LARGE_SCALE  = <large-scale><lsa><!-- Enable Layered Service Architecture, LSA. This requires a separate Cisco Smart License. --><enabled>false</enabled></lsa></large-scale>

# returns "true" if $(NCS_VERSION) >= (argument) else "false";
# used like: NCS_VER_GTE_5_3 := $(call ver_gte,5.3) (returns true if NCS_VERSION >= 5.3)
# accepts 3rd digit, ex $(call ver_gte,5.3.2)
define ver_gte
$(shell echo $(1) $(NCS_VERSION) | awk ' { print ($$1 <= $$2) ? "true" : "false" } ')
endef


ifeq ($(call ver_gte,5.3),true)
	AES256 = <AES256CFB128><key>0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef</key></AES256CFB128>
endif

ifeq ($(call ver_gte,5.4),true)
	RESTENABLED = 
endif

ifeq ($(call ver_gte,5.5),true)
	CLI_SHOW_LOGS = 
endif

ifeq ($(call ver_gte,5.6),true)
	SYSLOG_UDP = 
	LARGE_SCALE = 
endif

hasetup: init-cleaning
	@i=0; while [ $$i -lt $(NNODES) ]; do				\
		echo "setting up $(TEST_NCS_DIR)" ;			\
		mkdir -p $(TEST_NCS_DIR)/ssh; \
		chmod 400 ssh_host_dsa_key ;				\
		cp ssh_host_dsa_key $(TEST_NCS_DIR)/ssh/. ;		\
		cp ssh_host_dsa_key.pub $(TEST_NCS_DIR)/ssh/authorized_keys ;	\
		eval HAPORT=`echo '$$'HAPORT_$$i`;			\
		test "x$$HAPORT" = x && HAPORT=$(HAPORT);		\
		sed -e "s|%%HAPORT%%|$$HAPORT|"				\
			-e "s|%%NETCONFPORT%%|$$(($(NETCONF_BASE)+$$i))|"	\
			-e "s|%%IPCPORT%%|$$(($(IPC_BASE)+$$i))|"		\
			-e "s|%%CLIPORT%%|$$(($(CLI_BASE)+$$i))|"		\
			-e "s|%%REPLICATION%%|$(REPLICATION)|"		\
			-e "s|<!--%%OP%%-->|$(OPERATIONAL)|"		\
			-e "s|%%EXTENA%%|$(EXTENA)|"			\
			-e "s|%%EXTADDR%%|`pwd`/node$$i/socket|"		\
			-e "s|%%HAEXTADDR%%|`pwd`/node$$i/hasock|"		\
			-e "s|%%HATIMEOUT%%|$(HATIMEOUT)|"			\
			-e "s|%%STARTUP%%|$(STARTUP)|"			\
			-e "s|%%CANDIDATE%%|$(CANDIDATE)|"			\
			-e "s|%%REPLAY%%|$(REPLAY)|"			\
			-e "s|%NAME%|node$$i|g"				\
			-e "s|%%AES256%%|$(AES256)|g"			\
			-e "s|%%RESTENABLED%%|$(RESTENABLED)|g"		\
			-e "s|%%CLI_SHOW_LOGS%%|$(CLI_SHOW_LOGS)|g"		\
			-e "s|%%SYSLOG_UDP%%|$(SYSLOG_UDP)|g"		\
			-e "s|%%LARGE_SCALE%%|$(LARGE_SCALE)|g"		\
			< ncs.conf.tmpl > ./ncs.conf ;					\
		i=$$(($$i + 1)) ;					\
	done

init-cleaning:
	rm -f $(TEST_NCS_DIR)/ssh/ssh_host_dsa_key 
	rm -f $(TEST_NCS_DIR)/ssh/authorized_keys/ssh_host_dsa_key.pub

stop_nodes:
	i=0; \
	while [ $$i -lt $(NNODES) ]; do \
	    env NCS_IPC_PORT=$$(($(IPC_BASE)+$$i)) ncs --stop > /dev/null;\
	    i=$$(($$i + 1)) ; \
	done
	if [ -d netsim ]; then ncs-netsim stop ; fi || true

stop_node%:
	env NCS_IPC_PORT=$$(($(IPC_BASE)+$*)) ncs --stop

