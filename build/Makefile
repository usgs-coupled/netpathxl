# $Id: Makefile 2311 2007-10-09 22:48:59Z charlton $

#
# To run step by step
# 1. make source
# 2. make script
# 3. make build_all
# and step 3 may be broken using the following
# 3a. make prep
# 3b. make conf
# 3c. make build
# 3d. make install
# 3e. make pkg
# 3f. make spkg
# 3g. make finish
#

NAME := NetpathXL

SVN_URL := http://internalbrr.cr.usgs.gov/svn_GW

RELEASE ?= $(shell svn info $(SVN_URL) | egrep "Revision:" | cut -b 10- | ( read one two ; echo $$one ) )

ifneq ($(strip $(DATE)),)
DATE_STAMP  := $(shell date -d $(DATE) "+%m-%d-%Y")
TOUCH_STAMP :=  $(shell date -d $(DATE) "+%Y%m%d0000")
endif

TARBALL := $(NAME)-$(VERSION)-$(RELEASE).tar.gz

BUILD_SCRIPT := $(NAME)-$(VERSION)-$(RELEASE).sh

DEVENV := /cygdrive/c/Program\ Files/Microsoft\ Visual\ Studio\ 8/Common7/IDE/devenv

SLN := htJni/htJni.sln

REPOS_PATH ?= trunk

# (ie NetpathXL-1.1-2702-src.tar.bz2)
SRC_PACKAGE := $(NAME)-$(VERSION)-$(RELEASE)-src.tar.bz2

# (ie NetpathXL-1.1-2702.tar.bz2)
BIN_PACKAGE := $(NAME)-$(VERSION)-$(RELEASE).tar.bz2

# (ie NetpathXL-1.1-2702.msi)
SETUP_PACKAGE := $(NAME)-$(VERSION)-$(RELEASE).msi

all : check_args $(BIN_PACKAGE)
	echo "VERSION="$(VERSION)                  >> HISTORY
	echo "DATE="$(DATE)                        >> HISTORY
	echo "RELEASE="$(RELEASE)                  >> HISTORY
	echo "===================================" >> HISTORY

build_all : check_args $(BIN_PACKAGE)

prep : check_args
	time ./$(BUILD_SCRIPT) prep 2>&1 | tee prep-$(RELEASE).log

conf : check_args
	time ./$(BUILD_SCRIPT) conf 2>&1 | tee conf-$(RELEASE).log

build : check_args
	time ./$(BUILD_SCRIPT) build 2>&1 | tee build-$(RELEASE).log

install : check_args
	time ./$(BUILD_SCRIPT) install 2>&1 | tee install-$(RELEASE).log

pkg : check_args
	time ./$(BUILD_SCRIPT) pkg 2>&1 | tee pkg-$(RELEASE).log

spkg : check_args
	time ./$(BUILD_SCRIPT) spkg 2>&1 | tee spkg-$(RELEASE).log

finish : check_args
	time ./$(BUILD_SCRIPT) finish 2>&1 | tee finish-$(RELEASE).log

$(BIN_PACKAGE) : $(TARBALL) $(BUILD_SCRIPT)
	@echo "Building package $(BIN_PACKAGE) / $(SRC_PACKAGE)"	
	time ./$(BUILD_SCRIPT) all 2>&1 | tee all-$(RELEASE).log
	
source : check_args $(TARBALL)
	@echo "created $(TARBALL)"

$(TARBALL) : dist.sh $(DATE_STAMP).tstamp
	@echo "Creating $(TARBALL)"
	time ./dist.sh -v $(VERSION) -r $(RELEASE) -d $(DATE) -pr $(REPOS_PATH) 2>&1 | tee dist-$(RELEASE).log
	
script : check_version $(BUILD_SCRIPT)	
	@echo "created $(BUILD_SCRIPT)"
	
$(BUILD_SCRIPT) : build.sh
	ln -f build.sh $(BUILD_SCRIPT)
	
$(DATE_STAMP).tstamp : 
	rm -rf *.tstamp
	touch $(DATE_STAMP).tstamp
	
check_args : check_version check_date
	
check_version :
ifeq ($(strip $(VERSION)),)
	@echo "No version specified"
	@make -s usage
	@exit 1
endif	

check_date :
ifeq ($(strip $(DATE)),)
	@echo "No date specified"
	make usage
	exit 1
endif

usage:
	@echo ""
	@echo "usage: make VERSION=<number> DATE=<xx/xx/xxxx> [RELEASE=<number>] [SIG=<0|1>]"
	@echo "    ie make VERSION=0.5 DATE=10/04/2007"
	@echo "       make VERSION=0.5 DATE=10/04/2007 RELEASE=2297 SIG=1"
	@echo ""

FTP_DIR := ~/ftp

TOUCH_COMMAND := ssh srv2rcolkr "find $(FTP_DIR)/netpathxl -name \"*$(RELEASE)*\" | xargs touch -t $(TOUCH_STAMP)"
CHMOD_COMMAND := ssh srv2rcolkr "find $(FTP_DIR)/netpathxl -name \"*$(RELEASE)*\" | xargs chmod 644"

ftp : check_args ftp_netpathxl
	$(TOUCH_COMMAND)
	$(CHMOD_COMMAND)

chmod : check_args
	$(CHMOD_COMMAND)

touch : check_args
	$(TOUCH_COMMAND)

ftp_netpathxl :
	tar xvjf $(BIN_PACKAGE) $(SETUP_PACKAGE) README.TXT
	mv README.TXT README.$(RELEASE).TXT
	scp $(SETUP_PACKAGE) README.$(RELEASE).TXT srv2rcolkr:$(FTP_DIR)/netpathxl/.
	rm -rf $(SETUP_PACKAGE) README.$(RELEASE).TXT
