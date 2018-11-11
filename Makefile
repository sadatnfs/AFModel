R=R

PACKAGE=AFModel
VERSION := $(shell sed -n '/^Version: /s///p' glmmTMB/DESCRIPTION)

TARBALL := $(PACKAGE)_$(VERSION).tar.gz
ZIPFILE := =$(PACKAGE)_$(VERSION).zip

CPP_SRC := $(PACKAGE)/src/*.cpp


all:
	make build-package
	make install


build-package: $(TARBALL)
$(TARBALL): $(PACKAGE)/NAMESPACE $(CPP_SRC)
	$(R) CMD build --resave-data=no $(PACKAGE)

install: $(TARBALL)
	$(R) CMD INSTALL --preclean $<
	@touch $@


$(PACKAGE)/src/AFModel.so: $(PACKAGE)/src/AFModel.cpp
	cd $(PACKAGE)/src; echo "library(TMB); compile('AFModel.cpp', openmp=T)" | $(R) --slave
