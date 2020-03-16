# -*- mode: makefile; -*-

TARGET = target
VALGRIND_DIR = build/codetest
DOXYGEN_DIR = doc
COVERAGE_DIR = build
BUILD_DIR = build
INSTALL_DIR = $(HOME)/local
ARG =  -Db_coverage=true $(ASAN) -Dprefix=$(INSTALL_DIR)
LINTER = cclint
OPEN = $(shell which xdg-open || which gnome-open || which open)
PYTHON = /usr/bin/env python3
PIP = /usr/bin/env pip3
R = /usr/bin/env R --no-save --no-restore
GIT = /usr/bin/env git
CMAKE = /usr/bin/env cmake
GETVER = config/getrversion.py
NINJA = /usr/bin/env ninja
NINJA_BUILD_OPT = -v
PKGLIB = 0
BUILD = -DUSE_PKG_LIB=$(PKGLIB) -DNO_COTIRE=1 -DCMAKE_BUILD_TYPE=Debug \
  -DCMAKE_FIND_PACKAGE_NO_PACKAGE_REGISTRY=ON
ifneq ($(NINJA),)
  BUILD := $(BUILD) -GNinja
endif
# R package:
pkg = gof
PKG = $(shell $(GETVER) R-package/$(pkg))
R_DEP = 1
TESTR = $(pkg)_test
# python package
TESTPY = target_test

##################################################

default: checkinit build runr

all: clean run

##################################################

.PHONY: clean
clean: cleanr cleanpy
	@rm -Rf $(BUILD_DIR) $(VALGRIND_DIR) $(DOXYGEN_DIR)/html $(COVERAGE_DIR)

.PHONY: init init-submodules checkinit
init: init-submodules clean
	@echo "Build options: $(BUILD)"
	@$(CMAKE) -B build $(BUILD)

checkinit:
	@if [ ! -f "$(BUILD_DIR)/build.ninja" ]; then $(MAKE) init; fi

init-submodules:
	@if [ -z "`find \"lib/armadillo\" -mindepth 1 -exec echo notempty \; -quit`" ]; then \
	git submodule init && git submodule update; fi

.PHONY: run
run: 
	@if [ ! -d "$(BUILD_DIR)" ]; then $(MAKE) --no-print-directory init; fi
	@$(MAKE) --no-print-directory build # > /dev/null
	@printf "\n-----\n"
	@find build/ -maxdepth 1 -iname "*demo" -executable -type f \
	-exec {} \; 


.PHONY: build
build:
	@if [ -f $(BUILD_DIR)/build.ninja ]; then \
	$(NINJA) -C $(BUILD_DIR) $(NINJA_BUILD_OPT); \
	else \
	cd $(BUILD_DIR); make; \
	fi

.PHONY: install
install:
	$(NINJA) -C $(BUILD_DIR) install

.PHONY: uninstall
uninstall:
	$(NINJA) -C $(BUILD_DIR) uninstall

##################################################
## R package
##################################################

.PHONY: r cleanr buildr runr testr roxygen
buildr: cleanr
	@$(R) --slave -e "source('config/utilities.R'); \
	load_packages(c('Rcpp', 'RcppArmadillo', 'lava', 'optimx', 'futile.logger'))"
	@$(R) --slave -e "Rcpp::compileAttributes('R-package/${pkg}')"
	@$(R) CMD INSTALL R-package/${pkg}

testr:
	@$(R) -e 'testthat::test_package("./R-package/${pkg}/")'

runr:
	@cd misc; $(R) --silent -f $(TESTR).R

roxygen:
	@$(R) -e 'roxygen2::roxygenize("R-package/${pkg}")'

exportr:
	@rm -Rf $(BUILD_DIR)/R/$(pkg)
	@mkdir -p $(BUILD_DIR)/R/$(pkg)
	cd R-package/${pkg}; $(GIT) archive HEAD | (cd ../../$(BUILD_DIR)/R/$(pkg); tar x)
	cp src/*.cpp $(BUILD_DIR)/R/$(pkg)/src
	cp src/*.hpp $(BUILD_DIR)/R/$(pkg)/inst/include
	sed -i '/^OBJECTS\|SOURCES/d' $(BUILD_DIR)/R/$(pkg)/src/Makevars
	cd $(BUILD_DIR)/R; $(R) CMD build $(pkg) --compact-vignettes=gs+qpdf --resave-data=best

checkr: exportr
	cd $(BUILD_DIR)/R; $(R) CMD check `../../$(GETVER) $(pkg)` --timings --as-cran --no-multiarch --run-donttest

r: buildr runr

cleanr:
	@rm -Rf R-package/${pkg}/src/*.o R-package/${pkg}/src/*.so

.PHONY: syncr
syncr: exportr
	@if [ -d "../$(pkg)" ]; then \
	cp -rfv $(BUILD_DIR)/R/$(pkg)/.	 ../$(pkg)/; fi

##################################################
## Python package
##################################################

.PHONY: py cleanpy buildpy runpy testpy

buildpy:
	@cd python-package; $(PYTHON) setup.py install

testpy:
	@cd python-package; $(MAKE) test

runpy:
	@$(PYTHON) misc/$(TESTPY).py

py: buildpy runpy

cleanpy:
	@cd python-package; $(MAKE) --no-print-directory clean

##################################################
## Documentation
##################################################

.PHONY: docs doc
docs:
	@cd $(DOXYGEN_DIR); doxygen

doc:	docs
	@$(OPEN) $(DOXYGEN_DIR)/html/index.html

markdown:
	@grip 1313 -b

##################################################
## Unit tests
##################################################

.PHONY: t test testall
t:	run
	@ninja -C $(BUILD_DIR) test

test:	build
	build/$(TARGET)_test -s

testall: test r py testr testpy

##################################################
## Code coverage
##################################################
.PHONY: coverage
coverage:
	rm -Rf build/coverage
	mkdir -p build/coverage
	cd build/coverage; cmake -DCMAKE_BUILD_TYPE=Debug -DCOVERAGE_BUILD=1 ../../ && make coverage
	$(OPEN) build/coverage/coverage/index.html

##################################################
## Debugging, profiling, and memory inspection
##################################################

.PHONY: check
check:
	-cclint src/*.cpp src/*.h
	-cppcheck --enable=all src/

.PHONY: valgrind
## Alternatively, enable Address Sanitizer (ASAN =-Db_sanitize=address)
valgrind:
	@ninja -C build test_memcheck

##################################################
## Docker
##################################################

.PHONY: dockerbuild docker export
dockerbuild:
	@docker build . -t $(TARGET)_test

export:
	@rm -Rf ${PWD}/tmp/$(TARGET)
	@mkdir -p ${PWD}/tmp/$(TARGET)
	@git archive HEAD | (cd ${PWD}/tmp/$(TARGET); tar x)
	@git submodule foreach 'curdir=${PWD} cd ${PWD}/$$path; git archive HEAD | tar -x -C ${PWD}/tmp/$(TARGET)/$$path'
	@echo "Exported to '${PWD}/tmp/$(TARGET)'"
	@chmod -R 777 ${PWD}/tmp/$(TARGET)

dockerrun:
	docker run --user `id -u` -ti --rm --privileged -v ${PWD}/tmp/$(TARGET):/data $(TARGET)_test ${CMD}

docker: export dockerrun

