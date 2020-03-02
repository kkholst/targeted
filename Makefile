# -*- mode: makefile; -*-

VALGRIND_DIR := build/codetest
DOXYGEN_DIR := doc
COVERAGE_DIR := build
BUILD_DIR := build
INSTALL_DIR := $(HOME)/local
ARG =  -Db_coverage=true $(ASAN) -Dprefix=$(INSTALL_DIR)
LINTER = cclint
MESON = meson $(ARG)
# ASAN =-Db_sanitize=address
# CXX = clang++
# CC = g++
OPEN = $(shell which xdg-open || which gnome-open || which open)
PYTHON = /usr/bin/env python3
PIP = /usr/bin/env pip3
R = /usr/bin/env R --no-save --no-restore
CMAKE = /usr/bin/env cmake
R_DEP = 1
TEST = test
BUILD=debug
#BUILD=release

default: run

all: clean run

##################################################

.PHONY: clean
clean: cleanr cleanpy
	@rm -Rf $(BUILD_DIR) $(VALGRIND_DIR) $(DOXYGEN_DIR)/html $(COVERAGE_DIR)	

.PHONY: init init-meson init-submodules
init: clean
	@$(CMAKE) -G Ninja -B build

init-meson:
	@$(MESON) $(BUILD_DIR) --buildtype=$(BUILD)

init-submodules:
	@if [ -z "`find \"lib/armadillo\" -mindepth 1 -exec echo notempty \; -quit`" ]; then \
	git submodule init && git submodule update; fi

.PHONY: run
run: init-submodules
	@if [ ! -d "$(BUILD_DIR)" ]; then $(MAKE) --no-print-directory init; fi
	@$(MAKE) --no-print-directory build # > /dev/null
	@printf "\n"
	@build/dredemo
	@printf "\n\n"

.PHONY: build
build:
	@ninja -C $(BUILD_DIR)

.PHONY: install
install:
	ninja -C $(BUILD_DIR) install

.PHONY: uninstall
uninstall:
	ninja -C $(BUILD_DIR) uninstall

##################################################
## R package
##################################################

.PHONY: r cleanr buildr runr testr roxygen

buildr: cleanr
	@$(R) --slave -e "source('examples/utilities.R'); \
	load_packages(c('Rcpp', 'RcppArmadillo', 'lava', 'optimx', 'futile.logger'))"
	@$(R) --slave -e "Rcpp::compileAttributes('R-package')"
	@$(R) CMD INSTALL R-package

testr:
	@$(R) -e 'testthat::test_package("R-package")'

runr:
	@cd examples; $(R) --silent -f $(TEST).R

roxygen:
	@$(R) -e 'roxygen2::roxygenize("R-package")'

r: buildr runr

cleanr:
	@rm -Rf R-package/src/*.o R-package/src/*.so

##################################################
## Python package
##################################################

.PHONY: py cleanpy buildpy runpy testpy

buildpy:
	@cd python-package; $(PYTHON) setup.py install

testpy:
	@cd python-package; $(MAKE) test

runpy:
	@$(PYTHON) examples/$(TEST).py

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

##################################################
## Unit tests & code coverage
##################################################

.PHONY: test testall
test:	run
	@ninja -C $(BUILD_DIR) test

testall: test r py testr testpy

.PHONY: cov
cov:
	@$(MESON) $(COVERAGE_DIR) -Db_coverage=true
	@ninja -C $(COVERAGE_DIR)
	@ninja -C $(COVERAGE_DIR) test
	@ninja -C $(COVERAGE_DIR) coverage-html	
	@$(OPEN) $(COVERAGE_DIR)/meson-logs/coveragereport/index.html

##################################################
## Debugging, profiling, and memory inspection
##################################################

.PHONY: check
check:
	-cclint src/*.cpp src/*.h
	-cppcheck --enable=all src/

.PHONY: valgrind
## Alternatively, enable Address Sanitizer (ASAN argument)
valgrind-default:
	@ninja -C build test_memcheck

.PHONY: valgrind-meson
valgrind-meson:
	@meson $(VALGRIND_DIR)
	@cd $(VALGRIND_DIR); ninja test & meson test --wrap='valgrind  --tool=memcheck --leak-check=yes --show-reachable=yes --num-callers=20 --track-fds=yes '
	@less $(VALGRIND_DIR)/meson-logs/testlog-valgrind.txt
	@ninja -C build test_memcheck



##################################################
## Docker
##################################################

.PHONY: dockerbuild docker export
dockerbuild:
	@docker build . -t target_test

export:
	@rm -Rf ${PWD}/tmp/target
	@mkdir -p ${PWD}/tmp/target
	@git archive HEAD | (cd ${PWD}/tmp/target; tar x)
	@git submodule foreach 'curdir=${PWD} cd ${PWD}/$$path; git archive HEAD | tar -x -C ${PWD}/tmp/target/$$path'
	@echo "Exported to '${PWD}/tmp/target'"

dockerrun: 
	docker run -ti --rm --privileged -v ${PWD}/tmp/target:/data target_test ${CMD}

docker: dockerbuild export dockerrun

