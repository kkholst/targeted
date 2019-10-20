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
PYTHON = /usr/bin/env python
R = /usr/bin/env R
CMAKE = /usr/bin/env cmake

default: run

all: clean run

##################################################

.PHONY: clean
clean: cleanr cleanpy
	@rm -Rf $(BUILD_DIR) $(VALGRIND_DIR) $(DOXYGEN_DIR)/html $(COVERAGE_DIR)	

.PHONY: init-default
init-default: clean
	@$(CMAKE) -G Ninja -B build

.PHONY: run
run:
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
## Python and R packages
##################################################

.PHONY: r cleanr
r: cleanr
	$(R) CMD INSTALL R-package
	cd examples; $(R) --no-save -f test.R

cleanr:
	@rm -Rf R-package/src/*.o R-package/src/*.so

.PHONY: py cleanpy
py:
	cd python-package; $(PYTHON) setup.py install
	$(PYTHON) examples/test.py

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
test:	build
	@ninja -C $(BUILD_DIR) test

testall: test r py
	cd R-package; $(R) -e 'devtools::test()'
	cd python-package; $(MAKE) test

.PHONY: cov-default
cov-default:
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

.PHONY: valgrind-default
## Alternatively, enable Address Sanitizer (ASAN argument)
valgrind-default:
	@ninja -C build test_memcheck
