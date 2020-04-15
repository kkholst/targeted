# -*- mode: makefile; -*-

include $(wildcard config/*.mk)

TARGET = target
BUILD_DIR = build
VALGRIND_DIR = build/codetest
DOXYGEN_DIR = doc
COVERAGE_DIR = build
INSTALL_DIR = $(HOME)/local
ARG =  -Db_coverage=true $(ASAN) -Dprefix=$(INSTALL_DIR)
PKGLIB = OFF
IMG=# Dockerfile postfix
BUILD = -DUSE_PKG_LIB=$(PKGLIB) -DCOTIRE=OFF -DCMAKE_INSTALL_PREFIX=$(INSTALL_DIR) -DCMAKE_BUILD_TYPE=Debug \
  -DCMAKE_FIND_PACKAGE_NO_PACKAGE_REGISTRY=ON -Wno-dev
ifneq ($(NINJA),)
  BUILD := $(BUILD) -GNinja
endif

# R package:
pkg = targeted
TESTR = $(pkg)_test

# python package
TESTPY = targeted_test

##################################################

default: build runr

all: clean run

##################################################

.PHONY: clean
clean: cleanpy
	@rm -Rf $(BUILD_DIR) $(VALGRIND_DIR) $(DOXYGEN_DIR)/html $(COVERAGE_DIR)
	@$(MAKE) pkg=gof cleanr
	@$(MAKE) pkg=targeted cleanr
	@rm -Rf src/*.o src/*.so

.PHONY: init
init: clean
	@echo "Build options: $(BUILD)"
	@mkdir -p $(BUILD_DIR)
	@cd build; $(CMAKE) .. $(BUILD)

.PHONY: checkinit
checkinit: init-submodules
	@if [ ! -f "$(BUILD_DIR)/CMakeCache.txt" ]; then $(MAKE) init; fi

.PHONY: init-submodules
init-submodules:
	@if [ ! -f "lib/armadillo/.git" ]; then \
	$(GIT) submodule update --init --recursive; fi

.PHONY: run
run:
	@if [ ! -d "$(BUILD_DIR)" ]; then $(MAKE) --no-print-directory init; fi
	@$(MAKE) --no-print-directory build # > /dev/null
	@printf "\n-----\n"
	@find build/ -maxdepth 1 -iname "*demo" $(FINDEXEC) \
	-exec {} \;

.PHONY: build
build: checkinit
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

.PHONY: buildr
buildr: cleanr
	@$(R) --slave -e "source('config/utilities.R'); \
	load_packages(c('Rcpp', 'RcppArmadillo', 'lava', 'optimx', 'futile.logger'))"
	@$(R) --slave -e "Rcpp::compileAttributes('R-package/${pkg}')"
	@$(R) CMD INSTALL R-package/${pkg}

.PHONY: testr
testr:
	@$(R) -e 'testthat::test_package("./R-package/${pkg}/")'

.PHONY: runr
runr:
	@cd misc; $(R) --silent -f $(TESTR).R

.PHONY: roxygen
roxygen:
	@$(R) -e 'roxygen2::roxygenize("R-package/${pkg}")'

dep_file = R-package/${pkg}/src/dependencies
pkg_dep := $(shell if [ -f "$(dep_file)" ]; then cat ${dep_file}; fi)
pkg_cpp = $(foreach module, ${pkg_dep}, $(patsubst %, src/%.cpp, $(module)))
pkg_hpp = $(foreach module, $(pkg_dep), $(patsubst %, include/target/%.hpp, $(module)))

.PHONY: exportr
exportr:
	@rm -Rf $(BUILD_DIR)/R/$(pkg)
	@mkdir -p $(BUILD_DIR)/R/$(pkg)
	cd R-package/${pkg}; $(GIT) archive HEAD | (cd ../../$(BUILD_DIR)/R/$(pkg); tar x)
	@if [ -z "$(pkg_dep)" ]; then \
	cp src/*.cpp $(BUILD_DIR)/R/$(pkg)/src; \
	cp -a include/target/ $(BUILD_DIR)/R/$(pkg)/inst/include/; \
	else \
	cp $(pkg_cpp) $(BUILD_DIR)/R/$(pkg)/src; \
	mkdir -p $(BUILD_DIR)/R/$(pkg)/inst/include/target; \
	cp $(pkg_hpp) $(BUILD_DIR)/R/$(pkg)/inst/include/target; \
	fi
	sed -i '/^OBJECTS\|SOURCES/d' $(BUILD_DIR)/R/$(pkg)/src/Makevars
	cd $(BUILD_DIR)/R; $(R) CMD build $(pkg) --compact-vignettes=gs+qpdf --resave-data=best

.PHONY: checkr
checkr: exportr
	cd $(BUILD_DIR)/R; $(R) CMD check `$(GETVER) $(pkg)` --timings --as-cran --no-multiarch --run-donttest

.PHONY: rcheck
rcheck:
	cd R-package; $(R) CMD check $(pkg) --no-multiarch

.PHONY: r
r: buildr runr

.PHONY: cleanr
cleanr:
	@rm -Rf R-package/${pkg}/src/*.o R-package/${pkg}/src/*.so R-package/${pkg}.Rcheck

.PHONY: syncr
syncr: exportr
	@if [ -d "../$(pkg)" ]; then \
	cp -rfv $(BUILD_DIR)/R/$(pkg)/.	 ../$(pkg)/; fi

##################################################
## Python package
##################################################

.PHONY: py cleanpy buildpy runpy testpy exportpy

buildpy:
	@cd python-package; $(PYTHON) setup.py install

testpy:
	@cd python-package; $(MAKE) test

runpy:
	@$(PYTHON) misc/$(TESTPY).py

py: buildpy runpy

PYTHON_EXPORT = $(BUILD_DIR)/python
exportpy: clean
	@rm -Rf $(PYTHON_EXPORT); mkdir -p $(PYTHON_EXPORT)
	@cd python-package; $(GIT) archive HEAD | (cd ../$(PYTHON_EXPORT); tar x)
	@cp -a src $(PYTHON_EXPORT)/lib/target-cpp
	@cp -a include $(PYTHON_EXPORT)/lib/target-inc
	@cp -a lib/armadillo $(PYTHON_EXPORT)/lib
	@cp -a lib/catch2 $(PYTHON_EXPORT)/lib
	@cp -a lib/pybind11 $(PYTHON_EXPORT)/lib
	@echo "Python package exported to: ${PYTHON_EXPORT}"

cleanpy:
	@cd python-package; $(MAKE) --no-print-directory clean

##################################################
## Documentation
##################################################

.PHONY: docs doc markdown
docs:
	@cd $(DOXYGEN_DIR); $(DOXYGEN)

doc:	docs
	@$(OPEN) $(DOXYGEN_DIR)/html/index.html

markdown:
	@if [ -z "command -v grip" ]; then \
	echo "Install dependency: pip install grip"; \
	else grip 1313 -b; fi

##################################################
## Unit tests
##################################################

.PHONY: t test testall
t:	checkinit run
	@$(NINJA) -C $(BUILD_DIR) test

test:	checkinit build
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
	-@cclint src/*.cpp include/target/*.h*
	-@cppcheck --enable=warning,style,performance,portability,information,missingInclude --language=c++ --std=c11 -Isrc -Iinclude -Iinclude/target src/

.PHONY: valgrind
## Alternatively, enable Address Sanitizer (ASAN =-Db_sanitize=address)
valgrind:
	@$(NINJA) -C build test_memcheck

##################################################
## Container
##################################################

DOCKER=Dockerfile
DOCKERTAG=$(TARGET)
ifdef ($IMG)
	DOCKER := '$(DOCKER).$(IMG)'
	DOCKERTAG := '$(DOCKERTAG).$(IMG)'
endif

.PHONY: dockerbuild dockerrun docker export
dockerbuild: export
	@$(CONTAINER_RUNTIME) build . -f Dockerfile --network=host -t $(DOCKERTAG)

export:
	@rm -Rf $(BUILD_DIR)/export;
	$(GIT) clone . $(BUILD_DIR)/export
	$(GIT) submodule foreach '$(GIT) clone . ../../$(BUILD_DIR)/export/$(TARGET)/$$path'
# @rm -Rf ${PWD}/tmp/$(TARGET)
# @mkdir -p ${PWD}/tmp/$(TARGET)
# @git archive HEAD | (cd ${PWD}/tmp/$(TARGET); tar x)
# @git submodule foreach 'curdir=${PWD} cd ${PWD}/$$path; git archive HEAD | tar -x -C ${PWD}/tmp/$(TARGET)/$$path'
# @echo "Exported to '${PWD}/tmp/$(TARGET)'"
# @chmod -R 777 ${PWD}/tmp/$(TARGET)

docker:
	$(CONTAINER_RUNTIME) run --user `id -u` -ti --rm --privileged -v ${PWD}:/data $(TARGET) ${CMD}
