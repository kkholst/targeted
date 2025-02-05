# -*- mode: makefile; -*-

include $(wildcard config/*.mk)
TARGET = target
BUILD_DIR = build
VALGRIND_DIR = build/codetest
INSTALL_DIR = $(HOME)/local
PKGLIB = ON
IMG=# Dockerfile postfix
CXX=
EXTRA=
BUILD=-DUSE_PKG_LIB=$(PKGLIB) -DCOTIRE=OFF -DCMAKE_INSTALL_PREFIX=$(INSTALL_DIR) -DCMAKE_BUILD_TYPE=Debug \
  -DCMAKE_FIND_PACKAGE_NO_PACKAGE_REGISTRY=ON -Wno-dev -DCMAKE_VERBOSE_MAKEFILE=ON \
  -DCMAKE_EXPORT_COMPILE_COMMANDS=YES -H.
BUILD+=$(EXTRA)
ifneq ($(CXX),)
   BUILD+= -DCMAKE_CXX_COMPILER=$(CXX)
endif
ifneq ($(NINJA),)
   BUILD+= -GNinja
endif

# R package:
pkg = targeted
TESTR = $(pkg)_test

# python package
TESTPY = $(pkg)_test

##################################################

default: build run

all: clean run

##################################################
cleansrc:
	@rm -Rf $(BUILD_DIR) $(VALGRIND_DIR)
	@rm -Rf src/*.o src/*.so

clean: cleansrc

cleanall: cleansrc cleandoc

init: cleansrc
	@echo "Build options: $(BUILD)"
#	@$(CMAKE) $(BUILD) -B$(BUILD_DIR)
	@mkdir -p $(BUILD_DIR)
	@cd $(BUILD_DIR); $(CMAKE) $(BUILD) ..
	@ln -fs $(BUILD_DIR)/compile_commands.json

checkinit: init-submodules
	@if [ ! -f "$(BUILD_DIR)/CMakeCache.txt" ]; then $(MAKE) init; fi

init-submodules:
	@if [ ! -e "lib/armadillo/.git" ] || [ ! -e "lib/doctest/.git" ] \
	|| [ ! -e "lib/spdlog/.git" ] || [ ! -e "lib/pybind11/.git" ]; then \
	$(GIT) submodule update --init --recursive; fi

run:
	@if [ ! -d "$(BUILD_DIR)" ]; then $(MAKE) --no-print-directory init; fi
	@$(MAKE) --no-print-directory build # > /dev/null
		@find $(BUILD_DIR)/ -maxdepth 1 \
	\( -iname "*demo" -o -iname "*run" \) $(FINDEXEC) \
	-exec sh -c "printf '___ {} ____________________________________________________________________\n'; {}" \;

build: checkinit
	@if [ -f $(BUILD_DIR)/build.ninja ]; then \
	$(NINJA) -C $(BUILD_DIR) $(NINJA_BUILD_OPT); \
	else \
	cd $(BUILD_DIR); make; \
	fi

install: checkinit
	$(NINJA) -C $(BUILD_DIR) install

uninstall:
	$(NINJA) -C $(BUILD_DIR) uninstall

##################################################
## Documentation
##################################################

.PHONY: doc
doc:
	@cd doc; doxygen
	@cd doc/latex; $(MAKE)
	$(OPEN) doc/html/index.html

cleandoc:
	@rm -Rf doc/latex doc/html doc/xml

markdown:
	@if [ -z "command -v grip" ]; then \
	echo "Install dependency: pip install grip"; \
	else grip 1313 -b; fi

##################################################
## Unit tests
##################################################
t: checkinit run
	@$(NINJA) -C $(BUILD_DIR) test

test: checkinit build
	$(BUILD_DIR)/$(TARGET)_test -s
##################################################
## Code coverage
##################################################
cov: coverage
	cd $(BUILD_DIR)/coverage; $(MAKE) coverage
	$(OPEN) $(BUILD_DIR)/coverage/coverage/index.html

coverage:
	rm -Rf $(BUILD_DIR)/coverage; mkdir -p $(BUILD_DIR)/coverage
	cd $(BUILD_DIR)/coverage; $(CMAKE) -Wno-dev -DCMAKE_BUILD_TYPE=Debug -DCODE_COVERAGE=ON ../../ && $(MAKE)

##################################################
## Debugging, profiling, and memory inspection
##################################################
check:
	-@cclint src/*.cpp include/target/*.h* # misc/*demo.cpp misc/*run.cpp misc/*test.cpp
	-@cppcheck --enable=all --suppress=missingIncludeSystem \
		--enable=warning,style,performance,portability,information,missingInclude --language=c++ --std=c11 -Isrc -Iinclude src/

valgrind:
	@$(NINJA) -C build test_memcheck

sanitizer:
	$(MAKE) run test EXTRA="-DCMAKE_CXX_FLAGS='-fsanitize=address -fsanitize=leak -fsanitize=undefined -fsanitize=bool,signed-integer-overflow,null,alignment,float-divide-by-zero,bool' -DCMAKE_CXX_COMPILER=clang++"
	./build/sanitizer_check


##################################################
## Container
##################################################
DOCKER=Dockerfile
DOCKERTAG=$(TARGET)
ifdef ($IMG)
	DOCKER := '$(DOCKER).$(IMG)'
	DOCKERTAG := '$(DOCKERTAG).$(IMG)'
endif

dockerbuild: export
	@$(CONTAINER_RUNTIME) build . -f Dockerfile --network=host -t $(DOCKERTAG)

export:
	rm -Rf $(BUILD_DIR)/export
	$(GIT) clone . $(BUILD_DIR)/export;
	rm -Rf $(BUILD_DIR)/export/lib
	$(GIT) submodule foreach '$(GIT) clone . ../../$(BUILD_DIR)/export/$$path/'

docker:
	$(CONTAINER_RUNTIME) run --user `id -u` -ti --rm --privileged -v ${PWD}:/data $(TARGET) ${CMD}

## Use 'RD' instead of 'R'
dockersan: r_export
	$(CONTAINER_RUNTIME) run -ti --rm --privileged -v ./build/R/:/data \
	rocker/r-devel-san ${CMD}
