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

default: run

##################################################

all: clean run

clean:
	@rm -Rf $(BUILD_DIR) $(VALGRIND_DIR) $(DOXYGEN_DIR)/html $(COVERAGE_DIR)

init:	clean
	@$(MESON) $(BUILD_DIR)

run:
	@if [ ! -d "$(BUILD_DIR)" ]; then $(MAKE) --no-print-directory init; fi
	@$(MAKE) --no-print-directory build # > /dev/null
	@printf "\n"
	@build/dredemo
	@printf "\n\n"

build:
	@ninja -C $(BUILD_DIR)

install:
	ninja -C $(BUILD_DIR) install

uninstall:
	ninja -C $(BUILD_DIR) uninstall

##################################################
## Documentation
##################################################

docs:
	@cd $(DOXYGEN_DIR); doxygen

doc:	docs
	@$(OPEN) $(DOXYGEN_DIR)/html/index.html

##################################################
## Unit tests & code coverage
##################################################

rtest:
	R --no-save < ex.R

test:	build
	@ninja -C $(BUILD_DIR) test

cov:
	@$(MESON) $(COVERAGE_DIR) -Db_coverage=true
	@ninja -C $(COVERAGE_DIR)
	@ninja -C $(COVERAGE_DIR) test
	@ninja -C $(COVERAGE_DIR) coverage-html	
	@$(OPEN) $(COVERAGE_DIR)/meson-logs/coveragereport/index.html

##################################################
## Debugging, profiling, and memory inspection
##################################################

check:
	-cclint src/*.cpp src/*.h
	-cppcheck --enable=all src/

## Alternatively, enable Address Sanitizer (ASAN argument)
valgrind:
	@meson $(VALGRIND_DIR)
	@cd $(VALGRIND_DIR); ninja test & meson test --wrap='valgrind  --tool=memcheck --leak-check=yes --show-reachable=yes --num-callers=20 --track-fds=yes '
	@less $(VALGRIND_DIR)/meson-logs/testlog-valgrind.txt


##################################################
## Docker
##################################################

include $(HOME)/configuration/scripts/Makefile.docker
IMG=rtarget
CONTAINER=targetR
DATAHOST=/data
TARGET=$(HOME)/Software/target
TARGET_TMP=target.tmp

docker_run: drm drun dcopy
	@echo "Publish::org(read.csv(file='./result.csv'))" | R --slave
	@sleep 0.2
	@$(MAKE) drm &> /dev/null # Clean-up

drun:
	@docker run --name $(CONTAINER) -it \
	    --privileged -v $(DATAHOST):/data \
	    --user $$(id -u):$$(id -g) \
            $(IMG) &> batch.out

dcopy:
	@docker cp '$(CONTAINER)':/data/result.csv .

drm:
	@if [ "$$(docker ps -aq -f status=exited -f name=$(CONTAINER))" ]; then \
		docker rm '$(CONTAINER)' &> /dev/null; \
	fi;

dbuild:
	@rm -Rf $(TARGET_TMP) && cp -a $(TARGET) $(TARGET_TMP)
	@docker build --network=host -t $(IMG) . --build-arg TARGET=$(TARGET_TMP)

dr:
	@$(MAKE) --no-print-directory docker_xrun CMD="R --no-save"

##################################################

.PHONY: build test check dbuild drun dcopy drm docs doc cov
