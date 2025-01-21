include $(wildcard ../config/*.mk)
MODULES = $(wildcard *demo.cpp)
BINS    = $(foreach module, $(MODULES), $(patsubst %.cpp, %, $(module)))
BUILD_DIR = ../build

default: test

################################################################################

%:  %.cpp init
	@if [ -f $(BUILD_DIR)/build.ninja ]; then \
	$(NINJA) -C $(BUILD_DIR) $(NINJA_BUILD_OPT) $@; \
	else \
	cd $(BUILD_DIR); $(MAKE) $@; \
	fi
	@echo "=================================================="
	@$(BUILD_DIR)/$@


.PHONY: init
init:
	@if [ ! -f $(BUILD_DIR)/build.ninja ] && [ ! -f $(BUILD_DIR)/Makefile ]; then \
		cd ..; $(MAKE) init; \
	fi

.PHONY: test
test: ode_run
	@/usr/bin/env R --slave -e \
	"d <- read.csv('y.csv'); lava::idplot(d, type='l', return.data=TRUE)"

.PHONY: clean
clean:
	cd ..; $(MAKE) clean
