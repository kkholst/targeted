# -*- mode: makefile; -*-

UNAME := $(shell uname -s)
CONTAINER_RUNTIME := $(shell command -v podman 2> /dev/null || echo docker)
DOXYGEN = doxygen
OPEN := $(shell which xdg-open || which gnome-open || which open)
PYTHON := /usr/bin/env python3
PIPARG := # e.g., --user
PIP := /usr/bin/env pip3 $(PIPARG)
R := /usr/bin/env R --no-save --no-restore
GIT := /usr/bin/env git
CMAKE := /usr/bin/env cmake
SELF_DIR := $(dir $(lastword $(MAKEFILE_LIST)))
GETVER := $(abspath $(SELF_DIR))/getrversion.py
LINTER := /usr/bin/env cclint
NINJA := /usr/bin/env ninja
NINJA_BUILD_OPT = -v
FINDEXEC :=
ifeq ($(UNAME),Darwin)
  FINDEXEC +=  -perm +111
else
  FINDEXEC +=  -executable
endif
