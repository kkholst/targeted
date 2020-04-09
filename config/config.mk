# -*- mode: makefile; -*-

UNAME := $(shell uname -s)
CONTAINER_RUNTIME := $(shell command -v podman 2> /dev/null || echo docker)
DOXYGEN = $(shell command -v doxygen)
OPEN := $(shell which xdg-open || which gnome-open || which open)
PYTHON := /usr/bin/env python3
PIPARG := # e.g., --user
PIP := /usr/bin/env pip3 $(PIPARG)
R := /usr/bin/env R --no-save --no-restore
GIT := $(shell command -v git)
CMAKE := $(shell command -v cmake)
SELF_DIR := $(dir $(lastword $(MAKEFILE_LIST)))
GETVER := $(abspath $(SELF_DIR))/getrversion.py
LINTER := $(shell command -v cclint)
NINJA :=  $(shell command -v ninja)
NINJA_BUILD_OPT = -v
FINDEXEC :=
ifeq ($(UNAME),Darwin)
  FINDEXEC +=  -perm +111
else
  FINDEXEC +=  -executable
endif
