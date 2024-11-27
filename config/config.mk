# -*- mode: makefile; -*-

UNAME := $(shell uname -s)
CONTAINER_RUNTIME = $(shell command -v podman 2> /dev/null || echo docker)
DOXYGEN = doxygen
OPEN = $(shell which xdg-open || which gnome-open || which open)
PYTHON = /usr/bin/env python3
PIPARG = # e.g., --user
PIP = $(PYTHON) -m pip $(PIPARG)
PYTEST = /usr/bin/env pytest
R = /usr/bin/env R --no-save --no-restore --silent
GIT = git
CMAKE = /usr/bin/env cmake
SELF_DIR = $(dir $(lastword $(MAKEFILE_LIST)))
GETVER = $(abspath $(SELF_DIR))/getrversion.py
LINTER = cclint
NINJA := $(shell command -v ninja 2> /dev/null)
NINJA_BUILD_OPT = -v
FINDEXEC :=
SED_NOBACKUP :=
ifeq ($(UNAME),Darwin)
  FINDEXEC +=  -perm +111
  SED_NOBACKUP = ""
else
  FINDEXEC +=  -executable
endif