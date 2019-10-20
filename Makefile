# -*- mode: makefile; -*-

include base.mk

.PHONY: init
init: init-default

.PHONY: cov
cov: cov-default

.PHONY: valgrind
valgrind: valgrind-default
