PKG:=targeted
PIPARG:= # e.g., --user
PIP:=/usr/bin/env pip3 $(PIPARG)
PYTHON:=/usr/bin/env python3

default: install

.PHONY: update
update:
	$(PIP) install wheel setuptools twine --upgrade

UID := $(shell id -u)
PWD := $(shell pwd)
.PHONY: build source
DOCKERRUN = docker run -ti --rm --privileged --user $(UID):0 --network=host -v $(PWD):/io buildmany

build: clean docker
	$(DOCKERRUN)
	mv wheelhouse/* dist/; rmdir wheelhouse
	$(MAKE) sourcebuild

source:
	$(PYTHON) setup.py sdist

.PHONY: clean
clean:
	@rm -Rf dist/* *.egg-info build *.so *.so.* src/*.egg-info tests/bin/*

.PHONY: upload_test
upload_test:

	$(PYTHON) -m twine upload --repository pypitest dist/*
.PHONY: upload 
upload:	source
	$(PYTHON) -m twine upload dist/* --verbose --repository-url https://upload.pypi.org/legacy/

.PHONY: uninstall
uninstall:
	$(PIP) uninstall $(PKG) -y

.PHONY: install
install: uninstall
	$(PYTHON) setup.py install
	rm -Rf *.egg-info

.PHONY: check
check:
	$(PYTHON) setup.py flake8

.PHONY: test
test:
	$(PYTHON) setup.py test

.PHONY: installpypi_test
installpypi_test:
	$(PIP) install --upgrade --index-url https://test.pypi.org/simple/ --extra-index-url https://pypi.org/simple/ ${PKG}

.PHONY: installpypi
installpypi: uninstall
	$(PIP) install --upgrade $(PKG) --no-binary :all:


################################################################################

.PHONY: dockerbuild
dockerbuild:
	docker build  --build-arg UID=${UID} --network=host . -t  buildmany

.PHONY: dockertest
dockertest:
	docker run -ti --rm --privileged --network=host -v $(PWD):/io buildmany bash

DOCKERTEST=kkholst/stat:python
.PHONY: docker
docker:
	docker run -ti --rm --privileged --network=host -u 0:0 -v $(PWD):/data ${DOCKERTEST} bash
