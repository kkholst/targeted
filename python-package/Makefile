PKG:=targeted
PIP:=/usr/bin/env pip3
PYTHON:=/usr/bin/env python3

default: install

.PHONY: update
update:
	$(PIP) install wheel setuptools twine --user --upgrade

UID := $(shell id -u)
PWD := $(shell pwd)
.PHONY: build sourcebuild
DOCKERRUN = docker run -ti --rm --privileged --user $(UID):0 --network=host -v $(PWD):/io buildmany
build: clean dockerbuild
	$(DOCKERRUN)
	mv wheelhouse/* dist/; rmdir wheelhouse
	$(MAKE) sourcebuild

source:
	$(PYTHON) setup.py sdist

.PHONY: clean
clean:
	@rm -Rf dist/* *.egg-info build *.so *.so.* src/*.egg-info tests/bin/*

.PHONY: upload
upload:	
	$(PYTHON) -m twine upload --repository pypitest dist/*

.PHONY: install
install:
	$(PYTHON) setup.py install
	rm -Rf *.egg-info

.PHONY: check
check:
	$(PYTHON) setup.py flake8

.PHONY: test
test:
	$(PYTHON) setup.py test

.PHONY: installpypi
installpypi:
	$(PIP) install --upgrade --index-url https://test.pypi.org/simple/ --extra-index-url https://pypi.org/simple/ ${PKG}

.PHONY: dockerbuild
dockerbuild:
	docker build  --build-arg UID=${UID} . -t buildmany

.PHONY: dockertest
dockertest:
	docker run -ti --rm --privileged --network=host -v $(PWD):/io buildmany bash

DOCKERTEST=kkholst/rdbase:base
.PHONY: docker
docker:
	docker run -ti --rm --privileged --network=host -u 0:0 -v $(PWD):/data ${DOCKERTEST} bash
