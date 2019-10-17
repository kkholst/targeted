PKG:=pyb

default: install

.PHONY: update
update:
	pip3 install wheel setuptools twine --user --upgrade

UID := $(shell id -u)
PWD := $(shell pwd)
.PHONY: build sourcebuild
DOCKERRUN = docker run -ti --rm --privileged --user $(UID):0 --network=host -v $(PWD):/io buildmany
build: clean dockerbuild
	$(DOCKERRUN)
	mv wheelhouse/* dist/; rmdir wheelhouse
	$(MAKE) sourcebuild

sourcebuild:
	python3 setup.py sdist

.PHONY: clean
clean:
	rm -Rf dist/* *.egg-info build *.so *.so.*

.PHONY: upload
upload:	
	python3 -m twine upload --repository pypitest dist/*

.PHONY: install
install:
	pip3 install -e . --user --upgrade
	rm -Rf *.egg-info

.PHONY: check
check:
	python setup.py flake8

.PHONY: test
test:
	python setup.py test

.PHONY: installpypi
installpypi:
	pip3 install --upgrade --index-url https://test.pypi.org/simple/ --extra-index-url https://pypi.org/simple/ ${PKG}

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
