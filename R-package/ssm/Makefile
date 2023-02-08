
default: run

.PHONY: export
export:
	@mkdir -p tmp/targeted
	git archive HEAD | (cd tmp/targeted; tar x)
	cp ../../src/*.cpp tmp/targeted/src
	cp -a ../../include/target tmp/targeted/src

.PHONY: build
build:
	docker build . -f Dockerfile --tag rubsan_targeted

.PHONY: run
run: export
	docker run -ti --rm --privileged --volume $(shell pwd)/tmp:/project rubsan_targeted bash
