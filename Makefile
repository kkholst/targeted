PKG ?= targeted
R = R --silent --no-save --no-echo
BUILD_DIR = build
GETVER = $(shell cat DESCRIPTION | grep Version | cut -d":" -f2 | tr -d '[:blank:]')

default: check

rcpp:
	@echo 'Rcpp::compileAttributes(".")' | $(R)

readme:
	@echo 'devtools::build_readme(".")' | $(R)
	@cp inst/README.md README.md

roxygen:
	@echo 'devtools::document(".")' | $(R)

doc: roxygen rcpp readme

clean:
	@find vignettes inst '(' -name "*.html" -o -name "*_cache" -o -name "*_files" ')' -exec rm -Rf {} +
	@# remove output files of code coverage
	@rm -rf tests/lib tests/coverage-report.html
	@rm -rf build

.PHONY: build
build:
	@rm -Rf $(BUILD_DIR) && mkdir -p $(BUILD_DIR)
	@echo 'pkgbuild::build(path=".", dest_path="$(BUILD_DIR)", args="--compact-vignettes=qpdf --resave-data=best")' | $(R)

install:
	@echo 'devtools::install("$(pkg)", upgrade = "never")' | $(R)

dependencies-install:
	@echo 'devtools::install_deps("$(pkg)", dependencies = TRUE)' | $(R)

dependencies-upgrade:
	@echo 'devtools::install("$(pkg)", upgrade = "always")' | $(R)

check-cran: build
	@$(R) CMD check build/$(PKG)_$(GETVER).tar.gz --timings --as-cran --no-multiarch --run-donttest

check:
	@_R_CHECK_FORCE_SUGGESTS_=0 echo 'res <- rcmdcheck::rcmdcheck(".", build_args=c("--no-build-vignettes"), args=c("--ignore-vignettes"))' | $(R)

lint:
	@echo 'devtools::lint(".")' | $(R)

test-installed: # tests locally installed version package
	@echo 'tinytest::test_package("$(PKG)")' | $(R)

test-loadall:
	@echo 'devtools::load_all("."); tinytest::test_all(".")' | $(R)

coverage:
	@echo 'covr::report(file="tests/coverage-report.html")' | $(R)
	@open tests/coverage-report.html
