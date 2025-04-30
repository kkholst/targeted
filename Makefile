PKG ?= targeted
R = R --silent --no-save --no-echo
BUILD_DIR = build
GETVER = $(shell cat DESCRIPTION | grep Version | cut -d":" -f2 | tr -d '[:blank:]')
make_build_dir = rm -Rf $(BUILD_DIR) && mkdir -p $(BUILD_DIR)
CLIFF_CFG = .cliff.toml
CHANGELOG = CHANGELOG.md

default: check

# generate changelog for unreleased commits
cliff-unreleased:
	@git cliff --unreleased -c $(CLIFF_CFG)

cliff-unreleased-prepend:
	@git cliff --unreleased -c $(CLIFF_CFG) -p $(CHANGELOG)

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
	@$(make_build_dir)
	@echo 'pkgbuild::build(path=".", dest_path="$(BUILD_DIR)", args="--compact-vignettes=qpdf --resave-data=best")' | $(R)

install:
	@echo 'devtools::install(".", upgrade = "never")' | $(R)

dependencies-install:
	@echo 'devtools::install_deps(".", dependencies = TRUE)' | $(R)

dependencies-upgrade:
	@echo 'devtools::install(".", upgrade = "always")' | $(R)

check-cran: build
	@$(R) CMD check build/$(PKG)_$(GETVER).tar.gz --timings --as-cran --no-multiarch --run-donttest

check:
	@_R_CHECK_FORCE_SUGGESTS_=0 echo 'res <- rcmdcheck::rcmdcheck(".", build_args=c("--no-build-vignettes"), args=c("--ignore-vignettes"))' | $(R)

lint:
	@echo 'lintr::lint_package(show_progress = TRUE, exclusions = list("R/intsurv.R", "R/cumhaz.R"))' | $(R)

test: test-installed
test-installed: # tests locally installed version package
	@echo 'tinytest::test_package("$(PKG)")' | $(R)

test-loadall:
	@echo 'devtools::load_all("."); tinytest::test_all(".")' | $(R)

slowtest: test-slow
test-slow:
	@$(R) -q -e 'library("targeted"); tinytest::run_test_dir("inst/slowtest")'

test-all: test test-slow

coverage:
	@echo 'covr::report(file="tests/coverage-report.html")' | $(R)
	@open tests/coverage-report.html

.PHONY: man
man:
	@$(make_build_dir)
	@echo 'devtools::build_manual(".", path = "$(BUILD_DIR)")' | $(R)
	@open build/$(PKG)_$(GETVER).pdf
