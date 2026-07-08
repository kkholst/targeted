# AGENTS.md

R package `targeted` (semiparametric / causal inference). C++ backend
via Rcpp + RcppArmadillo.

## Layout gotchas

- `R/` source. `man/` and `R/RcppExports.R` and `NAMESPACE` are
  generated – never edit by hand. Regenerate via `make doc` (= roxygen +
  Rcpp::compileAttributes + README rebuild).
- `src/target/` is a **git submodule** (the C++ `target` library) plus
  its own submodules (`armadillo`, `spdlog`, `doctest`). Run
  `git submodule update --init --recursive` before building C++.
- `src/` top level only contains thin Rcpp interfaces
  (`*_interface.cpp`). Real C++ lives in `src/target/src` with its own
  `Makefile`, `CMakeLists.txt`, and `doctest` tests in
  `src/target/tests`.
- README is generated: edit `inst/README.Rmd`, then `make readme`
  (copies `inst/README.md` -\> `README.md`). Do not edit `README.md`
  directly.
- Tests live in `inst/tinytest/` (NOT `tests/testthat/`);
  `tests/tinytest.R` is just the dispatcher. Slow tests in
  `inst/slowtest/` are skipped by `R CMD check`.

## Commands (use Makefile)

- `make check` — `rcmdcheck` without vignettes (fastest pre-PR gate).
- `make check-cran` — full `R CMD check --as-cran --run-donttest`.
- `make test` — runs tinytest against the **installed** package. Run
  `make install` first if you changed code.
- `make test-loadall` — `devtools::load_all` + tinytest; use during
  iteration to skip reinstall.
- `make test-slow` — runs `inst/slowtest/` (expensive, not in normal
  `make test`).
- `make lint` — `lintr::lint_package()`. Config in `.lintr` (line 80,
  tidyverse-ish, several linters disabled).
- `make doc` — regenerate roxygen man pages, RcppExports, and README.
  Run after editing roxygen blocks, exported symbols, or
  `inst/README.Rmd`.
- `make rcpp` — only
  [`Rcpp::compileAttributes()`](https://rdrr.io/pkg/Rcpp/man/compileAttributes.html).
  Run after adding/removing `// [[Rcpp::export]]`.
- `make install` —
  [`remotes::install_local`](https://remotes.r-lib.org/reference/install_local.html),
  needed before `make test`.

Run a single test file (no built-in target):
`R -e 'devtools::load_all("."); tinytest::run_test_file("inst/tinytest/test_cate.R")'`.

## Conventions that bite

- **Argument names use dots, not underscores** for long names
  (e.g. `predict.args`, not `predict_args`). Function and method names
  use snake_case. Enforced by review, not lint.
- Roxygen `@param` lines must declare the type in parens,
  e.g. `@param n (integer) ...`.
- Tinytest does not auto-expose internals: test private functions via
  `targeted:::name(...)`.
- Test file naming mirrors `R/`: `R/cate.R` -\>
  `inst/tinytest/test_cate.R`.
- `lintr` excludes `inst/misc`, `vignettes`, and `R/RcppExports.R`.
  Don’t try to fix lint there.
- C++ style: Google C++ guide; check via `make check` inside
  `src/target` (uses `cppcheck` + `cclint`).

## Branch / PR

- Active dev branch is `dev` (not `main`). All PRs target `dev`.
- Branch prefix required: `feature/`, `bugfix/`, `hotfix/`, `docs/`, or
  `develop/`.
- PR title must follow Conventional Commits (commits get squashed).

## Misc

- R \>= 4.1 required
- Vignettes use **Quarto**
  (`SystemRequirements: Quarto command line tools`), not knitr-only.
- `targeted.Rcheck/`, `build/`, `tmp/`, `Library/` are local scratch —
  ignore.
- See `CONTRIBUTING.md` for the full version of the above.
