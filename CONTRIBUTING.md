# Contributing to target

First off, thanks for taking the time to contribute! ❤️

All types of contributions are encouraged and valued. See the
[Table of Contents](#table-of-contents)
for details about how to contribute code to this project. Please make sure to
read the relevant section before making your contribution. It will make it a lot
easier for us maintainers and smooth out the experience for all involved.

**Suggestions or bug reports** can be posted here: https://github.com/kkholst/targeted/issues.
For bug reports please include a small reproducible example which clearly demonstrates the bug.

## Table of Contents

- [Branches and branch prefixes](#branches-and-branch-prefixes)
- [Continuous Integration](#continuous-integration)
- [R package development](#r-package-development)

## Branches and branch prefixes

We currently maintain a single active development branch `develop`.

### Branch prefixes

We use prefixes to label branches. A meaningful short description follows the
prefix and hyphens (-) are used for separation. For example,
`feature/new-ml-model-interface` is a valid feature branch name.

*feature/*: Branches for developing new features.\
*bugfix/*: Branches for fixing non-critical bugs.\
*hotfix/*: Branches for fixing critical bugs.\
*docs/*: Branches for writing, updating, or fixing documentation.

### Pull requests

All pull requests (PRs) must be made on `develop`.
The title of the PR should follow the format of
[conventional commits](https://www.conventionalcommits.org/en/v1.0.0/) and a
summary of the proposed changes must be provided in the body of the PR. This
makes it easier for maintainers as title and body can be reused once all commits
are squashed before merging the feature branch into `develop`.

## Continuous integration

A variety of continuous integration tests are set up in
[.github/workflows](.github/workflows) to mitigate the risk of committing
malfunctioning code.

## R package development

### Style guide

Contributors should set up their development environment to comply with the
project specified [editorconfig](https://editorconfig.org/) configuration and
[lintr](https://lintr.r-lib.org/) static code analysis tool. We follow to the
largest extent the
[tidyverse style guide](https://style.tidyverse.org/index.html), which is
checked via the code linter. A notable difference is that we use dots for long
argument names of functions. For example, instead of `predict_args` we use
`predict.args`. We remain using snake case for names of functions and R6 class
methods.

### Unit testing

Unit tests are written using the
[tinytest](https://cran.r-project.org/web/packages/tinytest/index.html)
framework and go into the `inst/tinytest` directory. Tests which
take considerable time to complete (more than a few seconds) go into
`inst/slowtest`. These tests are not performed when checking with
package with `R CMD check`. The organization of test files should match the
organization of `R` files. That is, tests for a function in `R/cate.R` go into
`inst/tinytest/test_cate.R`.

**Tinytest** does not make internal functions directly callable like some other
unit testing packages do. Thus, testing internal functions requires using `:::`
inside the test files like

```{r}
output <- targeted:::some_internal_function(1)
expect_equal(output, 2)
```

More information about unit testing with **tinytest** is provided in this
[vignette](https://cran.r-project.org/web/packages/tinytest/vignettes/using_tinytest.pdf).

### Documentation

The R code is documented using
[roxygen2](https://cran.r-project.org/web/packages/roxygen2/vignettes/roxygen2.html).

Longer documentation are located in the `vignettes` directory in R-markdown format.


## C++ development

The **target** library follows the (Google's C++ style guide)[http://google.github.io/styleguide/cppguide.html>].

The code should be checked using the ``cppcheck`` static code
analyzer and ``cclint`` (which may installed from PyPi using ``pip3
install cclint``).

From the `src/target` directory these checks are made with `make check`.

The C++ code is documented using (Doxygen)
[http://www.doxygen.nl/manual/] ([ref](http://www.doxygen.nl/manual/commands.html)).

### Unit tests
C++ tests are located in the subdirectory `./src/target/tests`
using [doctest](https://github.com/doctest/doctest), and can be executed with
`make test` from the `src/targeto` directory.

The Undefined Behaviour Sanitizer via ``clang++`` can be executed with `make sanitizer`
which runs the unit-tests and examples from the directory ``misc``.


## Attribution

This guide is based on the **contributing.md**.
[Make your own](https://contributing.md/)!
