###############
Development
###############

Contributing
--------------------

Contributions to the **target** library (or any of the accompanying R
or python packages) are welcome as pull requests to the `develop
branch <https://github.com/kkholst/target/tree/develop>`_.  Please see the
below guidelines on *coding styles*, *testing* and *documentation*.

**Suggestions or bug reports** can be posted here: https://github.com/kkholst/target/issues

For bug reports please include a small reproducible example which
clearly demonstrates the bug.

Coding styles, unit tests and documentation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Documentation
~~~~~~~~~~~~~~~~~~~~

The C++ code is documented using Doxygen.

- http://www.doxygen.nl/manual/
- http://www.doxygen.nl/manual/commands.html

The python code is documented using the numpy docstring format

- https://numpydoc.readthedocs.io/en/latest/format.html

The R code is documented using roxygen2

- https://cran.r-project.org/web/packages/roxygen2/vignettes/roxygen2.html

The main documentation of this project is based on sphinx and the in
subdirectory (relative to the root of the repository) ``doc/source``

- https://www.sphinx-doc.org/en/master/

The documentation should be written in ReStructuredText (rst)

- https://docutils.sourceforge.io/docs/user/rst/quickstart.html
- https://docutils.sourceforge.io/docs/user/rst/quickref.html

or alternatively in Emacs Org mode (org).

- https://orgmode.org/

Org is the *preferred format*, in which case both the exported rst and
org file must be checked into the git repository (use `ox-ravel
<https://github.com/chasberry/orgmode-accessories>`_ to automatically
export to rst). Note, that in this case only the org-file should be
manually edited.

Unit tests
~~~~~~~~~~~~~~~~~~~~

C++ tests are located in the subdirectory `./tests
<https://github.com/kkholst/target/tree/develop/tests>`_ and written
using doctest

- https://github.com/onqtam/doctest/blob/master/doc/markdown/tutorial.md

The unit tests can be compiled and executed from the root directory with

.. code:: sh

  make test

R tests are located in the subdirectory `./R-package/targeted/tests
<https://github.com/kkholst/target/tree/develop/R-package/targeted/tests>`_ and written using testthat

- https://r-pkgs.org/tests.html

The unit tests can be compiled and executed from the root directory with

.. code:: sh

  make testr

To run the R package checks

.. code:: sh

  make checkr

Python tests are located in the subdirectory `./python-package/tests <https://github.com/kkholst/target/tree/develop/python-package/tests>`_

The unit tests can be compiled and executed from the root directory with

.. code:: sh

  make testpy


or directly from the ``python-package``directory with ``pytest-runner``:

.. code:: sh

  pytest


Code coverage
~~~~~~~~~~~~~~~~~~~~

Code coverage results are automatically created and posted here:

https://codecov.io/gh/kkholst/target

Reports can also be built locally by running

.. code:: sh

  make cov

from either the root directory (code coverage for C++ source code) or
``./python-package`` (code coverage for python source codE).


Coding style
~~~~~~~~~~~~~~~~~~~~

The **target** library follows the `Google's C++ style guide
<http://google.github.io/styleguide/cppguide.html>`_.

The code should be checked using the ``cppcheck`` static code
analyzer and ``cclint`` (which may installed from PyPi using ``pip3
install cclint``).

From the root directory run

.. code:: sh

  make check


Sanitizers
~~~~~~~~~~~~~~~~~~~~

The Undefined Behaviour Sanitizer via ``clang++`` can be executed with

.. code:: sh

  make sanitizer

which runs the unit-tests and examples from the directory ``misc``.

Roadmap
--------------------
