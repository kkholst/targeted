###############
Installation
###############

This program may be compiled as a shared library or as stand-alone
python and R libraries.


C++
--------------------

To build the shared library, ninja(s) are needed.

.. code:: sh

   pip install cmake ninja


The following dependencies are included as submodules:

#. Armadillo http://arma.sourceforge.net/docs.html
#. doctest (unit tests only) https://github.com/onqtam/doctest/
#. pybind11++ (python bindings only) https://pybind11.readthedocs.io
#. spdlog (logging) https://github.com/gabime/spdlog

Use `make <https://www.gnu.org/software/make/>`_ to install the
headers and shared libraries  (here the destination is the ``/opt/`` directory)

.. code:: sh

   make install INSTALL_DIR=/opt

R
--------------------

The R package can be built and installed directy from `CRAN <https://cran.r-project.org/>`_

.. code:: r

   install.packages("targeted")

or installed from source

.. code:: sh

   make r


python
--------------------

The python package can be installed directly from `PyPi <https://pypi.org/project/targeted/>`_

.. code:: sh

   pip install targeted

.. note:: Presently binary wheels are available for Linux
	  systems and Mac OS X running Python>=3.6. Windows
	  installations must be built from source.

or installed from source

.. code:: sh

   make py
