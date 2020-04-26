###############
Installation
###############

This program may be compiled as a shared library or as stand-alone
python and R libraries.


Dependencies
--------------------

To build the shared library ninja(s) are needed.

.. code:: sh

   pip install cmake ninja


The following dependencies are included as submodules:

#. Armadillo http://arma.sourceforge.net/docs.html
#. Catch2 (unit tests only) https://github.com/catchorg/Catch2/blob/master/docs/Readme.md#top
#. pybind11++ (python bindings only) https://pybind11.readthedocs.io
#. spdlog (logging) https://github.com/gabime/spdlog


R
--------------------

The R package can be built and installed with

.. code:: sh

   make r


python
--------------------

The python package can be installed directly from PyPi (wheels
available for Mac OS X and Linux):

.. code:: sh

   pip install targeted

or installed from source

.. code:: sh

   make py
