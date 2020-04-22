# Configuration file for the Sphinx documentation builder.
#
# This file only contains a selection of the most common options. For a full
# list see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Path setup --------------------------------------------------------------

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
#

import os
import sys
import targeted
import subprocess

file_loc = os.path.split(__file__)[0]
curr_path = os.path.dirname(os.path.abspath(os.path.expanduser(__file__)))
libpath = os.path.abspath(os.path.join(os.path.dirname(file_loc), '../../python-package/src/targeted'))
sys.path.insert(0, libpath)
sys.path.insert(0, curr_path)
print(libpath)

# -- Project information -----------------------------------------------------

project = 'target library'
copyright = u'2019-2020, Klaus Kähler Holst'
author = u'Klaus Kähler Holst'

# The full version, including alpha/beta/rc tags
release = targeted.__version__

master_doc = 'index'

# -- General configuration ---------------------------------------------------

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.

extensions = [
#    'matplotlib.sphinxext.plot_directive',
    'sphinx.ext.autodoc',
    'sphinx.ext.doctest',
    'sphinx.ext.napoleon',  # Support for numpy or google docstrings
    'sphinx.ext.mathjax',
    'sphinx.ext.ifconfig',
    'recommonmark',
    'sphinx.ext.todo',
    'sphinx.ext.intersphinx',
    'IPython.sphinxext.ipython_directive',
    'IPython.sphinxext.ipython_console_highlighting']

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = ['build', 'Thumbs.db', '.DS_Store', 'tmp']

source_suffix = ['.rst', '.md', '.txt']

# If true, `todo` and `todoList` produce output, else they produce nothing.
todo_include_todos = True


# -- Options for HTML output -------------------------------------------------

pygments_style = 'sphinx'

# Output file base name for HTML help builder.
htmlhelp_basename = project + 'doc'

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = ['_static']
html_extra_path = ['../tmp'] ## This is where the doxygen 'html' directory should be located (link)
html_copy_source = False
html_show_copyright = False
html_show_sphinx = False
# These paths are either relative to html_static_path
# or fully qualified paths (eg. https://...)
html_css_files = [
    'css/custom.css',
]
# html_logo = '../images/target.svg'
# html_style = '_static/css/custom.css'
# html_math_renderer = 'mathjax'
# html_favicon = ''
# html_baseurl = ''

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.

html_theme = 'default'

# use RTFD theme locally
on_rtd = os.environ.get("READTHEDOCS", None) == "True"
# if not on_rtd:  # only import and set the theme if we're building docs locally

import sphinx_typo3_theme
html_theme = "sphinx_typo3_theme"
html_theme_path = [sphinx_typo3_theme.get_html_theme_path()]
extensions.append("sphinx_typo3_theme")

# import pydata_sphinx_theme
# html_theme = "pydata_sphinx_theme"


# -- Options for LaTeX output ------------------------------------------------

latex_additional_files = ['../latex/refman.pdf']
latex_engine = 'pdflatex'
latex_theme = 'manual'
latex_elements = {
    'papersize': 'a4paper',
    'preamble': r'''
\usepackage{pdfpages}
'''}
latex_logo = '../images/targeted.pdf'

# ----------------------------------------------------------------------------

subprocess.call('cd .. ; doxygen', shell=True)

if on_rtd:
    subprocess.call('cd ../latex ; make', shell=True)

def setup(app):
    app.add_css_file('custom.css')
