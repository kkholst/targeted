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
#import guzzle_sphinx_theme
import pylons_sphinx_themes

file_loc = os.path.split(__file__)[0]
curr_path = os.path.dirname(os.path.abspath(os.path.expanduser(__file__)))
libpath = os.path.abspath(os.path.join(os.path.dirname(file_loc), '../../python-package/src/targeted'))
sys.path.insert(0, libpath)
sys.path.insert(0, curr_path)
print(libpath)

# sys.path.insert(0, os.path.abspath('../../src/'))

# -- Project information -----------------------------------------------------

project = 'targeted'
copyright = u'2019-2020, Klaus Kähler Holst'
author = u'Klaus Kähler Holst'
# The full version, including alpha/beta/rc tags

release = targeted.__version__
html_logo = '../images/small.png'

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
    'IPython.sphinxext.ipython_console_highlighting',
    'breathe']

# graphviz_output_format = 'png'
# plot_formats = [('svg', 300), ('png', 100), ('hires.png', 300)]
# plot_html_show_source_link = False
# plot_html_show_formats = False

breathe_projects = {
    "target" : "../xml/",
    }
breathe_default_project = "target"
master_doc = 'index'

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = ['build', 'Thumbs.db', '.DS_Store', 'tmp']

source_suffix = ['.rst', '.md', '.txt']

# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#

html_theme = 'default'
# html_theme = 'sphinx_rtd_theme'
pygments_style = 'sphinx'

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = ['_static']

html_extra_path = ['../tmp']

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
# html_theme_path = guzzle_sphinx_theme.html_theme_path()
# html_theme = 'guzzle_sphinx_theme'
# Register the theme as an extension to generate a sitemap.xml
# extensions.append("guzzle_sphinx_theme")

# Guzzle theme options (see theme.conf for more information)
# html_theme_options = {
#     # Set the name of the project to appear in the sidebar
#     "project_nav_name": "Target"
# }

# html_theme_path = pylons_sphinx_themes.get_html_themes_path()
# html_theme = 'pylons'
# # Register the theme as an extension to generate a sitemap.xml
# extensions.append("pylons_sphinx_themes")


# html_sidebars = {
#   '**': ['logo-text.html', 'globaltoc.html', 'searchbox.html']
# }

# html_sidebars = {               #
#     '**': [
#         'relations.html',  # needs 'show_related': True theme option to display
#         'searchbox.html',
#     ]
# }

# Output file base name for HTML help builder.
htmlhelp_basename = project + 'doc'


# -- Extension configuration -------------------------------------------------

# -- Options for todo extension ----------------------------------------------

# If true, `todo` and `todoList` produce output, else they produce nothing.
todo_include_todos = True

# use RTFD theme locally
# on_rtd is whether we are on readthedocs.org, this line of code grabbed from docs.readthedocs.org

on_rtd = os.environ.get("READTHEDOCS", None) == "True"

if not on_rtd:  # only import and set the theme if we're building docs locally
    import sphinx_rtd_theme

    html_theme = "sphinx_rtd_theme"
    html_theme_path = [sphinx_rtd_theme.get_html_theme_path()]



# hook for doxygen
def run_doxygen(folder):
    """Run the doxygen make command in the designated folder."""
    try:
        retcode = subprocess.call("cd %s; make doxygen" % folder, shell=True)
        if retcode < 0:
            sys.stderr.write("doxygen terminated by signal %s" % (-retcode))
    except OSError as e:
        sys.stderr.write("doxygen execution failed: %s" % e)


def generate_doxygen_xml(app):
    """Run the doxygen make commands if we're on the ReadTheDocs server"""
    read_the_docs_build = os.environ.get('READTHEDOCS', None) == 'True'
    if read_the_docs_build:
        run_doxygen('..')


def setup(app):
    app.add_stylesheet('custom.css')
