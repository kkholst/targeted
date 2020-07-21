from setuptools import setup, find_packages
from os import path

here = path.abspath(path.dirname(__file__))

with open(path.join(here, 'README.md'), encoding='utf-8') as f:
    long_description = f.read()

setup(
    name="sgd",
    version="0.0.0",
    description="A fast python library for Stochastic Gradient Descent",
    author="George Vassos",
    author_email="georgios.vassos1@maersk.com",
    classifiers=[
            'Development Status :: 3 - Alpha',
            # Indicate who your project is intended for
            'Intended Audience :: Developers',
            'Topic :: Software Development :: Build Tools',
            # Pick your license as you wish
            'License :: OSI Approved :: MIT License',
            # Specify the Python versions you support here. In particular, ensure
            # that you indicate you support Python 3. These classifiers are *not*
            # checked by 'pip install'. See instead 'python_requires' below.
            'Programming Language :: Python :: 3.7',
            'Programming Language :: Python :: 3.8',
            'Programming Language :: Python :: 3 :: Only',
        ],
    packages=find_packages(where='sgd'),
    python_requires='>=3.7',
    tests_require=['pytest'],
)


