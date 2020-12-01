import sys, os, subprocess, sysconfig
from distutils import sysconfig
import setuptools
from setuptools import Extension
from setuptools.command.build_ext import build_ext
from distutils.version import LooseVersion
from shutil import copyfile, copymode
import re
import platform

pkg = 'targeted'

with open('README.md', 'r') as fh:
    long_description = fh.read()

base_dir = os.path.dirname(__file__)

about = {}
with open(os.path.join(base_dir, 'src', pkg, '__about__.py')) as f:
    exec(f.read(), about)

class CMakeExtension(Extension):
    def __init__(self, name, sourcedir=''):
        Extension.__init__(self, name, sources=[])
        self.sourcedir = os.path.abspath(sourcedir)

class CMakeBuild(build_ext):
    def run(self):
        try:
            out = subprocess.check_output(['cmake', '--version'])
        except OSError:
            raise RuntimeError(
                'CMake must be installed to build the following extensions: ' +
                ', '.join(e.name for e in self.extensions))

        if platform.system() == 'Windows':
            cmake_version = LooseVersion(re.search(r'version\s*([\d.]+)',
                                         out.decode()).group(1))
            if cmake_version < '3.1.0':
                raise RuntimeError('CMake >= 3.1.0 is required on Windows')

        for ext in self.extensions:
            self.build_extension(ext)

    def build_extension(self, ext):
        extdir = os.path.abspath(
            os.path.dirname(self.get_ext_fullpath(ext.name)))
        cmake_args = ['-DCMAKE_LIBRARY_OUTPUT_DIRECTORY=' + extdir,
                      '-DPYTHON_EXECUTABLE=' + sys.executable,
                      '-DPYTHON_INCLUDE_DIR=' + sysconfig.get_python_inc()]

        cfg = 'Debug' if self.debug else 'Release'
        build_args = ['--config', cfg]

        if platform.system() == 'Windows':
            cmake_args += ['-DCMAKE_LIBRARY_OUTPUT_DIRECTORY_{}={}'.format(
                cfg.upper(),
                extdir)]
            if sys.maxsize > 2**32:
                cmake_args += ['-A', 'x64']
            build_args += ['--', '/m']
        else:
            cmake_args += ['-DCMAKE_BUILD_TYPE=' + cfg]
            build_args += ['--', '-j2']

        env = os.environ.copy()
        env['CXXFLAGS'] = '{} -DVERSION_INFO=\\"{}\\"'.format(
            env.get('CXXFLAGS', ''),
            self.distribution.get_version())
        if not os.path.exists(self.build_temp):
            os.makedirs(self.build_temp)
        subprocess.check_call(['cmake', ext.sourcedir] + cmake_args,
                              cwd=self.build_temp, env=env)
        subprocess.check_call(['cmake', '--build', '.'] + build_args,
                              cwd=self.build_temp)
        # Copy *_test file to tests directory
        test_bin = os.path.join(self.build_temp, pkg + '_c_test')
        if os.path.exists(test_bin):
            self.copy_test_file(test_bin)
        print()  # Add an empty line for cleaner output

    def copy_test_file(self, src_file):
        '''
        Copy ``src_file`` to ``dest_file`` ensuring parent directory exists.
        By default, message like `creating directory /path/to/package` and
        `copying directory /src/path/to/package -> path/to/package` are displayed
        on standard output. Adapted from scikit-build.
        '''
        # Create directory if needed
        dest_dir = os.path.join(os.path.dirname(
            os.path.abspath(__file__)), 'tests', 'bin')
        if dest_dir != '' and not os.path.exists(dest_dir):
            print('creating directory {}'.format(dest_dir))
            os.makedirs(dest_dir)

        # Copy file
        dest_file = os.path.join(dest_dir, os.path.basename(src_file))
        print('copying {} -> {}'.format(src_file, dest_file))
        copyfile(src_file, dest_file)
        copymode(src_file, dest_file)


ext_modules = [CMakeExtension(pkg+'/__'+pkg+'_c__')]
scripts = list(map(lambda x: 'bin/'+x, os.listdir('bin')))
cmdclass = dict(build_ext=CMakeBuild)

# do not require installation of extensions if built by ReadTheDocs
if 'READTHEDOCS' in os.environ and os.environ['READTHEDOCS'] == 'True':
    ext_modules = []
    if 'build_ext' in cmdclass:
        del cmdclass['build_ext']

setuptools.setup(
    name=about['__name__'],
    version=about['__version__'],
    author=about['__author__'],
    author_email=about['__email__'],
    url=about['__url__'],
    description=about['__description__'],
    long_description_content_type='text/markdown',
    long_description=long_description,
    packages=setuptools.find_packages('src'),
    package_dir={'': 'src'},
    ext_modules=ext_modules,
    cmdclass=cmdclass,
    zip_safe=False,
    license=about['__license__'],
    classifiers=[
        'Programming Language :: Python :: 3',
        'Programming Language :: C++',
        'Intended Audience :: Science/Research',
        'Topic :: Scientific/Engineering :: Mathematics',
        'License :: OSI Approved :: Apache Software License',
        'Operating System :: OS Independent'
    ],
    scripts=scripts,
    install_requires=[
        'pandas',
        'scipy>=1.3.2',
        'statsmodels>=0.10.2',
        'patsy>=0.5',
        'numpy>=1.15'
    ],
    package_data={pkg: ['data/*.dat', 'data/*.gz']},
    tests_require=['pytest'],
)
