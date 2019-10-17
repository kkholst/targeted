project('target', 'cpp',
        version: '0.1',
        default_options: ['warning_level=3', 'cpp_std=c++11'])

arma = dependency('armadillo', version: '>=8.1')
# rmath = dependency('libRmath', version: '>=3.0')
test = dependency('UnitTest++')
incdir = include_directories('src')


##################################################
##################################################
# Library and headers
install_headers(['src/target.h',
                 'src/glm.h',
                 'src/util.h'],
                subdir: 'target')
srclist = ['src/target.cpp', 'src/glm.cpp']
targetlib = shared_library('target',
                           sources: srclist,
                           dependencies: [arma],
                           include_directories: incdir,
                           install: true)

##################################################
# Executables
##################################################

executable('dredemo',
           sources: ['examples/dredemo.cpp'],
           dependencies: [arma],
           link_with: targetlib,
           include_directories: incdir,
           install: true, install_dir: 'bin/')

##################################################
# Unit tests
##################################################

e = executable('test_misc',
               sources: ['tests/test_misc.cpp'] + srclist,
               include_directories: incdir,
#               dependencies: [arma, rmath, test])
               dependencies: [arma, test])
test('Test suite one', e)
