project('libtarget', 'cpp',
        version: '0.1',
        default_options: ['warning_level=3', 'cpp_std=c++11'])

arma = dependency('armadillo', version: '>=8.1')
rmath = dependency('libRmath', version: '>=3.0')
test = dependency('UnitTest++')
incdir = include_directories('src')

install_headers('src/targeted.h', subdir: 'src')
srclist = ['src/targeted.cpp', 'src/glm.cpp']
targetedlib = shared_library('targeted',
                             sources: srclist,
                             dependencies: [arma, rmath],
                             include_directories: incdir,
                             install: true)

##################################################
# Executables
##################################################

executable('dredemo',
           sources: ['src/dredemo.cpp'],
           dependencies: [arma, rmath],
           link_with: targetedlib,
           include_directories: incdir,
           install: true, install_dir: 'bin/')

##################################################
# Unit tests
##################################################

e = executable('test_misc',
               sources: ['tests/test_misc.cpp'] + srclist,
               include_directories: incdir,           
               dependencies: [arma, rmath, test])
test('Test suite one', e)
