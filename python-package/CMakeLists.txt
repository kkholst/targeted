cmake_minimum_required(VERSION 2.8.12)
set(target "targeted_c")

set(RUN_TESTS FALSE)
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_BUILD_WITH_INSTALL_RPATH ON)

if (CMAKE_VERSION VERSION_LESS 3.0)
  project(${target})
  set(PROJECT_VERSION 0.1)
else()
  cmake_policy(SET CMP0048 NEW)
  project(${target})
endif()

################################################################################
# Libraries
################################################################################

find_program(CCACHE_FOUND ccache)
if(CCACHE_FOUND)
    message(STATUS "******************** Using ccache")
    set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE ccache)
    set_property(GLOBAL PROPERTY RULE_LAUNCH_LINK ccache)
endif(CCACHE_FOUND)

## Gold linker
if (UNIX AND NOT APPLE)
  execute_process(COMMAND ${CMAKE_C_COMPILER} -fuse-ld=gold -Wl,--version ERROR_QUIET OUTPUT_VARIABLE ld_version)
  if ("${ld_version}" MATCHES "GNU gold")
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -fuse-ld=gold -Wl,--disable-new-dtags")
    set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -fuse-ld=gold -Wl,--disable-new-dtags")
  endif()
endif()


# find Python
FIND_PACKAGE(PythonInterp REQUIRED)

# Get Python version
EXECUTE_PROCESS(COMMAND ${PYTHON_EXECUTABLE} --version OUTPUT_VARIABLE PYTHON_VERSION_RAW ERROR_VARIABLE PYTHON_VERSION_RAW)
STRING(REPLACE "\n" "" PYTHON_VERSION_RAW "${PYTHON_VERSION_RAW}")
STRING(REGEX MATCH "[0-9]\\.[0-9]" PYTHON_VERSION "${PYTHON_VERSION_RAW}")
STRING(REGEX MATCH "[0-9]\\.[0-9]\\.[0-9]" PYTHON_VERSION_WITH_PATCH "${PYTHON_VERSION_RAW}")

# Unset the previous values of the CMake cache variables related to Python libraries
# in case the value of PYTHON_EXECUTABLE CMake variable changed
UNSET(PYTHONLIBS_FOUND CACHE)
UNSET(PYTHON_LIBRARY CACHE)
UNSET(PYTHON_INCLUDE_DIR CACHE)
UNSET(PYTHON_INCLUDE_PATH CACHE)

# Find the Python library with the same version as the interpreter
# Python 3.2 library is suffixed by mu and Python >= 3.3 by m on some systems, also handle these cases
SET(Python_ADDITIONAL_VERSIONS ${PYTHON_VERSION}mu ${PYTHON_VERSION}m ${PYTHON_VERSION})

GET_FILENAME_COMPONENT(PYTHON_HOME_PATH ${PYTHON_EXECUTABLE} PATH)

# Ensure that correct Python include path and library are selected by CMake on Linux (in case of non standard installation)
SET(CMAKE_INCLUDE_PATH ${PYTHON_HOME_PATH}/../include ${CMAKE_INCLUDE_PATH})
SET(CMAKE_LIBRARY_PATH ${PYTHON_HOME_PATH}/../lib ${CMAKE_LIBRARY_PATH})

# Find Python includes and library
FIND_PACKAGE(PythonLibs REQUIRED)
  
##find_package(PythonLibs QUIET)
include_directories(${PYTHON_INCLUDE_DIRS})
message(STATUS "PYTHON_LIBRARIES = ${PYTHON_LIBRARIES}")
message(STATUS "PYTHON_EXECUTABLE = ${PYTHON_EXECUTABLE}")
message(STATUS "PYTHON_INCLUDE_DIRS = ${PYTHON_INCLUDE_DIRS}")

find_package(Armadillo 9.500 QUIET)
if (ARMADILLO_FOUND)	
  message ([Main]  " Armadillo FOUND")
else()
  message ([Main]  " Using Armadillo included in this package")
  find_package(LAPACK REQUIRED)
  set(DETECT_HDF5 OFF CACHE BOOL "Disable Armadillo HDF5 support")
  add_subdirectory(src/armadillo)
  include_directories(src/armadillo/include)
endif ()
message( [Main] " ARMADILLO_LIBRARIES = ${ARMADILLO_LIBRARIES}")
message( [Main] " ARMADILLO_INCLUDE_DIRS = ${ARMADILLO_INCLUDE_DIRS}")

################################################################################
# Tests
################################################################################

set(SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/src/target")
set(TEST_DIR "tests")
set(TESTS
  "${TEST_DIR}/test_main.cpp"
  "${TEST_DIR}/test_c.cpp")

if (RUN_TESTS) 
  find_package(Catch2 QUIET)
  if (Catch2_FOUND)	
    message ([Main]  " Catch2 FOUND")
  else()
    message ([Main]  " Using Catch2 included in this package")
    add_subdirectory("src/catch2")
    include_directories("src/catch2/include")
  endif()
  add_executable("${PROJECT_NAME}_test" ${TESTS})
  target_link_libraries("${PROJECT_NAME}_test" PRIVATE armadillo Catch2::Catch2)  
endif()

################################################################################
# Python bindings
################################################################################

find_package(pybind11 QUIET)
if (pybind11_FOUND)	
  message([Main]  " pybind11 FOUND")
else()
  message([Main]  " Using pybind11 included in this package")
  add_subdirectory(src/pybind11)
endif()

set(LIBTARGET_DIR "${CMAKE_SOURCE_DIR}/src/target-cpp")
file(GLOB LIBTARGET_SRC ${LIBTARGET_DIR}/*.cpp)
file(GLOB LIBTARGET_HDR ${LIBTARGET_DIR}/*.hpp)
message( [Main] " LIBTARGET_DIR = ${LIBTARGET_DIR}")
include_directories(${LIBTARGET_DIR} LIBTARGET)

pybind11_add_module(__${target}__
  ${LIBTARGET_SRC} ${LIBTARGET_HDR}
  src/targeted/interface.cpp
 )

if(APPLE)
  set_target_properties(__${target}__ PROPERTIES INSTALL_NAME_DIR "@rpath")
  set_target_properties(__${target}__ PROPERTIES INSTALL_RPATH "@loader_path")
else()
  set_target_properties(__${target}__ PROPERTIES INSTALL_RPATH "$ORIGIN")
  set_target_properties(__${target}__ PROPERTIES LINK_FLAGS "-shared")
endif()

target_link_libraries(__${target}__ PRIVATE armadillo)


