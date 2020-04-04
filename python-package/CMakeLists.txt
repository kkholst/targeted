cmake_minimum_required(VERSION 2.8.12)
set(target "targeted_c")

set(RUN_TESTS TRUE)   
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)
#set(CMAKE_BUILD_WITH_INSTALL_RPATH ON)

if (CMAKE_VERSION VERSION_LESS 3.0)
  project(${target})
  set(PROJECT_VERSION 0.0.7)
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

find_package(PythonLibs QUIET)
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
  set(DETECT_HDF5 false)
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

target_link_libraries(__${target}__ PRIVATE armadillo)
 
if(APPLE)
#  set_target_properties(__${target}__ PROPERTIES INSTALL_NAME_DIR "@rpath")
#  set_target_properties(__${target}__ PROPERTIES INSTALL_RPATH "@loader_path")
else()
# set_target_properties(__${target}__ PROPERTIES INSTALL_RPATH "$ORIGIN")
# set_target_properties(${target} PROPERTIES LINK_FLAGS "-shared")
endif()




