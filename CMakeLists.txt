################################################################################
# Copyright 2020 Klaus Holst (http://holst.it)
################################################################################
# CMake Properties:
#   NO_COTIRE: Disable precompiled headers.
#   USE_PKG_LIB: Force use of libraries supplied with this package instead of
#  	         system libraries.
# Cmake specific settings:
#   CMAKE_BUILD_TYPE: Debug (non-optimized code with debug symbols), RelWithDebInfo
#   CMAKE_VERBOSE_MAKEFILE: ON
################################################################################
cmake_minimum_required(VERSION 3.3) # Was 2.8.12
project("target")

set(CMAKE_VERBOSE_MAKEFILE ON)
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${PROJECT_SOURCE_DIR}/config")
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_program(CCACHE_FOUND ccache)
if(CCACHE_FOUND)
  set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE ccache)
  set_property(GLOBAL PROPERTY RULE_LAUNCH_LINK ccache)    
endif(CCACHE_FOUND)

if(NOT NO_COTIRE)
  set(ENV{CCACHE_SLOPPINESS} "pch_defines,time_macros") ## For precompiled headers to work with ccache:
  include(cotire)
endif()

################################################################################
# Libraries 
################################################################################

find_package(Armadillo 9.500 QUIET)
if (ARMADILLO_FOUND AND NOT USE_PKG_LIB)	
  message ([Main]  " Armadillo FOUND")
else()
  message ([Main]  " Using Armadillo included in this package")
  find_package(LAPACK REQUIRED)
  add_subdirectory("lib/armadillo")
  include_directories("lib/armadillo/include")
endif ()
message( [Main] " ARMADILLO_LIBRARIES = ${ARMADILLO_LIBRARIES}")
message( [Main] " ARMADILLO_INCLUDE_DIRS = ${ARMADILLO_INCLUDE_DIRS}")

find_package(Catch2 QUIET)
if (Catch2_FOUND AND NOT USE_PKG_LIB)	
  message ([Main]  " Catch2 FOUND")
else()
  message ([Main]  " Using Catch2 included in this package")
  add_subdirectory("lib/catch2")
  include_directories("lib/catch2/include")
endif()

find_package(spdlog QUIET)
if (spdlog_FOUND AND NOT USE_PKG_LIB)	
  message ([Main]  " spdlog FOUND")
 else()
  set(SPDLOG_BUILD_TESTS OFF)
  set(SPDLOG_BUILD_TESTS_HO OFF)   
  message ([Main]  " Using spdlog included in this package")
  add_subdirectory("lib/spdlog")
  include_directories("lib/spdlog/include")
endif()

################################################################################
# Main library
################################################################################

set(LIBTARGET_DIR "${CMAKE_SOURCE_DIR}/src")
file(GLOB LIBTARGET_SRC ${LIBTARGET_DIR}/*.cpp)
file(GLOB LIBTARGET_HDR ${LIBTARGET_DIR}/*.hpp)
include_directories(${LIBTARGET_DIR})
add_library(target ${LIBTARGET_SRC} ${LIBTARGET_HDR})
if(NOT NO_COTIRE)
  cotire(target) # precompile headers
endif()
message( [Main] " LIBTARGET_DIR = ${LIBTARGET_DIR}")
target_link_libraries(target PRIVATE armadillo)

################################################################################
# Executables
################################################################################

add_executable("crdemo" "misc/crdemo.cpp")
add_executable("dredemo" "misc/dredemo.cpp")
target_link_libraries("crdemo" PRIVATE armadillo target)
target_link_libraries("dredemo" PRIVATE armadillo target)

################################################################################
# Tests
################################################################################

set(TEST_DIR "tests")
set(TESTS
  "${TEST_DIR}/test_main.cpp"
  "${TEST_DIR}/test_misc.cpp"
  "${TEST_DIR}/test_mlogit.cpp"
  "${TEST_DIR}/test_target.cpp"  
  )

add_executable("${PROJECT_NAME}_test" ${TESTS})
target_link_libraries("${PROJECT_NAME}_test" PRIVATE armadillo Catch2::Catch2 target spdlog::spdlog)

find_program( MEMORYCHECK_COMMAND valgrind )
set( MEMORYCHECK_COMMAND_OPTIONS "--trace-children=yes --leak-check=full" )
include( CTest )
add_test(NAME "${PROJECT_NAME}_test" COMMAND "${PROJECT_NAME}_test")

add_custom_target(test_memcheck
    COMMAND ${CMAKE_CTEST_COMMAND} 
        --force-new-ctest-process --test-action memcheck
    COMMAND cat "${CMAKE_BINARY_DIR}/Testing/Temporary/MemoryChecker.*.log")

################################################################################
# Code coverage
################################################################################

if(COVERAGE_BUILD AND CMAKE_COMPILER_IS_GNUCXX)
  message([CODECOVERAGE])
  include(CodeCoverage)
  append_coverage_compiler_flags()
  setup_target_for_coverage_lcov(
          NAME coverage
          EXECUTABLE ${PROJECT_NAME}_test
          EXCLUDE "./lib/*" "${PROJECT_SOURCE_DIR}/lib/*")
endif()

################################################################################

if (CMAKE_BUILD_TYPE STREQUAL "Debug")
  message([DEBUG])
endif()



