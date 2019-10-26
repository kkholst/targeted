cmake_minimum_required(VERSION 2.8.12)
project("target")

#set(CMAKE_VERBOSE_MAKEFILE ON)
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_program(CCACHE_FOUND ccache)
if(CCACHE_FOUND)
    set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE ccache)
    set_property(GLOBAL PROPERTY RULE_LAUNCH_LINK ccache)
endif(CCACHE_FOUND)

find_package(Armadillo 9.500 QUIET)
if (ARMADILLO_FOUND)	
  MESSAGE ([Main]  " Armadillo FOUND")
else()
  MESSAGE ([Main]  " Using Armadillo included in this package")
  find_package(LAPACK REQUIRED)
  add_subdirectory("lib/armadillo")
  include_directories("lib/armadillo")
endif ()
MESSAGE( [Main] " ARMADILLO_LIBRARIES = ${ARMADILLO_LIBRARIES}")
MESSAGE( [Main] " ARMADILLO_INCLUDE_DIRS = ${ARMADILLO_INCLUDE_DIRS}")

find_package(UnitTest++ QUIET)
if (UnitTest++_FOUND)	
  MESSAGE ([Main]  " UnitTest++ FOUND")
else()
  MESSAGE ([Main]  " Using UnitTest++ included in this package")
  set(UTPP_INCLUDE_TESTS_IN_BUILD OFF CACHE BOOL "unit test building disabled" FORCE)  
  add_subdirectory("lib/unittest-cpp")
  include_directories("lib/unittest-cpp")
endif()

find_package(spdlog QUIET)
if (spdlog_FOUND)	
  MESSAGE ([Main]  " spdlog FOUND")
else()
  MESSAGE ([Main]  " Using spdlog included in this package")
  add_subdirectory("lib/spdlog")
endif()


################################################################################

SET(LIBTARGET_DIR "${CMAKE_SOURCE_DIR}/src")
FILE(GLOB LIBTARGET_SRC ${LIBTARGET_DIR}/*.cpp)
FILE(GLOB LIBTARGET_HDR ${LIBTARGET_DIR}/*.h*)
include_directories(${LIBTARGET_DIR} LIBTARGET)
add_library(target ${LIBTARGET_SRC} ${LIBTARGET_HDR})
MESSAGE( [Main] " LIBTARGET_DIR = ${LIBTARGET_DIR}")
target_link_libraries(target PRIVATE armadillo)

################################################################################
# Executables
################################################################################

add_executable("dredemo" "examples/dredemo.cpp")
target_link_libraries("dredemo" PRIVATE armadillo target)

################################################################################
# Tests
################################################################################

SET(SOURCE_DIR "target")
SET(TEST_DIR "tests")
SET(TESTS
  #"${SOURCE_DIR}/lib.cpp" 
  "${TEST_DIR}/test_misc.cpp")
    # "${TEST_DIR}/test_main.cpp"
    # "${TEST_DIR}/test_math.cpp")

add_executable("${PROJECT_NAME}_test" ${TESTS})
target_link_libraries("${PROJECT_NAME}_test" PRIVATE armadillo UnitTest++ target spdlog)

find_program( MEMORYCHECK_COMMAND valgrind )
set( MEMORYCHECK_COMMAND_OPTIONS "--trace-children=yes --leak-check=full" )
include( CTest )
add_test(NAME "${PROJECT_NAME}_test" COMMAND "${PROJECT_NAME}_test")

add_custom_target(test_memcheck
    COMMAND ${CMAKE_CTEST_COMMAND} 
        --force-new-ctest-process --test-action memcheck
    COMMAND cat "${CMAKE_BINARY_DIR}/Testing/Temporary/MemoryChecker.*.log")


