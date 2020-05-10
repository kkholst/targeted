################################################################################
# Copyright 2020 Klaus Holst (http://holst.it)
################################################################################
# CMake Properties:
#   COTIRE: Precompiled headers.
#   USE_PKG_LIB: Force use of libraries supplied with this package.
# Cmake specific settings:
#   CMAKE_BUILD_TYPE: Debug (non-optimized code with debug symbols), RelWithDebInfo
################################################################################
cmake_minimum_required(VERSION 3.3) # Was 2.8.12
project(target VERSION 0.1 DESCRIPTION "Targeted Inference Library" LANGUAGES CXX)

option(COTIRE  "precompiled headers" OFF)
option(USE_PKG_LIB  "Force use of libraries supplied with this package instead of system libraries" OFF)
option(CODE_COVERAGE "Enable coverage reporting" OFF)
option (BUILD_TESTING "Build the testing tree." ON)
option (USE_SANITIZER "Define CXX Sanitizer (Address, Undefined, Leak, Memory)" "")

set(CMAKE_VERBOSE_MAKEFILE ON)
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${PROJECT_SOURCE_DIR}/config")
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_program(CCACHE_FOUND ccache)
if(CCACHE_FOUND)
  set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE ccache)
  set_property(GLOBAL PROPERTY RULE_LAUNCH_LINK ccache)
endif(CCACHE_FOUND)

if(COTIRE)
  set(ENV{CCACHE_SLOPPINESS} "pch_defines,time_macros") ## For precompiled headers to work with ccache:
  include(cotire)
endif()

if (USE_SANITIZER STREQUAL "Address")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fsanitize=address")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsanitize=address")
elseif (USE_SANITIZER STREQUAL "Thread")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fsanitize=thread")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsanitize=thread")
elseif (USE_SANITIZER STREQUAL "Undefined")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fsanitize=undefined")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsanitize=undefined")
elseif (USE_SANITIZER STREQUAL "Leak")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fsanitize=leak")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsanitize=leak")
elseif (USE_SANITIZER STREQUAL "Memory")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fsanitize=memory")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsanitize=memory")
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
set(LIBTARGET_INC "${CMAKE_SOURCE_DIR}/include")
file(GLOB LIBTARGET_SRC ${LIBTARGET_DIR}/*.cpp)
file(GLOB LIBTARGET_HDR ${LIBTARGET_DIR}/*.hpp)
file(GLOB LIBTARGET_PUBLIC ${LIBTARGET_INC}/target/*.hpp)
add_library(target SHARED ${LIBTARGET_SRC} ${LIBTARGET_HDR} ${LIBTARGET_PUBLIC})
if(COTIRE)
  cotire(target) # precompile headers
endif()
message( [Main] " LIBTARGET_DIR = ${LIBTARGET_DIR}")
target_link_libraries(target PRIVATE armadillo)
set_target_properties(target PROPERTIES VERSION ${PROJECT_VERSION})
target_include_directories(target PUBLIC ${LIBTARGET_INC})
set_target_properties(target PROPERTIES PUBLIC_HEADER "${LIBTARGET_PUBLIC}")
include(GNUInstallDirs)
configure_file(config/target.pc.in target.pc @ONLY)

install(TARGETS target
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/target)
install(FILES ${CMAKE_BINARY_DIR}/target.pc
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/pkgconfig)

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


# Only build tests if we are the top-level project
# Allows this to be used by super projects with `add_subdirectory`
if (BUILD_TESTING AND (PROJECT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR))
  enable_testing()

  set(TESTS
  "tests/test_main.cpp"
  "tests/test_misc.cpp"
  "tests/test_mlogit.cpp"
  "tests/test_target.cpp"
  )
  add_executable("${PROJECT_NAME}_test" ${TESTS})

  find_program( MEMORYCHECK_COMMAND valgrind )
  set( MEMORYCHECK_COMMAND_OPTIONS "--trace-children=yes --leak-check=full" )
  include( CTest )
  add_test(NAME "${PROJECT_NAME}_test" COMMAND "${PROJECT_NAME}_test")

  target_link_libraries("${PROJECT_NAME}_test" PRIVATE armadillo Catch2::Catch2 target spdlog::spdlog)

  add_custom_target(test_memcheck
    COMMAND ${CMAKE_CTEST_COMMAND}
    --force-new-ctest-process --test-action memcheck
    COMMAND cat "${CMAKE_BINARY_DIR}/Testing/Temporary/MemoryChecker.*.log")

endif()

################################################################################
# Code coverage
################################################################################

if(CODE_COVERAGE AND CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
  message([Main] " Checking code coverage")
  include(CodeCoverage)
  add_library(coverage_config INTERFACE)
  append_coverage_compiler_flags()
  if(LCOV_PATH)
    setup_target_for_coverage_lcov(
      NAME coverage
      EXECUTABLE ${PROJECT_NAME}_test
      EXCLUDE "./lib/*" "${PROJECT_SOURCE_DIR}/lib/*")
  else()
    # Add required flags (GCC & LLVM/Clang)
    target_compile_options(coverage_config INTERFACE
      -O0        # no optimization
      -g         # generate debug info
      --coverage # sets all required flags
      )
    if(CMAKE_VERSION VERSION_GREATER_EQUAL 3.13)
      target_link_options(coverage_config INTERFACE --coverage)
    else()
      target_link_libraries(coverage_config INTERFACE --coverage)
    endif()
    #add_custom_target(coverage DEPENDS default)
  endif()
endif()

################################################################################

if (CMAKE_BUILD_TYPE STREQUAL "Debug")
  message([DEBUG])
endif()
