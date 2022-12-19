cmake_minimum_required(VERSION 3.16)
set(pkg "targeted_template")
set(target "${pkg}_c")

set(RUN_TESTS TRUE)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
project(${target} CXX)

################################################################################

find_program(CCACHE_FOUND ccache)
if(CCACHE_FOUND)
    message(STATUS "[Main] Using ccache")
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

################################################################################
# Libraries
################################################################################

# find Python
FIND_PACKAGE(PythonInterp REQUIRED 3)
FIND_PACKAGE(PythonLibs REQUIRED 3)

message ([Main]  " Using Armadillo included in this package")
set(DETECT_HDF5 OFF CACHE BOOL "Disable Armadillo HDF5 support")
add_subdirectory(lib/armadillo)
add_subdirectory(lib/eigen)

################################################################################
# Tests
################################################################################

set(SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/lib/target")
set(TEST_DIR "tests")
set(TESTS
  "${TEST_DIR}/test_main.cpp"
  "${TEST_DIR}/test_c.cpp")

if (RUN_TESTS)
  include_directories("lib/doctest")
  add_executable("${PROJECT_NAME}_test" ${TESTS})
  target_link_libraries("${PROJECT_NAME}_test" PRIVATE armadillo)
endif()

################################################################################
# Python bindings
################################################################################

find_package(pybind11 QUIET)
if (pybind11_FOUND)
  message([Main]  " pybind11 FOUND")
else()
  message([Main]  " Using pybind11 included in this package")
  add_subdirectory(lib/pybind11)
endif()

set(LIBTARGET_CPP "${CMAKE_SOURCE_DIR}/lib/target-cpp")
set(LIBTARGET_INC "${CMAKE_SOURCE_DIR}/lib/target-inc")
file(GLOB LIBTARGET_SRC ${LIBTARGET_CPP}/*.*pp)
file(GLOB LIBTARGET_HDR ${LIBTARGET_INC}/*.hpp)
message( [Main] " LIBTARGET_CPP = ${LIBTARGET_CPP}")
message( [Main] " LIBTARGET_INC = ${LIBTARGET_INC}")
include_directories(${LIBTARGET_INC} LIBTARGET)

file(GLOB PKG_SRC src/${pkg}/*.cpp)

pybind11_add_module(__${target}__
  ${LIBTARGET_CPP}/utils.cpp
  ${LIBTARGET_CPP}/glm.cpp
  # ${LIBTARGET_SRC}
  ${LIBTARGET_HDR}
  ${PKG_SRC}
  #src/${pkg}/interface.cpp
 )

if(APPLE)
  set_target_properties(__${target}__ PROPERTIES INSTALL_NAME_DIR "@rpath")
  set_target_properties(__${target}__ PROPERTIES INSTALL_RPATH "@loader_path")
else()
  set_target_properties(__${target}__ PROPERTIES INSTALL_RPATH "$ORIGIN;.")
  set_target_properties(__${target}__ PROPERTIES LINK_FLAGS "-shared")
  #set_target_properties(__${target}__ PROPERTIES LINK_FLAGS "-Wl,-rpath,$ORIGIN")
endif()

target_link_libraries(__${target}__ PRIVATE armadillo eigen)
