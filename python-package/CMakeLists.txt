cmake_minimum_required(VERSION 3.0)
set(target "targeted_c")

set(RUN_TESTS TRUE)
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_BUILD_WITH_INSTALL_RPATH ON)
 
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

message([Main] " ARMADILLO_LIBRARIES = ${ARMADILLO_LIBRARIES}")
message([Main] " ARMADILLO_INCLUDE_DIRS = ${ARMADILLO_INCLUDE_DIRS}")

message([Main] "PYTHON_LIBRARIES = ${PYTHON_LIBRARIES}")
message([Main] "PYTHON_EXECUTABLE = ${PYTHON_EXECUTABLE}")
message([Main] "PYTHON_INCLUDE_DIRS = ${PYTHON_INCLUDE_DIRS}")

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
  # set_target_properties(__${target}__ PROPERTIES MACOSX_RPATH TRUE)
  # set_target_properties(__${target}__ PROPERTIES CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)
  set_target_properties(__${target}__ PROPERTIES INSTALL_NAME_DIR "@rpath")
  set_target_properties(__${target}__ PROPERTIES INSTALL_RPATH "@loader_path")
else()
  set_target_properties(__${target}__ PROPERTIES INSTALL_RPATH "$ORIGIN")
  set_target_properties(__${target}__ PROPERTIES LINK_FLAGS "-shared")
endif()

target_link_libraries(__${target}__ PRIVATE armadillo)

#set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)
# if(APPLE)
#   set(CMAKE_INSTALL_RPATH "@executable_path")
#   set(CMAKE_INSTALL_NAME_DIR "@executable_path")
#   set(CMAKE_BUILD_WITH_INSTALL_RPATH ON)
# else()
#   set(CMAKE_BUILD_WITH_INSTALL_RPATH ON)
#   set(CMAKE_INSTALL_RPATH "$ORIGIN")
# endif()
