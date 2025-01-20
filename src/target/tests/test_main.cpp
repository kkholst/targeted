// tests_main.cpp
//
// In a doctest project with multiple files, dedicate one file to
// compile the source code of doctest itself and reuse the resulting
// object file for linking.

// Let doctest provide main():
#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <doctest/doctest.h>
