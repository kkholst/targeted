/*!
  @file util.h
  @author Klaus K. Holst
  @copyright 2018, Klaus Kähler Holst

  @brief Various utility functions and constants

*/

#ifndef SRC_UTIL_H_
#define SRC_UTIL_H_

// Foreground colors are in form of 3x, bacground are 4x
const char* COL_RESET = "\x1b[0m";
const char* COL_DEF   = "\x1b[39m";
const char* BLACK     = "\x1b[30m";
const char* RED       = "\x1b[31m";
const char* MAGENTA   = "\x1b[35m";
const char* YELLOW    = "\x1b[33m";
const char* GREEN     = "\x1b[32m";
const char* BLUE      = "\x1b[34m";
const char* CYAN      = "\x1b[36m";
const char* WHITE     = "\x1b[37m";
const char* GRAY      = "\x1b[90m";
const char* LRED      = "\x1b[91m";
const char* LGREEN    = "\x1b[92m";
const char* LYELLOW   = "\x1b[93m";
const char* LBLUE     = "\x1b[94m";
const char* LMAGENTA  = "\x1b[95m";
const char* LCYAN     = "\x1b[96m";
const char* LWHITE    = "\x1b[97m";

#endif  // SRC_UTIL_H_
