AC_INIT("targeted")
AC_LANG(C++)
AC_REQUIRE_CPP
AC_PROG_CC
AC_PROG_CXX

AC_CHECK_FILE(../../src/target.cpp,
        [PKG_ROOT=../../..],
        [PKG_ROOT=.])
AC_SUBST(PKG_ROOT)


PKG_SOURCES=$(cd src; echo *.cpp)
PKG_SOURCES2=$(cat src/dependencies | sed 's/$/.cpp/')
AS_IF([test "$PKG_ROOT" != "."],
[
PKG_SOURCES2=$(echo "${PKG_SOURCES2}" | sed 's/^/..\/..\/..\/src\//' | tr '\n' ' ')
PKG_SOURCES=$(echo "${PKG_SOURCES2} ${PKG_SOURCES}")
])
AC_SUBST(PKG_SOURCES)

AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT

echo "
 --------------------------------------------------
  Configuration for ${PACKAGE_NAME}
  ================

    cppflags: ${CPPFLAGS}
    cxxflags: ${CXXFLAGS}
    libs:     ${PKG_LIBS}
    sources: ${PKG_SOURCES}

  --------------------------------------------------
"
