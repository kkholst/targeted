AC_INIT([multiclass], [1.0], [klaus@holst.it])
dnl CXX=`"${R_HOME}/bin/R" CMD config CXX`
dnl CXXFLAGS=`"${R_HOME}/bin/R" CMD config CXXFLAGS`
AC_LANG(C++)
AC_PROG_CC
AC_PROG_CXX
# C++11 check
m4_include([m4/ax_cxx_compile_stdcxx.m4])
AX_CXX_COMPILE_STDCXX([11], [noext], [mandatory])


AC_DEFUN([AC_PROG_PKGCONFIG], [AC_CHECK_PROG(PKGCONFIG,pkg-config,yes)])
AC_PROG_PKGCONFIG

if test x"${PKGCONFIG}" == x"yes" && pkg-config --atleast-version=2.1.0 mlpack; then
   EXTRA_CFLAGS=`pkg-config --cflags mlpack`
   EXTRA_LIBS=`pkg-config --libs mlpack`
fi

dnl # AC_CONFIG_MACRO_DIR([m4])
dnl AC_REQUIRE_CPP
dnl PKG_CHECK_MODULES([mlpack], [mlpack >= 2])
dnl AC_CHECK_LIB([mlpack], [], [], [AC_MSG_ERROR([ was not found in mlpack])])
AC_SUBST(EXTRA_CFLAGS)
AC_SUBST(EXTRA_LIBS)

AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT

echo "
compiler:	${CXX}
cflags:         ${EXTRA_CFLAGS}
libs:           ${EXTRA_LIBS}
pkgconfig:      ${PKGCONFIG}
"
