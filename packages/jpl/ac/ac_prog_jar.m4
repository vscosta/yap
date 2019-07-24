dnl Available from the GNU Autoconf Macro Archive at:
dnl http://www.gnu.org/software/ac-archive/htmldoc/ac_prog_jar.html
dnl
AC_DEFUN([AC_PROG_JAR],[
AC_REQUIRE([AC_EXEEXT])dnl
if test "x$JAR" = x; then
  if test "x$JAVAPREFIX" = x; then
    AC_CHECK_PROGS(JAR, jar$EXEEXT)
  else
    AC_PATH_PROGS(JAR, jar$EXEEXT, , $JAVAPREFIX)
  fi
fi
test "x$JAR" = x && AC_MSG_ERROR([no acceptable jar program found in \$PATH])
AC_PROVIDE([$0])dnl
])
