dnl Available from the GNU Autoconf Macro Archive at:
dnl http://www.gnu.org/software/ac-archive/htmldoc/ac_prog_javadoc.html
dnl
AC_DEFUN([AC_PROG_JAVADOC],[
AC_REQUIRE([AC_EXEEXT])dnl
if test "x$JAVADOC" = x; then
  if test "x$JAVAPREFIX" = x; then
    AC_CHECK_PROGS(JAVADOC, javadoc$EXEEXT)
  else
    AC_PATH_PROGS(JAVADOC, javadoc$EXEEXT, , "$JAVAPREFIX")
  fi
fi
test "x$JAVADOC" = x && AC_MSG_ERROR([no acceptable javadoc generator found in \$PATH])
AC_PROVIDE([$0])dnl
])

