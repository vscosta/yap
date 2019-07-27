dnl Available from the GNU Autoconf Macro Archive at:
dnl http://www.gnu.org/software/ac-archive/htmldoc/ac_prog_java.html
dnl
AC_DEFUN([AC_PROG_JAVA],[
AC_REQUIRE([AC_EXEEXT])dnl
if test "x$JAVA" = x; then
  if test "x$JAVAPREFIX" = x; then
    AC_CHECK_PROGS(JAVA, kaffe$EXEEXT java$EXEEXT)
  else
    AC_PATH_PROGS(JAVA, kaffe$EXEEXT java$EXEEXT, , $JAVAPREFIX)
  fi
fi
test "x$JAVA" = x && AC_MSG_ERROR([no acceptable Java virtual machine found in \$PATH])
AC_PROG_JAVA_WORKS
AC_PROVIDE([$0])dnl
])
