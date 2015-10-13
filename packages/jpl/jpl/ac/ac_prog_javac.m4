dnl Available from the GNU Autoconf Macro Archive at:
dnl http://www.gnu.org/software/ac-archive/htmldoc/ac_prog_javac.html
dnl
AC_DEFUN([AC_PROG_JAVAC],[
AC_REQUIRE([AC_EXEEXT])dnl
if test "x$JAVAC" = x; then
  if test "x$JAVAPREFIX" = x; then
    AC_CHECK_PROGS(JAVAC, jikes$EXEEXT javac$EXEEXT gcj$EXEEXT guavac$EXEEXT)
  else
    AC_PATH_PROGS(JAVAC, jikes$EXEEXT javac$EXEEXT gcj$EXEEXT guavac$EXEEXT, , "$JAVAPREFIX")
  fi
fi
test "x$JAVAC" = x && AC_MSG_ERROR([no acceptable Java compiler found in \$PATH])
if test "$JAVAC" = "gcj$EXEEXT"; then
  case "$JAVACFLAGS" in
    *-C*)
	;;
    *)
	JAVACFLAGS="$JAVACFLAGS -C"
	;;
  esac
fi
AC_PROG_JAVAC_WORKS
AC_PROVIDE([$0])dnl
])
