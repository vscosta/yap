
# nothing to see yet.

if test $indep_packages = true;
then
  PKG=$(basename $(pwd))
  AC_INIT(install-sh)
  AC_PREREQ([2.50])
  AC_CONFIG_HEADER(../$PKG/config.h)
fi
