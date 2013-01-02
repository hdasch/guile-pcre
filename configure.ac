AC_INIT([guile-pcre], [1.0], [hdasch@gmail.com])

AC_PREREQ([2.69])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])
LT_PREREQ([2.4.1])
AC_CONFIG_MACRO_DIR([m4])
AC_PROG_CC
AM_PROG_AR
LT_INIT([dlopen])

AC_CONFIG_FILES([src/Makefile])
PKG_CHECK_MODULES([GUILE], [guile-2.2])
GUILE_SITE_DIR
AX_PKG_CHECK_VARIABLE([GUILE_EXTENSIONDIR], [guile-2.2], extensiondir, [directory for GUILE extension installation])

AC_OUTPUT(Makefile)
