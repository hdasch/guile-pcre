# PKG_CHECK_VARIABLE(VARIABLE, MODULE, MOD_VARIABLE, DESCRIPTION)
#
# Add facility to look up specific variables with pkg-config.
# Derived from PKG_CHECK_MODULES in pkg.m4 (part of pkg-config).
#
# --------------------------------------------------------------
AC_DEFUN([AX_PKG_CHECK_VARIABLE],
[AC_REQUIRE([PKG_PROG_PKG_CONFIG])dnl
AC_ARG_VAR([$1], [$4])dnl

pkg_failed=no
AC_MSG_CHECKING([for $1])

_PKG_CONFIG([$1], [variable=$3], [$2])

if test x$pkg_cv_$1 = x; then
   	AC_MSG_RESULT([no])
	$1[]_PKG_ERRORS="Variable $3 not defined for module $2."
	echo "$$1[]_PKG_ERRORS" >&AS_MESSAGE_LOG_FD
	m4_default([], [AC_MSG_ERROR(
[Package requirements ($2) were not met:

$$1_PKG_ERRORS

Consider adjusting the PKG_CONFIG_PATH environment variable if you
installed software in a non-standard prefix.])[]dnl
        ])
else
	$1[]=$pkg_cv_$1
        AC_MSG_RESULT([yes])
fi[]dnl
])# PKG_CHECK_VARIABLE
