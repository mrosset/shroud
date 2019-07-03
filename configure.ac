dnl -*- Autoconf -*-

AC_INIT(Shroud, 0.1.1)
AC_CONFIG_SRCDIR(shroud)
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([color-tests -Wall -Wno-portability foreign])
AM_SILENT_RULES([yes])

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])
AC_CONFIG_FILES([scripts/shroud], [chmod +x scripts/shroud])
AC_CONFIG_FILES([shroud/config.scm])

GUILE_PKG([3.0 2.2 2.0])
GUILE_PROGS

AC_PATH_PROGS([GPG], [gpg gpg2])
AC_PATH_PROG([XCLIP], [xclip])

AC_OUTPUT
