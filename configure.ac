AC_INIT([gnupaste], [0.1])
AC_CONFIG_SRCDIR([gnupaste.scm])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])

GUILE_PKG([2.2])
GUILE_PROGS
if test "x$GUILD" = "x"; then
   AC_MSG_ERROR(['guild' binary not found; please check your guile-2.x installation.])
fi

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([gnupaste-server], [chmod +x gnupaste-server])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])

AC_OUTPUT
