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

GUILE_MODULE_AVAILABLE([have_guile_wiredtiger], [(wiredtiger wiredtiger)])
AM_CONDITIONAL([HAVE_GUILE_WIREDTIGER], [test "x$have_guile_wired_tiger" = "xyes"])

AC_SUBST([localstatedir])
AC_SUBST([sysconfdir])
AC_SUBST([bindir])

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([gnupaste/config.scm])
AC_CONFIG_FILES([scripts/gnupaste], [chmod +x scripts/gnupaste])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])

AC_OUTPUT
