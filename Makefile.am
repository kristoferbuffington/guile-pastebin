bin_SCRIPTS =      \
  scripts/gnupaste

include guile.am

SOURCES =               \
  gnupaste/config.scm   \
  gnupaste/render.scm   \
  gnupaste/template.scm \
  gnupaste/paste.scm    \
  twitter/bootstrap.scm \
  gnupaste.scm

EXTRA_DIST =      \
  README          \
  bootstrap       \
  bin/gnupaste.in \
  pre-inst-env.in

AM_CFLAGS = -DLOCAL_STATE_DIR=\"${localstatedir}\"
gnupastedatadir = $(localstatedir)/gnupaste
gnupastedata_DATA = wt
