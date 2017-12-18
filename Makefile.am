bin_SCRIPTS =      \
  scripts/pastebin

include guile.am

SOURCES =                 \
  pastebin/config.scm     \
  pastebin/controller.scm \
  pastebin/router.scm     \
  pastebin/render.scm     \
  pastebin/template.scm   \
  pastebin/paste.scm      \
  twitter/bootstrap.scm   \
  pastebin.scm

EXTRA_DIST =      \
  README          \
  bootstrap       \
  bin/pastebin.in \
  pre-inst-env.in

AM_CFLAGS = -DLOCAL_STATE_DIR=\"${localstatedir}\"
pastebindatadir = $(localstatedir)/pastebin
pastebindata_DATA = wt
