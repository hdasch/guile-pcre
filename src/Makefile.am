## Process this file with automake to produce Makefile.in
## Or better still run "autoreconf -i" in ..

extensiondir = $(GUILE_EXTENSIONDIR)
extension_LTLIBRARIES = libguile-pcre.la

libguile_pcre_la_SOURCES = guile-pcre.c
libguile_pcre_la_CFLAGS = $(GUILE_CFLAGS)
libguile_pcre_la_LDFLAGS = -lpcre $(GUILE_LIBS)

sitedir = $(GUILE_SITEDIR)
dist_site_SCRIPTS = guile-pcre.scm
