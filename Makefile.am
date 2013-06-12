## Process this file with automake to produce Makefile.in
## Or better still run "autoreconf -i".

SUBDIRS = src doc
ACLOCAL_AMFLAGS = -I m4

.PHONY: check
check:
	GUILE_LOAD_PATH=. ./test.scm
