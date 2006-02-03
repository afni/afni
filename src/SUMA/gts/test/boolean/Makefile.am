## Process this file with automake to produce Makefile.in

INCLUDES = -I$(top_srcdir) -I$(top_srcdir)/src -I$(includedir)\
	 -DG_LOG_DOMAIN=\"Gts-test\"
LDADD = $(top_builddir)/src/libgts.la -lm
DEPS = $(top_builddir)/src/libgts.la

check_PROGRAMS = set boole cubes

TESTS = test.sh

EXTRA_DIST = $(TESTS) tests surfaces
