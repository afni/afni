## Process this file with automake to produce Makefile.in

INCLUDES = -I$(top_srcdir) -I$(top_srcdir)/src -I$(includedir)\
	 -DG_LOG_DOMAIN=\"Gts-test\"
LDADD = $(top_builddir)/src/libgts.la -lm
DEPS = $(top_builddir)/src/libgts.la

check_PROGRAMS = cartesian c1 c2 c3 double_prism

TESTS = flat.sh flat1.sh

EXTRA_DIST = flat.sh flat1.sh
