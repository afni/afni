## Process this file with automake to produce Makefile.in

INCLUDES = -I$(top_srcdir) -I$(top_srcdir)/src -I$(includedir) \
	 -DG_LOG_DOMAIN=\"Gts-test\"
LDADD = $(top_builddir)/src/libgts.la -lm
DEPS = $(top_builddir)/src/libgts.la

check_PROGRAMS = cartesian random

TESTS = cartesian.sh two_segments.sh too_close.sh

EXTRA_DIST = \
	cartesian.sh \
	cartesian_speed.sh \
	random_speed.sh \
	two_segments.gts two_segments.sh \
	too_close.gts too_close.sh
