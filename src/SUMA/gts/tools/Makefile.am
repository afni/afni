## Process this file with automake to produce Makefile.in

INCLUDES = -I$(top_srcdir) -I$(top_srcdir)/src -I$(includedir)\
	 -DG_LOG_DOMAIN=\"Gts-tools\"
LDADD = $(top_builddir)/src/libgts.la -lm
DEPS = $(top_builddir)/src/libgts.la

bin_PROGRAMS = gts2oogl gtscompare gtscheck stl2gts gts2dxf gts2stl
bin_SCRIPTS = gtstemplate

EXTRA_DIST = gtstemplate
