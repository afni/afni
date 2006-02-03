## Process this file with automake to produce Makefile.in

INCLUDES = -I$(top_srcdir) -I$(top_srcdir)/src -I$(includedir)\
	 -DG_LOG_DOMAIN=\"Gts-examples\"
LDADD = $(top_builddir)/src/libgts.la -lm
DEPS = $(top_builddir)/src/libgts.la

EXTRA_PROGRAMS = happrox

happrox_LDADD = $(LDADD) -lnetpbm

if HAS_NETPBM
NETPBM_EXTRA = happrox
endif

bin_PROGRAMS = \
	transform \
	delaunay \
	$(NETPBM_EXTRA)

noinst_PROGRAMS = \
	set \
	volume \
	cleanup \
	coarsen \
	refine \
	iso \
	gtstoc \
	oocs \
	partition \
	traverse \
	cartesian \
	split \
	smooth \
	sphere \
	stripe \
	optimize \
	merge
