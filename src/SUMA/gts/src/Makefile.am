## Process this file with automake to produce Makefile.in

INCLUDES = -I$(top_srcdir) -I$(includedir) -DG_LOG_DOMAIN=\"Gts\"

bin_SCRIPTS=gts-config

BUILT_SOURCES= \
	gts-config \
	predicates_init.h

gts-config: gts-config.in

lib_LTLIBRARIES = libgts.la

libgts_la_LDFLAGS = -version-info $(LT_CURRENT):$(LT_REVISION):$(LT_AGE)\
	-release $(LT_RELEASE) -export-dynamic

libgts_la_DEPENDENCIES = \
	predicates_init.h

libgts_la_SOURCES = \
	predicates_init.h \
	object.c \
	point.c \
	vertex.c \
	segment.c \
	edge.c \
	triangle.c \
	face.c \
	kdtree.c \
	bbtree.c \
	misc.c \
	gts.h \
	gts-private.h \
	predicates.c \
	predicates.h \
	rounding.h \
	heap.c \
	eheap.c \
	fifo.c \
	matrix.c \
	surface.c \
	stripe.c \
	vopt.c \
	refine.c \
	iso.c \
	isotetra.c \
	split.c \
	psurface.c \
	hsurface.c \
	cdt.c \
	boolean.c \
	named.c \
	oocs.c \
	container.c \
	graph.c \
	pgraph.c \
	partition.c \
	curvature.c

include_HEADERS = \
	gts.h gtsconfig.h

predicates.o: predicates.c predicates_init.h predicates.h
	$(COMPILE) -c $(srcdir)/predicates.c

predicates_init: predicates_init.c rounding.h
	$(COMPILE) $(srcdir)/predicates_init.c -o $(srcdir)/predicates_init

predicates_init.h: predicates_init
	./predicates_init >  $(srcdir)/predicates_init.h

CLEANFILES = $(BUILT_SOURCES)

gts.def: libgts.la .libs/libgts.a
	nm -g .libs/libgts.a | awk 'BEGIN{print "EXPORTS"}{if ($$2 == "T" || $$2 == "D" || $$2 == "B") print "    " $$3}' > gts.def

EXTRA_DIST = \
	predicates_init.c \
	gts.m4 \
	makefile.msc \
	config.h.win32 \
	gts.def \
	NOTES

dist-hook: gts.def
	cd $(distdir); rm -f $(BUILT_SOURCES)

m4datadir = $(datadir)/aclocal
m4data_DATA = gts.m4
