#
#	Makefile for netcdf libsrc
#

include ../macros.make

INCLUDES = -I.

LIBRARY 	= libnetcdf.a
ld_netcdf	= -L. -lnetcdf

HEADER	= netcdf.h

MANUAL	= netcdf.3

LIB_CSRCS = \
	attr.c \
	dim.c \
	error.c \
	libvers.c \
	nc.c \
	ncio.c \
	ncx.c \
	putget.c \
	string.c \
	v1hpg.c \
	v2i.c \
	var.c

PACKING_LIST = \
	$(LIB_CSRCS) \
	attr.m4 \
	depend \
	fbits.h \
	Makefile \
	nc.h \
	ncconfig.in \
	ncio.h \
	ncx.m4 \
	onstack.h \
	putget.m4 \
	ffio.c \
	posixio.c \
	ncx.h \
	ncx_cray.c \
	netcdf.3 \
	netcdf.h \
	rnd.h \
	test_nc.sav \
	t_nc.c \
	t_ncio.c \
	t_ncxx.m4 \
	t_ncxx.c \
	t_ncx.c

LIB_OBJS = $(LIB_CSRCS:.c=.o)

GARBAGE		= t_ncio.o t_ncio t_ncx.o t_ncx t_ncxx.o t_ncxx \
	t_nc.o t_nc test.nc *.so

DIST_GARBAGE	= ncconfig.h


all:		$(LIBRARY)

check:		full_test 

full_test:	test_ncx test nctest

install:	$(LIBDIR)/$(LIBRARY)	\
		$(INCDIR)/$(HEADER)	\
		$(MANDIR)/man3/$(MANUAL)

uninstall:
	-rm -f $(LIBDIR)/$(LIBRARY)
	-rm -f $(INCDIR)/$(HEADER)
	-rm -f $(MANDIR)/man3/$(MANUAL)


libvers.o:	../VERSION libvers.c
	$(COMPILE.c) -DVERSION=`cat ../VERSION` libvers.c

test_ncio:	t_ncio
	time ./t_ncio -s 16384 -c /tmp/test.io < t_ncio.in
	time ./t_ncio -s 16384 -w -S /tmp/test.io < t_ncio.in
	-rm /tmp/test.io

test_ncx:	t_ncx t_ncxx
	./t_ncx
	./t_ncxx

test:	t_nc
	./t_nc
	cmp test.nc test_nc.sav
	@echo '*** Success ***'

nctest:		$(LIBRARY)
	(cd ../nctest ; make test)

nc_test:	$(LIBRARY)
	(cd ../nc_test ; make test)


libvers:	../VERSION libvers.c
	$(LINK.c) -DVERSION=`cat ../VERSION` -DMAKE_PROGRAM libvers.c

t_ncio:		ncio.o t_ncio.o
	$(LINK.c) t_ncio.o ncio.o $(LIBS)

t_ncx:		t_ncx.o ncx.o
	$(LINK.c) t_ncx.o ncx.o $(LIBS)

t_ncxx:		t_ncxx.o ncx.o
	$(LINK.c) t_ncxx.o ncx.o $(LIBS)


t_nc:		t_nc.o $(LIBRARY)
	$(LINK.c) t_nc.o $(ld_netcdf) $(LIBS)

saber_src:
	#load -C $(CPPFLAGS) $(LIB_CSRCS)

tags:		FORCE
	ctags -t *.c *.h

# The rule for generating the manual page is here for completeness only.
# The manual page is actually part of the distribution so that we don't
# have to depend on the non-POSIX utility m4(1).
#
$(MANUAL):	../man/netcdf.m4
	$(M4) $(M4FLAGS) -DAPI=C $? >$@  || rm $@

include ../rules.make

.SUFFIXES: .ln
LINT = lint
LINT.c = $(LINT) $(LINTFLAGS) $(CPPFLAGS)
.c.ln:
	$(LINT.c) -c $<

llib-lnetcdf.ln: $(LIB_CSRCS)
	$(LINT.c) $(LIB_CSRCS) -y -o netcdf

lint: llib-lnetcdf.ln
	$(LINT.c) t_nc.c llib-lnetcdf.ln

attr.c:	attr.m4
ncx.c:	ncx.m4
putget.c: putget.m4
t_ncxx.c: t_ncxx.m4
# Ensure that the I/O module depends on ALL implementation files.
ncio.o:		ffio.c posixio.c

include depend
