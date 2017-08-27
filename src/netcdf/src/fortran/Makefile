# Makefile for the FORTRAN interface to the netCDF library
#


include ../macros.make


INCLUDES 	= -I../libsrc
LIBRARY		= ../libsrc/libnetcdf.a
# Don't use "-L../libsrc -lnetcdf" in the following because that doesn't
# work on a CRAY T90 (sigh).
ld_netcdf	= $(LIBRARY)
HEADER		= netcdf.inc
LIB_OBJS	= \
		  fort-attio.o	\
		  fort-control.o	\
		  fort-dim.o	\
		  fort-genatt.o	\
		  fort-geninq.o	\
		  fort-genvar.o	\
		  fort-lib.o	\
		  fort-misc.o	\
		  fort-v2compat.o	\
		  fort-vario.o	\
		  fort-var1io.o	\
		  fort-varaio.o	\
		  fort-varmio.o	\
		  fort-varsio.o
#

MANUAL		= netcdf.3f

GARBAGE		= \
		  copy.nc	\
		  ftest		\
		  ftest.f77	\
		  test.nc
#

DIST_GARBAGE	= nfconfig.inc

PACKING_LIST	= \
		  cfortran.doc	\
		  cfortran.h	\
		  depend	\
		  fills.nc	\
		  fort-attio.c	\
		  fort-control.c	\
		  fort-dim.c	\
		  fort-genatt.c	\
		  fort-geninq.c	\
		  fort-genvar.c	\
		  fort-lib.c	\
		  fort-lib.h	\
		  fort-misc.c	\
		  fort-v2compat.c	\
		  fort-vario.c	\
		  fort-var1io.c	\
		  fort-varaio.c	\
		  fort-varmio.c	\
		  fort-varsio.c	\
		  ftest.F	\
		  Makefile	\
		  ncfortran.h	\
		  netcdf.3f	\
		  netcdf.inc	\
		  nfconfig.in
#


all:		$(LIBRARY) $(MANUAL)

test:		ftest FORCE
	./ftest


install:	$(INCDIR)/$(HEADER)	\
		$(MANDIR)/man3f/$(MANUAL)

uninstall:
	-rm -f $(INCDIR)/$(HEADER)
	-rm -f $(MANDIR)/man3f/$(MANUAL)
	-rm -f $(MANDIR)/man3/$(MANUAL)

ftest:		ftest.o $(LIBRARY)
	$(LINK.F) ftest.o $(ld_netcdf) $(LIBS)

# The rule for generating the manual page is here for completeness only.
# The manual page is actually part of the distribution so that we don't
# have to depend on the non-POSIX utility m4(1).
#
$(MANUAL):	../man/netcdf.m4
	$(M4) $(M4FLAGS) -DAPI=FORTRAN $? >$@ || rm $@


include ../rules.make
include depend

ftest.o:	ftest.F
ftest.o:	netcdf.inc
ftest.o:	nfconfig.inc
