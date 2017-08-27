# Makefile for netCDF (semi)exhaustive test.
#

include ../macros.make
# CC = cc -fullwarn -woff 1498,1692


INCLUDES	= -I../libsrc
SRCS		= varget.c vargetg.c varput.c varputg.c vardef.c vartests.c \
		  vputget.c vputgetg.c driver.c cdftests.c dimtests.c rec.c \
		  atttests.c misctest.c add.c error.c emalloc.c val.c slabs.c
OBJS		= $(SRCS:.c=.o)
lib_netcdf	= ../libsrc/libnetcdf.a
ld_netcdf	= -L../libsrc -lnetcdf
time_log	= times
GARBAGE		= nctest lint.out test.nc test2.nc $(time_log)
PACKING_LIST	= \
		    add.c	\
		    add.h	\
		    atttests.c	\
		    cdftests.c	\
		    depend	\
		    dimtests.c	\
		    driver.c	\
		    emalloc.c	\
		    emalloc.h	\
		    error.c	\
		    error.h	\
		    Makefile	\
		    misctest.c	\
		    nctime.c	\
		    README	\
		    rec.c	\
		    slabs.c	\
		    testcdf.h	\
		    testfile_nc.sav	\
		    tests.h	\
		    timesum.awk	\
		    val.c	\
		    val.h	\
		    vardef.c	\
		    varget.c	\
		    vargetg.c	\
		    varput.c	\
		    varputg.c	\
		    vartests.c	\
		    vputget.c	\
		    vputgetg.c


all:		nctest

test:		nctest
	./nctest
	cmp testfile.nc testfile_nc.sav

install:

uninstall:


nctest:		$(OBJS) $(lib_netcdf)
	$(LINK.c) $(OBJS) $(ld_netcdf) $(LIBS) $(ld_math)

nctime:		nctime.o $(lib_netcdf)
	$(LINK.c) $(CFLAGS) nctime.o $(ld_netcdf) $(LIBS) 

time:	nctime
	time ./nctime 24 13 19 17 > $(time_log)
	awk -f timesum.awk < $(time_log)

saber_src:
	#load -C $(CPPFLAGS) $(SRCS) $(ld_netcdf) $(LIBS)

include ../rules.make
include depend
