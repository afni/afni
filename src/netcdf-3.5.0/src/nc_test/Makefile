# Makefile for netCDF (semi)exhaustive test.
#

include ../macros.make
# M4FLAGS         = -s -B7168
# CC		= cc -fullwarn -woff 1209,1506


INCLUDES	= -I../libsrc

ld_math		= $(MATHLIB)

SRCS		=   nc_test.c \
		    error.c \
		    test_get.c \
		    test_put.c \
		    test_read.c \
		    test_write.c \
		    util.c

OBJS		= $(SRCS:.c=.o)

lib_netcdf	= ../libsrc/libnetcdf.a
ld_netcdf	= -L../libsrc -lnetcdf

time_log	= times

GARBAGE		= nc_test test.nc scratch.nc lint.out $(time_log)

PACKING_LIST	=  $(SRCS) \
		    test_get.m4 \
		    test_put.m4 \
		    error.h \
	            tests.h \
		    depend	\
		    Makefile

all:		nc_test

test:		nc_test test.nc
	./nc_test
	@echo '*** Success ***'

readonly:	nc_test test.nc
	./nc_test -r

test.nc:  nc_test
	./nc_test -c

install:

uninstall:


nc_test:		$(OBJS) $(lib_netcdf)
	$(LINK.c) $(OBJS) $(ld_netcdf) $(ld_math) $(LIBS)

test_get.c:	test_get.m4

test_put.c:	test_put.m4

nctime:		nctime.o $(lib_netcdf)
	$(LINK.c) nctime.o $(ld_netcdf) $(ld_math) $(LIBS) 

time:	nctime
	time ./nctime 24 13 19 17 > $(time_log)
	awk -f timesum.awk < $(time_log)

saber_src:
	#load -C $(CPPFLAGS) $(SRCS) $(ld_netcdf) $(ld_math) $(LIBS)


include ../rules.make
include depend
