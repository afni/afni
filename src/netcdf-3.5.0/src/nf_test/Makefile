# Makefile for netCDF (semi)exhaustive FORTRAN test.
#

include ../macros.make

# M4FLAGS	= -s -B7168
# CC		= cc -fullwarn -woff 1209,1506


INCLUDES	= 

lib_netcdf	= ../libsrc/libnetcdf.a
# Don't use "-L../libsrc -lnetcdf" in the following because that doesn't
# work on a CRAY T90 (sigh).
ld_netcdf	= $(lib_netcdf)

M4SRCS		= \
		  test_get.m4	\
		  test_put.m4
#

FSRCS		= \
		  $(M4SRCS:.m4=.F)	\
		  nf_error.F	\
		  nf_test.F	\
		  test_read.F	\
		  test_write.F	\
		  util.F
#

CSRCS		= fortlib.c

OBJS		= \
		  test_get.o	\
		  test_put.o	\
		  nf_error.o	\
		  nf_test.o	\
		  test_read.o	\
		  test_write.o	\
		  util.o	\
		  fortlib.o

time_log	= times

GARBAGE		= \
		  $(time_log)	\
		  lint.out	\
		  nf_test	\
		  scratch.nc	\
		  *.f77
#

PACKING_LIST	= \
		  $(CSRCS)	\
		  $(FSRCS)	\
		  $(M4SRCS)	\
		  depend	\
		  Makefile	\
	          tests.inc
#

all:		nf_test

test:		nf_test test.nc
	./nf_test
	@echo '*** Success ***'

readonly:	nf_test test.nc
	./nf_test -r

test.nc:  nf_test
	./nf_test -c

install:

uninstall:


nf_test:	$(OBJS) $(lib_netcdf)
	$(LINK.F) $(OBJS) $(ld_netcdf) $(FLIBS) $(MATHLIB) $(LIBS)


tags:		$(FSRCS) $(CSRCS) FORCE
	ctags -t $(FSRCS) $(CSRCS) ../fortran/*.c ../libsrc/*.c

include ../rules.make

test_get.F:	test_get.m4
test_put.F:	test_put.m4

include depend
