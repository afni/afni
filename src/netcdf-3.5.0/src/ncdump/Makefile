# Makefile for ncdump(1).
#

include ../macros.make

NCGEN		= ../ncgen/ncgen
INCLUDES	= -I../libsrc

c_sources	= ncdump.c vardata.c dumplib.c
headers		= ncdump.h vardata.h dumplib.h

PROGRAM		= ncdump

PACKING_LIST	= $(c_sources) $(headers) depend \
		  test0.cdl ncdump.1 Makefile
MANUAL		= ncdump.1

lib_netcdf	= ../libsrc/libnetcdf.a

# Don't use "-L../libsrc -lnetcdf" in the following because that doesn't
# work on a CRAY T90 (sigh).
ld_netcdf	= $(lib_netcdf)

OBJS		=  ncdump.o vardata.o dumplib.o
GARBAGE		= $(PROGRAM) test0.nc test1.nc test1.cdl test2.cdl

all:		$(PROGRAM)

$(PROGRAM):	$(lib_netcdf) $(OBJS)
	$(LINK.c) $(OBJS) $(ld_netcdf) $(LIBS) 

test:		$(PROGRAM) FORCE
	$(NCGEN) -b test0.cdl
	./$(PROGRAM) -n test1 test0.nc > test1.cdl
	$(NCGEN) -b test1.cdl
	./$(PROGRAM) test1.nc > test2.cdl
	@cmp test1.cdl test2.cdl && \
	     echo "*** $(PROGRAM) test successful ***"

install:	$(BINDIR)/$(PROGRAM) $(MANDIR)/man1/$(MANUAL)

uninstall:
	-rm -f $(BINDIR)/$(PROGRAM)
	-rm -f $(MANDIR)/man1/$(MANUAL)

$(PROGRAM)_oc : $(c_sources)
	#setopt primary_language C
	#load -C $(CPPFLAGS) $(c_sources)
	#load -C $(LIBS)
	#setopt program_name $(PROGRAM)

TAGS:		FORCE
	etags `echo $(PACKING_LIST) | fmt -1 | egrep '\.c|\.h'

include ../rules.make
include depend
