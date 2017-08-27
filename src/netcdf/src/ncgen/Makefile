# Makefile for ncgen(1).
#

include ../macros.make

NCDUMP		= ../ncdump/ncdump
PROGRAM		= ncgen
INCLUDES	= -I../libsrc -I.
MANUAL		= ncgen.1
GARBAGE		= $(PROGRAM) \
		  netcdf.inc \
		  c0.nc c1.cdl c1.nc c2.cdl \
		  f0.nc \
		  ctest.c ctest ctest0.nc ctest1.cdl \
		  ftest.f ftest ftest0.nc ftest1.cdl

lib_netcdf	= ../libsrc/libnetcdf.a
# Don't use "-L../libsrc -lnetcdf" in the following because that doesn't
# work on a CRAY T90 (sigh).
ld_netcdf	= $(lib_netcdf)

PACKING_LIST	= Makefile depend escapes.c generic.h \
		  ncgenyy.c ncgentab.c ncgentab.h \
		  genlib.c genlib.h getfill.c init.c load.c \
		  main.c ncgen.1 ncgen.h ncgen.l ncgen.y c0.cdl

OBJS		= main.o load.o ncgentab.o escapes.o \
		  getfill.o init.o genlib.o

lex		= flex
yacc		= yacc

program_srcs	= main.c load.c ncgentab.c escapes.c \
		  getfill.c init.c genlib.c

all:		$(PROGRAM)

test:           $(PROGRAM) b-test c-test f-test FORCE

install:	$(BINDIR)/$(PROGRAM) $(MANDIR)/man1/$(MANUAL)

uninstall:
	-rm -f $(BINDIR)/$(PROGRAM)
	-rm -f $(MANDIR)/man1/$(MANUAL)

$(PROGRAM):	$(OBJS) $(lib_netcdf)
	$(LINK.c) $(OBJS) $(ld_netcdf) $(LIBS) 

# "cat >" is used in the following rule to support linktree(1)-style
# development and testing. "mv" is used to support brain-damaged make(1)
# utilities that can't handle symbolic links and insist on regenerating
# the target files (e.g. OSF/1).
#
ncgentab.c \
ncgentab.h:	ncgen.y ncgenyy.c ncgen.h
	@case `uname` in \
	OSF1) \
	    echo 1>&2 "(expect 1 shift/reduce conflict)"; \
	    set -x; \
	    $(yacc) -d ncgen.y; \
	    cat y.tab.c >ncgentab.c && rm y.tab.c; \
	    cat y.tab.h >ncgentab.h && rm y.tab.h; \
	    set +x; \
	    ;; \
	*)  echo 1>&2 "Warning: $@ is out-of-date with respect to ncgen.y"; \
	    echo 1>&2 "Warning: It should be recreated via $(yacc)" \
		"on an OSF/1 system"; \
	    : set -x; \
	    : $(yacc) -d ncgen.y; \
	    : mv y.tab.c ncgentab.c; \
	    : mv y.tab.h ncgentab.h; \
	    : set +x; \
	    ;; \
	esac

# "cat >" is used in the following rule to support linktree(1)-style
# development and testing on a Solaris system.  "mv" is used to support
# brain-damaged make(1) utilities that can't handle symbolic links and
# insist on regenerating the target file (e.g. OSF/1).
#
ncgenyy.c:	ncgen.l
	@case `uname -sr` in \
	'SunOS 5'*) \
	    $(lex) ncgen.l; \
	    cat lex.yy.c >ncgenyy.c && rm lex.yy.c; \
	    ;; \
	*)  echo 1>&2 "Warning: $@ is out-of-date with respect to ncgen.l"; \
	    echo 1>&2 "Warning: It should be recreated via $(lex)" \
		"on a SunOS 5 system"; \
	    set -x; \
	    $(lex) ncgen.l; \
	    mv lex.yy.c ncgenyy.c; \
	    set +x; \
	    ;; \
	esac

vmstab.h \
vmstab.c:	ncgen.y
	@echo 1>&2 "$@ is out-of-date with respect to $?"
	@echo 1>&2 "It must be recreated via POSIX yacc(1) on a VMS system"
	false
vms_yy.c:	ncgenyy.c
	@echo 1>&2 "$@ is out-of-date with respect to $?"
	@echo 1>&2 "It must be recreated via POSIX lex(1) on a VMS system"
	false

#
# test "-b" option of ncgen
#
b-test:		$(PROGRAM) c1.cdl
	@./$(PROGRAM) -b c1.cdl && \
	$(NCDUMP) c1.nc > c2.cdl
	@if diff c1.cdl c2.cdl; then \
	    echo "*** $(PROGRAM) -b test successful ***"; \
	else \
	    echo "*** $(PROGRAM) -b test failed ***"; \
	    exit 1; \
	fi

#
# test "-c" option of ncgen
#
c-test:	$(PROGRAM) c1.cdl
	./$(PROGRAM) -c -o ctest0.nc c0.cdl > ctest.c && \
	$(CC) -o ctest $(CFLAGS) $(LDFLAGS) $(CPPFLAGS) ctest.c $(ld_netcdf) $(LIBS) && \
	./ctest	&& \
	$(NCDUMP) -n c1 ctest0.nc > ctest1.cdl
	@if diff c1.cdl ctest1.cdl; then \
	    echo "*** $(PROGRAM) -c test successful ***"; \
	else \
	    echo "*** $(PROGRAM) -c test failed ***"; \
	    exit 1; \
	fi

c1.cdl:	$(PROGRAM) c0.cdl
	./$(PROGRAM) -b -o c0.nc c0.cdl
	$(NCDUMP) -n c1 c0.nc > $@

#
# test "-f" option of ncgen
#
f-test:	$(PROGRAM) c0.cdl c1.cdl
	@if [ -n "$(FC)" ]; then \
	    $(MAKE) $(MFLAGS) ftest1.cdl && \
	    if diff c1.cdl ftest1.cdl; then \
		echo "*** $(PROGRAM) -f test successful ***"; \
	    else \
		echo "*** $(PROGRAM) -f test failed ***"; \
		exit 1; \
	    fi; \
	else \
	    echo 1>&2 "\`$@' not made because no FORTRAN compiler"; \
	fi

ftest1.cdl: $(PROGRAM) c0.cdl netcdf.inc
	./$(PROGRAM) -f -o ftest0.nc c0.cdl > ftest.f
	$(COMPILE.f) ftest.f
	$(FC) $(FFLAGS) $(LDFLAGS) -o ftest ftest.o $(ld_netcdf) $(LIBS)
	./ftest
	$(NCDUMP) -n c1 ftest0.nc > ftest1.cdl

netcdf.inc:
	@if [ -n "$(FC)" ]; then \
	    cp ../fortran/$@ .; \
	else \
	    echo 1>&2 "\`$@' not made because no FORTRAN compiler"; \
	fi

$(PROGRAM)_src : $(program_srcs)
	#setopt primary_language C
	#load -C $(CPPFLAGS) $(program_srcs)
	#load -C $(LIBS)
	#load -C /usr/lang/SC2.0.1/libansi.a
	#setopt program_name gribtonc

$(PROGRAM)_obj : $(program_srcs)
	#setopt primary_language C
	#load -C $(CPPFLAGS) $(OBJS)
	#load -C $(LIBS)
	#setopt program_name gribtonc

include ../rules.make

include depend
