#include ../macros.make

INCLUDES	= -I../libsrc -I.

LIBRARY		= libnetcdf_c++.a
lib_netcdf	= ../libsrc/libnetcdf.a
ld_netcdf	= -L../libsrc -lnetcdf

HEADER1		= netcdfcpp.h
HEADER2		= ncvalues.h
HEADER3		= netcdf.hh

libs		=  $(LIBRARY) $(ld_netcdf) $(LIBS)

purify		= purify

LIB_OBJS	= netcdf.o ncvalues.o

# a test program that uses some of the draft C++ interface
prog		= nctst

prog_objs 	= nctst.o

all:		$(LIBRARY) $(prog)

test:	 $(prog) FORCE
	./$(prog) > test.out
	@cmp expected test.out && \
	    echo "*** C++ test successful ***" ;

$(prog) : $(prog_objs) $(LIBRARY) $(lib_netcdf)
	$(CXX) -o $@ $(CXXFLAGS) $(LDFLAGS) $(prog_objs) $(libs)

nctst_src: nctst.cpp netcdf.cpp ncvalues.cpp
	#load -I../libsrc $(nctst_src)
	#load -L../../lib ../../lib/libnetcdf.a
	#link

# dvips is part of the TeX package
dvips		= dvips
# texi2dvi and makeinfo are part of the texinfo package
texi2dvi	= texi2dvi
makeinfo	= makeinfo
# texi2html is available from http://wwwcn.cern.ch/dci/texi2html/
texi2html	= texi2html

print: nctst.cpp netcdfcpp.h netcdf.cpp ncvalues.h ncvalues.cpp
	vgrind -s11 -lC++ -t nctst.cpp netcdfcpp.h netcdf.cpp ncvalues.h \
				ncvalues.cpp | lpr
ps: cxxdoc.ps

info: cxxdoc.info

html: cxxdoc.html

cxxdoc.ps:	cxxdoc.tex
	$(texi2dvi) cxxdoc.tex
	$(dvips) -D 300 -o cxxdoc.ps cxxdoc.dvi

cxxdoc.info: cxxdoc.tex
	$(makeinfo) cxxdoc.tex

cxxdoc.html:	cxxdoc.tex
	$(texi2html) cxxdoc.tex

nctst.pure:	$(prog_objs)
	$(purify) $(LINK.cpp) $(prog_objs) $(libs) -o $@

include ../rules.make

netcdf.o: netcdfcpp.h ../libsrc/netcdf.h netcdf.cpp
ncvalues.o: ncvalues.h ncvalues.cpp
nctst.o: netcdfcpp.h nctst.cpp

include depend

