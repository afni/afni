include ../macros.make

# Override the definition for "ncdump" in "../macros.make".
NCDUMP		= ../ncdump/ncdump

HEADER		= $(NETCDF.MOD)
LIBRARY		= ../libsrc/libnetcdf.a
LIB_OBJS	= netcdf.o typeSizes.o
GARBAGE		= netcdf_test *.mod example.nc example.cdl
MANUAL		= netcdf.3f90
PACKING_LIST	= \
		Makefile \
		NOTES \
		Readme \
		f90aux.m4 \
		example_good.cdl \
		gen.m4 \
		netcdf.3f90 \
		netcdf.f90 \
		netcdf_attributes.f90 \
		netcdf_constants.f90 \
		netcdf_dims.f90 \
		netcdf_expanded.f90 \
		netcdf_externals.f90 \
		netcdf_file.f90 \
		netcdf_overloads.f90 \
		netcdf_test.f90 \
		netcdf_text_variables.f90 \
		netcdf_variables.f90 \
		netcdf_visibility.f90 \
		nvea.m4 \
		nves.m4 \
		typeSizes.f90

all:		$(LIBRARY)

test:		netcdf_test
	@./netcdf_test; \
	$(NCDUMP) example.nc >example.cdl; \
	if cmp example_good.cdl example.cdl; then \
	    echo '*** Success ***'; \
	else \
	    echo '*** Failure ***'; \
	    diff -c example_good.cdl example.cdl; \
	fi

netcdf_test:	netcdf_test.f90 netcdf.o typeSizes.o
	$(LINK.F90) netcdf_test.f90 netcdf.o typeSizes.o $(LIBRARY) $(F90LIBS)

typeSizes.o:	typeSizes.f90
	$(COMPILE.F90) typeSizes.f90

netcdf_expanded.f90:	gen.m4 f90aux.m4 nves.m4 nvea.m4
	$(M4) gen.m4 >$@

netcdf.o:	\
	    netcdf.f90 typeSizes.o netcdf_constants.f90 netcdf_externals.f90 \
	    netcdf_overloads.f90 netcdf_visibility.f90 netcdf_file.f90 \
	    netcdf_dims.f90 netcdf_attributes.f90 netcdf_variables.f90 \
	    netcdf_text_variables.f90 netcdf_expanded.f90
	$(COMPILE.F90) netcdf.f90

install:	$(INCDIR)/$(HEADER) $(MANDIR)/man3f90/$(MANUAL)

uninstall:
	-rm -f $(INCDIR)/$(HEADER $(MANDIR)/man3f90/$(MANUAL)

include ../rules.make
