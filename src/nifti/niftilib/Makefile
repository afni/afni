include ../Makefile

PROJNAME = niftiio


INCFLAGS = $(ZLIB_INC) $(ZNZ_INC)
LIBS = $(ZLIB_LIBS) $(ZNZ_LIBS) $(NIFTI_LIBS)

SRCS=nifti1_io.c
OBJS=nifti1_io.o
MOBJS=mjtest.o

SCRIPTS = 

depend:	
	$(RM) -f depend.mk
	$(MAKE) depend.mk

depend.mk:
	$(CC) $(DEPENDFLAGS) $(INCFLAGS) $(SRCS) >> depend.mk

lib: libniftiio.a

libniftiio.a: ${OBJS}
	${AR} -r libniftiio.a ${OBJS}

include depend.mk
