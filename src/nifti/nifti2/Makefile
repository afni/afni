
## Note the TARFILE_NAME embeds the release version number
TARFILE_NAME	= nifti2clib-0.0.1

USEZLIB         = -DHAVE_ZLIB

## Compiler  defines
CC		= gcc
DEPENDFLAGS	= -MM
IFLAGS	        = -I. -I../niftilib -I../znzlib
CFLAGS		= -Wall -std=gnu99 -pedantic $(USEZLIB) $(IFLAGS)

LLIBS 		= -lz -lm

OBJS	   = nifti2_io.o nifticdf.o znzlib.o

NIFTI2OBJS = $(N2OBJ) $(MISCOBJS)

TOOLS 	   = nifti_tool
EXAMPLES   = clib_02.nifti2


%.o : %.c %.h
	$(CC) -c $(CFLAGS) $< -o $@

## Targets

nifti_tool: nifti_tool.o nifti_tool.h nifti2objs
	$(CC) -o $@ $(CFLAGS) $(INCFLAGS) $< $(OBJS) $(LLIBS)

clib_02.nifti2: clib_02.nifti2.o nifti2objs
	$(CC) -o $@ $(CFLAGS) $(INCFLAGS) $< $(OBJS) $(LLIBS)

all: $(TOOLS) $(EXAMPLES)

nifti2objs: $(OBJS)

clean:
	$(RM) *.o $(TOOLS) $(EXAMPLES)

# objects from source residing elsewhere
nifticdf.o:
	$(CC) -c $(CFLAGS) ../nifticdf/nifticdf.c -o $@

znzlib.o:
	$(CC) -c $(CFLAGS) ../znzlib/znzlib.c -o $@

