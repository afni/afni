# Makefile for znzlib

# This file is released to the public domain

# specify the path to the include files for zlib on this system
# if it is not in the standard include paths for the compiler
#   e.g. -I/usr/unusual/include
LIBZINC=

# uncomment the following for allowing libz compression
# USE_ZLIB = -DHAVE_ZLIB
# LZLIB = -lz

all: nifti_stats nifti_tool nifti1_test

nifti_stats: nifti_stats.o
	rm -f nifti_stats
	gcc ${LIBZINC} -o nifti_stats nifti_stats.o -lm

nifti_tool: nifti_tool.o nifti1_io.o znzlib.o
	rm -f nifti_tool
	gcc ${LIBZINC} -o nifti_tool nifti_tool.o nifti1_io.o znzlib.o $(LZLIB) -lm

nifti1_test: nifti1_test.o nifti1_io.o znzlib.o
	rm -f nifti1_test
	gcc ${LIBZINC} -o nifti1_test nifti1_test.o nifti1_io.o znzlib.o $(LZLIB) -lm

.c.o:
	rm -f $@
	gcc -c $*.c $(USE_ZLIB)

clean:
	rm -f nifti_stats nifti1_test nifti_tool *.o

