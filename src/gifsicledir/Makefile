# Generated automatically from Makefile.in by configure.
# Makefile.in generated automatically by automake 1.4 from Makefile.am

# Copyright (C) 1994, 1995-8, 1999 Free Software Foundation, Inc.
# This Makefile.in is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY, to the extent permitted by law; without
# even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.

# Modified 27 Jul 2001 by RWCox
# - stripped out the make-ing of unneeded things

OBJS =  clp.o giffunc.o gifread.o gifunopt.o merge.o \
        optimize.o quantize.o support.o xform.o gifsicle.o gifwrite.o fmalloc.o

gifsicle: $(OBJS)
	$(CC) -o gifsicle $(LFLAGS) $(OBJS) -lm

.c.o:
	$(CC) -c $<

clean:
	$(RM) *.o
