# Unix Makefile for qhull
# - adapted by RWCox for AFNI purposes
# - stripped out the source code and compilation of unneeded things

# OBJS in execution frequency order.
OBJS = user.o global.o stat.o io.o geom2.o poly2.o \
       merge.o qhull.o geom.o poly.o qset.o mem.o

qhull: unix.o $(OBJS)
	$(CC) -o qhull $(LFLAGS) unix.o $(OBJS) -lm

.c.o:
	$(CC) -c $<

clean:
	$(RM) *.o
