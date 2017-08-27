# Unix Makefile for qhull
#
# Stripped down for AFNI -- 30 Sep 2010


OBJS = rboxlib.o user.o global.o stat.o io.o geom2.o poly2.o merge.o \
       libqhull.o geom.o poly.o qset.o mem.o usermem.o userprintf.o \
       random.o

.c.o:
	$(CC) -c $<

clean:
	$(RM) *.o 

qhull: unix.o $(OBJS)
	$(CC) -o qhull $(LFLAGS) unix.o $(OBJS) -lm 

qdelaunay:  qdelaun.o $(OBJS)
	$(CC) -o qdelaunay $(LFLAGS) qdelaun.o $(OBJS) -lm 

rbox:  rbox.o $(OBJS)
	$(CC) -o rbox $(LFLAGS) rbox.o $(OBJS) -lm 
