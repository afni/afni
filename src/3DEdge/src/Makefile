
CFILES = connexe.c \
	convert.c \
	extrema.c \
	iopnm.c \
	recbuffer.c \
	recline.c \
	test-edges.c \
	test-edges-pnm.c \
	test-hyster-pnm.c

COBJS = connexe.o \
	convert.o \
	extrema.o \
	iopnm.o \
	recbuffer.o \
	recline.o  \
	zcross.o 

EOBJS = test-edges.o \
	test-edges-pnm.o \
	test-hyster-pnm.o

#IFLAGS = -I. -I/usr/include -I/usr/X11R6/include -I/sw/include
#LFLAGS = -L. -L/usr/X11R6/lib -L/sw/lib 
#AR     = /usr/bin/ar
#RANLIB = /usr/bin/ranlib
#RM = /bin/rm -f

.c.o:; $(CC) $(IFLAGS) $(LFLAGS) -c $*.c;

all : clean test-edges test-edges-pnm test-hyster-pnm

clean :
	rm -f $(COBJS) $(EOBJS) test-edges test-edges-pnm test-hyster-pnm

lib3DEdge.a: $(COBJS)
	$(RM) $@
	$(AR) q lib3DEdge.a $(COBJS)
	$(RANLIB) lib3DEdge.a

test-edges : $(EOBJS) $(COBJS)
	$(CC) $(IFLAGS) $(LFLAGS) -o test-edges test-edges.o $(COBJS) -lm

test-edges-pnm : $(EOBJS) $(COBJS)
	$(CC) $(IFLAGS) $(LFLAGS)  -o test-edges-pnm test-edges-pnm.o $(COBJS) -lm

test-hyster-pnm : $(EOBJS) $(COBJS)
	$(CC) $(IFLAGS) $(LFLAGS)  -o test-hyster-pnm test-hyster-pnm.o $(COBJS) -lm
