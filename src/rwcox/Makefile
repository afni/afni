# This Makefile works on my personal Linux machine - RWCox

CEXTRA = -Wcomment -Wformat -DUSE_TRACING -fomit-frame-pointer -DHAVE_XDBE -pipe -Wuninitialized
CC     = gcc -O2 -ffast-math -DLINUX2 -DNO_GAMMA $(CEXTRA)
IFLAGS = -I. -I.. -I/usr/X11R6/include
LFLAGS = -L. -L.. -L/home/rwcox/abin -L/usr/X11R6/lib

PLUGIN_SUFFIX = so
PLUGIN_LFLAGS = -shared
PLUGIN_CC     = $(CC)
PLFLAGS       = -rdynamic -L. -L/usr/X11R6/lib

AR     = /usr/bin/ar
RANLIB = /usr/bin/ranlib
TAR    = /bin/tar
MKDIR  = /bin/mkdir
TAPE   = /dev/st0
GZIP   = /bin/gzip
LD     = gcc

RM = /bin/rm -f
MV = /bin/mv -f
CP = /bin/cp -f

LINT = /usr/bin/lint -a -b -u -v -x $(IFLAGS) $(CCDEFS)

INSTALLDIR = $(HOME)/abin
LIBDIR = $(INSTALLDIR)

LLIBS  = -lmri /usr/X11R6/lib/libXm.a -lXp -lXpm -lXext -lXmu -lXt -lX11 -lm  -ldl -lc

.c.o:
	$(RM) $@
	$(CC) -c $*.c $(IFLAGS)

.SUFFIXES:.$(PLUGIN_SUFFIX)

SO = $(PLUGIN_SUFFIX)

.c.$(SO):
	$(RM) $@
	$(PLUGIN_CC) -c $*.c $(IFLAGS)
	$(LD) $(PLUGIN_LFLAGS) -o $*.$(SO) $*.o

hasher:hasher.o
	$(CC) -o hasher hasher.o $(LFLAGS) $(LLIBS)
