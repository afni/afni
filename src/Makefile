# This Makefile works on my personal Linux machine - RWCox

CCDEBS = -DAFNI_DEBUG -DIMSEQ_DEBUG -DDISPLAY_DEBUG -DTHD_DEBUG
CEXTRA = -Wcomment -Wformat -DUSE_TRACING -DHAVE_XDBE -pipe -Wuninitialized
CC     = gcc -O2 -ffast-math -DLINUX2 -DNO_GAMMA $(CEXTRA)
CCVOL  = gcc -O2 -ffast-math -DLINUX2 -DNO_GAMMA $(CEXTRA)
CCFAST = gcc -O3 -ffast-math -DLINUX2 -DNO_GAMMA $(CEXTRA)
CCMIN  = gcc
CCD    = $(CC) $(CCDEBS)
IFLAGS = -I. -I/usr/X11R6/include
LFLAGS = -L. -L/usr/X11R6/lib

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

# for dynamic linking

# LLIBS  = -lmri -lXm -lXp -lXpm -lXext -lXmu -lXt -lX11 -lm  -ldl -lc

# for static linking to Motif, dynamic to all else

LLIBS  = -lmri /usr/X11R6/lib/libXm.a -lXp -lXpm -lXext -lXmu -lXt -lX11 -lm  -ldl -lc

# for static linking, as far as possible

# LLIBS = -lmri -ldl               \
#         /usr/X11R6/lib/libXm.a   \
#        /usr/X11R6/lib/libXpm.a  \
#        /usr/X11R6/lib/libXext.a \
#        /usr/X11R6/lib/libXmu.a  \
#        /usr/X11R6/lib/libXt.a   \
#        /usr/X11R6/lib/libSM.a   \
#        /usr/X11R6/lib/libICE.a  \
#        /usr/X11R6/lib/libX11.a  \
#        /usr/lib/libm.a          \
#        /usr/lib/libc.a

###############################################################

include Makefile.INCLUDE
