# This Makefile works on Linux 2.0.39

CCDEBS = -DAFNI_DEBUG -DIMSEQ_DEBUG -DDISPLAY_DEBUG -DTHD_DEBUG
CEXTRA = -Wcomment -Wformat -DUSE_TRACING -fomit-frame-pointer -DHAVE_XDBE
CC     = gcc -O2 -march=i586 -ffast-math -DLINUX2 -DNO_GAMMA $(CEXTRA)
CCVOL  = gcc -O2 -march=i586 -ffast-math -DLINUX2 -DNO_GAMMA $(CEXTRA)
CCFAST = gcc -O3 -march=i586 -ffast-math -DLINUX2 -DNO_GAMMA $(CEXTRA)
CCMIN  = gcc -s
CCD    = $(CC) $(CCDEBS)
IFLAGS = -I. -I/usr/motif-build/motif/lib -I/usr/X11R6/include
LFLAGS = -s -L. -L/usr/X11R6/lib -L/usr/motif-build/motif/lib

PLUGIN_SUFFIX = so
PLUGIN_LFLAGS = -shared
PLUGIN_CC     = $(CC)
PLFLAGS       = -s -rdynamic -L. -L/usr/motif-build/motif/lib -L/usr/X11R6/lib

AR     = /usr/bin/ar
RANLIB = /usr/bin/ranlib
TAR    = /bin/tar
MKDIR  = /bin/mkdir
GZIP   = /bin/gzip
LD     = gcc

RM = /bin/rm -f
MV = /bin/mv -f
CP = /bin/cp -f

LINT = /usr/bin/lint -a -b -u -v -x $(IFLAGS) $(CCDEFS)

INSTALLDIR = /home/ziad/abin
LIBDIR = $(INSTALLDIR)


# for dynamic linking

LLIBS  = -lmri -lXm -lXp -lXpm -lXext -lXmu -lXt -lX11 -lm  -ldl -lc

# for static linking to Motif, dynamic to all else

#LLIBS  = -lmri /usr/X11R6/lib/libXm.a -lXp -lXpm -lXext -lXmu -lXt -lX11 -lm  -ldl -lc

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

# vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
# For suma (NO STATIC LINKING OF GL libs)
SUMA_INCLUDE_PATH = -I/usr/X11R6/include -I./ -I../
SUMA_LINK_PATH = -L/usr/lib -L/usr/X11R6/lib -L../
SUMA_LINK_LIB = -lMesaGLwM -lMesaGLw -lGLU -lGL -lXmu -lXm -lXt -lXext -lX11 -lcurses -lm 
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

###############################################################

include Makefile.INCLUDE
