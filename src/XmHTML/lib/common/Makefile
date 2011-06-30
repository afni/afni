##############################################################################
#                                                                            #
# Makefile.org: libXmHTML, display independent code                          #
# Adjust at will                                                             #
#                                                                            #
##############################################################################
#                                                                            #
# (C)Copyright 1995-1997 Ripley Software Development                         #
# All Rights Reserved                                                        #
#                                                                            #
# This file is part of the XmHTML Widget Library.                            #
#                                                                            #
# This library is free software; you can redistribute it and/or              #
# modify it under the terms of the GNU Library General Public                #
# License as published by the Free Software Foundation; either               #
# version 2 of the License, or (at your option) any later version.           #
#                                                                            #
# This library is distributed in the hope that it will be useful,            #
# but WITHOUT ANY WARRANTY; without even the implied warranty of             #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          #
# Library General Public License for more details.                           #
#                                                                            #
# You should have received a copy of the GNU Library General Public          #
# License along with this library; if not, write to the Free                 #
# Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.         #
##############################################################################
#

# List of source and object files 
COMMONSRCS= events.c hash.c stack.c format.c fonts.c paint.c readXPM.c \
	readJPEGplc.c quantize.c colors.c private.c readBitmap.c public.c \
	readPNG.c readGIFplc.c readGIF.c error.c callbacks.c StringUtil.c \
	images.c readFLG.c readJPEG.c parse.c map.c layout.c LZWStream.c \
	psoutput.c HTTP.c cookie.c

COMMONOBJS= events.o hash.o stack.o format.o fonts.o paint.o readXPM.o \
	readJPEGplc.o quantize.o colors.o private.o readBitmap.o public.o \
	readPNG.o readGIFplc.o readGIF.o error.o callbacks.o StringUtil.o \
	images.o readFLG.o readJPEG.o parse.o map.o layout.o LZWStream.o \
	psoutput.o HTTP.o cookie.o

# Comment these out if you don't want to include the debug routines.
# or define NDEBUG in the toplevel Makefile.
DEBSRCS= debug.c
DEBOBJS= debug.o

SRCS = $(COMMONSRCS) $(DEBSRCS)
OBJS = $(COMMONOBJS) $(DEBOBJS)

# Target to make
TARGET=$(OBJS)

# Subdirectories to visit
SUBDIRS=

# rule to create .o files from .c files 
.c.o:
	$(RM) $@
	$(CC) $(CFLAGS) -DVERSION=$(VERSION) $(CPPFLAGS) $(INCLUDES) -c $<

# targets to build
all: $(TARGET)

depend:: $(SRCS) 
	$(MAKEDEPEND) $(INCLUDES) $(CPPFLAGS) $(SRCS)

clean::
	$(RM) $(OBJS)

distclean:: clean 
	$(RM) core *.out *.log make.world *.bak *.last *.auto *.rej *.orig
	$(RM) *.lh *.lcs *.lint stamp-includes
	$(CP) Makefile.org Makefile

#--------------------------------------------------------------------------
# don't delete anything below this line, makedepend depends on it
#--------------------------------------------------------------------------
