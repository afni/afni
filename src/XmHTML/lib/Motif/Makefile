##############################################################################
#                                                                            #
# Makefile.org: libXmHTML, Motif version                                     #
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
MOTIFSRCS= motif.c XmHTML.c output.c XmImage.c plc.c frames.c textsel.c \
	forms.c XCC.c Balloon.c warnings.c strings.c

MOTIFOBJS= motif.o XmHTML.o output.o XmImage.o plc.o frames.o textsel.o \
	forms.o XCC.o Balloon.o warnings.o strings.o

# Comment these out if you don't want to include the debug routines.
# or define NDEBUG in the toplevel Makefile.
DEBSRCS= debug_menu.c
DEBOBJS= debug_menu.o

SRCS = $(MOTIFSRCS) $(DEBSRCS)
OBJS = $(MOTIFOBJS) $(DEBOBJS)

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
