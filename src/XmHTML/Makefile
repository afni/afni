##############################################################################
#                                                                            #
# Makefile.org: default XmHTML makefile.                                     #
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
# The defaults in this Makefile are for Linux.

# Don't change this number!
VERSION = 1107
ARCHIVE = 1.1.7

# Target toolkit
PLATFORM=Motif
SUBDIRS= lib

# some common definitions
#
CP    = cp
RM    = rm -f
MV    = mv
LN    = ln -sf

# for building the library. 
AR    = ar cq 
# replace with echo or /bin/true if you system doesn't have/need ranlib
RANLIB= ranlib

SHELL     = /bin/sh
MAKE      = make 
# change this to makedepend if your system doesn't have gcc
MAKEDEPEND= gccmakedep

#####################
#                   #
# Compiler Settings #
#                   #
#####################
# CC       = cc
CFLAGS   = 
LDFLAGS  = 

# The following flags are *very* usefull if you are getting unresolved
# references to functions in libraries that you *know* to exist.
# When used, they cause ld to list its search for libraries, the object
# files it is considering for linking and a bunch of other information.
# It helped me to track down an incorrect version of libz that was
# lingering around in a directory where I never expected it to be.
# Note: these flags are for gcc!
# LDFLAGS  = -Wl,-t -Wl,--verbose

# Image Support
# libXpm *is* required
# JPEG images (jpeglib.h and libjpeg)          => -DHAVE_LIBJPEG
# PNG images  (png.h, zlib.h, libz and libpng) => -DHAVE_LIBPNG
# GZF images  (zlib.h and libz)                => -DHAVE_LIBZ or -DHAVE_LIBPNG
#
# You can configure the Progressive Image Loading to use either timeouts or
# work procedures. The default is to use timeouts (XmHTML will cycle through
# each image that is to be loaded progressively using a dynamically computed
# polling interval). If you want XmHTML to use work procedures, you need to 
# define PLC_WORKPROCS
# The differences between the two methods are:
# timeouts:  XmHTML is quite responsive to user-input, image loading not
#            really fast (but neither really slow). Polling interval is
#            computed dynamically depending on the state of each PLC (suspended
#            versus active); 
# workprocs: XmHTML not really responsive, image loading quite fast.
#
# Dithering: there are two algorithms available that allow XmHTML to construct
# a default palette if none has been specified.
# default: the default algorithm constructs an evenly distributed palette in
#          RGB space but is not always able to match the requested number of
#          colors and requires a minumum of 8 colors.
# simple:  this is the alternative, which always creates a palette with the
#          requested number of colors but does not always succeed in creating
#          an evenly distributed palette. It is selected by defining
#          DITHER_SIMPLE_COLORMAP.

IMAGEDEFINES = -DHAVE_LIBJPEG

# Header file locations. Add path to xpm.h if it isn't in the X11 include
# directory. Depending on the selected image support, the compiler needs to
# be able to find the following header files:
# HAVE_LIBJPEG => jpeglib.h
# HAVE_LIBPNG  => png.h and zlib.h
# HAVE_LIBZ => zlib.h

ZLIBINC       =
IMAGEINCLUDES = $(ZLIBINC)

# Image libraries
# No need to add -lXpm if you need to include it with LIBS below
# Depending on selected image support your linker needs to find the following
# libraries:
# HAVE_LIBJPEG => libjpeg
# HAVE_LIBPNG  => libpng and libz (and possibly libm)
# HAVE_LIBZ => libz (and possibly libm)

ZLIBLIB   = -lz -lm
IMAGELIBS = -ljpeg -lpng $(ZLIBLIB)

# It might be troublesome to know what the correct defines are for your
# system. A possible set of defines for some OS's is given below. Pick
# your choice.
#
# HPUX:
# 	-D_HPUX_SOURCE -D_POSIX_SOURCE
#
# Linux:
#	-D_BSD_SOURCE -D_GNU_SOURCE -D_POSIX_SOURCE -DHAVE_REGEX_H
#
# AIX 4.1.5:
#	-DHAVE_STRINGS_H -DHAVE_REGEX_H
#
#	(AIX 4.1.5 has str[n]casecmp, but it's defined in strings.h)
#
# Unixware 1.1.2
#	-DNEED_STRCASECMP -DNEED_STRERROR -DSVR4 -DHAVE_MEMCPY
#
# Unixware 2.1.1
#	-DNEED_STRCASECMP -D_POSIX_SOURCE
#
# LynxOS
#	-DNEED_STRCASECMP
#
# SCO 3.2
#	-DNEED_STRCASECMP -D_POSIX_SOURCE
#
# If you have dmalloc, you might wish to add -DDMALLOC
#
# If your system does not have str[n]casecmp, add -DNEED_STRCASECMP (unixware)
# If your system does not have strerror, add -DNEED_STRERROR
# If your system has POSIX regex.h, add -DHAVE_REGEX_H
#
# Add -DI18N for internationalization and multilingual support.
# Add -DPEDANTIC to get warnings about bad html and unsupported features.
# Add -DDEBUG to enable selective debug output (see the file DEBUGGING for more
# info).
#
CPPFLAGS = -DNDEBUG -DHAVE_REGEX_H $(IMAGEDEFINES) 

# Various include directories
LIBINC=-I. -I../include -I../include/common -I../../include -I../../include/common -I../../../jpeg-6b

# Platform specific includes
PLATFORMINC=-I../../include/XmHTML -I/usr/X11R6/include

# include directories
# If using dmalloc, add the path where the dmalloc.h include file can be 
# found. On my system it's in /usr/local/include
INCLUDES = $(LIBINC) $(PLATFORMINC) $(IMAGEINCLUDES) $(II)

LIBDIR = -L/usr/X11R6/lib -L../../..
LIBS   = -lXm -lXpm -lXmu -lXp -lXt -lXext -lX11 -lSM -lICE

# final line fed to the linker 
LOADLIBES = $(LIBDIR) $(LIBS) $(IMAGELIBS)

### End of User configurable section ###

# To guarantee robust "set -x" operation
export PS4=+

pass_flags = SHELL='$(SHELL)' \
	VERSION='$(VERSION)' \
	CC='$(CC)' \
	RM='$(RM)' \
	AR='$(AR)' \
	MV='$(MV)' \
	CP='$(CP)' \
	LN='$(LN)' \
	RANLIB='$(RANLIB)' \
	MAKEDEPEND='$(MAKEDEPEND)' \
	CFLAGS='$(CFLAGS)' \
	LDFLAGS='$(LDFLAGS)' \
	CPPFLAGS='$(CPPFLAGS) -D$(PLATFORM)' \
	INCLUDES='$(INCLUDES)' \
	LOADLIBES='$(LOADLIBES)' \
	DMALLOCLIB='$(DMALLOCLIB)' \
	ZLIBLIB='$(ZLIBLIB)' \
	PLATFORM='$(PLATFORM)'

all::
	@( for dir in $(SUBDIRS) ; do \
		(set -x ; cd $$dir ; $(MAKE) all $(pass_flags) ) ; \
	done)

# Separate rules for all targets 
lib::
	@(set -x; cd lib; $(MAKE) all $(pass_flags) )

http::
	@(set -x; cd http; $(MAKE) all $(pass_flags) )

depend:: 
	@( for dir in $(SUBDIRS) ; do \
		(set -x ; cd $$dir ; $(MAKE) depend $(pass_flags)) ; \
	done)

clean:: 
	@( for dir in $(SUBDIRS) ; do \
		(set -x ; cd $$dir ; $(MAKE) clean $(pass_flags) ) ; \
	done)

distclean:: 
	@( for dir in $(SUBDIRS) ; do \
		(set -x ; cd $$dir ; $(MAKE) distclean $(pass_flags) ) ; \
	done)
	@(set -x; cd book; $(MAKE) distclean $(pass_flags) )
	@(set -x; cd contrib; $(MAKE) distclean $(pass_flags) )
	$(RM) docs/sources.desc
	$(RM) core *.out *.log make.world *.bak *.last *.auto *.rej *.orig
	$(CP) Makefile.org Makefile

dist:: distclean
	$(LN) . XmHTML-$(ARCHIVE)
	tar cvf - XmHTML-$(ARCHIVE)/* | gzip -c > XmHTML-$(ARCHIVE).tar.gz
