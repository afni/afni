# Makefile for the VolPack library.
#
# Copyright (c) 1994 The Board of Trustees of The Leland Stanford
# Junior University.  All rights reserved.
# 
# Permission to use, copy, modify and distribute this software and its
# documentation for any purpose is hereby granted without fee, provided
# that the above copyright notice and this permission notice appear in
# all copies of this software and that you do not sell the software.
# Commercial licensing is available by contacting the author.
# 
# THE SOFTWARE IS PROVIDED "AS IS" AND WITHOUT WARRANTY OF ANY KIND,
# EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY
# WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
# 
# Author:
#    Phil Lacroute
#    Computer Systems Laboratory
#    Electrical Engineering Dept.
#    Stanford University
#

# shell to use for executing make commands
SHELL =		/bin/sh

###########################################################################
# Options that can be changed by the user.
###########################################################################

# list of optional C source files to build with code specialized
# for particular shaders and resampling filters
OPT_SRCS =	vp_compAC11B.c vp_compAC31B.c vp_compAC32B.c \
		vp_compAR11B.c vp_compAR31B.c vp_compAR32B.c

# object files corresponding to COMP_SRCS
OPT_OBJS =	vp_compAC11B.o vp_compAC31B.o vp_compAC32B.o \
		vp_compAR11B.o vp_compAR31B.o vp_compAR32B.o

# C define flags corresponding to COMP_SRCS; these tell the dispatch
# routines in the library which object files are available
OPT_FLAGS =	-DCOMP_AC11B   -DCOMP_AC31B   -DCOMP_AC32B \
		-DCOMP_AR11B   -DCOMP_AR31B   -DCOMP_AR32B

CSRC = vp_check.c vp_context.c vp_extract.c vp_file.c vp_linalg.c vp_octree.c \
       vp_renderA.c vp_renderB.c vp_renderC.c vp_renderR.c vp_resample.c \
       vp_rle.c vp_shade.c vp_transpose.c vp_util.c vp_view.c vp_warp.c
HSRC = volpack.h vp_global.h patchlevel.h
MSRC = vp_compA.m4 vp_warpA.m4
SUPPORT = Makefile.in configure.in no_edit_header makeopts.c
COMP_SRCS = vp_compAC1NB.c vp_compAC3NB.c vp_compAR1NB.c vp_compAR3NB.c \
       vp_compAC1PB.c vp_compAC3PB.c vp_compAR1PB.c vp_compAR3PB.c \
       vp_compAC00G.c vp_compAR00G.c vp_compAC1NS.c vp_compAC3NS.c \
       vp_compAR1NS.c vp_compAR3NS.c $(OPT_SRCS)
COMP_OBJS = vp_compAC1NB.o vp_compAC3NB.o vp_compAR1NB.o vp_compAR3NB.o \
       vp_compAC1PB.o vp_compAC3PB.o vp_compAR1PB.o vp_compAR3PB.o \
       vp_compAC00G.o vp_compAR00G.o vp_compAC1NS.o vp_compAC3NS.o \
       vp_compAR1NS.o vp_compAR3NS.o $(OPT_OBJS)
WARP_SRCS = vp_warpA101N.c vp_warpA301N.c vp_warpA110N.c vp_warpA111N.c \
       vp_warpA330N.c vp_warpA331N.c vp_warpA330R.c vp_warpA331R.c
WARP_OBJS = vp_warpA101N.o vp_warpA301N.o vp_warpA110N.o vp_warpA111N.o \
       vp_warpA330N.o vp_warpA331N.o vp_warpA330R.o vp_warpA331R.o
CGEN = vp_opts.c $(COMP_SRCS) $(WARP_SRCS)
OBJS = vp_check.o vp_context.o vp_extract.o vp_file.o vp_linalg.o vp_octree.o \
       vp_renderA.o vp_renderB.o vp_renderC.o vp_renderR.o vp_resample.o \
       vp_rle.o vp_shade.o vp_transpose.o vp_util.o vp_view.o vp_warp.o \
       vp_opts.o $(COMP_OBJS) $(WARP_OBJS)
COMP_IN = $(SRC_DIR)/no_edit_header $(SRC_DIR)/vp_compA.m4
WARP_IN = $(SRC_DIR)/no_edit_header $(SRC_DIR)/vp_warpA.m4
ESRC = vp_raycast.c
EOBJ = vp_raycast.o

libvolpack.a: $(OBJS)
	rm -f libvolpack.a
	ar cr libvolpack.a $(OBJS)
	$(RANLIB) libvolpack.a

$(OBJS): volpack.h vp_global.h config.h

######################################################################################
# Stuff added by RWCox to fit in with the AFNI package
######################################################################################

clean:
	$(RM) *.o

.c.o:
	$(RM) $@
	$(CC) -c $*.c -I. -I.. $(IFLAGS) $(OPT_FLAGS)
