# Copyright (c) 1995 The Regents of the University of California.
# All rights reserved.
#
# Permission to use, copy, modify, and distribute this software and its
# documentation for any purpose, without fee, and without written agreement is
# hereby granted, provided that the above copyright notice and the following
# two paragraphs appear in all copies of this software.
#
# IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
# DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
# OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
# CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
# AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
# ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
# PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
#

# O2 Aug 2001: Modified by RWCox for inclusion into the AFNI scheme

.c.o:
	$(CC) -c -Iheaders $<

clean:
	$(RM) *.o

MP_JPEG_OBJS = nojpeg.o

##############################################################################

MP_BASE_OBJS = mfwddct.o postdct.o huff.o bitio.o mheaders.o
MP_ENCODE_OBJS = iframe.o pframe.o bframe.o psearch.o bsearch.o block.o 
MP_OTHER_OBJS = mpeg.o subsample.o param.o rgbtoycc.o \
	readframe.o combine.o jrevdct.o frame.o fsize.o frametype.o \
	libpnmrw.o specifics.o rate.o opts.o
MP_PARALLEL_OBJS = parallel.o
MP_ALL_OBJS = $(MP_BASE_OBJS) $(MP_OTHER_OBJS) $(MP_ENCODE_OBJS) \
	      $(MP_PARALLEL_OBJS) $(MP_JPEG_OBJS) main.o

############
# BINARIES #
############

mpeg_encode: $(MP_ALL_OBJS)
	$(CC) $(CFLAGS) -o $@ $(MP_ALL_OBJS) -L.. $(LFLAGS)

headers.o: headers.c
	$(CC) $(CFLAGS) -c headers.c
