/*****
* LZWStream.h : LZW uncompressor public header file
*
* This file Version	$Revision$
*
* Creation date:		Thu May  8 04:57:06 GMT+0100 1997
* Last modification: 	$Date$
* By:					$Author$
* Current State:		$State$
*
* Author:				newt
*
* Copyright (C) 1994-1997 by Ripley Software Development
* All Rights Reserved
*
* This file is part of the XmHTML Widget Library.
*
* Permission to use, copy, modify, and distribute this software and its
* documentation for any purpose and without fee is hereby granted, provided
* that the above copyright notice appear in all copies and that both that
* copyright notice and this permission notice appear in supporting
* documentation.  This software is provided "as is" without express or
* implied warranty.
*
* Based on an idea from Derek D. Noonburg <derekn@ece.cmu.edu>, author of
* the public domain xpdf package, a PDF viewer.
*
*****/
/*****
* $Source$
*****/
/*****
* ChangeLog
* $Log$
* Revision 1.1  2011/06/30 16:08:56  rwcox
* Cadd
*
* Revision 1.3  1997/08/30 00:25:02  newt
* Changed copyright. Use of the LZWStream package is now totally unrestricted.
*
* Revision 1.2  1997/08/01 12:52:23  newt
* Progressive image loading changes
*
* Revision 1.1  1997/05/28 01:27:04  newt
* Initial Revision
*
*****/

#ifndef _LZWStream_h_
#define _LZWStream_h_

#define BUFFERSIZE	512

/*****
* LZW stream definition
*****/
typedef struct _LZWStream{
	FILE *zPipe;				/* uncompress file handle */
	FILE *f;					/* compress file handle */
	char zCmd[256];				/* uncompress command */
	char *zName;				/* compress file name, indexes in zCmd */
	int error;					/* uncompress error flag */
	int uncompressed;			/* uncompress finished flag */
	ImageBuffer *ib;			/* master input buffer */

	unsigned char accum[BUFFERSIZE];	/* buffered output */
	int acount;					/* current char count */

	/* LZW code computation variables */
	char buf[280];				/* input buffer */
	int curBit;					/* no of bits processed so far */
	int lastBit;				/* bitcount of last bit in input buffer */
	int lastByte;				/* last known processed byte */
	int done;					/* input done flag */
	int nextCode;				/* LZW code counter */

	/* global raster data variables */
	int codeSize;				/* bits per pixel */
	int codeBits;				/* bits used by each LZW code */
	int clearCode;				/* reset signal table signal */
	int endCode;				/* end-of-data signal */
	int maxCode;				/* start code signal */
	int maxCodeSize;			/* maximum signal table size */
	char outBuf[16];			/* compress output buffer */

	/* variables for images with 7 or less bits per pixel */
	int offset;					/* current bit offset */
	int freeEntry;				/* compress code counter */
	int n_bits;					/* output code size */
	int maxcode8;				/* maximum output signal */
	int clearFlag;				/* clear signal table flag */

	/* data readers */
	size_t (*readOK)(ImageBuffer*, unsigned char*, int);
	size_t (*getData)(ImageBuffer*, unsigned char*);

	char *err_msg;				/* error description */
}LZWStream;

/* create a new stream object */
extern LZWStream *LZWStreamCreate(ImageBuffer *ib, char *zCmd);

/*****
* Initialize uncompression. Possible return codes:
* -2: the read functions haven't been set;
* -1: temp file couldn't be opened;
*  0: gif code size is invalid/couldn't be read;
*  1: success.
*****/
extern int LZWStreamInit(LZWStream *lzw);

/* convert GIF LZW to compress LZW */
extern void LZWStreamConvert(LZWStream *lzw);

/* destroy the stream object */
extern void LZWStreamDestroy(LZWStream *lzw);

/* return an allocated buffer with uncompressed stream data */
extern unsigned char *LZWStreamUncompress(LZWStream *lzw, int *size);

/* read uncompressed data from the stream */
extern int LZWStreamFillBuffer(LZWStream *lzw, unsigned char *data, int size);

/* return original bits per pixel */
extern int LZWStreamGetCodeSize(LZWStream *lzw);

/* Don't add anything after this endif! */
#endif /* _LZWStream_h_ */
