#ifndef production
static char rcsId[]="$Header$";
#endif
/*****
* LZWStream.c : LZW uncompressor that doesn't use the LZW algorithm but
*				converts LZW compressed data to compress LZW data and
*				calls compress to do the uncompressing.
*
* This file Version	$Revision$
*
* Creation date:		Thu May  8 04:53:42 GMT+0100 1997
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
* ChangeLog
* $Log$
* Revision 1.1  2011/06/30 16:10:37  rwcox
* Cadd
*
* Revision 1.6  1998/04/27 06:55:01  newt
* tka stuff
*
* Revision 1.5  1998/04/04 06:27:47  newt
* XmHTML Beta 1.1.3
*
* Revision 1.4  1997/10/23 00:24:33  newt
* XmHTML Beta 1.1.0 release
*
* Revision 1.3  1997/08/30 00:23:48  newt
* Removed XmHTML dependencies.
*
* Revision 1.2  1997/08/01 12:52:18  newt
* Progressive image loading changes
*
* Revision 1.1  1997/05/28 01:27:00  newt
* Initial Revision
*
*****/
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef WINNT
#include <unistd.h>
#endif

#ifndef NO_XmHTML			/* defined when compiling for standalone */
#include "toolkit.h"
#include XmHTMLPrivateHeader
#else
#include "ImBuffer.h"		/* ImageBuffer stuff */
typedef unsigned char Boolean;
#define True	1
#define False	0
#endif

#include <LZWStream.h>

/*** External Function Prototype Declarations ***/

/*** Public Variable Declarations ***/
#ifdef DEBUG
int lzw_debug = False;
#endif

/*** Private Datatype Declarations ****/

#define INIT_BITS		9					/* minimum compress codeSize */
#define MAX_LZW_BITS	12					/* maximum GIF LZW codeSize */
#define MAX_BITS		13					/* maximum compress codeSize */
#define MAX_LZW_CODE	(1<<MAX_LZW_BITS)	/* maximum code GIF signal value */
#define MAX_CODE		(1<<MAX_BITS)		/* maximum code signal value */
#define FIRST			257					/* first free slot in compress */
#define CLEAR			256					/* compress clearcode signal */

/****
* Note: we set the maximum bits available for compress to 13 instead of 12.
* This ensures that the compress code storage keeps in sync with the GIF LZW
* codes when the gif code size is less than 8. Using 12 bits for compress
* would overflow the compress tables before the GIF clearCode signal is
* received.
****/

/*** Private Function Prototype Declarations ****/
static int  LZWStreamUncompressData(LZWStream *lzw);
static void LZWStreamPackBits(LZWStream *lzw, int code);
static void LZWStreamConvertBelow8(LZWStream *lzw);
static void LZWStreamConvert8OrAbove(LZWStream *lzw);

/****
* Two macros for buffered output; reduces the no of function calls.
* BUFFERSIZE determines the size of the output buffer and is defined in
* LZWStream.h (default is 512).
****/
/* flush output buffer if it contains something */
#define flushBuf { if(lzw->acount > 0) \
		{ \
			fwrite(lzw->accum, (size_t)1, (size_t)lzw->acount, lzw->f); \
			lzw->acount = 0; \
		} \
}

/* buffer output and flush when full */
#define charOut(C) { lzw->accum[lzw->acount++] = (unsigned char)C; \
	if(lzw->acount >= BUFFERSIZE-1) \
		flushBuf; \
}

#define MAXCODE(n_bits) ((1L << (n_bits)) - 1)

/*** Private Variable Declarations ***/
#ifdef DEBUG
static int bytes_out;
#endif

static char msg_buf[1024];
static char *err_str = "LZWStream Error: ";

/* bit packing masks */
static Byte lmask[9] = {0xff, 0xfe, 0xfc, 0xf8, 0xf0, 0xe0, 0xc0, 0x80, 0x00};
static Byte rmask[9] = {0x00, 0x01, 0x03, 0x07, 0x0f, 0x1f, 0x3f, 0x7f, 0xff};

/*****
* Name: 		LZWStreamGetCode MACRO
* Return Type: 	N/A
* Description: 	gets next code signal from the LZW stream
* In:
*	lzw:		current LZWStream
*	code:		value to update
* Returns:
*	N/A
* Note:
*	This is a macro instead of a function, since this is used for every
*	byte of data...
*****/
#define LZWStreamGetCode(lzw,code) do { \
	register int i, j; \
	code = 0; \
	if(lzw->error) \
	{ \
		lzw->error = 0; \
		code = lzw->clearCode; \
	} \
	else \
	{ \
		if((lzw->curBit + lzw->codeBits) >= lzw->lastBit) \
		{ \
			int count; \
			if(lzw->done) \
				code = -1; \
			else \
			{ \
				lzw->buf[0] = (int)((Byte)lzw->buf[lzw->lastByte-2]); \
				lzw->buf[1] = (int)((Byte)lzw->buf[lzw->lastByte-1]); \
				if((count = (int)lzw->getData(lzw->ib, \
					(Byte*)&(lzw->buf[2]))) == 0) \
					lzw->done = 1; \
				lzw->lastByte = 2 + count; \
				lzw->curBit = (lzw->curBit - lzw->lastBit) + 16; \
				lzw->lastBit = (2+count)*8 ; \
			} \
		} \
		if(code == 0) \
			for(i = lzw->curBit, j = 0; j < lzw->codeBits; ++i, ++j) \
				code |= (((int)((Byte)lzw->buf[i/8]) & (1<<(i%8)))!= 0)<<j; \
		lzw->curBit += lzw->codeBits; \
	} \
}while(0)

/*****
* Name: 		LZWStreamUncompressData
* Return Type: 	int
* Description:  calls "uncompress" to uncompress the LZW-compressed GIF raster
*				data
* In:
*	lzw:		current LZWStream
* Returns:
*	True upon success, False on error.
*****/
static int
LZWStreamUncompressData(LZWStream *lzw)
{
	/* no error */
	lzw->err_msg = NULL;

	if(lzw->zPipe == NULL)
	{
		/* flush output file */
		fflush(lzw->f);

		/* call uncompress on our converted GIF lzw data */
		if(system(lzw->zCmd))
		{
			sprintf(msg_buf, "%sCouldn't exec '%s'.", err_str,
				lzw->zCmd);
			lzw->err_msg = msg_buf;
			unlink(lzw->zName);
			lzw->error = True;
			return(False);
		}
		/* open the output file */
		lzw->zName[strlen(lzw->zName) - 2] = '\0';
		if((lzw->zPipe = fopen(lzw->zName, "r")) == NULL)
		{
			sprintf(msg_buf, "%sCouldn't open uncompress file '%s'. "
				"Corrupt data?", err_str, lzw->zName);
			lzw->err_msg = msg_buf;
			unlink(lzw->zName);
			lzw->error = True;
			return(False);
		}
	}
	lzw->uncompressed = True;
	return(True);
}

/*****
* Name: 		LZWStreamPackBits
* Return Type: 	void
* Description: 	buffers LZW codes and writes them out when the buffer is full.
* In:
*	lzw:		current LZWStream
*	code:		LZW code to be written.
* Returns:
*	nothing
* Note:
*	This routine is for gif images with less than 8 bits per pixel (= codeSize)
*	and packs the LZW codes the same way compress does. This is ABSOLUTELY
*	REQUIRED since compress only deals with codes between 8 and 15 bits.
*	This piece of code has been lifted almost literally from the public domain
*	compress.
*****/
static void
LZWStreamPackBits(LZWStream *lzw, int code)
{
	register int r_off = lzw->offset;	/* current offset in output buffer */
	register int bits = lzw->n_bits;	/* bits per pixel */
	register char *bp = lzw->outBuf;	/* ptr to current pos in buffer */

#ifdef DEBUG
    static int col = 0;
	if(lzw_debug == 2)
	    fprintf(stderr, "%5d%c", code,
		    (col+=6) >= 74 ? (col = 0, '\n') : ' ' );
#endif /* DEBUG */

	if(code >= 0)
	{
		/* Get to the first byte. */
		bp += (r_off >> 3);
		r_off &= 7;
		/*
		* Since code is always >= 8 bits, only need to mask the first
		* hunk on the left.
		*/
		*bp = (*bp & rmask[r_off]) | ((code << r_off) & lmask[r_off]);
		bp++;
		bits -= (8 - r_off);
		code >>= 8 - r_off;

		/* Get any 8 bit parts in the middle (<=1 for up to 16 bits). */
		if(bits >= 8)
		{
			*bp++ = code;
			code >>= 8;
			bits -= 8;
		}
		/* Last bits. */
		if(bits)
			*bp = code;

		lzw->offset += lzw->n_bits;
		if(lzw->offset == (lzw->n_bits << 3))
		{
			bp = lzw->outBuf;
			bits = lzw->n_bits;
#ifdef DEBUG
			bytes_out += bits;
#endif
			do
			{
				charOut(*bp++);
			}
			while(--bits);
			lzw->offset = 0;
		}

		/*
		* If the next entry is going to be too big for the code size,
		* then increase it, if possible.
		*/
		if(lzw->freeEntry > lzw->maxcode8 || lzw->clearFlag)
		{
			/* flush contents of output buffer */
			flushBuf;

			/*
			* Write the whole buffer, because the input side won't
			* discover the size increase until after it has read it.
			*/
		    if(lzw->offset > 0)
			{
				fwrite(lzw->outBuf, 1, lzw->n_bits, lzw->f);
#ifdef DEBUG
				bytes_out += lzw->n_bits;
#endif
		    }
		    lzw->offset = 0;

			if(lzw->clearFlag)
			{
				lzw->maxcode8 = MAXCODE(lzw->n_bits = INIT_BITS);
				lzw->clearFlag = False;
			}
			else
			{
				lzw->n_bits++;
				if (lzw->n_bits == MAX_BITS)
					lzw->maxcode8 = MAX_CODE;
				else
					lzw->maxcode8 = MAXCODE(lzw->n_bits);
			}
#ifdef DEBUG
			if(lzw_debug == 2)
			{
				fprintf(stderr, "\nChange to %d bits\n", lzw->n_bits );
				col = 0;
			}
#endif /* DEBUG */
		}
	}
	/* flush rest of buffer at EOF */
	else
	{
		/* flush contents of output buffer */
		flushBuf;

#ifdef DEBUG
		if(lzw_debug == 2)
		    fprintf(stderr, "\n" );
#endif /* DEBUG */
		if(lzw->offset > 0)
		{
			/* flush out remaining chars */
			fwrite(lzw->outBuf, 1, (lzw->offset+7)/8, lzw->f);

#ifdef DEBUG
			bytes_out += (lzw->offset + 7)/8;
#endif
			lzw->offset = 0;
			fflush(lzw->f);
		}
	}
}

/*****
* Name: 		LZWStreamConvertBelow8
* Return Type: 	void
* Description: 	converts the GIF LZW compressed raster data to the compress
*				compressed data format. codeSize < 8
* In:
*	lzw:		current LZWStream object
* Returns:
*	nothing
*****/
static void
LZWStreamConvertBelow8(LZWStream *lzw)
{
	int inCode, outCode;	/* input and output codes */
	Boolean eod = False;	/* set when end-of-data is reached */
	Boolean first;			/* indicates first code word after clear */
	int offset;

#ifdef DEBUG
	bytes_out = 3;	/* includes 3-byte header mojo */
#endif

	/* clear table */
	first = True;
	offset = 255 - lzw->clearCode;
	lzw->error = True;			/* skips first clearCode in raster data */
	lzw->nextCode = lzw->maxCode;
	lzw->freeEntry = FIRST;

	/*
	* mimic compress behaviour but get LZW code instead of computing it.
	* GIF LZW code needs to be corrected to match fixed compress codes so we
	* can properly reset the tables if required. We also must keep track of
	* the LZW codesize since it influences the values of the LZW codes.
	*/
	while(!eod)
	{
		LZWStreamGetCode(lzw,inCode);
		if(inCode == -1)
			break;
		/*****
		* First part: verify and adjust GIF LZW code signal.
		*****/

		/*
		* clearCode sets everything back to their initial values and then
		* reads the next code.
		*/
		if(inCode == lzw->clearCode)
		{
			/* reset code table */
			lzw->codeBits    = lzw->codeSize + 1;
			lzw->clearCode   = 1 << lzw->codeSize;
			lzw->endCode     = lzw->clearCode + 1;
			lzw->maxCodeSize = 2*lzw->clearCode;
			lzw->maxCode     = lzw->clearCode + 2;
			lzw->nextCode    = lzw->maxCode - 1;
			offset = 255 - lzw->clearCode;

			/* keep output side in sync */
			if(first)
				first = False;
			else
			{
				lzw->freeEntry = FIRST;
				lzw->clearFlag = True;
				LZWStreamPackBits(lzw, CLEAR);
#ifdef DEBUG
				if(lzw_debug == 2)
					fprintf(stderr, "clear\n");
#endif /* DEBUG */
			}
			/* get next code */
			do
			{
				LZWStreamGetCode(lzw, inCode);
				if (inCode == -1)
				{
					/* end-of-data, break out of it */
					eod = True;
					inCode = lzw->endCode;
					break;
				}
			}while(inCode == lzw->clearCode);
		}

		/* End-of-data, compress doesn't have this , so just break */
		if(inCode == lzw->endCode)
		{
			outCode = 0;
			eod = True;
			break;
		}
		/*
		* Check input code size and adjust if the next code would overflow
		* the LZW tables. Compressed GIF raster data uses a range starting
		* at codeSize + 1 up to 12 bits per code.
		*/
		if((++lzw->nextCode >= lzw->maxCodeSize) &&
			(lzw->maxCodeSize < MAX_LZW_CODE))
		{
			lzw->maxCodeSize *= 2;
			lzw->codeBits++;
		}

		/*****
		* Second part: write corrected LZW code
		*
		* write output block: the output buffer contains a series of bytes
		* with lengths anywhere between 3 and 12. Compress can only handle
		* codeSize 8 or up, anything below must be packed according to the
		* format compress expects.
		* This was the most difficult part to figure out, but its as simple
		* as this: the GIF lzw codes are just increased with the difference
		* of compress clearCode (fixed value of 256) and the current GIF
		* clearCode...
		*****/

		/* correct and flush */
		LZWStreamPackBits(lzw,
			inCode < lzw->clearCode ? inCode : inCode + offset);

		if(lzw->freeEntry < MAX_CODE)
			lzw->freeEntry++;
	}
	/* flush output buffer */
	LZWStreamPackBits(lzw, -1);

	/* and close output file */
	fflush(lzw->f);
	fclose(lzw->f);
	lzw->f = NULL;
}

/*****
* Name: 		LZWStreamConvert8OrAbove
* Return Type: 	void
* Description: 	converts the GIF LZW compressed raster data to the compress
*				compressed data format. codeSize >= 8
* In:
*	lzw:		current LZWStream object
* Returns:
*	nothing
*****/
static void
LZWStreamConvert8OrAbove(LZWStream *lzw)
{
	int outBuf[8];
	int inCode, outCode;	/* input and output codes */
	int outCodeBits;		/* size of output code */
	int maxOutSize;			/* maximum output signal */
	Boolean eod = False;	/* set when end-of-data is reached */
	Boolean clear;			/* set if table needs to be cleared */
	Boolean first;			/* indicates first code word after clear */
	int i, j;
	int outData, outBits;

#ifdef DEBUG
	bytes_out = 3;	/* includes 3-byte header mojo */
#endif

	/* init output side */
	outCodeBits = lzw->codeBits;
	maxOutSize  = 2*lzw->clearCode;

	/* clear table */
	first = True;
	lzw->error = True;		/* skips first clearCode in raster data */
	lzw->nextCode = lzw->maxCode - 1;
	clear = False;

	do
	{
		/* create output buffer */
		for (i = 0; i < 8; ++i)
		{
			/* check for table overflow */
			if(lzw->nextCode + 1 > 0x1001)
			{
				/* clear string table */
				inCode = 256;
			}
			/* read input code */
			else
			{
				do
				{
					LZWStreamGetCode(lzw, inCode);
					if (inCode == -1)
					{
						eod = True;
						inCode = 0;
					}
				}while (first && inCode == lzw->clearCode);
				first = False;
			}

			/*
			* code signal less than clear code signal: compressed data,
			* store it.
			*/
			if (inCode < lzw->clearCode)
			{
				outCode = inCode;
			}
			/*
			* code signal equals clear code. Set flag so string table will
			* be cleared. GIF clearCode signal value is dynamic, whereas
			* compress uses a fixed value of 256.
			*/
			else if (inCode == lzw->clearCode)
			{
				outCode = 256;
				clear = True;
				first = False;
			}
			/*
			* end code signal. Compress does not have this signal so just set
			* the end-of-data flag.
			*/
			else if (inCode == lzw->endCode)
			{
				outCode = 0;
				eod = True;
			}
			else
				/*
				* code signal higher than end code signal: compressed data,
				* store it.
				*/
				outCode = inCode - 1;

			outBuf[i] = outCode;

			/*
			* check input code size. Compressed GIF raster data uses a range
			* starting at codeSize + 1 up to 12 bits per code.
			*/
			if((++lzw->nextCode >= lzw->maxCodeSize) &&
				(lzw->maxCodeSize < MAX_LZW_CODE))
			{
				lzw->maxCodeSize *= 2;
				++lzw->codeBits;
			}

			/* check for eod/clear */
			if (eod)
			{
				break;
			}
			if(clear)
			{
				i = 8;
				break;
			}
		}

		/*
		* write output block: the output buffer contains a series of bytes
		* with lengths anywhere between 8 and 12. The output buffer is just
		* packed in the same way an LZW gif *writer* does it.
		*/
		outBits = 0;	/* max output code */
		outData = 0;	/* temporary output buffer */
		j = 0;
		while(j < i || outBits > 0)
		{
			/* package all codes */
			if(outBits < 8 && j < i)
			{
				outData = outData | (outBuf[j++] << outBits);
				outBits += outCodeBits;
			}
			charOut(outData & 0xff);
			outData >>= 8;
			outBits -= 8;
		}

#ifdef DEBUG
		bytes_out += outCodeBits;
#endif
		if(lzw->nextCode - 1 == maxOutSize)
		{
			outCodeBits = lzw->codeBits;
			maxOutSize *= 2;
		}

		/* clear table if necessary */
		if(clear)
		{
			lzw->codeBits    = lzw->codeSize + 1;
			lzw->clearCode   = 1 << lzw->codeSize;
			lzw->endCode     = lzw->clearCode + 1;
			lzw->maxCodeSize = 2*lzw->clearCode;
			lzw->maxCode     = lzw->clearCode + 2;
			lzw->nextCode    = lzw->maxCode - 1;
			maxOutSize       = 2*lzw->clearCode;
			outCodeBits      = lzw->codeBits;
			clear = False;
		}
	}while (!eod);

	/* flush any chars left */
	flushBuf;

	/* and close output file */
	fflush(lzw->f);
	fclose(lzw->f);
	lzw->f = NULL;
}

/*******
*** Public Functions
*******/

/*****
* Name: 		LZWStreamConvert
* Return Type: 	void
* Description: 	LZW decoder driver
* In:
*	lzw:		current LZWStream
* Returns:
*	nothing
*****/
void
LZWStreamConvert(LZWStream *lzw)
{
	if(lzw->codeSize >= 8)
		LZWStreamConvert8OrAbove(lzw);
	else
		LZWStreamConvertBelow8(lzw);
}

/*****
* Name: 		LZWStreamInit
* Return Type: 	int
* Description: 	initializes the given stream
* In:
*	lzw:		current LZWStream
* Returns:
*	0 when codeSize is wrong, -1 when no temporary file could be created,
*	1 otherwise.
*****/
int
LZWStreamInit(LZWStream *lzw)
{
	Byte c;
	int i;

#ifdef DEBUG
	char *chPtr = getenv("LZW_DEBUG");

	if(chPtr == '1')
		lzw_debug = 1;
	else if(chPtr == '2')
		lzw_debug = 2;
#endif
	/* no error */
	lzw->err_msg = NULL;

	/* check if we have the read functions */
	if(lzw->readOK == NULL || lzw->getData == NULL)
	{
		sprintf(msg_buf, "%sno read functions attached!", err_str);
		lzw->err_msg = msg_buf;
		return(-2);
	}

	/* init input side */
	lzw->done = 0;
	lzw->curBit = 0;
	lzw->lastBit = 0;
	lzw->lastByte = 2;

	for(i = 0; i < 280; i++)
		lzw->buf[i] = 0;
	for(i = 0; i < 16; i++)
		lzw->outBuf[i] = 0;

	/* initialize buffered output */
	memset(lzw->accum, '\0', BUFFERSIZE);
	lzw->acount = 0;

	/* close any open files */
	if(lzw->zPipe)
	{
		fclose(lzw->zPipe);
		lzw->zPipe = NULL;
	}
	if(lzw->f)
	{
		fclose(lzw->f);
		lzw->f = NULL;
		unlink(lzw->zName);
	}

	/* no data to uncompress yet */
	lzw->error = False;
	lzw->uncompressed = False;

	/* temporary output file */
	tmpnam(lzw->zName);
	strcat(lzw->zName, ".Z");

	/* open it */
	if(!(lzw->f = fopen(lzw->zName, "w")))
	{
		sprintf(msg_buf, "%scouldn't open temporary file '%s'.",
			err_str, lzw->zName);
		lzw->err_msg = msg_buf;
		return(-1);
	}

	/*
	* get codeSize (= how many bits each pixel takes) from ImageBuffer.
	*/
	if((lzw->readOK(lzw->ib, &c, 1)) == 0)
	{
		sprintf(msg_buf, "%scouldn't read GIF codesize.", err_str);
		lzw->err_msg = msg_buf;
		return(0);
	}

	/* GIF lzw codes */
	lzw->codeSize = (int)((Byte)c);	/* initial bits per pixel */

	/* initialize input side */
	lzw->codeBits    = lzw->codeSize + 1;
	lzw->clearCode   = 1 << lzw->codeSize;
	lzw->endCode     = lzw->clearCode + 1;
	lzw->maxCodeSize = 2*lzw->clearCode;
	lzw->maxCode     = lzw->clearCode + 2;
	lzw->nextCode    = lzw->maxCode;

	/* initialize codeSize < 8 output side */
	lzw->offset    = 0;
	lzw->clearFlag = False;
	lzw->n_bits    = INIT_BITS;
	lzw->maxcode8  = MAXCODE(lzw->n_bits);
	lzw->freeEntry = FIRST;

	/* check clearCode value */
	if(lzw->clearCode >= MAX_LZW_CODE)
	{
		sprintf(msg_buf, "%scorrupt raster data: bad GIF codesize (%i).",
			err_str, lzw->codeSize);
		lzw->err_msg = msg_buf;
		return(0);
	}

	/* Write compress header */
	charOut(0x1f);
	charOut(0x9d);

	/* gif max code length + 1, block mode flag */
	charOut((MAX_BITS & 0x1f) | 0x80);

	return(1);
}

/*****
* Name: 		LZWStreamCreate
* Return Type: 	LZWStream
* Description: 	allocates a new stream
* In:
*	ib:			data input buffer
* Returns:
*	a newly created stream object.
*****/
LZWStream*
LZWStreamCreate(ImageBuffer *ib, char *zCmd)
{
	static LZWStream *lzw;

	/* create a new stream */
	if((lzw = (LZWStream*)calloc(1, sizeof(LZWStream))) == NULL)
		return(NULL);

	/* store ptr to input buffer */
	lzw->ib = ib;

	/* create uncompress command */
	strcpy(lzw->zCmd, zCmd != NULL ? zCmd : "uncompress");
	strcat(lzw->zCmd, "  ");

	/* offset for temporary file in which compressed data is to be stored */
	lzw->zName = lzw->zCmd + strlen(lzw->zCmd);

	/* no default readOK and getData, they need to be set by caller */

	/* and return the new stream */
	return(lzw);
}

/*****
* Name: 		LZWStreamDestroy
* Return Type: 	void
* Description: 	destroys the given LZW stream
* In:
*	lzw:		LZWStream to destroy
* Returns:
*	nothing.
*****/
void
LZWStreamDestroy(LZWStream *lzw)
{
	if(lzw == NULL)
		return;

	if(lzw->zPipe)
		fclose(lzw->zPipe);
	if(lzw->f)
		fclose(lzw->f);
	unlink(lzw->zName);
	free(lzw);
}

/*****
* Name: 		LZWStreamFillBuffer
* Return Type: 	int
* Description:	read uncompressed data from the stream
* In:
*	lzw:		current LZWStream object
*	data:		output buffer
*	size:		size of output buffer
* Returns:
*	no of chars read from stream
*****/
int
LZWStreamFillBuffer(LZWStream *lzw, unsigned char *data, int size)
{
	int n;

	if(lzw->error)
		return(0);

	/* uncompress data if not yet done */
	if(!lzw->uncompressed || lzw->zPipe == NULL)
	{
		if(!(LZWStreamUncompressData(lzw)))
			return(0);
	}
	n = fread(data, 1, size, lzw->zPipe);
	return(n);
}

/*****
* Name: 		LZWStreamUncompress
* Return Type: 	unsigned char *
* Description:	return an allocated buffer with uncompressed stream data
* In:
*	lzw:		current LZWStream
*	size:		size of data read, filled upon return.
* Returns:
*	An allocated buffer when succesfully uncompressed and size is updated to
*	contain the no of characters read. Upon error, NULL is returned and size
*	is set to 0.
*****/
unsigned char *
LZWStreamUncompress(LZWStream *lzw, int *size)
{
	static unsigned char *data;

	*size = 0;

	if(lzw->error)
		return(NULL);

	/* no error */
	lzw->err_msg = NULL;

	/* uncompress data if not yet done */
	if(!lzw->uncompressed || lzw->zPipe == NULL)
	{
		if(!(LZWStreamUncompressData(lzw)))
			return(NULL);
	}

	fseek(lzw->zPipe, 0L, SEEK_END);
	*size = ftell(lzw->zPipe);

	/* sanity check */
	if(*size == 0)
	{
		sprintf(msg_buf, "%szero-length data file.", err_str);
		lzw->err_msg = msg_buf;
		return(NULL);
	}

	rewind(lzw->zPipe);

	data = (unsigned char*)malloc(*size*sizeof(unsigned char));

	/* read it all */
	(void)fread(data, *size, 1, lzw->zPipe);

	/* close and remove file */
	if(lzw->zPipe)
	{
		fclose(lzw->zPipe);
		lzw->zPipe = NULL;
	}
	if(lzw->f)
	{
		fclose(lzw->f);
		lzw->f = NULL;
		unlink(lzw->zName);
	}
	/* and return it */
	return(data);
}

int
LZWStreamGetCodeSize(LZWStream *lzw)
{
	return(lzw->codeSize);
}
