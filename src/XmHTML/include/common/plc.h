/*****
* plc.h : XmHTML progressive object loading interface
*
* This file Version	$Revision$
*
* Creation date:		Tue Jun 10 14:30:39 GMT+0100 1997
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
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Library General Public
* License as published by the Free Software Foundation; either
* version 2 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Library General Public License for more details.
*
* You should have received a copy of the GNU Library General Public
* License along with this library; if not, write to the Free
* Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*
*****/
/*****
* $Source$
*****/
/*****
* ChangeLog 
* $Log$
* Revision 1.1  2011/06/30 16:08:57  rwcox
* Cadd
*
* Revision 1.4  1998/04/27 07:02:47  newt
* tka stuff
*
* Revision 1.3  1998/04/04 06:28:26  newt
* XmHTML Beta 1.1.3
*
* Revision 1.2  1997/08/30 01:26:58  newt
* Small number of bugfixes, mostly wrong types of structure members.
*
* Revision 1.1  1997/08/01 12:51:51  newt
* Initial Revision
*
*****/ 

#ifndef _plc_h_
#define _plc_h_

/* Pull in required declarations */
#ifdef HAVE_LIBPNG
#  include <png.h>
#else				/* fix 10/27/97-01, shl */
#  ifdef HAVE_LIBZ
#    include <zlib.h>
#  endif
#endif

/*****
* png.h includes setjmp.h and issues a cpp error on Linux when it gets
* included more than once...
*****/
#ifdef HAVE_LIBJPEG
#  include <jpeglib.h>
#  ifndef HAVE_LIBPNG	
#    include <setjmp.h>
#  endif
#endif

/* GIF decoder */
#include "LZWStream.h"

/*****
* Definition of the Progressive Loader Context.
* This structure forms the basis of XmHTML's progressive object loading
* mechanism.
* All PLC's in use by a XmHTML widget are represented by a ringbuffer with
* various function pointers. The PLC monitoring routine will circulate this
* buffer using an adjustable interval, calling functions as they are
* necessary.
*****/
/*
* definition of a PLC data manipulation procedure.
* (PLCPtr is typedef'd in XmHTMLP.h)
*/
typedef void (*PLCProc)(PLCPtr);

typedef struct _PLC{
	String		url;				/* object identifier */
	union _PLCObject *object;		/* object-specific data */
	Boolean		obj_set;			/* indicates object type has been set */
	Byte		*buffer;			/* current data */
	Cardinal	buf_size;			/* size of buffer */
	Cardinal	size;				/* size of valid data in buffer */
	Cardinal	left;				/* bytes left in buffer */
	Byte		*next_in;			/* current position in buffer */

	Byte		*input_buffer;		/* input buffer */
	int			input_size;			/* size of input buffer */
	Cardinal	total_in;			/* total number of bytes received so far */
	Cardinal	max_in;				/* get_data() maximum request size */
	Cardinal	min_in;				/* get_data() minimum request size */

	int			plc_status;			/* current PLC status */
	int			plc_data_status;	/* last return value from get_data() */
	Boolean		initialized;		/* indicates object data has been set
									 * and actual processing can begin.
									 */

	XtPointer	priv_data;			/* private PLC data, used by XmHTML */
	XtPointer	user_data;			/* data registered for this PLC */

	struct s_funcs{					/* stream manipulation routines */
				XmHTMLGetDataProc get_data;
				XmHTMLEndDataProc end_data;
				PLCProc c_new;		/* PLCObject initializer */
	}sf;
	PLCProc		init;				/* object initializer function */
	PLCProc		destructor;			/* object destructor */
	PLCProc		transfer;			/* object transfer function */
	PLCProc		finalize;			/* object completion function */

	PLCProc		obj_funcs[3];		/* object manipulation functions */
	int			curr_obj_func;		/* current obj_func */
	Boolean		obj_funcs_complete;	/* obj_func calling flag */

	struct _PLC *self;				/* ptr to self */
	struct _PLC *next_plc;			/* ptr to next PLC */
	struct _PLC *prev_plc;			/* ptr to previous PLC */
}PLC;

/*****
* Explanation of the PLCProc fields.
*
* init():
*	this function is called when the object-specific data should be
*	initialized. When the object is initialized, the initialized field should
*	be set to True. The PLC cycler will call this function as long as the
*	initialized field is False, and the plc_status field is either PLC_ACTIVE
*	or PLC_SUSPEND.
* destructor():
*	this function is called if the object should destroy its own data.
*	It is called when the plc_status field reaches either PLC_COMPLETE or
*	PLC_ABORT. 
* transfer():
*	this function is called whenever an object-specific function returns.
*	The purpose of this function is to signal the application that it
*	can transfer the processed data to its final destination (for images, this
*	should include transfering the newly decoded scanlines to the screen
*	buffer). It is called whenever the PLC cycler returns from any function
*	in the obj_funcs array.
* finalize():
*	this function is called when the plc_status field reaches PLC_COMPLETE
*	(get_data() returned STREAM_END or processing has finished). The
*	application should then save *all* decoded data. The object may *not*
*	destruct itself, the PLC cycler will call the object-specific destructor
*	method when it has called the finalize() function.
*
* The PLCProc array contains object-specific functions.
* For images, only the first slot is used: it is the scanline function.
* curr_obj_func gives the index of the obj_func to call.
* obj_funcs_complete indicates whether or not the PLC cycler should continue
* calling any obj_func. If it is set to True, the cycler will call the
* finalize() PLCProc to allow final processing of the received data.
*****/

/*****
* PLC status flags
*****/
#define PLC_ACTIVE			0		/* PLC is active */
#define PLC_SUSPEND			1		/* PLC has been suspended */
#define PLC_ABORT			2		/* PLC has been aborted */
#define PLC_COMPLETE		3		/* PLC is done */

/*****
* The fun part: PLCObject definitions.
* Each object for which a PLC is to be used has a unique PLCObject definition.
* All objects are grouped in the PLCObject union and each object is identified
* by the type member of this union.
*****/

/*****
* global object
* The buffer and counters in this global object are used for storing the
* entire data as it is being received. For images the buffer and buffer
* counters will contain the compressed image data as it is being received.
* It is free for the decoders to decide whether or not this feature is used.
* (jpeg for example doesn't use it). The routines in plc.c only use the
* ``owner'' field of these fields, so be sure it's valid.
******/
#define plc_common_object_fields \
	Byte type;						/* type of object, may not be modified */ \
	Byte *buffer;					/* destination buffer */ \
	Cardinal buf_size;				/* size of destination buffer */ \
	Cardinal byte_count;			/* number of bytes received so far */ \
	Cardinal buf_pos;				/* current position in buffer */ \
	XmHTMLWidget owner				/* owner of this PLC */

typedef struct{
	plc_common_object_fields;		/* fields common for all PLC structures */
}PLCAny;

/*****
* Common image object fields.
* The common image object fields are divided in two main sections:
* public fields: these fields must be set/updated by the decoder and can
*                be used by the decoder. The image transfer function uses
*                the values of these fields to compose the image itself,
*                and *can* modify the values of data_pos and prev_pos for
*                backtracking purposes.
* private fields: these fields are used by the image transfer function, and
*                may *never* be touched by the decoder.
******/
#define plc_image_public_fields \
	int depth;						/* depth of image */ \
	Byte colorclass;				/* colorclass of image */ \
	Byte transparency;				/* transparency type of image */ \
	XCOLOR *cmap;					/* colormap for this image */ \
	int cmapsize;					/* size of colormap */ \
	int ncolors;					/* original no of colors in image */ \
	Cardinal width;					/* width in pixels */ \
	Cardinal height;				/* height in pixels (= no of scanlines)*/ \
	Cardinal npasses;				/* no of passes required on image data */ \
	Cardinal curr_pass;				/* current pass on data */ \
	Cardinal curr_scanline;			/* current scanline */ \
	Cardinal stride;				/* scanline stride */ \
	Byte *data;						/* raw image data */ \
	int data_size;					/* maximum data size */ \
	int data_pos;					/* current position in data */ \
	int prev_pos					/* last known position in data */

#define plc_image_private_fields \
	int used[XmHTML_MAX_IMAGE_COLORS];		/* array of used colors */ \
	int nused;						/* colors already used */ \
	unsigned long xcolors[XmHTML_MAX_IMAGE_COLORS]; /* arr alloc'd pixels */ \
	int bg_pixel;					/* transparent pixel index */ \
	XCOLOR *bg_cmap;				/* background colormap for this image */ \
	int bg_cmapsize;				/* background colormap size */ \
	PIXMAP pixmap;					/* destination pixmap */ \
	PIXMAP clipmask;				/* destination clipmask */ \
	Byte *clip_data;				/* raw clipmask data */ \
	Byte *scaled_data;				/* scaled image data */ \
	int sc_start;					/* curr. pos in scaled data */ \
	int sc_end;						/* end pos in scaled data */ \
	Boolean is_scaled;				/* True when scaling required */ \
	XImage *ximage;					/* destination image */ \
	XmImageInfo *info;				/* raw image information */ \
	XmHTMLImage *image				/* destination image */

/*****
* Common image object. This structure contains the fields that are common to
* *all* image objects. The main plc code (plc.c) uses this structure to do
* its magic. The image-specific objects are used by the respective decoders.
* Note that all image-specific objects *must* share the common image object.
*****/
typedef struct{
	plc_common_object_fields;		/* fields common for all PLC structures */
	plc_image_public_fields;		/* public  fields for all image objects */
	plc_image_private_fields;		/* private fields for all image objects */
}PLCImage;

/*****
* GIF image object
*****/
typedef struct{
	plc_common_object_fields;		/* fields common for all PLC structures */
	plc_image_public_fields;		/* public  fields for all image objects */
	plc_image_private_fields;		/* private fields for all image objects */

	/* GIF specific data follows */
	Byte gbuf[256];					/* block of compressed raster data */
	Boolean external_codec;			/* True -> uses external decoder */
	
	XmImageGifProc inflate;			/* external gif decoder */
	XmHTMLGIFStream *gstream;		/* GIFStream() stream object */

	ImageBuffer ib;					/* LZWStream data provider */
	LZWStream *lstream;				/* LZWStream() stream object */

}PLCImageGIF;

/*****
* GZF image object
*****/
typedef struct{
	plc_common_object_fields;		/* fields common for all PLC structures */
	plc_image_public_fields;		/* public  fields for all image objects */
	plc_image_private_fields;		/* private fields for all image objects */

	/* GZF specific data follows */
#if defined(HAVE_LIBPNG) || defined(HAVE_LIBZ)
	Byte zbuf[256];					/* block of compressed raster data */
	z_stream zstream;				/* zlib inflate() stream object */
#endif

}PLCImageGZF;

/*****
* JPEG image object
*****/
#ifdef HAVE_LIBJPEG
/* default libjpeg error override */
typedef struct _plc_jpeg_err_mgr{
	struct jpeg_error_mgr pub;		/* jpeg public fields */
	jmp_buf setjmp_buffer;			/* for return to caller */
}plc_jpeg_err_mgr;
	
typedef struct{
	plc_common_object_fields;		/* fields common for all PLC structures */
	plc_image_public_fields;		/* public  fields for all image objects */
	plc_image_private_fields;		/* private fields for all image objects */

	/* JPEG specific data follows */

	Boolean				init;				/* jpeg initialization complete? */
	struct jpeg_decompress_struct cinfo;	/* jpeg decompressor */
	plc_jpeg_err_mgr	jerr;				/* error manager object */
}PLCImageJPEG;

#else	/* !HAVE_LIBJPEG */

/*****
* dummy JPEG image object
*****/
typedef struct{
	plc_common_object_fields;		/* fields common for all PLC structures */
}PLCImageJPEG;

#endif /* HAVE_LIBJPEG */

/*****
* PNG image object
*****/
#ifdef HAVE_LIBPNG

typedef struct{
	plc_common_object_fields;		/* fields common for all PLC structures */
	plc_image_public_fields;		/* public  fields for all image objects */
	plc_image_private_fields;		/* private fields for all image objects */

	/* PNG specific data follows */
}PLCImagePNG;

#else /* !HAVE_LIBPNG */

/*****
* dummy PNG image object
*****/
typedef struct{
	plc_common_object_fields;		/* fields common for all PLC structures */
}PLCImagePNG;

#endif /* HAVE_LIBPNG */

/*****
* XPM image object
*****/
typedef struct{
	plc_common_object_fields;		/* fields common for all PLC structures */
	plc_image_public_fields;		/* public  fields for all image objects */
	plc_image_private_fields;		/* private fields for all image objects */

	/* XPM specific data follows */
}PLCImageXPM;

/*****
* XBM image object
*****/
typedef struct{
	plc_common_object_fields;		/* fields common for all PLC structures */
	plc_image_public_fields;		/* public  fields for all image objects */
	plc_image_private_fields;		/* private fields for all image objects */

	/* XBM specific data follows */
	int raster_length;
	int data_start;
}PLCImageXBM;

/*****
* document object
*****/
typedef struct{
	plc_common_object_fields;		/* fields common for all PLC structures */

	/* Document specific data follows */
}PLCDocument;

/*****
* This is the final PLC Object definition. It is a union of all the above
* object-specific structures. The type of the object in a PLC is identified
* by the value of the "type" member of this union.
*****/
typedef union _PLCObject{
	Byte type;						/* must not be changed, first element */
	PLCAny			plc_any;
	PLCImage		plc_any_image;	/* common object for all image PLC's */
	PLCImageGIF		plc_gif_image;
	PLCImageGZF		plc_gzf_image;
	PLCImagePNG		plc_png_image;
	PLCImageJPEG	plc_jpeg_image;
	PLCImageXPM		plc_xpm_image;
	PLCImageXBM		plc_xbm_image;
	PLCDocument		plc_doc;		/* future extension */
}PLCObject;

/*****
* Possible values for the type field in the PLCObject union.
* anyImage is an internal object used by the main plc code. It is also used
* by the GIF and GZF decoders which have a lot in common: GZF is my own
* GIF format in which only the format of the compressed data differs.
*****/
enum{
	/* 0 is a reserved value */
	plcAny = 1,			/* common object */
	plcAnyImage,		/* common image object */
	plcGIF,				/* gif image */
	plcGZF,				/* gzf image */
	plcPNG,				/* png image */
	plcJPEG,			/* jpeg image */
	plcXPM,				/* xpm image */
	plcXBM,				/* xbm image */
	plcDocument			/* html document */
};

/*****
* Private functions
*****/
/* make a data request */
extern Boolean _PLCDataRequest(PLC *plc);

/* read bytes from current PLC descriptor */
extern size_t _PLCReadOK(PLC *plc, Byte *buf, int size);

/*****
* Read a block of bytes from a PLC descriptor. A block of bytes is
* identified by a byte count followed by a block of data containing 
* byte_count bytes of data (only used for gif and gzf images).
*****/
extern size_t _PLCGetDataBlock(PLC *plc, Byte *buf);

/* rewind the current input buffer */
#define _PLCRewindInputBuffer(PLC)	do{ \
	(PLC)->left = (PLC)->size;		/* no of unprocessed bytes */ \
	(PLC)->next_in = (PLC)->buffer;	/* ptr to last processed byte */ \
}while(0)

/*****
* PLCProc definitions for the progressive gif loader
* defined in readGIFplc.c
*****/
extern void _PLC_GIF_Init(PLC *plc);
extern void _PLC_GIF_Destructor(PLC *plc);
extern void _PLC_GIF_ScanlineProc(PLC *plc);

/*****
* PLCProc definitions for the progressive gzf loader
* defined in readGIFplc.c
*****/
extern void _PLC_GZF_Init(PLC *plc);
extern void _PLC_GZF_Destructor(PLC *plc);
extern void _PLC_GZF_ScanlineProc(PLC *plc);

/*****
* PLCProc definitions for the progressive JPEG loader
* defined in readJPEGplc.c
*****/
extern void _PLC_JPEG_Init(PLC *plc);
extern void _PLC_JPEG_Destructor(PLC *plc);
extern void _PLC_JPEG_ScanlineProc(PLC *plc);

/*****
* PLCProc definitions for the progressive PNG loader
* defined in readPNGplc.c
*****/
#ifdef PLC_PNG
extern void _PLC_PNG_Init(PLC *plc);
extern void _PLC_PNG_Destructor(PLC *plc);
extern void _PLC_PNG_ScanlineProc(PLC *plc);
#endif

/*****
* PLCProc definitions for the progressive XPM loader
* defined in readXPM.c
*****/
extern void _PLC_XPM_Init(PLC *plc);
extern void _PLC_XPM_Destructor(PLC *plc);
extern void _PLC_XPM_ScanlineProc(PLC *plc);

/*****
* PLCProc definitions for the progressive XBM loader
* defined in readBitmap.c
*****/
extern void _PLC_XBM_Init(PLC *plc);
extern void _PLC_XBM_Destructor(PLC *plc);
extern void _PLC_XBM_ScanlineProc(PLC *plc);

/*****
* PLCProc definitions for the progressive document loader.
* defined in plc/format.c
*****/
#ifdef PLC_DOC
extern void _PLC_DOC_Init(PLC *plc);
extern void _PLC_DOC_Destructor(PLC *plc);
extern void _PLC_DOC_DisplayProc(PLC *plc);
#endif /* PLC_DOC */

/* Don't add anything after this endif! */
#endif /* _plc_h_ */
