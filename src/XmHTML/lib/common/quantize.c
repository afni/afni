#ifndef production
static char rcsId[]="$Header$";
#endif
/*****
* quantize.c : XmHTML color quantization routines
*
* This file Version	$Revision$
*
* Creation date:		Fri Sep 26 16:46:35 GMT+0100 1997
* Last modification: 	$Date$
* By:					$Author$
* Current State:		$State$
*
* Author:				newt
*
* Copyright (C) 1994-1997 by Ripley Software Development 
* All Rights Reserved
*
* This file is part of the XmHTML Widget library.
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
* The following copyright notice applies to ppm_quant and its supporting
* functions:
*
* ppmquant.c - quantize the colors in a pixmap down to a specified number
*
* Copyright (C) 1989, 1991 by Jef Poskanzer.
*
* Permission to use, copy, modify, and distribute this software and its
* documentation for any purpose and without fee is hereby granted, provided
* that the above copyright notice appear in all copies and that both that
* copyright notice and this permission notice appear in supporting
* documentation.  This software is provided "as is" without express or
* implied warranty.
*
* It also contains the following note to the above:
*
* Many people get confused by this legalese, especially the part about
* "without fee".  Does this mean you can't charge for any product that
* uses PBMPLUS?  No.  All it means is that you don't have to pay me.
* You can do what you want with this software.  Build it into your
* package, steal code from it, whatever.  Just be sure to let people
* know where it came from.
*
*****/
/*****
* ChangeLog 
* $Log$
* Revision 1.1  2011/06/30 16:10:38  rwcox
* Cadd
*
* Revision 1.7  1998/04/27 07:02:49  newt
* tka stuff
*
* Revision 1.6  1998/04/04 06:28:27  newt
* XmHTML Beta 1.1.3
*
* Revision 1.5  1997/10/23 00:25:17  newt
* XmHTML Beta 1.1.0 release
*
*****/ 
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define _WANT_TIMINGS	/* we want timings, see debug.h */

#include "toolkit.h"
#include XmHTMLPrivateHeader

#ifdef HAVE_XCCP_H
#include "XCCP.h"
#endif

/*** External Function Prototype Declarations ***/

/*** Public Variable Declarations ***/

/*** Private Datatype Declarations ****/
typedef struct{
	Byte r;
	Byte g;
	Byte b;
}pixel;

/* Color histogram stuff. */
struct chist_item{
	pixel color;
	int value;
};

struct chist_list_item{
	struct chist_item ch;
	struct chist_list_item *next;
};

struct box{
  int index;
  int colors;
  int sum;
};

typedef struct chist_item* chist_vec;
typedef struct chist_list_item* chist_list;
typedef chist_list* chash_table;
typedef struct box* box_vector;

/*** Private Function Prototype Declarations ****/
static void my_bcopy(char *src, char *dst, size_t len);

/* ppm_quant supporting functions */
static void ppm_freechist(chist_vec);
static void ppm_freechash(chash_table);
static int redcompare(const void*, const void*);
static int greencompare(const void*, const void*);
static int bluecompare(const void*, const void*);
static int sumcompare(const void*, const void*);
static chist_vec   mediancut(chist_vec, int, int, int, int);
static chist_vec   ppm_computechist(pixel **, int, int, int, int*);
static chist_vec   ppm_chashtochist(chash_table, int);
static chash_table ppm_computechash(pixel **, int, int, int, int*);
static chash_table ppm_allocchash(void);

/* the quantizer itself */
static int ppm_quant(Byte *pic24, pixel **pix, XmHTMLRawImageData *img_data,
	int max_colors);

/* fast rgb converter */
static Boolean QuickRGB(Byte *rgb, XmHTMLRawImageData *img_data,
	int max_colors);

/* fast quantizer to a fixed palette */
static void QuickQuantize(Byte *rgb, XmHTMLRawImageData *img_data);

/* various macros used by the quantization routines */
#define PPM_GETR(p) ((p).r)
#define PPM_GETG(p) ((p).g)
#define PPM_GETB(p) ((p).b)

#define PPM_ASSIGN(p,red,grn,blu) \
  { (p).r = (red); (p).g = (grn); (p).b = (blu); }

/* color compare */
#define PPM_EQUAL(p,q) ( (p).r == (q).r && (p).g == (q).g && (p).b == (q).b )

/* Color scaling macro -- to make writing ppmtowhatever easier. */
#define PPM_DEPTH(newp,p,oldmaxval,newmaxval) \
    PPM_ASSIGN( (newp), \
	        (int) PPM_GETR(p) * (newmaxval) / ((int)oldmaxval), \
	        (int) PPM_GETG(p) * (newmaxval) / ((int)oldmaxval), \
	        (int) PPM_GETB(p) * (newmaxval) / ((int)oldmaxval) )

/* Luminance macro, using only integer ops.  Returns an int (*256) */
#define PPM_LUMIN(p) \
  ( 77 * PPM_GETR(p) + 150 * PPM_GETG(p) + 29 * PPM_GETB(p) )

/* compute a hashvalue */
#define ppm_hashpixel(p) ((((int)PPM_GETR(p) * 33023 + \
	(int)PPM_GETG(p) * 30013 + (int)PPM_GETB(p) * 27011) & 0x7fffffff) \
	% HASH_SIZE)

/*** Private Variable Declarations ***/
#define MAXCOLORS 32767		/* max. no of colors to take into account */
#define HASH_SIZE 6553		/* color hashtable size */

/*****
* defines & macros for quick 24bit (RGB) image to 8bit (paletted) image
* We use a 3/3/2 colorcube.
*****/
#define RMASK		0xe0
#define RSHIFT		0
#define GMASK		0xe0
#define GSHIFT		3
#define BMASK		0xc0
#define BSHIFT		6

/* division speedup: multiply by this instead of dividing by 16 */
#define SIXTEENTH	0.0625

/*****
* Name: 		my_bcopy
* Return Type: 	void
* Description: 	safe bcopy, areas may overlap.
* In: 
*	src:		data to be copied;
*	dst:		destination;
*	len:		no of bytes to copy;
* Returns:
*	nothing.
*****/
static void
my_bcopy(char *src, char *dst, size_t len)
{
	/* areas are the same */
	if(src == dst || len <= 0)
		return;
  
	if(src < dst && src+len > dst)
	{
		/* do a backward copy */
		src = src + len - 1;
		dst = dst + len - 1;
		for(; len > 0; len--, src--, dst--)
			*dst = *src;
	}

	else
	{
		/* do a forward copy */
		for(; len > 0; len--, src++, dst++)
			*dst = *src;
	}
}

/*****
* Name:			QuickRGB
* Return Type: 	Boolean
* Description: 	attemps a quick 24 to 8 bit image conversion
* In: 
*	rgb:		raw image data, RGB format;
*	img_data:	destination;
*	max_colors:	maximum no of colors allowed;
* Returns:
*	True when image was converted, False if not.
*****/
static Boolean 
QuickRGB(Byte *rgb, XmHTMLRawImageData *img_data, int max_colors)
{
	unsigned long colors[XmHTML_MAX_IMAGE_COLORS],col;
	int i, num_colors, low, high, mid, width, height;
	Byte *p, *pix;

	SetTimer;

	width  = img_data->width;
	height = img_data->height;

	/* put the first color in the table by hand */
	num_colors = 0;
	mid = 0;  

	for(i = width*height, p = rgb; i; i--)
	{
		/* make truecolor pixel val */
		col  = (((Pixel) *p++) << 16);  
		col += (((Pixel) *p++) << 8);
		col +=  *p++;

		/* binary search the 'colors' array to see if it's in there */
		low = 0;
		high = num_colors - 1;
		while (low <= high)
		{
			mid = (low+high)/2;
			if(col < colors[mid])
				high = mid - 1;
			else if(col > colors[mid])
				low = mid + 1;
			else
				break;
		}

		if(high < low)
		{
			/* didn't find color in list, add it. */
			if(num_colors >= max_colors)
			{
				ShowTimer(13, ("quantize.c: QuickRGB end, more colors than "
					"allowed."));
				return(False);	/* more colors than allowed */
			}
			my_bcopy((char *)&colors[low], (char*)&colors[low+1],
				(num_colors - low) * sizeof(unsigned long));
			colors[low] = col;
			num_colors++;
		}
	}

	/* Pixelize data: map pixel values of the RGB image in the colormap */
	for(i = width*height, p = rgb, pix = img_data->data; i; i--,pix++)
	{
		col  = (((Pixel)*p++) << 16);  
		col += (((Pixel)*p++) << 8);
		col +=  *p++;

		/* binary search the 'colors' array.  It *IS* in there */
		low = 0;
		high = num_colors - 1;
		while(low <= high)
		{
			mid = (low+high)/2;
			if(col < colors[mid])
				high = mid - 1;
			else if(col > colors[mid])
				low = mid + 1;
			else
				break;
		}
		*pix = mid;
	}

	/* allocate colormap */
	AllocRawImageCmap(img_data, num_colors);

	/* and fill it */
	for(i = 0; i < num_colors; i++)
	{
		GETR(img_data->cmap[i]) = (( colors[i] >> 16)) << 8;
		GETG(img_data->cmap[i]) = (((colors[i] >> 8 ) & 0xff)) << 8;
		GETB(img_data->cmap[i]) = (( colors[i]  & 0xff)) << 8;
	}
	ShowTimer(13, ("quantize.c: QuickRGB success."));

	return(True);
}

/*****
* Name: 		QuickQuantize
* Return Type: 	Byte*
* Description: 	quick conversion of RGB image with > 256 colors to a
*				paletted image using a 256 colors imagemap.
* In: 
*	rgb:		rgb image data (3 bytes per pixel, rgb order), pixel 0 at
*				top left corner;
*	data:		buffer for reduced image, filled upon return.
*	width,height:
*				image dimensions;
*	cols:		colormap, filled upon return;
* Returns:
*	8bit pixelized image data. Palette data in *cols.
* Note:
*	This routine does a very quick quantization using a fixed palette and is
*	based on the routine quick_quant as found in xv28to8.c from xv-3.10 by
*	John Bradley. Used by permission.
*****/
static void
QuickQuantize(Byte *rgb, XmHTMLRawImageData *img_data)
{
	Byte *pp;
	int r, g, b;	/* RGB color components */
	int *thisLine, *nextLine, *thisPtr, *nextPtr, *tmpPtr;
	int i, j, val, size;
	int imax, jmax;
	int width  = img_data->width;
	int height = img_data->height;
	XCOLOR *cols;

	SetTimer;

	pp = img_data->data;
	size = width*3;	/* a single RGB scanline */
	imax = height-1;
	jmax = width-1;

	/* allocate a full colormap for this image */
	AllocRawImageCmap(img_data, XmHTML_MAX_IMAGE_COLORS);
	cols = img_data->cmap;

	/* fill colormap */
	for(i = 0; i < XmHTML_MAX_IMAGE_COLORS; i++)
	{
		GETR(cols[i]) =(((i << RSHIFT) & RMASK)*255 + 0.5*RMASK)/RMASK;
		GETG(cols[i]) =(((i << GSHIFT) & GMASK)*255 + 0.5*GMASK)/GMASK;
		GETB(cols[i]) =(((i << BSHIFT) & BMASK)*255 + 0.5*BMASK)/BMASK;
	}

	/* temporary work memory */
	thisLine = (int*)malloc(size*sizeof(int));
	nextLine = (int*)malloc(size*sizeof(int));

	/* get first line of picture */
	for(j = size, tmpPtr = nextLine; j; j--)
		*tmpPtr++ = (int)*rgb++;

	for(i = 0; i < height; i++)
	{
		tmpPtr = thisLine;
		thisLine = nextLine;
		nextLine = tmpPtr;	/* swap */

		/* get next line */
		if(i != imax)
			for(j = size, tmpPtr = nextLine; j; j--)
				*tmpPtr++ = (int)*rgb++;

		/* convert RGB scanline to indexed scanline */
		for(j = 0, thisPtr = thisLine, nextPtr = nextLine; j < width; 
			j++, pp++)
		{
			/* get RGB values */
			r = *thisPtr++;
			g = *thisPtr++;
			b = *thisPtr++;

			/* check validity of component ranges */
			RANGE(r,0,255);
			RANGE(g,0,255);
			RANGE(b,0,255);  
      
			/* choose actual pixel value */
			val = (((r & RMASK) >> RSHIFT) | ((g & GMASK) >> GSHIFT) |
				((b & BMASK) >> BSHIFT));
			*pp = val;
      
			/* compute color errors */
			r -= GETR(cols[val]);
			g -= GETG(cols[val]);
			b -= GETB(cols[val]);

			/* Add error fractions to adjacent pixels using a 3x3 matrix. */

			/* adjust RIGHT pixel */
			if(j != jmax)
			{
				thisPtr[0] += SIXTEENTH * (r*7); 
				thisPtr[1] += SIXTEENTH * (g*7); 
				thisPtr[2] += SIXTEENTH * (b*7);
			}

			/* do BOTTOM pixel */
			if(i != imax)
			{
				nextPtr[0] += SIXTEENTH * (r*5);
				nextPtr[1] += SIXTEENTH * (g*5);
				nextPtr[2] += SIXTEENTH * (b*5);

				/* do BOTTOM LEFT pixel */
				if(j > 0)
				{
					nextPtr[-3] += SIXTEENTH * (r*3);
					nextPtr[-2] += SIXTEENTH * (g*3);
					nextPtr[-1] += SIXTEENTH * (b*3);
				}
				/* do BOTTOM RIGHT pixel */
				if(j != jmax)
				{
					nextPtr[3] += SIXTEENTH * (r);
					nextPtr[4] += SIXTEENTH * (g);
					nextPtr[5] += SIXTEENTH * (b);
				}
				nextPtr += 3;
			}
		}
	}
	/* free it */
	free(thisLine);
	free(nextLine);

	/* upscale colormap */
	for(i = 0; i < XmHTML_MAX_IMAGE_COLORS; i++)
	{
		GETR(cols[i]) <<= 8;
		GETG(cols[i]) <<= 8;
		GETB(cols[i]) <<= 8;
	}

	ShowTimer(13, ("quantize.c: QuickQuantize end."));
}

/***************************************************************/
/* The following code based on code from the 'pbmplus' package */
/* written by Jef Poskanzer                                    */
/***************************************************************/
static int
ppm_quant(Byte *pic24, pixel **pix, XmHTMLRawImageData *img_data,
	int max_colors)
{
	pixel **pixels;
	chist_vec chv, colormap;
	chash_table cht;
	int i, cols, row, rows, colors;
	Byte *pic8 = img_data->data;
	Byte *picptr, maxval, newmaxval;

	register int index, col, limitcol;
	register pixel *pP;

	SetTimer;

	index = 0;
	maxval = 255;
	cols = img_data->width;
	rows = img_data->height;

	/* reformat RGB image data into a 2D array of pixel values */
	if(pix == (pixel**)NULL)
	{
		pixels = (pixel **)malloc(rows * sizeof(pixel*));

		for(row = 0; row < rows; row++)
		{
			pixels[row] = (pixel *) malloc(cols * sizeof(pixel));

			for(col=0, pP=pixels[row]; col<cols; col++, pP++)
			{
				pP->r = *pic24++;
				pP->g = *pic24++;
				pP->b = *pic24++;
			}
		}
	}
	else
		pixels = pix;

	/* create unclustered color histogram */
	for( ; ; )
	{
		chv = ppm_computechist(pixels, cols, rows, MAXCOLORS, &colors);
		if(chv != (chist_vec) 0)
			break;
    
		newmaxval = maxval / 2;

		for(row=0; row<rows; ++row)
			for(col=0, pP=pixels[row]; col<cols; ++col, ++pP)
				PPM_DEPTH( *pP, *pP, maxval, newmaxval );
		maxval = newmaxval;
	}

	/* Step 3: apply median-cut to histogram, making the new colormap. */
	colormap = mediancut(chv, colors, rows * cols, maxval, max_colors);

	/* free histogram */
	ppm_freechist(chv);

	/*****
	* Step 4: map the colors in the image to their closest match in the
	* new colormap, and write 'em out.
	*****/
	cht = ppm_allocchash();

	picptr = pic8;
	for(row = 0;  row < rows;  ++row)
	{
		col = 0;  limitcol = cols;  pP = pixels[row];

		do
		{
			int hash;
			chist_list chl;

			/* Check hash table to see if we have already matched this color. */
			hash = ppm_hashpixel(*pP);

			/* check for collisions */
			for(chl = cht[hash];  chl; chl = chl->next)
			{
				if(PPM_EQUAL(chl->ch.color, *pP))
				{
					index = chl->ch.value;
					break;
				}
			}

			if(!chl)
			{
				/* Not preset, search colormap for closest match. */
				register int i, r1, g1, b1, r2, g2, b2;
				register long dist, newdist;

				r1 = PPM_GETR(*pP);
				g1 = PPM_GETG(*pP);
				b1 = PPM_GETB(*pP);
				dist = 2000000000;

				for(i=0; i<max_colors; i++)
				{
					r2 = PPM_GETR(colormap[i].color);
					g2 = PPM_GETG(colormap[i].color);
					b2 = PPM_GETB(colormap[i].color);

					newdist = (r1 - r2)*(r1 - r2) +
						(g1 - g2)*(g1 - g2) + (b1 - b2)*(b1 - b2);

					if(newdist<dist)
					{
						index = i;
						dist = newdist;
					}
				}
				hash = ppm_hashpixel(*pP);
				chl = (chist_list)malloc(sizeof(struct chist_list_item));
				chl->ch.color = *pP;
				chl->ch.value = index;
				chl->next = cht[hash];
				cht[hash] = chl;
			}
			*picptr++ = index;
			++col;
			++pP;
		}
		while (col != limitcol);
	}
	/* free the pixels array */
	for(i = 0; i < rows; i++)
		free(pixels[i]);
	free(pixels);

	/* rescale colormap and save return colormap */
	if(img_data->cmapsize)
		free(img_data->cmap);
	AllocRawImageCmap(img_data, max_colors);
	for(i = 0; i < max_colors; i++)
	{
	    PPM_DEPTH(colormap[i].color, colormap[i].color, maxval, 255);
		GETR(img_data->cmap[i]) = (PPM_GETR(colormap[i].color)) << 8;
		GETG(img_data->cmap[i]) = (PPM_GETG(colormap[i].color)) << 8;
		GETB(img_data->cmap[i]) = (PPM_GETB(colormap[i].color)) << 8;
		GETP(img_data->cmap[i]) = i;
	}

	/* free cht and colormap */
	ppm_freechist(colormap);
	ppm_freechash(cht);

	ShowTimer(13, ("quantize.c: ppm_quant end."));

	return(0);
}

/*****
* Name: 		mediancut
* Return Type: 	chist_vec
* Description:	Here is the fun part, the median-cut colormap generator.
*				This is based on Paul Heckbert's paper "Color Image
*				Quantization for Frame Buffer Display",
*				SIGGRAPH '82 Proceedings, page 297.
* In: 
*
* Returns:
*
*****/
static chist_vec
mediancut(chist_vec chv, int colors, int sum, int maxval, int max_colors)
{
	chist_vec colormap;
	box_vector bv;
	int boxes, rl, gl, bl;
	pixel p;
	register int bi, i;

	bv = (box_vector)malloc(sizeof(struct box) * max_colors);
	colormap = (chist_vec)malloc(sizeof(struct chist_item) * max_colors);

	/* reset to zero */
	for(i=0; i<max_colors; i++)
		PPM_ASSIGN(colormap[i].color, 0, 0, 0);

	/* Set up the initial box. */
	bv[0].index = 0;
	bv[0].colors = colors;
	bv[0].sum = sum;
	boxes = 1;

	/* Main loop: split boxes until we have enough. */
	while(boxes < max_colors )
	{
		int sm, halfsum, lowersum;
		register int minr, maxr, ming, maxg, minb, maxb, v;
		register int indx, clrs;

		/* Find the first splittable box. */
		for(bi = 0; bv[bi].colors < 2 && bi < boxes; bi++);

		if(bi == boxes)
			break;	/* ran out of colors! */

		indx = bv[bi].index;
		clrs = bv[bi].colors;
		sm = bv[bi].sum;

		/*****
		* Go through the box finding the minimum and maximum of each
		* component - the boundaries of the box.
		*****/
		minr = maxr = PPM_GETR( chv[indx].color );
		ming = maxg = PPM_GETG( chv[indx].color );
		minb = maxb = PPM_GETB( chv[indx].color );

		for(i = 1; i < clrs; i++)
		{
			v = PPM_GETR( chv[indx + i].color );
			if (v < minr) minr = v;
			if (v > maxr) maxr = v;

			v = PPM_GETG( chv[indx + i].color );
			if (v < ming) ming = v;
			if (v > maxg) maxg = v;

			v = PPM_GETB( chv[indx + i].color );
			if (v < minb) minb = v;
			if (v > maxb) maxb = v;
		}

		/* Find the largest dimension, and sort by that component. */
		PPM_ASSIGN(p, maxr - minr, 0, 0);
		rl = PPM_LUMIN(p);

		PPM_ASSIGN(p, 0, maxg - ming, 0);
		gl = PPM_LUMIN(p);

		PPM_ASSIGN(p, 0, 0, maxb - minb);
		bl = PPM_LUMIN(p);

		if(rl >= gl && rl >= bl)
			qsort((char*)&(chv[indx]), (size_t)clrs,
				sizeof(struct chist_item), redcompare);
		else if(gl >= bl)
			qsort((char*)&(chv[indx]), (size_t)clrs,
				sizeof(struct chist_item), greencompare);
		else 
			qsort((char*)&(chv[indx]), (size_t)clrs,
				sizeof(struct chist_item), bluecompare);

		/*****
		* Now find the median based on the counts, so that about half the
		* pixels (not colors, pixels) are in each subdivision.
		*****/
		lowersum = chv[indx].value;
		halfsum = sm / 2;
		for (i=1; i<clrs-1; i++)
		{
			if(lowersum >= halfsum)
				break;
			lowersum += chv[indx + i].value;
		}

		/* Split the box, and sort to bring the biggest boxes to the top. */
		bv[bi].colors = i;
		bv[bi].sum = lowersum;
		bv[boxes].index = indx + i;
		bv[boxes].colors = clrs - i;
		bv[boxes].sum = sm - lowersum;
		++boxes;
		qsort((char*)bv, (size_t)boxes, sizeof(struct box),sumcompare);
	} /* while (boxes ... */
  
	/* Now choose a representative color for each box. */
  
	for(bi = 0; bi < boxes; bi++)
	{
		register int indx = bv[bi].index;
		register int clrs = bv[bi].colors;
		register long r = 0, g = 0, b = 0, sum = 0;

		for(i = 0; i < clrs; i++)
		{
			r += PPM_GETR( chv[indx + i].color ) * chv[indx + i].value;
			g += PPM_GETG( chv[indx + i].color ) * chv[indx + i].value;
			b += PPM_GETB( chv[indx + i].color ) * chv[indx + i].value;
			sum += chv[indx + i].value;
		}

		r = r / sum;
		if (r>maxval)
			r = maxval;	/* avoid math errors */
		g = g / sum;
		if (g>maxval)
			g = maxval;
		b = b / sum;
		if (b>maxval)
			b = maxval;

		PPM_ASSIGN( colormap[bi].color, r, g, b );
	}

	free(bv);
	return colormap;
}

static int
redcompare(const void *p1, const void *p2)
{
  return((int)PPM_GETR(((chist_vec)p1)->color) - 
         (int)PPM_GETR(((chist_vec)p2)->color));
}

static int
greencompare(const void *p1, const void *p2)
{
  return((int) PPM_GETG( ((chist_vec)p1)->color) - 
         (int) PPM_GETG( ((chist_vec)p2)->color));
}

static int
bluecompare(const void *p1, const void *p2)
{
  return((int) PPM_GETB( ((chist_vec)p1)->color) - 
         (int) PPM_GETB( ((chist_vec)p2)->color));
}

static int
sumcompare(const void *p1, const void *p2)
{
  return(((box_vector) p2)->sum - ((box_vector) p1)->sum);
}

static chist_vec 
ppm_computechist(pixel **pixels, int cols, int rows, int maxcolors,
	int *colorsP)
{
	chash_table cht;
	chist_vec chv;

	cht = ppm_computechash(pixels, cols, rows, maxcolors, colorsP);
	if(!cht)
		return((chist_vec)0);

	chv = ppm_chashtochist(cht, maxcolors);
	ppm_freechash(cht);
	return(chv);
}

static chash_table
ppm_computechash(pixel **pixels, int cols, int rows, int maxcolors,
	int *colorsP )
{
	chash_table cht;
	register pixel* pP;
	chist_list chl;
	int col, row, hash;

	cht = ppm_allocchash();
	*colorsP = 0;

	/* Go through the entire image, building a hash table of colors. */
	for(row = 0; row < rows; row++)
	{
		for (col = 0, pP = pixels[row];  col < cols;  col++, pP++)
		{
			hash = ppm_hashpixel(*pP);

			for(chl = cht[hash]; chl != (chist_list) 0; chl = chl->next)
			{
				if(PPM_EQUAL(chl->ch.color, *pP))
					break;
			}
      
			if(chl != (chist_list)0)
				++(chl->ch.value);
			else
			{
				if((*colorsP)++ > maxcolors)
				{
					ppm_freechash(cht);
					return((chash_table)0);
				}
	
				chl = (chist_list)malloc(sizeof(struct chist_list_item));

				chl->ch.color = *pP;
				chl->ch.value = 1;
				chl->next = cht[hash];
				cht[hash] = chl;
   			}
		}
	}
	return(cht);
}

static chash_table
ppm_allocchash(void)
{
	chash_table cht;
	int i;

	cht = (chash_table)malloc(HASH_SIZE * sizeof(chist_list));

	for(i = 0; i < HASH_SIZE; i++)
		cht[i] = (chist_list) 0;

	return(cht);
}

static chist_vec
ppm_chashtochist(chash_table cht, int maxcolors)
{
	chist_vec chv;
	chist_list chl;
	int i, j;

	/* Now collate the hash table into a simple chist array. */
	chv = (chist_vec)malloc(maxcolors * sizeof(struct chist_item));

	/* Loop through the hash table. */
	j = 0;
	for(i = 0; i < HASH_SIZE; i++)
	{
		for(chl = cht[i]; chl != (chist_list) 0; chl = chl->next)
		{
			/* Add the new entry. */
			chv[j] = chl->ch;
			++j;
		}
	}
	return(chv);
}

static void ppm_freechist(chist_vec chv)
{
  free((char*)chv);
}

static void
ppm_freechash(chash_table cht)
{
	int i;
	chist_list chl, chlnext;

	for(i = 0; i < HASH_SIZE; i++)
	{
		for(chl = cht[i]; chl != (chist_list) 0; chl = chlnext)
		{
			chlnext = chl->next;
			free( (char*) chl );
		}
	}
	free((char*)cht);
}

/*****
* Name: 		_XmHTMLConvert24to8
* Return Type: 	void
* Description: 	transforms a 24bit RGB image to an 8bit paletted image
* In: 
*	data:		original image data (in RGB format)
*	img_data:	raw image data. Will receive paletted image data and colormap.
*	max_colors:	maximum no of colors to use. Only used by ppm_quant.
*	mode:		rgb conversion mode to use.
* Returns:
*	nothing but img_data will contain the indexed image data and a correct
*	colormap with at most 256 colors.
*****/
void
_XmHTMLConvert24to8(Byte *data, XmHTMLRawImageData *img_data, int max_colors,
	Byte mode)
{
	Boolean done = False;

	_XmHTMLDebug(13, ("quantize.c: _XmHTMLConvert24to8, start for %i colors "
		"maximum.\n", max_colors));

	/*****
	* If this image isn't RGB, there's a good chance that this image
	* has less than 256 colors in it. So we first make a quick check
	* to see if this is indeed true. If true, this will produce the best
	* possible results as no quantization is performed.
	*****/
	if((mode == XmBEST || mode == XmQUICK) &&
		img_data->color_class != XmIMAGE_COLORSPACE_RGB)
		done = QuickRGB(data, img_data, max_colors);

	if(!done)
	{
		if(mode == XmBEST || mode == XmSLOW)
			ppm_quant(data, NULL, img_data, max_colors);
		else
			QuickQuantize(data, img_data);
	}
	_XmHTMLDebug(13, ("quantize.c: _XmHTMLConvert24to8, end\n"));
}

/*****
* Name:			_XmHTMLQuantizeImage
* Return Type: 	void
* Description: 	quantizes an image to max_colors
* In: 
*	img_data:	image to be quantized;
*	max_colors:	max. no of colors allowed;
* Returns:
*	nothing, but img_data is updated.
*****/
void
_XmHTMLQuantizeImage(XmHTMLRawImageData *img_data, int max_colors)
{
	Byte *ptr;
	int col, row;
	pixel **pixels;
	register pixel *pP;

	/* reformat image data into a 2D array of pixel values */
	pixels = (pixel **)malloc(img_data->height * sizeof(pixel*));
	ptr = img_data->data;

	for(row = 0; row < img_data->height; row++)
	{
		pixels[row] = (pixel *) malloc(img_data->width * sizeof(pixel));

		for(col = 0, pP = pixels[row]; col < img_data->width; col++, pP++)
		{
			/*****
			* colormap contains 16bit RGB values, so we must downscale
			* to 0-255 range.
			*****/
			pP->r = (GETR(img_data->cmap[*ptr])) >> 8;
			pP->g = (GETG(img_data->cmap[*ptr])) >> 8;
			pP->b = (GETB(img_data->cmap[*ptr++])) >> 8;
		}
	}
	/* quantize it */
	ppm_quant(NULL, pixels, img_data, max_colors);

	/* no need to upscale and free pixels, ppm_quant does that for us */
}

/*****
* Name:			_XmHTMLPixelizeRGB
* Return Type: 	void
* Description: 	converts RGB data to paletted data. Doesn't do any quantizing.
* In: 
*	rgb:		raw rgb data;
*	img_data:	destination.
* Returns:
*	nothing, but upon return img_data contains a valid colormap (with possibly
*	more than XmHTML_MAX_IMAGE_COLORS colors) and the data field has been
*	pixelized. 
*****/
void
_XmHTMLPixelizeRGB(Byte *rgb, XmHTMLRawImageData *img_data)
{
	Pixel *colors, col, max_colors;
	int i, num_colors, low, high, mid, width, height;
	Byte *p, *pix;

	width  = img_data->width;
	height = img_data->height;

	/* initialize colors array */
	max_colors = XmHTML_MAX_IMAGE_COLORS;
	colors = (Pixel*)malloc(max_colors * sizeof(Pixel));

	/* put the first color in the table by hand */
	num_colors = 0;
	mid = 0;  

	for(i = width*height, p = rgb; i; i--)
	{
		/* make truecolor pixel val */
		col  = (((Pixel) *p++) << 16);  
		col += (((Pixel) *p++) << 8);
		col +=  *p++;

		/* binary search the 'colors' array to see if it's in there */
		low = 0;
		high = num_colors - 1;
		while (low <= high)
		{
			mid = (low+high)/2;
			if(col < colors[mid])
				high = mid - 1;
			else if(col > colors[mid])
				low = mid + 1;
			else
				break;
		}

		if(high < low)
		{
			/* didn't find color in list, add it. */
			if(num_colors >= max_colors)
			{
				/* enlarge colors array */
				max_colors *= 2;
				colors = (Pixel*)realloc(colors, max_colors * sizeof(Pixel));
			}
			my_bcopy((char *)&colors[low], (char*)&colors[low+1],
				(num_colors - low) * sizeof(Pixel));
			colors[low] = col;
			num_colors++;
		}
	}

	/* destination buffer */
	if(img_data->data == (Byte*)NULL)
		img_data->data = (Byte*)malloc(width*height*sizeof(Byte));

	/* Pixelize data: map pixel values of the RGB image in the colormap */
	for(i = width*height, p = rgb, pix = img_data->data; i; i--,pix++)
	{
		col  = (((Pixel)*p++) << 16);  
		col += (((Pixel)*p++) << 8);
		col +=  *p++;

		/* binary search the 'colors' array.  It *IS* in there */
		low = 0;
		high = num_colors - 1;
		while(low <= high)
		{
			mid = (low+high)/2;
			if(col < colors[mid])
				high = mid - 1;
			else if(col > colors[mid])
				low = mid + 1;
			else
				break;
		}
		*pix = mid;
	}

	/* allocate colormap */
	AllocRawImageCmap(img_data, num_colors);

	/* fill upscale colormap */
	for(i = 0; i < num_colors; i++)
	{
		GETR(img_data->cmap[i]) = (( colors[i] >> 16)) << 8;
		GETG(img_data->cmap[i]) = (((colors[i] >> 8 ) & 0xff)) << 8;
		GETB(img_data->cmap[i]) = (( colors[i]  & 0xff)) << 8;
	}
	/* no longer needed */
	free(colors);
}

void
_XmHTMLDitherImage(XmHTMLWidget html, XmHTMLRawImageData *img_data)
{
	int r, g, b, er, eg, eb;
	int i, j, size, ex, used[XmHTML_MAX_IMAGE_COLORS];
	int *error = NULL, *er1 = NULL, *er2 = NULL, *ter;
	Dimension width  = img_data->width;
	Dimension height = img_data->height;
	Pixel val;
	Byte *ptr = img_data->data;
	XCOLOR *cmap = img_data->cmap;
	XCC xcc = html->html.xcc;
	XCCDither *dm = xcc->fast_dither;
	Boolean f;

	size = width * height;

	if(html->html.map_to_palette == XmBEST ||
		html->html.map_to_palette == XmSLOW)
	{
		error = (int*)malloc(6*width*sizeof(int));
		er1 = error;
		er2 = error + width*3;
		memset(error, 0, 6*width*sizeof(int));
	}
	else
	{
		for(i = 0; i < XmHTML_MAX_IMAGE_COLORS; i++)
			used[i] = -1;
	}

	for(i = 0; i < img_data->cmapsize; i++)
	{
		GETR(cmap[i]) >>=8;
		GETG(cmap[i]) >>=8;
		GETB(cmap[i]) >>=8;
	}


	switch(html->html.map_to_palette)
	{
		case XmQUICK: /* closest match, no error correction */
			for(i = 0; i < size; i++, ptr++)
			{
				if(used[(int)*ptr] == -1)
				{
					r = GETR(cmap[(int)*ptr]);
					g = GETG(cmap[(int)*ptr]);
					b = GETB(cmap[(int)*ptr]);
					used[(int)*ptr] = (int)XCCGetIndexFromPalette(xcc, &r,
						&g, &b, &f); 
				}
				*ptr = (Byte)used[(int)*ptr];
			}
			break;

		case XmBEST: /* predefined color & error matrices, FS */
			{
				for(i = 0; i < height; i++)
				{
					ter = er1;
					er1 = er2;
					er2 = ter;
					memset(er2, 0, width*3*sizeof(int));
					ex = 0;
					for(j = 0; j < width; j++)
					{
						r = GETR(cmap[(int)*ptr]);
						g = GETG(cmap[(int)*ptr]);
						b = GETB(cmap[(int)*ptr]);
						er = r + er1[ex++];
						eg = g + er1[ex++];
						eb = b + er1[ex++];
						RANGE(er,0,255);
						RANGE(eg,0,255);
						RANGE(eb,0,255);
						val = (Pixel)dm->fast_rgb[er>>3][eg>>3][eb>>3];
						r = dm->fast_err[er>>3][eg>>3][eb>>3];
						g = dm->fast_erg[er>>3][eg>>3][eb>>3];
						b = dm->fast_erb[er>>3][eg>>3][eb>>3];
						er = r;
						eg = g;
						eb = b;

						if(j < (width - 1))
						{
							er1[ex+0] += SIXTEENTH * (er*7);
							er1[ex+1] += SIXTEENTH * (eg*7);
							er1[ex+2] += SIXTEENTH * (eb*7);
						}
						if(i < (height - 1))
						{
							er2[ex - 3] += SIXTEENTH * (er*5);
							er2[ex - 2] += SIXTEENTH * (eg*5);
							er2[ex - 1] += SIXTEENTH * (eb*5);
							if(j > 0)
							{
								er2[ex - 6] += SIXTEENTH * (er*3);
								er2[ex - 5] += SIXTEENTH * (eg*3);
								er2[ex - 4] += SIXTEENTH * (eb*3);
							}
							if(j < (width - 1))
							{
								er2[ex + 0] = SIXTEENTH * (er);
								er2[ex + 1] = SIXTEENTH * (eg);
								er2[ex + 2] = SIXTEENTH * (eb);
							}
						}
						*ptr++ = (Byte)val;
					}
				}
			}
			break;

		case XmFAST: /* predefined color matrix, no error correction */
			for(i = 0; i < size; i++, ptr++)
			{
				if(used[(int)*ptr] == -1)
				{
					r = GETR(cmap[(int)*ptr]);
					g = GETG(cmap[(int)*ptr]);
					b = GETB(cmap[(int)*ptr]);
					used[(int)*ptr] = (int)dm->fast_rgb[r>>3][g>>3][b>>3];
				}
				*ptr = (Byte)used[(int)*ptr];
			}
			break;

		case XmSLOW: /* closest match & dynamic error matrices, FS */
			{
				for(i = 0; i < height; i++)
				{
					ter = er1;
					er1 = er2;
					er2 = ter;
					memset(er2, 0, width*3*sizeof(int));
					ex = 0;
					for(j = 0; j < width; j++)
					{
						r = GETR(cmap[(int)*ptr]);
						g = GETG(cmap[(int)*ptr]);
						b = GETB(cmap[(int)*ptr]);
						er = r + er1[ex++];
						eg = g + er1[ex++];
						eb = b + er1[ex++];
						RANGE(er,0,255);
						RANGE(eg,0,255);
						RANGE(eb,0,255);
						val = XCCGetIndexFromPalette(xcc, &er, &eg, &eb, &f);
						if(j < (width - 1))
						{
							er1[ex+0] += SIXTEENTH * (er*7);
							er1[ex+1] += SIXTEENTH * (eg*7);
							er1[ex+2] += SIXTEENTH * (eb*7);
						}
						if(i < (height - 1))
						{
							er2[ex - 3] += SIXTEENTH * (er*5);
							er2[ex - 2] += SIXTEENTH * (eg*5);
							er2[ex - 1] += SIXTEENTH * (eb*5);
							if(j > 0)
							{
								er2[ex - 6] += SIXTEENTH * (er*3);
								er2[ex - 5] += SIXTEENTH * (eg*3);
								er2[ex - 4] += SIXTEENTH * (eb*3);
							}
							if(j < (width - 1))
							{
								er2[ex + 0] = SIXTEENTH * (er);
								er2[ex + 1] = SIXTEENTH * (eg);
								er2[ex + 2] = SIXTEENTH * (eb);
							}
						}
						*ptr++ = (Byte)val;
					}
				}
			}
			break;
		default:
			return;
	}
	/* release error matrix if we've used one */
	if(html->html.map_to_palette == XmBEST ||
		html->html.map_to_palette == XmSLOW)
		free(error);

	/* replace colormap with palette */
	img_data->cmapsize = xcc->num_palette;
	img_data->cmap = (XCOLOR*)realloc(img_data->cmap,
		xcc->num_palette*sizeof(XCOLOR));
	memcpy(img_data->cmap, xcc->palette, xcc->num_palette*sizeof(XCOLOR));

	/* and upscale again for later use */
	for(i = 0; i < img_data->cmapsize; i++)
	{
		GETR(img_data->cmap[i]) <<=8;
		GETG(img_data->cmap[i]) <<=8;
		GETB(img_data->cmap[i]) <<=8;
	}
}
