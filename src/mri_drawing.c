/****************************************************************************
    Adapted from libppm5.c by RWCox, to draw stuff into an RGB image
*****************************************************************************/

/*---------------------------------------------------------------------------
** Copyright (C) 1989, 1991 by Jef Poskanzer.
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software is provided "as is" without express or
** implied warranty.
**
** The character drawing routines are by John Walker
** Copyright (C) 1994 by John Walker, kelvin@fourmilab.ch
-----------------------------------------------------------------------------*/

#include "mrilib.h"

#undef  DDA_SCALE
#define DDA_SCALE 8192

#undef  ASSPIX
#define ASSPIX(p,x,y,r,g,b) ( (p)[3*((x)+(y)*cols)  ] = (r) ,   \
                              (p)[3*((x)+(y)*cols)+1] = (g) ,   \
                              (p)[3*((x)+(y)*cols)+2] = (b)   )

/*--------------------------- Simple fill routine ---------------------------*/

void ppmd_filledrectangle( byte *pixels, int cols, int rows,
                           int x, int y, int width, int height , byte r,byte g,byte b )
{
    register int cx, cy, cwidth, cheight, col, row;

    /* Clip. */
    cx = x; cy = y; cwidth = width; cheight = height;
    if ( cx < 0 ) { cx = 0; cwidth += x; }
    if ( cy < 0 ) { cy = 0; cheight += y; }
    if ( cx + cwidth > cols ) cwidth = cols - cx;
    if ( cy + cheight > rows ) cheight = rows - cy;

    /* Draw. */
    for ( row = cy; row < cy + cheight; ++row )
	for ( col = cx; col < cx + cwidth; ++col )
            ASSPIX(pixels,col,row,r,g,b) ;
}

/*-------------------------------------------------------------------------------*/

#define ppmd_lineclip 1  /* always clip lines */

void ppmd_line( byte *pixels, int cols, int rows,
                int x0, int y0, int x1, int y1, byte r,byte g,byte b )
{
    register int cx0, cy0, cx1, cy1;

    /* Special case zero-length lines. */
    if ( x0 == x1 && y0 == y1 ) {
	if ( ( x0 >= 0 && x0 < cols && y0 >= 0 && y0 < rows ) )
            ASSPIX(pixels,x0,y0,r,g,b) ;
	return;
    }

    /* Clip. */
    cx0 = x0; cy0 = y0; cx1 = x1; cy1 = y1;
    if ( ppmd_lineclip ) {
	if ( cx0 < 0 )
	    {
	    if ( cx1 < 0 ) return;
	    cy0 = cy0 + ( cy1 - cy0 ) * ( -cx0 ) / ( cx1 - cx0 );
	    cx0 = 0;
	    }
	else if ( cx0 >= cols )
	    {
	    if ( cx1 >= cols ) return;
	    cy0 = cy0 + ( cy1 - cy0 ) * ( cols - 1 - cx0 ) / ( cx1 - cx0 );
	    cx0 = cols - 1;
	    }
	if ( cy0 < 0 )
	    {
	    if ( cy1 < 0 ) return;
	    cx0 = cx0 + ( cx1 - cx0 ) * ( -cy0 ) / ( cy1 - cy0 );
	    cy0 = 0;
	    }
	else if ( cy0 >= rows )
	    {
	    if ( cy1 >= rows ) return;
	    cx0 = cx0 + ( cx1 - cx0 ) * ( rows - 1 - cy0 ) / ( cy1 - cy0 );
	    cy0 = rows - 1;
	    }
	if ( cx1 < 0 )
	    {
	    cy1 = cy1 + ( cy0 - cy1 ) * ( -cx1 ) / ( cx0 - cx1 );
	    cx1 = 0;
	    }
	else if ( cx1 >= cols )
	    {
	    cy1 = cy1 + ( cy0 - cy1 ) * ( cols - 1 - cx1 ) / ( cx0 - cx1 );
	    cx1 = cols - 1;
	    }
	if ( cy1 < 0 )
	    {
	    cx1 = cx1 + ( cx0 - cx1 ) * ( -cy1 ) / ( cy0 - cy1 );
	    cy1 = 0;
	    }
	else if ( cy1 >= rows )
	    {
	    cx1 = cx1 + ( cx0 - cx1 ) * ( rows - 1 - cy1 ) / ( cy0 - cy1 );
	    cy1 = rows - 1;
	    }

	/* Check again for zero-length lines. */
	if ( cx0 == cx1 && cy0 == cy1 ){ ASSPIX(pixels,cx0,cy0,r,g,b) ; return; }
    } /* end of clip */

    /* Draw, using a simple DDA. */
    if ( abs( cx1 - cx0 ) > abs( cy1 - cy0 ) ) { /* Loop over X domain. */
	register long dy, srow;
	register int dx, col, row, prevrow;

	if ( cx1 > cx0 ) dx =  1;
	else             dx = -1;
	dy = ( cy1 - cy0 ) * DDA_SCALE / abs( cx1 - cx0 );
	prevrow = row = cy0;
	srow = row * DDA_SCALE + DDA_SCALE / 2;
	col = cx0;
	for ( ; ; ) {
            ASSPIX(pixels,col,row,r,g,b) ;
	    if ( col == cx1 ) break;
	    srow += dy; row = srow / DDA_SCALE; col += dx;
        }
    } else { /* Loop over Y domain. */
	register long dx, scol;
	register int dy, col, row, prevcol;

	if ( cy1 > cy0 ) dy =  1;
	else             dy = -1;
	dx = ( cx1 - cx0 ) * DDA_SCALE / abs( cy1 - cy0 );
	row = cy0;
	prevcol = col = cx0;
	scol = col * DDA_SCALE + DDA_SCALE / 2;
	for ( ; ; ) {
            ASSPIX(pixels,col,row,r,g,b) ;
	    if ( row == cy1 ) break;
	    row += dy; scol += dx; col = scol / DDA_SCALE;
        }
    }
}

/*-------------------------------------------------------------------------------*/

#define SPLINE_THRESH 3

void ppmd_spline3( byte *pixels, int cols, int rows,
                   int x0, int y0, int x1, int y1, int x2, int y2, byte r,byte g, byte b)
{
    register int xa, ya, xb, yb, xc, yc, xp, yp;

    xa = ( x0 + x1 ) / 2; ya = ( y0 + y1 ) / 2;
    xc = ( x1 + x2 ) / 2; yc = ( y1 + y2 ) / 2;
    xb = ( xa + xc ) / 2; yb = ( ya + yc ) / 2;

    xp = ( x0 + xb ) / 2; yp = ( y0 + yb ) / 2;
    if ( abs( xa - xp ) + abs( ya - yp ) > SPLINE_THRESH )
	ppmd_spline3( pixels, cols, rows, x0, y0, xa, ya, xb, yb, r,g,b ) ;
    else
	ppmd_line( pixels, cols, rows, x0, y0, xb, yb, r,g,b ) ;

    xp = ( x2 + xb ) / 2; yp = ( y2 + yb ) / 2;
    if ( abs( xc - xp ) + abs( yc - yp ) > SPLINE_THRESH )
	ppmd_spline3( pixels, cols, rows, xb, yb, xc, yc, x2, y2, r,g,b ) ;
    else
	ppmd_line( pixels, cols, rows, xb, yb, x2, y2, r,g,b ) ;
}

/*-------------------------------------------------------------------------------*/

void ppmd_polyspline( byte *pixels, int cols, int rows,
                      int x0,int y0, int nc, int* xc, int* yc, int x1,int y1, byte r,byte g,byte b )
{
    register int i, x, y, xn, yn;

    x = x0; y = y0;
    for ( i = 0; i < nc - 1; ++i ) {
	xn = ( xc[i] + xc[i + 1] ) / 2;
	yn = ( yc[i] + yc[i + 1] ) / 2;
	ppmd_spline3( pixels, cols, rows, x, y, xc[i], yc[i], xn, yn, r,g,b ) ;
	x = xn; y = yn;
    }
    ppmd_spline3( pixels, cols, rows, x, y, xc[nc - 1], yc[nc - 1], x1, y1, r,g,b ) ;
}

/*-------------------------------------------------------------------------------*/

void ppmd_circle( byte *pixels, int cols, int rows,
                  int cx, int cy, int radius, byte r,byte g,byte b )
{
    register int x0, y0, x, y, prevx, prevy, nopointsyet;
    register long sx, sy, e;

    x0 = x = radius;
    y0 = y = 0;
    sx = x * DDA_SCALE + DDA_SCALE / 2;
    sy = y * DDA_SCALE + DDA_SCALE / 2;
    e = DDA_SCALE / radius;
    ASSPIX(pixels,x+cx,y+cy,r,g,b) ;
    nopointsyet = 1;
    do {
	prevx = x; prevy = y;
	sx += e * sy / DDA_SCALE;
	sy -= e * sx / DDA_SCALE;
	x = sx / DDA_SCALE;
	y = sy / DDA_SCALE;
	if ( x != prevx || y != prevy ) {
            nopointsyet = 0;
            ASSPIX(pixels,x+cx,y+cy,r,g,b) ;
        }
    } while ( nopointsyet || x != x0 || y != y0 );
}

/*-------------------------------------------------------------------------------*/

/*		  Stroke character definitions

   The	following  character  definitions are derived from the (public
   domain) Hershey plotter  font  database,  using  the  single-stroke
   Roman font.

   Each  character  definition	begins	with 3 bytes which specify the
   number of X, Y plot pairs which follow, the negative  of  the  skip
   before  starting  to  draw  the  characters, and the skip after the
   character.  The first plot pair moves the pen to that location  and
   subsequent  pairs  draw  to	the  location given.  A pair of 192, 0
   raises the pen, moves to the location given by the following  pair,
   and resumes drawing with the pair after that.

   The  values  in  the  definition  tables are 8-bit two's complement
   signed numbers.  We  declare  the  table  as  "unsigned  char"  and
   manually  sign-extend  the  values because C compilers differ as to
   whether the type "char" is signed or unsigned, and  some  compilers
   don't  accept the qualifier "signed" which we would like to use for
   these items.  We specify negative numbers as their  unsigned  two's
   complements  to  avoid  complaints  from compilers which don't like
   initialising unsigned data with signed values.  Ahhh,  portability.
*/

static unsigned char
char32[] = { 0, 0, 21 },
char33[] = { 8, 251, 5, 0, 244, 0, 2, 192, 0, 0, 7, 255, 8, 0, 9, 1, 8, 0, 7 },
char34[] = { 17, 253, 15, 2, 244, 1, 245, 0, 244, 1, 243, 2, 244, 2, 246, 1,
	     248, 0, 249, 192, 0, 10, 244, 9, 245, 8, 244, 9, 243, 10, 244,
	     10, 246, 9, 248, 8, 249, },
char35[] = { 11, 246, 11, 1, 240, 250, 16, 192, 0, 7, 240, 0, 16, 192, 0, 250,
	     253, 8, 253, 192, 0, 249, 3, 7, 3 },
char36[] = { 26, 246, 10, 254, 240, 254, 13, 192, 0, 2, 240, 2, 13, 192, 0, 7,
	     247, 5, 245, 2, 244, 254, 244, 251, 245, 249, 247, 249, 249, 250,
	     251, 251, 252, 253, 253, 3, 255, 5, 0, 6, 1, 7, 3, 7, 6, 5, 8, 2,
	     9, 254, 9, 251, 8, 249, 6 },
char37[] = { 31, 244, 12, 9, 244, 247, 9, 192, 0, 252, 244, 254, 246, 254,
	     248, 253, 250, 251, 251, 249, 251, 247, 249, 247, 247, 248, 245,
	     250, 244, 252, 244, 254, 245, 1, 246, 4, 246, 7, 245, 9, 244,
	     192, 0, 5, 2, 3, 3, 2, 5, 2, 7, 4, 9, 6, 9, 8, 8, 9, 6, 9, 4, 7,
	     2, 5, 2 },
char38[] = { 34, 243, 13, 10, 253, 10, 252, 9, 251, 8, 251, 7, 252, 6, 254, 4,
	     3, 2, 6, 0, 8, 254, 9, 250, 9, 248, 8, 247, 7, 246, 5, 246, 3,
	     247, 1, 248, 0, 255, 252, 0, 251, 1, 249, 1, 247, 0, 245, 254,
	     244, 252, 245, 251, 247, 251, 249, 252, 252, 254, 255, 3, 6, 5,
	     8, 7, 9, 9, 9, 10, 8, 10, 7 },
char39[] = { 7, 251, 5, 0, 246, 255, 245, 0, 244, 1, 245, 1, 247, 0, 249, 255,
	     250 },
char40[] = { 10, 249, 7, 4, 240, 2, 242, 0, 245, 254, 249, 253, 254, 253, 2,
	     254, 7, 0, 11, 2, 14, 4, 16 },
char41[] = { 10, 249, 7, 252, 240, 254, 242, 0, 245, 2, 249, 3, 254, 3, 2, 2,
	     7, 0, 11, 254, 14, 252, 16 },
char42[] = { 8, 248, 8, 0, 250, 0, 6, 192, 0, 251, 253, 5, 3, 192, 0, 5, 253,
	     251, 3 },
char43[] = { 5, 243, 13, 0, 247, 0, 9, 192, 0, 247, 0, 9, 0 },
char44[] = { 8, 251, 5, 1, 8, 0, 9, 255, 8, 0, 7, 1, 8, 1, 10, 0, 12, 255, 13
	   },
char45[] = { 2, 243, 13, 247, 0, 9, 0 },
char46[] = { 5, 251, 5, 0, 7, 255, 8, 0, 9, 1, 8, 0, 7 },
char47[] = { 2, 245, 11, 9, 240, 247, 16 },
char48[] = { 17, 246, 10, 255, 244, 252, 245, 250, 248, 249, 253, 249, 0, 250,
	     5, 252, 8, 255, 9, 1, 9, 4, 8, 6, 5, 7, 0, 7, 253, 6, 248, 4,
	     245, 1, 244, 255, 244 },
char49[] = { 4, 246, 10, 252, 248, 254, 247, 1, 244, 1, 9 },
char50[] = { 14, 246, 10, 250, 249, 250, 248, 251, 246, 252, 245, 254, 244, 2,
	     244, 4, 245, 5, 246, 6, 248, 6, 250, 5, 252, 3, 255, 249, 9, 7, 9
	   },
char51[] = { 15, 246, 10, 251, 244, 6, 244, 0, 252, 3, 252, 5, 253, 6, 254, 7,
	     1, 7, 3, 6, 6, 4, 8, 1, 9, 254, 9, 251, 8, 250, 7, 249, 5 },
char52[] = { 6, 246, 10, 3, 244, 249, 2, 8, 2, 192, 0, 3, 244, 3, 9 },
char53[] = { 17, 246, 10, 5, 244, 251, 244, 250, 253, 251, 252, 254, 251, 1,
	     251, 4, 252, 6, 254, 7, 1, 7, 3, 6, 6, 4, 8, 1, 9, 254, 9, 251,
	     8, 250, 7, 249, 5 },
char54[] = { 23, 246, 10, 6, 247, 5, 245, 2, 244, 0, 244, 253, 245, 251, 248,
	     250, 253, 250, 2, 251, 6, 253, 8, 0, 9, 1, 9, 4, 8, 6, 6, 7, 3,
	     7, 2, 6, 255, 4, 253, 1, 252, 0, 252, 253, 253, 251, 255, 250, 2
	     },
char55[] = { 5, 246, 10, 7, 244, 253, 9, 192, 0, 249, 244, 7, 244 },
char56[] = { 29, 246, 10, 254, 244, 251, 245, 250, 247, 250, 249, 251, 251,
	     253, 252, 1, 253, 4, 254, 6, 0, 7, 2, 7, 5, 6, 7, 5, 8, 2, 9,
	     254, 9, 251, 8, 250, 7, 249, 5, 249, 2, 250, 0, 252, 254, 255,
	     253, 3, 252, 5, 251, 6, 249, 6, 247, 5, 245, 2, 244, 254, 244 },
char57[] = { 23, 246, 10, 6, 251, 5, 254, 3, 0, 0, 1, 255, 1, 252, 0, 250,
	     254, 249, 251, 249, 250, 250, 247, 252, 245, 255, 244, 0, 244, 3,
	     245, 5, 247, 6, 251, 6, 0, 5, 5, 3, 8, 0, 9, 254, 9, 251, 8, 250,
	     6 },
char58[] = { 11, 251, 5, 0, 251, 255, 252, 0, 253, 1, 252, 0, 251, 192, 0, 0,
	     7, 255, 8, 0, 9, 1, 8, 0, 7 },
char59[] = { 14, 251, 5, 0, 251, 255, 252, 0, 253, 1, 252, 0, 251, 192, 0, 1,
	     8, 0, 9, 255, 8, 0, 7, 1, 8, 1, 10, 0, 12, 255, 13 },
char60[] = { 3, 244, 12, 8, 247, 248, 0, 8, 9 },
char61[] = { 5, 243, 13, 247, 253, 9, 253, 192, 0, 247, 3, 9, 3 },
char62[] = { 3, 244, 12, 248, 247, 8, 0, 248, 9 },
char63[] = { 20, 247, 9, 250, 249, 250, 248, 251, 246, 252, 245, 254, 244, 2,
	     244, 4, 245, 5, 246, 6, 248, 6, 250, 5, 252, 4, 253, 0, 255, 0,
	     2, 192, 0, 0, 7, 255, 8, 0, 9, 1, 8, 0, 7 },
char64[] = { 55, 243, 14, 5, 252, 4, 250, 2, 249, 255, 249, 253, 250, 252,
	     251, 251, 254, 251, 1, 252, 3, 254, 4, 1, 4, 3, 3, 4, 1, 192, 0,
	     255, 249, 253, 251, 252, 254, 252, 1, 253, 3, 254, 4, 192, 0, 5,
	     249, 4, 1, 4, 3, 6, 4, 8, 4, 10, 2, 11, 255, 11, 253, 10, 250, 9,
	     248, 7, 246, 5, 245, 2, 244, 255, 244, 252, 245, 250, 246, 248,
	     248, 247, 250, 246, 253, 246, 0, 247, 3, 248, 5, 250, 7, 252, 8,
	     255, 9, 2, 9, 5, 8, 7, 7, 8, 6, 192, 0, 6, 249, 5, 1, 5, 3, 6, 4
	     },
char65[] = { 8, 247, 9, 0, 244, 248, 9, 192, 0, 0, 244, 8, 9, 192, 0, 251, 2,
	     5, 2 },
char66[] = { 23, 245, 10, 249, 244, 249, 9, 192, 0, 249, 244, 2, 244, 5, 245,
	     6, 246, 7, 248, 7, 250, 6, 252, 5, 253, 2, 254, 192, 0, 249, 254,
	     2, 254, 5, 255, 6, 0, 7, 2, 7, 5, 6, 7, 5, 8, 2, 9, 249, 9 },
char67[] = { 18, 246, 11, 8, 249, 7, 247, 5, 245, 3, 244, 255, 244, 253, 245,
	     251, 247, 250, 249, 249, 252, 249, 1, 250, 4, 251, 6, 253, 8,
	     255, 9, 3, 9, 5, 8, 7, 6, 8, 4 },
char68[] = { 15, 245, 10, 249, 244, 249, 9, 192, 0, 249, 244, 0, 244, 3, 245,
	     5, 247, 6, 249, 7, 252, 7, 1, 6, 4, 5, 6, 3, 8, 0, 9, 249, 9 },
char69[] = { 11, 246, 9, 250, 244, 250, 9, 192, 0, 250, 244, 7, 244, 192, 0,
	     250, 254, 2, 254, 192, 0, 250, 9, 7, 9 },
char70[] = { 8, 246, 8, 250, 244, 250, 9, 192, 0, 250, 244, 7, 244, 192, 0,
	     250, 254, 2, 254 },
char71[] = { 22, 246, 11, 8, 249, 7, 247, 5, 245, 3, 244, 255, 244, 253, 245,
	     251, 247, 250, 249, 249, 252, 249, 1, 250, 4, 251, 6, 253, 8,
	     255, 9, 3, 9, 5, 8, 7, 6, 8, 4, 8, 1, 192, 0, 3, 1, 8, 1 },
char72[] = { 8, 245, 11, 249, 244, 249, 9, 192, 0, 7, 244, 7, 9, 192, 0, 249,
	     254, 7, 254 },
char73[] = { 2, 252, 4, 0, 244, 0, 9 },
char74[] = { 10, 248, 8, 4, 244, 4, 4, 3, 7, 2, 8, 0, 9, 254, 9, 252, 8, 251,
	     7, 250, 4, 250, 2 },
char75[] = { 8, 245, 10, 249, 244, 249, 9, 192, 0, 7, 244, 249, 2, 192, 0,
	     254, 253, 7, 9 },
char76[] = { 5, 246, 7, 250, 244, 250, 9, 192, 0, 250, 9, 6, 9 },
char77[] = { 11, 244, 12, 248, 244, 248, 9, 192, 0, 248, 244, 0, 9, 192, 0, 8,
	     244, 0, 9, 192, 0, 8, 244, 8, 9 },
char78[] = { 8, 245, 11, 249, 244, 249, 9, 192, 0, 249, 244, 7, 9, 192, 0, 7,
	     244, 7, 9 },
char79[] = { 21, 245, 11, 254, 244, 252, 245, 250, 247, 249, 249, 248, 252,
	     248, 1, 249, 4, 250, 6, 252, 8, 254, 9, 2, 9, 4, 8, 6, 6, 7, 4,
	     8, 1, 8, 252, 7, 249, 6, 247, 4, 245, 2, 244, 254, 244 },
char80[] = { 13, 245, 10, 249, 244, 249, 9, 192, 0, 249, 244, 2, 244, 5, 245,
	     6, 246, 7, 248, 7, 251, 6, 253, 5, 254, 2, 255, 249, 255 },
char81[] = { 24, 245, 11, 254, 244, 252, 245, 250, 247, 249, 249, 248, 252,
	     248, 1, 249, 4, 250, 6, 252, 8, 254, 9, 2, 9, 4, 8, 6, 6, 7, 4,
	     8, 1, 8, 252, 7, 249, 6, 247, 4, 245, 2, 244, 254, 244, 192, 0,
	     1, 5, 7, 11 },
char82[] = { 16, 245, 10, 249, 244, 249, 9, 192, 0, 249, 244, 2, 244, 5, 245,
	     6, 246, 7, 248, 7, 250, 6, 252, 5, 253, 2, 254, 249, 254, 192, 0,
	     0, 254, 7, 9 },
char83[] = { 20, 246, 10, 7, 247, 5, 245, 2, 244, 254, 244, 251, 245, 249,
	     247, 249, 249, 250, 251, 251, 252, 253, 253, 3, 255, 5, 0, 6, 1,
	     7, 3, 7, 6, 5, 8, 2, 9, 254, 9, 251, 8, 249, 6 },
char84[] = { 5, 248, 8, 0, 244, 0, 9, 192, 0, 249, 244, 7, 244 },
char85[] = { 10, 245, 11, 249, 244, 249, 3, 250, 6, 252, 8, 255, 9, 1, 9, 4,
	     8, 6, 6, 7, 3, 7, 244 },
char86[] = { 5, 247, 9, 248, 244, 0, 9, 192, 0, 8, 244, 0, 9 },
char87[] = { 11, 244, 12, 246, 244, 251, 9, 192, 0, 0, 244, 251, 9, 192, 0, 0,
	     244, 5, 9, 192, 0, 10, 244, 5, 9 },
char88[] = { 5, 246, 10, 249, 244, 7, 9, 192, 0, 7, 244, 249, 9 },
char89[] = { 6, 247, 9, 248, 244, 0, 254, 0, 9, 192, 0, 8, 244, 0, 254 },
char90[] = { 8, 246, 10, 7, 244, 249, 9, 192, 0, 249, 244, 7, 244, 192, 0,
	    249, 9, 7, 9 },
char91[] = { 11, 249, 7, 253, 240, 253, 16, 192, 0, 254, 240, 254, 16, 192, 0,
	     253, 240, 4, 240, 192, 0, 253, 16, 4, 16 },
char92[] = { 2, 245, 11, 9, 16, 247, 240 },
char93[] = { 11, 249, 7, 2, 240, 2, 16, 192, 0, 3, 240, 3, 16, 192, 0, 252,
	     240, 3, 240, 192, 0, 252, 16, 3, 16 },
char94[] = { 7, 245, 11, 248, 2, 0, 253, 8, 2, 192, 0, 248, 2, 0, 254, 8, 2 },
char95[] = { 2, 253, 22, 0, 9, 20, 9 },
char96[] = { 7, 251, 5, 1, 244, 0, 245, 255, 247, 255, 249, 0, 250, 1, 249, 0,
	     248 },
char97[] = { 17, 247, 10, 6, 251, 6, 9, 192, 0, 6, 254, 4, 252, 2, 251, 255,
	     251, 253, 252, 251, 254, 250, 1, 250, 3, 251, 6, 253, 8, 255, 9,
	     2, 9, 4, 8, 6, 6 },
char98[] = { 17, 246, 9, 250, 244, 250, 9, 192, 0, 250, 254, 252, 252, 254,
	     251, 1, 251, 3, 252, 5, 254, 6, 1, 6, 3, 5, 6, 3, 8, 1, 9, 254,
	     9, 252, 8, 250, 6 },
char99[] = { 14, 247, 9, 6, 254, 4, 252, 2, 251, 255, 251, 253, 252, 251, 254,
	     250, 1, 250, 3, 251, 6, 253, 8, 255, 9, 2, 9, 4, 8, 6, 6 },
char100[] = { 17, 247, 10, 6, 244, 6, 9, 192, 0, 6, 254, 4, 252, 2, 251, 255,
	      251, 253, 252, 251, 254, 250, 1, 250, 3, 251, 6, 253, 8, 255, 9,
	      2, 9, 4, 8, 6, 6 },
char101[] = { 17, 247, 9, 250, 1, 6, 1, 6, 255, 5, 253, 4, 252, 2, 251, 255,
	      251, 253, 252, 251, 254, 250, 1, 250, 3, 251, 6, 253, 8, 255, 9,
	      2, 9, 4, 8, 6, 6 },
char102[] = { 8, 251, 7, 5, 244, 3, 244, 1, 245, 0, 248, 0, 9, 192, 0, 253,
	      251, 4, 251 },
char103[] = { 22, 247, 10, 6, 251, 6, 11, 5, 14, 4, 15, 2, 16, 255, 16, 253,
	      15, 192, 0, 6, 254, 4, 252, 2, 251, 255, 251, 253, 252, 251,
	      254, 250, 1, 250, 3, 251, 6, 253, 8, 255, 9, 2, 9, 4, 8, 6, 6 },
char104[] = { 10, 247, 10, 251, 244, 251, 9, 192, 0, 251, 255, 254, 252, 0,
	      251, 3, 251, 5, 252, 6, 255, 6, 9 },
char105[] = { 8, 252, 4, 255, 244, 0, 245, 1, 244, 0, 243, 255, 244, 192, 0,
	      0, 251, 0, 9 },
char106[] = { 11, 251, 5, 0, 244, 1, 245, 2, 244, 1, 243, 0, 244, 192, 0, 1,
	      251, 1, 12, 0, 15, 254, 16, 252, 16 },
char107[] = { 8, 247, 8, 251, 244, 251, 9, 192, 0, 5, 251, 251, 5, 192, 0,
	      255, 1, 6, 9 },
char108[] = { 2, 252, 4, 0, 244, 0, 9 },
char109[] = { 18, 241, 15, 245, 251, 245, 9, 192, 0, 245, 255, 248, 252, 250,
	      251, 253, 251, 255, 252, 0, 255, 0, 9, 192, 0, 0, 255, 3, 252,
	      5, 251, 8, 251, 10, 252, 11, 255, 11, 9 },
char110[] = { 10, 247, 10, 251, 251, 251, 9, 192, 0, 251, 255, 254, 252, 0,
	      251, 3, 251, 5, 252, 6, 255, 6, 9 },
char111[] = { 17, 247, 10, 255, 251, 253, 252, 251, 254, 250, 1, 250, 3, 251,
	      6, 253, 8, 255, 9, 2, 9, 4, 8, 6, 6, 7, 3, 7, 1, 6, 254, 4, 252,
	      2, 251, 255, 251 },
char112[] = { 17, 246, 9, 250, 251, 250, 16, 192, 0, 250, 254, 252, 252, 254,
	      251, 1, 251, 3, 252, 5, 254, 6, 1, 6, 3, 5, 6, 3, 8, 1, 9, 254,
	      9, 252, 8, 250, 6 },
char113[] = { 17, 247, 10, 6, 251, 6, 16, 192, 0, 6, 254, 4, 252, 2, 251, 255,
	      251, 253, 252, 251, 254, 250, 1, 250, 3, 251, 6, 253, 8, 255, 9,
	      2, 9, 4, 8, 6, 6 },
char114[] = { 8, 249, 6, 253, 251, 253, 9, 192, 0, 253, 1, 254, 254, 0, 252,
	      2, 251, 5, 251 },
char115[] = { 17, 248, 9, 6, 254, 5, 252, 2, 251, 255, 251, 252, 252, 251,
	      254, 252, 0, 254, 1, 3, 2, 5, 3, 6, 5, 6, 6, 5, 8, 2, 9, 255, 9,
	      252, 8, 251, 6 },
char116[] = { 8, 251, 7, 0, 244, 0, 5, 1, 8, 3, 9, 5, 9, 192, 0, 253, 251, 4,
	      251 },
char117[] = { 10, 247, 10, 251, 251, 251, 5, 252, 8, 254, 9, 1, 9, 3, 8, 6, 5,
	      192, 0, 6, 251, 6, 9 },
char118[] = { 5, 248, 8, 250, 251, 0, 9, 192, 0, 6, 251, 0, 9 },
char119[] = { 11, 245, 11, 248, 251, 252, 9, 192, 0, 0, 251, 252, 9, 192, 0,
	      0, 251, 4, 9, 192, 0, 8, 251, 4, 9 },
char120[] = { 5, 248, 9, 251, 251, 6, 9, 192, 0, 6, 251, 251, 9 },
char121[] = { 9, 248, 8, 250, 251, 0, 9, 192, 0, 6, 251, 0, 9, 254, 13, 252,
	      15, 250, 16, 249, 16 },
char122[] = { 8, 248, 9, 6, 251, 251, 9, 192, 0, 251, 251, 6, 251, 192, 0,
	      251, 9, 6, 9 },
char123[] = { 39, 249, 7, 2, 240, 0, 241, 255, 242, 254, 244, 254, 246, 255,
	      248, 0, 249, 1, 251, 1, 253, 255, 255, 192, 0, 0, 241, 255, 243,
	      255, 245, 0, 247, 1, 248, 2, 250, 2, 252, 1, 254, 253, 0, 1, 2,
	      2, 4, 2, 6, 1, 8, 0, 9, 255, 11, 255, 13, 0, 15, 192, 0, 255, 1,
	      1, 3, 1, 5, 0, 7, 255, 8, 254, 10, 254, 12, 255, 14, 0, 15, 2,
	      16 },
char124[] = { 2, 252, 4, 0, 240, 0, 16 },
char125[] = { 39, 249, 7, 254, 240, 0, 241, 1, 242, 2, 244, 2, 246, 1, 248, 0,
	      249, 255, 251, 255, 253, 1, 255, 192, 0, 0, 241, 1, 243, 1, 245,
	      0, 247, 255, 248, 254, 250, 254, 252, 255, 254, 3, 0, 255, 2,
	      254, 4, 254, 6, 255, 8, 0, 9, 1, 11, 1, 13, 0, 15, 192, 0, 1, 1,
	      255, 3, 255, 5, 0, 7, 1, 8, 2, 10, 2, 12, 1, 14, 0, 15, 254, 16,
	      },
char126[] = { 23, 255, 21, 2, 1, 0, 255, 1, 253, 3, 251, 5, 251, 7, 252,
	      11, 255, 13, 0, 15, 0, 17, 255, 18, 254, 192, 0, 2, 0, 1,
	      254, 3, 253, 5, 253, 7, 254, 11, 1, 13, 2, 15, 2, 17, 1, 18,
	      255, 18, 252 };

/*-------------------------------------------------------------------------------*/

/* Pointers to character definition tables. */

static unsigned char *ctab[] = {
    char32, char33, char34, char35, char36, char37, char38, char39, char40,
    char41, char42, char43, char44, char45, char46, char47, char48, char49,
    char50, char51, char52, char53, char54, char55, char56, char57, char58,
    char59, char60, char61, char62, char63, char64, char65, char66, char67,
    char68, char69, char70, char71, char72, char73, char74, char75, char76,
    char77, char78, char79, char80, char81, char82, char83, char84, char85,
    char86, char87, char88, char89, char90, char91, char92, char93, char94,
    char95, char96, char97, char98, char99, char100, char101, char102,
    char103, char104, char105, char106, char107, char108, char109, char110,
    char111, char112, char113, char114, char115, char116, char117, char118,
    char119, char120, char121, char122, char123, char124, char125, char126
};

/* Table used to look up sine of angles from 0 through 90 degrees.
   The value returned is the sine * 65536.  Symmetry is used to
   obtain sine and cosine for arbitrary angles using this table. */

static long sintab[] = {
    0, 1143, 2287, 3429, 4571, 5711, 6850, 7986, 9120, 10252, 11380,
    12504, 13625, 14742, 15854, 16961, 18064, 19160, 20251, 21336,
    22414, 23486, 24550, 25606, 26655, 27696, 28729, 29752, 30767,
    31772, 32768, 33753, 34728, 35693, 36647, 37589, 38521, 39440,
    40347, 41243, 42125, 42995, 43852, 44695, 45525, 46340, 47142,
    47929, 48702, 49460, 50203, 50931, 51643, 52339, 53019, 53683,
    54331, 54963, 55577, 56175, 56755, 57319, 57864, 58393, 58903,
    59395, 59870, 60326, 60763, 61183, 61583, 61965, 62328, 62672,
    62997, 63302, 63589, 63856, 64103, 64331, 64540, 64729, 64898,
    65047, 65176, 65286, 65376, 65446, 65496, 65526, 65536
};

/*-------------------------------------------------------------------------------*/

/*  ISIN  --  Return sine of an angle in integral degrees.  The
	      value returned is 65536 times the sine.  */

static long isin(int deg)
{
    /* Domain reduce to 0 to 360 degrees. */

    if (deg < 0) {
	deg = (360 - ((- deg) % 360)) % 360;
    } else if (deg >= 360) {
	deg = deg % 360;
    }

    /* Now look up from table according to quadrant. */

    if (deg <= 90) {
	return sintab[deg];
    } else if (deg <= 180) {
	return sintab[180 - deg];
    } else if (deg <= 270) {
	return -sintab[deg - 180];
    }
    return -sintab[360 - deg];
}

/*-------------------------------------------------------------------------------*/

/*  ICOS  --  Return cosine of an angle in integral degrees.  The
	      value returned is 65536 times the cosine.  */

static long icos(int deg) { return isin(deg + 90); }

/*-------------------------------------------------------------------------------*/

#define Schar(x) (u = (x), (((u) & 0x80) ? ((u) | (-1 ^ 0xFF)) : (u)))

#define Scalef 21	/* Font design size */
#define Descend 9	/* Descender offset */

/*-------------------------------------------------------------------------------*/

/* PPMD_TEXT  --  Draw the zero-terminated  string  s, with  its  baseline
		  starting  at	point  (x, y), inclined by angle degrees to
		  the X axis, with letters height pixels  high	(descenders
		  will	extend below the baseline).  ppmd_line does the actual drawing */

void ppmd_text(byte *pixels, int cols, int rows,
               int x, int y, int height, int angle, char *s, byte r,byte g,byte b )
{
    int xpos = x, ypos = y;
    long rotsin, rotcos;

    x = y = 0;
    rotsin = isin(-angle);
    rotcos = icos(-angle);
    while (*s) {
	unsigned char ch = *s++;
	int pen = 1;
	int u;

        if (ch >= ' ' && ch < 127) {
            ch -= ' ';
	    if (ctab[ch] != NULL) {
		int cn = *ctab[ch];
		unsigned char *cst = ctab[ch] + 3;
		int lx, ly;

		x -= Schar(*(ctab[ch] + 1));
		lx = x + Schar(*cst++);
		ly = y + Schar(*cst++);
		while (--cn > 0) {
		    if (*cst == 192) {
			pen = 0;
			cst += 2;
		    } else {
			int nx = x + Schar(*cst++);
			int ny = y + Schar(*cst++);
			if (pen) {
			    int mx1, my1, mx2, my2;
			    int tx1, ty1, tx2, ty2;

                            /* Note that up until this  moment  we've  been
			       working	in  an	arbitrary model co-ordinate
			       system with  fixed  size  and  no  rotation.
			       Before  drawing	the  stroke,  transform  to
			       viewing co-ordinates to	honour	the  height
			       and angle specifications. */

			    mx1 = (lx * height) / Scalef;
			    my1 = ((ly - Descend) * height) / Scalef;
			    mx2 = (nx * height) / Scalef;
			    my2 = ((ny - Descend) * height) / Scalef;
			    tx1 = xpos + (mx1 * rotcos - my1 * rotsin) / 65536;
			    ty1 = ypos + (mx1 * rotsin + my1 * rotcos) / 65536;
			    tx2 = xpos + (mx2 * rotcos - my2 * rotsin) / 65536;
			    ty2 = ypos + (mx2 * rotsin + my2 * rotcos) / 65536;
			    ppmd_line(pixels, cols, rows, tx1, ty1, tx2, ty2, r,g,b ) ;
			}
			lx = nx;
			ly = ny;
			pen = 1;
		    }
		}
		x += *(ctab[ch] + 2);
	    }
        } else if (ch == '\n') {
	    y += Scalef + Descend;
	    x = 0;
	}
    }
}

/*-------------------------------------------------------------------------------*/

void mri_drawline( MRI_IMAGE *im , int x0,int y0, int x1,int y1, byte r,byte g,byte b )
{
   if( im == NULL || im->kind != MRI_rgb ) return ;
   ppmd_line( MRI_RGB_PTR(im) , im->nx , im->ny , x0,y0,x1,y1 , r,g,b ) ;
}

/*-------------------------------------------------------------------------------*/

void mri_drawfilledrectangle( MRI_IMAGE *im ,
                              int x, int y, int width, int height , byte r,byte g,byte b )
{
   if( im == NULL || im->kind != MRI_rgb ) return ;
   ppmd_filledrectangle( MRI_RGB_PTR(im) , im->nx , im->ny , x,y,width,height , r,g,b ) ;
}

/*-------------------------------------------------------------------------------*/

void mri_drawemptyrectangle( MRI_IMAGE *im ,
                             int x, int y, int width, int height , byte r,byte g,byte b )
{
    register int cx, cy, cwidth, cheight, col, row;

    if( im == NULL || im->kind != MRI_rgb ) return ;

    /* Clip. */
    cx = x; cy = y; cwidth = width; cheight = height;
    if ( cx < 0 ) { cx = 0; cwidth += x; }
    if ( cy < 0 ) { cy = 0; cheight += y; }
    if ( cx + cwidth > im->nx ) cwidth = im->nx - cx;
    if ( cy + cheight > im->ny ) cheight = im->ny - cy;

    ppmd_line( MRI_RGB_PTR(im), im->nx,im->ny, cx      ,cy       , cx+width,cy       , r,g,b );
    ppmd_line( MRI_RGB_PTR(im), im->nx,im->ny, cx+width,cy       , cx+width,cy+height, r,g,b );
    ppmd_line( MRI_RGB_PTR(im), im->nx,im->ny, cx+width,cy+height, cx      ,cy+height, r,g,b );
    ppmd_line( MRI_RGB_PTR(im), im->nx,im->ny, cx      ,cy+height, cx      ,cy       , r,g,b );
}

/*-------------------------------------------------------------------------------*/

void mri_drawtext( MRI_IMAGE *im ,
                   int x, int y, int height, int angle, char *s,
                   byte r,byte g,byte b )
{
    if( im == NULL || im->kind != MRI_rgb ) return ;
    ppmd_text( MRI_RGB_PTR(im), im->nx,im->ny, x,y,height,angle , s , r,g,b ) ;
}
