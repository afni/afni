/*
 * vp_warp.c
 *
 * Support routines for 1-pass image warping.
 *
 * Copyright (c) 1994 The Board of Trustees of The Leland Stanford
 * Junior University.  All rights reserved.
 *
 * Permission to use, copy, modify and distribute this software and its
 * documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice and this permission notice appear in
 * all copies of this software and that you do not sell the software.
 * Commercial licensing is available by contacting the author.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS" AND WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
 *
 * Author:
 *    Phil Lacroute
 *    Computer Systems Laboratory
 *    Electrical Engineering Dept.
 *    Stanford University
 */

/*
 * $Date$
 * $Revision$
 */

#include "vp_global.h"

/* weights for bilirp filter; index with (yfrac, xfrac, tap number) */
float VPBilirpWeight[WARP_WEIGHT_ENTRIES][WARP_WEIGHT_ENTRIES][4];
static int BilirpWeightsReady = 0;	/* true if weight table initialized */

static void OrderCoords ANSI_ARGS((double coords[4][2], double lft[3][2],
    double rgt[3][2]));

/*
 * VPComputeWarpTables
 *
 * Precompute lookup tables for fast filter convolution during warping.
 */

void
VPComputeWarpTables()
{
    float *wptr;	/* pointer into weight table */

    int x, y;
    double in_x, in_y;

    if (BilirpWeightsReady)
	return;

#ifdef MEMSPY
    bin_init(BinNumber(__LINE__, __FILE__, "VPBilirpWeight"), -1, -1,
	     VPBilirpWeight, sizeof(VPBilirpWeight), "VPBilirpWeight");
#endif

    wptr = &VPBilirpWeight[0][0][0];
    for (y = 0; y < WARP_WEIGHT_ENTRIES; y++) {
	in_y = (double)y / (WARP_WEIGHT_ENTRIES-1);
	for (x = 0; x < WARP_WEIGHT_ENTRIES; x++) {
	    in_x = (double)x / (WARP_WEIGHT_ENTRIES-1);
	    *wptr++ = (1. - in_x)*(1. - in_y);
	    *wptr++ = in_x * (1. - in_y);
	    *wptr++ = (1. - in_x) * in_y;
	    *wptr++ = 1. - wptr[-1] - wptr[-2] - wptr[-3];
	}
    }

    BilirpWeightsReady = 1;
}

/*
 * VPAffineComputeImageOverlap
 *
 * Compute the intersection of the intermediate image and the final image.
 * This intersection is broken up into a series of trapezoids whose
 * bases are parallel to the scanlines of the final image (for ease
 * of scan conversion).  Two sets of trapezoids are computed: one set
 * gives the region of the final image for which the filter kernel is
 * completely contained within the intermedaite image.  The second set of
 * trapezoids gives the region of the final image for which the filter
 * kernel overlaps at least some of the intermedaite image, but may or may not
 * be completely contained.  Any region of the final image which is not
 * within the second set of trapezoids is not affected by the intermediate
 * image at all and should be set to the background color.
 */

void
VPAffineImageOverlap(in_width, in_height, out_width, out_height,
		     warp_matrix, filter_width, full_overlap, part_overlap)
int in_width, in_height;	/* input (intermediate) image size */
int out_width, out_height;	/* output (final) image size */
vpMatrix3 warp_matrix;		/* affine transformation from input image
				   to output image */
double filter_width;		/* filter kernel width in input image
				   coordinates */
Trapezoid full_overlap[9]; /* portion of the intersection with full
				      filter kernel overlap */
Trapezoid part_overlap[9]; /* portion of the intersection with partial
				      filter kernel overlap */
{
    double int_lft[3][2];	/* coordinates bounding the left side of the
				   full_overlap region in output coordinates,
				   sorted from top (lowest y) to bottom */
    double int_rgt[3][2];	/* right side of full_overlap region */
    double ext_lft[3][2];	/* left side of part_overlap region */
    double ext_rgt[3][2];	/* right side of part_overlap region */
    double coords[4][2];
    double inset;
    int ilft, irgt, elft, ergt;
    int region;
    int lasty, nexty, y;

    /* compute the bounding box of the full_overlap region and store it
       in int_lft and int_rgt */
    inset = -1.0 + filter_width / 2.0 + 1.0e-5;
    coords[0][0] = warp_matrix[0][0] * inset +
		   warp_matrix[0][1] * inset + 
		   warp_matrix[0][2];
    coords[0][1] = warp_matrix[1][0] * inset +
		   warp_matrix[1][1] * inset + 
		   warp_matrix[1][2];
    coords[1][0] = warp_matrix[0][0] * (in_width - 1 - inset) +
		   warp_matrix[0][1] * inset + 
		   warp_matrix[0][2];
    coords[1][1] = warp_matrix[1][0] * (in_width - 1 - inset) +
		   warp_matrix[1][1] * inset + 
		   warp_matrix[1][2];
    coords[2][0] = warp_matrix[0][0] * (in_width - 1 - inset) +
		   warp_matrix[0][1] * (in_height - 1 - inset) + 
		   warp_matrix[0][2];
    coords[2][1] = warp_matrix[1][0] * (in_width - 1 - inset) +
		   warp_matrix[1][1] * (in_height - 1 - inset) + 
		   warp_matrix[1][2];
    coords[3][0] = warp_matrix[0][0] * inset +
		   warp_matrix[0][1] * (in_height - 1 - inset) + 
		   warp_matrix[0][2];
    coords[3][1] = warp_matrix[1][0] * inset +
		   warp_matrix[1][1] * (in_height - 1 - inset) + 
		   warp_matrix[1][2];
    OrderCoords(coords, int_lft, int_rgt);

    /* compute the bounding box of the part_overlap region and store it
       in int_lft and int_rgt */
    inset = -filter_width / 2.0;
    coords[0][0] = warp_matrix[0][0] * inset +
		   warp_matrix[0][1] * inset + 
		   warp_matrix[0][2];
    coords[0][1] = warp_matrix[1][0] * inset +
		   warp_matrix[1][1] * inset + 
		   warp_matrix[1][2];
    coords[1][0] = warp_matrix[0][0] * (in_width - 1 - inset) +
		   warp_matrix[0][1] * inset + 
		   warp_matrix[0][2];
    coords[1][1] = warp_matrix[1][0] * (in_width - 1 - inset) +
		   warp_matrix[1][1] * inset + 
		   warp_matrix[1][2];
    coords[2][0] = warp_matrix[0][0] * (in_width - 1 - inset) +
		   warp_matrix[0][1] * (in_height - 1 - inset) + 
		   warp_matrix[0][2];
    coords[2][1] = warp_matrix[1][0] * (in_width - 1 - inset) +
		   warp_matrix[1][1] * (in_height - 1 - inset) + 
		   warp_matrix[1][2];
    coords[3][0] = warp_matrix[0][0] * inset +
		   warp_matrix[0][1] * (in_height - 1 - inset) + 
		   warp_matrix[0][2];
    coords[3][1] = warp_matrix[1][0] * inset +
		   warp_matrix[1][1] * (in_height - 1 - inset) + 
		   warp_matrix[1][2];
    OrderCoords(coords, ext_lft, ext_rgt);

    for (ilft = 0; ilft < 3 && int_lft[ilft][1] <= 0.; ilft++);
    for (irgt = 0; irgt < 3 && int_rgt[irgt][1] <= 0.; irgt++);
    for (elft = 0; elft < 3 && ext_lft[elft][1] <= 0.; elft++);
    for (ergt = 0; ergt < 3 && ext_rgt[ergt][1] <= 0.; ergt++);
    region = 0;
    lasty = -1;
    while (lasty < out_height-1) {
	ASSERT(region < 9);

	/* find nexty */
	nexty = out_height - 1;
	if (ilft < 3) {
	    y = (int)floor(int_lft[ilft][1]);
	    if (nexty > y)
		nexty = y;
	}
	if (irgt < 3) {
	    y = (int)floor(int_rgt[irgt][1]);
	    if (nexty > y)
		nexty = y;
	}
	if (elft < 3) {
	    y = (int)floor(ext_lft[elft][1]);
	    if (nexty > y)
		nexty = y;
	}
	if (ergt < 3) {
	    y = (int)floor(ext_rgt[ergt][1]);
	    if (nexty > y)
		nexty = y;
	}
	ASSERT((ilft == 0 && (int)floor(int_lft[0][1]) >= nexty) ||
	       (ilft == 3 && (int)floor(int_lft[2][1]) <= lasty) ||
	       (((int)floor(int_lft[ilft-1][1]) <= lasty || lasty == -1)
		&& (int)floor(int_lft[ilft][1]) >= nexty));
	ASSERT((irgt == 0 && (int)floor(int_rgt[0][1]) >= nexty) ||
	       (irgt == 3 && (int)floor(int_rgt[2][1]) <= lasty) ||
	       (((int)floor(int_rgt[irgt-1][1]) <= lasty || lasty == -1)
		&& (int)floor(int_rgt[irgt][1]) >= nexty));
	ASSERT((elft == 0 && (int)floor(ext_lft[0][1]) >= nexty) ||
	       (elft == 3 && (int)floor(ext_lft[2][1]) <= lasty) ||
	       (((int)floor(ext_lft[elft-1][1]) <= lasty || lasty == -1)
		&& (int)floor(ext_lft[elft][1]) >= nexty));
	ASSERT((ergt == 0 && (int)floor(ext_rgt[0][1]) >= nexty) ||
	       (ergt == 3 && (int)floor(ext_rgt[2][1]) <= lasty) ||
	       (((int)floor(ext_rgt[ergt-1][1]) <= lasty || lasty == -1)
		&& (int)floor(ext_rgt[ergt][1]) >= nexty));
	full_overlap[region].miny = lasty + 1;
	full_overlap[region].maxy = nexty;
	part_overlap[region].miny = lasty + 1;
	part_overlap[region].maxy = nexty;
	if (ilft == 0 || ilft == 3) {
	    /* this trapezoid does not intersect full_overlap */
	    full_overlap[region].x_top_lft = 0;
	    full_overlap[region].x_top_rgt = -1;
	    full_overlap[region].x_incr_lft = 0;
	    full_overlap[region].x_incr_rgt = 0;
	} else {
	    full_overlap[region].x_incr_lft = 
		(int_lft[ilft][0] - int_lft[ilft-1][0]) /
		(int_lft[ilft][1] - int_lft[ilft-1][1]);
	    full_overlap[region].x_top_lft =
		int_lft[ilft-1][0] + (lasty+1 - int_lft[ilft-1][1]) *
		full_overlap[region].x_incr_lft;
	    full_overlap[region].x_incr_rgt = 
		(int_rgt[irgt][0] - int_rgt[irgt-1][0]) /
		(int_rgt[irgt][1] - int_rgt[irgt-1][1]);
	    full_overlap[region].x_top_rgt =
		int_rgt[irgt-1][0] + (lasty+1 - int_rgt[irgt-1][1]) *
		full_overlap[region].x_incr_rgt;
	}
	if (elft == 0 || elft == 3) {
	    /* this trapezoid does not intersect part_overlap */
	    part_overlap[region].x_top_lft = 0;
	    part_overlap[region].x_top_rgt = -1;
	    part_overlap[region].x_incr_lft = 0;
	    part_overlap[region].x_incr_rgt = 0;
	} else {
	    part_overlap[region].x_incr_lft = 
		(ext_lft[elft][0] - ext_lft[elft-1][0]) /
		(ext_lft[elft][1] - ext_lft[elft-1][1]);
	    part_overlap[region].x_top_lft =
		ext_lft[elft-1][0] + (lasty+1 - ext_lft[elft-1][1]) *
		part_overlap[region].x_incr_lft;
	    part_overlap[region].x_incr_rgt = 
		(ext_rgt[ergt][0] - ext_rgt[ergt-1][0]) /
		(ext_rgt[ergt][1] - ext_rgt[ergt-1][1]);
	    part_overlap[region].x_top_rgt =
		ext_rgt[ergt-1][0] + (lasty+1 - ext_rgt[ergt-1][1]) *
		part_overlap[region].x_incr_rgt;
	}
	ASSERT(!(full_overlap[region].x_top_lft <= 
		 full_overlap[region].x_top_rgt &&
		 part_overlap[region].x_top_lft >
		 part_overlap[region].x_top_rgt));
	for (; ilft < 3 && (int)floor(int_lft[ilft][1]) <= nexty; ilft++);
	for (; irgt < 3 && (int)floor(int_rgt[irgt][1]) <= nexty; irgt++);
	for (; elft < 3 && (int)floor(ext_lft[elft][1]) <= nexty; elft++);
	for (; ergt < 3 && (int)floor(ext_rgt[ergt][1]) <= nexty; ergt++);
	region++;
	lasty = nexty;
    }
    for (; region < 9; region++) {
	full_overlap[region].miny = out_height;
	full_overlap[region].maxy = out_height;
	part_overlap[region].miny = out_height;
	part_overlap[region].maxy = out_height;
	full_overlap[region].x_top_lft = 0;
	full_overlap[region].x_top_rgt = -1;
	full_overlap[region].x_incr_lft = 0;
	full_overlap[region].x_incr_rgt = 0;
	part_overlap[region].x_top_lft = 0;
	part_overlap[region].x_top_rgt = -1;
	part_overlap[region].x_incr_lft = 0;
	part_overlap[region].x_incr_rgt = 0;
    }

#ifdef DEBUG_OVERLAP
    for (region = 0; region < 9; region++) {
	printf("region %d: y = %d to %d, [%d+%g [%d+%g %d+%g] %d+%g]\n",
	       region,
	       full_overlap[region].miny,
	       full_overlap[region].maxy,
	       (int)part_overlap[region].x_top_lft,
	       part_overlap[region].x_incr_lft,
	       (int)full_overlap[region].x_top_lft,
	       full_overlap[region].x_incr_lft,
	       (int)full_overlap[region].x_top_rgt,
	       full_overlap[region].x_incr_rgt,
	       (int)part_overlap[region].x_top_rgt,
	       part_overlap[region].x_incr_rgt);
    }
#endif
}

/*
 * OrderCoords
 *
 * Sort an array of coordinates.
 */

static void
OrderCoords(coords, lft, rgt)
double coords[4][2];	/* inputs */
double lft[3][2];	/* outputs */
double rgt[3][2];
{
    int index;
    double swap_buf;
    double sorted_coords[4][2];
    double xmid;

    /* sort the coordinates as follows:
       coords[0]: smallest y value; if there is a tie, then smallest x value
       		  to break the tie; this is the top or top left corner
       coords[1]: next coordinate in CCW order; this is the left or bottom
                  left corner
       coords[2]: next coordinate in CCW order; this is the bottom or
                  bottom right corner
       coords[3]: next coordinate in CCW order; this is the right or
       		  top right corner */

    /* search for coords[0] */
    index = 0;
    if (coords[1][1] < coords[index][1] ||
	(coords[1][1] == coords[index][1] && coords[1][0] < coords[index][0]))
	index = 1;
    if (coords[2][1] < coords[index][1] ||
	(coords[2][1] == coords[index][1] && coords[2][0] < coords[index][0]))
	index = 2;
    if (coords[3][1] < coords[index][1] ||
	(coords[3][1] == coords[index][1] && coords[3][0] < coords[index][0]))
	index = 3;
    sorted_coords[0][0] = coords[index][0];
    sorted_coords[0][1] = coords[index][1];

    /* coords[2] is the opposite corner */
    sorted_coords[2][0] = coords[(index+2)%4][0];
    sorted_coords[2][1] = coords[(index+2)%4][1];

    /* determine which of the remaining two coordinates is to the left
       of the line joining coords[0] and coords[2]; this coordinate
       is coords[1], and the final remaining coordinate is coords[3] */
    index = (index + 1) % 4;
    if (fabs(sorted_coords[0][1] - sorted_coords[2][1]) < VP_EPS) {
	/* input image is degenerate (transforms to a horizontal line) */
	lft[0][0] = sorted_coords[0][0];
	lft[0][1] = sorted_coords[0][1];
	lft[1][0] = sorted_coords[0][0];
	lft[1][1] = sorted_coords[0][1];
	lft[2][0] = sorted_coords[0][0];
	lft[2][1] = sorted_coords[0][1];
	rgt[0][0] = sorted_coords[2][0];
	rgt[0][1] = sorted_coords[2][1];
	rgt[1][0] = sorted_coords[2][0];
	rgt[1][1] = sorted_coords[2][1];
	rgt[2][0] = sorted_coords[2][0];
	rgt[2][1] = sorted_coords[2][1];
	return;
    }
    xmid = sorted_coords[0][0] + (coords[index][1] - sorted_coords[0][1]) *
	   (sorted_coords[2][0] - sorted_coords[0][0]) /
	   (sorted_coords[2][1] - sorted_coords[0][1]);
    if (coords[index][0] < xmid) {
	sorted_coords[1][0] = coords[index][0];
	sorted_coords[1][1] = coords[index][1];
	sorted_coords[3][0] = coords[(index+2)%4][0];
	sorted_coords[3][1] = coords[(index+2)%4][1];
    } else {
	sorted_coords[1][0] = coords[(index+2)%4][0];
	sorted_coords[1][1] = coords[(index+2)%4][1];
	sorted_coords[3][0] = coords[index][0];
	sorted_coords[3][1] = coords[index][1];
    }

    /* store the results in the output array */
    lft[0][0] = sorted_coords[0][0];
    lft[0][1] = sorted_coords[0][1];
    lft[1][0] = sorted_coords[1][0];
    lft[1][1] = sorted_coords[1][1];
    if (sorted_coords[1][1] == sorted_coords[2][1]) {
	lft[2][0] = sorted_coords[1][0];
	lft[2][1] = sorted_coords[1][1];
    } else {
	lft[2][0] = sorted_coords[2][0];
	lft[2][1] = sorted_coords[2][1];
    }
    if (sorted_coords[0][1] == sorted_coords[3][1]) {
	rgt[0][0] = sorted_coords[3][0];
	rgt[0][1] = sorted_coords[3][1];
	rgt[1][0] = sorted_coords[2][0];
	rgt[1][1] = sorted_coords[2][1];
	rgt[2][0] = sorted_coords[2][0];
	rgt[2][1] = sorted_coords[2][1];
    } else {
	rgt[0][0] = sorted_coords[0][0];
	rgt[0][1] = sorted_coords[0][1];
	rgt[1][0] = sorted_coords[3][0];
	rgt[1][1] = sorted_coords[3][1];
	rgt[2][0] = sorted_coords[2][0];
	rgt[2][1] = sorted_coords[2][1];
    }
}
