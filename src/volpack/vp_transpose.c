/*
 * vp_transpose.c
 *
 * Routines to transpose a raw volume.
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

static int TransposeBlock ANSI_ARGS((void *src, int src_xstride,
    int src_ystride, int src_zstride, void *dst, int dst_xstride,
    int dst_ystride, int dst_zstride, int xlen, int ylen, int zlen,
    int bytes_per_voxel));

/*
 * vpTranspose
 *
 * Transpose the raw volume data.
 */

vpResult
vpTranspose(vpc, kaxis)
vpContext *vpc;	/* context */
int kaxis;	/* axis which will have the largest stride after transposing */
{
    void *blk;			/* buffer to store data during transpose */
    int xlen, ylen, zlen;	/* volume size */
    int src_xstride, src_ystride, src_zstride; /* strides of src voxels */
    int dst_xstride, dst_ystride, dst_zstride; /* strides of dst voxels */
    int bytes_per_voxel;	/* size of voxel */
    int retcode;

    /* XXX replace with a blocked algorithm to conserve memory and
       improve cache performance */

    /* check for errors */
    if ((retcode = VPCheckRawVolume(vpc)) != VP_OK)
	return(retcode);

    /* decide on the new strides */
    xlen = vpc->xlen;
    ylen = vpc->ylen;
    zlen = vpc->zlen;
    src_xstride = vpc->xstride;
    src_ystride = vpc->ystride;
    src_zstride = vpc->zstride;
    bytes_per_voxel = vpc->raw_bytes_per_voxel;
    switch (kaxis) {
    case VP_X_AXIS:
	dst_xstride = ylen*zlen*bytes_per_voxel;
	dst_ystride = bytes_per_voxel;
	dst_zstride = ylen*bytes_per_voxel;
	break;
    case VP_Y_AXIS:
	dst_xstride = zlen*bytes_per_voxel;
	dst_ystride = zlen*xlen*bytes_per_voxel;
	dst_zstride = bytes_per_voxel;
	break;
    case VP_Z_AXIS:
	dst_xstride = bytes_per_voxel;
	dst_ystride = xlen*bytes_per_voxel;
	dst_zstride = xlen*ylen*bytes_per_voxel;
	break;
    default:
	return(VPSetError(vpc, VPERROR_BAD_OPTION));
    }
    if (src_xstride == dst_xstride && src_ystride == dst_ystride &&
	src_zstride == dst_zstride)
	return(VP_OK);

    /* transpose volume */
    Alloc(vpc, blk, void *, xlen*ylen*zlen*bytes_per_voxel,
	  "transpose_tmp");
    TransposeBlock(vpc->raw_voxels, src_xstride, src_ystride, src_zstride,
		   blk, dst_xstride, dst_ystride, dst_zstride,
		   xlen, ylen, zlen, bytes_per_voxel);
    bcopy(blk, vpc->raw_voxels, xlen*ylen*zlen*bytes_per_voxel);
    Dealloc(vpc, blk);
    vpc->xstride = dst_xstride;
    vpc->ystride = dst_ystride;
    vpc->zstride = dst_zstride;
    return(VP_OK);
}

/*
 * TransposeBlock
 *
 * Transpose a block of volume data by copying it from a source array
 * to a destination array using the indicated strides.
 */

static int
TransposeBlock(src, src_xstride, src_ystride, src_zstride, dst, dst_xstride,
	       dst_ystride, dst_zstride, xlen, ylen, zlen, bytes_per_voxel)
void *src;		/* source array */
int src_xstride;	/* strides for source array */
int src_ystride;
int src_zstride;
void *dst;		/* destination array */
int dst_xstride;	/* strides for destination array */
int dst_ystride;
int dst_zstride;
int xlen, ylen, zlen;	/* size of block in voxels per side */
int bytes_per_voxel;	/* size of a voxel */
{
    int x, y, z, b;
    unsigned char *src_ptr;
    unsigned char *dst_ptr;

    src_ptr = src;
    dst_ptr = dst;
    for (z = 0; z < zlen; z++) {
	for (y = 0; y < ylen; y++) {
	    for (x = 0; x < xlen; x++) {
		for (b = 0; b < bytes_per_voxel; b++)
		    dst_ptr[b] = src_ptr[b];
		src_ptr += src_xstride;
		dst_ptr += dst_xstride;
	    }
	    src_ptr += src_ystride - xlen*src_xstride;
	    dst_ptr += dst_ystride - xlen*dst_xstride;
	}
	src_ptr += src_zstride - ylen*src_ystride;
	dst_ptr += dst_zstride - ylen*dst_ystride;
    }
    return 0 ;  /* RWCox */
}
