/*
 * vp_renderB.c
 *
 * Brute-force shear-warp volume rendering algorithm.
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

static void AffineBruteForceRender ANSI_ARGS((vpContext *vpc));
static void ClassifySlice ANSI_ARGS((vpContext *vpc, int slicenum,
    float *opc_slice));
static void ShadeSlice ANSI_ARGS((vpContext *vpc, int slicenum,
    float *clr_slice));
static void ScaleColors ANSI_ARGS((double scale, float *clr_slice,
    int width, int height, int color_channels));
static void AlphaScaleColors ANSI_ARGS((float *opc_slice, float *clr_slice,
    int width, int height, int color_channels));
static void DepthCueSlice ANSI_ARGS((vpContext *vpc, float *clr_slice,
    int width, int height, int color_channels, double depth_00k,
    double depth_di, double depth_dj));
static void TranslateSlice ANSI_ARGS((float *opc_slice, float *clr_slice,
    int width, int height, double WgtTL_d, double WgtBL_d, double WgtTR_d,
    double WgtBR_d, int color_channels, float *resamp_opc_slice, 
    float *resmp_clr_slice));
static void CompositeSlice ANSI_ARGS((float *resamp_opc, float *resamp_clr,
    int width, int height, int color_channels, void *int_image_ptr,
    int int_image_width, double min_opacity));
static void AffineBruteForceWarp ANSI_ARGS((vpContext *vpc));

#define RWCOX                  /* attempts to speed up a little */
#ifdef RWCOX
static float max_opacity ;
#endif

/*
 * vpBruteForceRender
 *
 * Render an unclassified volume using the basic shear-warp algorithm
 * without any optimizations (no spatial data structure is used and
 * coherence is ignored).  Use this routine as a standard for
 * correctness checking.
 */

vpResult
vpBruteForceRender(vpc)
vpContext *vpc;
{
    int retcode;

    /* check for errors and initialize */
    if ((retcode = VPCheckRawVolume(vpc)) != VP_OK)
	return(retcode);
    if ((retcode = VPCheckClassifier(vpc)) != VP_OK)
	return(retcode);
    if ((retcode = VPCheckShader(vpc)) != VP_OK)
	return(retcode);
    if ((retcode = VPCheckImage(vpc)) != VP_OK)
	return(retcode);
    if ((retcode = VPFactorView(vpc)) != VP_OK)
	return(retcode);

#ifdef RWCOX
    max_opacity = vpc->max_opacity ;
#endif

    /* render */
    if (vpc->affine_view)
	AffineBruteForceRender(vpc);
    else
	return(VPSetError(vpc, VPERROR_BAD_OPTION));

    return(VP_OK);
}

/*
 * AffineBruteForceRender
 *
 * Render an unclassified volume using the brute-force shear-warp
 * algorithm for an affine view transformation.
 */

static void
AffineBruteForceRender(vpc)
vpContext *vpc;
{
    int icount;			/* voxels per voxel scanline */
    int jcount;			/* voxel scanlines per voxel slice */
    int kcount;			/* voxel slices in the volume */
    int k;			/* voxel slice index */
    int kstart, kstop;		/* values of k for first and last slices */
    int kincr;			/* value to add to k to get to the next slice
				   (either 1 or -1) */
    float slice_u, slice_v;	/* sheared object space coordinates of the
				   top-left corner of the current constant-k
				   slice of the volume data */
    int slice_u_int;		/* integer part of slice_u and slice_v */
    int slice_v_int;
    float slice_u_frac;		/* fractional part of slice_u and slice_v */
    float slice_v_frac;
    int slice_start_index;	/* index of top-left int. image pixel */
    float WgtTL, WgtBL,		/* weights in the range 0..1 which give the */
	  WgtTR, WgtBR;		/*   fractional contribution of the */
    				/*   neighboring voxels to the current */
    			        /*   intermediate image pixel */
    int color_channels;		/* number of color channels to compute */
    float *opc_slice;		/* opacities after correction for viewpoint */
    float *resamp_opc_slice;	/* opacities after resampling */
    float *clr_slice;		/* colors for current voxel slice */
    float *resamp_clr_slice;	/* colors after resampling */
#ifdef FAST_DEPTH_CUEING
    float slice_depth_cueing;	/* depth cueing factor for current slice */
    float slice_dc_ratio;	/* multiplier to get depth cueing factor
				   for the next slice */
#endif
    void *intim;		/* intermediate image pointer */

    Debug((vpc, VPDEBUG_RENDER, "Algorithm: affine brute force\n"));

    /* find size of volume */
    switch (vpc->best_view_axis) {
    case VP_X_AXIS:
	icount = vpc->ylen;
	jcount = vpc->zlen;
	kcount = vpc->xlen;
	break;
    case VP_Y_AXIS:
	icount = vpc->zlen;
	jcount = vpc->xlen;
	kcount = vpc->ylen;
	break;
    case VP_Z_AXIS:
	icount = vpc->xlen;
	jcount = vpc->ylen;
	kcount = vpc->zlen;
	break;
    default:
	VPBug("invalid viewing axis in AffineBruteForceRender");
    }

    /* initialize intermediate image */
    color_channels = vpc->color_channels;
    vpc->pad_int_to_maxwidth = 0;
    if (color_channels == 1) {
	bzero(vpc->int_image.gray_intim, vpc->intermediate_width *
	      vpc->intermediate_height * sizeof(GrayIntPixel));
    } else {
	ASSERT(color_channels == 3);
	bzero(vpc->int_image.rgb_intim, vpc->intermediate_width *
	      vpc->intermediate_height * sizeof(RGBIntPixel));
    }

    /* allocate memory for shaded and resampled voxel slices */
    Alloc(vpc, opc_slice, float *, icount*jcount*sizeof(float), "opc_slice");
    Alloc(vpc, resamp_opc_slice, float *, (icount+1)*(jcount+1)*sizeof(float),
	  "resamp_opc_slice");
    Alloc(vpc, clr_slice, float *, color_channels*icount*jcount*sizeof(float),
	  "clr_slice");
    Alloc(vpc, resamp_clr_slice, float *,
	  color_channels*(icount+1)*(jcount+1)*sizeof(float),
	  "resamp_clr_slice");

#ifdef FAST_DEPTH_CUEING
    /* initialize depth cueing */
    if (vpc->dc_enable) {
	slice_dc_ratio = VPSliceDepthCueRatio(vpc);
	slice_depth_cueing = 1.;
    }
#endif

    /* compute outer loop bounds */
    if (vpc->reverse_slice_order) {
	kstart = kcount-1;
	kstop = -1;
	kincr = -1;
    } else {
	kstart = 0;
	kincr = 1;
	kstop = kcount;
    }

    /* loop over slices of the voxel data in front-to-back order */
    for (k = kstart; k != kstop; k += kincr) {
	ReportStatus(vpc, (double)(k - kstart) / (double)(kstop - kstart));

	/* compute coordinates of top-left corner of slice in
	   sheared object space */
	slice_u = vpc->shear_i * k + vpc->trans_i;
	slice_v = vpc->shear_j * k + vpc->trans_j;
	slice_u_int = (int)ceil(slice_u) - 1;
	slice_v_int = (int)ceil(slice_v) - 1;

	/* compute resampling weights for this slice */
	slice_u_frac = slice_u - slice_u_int;
	slice_v_frac = slice_v - slice_v_int;
	WgtTL = slice_u_frac * slice_v_frac;
	WgtBL = slice_u_frac * ((float)1. - slice_v_frac);
	WgtTR = ((float)1. - slice_u_frac) * slice_v_frac;
	WgtBR = ((float)1. - slice_u_frac) * ((float)1. - slice_v_frac);

	/* classify the slice of voxels */
	ClassifySlice(vpc, k, opc_slice);

	/* shade the slice of voxels */
	ShadeSlice(vpc, k, clr_slice);

	/* perform depth cueing on the slice */
	if (vpc->dc_enable) {
#ifdef FAST_DEPTH_CUEING
	    ScaleColors(slice_depth_cueing, clr_slice, icount, jcount,
			color_channels);
	    slice_depth_cueing *= slice_dc_ratio;
#else
	    DepthCueSlice(vpc, clr_slice, icount, jcount, color_channels,
			  vpc->depth_000 + k*vpc->depth_dk,
			  vpc->depth_di, vpc->depth_dj);
#endif
	}

	/* weight the voxels colors by the voxel opacities */
	AlphaScaleColors(opc_slice, clr_slice, icount, jcount, color_channels);

	/* resample the slice of voxels */
	TranslateSlice(opc_slice, clr_slice, icount, jcount,
		       WgtTL, WgtBL, WgtTR, WgtBR, color_channels,
		       resamp_opc_slice, resamp_clr_slice);

	/* composite the slice of resampled voxels */
	slice_start_index = slice_u_int + slice_v_int*vpc->intermediate_width;
	if (color_channels == 1)
	    intim = &vpc->int_image.gray_intim[slice_start_index];
	else
	    intim = &vpc->int_image.rgb_intim[slice_start_index];
	CompositeSlice(resamp_opc_slice, resamp_clr_slice, icount+1, jcount+1,
		       color_channels, intim, vpc->intermediate_width,
		       vpc->min_opacity);
    }
    ReportStatus(vpc, 1.0);

#ifdef FAST_DEPTH_CUEING
    /* depth cue the intermediate image */
    if (vpc->dc_enable)
	VPDepthCueIntImage(vpc, vpc->reverse_slice_order ? kcount-1 : 0);
#endif

    /* warp the intermediate image into the final image */
    AffineBruteForceWarp(vpc);

    /* clean up */
    Dealloc(vpc, opc_slice);
    Dealloc(vpc, resamp_opc_slice);
    Dealloc(vpc, clr_slice);
    Dealloc(vpc, resamp_clr_slice);
}

/*
 * ClassifySlice
 *
 * Classify a slice of voxels.
 */

static void
ClassifySlice(vpc, slicenum, opc_slice)
vpContext *vpc;
int slicenum;
float *opc_slice;
{
    switch (vpc->best_view_axis) {
    case VP_X_AXIS:
	VPClassifyBlock(vpc, 1, slicenum, 0, 0, slicenum, vpc->ylen-1,
			vpc->zlen-1, opc_slice, 0, sizeof(float),
			vpc->ylen*sizeof(float));
	break;
    case VP_Y_AXIS:
	VPClassifyBlock(vpc, 1, 0, slicenum, 0, vpc->xlen-1, slicenum,
			vpc->zlen-1, opc_slice, vpc->zlen*sizeof(float),
			0, sizeof(float));
	break;
    case VP_Z_AXIS:
	VPClassifyBlock(vpc, 1, 0, 0, slicenum, vpc->xlen-1, vpc->ylen-1,
			slicenum, opc_slice, sizeof(float),
			vpc->xlen*sizeof(float), 0);
	break;
    }
}

/*
 * ShadeSlice
 *
 * Shade a slice of voxels.
 */

static void
ShadeSlice(vpc, slicenum, clr_slice)
vpContext *vpc;
int slicenum;
float *clr_slice;
{
    int color_bytes;

    color_bytes = sizeof(float) * vpc->color_channels;

    switch (vpc->best_view_axis) {
    case VP_X_AXIS:
	VPShadeBlock(vpc, slicenum, 0, 0, slicenum, vpc->ylen-1, vpc->zlen-1,
		     clr_slice, 0, color_bytes, vpc->ylen*color_bytes);
	break;
    case VP_Y_AXIS:
	VPShadeBlock(vpc, 0, slicenum, 0, vpc->xlen-1, slicenum, vpc->zlen-1,
		     clr_slice, vpc->zlen*color_bytes, 0, color_bytes);
	break;
    case VP_Z_AXIS:
	VPShadeBlock(vpc, 0, 0, slicenum, vpc->xlen-1, vpc->ylen-1, slicenum,
		     clr_slice, color_bytes, vpc->xlen*color_bytes, 0);
	break;
    }
}

/*
 * ScaleColors
 *
 * Weight voxel colors by a constant factor for the whole slice.
 */

static void
ScaleColors(scale, clr_slice, width, height, color_channels)
double scale;
float *clr_slice;
int width;
int height;
int color_channels;
{
    int i, j;
    float s;

    s = scale;

#ifndef RWCOX
    for (j = 0; j < height; j++) {
	for (i = 0; i < width; i++) {
	    if (color_channels == 1) {
		clr_slice[0] *= s;
	    } else {
		clr_slice[0] *= s;
		clr_slice[1] *= s;
		clr_slice[2] *= s;
	    }
	    clr_slice += color_channels;
	}
    }
#else
    if( color_channels == 1 ){
     for (j = 0; j < height; j++) {
	for (i = 0; i < width; i++) {
		clr_slice[0] *= s;
	    clr_slice ++;
	}
     }
    } else {
     for (j = 0; j < height; j++) {
	for (i = 0; i < width; i++) {
		clr_slice[0] *= s;
		clr_slice[1] *= s;
		clr_slice[2] *= s;
	    clr_slice += 3;
	}
     }
   }
#endif
}

/*
 * AlphaScaleColors
 *
 * Weight voxel colors by voxels opacities.
 */

static void
AlphaScaleColors(opc_slice, clr_slice, width, height, color_channels)
float *opc_slice;	/* 2D array of opacities (width by height) */
float *clr_slice;	/* 2D array of colors (width by height) */
int width;		/* size of voxel slice */
int height;
int color_channels;	/* number of color channels in clr_slice */
{
    int i, j;

#ifndef RWCOX
    for (j = 0; j < height; j++) {
	for (i = 0; i < width; i++) {
	    if (color_channels == 1) {
		clr_slice[0] *= opc_slice[0];
	    } else {
		clr_slice[0] *= opc_slice[0];
		clr_slice[1] *= opc_slice[0];
		clr_slice[2] *= opc_slice[0];
	    }
	    clr_slice += color_channels;
	    opc_slice++;
	}
    }
#else
    if( color_channels == 1 ){
     for (j = 0; j < height; j++) {
	for (i = 0; i < width; i++) {
		clr_slice[0] *= opc_slice[0];
	    clr_slice++;
	    opc_slice++;
	}
     }
    } else {
     for (j = 0; j < height; j++) {
	for (i = 0; i < width; i++) {
		clr_slice[0] *= opc_slice[0];
		clr_slice[1] *= opc_slice[0];
		clr_slice[2] *= opc_slice[0];
	    clr_slice += 3;
	    opc_slice++;
	}
     }
   }
#endif
}

/*
 * DepthCueSlice
 *
 * Apply depth cueing factor to each voxel in a slice.
 */

static void
DepthCueSlice(vpc, clr_slice, width, height, color_channels,
	      depth_00k, depth_di, depth_dj)
vpContext *vpc;
float *clr_slice;
int width;
int height;
int color_channels;
double depth_00k;		/* depth of top-left voxel in slice */
double depth_di, depth_dj;	/* change in depth for unit change in
				   i/j directions */
{
    int i, j;
    double depth, depth_0jk, factor;
    double dc_front_factor, dc_density;

    dc_front_factor = vpc->dc_front_factor;
    dc_density = vpc->dc_density;
    depth_0jk = depth_00k;
    for (j = 0; j < height; j++) {
	depth = depth_0jk;
	for (i = 0; i < width; i++) {
	    if (depth < 0.0)
		factor = dc_front_factor * exp(-dc_density);
	    else
		factor = dc_front_factor * exp(-dc_density * (1.0 - depth));
	    if (color_channels == 1) {
		clr_slice[0] *= factor;
	    } else {
		clr_slice[0] *= factor;
		clr_slice[1] *= factor;
		clr_slice[2] *= factor;
	    }
	    clr_slice += color_channels;
	    depth += depth_di;
	}
	depth_0jk += depth_dj;
    }
}

/*
 * TranslateSlice
 *
 * Translate and resample a slice of voxels.
 */

static void
TranslateSlice(opc_slice, clr_slice, width, height,
	       WgtTL_d, WgtBL_d, WgtTR_d, WgtBR_d,
	       color_channels, resamp_opc_slice, resamp_clr_slice)
float *opc_slice;	/* 2D array of opacities (width by height) */
float *clr_slice;	/* 2D array of colors (width by height) */
int width;		/* size of voxel slice */
int height;
double WgtTL_d;		/* resampling weights */
double WgtBL_d;
double WgtTR_d;
double WgtBR_d;
int color_channels;	/* number of color channels in clr_slice */
float *resamp_opc_slice;/* 2D array for storing resampled opacities
			   (width+1 by height+1) */
float *resamp_clr_slice;/* 2D array for storing resampled colors
			   (width+1 by height+1) */
{
    int i, j;
    float WgtTL, WgtBL, WgtTR, WgtBR;
    float OpcAcc, RClrAcc, GClrAcc, BClrAcc;

    WgtTL = WgtTL_d;
    WgtBL = WgtBL_d;
    WgtTR = WgtTR_d;
    WgtBR = WgtBR_d;
    for (j = 0; j <= height; j++) {
	for (i = 0; i <= width; i++) {
	    OpcAcc = 0.;
	    RClrAcc = 0.;
	    GClrAcc = 0.;
	    BClrAcc = 0.;
	    if (i > 0 && j > 0) {
		OpcAcc += WgtTL * opc_slice[-1-width];
		if (color_channels == 1) {
		    RClrAcc += WgtTL * clr_slice[-1-width];
		} else {
		    RClrAcc += WgtTL * clr_slice[3*(-1-width)];
		    GClrAcc += WgtTL * clr_slice[3*(-1-width)+1];
		    BClrAcc += WgtTL * clr_slice[3*(-1-width)+2];
		}
	    }
	    if (i > 0 && j < height) {
		OpcAcc += WgtBL * opc_slice[-1];
		if (color_channels == 1) {
		    RClrAcc += WgtBL * clr_slice[-1];
		} else {
		    RClrAcc += WgtBL * clr_slice[3*(-1)];
		    GClrAcc += WgtBL * clr_slice[3*(-1)+1];
		    BClrAcc += WgtBL * clr_slice[3*(-1)+2];
		}
	    }
	    if (i < width && j > 0) {
		OpcAcc += WgtTR * opc_slice[-width];
		if (color_channels == 1) {
		    RClrAcc += WgtTR * clr_slice[-width];
		} else {
		    RClrAcc += WgtTR * clr_slice[3*(-width)];
		    GClrAcc += WgtTR * clr_slice[3*(-width)+1];
		    BClrAcc += WgtTR * clr_slice[3*(-width)+2];
		}
	    }
	    if (i < width && j < height) {
		OpcAcc += WgtBR * opc_slice[0];
		if (color_channels == 1) {
		    RClrAcc += WgtBR * clr_slice[0];
		} else {
		    RClrAcc += WgtBR * clr_slice[3*(0)];
		    GClrAcc += WgtBR * clr_slice[3*(0)+1];
		    BClrAcc += WgtBR * clr_slice[3*(0)+2];
		}
	    }
	    *resamp_opc_slice = OpcAcc;
	    if (color_channels == 1) {
		*resamp_clr_slice = RClrAcc;
	    } else {
		resamp_clr_slice[0] = RClrAcc;
		resamp_clr_slice[1] = GClrAcc;
		resamp_clr_slice[2] = BClrAcc;
	    }
	    resamp_opc_slice++;
	    resamp_clr_slice += color_channels;
	    if (i != width) {
		opc_slice++;
		clr_slice += color_channels;;
	    }
	}
    }
}

/*
 * CompositeSlice
 *
 * Composite a resampled slice of voxels into the intermediate image.
 */

static void
CompositeSlice(resamp_opc, resamp_clr, width, height, color_channels,
	       int_image_ptr, int_image_width, min_opacity)
float *resamp_opc;	/* array of resampled opacities (width by height) */
float *resamp_clr;	/* array of resampled colors (width by height) */
int width;		/* size of resampled voxel arrays */
int height;
int color_channels;	/* number of color channels */
void *int_image_ptr;	/* pointer to intermediate image pixel corresponding
			   to top-left resampled voxel */
int int_image_width;	/* number of pixels in intermediate image scanline */
double min_opacity;	/* low opacity threshold */
{
    int i, j;
    float old_opc, old_r, old_g, old_b;
    float new_opc, new_r, new_g, new_b;
    GrayIntPixel *gray_intim;
    RGBIntPixel *rgb_intim;

    if (color_channels == 1)
	gray_intim = int_image_ptr;
    else
	rgb_intim = int_image_ptr;
    for (j = 0; j < height; j++) {
	for (i = 0; i < width; i++) {
	    if (*resamp_opc > min_opacity) {
		if (color_channels == 1) {
		    old_opc = gray_intim->opcflt;
#ifdef RWCOX
                    if( old_opc < max_opacity ){
#endif
		    old_r = gray_intim->clrflt;
		    new_opc = old_opc + *resamp_opc * ((float)1. - old_opc);
		    new_r = old_r + *resamp_clr * ((float)1. - old_opc);
		    gray_intim->opcflt = new_opc;
		    gray_intim->clrflt = new_r;
#ifdef RWCOX
                    }
#endif
		} else {
		    old_opc = rgb_intim->opcflt;
#ifdef RWCOX
                    if( old_opc < max_opacity ){
#endif
		    old_r = rgb_intim->rclrflt;
		    old_g = rgb_intim->gclrflt;
		    old_b = rgb_intim->bclrflt;
		    new_opc = old_opc + *resamp_opc * ((float)1. - old_opc);
		    new_r = old_r + resamp_clr[0] * ((float)1. - old_opc);
		    new_g = old_g + resamp_clr[1] * ((float)1. - old_opc);
		    new_b = old_b + resamp_clr[2] * ((float)1. - old_opc);
		    rgb_intim->opcflt = new_opc;
		    rgb_intim->rclrflt = new_r;
		    rgb_intim->gclrflt = new_g;
		    rgb_intim->bclrflt = new_b;
#ifdef RWCOX
                    }
#endif
		}
		
	    }
	    resamp_opc++;
	    if (color_channels == 1) {
		resamp_clr++;
		gray_intim++;
	    } else {
		resamp_clr += 3;
		rgb_intim++;
	    }
	} /* for i */
	if (color_channels == 1)
	    gray_intim += int_image_width - width;
	else
	    rgb_intim += int_image_width - width;
    } /* for j */
}

/*
 * AffineBruteForceWarp
 *
 * Warp the intermediate image into the final image (brute-force version,
 * affine transformations only).
 */

static void
AffineBruteForceWarp(vpc)
vpContext *vpc;
{
    unsigned char *int_image;	/* pointer to start of intermediate image
				   (GrayIntPixel or RGBIntPixel) */
    int int_width;		/* size of intermediate image */
    int int_height;
    int int_scanbytes;		/* bytes per scanline in intermediate image */
    unsigned char *image;	/* final image pixel */
    int i, j;			/* coordinates of final image pixel */
    float int_i_flt, int_j_flt;	/* position of final image pixel in
				   intermediate image coordinates */
    float int_i_int, int_j_int;	/* truncated int_i_flt, int_j_flt */
    int int_i, int_j;		/* integer int_i_int, int_j_int */
    double alpha_i, alpha_j;	/* separable interpolation weights */
    double wgt;			/* interpolation weight */
    GrayIntPixel *gray_pix;	/* intermediate image pixel (grayscale) */
    RGBIntPixel *rgb_pix;	/* intermediate image pixel (RGB) */
    double denom;
    double ma, mb, mc, md, me, mf;
    float r, g, b, alpha;
    int r_int, g_int, b_int, alpha_int;
    int color_channels;		/* number of color channels in int. image */
    int pixel_type;		/* type of output image pixel */

    /* initialize */
    color_channels = vpc->color_channels;
    pixel_type = vpc->pixel_type;
    int_width = vpc->intermediate_width;
    int_height = vpc->intermediate_height;
    if (vpc->color_channels == 1) {
	int_image = (unsigned char *)vpc->int_image.gray_intim;
	if (vpc->pad_int_to_maxwidth)
	    int_scanbytes = vpc->max_intermediate_width*sizeof(GrayIntPixel);
	else
	    int_scanbytes = vpc->intermediate_width*sizeof(GrayIntPixel);
    } else {
	int_image = (unsigned char *)vpc->int_image.rgb_intim;
	if (vpc->pad_int_to_maxwidth)
	    int_scanbytes = vpc->max_intermediate_width*sizeof(RGBIntPixel);
	else
	    int_scanbytes = vpc->intermediate_width*sizeof(RGBIntPixel);
    }

    /* compute transformation from final image pixel to intermediate
       image pixel */
    denom = 1. / (vpc->warp_2d[0][0]*vpc->warp_2d[1][1] -
		  vpc->warp_2d[0][1]*vpc->warp_2d[1][0]);
    ma = vpc->warp_2d[1][1] * denom;
    mb = -vpc->warp_2d[0][1] * denom;
    mc = (vpc->warp_2d[0][1]*vpc->warp_2d[1][2] - 
	  vpc->warp_2d[1][1]*vpc->warp_2d[0][2]) * denom;
    md = -vpc->warp_2d[1][0] * denom;
    me = vpc->warp_2d[0][0] * denom;
    mf = (vpc->warp_2d[1][0]*vpc->warp_2d[0][2] - 
	  vpc->warp_2d[0][0]*vpc->warp_2d[1][2]) * denom;

    /* loop over the pixels of the final image */
    for (j = 0; j < vpc->image_height; j++) {
	image = (unsigned char *)vpc->image + j*vpc->image_bytes_per_scan;
	for (i = 0; i < vpc->image_width; i++) {
	    /* reverse-map final image pixel into intermediate image */
	    int_i_flt = ma*i + mb*j + mc;
	    int_j_flt = md*i + me*j + mf;

	    /* compute interpolation weights */
	    int_i_int = floor(int_i_flt);
	    int_j_int = floor(int_j_flt);
	    alpha_i = int_i_flt - int_i_int;
	    alpha_j = int_j_flt - int_j_int;
	    int_i = (int)int_i_int;
	    int_j = (int)int_j_int;

	    /* interpolate */
	    r = 0;
	    g = 0;
	    b = 0;
	    alpha = 0;
	    if (int_i >= 0 && int_i < int_width &&
		int_j >= 0 && int_j < int_height) {
		wgt = (1. - alpha_i) * (1. - alpha_j);
		if (color_channels == 1) {
		    gray_pix = (GrayIntPixel *)(int_image + int_j*
						int_scanbytes) + int_i;
		    r += gray_pix->clrflt*wgt;
		    alpha += gray_pix->opcflt*wgt;
		} else {
		    rgb_pix = (RGBIntPixel *)(int_image + int_j*
					      int_scanbytes) + int_i;
		    r += rgb_pix->rclrflt*wgt;
		    g += rgb_pix->gclrflt*wgt;
		    b += rgb_pix->bclrflt*wgt;
		    alpha += rgb_pix->opcflt*wgt;
		}
	    }
	    if (int_i >= 0 && int_i < int_width &&
		int_j >= -1 && int_j < int_height-1) {
		wgt = (1. - alpha_i) * alpha_j;
		if (color_channels == 1) {
		    gray_pix = (GrayIntPixel *)(int_image + (int_j+1)*
						int_scanbytes) + int_i;
		    r += gray_pix->clrflt*wgt;
		    alpha += gray_pix->opcflt*wgt;
		} else {
		    rgb_pix = (RGBIntPixel *)(int_image + (int_j+1)*
					      int_scanbytes) + int_i;
		    r += rgb_pix->rclrflt*wgt;
		    g += rgb_pix->gclrflt*wgt;
		    b += rgb_pix->bclrflt*wgt;
		    alpha += rgb_pix->opcflt*wgt;
		}
	    }
	    if (int_i >= -1 && int_i < int_width-1 &&
		int_j >= 0 && int_j < int_height) {
		wgt = alpha_i * (1. - alpha_j);
		if (color_channels == 1) {
		    gray_pix = (GrayIntPixel *)(int_image + int_j*
						int_scanbytes) + int_i+1;
		    r += gray_pix->clrflt*wgt;
		    alpha += gray_pix->opcflt*wgt;
		} else {
		    rgb_pix = (RGBIntPixel *)(int_image + int_j*
					      int_scanbytes) + int_i+1;
		    r += rgb_pix->rclrflt*wgt;
		    g += rgb_pix->gclrflt*wgt;
		    b += rgb_pix->bclrflt*wgt;
		    alpha += rgb_pix->opcflt*wgt;
		}
	    }
	    if (int_i >= -1 && int_i < int_width-1 &&
		int_j >= -1 && int_j < int_height-1) {
		wgt = alpha_i * alpha_j;
		if (color_channels == 1) {
		    gray_pix = (GrayIntPixel *)(int_image + (int_j+1)*
						int_scanbytes) + int_i+1;
		    r += gray_pix->clrflt*wgt;
		    alpha += gray_pix->opcflt*wgt;
		} else {
		    rgb_pix = (RGBIntPixel *)(int_image + (int_j+1)*
					      int_scanbytes) + int_i+1;
		    r += rgb_pix->rclrflt*wgt;
		    g += rgb_pix->gclrflt*wgt;
		    b += rgb_pix->bclrflt*wgt;
		    alpha += rgb_pix->opcflt*wgt;
		}
	    }

	    /* clamp the pixel */
	    if (alpha > 255.)
		alpha_int = 255;
	    else if (alpha < 0.)
		alpha_int = 0;
	    else
		alpha_int = alpha;
	    if (r > 255.)
		r_int = 255;
	    else if (r < 0)
		r_int = 0;
	    else
		r_int = r;
	    
	    if (color_channels == 3) {
		if (g > 255.)
		    g_int = 255;
		else if (g < 0.)
		    g_int = 0;
		else
		    g_int = g;
		if (b > 255.)
		    b_int = 255;
		else if (b < 0.)
		    b_int = 0;
		else
		    b_int = b;
	    }

	    /* store the pixel */
	    switch (pixel_type) {
	    case VP_ALPHA:
		*image++ = alpha_int;
		break;
	    case VP_LUMINANCE:
		*image++ = r_int;
		break;
	    case VP_LUMINANCEA:
		*image++ = r_int;
		*image++ = alpha_int;
		break;
	    case VP_RGB:
		*image++ = r_int;
		*image++ = g_int;
		*image++ = b_int;
		break;
	    case VP_RGBA:
		*image++ = r_int;
		*image++ = g_int;
		*image++ = b_int;
		*image++ = alpha_int;
		break;
	    case VP_BGR:
		*image++ = b_int;
		*image++ = g_int;
		*image++ = r_int;
		break;
	    case VP_ABGR:
		*image++ = alpha_int;
		*image++ = b_int;
		*image++ = g_int;
		*image++ = r_int;
		break;
	    default:
		VPBug("bad pixel type");
	    }
	} /* for i */
    } /* for j */
}

#ifdef DEBUG
StoreFloatImage(data, width, height, scale, filename)
float *data;		/* array of input data */
int width, height;	/* size of array */
double scale;		/* factor for scaling pixel values */
char *filename;		/* name of file to store result */
{
    unsigned char *image, *imptr;
    int i, j;

    image = (unsigned char *)malloc(width*height);
    imptr = image;
    for (j = 0; j < height; j++) {
	for (i = 0; i < width; i++) {
	    *imptr++ = (int)rint(scale * *data++);
	}
    }
    VprWriteGrayscaleTIFF(filename, width, height, width, image);
    free(image);
}
#endif
