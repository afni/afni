/*
 * vp_renderA.c
 *
 * Shear-warp volume rendering algorithm for affine view transformations.
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

/*#define DUMP_SHADOW_VOLUME*/
/*#define DUMP_GRAY_VOLUME*/

extern void VPCompAC00G ANSI_ARGS((vpContext *vpc, int icount, int jcount,
    int k, double slice_depth_cueing_dbl, GrayIntPixel *intimage,
    double weightTLdbl, double weightBLdbl, double weightTRdbl,
    double weightBRdbl, unsigned char *run_lengths, void *voxel_data));
extern void VPCompAR00G ANSI_ARGS((vpContext *vpc, int icount, int jcount,
    int k, double slice_depth_cueing_dbl, GrayIntPixel *intimage,
    double weightTLdbl, double weightBLdbl, double weightTRdbl,
    double weightBRdbl, void *voxel_data, int voxel_istride,
    int voxel_jstride));
extern void VPWarpA101N ANSI_ARGS((GrayIntPixel *in_image, int in_width,
    int in_height, int in_bytes_per_scan, unsigned char *out_image,
    int out_width, int out_height, int out_bytes_per_scan,
    vpMatrix3 warp_matrix));
extern void VPWarpA110N ANSI_ARGS((GrayIntPixel *in_image, int in_width,
    int in_height, int in_bytes_per_scan, unsigned char *out_image,
    int out_width, int out_height, int out_bytes_per_scan,
    vpMatrix3 warp_matrix));
extern void VPWarpA111N ANSI_ARGS((GrayIntPixel *in_image, int in_width,
    int in_height, int in_bytes_per_scan, unsigned char *out_image,
    int out_width, int out_height, int out_bytes_per_scan,
    vpMatrix3 warp_matrix));
extern void VPWarpA301N ANSI_ARGS((RGBIntPixel *in_image, int in_width,
    int in_height, int in_bytes_per_scan, unsigned char *out_image,
    int out_width, int out_height, int out_bytes_per_scan,
    vpMatrix3 warp_matrix));
extern void VPWarpA330N ANSI_ARGS((RGBIntPixel *in_image, int in_width,
    int in_height, int in_bytes_per_scan, unsigned char *out_image,
    int out_width, int out_height, int out_bytes_per_scan,
    vpMatrix3 warp_matrix));
extern void VPWarpA331N ANSI_ARGS((RGBIntPixel *in_image, int in_width,
    int in_height, int in_bytes_per_scan, unsigned char *out_image,
    int out_width, int out_height, int out_bytes_per_scan,
    vpMatrix3 warp_matrix));
extern void VPWarpA330R ANSI_ARGS((RGBIntPixel *in_image, int in_width,
    int in_height, int in_bytes_per_scan, unsigned char *out_image,
    int out_width, int out_height, int out_bytes_per_scan,
    vpMatrix3 warp_matrix));
extern void VPWarpA331R ANSI_ARGS((RGBIntPixel *in_image, int in_width,
    int in_height, int in_bytes_per_scan, unsigned char *out_image,
    int out_width, int out_height, int out_bytes_per_scan,
    vpMatrix3 warp_matrix));

#ifdef STATISTICS
extern int vpResampleCount;
extern int vpCompositeCount;
extern int vpERTSkipCount;
extern int vpERTSkipAgainCount;
extern int vpERTUpdateCount;
extern int vpSpecialZeroSkipCount;
extern int vpRunFragmentCount;
#endif

/*
 * VPRenderAffine
 *
 * Render a classified volume with an affine viewing transformation.
 */

void
VPRenderAffine(vpc, algorithm, composite_func)
vpContext *vpc;
int algorithm;	/* USE_RLEVOLUME or USE_RAWVOLUME */
void (*composite_func)(); /* function to do the compositing */
{
    int icount;			/* voxels per voxel scanline */
    int jcount;			/* voxel scanlines per voxel slice */
    int kcount;			/* voxel slices in the volume */
    int istride;		/* strides for each dimension of raw volume */
    int jstride;
    int kstride;
    int k;			/* voxel slice index */
    int kstart, kstop;		/* values of k for first and last slices */
    int kincr;			/* value to add to k to get to the next slice
				   (either 1 or -1) */
    RLEVoxels *rle_voxels;	/* run-length encoded volume */
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
    float slice_depth_cueing;	/* depth cueing factor for current slice */
    float slice_dc_ratio;	/* multiplier to get depth cueing factor
				   for the next slice */
    unsigned char *run_lengths;	/* run lengths for slice */
    void *voxel_data;		/* voxel data for slice */
    void *intimage;		/* first intermediate image pixel for slice */
    int scan_offset_index;	/* index into scan_offsets for this slice */
    float shadow_slice_u;	/* top-left corner of voxel slice in shadow */
    float shadow_slice_v;	/*    buffer coordinates */
    int shadow_slice_u_int;	/* integer part of shadow_slice_u/v */
    int shadow_slice_v_int;
    int shadow_slice_start_index;/* index of top-left shadow buffer pixel */
    GrayIntPixel *shadow_image;	/* first shadow buffer pixel for slice */
    int shadow_k;		/* voxel slice number plus shadow bias */
#ifdef DUMP_SHADOW_VOLUME
    unsigned char *shadow_dump;
#endif
#ifdef DUMP_GRAY_VOLUME
    unsigned char *gray_dump;
#endif
#ifdef DUMP_SHADOW_VOLUME
    int dump_fd;
    int dump_value;
#else
#ifdef DUMP_GRAY_VOLUME
    int dump_fd;
    int dump_value;
#endif
#endif
#ifdef DEBUG
    GrayIntPixel *trace_gray_ptr = &vpc->int_image.gray_intim[vpc->trace_u + 
				    vpc->trace_v*vpc->intermediate_width];
    RGBIntPixel *trace_rgb_ptr = &vpc->int_image.rgb_intim[vpc->trace_u + 
				    vpc->trace_v*vpc->intermediate_width];
    float vox_depth;
#endif
    DECLARE_TIME(t0);
    DECLARE_TIME(t1);
    DECLARE_TIME(tA);
    DECLARE_TIME(tB);

#ifdef STATISTICS
    vpResampleCount = 0;
    vpCompositeCount = 0;
    vpERTSkipCount = 0;
    vpERTSkipAgainCount = 0;
    vpERTUpdateCount = 0;
    vpSpecialZeroSkipCount = 0;
    vpRunFragmentCount = 0;
#endif

    GET_TIME(vpc, tA);

    /* initialize for the fast classification algorithm */
    if (algorithm == USE_RAWVOLUME && vpc->mm_octree != NULL) {
	ASSERT(vpc->raw_voxels != NULL);
	GET_TIME(vpc, t0);
	VPComputeSummedAreaTable(vpc);
	VPClassifyOctree(vpc);
	GET_TIME(vpc, t1);
	STORE_TIME(vpc, VPTIMER_CLSFY_OCTREE, t0, t1);
    }

    /* find size of volume */
    if (algorithm == USE_RLEVOLUME) {
	switch (vpc->best_view_axis) {
	case VP_X_AXIS:
	    rle_voxels = vpc->rle_x;
	    break;
	case VP_Y_AXIS:
	    rle_voxels = vpc->rle_y;
	    break;
	case VP_Z_AXIS:
	    rle_voxels = vpc->rle_z;
	    break;
	default:
	    VPBug("invalid viewing axis in AffineRender");
	}
	icount = rle_voxels->ilen;
	jcount = rle_voxels->jlen;
	kcount = rle_voxels->klen;
    } else {
	switch (vpc->best_view_axis) {
	case VP_X_AXIS:
	    icount = vpc->ylen;
	    jcount = vpc->zlen;
	    kcount = vpc->xlen;
	    istride = vpc->ystride;
	    jstride = vpc->zstride;
	    kstride = vpc->xstride;
	    break;
	case VP_Y_AXIS:
	    icount = vpc->zlen;
	    jcount = vpc->xlen;
	    kcount = vpc->ylen;
	    istride = vpc->zstride;
	    jstride = vpc->xstride;
	    kstride = vpc->ystride;
	    break;
	case VP_Z_AXIS:
	    icount = vpc->xlen;
	    jcount = vpc->ylen;
	    kcount = vpc->zlen;
	    istride = vpc->xstride;
	    jstride = vpc->ystride;
	    kstride = vpc->zstride;
	    break;
	default:
	    VPBug("invalid viewing axis in AffineRender");
	}
    }

    GET_TIME(vpc, t0);

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

    /* initialize shadow buffer */
    if (vpc->enable_shadows) {
	vpc->pad_shadow_to_maxwidth = 0;
	bzero(vpc->shadow_buffer, vpc->shadow_width *
	      vpc->shadow_height * sizeof(GrayIntPixel));
    }
#ifdef DUMP_SHADOW_VOLUME
    Alloc(vpc, shadow_dump, char *, vpc->shadow_width * vpc->shadow_height *
	  kcount, "shadow_dump");
#endif
#ifdef DUMP_GRAY_VOLUME
    Alloc(vpc, gray_dump, char *, vpc->intermediate_width * 
	  vpc->intermediate_height * kcount, "gray_dump");
#endif

    GET_TIME(vpc, t1);
    STORE_TIME(vpc, VPTIMER_CLEAR, t0, t1);

    /* initialize depth cueing */
    slice_dc_ratio = VPSliceDepthCueRatio(vpc);
    slice_depth_cueing = 1.;
#ifdef DEBUG
    Debug((vpc, VPDEBUG_DEPTHCUE, "depth cueing at cube corners:\n"));
    vox_depth = vpc->depth_000 + 0*vpc->depth_di +
	0*vpc->depth_dj + 0*vpc->depth_dk;
    if (vox_depth < 0.0)
	vox_depth = 0.0;
    Debug((vpc, VPDEBUG_DEPTHCUE,
	   "  %3d %3d %3d: depth = %12.6f, factor = %12.6f\n",
	   0, 0, 0, vox_depth,
	   vpc->dc_front_factor * exp(-vpc->dc_density * (1.0 - vox_depth))));
    vox_depth = vpc->depth_000 + icount*vpc->depth_di +
	0*vpc->depth_dj + 0*vpc->depth_dk;
    if (vox_depth < 0.0)
	vox_depth = 0.0;
    Debug((vpc, VPDEBUG_DEPTHCUE,
	   "  %3d %3d %3d: depth = %12.6f, factor = %12.6f\n",
	   icount, 0, 0, vox_depth,
	   vpc->dc_front_factor * exp(-vpc->dc_density * (1.0 - vox_depth))));
    vox_depth = vpc->depth_000 + icount*vpc->depth_di +
	jcount*vpc->depth_dj + 0*vpc->depth_dk;
    if (vox_depth < 0.0)
	vox_depth = 0.0;
    Debug((vpc, VPDEBUG_DEPTHCUE,
	   "  %3d %3d %3d: depth = %12.6f, factor = %12.6f\n",
	   icount, jcount, 0, vox_depth,
	   vpc->dc_front_factor * exp(-vpc->dc_density * (1.0 - vox_depth))));
    vox_depth = vpc->depth_000 + 0*vpc->depth_di +
	jcount*vpc->depth_dj + 0*vpc->depth_dk;
    if (vox_depth < 0.0)
	vox_depth = 0.0;
    Debug((vpc, VPDEBUG_DEPTHCUE,
	   "  %3d %3d %3d: depth = %12.6f, factor = %12.6f\n",
	   0, jcount, 0, vox_depth,
	   vpc->dc_front_factor * exp(-vpc->dc_density * (1.0 - vox_depth))));
    vox_depth = vpc->depth_000 + 0*vpc->depth_di +
	0*vpc->depth_dj + kcount*vpc->depth_dk;
    if (vox_depth < 0.0)
	vox_depth = 0.0;
    Debug((vpc, VPDEBUG_DEPTHCUE,
	   "  %3d %3d %3d: depth = %12.6f, factor = %12.6f\n",
	   0, 0, kcount, vox_depth,
	   vpc->dc_front_factor * exp(-vpc->dc_density * (1.0 - vox_depth))));
    vox_depth = vpc->depth_000 + icount*vpc->depth_di +
	0*vpc->depth_dj + kcount*vpc->depth_dk;
    if (vox_depth < 0.0)
	vox_depth = 0.0;
    Debug((vpc, VPDEBUG_DEPTHCUE,
	   "  %3d %3d %3d: depth = %12.6f, factor = %12.6f\n",
	   icount, 0, kcount, vox_depth,
	   vpc->dc_front_factor * exp(-vpc->dc_density * (1.0 - vox_depth))));
    vox_depth = vpc->depth_000 + icount*vpc->depth_di +
	jcount*vpc->depth_dj + kcount*vpc->depth_dk;
    if (vox_depth < 0.0)
	vox_depth = 0.0;
    Debug((vpc, VPDEBUG_DEPTHCUE,
	   "  %3d %3d %3d: depth = %12.6f, factor = %12.6f\n",
	   icount, jcount, kcount, vox_depth,
	   vpc->dc_front_factor * exp(-vpc->dc_density * (1.0 - vox_depth))));
    vox_depth = vpc->depth_000 + 0*vpc->depth_di +
	jcount*vpc->depth_dj + kcount*vpc->depth_dk;
    if (vox_depth < 0.0)
	vox_depth = 0.0;
    Debug((vpc, VPDEBUG_DEPTHCUE,
	   "  %3d %3d %3d: depth = %12.6f, factor = %12.6f\n",
	   0, jcount, kcount, vox_depth,
	   vpc->dc_front_factor * exp(-vpc->dc_density * (1.0 - vox_depth))));
#endif /* DEBUG */

#ifdef DEBUG
    /* initialize pixel tracing */
    if (vpc->trace_u != -1) {
	if (vpc->trace_u < 0 || vpc->trace_v < 0 ||
	    vpc->trace_u >= vpc->intermediate_width ||
	    vpc->trace_v >= vpc->intermediate_height) {
	    printf("Traced pixel is out of bounds.\n");
	} else {
	    printf("Trace for pixel u=%d, v=%d",
		   vpc->trace_u, vpc->trace_v);
	    if (vpc->enable_shadows)
		printf(", shadow_k=%d", vpc->trace_shadow_k);
	    printf(" (View %c, slice size %d,%d)\n",
		   vpc->best_view_axis + 'X', icount, jcount);
	    printf("Slice   Slice      TopLft       BotLft       ");
	    printf("TopRgt       BotRgt    Compos.\n");
	    printf("       BRX/BRY  Opc/Clr/Wgt  Opc/Clr/Wgt  Opc/Clr/Wgt  ");
	    printf("Opc/Clr/Wgt  Opc/Clr\n");
	}
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
    shadow_k = kstart - vpc->shadow_bias * kincr;

    /* loop over slices of the voxel data in front-to-back order */
    for (k = kstart; k != kstop; k += kincr) {
	ReportStatus(vpc, (double)(k - kstart)/(double)(kstop - kstart));

	/* update shadow buffer */
	if (vpc->enable_shadows && shadow_k >= 0 && shadow_k < kcount) {
	    /* compute coordinates of slice in shadow buffer;
	       shadow bias determines which slice (usually
	       a few slices old in order to eliminate self-shadowing) */
	    shadow_slice_u = vpc->shadow_shear_i * shadow_k + 
		vpc->shadow_trans_i;
	    shadow_slice_v = vpc->shadow_shear_j * shadow_k +
		vpc->shadow_trans_j;
	    shadow_slice_u_int = (int)ceil(shadow_slice_u) - 1;
	    shadow_slice_v_int = (int)ceil(shadow_slice_v) - 1;
	    shadow_slice_start_index = shadow_slice_u_int +
		shadow_slice_v_int*vpc->shadow_width;
	    shadow_image = &vpc->shadow_buffer[shadow_slice_start_index];

	    /* compute resampling weights for voxel slice in shadow buffer */
	    slice_u_frac = shadow_slice_u - shadow_slice_u_int;
	    slice_v_frac = shadow_slice_v - shadow_slice_v_int;
	    WgtTL = slice_u_frac * slice_v_frac;
	    WgtBL = slice_u_frac * ((float)1. - slice_v_frac);
	    WgtTR = ((float)1. - slice_u_frac) * slice_v_frac;
	    WgtBR = ((float)1. - slice_u_frac) * ((float)1. - slice_v_frac);

	    /* composite voxel opacities into shadow buffer */
	    if (algorithm == USE_RLEVOLUME) {
		scan_offset_index = shadow_k *
		    rle_voxels->scan_offsets_per_slice;
		run_lengths = rle_voxels->run_lengths + 
		    rle_voxels->scan_offsets[scan_offset_index].first_len;
		voxel_data = (void *)((char *)rle_voxels->data +
		    rle_voxels->scan_offsets[scan_offset_index].first_data);
		VPCompAC00G(vpc, icount, jcount, shadow_k, slice_depth_cueing,
			    shadow_image, WgtTL, WgtBL, WgtTR, WgtBR,
			    run_lengths, voxel_data);
	    } else {
		voxel_data = (void *)((char *)vpc->raw_voxels + 
				      shadow_k*kstride);
		VPCompAR00G(vpc, icount, jcount, shadow_k, slice_depth_cueing,
			    shadow_image, WgtTL, WgtBL, WgtTR, WgtBR,
			    voxel_data, istride, jstride);
	    }
	}
	shadow_k += kincr;

	/* compute coordinates of top-left corner of voxel slice in
	   intermediate image */
	slice_u = vpc->shear_i * k + vpc->trans_i;
	slice_v = vpc->shear_j * k + vpc->trans_j;
	slice_u_int = (int)ceil(slice_u) - 1;
	slice_v_int = (int)ceil(slice_v) - 1;
	slice_start_index = slice_u_int + slice_v_int*vpc->intermediate_width;
	if (color_channels == 1)
	    intimage = &vpc->int_image.gray_intim[slice_start_index];
	else
	    intimage = &vpc->int_image.rgb_intim[slice_start_index];

	/* compute resampling weights for this slice */
	slice_u_frac = slice_u - slice_u_int;
	slice_v_frac = slice_v - slice_v_int;
	WgtTL = slice_u_frac * slice_v_frac;
	WgtBL = slice_u_frac * ((float)1. - slice_v_frac);
	WgtTR = ((float)1. - slice_u_frac) * slice_v_frac;
	WgtBR = ((float)1. - slice_u_frac) * ((float)1. - slice_v_frac);

	/* compute coordinates of voxel slice in shadow buffer */
	if (vpc->enable_shadows) {
	    shadow_slice_u = vpc->shadow_shear_i * k + vpc->shadow_trans_i;
	    shadow_slice_v = vpc->shadow_shear_j * k + vpc->shadow_trans_j;
	    shadow_slice_u_int = (int)ceil(shadow_slice_u) - 1;
	    shadow_slice_v_int = (int)ceil(shadow_slice_v) - 1;
	    shadow_slice_start_index = shadow_slice_u_int +
		shadow_slice_v_int*vpc->shadow_width;
	    shadow_image = &vpc->shadow_buffer[shadow_slice_start_index];
	}

	/* find voxel data for this slice and composite */
	if (algorithm == USE_RLEVOLUME) {
	    scan_offset_index = k * rle_voxels->scan_offsets_per_slice;
	    run_lengths = rle_voxels->run_lengths + 
	    	      rle_voxels->scan_offsets[scan_offset_index].first_len;
	    voxel_data = (void *)((char *)rle_voxels->data +
		     rle_voxels->scan_offsets[scan_offset_index].first_data);
#ifdef INDEX_VOLUME
	    composite_func(vpc, icount, jcount, k, slice_depth_cueing,
			   intimage, WgtTL, WgtBL, WgtTR, WgtBR,
			   run_lengths, voxel_data,
			   rle_voxels->voxel_index + k * icount * jcount,
			   shadow_image);
#else
	    composite_func(vpc, icount, jcount, k, slice_depth_cueing,
			   intimage, WgtTL, WgtBL, WgtTR, WgtBR,
			   run_lengths, voxel_data, shadow_image);
#endif
	} else {
	    voxel_data = (void *)((char *)vpc->raw_voxels + k*kstride);
	    composite_func(vpc, icount, jcount, k, slice_depth_cueing,
			   intimage, WgtTL, WgtBL, WgtTR, WgtBR,
			   voxel_data, istride, jstride, shadow_image);
	}

	/* update depth cueing factor */
	slice_depth_cueing *= slice_dc_ratio;

#ifdef DUMP_SHADOW_VOLUME
	vpGetImage(vpc, shadow_dump + k * vpc->shadow_width *
		   vpc->shadow_height, vpc->shadow_width, vpc->shadow_height,
		   vpc->shadow_width, VP_ALPHA, VP_SHADOW_BUFFER);
#endif
#ifdef DUMP_GRAY_VOLUME
	vpGetImage(vpc, gray_dump + k * vpc->intermediate_width *
		   vpc->intermediate_height, vpc->intermediate_width,
		   vpc->intermediate_height, VP_LUMINANCE, VP_IMAGE_BUFFER);
#endif

    }
    ReportStatus(vpc, 1.0);

    GET_TIME(vpc, t1);
    STORE_TIME(vpc, VPTIMER_COMPOSITE, t0, t1);

#ifdef DEBUG
    /* print traced pixel before depth cueing */
    if (vpc->trace_u != -1) {
	if (vpc->trace_u >= 0 && vpc->trace_v >= 0 &&
	    vpc->trace_u < vpc->intermediate_width &&
	    vpc->trace_v < vpc->intermediate_height) {
	    if (color_channels == 1) {
		printf("Before depth cueing: opc = %.9f = %d",
		       trace_gray_ptr->opcflt*255.,
		       (int)(trace_gray_ptr->opcflt*255.));
		printf("   clr = %.9f = %d\n",
		       trace_gray_ptr->clrflt,
		       (int)trace_gray_ptr->clrflt);
	    } else {
		printf("Before depth cueing: opc = %14.9f = %3d",
		       trace_rgb_ptr->opcflt*255.,
		       (int)(trace_rgb_ptr->opcflt*255.));
		printf("   r = %14.9f = %d\n",
		       trace_rgb_ptr->rclrflt,
		       (int)trace_rgb_ptr->rclrflt);
		printf("                                               ");
		printf("   g = %14.9f = %d\n",
		       trace_rgb_ptr->gclrflt,
		       (int)trace_rgb_ptr->gclrflt);
		printf("                                               ");
		printf("   b = %14.9f = %d\n",
		       trace_rgb_ptr->bclrflt,
		       (int)trace_rgb_ptr->bclrflt);
	    }
	}
    }
#endif

    /* depth cue the intermediate image */
    if (vpc->dc_enable) {
	GET_TIME(vpc, t0);
	VPDepthCueIntImage(vpc, vpc->reverse_slice_order ? kcount-1 : 0);
	GET_TIME(vpc, t1);
	STORE_TIME(vpc, VPTIMER_DEPTHCUE, t0, t1);
    }

#ifdef DEBUG
    /* print final value of traced pixel */
    if (vpc->trace_u != -1) {
	if (vpc->trace_u >= 0 && vpc->trace_v >= 0 &&
	    vpc->trace_u < vpc->intermediate_width &&
	    vpc->trace_v < vpc->intermediate_height) {
	    if (color_channels == 1) {
		printf("Final pixel value:   opc = %.9f = %d",
		       trace_gray_ptr->opcflt*255.,
		       (int)(trace_gray_ptr->opcflt*255.));
		printf("   clr = %.9f = %d\n",
		       trace_gray_ptr->clrflt,
		       (int)trace_gray_ptr->clrflt);
	    } else {
		printf("Final pixel value:   opc = %14.9f = %3d",
		       trace_rgb_ptr->opcflt*255.,
		       (int)(trace_rgb_ptr->opcflt*255.));
		printf("   r = %14.9f = %d\n",
		       trace_rgb_ptr->rclrflt,
		       (int)trace_rgb_ptr->rclrflt);
		printf("                                               ");
		printf("   g = %14.9f = %d\n",
		       trace_rgb_ptr->gclrflt,
		       (int)trace_rgb_ptr->gclrflt);
		printf("                                               ");
		printf("   b = %14.9f = %d\n",
		       trace_rgb_ptr->bclrflt,
		       (int)trace_rgb_ptr->bclrflt);
	    }
	}
    }
#endif

    /* warp the intermediate image into the final image */
    GET_TIME(vpc, t0);
    switch (vpc->pixel_type) {
    case VP_ALPHA:
	if (color_channels == 1) {
	    VPWarpA101N(vpc->int_image.gray_intim, vpc->intermediate_width,
			vpc->intermediate_height, sizeof(GrayIntPixel) *
			(vpc->pad_int_to_maxwidth ?
			 vpc->max_intermediate_width:vpc->intermediate_width),
			vpc->image, vpc->image_width, vpc->image_height,
			vpc->image_bytes_per_scan, vpc->warp_2d);
	} else {
	    VPWarpA301N(vpc->int_image.rgb_intim, vpc->intermediate_width,
			vpc->intermediate_height, sizeof(RGBIntPixel) *
			(vpc->pad_int_to_maxwidth ?
			 vpc->max_intermediate_width:vpc->intermediate_width),
			vpc->image, vpc->image_width, vpc->image_height,
			vpc->image_bytes_per_scan, vpc->warp_2d);
	}
	break;
    case VP_LUMINANCE:
	ASSERT(color_channels == 1);
	VPWarpA110N(vpc->int_image.gray_intim, vpc->intermediate_width,
		    vpc->intermediate_height, sizeof(GrayIntPixel) *
		    (vpc->pad_int_to_maxwidth ?
		     vpc->max_intermediate_width:vpc->intermediate_width),
		    vpc->image, vpc->image_width, vpc->image_height,
		    vpc->image_bytes_per_scan, vpc->warp_2d);
	break;
    case VP_LUMINANCEA:
	ASSERT(color_channels == 1);
	VPWarpA111N(vpc->int_image.gray_intim, vpc->intermediate_width,
		    vpc->intermediate_height, sizeof(GrayIntPixel) *
		    (vpc->pad_int_to_maxwidth ?
		     vpc->max_intermediate_width:vpc->intermediate_width),
		    vpc->image, vpc->image_width, vpc->image_height,
		    vpc->image_bytes_per_scan, vpc->warp_2d);
	break;
    case VP_RGB:
	ASSERT(color_channels == 3);
	VPWarpA330N(vpc->int_image.rgb_intim, vpc->intermediate_width,
		    vpc->intermediate_height, sizeof(RGBIntPixel) *
		    (vpc->pad_int_to_maxwidth ?
		     vpc->max_intermediate_width:vpc->intermediate_width),
		    vpc->image, vpc->image_width, vpc->image_height,
		    vpc->image_bytes_per_scan, vpc->warp_2d);
	break;
    case VP_RGBA:
	ASSERT(color_channels == 3);
	VPWarpA331N(vpc->int_image.rgb_intim, vpc->intermediate_width,
		    vpc->intermediate_height, sizeof(RGBIntPixel) *
		    (vpc->pad_int_to_maxwidth ?
		     vpc->max_intermediate_width:vpc->intermediate_width),
		    vpc->image, vpc->image_width, vpc->image_height,
		    vpc->image_bytes_per_scan, vpc->warp_2d);
	break;
    case VP_BGR:
	ASSERT(color_channels == 3);
	VPWarpA330R(vpc->int_image.rgb_intim, vpc->intermediate_width,
		    vpc->intermediate_height, sizeof(RGBIntPixel) *
		    (vpc->pad_int_to_maxwidth ?
		     vpc->max_intermediate_width:vpc->intermediate_width),
		    vpc->image, vpc->image_width, vpc->image_height,
		    vpc->image_bytes_per_scan, vpc->warp_2d);
	break;
    case VP_ABGR:
	ASSERT(color_channels == 3);
	VPWarpA331R(vpc->int_image.rgb_intim, vpc->intermediate_width,
		    vpc->intermediate_height, sizeof(RGBIntPixel) *
		    (vpc->pad_int_to_maxwidth ?
		     vpc->max_intermediate_width:vpc->intermediate_width),
		    vpc->image, vpc->image_width, vpc->image_height,
		    vpc->image_bytes_per_scan, vpc->warp_2d);
	break;
    default:
	VPBug("bad pixel type");
    }
    GET_TIME(vpc, t1);
    STORE_TIME(vpc, VPTIMER_WARP, t0, t1);

    GET_TIME(vpc, tB);
    STORE_TIME(vpc, VPTIMER_RENDER, tA, tB);

#ifdef DUMP_SHADOW_VOLUME
    printf("Dumping shadow map images to shadow.dump....");
    fflush(stdout);
    if ((dump_fd = creat("shadow.dump", 0644)) < 0)
	VPBug("open failed");
    dump_value = vpc->shadow_width;
    write(dump_fd, &dump_value, sizeof(int));
    dump_value = vpc->shadow_height;
    write(dump_fd, &dump_value, sizeof(int));
    dump_value = kcount;
    write(dump_fd, &dump_value, sizeof(int));
    write(dump_fd, shadow_dump, vpc->shadow_width * vpc->shadow_height *
	  kcount);
    close(dump_fd);
    printf("\n");
    Dealloc(vpc, shadow_dump);
#endif

#ifdef DUMP_GRAY_VOLUME
    printf("Dumping grayscale intermediate images to gray.dump....");
    fflush(stdout);
    if ((dump_fd = creat("gray.dump", 0644)) < 0)
	VPBug("open failed");
    dump_value = vpc->intermediate_width;
    write(dump_fd, &dump_value, sizeof(int));
    dump_value = vpc->intermediate_height;
    write(dump_fd, &dump_value, sizeof(int));
    dump_value = kcount;
    write(dump_fd, &dump_value, sizeof(int));
    write(dump_fd, gray_dump, vpc->intermediate_width *
	  vpc->intermediate_height * kcount);
    close(dump_fd);
    printf("\n");
    Dealloc(vpc, gray_dump);
#endif
}

#ifdef DEBUG
/*
 * vpPrintRayPath
 *
 * Print a trace of the voxels that contribute to the pixel specified
 * with vpTracePixel.
 */

vpResult
vpPrintRayPath(vpc)
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
    float WgtTL, WgtBL,		/* weights in the range 0..1 which give the */
	  WgtTR, WgtBR;		/*   fractional contribution of the */
    				/*   neighboring voxels to the current */
    			        /*   intermediate image pixel */
    int i, j;			/* voxel coordinates in current slice of
				   the voxel to the BR of the ray */
    int shadow_trace_u;		/* coords. of shadow buffer pixel to trace */
    int shadow_trace_v;
    int retcode;

    /* check for errors and initialize */
    if ((retcode = VPFactorView(vpc)) != VP_OK)
	return(retcode);
    if (vpc->trace_u < 0 || vpc->trace_v < 0 ||
	vpc->trace_u >= vpc->intermediate_width ||
	vpc->trace_v >= vpc->intermediate_height) {
	printf("Traced pixel is out of bounds.\n");
	return(VP_OK);
    }

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
	VPBug("invalid viewing axis in vpPrintRayPath");
    }

    /* print column headings */
    printf("Ray path for pixel u=%d, v=%d", vpc->trace_u, vpc->trace_v);
    if (vpc->enable_shadows)
	printf(", shadow_k=%d", vpc->trace_shadow_k);
    printf(" (View %c, slice size %d,%d)\n",
	   vpc->best_view_axis + 'X', icount, jcount);
    printf("Slice     TopLft            BotLft            TopRgt");
    printf("            BotRgt\n");
    printf("      _X_/_Y_/_Z_/Wgt   _X_/_Y_/_Z_/Wgt   _X_/_Y_/_Z_/Wgt");
    printf("   _X_/_Y_/_Z_/Wgt\n");

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
	/* compute coordinates of top-left corner of voxel slice in
	   intermediate image */
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

	/* compute intersection of the ray with this slice */
	i = vpc->trace_u - slice_u_int;
	j = vpc->trace_v - slice_v_int;

	/* print ray location at this slice */
	printf("[%3d]", k);
	switch (vpc->best_view_axis) {
	case VP_X_AXIS:
	    printf("%4d%4d%4d %3d  ", k, i-1, j-1, (int)(WgtTL*100.));
	    printf("%4d%4d%4d %3d  ", k, i-1,   j, (int)(WgtBL*100.));
	    printf("%4d%4d%4d %3d  ", k,   i, j-1, (int)(WgtTR*100.));
	    printf("%4d%4d%4d %3d\n", k,   i,   j, (int)(WgtBR*100.));
	    break;
	case VP_Y_AXIS:
	    printf("%4d%4d%4d %3d  ", j-1, k, i-1, (int)(WgtTL*100.));
	    printf("%4d%4d%4d %3d  ",   j, k, i-1, (int)(WgtBL*100.));
	    printf("%4d%4d%4d %3d  ", j-1, k,   i, (int)(WgtTR*100.));
	    printf("%4d%4d%4d %3d\n",   j, k,   i, (int)(WgtBR*100.));
	    break;
	case VP_Z_AXIS:
	    printf("%4d%4d%4d %3d  ", i-1, j-1, k, (int)(WgtTL*100.));
	    printf("%4d%4d%4d %3d  ", i-1, j,   k, (int)(WgtBL*100.));
	    printf("%4d%4d%4d %3d  ", i,   j-1, k, (int)(WgtTR*100.));
	    printf("%4d%4d%4d %3d\n", i,   j,   k, (int)(WgtBR*100.));
	    break;
	}
    } /* for k */

    if (!vpc->enable_shadows)
	return(VP_OK);

    /* compute coordinates of shadow buffer pixel to trace */
    shadow_trace_u = vpc->trace_u + 
	(int)ceil(vpc->shadow_shear_i*vpc->trace_shadow_k+vpc->shadow_trans_i)-
	(int)ceil(vpc->shear_i * vpc->trace_shadow_k + vpc->trans_i);
    shadow_trace_v = vpc->trace_v + 
	(int)ceil(vpc->shadow_shear_j*vpc->trace_shadow_k+vpc->shadow_trans_j)-
	(int)ceil(vpc->shear_j * vpc->trace_shadow_k + vpc->trans_j);

    /* print column headings for shadow trace */
    printf("\nShadow Ray Path (intersecting traced pixel at k=%d):\n",
	   vpc->trace_shadow_k);
    printf("Slice     TopLft            BotLft            TopRgt");
    printf("            BotRgt\n");
    printf("      _X_/_Y_/_Z_/Wgt   _X_/_Y_/_Z_/Wgt   _X_/_Y_/_Z_/Wgt");
    printf("   _X_/_Y_/_Z_/Wgt\n");

    /* loop over slices of the voxel data in front-to-back order */
    for (k = kstart; k != kstop; k += kincr) {
	/* compute coordinates of top-left corner of voxel slice in
	   intermediate image */
	slice_u = vpc->shadow_shear_i * k + vpc->shadow_trans_i;
	slice_v = vpc->shadow_shear_j * k + vpc->shadow_trans_j;
	slice_u_int = (int)ceil(slice_u) - 1;
	slice_v_int = (int)ceil(slice_v) - 1;

	/* compute resampling weights for this slice */
	slice_u_frac = slice_u - slice_u_int;
	slice_v_frac = slice_v - slice_v_int;
	WgtTL = slice_u_frac * slice_v_frac;
	WgtBL = slice_u_frac * ((float)1. - slice_v_frac);
	WgtTR = ((float)1. - slice_u_frac) * slice_v_frac;
	WgtBR = ((float)1. - slice_u_frac) * ((float)1. - slice_v_frac);

	/* compute intersection of the ray with this slice */
	i = shadow_trace_u - slice_u_int;
	j = shadow_trace_v - slice_v_int;

	/* print ray location at this slice */
	printf("[%3d]", k);
	switch (vpc->best_view_axis) {
	case VP_X_AXIS:
	    printf("%4d%4d%4d %3d  ", k, i-1, j-1, (int)(WgtTL*100.));
	    printf("%4d%4d%4d %3d  ", k, i-1,   j, (int)(WgtBL*100.));
	    printf("%4d%4d%4d %3d  ", k,   i, j-1, (int)(WgtTR*100.));
	    printf("%4d%4d%4d %3d\n", k,   i,   j, (int)(WgtBR*100.));
	    break;
	case VP_Y_AXIS:
	    printf("%4d%4d%4d %3d  ", j-1, k, i-1, (int)(WgtTL*100.));
	    printf("%4d%4d%4d %3d  ",   j, k, i-1, (int)(WgtBL*100.));
	    printf("%4d%4d%4d %3d  ", j-1, k,   i, (int)(WgtTR*100.));
	    printf("%4d%4d%4d %3d\n",   j, k,   i, (int)(WgtBR*100.));
	    break;
	case VP_Z_AXIS:
	    printf("%4d%4d%4d %3d  ", i-1, j-1, k, (int)(WgtTL*100.));
	    printf("%4d%4d%4d %3d  ", i-1, j,   k, (int)(WgtBL*100.));
	    printf("%4d%4d%4d %3d  ", i,   j-1, k, (int)(WgtTR*100.));
	    printf("%4d%4d%4d %3d\n", i,   j,   k, (int)(WgtBR*100.));
	    break;
	}
    } /* for k */
    return(VP_OK);
}
#endif /* DEBUG */
