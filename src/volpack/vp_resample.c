/*
 * vp_resample.c
 *
 * Routines to resample an array to a grid with a different resolution.
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

/* convert a float in the interval [0-1) to a 31-bit fixed point */
#define FLTFRAC_TO_FIX31(f)	((int)((f) * 2147483648.))

typedef struct {
    int in_ptr_offset;		/* offset in bytes from beginning of scanline
				   to first input sample for current 
				   output sample */
    float *wptr;		/* filter weights for the filter phase
				   for current output sample */
    int tap_min;		/* first tap to evaluate */
    int tap_max;		/* last tap to evaluate */
} FilterTemplate;

static void ResampleUchar ANSI_ARGS((vpContext *vpc, int num_dimens,
    int *src_dimens, int *dst_dimens, int *src_strides, int *dst_strides,
    unsigned char *in_array, unsigned char *out_array,
    FilterTemplate *template));
static void ResampleUshort ANSI_ARGS((vpContext *vpc, int num_dimens,
    int *src_dimens, int *dst_dimens, int *src_strides, int *dst_strides,
    unsigned short *in_array, unsigned short *out_array,
    FilterTemplate *template));
static void ResampleFloat ANSI_ARGS((vpContext *vpc, int num_dimens,
    int *src_dimens, int *dst_dimens, int *src_strides, int *dst_strides,
    float *in_array, float *out_array, FilterTemplate *template));
static float *ComputeWeights ANSI_ARGS((vpContext *vpc, int src_xlen, 
    int dst_xlen, int filter_type));

/*
 * vpSetFilter
 *
 * Set the filter to use for resampling.
 */

vpResult
vpSetFilter(vpc, num_taps, num_phases, weights)
vpContext *vpc;
int num_taps;	/* number of filter taps */
int num_phases;	/* number of filter phases */
float *weights;	/* table of filter weights (weights[num_phases][num_taps]) */
{
    int num_ones, bit;

    /* make sure num_taps is positive and num_phases is a power of two */
    if (num_taps < 1 || num_phases < 1)
	return(VPSetError(vpc, VPERROR_BAD_VALUE));
    num_ones = 0;
    for (bit = 0; bit < 32; bit++) {
	if (num_phases & (1 << bit))
	    num_ones++;
    }
    if (num_ones != 1)
	return(VPSetError(vpc, VPERROR_BAD_VALUE));

    /* store values in context */
    vpc->filter_num_taps = num_taps;
    vpc->filter_num_phases = num_phases;
    vpc->filter_weights = weights;
    return(VP_OK);
}

/*
 * vpResample
 *
 * Resample an array to a grid with a different resolution.
 */

vpResult
vpResample(vpc, num_dimens, src_dimens, dst_dimens, src_strides, dst_strides,
	   element_type, in_array, out_array)
vpContext *vpc;
int num_dimens;		/* number of dimensions in the two arrays */
int *src_dimens;	/* sizes of source array dimensions */
int *dst_dimens;	/* sizes of destination array dimensions (must
			   be the same, except for first dimension) */
int *src_strides;	/* strides of source array dimensions (in bytes) */
int *dst_strides;	/* strides of destination dimensions (in bytes) */
int element_type;	/* type of array element (VP_UCHAR, VP_USHORT,
			   VP_FLOAT) */
void *in_array;		/* input array containing data */
void *out_array;	/* storage for output array */
{
    int num_taps;		/* number of filter taps */
    int num_phases;		/* number of filter phases */
    int in_x_count;		/* length of input scanlines */
    int out_x_count;		/* length of output scanlines */
    int in_x_stride;		/* stride of input scanline elements */
    double scale_factor;	/* in_x = scale_factor * out_x */
    double in_x0;		/* location of center of first output sample
				   in the input scanline */
    int index0;			/* coordinate of input sample corresponding
				   to first filter tap for first output
				   sample */
    int phase0;			/* filter phase for first output sample */
    int index_incr;		/* change in index0 for next output
				   sample */
    int phase_incr;		/* change in phase0 for next output
				   sample */
    int unused_phase_bits;	/* number of low-order bits of the phase that
				   are ignored for indexing the weight table */
    FilterTemplate *template;	/* filter template */
    float *weights;		/* pointer to weight table */
    int in_offset;		/* offset to input sample */
    int index, phase;		/* current input sample index and phase */
    int out_x;			/* current output sample */
    int tap_min, tap_max;	/* bounds on tap number */
    int bit, d;

    /* check for errors */
    if (vpc->filter_weights == NULL)
	return(VPSetError(vpc, VPERROR_BAD_SIZE));

    /* find where the first output sample maps into the input array
       and compute the filter phase for that sample; also compute
       increments to get the input array position and filter phase
       for the next sample */
    num_taps = vpc->filter_num_taps;
    num_phases = vpc->filter_num_phases;
    in_x_count = src_dimens[0];
    out_x_count = dst_dimens[0];
    scale_factor = (double)in_x_count / (double)out_x_count;
    if (num_taps % 2 == 0) {
	/* even number of taps */

	/* map center of first output voxel (x=0.5) to input voxel space
	   (multiply by scale_factor), then translate by -0.5 to convert
	   input voxels centered at 0.5 to input voxels centered at 0.0 */
	in_x0 = 0.5 * scale_factor - 0.5;
	phase0 = FLTFRAC_TO_FIX31(in_x0 - floor(in_x0));
	index0 = (int)floor(in_x0) - num_taps/2 + 1;
    } else {
	/* odd number of taps */

	/* omit translation by -0.5 since filter phase is offset by 0.5 voxels
	   relative to previous case */
	in_x0 = 0.5 * scale_factor;
	phase0 = FLTFRAC_TO_FIX31(in_x0 - floor(in_x0));
	if (in_x0 < 0.5) {
	    index0 = (int)floor(in_x0) - num_taps/2;
	} else {
	    index0 = (int)floor(in_x0) - num_taps/2 - 1;
	}
    }
    index_incr = (int)floor(scale_factor);
    phase_incr = FLTFRAC_TO_FIX31(scale_factor - index_incr);
    unused_phase_bits = 0;
    for (bit = 0; bit < 32; bit++) {
	if (num_phases & (1 << bit)) {
	    unused_phase_bits = 31 - bit;
	    break;
	}
    }
    ASSERT(unused_phase_bits != 0);

    /* compute a template containing input array position and filter
       weights for each output sample in an output scanline */
    Alloc(vpc, template, FilterTemplate *, out_x_count*sizeof(FilterTemplate),
	  "FilterTemplate");
    weights = vpc->filter_weights;
    index = index0;
    phase = phase0;
    in_x_stride = src_strides[0];
    in_offset = index * in_x_stride;
    for (out_x = 0; out_x < out_x_count; out_x++) {
	tap_min = MAX(0, -index);
	tap_max = MIN(in_x_count - index - 1, num_taps-1);
	template[out_x].in_ptr_offset = in_offset + tap_min * in_x_stride;
	template[out_x].wptr = &weights[(phase >> unused_phase_bits) * num_taps
					+ tap_min];
	template[out_x].tap_min = tap_min;
	template[out_x].tap_max = tap_max;
	phase += phase_incr;
	if (phase < 0) {
	    phase &= 0x7FFFFFFF;
	    index += index_incr + 1;
	    in_offset += (index_incr + 1) * in_x_stride;
	} else {
	    index += index_incr;
	    in_offset += index_incr * in_x_stride;
	}
    }

    /* call a type-specific resampling routine */
    switch (element_type) {
    case VP_UCHAR:
	ResampleUchar(vpc, num_dimens, src_dimens, dst_dimens, src_strides,
		      dst_strides, in_array, out_array, template);
	break;
    case VP_USHORT:
	ResampleUshort(vpc, num_dimens, src_dimens, dst_dimens, src_strides,
		       dst_strides, in_array, out_array, template);
	break;
    case VP_FLOAT:
	ResampleFloat(vpc, num_dimens, src_dimens, dst_dimens, src_strides,
		      dst_strides, in_array, out_array, template);
	break;
    default:
	Dealloc(vpc, template);
	return(VPSetError(vpc, VPERROR_BAD_VALUE));
    }
    Dealloc(vpc, template);
    return(VP_OK);
}

/*
 * ResampleUchar
 *
 * Resample an array of unsigned chars.
 */

static void
ResampleUchar(vpc, num_dimens, src_dimens, dst_dimens, src_strides,
	      dst_strides, in_array, out_array, template)
vpContext *vpc;
int num_dimens;		/* number of dimensions in the two arrays */
int *src_dimens;	/* sizes of source array dimensions */
int *dst_dimens;	/* sizes of destination array dimensions (must
			   be the same, except for first dimension) */
int *src_strides;	/* strides of source array dimensions (in bytes) */
int *dst_strides;	/* strides of destination dimensions (in bytes) */
unsigned char *in_array;/* input array containing data */
unsigned char *out_array;/* storage for output array */
FilterTemplate *template;/* filter template */
{
    int out_x;			/* current output sample */
    float *wptr;		/* pointer to filter weights */
    float acc;			/* accumulator for resampled value */
    int tap;			/* current tap number */
    int tap_min, tap_max;	/* bounds on tap number */
    unsigned char *in_ptr;	/* pointer to first input sample that
				   affects current output sample */
    unsigned char *in_scan_ptr;	/* pointer to beginning of input scanline */
    unsigned char *out_ptr;	/* pointer to current output sample */
    unsigned char *out_scan_ptr;/* pointer to beginning of output scanline */
    FilterTemplate *sample_template;	/* template for output sample */
    int out_x_count;		/* number of elements in output scanline */
    int in_x_stride;		/* stride for input elements */
    int out_x_stride;		/* stride for output elements */
    int *scan_coord;		/* current scanline coordinates */
    int done;
    int dim;

    /* copy parameters into local variables */
    out_x_count = dst_dimens[0];
    in_x_stride = src_strides[0];
    out_x_stride = dst_strides[0];
    
    /* allocate space for current scanline coordinates */
    Alloc(vpc, scan_coord, int *, num_dimens * sizeof(int), "scan_coord");
    for (dim = 0; dim < num_dimens; dim++) {
	scan_coord[dim] = 0;
    }

    /* initialize pointers to first scanline */
    in_scan_ptr = in_array;
    out_scan_ptr = out_array;

    done = 0;
    while (!done) {
	/* resample one scanline */
	sample_template = template;
	out_ptr = out_scan_ptr;
	for (out_x = 0; out_x < out_x_count; out_x++) {
	    in_ptr = in_scan_ptr + sample_template->in_ptr_offset;
	    wptr = sample_template->wptr;
	    tap_min = sample_template->tap_min;
	    tap_max = sample_template->tap_max;
	    acc = 0;
	    for (tap = tap_min; tap <= tap_max; tap++) {
		acc += (float)(*in_ptr) * *wptr;
		in_ptr += in_x_stride;
		wptr++;
	    }
	    if (acc > 255.)
		*out_ptr = 255;
	    else if (acc < 0.)
		*out_ptr = 0;
	    else
		*out_ptr = (int)acc;
	    out_ptr += out_x_stride;
	    sample_template++;
	} /* for out_x */

	/* set pointers to next scanline */
	for (dim = 1; dim < num_dimens; dim++) {
	    if (++scan_coord[dim] < src_dimens[dim]) {
		in_scan_ptr += src_strides[dim];
		out_scan_ptr += dst_strides[dim];
		break;
	    } else if (dim == num_dimens-1) {
		done = 1;
	    } else {
		scan_coord[dim] = 0;
		in_scan_ptr -= src_strides[dim] * src_dimens[dim];
		out_scan_ptr -= dst_strides[dim] * dst_dimens[dim];
	    }
	}
    } /* while scanlines */

    /* clean up */
    Dealloc(vpc, scan_coord);
}

/*
 * ResampleUshort
 *
 * Resample an array of unsigned shorts.
 */

static void
ResampleUshort(vpc, num_dimens, src_dimens, dst_dimens, src_strides,
	       dst_strides, in_array, out_array, template)
vpContext *vpc;
int num_dimens;		/* number of dimensions in the two arrays */
int *src_dimens;	/* sizes of source array dimensions */
int *dst_dimens;	/* sizes of destination array dimensions (must
			   be the same, except for first dimension) */
int *src_strides;	/* strides of source array dimensions (in bytes) */
int *dst_strides;	/* strides of destination dimensions (in bytes) */
unsigned short *in_array;/* input array containing data */
unsigned short *out_array;/* storage for output array */
FilterTemplate *template;/* filter template */
{
    int out_x;			/* current output sample */
    float *wptr;		/* pointer to filter weights */
    float acc;			/* accumulator for resampled value */
    int tap;			/* current tap number */
    int tap_min, tap_max;	/* bounds on tap number */
    unsigned short *in_ptr;	/* pointer to first input sample that
				   affects current output sample */
    unsigned short *in_scan_ptr;/* pointer to beginning of input scanline */
    unsigned short *out_ptr;	/* pointer to current output sample */
    unsigned short *out_scan_ptr;/* pointer to beginning of output scanline */
    FilterTemplate *sample_template;	/* template for output sample */
    int out_x_count;		/* number of elements in output scanline */
    int in_x_stride;		/* stride for input elements */
    int out_x_stride;		/* stride for output elements */
    int *scan_coord;		/* current scanline coordinates */
    int done;
    int dim;

    /* copy parameters into local variables */
    out_x_count = dst_dimens[0];
    in_x_stride = src_strides[0];
    out_x_stride = dst_strides[0];
    
    /* allocate space for current scanline coordinates */
    Alloc(vpc, scan_coord, int *, num_dimens * sizeof(int), "scan_coord");
    for (dim = 0; dim < num_dimens; dim++) {
	scan_coord[dim] = 0;
    }

    /* initialize pointers to first scanline */
    in_scan_ptr = in_array;
    out_scan_ptr = out_array;

    done = 0;
    while (!done) {
	/* resample one scanline */
	sample_template = template;
	out_ptr = out_scan_ptr;
	for (out_x = 0; out_x < out_x_count; out_x++) {
	    in_ptr = in_scan_ptr + sample_template->in_ptr_offset;
	    wptr = sample_template->wptr;
	    tap_min = sample_template->tap_min;
	    tap_max = sample_template->tap_max;
	    acc = 0;
	    for (tap = tap_min; tap <= tap_max; tap++) {
		acc += (float)(*in_ptr) * *wptr;
		in_ptr = (unsigned short *)((char *)in_ptr + in_x_stride);
		wptr++;
	    }
	    if (acc > 65535.)
		*out_ptr = 65535;
	    else if (acc < 0.)
		*out_ptr = 0;
	    else
		*out_ptr = (int)acc;
	    out_ptr = (unsigned short *)((char *)out_ptr + out_x_stride);
	    sample_template++;
	} /* for out_x */

	/* set pointers to next scanline */
	for (dim = 1; dim < num_dimens; dim++) {
	    if (++scan_coord[dim] < src_dimens[dim]) {
		in_scan_ptr = (unsigned short *)((char *)in_scan_ptr +
						 src_strides[dim]);
		out_scan_ptr = (unsigned short *)((char *)out_scan_ptr +
						  dst_strides[dim]);
		break;
	    } else if (dim == num_dimens-1) {
		done = 1;
	    } else {
		scan_coord[dim] = 0;
		in_scan_ptr = (unsigned short *)((char *)in_scan_ptr -
			      src_strides[dim] * src_dimens[dim]);
		out_scan_ptr = (unsigned short *)((char *)out_scan_ptr -
			      dst_strides[dim] * dst_dimens[dim]);
	    }
	}
    } /* while scanlines */

    /* clean up */
    Dealloc(vpc, scan_coord);
}

/*
 * ResampleFloat
 *
 * Resample an array of unsigned shorts.
 */

static void
ResampleFloat(vpc, num_dimens, src_dimens, dst_dimens, src_strides,
	      dst_strides, in_array, out_array, template)
vpContext *vpc;
int num_dimens;		/* number of dimensions in the two arrays */
int *src_dimens;	/* sizes of source array dimensions */
int *dst_dimens;	/* sizes of destination array dimensions (must
			   be the same, except for first dimension) */
int *src_strides;	/* strides of source array dimensions (in bytes) */
int *dst_strides;	/* strides of destination dimensions (in bytes) */
float *in_array;	/* input array containing data */
float *out_array;	/* storage for output array */
FilterTemplate *template;/* filter template */
{
    int out_x;			/* current output sample */
    float *wptr;		/* pointer to filter weights */
    float acc;			/* accumulator for resampled value */
    int tap;			/* current tap number */
    int tap_min, tap_max;	/* bounds on tap number */
    float *in_ptr;		/* pointer to first input sample that
				   affects current output sample */
    float *in_scan_ptr;		/* pointer to beginning of input scanline */
    float *out_ptr;		/* pointer to current output sample */
    float *out_scan_ptr;	/* pointer to beginning of output scanline */
    FilterTemplate *sample_template;	/* template for output sample */
    int out_x_count;		/* number of elements in output scanline */
    int in_x_stride;		/* stride for input elements */
    int out_x_stride;		/* stride for output elements */
    int *scan_coord;		/* current scanline coordinates */
    int done;
    int dim;

    /* copy parameters into local variables */
    out_x_count = dst_dimens[0];
    in_x_stride = src_strides[0];
    out_x_stride = dst_strides[0];
    
    /* allocate space for current scanline coordinates */
    Alloc(vpc, scan_coord, int *, num_dimens * sizeof(int), "scan_coord");
    for (dim = 0; dim < num_dimens; dim++) {
	scan_coord[dim] = 0;
    }

    /* initialize pointers to first scanline */
    in_scan_ptr = in_array;
    out_scan_ptr = out_array;

    done = 0;
    while (!done) {
	/* resample one scanline */
	sample_template = template;
	out_ptr = out_scan_ptr;
	for (out_x = 0; out_x < out_x_count; out_x++) {
	    in_ptr = in_scan_ptr + sample_template->in_ptr_offset;
	    wptr = sample_template->wptr;
	    tap_min = sample_template->tap_min;
	    tap_max = sample_template->tap_max;
	    acc = 0;
	    for (tap = tap_min; tap <= tap_max; tap++) {
		acc += *in_ptr * *wptr;
		in_ptr = (float *)((char *)in_ptr + in_x_stride);
		wptr++;
	    }
	    *out_ptr = acc;
	    out_ptr = (float *)((char *)out_ptr + out_x_stride);
	    sample_template++;
	} /* for out_x */

	/* set pointers to next scanline */
	for (dim = 1; dim < num_dimens; dim++) {
	    if (++scan_coord[dim] < src_dimens[dim]) {
		in_scan_ptr = (float *)((char *)in_scan_ptr +
					src_strides[dim]);
		out_scan_ptr = (float *)((char *)out_scan_ptr +
					 dst_strides[dim]);
		break;
	    } else if (dim == num_dimens-1) {
		done = 1;
	    } else {
		scan_coord[dim] = 0;
		in_scan_ptr = (float *)((char *)in_scan_ptr -
			      src_strides[dim] * src_dimens[dim]);
		out_scan_ptr = (float *)((char *)out_scan_ptr -
			      dst_strides[dim] * dst_dimens[dim]);
	    }
	}
    } /* while scanlines */

    /* clean up */
    Dealloc(vpc, scan_coord);
}

/*
 * vpResample2D
 *
 * Resample a 2D array.
 */

vpResult
vpResample2D(in_array, in_x, in_y, out_array, out_x, out_y,
	     element_type, filter_type)
void *in_array;		/* input array containing data */
int in_x, in_y;		/* input array dimensions */
void *out_array;	/* storage for output array */
int out_x, out_y;	/* output array dimensions */
int element_type;	/* type of array element (VP_UCHAR, VP_USHORT,
			   VP_FLOAT) */
int filter_type;	/* type of filter (VP_BOX_FILTER, etc.) */
{
    int src_dimens[2], dst_dimens[2];
    int src_strides[2], dst_strides[2];
    void *tmp1_array;
    int element_size;
    vpResult code;
    vpContext *vpc;
    float *weights;

    /* compute size of array element and allocate intermediate arrays */
    switch (element_type) {
    case VP_UCHAR:
	element_size = 1;
	break;
    case VP_USHORT:
	element_size = 2;
	break;
    case VP_FLOAT:
	element_size = 4;
	break;
    default:
	return(VPSetError(vpc, VPERROR_BAD_OPTION));
    }
    vpc = vpCreateContext();
    Alloc(vpc, tmp1_array, void *, out_x*in_y*element_size, "resample_tmp1");

    /* resample first dimension */
    src_dimens[0] = in_x;
    src_dimens[1] = in_y;

    dst_dimens[0] = out_x;
    dst_dimens[1] = in_y;

    src_strides[0] = element_size;
    src_strides[1] = src_dimens[0] * src_strides[0];

    dst_strides[0] = element_size;
    dst_strides[1] = dst_dimens[0] * dst_strides[0];

    weights = ComputeWeights(vpc, src_dimens[0], dst_dimens[0], filter_type);
    if (weights == NULL) {
	Dealloc(vpc, tmp1_array);
	return(vpc->error_code);
    }
    code = vpResample(vpc, 2, src_dimens, dst_dimens, src_strides, dst_strides,
		      element_type, in_array, tmp1_array);
    Dealloc(vpc, weights);

    if (code != VP_OK) {
	Dealloc(vpc, tmp1_array);
	return(code);
    }

    /* resample second dimension */
    src_dimens[1] = out_x;
    src_dimens[0] = in_y;

    dst_dimens[1] = out_x;
    dst_dimens[0] = out_y;

    src_strides[1] = element_size;
    src_strides[0] = src_dimens[1] * src_strides[1];

    dst_strides[1] = element_size;
    dst_strides[0] = dst_dimens[1] * dst_strides[1];

    weights = ComputeWeights(vpc, src_dimens[0], dst_dimens[0], filter_type);
    if (weights == NULL) {
	Dealloc(vpc, tmp1_array);
	return(vpc->error_code);
    }
    code = vpResample(vpc, 2, src_dimens, dst_dimens, src_strides, dst_strides,
		      element_type, tmp1_array, out_array);
    Dealloc(vpc, weights);

    if (code != VP_OK) {
	Dealloc(vpc, tmp1_array);
	return(code);
    }

    /* clean up */
    Dealloc(vpc, tmp1_array);
    return(VP_OK);
}

/*
 * vpResample3D
 *
 * Resample a 3D array.
 */

vpResult
vpResample3D(in_array, in_x, in_y, in_z, out_array, out_x, out_y, out_z,
	     element_type, filter_type)
void *in_array;		/* input array containing data */
int in_x, in_y, in_z;	/* input array dimensions */
void *out_array;	/* storage for output array */
int out_x, out_y, out_z;/* output array dimensions */
int element_type;	/* type of array element (VP_UCHAR, VP_USHORT,
			   VP_FLOAT) */
int filter_type;	/* type of filter (VP_BOX_FILTER, etc.) */
{
    int src_dimens[3], dst_dimens[3];
    int src_strides[3], dst_strides[3];
    void *tmp1_array, *tmp2_array;
    int element_size;
    vpResult code;
    vpContext *vpc;
    float *weights;

    /* compute size of array element and allocate intermediate arrays */
    switch (element_type) {
    case VP_UCHAR:
	element_size = 1;
	break;
    case VP_USHORT:
	element_size = 2;
	break;
    case VP_FLOAT:
	element_size = 4;
	break;
    default:
	return(VPSetError(vpc, VPERROR_BAD_OPTION));
    }
    vpc = vpCreateContext();
    Alloc(vpc, tmp1_array, void *, out_x * in_y * in_z * element_size,
	  "resample_tmp1");
    Alloc(vpc, tmp2_array, void *, out_x * out_y * in_z * element_size,
	  "resample_tmp2");

    /* resample first dimension */
    src_dimens[0] = in_x;
    src_dimens[1] = in_y;
    src_dimens[2] = in_z;

    dst_dimens[0] = out_x;
    dst_dimens[1] = in_y;
    dst_dimens[2] = in_z;

    src_strides[0] = element_size;
    src_strides[1] = src_dimens[0] * src_strides[0];
    src_strides[2] = src_dimens[1] * src_strides[1];

    dst_strides[0] = element_size;
    dst_strides[1] = dst_dimens[0] * dst_strides[0];
    dst_strides[2] = dst_dimens[1] * dst_strides[1];

    weights = ComputeWeights(vpc, src_dimens[0], dst_dimens[0], filter_type);
    if (weights == NULL) {
	Dealloc(vpc, tmp1_array);
	Dealloc(vpc, tmp2_array);
	return(vpc->error_code);
    }
    code = vpResample(vpc, 3, src_dimens, dst_dimens, src_strides, dst_strides,
		      element_type, in_array, tmp1_array);
    Dealloc(vpc, weights);

    if (code != VP_OK) {
	Dealloc(vpc, tmp1_array);
	Dealloc(vpc, tmp2_array);
	return(code);
    }

    /* resample second dimension */
    src_dimens[1] = out_x;
    src_dimens[0] = in_y;
    src_dimens[2] = in_z;

    dst_dimens[1] = out_x;
    dst_dimens[0] = out_y;
    dst_dimens[2] = in_z;

    src_strides[1] = element_size;
    src_strides[0] = src_dimens[1] * src_strides[1];
    src_strides[2] = src_dimens[0] * src_strides[0];

    dst_strides[1] = element_size;
    dst_strides[0] = dst_dimens[1] * dst_strides[1];
    dst_strides[2] = dst_dimens[0] * dst_strides[0];

    weights = ComputeWeights(vpc, src_dimens[0], dst_dimens[0], filter_type);
    if (weights == NULL) {
	Dealloc(vpc, tmp1_array);
	Dealloc(vpc, tmp2_array);
	return(vpc->error_code);
    }
    code = vpResample(vpc, 3, src_dimens, dst_dimens, src_strides, dst_strides,
		      element_type, tmp1_array, tmp2_array);
    Dealloc(vpc, weights);

    if (code != VP_OK) {
	Dealloc(vpc, tmp1_array);
	Dealloc(vpc, tmp2_array);
	return(code);
    }

    /* resample third dimension */
    src_dimens[1] = out_x;
    src_dimens[2] = out_y;
    src_dimens[0] = in_z;

    dst_dimens[1] = out_x;
    dst_dimens[2] = out_y;
    dst_dimens[0] = out_z;

    src_strides[1] = element_size;
    src_strides[2] = src_dimens[1] * src_strides[1];
    src_strides[0] = src_dimens[2] * src_strides[2];

    dst_strides[1] = element_size;
    dst_strides[2] = dst_dimens[1] * dst_strides[1];
    dst_strides[0] = dst_dimens[2] * dst_strides[2];

    weights = ComputeWeights(vpc, src_dimens[0], dst_dimens[0], filter_type);
    if (weights == NULL) {
	Dealloc(vpc, tmp1_array);
	Dealloc(vpc, tmp2_array);
	return(vpc->error_code);
    }
    code = vpResample(vpc, 3, src_dimens, dst_dimens, src_strides, dst_strides,
		      element_type, tmp2_array, out_array);
    Dealloc(vpc, weights);

    if (code != VP_OK) {
	Dealloc(vpc, tmp1_array);
	Dealloc(vpc, tmp2_array);
	return(code);
    }

    /* clean up */
    Dealloc(vpc, tmp1_array);
    Dealloc(vpc, tmp2_array);
    return(VP_OK);
}

/*
 * ComputeWeights
 *
 * Allocate and compute a filter weight table for a predefined filter type.
 */

static float *
ComputeWeights(vpc, src_xlen, dst_xlen, filter_type)
vpContext *vpc;		/* context for storing table */
int src_xlen;		/* number of samples in input scanline */
int dst_xlen;		/* number of samples in output scanline */
int filter_type;	/* type of filter (VP_BOX_FILTER, etc.) */
{
    double scale_factor;
    int num_phases, num_taps, support, tap_limit, phases, table_size;
    int code;
    float *weights;

    switch (filter_type) {
    case VP_BOX_FILTER:
	support = 1;
	break;
    case VP_LINEAR_FILTER:
	support = 2;
	break;
    case VP_GAUSSIAN_FILTER:
	support = 3;
	break;
    case VP_BSPLINE_FILTER:
    case VP_MITCHELL_FILTER:
	support = 4;
	break;
    default:
	VPSetError(vpc, VPERROR_BAD_OPTION);
	return(NULL);
    }
    scale_factor = (double)dst_xlen / (double)src_xlen;
    if (scale_factor >= 1.0) {
	num_taps = support;
	num_phases = 1024;
    } else {
	num_taps = (double)support / scale_factor;
	tap_limit = 4;
	phases = 1024;
	while (1) {
	    if (num_taps <= tap_limit) {
		num_phases = phases;
		break;
	    }
	    tap_limit *= 2;
	    phases /= 2;
	    if (phases <= 1) {
		num_phases = 1;
		break;
	    }
	}
    }
    table_size = num_taps * num_phases * sizeof(float);
    Alloc(vpc, weights, float *, table_size, "weight_table");
    switch (filter_type) {
    case VP_BOX_FILTER:
	code = vpBoxFilter(num_taps, num_phases, weights, table_size);
	if (code != VP_OK) {
	    Dealloc(vpc, weights);
	    VPSetError(vpc, code);
	    return(NULL);
	}
	break;
    case VP_LINEAR_FILTER:
	code = vpLinearFilter(num_taps, num_phases, weights, table_size);
	if (code != VP_OK) {
	    Dealloc(vpc, weights);
	    VPSetError(vpc, code);
	    return(NULL);
	}
	break;
    case VP_GAUSSIAN_FILTER:
	code = vpGaussianFilter(VP_GAUSSIAN_SIGMA, num_taps, num_phases,
				weights, table_size);
	if (code != VP_OK) {
	    Dealloc(vpc, weights);
	    VPSetError(vpc, code);
	    return(NULL);
	}
	break;
    case VP_BSPLINE_FILTER:
	code = vpBicubicFilter(1.0, 0.0, num_taps, num_phases, weights,
			       table_size);
	if (code != VP_OK) {
	    Dealloc(vpc, weights);
	    VPSetError(vpc, code);
	    return(NULL);
	}
	break;
    case VP_MITCHELL_FILTER:
	code = vpBicubicFilter(1.0/3.0, 1.0/3.0, num_taps, num_phases,
			       weights, table_size);
	if (code != VP_OK) {
	    Dealloc(vpc, weights);
	    VPSetError(vpc, code);
	    return(NULL);
	}
	break;
    }
    vpSetFilter(vpc, num_taps, num_phases, weights);
    return(weights);
}

/*
 * vpBoxFilter
 *
 * Compute filter weights for box filter.
 * For abs(x) < 0.5:
 *      k(x) = C
 * (C is chosen so that k(x) integrates to 1).
 * Otherwise:
 *      k(x) = 0
 */

vpResult
vpBoxFilter(num_taps, num_phases, weights, weights_bytes)
int num_taps;	/* number of filter taps to compute */
int num_phases; /* number of phases to compute */
float *weights;	/* array for storing filter weights
		   (num_taps*num_phases entries) */
int weights_bytes; /* size of array (for error checking) */
{
    int p, t;
    float *wptr;
    double value;

    if (weights_bytes != num_taps * num_phases * sizeof(float))
	return(VPERROR_BAD_SIZE);
    if (num_phases % 2 != 0)
	return(VPERROR_BAD_VALUE);
    wptr = weights;
    value = 1.0 / (double)num_taps;
    for (p = 0; p < num_phases; p++) {
	for (t = 0; t < num_taps; t++) {
	    *wptr++ = value;
	}
    }
    return(VP_OK);
}

/*
 * vpLinearFilter
 *
 * Compute filter weights for linear interpolation.
 * For abs(x) < 1:
 *      k(x) = C * (1 - abs(x))
 * (C is chosen so that k(x) integrates to 1).
 * Otherwise:
 *      k(x) = 0
 */

vpResult
vpLinearFilter(num_taps, num_phases, weights, weights_bytes)
int num_taps;	/* number of filter taps to compute */
int num_phases; /* number of phases to compute */
float *weights;	/* array for storing filter weights
		   (num_taps*num_phases entries) */
int weights_bytes; /* size of array (for error checking) */
{
    int p, t;
    float *wptr1, *wptr2;
    double x0, delta_x, x, xa, tap_spacing, sum, normalize, value;

    if (weights_bytes != num_taps * num_phases * sizeof(float))
	return(VPERROR_BAD_SIZE);
    if (num_phases % 2 != 0)
	return(VPERROR_BAD_VALUE);
    wptr1 = weights;
    tap_spacing = 2.0 / (double)num_taps;
    x0 = -tap_spacing * ((double)num_taps/2.0 - 1.0);
    delta_x = tap_spacing / (double)num_phases;
    for (p = 0; p < num_phases/2; p++) {
	x = x0;
	sum = 0;
	for (t = 0; t < num_taps; t++) {
	    if (x < 0.0)
		xa = -x;
	    else
		xa = x;
	    value = 1.0 - xa;
	    wptr1[t] = value;
	    sum += value;
	    x += tap_spacing;
	}
	normalize = 1.0 / sum;
	for (t = 0; t < num_taps; t++) {
	    wptr1[t] *= normalize;
	}
	wptr1 += num_taps;
	x0 -= delta_x;
    }
    wptr2 = wptr1;
    for (p = 0; p < num_phases/2; p++) {
	for (t = 0; t < num_taps; t++) {
	    *wptr1++ = *--wptr2;
	}
    }
    return(VP_OK);
}

/*
 * vpBicubicFilter
 *
 * Compute filter weights for a Mitchell bicubic.
 *
 * See Mitchell, D.P. and Netravali, A.N., "Reconstruction filters in
 * computer graphics," Proc. SIGGRAPH '88 (Computer Graphics V22 N4),
 * p. 221-8.
 */

vpResult
vpBicubicFilter(b_value, c_value, num_taps, num_phases, weights, weights_bytes)
double b_value;	/* b in the filter kernel equation */
double c_value; /* c in the filter kernel equation */
int num_taps;	/* number of filter taps to compute */
int num_phases; /* number of phases to compute */
float *weights;	/* array for storing filter weights
		   (num_taps*num_phases entries) */
int weights_bytes; /* size of array (for error checking) */
{
    int p, t;
    float *wptr1, *wptr2;
    double x0, delta_x, x, xa, tap_spacing, sum, normalize, value;

    if (weights_bytes != num_taps * num_phases * sizeof(float))
	return(VPERROR_BAD_SIZE);
    if (num_phases % 2 != 0)
	return(VPERROR_BAD_VALUE);
    wptr1 = weights;
    tap_spacing = 4.0 / (double)num_taps;
    x0 = -tap_spacing * ((double)num_taps/2.0 - 1.0);
    delta_x = tap_spacing / (double)num_phases;
    for (p = 0; p < num_phases/2; p++) {
	x = x0;
	sum = 0;
	for (t = 0; t < num_taps; t++) {
	    if (x < 0.0)
		xa = -x;
	    else
		xa = x;
	    if (xa < 1.0) {
		value = (((12. - 9.*b_value - 6.*c_value)*xa - 18. +
			  12.*b_value + 6.*c_value)*xa*xa + 6. -
			 2.*b_value) * 1./6.;
	    } else {
		value = ((((-b_value - 6.*c_value)*xa + 6.*b_value +
			   30.*c_value)*xa - 12.*b_value - 48.*c_value)*xa +
			 8.*b_value + 24.*c_value)* 1./6.;
	    }
	    wptr1[t] = value;
	    sum += value;
	    x += tap_spacing;
	}
	normalize = 1.0 / sum;
	for (t = 0; t < num_taps; t++) {
	    wptr1[t] *= normalize;
	}
	wptr1 += num_taps;
	x0 -= delta_x;
    }
    wptr2 = wptr1;
    for (p = 0; p < num_phases/2; p++) {
	for (t = 0; t < num_taps; t++) {
	    *wptr1++ = *--wptr2;
	}
    }
    return(VP_OK);
}

/*
 * vpGaussianFilter
 *
 * Compute filter weights for a Gaussian.
 * For abs(x) <= 1.0:
 *      k(x) = C * exp(-x*x/(2*sigma*sigma))
 * (C is chosen so that k(x) integrates to 1).
 * Otherwise:
 *      k(x) = 0
 */

vpResult
vpGaussianFilter(sigma, num_taps, num_phases, weights, weights_bytes)
double sigma;   /* standard deviation */
int num_taps;	/* number of filter taps to compute */
int num_phases; /* number of phases to compute */
float *weights;	/* array for storing filter weights
		   (num_taps*num_phases entries) */
int weights_bytes; /* size of array (for error checking) */
{
    int p, t;
    float *wptr1, *wptr2;
    double x0, delta_x, x, tap_spacing, sigma2_inv, sum, normalize, value;

    if (weights_bytes != num_taps * num_phases * sizeof(float))
	return(VPERROR_BAD_SIZE);
    if (num_phases % 2 != 0)
	return(VPERROR_BAD_VALUE);
    wptr1 = weights;
    sigma2_inv = -1.0 / (2.0 * sigma * sigma);
    tap_spacing = 2.0 / (double)num_taps;
    x0 = -tap_spacing * ((double)num_taps/2.0 - 1.0);
    delta_x = tap_spacing / (double)num_phases;
    for (p = 0; p < num_phases/2; p++) {
	x = x0;
	sum = 0;
	for (t = 0; t < num_taps; t++) {
	    value = exp(x*x*sigma2_inv);
	    wptr1[t] = value;
	    sum += value;
	    x += tap_spacing;
	}
	normalize = 1.0 / sum;
	for (t = 0; t < num_taps; t++) {
	    wptr1[t] *= normalize;
	}
	wptr1 += num_taps;
	x0 -= delta_x;
    }
    wptr2 = wptr1;
    for (p = 0; p < num_phases/2; p++) {
	for (t = 0; t < num_taps; t++) {
	    *wptr1++ = *--wptr2;
	}
    }
    return(VP_OK);
}
