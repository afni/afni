/*
 * vp_global.h
 *
 * Non-exported global declarations for VolPack.
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

/*******************************************************************
 * Include files.                                                  *
 *******************************************************************/

#include "config.h"
#include "volpack.h"

#include <stdio.h>
#include <stdlib.h>   /* replaces malloc.h - 30 Apr 2003 */
#include <math.h>
extern double rint ANSI_ARGS((double x)); /* missing from Ultrix math.h */

#ifdef ANSI_C
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#if STDC_HEADERS || HAVE_STRING_H
#include <string.h>
/* An ANSI string.h and pre-ANSI memory.h might conflict.  */
#if !STDC_HEADERS && HAVE_MEMORY_H
#include <memory.h>
#endif /* not STDC_HEADERS and HAVE_MEMORY_H */
#define index strchr
#define rindex strrchr
#define bcopy(s, d, n) memcpy ((d), (s), (n))
#define bcmp(s1, s2, n) memcmp ((s1), (s2), (n))
#define bzero(s, n) memset ((s), 0, (n))
#else /* not STDC_HEADERS and not HAVE_STRING_H */
#include <strings.h>
/* memory.h and strings.h conflict on some systems.  */
#endif /* not STDC_HEADERS and not HAVE_STRING_H */

#ifdef HAVE_LORES_TIMER
/* use the low-resolution interval timer system call to collect timings */
#   define USE_TIMER
#   include <sys/time.h>
#   define DECLARE_TIME(t)	struct timeval t
#   define GET_TIME(vpc, t)	gettimeofday(&t)
#   define STORE_TIME(vpc, code, t0, t1)	\
    vpc->timer_ticks[code] += ((t1).tv_usec < (t0).tv_usec) ? \
	(((t1).tv_sec - (t0).tv_sec)*1000000 - ((t0).tv_usec - (t1).tv_usec)):\
	(((t1).tv_sec - (t0).tv_sec)*1000000 + ((t1).tv_usec - (t0).tv_usec));
#endif

#ifdef HAVE_HIRES_TIMER
/* use the high-resolution memory-mapped timer to collect timings 
   (available only on certain SGI architectures) */
#   define USE_TIMER
#   include <fcntl.h>
#   include <sys/mman.h>
#   include <sys/syssgi.h>
#   include <sys/immu.h>
#   define DECLARE_TIME(t)	unsigned t
#   define GET_TIME(vpc, t)	t = *vpc->timer_addr
#   define STORE_TIME(vpc, code, t0, t1)	\
    vpc->timer_ticks[code] += ((t1) < (t0)) ? \
	((0xFFFFFFFF - (t0)) + (t1)) : ((t1) - (t0));
#   define DECLARE_HIRES_TIME(t)	DECLARE_TIME(t)
#   define GET_HIRES_TIME(vpc, t)	GET_TIME(vpc, t)
#   define STORE_HIRES_TIME(vpc, code, t0, t1)	STORE_TIME(vpc, code, t0, t1)
#   define COPY_HIRES_TIME(t0, t1)	t0 = t1
#else
#   define DECLARE_HIRES_TIME(t)
#   define GET_HIRES_TIME(vpc, t)
#   define STORE_HIRES_TIME(vpc, code, t0, t1)
#   define COPY_HIRES_TIME(t0, t1)
#endif

#ifndef USE_TIMER
#   define DECLARE_TIME(t)
#   define GET_TIME(vpc, t)
#   define STORE_TIME(vpc, code, t0, t1)
#endif

/*******************************************************************
 * Constants.                                                      *
 *******************************************************************/

/* rendering algorithms */
#define USE_RLEVOLUME	10	/* use classified, run-length encoded volume */
#define USE_RAWVOLUME	11	/* use raw volume (with octree if present) */

/* shading modes */
#define CALLBACK_SHADER	20
#define LOOKUP_SHADER	21

/* surfaces */
#define EXT_SURFACE	0
#define INT_SURFACE	1

/* material property indexes */
#define MATL_AMB_R	0		/* ambient red coefficient */
#define MATL_DIFF_R	1		/* diffuse red coefficient */
#define MATL_SPEC_R	2		/* specular red coefficient */
#define MATL_AMB_G	3		/* ambient green coefficient */
#define MATL_DIFF_G	4		/* diffuse green coefficient */
#define MATL_SPEC_G	5		/* specular green coefficient */
#define MATL_AMB_B	6		/* ambient blue coefficient */
#define MATL_DIFF_B	7		/* diffuse blue coefficient */
#define MATL_SPEC_B	8		/* specular blue coefficient */
#define MATL_SHINY	9		/* specular exponent */
#define NUM_MATL_PROPS	10

/* default material properties */
#define DEFAULT_AMBIENT		(0.1*255.0)
#define DEFAULT_DIFFUSE		(0.4*255.0)
#define DEFAULT_SPECULAR	(0.5*255.0)
#define DEFAULT_SHINYNESS	10.0

/* default depth cueing parameters */
#define DEFAULT_DC_QUANTIZATION	(1./255.) /* quantization */

/* size of lookup table for filter kernel used in the 2D warp */
#define WARP_WEIGHT_INDEX_BITS	6
#define WARP_WEIGHT_ENTRIES	(1 << WARP_WEIGHT_INDEX_BITS)

/* constants for MinMaxOctree and fast classification */
#define VP_MAX_OCTREE_LEVELS	16	/* maximum number of levels in octree;
					   each side of volume can be at most
					   (2^VP_MAX_OCTREE_LEVELS)*
					   base_node_size */

#define MM_K_BIT	4		/* bits in child_octant field of */
#define MM_J_BIT	2		/* MMOctreeLevel */
#define MM_I_BIT	1

#define MM_EMPTY	0		/* node is transparent */
#define MM_FULL		1		/* node is non-transparent */
#define MM_PARTFULL	2		/* node is neither transparent
					   nor opaque and is not a leaf */

#define INDEX_RUN_IS_ZERO 0x7000	/* bit to indicate a run in an
					   index volume is a zero run */

/*******************************************************************
 * Macros.                                                         *
 *******************************************************************/

#ifndef NULL           /* added by RWCox */
#define NULL 0
#endif

#define MAX(a,b)        (((a)<(b)) ? (b) : (a))
#define MIN(a,b)        (((a)>(b)) ? (b) : (a))

/* Alloc: allocate memory */
#define Alloc(context, ptr, type, size, use)                            \
{                                                                       \
    (ptr) = (type)malloc((unsigned)(size));		                \
    if ((ptr) == NULL)							\
	VPBug("out of memory");						\
    if ((context)->log_alloc_func != NULL)                              \
        (context)->log_alloc_func((ptr),(size),(use),			\
		   __LINE__,__FILE__,(context)->client_data);		\
}

/* Dealloc: free memory */
#define Dealloc(context, ptr)						\
{									\
    if ((context)->log_free_func != NULL)                               \
        (context)->log_free_func(ptr, (context)->client_data);		\
    free((void *)ptr);							\
}

/* Debug: print debugging information */
#ifdef DEBUG
#define Debug(x)        VPDebug x
#else
#define Debug(x)
#endif

/* assertions */
#ifdef ASSERTIONS
#define ASSERT(expr)                                                    \
    if (!(expr))                                                        \
	VPBug("assertion failed on line %d of file %s",                 \
	      __LINE__, __FILE__)
#else
#define ASSERT(expr)
#endif

/* ReportStatus: call a callback to give a progress report */
#define ReportStatus(context, frac)      				\
    if ((context)->status_func != NULL)					\
	(context)->status_func(frac, (context)->client_data)

/* macros for accessing the fields of a voxel */
#define ByteField(voxel, offset)   (*((unsigned char *)(voxel) + (offset)))
#define ShortField(voxel, offset)  (*((unsigned short *)((char *)(voxel)+\
				      (offset))))
#define IntField(voxel, offset)    (*((unsigned *)((char *)(voxel)+(offset))))
#define VoxelField(voxel, offset, size) 			\
    (((size) == 1) ? ByteField(voxel, offset) :			\
     ((size) == 2) ? ShortField(voxel, offset) : 		\
		     IntField(voxel, offset))

/*******************************************************************
 * Type definitions.                                               *
 *******************************************************************/

/* ScanOffset: offsets to beginning of a run-length encoded voxel scanline */
typedef struct {		/* ScanOffset */
    int first_len;		/*   byte offset to first run length */
    int first_data;		/*   byte offset to first voxel data */
} ScanOffset;

#ifdef INDEX_VOLUME
/* VoxelLocation: pointer to the run containing a voxel and the position
   of the voxel in the run */
typedef struct {		/* VoxelLocation */
    unsigned char run_count;	/*   number of voxels in run starting from
				     this voxel (run_count = run_len for
				     first voxel in run, run_count = 1 for
				     last voxel in run) */
    unsigned char len_offset;	/*   offset in bytes from first run length
				     for scanline to run length for voxel */
    short data_offset;		/*   offset in bytes from first voxel data
				     for scanline to data for voxel, or -1
				     if voxel has no data (zero run) */
} VoxelLocation;
#endif /* INDEX_VOLUME */

/* RLEVoxels: a run-length encoding of a classified volume for one of the
	      three principal viewing axes */
typedef struct {		/* RLEVoxels */
    int ilen, jlen, klen;	/*   size of each dimension */
    int run_count;		/*   number of runs */
    unsigned char *run_lengths;	/*   length of each run */
    int data_count;		/*   number of entries in data */
    void *data;			/*   data */
    int scan_offsets_per_slice;	/*   number of entries in scan_offsets for
				     each slice of voxel data */
    ScanOffset *scan_offsets;	/*   array of offsets to scanlines */
    int mmapped;		/*   true if pointers point to memory-mapped
				     data */
#ifdef INDEX_VOLUME
    VoxelLocation *voxel_index;	/*   index for experimental ERT code */
#endif
} RLEVoxels;

/* RunData: statistics about the runs in a scanline of voxels */
typedef struct {		/* RunData */
    short run_length;		/*   length of current run */
    short next_index;		/*   index of next voxel in current scanline */
    union {
	struct {		/*   data for pass 1 (ClassifyRawScanline) */
	    short run_count;	/*   number of runs in scanline */
	    short non_zero_count; /* number of nontransparent voxels in
				     scanline */
	} p1;
	struct {		/*   data for pass 2 (RepackClassifiedVolume)*/
	    unsigned data_offset; /* byte offset for storage for voxel */
	    unsigned length_offset; /* byte offset for storage for runlength */
	} p2;
    } p;
} RunData;

/* GBuffer: growable buffer (a singly-linked list of buffers) */
#define GBUFFER_SIZE	1024	/* number of bytes per list element
				   (to optimize memory performance, size
				   should be small enough so that at
				   least two buffers fit in the cache) */

typedef struct _gbuffer {	/* GBuffer */
    int bytes_left;		/*   size of unallocated space */
    char *data_ptr;		/*   pointer to next available byte */
    struct _gbuffer *next;	/*   next buffer in list */
    char data[GBUFFER_SIZE];	/*   data */
} GBuffer;

/* ConstructionBuffer: buffer for storing data while constructing a
                       classified volume */
typedef struct {		/* ConstructionBuffer */
    RunData *rundata_y;		/*   statistics for y-axis runs (2D array)
				     index: x*zlen + z */
    RunData *rundata_z;		/*   statistics for z-axis runs (2D array)
				     index: y*xlen + x */
    GBuffer *data_buf_head;	/*   first buffer for storing voxels */
    GBuffer *data_buf_tail;	/*   last buffer in list */
    GBuffer *lengths_buf_head;	/*   first buffer for storing run lengths */
    GBuffer *lengths_buf_tail;	/*   last buffer in list */
    int non_zero_count;		/*   number of nonzero voxels in data bufs */
    int x_run_count;		/*   number of z-axis runs seen so far */
    unsigned char octree_runs[VP_MAX_VOLUME_DIM];
    				/*   estimated run lengths from octree */
    int octree_scans_left;	/*   number of scanlines until octree_runs
				     must be recomputed */
    int next_z;			/*   z coordinate of next scanline */
    int next_y;			/*   y coordinate of next scanline */
} ConstructionBuffer;

/* MinMaxOctree: min-max octree representation of the volume for
   fast classification */
typedef struct {		/* MinMaxOctree */
    int levels;			/*   number of levels in octree
				     level 0 = root = lowest detail
				     level levels-1 = base = finest detail */
    int root_node_size;		/*   voxels/side for root level */
    int base_node_size;		/*   voxels/side for base level */
    int range_bytes_per_node;	/*   bytes/node for min/max data */
    int base_bytes_per_node;	/*   bytes/node for base level
				     (min/max data + status) */
    int nonbase_bytes_per_node; /*   bytes/node for non-base level
				     (min/max data + status + ptr to child) */
    int node_offset[VP_MAX_FIELDS]; /* offset to min/max data for each
				       parameter in a node; min comes first */
    int status_offset;		/*   offset to status field */
    int child_offset;		/*   offset to child field */
    void *root;			/*   storage for the octree, and a pointer
				     to the root node */
    int octree_bytes;		/*   bytes of storage for the octree */

    /*
     * Layout of an octree node:
     *
     * Nodes at level 0 (root) through levels-2 are nonbase_bytes_per_node
     * bytes long.  This space is divided into a number of fields as follows:
     *
     *    minimum value for parameter 0 of the classification function
     *    maximum value for parameter 0
     *    minimum value for parameter 1
     *    maximum value for parameter 1
     *    .
     *    .
     *    .
     *    minimum value for parameter num_clsfy_params-1
     *    maximum value for parameter num_clsfy_params-1
     *    status field
     *    child field
     *
     * The min/max fields are the same size as the corresponding
     * classification parameters (one or two bytes; four bytes not allowed).
     * Node_offset[] gives offsets to the min field for each parameter.
     *
     * The status field is a one byte field which is filled in during
     * classification of the volume (VPClassifyOctree).  The possible
     * values are MM_EMPTY (node is transparent), MM_FULL (node has
     * no transparent children), or MM_PARTFULL.  Status_offset
     * gives the offset to this field from the beginning of the node.
     *
     * The child field is a one word field which gives the number of
     * bytes from the root of the octree to the first child node of
     * this node.  The remaining 7 children follow right after the
     * first child.  Child_offset gives the offset to this field
     * from the beginning of the node.
     *
     * There may be additional bytes of padding in between fields
     * to satisfy alignment restrictions.  This could make the code
     * non-portable on machines with bizarre requirements.
     *
     * Nodes at level levels-1 are base_bytes_per_node long and contain
     * the same fields except there is no child field.
     *
     * NOTE: As you can probably tell, I'm simulating a C structure here.
     * Unfortunately I can't use a real structure because I don't know the
     * number of fields in advance: it depends on the number and
     * size of the classification parameters.
     */
} MinMaxOctree;

/* MMOctreeLevel: data associated with one level of the min-max octree during
 *     a depth first traversal; this data is used to compute which octree
 *     nodes intersect the current scanline of interest */
typedef struct {		/* MMOctreeLevel */
    int level_size;		/*   voxels/side for a node at this level */
    int child_octant;		/*   octant number of first child to visit
				     when a scanline intersects a node at
				     this level */
    int child_offset1;		/*   offset (in bytes) of first child to
				     visit relative to child 0 */
    int child_offset2;		/*   offset (in bytes) of second child to
				     visit relative to child 0 */
    void *child2;		/*   points to 2nd child if first is
				     begin visit; NULL otherwise */
} MMOctreeLevel;

/* GrayIntPixel: grayscale intermediate image pixel */
typedef struct {		/* GrayIntPixel */
    float clrflt;		/*   color */
    float opcflt;		/*   opacity */
    short lnk;			/*   early-ray termination link */
    unsigned char zratio;	/*   integer part of opacity correction
				       exponent (perspective only) */
    unsigned char zfrac;	/*   fractional part of opacity correction
				       exponent (perspective only) */
} GrayIntPixel;

/* RGBIntPixel: RGB intermediate image pixel */
typedef struct {		/* RGBIntPixel */
    float rclrflt;		/*   color (red channel) */
    float gclrflt;		/*   color (green channel) */
    float bclrflt;		/*   color (blue channel) */
    float opcflt;		/*   opacity */
    short lnk;			/*   early-ray termination link */
    unsigned char zratio;	/*   integer part of opacity correction
				       exponent (perspective only) */
    unsigned char zfrac;	/*   fractional part of opacity correction
				       exponent (perspective only) */
} RGBIntPixel;

/* Trapezoid: a trapezoidal region of the output image (used to record
 *            intersection of the intermediate and final images) */
typedef struct {
    /* all coordinates are in the coordinate system of the final image */
    int miny;		/* first scanline of the final image which
			   overlaps the trapezoid */
    int maxy;		/* last scanline of the final image which
			   overlaps the trapezoid */
    double x_top_lft;	/* left edge of the trapezoid at y = miny */
    double x_top_rgt;	/* right edge of the trapezoid at y = miny */
    double x_incr_lft;	/* increment to add to x_top_lft to get the
			   left edge of the trapezoid at the next y */
    double x_incr_rgt;	/* increment to add to x_top_rgt to get the
			   right edge of the trapezoid at the next y */
} Trapezoid;

/* vpContext: rendering context */
struct _vp_context {			/* vpContext */

    /* *** USER-SPECIFIED PARAMETERS *** */

    /* raw volume data layout */
    int xlen, ylen, zlen;		/* voxels in each dimension */
    short raw_bytes_per_voxel;		/* size of raw voxel in bytes */
    short num_voxel_fields;		/* number of fields in the voxel */
    short num_shade_fields;		/* # of voxel fields for shading */
    short field_size[VP_MAX_FIELDS];	/* size of each field in bytes */
    short field_offset[VP_MAX_FIELDS];	/* byte offset for each field */
    int field_max[VP_MAX_FIELDS];	/* maximum value of each field */
    void *raw_voxels;			/* voxel data */
    int raw_voxels_size;		/* size of raw_voxels in bytes */
    int xstride, ystride, zstride;	/* byte stride for each dimension */

    /* classification specification */
    double min_opacity;			/* low opacity threshold (0.0-1.0) */
    int num_clsfy_params;		/* # of params for classification */
    int param_field[VP_MAX_FIELDS];	/* voxel field # for each param */
    float *clsfy_table[VP_MAX_FIELDS];	/* lookup table for each parameter */
    int clsfy_table_size[VP_MAX_FIELDS]; /* size of each table in bytes */

    /* shading specification */
    int color_channels;			/* number of color channels */
    int shading_mode;			/* type of shader to use */
    float *shade_color_table;		/* lookup table for LOOKUP_SHADER */
    int shade_color_table_size;		/* size of shade table in bytes */
    float *shade_weight_table;		/* lookup table for LOOKUP_SHADER */
    int shade_weight_table_size;	/* size of weight table in bytes */
    short num_materials;		/* # of material types in tables */
    short color_field;			/* voxel field # for color index */
    short weight_field;			/* field # for weight index */

    /* material properties for each material and surface side */
    double matl_props[VP_MAX_MATERIAL][2][NUM_MATL_PROPS]; /* 0.0-255.0 */

    /* lighting properties */
    short light_enable[VP_MAX_LIGHTS];	/* enable each light */
    vpVector4 light_vector[VP_MAX_LIGHTS]; /* normalized light direction
					      vectors (in eye coordinates) */
    vpVector3 light_color[VP_MAX_LIGHTS];  /* light colors (0.0-1.0) */
    int light_both_sides;		/* use two-sided lighting */
    int reverse_surface_sides;		/* reverse interior and exterior */

    /* depth cueing parameters */
    short dc_enable;			/* true to enable depth cueing */
    double dc_front_factor;		/* front depth cueing factor */
    double dc_density;			/* fog density for depth cueing */
    int dc_table_len_hint;		/* hint for dc_table_len */
    double dc_quantization;		/* minimum resolvable depth */

    /* view specification */
    vpMatrix4 transforms[3];		/* transformation matrices (VP_MODEL,
					   VP_VIEW and VP_PROJECT) */
    short current_matrix;		/* current matrix */
    int concat_left;			/* if true, concatenate matrices
					   on left instead of on right */
    int axis_override;			/* if not equal to VP_NO_AXIS then
					   this is an axis number (e.g.
					   VP_X_AXIS) that overrides the
					   computed value of best_view_axis */

    /* result image */
    void *image;			/* memory for image */
    int image_width;			/* image dimensions */
    int image_height;
    short image_bytes_per_scan;		/* size of scanline (with padding) */
    int pixel_type;			/* format for pixels (e.g. VP_RGB) */

    /* rendering parameters */
    double max_opacity;			/* high opacity threshold */
    short int_image_width_hint;		/* hint for size of int. image */
    short int_image_height_hint;
    short clamp_shade_table;		/* clamp shade table entries */

    /* min-max octree parameters */
    int param_maxrange[VP_MAX_FIELDS];	/* max. range of values allowed in an
					   mm_octree node w/o subdividing it */

    /* preclassified volume parameters */
    short rle_bytes_per_voxel;		/* bytes per voxel in RLEVoxels */
    int skip_rle_x;			/* if true, don't compute rle_x */
    int skip_rle_y;			/* if true, don't compute rle_y */
    int skip_rle_z;			/* if true, don't compute rle_z */

    /* parameters for rendering shadows */
    short enable_shadows;		/* if true, enable shadows */
    short shadow_light_num;		/* light source id (VP_LIGHT#) for
					   the light producing the shadows */
    short shadow_width_hint;		/* hint for width of shadow buffer */
    short shadow_height_hint;		/* hint for height of shadow buffer */
    float *shadow_color_table;		/* lookup table for LOOKUP_SHADER */
    int shadow_color_table_size;	/* size of shade table in bytes */
    int shadow_bias;			/* shadow bias distance */

    /* *** USER-SPECIFIED PARAMETERS THAT ARE NOT STORED IN CONTEXT FILE *** */

    /* dummy variable marking the boundary between state variables stored
       in volpack context files and those that are not; the value
       of this variable is never used or set */
    int end_of_parameters;

    /* callback functions */
    void (*shade_func)();		/* shade a voxel */
    int (*write_func) ANSI_ARGS((int, void *, unsigned)); /* write to file */
    int (*read_func) ANSI_ARGS((int, void *, unsigned));  /* read from file */
    void *(*mmap_func) ANSI_ARGS((int, unsigned, void *));/* memory map file */
    void (*log_alloc_func) ANSI_ARGS((void *,int,char *,int,char *,void *));
    					/* log memory allocation */
    void (*log_free_func) ANSI_ARGS((void *, void *));
					/* log memory deallocation */
    void (*status_func) ANSI_ARGS((double, void *)); /* give progress report */

    /* client data */
    void *client_data;			/* hook for client data structure */

    /* resampling filter for vpResample */
    int filter_num_taps;		/* number of filter taps */
    int filter_num_phases;		/* number of filter phases */
    float *filter_weights;		/* table of filter weights
					   (weights[num_phases][num_taps]) */

    /* *** INTERNAL DATA STRUCTURES *** */

    /* data structures for fast classification */
    MinMaxOctree *mm_octree;		/* min-max octree version of volume */
    unsigned *sum_table;		/* summed area table */
    int sum_table_size;			/* size of summed area table (bytes) */

    /* classified, run-length encoded volume data */
    RLEVoxels *rle_x;			/* RLE data for X viewing axis */
    RLEVoxels *rle_y;			/* RLE data for Y viewing axis */
    RLEVoxels *rle_z;			/* RLE data for Z viewing axis */
    ConstructionBuffer *cbuf;		/* buffer for constructing volume */

    /* factored viewing parameters */
    short factored_view_ready;		/* true if factored view is current */
    short affine_view;			/* true for affine transformation */
    short best_view_axis;		/* principal viewing axis */
    short reverse_slice_order;		/* compositing direction */
    short intermediate_width;		/* size of intermediate image */
    short intermediate_height;
    double shear_i;			/* shear coefficients */
    double shear_j;
    double trans_i;			/* translation coefficients */
    double trans_j;
    vpMatrix3 warp_2d;			/* final 2D transformation */
    double depth_000;			/* depth of first voxel in volume */
    double depth_di;			/* change in depth per unit i */
    double depth_dj;			/* change in depth per unit j */
    double depth_dk;			/* change in depth per unit k */

    /* work buffers for rendering */
    short max_intermediate_width;	/* size of intermediate image buffer */
    short max_intermediate_height;
    short max_scan_length;		/* size of scanline buffers */
    short intermediate_color_channels;	/* color channels in int. image */
    short pad_int_to_maxwidth;		/* pad intermediate image scanlines */
    union {				/* intermediate image (2D array) */
	GrayIntPixel *gray_intim;
	RGBIntPixel *rgb_intim;
    } int_image;

    /* parameters and data structures for shadows */
    double shadow_shear_i;		/* shear coeffs for light vector */
    double shadow_shear_j;
    double shadow_trans_i;		/* trans. coeffs for light vector */
    double shadow_trans_j;
    short shadow_width;			/* size of image in shadow buffer */
    short shadow_height;
    short max_shadow_width;		/* size of shadow buffer */
    short max_shadow_height;
    short pad_shadow_to_maxwidth;	/* pad shadow buffer scanlines */
    GrayIntPixel *shadow_buffer;	/* 2D shadow buffer */

    /* depth cueing */
    float *dc_table;			/* depth cueing lookup table */
    int dc_table_len;			/* number of entries in dc_table */

    /* opacity correction */
    float affine_opac_correct[VP_OPACITY_MAX+1];
    					/* maps opacity (0-255) to opacity
					   corrected for view (0.0-1.0) */
    float shadow_opac_correct[VP_OPACITY_MAX+1];
					/* opacity correction for shadow
					   buffer calculation */

    /* debugging info */
    vpResult error_code;		/* result code from first invalid
					   command since last call to
					   vpGetError() */
#ifdef DEBUG
    short debug_enable[VPDEBUG_COUNT];	/* flags to enable messages */
    int trace_u, trace_v;		/* intermediate image pixel to trace */
    int trace_shadow_k;			/* slice of interest for tracing
					   shadow opacity values */
#endif

#ifdef USE_TIMER
    unsigned timer_ticks[VPTIMER_COUNT];/* timer values in ticks */
    double timer_usec_per_tick;		/* microseconds per tick */
#ifdef HAVE_HIRES_TIMER
    volatile unsigned *timer_addr;	/* address of the timer */
    unsigned dummy_timer;		/* dummy location */
#endif
#endif

};

/*******************************************************************
 * Function prototypes.                                            *
 *******************************************************************/

/* vp_context.c */

/* vp_check.c */
extern vpResult		    VPCheckVoxelFields ANSI_ARGS((vpContext *vpc));
extern vpResult		    VPCheckRawVolume ANSI_ARGS((vpContext *vpc));
extern vpResult		    VPCheckClassifiedVolume ANSI_ARGS((
				vpContext *vpc, int axis));
extern vpResult		    VPCheckClassifier ANSI_ARGS((vpContext *vpc));
extern vpResult		    VPCheckShader ANSI_ARGS((vpContext *vpc));
extern vpResult		    VPCheckImage ANSI_ARGS((vpContext *vpc));
extern vpResult		    VPCheckShadows ANSI_ARGS((vpContext *vpc));
extern vpResult		    VPSetError ANSI_ARGS((vpContext *vpc,
				vpResult code));

/* vp_extract.c */
extern vpResult		    VPClassifyBlock ANSI_ARGS((vpContext *vpc,
				int correct, int x0, int y0, int z0,
				int x1, int y1, int z1, float *opc,
				int dst_xstride, int dst_ystride,
				int dst_zstride));
extern float		    VPClassifyVoxel ANSI_ARGS((vpContext *vpc,
				void *voxel));
extern vpResult		    VPShadeBlock ANSI_ARGS((vpContext *vpc,
				int x0, int y0, int z0, int x1, int y1, int z1,
				float *shd, int dst_xstride, int dst_ystride,
				int dst_zstride));
extern void		    VPQuantize ANSI_ARGS((float *src, int xlen,
				int ylen, int zlen, double scale, int maxvalue,
				unsigned char *dst, int dst_xstride,
				int dst_ystride, int dst_zstride));

/* vp_linalg.c */
extern void		    VPLoadTranslation ANSI_ARGS((vpMatrix4 m,
				double tx, double ty, double tz));
extern void		    VPLoadRotation ANSI_ARGS((vpMatrix4 m,
				int axis, double degrees));
extern void		    VPLoadScale ANSI_ARGS((vpMatrix4 m,
				double sx, double sy, double sz));

/* vp_view.c */
extern vpResult		    VPFactorView ANSI_ARGS((vpContext *vpc));
extern void		    VPComputeViewTransform ANSI_ARGS((vpContext *vpc,
				vpMatrix4 vm));
extern void		    VPResizeRenderBuffers ANSI_ARGS((vpContext *vpc,
				int max_width, int max_height, int max_scan));
extern void		    VPResizeDepthCueTable ANSI_ARGS((vpContext *vpc,
				int entries, int copy));
extern void		    VPComputeDepthCueTable ANSI_ARGS((vpContext *vpc,
				int first, int last));
extern float		    VPSliceDepthCueRatio ANSI_ARGS((vpContext *vpc));
extern void		    VPDepthCueIntImage ANSI_ARGS((vpContext *vpc,
				int slicenum));
extern void		    VPResizeShadowBuffer ANSI_ARGS((vpContext *vpc,
				int max_width, int max_height));

/* vp_util.c */
extern void		    VPBug ANSI_ARGS((char *fmt, ...));
#ifdef DEBUG
extern void		    VPDebug ANSI_ARGS((vpContext *vpc, int debug_code,
				char *fmt, ...));
#endif /* DEBUG */

/* vp_rle.c */
extern RLEVoxels *	    VPCreateRLEVoxels ANSI_ARGS((vpContext *vpc,
				int ilen, int jlen, int klen, int data_count,
				int run_count, int rle_bytes_per_voxel));
extern void		    VPDestroyRLEVoxels ANSI_ARGS((vpContext *vpc,
				RLEVoxels *rle_voxels));
#ifdef INDEX_VOLUME
extern vpResult		    VPComputeRLEScanOffsets ANSI_ARGS((
				vpContext *vpc));
#endif
#ifdef DEBUG
extern void		    VPCheckScanOffsets ANSI_ARGS((
				RLEVoxels *rle_voxels,
				int rle_bytes_per_voxel));
extern void		    VPValidateClassifiedVolume ANSI_ARGS((
				vpContext *vpc));
extern void		    VPDumpView ANSI_ARGS((vpContext *vpc));
extern void		    VPDumpClassifier ANSI_ARGS((vpContext *vpc));
#endif


/* vp_renderA.c */
extern void		    VPRenderAffine ANSI_ARGS((vpContext *vpc,
				int algorithm, void (*composite_func)()));

/* vp_octree.c */
extern void		    VPComputeSummedAreaTable ANSI_ARGS((
				vpContext *vpc));
extern void		    VPClassifyOctree ANSI_ARGS((vpContext *vpc));
extern void		    VPInitOctreeLevelStack ANSI_ARGS((vpContext *vpc,
			       MMOctreeLevel level_stack[VP_MAX_OCTREE_LEVELS],
			       int axis, int k));
extern int		    VPComputeScanRuns ANSI_ARGS((vpContext *vpc,
			       MMOctreeLevel level_stack[VP_MAX_OCTREE_LEVELS],
			       unsigned char *run_lengths, int axis,
			       int j, int icount));
extern int		    VPCheckRuns ANSI_ARGS((vpContext *vpc,
				unsigned char *run_lengths, int axis,
				int k, int j));
extern void		    VPTestMinMaxOctree ANSI_ARGS((vpContext *vpc));

/* vp_warp.c */
extern void		    VPComputeWarpTables ANSI_ARGS((void));
extern void		    VPAffineImageOverlap ANSI_ARGS((int in_width,
				int in_height, int out_width, int out_height,
				vpMatrix3 warp_matrix, double filter_width,
				Trapezoid full_overlap[9],
				Trapezoid part_overlap[9]));
