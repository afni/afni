/*
 * volpack.h
 *
 * Header file for VolPack.
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

#ifndef _VOLPACK
#define _VOLPACK

/*******************************************************************
 * Definitions to customize declarations for different C dialects. *
 *******************************************************************/

#define ANSI_C   /* RWCox */

#ifdef __STDC__
#ifndef NO_PROTOTYPE
#define ANSI_C
#endif
#endif

#ifdef __cplusplus
#define ANSI_C
#define EXTERN extern "C"
#else
#define EXTERN extern
#endif

#ifdef ANSI_C
#define ANSI_ARGS(x)    x
#else
#define ANSI_ARGS(x)    ()
#endif

/*******************************************************************
 * Configuration constants.                                        *
 *******************************************************************/

#define VP_EPS	1.0e-11		/* smallest reasonable nonzero number */
#define VP_MAX_VOLUME_DIM 1024	/* maximum length of a voxel scanline */
#define VP_MAX_FIELDS	6	/* maximum number of fields in a voxel */
#define VP_MAX_MATERIAL	6	/* maximum number of materials in a volume */
#define VP_MAX_LIGHTS	6	/* maximum number of lights */

/* magic numbers for disk files */
#define VP_CVFILE_MAGIC		0x4F4F5F5F /* classified volume file */
#define VP_OCTFILE_MAGIC	0x4F4F6F6F /* min-max octree file */
#define VP_RVFILE_MAGIC		0x4F4F7F7F /* raw volume file */
#define VP_VPCFILE_MAGIC	0x4F4F8F8F /* rendering context dump */

#define VP_VERSION		"1.1"	/* version string */
#define VP_MAJOR_VERSION	1	/* major version number */
#define VP_MINOR_VERSION	1	/* minor version number */

/*******************************************************************
 * Global constants.                                               *
 *******************************************************************/

/* options for vpGet* and vpSet* */
#define VP_XLEN			1000	/* x dimension of volume */
#define VP_YLEN			1001	/* y dimension of volume */
#define VP_ZLEN			1002	/* z dimension of volume */
#define VP_BYTES_PER_VOXEL	1003	/* size of raw voxel in bytes */
#define VP_VOXEL_FIELD_COUNT	1004	/* number of fields in a voxel */
#define VP_SHADE_FIELD_COUNT	1005	/* number of fields for shading */
#define VP_FIELD_SIZES		1006	/* size of each field */
#define VP_FIELD_OFFSETS	1007	/* byte offsets for each field */
#define VP_FIELD_MAXES		1008	/* max. value for each field */
#define VP_VOXEL_DATA		1009	/* pointer to voxel data */
#define VP_VOXEL_DATA_SIZE	1010	/* size of voxel data in bytes */
#define VP_VOXEL_XSTRIDE	1011	/* voxel data strides */
#define VP_VOXEL_YSTRIDE	1012	/* voxel data strides */
#define VP_VOXEL_ZSTRIDE	1013	/* voxel data strides */
#define VP_MIN_VOXEL_OPACITY	1014	/* thresh. for classification */
#define VP_CLASSIFY_FIELD_COUNT	1015	/* number of fields for classifier */
#define VP_CLASSIFY_FIELDS	1016	/* classification field numbers */
#define VP_CLASSIFY_TABLES	1017	/* classification tables */
#define VP_CLASSIFY_TABLE_SIZES	1018	/* classification table sizes */
#define VP_COLOR_CHANNELS	1019	/* number of color channels */
#define VP_SHADE_FUNC		1020	/* shading callback */
#define VP_GRAY_SHADE_FUNC	1021	/* grayscale shading function */
#define VP_RGB_SHADE_FUNC	1022	/* RGB shading function */
#define VP_SHADE_COLOR_TABLE	1023	/* shading color lookup table */
#define VP_SHADE_COLOR_SIZE	1024	/* size of shading color table */
#define VP_SHADE_WEIGHT_TABLE	1025	/* shading weight lookup table */
#define VP_SHADE_WEIGHT_SIZE	1026	/* size of shading weight table */
#define VP_SHADE_COLOR_FIELD	1027	/* field for color table index */
#define VP_SHADE_WEIGHT_FIELD	1028	/* field for weight table index */
#define VP_LIGHT_BOTH_SIDES	1029	/* use two-sided lighting */
#define VP_REVERSE_SURFACE_SIDES 1030	/* reverse interior and exterior */
#define VP_DEPTH_CUE		1031	/* depth cueing */
#define VP_DEPTH_CUE_FRONT	1032	/* depth cueing front factor */
#define VP_DEPTH_CUE_DENSITY	1034	/* depth cueing density */
#define VP_DEPTH_CUE_TABLE_SIZE	1035	/* # of entries in depth-cue table */
#define VP_DEPTH_CUE_SIZE_HINT	1036	/* hint for # of entries */
#define VP_DEPTH_CUE_QUANTIZATION 1037	/* smallest resolvable depth */
#define VP_CONCAT_MODE		1038	/* VP_CONCAT_LEFT or VP_CONCAT_RIGHT */
#define VP_IMAGE		1039	/* image array */
#define VP_IMAGE_WIDTH		1040	/* image width */
#define VP_IMAGE_HEIGHT		1041	/* image height */
#define VP_IMAGE_SCAN_SIZE	1042	/* bytes per scan in image */
#define VP_MAX_RAY_OPACITY	1043	/* thresh. for early ray termination */
#define VP_VIEW_AXIS		1044	/* principal viewing axis */
#define VP_INTERMEDIATE_WIDTH	1045	/* width of intermediate image */
#define VP_INTERMEDIATE_HEIGHT	1046	/* height of intermediate image */
#define VP_INTERMEDIATE_COLOR	1047	/* color channels in int. image */
#define VP_INT_WIDTH_HINT	1048	/* hint for intermediate image */
#define VP_INT_HEIGHT_HINT	1049	/* hint for intermediate height */
#define VP_VIEW_X_AXIS		1050	/* enable X viewing axis */
#define VP_VIEW_Y_AXIS		1051	/* enable Y viewing axis */
#define VP_VIEW_Z_AXIS		1052	/* enable Z viewing axis */
#define VP_VIEW_X_SIZE		1053	/* size of X view data */
#define VP_VIEW_Y_SIZE		1054	/* size of Y view data */
#define VP_VIEW_Z_SIZE		1055	/* size of Z view data */
#define VP_MMOCTREE_THRESHOLDS	1056	/* thresholds for min-max octree */
#define VP_MMOCTREE_SIZE	1057	/* size of min-max octree */
#define VP_LOG_ALLOC_FUNC	1058	/* function to log allocations */
#define VP_LOG_FREE_FUNC	1059	/* function to log deallocations */
#define VP_STATUS_FUNC		1060	/* function to give progress reports */
#define VP_READ_FUNC		1061	/* function to read from file */
#define VP_WRITE_FUNC		1062	/* function to write to file */
#define VP_MMAP_FUNC		1063	/* function to memory map a file */
#define VP_CLIENT_FUNC		1064	/* client data */
#define VP_MATERIAL_COUNT	1065	/* number of materials */
#define VP_CURRENT_MATRIX	1066	/* current transformation matrix */
#define VP_CLIENT_DATA		1067	/* client data */
#define VP_SHADOW		1068	/* enable shadows */
#define VP_SHADOW_LIGHT		1069	/* light number for shadows */
#define VP_SHADOW_WIDTH_HINT	1070	/* hint for width of shadow buffer */
#define VP_SHADOW_HEIGHT_HINT	1071	/* hint for height of shadow buffer */
#define VP_SHADOW_WIDTH		1072	/* width of shadow image */
#define VP_SHADOW_HEIGHT	1073	/* height of shadow image */
#define VP_SHADOW_COLOR_TABLE	1074	/* shadow color lookup table */
#define VP_SHADOW_COLOR_SIZE	1075	/* size of shadow color table */
#define VP_SHADOW_BIAS		1076	/* shadow bias distance */
#define VP_PIXEL_TYPE		1077	/* image pixel type */
#define VP_CLAMP_SHADE_TABLE	1078	/* clamp shade table entries */
#define VP_COMPOSITE_ORDER	1079	/* slice compositing order */
#define VP_AXIS_OVERRIDE	1080	/* override for best_view_axis */
#define VP_TRACE_SHADOW_K	1081	/* slice number for shadow tracing */

/* light numbers for vpSetLight */
#define VP_LIGHT0		2000
#define VP_LIGHT1		2001
#define VP_LIGHT2		2002
#define VP_LIGHT3		2003
#define VP_LIGHT4		2004
#define VP_LIGHT5		2005

/* property codes for vpSetLight */
#define VP_COLOR		2100	/* light color */
#define VP_DIRECTION		2101	/* light direction */

/* material numbers for vpSetMaterial */
#define VP_MATERIAL0		2200
#define VP_MATERIAL1		2201
#define VP_MATERIAL2		2202
#define VP_MATERIAL3		2203
#define VP_MATERIAL4		2204
#define VP_MATERIAL5		2205

/* property codes for vpSetMaterial */
#define VP_AMBIENT		2300	/* ambient material coefficients */
#define VP_DIFFUSE		2301	/* diffuse material coefficients */
#define VP_SPECULAR		2302	/* specular material coefficients */
#define VP_SHINYNESS		2303	/* specular exponent */


/* projection types for vpWindow and vpWindowPHIGS */
#define VP_PARALLEL		2400	/* parallel projection */
#define VP_PERSPECTIVE		2401	/* perspective projection */

/* volume type codes for vpExtract */
#define VP_RAW_VOLUME		2500	/* unclassified volume */
#define VP_CLASSIFIED_VOLUME    2501	/* classified volume, optimal view */
#define VP_CLX_VOLUME		2502	/* classified volume, X view */
#define VP_CLY_VOLUME		2503	/* classified volume, Y view */
#define VP_CLZ_VOLUME		2504	/* classified volume, Z view */

/* matrix concatenation modes for vpSeti(VP_CONCAT_MODE) */
#define VP_CONCAT_RIGHT		2600	/* concatenate matrices on right */
#define VP_CONCAT_LEFT		2601	/* concatenate matrices on left */

/* surface side codes for vpSetMaterial (these are bit fields) */
#define VP_EXTERIOR		1
#define VP_INTERIOR		2
#define VP_BOTH_SIDES		(VP_EXTERIOR | VP_INTERIOR)

/* principal axes (used as array indexes) */
#define VP_X_AXIS		0
#define VP_Y_AXIS		1
#define VP_Z_AXIS		2
#define VP_NO_AXIS		-1

/* transformation matrices (used as array indexes) */
#define VP_MODEL		0   /* modelling transform (object -> world) */
#define VP_VIEW			1   /* viewing transform (world -> eye) */
#define VP_PROJECT		2   /* projection transform (eye -> clip) */
#define VP_SCREEN		8   /* screen transform (object -> screen) */

/* special field type codes */
#define VP_SKIP_FIELD		-1	/* ignore a field */
#define VP_OPACITY_FIELD	-2	/* compute opacity */
#define VP_CORRECTED_OPAC_FIELD	-3	/* compute opac. corrected for view */
#define VP_COLOR_FIELD		-4	/* compute color */

/* buffer codes */
#define VP_IMAGE_BUFFER		4000	/* intermediate image buffer */
#define VP_SHADOW_BUFFER	4001	/* shadow buffer */

/* pixel formats */
#define VP_ALPHA		5000	/* opacity */
#define VP_LUMINANCE		5001	/* grayscale color */
#define VP_LUMINANCEA		5002	/* grayscale color + opacity */
#define VP_RGB			5003	/* RGB color */
#define VP_RGBA			5004	/* RGB color + opacity */
#define VP_BGR			5005	/* RGB color, reverse byte order */
#define VP_ABGR			5006	/* RGB color + opacity, reverse order*/

/* voxel fields computed by vpScanlineNormals */
#define VP_NORM_SIZE		2	/* 2 byte normal index */
#define VP_SCALAR_SIZE		1	/* 1 byte scalar value */
#define VP_GRAD_SIZE		1	/* 1 byte gradient magnitude */

#define VP_NORM_MAX		7923	/* maximum value of a 13 bit normal */
#define VP_SCALAR_MAX		255	/* maximum value of a scalar */
#define VP_GRAD_MAX		221	/* maximum value of a gradient */

#define VP_OPACITY_MAX		255	/* maximum value of an opacity */

/* data types */
#define VP_UCHAR		1200	/* unsigned char */
#define VP_USHORT		1201	/* unsigned short */
#define VP_FLOAT		1202	/* float */

/* filter types */
#define VP_BOX_FILTER		1300	/* box filter */
#define VP_LINEAR_FILTER	1301	/* linear filter (triangle) */
#define VP_GAUSSIAN_FILTER	1302	/* gaussian, sigma defined below */
#define VP_BSPLINE_FILTER	1303	/* cubic bspline filter */
#define VP_MITCHELL_FILTER	1304	/* Mitchell bicubic filter */

#define VP_GAUSSIAN_SIGMA	0.4

/*******************************************************************
 * Macros.                                                         *
 *******************************************************************/

/* set fields of a vector */
#define vpSetVector3(v, v0, v1, v2)	v[0]=v0; v[1]=v1; v[2]=v2
#define vpSetVector4(v, v0, v1, v2, v3)	v[0]=v0; v[1]=v1; v[2]=v2; v[3]=v3

/* compute an offset to a field in a structure */
#define vpFieldOffset(ptr, field)	((char *)&(ptr)->field - (char *)(ptr))

/*******************************************************************
 * Data type definitions.                                          *
 *******************************************************************/

typedef unsigned vpResult;		/* result code */
typedef double vpVector3[3];		/* 3 element vector */
typedef double vpVector4[4];		/* 4 element vector */
typedef double vpMatrix3[3][3];		/* 3 by 3 element matrix */
typedef double vpMatrix4[4][4];		/* 4 by 4 element matrix */

typedef struct _vp_context vpContext;

/*******************************************************************
 * Debugging codes.                                                *
 *******************************************************************/

#define VPDEBUG_VIEW		0	/* view transform calculations */
#define VPDEBUG_RENDER		1	/* high-level rendering stages */
#define VPDEBUG_RBUF		2	/* render buffer allocation */
#define VPDEBUG_OPCCORRECT	3	/* opacity correction */
#define VPDEBUG_DEPTHCUE	4	/* depth cueing */
#define VPDEBUG_PYRAMID		5	/* pyramid construction */
#define VPDEBUG_OCTREE		6	/* octree construction */
#define VPDEBUG_CLSFYOCTREE	7	/* octree classification */
#define VPDEBUG_OCTREERUNS	8	/* runs computed from octree */
#define VPDEBUG_OCTREETRAVERSE	9	/* octree traversal */
#define VPDEBUG_TRANSPOSE	10	/* volume transposing */
#define VPDEBUG_COMPOSITE	11	/* compositing */
#define VPDEBUG_SHADOW		12	/* shadows */
#define VPDEBUG_COUNT		13      /* total number of codes */

/*******************************************************************
 * Timer codes.                                                    *
 *******************************************************************/

#define VPTIMER_SHADE		0	/* compute shading lookup table */
#define VPTIMER_COMPOSITE	1	/* compositing loop */
#define VPTIMER_DEPTHCUE	2	/* depth cueing fixup loop */
#define VPTIMER_WARP		3	/* 2D warp */
#define VPTIMER_TRAVERSE_RUNS	4	/* traverse runs during compositing */
#define VPTIMER_PROCESS_VOXELS	5	/* process voxels during compositing */
#define VPTIMER_ERT		6	/* early ray termination overhead */
#define VPTIMER_CLSFY_OCTREE	7	/* classify octree nodes */
#define VPTIMER_TRAVERSE_OCTREE	8	/* traverse octree nodes */
#define VPTIMER_RENDER		9	/* render */
#define VPTIMER_CLEAR	       10	/* clear intermediate image */
#define VPTIMER_COUNT	       11	/* total number of codes */

/*******************************************************************
 * Error codes.                                                    *
 *******************************************************************/

#define VP_OK	0			/* successful return */
#define VPERROR_FIRST		1000
#define VPERROR_LIMIT_EXCEEDED	1000	/* exceeded a built-in limit */
#define VPERROR_SINGULAR	1001	/* singular vector or matrix */
#define VPERROR_IO		1002	/* file I/O error */
#define VPERROR_BAD_SIZE	1003	/* invalid buffer size */
#define VPERROR_BAD_IMAGE	1004	/* invalid image definition */
#define VPERROR_BAD_SHADER	1005	/* invalid shader definition */
#define VPERROR_BAD_CLASSIFIER	1006	/* invalid classifier definition */
#define VPERROR_BAD_VOLUME	1007	/* invalid volume definition */
#define VPERROR_BAD_VOXEL	1008	/* invalid voxel definition */
#define VPERROR_BAD_OPTION	1009	/* invalid option code */
#define VPERROR_BAD_VALUE	1010	/* argument out of range */
#define VPERROR_BAD_FILE	1011	/* file has bad magic number */
#define VPERROR_BAD_SHADOW	1012	/* cannot compute shadow buffer */
#define VPERROR_LAST		1012

/*******************************************************************
 * Global variables.                                               *
 *******************************************************************/

EXTERN char *vpCompilerOptions;

/*******************************************************************
 * Function declarations.                                          *
 *******************************************************************/

/* contexts */
EXTERN vpContext *	    vpCreateContext ANSI_ARGS((void));
EXTERN void		    vpDestroyContext ANSI_ARGS((vpContext *vpc));

/* volumes */
EXTERN vpResult		    vpSetVolumeSize ANSI_ARGS((vpContext *vpc,
				int xlen, int ylen, int zlen));
EXTERN vpResult		    vpSetVoxelSize ANSI_ARGS((vpContext *vpc,
				int bytes_per_voxel, int num_voxel_fields,
				int num_shade_fields, int num_clsfy_fields));
EXTERN vpResult		    vpSetVoxelField ANSI_ARGS((vpContext *vpc,
				int field_num, int field_size,
				int field_offset, int field_max));
EXTERN vpResult		    vpSetRawVoxels ANSI_ARGS((vpContext *vpc,
				void *raw_voxels, int raw_voxels_size,
				int xstride, int ystride, int zstride));

/* classification */
EXTERN vpResult		    vpSetClassifierTable ANSI_ARGS((vpContext *vpc,
				int param_num, int param_field, float *table,
				int table_size));
EXTERN vpResult		    vpClassifyScalars ANSI_ARGS((vpContext *vpc,
			        unsigned char *scalar_data, int length,
				int scalar_field, int grad_field,
				int norm_field));
EXTERN vpResult		    vpClassifyVolume ANSI_ARGS((vpContext *vpc));
EXTERN vpResult		    vpClassifyScanline ANSI_ARGS((vpContext *vpc,
				void *voxels));
EXTERN vpResult		    vpDestroyClassifiedVolume ANSI_ARGS((
				vpContext *vpc));
EXTERN vpResult		    vpMinMaxOctreeThreshold ANSI_ARGS((vpContext *vpc,
				int param, int range));
EXTERN vpResult		    vpCreateMinMaxOctree ANSI_ARGS((vpContext *vpc,
				int root_node_size, int base_node_size));
EXTERN vpResult		    vpDestroyMinMaxOctree ANSI_ARGS((vpContext *vpc));
EXTERN vpResult		    vpOctreeMask ANSI_ARGS((vpContext *vpc,
				unsigned char *array, int array_size,
				int max_level));

/* shading */
EXTERN vpResult		    vpSetLookupShader ANSI_ARGS((vpContext *vpc,
				int color_channels, int num_materials,
				int color_field, float *color_table,
				int color_table_size, int weight_field,
				float *weight_table, int weight_table_size));
EXTERN vpResult		    vpSetShadowLookupShader ANSI_ARGS((vpContext *vpc,
				int color_channels, int num_materials,
				int color_field, float *color_table,
				int color_table_size, int weight_field,
				float *weight_table, int weight_table_size,
				float *shadow_table, int shadow_table_size));
EXTERN vpResult		    vpSetMaterial ANSI_ARGS((vpContext *vpc,
				int material, int property, int surface_side,
				double r, double g, double b));
EXTERN vpResult		    vpSetLight ANSI_ARGS((vpContext *vpc,
				int light_num, int property, double n0,
				double n1, double n2));
EXTERN vpResult		    vpSetDepthCueing ANSI_ARGS((vpContext *vpc,
				double front_factor, double density));
EXTERN int		    vpNormalIndex ANSI_ARGS((double nx, double ny,
				double nz));
EXTERN vpResult		    vpNormal ANSI_ARGS((int n, double *nx, double *ny,
				double *nz));
EXTERN vpResult		    vpScanlineNormals ANSI_ARGS((vpContext *vpc,
				int length, unsigned char *scalar_data,
				unsigned char *scalar_minus_y,
				unsigned char *scalar_plus_y,
				unsigned char *scalar_minus_z,
				unsigned char *scalar_plus_z,
				void *voxel_data, int scalar_field,
				int grad_field, int norm_field));
EXTERN vpResult		    vpVolumeNormals ANSI_ARGS((vpContext *vpc,
				unsigned char *scalar_data, int length,
				int scalar_field, int grad_field,
				int norm_field));
EXTERN vpResult		    vpShadeTable ANSI_ARGS((vpContext *vpc));

/* view */
EXTERN vpResult		    vpCurrentMatrix ANSI_ARGS((vpContext *vpc,
				int option));
EXTERN vpResult		    vpIdentityMatrix ANSI_ARGS((vpContext *vpc));
EXTERN vpResult		    vpSetMatrix ANSI_ARGS((vpContext *vpc,
				vpMatrix4 matrix));
EXTERN vpResult		    vpMultMatrix ANSI_ARGS((vpContext *vpc,
				vpMatrix4 matrix));
EXTERN vpResult		    vpTranslate ANSI_ARGS((vpContext *vpc,
				double tx, double ty, double tz));
EXTERN vpResult		    vpRotate ANSI_ARGS((vpContext *vpc,
				int axis, double degrees));
EXTERN vpResult		    vpScale ANSI_ARGS((vpContext *vpc,
				double sx, double sy, double sz));
EXTERN vpResult		    vpWindow ANSI_ARGS((vpContext *vpc,
				int type, double left, double right,
				double bottom, double top, double near,
				double far));
EXTERN vpResult		    vpWindowPHIGS ANSI_ARGS((vpContext *vpc,
				vpVector3 vrp, vpVector3 vpn, vpVector3 vup,
				vpVector3 prp, double viewport_umin,
				double viewport_umax, double viewport_vmin,
				double viewport_vmax, double viewport_front,
				double viewport_back, int projection_type));

/* images */
EXTERN vpResult		    vpSetImage ANSI_ARGS((vpContext *vpc,
				unsigned char *image, int width, int height,
				int bytes_per_scan, int pixel_type));

/* other options */
EXTERN vpResult		    vpEnable ANSI_ARGS((vpContext *vpc,
				int option, int value));
EXTERN vpResult		    vpSeti ANSI_ARGS((vpContext *vpc, int option,
				int value));
EXTERN vpResult		    vpSetd ANSI_ARGS((vpContext *vpc, int option,
				double value));
EXTERN vpResult		    vpSetCallback ANSI_ARGS((vpContext *vpc,
				int option, void *func));
EXTERN vpResult		    vpSetClientData ANSI_ARGS((vpContext *vpc,
				void *client_data));
EXTERN vpResult		    vpSetDebug ANSI_ARGS((vpContext *vpc, int flag,
				int value));
EXTERN vpResult		    vpTracePixel ANSI_ARGS((vpContext *vpc,
				int trace_u, int trace_v));
EXTERN vpResult		    vpGetTimer ANSI_ARGS((vpContext *vpc, int option,
				int *iptr));
EXTERN vpResult		    vpClearTimer ANSI_ARGS((vpContext *vpc,
				int option));

/* rendering */
EXTERN vpResult		    vpRenderClassifiedVolume ANSI_ARGS((
				vpContext *vpc));
EXTERN vpResult		    vpRenderRawVolume ANSI_ARGS((vpContext *vpc));
EXTERN vpResult		    vpBruteForceRender ANSI_ARGS((vpContext *vpc));

/* retrieving state */
EXTERN vpResult		    vpGeti ANSI_ARGS((vpContext *vpc, int option,
				int *iptr));
EXTERN vpResult		    vpGetd ANSI_ARGS((vpContext *vpc, int option,
				double *dptr));
EXTERN vpResult		    vpGetp ANSI_ARGS((vpContext *vpc, int option,
				void **pptr));
EXTERN vpResult		    vpGetMatrix ANSI_ARGS((vpContext *vpc,
				int matrix_code, vpMatrix4 matrix));
EXTERN vpResult		    vpGetMaterial ANSI_ARGS((vpContext *vpc,
				int material, int property, int surface_side,
				double *r, double *g, double *b));
EXTERN vpResult		    vpGetLight ANSI_ARGS((vpContext *vpc,
				int light_num, int property,
				double *n0, double *n1, double *n2));
EXTERN vpResult		    vpGetImage ANSI_ARGS((vpContext *vpc,
				void *image, int width, int height,
				int scan_bytes, int pixel_type,
				int image_type));
EXTERN vpResult		    vpGetError ANSI_ARGS((vpContext *vpc));
EXTERN char *		    vpGetErrorString ANSI_ARGS((vpResult code));

/* linear algebra */
EXTERN void		    vpIdentity3 ANSI_ARGS((vpMatrix3 m));
EXTERN void		    vpIdentity4 ANSI_ARGS((vpMatrix4 m));
EXTERN vpResult		    vpNormalize3 ANSI_ARGS((vpVector3 v));
EXTERN void		    vpMatrixVectorMult4 ANSI_ARGS((vpVector4 v2,
				vpMatrix4 m, vpVector4 v1));
EXTERN void		    vpMatrixMult4 ANSI_ARGS((vpMatrix4 m3,
				vpMatrix4 m2, vpMatrix4 m1));
EXTERN void		    vpCrossProduct ANSI_ARGS((vpVector3 p,
				vpVector3 v, vpVector3 w));
EXTERN vpResult		    vpSolveSystem4 ANSI_ARGS((vpMatrix4 a, double **b,
				int m));

/* file I/O */
EXTERN vpResult		    vpStoreClassifiedVolume ANSI_ARGS((vpContext *vpc,
				int fd));
EXTERN vpResult		    vpLoadClassifiedVolume ANSI_ARGS((vpContext *vpc,
				int fd));
EXTERN vpResult		    vpStoreMinMaxOctree ANSI_ARGS((vpContext *vpc,
				int fd));
EXTERN vpResult		    vpLoadMinMaxOctree ANSI_ARGS((vpContext *vpc,
				int fd));
EXTERN vpResult		    vpStoreRawVolume ANSI_ARGS((vpContext *vpc,
				int fd));
EXTERN vpResult		    vpLoadRawVolume ANSI_ARGS((vpContext *vpc,
				int fd));
EXTERN vpResult		    vpStoreContext ANSI_ARGS((vpContext *vpc, int fd));
EXTERN vpResult		    vpLoadContext ANSI_ARGS((vpContext *vpc, int fd));

/* misc. utilities */
EXTERN vpResult		    vpExtract ANSI_ARGS((vpContext *vpc,
				int volume_type, int x0, int y0, int z0,
				int x1, int y1, int z1, int field, void *dst,
				int dst_size, int dst_xstride, int dst_ystride,
				int dst_zstride));
EXTERN vpResult		    vpRamp ANSI_ARGS((float *dst, int stride,
				int num_points, int *ramp_x, float *ramp_y));
EXTERN vpResult		    vpTranspose ANSI_ARGS((vpContext *vpc, int kaxis));
EXTERN vpResult		    vpSetFilter ANSI_ARGS((vpContext *vpc,
				int num_taps, int num_phases, float *weights));
EXTERN vpResult		    vpResample ANSI_ARGS((vpContext *vpc,
				int num_dimens, int *src_dimens,
				int *dst_dimens, int *src_strides,
				int *dst_strides, int element_type,
				void *in_array, void *out_array));
EXTERN vpResult		    vpResample2D ANSI_ARGS((
				void *in_array, int in_x, int in_y,
				void *out_array, int out_x, int out_y,
				int element_type, int filter_type));
EXTERN vpResult		    vpResample3D ANSI_ARGS((
				void *in_array, int in_x, int in_y, int in_z,
				void *out_array, int out_x, int out_y,
				int out_z, int element_type, int filter_type));
EXTERN vpResult		    vpBoxFilter ANSI_ARGS((int num_taps,
				int num_phases, float *weights,
				int weights_bytes));
EXTERN vpResult		    vpLinearFilter ANSI_ARGS((int num_taps,
				int num_phases, float *weights,
				int weights_bytes));
EXTERN vpResult		    vpBicubicFilter ANSI_ARGS((double b_value,
				double c_value, int num_taps, int num_phases,
				float *weights, int weights_bytes));
EXTERN vpResult		    vpGaussianFilter ANSI_ARGS((double sigma,
				int num_taps, int num_phases, float *weights,
				int weights_bytes));
#endif /* _VOLPACK */
