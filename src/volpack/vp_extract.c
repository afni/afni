/*
 * vp_extract.c
 *
 * Routines to extract fields from a volume.
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

static int ExtractRawVolume ANSI_ARGS((vpContext *vpc, int x0, int y0, int z0,
    int x1, int y1, int z1, int field, void *dst, int dst_xstride,
    int dst_ystride, int dst_zstride));
static int ClassifyRawVolume ANSI_ARGS((vpContext *vpc, int correct, 
    int x0, int y0, int z0, int x1, int y1, int z1, unsigned char *dst,
    int dst_xstride, int dst_ystride, int dst_zstride));
static int ShadeRawVolume ANSI_ARGS((vpContext *vpc, int x0, int y0, int z0,
    int x1, int y1, int z1, unsigned char *dst, int dst_xstride,
    int dst_ystride, int dst_zstride));
static float CorrectOpacity ANSI_ARGS((vpContext *vpc, int quant_opc,
    int x, int y, int z));
static void ShadeVoxel ANSI_ARGS((vpContext *vpc, void *voxel, int x,
    int y, int z, float *dst));
static int ExtractClassifiedVolume ANSI_ARGS((vpContext *vpc, int axis,
    int x0, int y0, int z0, int x1, int y1, int z1, int field, void *dst,
    int dst_xstride, int dst_ystride, int dst_zstride));

/*
 * vpExtract
 *
 * Extract a field from a volume.
 */

vpResult
vpExtract(vpc, volume_type, x0, y0, z0, x1, y1, z1, field, dst, dst_size,
	  dst_xstride, dst_ystride, dst_zstride)
vpContext *vpc;		/* context */
int volume_type;	/* which volume representation to extract from */
int x0, y0, z0;		/* origin of extracted region */
int x1, y1, z1;		/* opposite corner of extracted region */
int field;		/* field to extract */
void *dst;		/* buffer to store result into */
int dst_size;		/* size of dst in bytes */
int dst_xstride;	/* stride (in bytes) for destination array */
int dst_ystride;
int dst_zstride;
{
    int field_size;
    int xrange, yrange, zrange;
    int retcode;
    int axis;

    /* check for errors */
    if (x0 < 0 || y0 < 0 || z0 < 0 || x1 >= vpc->xlen || y1 >= vpc->ylen ||
	z1 >= vpc->zlen || x0 > x1 || y0 > y1 || z0 > z1)
	return(VPSetError(vpc, VPERROR_BAD_VALUE));
    if (field == VP_OPACITY_FIELD || field == VP_CORRECTED_OPAC_FIELD)
	field_size = 1;
    else if (field == VP_COLOR_FIELD)
	field_size = vpc->color_channels;
    else if (field < 0 || field >= vpc->num_voxel_fields)
	return(VPSetError(vpc, VPERROR_BAD_VALUE));
    else if (volume_type != VP_RAW_VOLUME && field >= vpc->num_shade_fields)
	return(VPSetError(vpc, VPERROR_BAD_VALUE));
    else
	field_size = vpc->field_size[field];
    if (dst == NULL || dst_size != field_size*(x1-x0+1)*(y1-y0+1)*(z1-z0+1))
	return(VPSetError(vpc, VPERROR_BAD_SIZE));

    /* choose axis */
    switch (volume_type) {
    case VP_CLASSIFIED_VOLUME:
	xrange = x1 - x0;
	yrange = y1 - y0;
	zrange = z1 - z0;
	if (vpc->rle_z != NULL && zrange < xrange && zrange < yrange)
	    axis = VP_Z_AXIS;
	else if (vpc->rle_x != NULL && xrange < yrange && xrange < zrange)
	    axis = VP_X_AXIS;
	else if (vpc->rle_z != NULL && yrange < zrange && yrange < xrange)
	    axis = VP_Y_AXIS;
	else if (vpc->rle_z != NULL && xrange >= yrange && xrange >= zrange)
	    axis = VP_Z_AXIS;
	else if (vpc->rle_x != NULL && yrange >= zrange)
	    axis = VP_X_AXIS;
	else if (vpc->rle_y != NULL)
	    axis = VP_Y_AXIS;
	else if (vpc->rle_z != NULL)
	    axis = VP_Z_AXIS;
	else if (vpc->rle_x != NULL)
	    axis = VP_X_AXIS;
	else
	    return(VPSetError(vpc, VPERROR_BAD_VOLUME));
	break;
    case VP_CLX_VOLUME:
	axis = VP_X_AXIS;
	break;
    case VP_CLY_VOLUME:
	axis = VP_Y_AXIS;
	break;
    case VP_CLZ_VOLUME:
	axis = VP_Z_AXIS;
	break;
    case VP_RAW_VOLUME:
	break;
    default:
	return(VPSetError(vpc, VPERROR_BAD_OPTION));
    }

    /* compute result */
    if (volume_type == VP_RAW_VOLUME) {
	if ((retcode = VPCheckRawVolume(vpc)) != VP_OK)
	    return(retcode);
	if (field == VP_OPACITY_FIELD)
	    return(ClassifyRawVolume(vpc, 0, x0, y0, z0, x1, y1, z1, dst,
				     dst_xstride, dst_ystride, dst_zstride));
	else if (field == VP_CORRECTED_OPAC_FIELD)
	    return(ClassifyRawVolume(vpc, 1, x0, y0, z0, x1, y1, z1, dst,
				     dst_xstride, dst_ystride, dst_zstride));
	else if (field == VP_COLOR_FIELD)
	    return(ShadeRawVolume(vpc, x0, y0, z0, x1, y1, z1, dst,
				  dst_xstride, dst_ystride, dst_zstride));
	else
	    return(ExtractRawVolume(vpc, x0, y0, z0, x1, y1, z1, field, dst,
				    dst_xstride, dst_ystride, dst_zstride));
    } else {
	if ((retcode = VPCheckClassifiedVolume(vpc, axis)) != VP_OK)
	    return(retcode);
	if (field == VP_COLOR_FIELD) {
	    return(VPSetError(vpc, VPERROR_BAD_VALUE));
	} else {
	    return(ExtractClassifiedVolume(vpc, axis, x0, y0, z0, x1, y1, z1,
			field, dst, dst_xstride, dst_ystride, dst_zstride));
	}
    }
}

/*
 * ExtractRawVolume
 *
 * Extract a field from a raw volume into an array.
 */

static int
ExtractRawVolume(vpc, x0, y0, z0, x1, y1, z1, field, dst,
		 dst_xstride, dst_ystride, dst_zstride)
vpContext *vpc;		/* context */
int x0, y0, z0;		/* origin of extracted region */
int x1, y1, z1;		/* opposite corner of extracted region */
int field;		/* field to extract */
void *dst;		/* buffer to store result into */
int dst_xstride;	/* stride (in bytes) for destination array */
int dst_ystride;
int dst_zstride;
{
    int x, y, z;
    unsigned char *voxel, *dstptr;
    int field_size;
    int field_offset;
    int xstride, ystride, zstride;
    int retcode;

    field_size = vpc->field_size[field];
    field_offset = vpc->field_offset[field];
    xstride = vpc->xstride;
    ystride = vpc->ystride;
    zstride = vpc->zstride;
    voxel = vpc->raw_voxels;
    voxel += x0*xstride + y0*ystride + z0*zstride;
    dstptr = dst;
    for (z = z0; z <= z1; z++) {
	for (y = y0; y <= y1; y++) {
	    for (x = x0; x <= x1; x++) {
		if (field_size == 1)
		    ByteField(dstptr, 0) = ByteField(voxel, field_offset);
		else if (field_size == 2)
		    ShortField(dstptr, 0) = ShortField(voxel, field_offset);
		else
		    IntField(dstptr, 0) = IntField(voxel, field_offset);
		dstptr += dst_xstride;
		voxel += xstride;
	    }
	    dstptr += dst_ystride - (x1-x0+1)*dst_xstride;
	    voxel += ystride - (x1-x0+1)*xstride;
	}
	dstptr += dst_zstride - (y1-y0+1)*dst_ystride;
	voxel += zstride - (y1-y0+1)*ystride;
    }
    return(VP_OK);
}

/*
 * ClassifyRawVolume
 *
 * Classify a portion of a raw volume, quantize the result, and store
 * as an array of 8-bit opacities.
 */

static int
ClassifyRawVolume(vpc, correct, x0, y0, z0, x1, y1, z1, dst,
		  dst_xstride, dst_ystride, dst_zstride)
vpContext *vpc;		/* context */
int correct;		/* if true then correct for view */
int x0, y0, z0;		/* origin of extracted region */
int x1, y1, z1;		/* opposite corner of extracted region */
unsigned char *dst;	/* buffer to store result into */
int dst_xstride;	/* stride (in bytes) for destination array */
int dst_ystride;
int dst_zstride;
{
    float *opc;
    int num_voxels;
    int retcode;

    /* check for errors */
    if ((retcode = VPCheckClassifier(vpc)) != VP_OK)
	return(retcode);

    /* compute opacities */
    num_voxels = (x1-x0+1)*(y1-y0+1)*(z1-z0+1);
    Alloc(vpc, opc, float *, num_voxels*sizeof(float), "opacity_block");
    VPClassifyBlock(vpc, correct, x0, y0, z0, x1, y1, z1, opc,
		    sizeof(float), (x1-x0+1)*sizeof(float),
		    (x1-x0+1)*(y1-y0+1)*sizeof(float));

    /* quantize opacities */
    VPQuantize(opc, x1-x0+1, y1-y0+1, z1-z0+1, 255., 255, dst,
	       dst_xstride, dst_ystride, dst_zstride);

    Dealloc(vpc, opc);
    return(VP_OK);
}

/*
 * ShadeRawVolume
 *
 * Shade a portion of a raw volume, quantize the result, and store
 * as an array of 8-bit intensities.
 */

static int
ShadeRawVolume(vpc, x0, y0, z0, x1, y1, z1, dst,
	       dst_xstride, dst_ystride, dst_zstride)
vpContext *vpc;		/* context */
int x0, y0, z0;		/* origin of extracted region */
int x1, y1, z1;		/* opposite corner of extracted region */
unsigned char *dst;	/* buffer to store result into */
int dst_xstride;	/* stride (in bytes) for destination array */
int dst_ystride;
int dst_zstride;
{
    float *shd;
    int num_colors;
    int retcode;
    int xstride, ystride, zstride;

    /* check for errors */
    if ((retcode = VPCheckShader(vpc)) != VP_OK)
	return(retcode);

    /* compute colors */
    num_colors = (x1-x0+1)*(y1-y0+1)*(z1-z0+1)*vpc->color_channels;
    Alloc(vpc, shd, float *, num_colors*sizeof(float), "color_block");
    xstride = vpc->color_channels * sizeof(float);
    ystride = xstride * (x1-x0+1);
    zstride = ystride * (y1-y0+1);
    VPShadeBlock(vpc, x0, y0, z0, x1, y1, z1, shd, xstride, ystride, zstride);

    /* quantize colors */
    VPQuantize(shd, x1-x0+1, y1-y0+1, z1-z0+1, 1., 255, dst,
	       dst_xstride, dst_ystride, dst_zstride);

    Dealloc(vpc, shd);
    return(VP_OK);
}

/*
 * VPClassifyBlock
 *
 * Classify a block of the current raw volume.  The result is an
 * array of floating point opacities in the range 0.0-1.0.
 */

vpResult
VPClassifyBlock(vpc, correct, x0, y0, z0, x1, y1, z1, opc,
		dst_xstride, dst_ystride, dst_zstride)
vpContext *vpc;		/* context */
int correct;		/* if true then correct for view */
int x0, y0, z0;		/* origin of extracted region */
int x1, y1, z1;		/* opposite corner of extracted region */
float *opc;		/* buffer to store result into */
int dst_xstride;	/* stride (in bytes) for destination array */
int dst_ystride;
int dst_zstride;
{
    unsigned char *voxel;
    int xstride, ystride, zstride;
    int x, y, z;
    float opacity;
    int quant_opc;
    int retcode;

    if (correct) {
	if ((retcode = VPFactorView(vpc)) != VP_OK)
	    return(retcode);
    }
    xstride = vpc->xstride;
    ystride = vpc->ystride;
    zstride = vpc->zstride;
    voxel = vpc->raw_voxels;
    voxel += x0*xstride + y0*ystride + z0*zstride;
    for (z = z0; z <= z1; z++) {
	for (y = y0; y <= y1; y++) {
	    for (x = x0; x <= x1; x++) {
		opacity = VPClassifyVoxel(vpc, voxel);
		if (correct) {
		    quant_opc = opacity * 255.;
		    if (quant_opc > 255)
			quant_opc = 255;
		    else if (quant_opc < 0)
			quant_opc = 0;
		    opacity = CorrectOpacity(vpc, quant_opc, x, y, z);
		}
		*opc = opacity;
		opc = (float *)((char *)opc + dst_xstride);
		voxel += xstride;
	    }
	    opc = (float *)((char *)opc + dst_ystride - (x1-x0+1)*dst_xstride);
	    voxel += ystride - (x1-x0+1)*xstride;
	}
	opc = (float *)((char *)opc + dst_zstride - (y1-y0+1)*dst_ystride);
	voxel += zstride - (y1-y0+1)*ystride;
    }
    return(VP_OK);
}

/*
 * VPClassifyVoxel
 *
 * Classify a single voxel.  Return value is an opacity.
 */

float
VPClassifyVoxel(vpc, voxel)
vpContext *vpc;		/* context */
void *voxel;		/* pointer to voxel */
{
    int num_params;		/* number of parameters to classifier */
    int p;			/* current parameter number */
    int field;			/* field for the parameter */
    int field_size;		/* size of the field */
    int field_offset;		/* offset for the field */
    int index;			/* index for table lookup */
    float opacity;		/* current value of the opacity */

    num_params = vpc->num_clsfy_params;
    opacity = 1;
    for (p = 0; p < num_params; p++) {
	/* get table index */
	field = vpc->param_field[p];
	field_offset = vpc->field_offset[field];
	field_size = vpc->field_size[field];
	index = VoxelField(voxel, field_offset, field_size);

	/* load table value */
	opacity *= vpc->clsfy_table[p][index];
    }
    return(opacity);
}

/*
 * CorrectOpacity
 *
 * Correct an opacity for the current view.
 * Return value is the corrected opacity.
 */

static float
CorrectOpacity(vpc, quant_opc, x, y, z)
vpContext *vpc;		/* context */
int quant_opc;		/* input opacity (0-255) */
int x, y, z;		/* voxel coordinates in object space */
{
    float opacity;

    if (vpc->affine_view) {
	opacity = vpc->affine_opac_correct[quant_opc];
    } else {
	/* XXX perspective rendering not available yet */
	opacity = (float)quant_opc / (float)255.;
    }
    return(opacity);
}

/*
 * VPShadeBlock
 *
 * Shade a block of the current raw volume.  The result is an
 * array of floating point colors in the range 0.0-255.0.
 */

vpResult
VPShadeBlock(vpc, x0, y0, z0, x1, y1, z1, shd,
	     dst_xstride, dst_ystride, dst_zstride)
vpContext *vpc;		/* context */
int x0, y0, z0;		/* origin of extracted region */
int x1, y1, z1;		/* opposite corner of extracted region */
float *shd;		/* buffer to store result into */
int dst_xstride;	/* stride (in bytes) for destination array */
int dst_ystride;
int dst_zstride;
{
    unsigned char *voxel;
    int xstride, ystride, zstride;
    int x, y, z;
    int color_channels;

    color_channels = vpc->color_channels;
    xstride = vpc->xstride;
    ystride = vpc->ystride;
    zstride = vpc->zstride;
    voxel = vpc->raw_voxels;
    voxel += x0*xstride + y0*ystride + z0*zstride;
    for (z = z0; z <= z1; z++) {
	for (y = y0; y <= y1; y++) {
	    for (x = x0; x <= x1; x++) {
		ShadeVoxel(vpc, voxel, x, y, z, shd);
		shd = (float *)((char *)shd + dst_xstride);
		voxel += xstride;
	    }
	    shd = (float *)((char *)shd + dst_ystride - (x1-x0+1)*dst_xstride);
	    voxel += ystride - (x1-x0+1)*xstride;
	}
	shd = (float *)((char *)shd + dst_zstride - (y1-y0+1)*dst_ystride);
	voxel += zstride - (y1-y0+1)*ystride;
    }
    return(VP_OK);
}

/*
 * ShadeVoxel
 *
 * Shade a voxel.
 */

static void
ShadeVoxel(vpc, voxel, x, y, z, dst)
vpContext *vpc;		/* context */
void *voxel;		/* voxel data */
int x, y, z;		/* voxel coordinates */
float *dst;		/* storage for result (1 or 3 intensities, 0-255) */
{
    int num_materials;
    int color_channels;
    int color_index_size, color_index_offset, color_index, color_table_offset;
    int weight_index_size, weight_index_offset, weight_index;
    int weight_table_offset;
    int m;
    float r, g, b;
    float *color_table;
    float *weight_table;

    /* check shading mode */
    if (vpc->shading_mode == CALLBACK_SHADER) {
	if (vpc->color_channels == 1)
	    vpc->shade_func(voxel, dst, vpc->client_data);
	else
	    vpc->shade_func(voxel, dst, dst+1, dst+2, vpc->client_data);
	return;
    } else if (vpc->shading_mode != LOOKUP_SHADER) {
	VPBug("unknown shader type");
    }

    /* compute table indices */
    num_materials = vpc->num_materials;
    color_channels = vpc->color_channels;
    color_index_size = vpc->field_size[vpc->color_field];
    color_index_offset = vpc->field_offset[vpc->color_field];
    color_index = VoxelField(voxel, color_index_offset, color_index_size);
    color_table_offset = color_index * num_materials;
    weight_index_size = vpc->field_size[vpc->weight_field];
    weight_index_offset = vpc->field_offset[vpc->weight_field];
    weight_index = VoxelField(voxel, weight_index_offset, weight_index_size);
    weight_table_offset = weight_index * num_materials;

    /* look up values in tables */
    if (color_channels == 1) {
	color_table = vpc->shade_color_table + color_table_offset;
	weight_table = vpc->shade_weight_table + weight_table_offset;
	if (num_materials == 1) {
	    r = *color_table;
	} else {
	    r = 0;
	    for (m = 0; m < num_materials; m++)
		r += *color_table++ * *weight_table++;
	}
	*dst = r;
    } else {
	color_table = vpc->shade_color_table + 3*color_table_offset;
	weight_table = vpc->shade_weight_table + weight_table_offset;
	if (num_materials == 1) {
	    r = *color_table++;
	    g = *color_table++;
	    b = *color_table;
	} else {
	    r = 0;
	    g = 0;
	    b = 0;
	    for (m = 0; m < num_materials; m++) {
		r += *color_table++ * *weight_table;
		g += *color_table++ * *weight_table;
		b += *color_table++ * *weight_table;
	    }
	}
	dst[0] = r;
	dst[1] = g;
	dst[2] = b;
    }
}

/*
 * VPQuantize
 *
 * Quantize a floating point array and store the result in a byte array.
 */

void
VPQuantize(src, xlen, ylen, zlen, scale, maxvalue, dst,
	   dst_xstride, dst_ystride, dst_zstride)
float *src;		/* floating point array */
int xlen, ylen, zlen;	/* array dimensions */
double scale;		/* scale to apply to each array element */
int maxvalue;		/* clamp each array element to this value */
unsigned char *dst;	/* store results here */
int dst_xstride;	/* stride (in bytes) for destination array */
int dst_ystride;
int dst_zstride;
{
    int value;
    int x, y, z;

    for (z = 0; z < zlen; z++) {
	for (y = 0; y < ylen; y++) {
	    for (x = 0; x < xlen; x++) {
		value = (int)rint(*src++ * scale);
		if (value > maxvalue)
		    value = maxvalue;
		else if (value < 0)
		    value = 0;
		*dst = value;
		dst += dst_xstride;
	    }
	    dst += dst_ystride - xlen*dst_xstride;
	}
	dst += dst_zstride - ylen*dst_ystride;
    }
}

/*
 * ExtractClassifiedVolume
 *
 * Extract a field from a classified volume into an array.
 */

static int
ExtractClassifiedVolume(vpc, axis, x0, y0, z0, x1, y1, z1, field, dst,
			dst_xstride, dst_ystride, dst_zstride)
vpContext *vpc;		/* context */
int axis;		/* which axis to extract from */
int x0, y0, z0;		/* origin of extracted region */
int x1, y1, z1;		/* opposite corner of extracted region */
int field;		/* field to extract */
void *dst;		/* buffer to store result into */
int dst_xstride;	/* stride (in bytes) for destination array */
int dst_ystride;
int dst_zstride;
{
    int i, j, k;		/* voxel coordinates in rotated object space */
    int i0, j0, k0;		/* origin of extracted region */
    int i1, j1, k1;		/* opposite corner of extracted region */
    int dst_istride;		/* stride (in bytes) for destination array */
    int dst_jstride;
    int dst_kstride;
    int ilen, jlen, klen;	/* volume size */
    RLEVoxels *rle_voxels;	/* run-length encoded, classified volume */
    unsigned char *voxel;	/* pointer to current voxel in volume */
    unsigned char *dstptr;	/* pointer to destination */
    unsigned char *length;	/* pointer to current run length */
    int run_length;		/* length of current run */
    int is_non_zero;		/* true if current run is nonzero */
    int rle_bytes_per_voxel;	/* size of unclassified voxel */
    int value;			/* value of parameter for current voxel */
    ScanOffset *slice_runs;	/* offsets to start of runs for a slice */
    int field_size;		/* size of field in bytes */
    int field_offset;		/* byte offset for voxel field */

    /* initialize */
    switch (axis) {
    case VP_X_AXIS:
	rle_voxels = vpc->rle_x;
	i0 = y0; j0 = z0; k0 = x0; i1 = y1; j1 = z1; k1 = x1;
	dst_istride = dst_ystride;
	dst_jstride = dst_zstride;
	dst_kstride = dst_xstride;
	break;
    case VP_Y_AXIS:
	rle_voxels = vpc->rle_y;
	i0 = z0; j0 = x0; k0 = y0; i1 = z1; j1 = x1; k1 = y1;
	dst_istride = dst_zstride;
	dst_jstride = dst_xstride;
	dst_kstride = dst_ystride;
	break;
    case VP_Z_AXIS:
	rle_voxels = vpc->rle_z;
	i0 = x0; j0 = y0; k0 = z0; i1 = x1; j1 = y1; k1 = z1;
	dst_istride = dst_xstride;
	dst_jstride = dst_ystride;
	dst_kstride = dst_zstride;
	break;
    default:
	return(VPSetError(vpc, VPERROR_BAD_OPTION));
    }
    if (rle_voxels == NULL)
	return(VPSetError(vpc, VPERROR_BAD_VOLUME));
    if (rle_voxels->scan_offsets_per_slice < 1)
	return(VPSetError(vpc, VPERROR_BAD_VOLUME));
    ilen = rle_voxels->ilen;
    jlen = rle_voxels->jlen;
    klen = rle_voxels->klen;
    rle_bytes_per_voxel = vpc->rle_bytes_per_voxel;
    if (field == VP_OPACITY_FIELD || field == VP_CORRECTED_OPAC_FIELD) {
	field_size = 1;
	field_offset = rle_bytes_per_voxel - 1;
    } else {
	field_size = vpc->field_size[field];
	field_offset = vpc->field_offset[field];
    }

    /* extract slice */
    dstptr = dst;
    for (k = k0; k <= k1; k++) {
	slice_runs = &rle_voxels->scan_offsets[k *
			rle_voxels->scan_offsets_per_slice];
	voxel = (unsigned char *)rle_voxels->data + slice_runs->first_data;
	length = rle_voxels->run_lengths + slice_runs->first_len;
	run_length = 0;
	is_non_zero = 1;
	for (j = 0; j < jlen; j++) {
	    for (i = 0; i < ilen; i++) {
		while (run_length == 0) {
		    run_length = *length++;
		    is_non_zero = !is_non_zero;
		}
		run_length--;
		if (i >= i0 && i <= i1 && j >= j0 && j <= j1) {
		    if (is_non_zero) {
			if (field_size == 1)
			    ByteField(dstptr, 0) = ByteField(voxel,
							     field_offset);
			else if (field_size == 2)
			    ShortField(dstptr, 0) = ShortField(voxel,
							       field_offset);
			else
			    IntField(dstptr, 0) = IntField(voxel,field_offset);
			voxel += rle_bytes_per_voxel;
		    } else {
			if (field_size == 1)
			    ByteField(dstptr, 0) = 0;
			else if (field_size == 2)
			    ShortField(dstptr, 0) = 0;
			else
			    IntField(dstptr, 0) = 0;
		    }
		    dstptr += dst_istride;
		} else {
		    if (is_non_zero)
			voxel += rle_bytes_per_voxel;
		}
	    }
	    if (j >= j0 && j <= j1)
		dstptr += dst_jstride - (i1-i0+1)*dst_istride;
	}
	dstptr += dst_kstride - (j1-j0+1)*dst_jstride;
    }
    return(VP_OK);
}
