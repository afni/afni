/*
 * vp_context.c
 *
 * Routines to create, modify and destroy vpContext objects.
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

static void InitContext ANSI_ARGS((vpContext *vpc));
#ifdef HAVE_HIRES_TIMER
static void StartHiResTimer ANSI_ARGS((vpContext *vpc));
#endif

#ifdef STATISTICS
int vpResampleCount = 0;
int vpCompositeCount = 0;
int vpERTSkipCount = 0;
int vpERTSkipAgainCount = 0;
int vpERTUpdateCount = 0;
int vpSpecialZeroSkipCount = 0;
int vpRunFragmentCount = 0;
#endif

/*
 * InitContext
 *
 * Initialize the fields of a context that have defaults.
 */

static void
InitContext(vpc)
vpContext *vpc;
{
    extern int read(), write();
    int m, l;

    vpc->xlen = 0;
    vpc->ylen = 0;
    vpc->zlen = 0;
    vpc->raw_bytes_per_voxel = 0;
    vpc->num_voxel_fields = 0;
    vpc->num_shade_fields = 0;
    vpc->min_opacity = 0.0;
    vpc->num_clsfy_params = 0;
    vpc->color_channels = 1;
    vpc->shading_mode = LOOKUP_SHADER;
    vpc->num_materials = 1;
    vpc->color_field = 0;
    vpc->weight_field = 0;
    for (m = 0; m < VP_MAX_MATERIAL; m++) {
	vpc->matl_props[m][EXT_SURFACE][MATL_AMB_R] = DEFAULT_AMBIENT;
	vpc->matl_props[m][EXT_SURFACE][MATL_DIFF_R] = DEFAULT_DIFFUSE;
	vpc->matl_props[m][EXT_SURFACE][MATL_SPEC_R] = DEFAULT_SPECULAR;
	vpc->matl_props[m][EXT_SURFACE][MATL_AMB_G] = DEFAULT_AMBIENT;
	vpc->matl_props[m][EXT_SURFACE][MATL_DIFF_G] = DEFAULT_DIFFUSE;
	vpc->matl_props[m][EXT_SURFACE][MATL_SPEC_G] = DEFAULT_SPECULAR;
	vpc->matl_props[m][EXT_SURFACE][MATL_AMB_B] = DEFAULT_AMBIENT;
	vpc->matl_props[m][EXT_SURFACE][MATL_DIFF_B] = DEFAULT_DIFFUSE;
	vpc->matl_props[m][EXT_SURFACE][MATL_SPEC_B] = DEFAULT_SPECULAR;
	vpc->matl_props[m][EXT_SURFACE][MATL_SHINY] = DEFAULT_SHINYNESS;

	vpc->matl_props[m][INT_SURFACE][MATL_AMB_R] = DEFAULT_AMBIENT;
	vpc->matl_props[m][INT_SURFACE][MATL_DIFF_R] = DEFAULT_DIFFUSE;
	vpc->matl_props[m][INT_SURFACE][MATL_SPEC_R] = DEFAULT_SPECULAR;
	vpc->matl_props[m][INT_SURFACE][MATL_AMB_G] = DEFAULT_AMBIENT;
	vpc->matl_props[m][INT_SURFACE][MATL_DIFF_G] = DEFAULT_DIFFUSE;
	vpc->matl_props[m][INT_SURFACE][MATL_SPEC_G] = DEFAULT_SPECULAR;
	vpc->matl_props[m][INT_SURFACE][MATL_AMB_B] = DEFAULT_AMBIENT;
	vpc->matl_props[m][INT_SURFACE][MATL_DIFF_B] = DEFAULT_DIFFUSE;
	vpc->matl_props[m][INT_SURFACE][MATL_SPEC_B] = DEFAULT_SPECULAR;
	vpc->matl_props[m][INT_SURFACE][MATL_SHINY] = DEFAULT_SHINYNESS;
    }
    for (l = 0; l < VP_MAX_LIGHTS; l++) {
	vpc->light_enable[l] = 0;
	vpSetVector4(vpc->light_vector[l], 0.57735, 0.57735, 0.57735, 1.);
	vpSetVector3(vpc->light_color[l], 1., 1., 1.);
    }
    vpc->light_enable[0] = 1;
    vpc->light_both_sides = 0;
    vpc->reverse_surface_sides = 0;
    vpc->dc_enable = 0;
    vpc->dc_front_factor = 1.;
    vpc->dc_density = 1.;
    vpc->dc_quantization = DEFAULT_DC_QUANTIZATION;
    vpIdentity4(vpc->transforms[VP_MODEL]);
    vpIdentity4(vpc->transforms[VP_VIEW]);
    vpIdentity4(vpc->transforms[VP_PROJECT]);
    vpWindow(vpc, VP_PARALLEL, -0.5, 0.5, -0.5, 0.5, -0.5, 0.5);
    vpc->current_matrix = VP_MODEL;
    vpc->concat_left = 0;
    vpc->axis_override = VP_NO_AXIS;
    vpc->max_opacity = 1.0;
    vpc->factored_view_ready = 0;
    vpc->int_image_width_hint = 0;
    vpc->int_image_height_hint = 0;
    vpc->clamp_shade_table = 1;
    vpc->enable_shadows = 0;
    vpc->shadow_light_num = VP_LIGHT0;
    vpc->shadow_width_hint = 0;
    vpc->shadow_height_hint = 0;
    vpc->shadow_bias = 4;
    vpc->write_func = write;
    vpc->read_func = read;
    vpc->mmap_func = NULL;
    vpc->log_alloc_func = NULL;
    vpc->log_free_func = NULL;
    vpc->status_func = NULL;
    vpc->client_data = NULL;
    vpc->error_code = VP_OK;
#ifdef DEBUG
    bzero(vpc->debug_enable, VPDEBUG_COUNT * sizeof(short));
    vpc->trace_u = -1;
    vpc->trace_v = -1;
    vpc->trace_shadow_k = 0;
#endif

#ifdef USE_TIMER
    bzero(vpc->timer_ticks, VPTIMER_COUNT * sizeof(unsigned));
#ifdef HAVE_LORES_TIMER
    vpc->timer_usec_per_tick = 1.0;
#endif
#ifdef HAVE_HIRES_TIMER
    StartHiResTimer(vpc);
#endif
#endif /* USE_TIMER */
}

/*
 * vpCreateContext
 *
 * Create a rendering context.
 */

vpContext *
vpCreateContext()
{
    vpContext *vpc;

    /* NOTE: cannot use Alloc() to allocate a context since it
       requires a context as an argument */
    if ((vpc = (vpContext *)malloc(sizeof(vpContext))) == NULL)
	VPBug("out of memory");
    bzero(vpc, sizeof(vpContext));
    InitContext(vpc);
    return(vpc);
}

/*
 * vpDestroyContext
 *
 * Destroy a rendering context.
 */

void
vpDestroyContext(vpc)
vpContext *vpc;
{
    VPResizeDepthCueTable(vpc, 0, 0);
    VPResizeRenderBuffers(vpc, 0, 0, 0);
    vpDestroyClassifiedVolume(vpc);
    vpDestroyMinMaxOctree(vpc);
    if (vpc->sum_table != NULL)
	Dealloc(vpc, vpc->sum_table);
    VPResizeShadowBuffer(vpc, 0, 0);

    /* NOTE: don't use Dealloc() to deallocate a context since
       it wasn't created with Alloc() */
    free((void *)vpc);
}

/*
 * vpSetVolumeSize
 *
 * Set the size of the volume.
 */

vpResult
vpSetVolumeSize(vpc, xlen, ylen, zlen)
vpContext *vpc;
int xlen, ylen, zlen;
{
    vpc->xlen = xlen;
    vpc->ylen = ylen;
    vpc->zlen = zlen;
    vpc->factored_view_ready = 0;
    vpDestroyClassifiedVolume(vpc);
    vpDestroyMinMaxOctree(vpc);
    return(VP_OK);
}

/*
 * vpSetVoxelSize
 *
 * Set the size of a voxel.
 */

vpResult
vpSetVoxelSize(vpc, bytes_per_voxel, num_voxel_fields, num_shade_fields,
	       num_clsfy_fields)
vpContext *vpc;
int bytes_per_voxel;
int num_voxel_fields;
int num_shade_fields;
int num_clsfy_fields;
{
    if (num_voxel_fields >= VP_MAX_FIELDS)
	return(VPSetError(vpc, VPERROR_LIMIT_EXCEEDED));
    if (num_clsfy_fields < 0 || num_clsfy_fields > num_voxel_fields)
	return(VPSetError(vpc, VPERROR_BAD_VALUE));
    if (num_shade_fields < 0 || num_shade_fields > num_voxel_fields)
	return(VPSetError(vpc, VPERROR_BAD_VALUE));
    vpc->raw_bytes_per_voxel = bytes_per_voxel;
    vpc->num_voxel_fields = num_voxel_fields;
    vpc->num_shade_fields = num_shade_fields;
    vpc->num_clsfy_params = num_clsfy_fields;
    vpDestroyClassifiedVolume(vpc);
    vpDestroyMinMaxOctree(vpc);
    return(VP_OK);
}

/*
 * vpSetVoxelField
 *
 * Set the size and position of a voxel field.
 */

vpResult
vpSetVoxelField(vpc, field_num, field_size, field_offset, field_max)
vpContext *vpc;
int field_num;
int field_size;
int field_offset;
int field_max;
{
    if (field_num < 0 || field_num >= vpc->num_voxel_fields)
	return(VPSetError(vpc, VPERROR_BAD_VALUE));
    if (field_size != 1 && field_size != 2 && field_size != 4)
	return(VPSetError(vpc, VPERROR_BAD_VALUE));
    if (field_offset < 0 || field_offset >= vpc->raw_bytes_per_voxel)
	return(VPSetError(vpc, VPERROR_BAD_VALUE));
    vpc->field_size[field_num] = field_size;
    vpc->field_offset[field_num] = field_offset;
    vpc->field_max[field_num] = field_max;
    vpDestroyClassifiedVolume(vpc);
    vpDestroyMinMaxOctree(vpc);
    return(VP_OK);
}

/*
 * vpSetRawVoxels
 *
 * Set the array of unclassified voxels.
 */

vpResult
vpSetRawVoxels(vpc, raw_voxels, raw_voxels_size, xstride, ystride, zstride)
vpContext *vpc;
int xstride, ystride, zstride;
void *raw_voxels;
int raw_voxels_size;
{
    vpc->raw_voxels = raw_voxels;
    vpc->raw_voxels_size = raw_voxels_size;
    vpc->xstride = xstride;
    vpc->ystride = ystride;
    vpc->zstride = zstride;
    if (raw_voxels_size != 0)
	vpDestroyClassifiedVolume(vpc);
    vpDestroyMinMaxOctree(vpc);
    return(VP_OK);
}

/*
 * vpSetClassifierTable
 *
 * Specify a lookup table for one of the classification function's
 * parameters.
 */

vpResult
vpSetClassifierTable(vpc, param_num, param_field, table, table_size)
vpContext *vpc;
int param_num;
int param_field;
float *table;
int table_size;
{
    if (param_num < 0 || param_num >= vpc->num_clsfy_params)
	return(VPSetError(vpc, VPERROR_BAD_VALUE));
    if (param_field < 0 || param_field >= vpc->num_voxel_fields)
	return(VPSetError(vpc, VPERROR_BAD_VALUE));
    vpc->param_field[param_num] = param_field;
    vpc->clsfy_table[param_num] = table;
    vpc->clsfy_table_size[param_num] = table_size;
    return(VP_OK);
}

/*
 * vpSetLookupShader
 *
 * Define a lookup shader.
 */

vpResult
vpSetLookupShader(vpc, color_channels, num_materials,
		  color_field, color_table, color_table_size,
		  weight_field, weight_table, weight_table_size)
vpContext *vpc;
int color_channels;
int num_materials;
int color_field;
float *color_table;
int color_table_size;
int weight_field;
float *weight_table;
int weight_table_size;
{
    if (color_channels != 1 && color_channels != 3)
	return(VPSetError(vpc, VPERROR_BAD_VALUE));
    if (num_materials <= 0 || num_materials >= VP_MAX_MATERIAL)
	return(VPSetError(vpc, VPERROR_LIMIT_EXCEEDED));
    if (color_field < 0 || color_field >= vpc->num_voxel_fields ||
	color_table == NULL || color_table_size == 0)
	return(VPSetError(vpc, VPERROR_BAD_VALUE));
    if (num_materials > 1) {
	if (weight_field < 0 || weight_field >= vpc->num_voxel_fields ||
	    weight_table == NULL || weight_table_size == 0)
	    return(VPSetError(vpc, VPERROR_BAD_VALUE));
    }
    vpc->color_channels = color_channels;
    vpc->shading_mode = LOOKUP_SHADER;
    vpc->shade_color_table = color_table;
    vpc->shade_color_table_size = color_table_size;
    vpc->shade_weight_table = weight_table;
    vpc->shade_weight_table_size = weight_table_size;
    vpc->num_materials = num_materials;
    vpc->color_field = color_field;
    vpc->weight_field = weight_field;
    return(VP_OK);
}

/*
 * vpSetShadowLookupShader
 *
 * Define a lookup shader that support shadows.
 */

vpResult
vpSetShadowLookupShader(vpc, color_channels, num_materials,
			color_field, color_table, color_table_size,
			weight_field, weight_table, weight_table_size,
			shadow_table, shadow_table_size)
vpContext *vpc;
int color_channels;
int num_materials;
int color_field;
float *color_table;
int color_table_size;
int weight_field;
float *weight_table;
int weight_table_size;
float *shadow_table;
int shadow_table_size;
{
    if (color_channels != 1 && color_channels != 3)
	return(VPSetError(vpc, VPERROR_BAD_VALUE));
    if (num_materials <= 0 || num_materials >= VP_MAX_MATERIAL)
	return(VPSetError(vpc, VPERROR_LIMIT_EXCEEDED));
    if (color_field < 0 || color_field >= vpc->num_voxel_fields ||
	color_table == NULL || color_table_size == 0)
	return(VPSetError(vpc, VPERROR_BAD_VALUE));
    if (num_materials > 1) {
	if (weight_field < 0 || weight_field >= vpc->num_voxel_fields ||
	    weight_table == NULL || weight_table_size == 0)
	    return(VPSetError(vpc, VPERROR_BAD_VALUE));
    }
    if (shadow_table_size != color_table_size)
	return(VPSetError(vpc, VPERROR_BAD_SIZE));
    vpc->color_channels = color_channels;
    vpc->shading_mode = LOOKUP_SHADER;
    vpc->shade_color_table = color_table;
    vpc->shade_color_table_size = color_table_size;
    vpc->shade_weight_table = weight_table;
    vpc->shade_weight_table_size = weight_table_size;
    vpc->num_materials = num_materials;
    vpc->color_field = color_field;
    vpc->weight_field = weight_field;
    vpc->shadow_color_table = shadow_table;
    vpc->shadow_color_table_size = shadow_table_size;
    return(VP_OK);
}

/*
 * vpSetMaterial
 *
 * Set material parameters.
 */

vpResult
vpSetMaterial(vpc, material, property, surface_side, r, g, b)
vpContext *vpc;
int material;
int property;
int surface_side;
double r, g, b;
{
    material -= VP_MATERIAL0;
    if (material < 0 || material >= vpc->num_materials)
	return(VPSetError(vpc, VPERROR_BAD_VALUE));
    if (surface_side == 0 || (surface_side & ~VP_BOTH_SIDES) != 0)
	return(VPSetError(vpc, VPERROR_BAD_OPTION));
    if (property != VP_SHINYNESS) {
	if (r < 0. || g < 0. || b < 0.)
	    return(VPSetError(vpc, VPERROR_BAD_VALUE));
    }
    switch (property) {
    case VP_AMBIENT:
	if (surface_side & VP_EXTERIOR) {
	    vpc->matl_props[material][EXT_SURFACE][MATL_AMB_R] = r * 255.;
	    vpc->matl_props[material][EXT_SURFACE][MATL_AMB_G] = g * 255.;
	    vpc->matl_props[material][EXT_SURFACE][MATL_AMB_B] = b * 255.;
	}
	if (surface_side & VP_INTERIOR) {
	    vpc->matl_props[material][INT_SURFACE][MATL_AMB_R] = r * 255.;
	    vpc->matl_props[material][INT_SURFACE][MATL_AMB_G] = g * 255.;
	    vpc->matl_props[material][INT_SURFACE][MATL_AMB_B] = b * 255.;
	}
	break;
    case VP_DIFFUSE:
	if (surface_side & VP_EXTERIOR) {
	    vpc->matl_props[material][EXT_SURFACE][MATL_DIFF_R] = r * 255.;
	    vpc->matl_props[material][EXT_SURFACE][MATL_DIFF_G] = g * 255.;
	    vpc->matl_props[material][EXT_SURFACE][MATL_DIFF_B] = b * 255.;
	}
	if (surface_side & VP_INTERIOR) {
	    vpc->matl_props[material][INT_SURFACE][MATL_DIFF_R] = r * 255.;
	    vpc->matl_props[material][INT_SURFACE][MATL_DIFF_G] = g * 255.;
	    vpc->matl_props[material][INT_SURFACE][MATL_DIFF_B] = b * 255.;
	}
	break;
    case VP_SPECULAR:
	if (surface_side & VP_EXTERIOR) {
	    vpc->matl_props[material][EXT_SURFACE][MATL_SPEC_R] = r * 255.;
	    vpc->matl_props[material][EXT_SURFACE][MATL_SPEC_G] = g * 255.;
	    vpc->matl_props[material][EXT_SURFACE][MATL_SPEC_B] = b * 255.;
	}
	if (surface_side & VP_INTERIOR) {
	    vpc->matl_props[material][INT_SURFACE][MATL_SPEC_R] = r * 255.;
	    vpc->matl_props[material][INT_SURFACE][MATL_SPEC_G] = g * 255.;
	    vpc->matl_props[material][INT_SURFACE][MATL_SPEC_B] = b * 255.;
	}
	break;
    case VP_SHINYNESS:
	if (surface_side & VP_EXTERIOR)
	    vpc->matl_props[material][EXT_SURFACE][MATL_SHINY] = r;
	if (surface_side & VP_INTERIOR)
	    vpc->matl_props[material][INT_SURFACE][MATL_SHINY] = r;
	break;
    default:
	return(VPSetError(vpc, VPERROR_BAD_OPTION));
    }
    return(VP_OK);
}

/*
 * vpSetLight
 *
 * Set the properties of a directional light source.
 */

vpResult
vpSetLight(vpc, light_num, property, n0, n1, n2)
vpContext *vpc;
int light_num;
int property;
double n0, n1, n2;
{
    vpVector4 v1, v2;

    light_num -= VP_LIGHT0;
    if (light_num < 0 || light_num >= VP_MAX_LIGHTS)
	return(VPSetError(vpc, VPERROR_LIMIT_EXCEEDED));
    switch (property) {
    case VP_DIRECTION:
	vpSetVector4(v1, n0, n1, n2, 1.);
	if (vpNormalize3(v1) != VP_OK)
	    return(VPSetError(vpc, VPERROR_SINGULAR));
	vpMatrixVectorMult4(v2, vpc->transforms[VP_MODEL], v1);
	vpc->light_vector[light_num][0] = v2[0];
	vpc->light_vector[light_num][1] = v2[1];
	vpc->light_vector[light_num][2] = v2[2];
	vpc->light_vector[light_num][3] = v2[3];
	break;
    case VP_COLOR:
	if (n0 < 0. || n0 > 1. || n1 < 0. || n1 > 1. || n2 < 0. || n2 > 1.)
	    return(VPSetError(vpc, VPERROR_BAD_VALUE));
	vpc->light_color[light_num][0] = n0;
	vpc->light_color[light_num][1] = n1;
	vpc->light_color[light_num][2] = n2;
	break;
    default:
	return(VPSetError(vpc, VPERROR_BAD_OPTION));
    }
    return(VP_OK);
}

/*
 * vpEnable
 *
 * Enable or disable an option.
 */

vpResult
vpEnable(vpc, option, value)
vpContext *vpc;
int option;
int value;
{
    switch (option) {
    case VP_LIGHT0:
    case VP_LIGHT1:
    case VP_LIGHT2:
    case VP_LIGHT3:
    case VP_LIGHT4:
    case VP_LIGHT5:
	if (value == 0)
	    vpc->light_enable[option - VP_LIGHT0] = 0;
	else
	    vpc->light_enable[option - VP_LIGHT0] = 1;
	break;
    case VP_LIGHT_BOTH_SIDES:
	if (value == 0)
	    vpc->light_both_sides = 0;
	else
	    vpc->light_both_sides = 1;
	break;
    case VP_REVERSE_SURFACE_SIDES:
	if (value == 0)
	    vpc->reverse_surface_sides = 0;
	else
	    vpc->reverse_surface_sides = 1;
	break;
    case VP_DEPTH_CUE:
	if (value == 0)
	    vpc->dc_enable = 0;
	else
	    vpc->dc_enable = 1;
	break;
    case VP_VIEW_X_AXIS:
	if (value == 0)
	    vpc->skip_rle_x = 1;
	else
	    vpc->skip_rle_x = 0;
	break;
    case VP_VIEW_Y_AXIS:
	if (value == 0)
	    vpc->skip_rle_y = 1;
	else
	    vpc->skip_rle_y = 0;
	break;
    case VP_VIEW_Z_AXIS:
	if (value == 0)
	    vpc->skip_rle_z = 1;
	else
	    vpc->skip_rle_z = 0;
	break;
    case VP_SHADOW:
	if (value == 0) {
	    vpc->enable_shadows = 0;
	} else {
	    vpc->enable_shadows = 1;
	}
	vpc->factored_view_ready = 0;
	break;
    case VP_CLAMP_SHADE_TABLE:
	if (value == 0)
	    vpc->clamp_shade_table = 0;
	else
	    vpc->clamp_shade_table = 1;
	break;
    default:
	return(VPSetError(vpc, VPERROR_BAD_VALUE));
    }
    return(VP_OK);
}

/*
 * vpSetDepthCueing
 *
 * Set depth cueing parameters.
 */

vpResult
vpSetDepthCueing(vpc, front_factor, density)
vpContext *vpc;
double front_factor;
double density;
{
    if (front_factor <= 0.)
	return(VPSetError(vpc, VPERROR_BAD_VALUE));
    vpc->dc_front_factor = front_factor;
    vpc->dc_density = density;
    if (vpc->dc_table_len > 0)
	VPComputeDepthCueTable(vpc, 0, vpc->dc_table_len-1);
    return(VP_OK);
}

/*
 * vpCurrentMatrix
 *
 * Set the current matrix.
 */

vpResult
vpCurrentMatrix(vpc, option)
vpContext *vpc;
int option;
{
    switch (option) {
    case VP_MODEL:
    case VP_VIEW:
    case VP_PROJECT:
	vpc->current_matrix = option;
	break;
    default:
	return(VPSetError(vpc, VPERROR_BAD_OPTION));
    }
    return(VP_OK);
}

/*
 * vpIdentityMatrix
 *
 * Load the identity into the current matrix.
 */

vpResult
vpIdentityMatrix(vpc)
vpContext *vpc;
{
    vpIdentity4(vpc->transforms[vpc->current_matrix]);
    vpc->factored_view_ready = 0;
    return(VP_OK);
}

/*
 * vpSetMatrix
 *
 * Load the elements of the current matrix.
 */

vpResult
vpSetMatrix(vpc, matrix)
vpContext *vpc;
vpMatrix4 matrix;
{
    bcopy(matrix, vpc->transforms[vpc->current_matrix], sizeof(vpMatrix4));
    vpc->factored_view_ready = 0;
    return(VP_OK);
}

/*
 * vpGetMatrix
 *
 * Get the elements of the indicated matrix.
 */

vpResult
vpGetMatrix(vpc, matrix_code, matrix)
vpContext *vpc;
int matrix_code;
vpMatrix4 matrix;
{
    switch (matrix_code) {
    case VP_MODEL:
    case VP_VIEW:
    case VP_PROJECT:
	bcopy(vpc->transforms[matrix_code], matrix, sizeof(vpMatrix4));
	break;
    case VP_SCREEN:
	VPComputeViewTransform(vpc, matrix);
	break;
    default:
	return(VPSetError(vpc, VPERROR_BAD_OPTION));
    }
    return(VP_OK);
}

/*
 * vpMultMatrix
 *
 * Multiply the current matrix by the given matrix.
 */

vpResult
vpMultMatrix(vpc, matrix)
vpContext *vpc;
vpMatrix4 matrix;
{
    vpMatrix4 tmp;

    if (vpc->concat_left)
	vpMatrixMult4(tmp, matrix, vpc->transforms[vpc->current_matrix]);
    else
	vpMatrixMult4(tmp, vpc->transforms[vpc->current_matrix], matrix);
    bcopy(tmp, vpc->transforms[vpc->current_matrix], sizeof(vpMatrix4));
    vpc->factored_view_ready = 0;
    return(VP_OK);
}

/*
 * vpTranslate
 *
 * Multiply the current matrix by a translation matrix.
 */

vpResult
vpTranslate(vpc, tx, ty, tz)
vpContext *vpc;
double tx, ty, tz;
{
    vpMatrix4 t, tmp;

    VPLoadTranslation(t, tx, ty, tz);
    if (vpc->concat_left)
	vpMatrixMult4(tmp, t, vpc->transforms[vpc->current_matrix]);
    else
	vpMatrixMult4(tmp, vpc->transforms[vpc->current_matrix], t);
    bcopy(tmp, vpc->transforms[vpc->current_matrix], sizeof(vpMatrix4));
    vpc->factored_view_ready = 0;
    return(VP_OK);
}    

/*
 * vpRotate
 *
 * Multiply the current matrix by a rotation matrix.
 */

vpResult
vpRotate(vpc, axis, degrees)
vpContext *vpc;
int axis;
double degrees;
{
    vpMatrix4 r, tmp;

    if (axis != VP_X_AXIS && axis != VP_Y_AXIS && axis != VP_Z_AXIS)
	return(VPSetError(vpc, VPERROR_BAD_OPTION));
    VPLoadRotation(r, axis, degrees);
    if (vpc->concat_left)
	vpMatrixMult4(tmp, r, vpc->transforms[vpc->current_matrix]);
    else
	vpMatrixMult4(tmp, vpc->transforms[vpc->current_matrix], r);
    bcopy(tmp, vpc->transforms[vpc->current_matrix], sizeof(vpMatrix4));
    vpc->factored_view_ready = 0;
    return(VP_OK);
}

/*
 * vpScale
 *
 * Multiply the current matrix by a scale matrix.
 */

vpResult
vpScale(vpc, sx, sy, sz)
vpContext *vpc;
double sx, sy, sz;
{
    vpMatrix4 s, tmp;

    VPLoadScale(s, sx, sy, sz);
    if (vpc->concat_left)
	vpMatrixMult4(tmp, s, vpc->transforms[vpc->current_matrix]);
    else
	vpMatrixMult4(tmp, vpc->transforms[vpc->current_matrix], s);
    bcopy(tmp, vpc->transforms[vpc->current_matrix], sizeof(vpMatrix4));
    vpc->factored_view_ready = 0;
    return(VP_OK);
}

/*
 * vpWindow
 *
 * Set the projection matrix for a perspective or
 * orthographic viewing volume.
 */

vpResult
vpWindow(vpc, type, left, right, bottom, top, near, far)
vpContext *vpc;
int type;
double left, right, bottom, top, near, far;
{
    vpMatrix4 projectm, tmp;

    if (left >= right || bottom >= top || near >= far)
	return(VPSetError(vpc, VPERROR_BAD_VALUE));
    if (type == VP_PERSPECTIVE) {
	if (near <= 0 || far <= 0)
	    return(VPSetError(vpc, VPERROR_BAD_VALUE));
    } else if (type != VP_PARALLEL) {
	return(VPSetError(vpc, VPERROR_BAD_OPTION));
    }

    vpIdentity4(projectm);
    if (type == VP_PARALLEL) {
	projectm[0][0] = 2. / (right - left);
	projectm[1][1] = 2. / (top - bottom);
	projectm[2][2] = 2. / (far - near);
	projectm[0][3] = (left + right) / (left - right);
	projectm[1][3] = (bottom + top) / (bottom - top);
	projectm[2][3] = (near + far) / (near - far);
    } else {
	/* XXX perspective rendering not available yet */
	return(VPSetError(vpc, VPERROR_BAD_OPTION));
#ifdef notdef
	projectm[0][0] = 2. * near / (right - left);
	projectm[1][1] = 2. * near / (top - bottom);
	projectm[2][2] = (near + far) / (near - far);
	projectm[0][2] = (right + left) / (right - left);
	projectm[1][2] = (top + bottom) / (top - bottom);
	projectm[3][2] = -1.;
	projectm[2][3] = 2. * far * near / (near - far);
#endif
    }
    if (vpc->concat_left)
	vpMatrixMult4(tmp, projectm, vpc->transforms[VP_PROJECT]);
    else
	vpMatrixMult4(tmp, vpc->transforms[VP_PROJECT], projectm);
    bcopy(tmp, vpc->transforms[VP_PROJECT], sizeof(vpMatrix4));
    vpc->factored_view_ready = 0;
    return(VP_OK);
}

/*
 * vpWindowPHIGS
 *
 * Setting the projection matrix using a PHIGS view specification.
 */

vpResult
vpWindowPHIGS(vpc, vrp, vpn, vup, prp, viewport_umin, viewport_umax,
	      viewport_vmin, viewport_vmax, viewport_front, viewport_back,
	      projection_type)
vpContext *vpc;
vpVector3 vrp;
vpVector3 vpn;
vpVector3 vup;
vpVector3 prp;
double viewport_umin;
double viewport_umax;
double viewport_vmin;
double viewport_vmax;
double viewport_front;
double viewport_back;
int projection_type;
{
    vpMatrix4 m1, m2, m3;
    double cw_x, cw_y;	/* center of window */
    double newz;

    if (viewport_umax <= viewport_umin || viewport_vmax <= viewport_vmin ||
	viewport_front <= viewport_back)
	return(VPSetError(vpc, VPERROR_BAD_VALUE));
    if (projection_type != VP_PARALLEL && projection_type != VP_PERSPECTIVE)
	return(VPSetError(vpc, VPERROR_BAD_OPTION));

    /* perspective rendering not available yet */
    if (projection_type == VP_PERSPECTIVE)
	return(VPSetError(vpc, VPERROR_BAD_OPTION));

    /* translate view reference point to the origin */
    VPLoadTranslation(m1, -vrp[0], -vrp[1], -vrp[2]);

    /* rotate VRC axes into world coordinate axes */
    vpIdentity4(m2);
    m2[2][0] = vpn[0];
    m2[2][1] = vpn[1];
    m2[2][2] = vpn[2];
    if (vpNormalize3(m2[2]) != VP_OK)
	return(VPSetError(vpc, VPERROR_SINGULAR));
    vpCrossProduct(m2[0], vup, m2[2]);
    if (vpNormalize3(m2[0]) != VP_OK)
	return(VPSetError(vpc, VPERROR_SINGULAR));
    vpCrossProduct(m2[1], m2[2], m2[0]);
    vpMatrixMult4(m3, m2, m1);

    if (projection_type == VP_PERSPECTIVE) {
	/* translate center of projection to the origin */
	VPLoadTranslation(m1, -prp[0], -prp[1], -prp[2]);
	vpMatrixMult4(m2, m1, m3);
	bcopy(m2, m3, sizeof(vpMatrix4));
    }

    /* shear to make DOP equal to the z axis */
    if (fabs(prp[2]) < VP_EPS)
	return(VPSetError(vpc, VPERROR_SINGULAR));
    vpIdentity4(m1);
    cw_x = 0.5 * (viewport_umin + viewport_umax);
    cw_y = 0.5 * (viewport_vmin + viewport_vmax);
    m1[0][2] = (cw_x - prp[0]) / prp[2];
    m1[1][2] = (cw_y - prp[1]) / prp[2];
    vpMatrixMult4(m2, m1, m3);

    if (projection_type == VP_PARALLEL) {
	/* translate to clip origin */
	VPLoadTranslation(m3, -cw_x, -cw_y, -viewport_back);
	vpMatrixMult4(m1, m3, m2);

	/* scale to clip coordinates */
	VPLoadScale(m2, 2. / (viewport_umax - viewport_umin),
		    2. / (viewport_vmax - viewport_vmin),
		    1. / (viewport_front - viewport_back));
	vpMatrixMult4(m3, m2, m1);
    } else {
	/* scale into canonical perspective view volume */
	if (fabs(prp[2] - viewport_back) < VP_EPS)
	    return(VPSetError(vpc, VPERROR_SINGULAR));
	vpIdentity4(m3);
	m3[0][0] = 2*prp[2] / ((viewport_umax - viewport_umin) *
			       (prp[2] - viewport_back));
	m3[1][1] = 2*prp[2] / ((viewport_vmax - viewport_vmin) *
			       (prp[2] - viewport_back));
	m3[2][2] = 1. / (prp[2] - viewport_back);
	vpMatrixMult4(m1, m3, m2);

	/* transform into clip coordinates */
	vpIdentity4(m2);
	newz = (prp[2] - viewport_front) / (viewport_back - prp[2]);
	m2[2][2] = 1. / (1. + newz);
	m2[2][3] = newz / (-1. - newz);
	m2[3][2] = -1.;
	m2[3][3] = 0.;
	vpMatrixMult4(m3, m2, m1);
    }
    if (vpc->concat_left)
	vpMatrixMult4(m1, m3, vpc->transforms[VP_PROJECT]);
    else
	vpMatrixMult4(m1, vpc->transforms[VP_PROJECT], m3);
    bcopy(m1, vpc->transforms[VP_PROJECT], sizeof(vpMatrix4));
    vpc->factored_view_ready = 0;
    return(VP_OK);
}

/*
 * vpSetImage
 *
 * Set the buffer to store the image into.
 */

vpResult
vpSetImage(vpc, image, width, height, bytes_per_scan, pixel_type)
vpContext *vpc;
unsigned char *image;
int width, height;
int bytes_per_scan;
int pixel_type;
{
    int bytes_per_pixel;

    /* check for errors */
    switch (pixel_type) {
    case VP_ALPHA:
    case VP_LUMINANCE:
	bytes_per_pixel = 1;
	break;
    case VP_LUMINANCEA:
	bytes_per_pixel = 2;
	break;
    case VP_RGB:
    case VP_BGR:
	bytes_per_pixel = 3;
	break;
    case VP_RGBA:
    case VP_ABGR:
	bytes_per_pixel = 4;
	break;
    default:
	return(VPSetError(vpc, VPERROR_BAD_OPTION));
    }
    if (bytes_per_scan < width * bytes_per_pixel)
	return(VPSetError(vpc, VPERROR_BAD_SIZE));

    /* update context */
    if (width != vpc->image_width || height != vpc->image_height) {
	vpc->image_width = width;
	vpc->image_height = height;
	vpc->factored_view_ready = 0;
    }
    vpc->image = image;
    vpc->image_bytes_per_scan = bytes_per_scan;
    vpc->pixel_type = pixel_type;
    return(VP_OK);
}

/*
 * vpSeti
 *
 * Set a rendering option with an integer value.
 */

vpResult
vpSeti(vpc, option, value)
vpContext *vpc;
int option;
int value;
{
    switch (option) {
    case VP_CONCAT_MODE:
	if (value == VP_CONCAT_LEFT)
	    vpc->concat_left = 1;
	else if (value == VP_CONCAT_RIGHT)
	    vpc->concat_left = 0;
	else
	    return(VPSetError(vpc, VPERROR_BAD_OPTION));
	break;
    case VP_DEPTH_CUE_SIZE_HINT:
	if (value < 0)
	    return(VPSetError(vpc, VPERROR_BAD_VALUE));
	vpc->dc_table_len_hint = value;
	break;
    case VP_INT_WIDTH_HINT:
	if (value < 0)
	    return(VPSetError(vpc, VPERROR_BAD_VALUE));
	vpc->int_image_width_hint = value;
	break;
    case VP_INT_HEIGHT_HINT:
	if (value < 0)
	    return(VPSetError(vpc, VPERROR_BAD_VALUE));
	vpc->int_image_height_hint = value;
	break;
    case VP_SHADOW_LIGHT:
	if (value < VP_LIGHT0 || value > VP_LIGHT5)
	    return(VPSetError(vpc, VPERROR_BAD_OPTION));
	vpc->shadow_light_num = value;
	break;
    case VP_SHADOW_WIDTH_HINT:
	if (value < 0)
	    return(VPSetError(vpc, VPERROR_BAD_VALUE));
	vpc->shadow_width_hint = value;
	break;
    case VP_SHADOW_HEIGHT_HINT:
	if (value < 0)
	    return(VPSetError(vpc, VPERROR_BAD_VALUE));
	vpc->shadow_height_hint = value;
	break;
    case VP_SHADOW_BIAS:
	if (value < 0)
	    return(VPSetError(vpc, VPERROR_BAD_VALUE));
	vpc->shadow_bias = value;
	vpc->factored_view_ready = 0;
	break;
    case VP_AXIS_OVERRIDE:
	switch (value) {
	case VP_X_AXIS:
	case VP_Y_AXIS:
	case VP_Z_AXIS:
	case VP_NO_AXIS:
	    vpc->axis_override = value;
	    break;
	default:
	    return(VPSetError(vpc, VPERROR_BAD_OPTION));
	    break;
	}
	vpc->factored_view_ready = 0;
	break;
    case VP_TRACE_SHADOW_K:
#ifdef DEBUG
	vpc->trace_shadow_k = value;
#endif
	break;
    default:
	return(VPSetError(vpc, VPERROR_BAD_OPTION));
    }
    return(VP_OK);
}

/*
 * vpSetd
 *
 * Set a rendering option with a double-precision value.
 */

vpResult
vpSetd(vpc, option, value)
vpContext *vpc;
int option;
double value;
{
    switch (option) {
    case VP_DEPTH_CUE_QUANTIZATION:
	if (value <= 0. || value >= 1.)
	    return(VPSetError(vpc, VPERROR_BAD_VALUE));
	vpc->dc_quantization = value;
	VPResizeDepthCueTable(vpc, 0, 0);
	break;
    case VP_MAX_RAY_OPACITY:
	if (value < 0. || value > 1.)
	    return(VPSetError(vpc, VPERROR_BAD_VALUE));
	vpc->max_opacity = value;
	break;
    case VP_MIN_VOXEL_OPACITY:
	if ((value < 0. || value > 1.) && value != -1.0)
	    return(VPSetError(vpc, VPERROR_BAD_VALUE));
	vpc->min_opacity = value;
	break;
    default:
	return(VPSetError(vpc, VPERROR_BAD_OPTION));
    }
    return(VP_OK);
}

/*
 * vpMinMaxOctreeThreshold
 *
 * Set the threshold of a parameter for constructing the min-max octree.
 */

vpResult
vpMinMaxOctreeThreshold(vpc, param, range)
vpContext *vpc;
int param;
int range;
{
    if (param >= vpc->num_clsfy_params)
	return(VPSetError(vpc, VPERROR_BAD_VALUE));
    vpc->param_maxrange[param] = range;
    return(VP_OK);
}

/*
 * vpSetCallback
 *
 * Set one of the callback functions.
 */

vpResult
vpSetCallback(vpc, option, func)
vpContext *vpc;
int option;
void *func;
{
    extern int read(), write();

    switch (option) {
    case VP_LOG_ALLOC_FUNC:
	vpc->log_alloc_func = func;
	break;
    case VP_LOG_FREE_FUNC:
	vpc->log_free_func = func;
	break;
    case VP_STATUS_FUNC:
	vpc->status_func = func;
	break;
    case VP_READ_FUNC:
	if (func == NULL)
	    vpc->read_func = read;
	else
	    vpc->read_func = func;
	break;
    case VP_WRITE_FUNC:
	if (func == NULL)
	    vpc->write_func = write;
	else
	    vpc->write_func = func;
	break;
    case VP_MMAP_FUNC:
	vpc->mmap_func = func;
	break;
    case VP_GRAY_SHADE_FUNC:
	if (func == NULL) {
	    vpc->shading_mode = LOOKUP_SHADER;
	} else {
	    vpc->color_channels = 1;
	    vpc->shading_mode = CALLBACK_SHADER;
	    vpc->shade_func = func;
	}
	break;
    case VP_RGB_SHADE_FUNC:
	if (func == NULL) {
	    vpc->shading_mode = LOOKUP_SHADER;
	} else {
	    vpc->color_channels = 3;
	    vpc->shading_mode = CALLBACK_SHADER;
	    vpc->shade_func = func;
	}
	break;
    default:
	return(VPSetError(vpc, VPERROR_BAD_OPTION));
    }
    return(VP_OK);
}

/*
 * vpSetClientData
 *
 * Set the client_data hook.
 */

vpResult
vpSetClientData(vpc, client_data)
vpContext *vpc;
void *client_data;
{
    vpc->client_data = client_data;
    return(VP_OK);
}

/*
 * vpSetDebug
 *
 * Set the value of a debugging flag.
 */

vpResult
vpSetDebug(vpc, flag, value)
vpContext *vpc;
int flag;
int value;
{
    if (flag < 0 || flag >= VPDEBUG_COUNT)
	return(VPSetError(vpc, VPERROR_BAD_OPTION));
#ifdef DEBUG
    vpc->debug_enable[flag] = value;
#endif
    return(VP_OK);
}

/*
 * vpTracePixel
 *
 * Trace one pixel of the intermediate image.
 */

vpResult
vpTracePixel(vpc, trace_u, trace_v)
vpContext *vpc;
int trace_u, trace_v;	/* pixel coordinates */
{
#ifdef DEBUG
    vpc->trace_u = trace_u;
    vpc->trace_v = trace_v;
#endif
    return(VP_OK);
}

/*
 * vpGeti
 *
 * Retrieve an integer-valued piece of state.
 */

vpResult
vpGeti(vpc, option, iptr)
vpContext *vpc;
int option;
int *iptr;
{
    int c;
    int retcode;

    switch (option) {
    case VP_XLEN:
	*iptr = vpc->xlen;
	break;
    case VP_YLEN:
	*iptr = vpc->ylen;
	break;
    case VP_ZLEN:
	*iptr = vpc->zlen;
	break;
    case VP_BYTES_PER_VOXEL:
	*iptr = vpc->raw_bytes_per_voxel;
	break;
    case VP_VOXEL_FIELD_COUNT:
	*iptr = vpc->num_voxel_fields;
	break;
    case VP_SHADE_FIELD_COUNT:
	*iptr = vpc->num_shade_fields;
	break;
    case VP_FIELD_SIZES:
	for (c = 0; c < vpc->num_voxel_fields; c++)
	    iptr[c] = vpc->field_size[c];
	break;
    case VP_FIELD_OFFSETS:
	for (c = 0; c < vpc->num_voxel_fields; c++)
	    iptr[c] = vpc->field_offset[c];
	break;
    case VP_FIELD_MAXES:
	for (c = 0; c < vpc->num_voxel_fields; c++)
	    iptr[c] = vpc->field_max[c];
	break;
    case VP_VOXEL_DATA_SIZE:
	*iptr = vpc->raw_voxels_size;
	break;
    case VP_VOXEL_XSTRIDE:
	*iptr = vpc->xstride;
	break;
    case VP_VOXEL_YSTRIDE:
	*iptr = vpc->ystride;
	break;
    case VP_VOXEL_ZSTRIDE:
	*iptr = vpc->zstride;
	break;
    case VP_CLASSIFY_FIELD_COUNT:
	*iptr = vpc->num_clsfy_params;
	break;
    case VP_CLASSIFY_FIELDS:
	for (c = 0; c < vpc->num_clsfy_params; c++)
	    iptr[c] = vpc->param_field[c];
	break;
    case VP_CLASSIFY_TABLE_SIZES:
	for (c = 0; c < vpc->num_clsfy_params; c++)
	    iptr[c] = vpc->clsfy_table_size[c];
	break;
    case VP_COLOR_CHANNELS:
	*iptr = vpc->color_channels;
	break;
    case VP_SHADE_COLOR_SIZE:
	*iptr = vpc->shade_color_table_size;
	break;
    case VP_SHADE_WEIGHT_SIZE:
	*iptr = vpc->shade_weight_table_size;
	break;
    case VP_MATERIAL_COUNT:
	*iptr = vpc->num_materials;
	break;
    case VP_SHADE_COLOR_FIELD:
	*iptr = vpc->color_field;
	break;
    case VP_SHADE_WEIGHT_FIELD:
	*iptr = vpc->weight_field;
	break;
    case VP_LIGHT0:
    case VP_LIGHT1:
    case VP_LIGHT2:
    case VP_LIGHT3:
    case VP_LIGHT4:
    case VP_LIGHT5:
	*iptr = vpc->light_enable[option - VP_LIGHT0];
	break;
    case VP_LIGHT_BOTH_SIDES:
	*iptr = vpc->light_both_sides;
	break;
    case VP_REVERSE_SURFACE_SIDES:
	*iptr = vpc->reverse_surface_sides;
	break;
    case VP_DEPTH_CUE:
	*iptr = vpc->dc_enable;
	break;
    case VP_DEPTH_CUE_TABLE_SIZE:
	*iptr = vpc->dc_table_len;
	break;
    case VP_DEPTH_CUE_SIZE_HINT:
	*iptr = vpc->dc_table_len_hint;
	break;
    case VP_CURRENT_MATRIX:
	*iptr = vpc->current_matrix;
	break;
    case VP_CONCAT_MODE:
	if (vpc->concat_left)
	    *iptr = VP_CONCAT_LEFT;
	else
	    *iptr = VP_CONCAT_RIGHT;
	break;
    case VP_IMAGE_WIDTH:
	*iptr = vpc->image_width;
	break;
    case VP_IMAGE_HEIGHT:
	*iptr = vpc->image_height;
	break;
    case VP_IMAGE_SCAN_SIZE:
	*iptr = vpc->image_bytes_per_scan;
	break;
    case VP_VIEW_AXIS:
	if ((retcode = VPFactorView(vpc)) != VP_OK)
	    return(retcode);
	*iptr = vpc->best_view_axis;
	break;
    case VP_INTERMEDIATE_WIDTH:
	if ((retcode = VPFactorView(vpc)) != VP_OK)
	    return(retcode);
	*iptr = vpc->intermediate_width;
	break;
    case VP_INTERMEDIATE_HEIGHT:
	if ((retcode = VPFactorView(vpc)) != VP_OK)
	    return(retcode);
	*iptr = vpc->intermediate_height;
	break;
    case VP_INTERMEDIATE_COLOR:
	if ((retcode = VPFactorView(vpc)) != VP_OK)
	    return(retcode);
	*iptr = vpc->intermediate_color_channels;
	break;
    case VP_INT_WIDTH_HINT:
	*iptr = vpc->int_image_width_hint;
	break;
    case VP_INT_HEIGHT_HINT:
	*iptr = vpc->int_image_height_hint;
	break;
    case VP_VIEW_X_AXIS:
	*iptr = !vpc->skip_rle_x;
	break;
    case VP_VIEW_Y_AXIS:
	*iptr = !vpc->skip_rle_y;
	break;
    case VP_VIEW_Z_AXIS:
	*iptr = !vpc->skip_rle_z;
	break;
    case VP_VIEW_X_SIZE:
	if (vpc->rle_x == NULL) {
	    *iptr = 0;
	} else {
	    *iptr = sizeof(RLEVoxels) + vpc->rle_x->run_count +
		    vpc->rle_x->data_count*vpc->rle_bytes_per_voxel +
		    vpc->rle_x->klen*vpc->rle_x->scan_offsets_per_slice*
			sizeof(ScanOffset);
	}
	break;
    case VP_VIEW_Y_SIZE:
	if (vpc->rle_y == NULL) {
	    *iptr = 0;
	} else {
	    *iptr = sizeof(RLEVoxels) + vpc->rle_y->run_count +
		    vpc->rle_y->data_count*vpc->rle_bytes_per_voxel +
		    vpc->rle_y->klen*vpc->rle_y->scan_offsets_per_slice*
			sizeof(ScanOffset);
	}
	break;
    case VP_VIEW_Z_SIZE:
	if (vpc->rle_z == NULL) {
	    *iptr = 0;
	} else {
	    *iptr = sizeof(RLEVoxels) + vpc->rle_z->run_count +
		    vpc->rle_z->data_count*vpc->rle_bytes_per_voxel +
		    vpc->rle_z->klen*vpc->rle_z->scan_offsets_per_slice*
			sizeof(ScanOffset);
	}
	break;
    case VP_MMOCTREE_THRESHOLDS:
	for (c = 0; c < vpc->num_clsfy_params; c++)
	    iptr[c] = vpc->param_maxrange[c];
	break;
    case VP_MMOCTREE_SIZE:
	if (vpc->mm_octree == NULL)
	    *iptr = 0;
	else
	    *iptr = sizeof(MinMaxOctree) + vpc->mm_octree->octree_bytes;
	break;
    case VP_SHADOW:
	*iptr = vpc->enable_shadows;
	break;
    case VP_SHADOW_LIGHT:
	*iptr = vpc->shadow_light_num;
	break;
    case VP_SHADOW_WIDTH_HINT:
	*iptr = vpc->shadow_width_hint;
	break;
    case VP_SHADOW_HEIGHT_HINT:
	*iptr = vpc->shadow_height_hint;
	break;
    case VP_SHADOW_WIDTH:
	if ((retcode = VPFactorView(vpc)) != VP_OK)
	    return(retcode);
	*iptr = vpc->shadow_width;
	break;
    case VP_SHADOW_HEIGHT:
	if ((retcode = VPFactorView(vpc)) != VP_OK)
	    return(retcode);
	*iptr = vpc->shadow_height;
	break;
    case VP_SHADOW_COLOR_SIZE:
	*iptr = vpc->shadow_color_table_size;
	break;
    case VP_SHADOW_BIAS:
	*iptr = vpc->shadow_bias;
	break;
    case VP_PIXEL_TYPE:
	*iptr = vpc->pixel_type;
	break;
    case VP_CLAMP_SHADE_TABLE:
	*iptr = vpc->clamp_shade_table;
	break;
    case VP_COMPOSITE_ORDER:
	if ((retcode = VPFactorView(vpc)) != VP_OK)
	    return(retcode);
	if (vpc->reverse_slice_order)
	    *iptr = -1;
	else
	    *iptr = 1;
	break;
    default:
	return(VPSetError(vpc, VPERROR_BAD_OPTION));
    }
    return(VP_OK);
}

/*
 * vpGetd
 *
 * Retrieve a double-precision-valued piece of state.
 */

vpResult
vpGetd(vpc, option, dptr)
vpContext *vpc;
int option;
double *dptr;
{
    int c;

    switch (option) {
    case VP_MIN_VOXEL_OPACITY:
	*dptr = vpc->min_opacity;
	break;
    case VP_DEPTH_CUE_FRONT:
	*dptr = vpc->dc_front_factor;
	break;
    case VP_DEPTH_CUE_DENSITY:
	*dptr = vpc->dc_density;
	break;
    case VP_DEPTH_CUE_QUANTIZATION:
	*dptr = vpc->dc_quantization;
	break;
    case VP_MAX_RAY_OPACITY:
	*dptr = vpc->max_opacity;
	break;
    default:
	return(VPSetError(vpc, VPERROR_BAD_OPTION));
    }
    return(VP_OK);
}

/*
 * vpGetp
 *
 * Retrieve a pointer-valued piece of state.
 */

vpResult
vpGetp(vpc, option, pptr)
vpContext *vpc;
int option;
void **pptr;
{
    int c;

    switch (option) {
    case VP_VOXEL_DATA:
	*pptr = vpc->raw_voxels;
	break;
    case VP_CLASSIFY_TABLES:
	for (c = 0; c < vpc->num_clsfy_params; c++)
	    pptr[c] = vpc->clsfy_table[c];
	break;
    case VP_SHADE_FUNC:
	*pptr = vpc->shade_func;
	break;
    case VP_SHADE_COLOR_TABLE:
	*pptr = vpc->shade_color_table;
	break;
    case VP_SHADE_WEIGHT_TABLE:
	*pptr = vpc->shade_weight_table;
	break;
    case VP_SHADOW_COLOR_TABLE:
	*pptr = vpc->shadow_color_table;
	break;
    case VP_IMAGE:
	*pptr = vpc->image;
	break;
    case VP_LOG_ALLOC_FUNC:
	*pptr = vpc->log_alloc_func;
	break;
    case VP_LOG_FREE_FUNC:
	*pptr = vpc->log_free_func;
	break;
    case VP_STATUS_FUNC:
	*pptr = vpc->status_func;
	break;
    case VP_READ_FUNC:
	*pptr = vpc->read_func;
	break;
    case VP_WRITE_FUNC:
	*pptr = vpc->write_func;
	break;
    case VP_MMAP_FUNC:
	*pptr = vpc->mmap_func;
	break;
    case VP_CLIENT_DATA:
	*pptr = vpc->client_data;
	break;
    default:
	return(VPSetError(vpc, VPERROR_BAD_OPTION));
    }
    return(VP_OK);
}

/*
 * vpGetMaterial
 *
 * Get material parameters.
 */

vpResult
vpGetMaterial(vpc, material, property, surface_side, r, g, b)
vpContext *vpc;
int material;
int property;
int surface_side;
double *r, *g, *b;
{
    material -= VP_MATERIAL0;
    if (material < 0 || material >= vpc->num_materials)
	return(VPSetError(vpc, VPERROR_BAD_VALUE));
    if (surface_side != VP_EXTERIOR && surface_side != VP_INTERIOR)
	return(VPSetError(vpc, VPERROR_BAD_OPTION));
    switch (property) {
    case VP_AMBIENT:
	if (surface_side == VP_EXTERIOR) {
	    *r = vpc->matl_props[material][EXT_SURFACE][MATL_AMB_R] / 255.;
	    *g = vpc->matl_props[material][EXT_SURFACE][MATL_AMB_G] / 255.;
	    *b = vpc->matl_props[material][EXT_SURFACE][MATL_AMB_B] / 255.;
	} else {
	    *r = vpc->matl_props[material][INT_SURFACE][MATL_AMB_R] / 255.;
	    *g = vpc->matl_props[material][INT_SURFACE][MATL_AMB_G] / 255.;
	    *b = vpc->matl_props[material][INT_SURFACE][MATL_AMB_B] / 255.;
	}
	break;
    case VP_DIFFUSE:
	if (surface_side == VP_EXTERIOR) {
	    *r = vpc->matl_props[material][EXT_SURFACE][MATL_DIFF_R] / 255.;
	    *g = vpc->matl_props[material][EXT_SURFACE][MATL_DIFF_G] / 255.;
	    *b = vpc->matl_props[material][EXT_SURFACE][MATL_DIFF_B] / 255.;
	} else {
	    *r = vpc->matl_props[material][INT_SURFACE][MATL_DIFF_R] / 255.;
	    *g = vpc->matl_props[material][INT_SURFACE][MATL_DIFF_G] / 255.;
	    *b = vpc->matl_props[material][INT_SURFACE][MATL_DIFF_B] / 255.;
	}
	break;
    case VP_SPECULAR:
	if (surface_side == VP_EXTERIOR) {
	    *r = vpc->matl_props[material][EXT_SURFACE][MATL_SPEC_R] / 255.;
	    *g = vpc->matl_props[material][EXT_SURFACE][MATL_SPEC_G] / 255.;
	    *b = vpc->matl_props[material][EXT_SURFACE][MATL_SPEC_B] / 255.;
	} else {
	    *r = vpc->matl_props[material][INT_SURFACE][MATL_SPEC_R] / 255.;
	    *g = vpc->matl_props[material][INT_SURFACE][MATL_SPEC_G] / 255.;
	    *b = vpc->matl_props[material][INT_SURFACE][MATL_SPEC_B] / 255.;
	}
	break;
    case VP_SHINYNESS:
	if (surface_side & VP_EXTERIOR)
	    *r = vpc->matl_props[material][EXT_SURFACE][MATL_SHINY];
	else
	    *r = vpc->matl_props[material][INT_SURFACE][MATL_SHINY];
	break;
    default:
	return(VPSetError(vpc, VPERROR_BAD_OPTION));
    }
    return(VP_OK);
}

/*
 * vpGetLight
 *
 * Get the properties of a directional light source.
 */

vpResult
vpGetLight(vpc, light_num, property, n0, n1, n2)
vpContext *vpc;
int light_num;
int property;
double *n0, *n1, *n2;
{
    light_num -= VP_LIGHT0;
    if (light_num < 0 || light_num >= VP_MAX_LIGHTS)
	return(VPSetError(vpc, VPERROR_LIMIT_EXCEEDED));
    switch (property) {
    case VP_DIRECTION:
	*n0 = vpc->light_vector[light_num][0];
	*n1 = vpc->light_vector[light_num][1];
	*n2 = vpc->light_vector[light_num][2];
	break;
    case VP_COLOR:
	*n0 = vpc->light_color[light_num][0];
	*n1 = vpc->light_color[light_num][1];
	*n2 = vpc->light_color[light_num][2];
	break;
    default:
	return(VPSetError(vpc, VPERROR_BAD_OPTION));
    }
    return(VP_OK);
}

/*
 * vpGetImage
 *
 * Get one of the intermediate rendering buffers.
 */

vpResult
vpGetImage(vpc, image, width, height, scan_bytes, pixel_type, image_type)
vpContext *vpc;		/* context */
void *image;		/* buffer for storing result */
int width;		/* expected width of image in buffer */
int height;		/* expected height of image in buffer */
int scan_bytes;		/* bytes per scanline in buffer */
int pixel_type;		/* type of pixel to store in buffer */
int image_type;		/* rendering buffer to extract from
			   (VP_IMAGE_BUFFER or VP_SHADOW_BUFFER) */
{
    int x, y;
    unsigned char *dst_ptr;
    GrayIntPixel *gray_pixel;
    RGBIntPixel *rgb_pixel;
    int value;
    int color_channels;

    switch (image_type) {
    case VP_IMAGE_BUFFER:
	if (width != vpc->intermediate_width ||
	    height != vpc->intermediate_height)
	    return(VPSetError(vpc, VPERROR_BAD_SIZE));
	color_channels = vpc->intermediate_color_channels;
	dst_ptr = image;
	switch (pixel_type) {
	case VP_ALPHA:
	    if (scan_bytes < width)
		return(VPSetError(vpc, VPERROR_BAD_SIZE));
	    if (color_channels == 1) {
		for (y = 0; y < height; y++) {
		    gray_pixel = vpc->int_image.gray_intim +
			(vpc->pad_int_to_maxwidth ?
			 vpc->max_intermediate_width*y :
			 vpc->intermediate_width*y);
		    dst_ptr = (unsigned char *)image + y * scan_bytes;
		    for (x = 0; x < width; x++) {
			value = (int)rint(gray_pixel->opcflt * 255.);
			if (value > 255)
			    *dst_ptr = 255;
			else
			    *dst_ptr = value;
			dst_ptr++;
			gray_pixel++;
		    }
		}
	    } else {
		for (y = 0; y < height; y++) {
		    rgb_pixel = vpc->int_image.rgb_intim +
			(vpc->pad_int_to_maxwidth ?
			 vpc->max_intermediate_width*y :
			 vpc->intermediate_width*y);
		    dst_ptr = (unsigned char *)image + y * scan_bytes;
		    for (x = 0; x < width; x++) {
			value = (int)rint(rgb_pixel->opcflt * 255.);
			if (value > 255)
			    *dst_ptr = 255;
			else
			    *dst_ptr = value;
			dst_ptr++;
			rgb_pixel++;
		    }
		}
	    }
	    break;
	case VP_LUMINANCE:
	    if (color_channels != 1)
		return(VPSetError(vpc, VPERROR_BAD_OPTION));
	    if (scan_bytes < width)
		return(VPSetError(vpc, VPERROR_BAD_SIZE));
	    for (y = 0; y < height; y++) {
		gray_pixel = vpc->int_image.gray_intim +
		    (vpc->pad_int_to_maxwidth ?
		     vpc->max_intermediate_width*y :
		     vpc->intermediate_width*y);
		dst_ptr = (unsigned char *)image + y * scan_bytes;
		for (x = 0; x < width; x++) {
		    value = (int)rint(gray_pixel->clrflt);
		    if (value > 255)
			*dst_ptr = 255;
		    else
			*dst_ptr = value;
		    dst_ptr++;
		    gray_pixel++;
		}
	    }
	    break;
	case VP_LUMINANCEA:
	    if (color_channels != 1)
		return(VPSetError(vpc, VPERROR_BAD_OPTION));
	    if (scan_bytes < 2*width)
		return(VPSetError(vpc, VPERROR_BAD_SIZE));
	    for (y = 0; y < height; y++) {
		gray_pixel = vpc->int_image.gray_intim +
		    (vpc->pad_int_to_maxwidth ?
		     vpc->max_intermediate_width*y :
		     vpc->intermediate_width*y);
		dst_ptr = (unsigned char *)image + y * scan_bytes;
		for (x = 0; x < width; x++) {
		    value = (int)rint(gray_pixel->clrflt);
		    if (value > 255)
			*dst_ptr = 255;
		    else
			*dst_ptr = value;
		    dst_ptr++;
		    value = (int)rint(gray_pixel->opcflt * 255.);
		    if (value > 255)
			*dst_ptr = 255;
		    else
			*dst_ptr = value;
		    dst_ptr++;
		    gray_pixel++;
		}
	    }
	    break;
	case VP_RGB:
	    if (color_channels != 3)
		return(VPSetError(vpc, VPERROR_BAD_OPTION));
	    if (scan_bytes < 3*width)
		return(VPSetError(vpc, VPERROR_BAD_SIZE));
	    for (y = 0; y < height; y++) {
		rgb_pixel = vpc->int_image.rgb_intim +
		    (vpc->pad_int_to_maxwidth ?
		     vpc->max_intermediate_width*y :
		     vpc->intermediate_width*y);
		dst_ptr = (unsigned char *)image + y * scan_bytes;
		for (x = 0; x < width; x++) {
		    value = (int)rint(rgb_pixel->rclrflt);
		    if (value > 255)
			*dst_ptr = 255;
		    else
			*dst_ptr = value;
		    dst_ptr++;
		    value = (int)rint(rgb_pixel->gclrflt);
		    if (value > 255)
			*dst_ptr = 255;
		    else
			*dst_ptr = value;
		    dst_ptr++;
		    value = (int)rint(rgb_pixel->bclrflt);
		    if (value > 255)
			*dst_ptr = 255;
		    else
			*dst_ptr = value;
		    dst_ptr++;
		    rgb_pixel++;
		}
	    }
	    break;
	case VP_RGBA:
	    if (color_channels != 3)
		return(VPSetError(vpc, VPERROR_BAD_OPTION));
	    if (scan_bytes < 4*width)
		return(VPSetError(vpc, VPERROR_BAD_SIZE));
	    for (y = 0; y < height; y++) {
		rgb_pixel = vpc->int_image.rgb_intim +
		    (vpc->pad_int_to_maxwidth ?
		     vpc->max_intermediate_width*y :
		     vpc->intermediate_width*y);
		dst_ptr = (unsigned char *)image + y * scan_bytes;
		for (x = 0; x < width; x++) {
		    value = (int)rint(rgb_pixel->rclrflt);
		    if (value > 255)
			*dst_ptr = 255;
		    else
			*dst_ptr = value;
		    dst_ptr++;
		    value = (int)rint(rgb_pixel->gclrflt);
		    if (value > 255)
			*dst_ptr = 255;
		    else
			*dst_ptr = value;
		    dst_ptr++;
		    value = (int)rint(rgb_pixel->bclrflt);
		    if (value > 255)
			*dst_ptr = 255;
		    else
			*dst_ptr = value;
		    dst_ptr++;
		    value = (int)rint(rgb_pixel->opcflt*255.);
		    if (value > 255)
			*dst_ptr = 255;
		    else
			*dst_ptr = value;
		    dst_ptr++;
		    rgb_pixel++;
		}
	    }
	    break;
	case VP_BGR:
	    if (color_channels != 3)
		return(VPSetError(vpc, VPERROR_BAD_OPTION));
	    if (scan_bytes < 3*width)
		return(VPSetError(vpc, VPERROR_BAD_SIZE));
	    for (y = 0; y < height; y++) {
		rgb_pixel = vpc->int_image.rgb_intim +
		    (vpc->pad_int_to_maxwidth ?
		     vpc->max_intermediate_width*y :
		     vpc->intermediate_width*y);
		dst_ptr = (unsigned char *)image + y * scan_bytes;
		for (x = 0; x < width; x++) {
		    value = (int)rint(rgb_pixel->bclrflt);
		    if (value > 255)
			*dst_ptr = 255;
		    else
			*dst_ptr = value;
		    dst_ptr++;
		    value = (int)rint(rgb_pixel->gclrflt);
		    if (value > 255)
			*dst_ptr = 255;
		    else
			*dst_ptr = value;
		    dst_ptr++;
		    value = (int)rint(rgb_pixel->rclrflt);
		    if (value > 255)
			*dst_ptr = 255;
		    else
			*dst_ptr = value;
		    dst_ptr++;
		    rgb_pixel++;
		}
	    }
	    break;
	case VP_ABGR:
	    if (color_channels != 3)
		return(VPSetError(vpc, VPERROR_BAD_OPTION));
	    if (scan_bytes < 4*width)
		return(VPSetError(vpc, VPERROR_BAD_SIZE));
	    for (y = 0; y < height; y++) {
		rgb_pixel = vpc->int_image.rgb_intim +
		    (vpc->pad_int_to_maxwidth ?
		     vpc->max_intermediate_width*y :
		     vpc->intermediate_width*y);
		dst_ptr = (unsigned char *)image + y * scan_bytes;
		for (x = 0; x < width; x++) {
		    value = (int)rint(rgb_pixel->opcflt*255.);
		    if (value > 255)
			*dst_ptr = 255;
		    else
			*dst_ptr = value;
		    dst_ptr++;
		    value = (int)rint(rgb_pixel->bclrflt);
		    if (value > 255)
			*dst_ptr = 255;
		    else
			*dst_ptr = value;
		    dst_ptr++;
		    value = (int)rint(rgb_pixel->gclrflt);
		    if (value > 255)
			*dst_ptr = 255;
		    else
			*dst_ptr = value;
		    dst_ptr++;
		    value = (int)rint(rgb_pixel->rclrflt);
		    if (value > 255)
			*dst_ptr = 255;
		    else
			*dst_ptr = value;
		    dst_ptr++;
		    rgb_pixel++;
		}
	    }
	    break;
	default:
	    return(VPSetError(vpc, VPERROR_BAD_OPTION));
	}
	break;
    case VP_SHADOW_BUFFER:
	if (pixel_type != VP_ALPHA)
	    return(VPSetError(vpc, VPERROR_BAD_OPTION));
	if (width != vpc->shadow_width || height != vpc->shadow_height)
	    return(VPSetError(vpc, VPERROR_BAD_SIZE));
	if (scan_bytes < width)
	    return(VPSetError(vpc, VPERROR_BAD_SIZE));
	for (y = 0; y < height; y++) {
	    gray_pixel = vpc->shadow_buffer + (vpc->pad_shadow_to_maxwidth ?
			vpc->max_shadow_width*y : vpc->shadow_width*y);
	    dst_ptr = (unsigned char *)image + y * scan_bytes;
	    for (x = 0; x < width; x++) {
		value = (int)rint(gray_pixel->opcflt * 255.);
		if (value > 255)
		    *dst_ptr = 255;
		else
		    *dst_ptr = value;
		dst_ptr++;
		gray_pixel++;
	    }
	}
	break;
    default:
	return(VPSetError(vpc, VPERROR_BAD_OPTION));
    }
    return(VP_OK);
}

/*
 * vpGetTimer
 *
 * Get the value of one of the timers.
 */

vpResult
vpGetTimer(vpc, option, iptr)
vpContext *vpc;
int option;
int *iptr;
{
    if (option < 0 || option >= VPTIMER_COUNT)
	return(VPSetError(vpc, VPERROR_BAD_OPTION));
#ifdef USE_TIMER
    *iptr = (int)(vpc->timer_ticks[option] * vpc->timer_usec_per_tick);
#else
    *iptr = 0;
#endif
    return(VP_OK);
}

/*
 * vpClearTimer
 *
 * Reset the value of one of the timers to zero.
 */

vpResult
vpClearTimer(vpc, option)
vpContext *vpc;
int option;
{
    if (option < 0 || option >= VPTIMER_COUNT)
	return(VPSetError(vpc, VPERROR_BAD_OPTION));
#ifdef USE_TIMER
    vpc->timer_ticks[option] = 0;
#endif
    return(VP_OK);
}

#ifdef HAVE_HIRES_TIMER
/*
 * StartHiResTimer
 *
 * Initialize the high-resolution memory mapped timer (available on
 * some models of SGI hardware).  On machines with a 64-bit timer
 * (e.g. Challenge or ONYX), HAVE_64BIT_TIMER must be defined for
 * proper operation.
 */

static void
StartHiResTimer(vpc)
vpContext *vpc;
{
    volatile unsigned timer_resolution; /* resolution of timer in psec. */
    unsigned phys_addr;		/* hardware address of timer */
    unsigned page_addr;		/* address of page containing timer */
    int fd;			/* file descriptor for file to be mapped */
    volatile unsigned *timer_addr; /* memory-mapped address of timer */


    /* set values to harmless defaults in case hardware doesn't really
       support a high-resolution timer */
    vpc->timer_usec_per_tick = 0.;
    vpc->timer_addr = &vpc->dummy_timer;
    vpc->dummy_timer = 0;

    phys_addr = syssgi(SGI_QUERY_CYCLECNTR, &timer_resolution);
    if ((int)phys_addr == -1)
	return;
    if ((fd = open("/dev/mmem", O_RDONLY)) < 0)
	return;
    page_addr = phys_addr & ~POFFMASK;
    timer_addr = (volatile unsigned *)mmap(0, POFFMASK, PROT_READ,
					   MAP_PRIVATE, fd, (int)page_addr);
    close(fd);
    if ((int)timer_addr == -1)
	return;
    vpc->timer_addr = (unsigned *)((unsigned)timer_addr + poff(phys_addr));
#ifdef HAVE_64BIT_TIMER
    vpc->timer_addr++;
    printf("Timer configured for 64 bits.\n");
#endif
    vpc->timer_usec_per_tick = timer_resolution * 1.0e-6;
    printf("Timer resolution is %d psec.\n", timer_resolution);
}
#endif /* HAVE_HIRES_TIMER */
