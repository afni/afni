/*
 * vp_view.c
 *
 * Routines to compute quantities derived from the view transformation.
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

static int FactorAffineView ANSI_ARGS((vpContext *vpc, vpMatrix4 vm));
static int FactorPerspectiveView ANSI_ARGS((vpContext *vpc, vpMatrix4 vm));
static void ComputeAffineOpacityCorrection ANSI_ARGS((vpContext *vpc,
    double shear_i, double shear_j, float table[VP_OPACITY_MAX+1]));
static void CheckRenderBuffers ANSI_ARGS((vpContext *vpc));
static void ComputeLightViewTransform ANSI_ARGS((vpContext *vpc,vpMatrix4 vm));
static int FactorLightView ANSI_ARGS((vpContext *vpc, vpMatrix4 vm));
static void CheckShadowBuffer ANSI_ARGS((vpContext *vpc));

/*
 * VPFactorView
 *
 * Factor the viewing matrix.
 */

vpResult
VPFactorView(vpc)
vpContext *vpc;
{
    vpMatrix4 vm;
    int retcode;

    if (vpc->factored_view_ready) {
	CheckRenderBuffers(vpc);
	return(VP_OK);
    }

    /* compute the overall view transformation */
    VPComputeViewTransform(vpc, vm);

    /* check if transformation is affine and factor it */
    if (fabs(vm[3][0]) < VP_EPS && fabs(vm[3][1]) < VP_EPS &&
	fabs(vm[3][2]) < VP_EPS && fabs(vm[3][3]-1.) < VP_EPS) {
	if ((retcode = FactorAffineView(vpc, vm)) != VP_OK)
	    return(retcode);
	ComputeAffineOpacityCorrection(vpc, vpc->shear_i, vpc->shear_j,
				       vpc->affine_opac_correct);
    } else {
	FactorPerspectiveView(vpc, vm);
    }
    CheckRenderBuffers(vpc);

    /* compute viewing transformation from the point of view of the light
       source (for calculating shadows) */
    if ((retcode = VPCheckShadows(vpc)) != VP_OK)
	return(retcode);
    if (vpc->enable_shadows) {
	ComputeLightViewTransform(vpc, vm);
	if ((retcode = FactorLightView(vpc, vm)) != VP_OK)
	    return(retcode);
	ComputeAffineOpacityCorrection(vpc, vpc->shadow_shear_i,
				       vpc->shadow_shear_j,
				       vpc->shadow_opac_correct);
	CheckShadowBuffer(vpc);
    }
    return(VP_OK);
}

/*
 * VPComputeViewTransform
 *
 * Compute the overall view transformation.
 */

void
VPComputeViewTransform(vpc, vm)
vpContext *vpc;
vpMatrix4 vm;	/* storage for result */
{
    vpMatrix4 prematrix;		/* transform volume to unit cube */
    vpMatrix4 viewportm;		/* viewport matrix */
    vpMatrix4 tmp1m, tmp2m, tmp3m;	/* temporary matrices */
    int maxdim;
    double scale;

    /* compute prematrix */
    vpIdentity4(prematrix);
    maxdim = vpc->xlen;
    if (vpc->ylen > maxdim)
	maxdim = vpc->ylen;
    if (vpc->zlen > maxdim)
	maxdim = vpc->zlen;
    scale = 1. / (double)maxdim;
    prematrix[0][0] = scale;
    prematrix[1][1] = scale;
    prematrix[2][2] = scale;
    prematrix[0][3] = -vpc->xlen * scale * 0.5;
    prematrix[1][3] = -vpc->ylen * scale * 0.5;
    prematrix[2][3] = -vpc->zlen * scale * 0.5;

    /* compute the viewport matrix */
    vpIdentity4(viewportm);
    viewportm[0][0] = 0.5 * vpc->image_width;
    viewportm[0][3] = 0.5 * vpc->image_width;
    viewportm[1][1] = 0.5 * vpc->image_height;
    viewportm[1][3] = 0.5 * vpc->image_height;
    viewportm[2][2] = -0.5;	/* minus sign: switch to left-handed coords. */
    viewportm[2][3] = 0.5;

    /* compute the view matrix */
    vpMatrixMult4(tmp1m, vpc->transforms[VP_MODEL], prematrix);
    vpMatrixMult4(tmp2m, vpc->transforms[VP_VIEW], tmp1m);
    vpMatrixMult4(tmp3m, vpc->transforms[VP_PROJECT], tmp2m);
    vpMatrixMult4(vm, viewportm, tmp3m);
}

/*
 * FactorAffineView
 *
 * Factor an affine viewing matrix into two parts:
 *  1) A shear and translation which map object coordinates into
 *     intermediate image coordinates.
 *  2) An affine warp which transforms intermediate image coordinates to
 *     image coordinates.
 * Return value is a result code.
 */

static int
FactorAffineView(vpc, vm)
vpContext *vpc;
vpMatrix4 vm;
{
    vpMatrix4 p;		/* permutation matrix */
    vpMatrix4 pvm;		/* permutation of the viewing matrix */
    int icount, jcount, kcount;	/* dimensions of volume in rotated space */
    vpVector4 xobj, yobj, zobj;
    vpVector4 xim, yim, zim;
    double x, y, z, denom;

    Debug((vpc, VPDEBUG_VIEW, "FactorAffineView\n"));

    vpc->affine_view = 1;

    /*
     * Transform the unit x, y and z object-coordinate vectors into image
     * space and see which one is most aligned with the view direction
     * (which is the z-axis in image coordinates).
     */
    vpSetVector4(xobj, 1., 0., 0., 0.);
    vpSetVector4(yobj, 0., 1., 0., 0.);
    vpSetVector4(zobj, 0., 0., 1., 0.);
    vpMatrixVectorMult4(xim, vm, xobj);
    vpMatrixVectorMult4(yim, vm, yobj);
    vpMatrixVectorMult4(zim, vm, zobj);
    x = fabs((vpNormalize3(xim) == VPERROR_SINGULAR) ? 0. : xim[2]);
    y = fabs((vpNormalize3(yim) == VPERROR_SINGULAR) ? 0. : yim[2]);
    z = fabs((vpNormalize3(zim) == VPERROR_SINGULAR) ? 0. : zim[2]);
    if (x >= y) {
	if (x >= z) {
	    vpc->best_view_axis = VP_X_AXIS;
	} else {
	    vpc->best_view_axis = VP_Z_AXIS;
	}
    } else {
	if (y >= z) {
	    vpc->best_view_axis = VP_Y_AXIS;
	} else {
	    vpc->best_view_axis = VP_Z_AXIS;
	}
    }
    switch (vpc->axis_override) {
    case VP_X_AXIS:
	if (x < VP_EPS)
	    return(VPSetError(vpc, VPERROR_SINGULAR));
	vpc->best_view_axis = VP_X_AXIS;
	break;
    case VP_Y_AXIS:
	if (y < VP_EPS)
	    return(VPSetError(vpc, VPERROR_SINGULAR));
	vpc->best_view_axis = VP_Y_AXIS;
	break;
    case VP_Z_AXIS:
	if (z < VP_EPS)
	    return(VPSetError(vpc, VPERROR_SINGULAR));
	vpc->best_view_axis = VP_Z_AXIS;
	break;
    default:
	break;
    }

    /* permute the rows of the viewing matrix so that the third axis is
       most parallel to the viewing direction */
    bzero(p, sizeof(vpMatrix4));
    switch (vpc->best_view_axis) {
    case VP_X_AXIS:
	p[0][2] = 1.;
	p[1][0] = 1.;
	p[2][1] = 1.;
	p[3][3] = 1.;
	icount = vpc->ylen;
	jcount = vpc->zlen;
	kcount = vpc->xlen;
	break;
    case VP_Y_AXIS:
	p[0][1] = 1.;
	p[1][2] = 1.;
	p[2][0] = 1.;
	p[3][3] = 1.;
	icount = vpc->zlen;
	jcount = vpc->xlen;
	kcount = vpc->ylen;
	break;
    case VP_Z_AXIS:
	p[0][0] = 1.;
	p[1][1] = 1.;
	p[2][2] = 1.;
	p[3][3] = 1.;
	icount = vpc->xlen;
	jcount = vpc->ylen;
	kcount = vpc->zlen;
	break;
    }
    vpMatrixMult4(pvm, vm, p);

    /* compute the shear coefficients */
    denom = pvm[0][0]*pvm[1][1] - pvm[0][1]*pvm[1][0];
    if (fabs(denom) < VP_EPS)
	return(VPSetError(vpc, VPERROR_SINGULAR));
    vpc->shear_i = (pvm[0][2]*pvm[1][1] - pvm[0][1]*pvm[1][2]) / denom;
    vpc->shear_j = (pvm[0][0]*pvm[1][2] - pvm[1][0]*pvm[0][2]) / denom;
    if (pvm[2][0]*vpc->shear_i + pvm[2][1]*vpc->shear_j - pvm[2][2] > 0)
	vpc->reverse_slice_order = 0;
    else
	vpc->reverse_slice_order = 1;

    /* compute the intermediate image size */
    vpc->intermediate_width = icount + 1 + (int)ceil((kcount-1)*
			      fabs(vpc->shear_i));
    vpc->intermediate_height = jcount + 1 + (int)ceil((kcount-1)*
			       fabs(vpc->shear_j));

    /* compute the translation coefficients */
    if (vpc->shear_i >= 0.)
	vpc->trans_i = 1.;
    else
	vpc->trans_i = 1. - vpc->shear_i * (kcount - 1);
    if (vpc->shear_j >= 0.)
	vpc->trans_j = 1.;
    else
	vpc->trans_j = 1. - vpc->shear_j * (kcount - 1);

    /* compute the depth coefficients */
    vpc->depth_di = pvm[2][0];
    vpc->depth_dj = pvm[2][1];
    vpc->depth_dk = pvm[2][2];
    vpc->depth_000 = pvm[2][3];

    /* compute the mapping from compositing space to image space */
    vpc->warp_2d[0][0] = pvm[0][0];
    vpc->warp_2d[0][1] = pvm[0][1];
    vpc->warp_2d[0][2] = pvm[0][3] - pvm[0][0]*vpc->trans_i -
			 pvm[0][1]*vpc->trans_j;
    vpc->warp_2d[1][0] = pvm[1][0];
    vpc->warp_2d[1][1] = pvm[1][1];
    vpc->warp_2d[1][2] = pvm[1][3] - pvm[1][0]*vpc->trans_i - 
			 pvm[1][1]*vpc->trans_j;
    vpc->warp_2d[2][0] = 0.;
    vpc->warp_2d[2][1] = 0.;
    vpc->warp_2d[2][2] = 1.;

    vpc->factored_view_ready = 1;

    Debug((vpc, VPDEBUG_VIEW, "  best_view_axis: %c%c\n", 
	   vpc->reverse_slice_order ? '-' : '+',
	   vpc->best_view_axis == VP_X_AXIS ? 'x' :
	   (vpc->best_view_axis == VP_Y_AXIS ? 'y' : 'z')));
    Debug((vpc, VPDEBUG_VIEW, "  shear factors: %g %g\n",
	   vpc->shear_i, vpc->shear_j));
    Debug((vpc, VPDEBUG_VIEW, "  translation: %g %g\n",
	   vpc->trans_i, vpc->trans_j));
    Debug((vpc, VPDEBUG_VIEW, "  depth: d000: %g\n", vpc->depth_000));
    Debug((vpc, VPDEBUG_VIEW, "         di:   %g\n", vpc->depth_di));
    Debug((vpc, VPDEBUG_VIEW, "         dj:   %g\n", vpc->depth_dj));
    Debug((vpc, VPDEBUG_VIEW, "         dk:   %g\n", vpc->depth_dk));
    Debug((vpc, VPDEBUG_VIEW, "  intermediate image size: %d %d\n",
	   vpc->intermediate_width, vpc->intermediate_height));
    return(VP_OK);
}

/*
 * FactorPerspectiveView
 *
 * Factor a perspective view matrix into two parts:
 *  1) A shear, translation and scale which map object coordinates into
 *     intermediate image coordinates.
 *  2) A perspective warp which transforms intermediate image coordinates to
 *     image coordinates.
 */

static int
FactorPerspectiveView(vpc, vm)
vpContext *vpc;
vpMatrix4 vm;
{
    vpc->affine_view = 0;
    return(VP_OK);

#ifdef notdef
    Matrix4 p;		/* permutation matrix */
    Matrix4 pvm;	/* permutation of the viewing matrix */
    Matrix4 m2d;	/* final warp */
    Matrix4 t;
    double alpha1, alpha2, alpha3, alpha4;
    int icount, jcount, kcount;	/* dimensions of volume in rotated space */
    Vector4 xobj, yobj, zobj;
    double x, y, z, denom;
    double i0, j0, i1, j1;
    double imin, imax, jmin, jmax;

    Debug((DEBUG_VIEW, "FactorPerspectiveView\n"));

    rbuf->perspective_proj = 1;

    /*
     * Transform the unit x, y and z object-coordinate vectors into image
     * space and see which one is most aligned with the view direction
     * (which is the z-axis in image coordinates).
     */
    xobj[0] = 1.; xobj[1] = 0.; xobj[2] = 0.; xobj[3] = 0.;
    yobj[0] = 0.; yobj[1] = 1.; yobj[2] = 0.; yobj[3] = 0.;
    zobj[0] = 0.; zobj[1] = 0.; zobj[2] = 1.; zobj[3] = 0.;
    TransformVector4(xobj, view->view_matrix);
    TransformVector4(yobj, view->view_matrix);
    TransformVector4(zobj, view->view_matrix);
    /* normalize each vector to unit length and compare the absolute value
       of the z component; note that the w component drops out */
    xobj[2] = fabs(xobj[2]);
    yobj[2] = fabs(yobj[2]);
    zobj[2] = fabs(zobj[2]);
    x = (xobj[2] < EPS) ? 0. :
	(xobj[2] / sqrt(xobj[0]*xobj[0] + xobj[1]*xobj[1] + xobj[2]*xobj[2]));
    y = (yobj[2] < EPS) ? 0. :
	(yobj[2] / sqrt(yobj[0]*yobj[0] + yobj[1]*yobj[1] + yobj[2]*yobj[2]));
    z = (zobj[2] < EPS) ? 0. :
	(zobj[2] / sqrt(zobj[0]*zobj[0] + zobj[1]*zobj[1] + zobj[2]*zobj[2]));
    if (x >= y) {
	if (x >= z) {
	    rbuf->best_view_axis = VP_XAXIS;
	} else {
	    rbuf->best_view_axis = VP_ZAXIS;
	}
    } else {
	if (y >= z) {
	    rbuf->best_view_axis = VP_YAXIS;
	} else {
	    rbuf->best_view_axis = VP_ZAXIS;
	}
    }

    /* permute the rows of the viewing matrix so that the third axis is
       most parallel to the viewing direction */
    bzero(p, sizeof(Matrix4));
    switch (rbuf->best_view_axis) {
    case VP_XAXIS:
	p[0][2] = 1.;
	p[1][0] = 1.;
	p[2][1] = 1.;
	p[3][3] = 1.;
	icount = ylen;
	jcount = zlen;
	kcount = xlen;
	break;
    case VP_YAXIS:
	p[0][1] = 1.;
	p[1][2] = 1.;
	p[2][0] = 1.;
	p[3][3] = 1.;
	icount = zlen;
	jcount = xlen;
	kcount = ylen;
	break;
    case VP_ZAXIS:
	p[0][0] = 1.;
	p[1][1] = 1.;
	p[2][2] = 1.;
	p[3][3] = 1.;
	icount = xlen;
	jcount = ylen;
	kcount = zlen;
	break;
    default:
	VPBug("wierd value for best_view_axis in FactorPerspectiveView\n");
    }
    MatrixMult4(pvm, view->view_matrix, p);

    /* compute the magic alpha coefficients */
    alpha1 = pvm[3][1] * (pvm[0][3]*pvm[1][2] - pvm[0][2]*pvm[1][3]) +
	     pvm[3][2] * (pvm[0][1]*pvm[1][3] - pvm[0][3]*pvm[1][1]) +
	     pvm[3][3] * (pvm[0][2]*pvm[1][1] - pvm[0][1]*pvm[1][2]);
    alpha2 = pvm[3][0] * (pvm[0][2]*pvm[1][3] - pvm[0][3]*pvm[1][2]) +
	     pvm[3][2] * (pvm[0][3]*pvm[1][0] - pvm[0][0]*pvm[1][3]) +
	     pvm[3][3] * (pvm[0][0]*pvm[1][2] - pvm[0][2]*pvm[1][0]);
    alpha3 = pvm[3][0] * (pvm[0][1]*pvm[1][2] - pvm[0][2]*pvm[1][1]) +
	     pvm[3][1] * (pvm[0][2]*pvm[1][0] - pvm[0][0]*pvm[1][2]) +
	     pvm[3][2] * (pvm[0][0]*pvm[1][1] - pvm[0][1]*pvm[1][0]);
    alpha4 = pvm[3][0] * (pvm[0][1]*pvm[1][3] - pvm[0][3]*pvm[1][1]) +
	     pvm[3][1] * (pvm[0][3]*pvm[1][0] - pvm[0][0]*pvm[1][3]) +
	     pvm[3][3] * (pvm[0][0]*pvm[1][1] - pvm[0][1]*pvm[1][0]);

    /* determine the order of the slices */
    if (pvm[2][2] - (pvm[2][0]*alpha1 + pvm[2][1]*alpha2)/(alpha3+alpha4) > 0)
	rbuf->reverse_k_order = 1;
    else
	rbuf->reverse_k_order = 0;

    /* compute the scale coefficients */
    rbuf->w_factor = alpha3 / alpha4;
    if (rbuf->reverse_k_order)
	rbuf->normalize_scale = 1. + rbuf->w_factor*(kcount-1);
    else
	rbuf->normalize_scale = 1.;

    /* compute the bounding box of the image in compositing space */
    denom = 1. / (alpha4 + alpha3*(kcount-1));
    i0 = rbuf->normalize_scale*alpha1*(kcount-1) * denom;
    j0 = rbuf->normalize_scale*alpha2*(kcount-1) * denom;
    i1 = rbuf->normalize_scale*(alpha4*icount + alpha1*(kcount-1)) * denom;
    j1 = rbuf->normalize_scale*(alpha4*jcount + alpha2*(kcount-1)) * denom;
    imin = MIN(0, i0);
    imax = MAX(rbuf->normalize_scale*icount, i1);
    jmin = MIN(0, j0);
    jmax = MAX(rbuf->normalize_scale*jcount, j1);

    /* compute the size of the intermediate image */
    rbuf->intermediate_width = (int)ceil(imax - imin);
    rbuf->intermediate_height = (int)ceil(jmax - jmin);

    /* compute the translation and shear coefficients */
    rbuf->shear_i = (rbuf->normalize_scale*alpha1 - alpha3*imin) / alpha4;
    rbuf->shear_j = (rbuf->normalize_scale*alpha2 - alpha3*jmin) / alpha4;
    rbuf->trans_i = -imin;
    rbuf->trans_j = -jmin;

    /* compute the depth coefficients */
    rbuf->depth_di = pvm[2][0];
    rbuf->depth_dj = pvm[2][1];
    rbuf->depth_dk = pvm[2][2];
    rbuf->depth_000 = pvm[2][3];
    rbuf->w_di = pvm[3][0];
    rbuf->w_dj = pvm[3][1];
    rbuf->w_dk = pvm[3][2];
    rbuf->w_000 = pvm[3][3];

    /* compute the mapping from compositing space to image space */
    Identity4(t);
    t[0][0] = 1. / rbuf->normalize_scale;
    t[1][1] = 1. / rbuf->normalize_scale;
    t[0][2] = -alpha1 / alpha4;
    t[1][2] = -alpha2 / alpha4;
    t[3][2] = -alpha3 / alpha4;
    t[0][3] = imin / rbuf->normalize_scale;
    t[1][3] = jmin / rbuf->normalize_scale;
    MatrixMult4(m2d, pvm, t);

    rbuf->warp_2d[0][0] = m2d[0][0];
    rbuf->warp_2d[1][0] = m2d[1][0];
    rbuf->warp_2d[2][0] = m2d[3][0];
    rbuf->warp_2d[0][1] = m2d[0][1];
    rbuf->warp_2d[1][1] = m2d[1][1];
    rbuf->warp_2d[2][1] = m2d[3][1];
    rbuf->warp_2d[0][2] = m2d[0][3];
    rbuf->warp_2d[1][2] = m2d[1][3];
    rbuf->warp_2d[2][2] = m2d[3][3];
#endif /* notdef */
}

/*
 * ComputeAffineOpacityCorrection
 *
 * Precompute a lookup table which corrects opacity for an affine viewing
 * transformation.  (Opacity correction accounts for variations in the
 * apparent thickness of a voxel depending on viewpoint.)
 */

static void
ComputeAffineOpacityCorrection(vpc, shear_i, shear_j, table)
vpContext *vpc;
double shear_i;
double shear_j;
float table[VP_OPACITY_MAX+1];
{
    float voxel_size;
    int i;

    Debug((vpc, VPDEBUG_OPCCORRECT,
	   "Computing affine opacity correction table.\n"));
    voxel_size = sqrt(1 + shear_i*shear_i + shear_j*shear_j);
    for (i = 0; i <= VP_OPACITY_MAX; i++) {
#ifdef NO_OPAC_CORRECT
	table[i] = (double)i / (double)VP_OPACITY_MAX;
#else
	table[i] = 1.-pow(1.-(double)i/(double)VP_OPACITY_MAX,voxel_size);
#endif
    }
}

/*
 * CheckRenderBuffers
 *
 * Resize the buffers used during rendering, if necessary.
 */

static void
CheckRenderBuffers(vpc)
vpContext *vpc;
{
    int new_max_width, new_max_height, new_max_scan;
    int resize = 0;

    /* determine if resizing is necessary */
    if (vpc->intermediate_width > vpc->max_intermediate_width) {
	new_max_width = MAX(vpc->intermediate_width,
			    vpc->int_image_width_hint);
	resize = 1;
    } else {
	new_max_width = MAX(vpc->max_intermediate_width,
			    vpc->int_image_width_hint);
    }
    if (vpc->intermediate_height > vpc->max_intermediate_height) {
	new_max_height = MAX(vpc->intermediate_height,
			     vpc->int_image_height_hint);
	resize = 1;
    } else {
	new_max_height = MAX(vpc->max_intermediate_height,
			     vpc->int_image_height_hint);
    }
    new_max_scan = vpc->xlen;
    if (vpc->ylen > new_max_scan)
	new_max_scan = vpc->ylen;
    if (vpc->zlen > new_max_scan)
	new_max_scan = vpc->zlen;
    if (new_max_scan > vpc->max_scan_length)
	resize = 1;
    if (vpc->color_channels != vpc->intermediate_color_channels)
	resize = 1;

    /* resize */
    if (resize)
	VPResizeRenderBuffers(vpc, new_max_width, new_max_height,new_max_scan);
}

/*
 * VPResizeRenderBuffers
 *
 * Resize the rendering buffers.
 */

void
VPResizeRenderBuffers(vpc, max_width, max_height, max_scan)
vpContext *vpc;
int max_width;	/* new width of the intermediate image */
int max_height;	/* new height of the intermediate image */
int max_scan;	/* new max. scanline length */
{
    /* free old buffers */
    if (vpc->int_image.gray_intim != NULL) {
	Debug((vpc, VPDEBUG_RBUF, "Freeing old RenderBuffer(%d,%d,%d,%d)\n",
	       vpc->max_intermediate_width, vpc->max_intermediate_height,
	       vpc->max_scan_length, vpc->intermediate_color_channels));
	Dealloc(vpc, vpc->int_image.gray_intim);
    }

    /* allocate new buffers */
    Debug((vpc, VPDEBUG_RBUF, "Allocating RenderBuffer(%d,%d,%d,%d)\n",
	   max_width, max_height, max_scan, vpc->color_channels));
    vpc->max_intermediate_width = max_width;
    vpc->max_intermediate_height = max_height;
    vpc->max_scan_length = max_scan;
    vpc->intermediate_color_channels = vpc->color_channels;
    if (max_width > 0) {
	if (vpc->color_channels == 1) {
	    Alloc(vpc, vpc->int_image.gray_intim, GrayIntPixel *,
		  max_width * max_height * sizeof(GrayIntPixel), "int_image");
	} else {
	    Alloc(vpc, vpc->int_image.rgb_intim, RGBIntPixel *,
		  max_width * max_height * sizeof(RGBIntPixel), "int_image");
	}
    } else {
	vpc->int_image.gray_intim = NULL;
    }
}

/*
 * VPResizeDepthCueTable
 *
 * Resize the depth cueing table.
 */

void
VPResizeDepthCueTable(vpc, entries, copy)
vpContext *vpc;
int entries;	/* new number of table entries */
int copy;	/* if true, copy old entries */
{
    float *new_dc_table;

    Debug((vpc, VPDEBUG_DEPTHCUE, "resizing dctable to %d entries (%s)\n",
	   entries, copy ? "copy" : "nocopy"));
    if (entries == 0) {
	if (vpc->dc_table != NULL) {
	    Dealloc(vpc, vpc->dc_table);
	    vpc->dc_table = NULL;
	}
	vpc->dc_table_len = 0;
    } else {
	Alloc(vpc, new_dc_table, float *, entries * sizeof(float), "dc_table");
	if (vpc->dc_table != NULL) {
	    if (copy && vpc->dc_table_len > 0) {
		bcopy(vpc->dc_table, new_dc_table,
		      MIN(vpc->dc_table_len, entries) * sizeof(float));
	    }
	    Dealloc(vpc, vpc->dc_table);
	}
	vpc->dc_table = new_dc_table;
	vpc->dc_table_len = entries;
    }
}

/*
 * VPComputeDepthCueTable
 *
 * Compute entries in the depth cueing lookup table.  
 */

void
VPComputeDepthCueTable(vpc, first, last)
vpContext *vpc;
int first;	/* first entry to compute */
int last;	/* last entry to compute */
{
    int c;
    double delta_depth, front_factor, density;

    Debug((vpc, VPDEBUG_DEPTHCUE, "computing dctable entries %d to %d\n",
	   first, last));
    delta_depth = vpc->dc_quantization;
    front_factor = vpc->dc_front_factor;
    density = vpc->dc_density;
    for (c = first; c <= last; c++)
	vpc->dc_table[c] = front_factor * exp(-density*(1.0 - c*delta_depth));
}

/*
 * VPSliceDepthCueRatio
 *
 * Return the ratio of the depth cueing factor for two adjacent slices
 * for an affine view.  A constant factor is applied to all voxels in a
 * slice, and then a fixup is applied to the pixels of the intermediate
 * image.  This produces the correct answer without having to compute
 * the depth of each voxel.
 */

float
VPSliceDepthCueRatio(vpc)
vpContext *vpc;
{
    float delta_depth;		/* change in depth between adjacent slices */
    float slice_dc_ratio;	/* return value */

    if (!vpc->dc_enable)
	return(1.);
    delta_depth = vpc->depth_dk - vpc->depth_di*vpc->shear_i -
		  vpc->depth_dj*vpc->shear_j;
    if (vpc->reverse_slice_order)
	delta_depth = -delta_depth;
    /* slice_dc_ratio = exp(-vpc->dc_density * (-delta_depth)) */
    slice_dc_ratio = exp(vpc->dc_density * delta_depth);
    Debug((vpc, VPDEBUG_DEPTHCUE, "slice_dc_ratio = %f\n", slice_dc_ratio));
    return(slice_dc_ratio);
}

/*
 * VPDepthCueIntImage
 *
 * Perform depth cueing on the intermediate image.
 */

void
VPDepthCueIntImage(vpc, slicenum)
vpContext *vpc;
int slicenum;	/* slice number corresponding to location of int. image */
{
    float pixel_depth_quant;	/* depth of current pixel in image
				   (multiplied by depth_quant) */
    int pixel_depth_int;	/* pixel_depth truncated to an integer */
    float left_depth;		/* depth of pixel on left edge of current
				   scanline in image */
    float left_depth_quant;	/* left_depth * depth_quant */
    float *dc_table;		/* depth cueing table */
    float depth_di, depth_dj;	/* change in depth for a unit change in each
				   rotated object space coordinate */
    float depth_quant;		/* number of quantization levels for depth */
    float depth_di_quant;	/* depth_di * depth_quant */
    float depth_dj_quant;	/* depth_dj * depth_quant */
    float max_depth;		/* maximum (closest) depth in image */
    int max_depth_int;		/* maximum quantized depth */
    int i, j;			/* intermediate image coordinates */
    float slice_u, slice_v;	/* sheared object space coordinates */
    GrayIntPixel *gray_intim;	/* image data (grayscale) */
    RGBIntPixel *rgb_intim;	/* image data (RGB) */
    int width, height;		/* size of intermediate image */
    int c;
#ifdef DEBUG
    float pix_depth;
#endif

    Debug((vpc, VPDEBUG_DEPTHCUE, "depth cueing intermediate image\n"));

    /* check the size of the depth cueing table and enlarge if necessary */
    width = vpc->intermediate_width;
    height = vpc->intermediate_height;
    depth_quant = 1.0 / vpc->dc_quantization;
    depth_di = vpc->depth_di;
    depth_dj = vpc->depth_dj;
    slice_u = vpc->shear_i * slicenum + vpc->trans_i;
    slice_v = vpc->shear_j * slicenum + vpc->trans_j;
    left_depth = vpc->depth_000 + vpc->depth_dk*slicenum -
		 slice_u*depth_di - slice_v*depth_dj;
    if (depth_di > 0) {
	if (depth_dj > 0) {
	    max_depth = left_depth + depth_di*width + depth_dj*height;
	} else {
	    max_depth = left_depth + depth_di*width;
	}
    } else {
	if (depth_dj > 0) {
	    max_depth = left_depth + depth_dj*height;
	} else {
	    max_depth = left_depth;
	}
    }
    max_depth_int = max_depth * depth_quant;
    if (max_depth_int >= vpc->dc_table_len) {
	c = vpc->dc_table_len;
	VPResizeDepthCueTable(vpc, max_depth_int+1, 1);
	VPComputeDepthCueTable(vpc, c, vpc->dc_table_len-1);
    }
    dc_table = vpc->dc_table;
    depth_di_quant = depth_di * depth_quant;
    depth_dj_quant = depth_dj * depth_quant;
    left_depth_quant = left_depth * depth_quant;

#ifdef DEBUG
    Debug((vpc, VPDEBUG_DEPTHCUE, "depth cueing at image corners:\n"));
    pix_depth = left_depth + 0*depth_di + 0*depth_dj;
    pixel_depth_int = (int)(pix_depth * depth_quant);
    if (pixel_depth_int < 0) pixel_depth_int = 0;
    Debug((vpc, VPDEBUG_DEPTHCUE,
	   "  %3d %3d: depth = %10.6f, factor = %10.6f, table[%d] = %10.6f\n",
	   0, 0, pix_depth,
	   vpc->dc_front_factor * exp(-vpc->dc_density * (1.0 - pix_depth)),
	   pixel_depth_int, dc_table[pixel_depth_int]));
    pix_depth = left_depth + width*depth_di + 0*depth_dj;
    pixel_depth_int = (int)(pix_depth * depth_quant);
    if (pixel_depth_int < 0) pixel_depth_int = 0;
    Debug((vpc, VPDEBUG_DEPTHCUE,
	   "  %3d %3d: depth = %10.6f, factor = %10.6f, table[%d] = %10.6f\n",
	   width, 0, pix_depth,
	   vpc->dc_front_factor * exp(-vpc->dc_density * (1.0 - pix_depth)),
	   pixel_depth_int, dc_table[pixel_depth_int]));
    pix_depth = left_depth + width*depth_di + height*depth_dj;
    pixel_depth_int = (int)(pix_depth * depth_quant);
    if (pixel_depth_int < 0) pixel_depth_int = 0;
    Debug((vpc, VPDEBUG_DEPTHCUE,
	   "  %3d %3d: depth = %10.6f, factor = %10.6f, table[%d] = %10.6f\n",
	   width, height, pix_depth,
	   vpc->dc_front_factor * exp(-vpc->dc_density * (1.0 - pix_depth)),
	   pixel_depth_int, dc_table[pixel_depth_int]));
    pix_depth = left_depth + 0*depth_di + height*depth_dj;
    pixel_depth_int = (int)(pix_depth * depth_quant);
    if (pixel_depth_int < 0) pixel_depth_int = 0;
    Debug((vpc, VPDEBUG_DEPTHCUE,
	   "  %3d %3d: depth = %10.6f, factor = %10.6f, table[%d] = %10.6f\n",
	   0, height, pix_depth,
	   vpc->dc_front_factor * exp(-vpc->dc_density * (1.0 - pix_depth)),
	   pixel_depth_int, dc_table[pixel_depth_int]));
#endif /* DEBUG */

    /* foreach pixel, compute depth and scale color by dc factor */
    if (vpc->color_channels == 1) {
	gray_intim = vpc->int_image.gray_intim;
	for (j = height; j > 0; j--) {
	    pixel_depth_quant = left_depth_quant;
	    left_depth_quant += depth_dj_quant;
	    for (i = width; i > 0; i--) {
		pixel_depth_int = pixel_depth_quant;
		pixel_depth_quant += depth_di_quant;
		if (pixel_depth_int < 0)
		    pixel_depth_int = 0;
		if (pixel_depth_int >= vpc->dc_table_len) {
		    VPBug("VPDepthCueIntImage: depth too large (%d >= %d)",
			  pixel_depth_int, vpc->dc_table_len);
		}
		gray_intim->clrflt *= dc_table[pixel_depth_int];
		gray_intim++;
	    } /* for i */
	} /* for j */
    } else {
	rgb_intim = vpc->int_image.rgb_intim;
	for (j = height; j > 0; j--) {
	    pixel_depth_quant = left_depth_quant;
	    left_depth_quant += depth_dj_quant;
	    for (i = width; i > 0; i--) {
		pixel_depth_int = pixel_depth_quant;
		pixel_depth_quant += depth_di_quant;
		if (pixel_depth_int < 0)
		    pixel_depth_int = 0;
		if (pixel_depth_int >= vpc->dc_table_len) {
		    VPBug("VPDepthCueIntImage: depth too large (%d >= %d)",
			  pixel_depth_int, vpc->dc_table_len);
		}
		rgb_intim->rclrflt *= dc_table[pixel_depth_int];
		rgb_intim->gclrflt *= dc_table[pixel_depth_int];
		rgb_intim->bclrflt *= dc_table[pixel_depth_int];
		rgb_intim++;
	    } /* for i */
	} /* for j */
    }
}

/*
 * ComputeLightViewTransform
 *
 * Compute the view transformation from the point of view of the one
 * light source that produces shadows.
 */

static void
ComputeLightViewTransform(vpc, vm)
vpContext *vpc;
vpMatrix4 vm;	/* storage for result */
{
    vpMatrix4 prematrix;	/* transform volume to unit cube */
    vpMatrix4 viewportm;		/* viewport matrix */
    vpVector3 vpn;		/* view plane normal */
    vpVector3 vup;		/* view up vector */
    vpVector3 tmp1v, tmp2v;	/* temporary vectors */
    vpMatrix4 view;		/* transform world coordinates to
				   eye coordinates, with view
				   direction equal to light vector */
    vpMatrix4 tmp1m, tmp2m;	/* temporary matrices */
    double lx, ly, lz;		/* components of light vector */
    int light_index;
    int maxdim;
    double scale;

    /* check for errors */
    ASSERT(vpc->shadow_light_num >= VP_LIGHT0 &&
	   vpc->shadow_light_num <= VP_LIGHT5);
    ASSERT(vpc->light_enable[vpc->shadow_light_num - VP_LIGHT0]);

    /* compute prematrix */
    vpIdentity4(prematrix);
    maxdim = vpc->xlen;
    if (vpc->ylen > maxdim)
	maxdim = vpc->ylen;
    if (vpc->zlen > maxdim)
	maxdim = vpc->zlen;
    scale = 1. / (double)maxdim;
    prematrix[0][0] = scale;
    prematrix[1][1] = scale;
    prematrix[2][2] = scale;
    prematrix[0][3] = -vpc->xlen * scale * 0.5;
    prematrix[1][3] = -vpc->ylen * scale * 0.5;
    prematrix[2][3] = -vpc->zlen * scale * 0.5;

    /* compute the world-to-eye coordinate transformation */
    light_index = vpc->shadow_light_num - VP_LIGHT0;
    lx = vpc->light_vector[light_index][0];
    ly = vpc->light_vector[light_index][1];
    lz = vpc->light_vector[light_index][2];
    vpSetVector3(vpn, lx, ly, lz);
    if (fabs(lx) < fabs(ly)) {
	if (fabs(lx) < fabs(lz)) {
	    vpSetVector3(vup, 1.0, 0.0, 0.0);
	} else {
	    vpSetVector3(vup, 0.0, 0.0, 1.0);
	}
    } else {
	if (fabs(ly) < fabs(lz)) {
	    vpSetVector3(vup, 0.0, 1.0, 0.0);
	} else {
	    vpSetVector3(vup, 0.0, 0.0, 1.0);
	}
    }
    vpCrossProduct(tmp1v, vup, vpn);
    vpCrossProduct(tmp2v, vpn, tmp1v);

    vpIdentity4(view);
    view[0][0] = tmp1v[0];
    view[0][1] = tmp1v[1];
    view[0][2] = tmp1v[2];
    view[1][0] = tmp2v[0];
    view[1][1] = tmp2v[1];
    view[1][2] = tmp2v[2];
    view[2][0] = vpn[0];
    view[2][1] = vpn[1];
    view[2][2] = vpn[2];

    /* initialize matrix to switch to left-handed coords. */
    vpIdentity4(viewportm);
    viewportm[2][2] = -1.0;

    /* compute the view matrix */
    vpMatrixMult4(tmp1m, vpc->transforms[VP_MODEL], prematrix);
    vpMatrixMult4(tmp2m, view, tmp1m);
    vpMatrixMult4(vm, viewportm, tmp2m);
}

/*
 * FactorLightView
 *
 * Factor an affine viewing matrix that specifies the view seen by
 * a light source.  Most of the parameters of the factorization are
 * taken from the factorization of the normal viewing matrix.
 */

static int
FactorLightView(vpc, vm)
vpContext *vpc;
vpMatrix4 vm;
{
    vpMatrix4 p;		/* permutation matrix */
    vpMatrix4 pvm;		/* permutation of the viewing matrix */
    int icount, jcount, kcount;	/* dimensions of volume in rotated space */
    double denom;

    Debug((vpc, VPDEBUG_SHADOW, "FactorLightView\n"));

    /* permute the rows of the viewing matrix according to the best viewing
       axis for the viewing direction */
    bzero(p, sizeof(vpMatrix4));
    switch (vpc->best_view_axis) {
    case VP_X_AXIS:
	p[0][2] = 1.;
	p[1][0] = 1.;
	p[2][1] = 1.;
	p[3][3] = 1.;
	icount = vpc->ylen;
	jcount = vpc->zlen;
	kcount = vpc->xlen;
	break;
    case VP_Y_AXIS:
	p[0][1] = 1.;
	p[1][2] = 1.;
	p[2][0] = 1.;
	p[3][3] = 1.;
	icount = vpc->zlen;
	jcount = vpc->xlen;
	kcount = vpc->ylen;
	break;
    case VP_Z_AXIS:
	p[0][0] = 1.;
	p[1][1] = 1.;
	p[2][2] = 1.;
	p[3][3] = 1.;
	icount = vpc->xlen;
	jcount = vpc->ylen;
	kcount = vpc->zlen;
	break;
    }
    vpMatrixMult4(pvm, vm, p);

    /* compute the shear coefficients */
    denom = pvm[0][0]*pvm[1][1] - pvm[0][1]*pvm[1][0];
    if (fabs(denom) < VP_EPS)
	return(VPSetError(vpc, VPERROR_SINGULAR));
    vpc->shadow_shear_i = (pvm[0][2]*pvm[1][1] - pvm[0][1]*pvm[1][2]) / denom;
    vpc->shadow_shear_j = (pvm[0][0]*pvm[1][2] - pvm[1][0]*pvm[0][2]) / denom;

    /* check that light direction is compatible with compositing direction */
    if (pvm[2][0]*vpc->shadow_shear_i + pvm[2][1]*vpc->shadow_shear_j -
	pvm[2][2] > 0) {
	if (vpc->reverse_slice_order != 0)
	    return(VPERROR_BAD_SHADOW);
    } else {
	if (vpc->reverse_slice_order != 1)
	    return(VPERROR_BAD_SHADOW);
    }

    /* compute the shadow buffer image size */
    vpc->shadow_width = (int)ceil((kcount-1)*fabs(vpc->shadow_shear_i)) +
	icount + 1;
    vpc->shadow_height = (int)ceil((kcount-1)*fabs(vpc->shadow_shear_j)) +
	jcount + 1;

    /* compute the translation coefficients */
    if (vpc->shadow_shear_i >= 0.)
	vpc->shadow_trans_i = 1.;
    else
	vpc->shadow_trans_i = 1. - vpc->shadow_shear_i * (kcount - 1);
    if (vpc->shadow_shear_j >= 0.)
	vpc->shadow_trans_j = 1.;
    else
	vpc->shadow_trans_j = 1. - vpc->shadow_shear_j * (kcount - 1);

    Debug((vpc, VPDEBUG_SHADOW, "  shadow shear factors: %g %g\n",
	   vpc->shadow_shear_i, vpc->shadow_shear_j));
    Debug((vpc, VPDEBUG_SHADOW, "  shadow translation: %g %g\n",
	   vpc->shadow_trans_i, vpc->shadow_trans_j));

    return(VP_OK);
}

/*
 * CheckShadowBuffer
 *
 * Resize the shadow buffer, if necessary.
 */

static void
CheckShadowBuffer(vpc)
vpContext *vpc;
{
    int new_max_width, new_max_height;
    int resize = 0;

    /* determine if resizing is necessary */
    if (vpc->shadow_width > vpc->max_shadow_width) {
	new_max_width = MAX(vpc->shadow_width, vpc->shadow_width_hint);
	resize = 1;
    } else {
	new_max_width = MAX(vpc->max_shadow_width, vpc->shadow_width_hint);
    }
    if (vpc->shadow_height > vpc->max_shadow_height) {
	new_max_height = MAX(vpc->shadow_height, vpc->shadow_height_hint);
	resize = 1;
    } else {
	new_max_height = MAX(vpc->max_shadow_height, vpc->shadow_height_hint);
    }

    /* resize */
    if (resize)
	VPResizeShadowBuffer(vpc, new_max_width, new_max_height);
}

/*
 * VPResizeShadowBuffer
 *
 * Resize the shadow buffer.
 */

void
VPResizeShadowBuffer(vpc, max_width, max_height)
vpContext *vpc;
int max_width;	/* new width of the intermediate image */
int max_height;	/* new height of the intermediate image */
{
    /* free old buffers */
    if (vpc->shadow_buffer != NULL) {
	Dealloc(vpc, vpc->shadow_buffer);
    }

    /* allocate new buffers */
    vpc->max_shadow_width = max_width;
    vpc->max_shadow_height = max_height;
    if (max_width > 0) {
	Alloc(vpc, vpc->shadow_buffer, GrayIntPixel *,
	      max_width * max_height * sizeof(GrayIntPixel), "shadow_buffer");
    } else {
	vpc->shadow_buffer = NULL;
    }
}
