/*
 * vp_shade.c
 *
 * Routines to implement the Phong shading equation using a lookup-table.
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

/*
 * Lookup Table Shading and Normal Vector Encoding
 *
 * The shader defined in this file implements the Phong shading equation
 * (I = Ia + Id*(N.L) + Is*(N.H)^n) using lookup tables.  To use this
 * shader you must include a "normal" field in each voxel.  This field
 * is computed by preprocessing the volume to estimate a surface normal
 * vector for each voxel (using the central-difference gradient operator)
 * and then encoding the normal in an index (13 bits in this program).
 * An index is stored in the normal field of each voxel.   At rendering
 * time the index is used to look up a color for the voxel in a table.
 *
 * There are many ways a normal vector can be encoded using the 13 bits of
 * the index.  A straight-forward method is to use 6 bits for the x component
 * of the vector, 6 bits for the y component, and 1 bit to indicate the sign
 * of the z component.  Assuming that the vector is normalized the z
 * component can be reconstructed from x and y.  Unfortunately this method
 * results in an uneven distribution of code points: the distance between
 * exactly-representable vectors is much smaller near N = +z or -z than
 * N = +x, -x, +y or -z (where x, y and z are the unit vectors).  This
 * can result in significant quantization error near the "equator", or more
 * table storage than necessary near the "poles".
 *
 * The normal vector encoding scheme used here is derived from a recursive
 * tesselation of a regular octahedron (an eight-sided solid with
 * equilateral triangular faces).  Consider subdividing each triangular
 * face into four smaller equilateral triangles, and then subdividing those
 * triangles recursively until a sufficiently large number of triangles
 * have been generated.  The representable normal vectors are the vectors
 * connecting the center of the solid to the vertices of the triangles.
 * The distribution of these vectors is not perfectly uniform (the density
 * is lower at the "mid-latitudes"), but the variation is relatively small.
 *
 * Each normal vector is now assigned a unique index as follows.  Let the
 * origin be at the center of the solid, and the x, y and z axes each
 * intersect a vertex of the original octahedron.  Use one bit of the index
 * to indicate the sign of the z component of the normal.  Now project the
 * subdivided triangle vertices onto the x-y plane.  This forms a square
 * grid of dots rotated 45 degrees relative to the x-y axes.  Starting at
 * (x=0, y=max), assign sequential integers to each normal vector projection,
 * proceeding left-to-right and top-to-bottom.  Finally, append the sign bit
 * for the z component to the least-significant end of the integer to get
 * the normal vector encoding.
 *
 * This scheme is useful because it is easy to compute the vector components
 * from an index and conversely to find the closest index for a given vector,
 * yet the distribution of representable vectors is pretty uniform.
 *
 * XXX better method is to rotate 45 degrees (M = [1 -1 ; 1 1]) and then
 * assign points in scanline order; no lookup tables needed; implement this!
 *
 * The layout of the shading lookup table is as follows:
 *    float shade_table[MAX_NORMAL+1][materials][color_channels]
 * where materials is the number of materials and color_channels is 1
 * for grayscale intensities or 3 for RGB intensities (stored in R,G,B
 * order).
 */

/* define normal index parameters; if you change these then you
   must change VP_NORM_MAX in volpack.h */
#define NORM_13			/* use 13 bit normals */
#undef NORM_15			/* don't use 15 bit normals */

#ifdef NORM_13			/* parameters for 13 bit normals */
#define NORM_N		44	/*   N parameter for normal computation */
#define NORM_BITS	13	/*   number of bits to encode a normal:
				     (1+ceil(log2(2*(N*N+N+1)))) */
#define MAX_NORMAL	7923	/*   maximum normal index */
#endif

#ifdef NORM_15			/* parameters for 15 bit normals */
#define NORM_N		90
#define NORM_BITS	15
#define MAX_NORMAL	16131
#endif


/* static lookup tables (computed only once, used for all vpContexts) */
static int NormalTablesInitialized = 0; /* set to 1 after initialization */
static short *NormalPy;	/* NormalPy[py] = normal index for the normal
				   whose projection in the x-y plane is (0,py)
				   and whose z component is +
				   (py = -NORM_N to +NORM_N) */
static short NormalPyStorage[1+2*NORM_N];	/* storage for NormalPy */
static short *NormalXLimit;	/* max abs(x) allowed for a given y */
static short NormalXLimitStorage[1+2*NORM_N]; /* storage for NormalXLimit */
static void InitNormalTables ANSI_ARGS((void));

/*
 * InitNormalTables
 *
 * Initialize lookup tables for computing normal indices.
 */

static void
InitNormalTables()
{
    int y, xcount, codepoint;
    int sum;
    double value;

    /* initialize NormalPy */
    xcount = 1;
    codepoint = 2;
    NormalPy = &NormalPyStorage[NORM_N];
    NormalXLimit = &NormalXLimitStorage[NORM_N];
    for (y = -NORM_N; y <= NORM_N; y++) {
	NormalPy[y] = codepoint + (((xcount-1)/2) << 1);
	codepoint += (xcount << 1);
	NormalXLimit[y] = (xcount-1)/2;
	if (y < 0)
	    xcount += 2;
	else
	    xcount -= 2;
    }

    NormalTablesInitialized = 1;
}

/*
 * vpNormalIndex
 *
 * Return the best normal index for the given normal vector.
 */

int
vpNormalIndex(nx, ny, nz)
double nx, ny, nz;
{
    int n, x, y, xlimit;
    double denom, denominv;

    if (!NormalTablesInitialized)
	InitNormalTables();
    denom = (nx < 0) ? -nx : nx;
    denom += (ny < 0) ? -ny : ny;
    denom += (nz < 0) ? -nz : nz;
    denominv = (double)NORM_N / denom;
    x = (int)rint(nx * denominv);
    y = (int)rint(ny * denominv);

    /* clamp x */
    xlimit = NormalXLimit[y];
    if (x < 0) {
	if (-x > xlimit)
	    x = -xlimit;
    } else {
	if (x > xlimit)
	    x = xlimit;
    }

    n = NormalPy[y] + (x << 1);
    if (nz < 0)
	n |= 1;
    ASSERT(n >= 0 && n <= VP_NORM_MAX);
    return(n);
}

/*
 * vpNormal
 *
 * Compute normal vector components given a normal vector index.
 */

vpResult
vpNormal(n, nx, ny, nz)
int n;			/* normal index */
double *nx, *ny, *nz;	/* storage for result */
{
    int py, px, pz, pxlimit2;
    double pxd, pyd, pzd, plength;

    if (!NormalTablesInitialized)
	InitNormalTables();
    for (py = -NORM_N; py <= NORM_N; py++) {
	pxlimit2 = 2 * ((py<0) ? (NORM_N + py) : (NORM_N - py));
	if (NormalPy[py] - pxlimit2 <= n &&
	    NormalPy[py] + pxlimit2 + 1 >= n) {
	    break;
	}
    }
    if (py > NORM_N) {
	return(VPERROR_BAD_VALUE);
    } else {
	px = (n - NormalPy[py]) >> 1;
	pz = NORM_N;
	if (px < 0)
	    pz += px;
	else
	    pz -= px;
	if (py < 0)
	    pz += py;
	else
	    pz -= py;
	if (n & 1)
	    pz = -pz;
	pxd = (double)px;
	pyd = (double)py;
	pzd = (double)pz;
	plength = 1. / sqrt(pxd*pxd+pyd*pyd+pzd*pzd);
	*nx = pxd * plength;
	*ny = pyd * plength;
	*nz = pzd * plength;
    }
    return(VP_OK);
}

/*
 * vpScanlineNormals
 *
 * Compute normals and/or gradients for a scanline of voxels.
 */

vpResult
vpScanlineNormals(vpc, length, scalar_data, scalar_minus_y,
		  scalar_plus_y, scalar_minus_z, scalar_plus_z,
		  voxel_data, scalar_field, grad_field, norm_field)
vpContext *vpc;		/* context */
int length;		/* number of scalars in scanline */
unsigned char *scalar_data;	/* scanline of scalar data */
unsigned char *scalar_minus_y;	/* adjacent scanline of scalar data (-y) */
unsigned char *scalar_plus_y;	/* adjacent scanline of scalar data (+y) */
unsigned char *scalar_minus_z;	/* adjacent scanline of scalar data (-z) */
unsigned char *scalar_plus_z;	/* adjacent scanline of scalar data (+z) */
void *voxel_data;	/* location to store first voxel */
int scalar_field;	/* voxel field for scalar, or VP_SKIP_FIELD */
int grad_field;		/* voxel field for gradient, or VP_SKIP_FIELD */
int norm_field;		/* voxel field for normal, or VP_SKIP_FIELD */
{
    int x;			/* voxel index */
    double grad_x;		/* components of the gradient vector */
    double grad_y;
    double grad_z;
    double twice_grad_mag;	/* twice the magnitude of the gradient */
    int grad;			/* gradient magnitude */
    int norm;			/* normal index */
    int edge;			/* true if this scanline is on the edge
				   of the volume */
    int voxel_size;		/* size of a voxel in bytes */
    int scalar_offset;		/* byte offset for scalar in voxel */
    int grad_offset;		/* byte offset for gradient in voxel */
    int norm_offset;		/* byte offset for normal in voxel */
    char *voxel;		/* pointer to current voxel */
    int retcode;

    /* check for errors */
    if ((retcode = VPCheckVoxelFields(vpc)) != VP_OK)
	return(retcode);
    if (scalar_field != VP_SKIP_FIELD) {
	if (scalar_field < 0 || scalar_field >= vpc->num_voxel_fields)
	    return(VPSetError(vpc, VPERROR_BAD_VALUE));
	if (vpc->field_size[scalar_field] != VP_SCALAR_SIZE)
	    return(VPSetError(vpc, VPERROR_BAD_VALUE));
	scalar_offset = vpc->field_offset[scalar_field];
    }
    if (grad_field != VP_SKIP_FIELD) {
	if (grad_field < 0 || grad_field >= vpc->num_voxel_fields)
	    return(VPSetError(vpc, VPERROR_BAD_VALUE));
	if (vpc->field_size[grad_field] != VP_GRAD_SIZE)
	    return(VPSetError(vpc, VPERROR_BAD_VALUE));
	grad_offset = vpc->field_offset[grad_field];
    }
    if (norm_field != VP_SKIP_FIELD) {
	if (norm_field < 0 || norm_field >= vpc->num_voxel_fields)
	    return(VPSetError(vpc, VPERROR_BAD_VALUE));
	if (vpc->field_size[norm_field] != VP_NORM_SIZE)
	    return(VPSetError(vpc, VPERROR_BAD_VALUE));
	norm_offset = vpc->field_offset[norm_field];
    }
    voxel_size = vpc->raw_bytes_per_voxel;

    /* compute the scanline */
    voxel = voxel_data;
    if (scalar_minus_y == NULL || scalar_plus_y == NULL ||
	scalar_minus_z == NULL || scalar_plus_z == NULL) {
	edge = 1;
    } else {
	edge = 0;
    }
    for (x = 0; x < length; x++) {
	/* compute gradient and normal for voxel x and store */
	if (edge || x == 0 || x == length-1) {
	    if (scalar_field != VP_SKIP_FIELD)
		ByteField(voxel, scalar_offset) = scalar_data[x];
	    if (grad_field != VP_SKIP_FIELD)
		ByteField(voxel, grad_offset) = 0;
	    if (norm_field != VP_SKIP_FIELD)
		ShortField(voxel, norm_offset) = 0;
	} else {
	    grad_x = (int)scalar_data[x+1] - (int)scalar_data[x-1];
	    grad_y = (int)scalar_plus_y[x] - (int)scalar_minus_y[x];
	    grad_z = (int)scalar_plus_z[x] - (int)scalar_minus_z[x];
	    twice_grad_mag = sqrt(grad_x*grad_x+grad_y*grad_y+grad_z*grad_z);
	    if (scalar_field != VP_SKIP_FIELD)
		ByteField(voxel, scalar_offset) = scalar_data[x];
	    if (grad_field != VP_SKIP_FIELD) {
		grad = (int)rint(0.5 * twice_grad_mag);
		ASSERT(grad >= 0 && grad <= VP_GRAD_MAX);
		ByteField(voxel, grad_offset) = grad;
	    }
	    if (norm_field != VP_SKIP_FIELD) {
		if (twice_grad_mag < VP_EPS)
		    norm = 0;
		else
		    norm = vpNormalIndex(grad_x / twice_grad_mag,
					 grad_y / twice_grad_mag,
					 grad_z / twice_grad_mag);
		ShortField(voxel, norm_offset) = norm;
	    }
	}

	/* go on to next voxel */
	voxel += voxel_size;
    }
    return(VP_OK);
}

/*
 * vpVolumeNormals
 *
 * Compute normals and/or gradients for a volume.  Result is stored
 * in raw_voxels in the current context.
 */

vpResult
vpVolumeNormals(vpc, scalar_data, length, scalar_field, grad_field, norm_field)
vpContext *vpc;		/* context */
unsigned char *scalar_data;	/* 3D array of scalar data */
int length;		/* number of scalars in scalar_data */
int scalar_field;	/* voxel field for scalar, or VP_SKIP_FIELD */
int grad_field;		/* voxel field for gradient, or VP_SKIP_FIELD */
int norm_field;		/* voxel field for normal, or VP_SKIP_FIELD */
{
    int xlen, ylen, zlen;	/* volume dimensions */
    int y, z;			/* loop indices */
    unsigned char *scalar;	/* pointer to current scalar */
    int scalar_ystride;		/* stride to next scalar scanline */
    int scalar_zstride;		/* stride to next scalar slice */
    char *voxel;		/* pointer to current voxel */
    int voxel_ystride;		/* stride to next voxel scanline */
    int voxel_zstride;		/* stride to next voxel slice */
    unsigned char *s_py, *s_my, *s_pz, *s_mz; /* ptrs to adjacent scans */
    int retcode;

    /* check for errors */
    if ((retcode = VPCheckRawVolume(vpc)) != VP_OK)
	return(retcode);
    xlen = vpc->xlen;
    ylen = vpc->ylen;
    zlen = vpc->zlen;
    if (xlen * ylen * zlen != length)
	return(VPSetError(vpc, VPERROR_BAD_SIZE));

    /* initialize */
    scalar = scalar_data;
    scalar_ystride = xlen;
    scalar_zstride = xlen*ylen;
    voxel = vpc->raw_voxels;
    voxel_ystride = vpc->ystride;
    voxel_zstride = vpc->zstride;

    /* compute volume data */
    for (z = 0; z < zlen; z++) {
	ReportStatus(vpc, (double)z / (double)zlen);
	for (y = 0; y < ylen; y++) {
	    s_my = (y == 0)      ? NULL : scalar - scalar_ystride;
	    s_py = (y == ylen-1) ? NULL : scalar + scalar_ystride;
	    s_mz = (z == 0)      ? NULL : scalar - scalar_zstride;
	    s_pz = (z == zlen-1) ? NULL : scalar + scalar_zstride;
	    retcode = vpScanlineNormals(vpc, xlen, scalar, s_my, s_py,
					s_mz, s_pz, voxel, scalar_field,
					grad_field, norm_field);
	    if (retcode != VP_OK)
		return(retcode);
	    scalar += scalar_ystride;
	    voxel += voxel_ystride;
	}
	scalar += scalar_zstride - ylen*scalar_ystride;
	voxel += voxel_zstride - ylen*voxel_ystride;
    }
    ReportStatus(vpc, 1.0);
    return(VP_OK);
}

/*
 * vpShadeTable
 *
 * Compute a shading lookup table for the current lighting and
 * model matrix.
 */

vpResult
vpShadeTable(vpc)
vpContext *vpc;
{
    int num_lights;		/* number of enabled lights */
    vpVector3 light_color[VP_MAX_LIGHTS]; /* light colors */
    vpVector4 obj_light[VP_MAX_LIGHTS]; /* light_vector in object space */
    vpVector4 obj_highlight[VP_MAX_LIGHTS]; /* halfway-vector */
    vpVector4 obj_viewpoint;	/* viewpoint in object coordinates */
    vpMatrix4 a;		/* linear system matrix */
    double *rhs[VP_MAX_LIGHTS+1];/* right-hand-side/solution vectors */
    int px, py, pz;		/* code point coordinates (integers) */
    double pxd, pyd, pzd;	/* code point coordinates (doubles) */
    double plength;
    int pxlimit;		/* maximum absolute value of px for this py */
    double nx, ny, nz;		/* normal vector components */
    double n_dot_v_xy;		/* normal_vector dot obj_viewpoint (x&y) */
    double n_dot_v_z;		/* normal_vector dot obj_viewpoint (z) */
    double n_dot_v;		/* normal_vector dot obj_viewpoint */
    double n_dot_l_xy;		/* normal_vector dot light_vector (x&y) */
    double n_dot_l_z;		/* normal_vector dot light_vector (z) */
    double n_dot_l;		/* normal_vector dot light_vector */
    double n_dot_h_xy;		/* normal_vector dot halfway_vector (x&y) */
    double n_dot_h_z;		/* normal_vector dot halfway_vector (z) */
    double n_dot_h;		/* normal_vector dot halfway_vector */
    float r, g, b;		/* intensities to store in shade table */
    float *table_row;		/* pointer to table row for current normal */
    float *table;		/* pointer to table entry */
    float *shadow_table_row;	/* pointer to table row for current normal */
    float *shadow_table;	/* pointer to shadow table entry */
    int surface_side;		/* EXT_SURFACE or INT_SURFACE */
    int znegative;		/* true iff nz is negative */
    int light_both_sides;	/* use two-sided lighting */
    int reverse_surface_sides;	/* reverse interior and exterior */
    int color_channels;		/* number of color channels */
    int num_materials;		/* number of materials */
    int retcode;
    double *matl_props;		/* material properties */
    int enable_shadows;		/* true if shadows are enabled */
    int shadow_light;		/* light index for light casting shadows */
    int clamp;			/* true if table entries should be clamped */
    int c, l, m;
#ifdef DEBUG
    vpVector4 tmpv;
#endif
    DECLARE_TIME(t0);
    DECLARE_TIME(t1);

    /* error checking */
    if (vpc->shading_mode != LOOKUP_SHADER)
	return(VP_OK);
    if ((retcode = VPCheckShader(vpc)) != VP_OK)
	return(retcode);
    if ((retcode = VPCheckShadows(vpc)) != VP_OK)
	return(retcode);
    ASSERT(vpc->color_channels == 1 || vpc->color_channels == 3);

    /* start timer */
    GET_TIME(vpc, t0);

    /* transform viewpoint vector and light vectors to object space */
    vpSetVector4(obj_viewpoint, 0., 0., 1., 1.);
    rhs[0] = obj_viewpoint;
    num_lights = 0;
    for (c = 0; c < VP_MAX_LIGHTS; c++) {
	if (vpc->light_enable[c]) {
	    bcopy(vpc->light_color[c], light_color[num_lights],
		  sizeof(vpVector3));
	    bcopy(vpc->light_vector[c], obj_light[num_lights],
		  sizeof(vpVector4));
	    rhs[num_lights+1] = obj_light[num_lights];
	    num_lights++;
	}
    }
    bcopy(vpc->transforms[VP_MODEL], a, sizeof(vpMatrix4));
    retcode = vpSolveSystem4(a, rhs, num_lights+1);
    if (retcode != VP_OK)
	return(retcode);

#ifdef DEBUG
    /* make sure the solver gave the right answer */
    vpMatrixVectorMult4(tmpv, vpc->transforms[VP_MODEL], obj_viewpoint);
    if (fabs(tmpv[0]) > VP_EPS || fabs(tmpv[1]) > VP_EPS ||
	fabs(tmpv[2] - 1.) > VP_EPS || fabs(tmpv[3] - 1.) > VP_EPS) {
	printf("\n");
	printf("Modelview:\n");
	printf("    %12.8f %12.8f %12.8f %12.8f\n",
	   vpc->transforms[VP_MODEL][0][0], vpc->transforms[VP_MODEL][0][1],
	   vpc->transforms[VP_MODEL][0][2], vpc->transforms[VP_MODEL][0][3]);
	printf("    %12.8f %12.8f %12.8f %12.8f\n",
	   vpc->transforms[VP_MODEL][1][0], vpc->transforms[VP_MODEL][1][1],
	   vpc->transforms[VP_MODEL][1][2], vpc->transforms[VP_MODEL][1][3]);
	printf("    %12.8f %12.8f %12.8f %12.8f\n",
	   vpc->transforms[VP_MODEL][2][0], vpc->transforms[VP_MODEL][2][1],
	   vpc->transforms[VP_MODEL][2][2], vpc->transforms[VP_MODEL][2][3]);
	printf("    %12.8f %12.8f %12.8f %12.8f\n",
	   vpc->transforms[VP_MODEL][3][0], vpc->transforms[VP_MODEL][3][1],
	   vpc->transforms[VP_MODEL][3][2], vpc->transforms[VP_MODEL][3][3]);
	VPBug("SolveSystem failed on viewpoint");
    }
    l = 0;
    for (c = 0; c < VP_MAX_LIGHTS; c++) {
	if (vpc->light_enable[c]) {
	    vpMatrixVectorMult4(tmpv, vpc->transforms[VP_MODEL], obj_light[l]);
	    if (fabs(tmpv[0] - vpc->light_vector[c][0]) > VP_EPS ||
		fabs(tmpv[1] - vpc->light_vector[c][1]) > VP_EPS ||
		fabs(tmpv[2] - vpc->light_vector[c][2]) > VP_EPS ||
		fabs(tmpv[3] - vpc->light_vector[c][3]) > VP_EPS)
		VPBug("SolveSystem failed on light %d\n", c);
	    l++;
	}
    }
#endif

    /* compute highlight vectors */
    for (l = 0; l < num_lights; l++) {
	obj_highlight[l][0] = obj_light[l][0] + obj_viewpoint[0];
	obj_highlight[l][1] = obj_light[l][1] + obj_viewpoint[1];
	obj_highlight[l][2] = obj_light[l][2] + obj_viewpoint[2];
	vpNormalize3(obj_highlight[l]);
    }

    /* initialize options */
    light_both_sides = vpc->light_both_sides;
    reverse_surface_sides = vpc->reverse_surface_sides;
    color_channels = vpc->color_channels;
    num_materials = vpc->num_materials;
    table = vpc->shade_color_table;
    enable_shadows = vpc->enable_shadows;
    if (enable_shadows) {
	shadow_table = vpc->shadow_color_table;
	shadow_light = vpc->shadow_light_num - VP_LIGHT0;
	bzero(shadow_table, vpc->shadow_color_table_size);
    } else {
	shadow_table = NULL;
	shadow_light = -1;
    }
    clamp = vpc->clamp_shade_table;

    /* store shade table entries for the zero-vector */
    for (znegative = 0; znegative <= 1; znegative++) {
	if (znegative) {
	    if (reverse_surface_sides)
		surface_side = EXT_SURFACE;
	    else
		surface_side = INT_SURFACE;
	} else {
	    if (reverse_surface_sides)
		surface_side = INT_SURFACE;
	    else
		surface_side = EXT_SURFACE;
	}
	for (m = 0; m < num_materials; m++) {
	    matl_props = vpc->matl_props[m][surface_side];
	    *table++ = matl_props[MATL_AMB_R];
	    if (color_channels == 3) {
		*table++ = matl_props[MATL_AMB_G];
		*table++ = matl_props[MATL_AMB_B];
	    }
	}
    }
    table_row = table;
    if (enable_shadows) {
	for (znegative = 0; znegative <= 1; znegative++) {
	    for (m = 0; m < num_materials; m++) {
		*shadow_table++ = 0;
		if (color_channels == 3) {
		    *shadow_table++ = 0;
		    *shadow_table++ = 0;
		}
	    }
	}
    }
    shadow_table_row = shadow_table;

    /* compute the shade table entries for nonzero normals */
    for (py = -NORM_N; py <= NORM_N; py++) {
	pxlimit = (py < 0) ? (NORM_N + py) : (NORM_N - py);
	pz = -1;
	pxd = (double)(-pxlimit-1);
	pyd = (double)py;
	pzd = (double)(-1);
	for (px = -pxlimit; px <= pxlimit; px++) {
	    /* compute normal vector components for this code point */
	    pxd += 1.0;
	    if (px <= 0) {
		pz++;
		pzd += 1.0;
	    } else {
		pz--;
		pzd -= 1.0;
	    }
	    plength = 1. / sqrt(pxd*pxd + pyd*pyd + pzd*pzd);
	    nx = pxd * plength;
	    ny = pyd * plength;
	    nz = pzd * plength;

	    /* compute n dot v (for determining surface side) */
	    n_dot_v_xy = nx*obj_viewpoint[0] + ny*obj_viewpoint[1];
	    n_dot_v_z = nz*obj_viewpoint[2];

	    /* store ambient light terms */
	    table = table_row;
	    for (znegative = 0; znegative <= 1; znegative++) {
		if (znegative)
		    n_dot_v = n_dot_v_xy - n_dot_v_z;
		else
		    n_dot_v = n_dot_v_xy + n_dot_v_z;
		if (reverse_surface_sides)
		    n_dot_v = -n_dot_v;
		if (n_dot_v >= 0)
		    surface_side = EXT_SURFACE;
		else
		    surface_side = INT_SURFACE;
		for (m = 0; m < num_materials; m++) {
		    matl_props = vpc->matl_props[m][surface_side];
		    *table++ = matl_props[MATL_AMB_R];
		    if (color_channels == 3) {
			*table++ = matl_props[MATL_AMB_G];
			*table++ = matl_props[MATL_AMB_B];
		    }
		}
	    }

	    /* loop over lights */
	    for (l = 0; l < num_lights; l++) {
		if (l == shadow_light)
		    table = shadow_table_row;
		else
		    table = table_row;

		/* compute n dot l and n dot h */
		n_dot_l_xy = nx*obj_light[l][0] + ny*obj_light[l][1];
		n_dot_l_z = nz*obj_light[l][2];
		n_dot_h_xy = nx*obj_highlight[l][0] + ny*obj_highlight[l][1];
		n_dot_h_z = nz*obj_highlight[l][2];

		/* loop over the two signs for z */
		for (znegative = 0; znegative <= 1; znegative++) {
		    if (znegative) {
			n_dot_v = n_dot_v_xy - n_dot_v_z;
			n_dot_l = n_dot_l_xy - n_dot_l_z;
			n_dot_h = n_dot_h_xy - n_dot_h_z;
		    } else {
			n_dot_v = n_dot_v_xy + n_dot_v_z;
			n_dot_l = n_dot_l_xy + n_dot_l_z;
			n_dot_h = n_dot_h_xy + n_dot_h_z;
		    }
		    if (reverse_surface_sides) {
			n_dot_v = -n_dot_v;
			n_dot_l = -n_dot_l;
			n_dot_h = -n_dot_h;
		    }
		    if (n_dot_v >= 0)
			surface_side = EXT_SURFACE;
		    else
			surface_side = INT_SURFACE;
		    if (light_both_sides) {
			n_dot_l = fabs(n_dot_l);
			n_dot_h = fabs(n_dot_h);
		    } else if (surface_side == EXT_SURFACE) {
			n_dot_l = MAX(n_dot_l, 0.0);
			n_dot_h = MAX(n_dot_h, 0.0);
		    } else {
			n_dot_l = MAX(-n_dot_l, 0.0);
			n_dot_h = MAX(-n_dot_h, 0.0);
		    }

		    /* loop over material types */
		    for (m = 0; m < num_materials; m++) {
			matl_props = vpc->matl_props[m][surface_side];
			*table++ += light_color[l][0]*(matl_props[MATL_DIFF_R]*
				    n_dot_l + matl_props[MATL_SPEC_R]*
				    pow(n_dot_h, matl_props[MATL_SHINY]));
			if (color_channels == 3) {
			    *table++ += light_color[l][1]*
				    (matl_props[MATL_DIFF_G]*
				    n_dot_l + matl_props[MATL_SPEC_G]*
				    pow(n_dot_h, matl_props[MATL_SHINY]));
			    *table++ += light_color[l][2]*
				    (matl_props[MATL_DIFF_B]*
				    n_dot_l + matl_props[MATL_SPEC_B]*
				    pow(n_dot_h, matl_props[MATL_SHINY]));
			}
		    } /* for m */
		} /* for znegative */
	    } /* for l */

	    /* clamp */
	    if (clamp) {
		if (enable_shadows) {
		    table = table_row;
		    shadow_table = shadow_table_row;
		    for (znegative = 0; znegative <= 1; znegative++) {
			for (m = 0; m < num_materials; m++) {
			    for (c = 0; c < color_channels; c++) {
				if (*table > 255.)
				    *table = 255.;
				if (*table + *shadow_table > 255.)
				    *shadow_table = 255. - *table;
				shadow_table++;
				table++;
			    }
			}
		    }
		} else {
		    table = table_row;
		    for (znegative = 0; znegative <= 1; znegative++) {
			for (m = 0; m < num_materials; m++) {
			    for (c = 0; c < color_channels; c++) {
				if (*table > 255.)
				    *table = 255.;
				table++;
			    }
			}
		    }
		}
	    }

	    if (num_materials == 1) {
		table_row += 2*color_channels;
	    } else {
		if (color_channels == 1)
		    table_row += 2*num_materials;
		else
		    table_row += 6*num_materials;
	    }

	    if (enable_shadows) {
		if (num_materials == 1) {
		    shadow_table_row += 2*color_channels;
		} else {
		    if (color_channels == 1)
			shadow_table_row += 2*num_materials;
		    else
			shadow_table_row += 6*num_materials;
		}
	    }
	} /* for px */
    } /* for py */

    /* stop timer */
    GET_TIME(vpc, t1);
    STORE_TIME(vpc, VPTIMER_SHADE, t0, t1);

    return(VP_OK);
}
