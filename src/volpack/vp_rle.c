/*
 * vp_rle.c
 *
 * Routines for run-length encoding classified volume data.
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

static void EncodeScanline ANSI_ARGS((vpContext *vpc, void *voxels,
    MinMaxOctree *octree, MMOctreeLevel level_stack[VP_MAX_OCTREE_LEVELS]));
static void InitRLE ANSI_ARGS((vpContext *vpc));
static void CacheVoxel ANSI_ARGS((vpContext *vpc, double opacity,
    void *rawvoxel));
static void CacheLength ANSI_ARGS((vpContext *vpc, int length));
static void CountNonZeroVoxel ANSI_ARGS((RunData *rundata, int index,
    int end_of_scan, vpContext *vpc));
static void RepackClassifiedVolume ANSI_ARGS((vpContext *vpc));
static RLEVoxels *CreateEmptyRLEVoxels ANSI_ARGS((vpContext *vpc,
    int ilen, int jlen, int klen));
static RLEVoxels *CreateRLEVoxelsFromRunData ANSI_ARGS((vpContext *vpc,
    int ilen, int jlen, int klen, int non_zero_count, RunData *run_data,
    int rle_bytes_per_voxel));
static void StoreNonZeroVoxel ANSI_ARGS((void *voxel, int rle_bytes_per_voxel,
    void *data, unsigned char *lengths, RunData *rundata, int index));
static void PadScanlines ANSI_ARGS((int ilen, int jlen, int klen,
    RunData *run_data, unsigned char *lengths));
static ConstructionBuffer *CreateConstructionBuffer ANSI_ARGS((
    vpContext *vpc));
static void DestroyConstructionBuffer ANSI_ARGS((vpContext *vpc,
    ConstructionBuffer *cbuf));
static GBuffer *CreateGBuffer ANSI_ARGS((vpContext *vpc));
static void DestroyGBuffer ANSI_ARGS((vpContext *vpc, GBuffer *gbuf));
#ifdef INDEX_VOLUME
static vpResult ComputeIndex ANSI_ARGS((vpContext *vpc,
    RLEVoxels *rle_voxels));
#endif
static vpResult ComputeScanOffsets ANSI_ARGS((vpContext *vpc,
    RLEVoxels *rle_voxels));
#ifdef DEBUG
static void ValidateRLEVoxels ANSI_ARGS((vpContext *vpc, RLEVoxels *rle,
    int istride, int jstride, int kstride, int axis));
#endif

/*
 * vpClassifyScalars
 *
 * Classify an array of scalars and store the result in the classified volume.
 */

vpResult
vpClassifyScalars(vpc, scalar_data, length, scalar_field, grad_field,
		  norm_field)
vpContext *vpc;		/* context */
unsigned char *scalar_data; /* 3D array of scalar data */
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
    unsigned char *s_py, *s_my, *s_pz, *s_mz; /* ptrs to adjacent scans */
    int retcode;		/* return code from vpScanlineNormals */
    void *voxel_scan;		/* buffer for storing one scan of raw voxels */

    /* check for errors */
    xlen = vpc->xlen;
    ylen = vpc->ylen;
    zlen = vpc->zlen;
    if (xlen == 0 || ylen == 0 || zlen == 0 || vpc->raw_bytes_per_voxel == 0)
	return(VPSetError(vpc, VPERROR_BAD_VOLUME));
    if (xlen * ylen * zlen != length)
	return(VPSetError(vpc, VPERROR_BAD_SIZE));

    /* initialize */
    scalar = scalar_data;
    scalar_ystride = xlen;
    scalar_zstride = xlen*ylen;
    Alloc(vpc, voxel_scan, void *, xlen*vpc->raw_bytes_per_voxel,"voxel_scan");

    /* compute volume data */
    for (z = 0; z < zlen; z++) {
	ReportStatus(vpc, (double)z / (double)zlen);
	for (y = 0; y < ylen; y++) {
	    s_my = (y == 0)      ? NULL : scalar - scalar_ystride;
	    s_py = (y == ylen-1) ? NULL : scalar + scalar_ystride;
	    s_mz = (z == 0)      ? NULL : scalar - scalar_zstride;
	    s_pz = (z == zlen-1) ? NULL : scalar + scalar_zstride;
	    voxel = voxel_scan;
	    retcode = vpScanlineNormals(vpc, xlen, scalar, s_my, s_py,
					s_mz, s_pz, voxel, scalar_field,
					grad_field, norm_field);
	    if (retcode != VP_OK) {
		Dealloc(vpc, voxel_scan);
		return(retcode);
	    }
	    retcode = vpClassifyScanline(vpc, voxel_scan);
	    if (retcode != VP_OK) {
		Dealloc(vpc, voxel_scan);
		return(retcode);
	    }
	    scalar += scalar_ystride;
	}
	scalar += scalar_zstride - ylen*scalar_ystride;
    }
    ReportStatus(vpc, 1.0);
    Dealloc(vpc, voxel_scan);
    return(VP_OK);
}

/*
 * vpClassifyVolume
 *
 * Classify the current raw volume and store the result in the
 * classified volume.
 */

vpResult
vpClassifyVolume(vpc)
vpContext *vpc;		/* context */
{
    int xlen, ylen, zlen;	/* volume dimensions */
    int y, z;			/* loop indices */
    char *voxel;		/* pointer to current voxel */
    int voxel_ystride;		/* stride to next voxel scanline */
    int voxel_zstride;		/* stride to next voxel slice */
    int retcode;		/* return code */
    MinMaxOctree *octree;	/* octree for fast classification */
    MMOctreeLevel level_stack[VP_MAX_OCTREE_LEVELS]; /* stack for octree */

    /* check for errors */
    if ((retcode = VPCheckRawVolume(vpc)) != VP_OK)
	return(retcode);
    if ((retcode = VPCheckClassifier(vpc)) != VP_OK)
	return(retcode);

    /* initialize */
    vpDestroyClassifiedVolume(vpc);
    InitRLE(vpc);
    xlen = vpc->xlen;
    ylen = vpc->ylen;
    zlen = vpc->zlen;
    voxel = vpc->raw_voxels;
    voxel_ystride = vpc->ystride;
    voxel_zstride = vpc->zstride;
    octree = vpc->mm_octree;
    if (octree != NULL) {
	VPComputeSummedAreaTable(vpc);
	VPClassifyOctree(vpc);
    }

    /* compute volume data */
    for (z = 0; z < zlen; z++) {
	ReportStatus(vpc, (double)z / (double)zlen);
	if (octree != NULL)
	    VPInitOctreeLevelStack(vpc, level_stack, VP_Z_AXIS, z);
	for (y = 0; y < ylen; y++) {
	    EncodeScanline(vpc, voxel, octree, level_stack);
	    voxel += voxel_ystride;
	}
	voxel += voxel_zstride - ylen*voxel_ystride;
    }
    ReportStatus(vpc, 1.0);

    return(VP_OK);
}

/*
 * vpClassifyScanline
 *
 * Apply the classification function to a scanline of raw voxels and append
 * it to the classified volume.
 */

vpResult
vpClassifyScanline(vpc, voxels)
vpContext *vpc;		/* context */
void *voxels;		/* voxel scanline */
{
    int retcode;

    /* initialize if this is the first scanline */
    if (vpc->cbuf == NULL) {
	if ((retcode = VPCheckClassifier(vpc)) != VP_OK)
	    return(retcode);
	vpDestroyClassifiedVolume(vpc);
	InitRLE(vpc);
    }

    /* encode scanline */
    EncodeScanline(vpc, voxels, NULL, NULL);

    return(VP_OK);
}

/*
 * EncodeScanline
 *
 * Classify and run-length encode one scanline of voxels.
 */

static void
EncodeScanline(vpc, voxels, octree, level_stack)
vpContext *vpc;		/* context */
void *voxels;		/* voxel scanline */
MinMaxOctree *octree;	/* octree for fast classification */
MMOctreeLevel level_stack[VP_MAX_OCTREE_LEVELS]; /* stack for octree */
{
    ConstructionBuffer *cbuf;	/* state preserved between calls */
    RunData rundata_x;		/* statistics for current x-axis run */
    RunData *rundata_y;		/* statistics for all y-axis runs */
    RunData *rundata_z;		/* statistics for all z-axis runs */
    int skip_rle_x;		/* if true, do not compute rle_x */
    int skip_rle_y;		/* if true, do not compute rle_y */
    int skip_rle_z;		/* if true, do not compute rle_z */
    int y, z;			/* y and z coordinates of scanline */
    int x;			/* index of current voxel in scanline */
    int xlen, ylen, zlen;	/* volume dimensions */
    float opacity;		/* current value of the opacity (0.0-1.0) */
    float min_opacity;		/* low opacity threshold */
    int raw_bytes_per_voxel;	/* bytes in unclassified voxel */
    int run_length;		/* length of last run */
    char *rawvoxel;		/* current unclassified voxel */
    unsigned char *octree_run_ptr;	/* pointer to current run */
    int voxel_count;		/* voxels remaining in current run */
    int retcode;

    /* initialize */
    cbuf = vpc->cbuf;
    xlen = vpc->xlen;
    ylen = vpc->ylen;
    zlen = vpc->zlen;
    bzero(&rundata_x, sizeof(RunData));
    rundata_y = &cbuf->rundata_y[cbuf->next_z];
    rundata_z = &cbuf->rundata_z[cbuf->next_y * xlen];
    skip_rle_x = vpc->skip_rle_x;
    skip_rle_y = vpc->skip_rle_y;
    skip_rle_z = vpc->skip_rle_z;
    min_opacity = vpc->min_opacity;
    raw_bytes_per_voxel = vpc->raw_bytes_per_voxel;
    rawvoxel = voxels;
    y = cbuf->next_y;
    z = cbuf->next_z;
    if (octree != NULL) {
	if (cbuf->octree_scans_left == 0) {
	    cbuf->octree_scans_left = VPComputeScanRuns(vpc, level_stack,
		cbuf->octree_runs, VP_Z_AXIS, y, xlen);
	}
	cbuf->octree_scans_left--;
	octree_run_ptr = cbuf->octree_runs;
    }

    /* loop over voxels in the scanline */
    x = 0;
    while (x < xlen) {
	if (octree == NULL) {
	    /* no octree available, so process all of the voxels in the scan */
	    voxel_count = xlen;
	} else do {
	    /* skip over a run of zero voxels */
	    voxel_count = *octree_run_ptr++;
	    rundata_y += zlen * voxel_count;
	    rundata_z += voxel_count;
	    rawvoxel += raw_bytes_per_voxel * voxel_count;
	    x += voxel_count;

	    /* get length of nonzero voxel run */
	    voxel_count = *octree_run_ptr++;
	} while (voxel_count == 0 && x < xlen);

	/* process the voxels in the nonzero run */
	while (voxel_count-- > 0) {
	    /* compute opacity */
	    opacity = VPClassifyVoxel(vpc, rawvoxel);

	    /* compare opacity to threshold */
	    if (opacity > min_opacity) {
		/* voxel is non-transparent, so save it */
		CacheVoxel(vpc, opacity, rawvoxel);
		if (!skip_rle_x) {
		    rundata_y->p.p1.non_zero_count++;
		    CountNonZeroVoxel(rundata_y, y, 0, NULL);
		}
		if (!skip_rle_y) {
		    rundata_z->p.p1.non_zero_count++;
		    CountNonZeroVoxel(rundata_z, z, 0, NULL);
		}
		rundata_x.p.p1.non_zero_count++;
		CountNonZeroVoxel(&rundata_x, x, 0, vpc);
	    }

	    rundata_y += zlen;
	    rundata_z++;
	    rawvoxel += raw_bytes_per_voxel;
	    x++;
	} /* while (voxel_count) */
    } /* for x */

    /* finish off the statistics for the scanline */
    CountNonZeroVoxel(&rundata_x, x, 1, vpc);

    /* update saved state */
    cbuf->non_zero_count += rundata_x.p.p1.non_zero_count;
    cbuf->x_run_count += rundata_x.p.p1.run_count;

    /* check if this is the last scanline in the volume */
    if (++cbuf->next_y == ylen) {
	cbuf->next_y = 0;
	cbuf->octree_scans_left = 0;
	if (++cbuf->next_z == zlen) {
	    RepackClassifiedVolume(vpc);
	    DestroyConstructionBuffer(vpc, vpc->cbuf);
	    vpc->cbuf = NULL;
#ifdef DEBUG
	    printf("\r");
	    if (!skip_rle_x) {
		printf("Checking X scanline offsets....\n");
		VPCheckScanOffsets(vpc->rle_x, vpc->rle_bytes_per_voxel);
	    }
	    if (!skip_rle_y) {
		printf("Checking Y scanline offsets....\n");
		VPCheckScanOffsets(vpc->rle_y, vpc->rle_bytes_per_voxel);
	    }
	    if (!skip_rle_z) {
		printf("Checking Z scanline offsets....\n");
		VPCheckScanOffsets(vpc->rle_z, vpc->rle_bytes_per_voxel);
	    }
	    VPValidateClassifiedVolume(vpc);
#endif
	}
    }
}

/*
 * InitRLE
 *
 * Initialize in preparation for creating a new run-length encoded volume.
 */

static void
InitRLE(vpc)
vpContext *vpc;
{
    int f;
    int rle_bytes_per_voxel, size, offset;
    int maxsize = 0;

    /* find out how many bytes of the raw voxel are used for shading */
    rle_bytes_per_voxel = 0;
    for (f = 0; f < vpc->num_shade_fields; f++) {
	size = vpc->field_size[f];
	offset = vpc->field_offset[f] + size;
	if (offset > rle_bytes_per_voxel)
	    rle_bytes_per_voxel = offset;
	if (size > maxsize)
	    maxsize = size;
    }

    /* add one byte for opacity and then pad to the byte boundary of
       the largest field in the voxel; this ensures alignment; the
       opacity is always stored in the last byte (so the padding
       is in between the shading fields and the opacity field) */
    rle_bytes_per_voxel++;
    rle_bytes_per_voxel = (rle_bytes_per_voxel + maxsize-1) & ~(maxsize-1);
    vpc->rle_bytes_per_voxel = rle_bytes_per_voxel;

    /* initialize construction buffer */
    vpc->cbuf = CreateConstructionBuffer(vpc);
}

/*
 * CacheVoxel
 *
 * Cache one voxel's data in the ConstructionBuffer.
 */

static void
CacheVoxel(vpc, opacity, rawvoxel)
vpContext *vpc;			/* context */
double opacity;			/* voxel's opacity */
void *rawvoxel;			/* raw voxel data */
{
    ConstructionBuffer *cbuf;	/* state during construction of volume */
    GBuffer *data_buf;		/* storage for cached voxels */
    void *data_ptr;		/* pointer to current voxel's storage */
    int rle_bytes_per_voxel;	/* bytes per voxel after classification */
    int opc_int;		/* quantized opacity */

    /* initialize */
    cbuf = vpc->cbuf;
    data_buf = cbuf->data_buf_tail;
    rle_bytes_per_voxel = vpc->rle_bytes_per_voxel;

    /* allocate more memory if necessary */
    if (data_buf->bytes_left < rle_bytes_per_voxel) {
	/* allocate more memory */
	data_buf->next = CreateGBuffer(vpc);
	data_buf = data_buf->next;
	cbuf->data_buf_tail = data_buf;
    }
    data_ptr = data_buf->data_ptr;

    /* copy the voxel fields required for shading */
    bcopy(rawvoxel, data_ptr, rle_bytes_per_voxel-1);

    /* quantize and store the opacity */
    opc_int = opacity*255.;
    if (opc_int > 255)
	opc_int = 255;
    else if (opc_int < 0)
	opc_int = 0;
    ByteField(data_ptr, rle_bytes_per_voxel-1) = opc_int;

    data_buf->data_ptr += rle_bytes_per_voxel;
    data_buf->bytes_left -= rle_bytes_per_voxel;
}

/*
 * CacheLength
 *
 * Cache one run length in the ConstructionBuffer.
 */

static void
CacheLength(vpc, length)
vpContext *vpc;
int length;
{
    GBuffer *lengths_buf;

    lengths_buf = vpc->cbuf->lengths_buf_tail;
    if (lengths_buf->bytes_left == 0) {
	/* allocate more memory */
	lengths_buf->next = CreateGBuffer(vpc);
	lengths_buf = lengths_buf->next;
	vpc->cbuf->lengths_buf_tail = lengths_buf;
    }
    *(lengths_buf->data_ptr)++ = length;
    lengths_buf->bytes_left--;
}

/*
 * CountNonZeroVoxel
 *
 * Update the run count and nonzero voxel count for a voxel scanline.
 * This routine adds one non-zero voxel to the scanline.  Index
 * indicates the position of the voxel in the scanline.  If that
 * position is not immediately adjacent to the last non-zero voxel then
 * a run of zero voxels is added as well.
 *
 * If the vpc argument is non-NULL then the lengths of any completed
 * runs are written out to the run length buffer.
 */

static void
CountNonZeroVoxel(rundata, index, end_of_scan, vpc)
RunData *rundata;	/* statistics for the scanline */
int index;		/* index of voxel in scanline */
int end_of_scan;	/* if true then finish the scanline instead of
			   adding a voxel */
vpContext *vpc;		/* context in which run lengths should be stored */
{
    int run_length;

    if (rundata->next_index != index) {
	/* a run of zero voxels intervenes between the current index
	   and the last nonzero voxel that was processed */

	if (rundata->next_index != 0) {
	    /* add the last nonzero run to the statistics */
	    run_length = rundata->run_length;
	    while (run_length > 255) {
		/* run is too long, so split it */
		run_length -= 255;
		rundata->p.p1.run_count += 2;
		if (vpc != NULL) {
		    CacheLength(vpc, 255);
		    CacheLength(vpc, 0);
		}
	    }
	    rundata->p.p1.run_count++;
	    if (vpc != NULL)
		CacheLength(vpc, run_length);
	}

	/* add the last zero run to the statistics */
	run_length = index - rundata->next_index;
	while (run_length > 255) {
	    /* run is too long, so split it */
	    run_length -= 255;
	    rundata->p.p1.run_count += 2;
	    if (vpc != NULL) {
		CacheLength(vpc, 255);
		CacheLength(vpc, 0);
	    }
	}
	rundata->p.p1.run_count++;
	if (vpc != NULL)
	    CacheLength(vpc, run_length);

	if (end_of_scan) {
	    /* add a zero-length nonzero run to finish the scanline */
	    rundata->p.p1.run_count++;
	    if (vpc != NULL)
		CacheLength(vpc, 0);
	} else {
	    /* start the new run */
	    rundata->run_length = 1;
	    rundata->next_index = index + 1;
	}
    } else if (!end_of_scan) {
	/* add a nonzero voxel to the current run */
	if (rundata->next_index == 0) {
	    rundata->p.p1.run_count++;	/* count initial zero run */
	    if (vpc != NULL)
		CacheLength(vpc, 0);
	}
	rundata->run_length++;
	rundata->next_index = index + 1;
    } else {
	/* scanline ends with a nonzero voxel run */
	run_length = rundata->run_length;
	while (run_length > 255) {
	    /* run is too long, so split it */
	    run_length -= 255;
	    rundata->p.p1.run_count += 2;
	    if (vpc != NULL) {
		CacheLength(vpc, 255);
		CacheLength(vpc, 0);
	    }
	}
	rundata->p.p1.run_count++;
	if (vpc != NULL)
	    CacheLength(vpc, run_length);
    }
}

/*
 * RepackClassifiedVolume
 *
 * Repack the data in the ConstructionBuffer after the last call to
 * vpClassifyScanline.  This procedure creates the three run-length
 * encoded copies of the classified voxels.
 */

static void
RepackClassifiedVolume(vpc)
vpContext *vpc;
{
    int xlen, ylen, zlen;	/* volume dimensions */
    int x, y, z;		/* voxel coordinates */
    int non_zero_count;		/* number of nonzero voxels in volume */
    int rle_bytes_per_voxel;	/* bytes per classified voxel */
    int skip_rle_x;		/* if true, compute rle_x */
    int skip_rle_y;		/* if true, compute rle_y */
    int skip_rle_z;		/* if true, compute rle_z */
    char *x_data;		/* voxel data for x viewpoint */
    char *y_data;		/* voxel data for y viewpoint */
    char *z_data;		/* voxel data for z viewpoint */
    unsigned char *x_lengths;	/* run length for x viewpoint */
    unsigned char *y_lengths;	/* run length for y viewpoint */
    unsigned char *z_lengths;	/* run length for z viewpoint */
    int z_data_offset;		/* offset to current data value in z volume */
    int z_length_offset;        /* offset to current length value in z volume*/
    GBuffer *data_buf;		/* next GBuffer containing voxel data */
    char *data;			/* pointer to next voxel */
    int data_bytes_left;	/* bytes of data left in data buffer */
    GBuffer *lengths_buf;	/* next GBuffer containing length data */
    unsigned char *lengths;	/* pointer to next length */
    int lengths_bytes_left;	/* bytes of data left in lengths buffer */
    int x_run_length;		/* length of current x-scanline run */
    int is_non_zero;		/* true if current x-scanline run is nonzero */
    RunData *rundata_y;		/* statistics for y-axis runs */
    RunData *rundata_z;		/* statistics for z-axis runs */

    /* initialize */
    xlen = vpc->xlen;
    ylen = vpc->ylen;
    zlen = vpc->zlen;
    non_zero_count = vpc->cbuf->non_zero_count;
    rle_bytes_per_voxel = vpc->rle_bytes_per_voxel;
    skip_rle_x = vpc->skip_rle_x;
    skip_rle_y = vpc->skip_rle_y;
    skip_rle_z = vpc->skip_rle_z;

    /* check for empty volume */
    if (non_zero_count == 0) {
	if (!skip_rle_x)
	    vpc->rle_x = CreateEmptyRLEVoxels(vpc, ylen, zlen, xlen);
	if (!skip_rle_y)
	    vpc->rle_y = CreateEmptyRLEVoxels(vpc, zlen, xlen, ylen);
	if (!skip_rle_z)
	    vpc->rle_z = CreateEmptyRLEVoxels(vpc, xlen, ylen, zlen);
	return;
    }

    /* allocate space for y-axis runs (used for the x viewing axis) */
    if (!skip_rle_x) {
	vpc->rle_x = CreateRLEVoxelsFromRunData(vpc, ylen, zlen, xlen,
	    non_zero_count, vpc->cbuf->rundata_y, rle_bytes_per_voxel);
	x_data = vpc->rle_x->data;
	x_lengths = vpc->rle_x->run_lengths;
    }

    /* allocate space for z-axis runs (used for the y viewing axis) */
    if (!skip_rle_y) {
	vpc->rle_y = CreateRLEVoxelsFromRunData(vpc, zlen, xlen, ylen, 
	    non_zero_count, vpc->cbuf->rundata_z, rle_bytes_per_voxel);
	y_data = vpc->rle_y->data;
	y_lengths = vpc->rle_y->run_lengths;
    }

    /* allocate space for x-axis runs (used for the z viewing axis) */
    if (!skip_rle_z) {
	vpc->rle_z = VPCreateRLEVoxels(vpc, xlen, ylen, zlen, non_zero_count,
	    vpc->cbuf->x_run_count, rle_bytes_per_voxel);
	Alloc(vpc, vpc->rle_z->scan_offsets, ScanOffset *,
	      zlen*sizeof(ScanOffset), "scan_offsets");
	vpc->rle_z->scan_offsets_per_slice = 1;
	z_data = vpc->rle_z->data;
	z_lengths = vpc->rle_z->run_lengths;
	z_data_offset = 0;
	z_length_offset = 0;
    }

    /* copy data into the three RLEVoxels structures */
    data_buf = vpc->cbuf->data_buf_head;
    data = NULL;
    data_bytes_left = 0;
    lengths_buf = vpc->cbuf->lengths_buf_head;
    lengths = NULL;
    lengths_bytes_left = 0;
    x_run_length = 0;
    is_non_zero = 1;
    for (z = 0; z < zlen; z++) {
	ReportStatus(vpc, (double)z / (double)zlen);
	if (!skip_rle_z) {
	    vpc->rle_z->scan_offsets[z].first_data = z_data_offset;
	    vpc->rle_z->scan_offsets[z].first_len =
		(z_length_offset & 0x1) ? z_length_offset + 1 :
		z_length_offset;
	}
	rundata_z = vpc->cbuf->rundata_z;
	for (y = 0; y < ylen; y++) {
	    rundata_y = &vpc->cbuf->rundata_y[z];
	    for (x = 0; x < xlen; x++) {
		while (x_run_length == 0) {
		    /* find length of next run */
		    if (lengths_bytes_left <= 0) {
			/* go to next lengths buffer */
			lengths = (unsigned char *)lengths_buf->data;
			lengths_bytes_left = GBUFFER_SIZE -
			    		     lengths_buf->bytes_left;
			lengths_buf = lengths_buf->next;
			if (!skip_rle_z) {
			    bcopy(lengths, z_lengths, lengths_bytes_left);
			    z_lengths += lengths_bytes_left;
			}
		    }
		    x_run_length = *lengths++;
		    lengths_bytes_left--;
		    is_non_zero = !is_non_zero;
		    z_length_offset++;
		}
		x_run_length--;	/* consume one voxel */
		if (is_non_zero) {
		    /* find the data for this voxel */
		    if (data_bytes_left <= 0) {
			data = data_buf->data;
			data_bytes_left = GBUFFER_SIZE - data_buf->bytes_left;
			data_buf = data_buf->next;
			if (!skip_rle_z) {
			    bcopy(data, z_data, data_bytes_left);
			    z_data = (char *)z_data + data_bytes_left;
			}
		    }

		    /* store voxel */
		    if (!skip_rle_x) {
			StoreNonZeroVoxel(data, rle_bytes_per_voxel, x_data,
					  x_lengths, rundata_y, y);
		    }
		    if (!skip_rle_y) {
			StoreNonZeroVoxel(data, rle_bytes_per_voxel, y_data,
					  y_lengths, rundata_z, z);
		    }
		    data += rle_bytes_per_voxel;
		    data_bytes_left -= rle_bytes_per_voxel;
		    z_data_offset += rle_bytes_per_voxel;
		}
		rundata_y += zlen;
		rundata_z++;
	    } /* for x */
	} /* for y */
    } /* for z */
    ReportStatus(vpc, 1.0);

    if (!skip_rle_x)
	PadScanlines(ylen, zlen, xlen, vpc->cbuf->rundata_y, x_lengths);
    if (!skip_rle_y)
	PadScanlines(zlen, xlen, ylen, vpc->cbuf->rundata_z, y_lengths);
}

/*
 * CreateEmptyRLEVoxels
 *
 * Create an empty RLEVoxels object (all voxels transparent).
 */

static RLEVoxels *
CreateEmptyRLEVoxels(vpc, ilen, jlen, klen)
vpContext *vpc;
int ilen, jlen, klen;
{
    RLEVoxels *rle_voxels;
    int j, k;
    unsigned char *run_lengths;
    ScanOffset *scan_offsets;

    rle_voxels = VPCreateRLEVoxels(vpc, ilen, jlen, klen, 1, 2*jlen*klen, 1);
    Alloc(vpc, rle_voxels->scan_offsets, ScanOffset *, klen*sizeof(ScanOffset),
	  "scan_offsets");
    rle_voxels->scan_offsets_per_slice = 1;
    run_lengths = rle_voxels->run_lengths;
    scan_offsets = rle_voxels->scan_offsets;
    for (k = 0; k < klen; k++) {
	scan_offsets->first_len = k*jlen*2;
	scan_offsets->first_data = 0;
	scan_offsets++;
	for (j = 0; j < jlen; j++) {
	    *run_lengths++ = ilen;
	    *run_lengths++ = 0;
	}
    }
    return(rle_voxels);
}

/*
 * CreateRLEVoxelsFromRunData
 *
 * Allocate an RLEVoxels structure using the data in a RunData array
 * in order to determine the required size.  Also reinitialize the RunData
 * array with pointers to the RLEVoxels data for the beginning of
 * each scanline.
 */

static RLEVoxels *
CreateRLEVoxelsFromRunData(vpc, ilen, jlen, klen, non_zero_count, run_data,
			   rle_bytes_per_voxel)
vpContext *vpc;		/* context */
int ilen, jlen, klen;	/* size of volume in rotated object space */
int non_zero_count;	/* number of nonzero voxels in volume */
RunData *run_data;	/* array of run statistics (jlen*klen entries) */
int rle_bytes_per_voxel;/* number of bytes to allocate for each voxel */
{
    int j, k;			/* scanline and slice number */
    int scan_run_count;		/* runs in current scanline */
    int run_count;		/* runs in entire volume */
    int scan_non_zero_count;	/* nonzero voxels in scanline */
    int data_offset;		/* scanline's offset in RLEVoxels->data */
    int length_offset;		/* scanline's offset in
				   RLEVoxels->run_lengths */
    ScanOffset *slice_offset;	/* offsets for each slice */
    RLEVoxels *rle_voxels;	/* return value */

    Alloc(vpc, slice_offset, ScanOffset *, klen*sizeof(ScanOffset),
	  "scan_offsets");

    /* accumulate the statistics for the last run in each scanline,
       count the total number of runs, and store the data and length
       offsets for the beginning of the scanline */
    data_offset = 0;
    length_offset = 0;
    run_count = 0;
    for (k = 0; k < klen; k++) {
	slice_offset[k].first_data = data_offset;
	slice_offset[k].first_len = length_offset;
	for (j = 0; j < jlen; j++) {
	    CountNonZeroVoxel(run_data, ilen, 1, NULL);
	    scan_non_zero_count = run_data->p.p1.non_zero_count;
	    scan_run_count = run_data->p.p1.run_count;
	    run_data->run_length = 0;
	    run_data->next_index = 0;
	    run_data->p.p2.data_offset = data_offset;
	    run_data->p.p2.length_offset = length_offset;
	    data_offset += scan_non_zero_count * rle_bytes_per_voxel;
	    length_offset += scan_run_count * sizeof(unsigned char);
	    run_count += scan_run_count;
	    run_data++;
	}
    }

    /* allocate space */
    rle_voxels = VPCreateRLEVoxels(vpc, ilen, jlen, klen, non_zero_count,
				   run_count, rle_bytes_per_voxel);
    rle_voxels->scan_offsets_per_slice = 1;
    rle_voxels->scan_offsets = slice_offset;
    return(rle_voxels);
}

/*
 * StoreNonZeroVoxel
 *
 * Store a nonzero voxel in an RLEVoxels object.  This function is
 * just like CountNonZeroVoxel except that it actually stores voxel data.
 */

static void
StoreNonZeroVoxel(voxel, rle_bytes_per_voxel, data, lengths, rundata, index)
void *voxel;		/* input voxel data */
int rle_bytes_per_voxel;/* size of voxel */
void *data;		/* location to store voxel */
unsigned char *lengths;	/* location to store run lengths */
RunData *rundata;	/* run length statistics for current voxel scanline */
int index;		/* index of voxel in scanline */
{
    int run_length;

    /* store the voxel */
    if (voxel != NULL) {
	bcopy(voxel, (char *)data + rundata->p.p2.data_offset,
	      rle_bytes_per_voxel);
	rundata->p.p2.data_offset += rle_bytes_per_voxel;
    }

    /* update run lengths */
    if (rundata->next_index != index) {
	/* a run of zero voxels intervenes between the current index
	   and the last nonzero voxel that was processed */

	if (rundata->next_index != 0) {
	    /* add the last nonzero run to the statistics */
	    run_length = rundata->run_length;
	    while (run_length > 255) {
		/* run is too long, so split it */
		run_length -= 255;
		lengths[rundata->p.p2.length_offset++] = 255;
		lengths[rundata->p.p2.length_offset++] = 0;
	    }
	    lengths[rundata->p.p2.length_offset++] = run_length;
	}

	/* add the last zero run to the statistics */
	run_length = index - rundata->next_index;
	while (run_length > 255) {
	    /* run is too long, so split it */
	    run_length -= 255;
	    lengths[rundata->p.p2.length_offset++] = 255;
	    lengths[rundata->p.p2.length_offset++] = 0;
	}
	lengths[rundata->p.p2.length_offset++] = run_length;

	if (voxel == NULL) {
	    /* add a zero-length nonzero run to finish the scanline */
	    lengths[rundata->p.p2.length_offset++] = 0;
	} else {
	    /* start the new run */
	    rundata->run_length = 1;
	    rundata->next_index = index + 1;
	}
    } else if (voxel != NULL) {
	/* add a nonzero voxel to the current run */
	if (rundata->next_index == 0) {
	    lengths[rundata->p.p2.length_offset++] = 0;
	}
	rundata->run_length++;
	rundata->next_index = index + 1;
    } else {
	/* scanline ends with a nonzero voxel run */
	run_length = rundata->run_length;
	while (run_length > 255) {
	    /* run is too long, so split it */
	    run_length -= 255;
	    lengths[rundata->p.p2.length_offset++] = 255;
	    lengths[rundata->p.p2.length_offset++] = 0;
	}
	lengths[rundata->p.p2.length_offset++] = run_length;
    }
}

/*
 * PadScanlines
 *
 * Make sure each scanline has an even number of runs.
 */

static void
PadScanlines(ilen, jlen, klen, run_data, lengths)
int ilen, jlen, klen;	/* size of volume in rotated object space */
RunData *run_data;	/* array of run statistics (jlen*klen entries) */
unsigned char *lengths;	/* beginning of run lengths array */
{
    int scan_count;		/* number of scanlines */
    int scan_run_count;		/* number of runs in scanline */
    int scan;			/* current scanline number */

    scan_count = jlen * klen;
    for (scan = 0; scan < scan_count; scan++) {
	StoreNonZeroVoxel(NULL, 0, NULL, lengths, run_data, ilen);
	run_data++;
    }
}

/*
 * VPCreateRLEVoxels
 *
 *
 * Allocate a new RLEVoxels object.
 */

RLEVoxels *
VPCreateRLEVoxels(vpc, ilen, jlen, klen, data_count, run_count,
		  rle_bytes_per_voxel)
vpContext *vpc;		/* context */
int ilen, jlen, klen;	/* dimensions in rotated object space */
int data_count;		/* number of nonzero voxels */
int run_count;		/* number of runs */
int rle_bytes_per_voxel;/* bytes of storage for one voxel */
{
    RLEVoxels *rle_voxels;

    Alloc(vpc, rle_voxels, RLEVoxels *, sizeof(RLEVoxels), "RLEVoxels");
    rle_voxels->ilen = ilen;
    rle_voxels->jlen = jlen;
    rle_voxels->klen = klen;
    rle_voxels->run_count = run_count;
    if (run_count > 0) {
	Alloc(vpc, rle_voxels->run_lengths, unsigned char *, run_count,
	      "run_lengths");
    } else {
	rle_voxels->run_lengths = NULL;
    }
    rle_voxels->data_count = data_count;
    if (data_count > 0) {
	Alloc(vpc, rle_voxels->data, void *, data_count * rle_bytes_per_voxel,
	      "voxeldata");
    } else {
	rle_voxels->data = NULL;
    }
    rle_voxels->scan_offsets_per_slice = 0;
    rle_voxels->scan_offsets = NULL;
    rle_voxels->mmapped = 0;
#ifdef INDEX_VOLUME
    rle_voxels->voxel_index = NULL;
#endif
    return(rle_voxels);
}

/*
 * VPDestroyRLEVoxels
 *
 * Destroy an RLEVoxels object.
 */

void
VPDestroyRLEVoxels(vpc, rle_voxels)
vpContext *vpc;
RLEVoxels *rle_voxels;
{
    if (!rle_voxels->mmapped) {
	if (rle_voxels->run_lengths != NULL)
	    Dealloc(vpc, rle_voxels->run_lengths);
	if (rle_voxels->data != NULL)
	    Dealloc(vpc, rle_voxels->data);
	if (rle_voxels->scan_offsets != NULL)
	    Dealloc(vpc, rle_voxels->scan_offsets);
    }
#ifdef INDEX_VOLUME
    if (rle_voxels->voxel_index != NULL)
	Dealloc(vpc, rle_voxels->voxel_index);
#endif
    Dealloc(vpc, rle_voxels);
}

/*
 * CreateConstructionBuffer
 *
 * Create a ConstructionBuffer object.
 */

static ConstructionBuffer *
CreateConstructionBuffer(vpc)
vpContext *vpc;
{
    ConstructionBuffer *cbuf;
    int xlen, ylen, zlen;

    xlen = vpc->xlen;
    ylen = vpc->ylen;
    zlen = vpc->zlen;
    Alloc(vpc, cbuf, ConstructionBuffer *, sizeof(ConstructionBuffer),
	  "ConstructionBuffer");
    Alloc(vpc, cbuf->rundata_y, RunData *, xlen*zlen*sizeof(RunData),
	  "rundata_y");
    Alloc(vpc, cbuf->rundata_z, RunData *, ylen*xlen*sizeof(RunData),
	  "rundata_z");
    bzero(cbuf->rundata_y, xlen*zlen*sizeof(RunData));
    bzero(cbuf->rundata_z, ylen*xlen*sizeof(RunData));
    cbuf->data_buf_head = CreateGBuffer(vpc);
    cbuf->data_buf_tail = cbuf->data_buf_head;
    cbuf->lengths_buf_head = CreateGBuffer(vpc);
    cbuf->lengths_buf_tail = cbuf->lengths_buf_head;
    cbuf->non_zero_count = 0;
    cbuf->x_run_count = 0;
    cbuf->octree_scans_left = 0;
    cbuf->next_z = 0;
    cbuf->next_y = 0;
    return(cbuf);
}

/*
 * DestroyConstructionBuffer
 *
 * Destroy a ConstructionBuffer object.
 */

static void
DestroyConstructionBuffer(vpc, cbuf)
vpContext *vpc;
ConstructionBuffer *cbuf;
{
    GBuffer *gbuf, *next_gbuf;

    Dealloc(vpc, cbuf->rundata_y);
    Dealloc(vpc, cbuf->rundata_z);
    for (gbuf = cbuf->data_buf_head; gbuf != NULL; gbuf = next_gbuf) {
	next_gbuf = gbuf->next;
	DestroyGBuffer(vpc, gbuf);
    }
    for (gbuf = cbuf->lengths_buf_head; gbuf != NULL; gbuf = next_gbuf) {
	next_gbuf = gbuf->next;
	DestroyGBuffer(vpc, gbuf);
    }
    Dealloc(vpc, cbuf);
}

/*
 * CreateGBuffer
 *
 * Create a GBuffer object.
 */

static GBuffer *
CreateGBuffer(vpc)
vpContext *vpc;
{
    GBuffer *gbuf;

    Alloc(vpc, gbuf, GBuffer *, sizeof(GBuffer), "GBuffer");
    gbuf->bytes_left = GBUFFER_SIZE;
    gbuf->data_ptr = gbuf->data;
    gbuf->next = NULL;
    return(gbuf);
}

/*
 * DestroyGBuffer
 *
 * Destroy a GBuffer.
 */

static void
DestroyGBuffer(vpc, gbuf)
vpContext *vpc;
GBuffer *gbuf;
{
    Dealloc(vpc, gbuf);
}

/*
 * vpDestroyClassifiedVolume
 *
 * Free all memory associated with a classified volume.
 */

vpResult
vpDestroyClassifiedVolume(vpc)
vpContext *vpc;
{
    if (vpc->cbuf != NULL) {
	DestroyConstructionBuffer(vpc, vpc->cbuf);
	vpc->cbuf = NULL;
    }
    if (vpc->rle_x != NULL) {
	VPDestroyRLEVoxels(vpc, vpc->rle_x);
	vpc->rle_x = NULL;
    }
    if (vpc->rle_y != NULL) {
	VPDestroyRLEVoxels(vpc, vpc->rle_y);
	vpc->rle_y = NULL;
    }
    if (vpc->rle_z != NULL) {
	VPDestroyRLEVoxels(vpc, vpc->rle_z);
	vpc->rle_z = NULL;
    }
    return(VP_OK);
}

#ifdef INDEX_VOLUME
/*
 * vpComputeRLEIndex
 *
 * Compute indexes for the classified volume data in a context.
 */

vpResult
vpComputeRLEIndex(vpc)
vpContext *vpc;
{
    vpResult result;

    if ((result = VPComputeRLEScanOffsets(vpc)) != VP_OK)
	return(result);
    if (vpc->rle_x != NULL) {
	if ((result = ComputeIndex(vpc, vpc->rle_x)) != VP_OK)
	    return(result);
    }
    if (vpc->rle_y != NULL) {
	if ((result = ComputeIndex(vpc, vpc->rle_y)) != VP_OK)
	    return(result);
    }
    if (vpc->rle_z != NULL) {
	if ((result = ComputeIndex(vpc, vpc->rle_z)) != VP_OK)
	    return(result);
    }
    return(VP_OK);
}

/*
 * ComputeIndex
 *
 * Compute an index that maps 3D voxel coordinates to the RLE run data
 * for the corresponding voxel.  The values stored in the index are
 * byte offsets to the beginning of the run containing the voxel,
 * plus a count indicating the position of the voxel in the run.
 * Return value is a result code.
 */

static vpResult
ComputeIndex(vpc, rle_voxels)
vpContext *vpc;
RLEVoxels *rle_voxels;
{
    int ilen, jlen, klen;	/* size of volume */
    unsigned char *RLElen;	/* pointer to current run length */
    VoxelLocation *index;	/* pointer to current index entry */
    int i, j, k;		/* current voxel coordinates */
    unsigned len_offset;	/* offset in bytes from beginning of
				   scanline to current run length */
    unsigned data_offset;	/* offset in bytes from beginning of
				   scanline to current voxel data */
    int run_is_zero;		/* true if current run is a zero run */
    int run_count;		/* voxels left in current run */
    int voxel_size;		/* size of a voxel in bytes */

    ilen = rle_voxels->ilen;
    jlen = rle_voxels->jlen;
    klen = rle_voxels->klen;
    RLElen = rle_voxels->run_lengths;
    if (rle_voxels->scan_offsets_per_slice != jlen)
	return(VPERROR_BAD_VOLUME);
    if (rle_voxels->voxel_index == NULL) {
	Alloc(vpc, rle_voxels->voxel_index, VoxelLocation *,
	      ilen * jlen * klen * sizeof(VoxelLocation), "voxel_index");
    }
    index = rle_voxels->voxel_index;
    voxel_size = vpc->rle_bytes_per_voxel;
    run_is_zero = 0;
    run_count = 0;
    for (k = 0; k < klen; k++) {
	for (j = 0; j < jlen; j++) {
	    ASSERT(run_is_zero == 0);
	    ASSERT(run_count == 0);
	    len_offset = 0;
	    data_offset = 0;
	    for (i = 0; i < ilen; i++) {
		/* record index for current voxel */
		if (len_offset > 256) {
		    Dealloc(vpc, rle_voxels->voxel_index);
		    rle_voxels->voxel_index = NULL;
		    return(VPERROR_LIMIT_EXCEEDED);
		}
		index->run_count = run_count;
		index->len_offset = len_offset;
		if (run_is_zero)
		    index->data_offset = data_offset | INDEX_RUN_IS_ZERO;
		else
		    index->data_offset = data_offset;
		index++;

		/* go on to next voxel */
		while (run_count == 0) {
		    run_count = *RLElen++;
		    run_is_zero = !run_is_zero;
		    len_offset++;
		}
		run_count--;
		if (!run_is_zero)
		    data_offset += voxel_size;
	    }
	    ASSERT(run_count == 0);
	    if (run_is_zero) {
		run_count = *RLElen++;
		run_is_zero = !run_is_zero;
		len_offset++;
	    }
	}
    }
    return(VP_OK);
}
#endif /* INDEX_VOLUME */

/*
 * VPComputeRLEScanOffsets
 *
 * Recompute the scan_offsets arrays for the classified volume data in
 * a context.  Return value is a result code.
 */

vpResult
VPComputeRLEScanOffsets(vpc)
vpContext *vpc;
{
    vpResult result;

    if (vpc->rle_x != NULL) {
	if ((result = ComputeScanOffsets(vpc, vpc->rle_x)) != VP_OK)
	    return(result);
#ifdef DEBUG
	VPCheckScanOffsets(vpc->rle_x, vpc->rle_bytes_per_voxel);
#endif
    }

    if (vpc->rle_y != NULL) {
	if ((result = ComputeScanOffsets(vpc, vpc->rle_y)) != VP_OK)
	    return(result);
#ifdef DEBUG
	VPCheckScanOffsets(vpc->rle_y, vpc->rle_bytes_per_voxel);
#endif
    }

    if (vpc->rle_z != NULL) {
	if ((result = ComputeScanOffsets(vpc, vpc->rle_z)) != VP_OK)
	    return(result);
#ifdef DEBUG
	VPCheckScanOffsets(vpc->rle_z, vpc->rle_bytes_per_voxel);
#endif
    }
    return(VP_OK);
}

/*
 * ComputeScanOffsets
 *
 * Recompute the scan_offsets array for a classified volume.
 * Return value is a result code.
 */

static vpResult
ComputeScanOffsets(vpc, rle_voxels)
vpContext *vpc;
RLEVoxels *rle_voxels;
{
    int ilen, jlen, klen;	/* size of volume */
    unsigned char *RLElen;	/* pointer to current run length */
    ScanOffset *scan_offset;	/* pointer to current scanline offset */
    int i, j, k;		/* current voxel coordinates */
    unsigned len_offset;	/* offset in bytes from beginning of
				   run lengths to current run length */
    unsigned data_offset;	/* offset in bytes from beginning of
				   voxel data to current voxel data */
    int voxel_size;		/* size of a voxel in bytes */
    int zerocount, nonzerocount;

    if (rle_voxels->mmapped)
	return(VPERROR_IO);
    ilen = rle_voxels->ilen;
    jlen = rle_voxels->jlen;
    klen = rle_voxels->klen;
    RLElen = rle_voxels->run_lengths;
    if (rle_voxels->scan_offsets_per_slice != jlen) {
	if (rle_voxels->scan_offsets != NULL)
	    Dealloc(vpc, rle_voxels->scan_offsets);
	Alloc(vpc, rle_voxels->scan_offsets, ScanOffset *,
	      klen * jlen * sizeof(ScanOffset), "scan_offsets");
	rle_voxels->scan_offsets_per_slice = jlen;
    }
    scan_offset = rle_voxels->scan_offsets;
    len_offset = 0;
    data_offset = 0;
    voxel_size = vpc->rle_bytes_per_voxel;

    for (k = 0; k < klen; k++) {
	for (j = 0; j < jlen; j++) {
	    scan_offset->first_len = len_offset;
	    scan_offset->first_data = data_offset;
	    scan_offset++;
	    for (i = 0; i < ilen; ) {
		zerocount = *RLElen++;	 /* get length of run of zeros */
		nonzerocount = *RLElen++;/* get length of run of non-zeros */
		len_offset += 2;
		data_offset += nonzerocount * voxel_size;
		i += zerocount + nonzerocount;
	    }
	    ASSERT(i == ilen);
	}
    }
    return(VP_OK);
}

#ifdef DEBUG
/*
 * VPCheckScanOffsets
 *
 * Check the scan_offsets field of an RLEVolume for internal consistency.
 */

void
VPCheckScanOffsets(rle_voxels, rle_bytes_per_voxel)
RLEVoxels *rle_voxels;
{
    int i, j, k;
    int ilen, jlen, klen;
    int run_length;
    int is_non_zero;
    unsigned char *run_length_ptr;
    int length_offset;
    int data_offset;
    int scan_offsets_per_slice;
    ScanOffset *scan_offset;

    scan_offsets_per_slice = rle_voxels->scan_offsets_per_slice;
    if (scan_offsets_per_slice == 0)
	return;
    ilen = rle_voxels->ilen;
    jlen = rle_voxels->jlen;
    klen = rle_voxels->klen;
    run_length_ptr = rle_voxels->run_lengths;
    run_length = 0;
    is_non_zero = 1;
    length_offset = 0;
    data_offset = 0;
    for (k = 0; k < klen; k++) {
	for (j = 0; j < jlen; j++) {
	    if (j < scan_offsets_per_slice) {
		scan_offset = &rle_voxels->scan_offsets[
			       k*scan_offsets_per_slice + j];
		if (scan_offset->first_len != length_offset) {
		    printf("Bad length offset on slice %d, scanline %d: ",k,j);
		    printf("%d should be %d\n", scan_offset->first_len,
			   length_offset);
		}
		if (scan_offset->first_data != data_offset) {
		    printf("Bad data offset on slice %d, scanline %d: ",k,j);
		    printf("%d should be %d\n", scan_offset->first_data,
			   data_offset);
		}
	    }
	    for (i = 0; i < ilen; i++) {
		while (run_length == 0) {
		    run_length = *run_length_ptr++;
		    is_non_zero = !is_non_zero;
		    length_offset++;
		}
		run_length--;
		if (is_non_zero)
		    data_offset += rle_bytes_per_voxel;
	    }
	    if (run_length != 0) {
		printf("Run did not terminate at end of scanline ");
		printf("on slice %d, scanline %d\n", k, j);
	    }
	    if (!is_non_zero) {
		if (*run_length_ptr++ != 0) {
		    printf("Missing zero run at end of scanline ");
		    printf("on slice %d, scanline %d\n", k, j);
		}
		is_non_zero = !is_non_zero;
		length_offset++;
	    }
	}
    }
}

/*
 * VPValidateClassifiedVolume
 *
 * Compare the classified volume to the unclassified volume.
 */

void
VPValidateClassifiedVolume(vpc)
vpContext *vpc;
{
    if (vpc->raw_voxels == NULL)
	return;
    if (vpc->rle_z != NULL) {
	printf("Checking Z view....\n");
	ValidateRLEVoxels(vpc, vpc->rle_z, vpc->xstride, vpc->ystride,
			  vpc->zstride, VP_Z_AXIS);
    }
    if (vpc->rle_y != NULL) {
	printf("Checking Y view....\n");
	ValidateRLEVoxels(vpc, vpc->rle_y, vpc->zstride, vpc->xstride,
			  vpc->ystride, VP_Y_AXIS);
    }
    if (vpc->rle_x != NULL) {
	printf("Checking X view....\n");
	ValidateRLEVoxels(vpc, vpc->rle_x, vpc->ystride, vpc->zstride,
			  vpc->xstride, VP_X_AXIS);
    }
}

static void
ValidateRLEVoxels(vpc, rle, istride, jstride, kstride, axis)
vpContext *vpc;
RLEVoxels *rle;
int istride, jstride, kstride;
int axis;
{
    char *rawvoxel;
    char *rlevoxel;
    unsigned char *lengths;
    int i, j, k;
    int count;
    int is_non_zero;
    int num_runs;
    float opacity;
    int ilen, jlen, klen;
    int founderror;
    int raw_opc_int;
    int rle_opc_int;
    int rle_bytes_per_voxel;

    rawvoxel = (char *)vpc->raw_voxels;
    rlevoxel = (char *)rle->data;
    lengths = rle->run_lengths;
    ilen = rle->ilen;
    jlen = rle->jlen;
    klen = rle->klen;
    rle_bytes_per_voxel = vpc->rle_bytes_per_voxel;
    founderror = 0;
    for (k = 0; k < klen; k++) {
	for (j = 0; j < jlen; j++) {
	    count = 0;
	    is_non_zero = 1;
	    num_runs = 0;
	    for (i = 0; i < ilen; i++) {
		while (count == 0) {
		    count = *lengths++;
		    is_non_zero = !is_non_zero;
		    if (++num_runs > rle->ilen)
			VPBug("runaway scan detected by ValidateRLEVoxels");
		}
		opacity = VPClassifyVoxel(vpc, rawvoxel);
		if (is_non_zero) {
		    if (opacity <= vpc->min_opacity &&
			fabs(opacity - vpc->min_opacity) > 0.001) {
			printf("\n");
			printf("**** zero rawvoxel in nonzero rlerun ****\n");
			printf("voxel (i,j,k)=(%d,%d,%d), viewaxis %d\n",
			       i, j, k, axis);
			printf("Actual opacity: %17.15f\n", opacity);
			printf("Threshold:      %17.15f\n", vpc->min_opacity);
			founderror = 1;
		    }
		    raw_opc_int = (int)rint(opacity*255.);
		    rle_opc_int = ByteField(rlevoxel, rle_bytes_per_voxel-1);
		    if (abs(raw_opc_int - rle_opc_int) > 1) {
			printf("\n");
			printf("**** rawvoxel and rlevoxel disagree ****\n");
			printf("voxel (i,j,k)=(%d,%d,%d), viewaxis %d\n",
			       i, j, k, axis);
			printf("Raw opacity: %3d\n", raw_opc_int);
			printf("RLE opacity: %3d\n", rle_opc_int);
			founderror = 1;
		    }
		    rlevoxel += rle_bytes_per_voxel;
		} else {
		    if (opacity > vpc->min_opacity &&
			fabs(opacity - vpc->min_opacity) > 0.001) {
			printf("\n");
			printf("**** nonzero rawvoxel in zero rlerun ****\n");
			printf("voxel (i,j,k)=(%d,%d,%d), viewaxis %d\n",
			       i, j, k, axis);
			printf("Actual opacity: %17.15f\n", opacity);
			printf("Threshold:      %17.15f\n", vpc->min_opacity);
			founderror = 1;
		    }
		}
		if (founderror) {
		    VPDumpClassifier(vpc);
		    VPBug("ValidateRLEVoxels found a problem");
		}
		rawvoxel += istride;
		count--;
	    }
	    if (count != 0)
		VPBug("Run did not terminate at end of scanline");
	    if (!is_non_zero) {
		if (*lengths++ != 0)
		    VPBug("Missing zero run at end of scanline");
		is_non_zero = !is_non_zero;
	    }
	    rawvoxel += jstride - ilen*istride;
	}
	rawvoxel += kstride - jlen*jstride;
    }
}
#endif

void
VPDumpView(vpc)
vpContext *vpc;
{
    int c;

    printf("MODEL:\n");
    for (c = 0; c < 4; c++) {
	printf("    %12.6f %12.6f %12.6f %12.6f\n",
	       vpc->transforms[VP_MODEL][c][0],
	       vpc->transforms[VP_MODEL][c][1],
	       vpc->transforms[VP_MODEL][c][2],
	       vpc->transforms[VP_MODEL][c][3]);
    }
    printf("VIEW:\n");
    for (c = 0; c < 4; c++) {
	printf("    %12.6f %12.6f %12.6f %12.6f\n",
	       vpc->transforms[VP_MODEL][c][0],
	       vpc->transforms[VP_MODEL][c][1],
	       vpc->transforms[VP_MODEL][c][2],
	       vpc->transforms[VP_MODEL][c][3]);
    }
    printf("PROJECT:\n");
    for (c = 0; c < 4; c++) {
	printf("    %12.6f %12.6f %12.6f %12.6f\n",
	       vpc->transforms[VP_MODEL][c][0],
	       vpc->transforms[VP_MODEL][c][1],
	       vpc->transforms[VP_MODEL][c][2],
	       vpc->transforms[VP_MODEL][c][3]);
    }
}

void
VPDumpClassifier(vpc)
vpContext *vpc;
{
    int c, d;

    for (d = 0; d < vpc->num_clsfy_params; d++) {
	printf("CLASSIFIER PARAM %d:\n   ", d);
	for (c = 0; c < vpc->field_max[vpc->param_field[d]]; c++) {
	    printf(" %8.6f", vpc->clsfy_table[d][c]);
	    if (c % 8 == 7)
		printf("\n   ");
	}
	printf("\n");
    }
}
