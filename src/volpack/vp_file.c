/*
 * vp_file.c
 *
 * Routines for loading and storing volume data in disk files.
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

static int StoreRLEVoxels ANSI_ARGS((vpContext *vpc, int fd,
    RLEVoxels *rle_voxels));
static int LoadRLEVoxels ANSI_ARGS((vpContext *vpc, int fd,
    RLEVoxels *rle_voxels, int offsets, int swab));
static void SwapWords ANSI_ARGS((void *data, unsigned size));
static void SwapVoxels ANSI_ARGS((vpContext *vpc, void *voxels,
    int num_voxels, int fields, int bytes_per_voxel));
#ifdef DEBUG
void VPCheckScanOffsets ANSI_ARGS((RLEVoxels *rle_voxels,
    int rle_bytes_per_voxel));
#endif
static void SwapOctreeNode ANSI_ARGS((vpContext *vpc, int level, void *node));
static int StoreTable ANSI_ARGS((vpContext *vpc, int fd, float *ptr,
    unsigned size));
static int LoadTable ANSI_ARGS((vpContext *vpc, int fd, float **ptr_ptr,
    unsigned *size_ptr));

/*******************************************************************
 * Classified Volume Files.                                        *
 *******************************************************************/

/* file header structure */
typedef struct {
    unsigned magic;		/* magic number for identification */
    unsigned xlen;		/* voxels in each dimension */
    unsigned ylen;
    unsigned zlen;
    unsigned bytes_per_voxel;	/* size of a classified voxel */
    unsigned num_shade_fields;	/* number of fields in a classified voxel
				   (not including opacity) */
    unsigned num_x_runs;	/* number of run lengths for X view */
    unsigned num_x_voxels;	/* number of nonzero voxels for X view */
    unsigned num_x_offsets;	/* number of offsets per slice for X view */
    unsigned num_y_runs;	/* number of run lengths for Y view */
    unsigned num_y_voxels;	/* number of nonzero voxels for Y view */
    unsigned num_y_offsets;	/* number of offsets per slice for Y view */
    unsigned num_z_runs;	/* number of run lengths for Z view */
    unsigned num_z_voxels;	/* number of nonzero voxels for Z view */
    unsigned num_z_offsets;	/* number of offsets per slice for Z view */
    float min_opacity;		/* low opacity threshold */
} RLEVoxelHdr;

/*
 * File layout:
 *   RLEVoxelHdr hdr;
 *   unsigned field_size[hdr.num_shade_fields];	   (size of each voxel field)
 *   unsigned field_offset[hdr.num_shade_fields];  (offset for each field)
 *   unsigned field_max[hdr.num_shade_fields];     (max. value of each field)
 *   padding to align to double word
 *   unsigned char x_run_lengths[hdr.num_x_runs];  (run lengths for X view)
 *   padding to align to double word
 *   char x_data[hdr.num_x_voxels*hdr.bytes_per_voxel]; (voxel data for X view)
 *   padding to align to double word
 *   ScanOffset x_offsets[hdr.num_x_offsets];	   (scanline offset for X view)
 *   padding to align to double word
 *   unsigned char y_run_lengths[hdr.num_y_runs];  (run lengths for Y view)
 *   padding to align to double word
 *   char y_data[hdr.num_y_voxels*hdr.bytes_per_voxel]; (voxel data for Y view)
 *   padding to align to double word
 *   ScanOffset y_offsets[hdr.num_y_offsets];	   (scanline offset for Y view)
 *   padding to align to double word
 *   unsigned char z_run_lengths[hdr.num_z_runs];  (run lengths for Z view)
 *   padding to align to double word
 *   char z_data[hdr.num_z_voxels*hdr.bytes_per_voxel]; (voxel data for Z view)
 *   padding to align to double word
 *   ScanOffset z_offsets[hdr.num_z_offsets];	   (scanline offset for Z view)
 *
 * The padding ensures that voxel data can be mapped into memory
 * without any word alignment problems.
 */

/*
 * vpStoreClassifiedVolume
 *
 * Store a run-length encoded, classified volume to a file.
 */

vpResult
vpStoreClassifiedVolume(vpc, fd)
vpContext *vpc;	/* context containing the volume */
int fd;		/* UNIX file descriptor open for writing */
{
    RLEVoxelHdr header;
    unsigned field_data[3*VP_MAX_FIELDS];
    int nsf, c;
    unsigned size;
    char pad_data[8];
    int pad_bytes;
    int retcode;

    /* check for errors */
    if ((retcode = VPCheckVoxelFields(vpc)) != VP_OK)
	return(retcode);

    /* write header */
    header.magic = VP_CVFILE_MAGIC;
    header.xlen = vpc->xlen;
    header.ylen = vpc->ylen;
    header.zlen = vpc->zlen;
    header.bytes_per_voxel = vpc->rle_bytes_per_voxel;
    header.num_shade_fields = vpc->num_shade_fields;
    if (vpc->rle_x == NULL) {
	header.num_x_runs = 0;
	header.num_x_voxels = 0;
	header.num_x_offsets = 0;
    } else {
	if ((retcode = VPCheckClassifiedVolume(vpc, VP_X_AXIS)) != VP_OK)
	    return(retcode);
	header.num_x_runs = vpc->rle_x->run_count;
	header.num_x_voxels = vpc->rle_x->data_count;
	header.num_x_offsets = vpc->rle_x->scan_offsets_per_slice;
    }
    if (vpc->rle_y == NULL) {
	header.num_y_runs = 0;
	header.num_y_voxels = 0;
	header.num_y_offsets = 0;
    } else {
	if ((retcode = VPCheckClassifiedVolume(vpc, VP_Y_AXIS)) != VP_OK)
	    return(retcode);
	header.num_y_runs = vpc->rle_y->run_count;
	header.num_y_voxels = vpc->rle_y->data_count;
	header.num_y_offsets = vpc->rle_y->scan_offsets_per_slice;
    }
    if (vpc->rle_z == NULL) {
	header.num_z_runs = 0;
	header.num_z_voxels = 0;
	header.num_z_offsets = 0;
    } else {
	if ((retcode = VPCheckClassifiedVolume(vpc, VP_Z_AXIS)) != VP_OK)
	    return(retcode);
	header.num_z_runs = vpc->rle_z->run_count;
	header.num_z_voxels = vpc->rle_z->data_count;
	header.num_z_offsets = vpc->rle_z->scan_offsets_per_slice;
    }
    header.min_opacity = vpc->min_opacity;
    if (vpc->write_func(fd, &header, sizeof(header)) != sizeof(header))
	return(VPSetError(vpc, VPERROR_IO));

    /* write voxel layout information */
    nsf = vpc->num_shade_fields;
    for (c = 0; c < nsf; c++) {
	field_data[c] = vpc->field_size[c];
	field_data[nsf + c] = vpc->field_offset[c];
	field_data[2*nsf + c] = vpc->field_max[c];
    }
    size = 3*nsf*sizeof(unsigned);
    if (vpc->write_func(fd, field_data, size) != size)
	return(VPSetError(vpc, VPERROR_IO));

    /* padding after header */
    pad_bytes = (8 - ((sizeof(header) + size) % 8)) & 0x7;
    if (pad_bytes > 0) {
	bzero(pad_data, pad_bytes);
	if (vpc->write_func(fd, pad_data, pad_bytes) != pad_bytes)
	    return(VPSetError(vpc, VPERROR_IO));
    }

    /* write data */
    if (vpc->rle_x != NULL) {
	if ((c = StoreRLEVoxels(vpc, fd, vpc->rle_x)) != VP_OK)
	    return(c);
    }
    if (vpc->rle_y != NULL) {
	if ((c = StoreRLEVoxels(vpc, fd, vpc->rle_y)) != VP_OK)
	    return(c);
    }
    if (vpc->rle_z != NULL) {
	if ((c = StoreRLEVoxels(vpc, fd, vpc->rle_z)) != VP_OK)
	    return(c);
    }

    return(VP_OK);
}

/*
 * StoreRLEVoxels
 *
 * Write an RLEVoxels structure to a file.
 */

static int
StoreRLEVoxels(vpc, fd, rle_voxels)
vpContext *vpc;
int fd;
RLEVoxels *rle_voxels;
{
    int size;
    char pad_data[8];
    int pad_bytes;

    bzero(pad_data, sizeof(pad_data));
    if (rle_voxels->run_count > 0) {
	size = rle_voxels->run_count;
	if (vpc->write_func(fd, rle_voxels->run_lengths, size) != size)
	    return(VPSetError(vpc, VPERROR_IO));

	pad_bytes = (8 - (size % 8)) & 0x7;
	if (pad_bytes > 0) {
	    if (vpc->write_func(fd, pad_data, pad_bytes) != pad_bytes)
		return(VPSetError(vpc, VPERROR_IO));
	}
    }
    if (rle_voxels->data_count > 0) {
	size = rle_voxels->data_count * vpc->rle_bytes_per_voxel;
	if (vpc->write_func(fd, rle_voxels->data, size) != size)
	    return(VPSetError(vpc, VPERROR_IO));

	pad_bytes = (8 - (size % 8)) & 0x7;
	if (pad_bytes > 0) {
	    if (vpc->write_func(fd, pad_data, pad_bytes) != pad_bytes)
		return(VPSetError(vpc, VPERROR_IO));
	}
    }
    if (rle_voxels->scan_offsets_per_slice > 0) {
	size = rle_voxels->scan_offsets_per_slice * rle_voxels->klen *
	    sizeof(ScanOffset);
	if (vpc->write_func(fd, rle_voxels->scan_offsets, size) != size)
	    return(VPSetError(vpc, VPERROR_IO));

	pad_bytes = (8 - (size % 8)) & 0x7;
	if (pad_bytes > 0) {
	    if (vpc->write_func(fd, pad_data, pad_bytes) != pad_bytes)
		return(VPSetError(vpc, VPERROR_IO));
	}
    }
    return(VP_OK);
}

/*
 * vpLoadClassifiedVolume
 *
 * Load a run-length encoded, classified volume from a file.
 */

vpResult
vpLoadClassifiedVolume(vpc, fd)
vpContext *vpc;	/* context to store the volume into */
int fd;		/* UNIX file descriptor open for reading */
{
    RLEVoxelHdr header;
    unsigned field_data[3*VP_MAX_FIELDS];
    int nsf, c, swab;
    unsigned size;
    unsigned char *data;
    char pad_data[8];
    int pad_bytes;
    unsigned x_run_offset;
    unsigned x_data_offset;
    unsigned x_offset_offset;
    unsigned y_run_offset;
    unsigned y_data_offset;
    unsigned y_offset_offset;
    unsigned z_run_offset;
    unsigned z_data_offset;
    unsigned z_offset_offset;
    int current_offset;
    int destroy_old_volume;

    /* read header */
    if (vpc->read_func(fd, &header, sizeof(header)) != sizeof(header))
	return(VPSetError(vpc, VPERROR_IO));
    swab = 0;
    if (header.magic != VP_CVFILE_MAGIC) {
	SwapWords(&header, sizeof(header));
	if (header.magic != VP_CVFILE_MAGIC)
	    return(VPSetError(vpc, VPERROR_BAD_FILE));
	swab = 1;
    }

    /* read voxel layout information */
    size = 3 * header.num_shade_fields * sizeof(unsigned);
    if (vpc->read_func(fd, field_data, size) != size)
	return(VPSetError(vpc, VPERROR_IO));
    if (swab)
	SwapWords(field_data, size);

    /* padding after header */
    pad_bytes = (8 - ((sizeof(header) + size) % 8)) & 0x7;
    if (pad_bytes > 0) {
	if (vpc->read_func(fd, pad_data, pad_bytes) != pad_bytes)
	    return(VPSetError(vpc, VPERROR_IO));
    }

    /* check for consistency with old volume data */
    destroy_old_volume = 0;
    if (vpc->xlen != header.xlen || vpc->ylen != header.ylen ||
	vpc->zlen != header.zlen ||
	vpc->raw_bytes_per_voxel < header.bytes_per_voxel ||
	vpc->num_voxel_fields < header.num_shade_fields)
	destroy_old_volume = 1;
    nsf = header.num_shade_fields;
    for (c = 0; c < nsf; c++) {
	if (vpc->field_size[c] != field_data[c] ||
	    vpc->field_offset[c] != field_data[nsf + c] ||
	    vpc->field_max[c] != field_data[2*nsf + c])
	    destroy_old_volume = 1;
    }
    if (destroy_old_volume) {
	vpDestroyClassifiedVolume(vpc);
	vpDestroyMinMaxOctree(vpc);
	vpc->raw_voxels = NULL;
	vpc->raw_voxels_size = 0;
	vpc->xstride = 0;
	vpc->ystride = 0;
	vpc->zstride = 0;
    }

    /* load new volume size */
    if (destroy_old_volume) {
	vpc->xlen = header.xlen;
	vpc->ylen = header.ylen;
	vpc->zlen = header.zlen;
	vpc->raw_bytes_per_voxel = header.bytes_per_voxel;
	nsf = header.num_shade_fields;
	vpc->num_voxel_fields = nsf;
	for (c = 0; c < nsf; c++) {
	    vpc->field_size[c] = field_data[c];
	    vpc->field_offset[c] = field_data[nsf + c];
	    vpc->field_max[c] = field_data[2*nsf + c];
	}
    }
    vpc->num_shade_fields = nsf;
    vpc->min_opacity = header.min_opacity;
    vpc->rle_bytes_per_voxel = header.bytes_per_voxel;

    /* load new volume data */
    if (vpc->mmap_func != NULL && !swab) {
	/* compute file offsets */
	current_offset = sizeof(header) + size;
	current_offset += (8 - (current_offset % 8)) & 0x7;
	x_run_offset = current_offset;
	current_offset += header.num_x_runs;
	current_offset += (8 - (current_offset % 8)) & 0x7;
	x_data_offset = current_offset;
	current_offset += header.num_x_voxels * header.bytes_per_voxel;
	current_offset += (8 - (current_offset % 8)) & 0x7;
	x_offset_offset = current_offset;
	current_offset += header.num_x_offsets * sizeof(ScanOffset);
	current_offset += (8 - (current_offset % 8)) & 0x7;
	y_run_offset = current_offset;
	current_offset += header.num_y_runs;
	current_offset += (8 - (current_offset % 8)) & 0x7;
	y_data_offset = current_offset;
	current_offset += header.num_y_voxels * header.bytes_per_voxel;
	current_offset += (8 - (current_offset % 8)) & 0x7;
	y_offset_offset = current_offset;
	current_offset += header.num_y_offsets * sizeof(ScanOffset);
	current_offset += (8 - (current_offset % 8)) & 0x7;
	z_run_offset = current_offset;
	current_offset += header.num_z_runs;
	current_offset += (8 - (current_offset % 8)) & 0x7;
	z_data_offset = current_offset;
	current_offset += header.num_z_voxels * header.bytes_per_voxel;
	current_offset += (8 - (current_offset % 8)) & 0x7;
	z_offset_offset = current_offset;
	current_offset += header.num_z_offsets * sizeof(ScanOffset);

	/* memory-map the data */
	if ((data = vpc->mmap_func(fd, current_offset,
				   vpc->client_data)) == NULL)
	    return(VPSetError(vpc, VPERROR_IO));

	/* assign pointers to x view data */
	vpc->rle_x = VPCreateRLEVoxels(vpc, header.ylen, header.zlen,
				       header.xlen, 0, 0, 0);
	vpc->rle_x->run_count = header.num_x_runs;
	if (header.num_x_runs > 0)
	    vpc->rle_x->run_lengths = (unsigned char *)(data + x_run_offset);
	vpc->rle_x->data_count = header.num_x_voxels;
	if (header.num_x_voxels > 0)
	    vpc->rle_x->data = (void *)(data + x_data_offset);
	vpc->rle_x->scan_offsets_per_slice = header.num_x_offsets;
	if (header.num_x_offsets > 0)
	    vpc->rle_x->scan_offsets = (ScanOffset *)(data + x_offset_offset);
	vpc->rle_x->mmapped = 1;

	/* assign pointers to y view data */
	vpc->rle_y = VPCreateRLEVoxels(vpc, header.zlen, header.xlen,
				       header.ylen, 0, 0, 0);
	vpc->rle_y->run_count = header.num_y_runs;
	if (header.num_y_runs > 0)
	    vpc->rle_y->run_lengths = (unsigned char *)(data + y_run_offset);
	vpc->rle_y->data_count = header.num_y_voxels;
	if (header.num_y_voxels > 0)
	    vpc->rle_y->data = (void *)(data + y_data_offset);
	vpc->rle_y->scan_offsets_per_slice = header.num_y_offsets;
	if (header.num_y_offsets > 0)
	    vpc->rle_y->scan_offsets = (ScanOffset *)(data + y_offset_offset);
	vpc->rle_y->mmapped = 1;

	/* assign pointers to z view data */
	vpc->rle_z = VPCreateRLEVoxels(vpc, header.xlen, header.ylen,
				       header.zlen, 0, 0, 0);
	vpc->rle_z->run_count = header.num_z_runs;
	if (header.num_z_runs > 0)
	    vpc->rle_z->run_lengths = (unsigned char *)(data + z_run_offset);
	vpc->rle_z->data_count = header.num_z_voxels;
	if (header.num_z_voxels > 0)
	    vpc->rle_z->data = (void *)(data + z_data_offset);
	vpc->rle_z->scan_offsets_per_slice = header.num_z_offsets;
	if (header.num_z_offsets > 0)
	    vpc->rle_z->scan_offsets = (ScanOffset *)(data + z_offset_offset);
	vpc->rle_z->mmapped = 1;
    } else {
	/* read the x view data into memory */
	if (header.num_x_runs != 0) {
	    vpc->rle_x = VPCreateRLEVoxels(vpc, header.ylen, header.zlen,
		header.xlen, header.num_x_voxels, header.num_x_runs,
		header.bytes_per_voxel);
	    if ((c = LoadRLEVoxels(vpc, fd, vpc->rle_x, header.num_x_offsets,
				   swab)) != VP_OK)
		return(c);
	}

	/* read the y view data into memory */
	if (header.num_y_runs != 0) {
	    vpc->rle_y = VPCreateRLEVoxels(vpc, header.zlen, header.xlen,
		header.ylen, header.num_y_voxels, header.num_y_runs,
		header.bytes_per_voxel);
	    if ((c = LoadRLEVoxels(vpc, fd, vpc->rle_y, header.num_y_offsets,
				   swab)) != VP_OK)
		return(c);
	}

	/* read the z view data into memory */
	if (header.num_z_runs != 0) {
	    vpc->rle_z = VPCreateRLEVoxels(vpc, header.xlen, header.ylen,
		header.zlen, header.num_z_voxels, header.num_z_runs,
		header.bytes_per_voxel);
	    if ((c = LoadRLEVoxels(vpc, fd, vpc->rle_z, header.num_z_offsets,
				   swab)) != VP_OK)
		return(c);
	}
    }
#ifdef DEBUG
    if (vpc->rle_x != NULL) {
	printf("Checking X scanline offsets....\n");
	VPCheckScanOffsets(vpc->rle_x, vpc->rle_bytes_per_voxel);
    }
    if (vpc->rle_y != NULL) {
	printf("Checking Y scanline offsets....\n");
	VPCheckScanOffsets(vpc->rle_y, vpc->rle_bytes_per_voxel);
    }
    if (vpc->rle_z != NULL) {
	printf("Checking Z scanline offsets....\n");
	VPCheckScanOffsets(vpc->rle_z, vpc->rle_bytes_per_voxel);
    }
#endif
    return(VP_OK);
}

/*
 * LoadRLEVoxels
 *
 * Load an RLEVoxels structure from a file.
 */

static int
LoadRLEVoxels(vpc, fd, rle_voxels, offsets, swab)
vpContext *vpc;
int fd;
RLEVoxels *rle_voxels;
int offsets;
int swab;
{
    int size;
    char pad_data[8];
    int pad_bytes;

    if (rle_voxels->run_count > 0) {
	size = rle_voxels->run_count;
	if (vpc->read_func(fd, rle_voxels->run_lengths, size) != size)
	    return(VPSetError(vpc, VPERROR_IO));

	pad_bytes = (8 - (size % 8)) & 0x7;
	if (pad_bytes > 0) {
	    if (vpc->read_func(fd, pad_data, pad_bytes) != pad_bytes)
		return(VPSetError(vpc, VPERROR_IO));
	}
    }
    if (rle_voxels->data_count > 0) {
	size = rle_voxels->data_count * vpc->rle_bytes_per_voxel;
	if (vpc->read_func(fd, rle_voxels->data, size) != size)
	    return(VPSetError(vpc, VPERROR_IO));
	if (swab)
	    SwapVoxels(vpc, rle_voxels->data, rle_voxels->data_count,
		       vpc->num_shade_fields, vpc->rle_bytes_per_voxel);

	pad_bytes = (8 - (size % 8)) & 0x7;
	if (pad_bytes > 0) {
	    if (vpc->read_func(fd, pad_data, pad_bytes) != pad_bytes)
		return(VPSetError(vpc, VPERROR_IO));
	}
    }
    if (offsets > 0) {
	rle_voxels->scan_offsets_per_slice = offsets;
	size = rle_voxels->klen * offsets * sizeof(ScanOffset);
	Alloc(vpc, rle_voxels->scan_offsets, ScanOffset *, size,
	      "scan_offsets");
	if (vpc->read_func(fd, rle_voxels->scan_offsets, size) != size)
	    return(VPSetError(vpc, VPERROR_IO));
	if (swab)
	    SwapWords(rle_voxels->scan_offsets, size);

	pad_bytes = (8 - (size % 8)) & 0x7;
	if (pad_bytes > 0) {
	    if (vpc->read_func(fd, pad_data, pad_bytes) != pad_bytes)
		return(VPSetError(vpc, VPERROR_IO));
	}
    }
    return(VP_OK);
}

/*
 * SwapWords
 *
 * Byte-swap word data to change the endianess.
 */

static void
SwapWords(data, size)
void *data;
unsigned size;
{
    unsigned char *ptr;
    int tmp1, tmp2;

    ptr = data;
    while (size >= 4) {
	tmp1 = ptr[0]; ptr[0] = ptr[3]; ptr[3] = tmp1;
	tmp2 = ptr[1]; ptr[1] = ptr[2]; ptr[2] = tmp2;
	size -= 4;
	ptr += 4;
    }
}

/*
 * SwapVoxels
 *
 * Byte-swap voxel data to change the endianess.
 */

static void
SwapVoxels(vpc, voxels, num_voxels, fields, bytes_per_voxel)
vpContext *vpc;		/* context */
void *voxels;		/* array of voxels */
int num_voxels;		/* number of voxels in the array */
int fields;		/* number of fields in voxel */
int bytes_per_voxel;	/* size of voxel in bytes */
{
    int f, size, offset;
    unsigned char *voxel_ptr;
    int tmp1, tmp2;

    /* check if any of the fields of the voxel need swapping */
    size = 0;
    for (f = 0; f < fields; f++) {
	if (vpc->field_size[f] > size)
	    size = vpc->field_size[f];
    }
    if (size <= 1)
	return;

    /* do the swapping */
    voxel_ptr = voxels;
    while (num_voxels-- > 0) {
	for (f = 0; f < fields; f++) {
	    size = vpc->field_size[f];
	    if (size == 1)
		continue;
	    offset = vpc->field_offset[f];
	    if (size == 2) {
		tmp1 = voxel_ptr[offset];
		voxel_ptr[offset] = voxel_ptr[offset+1];
		voxel_ptr[offset+1] = tmp1;
	    } else {
		tmp1 = voxel_ptr[offset];
		voxel_ptr[offset] = voxel_ptr[offset+3];
		voxel_ptr[offset+3] = tmp1;
		tmp2 = voxel_ptr[offset+1];
		voxel_ptr[offset+1] = voxel_ptr[offset+2];
		voxel_ptr[offset+2] = tmp2;
	    }
	}
	voxel_ptr += bytes_per_voxel;
    }
}

/*******************************************************************
 * Min-Max Octree Files.                                           *
 *******************************************************************/

/* file header structure */
typedef struct {
    unsigned magic;		/* magic number for identification */
    unsigned xlen;		/* voxels in each dimension */
    unsigned ylen;
    unsigned zlen;
    int num_clsfy_params;	/* # of params for classification */
    int levels;			/* number of levels in octree */
    int root_node_size;		/* voxels/side for root level */
    int base_node_size;		/* voxels/side for base level */
    int range_bytes_per_node;	/* bytes/node for min/max data */
    int base_bytes_per_node;	/* bytes/node for base level */
    int nonbase_bytes_per_node; /* bytes/node for non-base level */
    int status_offset;		/* offset to status field */
    int child_offset;		/* offset to child field */
    unsigned octree_bytes;	/* bytes of storage for the octree */
} MinMaxOctreeHdr;

/*
 * File layout:
 *   MinMaxOctreeHdr hdr;
 *   unsigned param_size[hdr.num_clsfy_params];	(size of each parameter, bytes)
 *   unsigned param_max[hdr.num_clsfy_params];  (max. value of each parameter)
 *   unsigned node_offset[hdr.num_clsfy_params];(node offset to min/max data)
 *   char data[octree_bytes];	(octree data)
 */

/*
 * vpStoreMinMaxOctree
 *
 * Store a min-max octree to a file.
 */

vpResult
vpStoreMinMaxOctree(vpc, fd)
vpContext *vpc;	/* context containing the octree */
int fd;		/* UNIX file descriptor open for writing */
{
    MinMaxOctreeHdr header;
    unsigned field_data[3*VP_MAX_FIELDS];
    int ncp, c;
    unsigned size;

    if (vpc->mm_octree == NULL)
	return(VPSetError(vpc, VPERROR_BAD_SIZE));

    /* write header */
    bzero(&header, sizeof(MinMaxOctreeHdr));
    header.magic = VP_OCTFILE_MAGIC;
    header.xlen = vpc->xlen;
    header.ylen = vpc->ylen;
    header.zlen = vpc->zlen;
    header.num_clsfy_params = vpc->num_clsfy_params;
    header.levels = vpc->mm_octree->levels;
    header.root_node_size = vpc->mm_octree->root_node_size;
    header.base_node_size = vpc->mm_octree->base_node_size;
    header.range_bytes_per_node = vpc->mm_octree->range_bytes_per_node;
    header.base_bytes_per_node = vpc->mm_octree->base_bytes_per_node;
    header.nonbase_bytes_per_node = vpc->mm_octree->nonbase_bytes_per_node;
    header.status_offset = vpc->mm_octree->status_offset;
    header.child_offset = vpc->mm_octree->child_offset;
    header.octree_bytes = vpc->mm_octree->octree_bytes;
    if (vpc->write_func(fd, &header, sizeof(header)) != sizeof(header))
	return(VPSetError(vpc, VPERROR_IO));

    /* write parameter size/offset information */
    ncp = vpc->num_clsfy_params;
    for (c = 0; c < ncp; c++) {
	field_data[c] = vpc->field_size[vpc->param_field[c]];
	field_data[ncp + c] = vpc->field_max[vpc->param_field[c]];
	field_data[2*ncp + c] = vpc->mm_octree->node_offset[c];
    }
    size = 3*ncp*sizeof(unsigned);
    if (vpc->write_func(fd, field_data, size) != size)
	return(VPSetError(vpc, VPERROR_IO));

    /* write octree data */
    size = vpc->mm_octree->octree_bytes;
    if (vpc->write_func(fd, vpc->mm_octree->root, size) != size)
	return(VPSetError(vpc, VPERROR_IO));

    return(VP_OK);
}

/*
 * vpLoadMinMaxOctree
 *
 * Load a min-max octree from a file.
 */

vpResult
vpLoadMinMaxOctree(vpc, fd)
vpContext *vpc;	/* context to store the octree into */
int fd;		/* UNIX file descriptor open for reading */
{
    MinMaxOctreeHdr header;
    unsigned field_data[3*VP_MAX_FIELDS];
    int ncp, c, swab;
    unsigned size;

    /* read header */
    if (vpc->read_func(fd, &header, sizeof(header)) != sizeof(header))
	return(VPSetError(vpc, VPERROR_IO));
    swab = 0;
    if (header.magic != VP_OCTFILE_MAGIC) {
	SwapWords(&header, sizeof(header));
	if (header.magic != VP_OCTFILE_MAGIC)
	    return(VPSetError(vpc, VPERROR_BAD_FILE));
	swab = 1;
    }

    /* read parameter size/offset information */
    size = 3 * header.num_clsfy_params * sizeof(unsigned);
    if (vpc->read_func(fd, field_data, size) != size)
	return(VPSetError(vpc, VPERROR_IO));
    if (swab)
	SwapWords(field_data, size);

    /* check for consistency with current volume data */
    if ((c = VPCheckRawVolume(vpc)) != VP_OK)
	return(c);
    if (header.xlen != vpc->xlen || header.ylen != vpc->ylen ||
	header.zlen != vpc->zlen ||
	header.num_clsfy_params != vpc->num_clsfy_params)
	return(VPSetError(vpc, VPERROR_BAD_VOLUME));
    ncp = vpc->num_clsfy_params;
    for (c = 0; c < ncp; c++) {
	if (field_data[c] != vpc->field_size[vpc->param_field[c]] ||
	    field_data[ncp + c] != vpc->field_max[vpc->param_field[c]])
	    return(VPSetError(vpc, VPERROR_BAD_VOXEL));
    }

    /* clear old octree */
    vpDestroyMinMaxOctree(vpc);

    /* initialize new octree */
    Alloc(vpc, vpc->mm_octree, MinMaxOctree *, sizeof(MinMaxOctree),
	  "MinMaxOctree");
    bzero(vpc->mm_octree, sizeof(MinMaxOctree));
    vpc->mm_octree->levels = header.levels;
    vpc->mm_octree->root_node_size = header.root_node_size;
    vpc->mm_octree->base_node_size = header.base_node_size;
    vpc->mm_octree->range_bytes_per_node = header.range_bytes_per_node;
    vpc->mm_octree->base_bytes_per_node = header.base_bytes_per_node;
    vpc->mm_octree->nonbase_bytes_per_node = header.nonbase_bytes_per_node;
    vpc->mm_octree->status_offset = header.status_offset;
    vpc->mm_octree->child_offset = header.child_offset;
    vpc->mm_octree->octree_bytes = header.octree_bytes;
    ncp = header.num_clsfy_params;
    for (c = 0; c < ncp; c++)
	vpc->mm_octree->node_offset[c] = field_data[2*ncp + c];

    /* load octree data */
    size = header.octree_bytes;
    Alloc(vpc, vpc->mm_octree->root, void *, size, "mm_octree");
    if (vpc->read_func(fd, vpc->mm_octree->root, size) != size)
	return(VPSetError(vpc, VPERROR_IO));
    if (swab)
	SwapOctreeNode(vpc, 0, vpc->mm_octree->root);

    return(VP_OK);
}

/*
 * SwapOctreeNode
 *
 * Recursive depth-first traversal of an octree to byte-swap each node's
 * data (in order to switch the endianess).
 */

static void
SwapOctreeNode(vpc, level, node)
vpContext *vpc;
int level;
void *node;
{
    int p, field, size, offset, tmp1, tmp2;
    int child_bytes_per_node;
    char *node_ptr = node;

    /* byte swap min-max data */
    for (p = 0; p < vpc->num_clsfy_params; p++) {
	field = vpc->param_field[p];
	size = vpc->field_size[field];
	if (size != 1) {
	    ASSERT(size == 2);
	    offset = vpc->mm_octree->node_offset[p];
	    tmp1 = node_ptr[offset];
	    node_ptr[offset] = node_ptr[offset+1];
	    node_ptr[offset+1] = tmp1;
	    tmp2 = node_ptr[offset+2];
	    node_ptr[offset+2] = node_ptr[offset+3];
	    node_ptr[offset+3] = tmp2;
	}
    }

    /* byte swap child pointer and recurse */
    if (level != vpc->mm_octree->levels-1) {
	offset = vpc->mm_octree->child_offset;
	tmp1 = node_ptr[offset];
	node_ptr[offset] = node_ptr[offset+3];
	node_ptr[offset+3] = tmp1;
	tmp2 = node_ptr[offset+1];
	node_ptr[offset+1] = node_ptr[offset+2];
	node_ptr[offset+2] = tmp2;

	ASSERT(IntField(node, offset) != 0);
	node_ptr = (char *)vpc->mm_octree->root + IntField(node, offset);
	if (level == vpc->mm_octree->levels-2)
	    child_bytes_per_node = vpc->mm_octree->base_bytes_per_node;
	else
	    child_bytes_per_node = vpc->mm_octree->nonbase_bytes_per_node;
	SwapOctreeNode(vpc, level+1, node_ptr);
	node_ptr += child_bytes_per_node;
	SwapOctreeNode(vpc, level+1, node_ptr);
	node_ptr += child_bytes_per_node;
	SwapOctreeNode(vpc, level+1, node_ptr);
	node_ptr += child_bytes_per_node;
	SwapOctreeNode(vpc, level+1, node_ptr);
	node_ptr += child_bytes_per_node;
	SwapOctreeNode(vpc, level+1, node_ptr);
	node_ptr += child_bytes_per_node;
	SwapOctreeNode(vpc, level+1, node_ptr);
	node_ptr += child_bytes_per_node;
	SwapOctreeNode(vpc, level+1, node_ptr);
	node_ptr += child_bytes_per_node;
	SwapOctreeNode(vpc, level+1, node_ptr);
    }
}

/*******************************************************************
 * Raw Volume Files.                                               *
 *******************************************************************/

/* file header structure */
typedef struct {
    unsigned magic;		/* magic number for identification */
    unsigned xlen;		/* voxels in each dimension */
    unsigned ylen;
    unsigned zlen;
    unsigned bytes_per_voxel;	/* size of a raw voxel */
    unsigned num_voxel_fields;	/* number of fields in a voxel */
    unsigned num_shade_fields;	/* number of fields for shading */
    unsigned num_clsfy_fields;	/* number of fields for classification */
    int xstride;		/* strides for voxel data */
    int ystride;
    int zstride;
} RawVoxelHdr;

/*
 * File layout:
 *   RawVoxelHdr hdr;
 *   unsigned field_size[hdr.num_shade_fields];	   (size of each voxel field)
 *   unsigned field_offset[hdr.num_shade_fields];  (offset for each field)
 *   unsigned field_max[hdr.num_shade_fields];     (max. value of each field)
 *   char data[hdr.xlen*hdr.ylen*hdr.zlen*hdr.bytes_per_voxel]; (volume data)
 */

/*
 * vpStoreRawVolume
 *
 * Store an unclassified volume to a file.
 */

vpResult
vpStoreRawVolume(vpc, fd)
vpContext *vpc;	/* context containing the volume */
int fd;		/* UNIX file descriptor open for writing */
{
    RawVoxelHdr header;
    unsigned field_data[3*VP_MAX_FIELDS];
    int nvf, c;
    unsigned size;
    int retcode;

    /* check for errors */
    if ((retcode = VPCheckRawVolume(vpc)) != VP_OK)
	return(retcode);

    /* write header */
    header.magic = VP_RVFILE_MAGIC;
    header.xlen = vpc->xlen;
    header.ylen = vpc->ylen;
    header.zlen = vpc->zlen;
    header.bytes_per_voxel = vpc->raw_bytes_per_voxel;
    header.num_voxel_fields = vpc->num_voxel_fields;
    header.num_shade_fields = vpc->num_shade_fields;
    header.num_clsfy_fields = vpc->num_clsfy_params;
    header.xstride = vpc->xstride;
    header.ystride = vpc->ystride;
    header.zstride = vpc->zstride;
    if (vpc->write_func(fd, &header, sizeof(header)) != sizeof(header))
	return(VPSetError(vpc, VPERROR_IO));

    /* write voxel layout information */
    nvf = vpc->num_voxel_fields;
    for (c = 0; c < nvf; c++) {
	field_data[c] = vpc->field_size[c];
	field_data[nvf + c] = vpc->field_offset[c];
	field_data[2*nvf + c] = vpc->field_max[c];
    }
    size = 3*nvf*sizeof(unsigned);
    if (vpc->write_func(fd, field_data, size) != size)
	return(VPSetError(vpc, VPERROR_IO));

    /* write data */
    if (vpc->write_func(fd, vpc->raw_voxels, vpc->raw_voxels_size) !=
	vpc->raw_voxels_size)
	return(VPSetError(vpc, VPERROR_IO));

    return(VP_OK);
}

/*
 * vpLoadRawVolume
 *
 * Load an unclassified volume from a file.
 */

vpResult
vpLoadRawVolume(vpc, fd)
vpContext *vpc;	/* context to store the volume into */
int fd;		/* UNIX file descriptor open for reading */
{
    RawVoxelHdr header;
    unsigned field_data[3*VP_MAX_FIELDS];
    int nvf, c, swab;
    unsigned size;
    unsigned voxel_offset;
    unsigned char *data;
    int destroy_old_volume;

    /* read header */
    if (vpc->read_func(fd, &header, sizeof(header)) != sizeof(header))
	return(VPSetError(vpc, VPERROR_IO));
    swab = 0;
    if (header.magic != VP_RVFILE_MAGIC) {
	SwapWords(&header, sizeof(header));
	if (header.magic != VP_RVFILE_MAGIC)
	    return(VPSetError(vpc, VPERROR_BAD_FILE));
	swab = 1;
    }

    /* read voxel layout information */
    size = 3 * header.num_voxel_fields * sizeof(unsigned);
    if (vpc->read_func(fd, field_data, size) != size)
	return(VPSetError(vpc, VPERROR_IO));
    if (swab)
	SwapWords(field_data, size);
    voxel_offset = sizeof(header) + size;

    /* destroy old volume data */
    vpDestroyClassifiedVolume(vpc);
    vpDestroyMinMaxOctree(vpc);

    /* load new volume size */
    vpc->xlen = header.xlen;
    vpc->ylen = header.ylen;
    vpc->zlen = header.zlen;
    vpc->raw_bytes_per_voxel = header.bytes_per_voxel;
    vpc->num_voxel_fields = header.num_voxel_fields;
    vpc->num_shade_fields = header.num_shade_fields;
    vpc->num_clsfy_params = header.num_clsfy_fields;
    vpc->xstride = header.xstride;
    vpc->ystride = header.ystride;
    vpc->zstride = header.zstride;
    nvf = header.num_voxel_fields;
    for (c = 0; c < nvf; c++) {
	vpc->field_size[c] = field_data[c];
	vpc->field_offset[c] = field_data[nvf + c];
	vpc->field_max[c] = field_data[2*nvf + c];
    }

    /* load new volume data */
    size = vpc->xlen*vpc->ylen*vpc->zlen*vpc->raw_bytes_per_voxel;
    vpc->raw_voxels_size = size;
    if (vpc->mmap_func != NULL && !swab) {
	if ((vpc->raw_voxels = vpc->mmap_func(fd, voxel_offset,
					      vpc->client_data)) == NULL)
	    return(VPSetError(vpc, VPERROR_IO));
    } else {
	Alloc(vpc, vpc->raw_voxels, void *, size, "raw_voxels");
	if (vpc->read_func(fd, vpc->raw_voxels, size) != size)
	    return(VPSetError(vpc, VPERROR_IO));
	if (swab) {
	    SwapVoxels(vpc, vpc->raw_voxels, vpc->xlen*vpc->ylen*vpc->zlen,
		       vpc->num_voxel_fields, vpc->raw_bytes_per_voxel);
	}
    }

    return(VP_OK);
}

/*******************************************************************
 * Rendering Context Dump Files.                                   *
 *******************************************************************/

/* file header structure */
typedef struct {
    unsigned magic;		/* magic number for identification */
    unsigned major_version;	/* major version number */
    unsigned minor_version;	/* minor version number */
    unsigned max_fields;	/* value of VP_MAX_FIELDS */
    unsigned max_material;	/* value of VP_MAX_MATERIAL */
    unsigned max_lights;	/* value of VP_MAX_LIGHTS */
} VpcHdr;

/*
 * File layout:
 *   VpcHdr hdr;
 *   vpContext vpc; --> truncated just before "end_of_parameters" field
 *   unsigned shade_color_table_size;
 *   float shade_color_table[shade_color_table_size];
 *   unsigned shade_weight_table_size;
 *   float shade_weight_table[shade_weight_table_size];
 *   for i = 1 to vpc.num_clsfy_params:
 *       int clsfy_table_size;
 *       float clsfy_table[clsfy_table_size];
 */

/*
 * vpStoreContext
 *
 * Store the contents of a volpack context to a file.  All state parameters
 * stored directly in the vpContext structure are stored.  User-supplied
 * lookup tables are also stored.  Volume data and octrees are not stored
 * (use the routines specifically for storing those data structures), and
 * internal tables that can be computed from other state variables
 * (e.g. depth cueing lookup table) are not stored.
 */

vpResult
vpStoreContext(vpc, fd)
vpContext *vpc;
int fd;
{
    VpcHdr header;
    int i;
    unsigned vpc_size;

    header.magic = VP_VPCFILE_MAGIC;
    header.major_version = VP_MAJOR_VERSION;
    header.minor_version = VP_MINOR_VERSION;
    header.max_fields = VP_MAX_FIELDS;
    header.max_material = VP_MAX_MATERIAL;
    header.max_lights = VP_MAX_LIGHTS;
    vpc_size = vpFieldOffset(vpc, end_of_parameters);
    if (vpc->write_func(fd, &header, sizeof(header)) != sizeof(header))
	return(VPSetError(vpc, VPERROR_IO));
    if (vpc->write_func(fd, vpc, vpc_size) != vpc_size)
	return(VPSetError(vpc, VPERROR_IO));
    if (!StoreTable(vpc, fd, vpc->shade_color_table,
		    vpc->shade_color_table_size))
	return(VPSetError(vpc, VPERROR_IO));
    if (!StoreTable(vpc, fd, vpc->shade_weight_table,
		    vpc->shade_weight_table_size))
	return(VPSetError(vpc, VPERROR_IO));
    for (i = 0; i < vpc->num_clsfy_params; i++) {
	if (!StoreTable(vpc, fd, vpc->clsfy_table[i],
			vpc->clsfy_table_size[i]))
	    return(VPSetError(vpc, VPERROR_IO));
    }
    return(VP_OK);
}

/*
 * StoreTable
 *
 * Store a table to a file and check for errors.  Return value is 1 for
 * success, 0 for failure.
 */

static int
StoreTable(vpc, fd, ptr, size)
vpContext *vpc;
int fd;
float *ptr;
unsigned size;
{
    if (size == 0 || ptr == NULL) {
	size = 0;
	if (vpc->write_func(fd, &size, sizeof(size)) != sizeof(size))
	    return(0);
    } else {
	if (vpc->write_func(fd, &size, sizeof(size)) != sizeof(size))
	    return(0);
	if (vpc->write_func(fd, ptr, size) != size)
	    return(0);
    }
    return(1);
}

/*
 * vpLoadContext
 *
 * Load a volpack context from a file.  The old contents of the context are
 * destroyed, including any volume data.  Lookup tables for shading and
 * classification that are loaded from the file are stored in newly-allocated
 * memory, but the application is responsible for freeing the tables;
 * existing tables in the context are not overwritten (since there is no
 * way for the application to predict the right table sizes), and the new
 * tables are not freed when vpDestroyContext is called (since volpack
 * normally does not manage the tables).  Byte swapping is not performed.
 */

vpResult
vpLoadContext(vpc, fd)
vpContext *vpc;
int fd;
{
    VpcHdr header;
    int swab, i;
    unsigned vpc_size;

    /* read header */
    if (vpc->read_func(fd, &header, sizeof(header)) != sizeof(header))
	return(VPSetError(vpc, VPERROR_IO));
    swab = 0;
    if (header.magic != VP_VPCFILE_MAGIC)
	return(VPSetError(vpc, VPERROR_BAD_FILE));
    if (header.major_version != VP_MAJOR_VERSION || 
	header.minor_version != VP_MINOR_VERSION ||
	header.max_fields != VP_MAX_FIELDS ||
	header.max_material != VP_MAX_MATERIAL ||
	header.max_lights != VP_MAX_LIGHTS) {
	return(VPSetError(vpc, VPERROR_BAD_VALUE));
    }

    /* destroy old data structures */
    vpDestroyMinMaxOctree(vpc);
    vpDestroyClassifiedVolume(vpc);

    /* load new context */
    vpc_size = vpFieldOffset(vpc, end_of_parameters);
    if (vpc->read_func(fd, vpc, vpc_size) != vpc_size)
	return(VPSetError(vpc, VPERROR_IO));
    vpc->raw_voxels = NULL;
    for (i = 0; i < VP_MAX_FIELDS; i++)
	vpc->clsfy_table[i] = NULL;
    vpc->shade_color_table = NULL;
    vpc->shade_weight_table = NULL;
    vpc->image = NULL;
    if (vpc->shade_func == NULL)
	vpc->shading_mode = LOOKUP_SHADER;
    if (!LoadTable(vpc, fd, &vpc->shade_color_table,
		   (unsigned *)&vpc->shade_color_table_size))
	goto failed;
    if (!LoadTable(vpc, fd, &vpc->shade_weight_table,
		   (unsigned *)&vpc->shade_weight_table_size))
	goto failed;
    for (i = 0; i < vpc->num_clsfy_params; i++) {
	if (!LoadTable(vpc, fd, &vpc->clsfy_table[i],
		       (unsigned *)&vpc->clsfy_table_size[i]))
	    goto failed;
    }
    return(VP_OK);

 failed:
    if (vpc->shade_color_table != NULL) {
	Dealloc(vpc, vpc->shade_color_table);
	vpc->shade_color_table = NULL;
    }
    if (vpc->shade_weight_table != NULL) {
	Dealloc(vpc, vpc->shade_weight_table);
	vpc->shade_weight_table = NULL;
    }
    for (i = 0; i < vpc->num_clsfy_params; i++) {
	if (vpc->clsfy_table[i] != NULL) {
	    Dealloc(vpc, vpc->clsfy_table[i]);
	    vpc->clsfy_table[i] = NULL;
	}
    }
    return(VPSetError(vpc, VPERROR_IO));
}

/*
 * LoadTable
 *
 * Load a table from a file and check for errors.  Return value is 1 for
 * success, 0 for failure.
 */

static int
LoadTable(vpc, fd, ptr_ptr, size_ptr)
vpContext *vpc;
int fd;
float **ptr_ptr;
unsigned *size_ptr;
{
    if (vpc->read_func(fd, size_ptr, sizeof(unsigned)) != sizeof(unsigned))
	return(0);
    if (*size_ptr != 0) {
	Alloc(vpc, *ptr_ptr, void *, *size_ptr, "lookup table");
	if (vpc->read_func(fd, *ptr_ptr, *size_ptr) != *size_ptr)
	    return(0);
    }
    return(1);
}
