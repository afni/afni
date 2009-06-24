/*
 * vp_octree.c
 *
 * Routines to create and destroy MinMaxOctree objects for fast classification.
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
 * OctantOrder: octant traversal order, depending on best_view_axis
 */

static int OctantOrder[3][8] = {
    { 0, 2, 4, 6, 1, 3, 5, 7 }, /* VP_X_AXIS */
    { 0, 4, 1, 5, 2, 6, 3, 7 }, /* VP_Y_AXIS */
    { 0, 1, 2, 3, 4, 5, 6, 7 }  /* VP_Z_AXIS */
};

static void CreatePyramid ANSI_ARGS((vpContext *vpc,
    void *mm_pyramid[VP_MAX_OCTREE_LEVELS]));
static void DescendPyramid ANSI_ARGS((vpContext *vpc,
    void *mm_pyramid[VP_MAX_OCTREE_LEVELS], int level,
    int x, int y, int z, int nodes_per_side, void *parent_node,
    int *octree_offset));
static void Compute1DSummedAreaTable ANSI_ARGS((vpContext *vpc));
static void Compute2DSummedAreaTable ANSI_ARGS((vpContext *vpc));
static void ClassifyOctree1 ANSI_ARGS((vpContext *vpc));
static void ClassifyOctree2 ANSI_ARGS((vpContext *vpc));
static void ComputeOctreeMask ANSI_ARGS((vpContext *vpc, int level,
    int xn, int yn, int zn, int node_size, void *parent_node,
    unsigned char *array, int max_level));

/*
 * vpCreateMinMaxOctree
 *
 * Create a MinMaxOctree representation of the volume for fast classification.
 */

vpResult
vpCreateMinMaxOctree(vpc, root_node_size, base_node_size)
vpContext *vpc;
int root_node_size;	/* ignored for now */
int base_node_size;	/* controls level of detail of smallest nodes */
{
    int max_dim, retcode, p, f;
    int field_size;
    int bytes_per_node;
    int level_size, levels;
    void *mm_pyramid[VP_MAX_OCTREE_LEVELS];
    int octree_offset;

    /* check for errors */
    if ((retcode = VPCheckRawVolume(vpc)) != VP_OK)
	return(retcode);
    if (vpc->num_clsfy_params <= 0 ||
	vpc->num_clsfy_params > vpc->num_voxel_fields)
	return(VPSetError(vpc, VPERROR_BAD_VOXEL));
    for (p = 0; p < vpc->num_clsfy_params; p++) {
	f = vpc->param_field[p];
	if (f < 0 || f >= vpc->num_voxel_fields)
	    return(VPSetError(vpc, VPERROR_BAD_CLASSIFIER));
	if (p > 0 && f <= vpc->param_field[p-1])
	    return(VPSetError(vpc, VPERROR_BAD_CLASSIFIER));
    }

    max_dim = vpc->xlen;
    if (vpc->ylen > max_dim)
	max_dim = vpc->ylen;
    if (vpc->zlen > max_dim)
	max_dim = vpc->zlen;
    switch (base_node_size) {	/* must be a power of 2 */
    case 1:
    case 2:
    case 4:
    case 8:
    case 16:
    case 32:
    case 64:
    case 128:
    case 256:
    case 512:
	break;
    default:
	return(VPSetError(vpc, VPERROR_BAD_VALUE));
    }
    for (p = 0; p < vpc->num_clsfy_params; p++) {
	if (vpc->field_size[vpc->param_field[p]] == 4)
	    return(VPSetError(vpc, VPERROR_BAD_CLASSIFIER));
    }

    /* allocate mm_octree structure */
    Alloc(vpc, vpc->mm_octree, MinMaxOctree *, sizeof(MinMaxOctree),
	  "MinMaxOctree");
    bzero(vpc->mm_octree, sizeof(MinMaxOctree));
    vpc->mm_octree->base_node_size = base_node_size;

    /* compute field sizes */
    bytes_per_node = 0;
    for (p = 0; p < vpc->num_clsfy_params; p++) {
	vpc->mm_octree->node_offset[p] = bytes_per_node;
	bytes_per_node += 2 * vpc->field_size[vpc->param_field[p]];
    }
    vpc->mm_octree->range_bytes_per_node = bytes_per_node;
    vpc->mm_octree->status_offset = bytes_per_node;
    bytes_per_node++;				/* add byte for status field */
    bytes_per_node = (bytes_per_node + 1) & ~1;	/* align to short boundary */
    vpc->mm_octree->base_bytes_per_node = bytes_per_node;
    bytes_per_node = (bytes_per_node + 3) & ~3;	/* align to word boundary */
    vpc->mm_octree->child_offset = bytes_per_node;
    bytes_per_node += sizeof(unsigned);		/* add word for child field */
    vpc->mm_octree->nonbase_bytes_per_node = bytes_per_node;

    /* compute number of levels */
    levels = 1;
    level_size = base_node_size;
    while (level_size < max_dim) {
	level_size *= 2;
	levels++;
    }
    if (levels >= VP_MAX_OCTREE_LEVELS) {
	vpDestroyMinMaxOctree(vpc);
	return(VPSetError(vpc, VPERROR_LIMIT_EXCEEDED));
    }
    vpc->mm_octree->levels = levels;
    vpc->mm_octree->root_node_size = level_size;

    /* build a min-max pyramid representation of the volume */
    CreatePyramid(vpc, mm_pyramid);

    /* determine how much space is needed for the octree nodes */
    octree_offset = vpc->mm_octree->nonbase_bytes_per_node; /* root node */
    DescendPyramid(vpc, mm_pyramid, 0, 0, 0, 0, 1, NULL, &octree_offset);

    /* create the min-max octree nodes */
    Alloc(vpc, vpc->mm_octree->root, void *, octree_offset, "mm_octree");
    vpc->mm_octree->octree_bytes = octree_offset;
    octree_offset = vpc->mm_octree->nonbase_bytes_per_node;
    Debug((vpc, VPDEBUG_OCTREE, "Octree:\n"));
    DescendPyramid(vpc, mm_pyramid, 0, 0, 0, 0, 1, vpc->mm_octree->root,
		   &octree_offset);

    /* clean up and return */
    Dealloc(vpc, mm_pyramid[0]);
    return(VP_OK);
}

/*
 * vpDestroyMinMaxOctree
 *
 * Destroy the MinMaxOctree representation of the volume.
 */

vpResult
vpDestroyMinMaxOctree(vpc)
vpContext *vpc;
{
    if (vpc->mm_octree != NULL) {
	if (vpc->mm_octree->root != NULL) {
	    Dealloc(vpc, vpc->mm_octree->root);
	    vpc->mm_octree->root = NULL;
	}
	Dealloc(vpc, vpc->mm_octree);
	vpc->mm_octree = NULL;
    }
    return(VP_OK);
}

/*
 * CreatePyramid
 *
 * Create a min-max pyramid representation of the volume.
 */

static void
CreatePyramid(vpc, mm_pyramid)
vpContext *vpc;
void *mm_pyramid[VP_MAX_OCTREE_LEVELS];
{
    int pyr_size;		/* size of pyramid in bytes */
    int level, pyr_levels;	/* current, total pyramid levels */
    int level_offset;		/* byte offset to beginning of level */
    int nodes_per_side;		/* nodes per side at current level */
    int node_size;		/* voxels per side in node */
    char *pyr_node;		/* current node of pyramid */
    char *pyr_src;		/* pyramid node being read */
    char *pyr_dst;		/* pyramid node being written */
    char *voxel;		/* voxel being read */
    int x, y, z;		/* coordinates of current pyramid node */
    int nx, ny, nz;		/* coordinates of voxel within node */
    int xlen, ylen, zlen;	/* size of volume */
    int voxel_xstride;		/* volume strides */
    int voxel_ystride;
    int voxel_zstride;
    int param_size[VP_MAX_FIELDS]; /* size of each parameter */
    int param_offset[VP_MAX_FIELDS];/* voxel offset of each parameter */
    int node_offset[VP_MAX_FIELDS]; /* offset of parameter in octree node */
    int max_value[VP_MAX_FIELDS]; /* max. value of each parameter in node */
    int min_value[VP_MAX_FIELDS]; /* min. value of each parameter in node */
    int num_params;		/* number of params for classifier */
    int p;			/* parameter number */
    int value;			/* parameter value */
    int pyr_bytes_per_node;	/* size of node in bytes */
    int pyr_offsets[8];		/* offsets from pyr_src to each of its
				   neighbors (0,+X,+Y,+XY,+Z,+XZ,+YZ,+XYZ) */
    int elem;			/* index into pyr_offsets */

    /* allocate space for pyramid */
    ASSERT(vpc->mm_octree != NULL);
    ASSERT(vpc->mm_octree->levels > 0);
    ASSERT(vpc->xlen > 0);
    ASSERT(vpc->raw_voxels != NULL);
    ASSERT(vpc->num_clsfy_params > 0);
    pyr_levels = vpc->mm_octree->levels;
    pyr_size = vpc->mm_octree->base_bytes_per_node;
    pyr_bytes_per_node = vpc->mm_octree->range_bytes_per_node;
    for (level = pyr_levels; level > 0; level--)
	pyr_size = pyr_size*8 + pyr_bytes_per_node;
    Alloc(vpc, mm_pyramid[0], void *, pyr_size, "mm_pyramid");
    level_offset = pyr_bytes_per_node;
    nodes_per_side = 1;
    for (level = 1; level < vpc->mm_octree->levels; level++) {
	mm_pyramid[level] = (char *)mm_pyramid[level-1] + level_offset;
	level_offset *= 8;
	nodes_per_side *= 2;
    }

    /* build the base level of the pyramid */
    xlen = vpc->xlen;
    ylen = vpc->ylen;
    zlen = vpc->zlen;
    voxel_xstride = vpc->xstride;
    voxel_ystride = vpc->ystride;
    voxel_zstride = vpc->zstride;
    voxel = vpc->raw_voxels;
    num_params = vpc->num_clsfy_params;
    for (p = 0; p < num_params; p++) {
	param_size[p] = vpc->field_size[vpc->param_field[p]];
	param_offset[p] = vpc->field_offset[vpc->param_field[p]];
	node_offset[p] = vpc->mm_octree->node_offset[p];
    }
    node_size = vpc->mm_octree->base_node_size;
    pyr_dst = mm_pyramid[pyr_levels-1];
    Debug((vpc, VPDEBUG_PYRAMID, "Pyramid Level %d:\n", pyr_levels-1));
    for (z = 0; z < nodes_per_side; z++) {
	ReportStatus(vpc, (double)z / (double)nodes_per_side);

	for (y = 0; y < nodes_per_side; y++) {
	    for (x = 0; x < nodes_per_side; x++) {
		/* clear the min/max values for the current node */
		for (p = 0; p < num_params; p++) {
		    max_value[p] = -1;
		    min_value[p] = 65536;
		}

		/* loop over voxels in the node */
		if (z * node_size >= zlen || y * node_size >= ylen ||
		    x * node_size >= xlen) {
		    for (p = 0; p < num_params; p++) {
			max_value[p] = 0;
			min_value[p] = 0;
		    }
		    voxel += node_size * voxel_zstride;
		} else for (nz = 0; nz < node_size; nz++) {
		    if (z * node_size + nz >= zlen) {
			voxel += (node_size - nz) * voxel_zstride;
			break;
		    }
		    for (ny = 0; ny < node_size; ny++) {
			if (y * node_size + ny >= ylen) {
			    voxel += (node_size - ny) * voxel_ystride;
			    break;
			}
			for (nx = 0; nx < node_size; nx++) {
			    if (x * node_size + nx >= xlen) {
				voxel += (node_size - nx) * voxel_xstride;
				break;
			    }

			    /* compare each field against current min/max */
			    for (p = 0; p < num_params; p++) {
				ASSERT(voxel == (char *)vpc->raw_voxels +
				       (x*node_size+nx)*voxel_xstride +
				       (y*node_size+ny)*voxel_ystride +
				       (z*node_size+nz)*voxel_zstride);
				value = VoxelField(voxel, param_offset[p],
						   param_size[p]);
				if (value > max_value[p])
				    max_value[p] = value;
				if (value < min_value[p])
				    min_value[p] = value;
			    }
			    voxel += voxel_xstride;
			} /* for nx */
			voxel += voxel_ystride - node_size*voxel_xstride;
		    } /* for ny */
		    voxel += voxel_zstride - node_size*voxel_ystride;
		} /* for nz */

		/* store the min/max values for this node */
		Debug((vpc, VPDEBUG_PYRAMID, "  Node %d,%d,%d:\n", x, y, z));
		for (p = 0; p < num_params; p++) {
		    ASSERT(max_value[p] >= 0 && max_value[p] < 65536);
		    ASSERT(min_value[p] >= 0 && min_value[p] < 65536);
		    Debug((vpc, VPDEBUG_PYRAMID,
			   "    Param %d: min %d, max %d\n",
			   p, min_value[p], max_value[p]));
		    if (param_size[p] == 1) {
			ByteField(pyr_dst, node_offset[p]) = min_value[p];
			ByteField(pyr_dst, node_offset[p]+1) = max_value[p];
		    } else {
			ASSERT(param_size[p] == 2);
			ShortField(pyr_dst, node_offset[p]) = min_value[p];
			ShortField(pyr_dst, node_offset[p]+2) = max_value[p];
		    }
		}
		pyr_dst += pyr_bytes_per_node;
		voxel += node_size * (voxel_xstride - voxel_zstride);
	    } /* for x */
	    voxel += node_size*(voxel_ystride - nodes_per_side*voxel_xstride);
	} /* for y */
	voxel += node_size*(voxel_zstride - nodes_per_side*voxel_ystride);
    } /* for z */
    ReportStatus(vpc, 1.0);

    /* build the rest of the pyramid */
    for (level = pyr_levels-2; level >= 0; level--) {
	ReportStatus(vpc, 1. - (double)(level+1)/(double)(pyr_levels-1));
	Debug((vpc, VPDEBUG_PYRAMID, "Pyramid Level %d:\n", level));
	pyr_dst = mm_pyramid[level];
	pyr_node = mm_pyramid[level+1];
	pyr_offsets[0] = 0;
	pyr_offsets[1] = pyr_bytes_per_node;
	pyr_offsets[2] = nodes_per_side * pyr_bytes_per_node;
	pyr_offsets[3] = pyr_offsets[2] + pyr_bytes_per_node;
	pyr_offsets[4] = pyr_offsets[2] * nodes_per_side;
	pyr_offsets[5] = pyr_offsets[4] + pyr_bytes_per_node;
	pyr_offsets[6] = pyr_offsets[4] + pyr_offsets[2];
	pyr_offsets[7] = pyr_offsets[6] + pyr_bytes_per_node;
	node_size *= 2;
	nodes_per_side /= 2;
	for (z = 0; z < nodes_per_side; z++) {
	    for (y = 0; y < nodes_per_side; y++) {
		for (x = 0; x < nodes_per_side; x++) {
		    /* clear the min/max values for the current node */
		    for (p = 0; p < num_params; p++) {
			max_value[p] = -1;
			min_value[p] = 65536;
		    }

		    /* loop over the eight children of this node */
		    for (elem = 0; elem < 8; elem++) {
			pyr_src = pyr_node + pyr_offsets[elem];
			/* compare min/max values of children with current
			   min/max values for the node */
			for (p = 0; p < num_params; p++) {
			    value = VoxelField(pyr_src, node_offset[p],
					       param_size[p]);
			    if (value < min_value[p])
				min_value[p] = value;
			    value = VoxelField(pyr_src, node_offset[p] +
					       param_size[p], param_size[p]);
			    if (value > max_value[p])
				max_value[p] = value;
			}
		    }

		    /* store the min/max values for this node */
		    Debug((vpc, VPDEBUG_PYRAMID, "  Node %d,%d,%d:\n",x,y,z));
		    for (p = 0; p < num_params; p++) {
			ASSERT(max_value[p] >= 0 && max_value[p] < 65536);
			ASSERT(min_value[p] >= 0 && min_value[p] < 65536);
			Debug((vpc, VPDEBUG_PYRAMID,
			       "    Param %d: min %d, max %d\n",
			       p, min_value[p], max_value[p]));
			if (param_size[p] == 1) {
			    ByteField(pyr_dst, node_offset[p]) = min_value[p];
			    ByteField(pyr_dst, node_offset[p]+1)=max_value[p];
			} else {
			    ASSERT(param_size[p] == 2);
			    ShortField(pyr_dst, node_offset[p]) = min_value[p];
			    ShortField(pyr_dst, node_offset[p]+2)=max_value[p];
			}
		    }

		    /* go on to the next node */
		    pyr_dst += pyr_bytes_per_node;
		    pyr_node += 2*pyr_bytes_per_node;
		} /* for x */
		pyr_node += (2*nodes_per_side)*pyr_bytes_per_node;
	    } /* for y */
	    pyr_node += (2*nodes_per_side)*(2*nodes_per_side)*
			pyr_bytes_per_node;
	} /* for z */
    } /* for level */
    ReportStatus(vpc, 1.0);
}

/*
 * DescendPyramid
 *
 * Descend the pyramid recursively, either to count how many nodes will
 * be copied to the octree (if parent_node == NULL) or to actually copy them.
 */

static void
DescendPyramid(vpc, mm_pyramid, level, x, y, z, nodes_per_side,
	       parent_node, octree_offset)
vpContext *vpc;		/* context */
void *mm_pyramid[VP_MAX_OCTREE_LEVELS];	/* min-max pyramid */
int level;		/* current level */
int x, y, z;		/* current node coordinates (in coordinate system
			   of the current level) */
int nodes_per_side;	/* # nodes at current level per side of volume */
void *parent_node;	/* parent octree node (or NULL) */
int *octree_offset;	/* bytes from root of octree to next free location */
{
    char *pyr_ptr;
    char *child_node;
    int p;
    MinMaxOctree *mm_octree;
    int pyr_bytes_per_node;
    int base_bytes_per_node;
    int nonbase_bytes_per_node;
    int child_bytes_per_node;
    int field_size;
    int field_offset;
    int child_offset;
    int range;
    int subdivide;

    ASSERT(vpc->mm_octree != NULL);
    mm_octree = vpc->mm_octree;
    pyr_bytes_per_node = mm_octree->range_bytes_per_node;
    base_bytes_per_node = mm_octree->base_bytes_per_node;
    nonbase_bytes_per_node = mm_octree->nonbase_bytes_per_node;
    child_offset = mm_octree->child_offset;
    pyr_ptr = (char *)mm_pyramid[level] + ((z*nodes_per_side + y) *
	      nodes_per_side + x) * pyr_bytes_per_node;

    /* copy min/max data from pyramid node to octree node */
    if (parent_node != NULL) {
	Debug((vpc, VPDEBUG_OCTREE,
	       "  Node at level %d, coords %d,%d,%d, addr 0x%08x\n",
	       level, x, y, z, parent_node));
	for (p = 0; p < pyr_bytes_per_node; p++)
	    ByteField(parent_node, p) = ByteField(pyr_ptr, p);
    }

    /* descend to next level */
    if (level < mm_octree->levels-1) {
	/* check if we should subdivide node or not */
	subdivide = 0;
	for (p = 0; p < vpc->num_clsfy_params; p++) {
	    field_size = vpc->field_size[vpc->param_field[p]];
	    field_offset = mm_octree->node_offset[p];
	    if (field_size == 1) {
		range = ByteField(pyr_ptr, field_offset+1) -
			ByteField(pyr_ptr, field_offset);
	    } else {
		ASSERT(field_size == 2);
		range = ShortField(pyr_ptr, field_offset+2) -
			ShortField(pyr_ptr, field_offset);
	    }
	    if (range > vpc->param_maxrange[p]) {
		subdivide = 1;
		break;
	    }
	}

	if (subdivide) {
	    /* store offset to child */
	    if (parent_node != NULL) {
		child_node = (char *)mm_octree->root + *octree_offset;
		IntField(parent_node, child_offset) = *octree_offset;
		Debug((vpc, VPDEBUG_OCTREE,
		       "    Storing children at offset = %d, addr = 0x%08x\n",
		       *octree_offset, child_node));
	    }
	    if (level == mm_octree->levels-2)
		child_bytes_per_node = base_bytes_per_node;
	    else
		child_bytes_per_node = nonbase_bytes_per_node;
	    *octree_offset += 8 * child_bytes_per_node;
	    if (parent_node == NULL) {
		child_node = NULL;
		child_bytes_per_node = 0;
	    }

	    /* visit children */
	    DescendPyramid(vpc, mm_pyramid, level+1, x*2, y*2, z*2,
			   nodes_per_side*2, child_node, octree_offset);
	    child_node += child_bytes_per_node;
	    DescendPyramid(vpc, mm_pyramid, level+1, x*2+1, y*2, z*2,
			   nodes_per_side*2, child_node, octree_offset);
	    child_node += child_bytes_per_node;
	    DescendPyramid(vpc, mm_pyramid, level+1, x*2, y*2+1, z*2,
			   nodes_per_side*2, child_node, octree_offset);
	    child_node += child_bytes_per_node;
	    DescendPyramid(vpc, mm_pyramid, level+1, x*2+1, y*2+1, z*2,
			   nodes_per_side*2, child_node, octree_offset);
	    child_node += child_bytes_per_node;
	    DescendPyramid(vpc, mm_pyramid, level+1, x*2, y*2, z*2+1,
			   nodes_per_side*2, child_node, octree_offset);
	    child_node += child_bytes_per_node;
	    DescendPyramid(vpc, mm_pyramid, level+1, x*2+1, y*2, z*2+1,
			   nodes_per_side*2, child_node, octree_offset);
	    child_node += child_bytes_per_node;
	    DescendPyramid(vpc, mm_pyramid, level+1, x*2, y*2+1, z*2+1,
			   nodes_per_side*2, child_node, octree_offset);
	    child_node += child_bytes_per_node;
	    DescendPyramid(vpc, mm_pyramid, level+1, x*2+1,y*2+1,z*2+1,
			   nodes_per_side*2, child_node, octree_offset);
	} else {
	    /* node has no children; store NULL pointer */
	    Debug((vpc, VPDEBUG_OCTREE, "    Not subdividing.\n"));
	    if (parent_node != NULL) {
		IntField(parent_node, child_offset) = 0;
	    }
	}
    }
}

/*
 * VPComputeSummedAreaTable
 *
 * Build the summed-area table for fast-classification.
 */

void
VPComputeSummedAreaTable(vpc)
vpContext *vpc;
{
    /* use a special-case version for lower dimensions
       (faster since C optimizer does a better job) */
    switch (vpc->num_clsfy_params) {
    case 1:
	Compute1DSummedAreaTable(vpc);
	break;
    case 2:
	Compute2DSummedAreaTable(vpc);
	break;
    default:
	/* XXX add code for ND classifiers */
	VPBug("VPComputeSummedAreaTable can only handle 1D or 2D classifiers");
	break;
    }
}

/*
 * Compute1DSummedAreaTable
 *
 * Build a 1D summed area table.
 */

static void
Compute1DSummedAreaTable(vpc)
vpContext *vpc;
{
    int p0max, p0value;
    unsigned table_size;
    float opacity, min_opacity, *p0table;
    unsigned sum;
    unsigned *entry;

    p0max = vpc->field_max[vpc->param_field[0]];
    table_size = (p0max+1) * sizeof(unsigned);
    p0table = vpc->clsfy_table[0];
    min_opacity = vpc->min_opacity;
    if (vpc->sum_table == NULL || table_size != vpc->sum_table_size) {
	if (vpc->sum_table != NULL)
	    Dealloc(vpc, vpc->sum_table);
	Alloc(vpc, vpc->sum_table, unsigned *, table_size, "sum_table");
	vpc->sum_table_size = table_size;
    }
    entry = vpc->sum_table;
    for (p0value = 0; p0value <= p0max; p0value++) {
	opacity = p0table[p0value];
	if (opacity > min_opacity)
	    sum = 1;
	else
	    sum = 0;
	if (p0value > 0)
	    sum += entry[-1];
	entry[0] = sum;
	entry++;
    }
}

/*
 * Compute2DSummedAreaTable
 *
 * Build a 2D summed area table.
 */

static void
Compute2DSummedAreaTable(vpc)
vpContext *vpc;
{
    int p0max, p0value, p1max, p1value;
    unsigned table_size;
    float opacity, min_opacity, *p0table, *p1table;
    unsigned sum;
    unsigned *entry;

    p0max = vpc->field_max[vpc->param_field[0]];
    p1max = vpc->field_max[vpc->param_field[1]];
    table_size = (p0max+1) * (p1max+1) * sizeof(unsigned);
    p0table = vpc->clsfy_table[0];
    p1table = vpc->clsfy_table[1];
    min_opacity = vpc->min_opacity;
    if (vpc->sum_table == NULL || table_size != vpc->sum_table_size) {
	if (vpc->sum_table != NULL)
	    Dealloc(vpc, vpc->sum_table);
	Alloc(vpc, vpc->sum_table, unsigned *, table_size, "sum_table");
	vpc->sum_table_size = table_size;
    }
    entry = vpc->sum_table;
    for (p0value = 0; p0value <= p0max; p0value++) {
	for (p1value = 0; p1value <= p1max; p1value++) {
	    opacity = p0table[p0value] * p1table[p1value];
	    if (opacity > min_opacity)
		sum = 1;
	    else
		sum = 0;
	    if (p1value > 0) {
		sum += entry[-1];
		if (p0value > 0) {
		    sum += entry[-(p1max+1)];
		    sum -= entry[-(p1max+1)-1];
		}
	    } else if (p0value > 0) {
		sum += entry[-(p1max+1)];
	    }
	    entry[0] = sum;
	    entry++;
	}
    }
}

/*
 * VPClassifyOctree
 *
 * Descend an octree and classify each node as full, empty or partfull.
 */

void
VPClassifyOctree(vpc)
vpContext *vpc;
{
    /* use a special-case version for lower dimensions
       (faster since C optimizer does a better job) */
    switch (vpc->num_clsfy_params) {
    case 1:
	ClassifyOctree1(vpc);
	break;
    case 2:
	ClassifyOctree2(vpc);
	break;
    default:
	/* XXX add code for ND classifiers */
	VPBug("VPClassifyOctree can only handle 2D classifiers");
	break;
    }
}

/*
 * ClassifyOctree1
 *
 * Descend an octree and classify each node as full, empty or partfull.
 * Specialized for a 1 parameter classification function (1D summed
 * area table).
 */

static void
ClassifyOctree1(vpc)
vpContext *vpc;
{
    char *node_stack[VP_MAX_OCTREE_LEVELS]; /* stack of node addresses */
    int count_stack[VP_MAX_OCTREE_LEVELS];  /* stack of node child counts;
				   when count drops to zero, pop up a level */
    int level;			/* current octree level */
    int max_level;		/* highest octree level */
    char *octree_root;		/* root node of octree */
    char *node;			/* current octree node */
    unsigned area;		/* area computed from the summed-area table */
    int status;			/* classification status of current node */
    unsigned *sum_table;	/* summed area table */
    int p0max, p0min;		/* parameter 0 extrema */
    int p0size;			/* parameter size */
    int child_offset;		/* offset of child field in node */
    int status_offset;		/* offset of status field in node */
    int base_bytes_per_node;	/* size of base node in bytes */
    int nonbase_bytes_per_node;	/* size of nonbase node in bytes */
    int child_count;		/* children left at current level */

    /* initialize */
    ASSERT(vpc->sum_table != NULL);
    ASSERT(vpc->mm_octree != NULL);
    ASSERT(vpc->mm_octree->root != NULL);
    ASSERT(vpc->sum_table_size == sizeof(unsigned) *
	   (vpc->field_max[vpc->param_field[0]]+1));
    sum_table = vpc->sum_table;
    max_level = vpc->mm_octree->levels - 1;
    octree_root = vpc->mm_octree->root;
    p0size = vpc->field_size[vpc->param_field[0]];
    status_offset = vpc->mm_octree->status_offset;
    child_offset = vpc->mm_octree->child_offset;
    base_bytes_per_node = vpc->mm_octree->base_bytes_per_node;
    nonbase_bytes_per_node = vpc->mm_octree->nonbase_bytes_per_node;
    node = octree_root;
    level = 0;

    /* do a depth-first, preorder traversal of the octree */
    Debug((vpc, VPDEBUG_CLSFYOCTREE, "Classifying octree:\n"));
    while (1) {
	/* find min/max values for both parameters in this node */
	if (p0size == 1) {
	    p0min = ByteField(node, 0)-1;
	    p0max = ByteField(node, 1);
	} else {
	    p0min = ShortField(node, 0)-1;
	    p0max = ShortField(node, 2);
	}

	/* integrate the opacities in the node using the summed area table */
	area = sum_table[p0max];
	if (p0min >= 0)
	    area -= sum_table[p0min];

	/* decide if node is full, empty or partfull */
	if (area == 0) {
	    status = MM_EMPTY;
	} else if (level != max_level && IntField(node, child_offset) != 0 &&
		   area != (p0max - p0min)) {
	    status = MM_PARTFULL;
	} else {
	    status = MM_FULL;
	}
	ByteField(node, status_offset) = status;
	Debug((vpc, VPDEBUG_CLSFYOCTREE,
	       "  Level %d: node is %s (addr 0x%08x)\n", level,
	       status == MM_EMPTY ? "empty" : (status == MM_FULL ? "full" :
	       "partfull"), node));

	/* move to next node in tree traversal */
	if (status == MM_PARTFULL) {
	    /* move down to first child in next level */
	    node = octree_root + IntField(node, child_offset);
	    Debug((vpc, VPDEBUG_CLSFYOCTREE,
		   "  Descending.  Children at offset %d, addr 0x%08x\n",
		   IntField(node, child_offset), node));
	    node_stack[level] = node;
	    count_stack[level] = 7;	/* number of remaining children */
	    level++;
	    ASSERT(level <= max_level);
	} else {
	    do {
		/* move up to a node with unvisited children */
		Debug((vpc, VPDEBUG_CLSFYOCTREE, "  Ascending.\n"));
		level--;
		if (level < 0)
		    break;
		child_count = count_stack[level]--;
		ASSERT(child_count >= 0 && child_count <= 7);
	    } while (child_count == 0);
	    if (level < 0)
		break;	/* traversal of octree is done! */

	    /* descend to the next child of this node */
	    if (level == max_level-1)
		node = node_stack[level] + base_bytes_per_node;
	    else
		node = node_stack[level] + nonbase_bytes_per_node;
	    Debug((vpc, VPDEBUG_CLSFYOCTREE,
		   "  Descending to child at 0x%08x.\n", node));
	    node_stack[level] = node;
	    level++;
	    ASSERT(level <= max_level);
	}
    } /* while (1) */
}

/*
 * ClassifyOctree2
 *
 * Descend an octree and classify each node as full, empty or partfull.
 * Specialized for a 2 parameter classification function (2D summed
 * area table).
 */

static void
ClassifyOctree2(vpc)
vpContext *vpc;
{
    char *node_stack[VP_MAX_OCTREE_LEVELS]; /* stack of node addresses */
    int count_stack[VP_MAX_OCTREE_LEVELS];  /* stack of node child counts;
				   when count drops to zero, pop up a level */
    int level;			/* current octree level */
    int max_level;		/* highest octree level */
    char *octree_root;		/* root node of octree */
    char *node;			/* current octree node */
    unsigned area;		/* area computed from the summed-area table */
    int status;			/* classification status of current node */
    unsigned *sum_table;	/* summed area table */
    int sum_table_dim1;		/* size of last dimension of sum_table */
    int p0max, p0min;		/* parameter 0 extrema */
    int p1max, p1min;		/* parameter 1 extrema */
    int p0size, p1size;		/* parameter sizes */
    int child_offset;		/* offset of child field in node */
    int status_offset;		/* offset of status field in node */
    int base_bytes_per_node;	/* size of base node in bytes */
    int nonbase_bytes_per_node;	/* size of nonbase node in bytes */
    int child_count;		/* children left at current level */

    /* initialize */
    ASSERT(vpc->sum_table != NULL);
    ASSERT(vpc->mm_octree != NULL);
    ASSERT(vpc->mm_octree->root != NULL);
    ASSERT(vpc->sum_table_size == sizeof(unsigned) *
	   (vpc->field_max[vpc->param_field[0]]+1) *
	   (vpc->field_max[vpc->param_field[1]]+1));
    sum_table = vpc->sum_table;
    max_level = vpc->mm_octree->levels - 1;
    octree_root = vpc->mm_octree->root;
    p0size = vpc->field_size[vpc->param_field[0]];
    p1size = vpc->field_size[vpc->param_field[1]];
    sum_table_dim1 = vpc->field_max[vpc->param_field[1]] + 1;
    status_offset = vpc->mm_octree->status_offset;
    child_offset = vpc->mm_octree->child_offset;
    base_bytes_per_node = vpc->mm_octree->base_bytes_per_node;
    nonbase_bytes_per_node = vpc->mm_octree->nonbase_bytes_per_node;
    node = octree_root;
    level = 0;

    /* do a depth-first, preorder traversal of the octree */
    Debug((vpc, VPDEBUG_CLSFYOCTREE, "Classifying octree:\n"));
    while (1) {
	/* find min/max values for both parameters in this node */
	if (p0size == 1) {
	    p0min = ByteField(node, 0)-1;
	    p0max = ByteField(node, 1);
	} else {
	    p0min = ShortField(node, 0)-1;
	    p0max = ShortField(node, 2);
	}
	if (p1size == 1) {
	    p1min = ByteField(node, 2*p0size)-1;
	    p1max = ByteField(node, 2*p0size+1);
	} else {
	    p1min = ShortField(node, 2*p0size)-1;
	    p1max = ShortField(node, 2*p0size+2);
	}

	/* integrate the opacities in the node using the summed area table */
	area = sum_table[p0max * sum_table_dim1 + p1max];
	if (p0min >= 0) {
	    if (p1min >= 0) {
		area += sum_table[p0min * sum_table_dim1 + p1min];
		area -= sum_table[p0max * sum_table_dim1 + p1min];
	    }
	    area -= sum_table[p0min * sum_table_dim1 + p1max];
	} else {
	    if (p1min >= 0)
		area -= sum_table[p0max * sum_table_dim1 + p1min];
	}

	/* decide if node is full, empty or partfull */
	if (area == 0) {
	    status = MM_EMPTY;
	} else if (level != max_level && IntField(node, child_offset) != 0 &&
		   area != (p1max - p1min)*(p0max - p0min)) {
	    status = MM_PARTFULL;
	} else {
	    status = MM_FULL;
	}
	ByteField(node, status_offset) = status;
	Debug((vpc, VPDEBUG_CLSFYOCTREE,
	       "  Level %d: node is %s (addr 0x%08x)\n", level,
	       status == MM_EMPTY ? "empty" : (status == MM_FULL ? "full" :
	       "partfull"), node));

	/* move to next node in tree traversal */
	if (status == MM_PARTFULL) {
	    /* move down to first child in next level */
	    node = octree_root + IntField(node, child_offset);
	    Debug((vpc, VPDEBUG_CLSFYOCTREE,
		   "  Descending.  Children at offset %d, addr 0x%08x\n",
		   IntField(node, child_offset), node));
	    node_stack[level] = node;
	    count_stack[level] = 7;	/* number of remaining children */
	    level++;
	    ASSERT(level <= max_level);
	} else {
	    do {
		/* move up to a node with unvisited children */
		Debug((vpc, VPDEBUG_CLSFYOCTREE, "  Ascending.\n"));
		level--;
		if (level < 0)
		    break;
		child_count = count_stack[level]--;
		ASSERT(child_count >= 0 && child_count <= 7);
	    } while (child_count == 0);
	    if (level < 0)
		break;	/* traversal of octree is done! */

	    /* descend to the next child of this node */
	    if (level == max_level-1)
		node = node_stack[level] + base_bytes_per_node;
	    else
		node = node_stack[level] + nonbase_bytes_per_node;
	    Debug((vpc, VPDEBUG_CLSFYOCTREE,
		   "  Descending to child at 0x%08x.\n", node));
	    node_stack[level] = node;
	    level++;
	    ASSERT(level <= max_level);
	}
    } /* while (1) */
}

/*
 * VPInitOctreeLevelStack
 *
 * Initialize an MMOctreeLevel stack.
 */

void
VPInitOctreeLevelStack(vpc, level_stack, axis, k)
vpContext *vpc;
MMOctreeLevel level_stack[VP_MAX_OCTREE_LEVELS];
int axis;	/* principle viewing axis */
int k;		/* current slice number */
{
    int max_level, level, last_node_size;
    int child_octant, child_bytes_per_node;
    int *octant_order;

    ASSERT(vpc->mm_octree != NULL);
    max_level = vpc->mm_octree->levels-1;
    level_stack[max_level].level_size = vpc->mm_octree->base_node_size;
    level_stack[max_level].child_octant = -1;
    level_stack[max_level].child_offset1 = -1;
    level_stack[max_level].child_offset2 = -1;
    level_stack[max_level].child2 = NULL;
    last_node_size = vpc->mm_octree->base_node_size;
    octant_order = OctantOrder[axis];
    Debug((vpc, VPDEBUG_OCTREETRAVERSE, "Octants for next scanline:\n"));
    for (level = max_level-1; level >= 0; level--) {
	level_stack[level].level_size = last_node_size * 2;
	child_octant = ((k / last_node_size) & 1) * MM_K_BIT;
	last_node_size *= 2;
	level_stack[level].child_octant = child_octant;
	if (level == max_level-1)
	    child_bytes_per_node = vpc->mm_octree->base_bytes_per_node;
	else
	    child_bytes_per_node = vpc->mm_octree->nonbase_bytes_per_node;
	ASSERT(child_octant >= 0 && child_octant < 7);
	ASSERT(octant_order[child_octant] >= 0 &&
	       octant_order[child_octant] < 8);
	level_stack[level].child_offset1 = octant_order[child_octant] *
	 				   child_bytes_per_node;
	level_stack[level].child_offset2 = octant_order[child_octant+1] *
	 				   child_bytes_per_node;
	Debug((vpc, VPDEBUG_OCTREETRAVERSE, "  Level %d: %d, then %d\n",
	       level,octant_order[child_octant],octant_order[child_octant+1]));
    }
}

/*
 * VPComputeScanRuns
 *
 * For a given voxel scanline, produce a sequence of run lengths
 * which give a conservative estimate of the non-transparent portions
 * of the scanline.  The runs are computed by finding which nodes
 * of the classified min-max octree are intersected by the scanline.
 *
 * The return value is the number of scanlines for which this run data
 * is valid.
 */

int
VPComputeScanRuns(vpc, level_stack, run_lengths, axis, j, icount)
vpContext *vpc;
MMOctreeLevel level_stack[VP_MAX_OCTREE_LEVELS]; /* saved state */
unsigned char *run_lengths; /* storage for run lengths */
int axis;		/* principle viewing axis */
int j;			/* scanline number */
int icount;		/* scanline length */
{
    int octree_maxlevel;
    int level;
    int max_level = vpc->mm_octree->levels-1;
    int child_octant, child_bytes_per_node;
    int base_bytes_per_node, nonbase_bytes_per_node;
    int i;
    char *octree_root, *node;
    int run_type;
    int run_length;
    int run_piece;
    int status_offset;
    int child_offset;
    int status;
    int *octant_order;

    Debug((vpc, VPDEBUG_OCTREERUNS, "Runs for scanline %d:\n", j));
    Debug((vpc, VPDEBUG_OCTREETRAVERSE, "Traversal for scanline %d:\n", j));
    ASSERT(vpc->mm_octree != NULL);
    ASSERT(vpc->mm_octree->root != NULL);
    base_bytes_per_node = vpc->mm_octree->base_bytes_per_node;
    nonbase_bytes_per_node = vpc->mm_octree->nonbase_bytes_per_node;
    status_offset = vpc->mm_octree->status_offset;
    child_offset = vpc->mm_octree->child_offset;
    octree_maxlevel = -1;
    i = icount;
    octree_root = vpc->mm_octree->root;
    node = octree_root;
    level = 0;
    run_type = MM_EMPTY;
    run_length = 0;
    octant_order = OctantOrder[axis];

    /* traverse the octree */
    while (1) {
	/* descend tree to next node which is not partfull */
	while (1) {
	    status = ByteField(node, status_offset);
	    Debug((vpc, VPDEBUG_OCTREETRAVERSE, "    Node at level %d: %s\n",
		   level, status == MM_PARTFULL ? "partfull" :
		   (status == MM_FULL ? "full" : "empty")));
	    ASSERT(status == MM_PARTFULL || status == MM_FULL ||
		   status == MM_EMPTY);
	    if (status != MM_PARTFULL)
		break;
	    ASSERT(IntField(node, child_offset) != 0);
	    Debug((vpc, VPDEBUG_OCTREETRAVERSE,
		   "    Children at base %d, offsets %d, %d; ",
		   IntField(node, child_offset),
		   level_stack[level].child_offset1,
		   level_stack[level].child_offset2));
	    Debug((vpc, VPDEBUG_OCTREETRAVERSE, "status %d, %d\n",
		   ByteField(octree_root + IntField(node, child_offset) +
		   level_stack[level].child_offset1, status_offset),
		   ByteField(octree_root + IntField(node, child_offset) +
		   level_stack[level].child_offset2, status_offset)));
	    node = octree_root + IntField(node, child_offset);
	    level_stack[level].child2 = node+level_stack[level].child_offset2;
	    node += level_stack[level].child_offset1;
	    level++;
	    Debug((vpc, VPDEBUG_OCTREETRAVERSE, "    Descending.\n"));
	    ASSERT(level < vpc->mm_octree->levels);
	}
	if (level > octree_maxlevel)
	    octree_maxlevel = level;

	/* add current node to the list of runs */
	run_piece = MIN(level_stack[level].level_size, i);
	i -= run_piece;
	if (status == run_type) {
	    run_length += run_piece;
	} else {
	    Debug((vpc, VPDEBUG_OCTREETRAVERSE, "    New run.\n"));
	    while (run_length > 255) {
		Debug((vpc, VPDEBUG_OCTREERUNS, " 255 0"));
		*run_lengths++ = 255;
		*run_lengths++ = 0;
		run_length -= 255;
	    }
	    Debug((vpc, VPDEBUG_OCTREERUNS, " %d", run_length));
	    *run_lengths++ = run_length;
	    run_type ^= 1;
	    run_length = run_piece;
	}
	Debug((vpc, VPDEBUG_OCTREETRAVERSE, "    Added %d to run.\n",
	       run_piece));
	if (i == 0)
	    break;	/* traversal is done */

	/* move back up the tree to the next node with unvisited children */
	do {
	    Debug((vpc, VPDEBUG_OCTREETRAVERSE, "    Ascending.\n"));
	    level--;
	    ASSERT(level >= 0);
	} while (level_stack[level].child2 == NULL);

	/* descend to next child */
	Debug((vpc, VPDEBUG_OCTREETRAVERSE, "    Next child--descending.\n"));
	node = level_stack[level].child2;
	level_stack[level].child2 = NULL;
	level++;
	ASSERT(level < vpc->mm_octree->levels);
    } /* while (1) */

    /* write out the last run */
    while (run_length > 255) {
	Debug((vpc, VPDEBUG_OCTREERUNS, " 255 0"));
	*run_lengths++ = 255;
	*run_lengths++ = 0;
	run_length -= 255;
    }
    Debug((vpc, VPDEBUG_OCTREERUNS, " %d", run_length));
    *run_lengths++ = run_length;
    if (run_type == MM_EMPTY) {
	Debug((vpc, VPDEBUG_OCTREERUNS, " 0"));
	*run_lengths++ = 0;
    }
    Debug((vpc, VPDEBUG_OCTREERUNS, "\n"));

    Debug((vpc, VPDEBUG_OCTREETRAVERSE, "Covered %d scanlines.\n",
	   level_stack[octree_maxlevel].level_size));

    /* update state for next scanline: adjust child_octant and then
       use it to compute child_offset1 and child_offset2 */
    j += level_stack[octree_maxlevel].level_size;
    max_level = vpc->mm_octree->levels-1;
    Debug((vpc, VPDEBUG_OCTREETRAVERSE, "Octants for next scanline:\n"));
    for (level = max_level-1; level >= 0; level--) {
	child_octant = level_stack[level].child_octant;
	if (level >= octree_maxlevel)
	    child_octant &= MM_K_BIT;
	else if ((j & (level_stack[level].level_size/2)) == 0)
	    child_octant &= ~MM_J_BIT;
	else
	    child_octant |= MM_J_BIT;
	level_stack[level].child_octant = child_octant;

	if (level == max_level-1)
	    child_bytes_per_node = base_bytes_per_node;
	else
	    child_bytes_per_node = nonbase_bytes_per_node;
	level_stack[level].child_offset1 = octant_order[child_octant] *
	 				   child_bytes_per_node;
	level_stack[level].child_offset2 = octant_order[child_octant+1] *
	 				   child_bytes_per_node;
	Debug((vpc, VPDEBUG_OCTREETRAVERSE, "  Level %d: %d, then %d\n",
	       level,octant_order[child_octant],octant_order[child_octant+1]));
    }

    /* return the number of scanlines for which the run lengths are valid
       (which is the size of the smallest octree node the scanline hit) */
    return(level_stack[octree_maxlevel].level_size);
}

/*
 * vpOctreeMask
 *
 * Fill a 3D array with a mask computed from an octree. 
 * Each array element is set to one of three values depending upon
 * the value of the corresponding voxel in the octree:
 *    0		voxel is definitely transparent
 *    255	voxel may be non-transparent
 *    128	voxel may be non-transparent, and more detailed information
 *		is available at deeper levels of the octree which were not
 *		visited
 */

vpResult
vpOctreeMask(vpc, array, array_size, max_level)
vpContext *vpc;		/* context */
unsigned char *array;	/* array for result */
int array_size;		/* size of array in bytes */
int max_level;
{
    int c;
    unsigned char *aptr;
    int retcode;

    /* error checks */
    if (vpc->mm_octree == NULL)
	return(VPSetError(vpc, VPERROR_BAD_SIZE));
    if ((retcode = VPCheckClassifier(vpc)) == VP_OK)
	return(retcode);
    if (array_size != vpc->xlen*vpc->ylen*vpc->zlen)
	return(VPSetError(vpc, VPERROR_BAD_SIZE));

    /* classify the octree */
    VPComputeSummedAreaTable(vpc);
    VPClassifyOctree(vpc);
    ComputeOctreeMask(vpc, 0, 0, 0, 0, vpc->mm_octree->root_node_size,
		      vpc->mm_octree->root, array, max_level);
    return(VP_OK);
}

/*
 * ComputeOctreeMask
 *
 * Recursive helper function for vpOctreeMask.
 */

static void
ComputeOctreeMask(vpc, level, xn, yn, zn, node_size, parent_node, array,
		  max_level)
vpContext *vpc;		/* context */
int level;		/* current level */
int xn, yn, zn;		/* current node coordinates (in coordinate system
			   of the current level) */
int node_size;		/* voxel per side of node at this level */
void *parent_node;	/* parent octree node */
unsigned char *array;	/* array for storing result */
int max_level;		/* deepest level of the tree to visit */
{
    char *child_node, *octree_root;
    int child_bytes_per_node;
    int child_offset;
    int status_offset;
    int status, value;
    int x, y, z, x0, y0, z0, x1, y1, z1;
    int array_ystride, array_zstride;

    /* initialize */
    status_offset = vpc->mm_octree->status_offset;
    child_offset = vpc->mm_octree->child_offset;
    if (level == vpc->mm_octree->levels-2)
	child_bytes_per_node = vpc->mm_octree->base_bytes_per_node;
    else
	child_bytes_per_node = vpc->mm_octree->nonbase_bytes_per_node;
    octree_root = vpc->mm_octree->root;

    /* base case */
    status = ByteField(parent_node, status_offset);
    if (level == max_level || status != MM_PARTFULL) {
	if (status == MM_EMPTY)
	    value = 0;
	else if (status == MM_FULL)
	    value = 255;
	else if (status == MM_PARTFULL)
	    value = 128;
	else
	    VPBug("bad status value in ComputeOctreeMask, nodeaddr = 0x%08x",
		  parent_node);
	x0 = xn * node_size;
	y0 = yn * node_size;
	z0 = zn * node_size;
	x1 = MIN(x0 + node_size, vpc->xlen) - 1;
	y1 = MIN(y0 + node_size, vpc->ylen) - 1;
	z1 = MIN(z0 + node_size, vpc->zlen) - 1;
	array_ystride = vpc->xlen;
	array_zstride = vpc->xlen * vpc->ylen;
	for (z = z0; z <= z1; z++) {
	    for (y = y0; y <= y1; y++) {
		for (x = x0; x <= x1; x++) {
		    array[z*array_zstride + y*array_ystride + x] = value;
		}
	    }
	}
	return;
    }
    ASSERT(IntField(parent_node, child_offset) != 0);

    /* visit children */
    child_node = octree_root + IntField(parent_node, child_offset);
    ComputeOctreeMask(vpc, level+1, xn*2, yn*2, zn*2, node_size/2,
		      child_node, array, max_level);
    child_node += child_bytes_per_node;
    ComputeOctreeMask(vpc, level+1, xn*2+1, yn*2, zn*2, node_size/2,
		      child_node, array, max_level);
    child_node += child_bytes_per_node;
    ComputeOctreeMask(vpc, level+1, xn*2, yn*2+1, zn*2, node_size/2,
		      child_node, array, max_level);
    child_node += child_bytes_per_node;
    ComputeOctreeMask(vpc, level+1, xn*2+1, yn*2+1, zn*2, node_size/2,
		      child_node, array, max_level);
    child_node += child_bytes_per_node;
    ComputeOctreeMask(vpc, level+1, xn*2, yn*2, zn*2+1, node_size/2,
		      child_node, array, max_level);
    child_node += child_bytes_per_node;
    ComputeOctreeMask(vpc, level+1, xn*2+1, yn*2, zn*2+1, node_size/2,
		      child_node, array, max_level);
    child_node += child_bytes_per_node;
    ComputeOctreeMask(vpc, level+1, xn*2, yn*2+1, zn*2+1, node_size/2,
		      child_node, array, max_level);
    child_node += child_bytes_per_node;
    ComputeOctreeMask(vpc, level+1, xn*2+1,yn*2+1,zn*2+1, node_size/2,
		      child_node, array, max_level);
}

#ifdef DEBUG
/*
 * VPCheckRuns
 *
 * Check a scanline of run lengths for validity by comparing it to
 * the raw volume data.  Return value is the number of voxels in
 * nonzero runs which are actually zero (due to conservative
 * approximations.)  If an error is detected then VPBug is called.
 */

int
VPCheckRuns(vpc, run_lengths, axis, k, j)
vpContext *vpc;
unsigned char *run_lengths;/* run lengths */
int axis;		/* principle viewing axis */
int k;			/* slice number */
int j;			/* scanline number */
{
    char *voxel;
    int i;
    int icount;
    int count;
    int is_non_zero;
    int num_runs;
    float opacity;
    int badpredictions;
    int istride;

    switch (axis) {
    case VP_X_AXIS:
	voxel = (char *)vpc->raw_voxels + k*vpc->xstride + j*vpc->zstride;
	istride = vpc->ystride;
	icount = vpc->ylen;
	break;
    case VP_Y_AXIS:
	voxel = (char *)vpc->raw_voxels + k*vpc->ystride + j*vpc->xstride;
	istride = vpc->zstride;
	icount = vpc->zlen;
	break;
    case VP_Z_AXIS:
	voxel = (char *)vpc->raw_voxels + k*vpc->zstride + j*vpc->ystride;
	istride = vpc->xstride;
	icount = vpc->xlen;
	break;
    default:
	VPBug("bad axis in VPCheckRuns");
    }

    count = 0;
    is_non_zero = 1;
    num_runs = 0;
    badpredictions = 0;
    for (i = 0; i < icount; i++) {
	while (count == 0) {
	    count = *run_lengths++;
	    is_non_zero = !is_non_zero;
	    if (++num_runs > icount)
		VPBug("runaway scanline detected by VPCheckRuns");
	}
	opacity = VPClassifyVoxel(vpc, voxel);
	if (opacity > vpc->min_opacity && 
	    fabs(opacity - vpc->min_opacity) > 0.001) {
	    if (!is_non_zero) {
		printf("\n");
		printf("VPCheckRuns: error on voxel (i,j,k)=(%d,%d,%d), ",
		       i, j, k);
		printf("viewaxis %d\n", axis);
		printf("Actual opacity: %17.15f\n", opacity);
		printf("Threshold:      %17.15f\n", vpc->min_opacity);
		VPDumpView(vpc);
		VPDumpClassifier(vpc);
		VPBug("nonzero voxel in zero run detected by VPCheckRuns");
	    }
	} else {
	    if (is_non_zero)
		badpredictions++;
	}
	voxel += istride;
	count--;
    }
    if (count != 0)
	VPBug("run that overshoots end of scanline detected by VPCheckRuns");
    if (!is_non_zero) {
	if (*run_lengths != 0)
	    VPBug("missing 0 run at end of scanline detected by VPCheckRuns");
    }
    return(badpredictions);
}

/*
 * VPTestMinMaxOctree
 *
 * Test out the MinMaxOctree routines.
 */

void
VPTestMinMaxOctree(vpc)
vpContext *vpc;
{
    int x, y, z;
    MMOctreeLevel level_stack[VP_MAX_OCTREE_LEVELS];
    unsigned char run_lengths[VP_MAX_VOLUME_DIM];
    int badpredictions;
    int scans_left;
    float accuracy;

    ASSERT(vpc->mm_octree != NULL);
    VPComputeSummedAreaTable(vpc);
    VPClassifyOctree(vpc);

    badpredictions = 0;
    printf("Checking +Z axis runs...\n");
    for (z = 0; z < vpc->zlen; z++) {
	ReportStatus(vpc, (double)z / (double)vpc->zlen);
	Debug((vpc, VPDEBUG_OCTREERUNS, "*** Slice %d ***\n", z));
	VPInitOctreeLevelStack(vpc, level_stack, VP_Z_AXIS, z);
	scans_left = 0;
	for (y = 0; y < vpc->ylen; y++) {
	    if (scans_left == 0) {
		scans_left = VPComputeScanRuns(vpc, level_stack, run_lengths,
					       VP_Z_AXIS, y, vpc->xlen);
	    }
	    scans_left--;
	    badpredictions += VPCheckRuns(vpc, run_lengths, VP_Z_AXIS, z, y);
	}
    }
    ReportStatus(vpc, 1.0);
    accuracy = 1. - ((double)badpredictions /
		     (double)(vpc->xlen*vpc->ylen*vpc->zlen));
    printf("VPTestMinMaxOctree: PASSED.\n");
    printf("Prediction accuracy: %.1f%% (%d bad predictions)\n",
	   accuracy*100., badpredictions);

    badpredictions = 0;
    printf("Checking +Y axis runs...\n");
    for (y = 0; y < vpc->ylen; y++) {
	ReportStatus(vpc, (double)y / (double)vpc->ylen);
	Debug((vpc, VPDEBUG_OCTREERUNS, "*** Slice %d ***\n", y));
	VPInitOctreeLevelStack(vpc, level_stack, VP_Y_AXIS, y);
	scans_left = 0;
	for (x = 0; x < vpc->xlen; x++) {
	    if (scans_left == 0) {
		scans_left = VPComputeScanRuns(vpc, level_stack, run_lengths,
					       VP_Y_AXIS, x, vpc->zlen);
	    }
	    scans_left--;
	    badpredictions += VPCheckRuns(vpc, run_lengths, VP_Y_AXIS, y, x);
	}
    }
    ReportStatus(vpc, 1.0);
    accuracy = 1. - ((double)badpredictions /
		     (double)(vpc->xlen*vpc->ylen*vpc->zlen));
    printf("VPTestMinMaxOctree: PASSED.\n");
    printf("Prediction accuracy: %.1f%% (%d bad predictions)\n",
	   accuracy*100., badpredictions);

    badpredictions = 0;
    printf("Checking +X axis runs...\n");
    for (x = 0; x < vpc->xlen; x++) {
	ReportStatus(vpc, (double)x / (double)vpc->xlen);
	Debug((vpc, VPDEBUG_OCTREERUNS, "*** Slice %d ***\n", x));
	VPInitOctreeLevelStack(vpc, level_stack, VP_X_AXIS, x);
	scans_left = 0;
	for (z = 0; z < vpc->zlen; z++) {
	    if (scans_left == 0) {
		scans_left = VPComputeScanRuns(vpc, level_stack, run_lengths,
					       VP_X_AXIS, z, vpc->ylen);
	    }
	    scans_left--;
	    badpredictions += VPCheckRuns(vpc, run_lengths, VP_X_AXIS, x, z);
	}
    }
    ReportStatus(vpc, 1.0);
    accuracy = 1. - ((double)badpredictions /
		     (double)(vpc->xlen*vpc->ylen*vpc->zlen));
    printf("VPTestMinMaxOctree: PASSED.\n");
    printf("Prediction accuracy: %.1f%% (%d bad predictions)\n",
	   accuracy*100., badpredictions);

    badpredictions = 0;
    printf("Checking -Z axis runs...\n");
    for (z = vpc->zlen-1; z >= 0; z--) {
	ReportStatus(vpc, (double)(vpc->zlen-1-z) / (double)vpc->zlen);
	Debug((vpc, VPDEBUG_OCTREERUNS, "*** Slice %d ***\n", z));
	VPInitOctreeLevelStack(vpc, level_stack, VP_Z_AXIS, z);
	scans_left = 0;
	for (y = 0; y < vpc->ylen; y++) {
	    if (scans_left == 0) {
		scans_left = VPComputeScanRuns(vpc, level_stack, run_lengths,
					       VP_Z_AXIS, y, vpc->xlen);
	    }
	    scans_left--;
	    badpredictions += VPCheckRuns(vpc, run_lengths, VP_Z_AXIS, z, y);
	}
    }
    ReportStatus(vpc, 1.0);
    accuracy = 1. - ((double)badpredictions /
		     (double)(vpc->xlen*vpc->ylen*vpc->zlen));
    printf("VPTestMinMaxOctree: PASSED.\n");
    printf("Prediction accuracy: %.1f%% (%d bad predictions)\n",
	   accuracy*100., badpredictions);

    badpredictions = 0;
    printf("Checking -Y axis runs...\n");
    for (y = vpc->ylen-1; y >= 0; y--) {
	ReportStatus(vpc, (double)(vpc->ylen-1-y) / (double)vpc->ylen);
	Debug((vpc, VPDEBUG_OCTREERUNS, "*** Slice %d ***\n", y));
	VPInitOctreeLevelStack(vpc, level_stack, VP_Y_AXIS, y);
	scans_left = 0;
	for (x = 0; x < vpc->xlen; x++) {
	    if (scans_left == 0) {
		scans_left = VPComputeScanRuns(vpc, level_stack, run_lengths,
					       VP_Y_AXIS, x, vpc->zlen);
	    }
	    scans_left--;
	    badpredictions += VPCheckRuns(vpc, run_lengths, VP_Y_AXIS, y, x);
	}
    }
    ReportStatus(vpc, 1.0);
    accuracy = 1. - ((double)badpredictions /
		     (double)(vpc->xlen*vpc->ylen*vpc->zlen));
    printf("VPTestMinMaxOctree: PASSED.\n");
    printf("Prediction accuracy: %.1f%% (%d bad predictions)\n",
	   accuracy*100., badpredictions);

    badpredictions = 0;
    printf("Checking -X axis runs...\n");
    for (x = vpc->xlen-1; x >= 0; x--) {
	ReportStatus(vpc, (double)(vpc->xlen-1-x) / (double)vpc->xlen);
	Debug((vpc, VPDEBUG_OCTREERUNS, "*** Slice %d ***\n", x));
	VPInitOctreeLevelStack(vpc, level_stack, VP_X_AXIS, x);
	scans_left = 0;
	for (z = 0; z < vpc->zlen; z++) {
	    if (scans_left == 0) {
		scans_left = VPComputeScanRuns(vpc, level_stack, run_lengths,
					       VP_X_AXIS, z, vpc->ylen);
	    }
	    scans_left--;
	    badpredictions += VPCheckRuns(vpc, run_lengths, VP_X_AXIS, x, z);
	}
    }
    ReportStatus(vpc, 1.0);
    accuracy = 1. - ((double)badpredictions /
		     (double)(vpc->xlen*vpc->ylen*vpc->zlen));
    printf("VPTestMinMaxOctree: PASSED.\n");
    printf("Prediction accuracy: %.1f%% (%d bad predictions)\n",
	   accuracy*100., badpredictions);
}
#else

int
VPCheckRuns(vpc, run_lengths, axis, k, j)
vpContext *vpc;
unsigned char *run_lengths;/* run lengths */
int axis;		/* principle viewing axis */
int k;			/* slice number */
int j;			/* scanline number */
{
  return 0 ;  /* RWCox */
}

void
VPTestMinMaxOctree(vpc)
vpContext *vpc;
{
}

#endif /* DEBUG */
