/*plug_threshold.c - AFNI plugin that creates a mask separating brain from
  non-brain, and saves this mask as a fim dataset.
  Copyright (c) 2000 Matthew Belmonte

  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2
  of the License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

  If you find this program useful, please send mail to Matthew Belmonte
  <belmonte@mit.edu>.
*/

/*Most scanners, including GE Signa and Siemens Vision systems, do not use the
  full 16-bit dynamic range.  THRESH_MAX_BRAIN can therefore be decreased to
  about 3000 to save memory, if desired.  It's set to 32766 here rather than to
  32767 since we want to be able to add one to it without causing a signed
  overflow.*/
#define THRESH_MAX_BRAIN 0x7ffe

/*A median-filter length of 9 seems to work for every data set that I've tested.
  If your scanner gives a banded distribution of voxel intensities instead of a
  uniform distribution, you may need to increase this value or apply some
  smoothing to the histogram.*/
#define THRESH_FILTER_LEN 9

#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include <strings.h>
#include <math.h>
#include "afni.h"

#ifndef DARWIN
#include <values.h>
#endif

#ifndef MAXINT
#define MAXINT (1<<30)
#endif

static char help[] =
  "This plugin generates a mask dataset that labels brain with the value 1\n"
  "and non-brain with the value 0.  The algorithm operates in two phases,\n"
  "first finding the best threshold intensity to separate brain from\n"
  "non-brain, then applying that threshold along with information on\n"
  "connected regions to generate the mask.\n\n"

  "The first phase of the algorithm begins by forming an image each of whose\n"
  "voxels is the mean of the time series for the corresponding voxel in the\n"
  "echo-planar dataset.  (The first two acquisitions are excluded from the\n"
  "computation of these means, in order to allow the tissue to reach a\n"
  "magnetic steady state.)  The program constructs a histogram of these\n"
  "means, and then computes the centroid of the log-transformed histogram.\n"
  "This centroid value is assumed to lie somewhere in the interval between\n"
  "the non-brain peak to the left and the brain peak to the right.  (This\n"
  "assumption holds as long as the field of view is appropriate for the size\n"
  "of the subject's head -- i.e., as long as the air peak doesn't dominate\n"
  "the log-transformed histogram.)  The maximum of the median-filtered\n"
  "histogram in the interval to the right of the centroid value is the brain\n"
  "peak.  The maximum to the left of the centroid value is the air peak.\n"
  "The minimum between these two peaks is the threshold between air, muscle,\n"
  "and bone to the left and brain to the right.\n\n"

  "The second phase of the algorithm region-grows from a corner of the image,\n"
  "stopping at voxels whose intensities exceed the threshold that was\n"
  "computed in the previous phase.  The set of voxels not touched by this\n"
  "region-growing step necessarily includes the brain, although it generally\n"
  "also includes other high-intensity structures such as the eyes and perhaps\n"
  "some smaller clumps of tissue elsewhere in the head.  To get rid of these\n"
  "smaller, non-brain islets, we region-grow from each untouched voxel, and\n"
  "identify the contiguous region whose volume is greatest.  This region is\n"
  "labelled as brain, and everything outside it is labelled as non-brain.\n\n"

"AUTHOR\n\n"

  "This plugin was written by Matthew Belmonte <belmonte@mit.edu> of the\n"
  "MIT Student Information Processing Board, supported by a grant from the\n"
  "National Alliance for Autism Research.\n\n"

"VERSION\n\n"

  "1.1   (14 June 2001)\n\n"

"SEE ALSO\n\n"

  "Draw Dataset Plugin",

	    hint[] = "mask out non-brain",
	    input_label[] = "Input",
	    output_label[] = "Output";

/*encoding and decoding functions for mapping a triple to a single integer*/
#define coord_encode(x, y, z, xdim, ydim) ((x) + (xdim)*((y) + (ydim)*(z)))

static void coord_decode(coords, x, y, z, xdim, ydim)
int coords, *x, *y, *z, xdim, ydim;
  {
  *x = coords % xdim;
  coords /= xdim;
  *y = coords % ydim;
  *z = coords / ydim;
  }

/**********************************************************************
 * MEDIAN FILTERING ROUTINES					      *
 * Adapted from median.c in the Gnuroscan package by Matthew Belmonte *
 **********************************************************************/

typedef struct _node {
  int value;
  int ref_count, subtree_ref_count;
  struct _node *parent, *left_child, *right_child;
  } node;
#define NULLTREE ((node *)0)

typedef struct {
  node *tree;
  int head, tail;
  node *(queue[1+THRESH_FILTER_LEN]);
  } btree;

/*Allocate a node whose fields contain the given values.*/
static node *create_node(v, rc, src, p, l, r)
int v;
int rc, src;
node *p, *l, *r;
  {
  register node *n;
  n = (node *)malloc(sizeof(node));
  n->value = v;
  n->ref_count = rc;
  n->subtree_ref_count = src;
  n->parent = p;
  n->left_child = l;
  n->right_child = r;
  return(n);
  }

/*Deallocate a node.*/
static void destroy_node(n)
node *n;
  {
  free(n);
  }

/*Deallocate all the nodes of the tree rooted at the given node 'n'.*/
static void destroy_tree(n)
node *n;
  {
  register node *temp;
  while(n != NULLTREE)
    {
    destroy_tree(n->left_child);
    temp = n;
    n = n->right_child;
    destroy_node(temp);
    }
  }

/*Prune node 'n' from the tree rooted at 't->tree', rearranging the tree as
  necessary in order to preserve the descendants of 'n'.*/
static void prune(t, n)
btree *t;
node *n;
  {
  register node **prune_site, *largest;
  register int ref_count_of_largest;
  prune_site = (n->parent==NULLTREE? &(t->tree): n==n->parent->left_child? &(n->parent->left_child): &(n->parent->right_child));
  if(n->left_child == NULLTREE)
    {
    *prune_site = n->right_child;
    if(*prune_site != NULLTREE)
      (*prune_site)->parent = n->parent;
    destroy_node(n);
    }
  else if(n->right_child == NULLTREE)
    {
    *prune_site = n->left_child;
    if(*prune_site != NULLTREE)
      (*prune_site)->parent = n->parent;
    destroy_node(n);
    }
  else
    {
  /*find the largest value in the left subtree of 'n'*/
    for(largest = n->left_child; largest->right_child != NULLTREE; largest = largest->right_child)
      ;
  /*adjust reference counts to reflect the pruning of this largest value*/
    ref_count_of_largest = largest->ref_count;
    for(largest = n->left_child; largest->right_child != NULLTREE; largest = largest->right_child)
      largest->subtree_ref_count -= ref_count_of_largest;
  /*prune the largest value by replacing it with its left subtree*/
    if(largest==largest->parent->left_child)
      {
      largest->parent->left_child = largest->left_child;
      if(largest->parent->left_child != NULLTREE)
	largest->parent->left_child->parent = largest->parent;
      }
    else
      {
      largest->parent->right_child = largest->left_child;
      if(largest->parent->right_child != NULLTREE)
	largest->parent->right_child->parent = largest->parent;
      }
  /*substitute this largest-valued node for node 'n'*/
    if(n->parent == NULLTREE)
      t->tree = largest;
    else if(n == n->parent->left_child)
      n->parent->left_child = largest;
    else
      n->parent->right_child = largest;
    largest->parent = n->parent;
    largest->left_child = n->left_child;
    largest->right_child = n->right_child;
    if(largest->left_child != NULLTREE)
      largest->left_child->parent = largest;
    if(largest->right_child != NULLTREE)
      largest->right_child->parent = largest;
    largest->subtree_ref_count = largest->ref_count + (largest->left_child==NULLTREE? 0: largest->left_child->subtree_ref_count) + (largest->right_child==NULLTREE? 0: largest->right_child->subtree_ref_count);
    destroy_node(n);
    }
  }

/*Delete from the given btree the node at the head of the associated queue.*/
static void delete_oldest(t)
btree *t;
  {
  register node *n;
  if((t->tail+1)%(THRESH_FILTER_LEN+1) == t->head)
    {
    fprintf(stderr, "delete_oldest: queue is empty!\n");
    return;
    }
  for(n = t->queue[t->head]->parent; n != NULLTREE; n = n->parent)
    n->subtree_ref_count--;
  n = t->queue[t->head];
  t->head = (t->head+1)%(THRESH_FILTER_LEN+1);
  if(n->ref_count == 1)
    prune(t, n);
  else
    {
    n->ref_count--;
    n->subtree_ref_count--;
    }
  }

/*Return the median of all the values in the given btree.  Do not alter the
  btree.*/
static int extract_median(t)
btree *t;
  {
  register node *n;
  register int left_count, right_count, left_size, right_size, middle_position;
  if((t->tail+1)%(THRESH_FILTER_LEN+1) == t->head)
    {
    fprintf(stderr, "extract_median: queue is empty!\n");
    return(0);
    }
  n = t->tree;
  middle_position = n->subtree_ref_count/2+1;
  left_count = right_count = 0;
  left_size = (n->left_child==NULLTREE? 0: n->left_child->subtree_ref_count);
  right_size = (n->right_child==NULLTREE? 0: n->right_child->subtree_ref_count);
  while(abs(left_count+left_size - (right_count+right_size)) > n->ref_count)
  /*inv: 'left_count' is the number of values less than the median that have
    been excluded during traversal of the path from 't->tree' to 'n', and
    'left_size' is the size of the left subtree of the current node 'n'.
    'right_count' and 'right_size' are similar.*/
    {
    if(left_count+left_size+n->ref_count >= middle_position)
      {
      right_count += n->ref_count+right_size;
      n = n->left_child;
      }
    else
      {
      left_count += n->ref_count+left_size;
      n = n->right_child;
      }
    left_size = (n->left_child==NULLTREE? 0: n->left_child->subtree_ref_count);
    right_size = (n->right_child==NULLTREE? 0: n->right_child->subtree_ref_count);
    }
  return(n->value);
  }

/*Insert the given value into the given btree and place it at the tail of the
  associated queue.*/
static void insert_newest(v, t)
int v;
btree *t;
  {
  register node *n, *p;
  if((t->tail+2)%(THRESH_FILTER_LEN+1) == t->head)
    {
    fprintf(stderr, "insert_newest: queue is full; deleting oldest to make room\n");
    delete_oldest(t);
    }
  t->tail = (t->tail+1)%(THRESH_FILTER_LEN+1);
  p = NULLTREE;
  n = t->tree;
  while((n != NULLTREE) && (n->value != v))
  /*inv: 'p' is the parent of 'n'.  All 'subtree_ref_count' fields on the path
    from 't->tree' to 'p' have been incremented.  The proper location for the
    new value 'v' lies somewhere in the subtree rooted at 'n'.*/
    {
    n->subtree_ref_count++;
    p = n;
    n = (v<n->value? n->left_child: n->right_child);
    }
  if(n == NULLTREE)
    {
    register node **graft_site;
    graft_site = (p==NULLTREE? &(t->tree):
      v<p->value? &(p->left_child): &(p->right_child)
      );
    *graft_site = create_node(v, 1, 1, p, NULLTREE, NULLTREE);
    t->queue[t->tail] = *graft_site;
    }
  else
    {
    n->ref_count++;
    n->subtree_ref_count++;
    t->queue[t->tail] = n;
    }
  }

/***************************
 * REGION-GROWING ROUTINES *
 ***************************/

#define UNVISITED 0	/*not yet evaluated or queued*/
#define INCLUDED 1	/*included in target region*/
#define EXCLUDED 2	/*excluded from target region*/
#define QUEUED 3	/*queued for evaluation*/
/*Find the set of voxels in img whose intensities are less than the given
  threshold and that are contiguous with the given starting voxel (x,y,z).
  Label the corresponding voxels in mask_img with the value INCLUDED+region_num.
  (mask_img should have been initialised to UNVISITED by the calling routine.
  With UNVISITED defined as 0, calloc() or bzero() suffices to accomplish this
  initialisation.  Multiple calls can be made without intervening
  re-initialisations only in the case in which the regions being labelled are
  unconnected.)  This routine returns the size in voxels of the labelled
  region.*/
static int THRESH_region_grow(img, mask_img, stack, region_num, x, y, z, xdim, ydim, zdim, threshold)
short	*img,		/*source image*/
	*mask_img;	/*same dimensions as img, pre-initialised to UNVISITED*/
int	*stack,		/*a block of xdim*ydim*zdim ints*/
	region_num,	/*unique number with which to label this region*/
	x, y, z,	/*starting location of this region*/
	xdim, ydim, zdim; /*dimensions*/
short	threshold;	/*threshold below which pixels are included*/
  {
  register int dx, dy, dz;	/*increments to current voxel index*/
  register int	*sp,		/*stack of voxels scheduled for visiting*/
		*sp2,		/*stack of excluded voxels to unlabel*/
		region_size;	/*count of included voxels*/
  region_size = 0;
  sp = stack;
  sp2 = stack+xdim*ydim*zdim;
  *sp = coord_encode(x, y, z, xdim, ydim);
  mask_img[*(sp++)] = QUEUED;
/*inv: the stack contains all reachable points that still need to be visited*/
  while(sp != stack)
    if(mask_img[*(--sp)] == QUEUED)
      {
      if(img[*sp] < threshold)
	{
	mask_img[*sp] = INCLUDED+region_num;
	region_size++;
	coord_decode(*sp, &x, &y, &z, xdim, ydim);
	for(dz = -1; dz <= 1; dz++)
	  for(dy = -1; dy <= 1; dy++)
	    for(dx = -1; dx <= 1; dx++)
	      if((x+dx >= 0) && (x+dx < xdim) && (y+dy >= 0) && (y+dy < ydim) && (z+dz >= 0) && (z+dz < zdim) && (dx || dy || dz))
		{
		*sp = coord_encode(x+dx, y+dy, z+dz, xdim, ydim);
		if(mask_img[*sp] == UNVISITED)
		  mask_img[*(sp++)] = QUEUED;
		}
	}
      else
	{
	mask_img[*sp] = EXCLUDED;
	*(--sp2) = *sp;
	}
      }
/*remove all EXCLUDED labels and return these voxels to the UNVISITED state*/
  while(sp2 != stack+xdim*ydim*zdim)
    mask_img[*(sp2++)] = UNVISITED;
/*the visited areas of mask_img now consist entirely of UNVISITED or INCLUDED*/
  return(region_size);
  }

/*On entry, img is the averaged echo-planar image.  On exit, each voxel in img
  has been replaced by a 1 if it's been identified as a brain voxel, or by a 0
  otherwise.  Returns 0 if OK, 1 if out of memory.*/
static int THRESH_mask_brain(img, xdim, ydim, zdim, threshold)
short *img;
int xdim, ydim, zdim;
short threshold;
  {
  register int x, y, z, region;
  int region_size, max_region_size, max_region;
  short *mask_img;
  int *stack;
  mask_img = calloc(xdim*ydim*zdim, sizeof(*mask_img));
  if(mask_img == (short *)0)
    return 1;
  stack = (int *)calloc(xdim*ydim*zdim, sizeof(*stack));
  if(stack == (int *)0)
    {
    free(mask_img);
    return 1;
    }
/*make all non-brain regions in mask_img INCLUDED, and leave all brain regions
  UNVISITED*/
  THRESH_region_grow(img, mask_img, stack, 0, 0, 0, 0, xdim, ydim, zdim, threshold);
  bzero((char *)img, xdim*ydim*zdim*sizeof(*img)); /*fill img with UNVISITED*/
/*now label all the volumes that were not INCLUDED in mask_img*/
  region = 1;
  max_region_size = 0;
  for(z = 0; z != zdim; z++)
    for(y = 0; y != ydim; y++)
      for(x = 0; x != xdim; x++)
	if(img[coord_encode(x, y, z, xdim, ydim)] == UNVISITED)
	  {
	  region_size = THRESH_region_grow(mask_img, img, stack, region, x, y, z, xdim, ydim, zdim, INCLUDED);
	  if(region_size > max_region_size)
	    {
	    max_region_size = region_size;
	    max_region = region;
	    }
	  region++;
	  }
/*zero all but the largest region, which is assumed to be the brain*/
  for(z = 0; z != xdim*ydim*zdim; z++)
    img[z] = (img[z] == INCLUDED+max_region);
  free(stack);
  free(mask_img);
  return 0;
  }

/*****************
 * MAIN ROUTINES *
 *****************/

/*Scan from the right-hand end of the histogram until the median-filtered value
  of the histogram definitely exceeds zero (right-hand edge of the brain peak).
  Then continue scanning left, keeping track of the filtered histogram's
  minimum value, till its current value becomes very large (right-hand edge of
  the air peak).  The position of this minimum is the intensity threshold for
  distinguishing brain from non-brain.*/
static short *THRESH_compute(dset, verbose)
THD_3dim_dataset *dset;
int verbose;
  {
  register int t, x, y, z;
  int xdim, ydim, zdim, nvox, tdim, histo_min, histo_max, centroid;
  double centroid_num, centroid_denom, ln;
  long sum;
  short *img;
  short air_peak, brain_peak, cutoff;
  btree filter;
  int histogram[1+THRESH_MAX_BRAIN];
  xdim = dset->daxes->nxx;
  ydim = dset->daxes->nyy;
  zdim = dset->daxes->nzz;
  nvox = xdim*ydim*zdim;
  tdim = DSET_NUM_TIMES(dset);
  bzero((char *)histogram, (1+THRESH_MAX_BRAIN)*sizeof(*histogram));
  img = (short *)calloc(nvox, sizeof(short));
  if(img == (short *)0)
    return((short *)0);
  /*Form an image each of whose voxels is the mean of the time series for the
    corresponding voxel in the original image.  Construct a histogram of these
    mean intensities.*/
  for(z = 0; z != zdim; z++)
    for(y = 0; y != ydim; y++)
      for(x = 0; x != xdim; x++)
	{
	sum = 0L;
	for (t = 2; t < tdim; t++) /*t=2 to stabilise transverse magnetisation*/
	  sum += ((short *)DSET_ARRAY(dset, t))[x + xdim*(y + ydim*z)];
	sum = (sum+(tdim-2)/2)/(tdim-2);
	img[coord_encode(x, y, z, xdim, ydim)] = (short)sum;
	if((sum >= 0) && (sum <= THRESH_MAX_BRAIN))
	  histogram[sum]++;
	}
  centroid_num = centroid_denom = 0.0;
  for(x = THRESH_MAX_BRAIN; x != 0; x--)
    {
    ln = log((double)(1+histogram[x]));
    centroid_num += x*ln;
    centroid_denom += ln;
    }
  centroid = (int)(centroid_num/centroid_denom);

  filter.tree = NULLTREE;
  filter.head = 0;
  filter.tail = -1;
  x = THRESH_MAX_BRAIN;
  while(x > THRESH_MAX_BRAIN-THRESH_FILTER_LEN)
    insert_newest(histogram[--x], &filter);
  histo_max = -1;
  histo_min = MAXINT;
  cutoff = brain_peak = THRESH_MAX_BRAIN;
  /*inv: filter contains histogram[x..x+THRESH_FILTER_LEN-1], histo_max is the
    maximum median-filtered value of histogram[x+1+THRESH_FILTER_LEN/2..
    THRESH_MAX_BRAIN-(THRESH_FILTER_LEN+1)/2, brain_peak is the index at which
    histo_max occurs, histo_min is the minimum median-filtered value of
    histogram[x+1+THRESH_FILTER_LEN/2..brain_peak], and cutoff is the index at
    which histo_min occurs.*/
  while(x > centroid-THRESH_FILTER_LEN/2)
    {
    y = extract_median(&filter);
    if(y > histo_max)
      {
      cutoff = brain_peak = x+THRESH_FILTER_LEN/2;
      histo_min = histo_max = y;
      }
    else if(y < histo_min)
      {
      cutoff = x+THRESH_FILTER_LEN/2;
      histo_min = y;
      }
    delete_oldest(&filter);
    insert_newest(histogram[--x], &filter);
    }
  /*inv: filter contains histogram[x..x+THRESH_FILTER_LEN-1], histo_min is the
    minimum median-filtered value of histogram[x+1+THRESH_FILTER_LEN/2..
    brain_peak], and cutoff is the index at which histo_min occurs.*/
  while((x >= 0) && ((y = extract_median(&filter)) <= brain_peak))
    {
    if(y < histo_min)
      {
      histo_min = y;
      cutoff = x+THRESH_FILTER_LEN/2;
      }
    delete_oldest(&filter);
    insert_newest(histogram[--x], &filter);
    }
  for(z = cutoff-THRESH_FILTER_LEN/2, y = z+THRESH_FILTER_LEN; z < y; z++)
    if(histogram[z] < histo_min)
      {
      histo_min = histogram[z];
      cutoff = z;
      }
  if(verbose)
    printf("centroid %d, brain peak %d, air peak edge %d, threshold %d\n", centroid, brain_peak, x+THRESH_FILTER_LEN/2, cutoff);
  destroy_tree(filter.tree);
  free(histogram);
  /*region-grow from the edge of the image inward, filling with zeroes*/
  if(THRESH_mask_brain(img, xdim, ydim, zdim, cutoff))
    {
    free(img);
    return((short *)0);
    }
  return(img);
  }

char *THRESH_main(plint)
PLUGIN_interface *plint;
  {
  int t;
  char *prefix;
  THD_3dim_dataset *dset, *mask;
  short *mask_img;
  if(plint == (PLUGIN_interface *)0)
    return "THRESH_main: null input";

/*Make sure that the input dataset exists and is stored in a format that we can
  understand (MRI_short)*/
  PLUTO_next_option(plint);
  dset = PLUTO_find_dset(PLUTO_get_idcode(plint));
  if(dset == (THD_3dim_dataset *)0)
    return "bad dataset";
  for(t = 0; t != dset->dblk->nvals; t++)
    if(DSET_BRICK_TYPE(dset, t) != MRI_short)
      return("thresholding on non-short values is not implemented");

/*Make sure that the prefix specified for the new dataset is a valid prefix*/
  PLUTO_next_option(plint);
  prefix = PLUTO_get_string(plint);
  if(!PLUTO_prefix_ok(prefix))
    return("bad prefix");

/*Make sure source dataset is in memory*/
  DSET_load(dset);
  mask_img = THRESH_compute(dset, 1);
  if(mask_img == (short *)0)
    return("out of memory");

/*create the output dataset*/
  mask = EDIT_empty_copy(dset);
  if(EDIT_dset_items(mask,
	ADN_prefix, prefix,
	ADN_malloc_type, DATABLOCK_MEM_MALLOC, /*hold in r/w memory*/
	ADN_datum_all, MRI_short,	/*store as (scaled) short ints*/
	ADN_nvals, 1,			/*single sub-brick*/
	ADN_ntt, 0,			/*no time dimension*/
	ADN_type, ISHEAD(dset)?
	  HEAD_FUNC_TYPE: GEN_FUNC_TYPE, /*functional image*/
	ADN_func_type, FUNC_FIM_TYPE,	/*intensity + z-score*/
	ADN_none))
    return("EDIT_dset_items error");
  EDIT_BRICK_LABEL(mask, 0, "Mask");
  mri_fix_data_pointer(mask_img, DSET_BRICK(mask, 0));
  EDIT_BRICK_FACTOR(mask, 0, 0.0);	/*no scaling*/
  DSET_write(mask);
  PLUTO_add_dset(plint, mask, DSET_ACTION_NONE);
  return (char *)0;
  }

PLUGIN_interface *PLUGIN_init(ncall)
int ncall;
  {
  PLUGIN_interface *plint;
  if(ncall > 0)
    return (PLUGIN_interface *)0;	/*only one interface*/
/*set titles and entry point*/
  plint = PLUTO_new_interface("Threshold", hint, help, PLUGIN_CALL_VIA_MENU, THRESH_main);
  PLUTO_add_hint(plint, hint);
/*first line of dialogue box: input dataset*/
  PLUTO_add_option(plint, input_label, input_label, TRUE);
  PLUTO_add_dataset(plint, "Dataset", ANAT_SPGR_MASK | ANAT_EPI_MASK, 0, DIMEN_4D_MASK | BRICK_SHORT_MASK);
/*second line of dialogue box: output dataset*/
  PLUTO_add_option(plint, output_label, output_label, TRUE);
  PLUTO_add_string(plint, "Prefix", 0, (char **)0, 19);
  return plint;
  }
