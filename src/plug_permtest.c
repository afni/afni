/*plug_permtest.c - AFNI plugin that applies a permutation test to a 3D+time
  dataset to create a fizt dataset.
  Copyright (c) 2000 - 2005 Matthew Belmonte

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

#define NUM_ITERS 10000
#define NUM_COORDS 2

#define BLAST 33333.0
#include <sys/types.h>
#include <sys/param.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "afni.h"

/*a macro that wraps THD_pval_to_stat() (which in turn evaluates to a call to
  the one-tailed probability-to-z-score function normal_p2t()) so that it
  behaves like Gary Perlman's two-tailed zcrit() function in Netlib*/
#define critz(p) \
   (((p) >= 0.5)? THD_pval_to_stat(2.0*(1.0-(p)), FUNC_ZT_TYPE, (float *)0) \
		: -THD_pval_to_stat(2.0*(p), FUNC_ZT_TYPE, (float *)0))

static char help[] =
  "This plugin implements a permutation test, a nonparametric statistical\n"
  "method that avoids the pitfall of over-correcting for multiple comparisons\n"
  "since it implicitly takes into account spatial correlations in the data.\n"
  "If you use this software, please take a moment to send mail to the author,\n"
  "belmonte@mit.edu, and cite the following paper in your report:\n\n"

  "Matthew K Belmonte and Deborah A Yurgelun-Todd, `Permutation Testing Made\n"
  "Practical for Functional Magnetic Resonance Image Analysis',\n"
  "IEEE Transactions on Medical Imaging 20(3):243-248 (March 2001).\n\n"

"USAGE\n\n"

  "`Input' is the echo-planar dataset for which the permutation test is to be\n"
  "computed.\n\n"

  "`Ideal' is the ideal time series against which the echo-planar data are to\n"
  "be correlated.\n\n"

  "`Ort', an optional parameter, is a time series to which the echo-planar\n"
  "data are to be orthogonalised.  This feature is useful when one wants to\n"
  "analyse responses to two superposed conditions independently.  (Note that\n"
  "the data are always detrended, i.e. orthogonalised to a linear series --\n"
  "this optional orthogonalisation is in addition to the detrending step.)\n\n"

  "`Output' is the output dataset.  The activation data saved in this dataset\n"
  "consist of a sub-brick of regression coefficients and a sub-brick of\n"
  "probability values.  As AFNI provides no standard format for storing\n"
  "probabilities, these are mapped to z-scores and the entire dataset is\n"
  "saved as a FIZT dataset.\n\n"

  "`alpha level' is the tail probability at which to stop marking voxels as\n"
  "activated.  (Note that the slider in the `Define Overlay' panel will\n"
  "have no effect below this value.)  You should avoid setting this value\n"
  "very high, lest the Phase 3 algorithm run out of substitute points in too\n"
  "many cases (for an explanation of substitutes, see the article cited above).\n\n"

  "`AR order', an optional parameter, is the order (i.e. the number of\n"
  "samples of look-back) of the autoregressive model used to remove noise\n"
  "colouring from the fMRI time series before permuting it, and to restore\n"
  "this colouring after permutation.  At field strengths (B0) of 3 Tesla or\n"
  "more, or repetition times (TR) of 2 seconds or less, autocorrelation can\n"
  "be non-negligible and you therefore should select this option.  In most\n"
  "cases an autoregressive order of 1 should suffice, though you may want to\n"
  "try going up to 3.  You can experiment with greater orders by examining\n"
  "the automatically generated files DSET_autocorrelation.1D and\n"
  "DSET_autoregression.1D, where DSET is the name of the 3D+time dataset to\n"
  "which you've just applied the permutation test; if the Nth autocorrelation\n"
  "coefficient is near zero then you can safely restrict the autoregressive\n"
  "model to order N-1.  (In extreme cases the program will detect this\n"
  "condition and decrease the model order automatically.)\n"
	"\tAutoregressive modelling was suggested early on as a method of\n"
  "correcting for coloured noise as a preparatory step to permutation testing\n"
  "of fMRI time series [1].  Although methods based around permuting in a\n"
  "wavelet domain leave residuals that are more temporally independent in\n"
  "comparison to those left by autoregressive methods [2], it's uclear how\n"
  "much of this residual temporal dependence arises from coloured noise\n"
  "versus how much may reflect the presence of (subthreshold) signal.\n"
  "Recently it's been shown that wavelet methods, like Fourier methods, may\n"
  "tend to underestimate thresholds for statistical significance (and\n"
  "therefore risk producing false positive results) when the permuted series\n"
  "include frequency-localised BOLD signal as well as noise [3] -- a\n"
  "condition that applies especially in block designs.  For these reasons as\n"
  "as well as for simplicity of implementation, I've chosen to implement here\n"
  "autoregressive rather than wavelet whitening.\n"
  "[1] JJ Locascio, PJ Jennings, CI Moore, S Corkin, `Time Series Analysis in\n"
  "    the Time Domain and Resampling Methods for Studies of Functional\n"
  "    Magnetic Resonance Brain Imaging', Human Brain Mapping 5(3):168-193\n"
  "    (1997).\n"
  "[2] ET Bullmore, C Long, J Suckling, J Fadili, G Calvert, F Zelaya,\n"
  "    TA Carpenter, MJ Brammer, `Colored Noise and Computational Inference\n"
  "    in Neurophysiological (fMRI) Time Series Analysis: Resampling Methods\n"
  "    in Time and Wavelet Domains', Human Brain Mapping 12(2):61-78\n"
  "    (February 2001).\n"
  "[3] O Friman, C-F Westin, `Resampling fMRI Time Series', NeuroImage\n"
  "    25(3):859-867 (15 April 2005).\n\n"

  "`AR exclude', an optional parameter which has effect only when `AR order'\n"
  "has been specified, is a vector of flags in which non-zero values specify\n"
  "that the corresponding elements of the fMRI time series are to be excluded\n"
  "from the autocorrelation computation.  If this parameter is not specified,\n"
  "all time points are included.  Normally you won't need to specify this\n"
  "parameter, although it may be useful in some block designs in which the\n"
  "noise structure depends on the experimental condition.\n\n"

  "`two-tailed', `one-tailed positive', and `one-tailed negative' select the\n"
  "tail(s) of interest.  Exactly one of these options must be selected.\n\n"

  "`Mask', an optional parameter, is a FIM dataset whose three spatial\n"
  "dimensions match those of the echo-planar dataset.  Only those\n"
  "coordinates whose mask values are between 1 and the largest positive\n"
  "short integer are included in the permutation test.  These inclusion\n"
  "limits can be overridden using the optional `least mask value' and\n"
  "`greatest mask value' parameters.  Mask datasets can be generated\n"
  "automatically using the Threshold plugin (q.v.), or using 3dAutomask.\n\n"

"AUTHOR\n\n"

  "This plugin was written by Matthew Belmonte <belmonte@mit.edu> of the\n"
  "MIT Student Information Processing Board, supported by a grant from the\n"
  "National Alliance for Autism Research.\n\n"

"REVISION HISTORY\n\n"

  "2.0  7 November 2005   added autoregressive correction for coloured noise\n"
  "1.3  20 January 2005   added support for 3dAutomask and other byte-valued masks\n"
  "1.2  19 December 2002  fixed a dtree bug that caused a rare crash in phase 3\n"
  "1.1  14 June 2001      cosmetic changes only\n"
  "1.0  4 January 2001    initial release\n\n"

"SEE ALSO\n\n"

  "Threshold Plugin  or  3dAutomask program\n"
  "Draw Dataset Plugin\n"
  "(These normally are applied before the permutation test.)",

	    hint[] = "compute FIM with permutation test",
	    input_label[] = "Input",
	    ts_label[] = "Ideal",
	    ort_label[] = "Ort",
	    output_label[] = "Output",
	    alpha_label[] = "alpha level (0,1]",
	    AR_order_label[] = "AR order",
	    AR_excl_label[] = "AR exclude",
	    tails2[] = "two-tailed",
	    tails1pos[] = "one-tailed positive",
	    tails1neg[] = "one-tailed negative",
	    mask_label[] = "Mask",
	    masklo_label[] = "least mask value",
	    maskhi_label[] = "greatest mask value";

/*data structures for trees*/

/*singly-indexed tree: ordered on correlations*/
typedef struct _snode {
	double corr;			/*correlation (key)*/
	int coords;			/*coordinates: x+xdim*(y+ydim*z) */
	struct _snode *left, *right;	/*children*/
	} SNODE;

typedef struct {
	SNODE *mem,			/*base address of tree node storage*/
	      *next,			/*next free tree storage address*/
	      *root;			/*==tree_mem if tree is non-null*/
	int xdim, ydim;			/*maximum coordinate values*/
	} STREE;

#define stree_null(t) ((t)->root == (SNODE *)0)

typedef struct {			/*pairs (r, (x,y,z))*/
	double corr;
	int coords;
	} CLIST;

/*doubly-indexed tree: ordered on correlations and on coordinates*/
typedef struct _dnode {
	CLIST *data;			/*correlations and coordinates*/
	int dlen;			/*# of valid entries in data[]*/
	int size;			/*size of this correlation subtree*/
	struct _dnode *corr_lchild,	/*children in correlation tree*/
		      *corr_rchild,
		      *corr_parent,	/*parent in correlation tree*/
		      *coord_lchild,	/*children in coordinate tree*/
		      *coord_rchild;
	} DNODE;

typedef struct {
	DNODE *mem,			/*base address of tree node storage*/
	      *next,			/*next free tree storage address*/
	      *corr_root, *coord_root;	/*==mem if tree is non-null*/
	int xdim, ydim;			/*maximum coordinate values*/
	} DTREE;

#define dtree_size(t) (((t)->corr_root == (DNODE *)0)? 0: ((t)->corr_root->size))

/*encoding and decoding functions for mapping a triple to a single integer*/
#define coord_encode(x, y, z, xdim, ydim) ((x) + (xdim)*((y) + (ydim)*(z)))

static void coord_decode(int coords, int *x, int *y, int *z, int xdim, int ydim)
  {
  *x = coords % xdim;
  coords /= xdim;
  *y = coords % ydim;
  *z = coords / ydim;
  }

#define sign(x) (((x)<0)? -1: 1)

#ifdef PERMTEST_DEBUG

/*flush output buffers on SIGUSR1 - useful for batch debugging*/
#include <signal.h>
static void flush(int sig)
  {
  fflush(stdout);
  fflush(stderr);
  signal(SIGUSR1, flush);
  }

/*slave routine to stree_traverse()*/
void rcsv_stree_traverse(SNODE *t, int indent)
  {
  register int i;
  while(t)
    {
    for(i = 0; i != indent; i++)
      putchar(' ');
    indent += 2;
    printf("%lx: corr=%f coords=%d left=%lx right=%lx\n", (long)t, t->corr, t->coords, (long)(t->left), (long)(t->right));
    rcsv_stree_traverse(t->left, indent);
    t = t->right;
    }
  }

/*Print a traversal of the given stree.*/
void stree_traverse(STREE *t)
  {
  printf("t=%lx\n  mem=%lx  next=%lx = mem+%ld  root=%lx xdim=%d ydim=%d\n",
	(long)t,
	(long)(t->mem),
	(long)(t->next),
	(((long)(t->next))-(long)(t->mem))/sizeof(SNODE),
	(long)(t->root),
	t->xdim, t->ydim);
  rcsv_stree_traverse(t->root, 0);
  putchar('\n');
  }

/*slave routine to dtree_traverse()*/
void rcsv_dtree_traverse(DNODE *t, int indent, int order)
  {
  register int i;
  while(t)
    {
    for(i = 0; i != indent; i++)
      putchar(' ');
    indent += 2;
    printf("%lx: data=", (long)t);
    for(i = 0; i != t->dlen; i++)
      printf("%f %d ", t->data[i].corr, t->data[i].coords);
    printf("size=%d corr_lchild=%lx corr_rchild=%lx corr_parent=%lx coord_lchild=%lx coord_rchild=%lx\n", t->size, (long)(t->corr_lchild), (long)(t->corr_rchild), (long)(t->corr_parent), (long)(t->coord_lchild), (long)(t->coord_rchild));
    if(order)
      {
      rcsv_dtree_traverse(t->coord_lchild, indent, order);
      t = t->coord_rchild;
      }
    else
      {
      rcsv_dtree_traverse(t->corr_lchild, indent);
      t = t->corr_rchild;
      }
    }
  }

/*Print a traversal of a dtree.  Use corr links if order=0, coord links if
  order=1.*/
void dtree_traverse(DTREE *t, int order)
  {
  printf("t=%lx\n  mem=%lx  next=%lx = mem+%ld  coord_root=%lx corr_root=%lx xdim=%d ydim=%d\n",
	(long)t,
	(long)(t->mem),
	(long)(t->next),
	(((long)(t->next))-(long)(t->mem))/sizeof(DNODE),
	(long)(t->coord_root),
	(long)(t->corr_root),
	t->xdim, t->ydim);
  rcsv_dtree_traverse((order? t->coord_root: t->corr_root), 0, order);
  putchar('\n');
  }

/*slave routine to dtree_check_sizes()*/
int rcsv_dtree_check_sizes(DNODE *t)
  {
  int s;
  if(t)
    {
    s = 1+rcsv_dtree_check_sizes(t->corr_lchild)+rcsv_dtree_check_sizes(t->corr_rchild);
    if(s != t->size)
      {
      printf("%lx->size = %d should be %d\n", (long)t, t->size, s);
      fflush(stdout);
      }
    return(t->size);
    }
  return 0;
  }

/*Check that the size fields of a dtree are consistent with each other and with
  the structure of the tree.*/
int dtree_check_sizes(DTREE *t)
  {
  return(rcsv_dtree_check_sizes(t->corr_root));
  }

/*slave routine to dtree_circtest()*/
void rcsv_dtree_circtest(DNODE *t, DNODE *mem, char *mark, int n, int order)
  {
  register int m;
  while(t)
    {
    m = (int)((((long)t)-(long)mem)/(((long)(mem+1))-((long)mem)));
    if((m >= n) || (m < 0))
      printf("out-of-bounds reference %lx (mem+%d)\n", (long)t, m);
    else if(mark[m])
      printf("duplicate reference %lx (mem+%d)\n", (long)t, m);
    else
      {
      mark[m] = 1;
      if(order)
	{
	rcsv_dtree_circtest(t->coord_lchild, mem, mark, n, order);
	t = t->coord_rchild;
	}
      else
	{
	rcsv_dtree_circtest(t->corr_lchild, mem, mark, n, order);
	t = t->corr_rchild;
	}
      }
    }
  }

/*Test the given dtree for circular or out-of-bounds references.  Use corr links
  if order=0, coord links if order=1.*/
void dtree_circtest(DTREE *t, int order)
  {
  char *mark;
  int n;
  n = (int)((((long)(t->next))-(long)(t->mem))/(((long)(t->mem+1))-((long)(t->mem))));
  mark = calloc(n, 1);
  if(mark)
    {
    rcsv_dtree_circtest((order? t->coord_root: t->corr_root), t->mem, mark, n, order);
    free(mark);
    }
  else
    fprintf(stderr, "dtree_circtest: couldn't calloc mark\n");
  }

/*slave routine to dtree_check_parent_pointers()*/
int rcsv_dtree_check_parent_pointers(DNODE *t)
  {
  int bad;
  bad = 0;
  while(t != (DNODE *)0)
    {
    if(t->corr_lchild)
      {
      if(t->corr_lchild->corr_parent != t)
	{
	printf("%lx->corr_lchild = %lx, but %lx->corr_parent = %lx\n",
	  (long)t, (long)(t->corr_lchild), (long)(t->corr_lchild),
	  (long)(t->corr_lchild->corr_parent));
	bad = 1;
	}
      if(rcsv_dtree_check_parent_pointers(t->corr_lchild)) bad = 1;
      }
    if((t->corr_rchild) && (t->corr_rchild->corr_parent != t))
      {
      printf("%lx->corr_rchild = %lx, but %lx->corr_parent = %lx\n",
	(long)t, (long)(t->corr_rchild), (long)(t->corr_rchild),
	(long)(t->corr_rchild->corr_parent));
      bad = 1;
      }
    t = t->corr_rchild;
    }
  return bad;
  }

/*Verify that the corr_parent fields inside t are consistent with the
  corr_lchild and corr_rchild fields.*/
int dtree_check_parent_pointers(DTREE *t)
  {
  return(rcsv_dtree_check_parent_pointers(t->corr_root));
  }

/*Print hex dump of an IEEE double-precision floating-point number, MSB first.*/
void fdump(double *x)
  {
  double test = 2.0;
  int i;
  register char *c;
  c = (char *)x;
  if(*(char *)&test)
    for(i = 7; i >= 0; i--)
      printf("%02x", c[i]&0xff);
  else
    for(i = 0; i != 8; i++)
      printf("%02x", c[i]&0xff);
  }

#endif

/*Which metric is used for comparison of correlation values depends on which
  tail(s) of the distribution interest us.  For the two-tailed case, our
  `maximum' should be the most extreme value from either tail, so we use the
  absolute value function fabs() from libm.  For the negative one-tailed case,
  our `maximum' should be the most negative value, so we use the negation
  function neg().  For the positive one-tailed case, our maximum should be the
  most positive value, so we use the identity function id().*/

static double id(double r)
  {
  return(r);
  }

static double neg(double r)
  {
  return(-r);
  }

static double (*magnitude)(double);

/*Allocate storage and initialise a tree that can contain up to nelem nodes.*/
static STREE *stree_create(int xdim, int ydim, int nelem)
  {
  STREE *t;
  t = (STREE *)malloc(sizeof(*t));
  if(t == (STREE *)0)
    return((STREE *)0);
  t->mem = (SNODE *)calloc(nelem, sizeof(*(t->mem)));
  if(t->mem == (SNODE *)0)
    {
    free(t);
    return((STREE *)0);
    }
  t->next = t->mem;
  t->root = (SNODE *)0;
  t->xdim = xdim;
  t->ydim = ydim;
  return(t);
  }

/*Deallocate a tree.*/
static void stree_destroy(STREE *t)
  {
  free(t->mem);
  free(t);
  }

/*Insert the given value into the tree.*/
static void stree_insert(STREE *t, double r, int x, int y, int z)
  {
  register SNODE **p;
  double rmag;
  p = &(t->root);
  rmag = (*magnitude)(r);
/*inv: The path from t->root to p is labelled with values whose magnitudes
  bound or equal that of r.*/
  while(*p != (SNODE *)0)
    p = (rmag < (*magnitude)((*p)->corr))? &((*p)->left): &((*p)->right);
  t->next->corr = r;
  t->next->coords = coord_encode(x, y, z, t->xdim, t->ydim);
  t->next->left = (SNODE *)0;
  t->next->right = (SNODE *)0;
  *p = t->next++;
  }

/*Delete the maximum correlation value from the given tree, and return that
  value and its associated coordinates.*/
static double stree_extract_max(STREE *t, int *x, int *y, int *z)
  {
  register SNODE **p, **pp;
  SNODE *target;
  if(t->root == (SNODE *)0)
    {
    fprintf(stderr, "Error: STREE underflow\n- this happens when every voxel in the brain has been identified as activated\n");
    return(0.0);
    }
  if((t->root->left == (SNODE *)0) && (t->root->right == (SNODE *)0))
    {
    target = t->root;
    t->root = (SNODE *)0;
    t->next = t->mem;
    }
  else
    {
    p = &(t->root);
  /*inv: **pp and **p are successive links in a chain of right children
    beginning at t->root.*/
    do
      {
      pp = p;
      p = &((*p)->right);
      } while((*p) != (SNODE *)0);
    target = *pp;
    *pp = (*pp)->left;
    }
  coord_decode(target->coords, x, y, z, t->xdim, t->ydim);
  return(target->corr);
  }

/*Allocate storage and initialise a dtree that can contain up to nelem nodes.*/
static DTREE *dtree_create(int xdim, int ydim, int nelem)
  {
  DTREE *t;
  t = (DTREE *)malloc(sizeof(*t));
  if(t == (DTREE *)0)
    return((DTREE *)0);
  t->mem = (DNODE *)calloc(nelem, sizeof(*(t->mem)));
  if(t->mem == (DNODE *)0)
    {
    free(t);
    return((DTREE *)0);
    }
  t->next = t->mem;
  t->corr_root = (DNODE *)0;
  t->coord_root = (DNODE *)0;
  t->xdim = xdim;
  t->ydim = ydim;
  return(t);
  }

/*Deallocate a dtree.*/
static void dtree_destroy(DTREE *t)
  {
  free(t->mem);
  free(t);
  }

/*Insert the given value into the dtree, using the given storage location.*/
static void dtree_insert_at_node(DTREE *t, DNODE *node)
  {
  register DNODE **p,			/*pointer into the coord tree*/
		 *q, *qparent;		/*pointers to adjacent corr tree nodes*/
  p = &(t->coord_root);
/*inv: The path from t->coord_root to *p is labelled with values that bound or
  equal node->data->coords.*/
  while(*p != (DNODE *)0)
    p = ((node->data->coords > (*p)->data->coords)? &((*p)->coord_rchild): &((*p)->coord_lchild));
/*link the node into the coord tree*/
  node->coord_lchild = (DNODE *)0;
  node->coord_rchild = (DNODE *)0;
  *p = node;
/*link the node into the corr tree*/
  node->size = 1;
  node->corr_lchild = (DNODE *)0;
  node->corr_rchild = (DNODE *)0;
  if(t->corr_root == (DNODE *)0)
    {
  /*insertion into an empty tree is a special case*/
    node->corr_parent = (DNODE *)0;
    t->corr_root = node;
    }
  else
    {
    q = t->corr_root;
  /*inv: The path from t->corr_root to q is labelled with values that bound or
    equal r.*/
    do
      {
      q->size++;
      qparent = q;
      q = ((node->data->corr > q->data->corr)? q->corr_rchild: q->corr_lchild);
      } while(q != (DNODE *)0);
  /*link the node into the correlation tree*/
    node->corr_parent = qparent;
    if(node->data->corr > qparent->data->corr)
      qparent->corr_rchild = node;
    else
      qparent->corr_lchild = node;
    }
  }

static int num_coords;

/*Insert the given value into the dtree.*/
static void dtree_insert(DTREE *t, CLIST *clist)
  {
  t->next->data = clist;
  t->next->dlen = num_coords;
  dtree_insert_at_node(t, t->next++);
  }

static int num_coords_exhausted;

/*p points to a coord_lchild, coord_rchild, or coord_root field that is to be
  unlinked from its coord children and its corr children and corr parent within
  the tree t.*/
void dtree_unlink_node(DTREE *t, DNODE **p)
  {
  register DNODE **q;
  DNODE *node, *parent, *temp;
  node = *p;
  if(node->coord_lchild == (DNODE *)0)
    *p = node->coord_rchild;
  else if(node->coord_rchild == (DNODE *)0)
    *p = node->coord_lchild;
  else
    {
  /*inv: *q is the rightmost node at the current level in the left subtree of
    (*p).*/
    for(q = &(node->coord_lchild); (*q)->coord_rchild != (DNODE *)0; q = &((*q)->coord_rchild))
      ;
    (*q)->coord_rchild = node->coord_rchild;
    *p = *q;
    if(q != &(node->coord_lchild))
      {
      DNODE *temp;
      temp = (*q)->coord_lchild;
      (*q)->coord_lchild = node->coord_lchild;
      *q = temp;
      }
    }
/*The node has been unlinked from the coord tree.  Now unlink it from the corr
  tree, using a procedure analogous to that which was implemented above for the
  coord tree.  First, decrement the size counts from the current position back
  to the corr root:*/
  for(parent = node->corr_parent; parent != (DNODE *)0; parent = parent->corr_parent)
    parent->size--;
/*Make p point to the link that leads into the subtree rooted at node.*/
  p = ((node->corr_parent == (DNODE *)0)? &(t->corr_root):
	(node == node->corr_parent->corr_lchild)?
		&(node->corr_parent->corr_lchild):
		&(node->corr_parent->corr_rchild));
/*The cases of fewer than two children are easy to handle:*/
  if(node->corr_lchild == (DNODE *)0)
    {
    if(node->corr_rchild)
      node->corr_rchild->corr_parent = node->corr_parent;
    *p = node->corr_rchild;
    }
  else if(node->corr_rchild == (DNODE *)0)
    {
    if(node->corr_lchild)
      node->corr_lchild->corr_parent = node->corr_parent;
    *p = node->corr_lchild;
    }
/*In the two-child case, we find the rightmost node of the left subtree and
  promote it.*/
  else
    {
  /*inv: *q is the rightmost node at the current level in the left subtree of
    (*p).*/
    for(q = &(node->corr_lchild); (*q)->corr_rchild != (DNODE *)0; q = &((*q)->corr_rchild))
      (*q)->size--;
    (*q)->corr_rchild = node->corr_rchild;
    if(node->corr_rchild)
      node->corr_rchild->corr_parent = *q;
    *p = *q;
    parent = (*q)->corr_parent;
    (*q)->corr_parent = node->corr_parent;
    if(q != &(node->corr_lchild))
      {
  /*If the left subtree of the rightmost node is non-null, promote it to fill
    the place that has just been vacated by the promoted rightmost node.*/
      temp = (*q)->corr_lchild;
      (*q)->corr_lchild = node->corr_lchild;
      node->corr_lchild->corr_parent = *q;
      *q = temp;
      if(temp)
	temp->corr_parent = parent;
      }
    (*p)->size = 1
	+ ((*p)->corr_lchild? (*p)->corr_lchild->size: 0)
	+ ((*p)->corr_rchild? (*p)->corr_rchild->size: 0);
    }
  }

/*Delete from the dtree all nodes that are labelled with the given coordinates.
  For each of the deleted nodes, if any substitute coordinates remain in the
  coordinate list, use those substitute coordinates to re-insert the node into
  the dtree.  Otherwise increment num_coords_exhausted.*/
static void dtree_delete_coords(DTREE *t, int x, int y, int z, short *deleted)
/*deleted[x+xdim*(y+ydim*z)] is nonzero iff (x,y,z) has not yet been deleted*/
  {
  register DNODE **p,			/*pointer into coord tree*/
		 *del;			/*pointer to deleted node*/
  int target_coords_not_found,		/*flag indicates coords not yet found*/
      coords;				/*encoded form of target coordinates*/
  coords = coord_encode(x, y, z, t->xdim, t->ydim);
  p = &(t->coord_root);
  target_coords_not_found = 1;
/*inv: All remaining nodes whose coordinates match the target coords are in the
  subtree rooted at *p.  All matching nodes outside this subtree have been
  replaced by nodes with substitute coordinates and correlations.*/
  while(target_coords_not_found && (*p != (DNODE *)0))
    {
    if(coords > (*p)->data->coords)
      p = &((*p)->coord_rchild);
    else if(coords < (*p)->data->coords)
      p = &((*p)->coord_lchild);
    else
      {
    /*found a matching node*/
      target_coords_not_found = 0;
      do
	{
	del = *p;
	dtree_unlink_node(t, p); /*note: *p != del after this call!*/
      /*re-insert the node with substitute data, if any substitutes remain*/
	do {del->dlen--; del->data++;
      /*check that substitutes haven't been exhausted (dlen != 0) and the
	coordinates associated with the substitute at the head of the list are
	still not valid (deleted[del->data->coords] != 0)*/
	   } while((del->dlen != 0) && deleted[del->data->coords]);
      /*If valid substitute data are available, re-insert this node using those
	substitutes.  Otherwise, increment the count of occurrences of failed
	substitutions.*/
	if(del->dlen != 0)
	  dtree_insert_at_node(t, del);
	else
	  num_coords_exhausted++;
	} while(*p && (coords == (*p)->data->coords));
/*By clearing the flag target_coords_not_found on entry to the inner loop, we
  ensure that the outer loop will terminate once the inner loop's guard has
  become false.  The correctness of this strategy depends on the structure of
  the auxiliary routine dtree_unlink_node() and on the following line in
  dtree_insert_at_node() above:
    p = ((node->data->coords > (*p)->data->coords)? &((*p)->coord_rchild):
						    &((*p)->coord_lchild));
  This line makes sets of equal coordinates into *left*-recursive rather than
  right-recursive trees, and accordingly, dtree_unlink_node() looks in the
  *left* subtree for a node to promote into the place of the deleted node as
  (*p).*/
      }
    }
  }

/*Return a fraction in the interval (0,1) that describes the ordinal position
  that the given correlation value would occupy within the ordered contents of
  the dtree, were it to be added to the tree.*/
static double dtree_position(DTREE *t, double r)
  {
  register DNODE *p;
  register int rank;
  p = t->corr_root;
  if((p == (DNODE *)0) || (p->size == 1))
    return(0.5);
  rank = 0;
/*inv: rank/2 is the number of correlation values in the tree that are less
  than r and are not within the subtree rooted at p, plus half of the number
  of correlation values in the tree that equal r and are not within the subtree
  rooted at p.*/
  while(p != (DNODE *)0)
    if(r > p->data->corr)
      {
      rank += 2*(1+(p->corr_lchild? p->corr_lchild->size: 0));
      p = p->corr_rchild;
      }
    else
      {
      if(r == p->data->corr)
	rank++; /*values equal to the target count only half*/
      p = p->corr_lchild;
      }
  if(rank == 2*t->corr_root->size)
    rank--;
  else if(rank == 0)
    rank++;
  return(rank / (2.0*t->corr_root->size));
  }

/*Randomly permute the integer array a[0..n-1].
  Richard Durstenfeld, `Algorithm 235: Random Permutation [G6]',
  Communications of the Association for Computing Machinery 7:7:420 (1964).*/
static void shuffle(int *a, int n)
  {
  register int i, j, b;
  for(i = n-1; i != 0; i--)
    {
    j = (int)(random()%(i+1));
    b = a[i]; a[i] = a[j]; a[j] = b;
    }
  }

/*Prepare static sums for correlate().*/
static void correlate_prep(float *x, float *y, int nxy, double *sy, double *syy)
  {
  register int n;	/*length of time series, excluding ignored points*/
  *sy = *syy = 0.0;
  n = 0;
  while(nxy--)
    {
    if(*x < BLAST)
      {
      *syy += *y**y;
      *sy += *y;
      n++;
      }
    x++; y++;
    }
  *syy -= *sy**sy/n;
  }

/*Correlate the ideal time series x with the actual time series y, where both
  series are of length nxy.  If the pointers r, m, and b are non-null, they
  receive, respectively, the correlation coefficient and the slope and
  intercept of the regression line.  A call to correlate() must be preceded by
  calls to correlate_prep() of the following forms:
  correlate_prep(x, y, nxy, &sx, &sxx);
  correlate_prep(x, y, nxy, &sy, &syy);*/
static void correlate(
	float *x,	/*ideal time series (points >= BLAST are ignored)*/
	float *y,	/*actual time series*/
	int nxy,	/*length of time series (including any ignored points)*/
	double sx,	/*sum of x[i] | 0<=i<nxy and x[i]<BLAST*/
	double sxx,	/*sum of x[i]^2 | 0<=i<nxy and x[i]<BLAST*/
	double sy,	/*sum of y[i] | 0<=i<nxy and x[i]<BLAST*/
	double syy,	/*sum of y[i]^2 | 0<=i<nxy and x[i]<BLAST*/
	double *r,	/*correlation coefficient*/
	double *m,	/*slope*/
	double *b)	/*intercept*/
  {
  register double sxy;
  register int n;	/*length of time series, excluding ignored points*/
  double slope;
  if(sxx == 0.0)
    {
    if(r) *r = 0.0;
    if(m) *m = 0.0;
    if(b)
      {
      n = 0;
      while(nxy--)
	if(x[nxy] < BLAST)
	  n++;
      *b = sy/n;
      }
    }
  else
    {
    sxy = 0.0;
    n = 0;
    while(nxy--)
      {
      if(*x < BLAST)
	{
	sxy += *x**y;
	n++;
	}
      x++; y++;
      }
    sxy -= sx*sy/n;
    if(r)
      *r = ((syy == 0.0)? 0.0: (sxy/sqrt(sxx*syy)));
    slope = sxy/sxx;
    if(m) *m = slope;
    if(b) *b = (sy - slope*sx)/n;
    }
  }

/*Orthogonalise y to x, where m and b have been set by a call to correlate().
  Store the result in new_y (which may point to the same location as y, in the
  case in which the original value of y need not be saved).  For linear
  detrending, input x[i] = i, 0<=i<n.*/
static void orthogonalise(float *x, float *y, float *new_y, int n, double m, double b)
  {
  while(n--)
    if(x[n] < BLAST)
      new_y[n] = y[n]-(m*x[n]+b);
  }

/*Initialise the numerator sums (*cov)[0..order-1], the denominator sum *var,
  the number of included time points *n_incl, and optionally the exclusion
  vector excl[order..n-1], for an AR(order) autocorrelation computation.  If
  the pointer excl_0 is non-null then excl_0[0..n-1] is a time series whose
  elements are non-zero iff the corresponding element of the fMRI time series
  is to be excluded from the autocorrelation computation.  (For example, in the
  context of a block design it may be desirable to exclude expected intervals of
  activation, and to include only expected baseline intervals.)  Compute
  excl[order..n-1] such that excl_0[t] ==> excl[max(t, order) .. min(t+order,
  n-1)]  for all i in [0, n-1], that is, the exclusion of a time point also
  excludes the following 'order' time points.  (This requirement ensures that
  looking back at lagging points yields valid data.)  ((*excl)-order) and *cov
  are dynamically allocated; the calling routine is responsible for invoking
  autocorrelate_finish() after the last call to autocorrelate_compute() to free
  ((*excl)-order), and for freeing *cov itself once the correlations are no
  longer needed.  Return 0 if successful, 1 if out of memory.*/
/*          K&R to ANSI from Greg Balls   7 Aug 2006 [rickr] */
static int autocorrelate_init(float *excl_0, int n, int order, char **excl,
                              double **cov, double *var, int *n_incl)
  {
  register int t, k;
  *cov = (double *)calloc(order, sizeof(**cov));
  if(*cov == (double *)0)
    return(1);
/*no need to zero (*cov)[0..order-1], since calloc() has already done so*/
  *var = 0.0;
  *n_incl = n-order;
  if(excl_0 != (float *)0)
    {
    *excl = calloc(n, sizeof(**excl));
    if(*excl == (char *)0)
      {
      free(*cov);
      return(1);
      }
  /*no need to initialise (*excl)[] to 0, since calloc() has already done so*/
  /*compute (*excl)[] at its lower boundary*/
    for(t = 0; t != order; t++)
      if(excl_0[t] != 0.0)
	{
	for(k = order-t; k <= order; k++)
	  (*excl)[t+k] = 1;
	*n_incl -= t+1;
	}
  /*compute (*excl)[] between its boundaries*/
    while(t < n-order)
      {
      if(excl_0[t] != 0.0)
	{
	for(k = 0; k <= order; k++)
	  (*excl)[t+k] = 1;
	*n_incl -= order+1;
	}
      t++;
      }
  /*compute (*excl)[] at its upper boundary*/
    while(t < n)
      {
      if(excl_0[t] != 0.0)
	{
	for(k = 0; k < n-t; k++)
	  (*excl)[t+k] = 1;
	*n_incl -= n-t;
	}
      t++;
      }
    }
  else
    *excl = (char *)0;
  return(0);
  }

/*Given ts[0..n-1], a voxel's detrended fMRI time series, and optionally
  excl[0..n-1], a vector of flags computed by autocorrelate_init() that signify
  exclusion from the autocorrelation computation, accumulate data from this
  voxel's time series into the numerator sums cov[0..order-1] and the
  denominator sum var for computing autocorrelation coefficients in an AR(order)
  model.*/
/*          K&R to ANSI from Greg Balls   7 Aug 2006 [rickr] */
static void autocorrelate_sum(
    float  *ts,	    /* detrended fMRI time series from one voxel */
    int     n,	    /* length of ts[] */
    int     n_incl, /* number of 0's in excl[0..n-1],  */
                    /*     else n-order if no excl[] */
    int     order,  /* AR order - i.e., number of lags modelled in r[]*/
    char   *excl,   /* excl[i]=0 <==> include ts[i] in autocorrelation */
                    /*     computation*/
    double *cov,/*cov[k-1] accumulates Sigma[(ts[i]-ts_avg)*(ts[i-k]-ts_avg)],*/
                /*		       !excl[i] & !excl[i-k], order<=i<n*/
    double *var	/**  var accumulates Sigma[(ts[i]-ts_avg)^2],		*/
		/*  		     !excl[i] & !excl[i-k], order<=i<n	*/
)
  {
  register int t,	/*time index*/
	k;		/*AR lag index*/
  double avg,		/*average of ts[] over all included time points*/
	excursion;	/*current sample's excursion from the mean*/
  avg = 0.0;
  if(excl == (char *)0)
    {
  /*if no exclusion vector has been provided, then all points are included*/
    for(t = order; t < n; t++)
      avg += ts[t];
    avg /= n_incl;
    for(t = order; t < n; t++)
      {
      excursion = ts[t]-avg;
      for(k = order; k; k--)
	cov[k-1] += excursion*(ts[t-k]-avg);
      *var += excursion*excursion;
      }
    }
  else
    {
  /*exclude the points specified by the exclusion vector*/
    for(t = order; t < n; t++)
      if(!excl[t])
	avg += ts[t];
    avg /= n_incl;
    for(t = order; t < n; t++)
      if(!excl[t])
	{
	excursion = ts[t]-avg;
	for(k = order; k; k--)
	  cov[k-1] += excursion*(ts[t-k]-avg);
	*var += excursion*excursion;
	}
    }
  }

/*Finish the autocorrelation computation by dividing the numerator sums
  cov[0..order-1] by the denominator sum var, and leaving the resulting
  autocorrelations in cov[0..order-1].  Check for autocorrelations that are
  impossible (|r| > 1, possible in the event of some weird accumulation of
  rounding errors), or physiologically implausible (|r| = 1 or r < 0).  If such
  values are found, eliminate them from the model by decreasing the model's
  order (*order).  Also, deallocate the storage used for excl[order..n-1].*/
static void autocorrelate_finish(int *order, char *excl, double *cov,
                                 double var)
/* int *order; char *excl; double *cov, var; */
  {
  register int k;
  if(excl != (char *)0)
    free(excl);			/*exclusion vector is no longer needed*/
  k = *order;
  while(k--)
    {
    cov[k] /= var;
    if((cov[k] >= 1.0) || (cov[k] < 0.0))
      {
      fprintf(stderr, "Suspect AR(%d) autocorrelation %f; restricting model to AR(%d)\n", k+1, cov[k], k);
#ifdef AR_DEBUG
      fprintf(stderr, "\tDEBUG MODE: OVERRIDING THIS RESTRICTION\n");
#else
      *order = k;
#endif
      }
    }
  }

/*Solve the system of equations represented by the augmented matrix
  m[0..n-1][0..n] (in row-major order), using simple Gaussian elimination.
  (n is assumed to be small enough so that LU decomposition wouldn't yield any
  substantial increase in speed.)  If m is singular, 1 is returned.  If m is
  non-singular, 0 is returned and the solutions are left in m[0..n-1][n].*/
/*          K&R to ANSI from Greg Balls   7 Aug 2006 [rickr] */
static int solve(double **m, int n)
/* double **m; int n; */
  {
  register int i, j, k;
  int singular;
  double *tmprow;
  singular = 0;
/*inv: m is a linear combination of the original m, and
       for all i where 0<=i<j, (m[ii]=1 and m[i][i+1..n-1]=0)*/
  for(j = 0; (!singular) && (j != n); j++)
    {
  /*inv: m[j..i-1][j]=0*/
    for(i = j; (i != n) && (m[i][j] == 0.0); i++)
      ;
    if(i == n)
      singular = 1;
    else
      {
    /*m[i][j] is the topmost nonzero element in column j - if row i isn't
      already the top row, make it the top row by exchanging rows:*/
      if(i != j)
	{
	tmprow = m[i];
	m[i] = m[j];
	m[j] = tmprow;
	}
    /*m[j][j] is nonzero; multiply row j by 1/m[j][j] to make m[j][j]=1*/
      for(i = n; i != j; i--)
	m[j][i] /= m[j][j];
      m[j][j] = 1.0;
    /*subtract scalar multiples of row j from rows j+1..n-1 so as to make
      the lower column m[j+1..n-1][j]=0*/
    /*inv: m is a linear combination of the original m, and m[j+1..i-1][j]=0*/
      for(i = j+1; i != n; i++)
	{
	for(k = n; k != j; k--)
	  m[i][k] -= m[i][j]*m[j][k];
	m[i][j]= 0.0;
	}
      }
    }
  if(!singular)
    {
  /*inv: m is a linear combination of the original m with rows j+1..n-1 solved*/
    for(j = n-1; j; j--)
    /*inv: m is a linear combination of the original m and m[0..i-1][j]=0*/
      for(i = 0; i != j; i++)
	{
	m[i][n] -= m[i][j]*m[j][n];
	m[i][j] = 0.0;
	}
    }
  return(singular);
  }

/*On entry, r[0..order-1] are the autocorrelation coefficients for lags
  1..order.  On successful exit, 0 is returned and r[0..order-1] are the
  autoregression coefficients for lags 1..order - i.e., they implement the
  AR(order) model ts[t] = r[0]*ts[t-1] + r[1]*ts[t-2] + ...
  + r[order-1]*ts[t-order] + e(t).  If a singular matrix is encountered,
  or if a memory allocation error arises, r[] is zeroed and 1 is returned.*/
/*          K&R to ANSI from Greg Balls   7 Aug 2006 [rickr] */
static int yule_walker(
    double *r, /* r[0..order-1] are the autocorrelations for lags 1..order */
    int order )
  {
  register int i, k;
  int retval;
  double **m;	/*the Yule-Walker matrix*/
/*allocate storage for all rows m[0..order-1]*/
  m = (double **)calloc(order, sizeof(*m));
  if(m == (double **)0)
    retval = 1;
  else
    {
  /*set up row pointers*/
    *m = (double *)calloc(order*(order+1), sizeof(**m));
    if(*m == (double *)0)
      {
      free(m);
      retval = 1;
      }
    else
      {
      for(i = 1; i != order; i++)
	m[i] = m[i-1]+order+1;		/*1 extra column for augmented matrix*/
      for(i = 0; i != order; i++)
        {
        for(k = i-1; k >= 0; k--)
	  m[i][k] = r[i-k-1];		/*m[i][i-1..0] = r[0..i-1]*/
        m[i][i] = 1.0;			/*diagonal is 1*/
        for(k = i+1; k != order; k++)
	  m[i][k] = r[k-i-1];		/*m[i][i+1..order-1] = r[0..order-2-i]*/
        m[i][k] = r[i];			/*rightmost column is r[0..order-1]*/
        }

#ifdef AR_DEBUG
{int i, j; printf("\nYULE-WALKER MATRIX:\n");
for(i=0; i!=order; i++){
  for(j=0; j!=1+order; j++) printf("%f ", m[i][j]);
  putchar('\n');}}
#endif

      retval = solve(m, order);

#ifdef AR_DEBUG
{int i, j; printf("YULE-WALKER SOLUTION:\n");
for(i=0; i!=order; i++){
  for(j=0; j!=1+order; j++) printf("%f ", m[i][j]);
  putchar('\n');}}
#endif

      if(retval == 0)
    /*read out autoregression coefficients*/
        for(i = 0; i != order; i++)
	  r[i] = m[i][order];
      free(*m);
      free(m);
      }
    }
/*if there was a singularity or a memory error, zero r[]*/
  if(retval)
    for(i = 0; i != order; i++)
      r[i] = 0.0;
  return(retval);
  }

/*Apply the AR(order) autoregression coefficients reg[0..order-1] to whiten the
  coloured time series coloured[0..n-1].  Place the result in white[0..n-1].
  coloured[] and white[] must be distinct storage.*/
/*          K&R to ANSI from Greg Balls   7 Aug 2006 [rickr] */
static void whiten(
    const float *coloured,	/*coloured[0..n-1] (source)*/
    float *white,		/*white[0..n-1] (destination)*/
    int n,
    double *reg,		/*reg[0..order-1] (AR(order) regression coeff.*/
    int order)
  {
  register int t, k;
  *white = *coloured;
  for(t = 1; t < order; t++)
    {
    white[t] = coloured[t];
    for(k = 0; k != order; k++)
      white[t] -= reg[k]*coloured[(t-k<=0)? 0: (t-k-1)];
    }
  while(t < n)
    {
    white[t] = coloured[t];
    for(k = 0; k != order; k++)
      white[t] -= reg[k]*coloured[t-k-1];
    t++;
    }
  }

/*Apply the AR(order) autoregression coefficients reg[0..order-1] to colour
  the white time series white[0..n-1].  Place the result in coloured[0..n-1].
  coloured[] and white[] must be distinct storage.*/
static void colour(
    float *coloured,	/*coloured[0..n-1] (source)*/
    const float *white,	/*white[0..n-1] (destination)*/
    int n,
    double *reg,	/*reg[0..order-1] (AR(order) regression coeff.*/
    int order)
  {
  register int t, k;
  *coloured = *white;
  for(t = 1; t < order; t++)
    {
    coloured[t] = white[t];
    for(k = 0; k != order; k++)
      coloured[t] += reg[k]*coloured[(t-k<=0)? 0: (t-k-1)];
    }
  while(t < n)
    {
    coloured[t] = white[t];
    for(k = 0; k != order; k++)
      coloured[t] += reg[k]*coloured[t-k-1];
    t++;
    }
  }

/*Write vec[0..len-1] to the ASCII file whose name is filename_prefix
  concatenated with filename_suffix concatenated with ".1D".*/
static int write_1D_file(
    const double *vec,
    int len,
    const char *filename_prefix,
    const char *filename_suffix)
  {
  register int i, j;
  FILE *fp;
  char filename[1+MAXPATHLEN];
  for(i = 0; filename_prefix[i] != '\0'; i++)
    filename[i] = filename_prefix[i];
  for(j = 0; filename_suffix[j] != '\0'; j++)
    filename[i++] = filename_suffix[j];
  filename[i++] = '.';
  filename[i++] = '1';
  filename[i++] = 'D';
  filename[i++] = '\0';
  fp = fopen(filename, "w");
  if(fp == NULL)
    {
    perror(filename);
    return(1);
    }
  while(len--)
    fprintf(fp, "%f\n", *(vec++));
  fclose(fp);
  return(0);
  }

#ifdef AR_DEBUG

/*test driver: writes a coloured time series to "permtest_autoregr_series.dat",
  writes this time series' autocorrelation coefficients to "_autocorrelation"
  and its autoregression coefficients to "_autoregression", then writes the
  corresponding whitened time series to "permtest_autoregr_whitened.dat" and
  the recoloured time series to "permtest_autoregr_recoloured.dat" and prints
  the autocorrelation coefficients of this recoloured time series.*/
void AR_test()
  {
  FILE *tsfile;
  const int tslen = 200;
  int i, j, k;
  float ts[tslen],	/*original, coloured time series*/
	white[tslen],	/*whitened time series*/
	excl_0[tslen];	/*time points to be excluded from the computation*/
  char *excl;		/*excl_0[] with time lags of length 'order'*/
  double *r,		/*covariances, autocorrelations, autoregressions*/
	var,		/*variance*/
	e0, e1, e2, e3;	/*additive coloured noise terms*/
  int order,		/*autoregressive order*/
	n_incl;		/*# of included (i.e. non-excluded) points*/
  order = 3;
  tsfile = fopen("permtest_autoregr_series.dat", "w");
  if(tsfile == NULL) exit(1);
  e1 = e2 = e3 = 0.0;
  for(i = 0; i != tslen; i++)
    {
  /*10% + 20% + 30% = 60% coloured noise plus 40% white noise:*/
    e0 = 0.1*e3 + 0.2*e2 + 0.3*e1 + 0.4*((random()/(double)RAND_MAX)-0.5);
    ts[i] = e0		/*	+sin((2.0*3.14159*i)/(tslen-order))	*/ ;
    e3 = e2; e2 = e1; e1 = e0;
    fprintf(tsfile, "%f\n", ts[i]);
    }
  fclose(tsfile);
  j = order*order;
  k = 0;
/*alternating included and excluded blocks of 9 samples each:*/
  for(i = 0; i != tslen; i++, j--)
    {
    if(j == 0)
      {
      j = order*order;
      k = !k;
      }
    excl_0[i] = k;
    }
  if(autocorrelate_init((float *)0, tslen, order, &excl, &r, &var, &n_incl))
    {
    fprintf(stderr, "autocorrelate_init: out of memory\n");
    return(1);
    }
  autocorrelate_sum(ts, tslen, n_incl, order, excl, r, &var);
  autocorrelate_finish(&order, excl, r, var);
  write_1D_file(r, order, "", "autocorrelation");
  if(order)
    {
    if(yule_walker(r, order))
      /*this never happens*/
      fprintf(stderr,
	"Cannot compute autoregression; Yule-Walker matrix is singular.\n");
    else
      {
      write_1D_file(r, order, "", "autoregression");
      whiten(ts, white, tslen, r, order);
      tsfile = fopen("permtest_autoregr_whitened.dat", "w");
      if(tsfile == NULL) exit(1);
      for(i = 0; i != tslen; i++)
	fprintf(tsfile, "%f\n", white[i]);
      fclose(tsfile);
      colour(ts, white, tslen, r, order);
      tsfile = fopen("permtest_autoregr_recoloured.dat", "w");
      if(tsfile == NULL) exit(1);
      for(i = 0; i != tslen; i++)
	fprintf(tsfile, "%f\n", ts[i]);
      fclose(tsfile);
      }
    free(r);
    if(autocorrelate_init((float *)0, tslen, order, &excl, &r, &var, &n_incl))
      {
      fprintf(stderr, "autocorrelate_init: out of memory\n");
      return(1);
      }
    autocorrelate_sum(white, tslen, n_incl, order, excl, r, &var);
    for(i = 1; i <= order; i++)
      printf("\t\tAR(%d)", i);
    autocorrelate_finish(&order, excl, r, var);
    printf("\nautocorrelations:");
    for(i = 0; i != order; i++)
      printf("\t%f", r[i]);
    putchar('\n');
    }
  free(r);
  }

#endif

/*Compute the permutation test.  Return 0 if successful, 1 if error.*/
static int PERMTEST_compute(
  PLUGIN_interface *plint,	/*AFNI plugin interface*/
  THD_3dim_dataset *dset,	/*AFNI 3D+time dataset*/
  MRI_IMAGE *ref_ts,		/*reference time series*/
  MRI_IMAGE *ort_ts,		/*time series against which to orthogonalise*/
  double pcrit,			/*critical probability*/
  int one_tailed,	/*-1=one-tailed (-), 0=two-tailed, 1=one-tailed (+)*/
	/*returned parameters:*/
  short **intensities,		/*address of pointer to intensities*/
  short **zvals,		/*address of pointer to z-scores*/
  double *fim_scale,		/*scaling factor for intensities*/
  THD_3dim_dataset *mask,	/*mask dataset, or NULL for no mask*/
  float masklo,			/*lower and upper bounds on masked range*/
  float maskhi,
  int verbose,		/*1 for verbose info on coordinates, 0 otherwise*/
  int AR_order,			/*autoregressive model order*/
  MRI_IMAGE *AR_excl_ts)	/*autoregression exclusion flags*/
  {
  register int t, iter, xyz;	/*indices and counters*/
  int x, y, z,
      xdim, ydim, zdim,		/*spatial dimensions*/
      tdim,			/*temporal dimension of dataset*/
      tsize,			/*# of included (< BLAST) points, tsize<=tdim*/
      ignore,			/*# of ignored (>=BLAST) pts at head of series*/
      t2,
      *tindex,			/*temporal sequence of all included images*/
      *sequence;		/*permuted temporal sequence*/
  float *vox_xyz,		/*time series for one voxel*/
	*AR_ts,			/*time series after whitening or colouring*/
	*vox,			/*3D+time data indexed by z,y,x,t*/
	*ts,		      /*storage for linear series (ts[t]=t, 0<=t<tsize),
				 and later for randomised time series*/
	*fit_coeff,		/*fit coefficients*/
	mask_val;		/*value of mask at current voxel*/
  STREE *actual_corr;
  DTREE *randomised_corr;
  CLIST *coord_mem,			/*data area for coord-corr pair lists*/
	*coord_next;			/*next free location in coord_mem*/
  double sx_trend, sx_ref, sx_ort, *sy,	/*correlation parameters*/
	 sxx_trend, sxx_ref, sxx_ort, *syy,
	 corr, slope, intercept,	/*regression coefficients*/
	 max_corr;			/*correlation w/ max absolute value*/
  int percent_done, prev_percent_done,	/*statistics for progress meter*/
      not_done;

/*variables for the autoregression computation*/
  double *AR,			/*autoregression coefficients*/
	  AR_var;		/*variance*/
  int AR_n_incl;		/*# of points included in covariance calc.*/
  char *AR_excl;		/*flags at each time point, 1=excluded*/

/*set dimensions*/
  xdim = dset->daxes->nxx;
  ydim = dset->daxes->nyy;
  zdim = dset->daxes->nzz;
  tdim = DSET_NUM_TIMES(dset);
  switch(one_tailed)
    {
    case -1: magnitude = neg; break;
    case 0: magnitude = fabs; break;
    case 1: magnitude = id;
    }
  num_coords = NUM_COORDS;
  num_coords_exhausted = 0;
  if(ort_ts != (MRI_IMAGE *)0) /*need to save extra points if orthogonalising*/
    num_coords *= 2;
/*allocate storage for functional intensities*/
  *intensities = (short *)calloc(xdim*ydim*zdim, sizeof(**intensities));
  if(*intensities == (short *)0)
    return 1;
/*allocate storage for z-scores*/
  *zvals = (short *)calloc(xdim*ydim*zdim, sizeof(**zvals));
  if(*zvals == (short *)0)
    {
    free(*intensities);
    return 1;
    }
  /*no need to initialise (*zvals)[] to zeroes, since calloc() does it for us*/
  /*Initialise temporal sequences:  `tindex' is a mapping from the time index
    used in statistical tests to the time index used for storage in the AFNI
    dataset; it excludes ignored time points.  `sequence' is initially the same
    as tindex but will be permuted during each iteration of the resampling
    procedure.*/
  tindex = (int *)calloc(tdim, sizeof(*tindex));
  if(tindex == (int *)0)
    {
    free(*zvals);
    free(*intensities);
    return 1;
    }
  sequence = (int *)calloc(tdim, sizeof(*sequence));
  if(sequence == (int *)0)
    {
    free(tindex);
    free(*zvals);
    free(*intensities);
    return 1;
    }
  for((t = 0), (tsize = 0); t != tdim; t++)
    if(MRI_FLOAT_PTR(ref_ts)[t] < BLAST)
      {
      tindex[tsize] = t;
      sequence[tsize++] = t;
      }
  if(tsize < 4)		/*make sure we have enough points for regression*/
    {
    free(sequence);
    free(tindex);
    free(*zvals);
    free(*intensities);
    fprintf(stderr, "ERROR: %d time points aren't enough to perform regression\n", tsize);
    return 1;
    }
/*initialise data*/
  vox = (float *)calloc(xdim*ydim*zdim*tdim, sizeof(*vox));
  if(vox == (float *)0)
    {
    free(sequence);
    free(tindex);
    free(*zvals);
    free(*intensities);
    return 1;
    }
  sy = (double *)calloc(xdim*ydim*zdim, sizeof(*sy));
  if(sy == (double *)0)
    {
    free(vox);
    free(sequence);
    free(tindex);
    free(*zvals);
    free(*intensities);
    return 1;
    }
  syy = (double *)calloc(xdim*ydim*zdim, sizeof(*syy));
  if(syy == (double *)0)
    {
    free(sy);
    free(vox);
    free(sequence);
    free(tindex);
    free(*zvals);
    free(*intensities);
    return 1;
    }
  ts = (float *)calloc(tdim, sizeof(*ts));
  if(ts == (float *)0)
    {
    free(syy);
    free(sy);
    free(vox);
    free(sequence);
    free(tindex);
    free(*zvals);
    free(*intensities);
    return 1;
    }
  actual_corr = stree_create(xdim, ydim, xdim*ydim*zdim);
  if(actual_corr == (STREE *)0)
    {
    free(syy);
    free(sy);
    free(ts);
    free(vox);
    free(sequence);
    free(tindex);
    free(*zvals);
    free(*intensities);
    return 1;
    }
  fit_coeff = (float *)calloc(xdim*ydim*zdim, sizeof(*fit_coeff));
  if(fit_coeff == (float *)0)
    {
    stree_destroy(actual_corr);
    free(syy);
    free(sy);
    free(ts);
    free(vox);
    free(sequence);
    free(tindex);
    free(*zvals);
    free(*intensities);
    return 1;
    }

/******************************************************************************
 * PHASE 1: Copy data, remove linear trend, optionally orthogonalise against  *
 *	    a user-supplied series, and correlate with the ideal series.      *
 ******************************************************************************/
  for(ignore = 0; MRI_FLOAT_PTR(ref_ts)[ignore] >= BLAST; ignore++)
    ;
/*linear series to use in detrending*/
  for(t = 0; t != tdim; t++)
    ts[t] = (float)t;
/*sum of the linear series, and sum of its squares*/
  sx_trend = (double)(((tdim-1)*tdim - (ignore-1)*ignore)/2);
  sxx_trend = ((double)((tdim*(tdim*(2*tdim-3)+1) - ignore*(ignore*(2*ignore-3)+1))/6)) - sx_trend*sx_trend/(tdim-ignore);
/*compute the sum and sum of squares of the ideal time series*/
  correlate_prep(MRI_FLOAT_PTR(ref_ts)+ignore, MRI_FLOAT_PTR(ref_ts)+ignore, tdim-ignore, &sx_ref, &sxx_ref);
/*compute the sum and sum of squares of the orthogonal series, if specified*/
  if(ort_ts != (MRI_IMAGE *)0)
    correlate_prep(MRI_FLOAT_PTR(ort_ts), MRI_FLOAT_PTR(ort_ts), tdim, &sx_ort, &sxx_ort);
  *fim_scale = 0.0;
  mask_val = masklo;
/*if autoregressive correction for coloured noise has been requested,
  iniialise the autoregression computation:*/
  if(AR_order && autocorrelate_init(((AR_excl_ts != (MRI_IMAGE *)0)?
					MRI_FLOAT_PTR(AR_excl_ts)+ignore:
					(float *)0),
    tdim-ignore, AR_order, &AR_excl, &AR, &AR_var, &AR_n_incl))
    {
    fprintf(stderr, "autocorrelate_init: out of memory\n");
    return 1;
    }
  xyz = 0;
  for(z = 0; z !=  zdim; z++)
    for(y = 0; y != ydim; y++)
      for(x = 0; x != xdim; x++)
	{
	if(mask != NULL)
	  {
	/*fetch the mask brick's scaling factor...*/
	  mask_val = DSET_BRICK_FACTOR(mask, 0);
	  if(mask_val == 0.0)
	    mask_val = 1.0;
	/*...and multiply it by the mask datum at coordinates (x, y, z)*/
	  if(DSET_BRICK_TYPE(mask, 0) == MRI_short)
	    mask_val *= ((short *)DSET_ARRAY(mask, 0))[xyz];
	  else
	    mask_val *= ((char *)DSET_ARRAY(mask, 0))[xyz];
	  }
	if((mask_val >= masklo) && (mask_val <= maskhi))
	  {
	  vox_xyz = vox + tdim*xyz;
	  for(t = 0; t != tdim; t++)
	    {
	  /*fetch the time series brick's scaling factor...*/
	    vox_xyz[t] = DSET_BRICK_FACTOR(dset, t);
	    if(vox_xyz[t] == 0.0)
	      vox_xyz[t] = 1.0;
	  /*...and multiply it by the time series datum at (x, y, z, t)*/
	    vox_xyz[t] *= ((short *)DSET_ARRAY(dset, t))[xyz];
	    }
	/*detrend*/
	  correlate_prep(ts+ignore, vox_xyz+ignore, tdim-ignore, sy+xyz, syy+xyz);
	  correlate(ts+ignore, vox_xyz+ignore, tdim-ignore, sx_trend, sxx_trend, sy[xyz], syy[xyz], (double *)0, &slope, &intercept);
	  orthogonalise(ts, vox_xyz, vox_xyz, tdim, slope, intercept+slope*ignore);
	/*remove the orthogonalisation time series, if one has been supplied*/
	  if(ort_ts != (MRI_IMAGE *)0)
	    {
	    correlate_prep(MRI_FLOAT_PTR(ort_ts), vox_xyz, tdim, sy+xyz, syy+xyz);
	    correlate(MRI_FLOAT_PTR(ort_ts), vox_xyz, tdim, sx_ort, sxx_ort, sy[xyz], syy[xyz], (double *)0, &slope, &intercept);
	    orthogonalise(MRI_FLOAT_PTR(ort_ts), vox_xyz, vox_xyz, tdim, slope, intercept);
	    }
	/*correlate*/
	  correlate_prep(MRI_FLOAT_PTR(ref_ts)+ignore, vox_xyz+ignore, tdim-ignore, sy+xyz, syy+xyz);
	  correlate(MRI_FLOAT_PTR(ref_ts)+ignore, vox_xyz+ignore, tdim-ignore, sx_ref, sxx_ref, sy[xyz], syy[xyz], &corr, &slope, (double *)0);
	/*save regression coefficient*/
	  fit_coeff[xyz] = slope;
	/*retain greatest slope for use in computing scaling factor*/
	  slope = fabs(slope);
	  if(slope > *fim_scale)
	    *fim_scale = slope;
	/*save correlation coefficient*/
	  stree_insert(actual_corr, corr, x, y, z);
	/*compute sums for autocorregression computation, if requested*/
	  if(AR_order)
	    autocorrelate_sum(vox_xyz+ignore, tdim-ignore, AR_n_incl, AR_order, AR_excl, AR, &AR_var);
	  }
	xyz++;
	}
/*if autoregression has been requested, compute autocorrelations from sums...*/
  if(AR_order)
    autocorrelate_finish(&AR_order, AR_excl, AR, AR_var);
/*...& solve the Yule-Walker equations to derive autoregression coefficients
  (note that autocorrelate_finish() may have decreased AR_order, perhaps all
  the way to zero, if it has detected unrealistic autocorrelations)*/
  if(AR_order)
    {
  /*write the autocorrelation coefficients to a .1D file*/
    write_1D_file(AR, AR_order, DSET_FILECODE(dset), "_autocorrelation");
    if(yule_walker(AR, AR_order))
      {
      /*this never happens*/
      fprintf(stderr,
	"Cannot compute autoregression; Yule-Walker matrix is singular.\n");
      AR_order = 0;
      }
    else
      {
    /*write the autoregression coefficients to a .1D file*/
      write_1D_file(AR, AR_order, DSET_FILECODE(dset), "_autoregression");
    /*apply the Yule-Walker autoregression coefficients to whiten the time
      series at each voxel, iterating through the voxels just as in the loop
      above*/
      AR_ts = (float *)calloc(tdim, sizeof(*AR_ts));
      if(AR_ts == (float *)0)
	{
	fprintf(stderr, "Cannot apply autoregression: out of memory\n");
	AR_order = 0;
	}
      else
	{
	xyz = 0;
	for(z = 0; z !=  zdim; z++)
	  for(y = 0; y != ydim; y++)
	    for(x = 0; x != xdim; x++)
	      {
	      if(mask != NULL)
		{
	    /*fetch the mask brick's scaling factor...*/
		mask_val = DSET_BRICK_FACTOR(mask, 0);
		if(mask_val == 0.0)
		  mask_val = 1.0;
	    /*...and multiply it by the mask datum at coordinates (x, y, z)*/
		if(DSET_BRICK_TYPE(mask, 0) == MRI_short)
		  mask_val *= ((short *)DSET_ARRAY(mask, 0))[xyz];
		else
		  mask_val *= ((char *)DSET_ARRAY(mask, 0))[xyz];
		}
	      if((mask_val >= masklo) && (mask_val <= maskhi))
		{
		vox_xyz = vox + tdim*xyz;
		for(t = 0; t != tdim; t++)
		  {
		/*fetch the time series brick's scaling factor...*/
		  vox_xyz[t] = DSET_BRICK_FACTOR(dset, t);
		  if(vox_xyz[t] == 0.0)
		    vox_xyz[t] = 1.0;
		/*...and multiply it by the time series datum at (x, y, z, t)*/
		  vox_xyz[t] *= ((short *)DSET_ARRAY(dset, t))[xyz];
		  }
	      /*whiten, and copy the result over the original, coloured data*/
		whiten(vox_xyz, AR_ts, tdim, AR, AR_order);
		for(t = 0; t != tdim; t++)
		  vox_xyz[t] = AR_ts[t];
		}
	      xyz++;
	      }
	}
      }
    }
/*make sure that the mask left us *some* voxels*/
  if(stree_null(actual_corr))
    {
    fprintf(stderr, "PERMTEST: All voxels are masked!\n");
    return 1;
    }
  *fim_scale /= MRI_TYPE_maxval[MRI_short];
/*copy fit coefficients for all voxels, scaled for storage in shorts*/
  for(t = 0; t != xdim*ydim*zdim; t++)
    (*intensities)[t] = (short)(fit_coeff[t] / *fim_scale);
  free(fit_coeff);
  randomised_corr = dtree_create(xdim, ydim, NUM_ITERS);
  if(randomised_corr == (DTREE *)0)
    {
    stree_destroy(actual_corr);
    free(syy);
    free(sy);
    free(ts);
    free(vox);
    free(sequence);
    free(tindex);
    free(*zvals);
    free(*intensities);
    return 1;
    }
  coord_mem = (CLIST *)calloc(NUM_ITERS*num_coords, sizeof(*coord_mem));
  if(coord_mem == (CLIST *)0)
    {
    dtree_destroy(randomised_corr);
    stree_destroy(actual_corr);
    free(syy);
    free(sy);
    free(ts);
    free(vox);
    free(sequence);
    free(tindex);
    free(*zvals);
    free(*intensities);
    return 1;
    }
  coord_next = coord_mem;
/*initialise progress meter*/
  prev_percent_done = -1;
  PLUTO_popup_meter(plint);

/******************************************************************************
 * PHASE 2: Compute the empirical distribution.				      *
 ******************************************************************************/
  for(iter = 0; iter != NUM_ITERS; iter++)
    {
    percent_done = (100*iter)/NUM_ITERS;
    if(percent_done > prev_percent_done)
      {
      prev_percent_done = percent_done;
      PLUTO_set_meter(plint, percent_done);
      }
    shuffle(sequence, tsize);
    for(t = 0; t != num_coords; t++)
      coord_next[t].corr = 0.0;
    xyz = 0;
    for(z = 0; z !=  zdim; z++)
      for(y = 0; y != ydim; y++)
	for(x = 0; x != xdim; x++)
	  {
	  if(mask != NULL)
	    {
	  /*fetch the mask brick's scaling factor...*/
	    mask_val = DSET_BRICK_FACTOR(mask, 0);
	    if(mask_val == 0.0)
	      mask_val = 1.0;
	  /*...and multiply it by the mask datum at coordinates (x, y, z)*/
	    if(DSET_BRICK_TYPE(mask, 0) == MRI_short)
	      mask_val *= ((short *)DSET_ARRAY(mask, 0))[xyz];
	    else
	      mask_val *= ((char *)DSET_ARRAY(mask, 0))[xyz];
	    }
	  if((mask_val >= masklo) && (mask_val <= maskhi))
	    {
	    vox_xyz = vox + tdim*xyz;
	  /*apply the autoregression coefficients to re-colour the permuted
	    time series before correlating*/
	    if(AR_order)
	      {
	    /*fetch the time series, in the permuted order given by sequence[],
	      but with ignored and excluded points copied in their original
	      positions - these are needed in order to fill in the whole array
	      so that colour() below will have valid data at each time point.*/
	      for(t = 0; t != ignore; t++)
		ts[t] = vox_xyz[t];
	      for(t2 = 0; t != tdim; t++)
		ts[t] = ((MRI_FLOAT_PTR(ref_ts)[t] < BLAST)?
			  vox_xyz[sequence[t2++]]:
			  vox_xyz[t]);
	    /*colour the permuted time series*/
	      colour(AR_ts, ts, tdim, AR, AR_order);
	    /*copy the coloured series into the work space*/
	      for(t = 0; t != tdim; t++)
		ts[t] = AR_ts[t];
	    /*compute new sums for the permuted & re-coloured time series*/
	      correlate_prep(MRI_FLOAT_PTR(ref_ts)+ignore, ts+ignore, tdim-ignore, sy+xyz, syy+xyz);
	      }
	    else
	      {
	    /*fetch the time series, in the permuted order given by sequence[]*/
	      for((t = ignore), (t2 = 0); t != tdim; t++)
		if(MRI_FLOAT_PTR(ref_ts)[t] < BLAST)
		  ts[t] = vox_xyz[sequence[t2++]];
	      }
	    correlate(MRI_FLOAT_PTR(ref_ts)+ignore, ts+ignore, tdim-ignore, sx_ref, sxx_ref, sy[xyz], syy[xyz], &corr, (double *)0, (double *)0);
	  /*Since num_coords is tiny, any sorting algorithm faster
	    than linear insertion wouldn't be worth the trouble.*/
	    for(t = 0; t < num_coords; t++)
	      if((*magnitude)(corr) > (*magnitude)(coord_next[t].corr))
		{
		t2 = t;
		while(++t != num_coords)
		  {
		  coord_next[t].corr = coord_next[t-1].corr;
		  coord_next[t].coords = coord_next[t-1].coords;
		  }
		coord_next[t2].corr = corr;
		coord_next[t2].coords = coord_encode(x, y, z, xdim, ydim);
		}
	    }
	  xyz++;
	  }
    dtree_insert(randomised_corr, coord_next);
    if(verbose)
      printf("iter=%d max_corr=%f\n", iter, coord_next->corr);
    coord_next += num_coords;
    }
#ifdef PERMTEST_DEBUG
  dtree_traverse(randomised_corr, 0);	/*dump distribution to stdout*/
#endif
  if(AR_order)
    free(AR_ts);
  PLUTO_set_meter(plint, 100);

/******************************************************************************
 * PHASE 3: Rank correlations from the actual data set within the empirical   *
 *	    distribution.						      *
 ******************************************************************************/
  do
    {
    not_done = 0;
  /*extract the most significant *correlation* (in [-1,1])*/
    max_corr = stree_extract_max(actual_corr, &x, &y, &z);
  /*map it to an adjusted *probability* (in [0,1])*/
    corr = dtree_position(randomised_corr, max_corr);
    if(verbose)
      printf("(%2d,%2d,%2d) raw r=%f adjusted p=%f", x, y, z, max_corr, corr);
    if(((one_tailed == 0) && (fabs(corr-0.5) >= 0.5-pcrit/2.0))
     ||((one_tailed == -1) && (corr <= pcrit))
     ||((one_tailed == 1) && (corr >= 1.0-pcrit)))
	{
	slope = critz(corr);
	if(verbose)
	  printf(" zcrit=%f\n", slope);
	(*zvals)[x + xdim*(y + ydim*z)] = (short)(slope*FUNC_ZT_SCALE_SHORT);
	not_done = 1;
/******************************************************************************
 *percent completion of this phase of the algorithm can be expressed as follows:
 *	100.0 * ((one_tailed == 1)? 1.0-corr:				      *
 *		  ((one_tailed == 0) && (corr > 0.5))?			      *
 *		    2.0*(1.0-corr):					      *
 *		      corr) / pcrit;					      *
 ******************************************************************************/
      /*since this point is now defined as activated, exclude its data from the
	empirical distribution*/
	dtree_delete_coords(randomised_corr, x, y, z, *zvals);
	}
    } while(not_done && (dtree_size(randomised_corr) > NUM_ITERS/2));
			/*^^^ stop if we've deleted too many coordinates ^^^*/
  if(verbose)
    putchar('\n');
  free(coord_mem);
  dtree_destroy(randomised_corr);
  stree_destroy(actual_corr);
  free(syy);
  free(sy);
  free(ts);
  free(vox);
  free(sequence);
  free(tindex);
  return 0;
  }

/*In the event of a conflict with some future revision of the AFNI widgets
  code, PERMTEST_set_logo() and PERMTEST_reset_logo() can be redefined as null
  functions and everything will still work right (though it would be a shame to
  lose the free advertising!).*/

static Pixmap sipb_pixmap;		/*pixmap of SIPB logo*/

static void PERMTEST_set_logo(PLUGIN_interface *plint)
  {
  Pixel bg_pix=0, fg_pix=0;			/*colours from control window*/
#define sipb_width 90
#define sipb_height 90
  static char sipb_bits[] = {		/*bitmap of SIPB logo*/
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0x1f, 0x00, 0x00, 0x30, 0x00, 0x00, 0x00, 0x18, 0x00, 0x00, 0xff, 0x03, 
  0x1f, 0x00, 0x00, 0x30, 0x00, 0x00, 0x00, 0x18, 0x00, 0x00, 0xf8, 0x03, 
  0x1f, 0x00, 0x00, 0x30, 0x00, 0x00, 0x00, 0x18, 0x00, 0x00, 0xf0, 0x03, 
  0x1f, 0x00, 0x00, 0x30, 0x00, 0x00, 0x00, 0x18, 0x00, 0x00, 0xf0, 0x03, 
  0x3f, 0x00, 0x0c, 0xf0, 0x00, 0x7c, 0x00, 0x7e, 0x00, 0x03, 0xe0, 0x03, 
  0x7f, 0x00, 0x18, 0xf0, 0x01, 0xfe, 0x00, 0xff, 0x00, 0x0f, 0xe0, 0x03, 
  0x7f, 0x00, 0x30, 0xf0, 0x01, 0xfe, 0x00, 0xff, 0x00, 0x1f, 0xe0, 0x03, 
  0xff, 0x00, 0x60, 0xf0, 0x01, 0xfe, 0x00, 0xff, 0x00, 0x1f, 0xe0, 0x03, 
  0xff, 0x01, 0x60, 0xf0, 0x01, 0xfe, 0x00, 0xff, 0x00, 0x0f, 0xe0, 0x03, 
  0xff, 0x03, 0xc0, 0xff, 0x01, 0xfe, 0x00, 0xff, 0x00, 0x0f, 0xe0, 0x03, 
  0xff, 0x07, 0x80, 0xff, 0x01, 0xfe, 0x00, 0xff, 0x00, 0x03, 0xe0, 0x03, 
  0xff, 0x0f, 0x00, 0xff, 0x01, 0xfe, 0x00, 0xff, 0x00, 0x00, 0xf0, 0x03, 
  0xff, 0x1f, 0x00, 0xff, 0x01, 0xfe, 0x00, 0xff, 0x00, 0x00, 0xf8, 0x03, 
  0xff, 0x3f, 0x00, 0xff, 0x01, 0xfe, 0x00, 0xff, 0x00, 0x00, 0xe0, 0x03, 
  0xff, 0x3f, 0x00, 0xff, 0x01, 0xfe, 0x00, 0xff, 0x00, 0x00, 0xe0, 0x03, 
  0xff, 0x1f, 0x80, 0xff, 0x01, 0xfe, 0x00, 0xff, 0x00, 0x03, 0xc0, 0x03, 
  0xff, 0x0f, 0xc0, 0xff, 0x01, 0xfe, 0x00, 0xff, 0x00, 0x0f, 0xc0, 0x03, 
  0xff, 0x07, 0xe0, 0xff, 0x01, 0xfe, 0x00, 0xff, 0x00, 0x1f, 0xc0, 0x03, 
  0xff, 0x03, 0x70, 0xf0, 0x01, 0xfe, 0x00, 0xff, 0x00, 0x3f, 0xc0, 0x03, 
  0xff, 0x01, 0x78, 0xf0, 0x01, 0xfe, 0x00, 0xff, 0x00, 0x3f, 0xc0, 0x03, 
  0xff, 0x00, 0x3c, 0xf0, 0x01, 0xfe, 0x00, 0xff, 0x00, 0x1f, 0xc0, 0x03, 
  0x7f, 0x00, 0x00, 0xf0, 0x01, 0xfe, 0x00, 0xff, 0x00, 0x0f, 0xc0, 0x03, 
  0x3f, 0x00, 0x00, 0xf0, 0x00, 0x7c, 0x00, 0x7e, 0x00, 0x07, 0xc0, 0x03, 
  0x1f, 0x00, 0x00, 0x30, 0x00, 0x10, 0x00, 0x18, 0x00, 0x00, 0xe0, 0x03, 
  0x1f, 0x00, 0x00, 0x30, 0x00, 0x10, 0x00, 0x18, 0x00, 0x00, 0xf0, 0x03, 
  0x1f, 0x00, 0x00, 0x30, 0x00, 0x10, 0x00, 0x18, 0x00, 0x00, 0xf8, 0x03, 
  0x1f, 0x00, 0x00, 0x30, 0x00, 0x10, 0x00, 0x18, 0x00, 0x00, 0xff, 0x03, 
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0x40, 0xfe, 0x3f, 0x9f, 0x04, 0xfe, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0x73, 0xfe, 0x3f, 0x8e, 0x9c, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0x73, 0x38, 0x3e, 0x8e, 0x9c, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0x73, 0x92, 0x3c, 0x84, 0x9c, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0x73, 0x12, 0x3c, 0x95, 0x9c, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0x73, 0x92, 0x3f, 0x91, 0x9c, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0x73, 0x92, 0x3c, 0x9b, 0x9c, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0x73, 0x32, 0x3e, 0x9b, 0x9c, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0xc3, 0xfc, 0x9f, 0xff, 0xcf, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0x99, 0xfc, 0x9f, 0xff, 0xcf, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0xf1, 0x48, 0x86, 0xb1, 0x8c, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0xc3, 0x4c, 0x92, 0x24, 0xc9, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0x8f, 0x4c, 0x9a, 0x20, 0xc9, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0x9d, 0x4c, 0x9a, 0x3c, 0xc9, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0x99, 0x4c, 0x92, 0x24, 0xc9, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0xc3, 0x99, 0x86, 0x31, 0x99, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0xe1, 0x1f, 0xff, 0xff, 0xff, 0x99, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0xf3, 0xcf, 0xff, 0xff, 0xff, 0xf9, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0xb3, 0x8c, 0xb1, 0x48, 0x8e, 0x91, 0xb1, 0xfc, 0xff, 0x03, 
  0xff, 0xff, 0x33, 0xc9, 0x24, 0x92, 0x34, 0x99, 0x24, 0xf9, 0xff, 0x03, 
  0xff, 0xff, 0x33, 0xc9, 0x24, 0x93, 0x0c, 0x99, 0x24, 0xf9, 0xff, 0x03, 
  0xff, 0xff, 0x33, 0xc9, 0x24, 0x93, 0x24, 0x99, 0x24, 0xf9, 0xff, 0x03, 
  0xff, 0xff, 0x33, 0xc9, 0x24, 0x93, 0x24, 0x99, 0x24, 0xf9, 0xff, 0x03, 
  0xff, 0xff, 0x21, 0xc9, 0x31, 0x93, 0x4c, 0x92, 0x31, 0xf9, 0xff, 0x03, 
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0xc1, 0xff, 0xff, 0xff, 0xff, 0xf9, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0x99, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0x99, 0x12, 0xc7, 0x38, 0x8e, 0x29, 0xa7, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0x99, 0x48, 0x52, 0x92, 0x24, 0x49, 0x92, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0xc1, 0x4c, 0x72, 0x30, 0x8e, 0x49, 0x9a, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0xf9, 0x4c, 0x72, 0xfe, 0x3c, 0x49, 0x9a, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0xf9, 0x4c, 0x52, 0x92, 0x24, 0x49, 0x92, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0xf9, 0x1c, 0xc7, 0x38, 0x8e, 0x49, 0x86, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x9f, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xc7, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0xc1, 0xff, 0xff, 0xe7, 0xff, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0x99, 0xff, 0xff, 0xe7, 0xff, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0x99, 0x71, 0x2c, 0xe1, 0xff, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0xc1, 0xa4, 0x89, 0xe4, 0xff, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0x99, 0x64, 0xc8, 0xe6, 0xff, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0x99, 0x24, 0xc9, 0xe6, 0xff, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0x99, 0x24, 0xc9, 0xe4, 0xff, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0xc1, 0x71, 0xc2, 0xe1, 0xff, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x03, 
  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x03, 
  };
  /*Initialise SIPB logo, in the current controller window only.  Leave all
    other controller windows with the default MCW logo.  (Adapted from
    afni_widg:AFNI_make_widgets).*/
  XtVaGetValues(plint->im3d->vwid->top_form,
		XmNforeground, &bg_pix,     /*note reversal of fg & bg colours*/
		XmNbackground, &fg_pix, NULL);
  sipb_pixmap = XCreatePixmapFromBitmapData(
		XtDisplay(plint->im3d->vwid->top_shell),
		RootWindowOfScreen(XtScreen(plint->im3d->vwid->top_shell)),
		sipb_bits, sipb_width, sipb_height,
		fg_pix, bg_pix,
		DefaultDepthOfScreen(XtScreen(plint->im3d->vwid->top_shell)));
/*shameless self-aggrandisement (adapted from afni:AFNI_set_cursor)*/
  XtVaSetValues(plint->im3d->vwid->picture, XmNlabelPixmap, sipb_pixmap, NULL);
  }

static void PERMTEST_reset_logo(PLUGIN_interface *plint)
  {
  XtVaSetValues(plint->im3d->vwid->picture, XmNlabelPixmap, XmUNSPECIFIED_PIXMAP, NULL);
  XFreePixmap(XtDisplay(plint->im3d->vwid->top_shell), sipb_pixmap);
  }

char *PERMTEST_main(PLUGIN_interface *plint)
  {
  int t;
  char *prefix;
  MRI_IMAGE *ref_time_series, *orthogonal_time_series, *AR_excl_time_series;
  THD_3dim_dataset *dset, *mask, *new_dset;
  short *intensities, *zvals;
  double fim_scale,			/*scaling factor for short image data*/
	 pcrit;				/*alpha level*/
  char *optiontag;			/*one of tails2, tails1pos, tails1neg*/
  int one_tailed,			/*0, 1, or -1*/
	AR_order;			/*autoregressive model order*/
  float masklo, maskhi;			/*bounds on mask dataset*/
  static char tails_err[] = "exactly one of two-tailed, one-tailed positive, or one-tailed negative must be chosen";

#ifdef PERMTEST_DEBUG
  void (*handler)(int);
#endif

  if(plint == (PLUGIN_interface *)0)
    return "PERMTEST_main: null input";

/*Make sure that the input dataset exists and is stored in a format that we can
  understand (MRI_short)*/
  PLUTO_next_option(plint);
  dset = PLUTO_find_dset(PLUTO_get_idcode(plint));
  if(dset == (THD_3dim_dataset *)0)
    return "bad dataset";
  for(t = 0; t != dset->dblk->nvals; t++)
/*to do: make copies of this routine that work on MRI_byte and on MRI_float*/
    if(DSET_BRICK_TYPE(dset, t) != MRI_short)
      return("permutation test on non-short values is not implemented");

/*Make sure that the time series exists and is stored in a format that we can
  understand (MRI_float), and contains enough points to cover the time
  dimension of the specified dataset*/
  PLUTO_next_option(plint);
  ref_time_series = PLUTO_get_timeseries(plint);
  if((ref_time_series == (MRI_IMAGE *)0)
   ||(ref_time_series->kind != MRI_float)
   ||(ref_time_series->nx < DSET_NUM_TIMES(dset)))
    return("bad time series");

/*Read the orthogonalisation time series, if it exists*/
  optiontag = PLUTO_get_optiontag(plint);
  if(strcmp(optiontag, ort_label))
    orthogonal_time_series = (MRI_IMAGE *)0;
  else
    {
    orthogonal_time_series = PLUTO_get_timeseries(plint);
    if((orthogonal_time_series == (MRI_IMAGE *)0)
     ||(orthogonal_time_series->kind != MRI_float)
     ||(orthogonal_time_series->nx < DSET_NUM_TIMES(dset)))
      return("bad ort");
    PLUTO_next_option(plint);
    }

/*Make sure that the prefix specified for the new dataset is a valid prefix*/
  prefix = PLUTO_get_string(plint);
  if(!PLUTO_prefix_ok(prefix))
    return("bad prefix");

/*Read the alpha level*/
  PLUTO_next_option(plint);
  pcrit = PLUTO_get_number(plint);

/*Read the autoregressive model order, if autoregression has been specified*/
  optiontag = PLUTO_get_optiontag(plint);
  if(strcmp(optiontag, AR_order_label))
    AR_order = 0;
  else
    {
    AR_order = (int)PLUTO_get_number(plint);
    optiontag = PLUTO_get_optiontag(plint);
    }

/*Read the autoregression exclusion flags vector, if it exists*/
  if(strcmp(optiontag, AR_excl_label))
    AR_excl_time_series = (MRI_IMAGE *)0;
  else
    {
    AR_excl_time_series = PLUTO_get_timeseries(plint);
    if((AR_excl_time_series == (MRI_IMAGE *)0)
     ||(AR_excl_time_series->kind != MRI_float)
     ||(AR_excl_time_series->nx < DSET_NUM_TIMES(dset)))
      return("bad AR exclusion vector");
    optiontag = PLUTO_get_optiontag(plint);
    }

/*Exactly one of two-tailed, one-tailed positive, one-tailed negative*/
  if(optiontag == (char *)0)
    return(tails_err);
  if(strcmp(optiontag, tails2))
    {
    if(strcmp(optiontag, tails1pos))
      {
      if(strcmp(optiontag, tails1neg))
	return(tails_err);
      else
	one_tailed = -1;
      }
    else
      one_tailed = 1;
    }
  else
    one_tailed = 0;
  optiontag = PLUTO_get_optiontag(plint);
  if((optiontag != (char *)0) && strcmp(optiontag, mask_label))
    return(tails_err);

/*Optional mask dataset*/
  masklo = 1.0;
  maskhi = 32767.0;
  if(optiontag == (char *)0)
    mask = (THD_3dim_dataset *)0;
  else
    {
    mask = PLUTO_find_dset(PLUTO_get_idcode(plint));
    if(mask == (THD_3dim_dataset *)0)
      return("bad mask");
    if((DSET_BRICK_TYPE(mask, 0) != MRI_short)
    && (DSET_BRICK_TYPE(mask, 0) != MRI_byte))
      return("mask brick type must be byte or short integer");
    optiontag = PLUTO_get_optiontag(plint);
    if(optiontag != (char *)0)
      {
      if(strcmp(optiontag, masklo_label))
	maskhi = PLUTO_get_number(plint);
      else
	{
	masklo = PLUTO_get_number(plint);
	if(PLUTO_get_optiontag(plint) != (char *)0)
	  maskhi = PLUTO_get_number(plint);
	}
      }
    DSET_load(mask);
    }

#ifdef PERMTEST_DEBUG
  handler = signal(SIGUSR1, flush);
#endif

/*toot our own horn :-)*/
  PERMTEST_set_logo(plint);
/*Make sure source dataset is in memory*/
  DSET_load(dset);
  if(PERMTEST_compute(plint, dset, ref_time_series, orthogonal_time_series, pcrit, one_tailed, &intensities, &zvals, &fim_scale, mask, masklo, maskhi, 0, AR_order, AR_excl_time_series))
    {
    PERMTEST_reset_logo(plint);
    return("out of memory");
    }
  if(num_coords_exhausted)
    printf("%d of %d points (%d%%) were deleted from the distribution due to\nexhausted coordinates.  If this fraction is larger than about 10%%, consider\nrecompiling plug_permtest.so with a NUM_COORDS larger than the current value of\n%d.\n", num_coords_exhausted, NUM_ITERS, (100*num_coords_exhausted+NUM_ITERS/2)/NUM_ITERS, NUM_COORDS);

/*create the output dataset*/
  new_dset = EDIT_empty_copy(dset);
  if(EDIT_dset_items(new_dset,
	ADN_prefix, prefix,
	ADN_malloc_type, DATABLOCK_MEM_MALLOC, /*hold in r/w memory*/
	ADN_datum_all, MRI_short,	/*store as (scaled) short ints*/
	ADN_nvals, 2,			/*2 sub-bricks: intensity + z-score*/
	ADN_ntt, 0,			/*no time dimension*/
	ADN_type, ISHEAD(dset)?
	  HEAD_FUNC_TYPE: GEN_FUNC_TYPE, /*functional image*/
	ADN_func_type, FUNC_ZT_TYPE,	/*intensity + z-score*/
	ADN_none))
    {
    PERMTEST_reset_logo(plint);
    return("EDIT_dset_items error");
    }
  EDIT_BRICK_LABEL(new_dset, 0, "Fit Coef");
  mri_fix_data_pointer(intensities, DSET_BRICK(new_dset, 0));
  EDIT_BRICK_FACTOR(new_dset, 0, (float)fim_scale);
  EDIT_BRICK_LABEL(new_dset, 1, "z-score");
  EDIT_BRICK_TO_FIZT(new_dset, 1);
  mri_fix_data_pointer(zvals, DSET_BRICK(new_dset, 1));
  EDIT_BRICK_FACTOR(new_dset, 1, (float)(1.0/FUNC_ZT_SCALE_SHORT));
  DSET_overwrite(new_dset);
  PLUTO_add_dset(plint, new_dset, DSET_ACTION_MAKE_CURRENT);
#ifdef PERMTEST_DEBUG
  signal(SIGUSR1, handler);
#endif
  PERMTEST_reset_logo(plint);
  return (char *)0;
  }


DEFINE_PLUGIN_PROTOTYPE

PLUGIN_interface *PLUGIN_init(int ncall)
  {
  PLUGIN_interface *plint;
  if(ncall > 0)
    return (PLUGIN_interface *)0;	/*only one interface*/
/*set titles and entry point*/
  plint = PLUTO_new_interface("Permutation Test", hint, help, PLUGIN_CALL_VIA_MENU, PERMTEST_main);
  PLUTO_add_hint(plint, hint);
/*first line of dialogue box: input dataset*/
  PLUTO_add_option(plint, input_label, input_label, TRUE);
  PLUTO_add_dataset(plint, "Dataset", ANAT_SPGR_MASK | ANAT_EPI_MASK, 0, DIMEN_4D_MASK | BRICK_SHORT_MASK);
/*second line of dialogue box: input time series*/
  PLUTO_add_option(plint, ts_label, ts_label, TRUE);
  PLUTO_add_timeseries(plint, "Reference Time Series");
/*third line of dialogue box: time series against which to orthogonalise*/
  PLUTO_add_option(plint, ort_label, ort_label, FALSE);
  PLUTO_add_timeseries(plint, "Orthogonalisation Time Series");
/*fourth line of dialogue box: output dataset*/
  PLUTO_add_option(plint, output_label, output_label, TRUE);
  PLUTO_add_string(plint, "Prefix", 0, (char **)0, 19);
/*fifth line of dialogue box: alpha level (range 10^-4..1, default 0.05)*/
  PLUTO_add_option(plint, alpha_label, alpha_label, TRUE);
  PLUTO_add_number(plint, "alpha level", 1, 10000, 4, 500, 1);
/*sixth line of dialogue box: autoregressive model order*/
  PLUTO_add_option(plint, AR_order_label, AR_order_label, FALSE);
  PLUTO_add_number(plint, "autoregressive model order", 0, 32, 0, 1, 1);
/*seventh line of dialogue box: autoregression exclusion vector*/
  PLUTO_add_option(plint, AR_excl_label, AR_excl_label, FALSE);
  PLUTO_add_timeseries(plint, "Autoregression Exclusion Flags");
/*penultimate lines of dialogue box: tail options*/
  PLUTO_add_option(plint, tails2, tails2, FALSE);
  PLUTO_add_option(plint, tails1pos, tails1pos, FALSE);
  PLUTO_add_option(plint, tails1neg, tails1neg, FALSE);
/*last lines of dialogue box: mask dataset and bounds*/
  PLUTO_add_option(plint, mask_label, mask_label, FALSE);
  PLUTO_add_dataset(plint, "mask dataset", 0, FUNC_FIM_MASK, DIMEN_3D_MASK | BRICK_SHORT_MASK | BRICK_BYTE_MASK);
  PLUTO_add_option(plint, masklo_label, masklo_label, FALSE);
  PLUTO_add_number(plint, "voxel is masked if >=", 0, 0x7fff, 0, 1, 1);
  PLUTO_add_option(plint, maskhi_label, maskhi_label, FALSE);
  PLUTO_add_number(plint, "voxel is masked if <=", 0, 0x7fff, 0, 1, 1);
  return plint;
  }
