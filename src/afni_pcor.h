#ifndef _AFNI_PCOR_HEADER_
#define _AFNI_PCOR_HEADER_

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     header file for recursive partial correlation calculations
     -- RWCox, Feb 1994
     -- March 1995: this work is the basis for the paper
         "Real-Time Functional Magnetic Resonance Imaging"
         by R.W. Cox, A. Jesmanowicz, and J.S. Hyde,
         Magn. Res. Med. 33:230-236.
     -- June 1995: added get_variance and some comments
     -- June 1996: modified to fit into AFNI
     -- Jan  2000: added get_stdev
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "mrilib.h"

/*** some macros ***/

/** combine two interpreted tokens into one using TWO_TWO **/

#ifndef TWO_TWO
#  define TWO_ONE(x,y) x ## y
#  define TWO_TWO(x,y) TWO_ONE(x,y)
#endif

#ifndef  SQR
#  define SQR(x)  ((x)*(x))
#endif

#ifndef MAX
#  define MAX(x,y) (((x)>(y)) ? (x) : (y))
#endif

#ifndef MIN
#  define MIN(x,y) (((x)<(y)) ? (x) : (y))
#endif

/*** default is to expand the inner loop in update_voxel_corr for small
     values of nref;  if this is not desired, comment the next line out ***/

#define EXPAND_UPDATE

#define REF_EPS  1.0e-7

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/*** this type holds the independent-of-voxel references information ***/

   /* note that the partial correlation coefficient of the data vectors
      is calculated with respect to the LAST reference vector,
      removing the effects of all the previous reference vectors */

typedef struct {
          int nref ;                /* number of ref vectors */
          int nupdate ;             /* number of times has been updated */
          float **chol ,            /* nref X nref Cholesky factor */
                *alp , *ff , *gg ;  /* alpha, f, and g factors */
          float *rmin , *rmax ,     /* 14 Jan 1998 additions: */
                *rsum ;             /* statistics of references */
          float betasq ;            /* 1/beta^2 factor */
   } PCOR_references ;

 /*** RCH is for access to the (ii,jj) element
      of the Cholesky factor of a given set of references;
      ii >= jj, since this is the lower triangular factor

      N.B.: compared to the paper's notation,
            L+1     = nref
            c_{i,j} = RCH(rr,i-1,j-1)  for i=1..L+1 , j=1..i,
        and c_{i,j} = VCH(vv,vox,j-1)  for i=L+2, j=1..L+2    ***/

#define RCH(rr,ii,jj)  (rr->chol[(ii)][(jj)])

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/*** this type holds all the voxel-specific data ***/

typedef struct {
          int nvox ;      /* number of voxels */
          int nref ;      /* number of references to allow for */
          int nupdate ;   /* number of times it has been updated */
          float *chrow ;  /* last row (length nref+1)
                             of Cholesky factor for each voxel
                             (N.B.:  last element is actually squared) */
   } PCOR_voxel_corr ;

   /* VCH is for access to the jj-th element of the last row of
      the Cholesky factor for the vox-th voxel in a given voxel_corr struct;

      N.B.:  the last element [VCH(vv,vox,nref)] is the SQUARE of
             the actual Cholesky diagonal element, since that is all that the
             partial correlation coefficient algorithm actually needs;
             this prevents taking a square root at each time step for each voxel */

#define VCH(vv,vox,jj) (vv->chrow[(jj)+(vox)*(vv->nref+1)])

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/*** prototypes ***/

extern PCOR_references * new_PCOR_references(int) ;
extern PCOR_voxel_corr * new_PCOR_voxel_corr(int,int) ;

extern void free_PCOR_voxel_corr(PCOR_voxel_corr *) ;
extern void free_PCOR_references(PCOR_references *) ;

extern void PCOR_get_lsqfit(PCOR_references *, PCOR_voxel_corr *, float *fit[]) ;
extern void PCOR_get_coef  (PCOR_references *, PCOR_voxel_corr *, float *) ;
extern void PCOR_get_pcor  (PCOR_references *, PCOR_voxel_corr *, float *) ;
extern void PCOR_get_mcor  (PCOR_references *, PCOR_voxel_corr *, int , float *) ;
extern void PCOR_get_perc  (PCOR_references *, PCOR_voxel_corr *, float *,float *, int );

extern void PCOR_get_pcor_and_coef(PCOR_references *, PCOR_voxel_corr *,
                                   float, float *,float * ) ;

extern void PCOR_get_variance(PCOR_voxel_corr *, float *) ;
extern void PCOR_get_stdev   (PCOR_voxel_corr *, float *) ;  /* 03 Jan 2000 */

extern void update_PCOR_references(float *,PCOR_references *) ;

extern void PCOR_update_short( short * , PCOR_references * , PCOR_voxel_corr * ) ;
extern void PCOR_update_float( float * , PCOR_references * , PCOR_voxel_corr * ) ;
extern void PCOR_update_byte ( byte  * , PCOR_references * , PCOR_voxel_corr * ) ;

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
#endif
