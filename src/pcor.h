/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
/***
     header file for recursive partial correlation calculations
     -- RWCox, Feb 1994
     -- March 1995: this work is the basis for the paper
         "Real-Time Functional Magnetic Resonance Imaging"
         by RW Cox, A Jesmanowicz, and JS Hyde,
         Magnetic Resonance in Medicine, 33:230-236.
     -- June 1995: added get_variance and some comments
***/

#ifndef PCOR_HEADER
#define PCOR_HEADER

#include <stdlib.h>
#include <stdio.h>
#include <math.h>

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/*** some macros ***/

#ifndef  SQR
#define SQR(x)  ((x)*(x))
#endif

#ifndef MAX
#  define MAX(x,y) (((x)>(y)) ? (x) : (y))
#endif

#ifndef MIN
#  define MIN(x,y) (((x)<(y)) ? (x) : (y))
#endif

/*** default is to expand the inner loop in update_voxel_corr for
     small values of nref;  if this is not desired, compile with
     the switch -DNO_EXPAND_UPDATE, or comment the next line out ***/

#define EXPAND_UPDATE

#ifdef NO_EXPAND_UPDATE
#undef EXPAND_UPDATE
#endif

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/*** Define atomic data types ***/

#ifndef REF_FLOAT_SINGLE           /* compile time option to save space */
   typedef double ref_float ;      /* internal storage of reference data */
#  define REF_EPS  1.0e-13         /* a small number of this type */
#else
   typedef float ref_float ;
#  define REF_EPS  1.0e-7
#endif  /* REF_FLOAT_SINGLE */

/*-------------------------------*/

#ifndef VOX_SHORT                  /* define voxel data as shorts or floats */
   typedef float vox_data ;
#else
   typedef short vox_data ;
#endif  /* VOX_SHORT */

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/*** this type holds the independent-of-voxel references information ***/

   /* note that the partial correlation coefficient of the data vectors
      is calculated with respect to the LAST reference vector,
      removing the effects of all the previous reference vectors */

typedef struct {
          int nref ;                    /* number of ref vectors */
          int nupdate ;                 /* number of times has been updated */
          ref_float **chol ,            /* nref X nref Cholesky factor */
                    *alp , *ff , *gg ;  /* alpha, f, and g factors */
          ref_float betasq ;            /* 1/beta^2 factor */
   } references ;

 /*** RCH is for access to the (ii,jj) element
      of the Cholesky factor of a given set of references;
      ii >= jj, since this is the lower triangular factor

      N.B.: compared to the paper's notation,
            L+1     = nref
            c_{i,j} = RCH(rr,i-1,j-1)  for i=1..L+1 , j=1..i,
        and c_{i,j} = VCH(vv,vox,j-1)  for i=L+2, j=1..L+2    ***/

#define RCH(rr,ii,jj)  (rr->chol[(ii)][(jj)])

#ifdef OV_DEBUG1
#define REF_DUMP(rr,str) \
  {  int iq,jq ; ref_float qsum ; \
     fprintf(stderr,"%s: reference dump, nref=%d betasq=%11.4e\n", \
             str , rr->nref , rr->betasq ) ; \
     for( iq=0 ; iq < rr->nref ; iq++){ \
       fprintf(stderr," ROW %d: ",iq) ; \
       qsum = 0.0 ; \
       for( jq=0 ; jq <= iq ; jq++ ){ \
          fprintf(stderr,"%11.4e ",RCH(rr,iq,jq)); \
          qsum += SQR(RCH(rr,iq,jq)) ; \
       } \
       fprintf(stderr,": qsum=%11.4e\n",qsum) ; \
       fprintf(stderr,"      alpha=%11.4e  ff=%11.4e  gg=%11.4e\n", \
               rr->alp[iq] , rr->ff[iq] , rr->gg[iq] ) ; \
     } \
   }
#endif

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/*** this type holds all the voxel-specific data ***/

typedef struct {
          int nvox ;          /* number of voxels */
          int nref ;          /* number of references to allow for */
          int nupdate ;       /* number of times it has been updated */
          ref_float *chrow ;  /* last row (length nref+1)
                                 of Cholesky factor for each voxel
                                 (N.B.:  last element is actually squared) */
   } voxel_corr ;

   /* VCH is for access to the jj-th element of the last row of
      the Cholesky factor for the vox-th voxel in a given voxel_corr struct;

      N.B.:  the last element [VCH(vv,vox,nref)] is the SQUARE of
             the actual Cholesky diagonal element, since that is all that the
             partial correlation coefficient algorithm actually needs;
             this prevents taking a square root at each time step for each voxel */

#define VCH(vv,vox,jj) (vv->chrow[(jj)+(vox)*(vv->nref+1)])

#ifdef OV_DEBUG1
#define VD 2001
#define VOX_DUMP(vv,vox,str) \
   {  int jq ; ref_float qsum = 0.0 ; \
      fprintf(stderr,"%s: voxel_corr dump #%d\n  ",str,vox) ; \
      for( jq=0 ; jq < vv->nref ; jq++ ){ \
        fprintf(stderr,"%11.4e ",VCH(vv,vox,jq)) ; \
        qsum += SQR(VCH(vv,vox,jq)) ; \
      } \
      qsum += VCH(vv,vox,vv->nref) ; \
      fprintf(stderr,"%11.4e : qsum=%11.4e\n",VCH(vv,vox,vv->nref),qsum); \
   }
#endif

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/*** this type holds the return data for thresholding results ***/

typedef struct {
          int   num_pcor_pos , num_pcor_neg , num_coef_pos , num_coef_neg ;
          float max_pcor_pos , max_pcor_neg , max_coef_pos , max_coef_neg ;
   } thresh_result ;

#ifdef OV_DEBUG1
#define THR_DUMP(thr,str) \
fprintf(stderr,"thresh_results dump for %s\n:") ; \
fprintf(stderr," num_pcor_pos=%d neg=%d  num_coef_pos=%d neg=%d\n", \
 (thr).num_pcor_pos,(thr).num_pcor_neg,(thr).num_coef_pos,(thr).num_coef_neg ) ; \
fprintf(stderr," max_pcor_pos=%11.4g neg=%11.4g  max_coef_pos=%11.4g neg=%11.4g\n", \
 (thr).max_pcor_pos,(thr).max_pcor_neg,(thr).max_coef_pos,(thr).max_coef_neg ) ;
#endif

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/*** prototypes ***/

extern references * new_references() ;

extern void update_references() ;

extern voxel_corr * new_voxel_corr() ;

extern void free_voxel_corr() ;

extern void free_references() ;

extern void update_voxel_corr() ;

extern void get_pcor() ;

extern void get_coef() ;

extern void get_pcor_thresh_coef() ;

extern void get_variances() ;

extern void get_lsqfit() ;

#endif
