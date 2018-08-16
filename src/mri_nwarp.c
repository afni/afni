
/*****************************************************************************/
/***    Generally, this set of functions is designed to be #include-d      ***/
/***    and compiled with OpenMP enabled (USE_OMP #define-d), for speed    ***/
/*****************************************************************************/

#include "mrilib.h"
#include "r_new_resam_dset.h"

#ifdef USE_OMP
# include <omp.h>
# define NUM_DHARRAY 6
  static int    nthmax=1 ;      /* size of temp arrays below:  */
  static double *dhaar=NULL ;   /* must be set in main program */
  static double *dhbbr=NULL ;   /* see 3dQwarp.c for example   */
  static double *dhccr=NULL ;
  static double *dhddr=NULL ;   /* these arrays are used to store */
  static double *dheer=NULL ;   /* cumulative values on a per-thread */
  static double *dhffr=NULL ;   /* basis for final summation later */
#else
# define nthmax 1
  static double dhaar[1] ;      /* with only 1 'thread' in OpenMP-free */
  static double dhbbr[1] ;      /* code, these arrays can be fixed size */
  static double dhccr[1] ;
  static double dhddr[1] ;
  static double dheer[1] ;
  static double dhffr[1] ;
#endif

#define DEBUG_CATLIST

/*..........................................................................*/
/** Note that the functions for 3dQwarp (4000+ lines of code)
    are only compiled if macro ALLOW_QWARP is #define-d -- see 3dQwarp.c.
    Also note that the 'plusminus' warping will only be compiled if the
    macro ALLOW_PLUSMINUS is also #define-d.
*//*........................................................................*/

#ifdef ALLOW_QWARP
#include "thd_incorrelate.c"   /* for the 3dQwarp cost (INCOR) functions */
#endif

/* macro to free a pointer if it's not NULL */

#undef  FREEIFNN
#define FREEIFNN(x) do{ if((x)!=NULL){ free((void *)(x)); (x)=NULL;} } while(0)

/* minimum num grid points in a given direction */
/* also is minimum patch size for 3dQwarp funcs */

#undef  NGMIN
#define NGMIN   5        /* if Hngmin goes this small, things become VERY slow!! */
#undef  NGMINS
#define NGMINS "5"       /* string version of the above -- must match! */

#undef  NGMIN_Q
#define NGMIN_Q 7        /* smallest grid allowed for quintic warp */

#undef  NGMIN_PLUS_3     /* smallest grid for basis5 warp */
#define NGMIN_PLUS_3 11

#undef  NGMAX_PLUS_3
#define NGMAX_PLUS_3 23  /* largest grid for basis5 warp (-5final) */

#undef  NGMIN_PLUS_2     /* smallest grid for basis4 warp */
#define NGMIN_PLUS_2 9

#undef  NGMAX_PLUS_2
#define NGMAX_PLUS_2 23  /* largest grid for basis4 warp (-4final) */

#undef  NGMIN_PLUS_1
#define NGMIN_PLUS_1 7   /* smallest grid for basis3 warp */

#undef  NGMAX_PLUS_1
#define NGMAX_PLUS_1 23  /* largest grid for basis3 warp (-3final) */

#undef  NVOXMAX_PLUS
#define NVOXMAX_PLUS 12168 /* 23^3+1 */

#define WARP_CODE_STRING(wc)                         \
          (  (wc == MRI_QUINTIC)       ? "quintic"   \
           : (wc == MRI_CUBIC_PLUS_1 ) ? "cubic+1"   \
           : (wc == MRI_CUBIC_PLUS_2 ) ? "cubic+2"   \
           : (wc == MRI_CUBIC_PLUS_3 ) ? "cubic+3" : "  cubic" )

/* control verbosity of mri_nwarp functions */

static int verb_nww = 0 ;
void NwarpCalcRPN_verb(int i){ verb_nww = i; }

/* other verbosity (mostly for Qwarp, far far below) */

static int Hverb = 1 ;

/*---------------------------------------------------------------------------*/
/*######   Blocks of code indicated in the Table of Contents below      #####*/
/*######   are each surrounded by #if 1 ... #endif pairs, to make it    #####*/
/*######   easy to skip between the beginning and end of code blocks.   #####*/
/*######   (At least, easy using vi and the % key for match-jumping.)   #####*/
/*---------------------------------------------------------------------------*/

#if 1 /*(TOC)*/
/*===========================================================================*/
/*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/*----------------------------------------------------------------------------
------- Table of Contents -- e.g., search forward for the string (C4) --------

     (C1) Prolegomenon, Definitions, and Background
     (C2) Functions for computing external slopes for a warp
     (C3) Functions for elementary operations on warps
     (C4) Functions for extending or truncating a warp grid size
     (C5) Functions to compute various 'norms' (size estimates) of a warp
     (C6) Functions that do conversions between index warps and dataset warps
     (C7) Functions to compute auxiliary functions of an index warp, including
          stuff for the 3dQwarp penalties, and the 3dNwarpFuncs output
     (C8) Functions for interpolating all 3 components of an index warp at once
     (C9) Functions to carry out warp compositions
    (C10) Functions to invert an index warp in its entirety
    (C11) Functions to compute the 'square root' of a warp
    (C12) Functions for taking stuff from 3dAllineate -nwarp output and
          producing a warp (obsolete, or nearly so)
    (C13) Functions for interpolating images
    (C14) Functions used in 3dNwarpXYZ.c to warp a small number of points given
          by x,y,z triples, rather than warping a whole grid at once
    (C15) Functions to warp a dataset, given a warp defined by a dataset
    (C16) Reverse Polish Notation warp calculator (3dNwarpCalc program)
    (C17) Functions for reading warps and inverting/catenating them right away
          (for the '-nwarp' input of various 3dNwarp programs)

    The following functions are only compiled if ALLOW_QWARP is #define-d:

     (Q1) Introduction to 3dQwarp, and Global variables for 3dQwarp-ing
          (includes an outline of the sequence of function calls for 3dQwarp)
     (Q2) Basis functions (cubic and quintic) for warping
     (Q3) Functions to create a patch warp from basis functions and parameters
     (Q4) Evaluate the incrementally warped source image at the combination
          of the current global warp and the local patch warp
     (Q5) Functions to evaluate the warp penalty and cost function
     (Q6) Functions to setup global variables for warp optimization process
     (Q7) Function that actually optimizes one incremental patch warp
     (Q8) Functions that drive the warp searching process by looping over
          patches of shrinking sizes
     (Q9) Functions for duplo-ing a warp or image (up and down in size)
    (Q10) Function for warp optimization with duplo-ization
    (Q11) All the above functions copied and edited for plusminus warping
-----------------------------------------------------------------------------*/
/*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/*===========================================================================*/

#endif /*(TOC)*/ /*##########################################################*/

#if 1
/*===========================================================================*/
/*---------- (C1) Prolegomenon -----------------------------------------------

   This file contains the AFNI functions for nonlinear warping, which are
   wrapped up in various ways in the programs 3dQwarp.c and 3dNwarp*.c;
   as mentioned earlier, this file is intended to be #include-d into a
   program, although it is also compiled into the AFNI libmri.  The 3dQwarp
   functions are only compiled if macro ALLOW_QWARP is #define-d before this
   file is #include-d, since they are only used in the 3dQwarp.c program and
   they comprise about half of this file.

--------------- Definition of an Index Warp Struct ---------------------------

   The functions herein mostly operate on IndexWarp3D structs.  Such a struct
   represents an "index warp in 3D" -- that is, it maps indexes and not
   coordinates.  Most functions that operate on these things start with
   the string 'IW3D_'.  The basic struct is pretty simple (from mrilib.h):

      typedef struct {
        int    nx ,  ny ,  nz ;
        float *xd , *yd , *zd , *hv , *je , *se ;
        int   use_es ;
        float es_xd_xp, es_xd_xm, es_xd_yp, es_xd_ym, es_xd_zp, es_xd_zm,
              es_yd_xp, es_yd_xm, es_yd_yp, es_yd_ym, es_yd_zp, es_yd_zm,
              es_zd_xp, es_zd_xm, es_zd_yp, es_zd_ym, es_zd_zp, es_zd_zm ;
        mat44 cmat , imat ;
        char *geomstring ;
        int view ;
      } IndexWarp3D ;

      nx, ny, nz = dimensions of the grid arrays (nz==1 is allowed)
      xd, yd, zd = index displacements = arrays of size nx*ny*nz;
                   the voxel at 3D index (i,j,k) is mapped to
                     i + xd[i+j*nx+k*nx*ny]
                     j + yd[i+j*nx+k*nx*ny]
                     k + zd[i+j*nx+k*nx*ny]
      hv         = volume of the warped cube (hexahedron)
                     [i,i+1] X [j,j+1] X [k,k+1]
      je         = bulk volume distortion energy (part of 3dQwarp penalty)
      se         = shear and vorticity energy (other part of penalty);
                 --> hv, je, se are usually NULL, and are what gets calculated
                     in 3dNwarpFuncs via function IW3D_load_bsv()
      use_es     = flag to use external slopes;
                     if this is nonzero, then for (i,j,k) outside the grid,
                     the es_* variables are used to linearly extrapolate the
                     edge displacements -- this helps in warp inversion and
                     other places
      es_Ad_BS   = external slope for displacement Ad (A=x or y or z),
       where         at the B face (B=x or y or z) in the S direction
       A=x,y, or z   (S=m or p; short for 'minus' and 'plus').  For example,
       B=x,y, or z   es_xd_ym is the external slope for the xd displacement
       S=m or p      at the y-minus face (y=0) -- so that if the displacement
                     is needed at (i,j,k) for j < 0, then the value returned is
                     xd[i+0*nx+k*nx*ny] + es_xd_ym*j -- assuming i and k are
                     inside the grid ranges 0..nx-1 and 0..nz-1.
                   * See function IW3D_interp_linear() for a full example
                     of how the external slopes are used.
                   * These external slopes are calculated via function
                     IW3D_load_external_slopes() by taking the outer
                     5 layers along each face and computing the mean slope
                     in the face direction (j=0..4 in the example above).

         The components below are for relating the IndexWarp3D
         struct to a 'real' space 3D dataset:

      cmat       = matrix to get DICOM x,y,z coordinates from i,j,k
      imat       = inverse of cmat (convert coordinate to index)
      geomstring = string to represent geometry of 3D dataset that
                     corresponds to this index warp; if the index warp
                     was created directly from a dataset via function
                     IW3D_from_dataset(), then that's when geomstring is
                     set, otherwise it will be set using function
                     IW3D_adopt_dataset() -- along with the other
                     dataset-ish stuff (cmat, imat, view).
      view       = view code for that dataset (should be upgraded to space)

   There is also a struct IndexWarp3D_pair, which holds 2 warps, one the
   forward warp and one the inverse warp via function IW3D_invert().  This
   struct doesn't get used much, as it turns out.  At one time, I thought
   I would always keep the warp inverse around, at least for input and
   output to datasets (cf. infra), but that didn't happen.

--------------- Warps stored as 3D Datasets -----------------------------------

   Externally, warps are stored as 3D datasets, with (at least) 3 sub-bricks:
   one for each of the x, y, and z displacements.  A warp dataset is created
   from an IndexWarp3D struct using function IW3D_to_dataset(), with the
   geomstring component providing the key information about how to lay out
   the 3D dataset grid.  The xd, yd, zd index displacements are converted
   to DICOM x,y,z displacments in mm using the cmat component.  Note, however,
   that the dataset itself may not be stored in DICOM RAI order, but that the
   displacments always refer to DICOM ordered vectors.  This can be a little
   confusing, so beware.

   Incidentally, storing a warp as a set of displacements has a number of
   advantages over storing the actual displaced locations (i.e., indexes or
   coordinates).  For one, the identity warp is just a big collection of
   zeros.  For another, interpolation to a different grid is simpler.

-------------- Understanding Warps and Displacements --------------------------

   Since warps are stored as displacements, the actual transformation of
   (index) coordinates is

     i -> i + xd[i,j,k]  (loosely speaking)
     j -> j + yd[i,j,k]
     k -> k + zd[i,j,k]

   so you will see various places where the addition of the original
   coordinates is needed; for example, when composing 2 warps in function
   IW3D_compose(), we have to compute B(A(x)), where the warps are defined
   by A(x) = x + a(x) and B(x) = x + b(x), so B(A(x)) = x+a(x) + b(x+a(x))
   -- and so b() must be interpolated at these non-integer index values --
   which explains why the output warp is computed as a(x)+b(x+a(x)) -- the
   double appearance of a(x) is required by this storage mechanism.

   In a more complicated case, in inverting an index warp, the following
   composition is needed in function IW3D_invert_newt(): B(2x-A(B(x))).
   This computation is carried out by the following sequence of steps
      A(x)              = x + a(x)  [by definition]
      B(x)              = x + b(x)  [by definition]
      A(B(x))           = x + b(x) + a(x+b(x))
      2x - A(B(x))      = x - b(x) - a(x+b(x))
      B( 2x - A(B(x)) ) = x - b(x) - a(x+b(x)) + b(x-b(x)-a(x+b(x)))
   You should understand how this expansion works in order to be able to
   understand any of the warp manipulation functions.

--------------- Forward and Inverse Warps -------------------------------------

   A nonlinear warp stores the displacements (in indexes or DICOM mm) from a
   base dataset grid to a source dataset grid.  For computing the source dataset
   warped to the base dataset grid, these displacements are needed, so that for
   each grid point in the output (warped) dataset, the corresponding location in
   the source dataset can be found.  That is, this 'forward' warp is good for
   finding where a given point in the base dataset maps to in the source dataset.
   However, for finding where a given point in the source dataset maps to in the
   base dataset, the 'inverse' warp is needed.

--------------- OpenMP code ---------------------------------------------------

   A large number of compute intensive functions use OpenMP to parallelize
   the lengthy computations.  This makes the source code somewhat more
   complicated and less transparent than it could otherwise be.  If you
   need some tutorial on OpenMP, start with
     https://afni.nimh.nih.gov/pub/dist/doc/misc/OpenMP.html

-----------------------------------------------------------------------------*/
/*===========================================================================*/

#endif /*(C1)*/ /*###########################################################*/

#if 1
/*===========================================================================*/
/* (C2) Functions below are for computing the external slopes for a warp.    */
/*===========================================================================*/

/*---------------------------------------------------------------------------*/
/* The following macros are for loading and unloading the extra slope
   variables for extending a warp outside its domain of definition.
*//*-------------------------------------------------------------------------*/

/* declare 18 local variables to hold the external slopes */

#define ES_DECLARE_FLOATS \
  float es_xd_xp=0.0f, es_xd_xm=0.0f, es_xd_yp=0.0f, es_xd_ym=0.0f, es_xd_zp=0.0f, es_xd_zm=0.0f, \
        es_yd_xp=0.0f, es_yd_xm=0.0f, es_yd_yp=0.0f, es_yd_ym=0.0f, es_yd_zp=0.0f, es_yd_zm=0.0f, \
        es_zd_xp=0.0f, es_zd_xm=0.0f, es_zd_yp=0.0f, es_zd_ym=0.0f, es_zd_zp=0.0f, es_zd_zm=0.0f

/* pack the 18 external slopes in a warp struct into an array */

#define ES_PACK(AA,eqq)                                                            \
 do{ int qw=0 ;                                                                    \
     eqq[qw++] = AA->es_xd_xp; eqq[qw++] = AA->es_xd_xm; eqq[qw++] = AA->es_xd_yp; \
     eqq[qw++] = AA->es_xd_ym; eqq[qw++] = AA->es_xd_zp; eqq[qw++] = AA->es_xd_zm; \
     eqq[qw++] = AA->es_yd_xp; eqq[qw++] = AA->es_yd_xm; eqq[qw++] = AA->es_yd_yp; \
     eqq[qw++] = AA->es_yd_ym; eqq[qw++] = AA->es_yd_zp; eqq[qw++] = AA->es_yd_zm; \
     eqq[qw++] = AA->es_zd_xp; eqq[qw++] = AA->es_zd_xm; eqq[qw++] = AA->es_zd_yp; \
     eqq[qw++] = AA->es_zd_ym; eqq[qw++] = AA->es_zd_zp; eqq[qw++] = AA->es_zd_zm; \
 } while(0)

/* pack the 18 external slopes in local variables into an array */

#define ES_PACKVEC(eqq)                                                \
 do{ int qw=0 ;                                                        \
     eqq[qw++] = es_xd_xp; eqq[qw++] = es_xd_xm; eqq[qw++] = es_xd_yp; \
     eqq[qw++] = es_xd_ym; eqq[qw++] = es_xd_zp; eqq[qw++] = es_xd_zm; \
     eqq[qw++] = es_yd_xp; eqq[qw++] = es_yd_xm; eqq[qw++] = es_yd_yp; \
     eqq[qw++] = es_yd_ym; eqq[qw++] = es_yd_zp; eqq[qw++] = es_yd_zm; \
     eqq[qw++] = es_zd_xp; eqq[qw++] = es_zd_xm; eqq[qw++] = es_zd_yp; \
     eqq[qw++] = es_zd_ym; eqq[qw++] = es_zd_zp; eqq[qw++] = es_zd_zm; \
 } while(0)

/* unpack the 18 local external slope variables from an array */

#define ES_UNPACKVEC(eqq)                                              \
 do{ int qw=0 ;                                                        \
     es_xd_xp = eqq[qw++]; es_xd_xm = eqq[qw++]; es_xd_yp = eqq[qw++]; \
     es_xd_ym = eqq[qw++]; es_xd_zp = eqq[qw++]; es_xd_zm = eqq[qw++]; \
     es_yd_xp = eqq[qw++]; es_yd_xm = eqq[qw++]; es_yd_yp = eqq[qw++]; \
     es_yd_ym = eqq[qw++]; es_yd_zp = eqq[qw++]; es_yd_zm = eqq[qw++]; \
     es_zd_xp = eqq[qw++]; es_zd_xm = eqq[qw++]; es_zd_yp = eqq[qw++]; \
     es_zd_ym = eqq[qw++]; es_zd_zp = eqq[qw++]; es_zd_zm = eqq[qw++]; \
 } while(0)

/* unpack the 18 local external slope variables from a warp struct */

#define ES_WARP_TO_LOCAL(AA)                       \
 do{ es_xd_xp=AA->es_xd_xp; es_xd_xm=AA->es_xd_xm; \
     es_yd_xp=AA->es_yd_xp; es_yd_xm=AA->es_yd_xm; \
     es_zd_xp=AA->es_zd_xp; es_zd_xm=AA->es_zd_xm; \
     es_xd_yp=AA->es_xd_yp; es_xd_ym=AA->es_xd_ym; \
     es_yd_yp=AA->es_yd_yp; es_yd_ym=AA->es_yd_ym; \
     es_zd_yp=AA->es_zd_yp; es_zd_ym=AA->es_zd_ym; \
     es_xd_zp=AA->es_xd_zp; es_xd_zm=AA->es_xd_zm; \
     es_yd_zp=AA->es_yd_zp; es_yd_zm=AA->es_yd_zm; \
     es_zd_zp=AA->es_zd_zp; es_zd_zm=AA->es_zd_zm; } while(0)


/*---------------------------------------------------------------------------*/
/* The following functions are for providing a linear extension to a warp,
    based on the final 3..5 layers in the warp.
    These macros compute the slope based on 3, 4, or 5 points.
*//*-------------------------------------------------------------------------*/

#define SLOPE_3(x0,x1,x2) ( -0.5f*(x0+x1+x2)                            \
                           + 0.5f*(x1+2.0f*x2) )

#define SLOPE_4(x0,x1,x2,x3) ( -0.3f*(x0+x1+x2+x3)                      \
                               +0.2f*(x1+2.0f*x2+3.0f*x3) )

#define SLOPE_5(x0,x1,x2,x3,x4) ( -0.2f*(x0+x1+x2+x3+x4)                \
                                  +0.1f*(x1+2.0f*x2+3.0f*x3+4.0f*x4) )

/*---------------------------------------------------------------------------*/
/* This function returns the MEAN slope from a collection of vectors;
   that is, the slope is calculated using 3, 4, or 5 points
   (one from each vector), and then these are averaged to give the output.
   In this library, these vectors are formed from the outer layers of the
   faces of the warp in function IW3D_load_external_slopes().
*//*-------------------------------------------------------------------------*/

static float mean_slope( int nvec , int veclen , float **vec )
{
   float ssum=0.0f ; int iv ;

   if( nvec < 3 || veclen < 1 || vec == NULL ) return ssum ;

   switch( nvec ){
     case 3:
       for( iv=0 ; iv < veclen ; iv++ )
         ssum += SLOPE_3(vec[0][iv],vec[1][iv],vec[2][iv]) ;
     break ;

     case 4:
       for( iv=0 ; iv < veclen ; iv++ )
         ssum += SLOPE_4(vec[0][iv],vec[1][iv],vec[2][iv],vec[3][iv]) ;
     break ;

     default:
     case 5:
       for( iv=0 ; iv < veclen ; iv++ )
         ssum += SLOPE_5(vec[0][iv],vec[1][iv],vec[2][iv],vec[3][iv],vec[4][iv]) ;
     break ;
   }

   return (ssum/veclen) ;
}

/*---------------------------------------------------------------------------*/
/* Initialize the external slopes in a warp, to zero (of course). */

void IW3D_zero_external_slopes( IndexWarp3D *AA )
{
   if( AA == NULL ) return ;
   AA->use_es = 0 ;
      AA->es_xd_xp = AA->es_xd_xm = AA->es_xd_yp = AA->es_xd_ym = AA->es_xd_zp = AA->es_xd_zm
    = AA->es_yd_xp = AA->es_yd_xm = AA->es_yd_yp = AA->es_yd_ym = AA->es_yd_zp = AA->es_yd_zm
    = AA->es_zd_xp = AA->es_zd_xm = AA->es_zd_yp = AA->es_zd_ym = AA->es_zd_zp = AA->es_zd_zm = 0.0f ;
   return ;
}

/*----------------------------------------------------------------------------*/
/* Compute the external slopes in a warp, using the mean_slope() function
   above applied to each face, extracting the vectors from the face's layers.
   Code for deciphering the slope names, where 'Q' is 'x' or 'y' or 'z'
      es = external slope
      Qd = Q direction displacment
      Qm = along the Q face in the minus direction
      Qp = along the Q face in the plus direction
   Note that the slopes are always in the positive direction of the coordinate
   being changes, even at the minus faces.  For example, es_xd_xm > 0 means
   that for the x-index being negative (outside the 'xm' face), then the
   x-displacement ('xd') will get a negative addition of es_xd_xm * x-index
   (multiplying the positive slope times the negative index).

   This code is pretty simple, just repetitive (6 faces times 3 displacments).
*//*--------------------------------------------------------------------------*/

void IW3D_load_external_slopes( IndexWarp3D *AA )
{
   int nx,ny,nz,nxy , veclen,nvec , pp,qq,rr,ss , nbig ;
   float **vec , *dar ;

ENTRY("IW3D_load_external_slopes") ;

   if( AA == NULL ) EXRETURN ;

   nx = AA->nx ; ny = AA->ny ; nz = AA->nz ; nxy = nx*ny ;  /* dimensions */

   /* make space for the vectors to extract */

   nbig = MAX(nx,ny) ; nbig = MAX(nbig,nz) ; nbig = nbig*nbig ;
   vec = (float **)malloc(sizeof(float *)*5) ;
   for( pp=0 ; pp < 5 ; pp++ ) vec[pp] = (float *)malloc(sizeof(float)*nbig) ;

#define DD(i,j,k) dar[(i)+(j)*nx+(k)*nxy]  /* access dar[i,j,k] */

   /* x=0 and x=nx-1 faces */

     /* how many surface slabs to use (5 or 4 or 3) */

   nvec = nx/3 ; if( nvec > 5 ) nvec = 5 ; else if( nvec < 3 ) nvec = 0 ;
   veclen = ny*nz ;  /* size of the x faces */

   dar = AA->xd ;                   /* extract x displacements at x=0 face */
   for( pp=0 ; pp < nvec ; pp++ ){
     for( ss=qq=0 ; qq < ny ; qq++ ){
       for( rr=0 ; rr < nz ; rr++ ) vec[pp][ss++] = DD(pp,qq,rr) ;
   }}
   AA->es_xd_xm = mean_slope( nvec , veclen , vec ) ; /* x minus face */

   for( pp=0 ; pp < nvec ; pp++ ){  /* at x=nx-1 face */
     for( ss=qq=0 ; qq < ny ; qq++ ){
       for( rr=0 ; rr < nz ; rr++ ) vec[pp][ss++] = DD(nx-1-pp,qq,rr) ;
   }}
   /* below, we have to negate the mean_slope() output since
      we loaded the faces in the order of decreasing x-index */
   AA->es_xd_xp = - mean_slope( nvec , veclen , vec ) ; /* x plus face */

   /** below here, edited copies for all combinations of
       displacement vectors (xd,yd,zd) and face (xm,xp,ym,yp,zm,zp) **/

   dar = AA->yd ;                   /* y displacements */
   for( pp=0 ; pp < nvec ; pp++ ){
     for( ss=qq=0 ; qq < ny ; qq++ ){
       for( rr=0 ; rr < nz ; rr++ ) vec[pp][ss++] = DD(pp,qq,rr) ;
   }}
   AA->es_yd_xm = mean_slope( nvec , veclen , vec ) ; /* x minus face */

   for( pp=0 ; pp < nvec ; pp++ ){
     for( ss=qq=0 ; qq < ny ; qq++ ){
       for( rr=0 ; rr < nz ; rr++ ) vec[pp][ss++] = DD(nx-1-pp,qq,rr) ;
   }}
   AA->es_yd_xp = - mean_slope( nvec , veclen , vec ) ; /* x plus face */

   dar = AA->zd ;                   /* z displacments */
   for( pp=0 ; pp < nvec ; pp++ ){
     for( ss=qq=0 ; qq < ny ; qq++ ){
       for( rr=0 ; rr < nz ; rr++ ) vec[pp][ss++] = DD(pp,qq,rr) ;
   }}
   AA->es_zd_xm = mean_slope( nvec , veclen , vec ) ; /* x minus face */

   for( pp=0 ; pp < nvec ; pp++ ){
     for( ss=qq=0 ; qq < ny ; qq++ ){
       for( rr=0 ; rr < nz ; rr++ ) vec[pp][ss++] = DD(nx-1-pp,qq,rr) ;
   }}
   AA->es_zd_xp = - mean_slope( nvec , veclen , vec ) ; /* x plus face */

   /* repeat the above for the y=0 and y=ny-1 faces */

   nvec = ny/3 ; if( nvec > 5 ) nvec = 5 ; else if( nvec < 3 ) nvec = 0 ;
   veclen = nz*nx ;

   dar = AA->xd ;
   for( pp=0 ; pp < nvec ; pp++ ){
     for( ss=qq=0 ; qq < nz ; qq++ ){
       for( rr=0 ; rr < nx ; rr++ ) vec[pp][ss++] = DD(rr,pp,qq) ;
   }}
   AA->es_xd_ym = mean_slope( nvec , veclen , vec ) ;

   for( pp=0 ; pp < nvec ; pp++ ){
     for( ss=qq=0 ; qq < nz ; qq++ ){
       for( rr=0 ; rr < nx ; rr++ ) vec[pp][ss++] = DD(rr,ny-1-pp,qq) ;
   }}
   AA->es_xd_yp = - mean_slope( nvec , veclen , vec ) ;

   dar = AA->yd ;
   for( pp=0 ; pp < nvec ; pp++ ){
     for( ss=qq=0 ; qq < nz ; qq++ ){
       for( rr=0 ; rr < nx ; rr++ ) vec[pp][ss++] = DD(rr,pp,qq) ;
   }}
   AA->es_yd_ym = mean_slope( nvec , veclen , vec ) ;

   for( pp=0 ; pp < nvec ; pp++ ){
     for( ss=qq=0 ; qq < nz ; qq++ ){
       for( rr=0 ; rr < nx ; rr++ ) vec[pp][ss++] = DD(rr,ny-1-pp,qq) ;
   }}
   AA->es_yd_yp = - mean_slope( nvec , veclen , vec ) ;

   dar = AA->zd ;
   for( pp=0 ; pp < nvec ; pp++ ){
     for( ss=qq=0 ; qq < nz ; qq++ ){
       for( rr=0 ; rr < nx ; rr++ ) vec[pp][ss++] = DD(rr,pp,qq) ;
   }}
   AA->es_zd_ym = mean_slope( nvec , veclen , vec ) ;

   for( pp=0 ; pp < nvec ; pp++ ){
     for( ss=qq=0 ; qq < nz ; qq++ ){
       for( rr=0 ; rr < nx ; rr++ ) vec[pp][ss++] = DD(rr,ny-1-pp,qq) ;
   }}
   AA->es_zd_yp = - mean_slope( nvec , veclen , vec ) ;

   /* and for the z=0 and z=nz-1 faces */

   nvec = nz/3 ; if( nvec > 5 ) nvec = 5 ; else if( nvec < 3 ) nvec = 0 ;
   veclen = nx*ny ;

   dar = AA->xd ;
   for( pp=0 ; pp < nvec ; pp++ ){
     for( ss=qq=0 ; qq < nx ; qq++ ){
       for( rr=0 ; rr < ny ; rr++ ) vec[pp][ss++] = DD(qq,rr,pp) ;
   }}
   AA->es_xd_zm = mean_slope( nvec , veclen , vec ) ;

   for( pp=0 ; pp < nvec ; pp++ ){
     for( ss=qq=0 ; qq < nx ; qq++ ){
       for( rr=0 ; rr < ny ; rr++ ) vec[pp][ss++] = DD(qq,rr,nz-1-pp) ;
   }}
   AA->es_xd_zp = - mean_slope( nvec , veclen , vec ) ;

   dar = AA->yd ;
   for( pp=0 ; pp < nvec ; pp++ ){
     for( ss=qq=0 ; qq < nx ; qq++ ){
       for( rr=0 ; rr < ny ; rr++ ) vec[pp][ss++] = DD(qq,rr,pp) ;
   }}
   AA->es_yd_zm = mean_slope( nvec , veclen , vec ) ;

   for( pp=0 ; pp < nvec ; pp++ ){
     for( ss=qq=0 ; qq < nx ; qq++ ){
       for( rr=0 ; rr < ny ; rr++ ) vec[pp][ss++] = DD(qq,rr,nz-1-pp) ;
   }}
   AA->es_yd_zp = - mean_slope( nvec , veclen , vec ) ;

   dar = AA->zd ;
   for( pp=0 ; pp < nvec ; pp++ ){
     for( ss=qq=0 ; qq < nx ; qq++ ){
       for( rr=0 ; rr < ny ; rr++ ) vec[pp][ss++] = DD(qq,rr,pp) ;
   }}
   AA->es_zd_zm = mean_slope( nvec , veclen , vec ) ;

   for( pp=0 ; pp < nvec ; pp++ ){
     for( ss=qq=0 ; qq < nx ; qq++ ){
       for( rr=0 ; rr < ny ; rr++ ) vec[pp][ss++] = DD(qq,rr,nz-1-pp) ;
   }}
   AA->es_zd_zp = - mean_slope( nvec , veclen , vec ) ;

#undef DD

   /* trash the workspace vectors */

   for( pp=0 ; pp < 5 ; pp++ ) free(vec[pp]) ;
   free(vec) ;

   AA->use_es = 1 ;
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/* Very similar to above, but for warps stored in a dataset; here, the
   external slopes (in mm per voxel) are stored in the output vector.
   This is used in the 3dNwarpXYZ.c functions defined below in (C14),
   but not elsewhere.
*//*-------------------------------------------------------------------------*/

floatvec * THD_nwarp_external_slopes( THD_3dim_dataset *dset_nwarp )
{
   int nx,ny,nz,nxy , veclen,nvec , pp,qq,rr,ss , nbig ;
   float **vec , *dar ;
   floatvec *esv=NULL ;
   ES_DECLARE_FLOATS ;

ENTRY("THD_nwarp_external_slopes") ;

   if( dset_nwarp == NULL || DSET_NVALS(dset_nwarp) < 3 ) RETURN(NULL) ;
   if( DSET_BRICK_TYPE(dset_nwarp,0) != MRI_float       ) RETURN(NULL) ;
   DSET_load(dset_nwarp) ; if( !DSET_LOADED(dset_nwarp) ) RETURN(NULL) ;

   nx = DSET_NX(dset_nwarp); ny = DSET_NY(dset_nwarp); nz = DSET_NZ(dset_nwarp); nxy = nx*ny;

   /* make space for the vectors to extract */

   nbig = MAX(nx,ny) ; nbig = MAX(nbig,nz) ; nbig = nbig*nbig ;
   vec = (float **)malloc(sizeof(float *)*5) ;
   for( pp=0 ; pp < 5 ; pp++ ) vec[pp] = (float *)malloc(sizeof(float)*nbig) ;

#define DD(i,j,k) dar[(i)+(j)*nx+(k)*nxy]

   /* x=0 and x=nx-1 faces */

   nvec = nx/3 ; if( nvec > 5 ) nvec = 5 ; else if( nvec < 3 ) nvec = 0 ;
   veclen = ny*nz ;

   dar = (float *)DSET_ARRAY(dset_nwarp,0) ;  /* x displacements */
   for( pp=0 ; pp < nvec ; pp++ ){
     for( ss=qq=0 ; qq < ny ; qq++ ){
       for( rr=0 ; rr < nz ; rr++ ) vec[pp][ss++] = DD(pp,qq,rr) ;
   }}
   es_xd_xm = mean_slope( nvec , veclen , vec ) ; /* x minus face */

   for( pp=0 ; pp < nvec ; pp++ ){
     for( ss=qq=0 ; qq < ny ; qq++ ){
       for( rr=0 ; rr < nz ; rr++ ) vec[pp][ss++] = DD(nx-1-pp,qq,rr) ;
   }}
   es_xd_xp = - mean_slope( nvec , veclen , vec ) ; /* x plus face */

   dar = (float *)DSET_ARRAY(dset_nwarp,1) ;  /* y displacements */
   for( pp=0 ; pp < nvec ; pp++ ){
     for( ss=qq=0 ; qq < ny ; qq++ ){
       for( rr=0 ; rr < nz ; rr++ ) vec[pp][ss++] = DD(pp,qq,rr) ;
   }}
   es_yd_xm = mean_slope( nvec , veclen , vec ) ; /* x minus face */

   for( pp=0 ; pp < nvec ; pp++ ){
     for( ss=qq=0 ; qq < ny ; qq++ ){
       for( rr=0 ; rr < nz ; rr++ ) vec[pp][ss++] = DD(nx-1-pp,qq,rr) ;
   }}
   es_yd_xp = - mean_slope( nvec , veclen , vec ) ; /* x plus face */

   dar = (float *)DSET_ARRAY(dset_nwarp,2) ;  /* z displacements */
   for( pp=0 ; pp < nvec ; pp++ ){
     for( ss=qq=0 ; qq < ny ; qq++ ){
       for( rr=0 ; rr < nz ; rr++ ) vec[pp][ss++] = DD(pp,qq,rr) ;
   }}
   es_zd_xm = mean_slope( nvec , veclen , vec ) ; /* x minus face */

   for( pp=0 ; pp < nvec ; pp++ ){
     for( ss=qq=0 ; qq < ny ; qq++ ){
       for( rr=0 ; rr < nz ; rr++ ) vec[pp][ss++] = DD(nx-1-pp,qq,rr) ;
   }}
   es_zd_xp = - mean_slope( nvec , veclen , vec ) ; /* x plus face */

   /* repeat the above for the y=0 and y=ny-1 faces */

   nvec = ny/3 ; if( nvec > 5 ) nvec = 5 ; else if( nvec < 3 ) nvec = 0 ;
   veclen = nz*nx ;

   dar = (float *)DSET_ARRAY(dset_nwarp,0) ;
   for( pp=0 ; pp < nvec ; pp++ ){
     for( ss=qq=0 ; qq < nz ; qq++ ){
       for( rr=0 ; rr < nx ; rr++ ) vec[pp][ss++] = DD(rr,pp,qq) ;
   }}
   es_xd_ym = mean_slope( nvec , veclen , vec ) ;

   for( pp=0 ; pp < nvec ; pp++ ){
     for( ss=qq=0 ; qq < nz ; qq++ ){
       for( rr=0 ; rr < nx ; rr++ ) vec[pp][ss++] = DD(rr,ny-1-pp,qq) ;
   }}
   es_xd_yp = - mean_slope( nvec , veclen , vec ) ;

   dar = (float *)DSET_ARRAY(dset_nwarp,1) ;
   for( pp=0 ; pp < nvec ; pp++ ){
     for( ss=qq=0 ; qq < nz ; qq++ ){
       for( rr=0 ; rr < nx ; rr++ ) vec[pp][ss++] = DD(rr,pp,qq) ;
   }}
   es_yd_ym = mean_slope( nvec , veclen , vec ) ;

   for( pp=0 ; pp < nvec ; pp++ ){
     for( ss=qq=0 ; qq < nz ; qq++ ){
       for( rr=0 ; rr < nx ; rr++ ) vec[pp][ss++] = DD(rr,ny-1-pp,qq) ;
   }}
   es_yd_yp = - mean_slope( nvec , veclen , vec ) ;

   dar = (float *)DSET_ARRAY(dset_nwarp,2) ;
   for( pp=0 ; pp < nvec ; pp++ ){
     for( ss=qq=0 ; qq < nz ; qq++ ){
       for( rr=0 ; rr < nx ; rr++ ) vec[pp][ss++] = DD(rr,pp,qq) ;
   }}
   es_zd_ym = mean_slope( nvec , veclen , vec ) ;

   for( pp=0 ; pp < nvec ; pp++ ){
     for( ss=qq=0 ; qq < nz ; qq++ ){
       for( rr=0 ; rr < nx ; rr++ ) vec[pp][ss++] = DD(rr,ny-1-pp,qq) ;
   }}
   es_zd_yp = - mean_slope( nvec , veclen , vec ) ;

   /* and for the z=0 and z=nz-1 faces */

   nvec = nz/3 ; if( nvec > 5 ) nvec = 5 ; else if( nvec < 3 ) nvec = 0 ;
   veclen = nx*ny ;

   dar = (float *)DSET_ARRAY(dset_nwarp,0) ;
   for( pp=0 ; pp < nvec ; pp++ ){
     for( ss=qq=0 ; qq < nx ; qq++ ){
       for( rr=0 ; rr < ny ; rr++ ) vec[pp][ss++] = DD(qq,rr,pp) ;
   }}
   es_xd_zm = mean_slope( nvec , veclen , vec ) ;

   for( pp=0 ; pp < nvec ; pp++ ){
     for( ss=qq=0 ; qq < nx ; qq++ ){
       for( rr=0 ; rr < ny ; rr++ ) vec[pp][ss++] = DD(qq,rr,nz-1-pp) ;
   }}
   es_xd_zp = - mean_slope( nvec , veclen , vec ) ;

   dar = (float *)DSET_ARRAY(dset_nwarp,1) ;
   for( pp=0 ; pp < nvec ; pp++ ){
     for( ss=qq=0 ; qq < nx ; qq++ ){
       for( rr=0 ; rr < ny ; rr++ ) vec[pp][ss++] = DD(qq,rr,pp) ;
   }}
   es_yd_zm = mean_slope( nvec , veclen , vec ) ;

   for( pp=0 ; pp < nvec ; pp++ ){
     for( ss=qq=0 ; qq < nx ; qq++ ){
       for( rr=0 ; rr < ny ; rr++ ) vec[pp][ss++] = DD(qq,rr,nz-1-pp) ;
   }}
   es_yd_zp = - mean_slope( nvec , veclen , vec ) ;

   dar = (float *)DSET_ARRAY(dset_nwarp,2) ;
   for( pp=0 ; pp < nvec ; pp++ ){
     for( ss=qq=0 ; qq < nx ; qq++ ){
       for( rr=0 ; rr < ny ; rr++ ) vec[pp][ss++] = DD(qq,rr,pp) ;
   }}
   es_zd_zm = mean_slope( nvec , veclen , vec ) ;

   for( pp=0 ; pp < nvec ; pp++ ){
     for( ss=qq=0 ; qq < nx ; qq++ ){
       for( rr=0 ; rr < ny ; rr++ ) vec[pp][ss++] = DD(qq,rr,nz-1-pp) ;
   }}
   es_zd_zp = - mean_slope( nvec , veclen , vec ) ;

#undef DD

   for( pp=0 ; pp < 5 ; pp++ ) free(vec[pp]) ;
   free(vec) ;

   MAKE_floatvec(esv,18) ; ES_PACKVEC(esv->ar) ;
   RETURN(esv) ;
}

/* End of external slope calculation stuff */
/*===========================================================================*/

#endif /*(C2)*/ /*###########################################################*/

#if 1
/*===========================================================================*/
/* (C3) Functions below are for elementary operations on warps;
        creation, destruction, copying, scaling, etc.  Plus warp pair stuff. */
/*===========================================================================*/

/* access 3D array far[i,j,k] where ni, nij are pre-computed access factors */

#undef  FSUB
#define FSUB(far,i,j,k,ni,nij) far[(i)+(j)*(ni)+(k)*(nij)]

/*---------------------------------------------------------------------------*/
/* Creation of a warp, ex nihilo! */

IndexWarp3D * IW3D_create( int nx , int ny , int nz )
{
   IndexWarp3D *AA ;

ENTRY("IW3D_create") ;

   if( nx < NGMIN && ny < NGMIN && nz < NGMIN ) RETURN(NULL) ;

   AA = (IndexWarp3D *)calloc(1,sizeof(IndexWarp3D)) ;

   AA->nx = nx ; AA->ny = ny ; AA->nz = nz ;

   AA->xd = (float *)calloc(nx*ny*nz,sizeof(float)) ;    /* zero filled */
   AA->yd = (float *)calloc(nx*ny*nz,sizeof(float)) ;  /* displacements */
   AA->zd = (float *)calloc(nx*ny*nz,sizeof(float)) ;

   AA->hv = NULL ;                      /* to be filled in later, maybe */
   AA->je = NULL ;                      /* to be filled in later, maybe */
   AA->se = NULL ;                      /* to be filled in later, maybe */

   LOAD_IDENT_MAT44(AA->cmat) ;   /* dataset geometry currently unknown */
   LOAD_IDENT_MAT44(AA->imat) ;
   IW3D_zero_external_slopes(AA) ;
   AA->geomstring = NULL ;
   AA->view = VIEW_ORIGINAL_TYPE ;

   RETURN(AA) ;
}

/*---------------------------------------------------------------------------*/
/* Into the valley of death rode the warp!
   mallocs to the left of them, callocs to the right!
*//*-------------------------------------------------------------------------*/

void IW3D_destroy( IndexWarp3D *AA )
{
ENTRY("IW3D_destroy") ;
   if( AA != NULL ){
     FREEIFNN(AA->xd); FREEIFNN(AA->yd); FREEIFNN(AA->zd);
     FREEIFNN(AA->hv); FREEIFNN(AA->je); FREEIFNN(AA->se);
     FREEIFNN(AA->geomstring) ;
     free(AA);
   }
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/* Fill a warp with zero displacments */

void IW3D_zero_fill( IndexWarp3D *AA )
{
   size_t nbyt  ;

ENTRY("IW3D_zero_fill") ;

   if( AA == NULL ) EXRETURN ;
   nbyt = sizeof(float) * AA->nx * AA->ny * AA->nz ;
   if( AA->xd != NULL ) AAmemset( AA->xd , 0 , nbyt ) ;
   if( AA->yd != NULL ) AAmemset( AA->yd , 0 , nbyt ) ;
   if( AA->zd != NULL ) AAmemset( AA->zd , 0 , nbyt ) ;
   if( AA->hv != NULL ) AAmemset( AA->hv , 0 , nbyt ) ;
   if( AA->je != NULL ) AAmemset( AA->je , 0 , nbyt ) ;
   if( AA->se != NULL ) AAmemset( AA->se , 0 , nbyt ) ;
   IW3D_zero_external_slopes(AA) ;
   EXRETURN ;
}

/*----------------------------------------------------------------------------*/
/* return 1 if warp is filled with all zero displacements, 0 otherwise */

int IW3D_is_zero( IndexWarp3D *AA )
{
   int ii , nvox ; float *xd, *yd, *zd ;
   if( AA == NULL ) return 0 ;
   xd = AA->xd ; if( xd == NULL ) return 0 ;
   yd = AA->yd ; if( yd == NULL ) return 0 ;
   zd = AA->zd ; if( zd == NULL ) return 0 ;
   nvox = AA->nx * AA->ny * AA->nz ;
   for( ii=0 ; ii < nvox ; ii++ )
     if( xd[ii] != 0.0f || yd[ii] != 0.0f || zd[ii] != 0.0f ) return 0 ;
   return 1 ;
}

/*----------------------------------------------------------------------------*/
/* Make the geometry fields of an index warp match that of a dataset. */

void IW3D_adopt_dataset( IndexWarp3D *AA , THD_3dim_dataset *dset )
{
   mat44 cmat , imat ; char *gstr ;

ENTRY("IW3D_adopt_dataset") ;

   if( AA == NULL || !ISVALID_DSET(dset) ) EXRETURN ;

   /* check for grid mismatch error */

   if( DSET_NX(dset) != AA->nx || DSET_NY(dset) != AA->ny || DSET_NZ(dset) != AA->nz ){
     ERROR_message("IW3D_adopt_dataset: grid mismatch\n"
                   "     AA(%d,%d,%d) doesn't match dataset %s(%d,%d,%d)" ,
                   AA->nx , AA->ny , AA->nz ,
                   DSET_NX(dset) , DSET_NY(dset) , DSET_NZ(dset) ) ;
     EXRETURN ;
   }

   if( !ISVALID_MAT44(dset->daxes->ijk_to_dicom) )  /* get this matrix */
     THD_daxes_to_mat44(dset->daxes) ;              /* if it's not OK */

   DSET_CHECKAXES_REAL(dset) ;

   cmat = dset->daxes->ijk_to_dicom ;  /* takes ijk to xyz */
   imat = MAT44_INV(cmat) ;            /* takes xyz to ijk */

   AA->cmat = cmat ; AA->imat = imat ;
   gstr = EDIT_get_geometry_string(dset) ;
   if( gstr != NULL ) AA->geomstring = gstr ;
   else               AA->geomstring = NULL ;
   AA->view = dset->view_type ;

#if 0
INFO_message("IW3D_adopt_dataset geomstring = %s",AA->geomstring) ;
DUMP_MAT44("IW3D_adopt_dataset cmat",cmat) ;
#endif

   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/* Warp pairs aren't used much; at one time, I thought they'd be used more
   (keep a copy of the inverse warp along with the forward warp at all times),
   but that just turned out not to happen.
*//*-------------------------------------------------------------------------*/

IndexWarp3D_pair * IW3D_pair_insert( IndexWarp3D *AA , IndexWarp3D *BB )
{
   IndexWarp3D_pair *PP ;

   PP = (IndexWarp3D_pair *)malloc(sizeof(IndexWarp3D_pair)) ;
   PP->fwarp = AA ;  /* forward warp */
   PP->iwarp = BB ;  /* inverse warp */
   return PP ;
}

/*---------------------------------------------------------------------------*/
/* Compute the inverse warp and insert it into the pair */

void IW3D_pair_invertify( IndexWarp3D_pair *PP )
{
ENTRY("IW3D_pair_invertify") ;
   if( PP == NULL || PP->fwarp == NULL ) EXRETURN ;
   IW3D_destroy( PP->iwarp ) ;
   PP->iwarp = IW3D_invert( PP->fwarp , NULL , MRI_QUINTIC ) ;
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/* Swap the forward and inverse warp pointers */

void IW3D_pair_swapify( IndexWarp3D_pair *PP )
{
   IndexWarp3D *AA ;
   if( PP == NULL ) return ;
   AA = PP->fwarp ; PP->fwarp = PP->iwarp ; PP->iwarp = AA ;
   return ;
}

/*---------------------------------------------------------------------------*/
/* Delete a warp pair from the universe */

void IW3D_pair_destroy( IndexWarp3D_pair *PP )
{
   if( PP != NULL ){
     IW3D_destroy(PP->fwarp) ;
     IW3D_destroy(PP->iwarp) ;
     free(PP) ;
   }
   return ;
}

/*---------------------------------------------------------------------------*/
/* A 'vacant' warp doesn't contain pointers to displacements;
   presumably, these will be supplied later (or the warp is a placeholder).
*//*-------------------------------------------------------------------------*/

IndexWarp3D * IW3D_create_vacant( int nx , int ny , int nz )
{
   IndexWarp3D *AA ;

ENTRY("IW3D_create_vacant") ;

   if( nx < NGMIN && ny < NGMIN && nz < NGMIN ) RETURN(NULL) ;

   AA = (IndexWarp3D *)calloc(1,sizeof(IndexWarp3D)) ;
   AA->nx = nx ; AA->ny = ny ; AA->nz = nz ;
   AA->xd = NULL ; AA->yd = NULL ; AA->zd = NULL ;
   AA->hv = NULL ; AA->je = NULL ; AA->se = NULL ;
   LOAD_IDENT_MAT44(AA->cmat) ;
   LOAD_IDENT_MAT44(AA->imat) ;
   IW3D_zero_external_slopes(AA) ;
   AA->geomstring = NULL ;
   AA->view = VIEW_ORIGINAL_TYPE ;

   RETURN(AA) ;
}

/*---------------------------------------------------------------------------*/
/* Create a copy of a dataset, but with zero-filled displacements */

IndexWarp3D * IW3D_empty_copy( IndexWarp3D *AA )
{
   IndexWarp3D *BB ; int nxyz ;

ENTRY("IW3D_empty_copy") ;

   if( AA == NULL ) RETURN(NULL) ;

   BB = IW3D_create( AA->nx , AA->ny , AA->nz ) ;

   BB->cmat = AA->cmat ; BB->imat = AA->imat ;
   IW3D_zero_external_slopes(BB) ;

   if( AA->geomstring != NULL )
     BB->geomstring = strdup(AA->geomstring) ;

   BB->view = AA->view ;

   RETURN(BB) ;
}

/*---------------------------------------------------------------------------*/
/* Make a copy, scaling displacements by fac. */

IndexWarp3D * IW3D_copy( IndexWarp3D *AA , float fac )
{
   IndexWarp3D *BB ; int nxyz ;

ENTRY("IW3D_copy") ;

   if( AA == NULL ) RETURN(NULL) ;

   BB = IW3D_empty_copy(AA) ;  /* all zero displacements */

   nxyz = AA->nx * AA->ny * AA->nz ;

   if( fac == 1.0f ){
     AAmemcpy( BB->xd , AA->xd , sizeof(float)*nxyz ) ;
     AAmemcpy( BB->yd , AA->yd , sizeof(float)*nxyz ) ;
     AAmemcpy( BB->zd , AA->zd , sizeof(float)*nxyz ) ;
   } else if( fac != 0.0f ){
     int qq ;
     for( qq=0 ; qq < nxyz ; qq++ ){
       BB->xd[qq] = fac * AA->xd[qq] ;
       BB->yd[qq] = fac * AA->yd[qq] ;
       BB->zd[qq] = fac * AA->zd[qq] ;
     }
   }
   IW3D_load_external_slopes(BB) ;

   RETURN(BB) ;
}

/*---------------------------------------------------------------------------*/
/* Same as IW3D_copy(), but for pairs -- this probably makes no sense */

IndexWarp3D_pair * IW3D_pair_copy( IndexWarp3D_pair *AA , float fac )
{
   IndexWarp3D_pair *BB ;

   if( AA == NULL ) return NULL ;

   BB = (IndexWarp3D_pair *)malloc(sizeof(IndexWarp3D_pair)) ;
   BB->fwarp = IW3D_copy( AA->fwarp , fac ) ;
   BB->iwarp = IW3D_copy( AA->iwarp , fac ) ;
   return BB ;
}

/*---------------------------------------------------------------------------*/
/* Scale displacements by fac in-place -- not like IW3D_copy() */

void IW3D_scale( IndexWarp3D *AA , float fac )
{
   int nxyz , qq ;

ENTRY("IW3D_scale") ;

   if( AA == NULL || fac == 1.0f ) EXRETURN ;

   nxyz = AA->nx * AA->ny * AA->nz ;

   for( qq=0 ; qq < nxyz ; qq++ ){
     AA->xd[qq] *= fac ;
     AA->yd[qq] *= fac ;
     AA->zd[qq] *= fac ;
   }
   IW3D_load_external_slopes(AA) ;

   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/* Scale displacements by 3 separate factors (in-place). */

void IW3D_3scale( IndexWarp3D *AA , float xfac, float yfac, float zfac )
{
   int nxyz , qq ;

ENTRY("IW3D_3scale") ;

   if( AA == NULL ) EXRETURN ;

   nxyz = AA->nx * AA->ny * AA->nz ;

   for( qq=0 ; qq < nxyz ; qq++ ){
     AA->xd[qq] *= xfac ;
     AA->yd[qq] *= yfac ;
     AA->zd[qq] *= zfac ;
   }
   IW3D_load_external_slopes(AA) ;

   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/* Sum two warp displacements, with factors thrown in for fun. */

IndexWarp3D * IW3D_sum( IndexWarp3D *AA, float Afac, IndexWarp3D *BB, float Bfac )
{
   IndexWarp3D *CC ; int nxyz , qq ;

ENTRY("IW3D_sum") ;

   if( AA == NULL && BB == NULL ) RETURN(NULL) ;

   if( AA == NULL || Afac == 0.0f ){              /* is one input not used? */
     CC = IW3D_copy( BB , Bfac ) ; RETURN(CC) ;
   } else if( BB == NULL || Bfac == 0.0f ){
     CC = IW3D_copy( AA , Afac ) ; RETURN(CC) ;
   }

   nxyz = AA->nx * AA->ny * AA->nz ;

   if( BB->nx * BB->ny * BB->nz != nxyz ) RETURN(NULL) ;  /* bad inputs! */

   CC = IW3D_empty_copy(AA) ;  /* all zero displacements */

   for( qq=0 ; qq < nxyz ; qq++ ){
     CC->xd[qq] = Afac * AA->xd[qq] + Bfac * BB->xd[qq] ;
     CC->yd[qq] = Afac * AA->yd[qq] + Bfac * BB->yd[qq] ;
     CC->zd[qq] = Afac * AA->zd[qq] + Bfac * BB->zd[qq] ;
   }
   IW3D_load_external_slopes(CC) ;

   RETURN(CC) ;
}

#endif /*(C3)*/ /*###########################################################*/

#if 1
/*===========================================================================*/
/* (C4) The functions below extend or truncate a warp grid.                  */
/*===========================================================================*/

/*---------------------------------------------------------------------------*/
/* Extend (and/or truncate) an index warp.
   Extension outside the original grid is via linear extrapolation,
   unless zpad != 0, in which case extensions of the displacements outside
   the original grid are just set to zero.
*//*-------------------------------------------------------------------------*/

IndexWarp3D * IW3D_extend( IndexWarp3D *AA , int nxbot , int nxtop ,
                                             int nybot , int nytop ,
                                             int nzbot , int nztop , int zpad )
{
   IndexWarp3D *BB ;
   int nxold,nyold,nzold , nxnew,nynew,nznew ;

ENTRY("IW3D_extend") ;

   if( AA == NULL ) RETURN(NULL) ;

   nxold = AA->nx ; nyold = AA->ny ; nzold = AA->nz ;
   nxnew = nxold + nxbot + nxtop ; if( nxnew < 1 ) RETURN(NULL) ;
   nynew = nyold + nybot + nytop ; if( nynew < 1 ) RETURN(NULL) ;
   nznew = nzold + nzbot + nztop ; if( nznew < 1 ) RETURN(NULL) ;

   BB = IW3D_create_vacant( nxnew , nynew , nznew ) ;

   /* here, any extension is zero-padded by EDIT_volpad */

   if( AA->xd != NULL )
     BB->xd = (float *)EDIT_volpad( nxbot,nxtop, nybot,nytop, nzbot,nztop,
                                    nxold,nyold,nzold , MRI_float , AA->xd ) ;
   if( AA->yd != NULL )
     BB->yd = (float *)EDIT_volpad( nxbot,nxtop, nybot,nytop, nzbot,nztop,
                                    nxold,nyold,nzold , MRI_float , AA->yd ) ;
   if( AA->zd != NULL )
     BB->zd = (float *)EDIT_volpad( nxbot,nxtop, nybot,nytop, nzbot,nztop,
                                    nxold,nyold,nzold , MRI_float , AA->zd ) ;

#undef  AR
#undef  BR
#define AR(qd,i,j,k) AA->qd[(i)+(j)*nxold+(k)*nxyold]  /* (i,j,k) in AA array */
#define BR(qd,i,j,k) BB->qd[(i)+(j)*nxnew+(k)*nxynew]  /* (i,j,k) in BB array */

   /* Use external slopes to extend in a nonzero way outside the box [Apr 2014] */

   if( !zpad && (nxbot > 0 || nxtop > 0 || nybot > 0 || nytop > 0 || nzbot > 0 || nztop > 0) ){
     int nxyold=nxold*nyold, nxynew=nxnew*nynew, nxo1=nxold-1, nyo1=nyold-1, nzo1=nzold-1 ;

     IW3D_load_external_slopes(AA) ;

AFNI_OMP_START ;
#pragma omp parallel if( nznew > 16 )
     { int ii,jj,kk, iq,jq,kq, di,dj,dk ;
       float Exx,Exy,Exz, Eyx,Eyy,Eyz, Ezx,Ezy,Ezz ;
         for( kk=0 ; kk < nznew ; kk++ ){
           kq = kk-nzbot ; dk = 0 ; Exz = Eyz = Ezz = 0.0f ;
                if( kq < 0 )   { dk = kq     ; kq = 0 ; Exz=AA->es_xd_zm*dk; Eyz=AA->es_yd_zm*dk; Ezz=AA->es_zd_zm*dk; }
           else if( kq > nzo1 ){ dk = kq-nzo1; kq=nzo1; Exz=AA->es_xd_zp*dk; Eyz=AA->es_yd_zp*dk; Ezz=AA->es_zd_zp*dk; }
           for( jj=0 ; jj < nynew ; jj++ ){
             jq = jj-nybot ; dj = 0 ; Exy = Eyy = Ezy = 0.0f ;
                  if( jq < 0 )   { dj = jq     ; jq = 0 ; Exy=AA->es_xd_ym*dj; Eyy=AA->es_yd_ym*dj; Ezy=AA->es_zd_ym*dj; }
             else if( jq > nyo1 ){ dj = jq-nyo1; jq=nyo1; Exy=AA->es_xd_yp*dj; Eyy=AA->es_yd_yp*dj; Ezy=AA->es_zd_yp*dj; }
             for( ii=0 ; ii < nxnew ; ii++ ){
               iq = ii-nybot ; di = 0 ; Exx = Eyx = Ezx = 0.0f ;
                    if( iq < 0 )   { di = iq     ; iq = 0 ; Exx=AA->es_xd_xm*di; Eyx=AA->es_yd_xm*di; Ezx=AA->es_zd_xm*di; }
               else if( iq > nxo1 ){ di = iq-nxo1; iq=nxo1; Exx=AA->es_xd_xp*di; Eyx=AA->es_yd_xp*di; Ezx=AA->es_zd_xp*di; }
               if( di || dj || dk ){
                 BR(xd,ii,jj,kk) = AR(xd,iq,jq,kq) + Exx + Exy + Exz ;
                 BR(yd,ii,jj,kk) = AR(yd,iq,jq,kq) + Eyx + Eyy + Eyz ;
                 BR(zd,ii,jj,kk) = AR(zd,iq,jq,kq) + Ezx + Ezy + Ezz ;
               }
       } } }
     }
AFNI_OMP_END ;
   }
#undef  AR
#undef  BR

   /* the extended warp requires a modified geometry string,
      done in the most brute force way anyone could ever imagine:
        make a dataset (header only) = qset
        zero pad it (header only)    = adset
        the new index warp adopts adset's geometry
        throw these temporary datasets away as untouchable trash */

   if( AA->geomstring != NULL ){
     THD_3dim_dataset *qset , *adset ;
     qset = EDIT_geometry_constructor(AA->geomstring,"TweedleDum") ;
     adset = THD_zeropad( qset , nxbot,nxtop , nybot,nytop , nzbot,nztop ,
                          "TweedleDee" , ZPAD_IJK | ZPAD_EMPTY ) ;
     IW3D_adopt_dataset( BB , adset ) ;
     DSET_delete(adset) ; DSET_delete(qset) ;  /* the trash */
   }

   IW3D_load_external_slopes(BB) ;
   BB->view = AA->view ;
   RETURN(BB) ;
}

/*---------------------------------------------------------------------------*/
/* This function used to be more impressive, but now just calls the
   one above to do its work -- but there is no extrapolation (zpad==1).
*//*-------------------------------------------------------------------------*/

IndexWarp3D * IW3D_zeropad( IndexWarp3D *AA , int nxbot , int nxtop ,
                                              int nybot , int nytop ,
                                              int nzbot , int nztop  )
{
  return IW3D_extend( AA, nxbot,nxtop,nybot,nytop,nzbot,nztop , 1 ) ;
}

/*---------------------------------------------------------------------------*/
/* Extend (and/or truncate) a dataset that represents a warp;
   this is done the brute force way, by conversion to an index warp,
   extension of that, and then conversion back to a dataset struct.
   Note that there is no option for zero-padded extension.
*//*-------------------------------------------------------------------------*/

THD_3dim_dataset * THD_nwarp_extend( THD_3dim_dataset *dset_nwarp ,
                                     int nxbot , int nxtop ,
                                     int nybot , int nytop ,
                                     int nzbot , int nztop  )
{
   IndexWarp3D *AA , *BB ;
   THD_3dim_dataset *qset ;

ENTRY("THD_nwarp_extend") ;

   if( dset_nwarp == NULL || DSET_NVALS(dset_nwarp) < 3 ){
     ERROR_message("Warp extend(%d,%d,%d,%d,%d,%d) fails: input warp %s invalid",
                    nxbot,nxtop,nybot,nytop,nzbot,nztop ,
                    ISVALID_DSET(dset_nwarp) ? DSET_BRIKNAME(dset_nwarp) : "NULL" ) ;
     RETURN(NULL) ;
   }
   DSET_load(dset_nwarp) ;
   if( !DSET_LOADED(dset_nwarp) ){
     ERROR_message("Warp extend(%d,%d,%d,%d,%d,%d) fails: can't load warp dataset %s from disk",
                    nxbot,nxtop,nybot,nytop,nzbot,nztop ,
                    DSET_BRIKNAME(dset_nwarp) ) ;
     RETURN(NULL) ;
   }

   AA = IW3D_from_dataset( dset_nwarp , 0 , 0 ) ;
   DSET_unload(dset_nwarp) ;
   if( AA == NULL ){
     ERROR_message("Warp extend(%d,%d,%d,%d,%d,%d) fails: can't convert dataset %s to IW3D format",
                    nxbot,nxtop,nybot,nytop,nzbot,nztop ,
                    DSET_BRIKNAME(dset_nwarp) ) ;
     RETURN(NULL) ;
   }

   BB = IW3D_extend( AA , nxbot,nxtop,nybot,nytop,nzbot,nztop , 0 ) ;
   IW3D_destroy(AA) ;
   if( BB == NULL ){
     ERROR_message("Warp extend(%d,%d,%d,%d,%d,%d) fails: can't extend IW3D format of dataset %s" ,
                    nxbot,nxtop,nybot,nytop,nzbot,nztop ,
                    DSET_BRIKNAME(dset_nwarp) ) ;
     RETURN(NULL) ;
   }

   qset = IW3D_to_dataset( BB , "ExtendedWarp" ) ;
   IW3D_destroy(BB) ;
   if( qset == NULL ){
     ERROR_message("Warp extend(%d,%d,%d,%d,%d,%d) fails: can't convert IW3D format of %s back to dataset" ,
                    nxbot,nxtop,nybot,nytop,nzbot,nztop ,
                    DSET_BRIKNAME(dset_nwarp) ) ;
   }
   RETURN(qset) ;
}

#endif /*(C4)*/ /*###########################################################*/

#if 1
/*===========================================================================*/
/* (C5) The functions below compute various 'norms' (size
   estimates) of an index warp.  These comprise pretty simple code.          */
/*===========================================================================*/

/*---------------------------------------------------------------------------*/
/* If BB == NULL, just the L1 norm of AA.  Otherwise, the norm of AA-BB. */

float IW3D_normL1( IndexWarp3D *AA , IndexWarp3D *BB )
{
   int qq , nxyz ; float sum , *xda,*yda,*zda ;

   if( AA == NULL ){
     if( BB == NULL ) return 0.0f ;
     AA = BB ; BB = NULL ;
   }

   nxyz = AA->nx * AA->ny * AA->nz ;
   xda = AA->xd ; yda = AA->yd ; zda = AA->zd ;

   sum = 0.0f ;
   if( BB == NULL || BB->nx != AA->nx || BB->ny != AA->ny || BB->nz != AA->nz ){
     for( qq=0 ; qq < nxyz ; qq++ )
       sum += fabsf(xda[qq])+fabsf(yda[qq])+fabsf(zda[qq]) ;
   } else {
     float *xdb=BB->xd , *ydb = BB->yd , *zdb = BB->zd ;
     for( qq=0 ; qq < nxyz ; qq++ )
       sum += fabsf(xda[qq]-xdb[qq])+fabsf(yda[qq]-ydb[qq])+fabsf(zda[qq]-zdb[qq]) ;
   }

   return (sum/nxyz) ;
}

/*---------------------------------------------------------------------------*/
/* If BB == NULL, just the L2 norm of AA.  Otherwise, the norm of AA-BB. */

float IW3D_normL2( IndexWarp3D *AA , IndexWarp3D *BB )
{
   int qq , nxyz ; float sum , *xda,*yda,*zda ;

   if( AA == NULL ){
     if( BB == NULL ) return 0.0f ;
     AA = BB ; BB = NULL ;
   }

   nxyz = AA->nx * AA->ny * AA->nz ;
   xda = AA->xd ; yda = AA->yd ; zda = AA->zd ;

   sum = 0.0f ;
   if( BB == NULL || BB->nx != AA->nx || BB->ny != AA->ny || BB->nz != AA->nz ){
     for( qq=0 ; qq < nxyz ; qq++ )
       sum += SQR(xda[qq])+SQR(yda[qq])+SQR(zda[qq]) ;
   } else {
     float *xdb=BB->xd , *ydb = BB->yd , *zdb = BB->zd ;
     for( qq=0 ; qq < nxyz ; qq++ )
       sum += SQR(xda[qq]-xdb[qq])+SQR(yda[qq]-ydb[qq])+SQR(zda[qq]-zdb[qq]) ;
   }

   return sqrtf(sum/nxyz) ;
}

/*---------------------------------------------------------------------------*/
/* If BB == NULL, the L-infinity norm of AA.  Otherwise, the norm of AA-BB. */

float IW3D_normLinf( IndexWarp3D *AA , IndexWarp3D *BB )
{
   int qq , nxyz ; float vmax,val , *xda,*yda,*zda ;

   if( AA == NULL ){
     if( BB == NULL ) return 0.0f ;
     AA = BB ; BB = NULL ;
   }

   nxyz = AA->nx * AA->ny * AA->nz ;
   xda = AA->xd ; yda = AA->yd ; zda = AA->zd ;

   vmax = 0.0f ;
   if( BB == NULL || BB->nx != AA->nx || BB->ny != AA->ny || BB->nz != AA->nz ){
     for( qq=0 ; qq < nxyz ; qq++ ){
       val = SQR(xda[qq])+SQR(yda[qq])+SQR(zda[qq]) ;
       if( val > vmax ) vmax = val ;
     }
   } else {
     float *xdb=BB->xd , *ydb = BB->yd , *zdb = BB->zd ;
     for( qq=0 ; qq < nxyz ; qq++ ){
       val = SQR(xda[qq]-xdb[qq])+SQR(yda[qq]-ydb[qq])+SQR(zda[qq]-zdb[qq]) ;
       if( val > vmax ) vmax = val ;
     }
   }

   return sqrtf(vmax) ;
}

/*---------------------------------------------------------------------------*/
/* Find the max displacements in 3D from a warp stored as a dataset. */

float_triple THD_nwarp_maxdisp( THD_3dim_dataset *nset )
{
   float_triple dxyz = {0.0f,0.0f,0.f} ;
   float dx=0.0f,dy=0.0f,dz=0.0f ;
   float *xd,*yd,*zd , val ;
   int nvox , ii ;

ENTRY("THD_nwarp_maxdisp") ;

   if( !ISVALID_DSET(nset)                  ) RETURN(dxyz) ;
   if( DSET_NVALS(nset)        <  3         ) RETURN(dxyz) ;
   if( DSET_BRICK_TYPE(nset,0) != MRI_float ) RETURN(dxyz) ;
   DSET_load(nset) ; if( !DSET_LOADED(nset) ) RETURN(dxyz) ;

   xd = (float *)DSET_ARRAY(nset,0) ;
   yd = (float *)DSET_ARRAY(nset,1) ;
   zd = (float *)DSET_ARRAY(nset,2) ;
   nvox = DSET_NVOX(nset) ;
   for( ii=0 ; ii < nvox ; ii++ ){
     val = fabsf(xd[ii]) ; if( val > dx ) dx = val ;
     val = fabsf(yd[ii]) ; if( val > dy ) dy = val ;
     val = fabsf(zd[ii]) ; if( val > dz ) dz = val ;
   }
   dxyz.a = dx ; dxyz.b = dy ; dxyz.c = dz ; RETURN(dxyz) ;
}

/*===========================================================================*/
#if 0
/*----------------------------------------------------------------------------*/
/* smooth a warp locally (obviously not implemented!) */

#define M7  0.142857143f  /* 1/7  */
#define M28 0.035714286f  /* 1/28 */
#define M84 0.011904762f  /* 1/84 */

void IW3D_7smooth( IndexWarp3D *AA )
{
}
/*----------------------------------------------------------------------------*/
#endif
/*============================================================================*/

#endif /*(C5)*/ /*############################################################*/

#if 1
/*===========================================================================*/
/* (C6) Functions below do conversions between index warps and dataset warps */
/*===========================================================================*/

/*----------------------------------------------------------------------------*/
/* Convert a 3D dataset of displacments in mm to an index warp.
     empty != 0 ==> displacements will be all zero
     ivs   != 0 ==> extract sub-bricks [ivs..ivs+2] for the displacments
   Normal usage has empty = ivs = 0.
   See IW3D_to_dataset() for the reverse operation.
*//*--------------------------------------------------------------------------*/

IndexWarp3D * IW3D_from_dataset( THD_3dim_dataset *dset , int empty , int ivs )
{
   IndexWarp3D *AA ;
   MRI_IMAGE *xim , *yim , *zim ;
   mat44 cmat , imat ;
   int nx,ny,nz , nxyz , ii ;
   float *xar,*yar,*zar , *xda,*yda,*zda ;
   char *gstr ;

ENTRY("IW3D_from_dataset") ;

   if( !ISVALID_DSET(dset) || ivs < 0 ) RETURN(NULL) ;

   /* check if dataset is properly constituted, if needed */

   if( !empty ){
     if( DSET_NVALS(dset) < 3+ivs ) RETURN(NULL) ;
     if( !DSET_LOADED(dset) ){
       DSET_load(dset) ; if( !DSET_LOADED(dset) ) RETURN(NULL) ;
     }
   }

   if( !ISVALID_MAT44(dset->daxes->ijk_to_dicom) )
     THD_daxes_to_mat44(dset->daxes) ;

   /* coordinate transformation, needed to take mm displacements
      in the dataset to voxel index displacments in the index warp */

   cmat = dset->daxes->ijk_to_dicom ;  /* takes ijk to xyz */
   imat = MAT44_INV(cmat) ;            /* takes xyz to ijk */

   nx = DSET_NX(dset); ny = DSET_NY(dset); nz = DSET_NZ(dset); nxyz = nx*ny*nz;

   AA = IW3D_create(nx,ny,nz) ;  /* the output of this function */

   AA->cmat = cmat ; AA->imat = imat ;
   gstr = EDIT_get_geometry_string(dset) ;
   if( gstr != NULL ) AA->geomstring = gstr ;
   else               AA->geomstring = NULL ;  /* should never happen! */

   AA->view = dset->view_type ;

   if( !empty ){
     xda = AA->xd ; yda = AA->yd ; zda = AA->zd ;
     xim = THD_extract_float_brick(ivs+0,dset) ; xar = MRI_FLOAT_PTR(xim) ;
     yim = THD_extract_float_brick(ivs+1,dset) ; yar = MRI_FLOAT_PTR(yim) ;
     zim = THD_extract_float_brick(ivs+2,dset) ; zar = MRI_FLOAT_PTR(zim) ;
     DSET_unload(dset) ;

     /* convert displacments in DICOM xyz mm to index displacements */

 AFNI_OMP_START ;
#pragma omp parallel if( nxyz > 1111 )
 { int ii ;
#pragma omp for
     for( ii=0 ; ii < nxyz ; ii++ ){  /* convert mm to index displacements */
       MAT33_VEC( imat , xar[ii],yar[ii],zar[ii] , xda[ii],yda[ii],zda[ii] ) ;
     }
 }
 AFNI_OMP_END ;

     mri_free(zim) ; mri_free(yim) ; mri_free(xim) ;
   }

   IW3D_load_external_slopes(AA) ;
   RETURN(AA) ;
}

#if 0
/*----------------------------------------------------------------------------*/
/* Create a warp pair from a single dataset (not used anywhere, I think).
   Uses sub-bricks 0-2 for the forward warp, and 3-5 for the inverse warp.
   If sub-bricks 3-5 aren't there, then the inverse warp is created.
*//*--------------------------------------------------------------------------*/

IndexWarp3D_pair * IW3D_pair_from_dataset( THD_3dim_dataset *dset )
{
   IndexWarp3D_pair *PP ;

ENTRY("IW3D_pair_from_dataset") ;

   if( !ISVALID_DSET(dset) ) RETURN(NULL) ;

   if( DSET_NVALS(dset) < 3 ) RETURN(NULL) ;
   DSET_load(dset) ; if( !DSET_LOADED(dset) ) RETURN(NULL) ;

   PP = (IndexWarp3D_pair *)malloc(sizeof(IndexWarp3D_pair)) ;
   PP->iwarp = NULL ;

   PP->fwarp = IW3D_from_dataset( dset , 0 , 0 ) ;
   if( PP->fwarp == NULL ){
      IW3D_pair_destroy(PP) ; RETURN(NULL) ;
   }

   if( DSET_NVALS(dset) >= 6 )
     PP->iwarp = IW3D_from_dataset( dset , 0 , 3 ) ;
   if( PP->iwarp == NULL )
     PP->iwarp = IW3D_invert( PP->fwarp , NULL , MRI_LINEAR ) ;

   RETURN(PP) ;
}
#endif

/*----------------------------------------------------------------------------*/
/* Convert an index warp to a 3D dataset of spatial displacmements in mm.
   See IW3D_from_dataset() for the reverse operation.
*//*--------------------------------------------------------------------------*/

static int save_aux_volumes = 0 ; /* user sets this to get auxiliary volumes */

THD_3dim_dataset * IW3D_to_dataset( IndexWarp3D *AA , char *prefix )
{
   THD_3dim_dataset *dset ;
   float *xar,*yar,*zar,*har,*jar,*sar , *xda,*yda,*zda,*hva,*jea,*sea , hfac ;
   mat44 cmat , imat ;
   int ii , nxyz ;

ENTRY("IW3D_to_dataset") ;

   if( AA == NULL ) RETURN(NULL) ;

   /* we need the geometry string to know how to contstruct the dataset */

   if( AA->geomstring == NULL ){
     char *gstr = EDIT_imat_to_geometry_string(AA->imat,AA->nx,AA->ny,AA->nz) ;
     if( gstr == NULL ) RETURN(NULL) ;  /* should not transpire */
     AA->geomstring = gstr ;
   }

STATUS("create dataset") ;
   dset = EDIT_geometry_constructor( AA->geomstring , prefix ) ;
   DSET_CHECKAXES_REAL(dset) ;

   EDIT_dset_items( dset ,
                      ADN_nvals     , (save_aux_volumes) ? 6 : 3 ,
                      ADN_datum_all , MRI_float ,
                      ADN_view_type , AA->view  ,
                    NULL ) ;
   EDIT_BRICK_LABEL( dset , 0 , "x_delta" ) ;
   EDIT_BRICK_LABEL( dset , 1 , "y_delta" ) ;
   EDIT_BRICK_LABEL( dset , 2 , "z_delta" ) ;
   if( save_aux_volumes ){
     EDIT_BRICK_LABEL( dset , 3 , "hexvol"  ) ;
     EDIT_BRICK_LABEL( dset , 4 , "BulkEn"  ) ;
     EDIT_BRICK_LABEL( dset , 5 , "ShearEn" ) ;
   }

   xda = AA->xd ; yda = AA->yd ; zda = AA->zd ;  /* index displacment arrays */
   nxyz = AA->nx * AA->ny * AA->nz ;
   cmat = AA->cmat ; imat = AA->imat ;

#if 0
DUMP_MAT44("IW3D_to_dataset cmat",cmat) ;
DUMP_MAT44("IW3D_to_dataset dset ijk_to_dicom",dset->daxes->ijk_to_dicom) ;
DUMP_MAT44("IW3D_to_dataset dset ijk_to_dicom_real",dset->daxes->ijk_to_dicom_real) ;
#endif

   xar = (float *)malloc(sizeof(float)*nxyz) ;  /* output arrays for dataset */
   yar = (float *)malloc(sizeof(float)*nxyz) ;
   zar = (float *)malloc(sizeof(float)*nxyz) ;
   if( save_aux_volumes ){
     har = (float *)malloc(sizeof(float)*nxyz) ;
     jar = (float *)malloc(sizeof(float)*nxyz) ;
     sar = (float *)malloc(sizeof(float)*nxyz) ;

STATUS("load hexvol") ;
     (void)IW3D_load_hexvol(AA,NULL) ;          /* compute the aux data into */
STATUS("load energy") ;                         /* the warp structure */
     (void)IW3D_load_energy(AA) ;
STATUS("done with aux volumes") ;
     jea = AA->je ; sea  = AA->se ;
   } else {
     har = jar = sar = jea = sea = NULL ;
   }
   hva = AA->hv ; hfac = MAT44_DET(cmat) ; hfac = fabsf(hfac) ;

STATUS("transform to displacements in mm") ;
 AFNI_OMP_START ;
#pragma omp parallel if( nxyz > 1111 )
 { int ii ;
#pragma omp for
   for( ii=0 ; ii < nxyz ; ii++ ){
     MAT33_VEC( cmat , xda[ii],yda[ii],zda[ii] , xar[ii],yar[ii],zar[ii] ) ;
     if( save_aux_volumes ){
       har[ii] = hfac * hva[ii] ; jar[ii] = jea[ii] ; sar[ii] = sea[ii] ;
     }
   }
 }
 AFNI_OMP_END ;

STATUS("substitute bricks") ;
   EDIT_substitute_brick( dset , 0 , MRI_float , xar ) ;
   EDIT_substitute_brick( dset , 1 , MRI_float , yar ) ;
   EDIT_substitute_brick( dset , 2 , MRI_float , zar ) ;
   if( save_aux_volumes ){
     EDIT_substitute_brick( dset , 3 , MRI_float , har ) ;
     EDIT_substitute_brick( dset , 4 , MRI_float , jar ) ;
     EDIT_substitute_brick( dset , 5 , MRI_float , sar ) ;
   }

STATUS("done") ;
   RETURN(dset) ;
}

#endif /*(C6)*/ /*############################################################*/

#if 1
/*============================================================================*/
/* (C7) Functions below compute auxiliary functions of an index warp,
   including stuff for the 3dQwarp penalties, and the 3dNwarpFuncs
   stuff (which are closely related to the 3dQwarp penalties).                */
/*============================================================================*/

/*===========================================================================*/
/* The function below was put here for computation of the eigenvalues of the
   strain tensor, but wasn't actually needed -- since all that is needed
   is the sum of their squares, which is more easily computed as the trace
   of the square of the matrix.  Left in here as a curiosity.
   (And remember Ziad's maxim: NEVER throw code away!)
*//*-------------------------------------------------------------------------*/
#if 0
/*---------------------------------------------------------------------------*/
/* Return the 3 eigenvalues of a 3x3 symmetric matrix.
     - Input matrix is [  a[0] a[1] a[2] ]
                       [  a[1] a[3] a[4] ]
                       [  a[2] a[4] a[5] ]
     - Method is direct solution of cubic characteristic equation
     - Output eigenvalues are not sorted
-----------------------------------------------------------------------------*/

static INLINE double_triple eigval_sym3x3( double *a )
{
   double aa,bb,cc,dd,ee,ff ;
   double a1,a2,a3 , qq,rr, qs,th ;
   double aba,abb,abc,abd,abe,abf , ann,anni ;
   double_triple eee={0.0,0.0,0.0} ;

   if( a == NULL ) return eee ;

   /*----- unload matrix into local variables -----*/

   aa = a[0] ; bb = a[1] ; cc = a[2] ;  /* matrix is [ aa bb cc ]  */
   dd = a[3] ; ee = a[4] ; ff = a[5] ;  /*           [ bb dd ee ]  */
                                        /*           [ cc ee ff ]  */
   aba = fabs(aa) ; abb = fabs(bb) ; abc = fabs(cc) ;
   abd = fabs(dd) ; abe = fabs(ee) ; abf = fabs(ff) ;
   ann = aba+abb+abc+abd+abe+abf   ;                 /* matrix 'norm' */

   if( ann == 0.0 ) return eee ; /* matrix is all zero! */

   /*----- check for matrix that is essentially diagonal -----*/

#undef  EPS
#define EPS  1.e-8

   if( abb+abc+abe == 0.0 ||
       ( EPS*aba > (abb+abc) && EPS*abd > (abb+abe) && EPS*abf > (abc+abe) ) ){

     eee.a = aa ; eee.b = dd ; eee.c = ff ; return eee ;
   }

   /*-- Scale matrix so abs sum is 1; unscale e[i] on output --*/

   anni = 1.0 / ann ;                      /* ann != 0, from above */
   aa *= anni ; bb *= anni ; cc *= anni ;
   dd *= anni ; ee *= anni ; ff *= anni ;

   /*----- not diagonal ==> must solve cubic polynomial for eigenvalues -----*/
   /*      the cubic polynomial is x**3 + a1*x**2 + a2*x + a3 = 0            */

   a1 = -(aa+dd+ff) ;
   a2 =  (aa*ff+aa*dd+dd*ff - bb*bb-cc*cc-ee*ee) ;
   a3 =  ( aa*(ee*ee-dd*ff) + bb*(bb*ff-cc*ee) + cc*(cc*dd-bb*ee) ) ;

   /*-- Rewrite classical formula for qq as a sum of squares --*/
   /*-- [to ensure that it will not be negative by roundoff] --*/
#if 0
   qq = (a1*a1 - 3.0*a2) / 9.0 ;  /* classical formula */
#else
   qq = (  0.5 * ( SQR(dd-aa) + SQR(ff-aa) + SQR(ff-dd) )
         + 3.0 * ( bb*bb      + cc*cc      + ee*ee      ) ) / 9.0 ;
#endif
   rr = (2.0*a1*a1*a1 - 9.0*a1*a2 + 27.0*a3) / 54.0 ;

   if( qq <= 0.0 ){       /*** This should never happen!!! ***/
     qs = qq = rr = 0.0 ;
   } else {
     qs = sqrt(qq) ; rr = rr / (qs*qq) ; qs *= 2.0 ;
     if( rr < -1.0 ) rr = -1.0 ; else if( rr > 1.0 ) rr = 1.0 ;
   }
   th = acos(rr) ; a1 /= 3.0 ;

   eee.a = -ann * ( qs * cos(  th        /3.0 ) + a1 ) ;
   eee.b = -ann * ( qs * cos( (th+2.0*PI)/3.0 ) + a1 ) ;
   eee.c = -ann * ( qs * cos( (th+4.0*PI)/3.0 ) + a1 ) ;

   return eee ;
}
#endif

/*----------------------------------------------------------------------------*/
/* macros to ease the computation of volume of a hexahedron (distorted cube) */

#undef  DA
#undef  DB
#undef  DC
#define DA(p,q) (p.a-q.a)  /* difference in a direction */
#define DB(p,q) (p.b-q.b)  /* difference in b direction */
#define DC(p,q) (p.c-q.c)  /* difference in c direction */

#undef  TRIPROD  /* triple product */
#define TRIPROD(ax,ay,az,bx,by,bz,cx,cy,cz) ( (ax)*((by)*(cz)-(bz)*(cy)) \
                                             +(bx)*((cy)*(az)-(cz)*(ay)) \
                                             +(cx)*((ay)*(bz)-(az)*(by))  )
/* 1D index from 3D indexes */
#undef  IJK
#define IJK(i,j,k) ((i)+(j)*nx+(k)*nxy)

/* set components of the xx triple */
#undef  C2F
#define C2F(p,q,r,xx) ( (xx).a = (p) , (xx).b = (q) , (xx).c = (r) )

/* add xyz displacements into abc components of xx triple */
#undef  D2F
#define D2F(pqr,xx)   ( (xx).a+=xda[pqr], (xx).b+=yda[pqr], (xx).c+=zda[pqr] )

/* load xyz displacements into abc components of xx triple */
#undef  E2F
#define E2F(pqr,xx)   ( (xx).a =xda[pqr], (xx).b =yda[pqr], (xx).c =zda[pqr] )

/*----------------------------------------------------------------------------*/
/* Compute the bulk and shear energies from DISPLACEMENTS at corners.
   Loosely based on http://en.wikipedia.org/wiki/Neo-Hookean_solid.
     d000 is the 3D displacement at corner 000, etc.
     where corner lmn (each index l,m,n is 0 or 1) refers to the
     corner offset by l in the x direction,
                   by m in the y direction,
               and by n in the z direction, from the 000 corner.
   This function is used in computation of the 3dQwarp penalty;
   see function IW3D_load_energy() below.
*//*--------------------------------------------------------------------------*/

static INLINE float_pair hexahedron_energy( float_triple d000, float_triple d100,
                                            float_triple d010, float_triple d110,
                                            float_triple d001, float_triple d101,
                                            float_triple d011, float_triple d111 )
{
   float fxx,fxy,fxz, fyx,fyy,fyz, fzx,fzy,fzz ;
   float II , JJ , VV , jcb ;
   float_pair en ;

   /* Load 3x3 strain matrix (differential displacements);
      the 0.5 factor take the average of 2 first differences
      relative to the oppposite (000 and 111) corners of
      the hexahedron; for example,
        fxy = derivative of y displacement with respect to x,
             computed by averaging 1st differences in the
             x direction at the 000 corner = DB(d100,d000)
                     and at the 111 corner = DB(d111,d011)
             since the differences in the corner indexes in the
             DB() macro are in the first index = x direction code.
      Note that this matrix will not necessarily be symmetric!!   */

   fxx = ( DA(d100,d000) + DA(d111,d011) ) * 0.5f + 1.0f ;
   fxy = ( DB(d100,d000) + DB(d111,d011) ) * 0.5f ;
   fxz = ( DC(d100,d000) + DC(d111,d011) ) * 0.5f ;

   fyx = ( DA(d010,d000) + DA(d111,d101) ) * 0.5f ;
   fyy = ( DB(d010,d000) + DB(d111,d101) ) * 0.5f + 1.0f ;
   fyz = ( DC(d010,d000) + DC(d111,d101) ) * 0.5f ;

   fzx = ( DA(d001,d000) + DA(d111,d110) ) * 0.5f ;
   fzy = ( DB(d001,d000) + DB(d111,d110) ) * 0.5f ;
   fzz = ( DC(d001,d000) + DC(d111,d110) ) * 0.5f + 1.0f ;

   /* determinant = bulk volume (1==unchanged) */

   JJ = TRIPROD( fxx,fxy,fxz, fyx,fyy,fyz, fzx,fzy,fzz ) ;
   if( JJ < 0.1f ) JJ = 0.1f ; else if( JJ > 10.0f ) JJ = 10.0f ;  /* limits */

   /* trace of matrix square = sum of eigenvalue squares of matrix,
      which in turn gives the shear energy of the displacement tensor */

   II = (  fxx*fxx + fyy*fyy + fzz*fzz
         + fxy*fxy + fyx*fyx + fxz*fxz
         + fzx*fzx + fyz*fyz + fzy*fzy ) ;

   /* "vorticity" penalty added in, for fun (to avoid warp "twistiness") */

   fxx = fyz - fzy ; fyy = fxz - fzx ; fzz = fxy - fyx ;
   VV = 2.0f*( fxx*fxx + fyy*fyy + fzz*fzz ) ;

   jcb = cbrtf(JJ*JJ) ;   /* J^(2/3) */

   /* sum of shear and vorticity penalties, scaled to be unitless */

   II = (II + VV) / jcb - 3.0f ; if( II < 0.0f ) II = 0.0f ;

   /* compute energies (penalties) into the two
      components of the output -- will be summed later */

   jcb = (JJ-1.0f/JJ) ; en.a = 0.333f*jcb*jcb ; en.b = II ;
   return en ;
}

/*----------------------------------------------------------------------------*/
/* Load the deformation energies for all voxels, using hexahedron_energy();
   return value is the sum of all the energies (AKA the warp penalty).
   There are two optional deformation energy arrays stored inside an
   index warp:
     je = Jacobian energy = energy of bulk compression or dilation
     se = Shear energy = energy of shearing and twisting
   The penalty for 3dQwarp is computed from each voxel energy E by
     max(je-1,0)^4 + max(se-1,0)^4
   In this way, small deformations are penalty-free, and then voxel-wise
   energies over 1 become heavily penalized. Search below for the "HPEN_"
   variables and "Hpen_" functions to see exactly how these values are used.
*//*--------------------------------------------------------------------------*/

static float Hpen_cut = 1.0f; /* voxel-wise penalties below this are excluded */

float IW3D_load_energy( IndexWarp3D *AA )
{
   float enout=0.0f ;
   float *xda, *yda , *zda , *jea,*sea , jetop,setop ;
   int nx,ny,nz , nxy,nxyz , ii ;

ENTRY("IW3D_load_energy") ;

   if( AA == NULL ) RETURN(enout) ;

   nx = AA->nx ; ny = AA->ny ; nz = AA->nz ; nxy = nx*ny ; nxyz = nxy*nz ;

   xda = AA->xd ; yda = AA->yd ; zda = AA->zd ;  /* displacement arrays */

STATUS("get/create je/se arrays") ;
   jea = AA->je; if( jea == NULL ) jea = AA->je = (float *)calloc(nxyz,sizeof(float));
   sea = AA->se; if( sea == NULL ) sea = AA->se = (float *)calloc(nxyz,sizeof(float));

STATUS("set dhhar -> 0") ;
   AAmemset( dhaar , 0 , sizeof(double)*nthmax ) ; /* initialize per thread sums */

STATUS("start the work (=energy; get it?)") ;
 AFNI_OMP_START ;
#pragma omp parallel
 { float_triple x0,x1,x2,x3,x4,x5,x6,x7 ; float_pair en ;
   int ii,jj,kk , ip,jp,kp , ijk , qq , ith=0 ; float esum=0.0f, ev ;
#pragma omp for
   for( qq=0 ; qq < nxyz ; qq++ ){
     ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
     ip = ii+1 ; jp = jj+1 ; kp = kk+1 ;
     if( ip == nx ) ip-- ; if( jp == ny ) jp-- ; if( kp == nz ) kp-- ;
     ijk = IJK(ip,jj,kk) ; E2F(ijk,x1) ;  /* load displacements at */
     ijk = IJK(ii,jp,kk) ; E2F(ijk,x2) ;  /* each of the 8 corners */
     ijk = IJK(ip,jp,kk) ; E2F(ijk,x3) ;
     ijk = IJK(ii,jj,kp) ; E2F(ijk,x4) ;
     ijk = IJK(ip,jj,kp) ; E2F(ijk,x5) ;
     ijk = IJK(ii,jp,kp) ; E2F(ijk,x6) ;
     ijk = IJK(ip,jp,kp) ; E2F(ijk,x7) ;
     ijk = qq            ; E2F(ijk,x0) ; /* the 000 corner */
     en  = hexahedron_energy(x0,x1,x2,x3,x4,x5,x6,x7); jea[qq] = en.a; sea[qq] = en.b;
     ev  = jea[qq]-Hpen_cut ; if( ev > 0.0f ) esum += (ev*ev)*(ev*ev) ;
     ev  = sea[qq]-Hpen_cut ; if( ev > 0.0f ) esum += (ev*ev)*(ev*ev) ;
   }
#ifdef USE_OMP
   ith = omp_get_thread_num() ;
#endif
   dhaar[ith] = (double)esum ;  /* per thread save of sum */
 }
 AFNI_OMP_END ;
STATUS("work is done") ;

  for( ii=0 ; ii < nthmax ; ii++ ) enout += dhaar[ii] ;  /* final summation */
  RETURN(enout) ;
}

/*----------------------------------------------------------------------------*/
/* Compute the bulk, shear, and vorticity deformations, from DISPLACEMENTS
   at corners, for one voxel.
*//*--------------------------------------------------------------------------*/

static INLINE float_triple hexahedron_bsv( float dx, float dy, float dz ,
                                           float_triple d000 , float_triple d100 ,
                                           float_triple d010 , float_triple d110 ,
                                           float_triple d001 , float_triple d101 ,
                                           float_triple d011 , float_triple d111  )
{
   float fxx,fxy,fxz, fyx,fyy,fyz, fzx,fzy,fzz , jcb ;
   float_triple bsv ;

   if( dx <= 0.0f ) dx = 1.0f ;
   if( dy <= 0.0f ) dy = 1.0f ;
   if( dz <= 0.0f ) dz = 1.0f ;

   /* load strain matrix */

   fxx = ( DA(d100,d000) + DA(d111,d011) ) * 0.5f + 1.0f ;
   fxy = ( DB(d100,d000) + DB(d111,d011) ) * 0.5f * (dy/dx) ;
   fxz = ( DC(d100,d000) + DC(d111,d011) ) * 0.5f * (dz/dx) ;

   fyx = ( DA(d010,d000) + DA(d111,d101) ) * 0.5f * (dx/dy) ;
   fyy = ( DB(d010,d000) + DB(d111,d101) ) * 0.5f + 1.0f ;
   fyz = ( DC(d010,d000) + DC(d111,d101) ) * 0.5f * (dz/dy) ;

   fzx = ( DA(d001,d000) + DA(d111,d110) ) * 0.5f * (dx/dz) ;
   fzy = ( DB(d001,d000) + DB(d111,d110) ) * 0.5f * (dy/dz) ;
   fzz = ( DC(d001,d000) + DC(d111,d110) ) * 0.5f + 1.0f ;

   /* determinant = bulk volume (1=unchanged) */

   jcb   = TRIPROD( fxx,fxy,fxz, fyx,fyy,fyz, fzx,fzy,fzz ) ;
   bsv.a = jcb - 1.0f ;
   jcb   = cbrtf(jcb*jcb) ;

   /* trace of matrix square = shear energy */

   bsv.b = (  fxx*fxx + fyy*fyy + fzz*fzz
            + fxy*fxy + fyx*fyx + fxz*fxz
            + fzx*fzx + fyz*fyz + fzy*fzy ) / jcb - 3.0f ;
   if( bsv.b < 0.0f ) bsv.b = 0.0f ;

   /* "vorticity" energy */

   fxx = fyz - fzy ; fyy = fxz - fzx ; fzz = fxy - fyx ;
   bsv.c = ( fxx*fxx + fyy*fyy + fzz*fzz ) / jcb ;

   return bsv ;
}

/*----------------------------------------------------------------------------*/
/* Load the deformation values for all voxels [26 Jul 2013] */

void IW3D_load_bsv( IndexWarp3D *AA , float dx,float dy,float dz ,
                    float *bb , float *ss , float *vv )
{
   float enout=0.0f ;
   float *xda, *yda , *zda , *jea,*sea , jetop,setop ;
   int nx,ny,nz , nxy,nxyz , ii ;

ENTRY("IW3D_load_bsv") ;

   if( AA == NULL ) EXRETURN ;
   if( bb == NULL && ss == NULL && vv == NULL ) EXRETURN ;

   nx = AA->nx ; ny = AA->ny ; nz = AA->nz ; nxy = nx*ny ; nxyz = nxy*nz ;

   xda = AA->xd ; yda = AA->yd ; zda = AA->zd ;

 AFNI_OMP_START ;
#pragma omp parallel
 { float_triple x0,x1,x2,x3,x4,x5,x6,x7 ; float_triple bsv ;
   int ii,jj,kk , ip,jp,kp , ijk , qq ; float esum=0.0f, ev ;
#pragma omp for
   for( qq=0 ; qq < nxyz ; qq++ ){
     ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
     ip = ii+1 ; jp = jj+1 ; kp = kk+1 ;
     if( ip == nx ) ip-- ; if( jp == ny ) jp-- ; if( kp == nz ) kp-- ;
     ijk = IJK(ip,jj,kk) ; E2F(ijk,x1) ;
     ijk = IJK(ii,jp,kk) ; E2F(ijk,x2) ;
     ijk = IJK(ip,jp,kk) ; E2F(ijk,x3) ;
     ijk = IJK(ii,jj,kp) ; E2F(ijk,x4) ;
     ijk = IJK(ip,jj,kp) ; E2F(ijk,x5) ;
     ijk = IJK(ii,jp,kp) ; E2F(ijk,x6) ;
     ijk = IJK(ip,jp,kp) ; E2F(ijk,x7) ;
     ijk = qq            ; E2F(ijk,x0) ;
     bsv = hexahedron_bsv(dx,dy,dz,x0,x1,x2,x3,x4,x5,x6,x7);
     if( bb != NULL ) bb[qq] = bsv.a ;
     if( ss != NULL ) ss[qq] = bsv.b ;
     if( vv != NULL ) vv[qq] = bsv.c ;
   }
 }
 AFNI_OMP_END ;

  EXRETURN ;
}

/*----------------------------------------------------------------------------*/
/* sum up the warp penalties */

#ifdef USE_OMP
double HPEN_addup( int njs , float *je , float *se )  /* parallelized version */
{
   double esum ; int qq ;              /* these are function global variables */

   AAmemset( dhaar , 0 , sizeof(double)*nthmax ) ;
#pragma omp parallel
   { int ii , ith ; double ev , dh=0.0 ;    /* these are per-thread variables */
#pragma omp for
     for( ii=0 ; ii < njs ; ii++ ){
       ev = je[ii]-Hpen_cut ; if( ev > 0.0 ) dh += (ev*ev)*(ev*ev) ;
       ev = se[ii]-Hpen_cut ; if( ev > 0.0 ) dh += (ev*ev)*(ev*ev) ;
     }
     ith = omp_get_thread_num() ; dhaar[ith] = dh ;  /* dhaar = temp array */
   }

   for( esum=0.0,qq=0 ; qq < nthmax ; qq++ ) esum += dhaar[qq] ;
   return esum ;
}

#else  /*---------------------------------------------------------------------*/

double HPEN_addup( int njs , float *je , float *se )       /* simpler version */
{
   int ii ; double ev , esum=0.0 ;
   for( ii=0 ; ii < njs ; ii++ ){
     ev = je[ii]-Hpen_cut ; if( ev > 0.0 ) esum += (ev*ev)*(ev*ev) ;
     ev = se[ii]-Hpen_cut ; if( ev > 0.0 ) esum += (ev*ev)*(ev*ev) ;
   }
   return esum ;
}
#endif

/*----------------------------------------------------------------------------*/
#ifndef HAVE_HEXVOL /* this function exists in some other places in AFNI-land */
#define HAVE_HEXVOL
/*----------------------------------------------------------------------------*/
/* Volume of a hexahedron (distorted cube) given by 8 corners.
   Looking down from the top, the bottom plane points are numbered so:
       2 -- 3
       |    |  and the top plane is similar (add 4 to each index),
       0 -- 1  with point #(i+4) 'above' point #i.
*//*--------------------------------------------------------------------------*/

static INLINE float hexahedron_volume( float_triple x0 , float_triple x1 ,
                                       float_triple x2 , float_triple x3 ,
                                       float_triple x4 , float_triple x5 ,
                                       float_triple x6 , float_triple x7  )
{
   float xa,ya,za , xb,yb,zb , xc,yc,zc , vol ;

   xa = DA(x7,x1)+DA(x6,x0); ya = DB(x7,x1)+DB(x6,x0); za = DC(x7,x1)+DC(x6,x0);
   xb = DA(x7,x2)          ; yb = DB(x7,x2)          ; zb = DC(x7,x2) ;
   xc = DA(x3,x0)          ; yc = DB(x3,x0)          ; zc = DC(x3,x0) ;
   vol = TRIPROD(xa,ya,za,xb,yb,zb,xc,yc,zc) ;
   xa = DA(x6,x0)          ; ya = DB(x6,x0)          ; za = DC(x6,x0) ;
   xb = DA(x7,x2)+DA(x5,x0); yb = DB(x7,x2)+DB(x5,x0); zb = DC(x7,x2)+DC(x5,x0);
   xc = DA(x7,x4)          ; yc = DB(x7,x4)          ; zc = DC(x7,x4) ;
   vol += TRIPROD(xa,ya,za,xb,yb,zb,xc,yc,zc) ;
   xa = DA(x7,x1)          ; ya = DB(x7,x1)          ; za = DC(x7,x1) ;
   xb = DA(x5,x0)          ; yb = DB(x5,x0)          ; zb = DC(x5,x0) ;
   xc = DA(x7,x4)+DA(x3,x0); yc = DB(x7,x4)+DB(x3,x0); zc = DC(x7,x4)+DC(x3,x0);
   vol += TRIPROD(xa,ya,za,xb,yb,zb,xc,yc,zc) ;
   return (0.08333333f*vol) ;
}
#endif /* HAVE_HEXVOL */

#undef TRIPROD
#undef DA
#undef DB
#undef DC

/*---------------------------------------------------------------------------*/
/* Load the volumes of each hexahedral element in the displaced grid.
   An undistorted voxel will get volume 1, since AA is a unitless warp. */

float IW3D_load_hexvol( IndexWarp3D *AA , float *hv )
{
   float *xda, *yda , *zda , *hva , top,bot ;
   int nx,ny,nz , nxy,nxyz , ii ;
   float hvm = 0.0f ;

ENTRY("IW3D_load_hexvol") ;

   if( AA == NULL ) RETURN(hvm) ;

   nx = AA->nx ; ny = AA->ny ; nz = AA->nz ; nxy = nx*ny ; nxyz = nxy*nz ;

   xda = AA->xd ; yda = AA->yd ; zda = AA->zd ;

   hva = hv ;
   if( hva == NULL ){
     hva = AA->hv ;
     if( hva == NULL ) hva = AA->hv = (float *)calloc(nxyz,sizeof(float)) ;
   }

 AFNI_OMP_START ;
#pragma omp parallel
 { float_triple x0,x1,x2,x3,x4,x5,x6,x7 ;
   int ii,jj,kk , ip,jp,kp , ijk , qq ;
#pragma omp for
   for( qq=0 ; qq < nxyz ; qq++ ){
     ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
     ip = ii+1 ; jp = jj+1 ; kp = kk+1 ;
     C2F(ii,jj,kk,x0); C2F(ip,jj,kk,x1); C2F(ii,jp,kk,x2); C2F(ip,jp,kk,x3);
     C2F(ii,jj,kp,x4); C2F(ip,jj,kp,x5); C2F(ii,jp,kp,x6); C2F(ip,jp,kp,x7);
     if( ip == nx ) ip-- ; if( jp == ny ) jp-- ; if( kp == nz ) kp-- ;
     ijk = IJK(ip,jj,kk) ; D2F(ijk,x1) ;
     ijk = IJK(ii,jp,kk) ; D2F(ijk,x2) ;
     ijk = IJK(ip,jp,kk) ; D2F(ijk,x3) ;
     ijk = IJK(ii,jj,kp) ; D2F(ijk,x4) ;
     ijk = IJK(ip,jj,kp) ; D2F(ijk,x5) ;
     ijk = IJK(ii,jp,kp) ; D2F(ijk,x6) ;
     ijk = IJK(ip,jp,kp) ; D2F(ijk,x7) ;
     ijk = qq            ; D2F(ijk,x0) ;
     hva[qq] = hexahedron_volume(x0,x1,x2,x3,x4,x5,x6,x7) ;
   }
 }
 AFNI_OMP_END ;

  RETURN(hvm) ;
}

#if 0
/*---------------------------------------------------------------------------*/
/* Load hexahedron volumes over just PART of the warp (never used). */
void IW3D_load_hexvol_box( IndexWarp3D *AA ,
                           int ibot,int itop, int jbot,int jtop, int kbot,int ktop )
{
   float *xda, *yda , *zda , *hva , top,bot ;
   int nx,ny,nz , nxy , ii , nbx,nby,nbz,nbxy,nbxyz ;

   if( AA == NULL ) return ;

   nx = AA->nx ; ny = AA->ny ; nz = AA->nz ; nxy = nx*ny ;

   nbx  = (itop-ibot+1) ;
   nby  = (jtop-jbot+1) ; nbxy  = nbx *nby ;
   nbz  = (ktop-kbot+1) ; nbxyz = nbxy*nbz ;

   xda = AA->xd ; yda = AA->yd ; zda = AA->zd ;

   hva = AA->hv ;
   if( hva == NULL ) hva = AA->hv = (float *)calloc(nxy*nz,sizeof(float)) ;

 AFNI_OMP_START ;
#pragma omp parallel
 { float_triple x0,x1,x2,x3,x4,x5,x6,x7 ;
   int ii,jj,kk , ip,jp,kp , ijk , qq ;
#pragma omp for
   for( qq=0 ; qq < nbxyz ; qq++ ){
     ii = qq * nbx ; kk = qq / nbxy ; jj = (qq-kk*nbxy) / nbx ;
     ii += ibot ; jj += jbot ; kk += kbot ;
     ip = ii+1 ; jp = jj+1 ; kp = kk+1 ;
     C2F(ii,jj,kk,x0); C2F(ip,jj,kk,x1); C2F(ii,jp,kk,x2); C2F(ip,jp,kk,x3);
     C2F(ii,jj,kp,x4); C2F(ip,jj,kp,x5); C2F(ii,jp,kp,x6); C2F(ip,jp,kp,x7);
     if( ip == nx ) ip-- ; if( jp == ny ) jp-- ; if( kp == nz ) kp-- ;
     ijk = IJK(ip,jj,kk) ; D2F(ijk,x1) ;
     ijk = IJK(ii,jp,kk) ; D2F(ijk,x2) ;
     ijk = IJK(ip,jp,kk) ; D2F(ijk,x3) ;
     ijk = IJK(ii,jj,kp) ; D2F(ijk,x4) ;
     ijk = IJK(ip,jj,kp) ; D2F(ijk,x5) ;
     ijk = IJK(ii,jp,kp) ; D2F(ijk,x6) ;
     ijk = IJK(ip,jp,kp) ; D2F(ijk,x7) ;
     ijk = IJK(ii,jj,kk) ; D2F(ijk,x0) ;
     hva[ijk] = hexahedron_volume(x0,x1,x2,x3,x4,x5,x6,x7) ;
   }
 } /* end of parallel code */
 AFNI_OMP_END ;

 return ;
}
#endif

#undef C2F
#undef D2F
#undef E2F

#endif /*(C7)*/ /*############################################################*/

#if 1
/*============================================================================*/
/* (C8) The following functions are for interpolating all 3 components of an
   index warp at one time, and are recklessly adapted from mri_genalign_util.c;
   however, since Zhark wrote those, Zhark can steal them (right?).           */
/*============================================================================*/

#undef  CLIP
#define CLIP(mm,nn) if((mm) < 0)(mm)=0; else if((mm) > (nn))(mm)=(nn)

#undef  QLIP
#define QLIP(mm,nn) if( (mm) > (nn) ) (mm)=(nn)

#undef  AJK
#define AJK(aaa,j,k) ((aaa)+(j)*nx+(k)*nxy)

/*---------------------------------------------------------------------------*/
/*! Interpolate displacements using linear method.
      nxx, nyy, nzz = grid dimensions of displacment arrays
      aar, bar, car = displacement arrays
      use_es        = flag to use external slope array
      esar          = external slope array
      npp           = number of points at which to interpolate
      ip, jp, kp    = float indexes at which to interpolate
      uar, var, war = output arrays [npp]
*//*-------------------------------------------------------------------------*/

void IW3D_interp_linear( int nxx , int nyy , int nzz ,
                         float *aar , float *bar , float *car ,
                         int use_es , float *esar ,
                         int npp, float *ip, float *jp, float *kp,
                         float *uar , float *var , float *war     )
{
 /** note all variables are thread local (except function args) **/
 AFNI_OMP_START ;
#pragma omp parallel if( npp > 1111 )
 {
   int nx=nxx, ny=nyy, nz=nzz, nxy=nx*ny, pp ;
   int nx1=nx-1,ny1=ny-1,nz1=nz-1 ;
   int nx2=nx-2,ny2=ny-2,nz2=nz-2 ;
   float fx,fy,fz , xx,yy,zz ;
   int   ix_00,ix_p1 , jy_00,jy_p1 , kz_00,kz_p1 , ix,jy,kz ;
   float wt_00,wt_p1 ;
   float f_j00_k00, f_jp1_k00, f_j00_kp1, f_jp1_kp1, f_k00, f_kp1 ;
   float g_j00_k00, g_jp1_k00, g_j00_kp1, g_jp1_kp1, g_k00, g_kp1 ;
   float h_j00_k00, h_jp1_k00, h_j00_kp1, h_jp1_kp1, h_k00, h_kp1 ;

   int ues=use_es ;
   ES_DECLARE_FLOATS ; float uex=0.0f,vex=0.0f,wex=0.0f ;
   float eex=0.0f,eey=0.0f,eez=0.0f , Exx=0.0f,Exy=0.0f,Exz=0.0f ,
         Eyx=0.0f,Eyy=0.0f,Eyz=0.0f , Ezx=0.0f,Ezy=0.0f,Ezz=0.0f ;

   /* if using external slopes, load local variables with these slopes */

   if( ues ) ES_UNPACKVEC(esar) ;

#pragma omp for
   for( pp=0 ; pp < npp ; pp++ ){              /* loop over output points */
     xx = ip[pp] ; yy = jp[pp] ; zz = kp[pp] ;
     if( !ues ){   /* not using external slopes */
            if( xx < 0.0f ){ ix = 0       ; fx = 0.0f ; }  /* just truncate */
       else if( xx < nx1  ){ ix = (int)xx ; fx = xx-ix; }  /* if out of range */
       else                { ix = nx2     ; fx = 1.0f ; }
            if( yy < 0.0f ){ jy = 0       ; fy = 0.0f ; }
       else if( yy < ny1  ){ jy = (int)yy ; fy = yy-jy; }
       else                { jy = ny2     ; fy = 1.0f ; }
            if( zz < 0.0f ){ kz = 0       ; fz = 0.0f ; }
       else if( zz < nz1  ){ kz = (int)zz ; fz = zz-kz; }
       else                { kz = nz2     ; fz = 1.0f ; }
     } else {      /* using external slopes */
       int aem=0 ; /* flag for if external slopes are needed */
            if( xx < 0.0f ){ eex = xx    ; ix = 0      ; fx = 0.0f; aem++; Exx=es_xd_xm; Eyx=es_yd_xm; Ezx=es_zd_xm; }
       else if( xx < nx1  ){ eex = 0.0f  ; ix = (int)xx; fx = xx-ix;       }
       else                { eex = xx-nx1; ix = nx2    ; fx = 1.0f; aem++; Exx=es_xd_xp; Eyx=es_yd_xp; Ezx=es_zd_xp; }
            if( yy < 0.0f ){ eey = yy    ; jy = 0      ; fy = 0.0f; aem++; Exy=es_xd_ym; Eyy=es_yd_ym; Ezy=es_zd_ym; }
       else if( yy < ny1  ){ eey = 0.0f  ; jy = (int)yy; fy = yy-jy;       }
       else                { eey = yy-ny1; jy = ny2    ; fy = 1.0f; aem++; Exy=es_xd_yp; Eyy=es_yd_yp; Ezy=es_zd_yp; }
            if( zz < 0.0f ){ eez = zz    ; kz = 0      ; fz = 0.0f; aem++; Exz=es_xd_zm; Eyz=es_yd_zm; Ezz=es_zd_zm; }
       else if( zz < nz1  ){ eez = 0.0f  ; kz = (int)zz; fz = zz-kz;       }
       else                { eez = zz-nz1; kz = nz2    ; fz = 1.0f; aem++; Exz=es_xd_zp; Eyz=es_yd_zp; Ezz=es_zd_zp; }
       if( aem ){
         uex = Exx*eex + Exy*eey + Exz*eez ;  /* eex = how far past edge of warp, in x-direction (etc.) */
         vex = Eyx*eex + Eyy*eey + Eyz*eez ;  /* Eab = external slope for 'a' displacement, */
         wex = Ezx*eex + Ezy*eey + Ezz*eez ;  /*       along the 'b' direction, for a and b = x or y or z */
       } else {
         uex = vex = wex = 0.0f ;
       }
     }

     /* special case of no interpolation needed */

     if( fabsf(fx)+fabsf(fy)+fabsf(fz) < 0.00222f ){  /* 06 Nov 2014 */
       uar[pp] = aar[IJK(ix,jy,kz)] + uex ;
       var[pp] = bar[IJK(ix,jy,kz)] + vex ;
       war[pp] = car[IJK(ix,jy,kz)] + wex ;
       continue ;
     }

     ix_00 = ix ; ix_p1 = ix_00+1 ; /* at this point, we are 'fx' between indexes ix_00 and ix_p1 */
     jy_00 = jy ; jy_p1 = jy_00+1 ;
     kz_00 = kz ; kz_p1 = kz_00+1 ;

     wt_00 = 1.0f-fx ; wt_p1 = fx ;  /* weights for ix_00 and ix_p1 points */

#undef  XINT  /* linear interpolation macro in array 'aaa' at plane jk */
#define XINT(aaa,j,k) wt_00*aaa[IJK(ix_00,j,k)]+wt_p1*aaa[IJK(ix_p1,j,k)]

     /* interpolate to location ix+fx at each jy,kz level */

     f_j00_k00 = XINT(aar,jy_00,kz_00) ; f_jp1_k00 = XINT(aar,jy_p1,kz_00) ;
     f_j00_kp1 = XINT(aar,jy_00,kz_p1) ; f_jp1_kp1 = XINT(aar,jy_p1,kz_p1) ;
     g_j00_k00 = XINT(bar,jy_00,kz_00) ; g_jp1_k00 = XINT(bar,jy_p1,kz_00) ;
     g_j00_kp1 = XINT(bar,jy_00,kz_p1) ; g_jp1_kp1 = XINT(bar,jy_p1,kz_p1) ;
     h_j00_k00 = XINT(car,jy_00,kz_00) ; h_jp1_k00 = XINT(car,jy_p1,kz_00) ;
     h_j00_kp1 = XINT(car,jy_00,kz_p1) ; h_jp1_kp1 = XINT(car,jy_p1,kz_p1) ;

     /* interpolate to jy+fy at each kz level */

     wt_00 = 1.0f-fy ; wt_p1 = fy ;
     f_k00 =  wt_00 * f_j00_k00 + wt_p1 * f_jp1_k00 ;
     f_kp1 =  wt_00 * f_j00_kp1 + wt_p1 * f_jp1_kp1 ;
     g_k00 =  wt_00 * g_j00_k00 + wt_p1 * g_jp1_k00 ;
     g_kp1 =  wt_00 * g_j00_kp1 + wt_p1 * g_jp1_kp1 ;
     h_k00 =  wt_00 * h_j00_k00 + wt_p1 * h_jp1_k00 ;
     h_kp1 =  wt_00 * h_j00_kp1 + wt_p1 * h_jp1_kp1 ;

     /* interpolate to kz+fz to get output */

     uar[pp] = (1.0f-fz) * f_k00 + fz * f_kp1 + uex ;  /* note add-in of values */
     var[pp] = (1.0f-fz) * g_k00 + fz * g_kp1 + vex ;  /* from external slope */
     war[pp] = (1.0f-fz) * h_k00 + fz * h_kp1 + wex ;  /* calculation above */
   }

 } /* end OpenMP */
 AFNI_OMP_END ;

   return ;
}

/*---------------------------------------------------------------------------*/
/* Interpolation with weighted (tapered) sinc in 3D.
   ++ Taper function wtap(r) is defined to be 1 for 0 <= r <= WCUT
       and for WCUT < r < 1 is a raised cosine dropping down to wtap(r=1) = 0.
       This choice was made to keep the variance smoothing artifact low.
   ++ Radius of sinc window is WRAD, so the actual taper used is wtap(x/WRAD)
*//*-------------------------------------------------------------------------*/

#undef  WCUT
#define WCUT 0.1f    /* cutoff point for taper */

#undef  WRAD
#define WRAD 5.0001f /* width of sinc interpolation (float) */

#undef  IRAD
#define IRAD 5       /* width of sinc interpolation (int) */

#undef  PIF
#define PIF 3.1415927f /* PI in float */

/* sinc function = sin(PI*x)/(PI*x) [N.B.: x will always be >= 0] */

#undef  sinc
#define sinc(x) ( ((x)>0.01f) ? sinf(PIF*(x))/(PIF*(x))     \
                              : 1.0f - 1.6449341f*(x)*(x) )

/* Weight (taper) function, declining from wtap(WCUT)=1 to wtap(1)=0 */
/* Note that the input to wtap will always be between WCUT and 1.   */

#undef  wtap
#define wtap(x) ( 0.53836f+0.46164f*cosf(PIF*((x)-WCUT)/(1.0f-WCUT)) )

#undef  AW
#undef  BW
#undef  CW
#define AW(i) aarjk[iqq[i]]*wtt[i]  /* access macros */
#define BW(i) barjk[iqq[i]]*wtt[i]  /* with weighting */
#define CW(i) carjk[iqq[i]]*wtt[i]

/*---------------------------------------------------------------------------*/
/*! Interpolate using wsinc5 method (slow and accurate);
    for usage and comments, see the _linear function above.
*//*-------------------------------------------------------------------------*/

void IW3D_interp_wsinc5( int nxx , int nyy , int nzz ,
                         float *aar , float *bar , float *car ,
                         int use_es , float *esar ,
                         int npp, float *ip, float *jp, float *kp,
                         float *uar , float *var , float *war     )
{
ENTRY("IW3D_interp_wsinc5") ;
 AFNI_OMP_START ;
#pragma omp parallel if( npp > 333 )
 {
   int nx=nxx, ny=nyy, nz=nzz, nxy=nx*ny, nxyz=nxy*nz,nxyz1=nxyz-1, pp, ix,jy,kz ;
   float xx,yy,zz , fx,fy,fz ;
   float *aarjk , *barjk , *carjk ;
   int nx1=nx-1,ny1=ny-1,nz1=nz-1 ;
   int nx2=nx-2,ny2=ny-2,nz2=nz-2 ;

   float xw,yw,zw,rr , asum,bsum,csum,wsum,wfac,wt ;
   int   iq,jq,kq,iqp , qq,jj,kk , ddi,ddj,ddk ;
   float xsin[2*IRAD] , ysin[2*IRAD]        , zsin[2*IRAD] ;
   float wtt[2*IRAD]  , ajk[2*IRAD][2*IRAD] , ak[2*IRAD]   ;
   float                bjk[2*IRAD][2*IRAD] , bk[2*IRAD]   ;
   float                cjk[2*IRAD][2*IRAD] , ck[2*IRAD]   ;
   int   iqq[2*IRAD]  ;

   int ues=use_es , outside ;
   ES_DECLARE_FLOATS ; float uex=0.0f,vex=0.0f,wex=0.0f ;
   float eex=0.0f,eey=0.0f,eez=0.0f , Exx=0.0f,Exy=0.0f,Exz=0.0f ,
         Eyx=0.0f,Eyy=0.0f,Eyz=0.0f , Ezx=0.0f,Ezy=0.0f,Ezz=0.0f ;

   if( ues ) ES_UNPACKVEC(esar) ;

   /*----- loop over points -----*/

#pragma omp for
   for( pp=0 ; pp < npp ; pp++ ){
     xx = ip[pp] ; yy = jp[pp] ; zz = kp[pp] ;
     if( !ues ){
            if( xx < 0.0f ){ ix = 0       ; fx = 0.0f ; outside = 1 ; }
       else if( xx < nx1  ){ ix = (int)xx ; fx = xx-ix; outside = 0 ; }
       else                { ix = nx2     ; fx = 1.0f ; outside = 1 ; }
            if( yy < 0.0f ){ jy = 0       ; fy = 0.0f ; outside = 1 ; }
       else if( yy < ny1  ){ jy = (int)yy ; fy = yy-jy; outside = 0 ; }
       else                { jy = ny2     ; fy = 1.0f ; outside = 1 ; }
            if( zz < 0.0f ){ kz = 0       ; fz = 0.0f ; outside = 1 ; }
       else if( zz < nz1  ){ kz = (int)zz ; fz = zz-kz; outside = 0 ; }
       else                { kz = nz2     ; fz = 1.0f ; outside = 1 ; }
     } else {
       int aem=0 ;
            if( xx < 0.0f ){ eex = xx    ; ix = 0      ; fx = 0.0f; aem++; Exx=es_xd_xm; Eyx=es_yd_xm; Ezx=es_zd_xm; }
       else if( xx < nx1  ){ eex = 0.0f  ; ix = (int)xx; fx = xx-ix;       }
       else                { eex = xx-nx1; ix = nx2    ; fx = 1.0f; aem++; Exx=es_xd_xp; Eyx=es_yd_xp; Ezx=es_zd_xp; }
            if( yy < 0.0f ){ eey = yy    ; jy = 0      ; fy = 0.0f; aem++; Exy=es_xd_ym; Eyy=es_yd_ym; Ezy=es_zd_ym; }
       else if( yy < ny1  ){ eey = 0.0f  ; jy = (int)yy; fy = yy-jy;       }
       else                { eey = yy-ny1; jy = ny2    ; fy = 1.0f; aem++; Exy=es_xd_yp; Eyy=es_yd_yp; Ezy=es_zd_yp; }
            if( zz < 0.0f ){ eez = zz    ; kz = 0      ; fz = 0.0f; aem++; Exz=es_xd_zm; Eyz=es_yd_zm; Ezz=es_zd_zm; }
       else if( zz < nz1  ){ eez = 0.0f  ; kz = (int)zz; fz = zz-kz;       }
       else                { eez = zz-nz1; kz = nz2    ; fz = 1.0f; aem++; Exz=es_xd_zp; Eyz=es_yd_zp; Ezz=es_zd_zp; }
       if( aem ){
         uex = Exx*eex + Exy*eey + Exz*eez ;  /* linear extrapolation outside box */
         vex = Eyx*eex + Eyy*eey + Eyz*eez ;
         wex = Ezx*eex + Ezy*eey + Ezz*eez ;
       } else {
         uex = vex = wex = 0.0f ;
       }
     }

     /* special case of no interpolation needed */

     if( fabsf(fx)+fabsf(fy)+fabsf(fz) < 0.00222f ){  /* 06 Nov 2014 */
       uar[pp] = aar[IJK(ix,jy,kz)] + uex ;
       var[pp] = bar[IJK(ix,jy,kz)] + vex ;
       war[pp] = car[IJK(ix,jy,kz)] + wex ;
       continue ;
     }

#if 0  /** this is the old (pre external slopes) method **/
     if( outside ){                 /* use value at nearest edge point */
       qq = ix + jy*nx + kz*nxy ; CLIP(qq,nxyz1) ;
       uar[pp] = aar[qq] ; var[pp] = bar[qq] ; war[pp] = car[qq] ;
       continue ;
     }
#endif

     /*- x interpolations -*/

     for( wsum=0.0f,qq=-IRAD+1 ; qq <= IRAD ; qq++ ){  /* get weights */
       xw  = fabsf(fx - qq) ; wt = sinc(xw) ;
       xw /= WRAD ; if( xw > WCUT ) wt *= wtap(xw) ;
       wtt[qq+(IRAD-1)] = wt ; wsum += wt ;
       iq = ix+qq ; CLIP(iq,nx1) ; iqq[qq+(IRAD-1)] = iq ;
     }
     wfac = wsum ;

     for( jj=-IRAD+1 ; jj <= IRAD ; jj++ ){    /* do interps for */
       jq = jy+jj ; CLIP(jq,ny1) ;             /* each component */
       for( kk=-IRAD+1 ; kk <= IRAD ; kk++ ){
         kq = kz+kk ; CLIP(kq,nz1) ;
         aarjk = AJK(aar,jq,kq) ;
         barjk = AJK(bar,jq,kq) ;
         carjk = AJK(car,jq,kq) ;
         ajk[jj+(IRAD-1)][kk+(IRAD-1)] =
           AW(0)+AW(1)+AW(2)+AW(3)+AW(4)+AW(5)+AW(6)+AW(7)+AW(8)+AW(9) ;
         bjk[jj+(IRAD-1)][kk+(IRAD-1)] =
           BW(0)+BW(1)+BW(2)+BW(3)+BW(4)+BW(5)+BW(6)+BW(7)+BW(8)+BW(9) ;
         cjk[jj+(IRAD-1)][kk+(IRAD-1)] =
           CW(0)+CW(1)+CW(2)+CW(3)+CW(4)+CW(5)+CW(6)+CW(7)+CW(8)+CW(9) ;
       }
     }

     /*- y interpolations -*/

     for( wsum=0.0f,qq=-IRAD+1 ; qq <= IRAD ; qq++ ){  /* get weights */
       yw  = fabsf(fy - qq) ; wt = sinc(yw) ;
       yw /= WRAD ; if( yw > WCUT ) wt *= wtap(yw) ;
       wtt[qq+(IRAD-1)] = wt ; wsum += wt ;
     }
     wfac *= wsum ;

     for( kk=-IRAD+1 ; kk <= IRAD ; kk++ ){  /* interps */
       for( asum=bsum=csum=0.0f,jj=-IRAD+1 ; jj <  IRAD ; jj+=2 ){  /* unrolled by 2 */
         asum += wtt[jj+(IRAD-1)]*ajk[jj+(IRAD-1)][kk+(IRAD-1)]
                +wtt[jj+ IRAD   ]*ajk[jj+ IRAD   ][kk+(IRAD-1)] ;
         bsum += wtt[jj+(IRAD-1)]*bjk[jj+(IRAD-1)][kk+(IRAD-1)]
                +wtt[jj+ IRAD   ]*bjk[jj+ IRAD   ][kk+(IRAD-1)] ;
         csum += wtt[jj+(IRAD-1)]*cjk[jj+(IRAD-1)][kk+(IRAD-1)]
                +wtt[jj+ IRAD   ]*cjk[jj+ IRAD   ][kk+(IRAD-1)] ;
       }
       ak[kk+(IRAD-1)] = asum ;
       bk[kk+(IRAD-1)] = bsum ;
       ck[kk+(IRAD-1)] = csum ;
     }

     /*- z interpolation -*/

     for( wsum=0.0f,qq=-IRAD+1 ; qq <= IRAD ; qq++ ){  /* get weights */
       zw  = fabsf(fz - qq) ; wt = sinc(zw) ;
       zw /= WRAD ; if( zw > WCUT ) wt *= wtap(zw) ;
       wtt[qq+(IRAD-1)] = wt ; wsum += wt ;
     }
     wfac *= wsum ;

     /* interps */

     for( asum=bsum=csum=0.0f,kk=-IRAD+1 ; kk <  IRAD ; kk+=2 ){  /* unrolled by 2 */
       asum += wtt[kk+(IRAD-1)] * ak[kk+(IRAD-1)] + wtt[kk+IRAD] * ak[kk+IRAD] ;
       bsum += wtt[kk+(IRAD-1)] * bk[kk+(IRAD-1)] + wtt[kk+IRAD] * bk[kk+IRAD] ;
       csum += wtt[kk+(IRAD-1)] * ck[kk+(IRAD-1)] + wtt[kk+IRAD] * ck[kk+IRAD] ;
     }

     uar[pp] = asum / wfac + uex ;
     var[pp] = bsum / wfac + vex ;
     war[pp] = csum / wfac + wex ;

   } /* whew! */

 } /* end OpenMP */
 AFNI_OMP_END ;

   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/* define quintic interpolation polynomials (Lagrange) */

#undef  Q_M2
#undef  Q_M1
#undef  Q_00
#undef  Q_P1
#undef  Q_P2
#undef  Q_P3
#define Q_M2(x)  (x*(x*x-1.0f)*(2.0f-x)*(x-3.0f)*0.008333333f)
#define Q_M1(x)  (x*(x*x-4.0f)*(x-1.0f)*(x-3.0f)*0.041666667f)
#define Q_00(x)  ((x*x-4.0f)*(x*x-1.0f)*(3.0f-x)*0.083333333f)
#define Q_P1(x)  (x*(x*x-4.0f)*(x+1.0f)*(x-3.0f)*0.083333333f)
#define Q_P2(x)  (x*(x*x-1.0f)*(x+2.0f)*(3.0f-x)*0.041666667f)
#define Q_P3(x)  (x*(x*x-1.0f)*(x*x-4.0f)*0.008333333f)

/*---------------------------------------------------------------------------*/
/*! Interpolate using quintic method -- see _linear function for relevant
    comments; this is between linear and wsinc5 in accuracy and speed.
*//*-------------------------------------------------------------------------*/

void IW3D_interp_quintic( int nxx , int nyy , int nzz ,
                          float *aar , float *bar , float *car ,
                          int use_es , float *esar ,
                          int npp, float *ip, float *jp, float *kp,
                          float *uar , float *var , float *war     )
{
 AFNI_OMP_START ;
#pragma omp parallel if( npp > 666 )
 {
   int nx=nxx , ny=nyy , nz=nzz , nxy=nx*ny , pp ;
   float xx,yy,zz , fx,fy,fz ;
   int nx1=nx-1,ny1=ny-1,nz1=nz-1, ix,jy,kz ;
   int nx2=nx-2,ny2=ny-2,nz2=nz-2 ;
   int ix_m2,ix_m1,ix_00,ix_p1,ix_p2,ix_p3 ; /* interpolation indices */
   int jy_m2,jy_m1,jy_00,jy_p1,jy_p2,jy_p3 ; /* (input image) */
   int kz_m2,kz_m1,kz_00,kz_p1,kz_p2,kz_p3 ;

   float wt_m2,wt_m1,wt_00,wt_p1,wt_p2,wt_p3 ; /* interpolation weights */

   float f_jm2_km2, f_jm1_km2, f_j00_km2, f_jp1_km2, f_jp2_km2, f_jp3_km2,
         f_jm2_km1, f_jm1_km1, f_j00_km1, f_jp1_km1, f_jp2_km1, f_jp3_km1,
         f_jm2_k00, f_jm1_k00, f_j00_k00, f_jp1_k00, f_jp2_k00, f_jp3_k00,
         f_jm2_kp1, f_jm1_kp1, f_j00_kp1, f_jp1_kp1, f_jp2_kp1, f_jp3_kp1,
         f_jm2_kp2, f_jm1_kp2, f_j00_kp2, f_jp1_kp2, f_jp2_kp2, f_jp3_kp2,
         f_jm2_kp3, f_jm1_kp3, f_j00_kp3, f_jp1_kp3, f_jp2_kp3, f_jp3_kp3,
         f_km2    , f_km1    , f_k00    , f_kp1    , f_kp2    , f_kp3     ;
   float g_jm2_km2, g_jm1_km2, g_j00_km2, g_jp1_km2, g_jp2_km2, g_jp3_km2,
         g_jm2_km1, g_jm1_km1, g_j00_km1, g_jp1_km1, g_jp2_km1, g_jp3_km1,
         g_jm2_k00, g_jm1_k00, g_j00_k00, g_jp1_k00, g_jp2_k00, g_jp3_k00,
         g_jm2_kp1, g_jm1_kp1, g_j00_kp1, g_jp1_kp1, g_jp2_kp1, g_jp3_kp1,
         g_jm2_kp2, g_jm1_kp2, g_j00_kp2, g_jp1_kp2, g_jp2_kp2, g_jp3_kp2,
         g_jm2_kp3, g_jm1_kp3, g_j00_kp3, g_jp1_kp3, g_jp2_kp3, g_jp3_kp3,
         g_km2    , g_km1    , g_k00    , g_kp1    , g_kp2    , g_kp3     ;
   float h_jm2_km2, h_jm1_km2, h_j00_km2, h_jp1_km2, h_jp2_km2, h_jp3_km2,
         h_jm2_km1, h_jm1_km1, h_j00_km1, h_jp1_km1, h_jp2_km1, h_jp3_km1,
         h_jm2_k00, h_jm1_k00, h_j00_k00, h_jp1_k00, h_jp2_k00, h_jp3_k00,
         h_jm2_kp1, h_jm1_kp1, h_j00_kp1, h_jp1_kp1, h_jp2_kp1, h_jp3_kp1,
         h_jm2_kp2, h_jm1_kp2, h_j00_kp2, h_jp1_kp2, h_jp2_kp2, h_jp3_kp2,
         h_jm2_kp3, h_jm1_kp3, h_j00_kp3, h_jp1_kp3, h_jp2_kp3, h_jp3_kp3,
         h_km2    , h_km1    , h_k00    , h_kp1    , h_kp2    , h_kp3     ;

   int ues=use_es ;
   ES_DECLARE_FLOATS ; float uex=0.0f,vex=0.0f,wex=0.0f ;
   float eex=0.0f,eey=0.0f,eez=0.0f , Exx=0.0f,Exy=0.0f,Exz=0.0f ,
         Eyx=0.0f,Eyy=0.0f,Eyz=0.0f , Ezx=0.0f,Ezy=0.0f,Ezz=0.0f ;

   if( ues ) ES_UNPACKVEC(esar) ;

#pragma omp for
   for( pp=0 ; pp < npp ; pp++ ){
     xx = ip[pp] ; yy = jp[pp] ; zz = kp[pp] ;
     if( !ues ){
            if( xx < 0.0f ){ ix = 0       ; fx = 0.0f ; }
       else if( xx < nx1  ){ ix = (int)xx ; fx = xx-ix; }
       else                { ix = nx2     ; fx = 1.0f ; }
            if( yy < 0.0f ){ jy = 0       ; fy = 0.0f ; }
       else if( yy < ny1  ){ jy = (int)yy ; fy = yy-jy; }
       else                { jy = ny2     ; fy = 1.0f ; }
            if( zz < 0.0f ){ kz = 0       ; fz = 0.0f ; }
       else if( zz < nz1  ){ kz = (int)zz ; fz = zz-kz; }
       else                { kz = nz2     ; fz = 1.0f ; }
     } else {
       int aem=0 ;
            if( xx < 0.0f ){ eex = xx    ; ix = 0      ; fx = 0.0f; aem++; Exx=es_xd_xm; Eyx=es_yd_xm; Ezx=es_zd_xm; }
       else if( xx < nx1  ){ eex = 0.0f  ; ix = (int)xx; fx = xx-ix;       }
       else                { eex = xx-nx1; ix = nx2    ; fx = 1.0f; aem++; Exx=es_xd_xp; Eyx=es_yd_xp; Ezx=es_zd_xp; }
            if( yy < 0.0f ){ eey = yy    ; jy = 0      ; fy = 0.0f; aem++; Exy=es_xd_ym; Eyy=es_yd_ym; Ezy=es_zd_ym; }
       else if( yy < ny1  ){ eey = 0.0f  ; jy = (int)yy; fy = yy-jy;       }
       else                { eey = yy-ny1; jy = ny2    ; fy = 1.0f; aem++; Exy=es_xd_yp; Eyy=es_yd_yp; Ezy=es_zd_yp; }
            if( zz < 0.0f ){ eez = zz    ; kz = 0      ; fz = 0.0f; aem++; Exz=es_xd_zm; Eyz=es_yd_zm; Ezz=es_zd_zm; }
       else if( zz < nz1  ){ eez = 0.0f  ; kz = (int)zz; fz = zz-kz;       }
       else                { eez = zz-nz1; kz = nz2    ; fz = 1.0f; aem++; Exz=es_xd_zp; Eyz=es_yd_zp; Ezz=es_zd_zp; }
       if( aem ){
         uex = Exx*eex + Exy*eey + Exz*eez ;
         vex = Eyx*eex + Eyy*eey + Eyz*eez ;
         wex = Ezx*eex + Ezy*eey + Ezz*eez ;
       } else {
         uex = vex = wex = 0.0f ;
       }
     }

     /* special case of no interpolation needed */

     if( fabsf(fx)+fabsf(fy)+fabsf(fz) < 0.00222f ){  /* 06 Nov 2014 */
       uar[pp] = aar[IJK(ix,jy,kz)] + uex ;
       var[pp] = bar[IJK(ix,jy,kz)] + vex ;
       war[pp] = car[IJK(ix,jy,kz)] + wex ;
       continue ;
     }

     /* compute indexes from which to interpolate (-2,-1,0,+1,+2,+3),
        but clipped to lie inside input image volume                 */

     ix_m1 = ix-1    ; ix_00 = ix      ; ix_p1 = ix+1    ; ix_p2 = ix+2    ;
     CLIP(ix_m1,nx1) ; CLIP(ix_00,nx1) ; CLIP(ix_p1,nx1) ; CLIP(ix_p2,nx1) ;
     ix_m2 = ix-2    ; ix_p3 = ix+3 ;
     CLIP(ix_m2,nx1) ; CLIP(ix_p3,nx1) ;

     jy_m1 = jy-1    ; jy_00 = jy      ; jy_p1 = jy+1    ; jy_p2 = jy+2    ;
     CLIP(jy_m1,ny1) ; CLIP(jy_00,ny1) ; CLIP(jy_p1,ny1) ; CLIP(jy_p2,ny1) ;
     jy_m2 = jy-2    ; jy_p3 = jy+3 ;
     CLIP(jy_m2,ny1) ; CLIP(jy_p3,ny1) ;

     kz_m1 = kz-1    ; kz_00 = kz      ; kz_p1 = kz+1    ; kz_p2 = kz+2    ;
     CLIP(kz_m1,nz1) ; CLIP(kz_00,nz1) ; CLIP(kz_p1,nz1) ; CLIP(kz_p2,nz1) ;
     kz_m2 = kz-2    ; kz_p3 = kz+3 ;
     CLIP(kz_m2,nz1) ; CLIP(kz_p3,nz1) ;

     wt_m1 = Q_M1(fx) ; wt_00 = Q_00(fx) ;  /* interpolation weights */
     wt_p1 = Q_P1(fx) ; wt_p2 = Q_P2(fx) ;  /* in x-direction        */
     wt_m2 = Q_M2(fx) ; wt_p3 = Q_P3(fx) ;

#undef  XINT  /* quintic interpolation in the x direction in array aaa */
#define XINT(aaa,j,k) wt_m2*aaa[IJK(ix_m2,j,k)]+wt_m1*aaa[IJK(ix_m1,j,k)] \
                     +wt_00*aaa[IJK(ix_00,j,k)]+wt_p1*aaa[IJK(ix_p1,j,k)] \
                     +wt_p2*aaa[IJK(ix_p2,j,k)]+wt_p3*aaa[IJK(ix_p3,j,k)]

     /* interpolate to location ix+fx at each jy,kz level */

     f_jm2_km2 = XINT(aar,jy_m2,kz_m2) ; f_jm1_km2 = XINT(aar,jy_m1,kz_m2) ;
     f_j00_km2 = XINT(aar,jy_00,kz_m2) ; f_jp1_km2 = XINT(aar,jy_p1,kz_m2) ;
     f_jp2_km2 = XINT(aar,jy_p2,kz_m2) ; f_jp3_km2 = XINT(aar,jy_p3,kz_m2) ;
     f_jm2_km1 = XINT(aar,jy_m2,kz_m1) ; f_jm1_km1 = XINT(aar,jy_m1,kz_m1) ;
     f_j00_km1 = XINT(aar,jy_00,kz_m1) ; f_jp1_km1 = XINT(aar,jy_p1,kz_m1) ;
     f_jp2_km1 = XINT(aar,jy_p2,kz_m1) ; f_jp3_km1 = XINT(aar,jy_p3,kz_m1) ;
     f_jm2_k00 = XINT(aar,jy_m2,kz_00) ; f_jm1_k00 = XINT(aar,jy_m1,kz_00) ;
     f_j00_k00 = XINT(aar,jy_00,kz_00) ; f_jp1_k00 = XINT(aar,jy_p1,kz_00) ;
     f_jp2_k00 = XINT(aar,jy_p2,kz_00) ; f_jp3_k00 = XINT(aar,jy_p3,kz_00) ;
     f_jm2_kp1 = XINT(aar,jy_m2,kz_p1) ; f_jm1_kp1 = XINT(aar,jy_m1,kz_p1) ;
     f_j00_kp1 = XINT(aar,jy_00,kz_p1) ; f_jp1_kp1 = XINT(aar,jy_p1,kz_p1) ;
     f_jp2_kp1 = XINT(aar,jy_p2,kz_p1) ; f_jp3_kp1 = XINT(aar,jy_p3,kz_p1) ;
     f_jm2_kp2 = XINT(aar,jy_m2,kz_p2) ; f_jm1_kp2 = XINT(aar,jy_m1,kz_p2) ;
     f_j00_kp2 = XINT(aar,jy_00,kz_p2) ; f_jp1_kp2 = XINT(aar,jy_p1,kz_p2) ;
     f_jp2_kp2 = XINT(aar,jy_p2,kz_p2) ; f_jp3_kp2 = XINT(aar,jy_p3,kz_p2) ;
     f_jm2_kp3 = XINT(aar,jy_m2,kz_p3) ; f_jm1_kp3 = XINT(aar,jy_m1,kz_p3) ;
     f_j00_kp3 = XINT(aar,jy_00,kz_p3) ; f_jp1_kp3 = XINT(aar,jy_p1,kz_p3) ;
     f_jp2_kp3 = XINT(aar,jy_p2,kz_p3) ; f_jp3_kp3 = XINT(aar,jy_p3,kz_p3) ;

     g_jm2_km2 = XINT(bar,jy_m2,kz_m2) ; g_jm1_km2 = XINT(bar,jy_m1,kz_m2) ;
     g_j00_km2 = XINT(bar,jy_00,kz_m2) ; g_jp1_km2 = XINT(bar,jy_p1,kz_m2) ;
     g_jp2_km2 = XINT(bar,jy_p2,kz_m2) ; g_jp3_km2 = XINT(bar,jy_p3,kz_m2) ;
     g_jm2_km1 = XINT(bar,jy_m2,kz_m1) ; g_jm1_km1 = XINT(bar,jy_m1,kz_m1) ;
     g_j00_km1 = XINT(bar,jy_00,kz_m1) ; g_jp1_km1 = XINT(bar,jy_p1,kz_m1) ;
     g_jp2_km1 = XINT(bar,jy_p2,kz_m1) ; g_jp3_km1 = XINT(bar,jy_p3,kz_m1) ;
     g_jm2_k00 = XINT(bar,jy_m2,kz_00) ; g_jm1_k00 = XINT(bar,jy_m1,kz_00) ;
     g_j00_k00 = XINT(bar,jy_00,kz_00) ; g_jp1_k00 = XINT(bar,jy_p1,kz_00) ;
     g_jp2_k00 = XINT(bar,jy_p2,kz_00) ; g_jp3_k00 = XINT(bar,jy_p3,kz_00) ;
     g_jm2_kp1 = XINT(bar,jy_m2,kz_p1) ; g_jm1_kp1 = XINT(bar,jy_m1,kz_p1) ;
     g_j00_kp1 = XINT(bar,jy_00,kz_p1) ; g_jp1_kp1 = XINT(bar,jy_p1,kz_p1) ;
     g_jp2_kp1 = XINT(bar,jy_p2,kz_p1) ; g_jp3_kp1 = XINT(bar,jy_p3,kz_p1) ;
     g_jm2_kp2 = XINT(bar,jy_m2,kz_p2) ; g_jm1_kp2 = XINT(bar,jy_m1,kz_p2) ;
     g_j00_kp2 = XINT(bar,jy_00,kz_p2) ; g_jp1_kp2 = XINT(bar,jy_p1,kz_p2) ;
     g_jp2_kp2 = XINT(bar,jy_p2,kz_p2) ; g_jp3_kp2 = XINT(bar,jy_p3,kz_p2) ;
     g_jm2_kp3 = XINT(bar,jy_m2,kz_p3) ; g_jm1_kp3 = XINT(bar,jy_m1,kz_p3) ;
     g_j00_kp3 = XINT(bar,jy_00,kz_p3) ; g_jp1_kp3 = XINT(bar,jy_p1,kz_p3) ;
     g_jp2_kp3 = XINT(bar,jy_p2,kz_p3) ; g_jp3_kp3 = XINT(bar,jy_p3,kz_p3) ;

     h_jm2_km2 = XINT(car,jy_m2,kz_m2) ; h_jm1_km2 = XINT(car,jy_m1,kz_m2) ;
     h_j00_km2 = XINT(car,jy_00,kz_m2) ; h_jp1_km2 = XINT(car,jy_p1,kz_m2) ;
     h_jp2_km2 = XINT(car,jy_p2,kz_m2) ; h_jp3_km2 = XINT(car,jy_p3,kz_m2) ;
     h_jm2_km1 = XINT(car,jy_m2,kz_m1) ; h_jm1_km1 = XINT(car,jy_m1,kz_m1) ;
     h_j00_km1 = XINT(car,jy_00,kz_m1) ; h_jp1_km1 = XINT(car,jy_p1,kz_m1) ;
     h_jp2_km1 = XINT(car,jy_p2,kz_m1) ; h_jp3_km1 = XINT(car,jy_p3,kz_m1) ;
     h_jm2_k00 = XINT(car,jy_m2,kz_00) ; h_jm1_k00 = XINT(car,jy_m1,kz_00) ;
     h_j00_k00 = XINT(car,jy_00,kz_00) ; h_jp1_k00 = XINT(car,jy_p1,kz_00) ;
     h_jp2_k00 = XINT(car,jy_p2,kz_00) ; h_jp3_k00 = XINT(car,jy_p3,kz_00) ;
     h_jm2_kp1 = XINT(car,jy_m2,kz_p1) ; h_jm1_kp1 = XINT(car,jy_m1,kz_p1) ;
     h_j00_kp1 = XINT(car,jy_00,kz_p1) ; h_jp1_kp1 = XINT(car,jy_p1,kz_p1) ;
     h_jp2_kp1 = XINT(car,jy_p2,kz_p1) ; h_jp3_kp1 = XINT(car,jy_p3,kz_p1) ;
     h_jm2_kp2 = XINT(car,jy_m2,kz_p2) ; h_jm1_kp2 = XINT(car,jy_m1,kz_p2) ;
     h_j00_kp2 = XINT(car,jy_00,kz_p2) ; h_jp1_kp2 = XINT(car,jy_p1,kz_p2) ;
     h_jp2_kp2 = XINT(car,jy_p2,kz_p2) ; h_jp3_kp2 = XINT(car,jy_p3,kz_p2) ;
     h_jm2_kp3 = XINT(car,jy_m2,kz_p3) ; h_jm1_kp3 = XINT(car,jy_m1,kz_p3) ;
     h_j00_kp3 = XINT(car,jy_00,kz_p3) ; h_jp1_kp3 = XINT(car,jy_p1,kz_p3) ;
     h_jp2_kp3 = XINT(car,jy_p2,kz_p3) ; h_jp3_kp3 = XINT(car,jy_p3,kz_p3) ;

     /* interpolate to jy+fy at each kz level */

     wt_m1 = Q_M1(fy) ; wt_00 = Q_00(fy) ; wt_p1 = Q_P1(fy) ;
     wt_p2 = Q_P2(fy) ; wt_m2 = Q_M2(fy) ; wt_p3 = Q_P3(fy) ;

     f_km2 =  wt_m2 * f_jm2_km2 + wt_m1 * f_jm1_km2 + wt_00 * f_j00_km2
            + wt_p1 * f_jp1_km2 + wt_p2 * f_jp2_km2 + wt_p3 * f_jp3_km2 ;
     f_km1 =  wt_m2 * f_jm2_km1 + wt_m1 * f_jm1_km1 + wt_00 * f_j00_km1
            + wt_p1 * f_jp1_km1 + wt_p2 * f_jp2_km1 + wt_p3 * f_jp3_km1 ;
     f_k00 =  wt_m2 * f_jm2_k00 + wt_m1 * f_jm1_k00 + wt_00 * f_j00_k00
            + wt_p1 * f_jp1_k00 + wt_p2 * f_jp2_k00 + wt_p3 * f_jp3_k00 ;
     f_kp1 =  wt_m2 * f_jm2_kp1 + wt_m1 * f_jm1_kp1 + wt_00 * f_j00_kp1
            + wt_p1 * f_jp1_kp1 + wt_p2 * f_jp2_kp1 + wt_p3 * f_jp3_kp1 ;
     f_kp2 =  wt_m2 * f_jm2_kp2 + wt_m1 * f_jm1_kp2 + wt_00 * f_j00_kp2
            + wt_p1 * f_jp1_kp2 + wt_p2 * f_jp2_kp2 + wt_p3 * f_jp3_kp2 ;
     f_kp3 =  wt_m2 * f_jm2_kp3 + wt_m1 * f_jm1_kp3 + wt_00 * f_j00_kp3
            + wt_p1 * f_jp1_kp3 + wt_p2 * f_jp2_kp3 + wt_p3 * f_jp3_kp3 ;

     g_km2 =  wt_m2 * g_jm2_km2 + wt_m1 * g_jm1_km2 + wt_00 * g_j00_km2
            + wt_p1 * g_jp1_km2 + wt_p2 * g_jp2_km2 + wt_p3 * g_jp3_km2 ;
     g_km1 =  wt_m2 * g_jm2_km1 + wt_m1 * g_jm1_km1 + wt_00 * g_j00_km1
            + wt_p1 * g_jp1_km1 + wt_p2 * g_jp2_km1 + wt_p3 * g_jp3_km1 ;
     g_k00 =  wt_m2 * g_jm2_k00 + wt_m1 * g_jm1_k00 + wt_00 * g_j00_k00
            + wt_p1 * g_jp1_k00 + wt_p2 * g_jp2_k00 + wt_p3 * g_jp3_k00 ;
     g_kp1 =  wt_m2 * g_jm2_kp1 + wt_m1 * g_jm1_kp1 + wt_00 * g_j00_kp1
            + wt_p1 * g_jp1_kp1 + wt_p2 * g_jp2_kp1 + wt_p3 * g_jp3_kp1 ;
     g_kp2 =  wt_m2 * g_jm2_kp2 + wt_m1 * g_jm1_kp2 + wt_00 * g_j00_kp2
            + wt_p1 * g_jp1_kp2 + wt_p2 * g_jp2_kp2 + wt_p3 * g_jp3_kp2 ;
     g_kp3 =  wt_m2 * g_jm2_kp3 + wt_m1 * g_jm1_kp3 + wt_00 * g_j00_kp3
            + wt_p1 * g_jp1_kp3 + wt_p2 * g_jp2_kp3 + wt_p3 * g_jp3_kp3 ;

     h_km2 =  wt_m2 * h_jm2_km2 + wt_m1 * h_jm1_km2 + wt_00 * h_j00_km2
            + wt_p1 * h_jp1_km2 + wt_p2 * h_jp2_km2 + wt_p3 * h_jp3_km2 ;
     h_km1 =  wt_m2 * h_jm2_km1 + wt_m1 * h_jm1_km1 + wt_00 * h_j00_km1
            + wt_p1 * h_jp1_km1 + wt_p2 * h_jp2_km1 + wt_p3 * h_jp3_km1 ;
     h_k00 =  wt_m2 * h_jm2_k00 + wt_m1 * h_jm1_k00 + wt_00 * h_j00_k00
            + wt_p1 * h_jp1_k00 + wt_p2 * h_jp2_k00 + wt_p3 * h_jp3_k00 ;
     h_kp1 =  wt_m2 * h_jm2_kp1 + wt_m1 * h_jm1_kp1 + wt_00 * h_j00_kp1
            + wt_p1 * h_jp1_kp1 + wt_p2 * h_jp2_kp1 + wt_p3 * h_jp3_kp1 ;
     h_kp2 =  wt_m2 * h_jm2_kp2 + wt_m1 * h_jm1_kp2 + wt_00 * h_j00_kp2
            + wt_p1 * h_jp1_kp2 + wt_p2 * h_jp2_kp2 + wt_p3 * h_jp3_kp2 ;
     h_kp3 =  wt_m2 * h_jm2_kp3 + wt_m1 * h_jm1_kp3 + wt_00 * h_j00_kp3
            + wt_p1 * h_jp1_kp3 + wt_p2 * h_jp2_kp3 + wt_p3 * h_jp3_kp3 ;

     /* interpolate to kz+fz to get output */

     wt_m1 = Q_M1(fz) ; wt_00 = Q_00(fz) ; wt_p1 = Q_P1(fz) ;
     wt_p2 = Q_P2(fz) ; wt_m2 = Q_M2(fz) ; wt_p3 = Q_P3(fz) ;

     uar[pp] =  wt_m2 * f_km2 + wt_m1 * f_km1 + wt_00 * f_k00
              + wt_p1 * f_kp1 + wt_p2 * f_kp2 + wt_p3 * f_kp3 + uex ;

     var[pp] =  wt_m2 * g_km2 + wt_m1 * g_km1 + wt_00 * g_k00
              + wt_p1 * g_kp1 + wt_p2 * g_kp2 + wt_p3 * g_kp3 + vex ;

     war[pp] =  wt_m2 * h_km2 + wt_m1 * h_km1 + wt_00 * h_k00
              + wt_p1 * h_kp1 + wt_p2 * h_kp2 + wt_p3 * h_kp3 + wex ;
   }

 } /* end OpenMP */
 AFNI_OMP_END ;

  return ;
}

/*---------------------------------------------------------------------------*/
/* Generic interpolation of warp components, given icode specifying method. */

void IW3D_interp( int icode ,
                  int nxx , int nyy , int nzz ,
                  float *aar , float *bar , float *car ,
                  int use_es , float *esar ,
                  int npp, float *ip, float *jp, float *kp,
                  float *uar , float *var , float *war     )
{
   switch( icode ){
     case MRI_NN:
     case MRI_LINEAR:
       IW3D_interp_linear( nxx , nyy , nzz , aar , bar , car ,
                           use_es , esar ,
                           npp , ip  , jp  , kp  , uar , var , war ) ;
     break ;

     case MRI_CUBIC:
     case MRI_QUINTIC:
       IW3D_interp_quintic( nxx , nyy , nzz , aar , bar , car ,
                            use_es , esar ,
                            npp , ip  , jp  , kp  , uar , var , war ) ;
     break ;

     default:
     case MRI_WSINC5:
       IW3D_interp_wsinc5( nxx , nyy , nzz , aar , bar , car ,
                           use_es , esar ,
                           npp , ip  , jp  , kp  , uar , var , war ) ;
     break ;
   }

   return ;
}

#endif /*(C8)*/ /*############################################################*/

#if 1
/*============================================================================*/
/* (C9) Functions below carry out warp compositions,                          */
/*      including where one of the 'warps' is just a matrix.                  */
/*============================================================================*/

/*---------------------------------------------------------------------------*/

#undef  NPER
#define NPER 4194304  /* 16 Mbyte per temp float array */

/* determine size of a temp array */

#undef  NALL
#define NALL(nnn) (((nnn) > NPER+12345) ? NPER : (nnn))

/*---------------------------------------------------------------------------*/
/* B(A(x)) where B = matrix, A = warp, icode = unused
   -- no interpolation is needed for this operation, since
      the matrix B can just be applied to the warp displacement from A
   -- 'w1m2' means 'warp #1, matrix #2'
*//*-------------------------------------------------------------------------*/

IndexWarp3D * IW3D_compose_w1m2( IndexWarp3D *AA , mat44 BB , int icode )
{
   int nx,ny,nz,nxy,nxyz ;
   float *xda,*yda,*zda , *xdc,*ydc,*zdc ;
   IndexWarp3D *CC=NULL ;
   mat44 BI , BL ;

ENTRY("IW3D_compose_w1m2") ;

   if( AA == NULL ) RETURN(CC) ;

   nx = AA->nx ; ny = AA->ny ; nz = AA->nz ; nxy = nx*ny ; nxyz = nxy*nz ;

   BL = BB ;          /* BL = local copy of B */
   BI = BL ;          /* BI = B - I */
   BI.m[0][0] -= 1.0f ; BI.m[1][1] -= 1.0f ; BI.m[2][2] -= 1.0f ;

   CC = IW3D_empty_copy(AA) ;

   xda = AA->xd ; yda = AA->yd ; zda = AA->zd ;
   xdc = CC->xd ; ydc = CC->yd ; zdc = CC->zd ;

 AFNI_OMP_START ;
#pragma omp parallel if( nxyz > 1111 )
 { int qq , ii,jj,kk ; float xb,yb,zb , xm,ym,zm ;
#pragma omp for
     for( qq=0 ; qq < nxyz ; qq++ ){
       ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
       MAT33_VEC(BL,xda[qq],yda[qq],zda[qq],xb,yb,zb) ;  /* B * dis(x) */
       MAT44_VEC(BI,ii     ,jj     ,kk     ,xm,ym,zm) ;  /* (B-I) * x  */
       xdc[qq] = xb+xm ; ydc[qq] = yb+ym ; zdc[qq] = zb+zm ; /* add up */
     }
 }
 AFNI_OMP_END ;

   IW3D_load_external_slopes(CC) ; RETURN(CC) ;
}

/*---------------------------------------------------------------------------*/
/* A(B(x)) where B = matrix, A = warp, icode = interpolation method;
   -- in this function, interplation IS necessary, since the
      displacement from B(x) will not be exactly on a grid point.
   -- 'm1w2' means 'matrix #1, warp #2'
*//*-------------------------------------------------------------------------*/

IndexWarp3D * IW3D_compose_m1w2( mat44 BB , IndexWarp3D *AA , int icode )
{
   int nx,ny,nz,nxy,nxyz , nall , pp,qtop;
   float *xda,*yda,*zda , *xdc,*ydc,*zdc , *xq,*yq,*zq ;
   IndexWarp3D *CC=NULL ;
   mat44 BL ;
   float esar[18] ;

ENTRY("IW3D_compose_m1w2") ;

   if( AA == NULL ) RETURN(CC) ;

   nx = AA->nx ; ny = AA->ny ; nz = AA->nz ; nxy = nx*ny ; nxyz = nxy*nz ;

   BL = BB ; /* BL = local copy of B */

   CC = IW3D_empty_copy(AA) ;

   xda = AA->xd ; yda = AA->yd ; zda = AA->zd ;
   xdc = CC->xd ; ydc = CC->yd ; zdc = CC->zd ;

   nall = NALL(nxyz) ;

   xq = (float *)malloc(sizeof(float)*nall) ;  /* indexes at which to */
   yq = (float *)malloc(sizeof(float)*nall) ;  /* interpolate the AA warp */
   zq = (float *)malloc(sizeof(float)*nall) ;

   for( pp=0 ; pp < nxyz ; pp+=nall ){  /* loop over segments */

     qtop = MIN( nxyz , pp+nall ) ;  /* process points from pp to qtop-1 */

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 1111 )
 { int qq , ii,jj,kk ;
#pragma ivdep  /* for Intel icc compiler */
#pragma omp for
     for( qq=pp ; qq < qtop ; qq++ ){
       ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;  /* compute xq, */
       MAT44_VEC(BL,ii,jj,kk,xq[qq-pp],yq[qq-pp],zq[qq-pp]) ;  /* yq, and zd */
     }
 }
 AFNI_OMP_END ;

     /* Interpolate A() warp index displacments at the B(x) locations */

     if( AA->use_es ) ES_PACK(AA,esar) ;  /* load external slopes */
     IW3D_interp( icode, nx,ny,nz , xda   , yda   , zda      ,
                                    AA->use_es    , esar     ,
                         qtop-pp  , xq    , yq    , zq       ,
                                    xdc+pp, ydc+pp, zdc+pp    ) ;

     /* Add in the B(x) displacments to get the total
        index displacment from each original position: B(x)-x + A(x+B(x)) */

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 1111 )
 { int qq, ii,jj,kk ;
#pragma omp for
     for( qq=pp ; qq < qtop ; qq++ ){
       ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
       xdc[qq] += xq[qq-pp] - ii ;
       ydc[qq] += yq[qq-pp] - jj ;
       zdc[qq] += zq[qq-pp] - kk ;
     }
 }
 AFNI_OMP_END ;

   } /* end of loop over segments of length NPER (or less) */

   free(zq) ; free(yq) ; free(xq) ;
   IW3D_load_external_slopes(CC) ; RETURN(CC) ;
}

/*---------------------------------------------------------------------------*/
/* Compute B(A(x)) where both B and A are warps */

IndexWarp3D * IW3D_compose( IndexWarp3D *AA , IndexWarp3D *BB , int icode )
{
   int nx,ny,nz,nxy,nxyz , nall , pp,qtop;
   float *xda,*yda,*zda , *xq,*yq,*zq , *xdc,*ydc,*zdc ;
   IndexWarp3D *CC ;
   float esar[18] ;

ENTRY("IW3D_compose") ;

        if( AA == NULL ){ CC = IW3D_copy(BB,1.0f) ; RETURN(CC) ; }
   else if( BB == NULL ){ CC = IW3D_copy(AA,1.0f) ; RETURN(CC) ; }

   nx = AA->nx ; ny = AA->ny ; nz = AA->nz ; nxy = nx*ny ; nxyz = nxy*nz ;

   if( nx != BB->nx || ny != BB->ny || nz != BB->nz ) RETURN(NULL) ;

   nall = NALL(nxyz) ;

   xq = (float *)malloc(sizeof(float)*nall) ;  /* workspaces */
   yq = (float *)malloc(sizeof(float)*nall) ;  /* for x+A(x) */
   zq = (float *)malloc(sizeof(float)*nall) ;

   CC = IW3D_empty_copy(AA) ;  /* output warp */

   xda = AA->xd ; yda = AA->yd ; zda = AA->zd ;  /* input displacements */
   xdc = CC->xd ; ydc = CC->yd ; zdc = CC->zd ;

   for( pp=0 ; pp < nxyz ; pp+=nall ){  /* loop over segments */

     qtop = MIN( nxyz , pp+nall ) ;  /* process points from pp to qtop-1 */

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 1111 )
 { int qq , ii,jj,kk ;
#pragma omp for
     for( qq=pp ; qq < qtop ; qq++ ){
       ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
       xq[qq-pp] = ii + xda[qq] ;  /* x+A(x) warped indexes */
       yq[qq-pp] = jj + yda[qq] ;
       zq[qq-pp] = kk + zda[qq] ;
     }
 }
 AFNI_OMP_END ;

     /* Interpolate B() warp index displacments at the x+A(x) locations */

     if( BB->use_es ) ES_PACK(BB,esar) ;
     IW3D_interp( icode, nx,ny,nz , BB->xd, BB->yd, BB->zd ,
                                    BB->use_es , esar      ,
                         qtop-pp  , xq    , yq    , zq     ,
                                    xdc+pp, ydc+pp, zdc+pp  ) ;

     /* Add in the A() displacments to get the total
        index displacment from each original position: A(x) + B(x+A(x)) */

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 1111 )
 { int qq ;
#pragma omp for
     for( qq=pp ; qq < qtop ; qq++ ){
       xdc[qq] += xda[qq] ; ydc[qq] += yda[qq] ; zdc[qq] += zda[qq] ;
     }
 }
 AFNI_OMP_END ;

   } /* end of loop over segments of length NPER (or less) */

   free(zq) ; free(yq) ; free(xq) ;
   IW3D_load_external_slopes(CC) ; RETURN(CC) ;
}

#if 0  /* this code is never used, so is left out */
/*---------------------------------------------------------------------------*/
/* Compute A^(2^lev) , using linear interpolation only */

IndexWarp3D * IW3D_2pow( IndexWarp3D *AA , int lev )
{
   int nx,ny,nz,nxy,nxyz , nall , pp,qtop , ll ;
   float *xdb,*ydb,*zdb , *xq,*yq,*zq , *xdc,*ydc,*zdc ;
   IndexWarp3D *BB , *CC , *TT ;
   float esar[18] ;

ENTRY("IW3D_2pow") ;

   if( AA == NULL ) RETURN(NULL) ;  /* duh */

   /* simple case of squaring */

   if( lev == 1 ){ BB = IW3D_compose(AA,AA,MRI_LINEAR) ; RETURN(BB) ; }

   BB = IW3D_copy(AA,1.0f) ;  /* BB = AA (lev=0 result) */

   if( lev <= 0 ) RETURN(BB) ;

   nx = BB->nx ; ny = BB->ny ; nz = BB->nz ; nxy = nx*ny ; nxyz = nxy*nz ;

   nall = NALL(nxyz) ;

   xq = (float *)malloc(sizeof(float)*nall) ;  /* workspace */
   yq = (float *)malloc(sizeof(float)*nall) ;
   zq = (float *)malloc(sizeof(float)*nall) ;

   CC = IW3D_empty_copy(BB) ;

   /* input = BB ; compute CC = BB(BB(x)) ;
      then swap so output = BB ; wash, rinse, repeat */

   for( ll=1 ; ll <= lev ; ll++ ){

     xdb = BB->xd ; ydb = BB->yd ; zdb = BB->zd ;
     xdc = CC->xd ; ydc = CC->yd ; zdc = CC->zd ;

     for( pp=0 ; pp < nxyz ; pp+=nall ){  /* loop over segments */

       qtop = MIN( nxyz , pp+nall ) ;  /* process points from pp to qtop-1 */

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 1111 )
 { int qq , ii,jj,kk ;
#pragma omp for
       for( qq=pp ; qq < qtop ; qq++ ){
         ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
         xq[qq-pp] = ii + xdb[qq] ;  /* x+B(x) warped indexes */
         yq[qq-pp] = jj + ydb[qq] ;
         zq[qq-pp] = kk + zdb[qq] ;
       }
 }
 AFNI_OMP_END ;

       /* Interpolate B() warp index displacments,
          at the B() locations, into the C() warp */

       if( BB->use_es ) ES_PACK(BB,esar) ;
       IW3D_interp_linear( nx,ny,nz , xdb   , ydb   , zdb   ,
                           BB->use_es , esar ,
                           qtop-pp  , xq    , yq    , zq    ,
                                      xdc+pp, ydc+pp, zdc+pp ) ;

        /* Add in the B() displacments to get the total
           index displacment from each original position: B(x) + B(x+B(x)) */

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 1111 )
 { int qq ;
#pragma omp for
        for( qq=pp ; qq < qtop ; qq++ ){
          xdc[qq] += xdb[qq] ; ydc[qq] += ydb[qq] ; zdc[qq] += zdb[qq] ;
        }
 }
 AFNI_OMP_END ;

      } /* end of loop over segments of length NPER (or less) */

      /* at this point, CC = BB(BB(x)) ;
         now swap them, to square BB again on next time thru loop */

      TT = CC ; CC = BB ; BB = TT ;
      IW3D_load_external_slopes(BB) ;
   }

   /* at the end, BB is the result, and CC is trash */

   IW3D_destroy(CC) ; free(zq) ; free(yq) ; free(xq) ;
   RETURN(BB) ;
}
#endif

#endif /*(C9)*/ /*############################################################*/

#if 1
/*============================================================================*/
/* (C10) Functions to invert an index warp in its entirety                    */
/*============================================================================*/

/*---------------------------------------------------------------------------*/
/* Compute B( 2x - A(B(x)) ) = Newton step for computing Ainv(x),
   given an initial estimate of the inverse in B(x);
   -- actually, a damping factor is applied so that the output is actually
      C(x) = (1-inewtfac) * B(x) + inewtfac * B( 2x - A(B(x)) ) ;
      the purpose of this is to reduce any instabilities in the iteration.
   -- the calling function may adjust inewtfac up or down
*//*-------------------------------------------------------------------------*/

static float inewtfac = 0.5f ; /* damping factor for iteration */
static int   inewtfix = 0 ;    /* is damping factor to be fixed? */

IndexWarp3D * IW3D_invert_newt( IndexWarp3D *AA, IndexWarp3D *BB, int icode )
{
   int nx,ny,nz,nxy,nxyz , nall , pp,qtop ;
   float *xda,*yda,*zda , *xq,*yq,*zq,*xr,*yr,*zr , *xdc,*ydc,*zdc , *xdb,*ydb,*zdb ;
   IndexWarp3D *CC ;
   float esar[18] ;

ENTRY("IW3D_invert_newt") ;

   if( AA == NULL || BB == NULL ) RETURN(NULL) ;  /* stoopidd luser */

   nx = AA->nx ; ny = AA->ny ; nz = AA->nz ; nxy = nx*ny ; nxyz = nxy*nz ;

   if( nx != BB->nx || ny != BB->ny || nz != BB->nz ) RETURN(NULL) ;

   nall = NALL(nxyz) ;                /* workspace size */

   xq = (float *)malloc(sizeof(float)*nall) ;  /* workspace */
   yq = (float *)malloc(sizeof(float)*nall) ;
   zq = (float *)malloc(sizeof(float)*nall) ;

   xr = (float *)malloc(sizeof(float)*nall) ;
   yr = (float *)malloc(sizeof(float)*nall) ;
   zr = (float *)malloc(sizeof(float)*nall) ;

   CC = IW3D_empty_copy(AA) ;  /* will be the output */

   xda = AA->xd ; yda = AA->yd ; zda = AA->zd ;
   xdb = BB->xd ; ydb = BB->yd ; zdb = BB->zd ;
   xdc = CC->xd ; ydc = CC->yd ; zdc = CC->zd ;

   /* Warps are stored as voxel index displacements, so we have
        A(x)              = x + a(x)  [not needed below]
        B(x)              = x + b(x)
        A(B(x))           = x + b(x) + a(x+b(x))
        2x - A(B(x))      = x - b(x) - a(x+b(x))
        B( 2x - A(B(x)) ) = x - b(x) - a(x+b(x)) + b(x-b(x)-a(x+b(x)))
      These steps are carried out in the inner loops below:           */

   for( pp=0 ; pp < nxyz ; pp+=nall ){  /* loop over segments */

     qtop = MIN( nxyz , pp+nall ) ;  /* process points from pp to qtop-1 */

     /* Compute [xq,yq,zq] = x+b(x) */

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 1111 )
 { int qq , ii,jj,kk ;
#pragma ivdep
#pragma omp for
     for( qq=pp ; qq < qtop ; qq++ ){
       ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
       xq[qq-pp] = ii + xdb[qq] ;
       yq[qq-pp] = jj + ydb[qq] ;
       zq[qq-pp] = kk + zdb[qq] ;
     }
 }
 AFNI_OMP_END ;

     /* Compute [xr,yr,zr] = a(x+b(x)) */

     if( AA->use_es ) ES_PACK(AA,esar) ;
     IW3D_interp( icode, nx,ny,nz , xda, yda, zda,
                         AA->use_es , esar ,
                         qtop-pp  , xq , yq , zq ,
                                    xr , yr , zr  ) ;

     /* Compute [xr,yr,zr] = x - b(x) - a(x+b(x)) */

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 1111 )
 { int qq , ii,jj,kk ;
#pragma ivdep
#pragma omp for
     for( qq=pp ; qq < qtop ; qq++ ){
       ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
       xr[qq-pp] = ii - xdb[qq] - xr[qq-pp] ;
       yr[qq-pp] = jj - ydb[qq] - yr[qq-pp] ;
       zr[qq-pp] = kk - zdb[qq] - zr[qq-pp] ;
     }
 }
 AFNI_OMP_END ;

     /* Compute [xq,yq,zq] = b(x-b(x)-a(x+b(x))) */

     if( BB->use_es ) ES_PACK(BB,esar) ;
     IW3D_interp( icode, nx,ny,nz , xdb, ydb, zdb,
                         BB->use_es , esar ,
                         qtop-pp  , xr , yr , zr ,
                                    xq , yq , zq  ) ;

     /* Compute result = -b(x) - a(x+b(x)) + b(x-b(x)-a(x+b(x))) */

     if( inewtfac <= 0.0f ){  /* undamped Newton */

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 1111 )
 { int qq , ii,jj,kk ;
#pragma ivdep
#pragma omp for
       for( qq=pp ; qq < qtop ; qq++ ){
         ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
         xdc[qq] = xr[qq-pp] - ii + xq[qq-pp] ;
         ydc[qq] = yr[qq-pp] - jj + yq[qq-pp] ;
         zdc[qq] = zr[qq-pp] - kk + zq[qq-pp] ;
       }
 }
 AFNI_OMP_END ;

     } else {                 /* damped Newton (for stability) */

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 1111 )
 { int qq , ii,jj,kk ;
   register float nf , nf1 ;
   nf = inewtfac ; nf1 = 1.0f - nf ;
#pragma ivdep
#pragma omp for
       for( qq=pp ; qq < qtop ; qq++ ){
         ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
         xdc[qq] = nf * (xr[qq-pp]-ii+xq[qq-pp]) + nf1*xdb[qq] ;
         ydc[qq] = nf * (yr[qq-pp]-jj+yq[qq-pp]) + nf1*ydb[qq] ;
         zdc[qq] = nf * (zr[qq-pp]-kk+zq[qq-pp]) + nf1*zdb[qq] ;
       }
 }
 AFNI_OMP_END ;

     }

   } /* end of loop over segments of length NPER (or less) */

   free(zr); free(yr); free(xr); free(zq); free(yq); free(xq);
   IW3D_load_external_slopes(CC) ; RETURN(CC);
}

/*---------------------------------------------------------------------------*/
/* Find the inverse warp BB(x) to AA(x).  If not NULL, BBinit is the
   initial estimate for BB(x).  icode tells how to interpolate warps.
*//*-------------------------------------------------------------------------*/

IndexWarp3D * IW3D_invert( IndexWarp3D *AA , IndexWarp3D *BBinit , int icode )
{
   IndexWarp3D *BB , *CC ;
   float normAA , normBC , nrat , orat ;
   int ii , nnewt=0 , nss,nii , jcode=MRI_LINEAR ;
   float switchval=0.001f ;

ENTRY("IW3D_invert") ;

   if( AA == NULL ) RETURN(NULL) ;

   normAA = IW3D_normLinf( AA , NULL ) ;
   if( normAA == 0.0f ){                       /* input AA is all zero?! */
     BB = IW3D_empty_copy(AA) ; RETURN(BB) ;
   }
   if( icode == MRI_WSINC5 ) icode = MRI_QUINTIC ; /* wsinc5 is too slow */

   /* BB = initial guess at inverse */

   if( verb_nww ) ININFO_message(" -- invert max|AA|=%f",normAA) ;

   if( BBinit == NULL ){  /* approximate inverse by power iteration */
     int pp = 1+(int)ceil(log2(normAA)) ;  /* number of iterations */
     float qq ;
     if( pp < 2 ) pp = 2 ;
     qq = pow(0.5,pp) ;
     if( verb_nww ) ININFO_message("  - init nstep=%d qq=1/2^%d=%f",pp,pp,qq) ;
     BB = IW3D_copy( AA,-qq ) ;     /* BB = I-qq*a(x) where A(x) = x + a(x) */
     for( ii=0 ; ii < pp ; ii++ ){  /* compute BB = [I-qq*a(x)]^pp */
       if( verb_nww > 1 ) ININFO_message("  - init step %d",ii+1) ;
       else if( Hverb && !verb_nww )   fprintf(stderr,"*") ;
       CC = IW3D_compose(BB,BB,jcode) ; IW3D_destroy(BB) ; BB = CC ;
     }
   } else {
     BB = IW3D_copy( BBinit , 1.0f ) ;  /* that was easy */
   }

   normAA = IW3D_normL1( AA , NULL ) ;
   if( !inewtfix ){                              /* initialize Newton */
     inewtfac = 2.0f / (2.0f+sqrtf(normAA)) ;    /* damping factor based */
     if( inewtfac > 0.333f ) inewtfac = 0.333f ; /* on size of input AA */
   }

   if( verb_nww )
     ININFO_message("  - start iterations: normAA=%f inewtfac=%f",normAA,inewtfac) ;
   else if( Hverb )
     fprintf(stderr,".") ;  /* for 3dNwarpAdjust (etc.) user pacification */

   /* iterate some, until convergence or exhaustion */

#if 0
   if( getenv("AFNI_NWARP_SWITCHVAL") != NULL ){
     switchval = (float)AFNI_numenv("AFNI_NWARP_SWITCHVAL") ;
          if( switchval <= 0.0002f ) switchval = 0.0002f ;
     else if( switchval >= 0.0100f ) switchval = 0.0100f ;
   }
#endif

   nrat = 666.666f ;  /* will be convergence criterion */

   for( nii=nss=ii=0 ; ii < 69 ; ii++ ){ /* shouldn't take so many iterations */

     /* take a Newton step [linear interp to start, higher order later] */

     CC = BB ; BB = IW3D_invert_newt(AA,CC,jcode) ;

     if( !verb_nww && Hverb ) fprintf(stderr,".") ;  /* for 3dNwarpAdjust */

     /* how close are they now? */

     normBC = IW3D_normL1( BB , CC ) ; IW3D_destroy(CC) ;

     orat = nrat ; nrat = normBC / normAA ; /* this is the closeness measure */

     if( verb_nww ) ININFO_message("  - iterate %d nrat=%f",++nnewt,nrat) ;

     /* check for convergence of B and C or at least closeness */

     if( jcode != icode && nrat < switchval ){ /* switch to better interp? */
       jcode = icode ; nss = 0 ;
       if( verb_nww ) ININFO_message("  - switching from linear interp") ;
       else if( Hverb ) fprintf(stderr,":") ;
       continue ;                        /* iterate at least one more time */
     }

     if( nrat < 0.0001f ){  /* maybe we're done already? */
       if( verb_nww ) ININFO_message(" -- iteration converged") ;
       RETURN(BB) ;         /* converged! */
     }

     /* if things are getting worse, what to do?! */

     if( ii > 3 && nrat > orat ){
       nii++ ; if( nii == 2 ) break ;  /* getting worse! :-( */
     } else {
       nii = 0 ;
     }

     /* update the damping factor for the next Newton step;
        make it larger if things got better, make it smaller if worser */

     if( !inewtfix ){
       if( nss > 0 && nrat < 0.199f && nrat < orat && inewtfac < 0.678901f ){
         nss = 0 ; inewtfac *= 1.234f ; if( inewtfac > 0.678901f ) inewtfac = 0.678901f ;
         if( verb_nww > 1 ) ININFO_message("  - switch to inewtfac=%f",inewtfac) ;
       } else if( nss > 0 && nrat > orat ){
         nss = -66 ; inewtfac *= 0.444f ;
         if( verb_nww > 1 ) ININFO_message("  - switch to inewtfac=%f",inewtfac) ;
       } else {
         nss++ ;
       }
     }

   } /* end of iteration loop */

   /* failed to converge, return latest result anyhoo [not common] */

   WARNING_message("IW3D_invert: iterations failed to converge :-(") ;
   RETURN(BB) ;
}

/*---------------------------------------------------------------------------*/
/* Invert a dataset that represents a warp.
   This is done the brute force way, by conversion to an index warp,
   inversion of that, and then conversion back to a dataset struct.
*//*-------------------------------------------------------------------------*/

THD_3dim_dataset * THD_nwarp_invert( THD_3dim_dataset *dset_nwarp )
{
   IndexWarp3D *AA , *BB ;
   THD_3dim_dataset *qset ;

ENTRY("THD_nwarp_invert") ;

   if( dset_nwarp == NULL || DSET_NVALS(dset_nwarp) < 3 ) RETURN(NULL) ;
   DSET_load(dset_nwarp) ; if( !DSET_LOADED(dset_nwarp) ) RETURN(NULL) ;

   AA = IW3D_from_dataset( dset_nwarp , 0 , 0 ) ;  DSET_unload(dset_nwarp) ;
   BB = IW3D_extend( AA ,  32, 32, 32, 32, 32, 32 , 0 ) ; IW3D_destroy(AA) ;
   AA = IW3D_invert( BB , NULL , MRI_QUINTIC ) ;          IW3D_destroy(BB) ;
   BB = IW3D_extend( AA , -32,-32,-32,-32,-32,-32 , 0 ) ; IW3D_destroy(AA) ;

   qset = IW3D_to_dataset( BB , "InvertedWarp" ) ;        IW3D_destroy(BB) ;
   qset->view_type = dset_nwarp->view_type ;
   THD_init_diskptr_names( qset->dblk->diskptr, NULL,NULL,NULL, qset->view_type, False ) ;
   RETURN(qset) ;
}

#endif /*(C10)*/ /*###########################################################*/

#if 1
/*============================================================================*/
/* (C11) Functions to compute the 'square root' of a warp.  There are various
   flavors here, because this is a difficult problem that may not always
   have an exact solution.  On the other hand, the square root is probably
   not actually very useful (esp. given the 3dQwarp -plusminus option).       */
/*============================================================================*/

#define USE_SQRTPAIR  /* this enables the better code (farther below) */

#ifndef USE_SQRTPAIR  /* old and worser method [sometimes does not converge] */
/*----------------------------------------------------------------------------*/
/* Iteration step for sqrt:  Bnew(x) = B( 1.5*x - 0.5*A(B(B(x))) )
   This is actually a step to produce the square root of inverse(A).
   [This method can be unstable -- the IW3D_sqrtpair functions are better.]
*//*-------------------------------------------------------------------------*/

static float sstepfac = 0.5f ;

static float sstepfac_MAX = 0.432111f ;
static float sstepfac_MIN = 0.135799f ;

IndexWarp3D * IW3D_sqrtinv_step( IndexWarp3D *AA, IndexWarp3D *BB, int icode )
{
   int nx,ny,nz,nxy,nxyz , nall , pp,qtop ;
   float *xda,*yda,*zda , *xq,*yq,*zq,*xr,*yr,*zr , *xdc,*ydc,*zdc , *xdb,*ydb,*zdb ;
   IndexWarp3D *CC ;
   float esar[18] ;

ENTRY("IW3D_sqrtinv_step") ;

   if( AA == NULL || BB == NULL ) RETURN(NULL) ;  /* stoopidd luser */

   nx = AA->nx ; ny = AA->ny ; nz = AA->nz ; nxy = nx*ny ; nxyz = nxy*nz ;

   if( nx != BB->nx || ny != BB->ny || nz != BB->nz ) RETURN(NULL) ;

   nall = NALL(nxyz) ;

   xq = (float *)malloc(sizeof(float)*nall) ;  /* workspace */
   yq = (float *)malloc(sizeof(float)*nall) ;
   zq = (float *)malloc(sizeof(float)*nall) ;

   xr = (float *)malloc(sizeof(float)*nall) ;
   yr = (float *)malloc(sizeof(float)*nall) ;
   zr = (float *)malloc(sizeof(float)*nall) ;

   CC = IW3D_empty_copy(AA) ;

   xda = AA->xd ; yda = AA->yd ; zda = AA->zd ;
   xdb = BB->xd ; ydb = BB->yd ; zdb = BB->zd ;
   xdc = CC->xd ; ydc = CC->yd ; zdc = CC->zd ;

   /* Warps are stored as displacments:
       A(x) = x + a(x)
       B(x) = x + b(x)  et cetera */

   for( pp=0 ; pp < nxyz ; pp+=nall ){  /* loop over segments */

     qtop = MIN( nxyz , pp+nall ) ;  /* process points from pp to qtop-1 */

     /* Compute [xq,yq,zq] = B(x) = x+b(x) */

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 1111 )
 { int qq , ii,jj,kk ;
#pragma omp for
     for( qq=pp ; qq < qtop ; qq++ ){
       ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
       xq[qq-pp] = ii + xdb[qq] ;
       yq[qq-pp] = jj + ydb[qq] ;
       zq[qq-pp] = kk + zdb[qq] ;
     }
 }
 AFNI_OMP_END ;

     /* Compute [xr,yr,zr] = b(B(x)) */

     if( BB->use_es ) ES_PACK(BB,esar) ;
     IW3D_interp( icode, nx,ny,nz , xdb, ydb, zdb,
                         BB->use_es , esar ,
                         qtop-pp  , xq , yq , zq ,
                                    xr , yr , zr  ) ;

     /* Compute [xr,yr,zr] = B(B(x)) = B(x) + b(B(x)) */

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 1111 )
 { int qq ;
#pragma omp for
     for( qq=pp ; qq < qtop ; qq++ ){
       xr[qq-pp] += xq[qq-pp] ;
       yr[qq-pp] += yq[qq-pp] ;
       zr[qq-pp] += zq[qq-pp] ;
     }
 }
 AFNI_OMP_END ;

     /* Compute [xq,yq,zq] = a(B(B(x))) */

     if( AA->use_es ) ES_PACK(AA,esar) ;
     IW3D_interp( icode, nx,ny,nz , xda, yda, zda,
                         AA->use_es , esar ,
                         qtop-pp  , xr , yr , zr ,
                                    xq , yq , zq  ) ;

     /* Compute [xq,yq,zq] = 1.5*x - 0.5*A(B(B(x)))
                           = 1.5*x - 0.5*( B(B(x)) + a(B(B(x))) ) */

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 1111 )
 { int qq , ii,jj,kk ;
#pragma omp for
     for( qq=pp ; qq < qtop ; qq++ ){
       ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
       xq[qq-pp] = 1.5f*ii - 0.5f*( xr[qq-pp] + xq[qq-pp] ) ;
       yq[qq-pp] = 1.5f*jj - 0.5f*( yr[qq-pp] + yq[qq-pp] ) ;
       zq[qq-pp] = 1.5f*kk - 0.5f*( zr[qq-pp] + zq[qq-pp] ) ;
     }
 }
 AFNI_OMP_END ;

     /* Compute [xr,yr,zr] = b(1.5*x - 0.5*A(B(B(x)))) */

     if( BB->use_es ) ES_PACK(BB,esar) ;
     IW3D_interp( icode, nx,ny,nz , xdb, ydb, zdb,
                         BB->use_es , esar ,
                         qtop-pp  , xq , yq , zq ,
                                    xr , yr , zr  ) ;

     /* Compute the answer: B(1.5*x - 0.5*A(B(B(x))))
                          = 1.5*x - 0.5*A(B(B(x))) + b(1.5*x - 0.5*A(B(B(x)))) */

     if( sstepfac <= 0.0f ){

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 1111 )
 { int qq , ii,jj,kk ;
#pragma omp for
       for( qq=pp ; qq < qtop ; qq++ ){
         ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
         xdc[qq] = xq[qq-pp] + xr[qq-pp] - ii ; /* must subtract off x [ii,jj,kk] */
         ydc[qq] = yq[qq-pp] + yr[qq-pp] - jj ; /* to make result be displacments */
         zdc[qq] = zq[qq-pp] + zr[qq-pp] - kk ;
       }
 }
 AFNI_OMP_END ;

     } else {

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 1111 )
 { int qq , ii,jj,kk ;
   register float sf , sf1 ;
   sf = sstepfac ; sf1 = 1.0f - sf ;
#pragma omp for
       for( qq=pp ; qq < qtop ; qq++ ){
         ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
         xdc[qq] = sf * (xq[qq-pp] + xr[qq-pp] - ii) + sf1 * xdb[qq] ;
         ydc[qq] = sf * (yq[qq-pp] + yr[qq-pp] - jj) + sf1 * ydb[qq] ;
         zdc[qq] = sf * (zq[qq-pp] + zr[qq-pp] - kk) + sf1 * zdb[qq] ;
       }
 }
 AFNI_OMP_END ;

     }

   } /* end of loop over segments of length NPER (or less) */

   free(zr); free(yr); free(xr); free(zq); free(yq); free(xq);
   IW3D_load_external_slopes(CC) ; RETURN(CC);
}

/*---------------------------------------------------------------------------*/
/*** This is bad (unstable) -- don't use it! ***/
#if 0
IndexWarp3D * IW3D_sqrtinv_stepQ( IndexWarp3D *AA, IndexWarp3D *BB, int icode )
{
   int nx,ny,nz,nxy,nxyz , nall , pp,qtop ;
   float *xda,*yda,*zda , *xq,*yq,*zq,*xr,*yr,*zr , *xdc,*ydc,*zdc , *xdb,*ydb,*zdb ;
   IndexWarp3D *CC ;
   float esar[18] ;

ENTRY("IW3D_sqrtinv_stepQ") ;

   if( AA == NULL || BB == NULL ) RETURN(NULL) ;  /* stoopidd luser */

   nx = AA->nx ; ny = AA->ny ; nz = AA->nz ; nxy = nx*ny ; nxyz = nxy*nz ;

   if( nx != BB->nx || ny != BB->ny || nz != BB->nz ) RETURN(NULL) ;

   nall = NALL(nxyz) ;

   xq = (float *)malloc(sizeof(float)*nall) ;  /* workspace */
   yq = (float *)malloc(sizeof(float)*nall) ;
   zq = (float *)malloc(sizeof(float)*nall) ;

   xr = (float *)malloc(sizeof(float)*nall) ;
   yr = (float *)malloc(sizeof(float)*nall) ;
   zr = (float *)malloc(sizeof(float)*nall) ;

   CC = IW3D_empty_copy(AA) ;

   xda = AA->xd ; yda = AA->yd ; zda = AA->zd ;
   xdb = BB->xd ; ydb = BB->yd ; zdb = BB->zd ;
   xdc = CC->xd ; ydc = CC->yd ; zdc = CC->zd ;

   /* Warps are stored as displacments:
       A(x) = x + a(x)
       B(x) = x + b(x)  et cetera */

   for( pp=0 ; pp < nxyz ; pp+=nall ){  /* loop over segments */

     qtop = MIN( nxyz , pp+nall ) ;  /* process points from pp to qtop-1 */

     /* Compute [xq,yq,zq] = A(x) = x+a(x) */

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 1111 )
 { int qq , ii,jj,kk ;
#pragma omp for
     for( qq=pp ; qq < qtop ; qq++ ){
       ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
       xq[qq-pp] = ii + xda[qq] ;
       yq[qq-pp] = jj + yda[qq] ;
       zq[qq-pp] = kk + zda[qq] ;
     }
 }
 AFNI_OMP_END ;

     /* Compute [xr,yr,zr] = b(A(x)) */

     if( BB->use_es ) ES_PACK(BB,esar) ;
     IW3D_interp( icode, nx,ny,nz , xdb, ydb, zdb,
                         BB->use_es , esar ,
                         qtop-pp  , xq , yq , zq ,
                                    xr , yr , zr  ) ;

     /* Compute [xr,yr,zr] = B(A(x)) = A(x) + b(A(x)) */

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 1111 )
 { int qq ;
#pragma omp for
     for( qq=pp ; qq < qtop ; qq++ ){
       xr[qq-pp] += xq[qq-pp] ;
       yr[qq-pp] += yq[qq-pp] ;
       zr[qq-pp] += zq[qq-pp] ;
     }
 }
 AFNI_OMP_END ;

     /* Compute [xq,yq,zq] = b(B(A(x))) */

     if( AA->use_es ) ES_PACK(AA,esar) ;
     IW3D_interp( icode, nx,ny,nz , xdb, ydb, zdb,
                         AA->use_es , esar ,
                         qtop-pp  , xr , yr , zr ,
                                    xq , yq , zq  ) ;

     /* Compute [xq,yq,zq] = 1.5*x - 0.5*B(B(A(x)))
                           = 1.5*x - 0.5*( B(A(x)) + b(B(A(x))) ) */

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 1111 )
 { int qq , ii,jj,kk ;
#pragma omp for
     for( qq=pp ; qq < qtop ; qq++ ){
       ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
       xq[qq-pp] = 1.5f*ii - 0.5f*( xr[qq-pp] + xq[qq-pp] ) ;
       yq[qq-pp] = 1.5f*jj - 0.5f*( yr[qq-pp] + yq[qq-pp] ) ;
       zq[qq-pp] = 1.5f*kk - 0.5f*( zr[qq-pp] + zq[qq-pp] ) ;
     }
 }
 AFNI_OMP_END ;

     /* Compute [xr,yr,zr] = b(1.5*x - 0.5*B(B(A(x)))) */

     if( BB->use_es ) ES_PACK(BB,esar) ;
     IW3D_interp( icode, nx,ny,nz , xdb, ydb, zdb,
                         BB->use_es , esar ,
                         qtop-pp  , xq , yq , zq ,
                                    xr , yr , zr  ) ;

     /* Compute the answer: B(1.5*x - 0.5*B(B(A(x))))
                          = 1.5*x - 0.5*B(B(A(x))) + b(1.5*x - 0.5*B(B(A(x)))) */

     if( sstepfac <= 0.0f ){

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 1111 )
 { int qq , ii,jj,kk ;
#pragma omp for
       for( qq=pp ; qq < qtop ; qq++ ){
         ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
         xdc[qq] = xq[qq-pp] + xr[qq-pp] - ii ; /* must subtract off x [ii,jj,kk] */
         ydc[qq] = yq[qq-pp] + yr[qq-pp] - jj ; /* to make result be displacments */
         zdc[qq] = zq[qq-pp] + zr[qq-pp] - kk ;
       }
 }
 AFNI_OMP_END ;

     } else {

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 1111 )
 { int qq , ii,jj,kk ;
   register float sf , sf1 ;
   sf = sstepfac ; sf1 = 1.0f - sf ;
#pragma omp for
       for( qq=pp ; qq < qtop ; qq++ ){
         ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
         xdc[qq] = sf * (xq[qq-pp] + xr[qq-pp] - ii) + sf1 * xdb[qq] ;
         ydc[qq] = sf * (yq[qq-pp] + yr[qq-pp] - jj) + sf1 * ydb[qq] ;
         zdc[qq] = sf * (zq[qq-pp] + zr[qq-pp] - kk) + sf1 * zdb[qq] ;
       }
 }
 AFNI_OMP_END ;

     }

   } /* end of loop over segments of length NPER (or less) */

   free(zr); free(yr); free(xr); free(zq); free(yq); free(xq);
   IW3D_load_external_slopes(CC) ; RETURN(CC);
}
#endif

/*---------------------------------------------------------------------------*/
/* Find the inverse square root of warp AA(x):
      the warp BB(x) such that AA(BB(BB(x))) = identity.
   If you want the square root of AA(x), then either invert AA
   before calling this function, or invert the result afterwards. */

IndexWarp3D * IW3D_sqrtinv( IndexWarp3D *AA , IndexWarp3D *BBinit , int icode )
{
   IndexWarp3D *BB , *CC ;
   float normAA , normBC , nrat , orat ;
   int ii , nstep=0 , nss , jcode=MRI_LINEAR ;

ENTRY("IW3D_sqrtinv") ;

   if( AA == NULL ) RETURN(NULL) ;

   normAA = IW3D_normLinf( AA , NULL ) ;
   if( normAA == 0.0f ){
     BB = IW3D_empty_copy(AA) ; RETURN(BB) ;
   }
   if( icode == MRI_WSINC5 ) icode = MRI_QUINTIC ;

   /* BB = initial guess at inverse square root */

   if( verb_nww ) ININFO_message(" -- sqrtinv max|AA|=%f",normAA) ;

   if( BBinit == NULL ){
     int pp = 2 + (int)ceil(log2(normAA)) ; float qq ;
     if( pp < 4 ) pp = 4 ;
     qq = pow(0.5,pp+1.0) ;
     if( verb_nww ) ININFO_message("  - init nstep=%d qq=1/2^%d=%f",pp,pp+1,qq) ;
     BB = IW3D_copy( AA,-qq ) ;
     for( ii=0 ; ii < pp ; ii++ ){
       if( verb_nww > 1 ) ININFO_message("  - init step %d",ii+1) ;
       CC = IW3D_compose(BB,BB,jcode) ; IW3D_destroy(BB) ; BB = CC ;
     }
   } else {
     BB = IW3D_copy( BBinit , 1.0f ) ;
   }

   normAA   = IW3D_normL1( AA , NULL ) ;
   sstepfac = 1.0f / (1.0f+sqrtf(normAA)) ;  /* Newton damping factor */

   nrat = AFNI_numenv("AFNI_NWARP_SSTEPFAC_MIN") ;
   if( nrat > 0.0f && nrat < 1.0f ) sstepfac_MIN = nrat ;

   nrat = AFNI_numenv("AFNI_NWARP_SSTEPFAC_MAX") ;
   if( nrat > 0.0f && nrat < 1.0f ) sstepfac_MAX = nrat ;

   if( sstepfac > sstepfac_MIN ) sstepfac = sstepfac_MIN ;

   if( verb_nww )
     ININFO_message("  - start iterations: normAA=%f sstepfac=%f",normAA,sstepfac) ;

   /* iterate some, until convergence or exhaustion */

   nrat = 666.666f ;

   for( nss=ii=0 ; ii < 39 ; ii++ ){

     /* take a step */

     CC = BB ;
#if 1
     BB = IW3D_sqrtinv_step(AA,CC,jcode) ;
#else
     BB = IW3D_sqrtinv_stepQ(AA,CC,jcode) ;  /* the old (bad) way */
#endif

     /* how close are they now? */

     normBC = IW3D_normL1( BB , CC ) ; IW3D_destroy(CC) ;

     orat = nrat ; nrat = normBC / normAA ;

     if( verb_nww ) ININFO_message("  - iterate %d nrat=%f",++nstep,nrat) ;

     /* check for convergence of B and C */

     if( jcode != icode && nrat < 0.002f ){
       jcode = icode ; nss = 0 ;
       if( verb_nww ) ININFO_message("  - switching from linear interp") ;
       continue ;
     }

     if( nrat < 0.001f ){
       if( verb_nww ) ININFO_message(" -- iteration converged") ;
       RETURN(BB) ;   /* converged */
     }

     if( nss > 0 && nrat < 0.199f && nrat < orat && sstepfac < sstepfac_MAX ){
       nss = 0 ; sstepfac *= 1.123f ; if( sstepfac > sstepfac_MAX ) sstepfac = sstepfac_MAX ;
       if( verb_nww > 1 ) ININFO_message("  - switch to sstepfac=%f",sstepfac) ;
     } else if( nss > 0 && nrat > orat ){
       nss = -66 ; sstepfac *= 0.444f ;
       if( verb_nww > 1 ) ININFO_message("  - switch to sstepfac=%f",sstepfac) ;
     } else {
       nss++ ;
     }

   }

   /* failed to converge, return latest result anyhoo */

   WARNING_message("sqrtinv: iterations failed to converge beautifully") ;
   RETURN(BB) ;
}
/*---------------------------------------------------------------------------*/
#else /* USE_SQRTPAIR is defined!!! */
/*---------------------------------------------------------------------------*/
/* The following iterates on pairs of warps to produce the sqrt and
   sqrtinv at the same time.  It is slower but more stable than the
   previous methods.
*//*-------------------------------------------------------------------------*/

static float spgam = 1.0f ;
static int   spini = 0 ;

float IW3D_sqrtpair_step( IndexWarp3D_pair *YYZZ , int icode )
{
   IndexWarp3D *YY , *ZZ , *Yinv , *Zinv ;
   float *Yixd, *Yiyd, *Yizd , *Zixd , *Ziyd , *Zizd ;
   float *Yfxd, *Yfyd, *Yfzd , *Zfxd , *Zfyd , *Zfzd ;
   int nxyz ; float tsum=0.0f ;

   YY = YYZZ->fwarp ; ZZ = YYZZ->iwarp ; nxyz = YY->nx * YY->ny * YY->nz ;

   if( spini ){
     Yinv = IW3D_invert( YY , ZZ , icode ) ;
     Zinv = IW3D_invert( ZZ , YY , icode ) ;
   } else {
     Yinv = IW3D_invert( YY , NULL , icode ) ;
     Zinv = IW3D_invert( ZZ , NULL , icode ) ;
   }

   Yixd = Yinv->xd ; Yiyd = Yinv->yd ; Yizd = Yinv->zd ;
   Zixd = Zinv->xd ; Ziyd = Zinv->yd ; Zizd = Zinv->zd ;
   Yfxd = YY->xd   ; Yfyd = YY->yd   ; Yfzd = YY->zd   ;
   Zfxd = ZZ->xd   ; Zfyd = ZZ->yd   ; Zfzd = ZZ->zd   ;

 AFNI_OMP_START ;
#pragma omp parallel
 { int qq ; float sf , sf1 , yf,zf,yi,zi , esum=0.0f ;
   sf = 0.5f*spgam ; sf1 = 0.5f/spgam ;

#pragma omp for
   for( qq=0 ; qq < nxyz ; qq++ ){
     yf = Yfxd[qq] ; zf = Zfxd[qq] ; yi = Yixd[qq] ; zi = Zixd[qq] ;
     Yfxd[qq] = sf*yf + sf1*zi ; Zfxd[qq] = sf*zf + sf1*yi ;
     esum += fabsf(Yfxd[qq]-yf) + fabsf(Zfxd[qq]-zf) ;

     yf = Yfyd[qq] ; zf = Zfyd[qq] ; yi = Yiyd[qq] ; zi = Ziyd[qq] ;
     Yfyd[qq] = sf*yf + sf1*zi ; Zfyd[qq] = sf*zf + sf1*yi ;
     esum += fabsf(Yfyd[qq]-yf) + fabsf(Zfyd[qq]-zf) ;

     yf = Yfzd[qq] ; zf = Zfzd[qq] ; yi = Yizd[qq] ; zi = Zizd[qq] ;
     Yfzd[qq] = sf*yf + sf1*zi ; Zfzd[qq] = sf*zf + sf1*yi ;
     esum += fabsf(Yfzd[qq]-yf) + fabsf(Zfzd[qq]-zf) ;
   }
#pragma omp critical
   { tsum += esum ; }
 }
 AFNI_OMP_END ;

   IW3D_destroy(Yinv) ; IW3D_destroy(Zinv) ;
   IW3D_load_external_slopes(YY) ; IW3D_load_external_slopes(ZZ) ;
   return (tsum/nxyz) ;
}

/*---------------------------------------------------------------------------*/
/* Compute the warp pair ( sqrt(AA) , sqrtinv(AA) ) */

IndexWarp3D_pair * IW3D_sqrtpair( IndexWarp3D *AA , int icode )
{
   IndexWarp3D_pair *YYZZ ; IndexWarp3D *YY , *ZZ ;
   float tsum , normAA , nrat,orat ; int nite ;

   /*-- initialize Y = 0.5*A , Z = 0.5*inv(A) --*/

   if( verb_nww ) INFO_message("*** start sqrtpair") ;

   normAA = IW3D_normL2(AA,NULL) ;
   YY = IW3D_copy(AA,0.5f) ;
   ZZ = IW3D_invert(AA,NULL,MRI_LINEAR) ; IW3D_scale(ZZ,0.5f) ;
   YYZZ = malloc(sizeof(IndexWarp3D_pair)) ;
   YYZZ->fwarp = YY ; YYZZ->iwarp = ZZ ;

   spgam = 1.01f ; spini = 0 ; nrat = 666.0f ;

   spini = 1 ; inewtfix = 1 ; inewtfac = 0.666666f ;

   for( nite=0 ; nite < 39 ; nite++ ){

     orat = nrat ;

     tsum = IW3D_sqrtpair_step( YYZZ , MRI_LINEAR ) ;

     nrat = tsum / normAA ;
     if( verb_nww ) ININFO_message("*** sqrtpair: nite=%d  nrat=%g",nite,nrat) ;

     if( nrat < 0.001666f              ) break ;  /* converged? */
     if( nite > 2 && nrat > orat*0.99f ) break ;  /* not converging at all? */
   }

   if( verb_nww ) INFO_message("*** sqrtpair: exit after %d iterations",nite+1) ;

   inewtfix = 0 ;
   return YYZZ ;
}

/*---------------------------------------------------------------------------*/
/* Compute the warp pair ( sqrt(dset) , sqrtinv(dset) ) from a dataset. */

THD_3dim_dataset * THD_nwarp_sqrt( THD_3dim_dataset *dset_nwarp , int invert )
{
   THD_3dim_dataset *qset ;
   IndexWarp3D *AA , *BB ; IndexWarp3D_pair *ABpair ; char *prefix=NULL ;

ENTRY("THD_nwarp_sqrt") ;

   if( dset_nwarp == NULL || DSET_NVALS(dset_nwarp) < 3 ) RETURN(NULL) ;
   DSET_load(dset_nwarp) ; if( !DSET_LOADED(dset_nwarp) ) RETURN(NULL) ;

   AA = IW3D_from_dataset( dset_nwarp , 0 , 0 ) ;  DSET_unload(dset_nwarp) ;
   BB = IW3D_extend( AA ,  32, 32, 32, 32, 32, 32 , 0 ) ; IW3D_destroy(AA) ;

   ABpair = IW3D_sqrtpair( BB , MRI_QUINTIC ) ; IW3D_destroy(BB) ;
   if( ABpair == NULL ) RETURN(NULL) ;

   if( invert ){
     AA = IW3D_extend( ABpair->iwarp , -32,-32,-32,-32,-32,-32 , 0 ) ;
     prefix = "SqrtInvWarp" ;
   } else {
     AA = IW3D_extend( ABpair->fwarp , -32,-32,-32,-32,-32,-32 , 0 ) ;
     prefix = "SqrtWarp" ;
   }

   IW3D_pair_destroy(ABpair) ; IW3D_adopt_dataset(AA,dset_nwarp) ;

   qset = IW3D_to_dataset( AA , prefix ) ; IW3D_destroy(AA) ;
   qset->view_type = dset_nwarp->view_type ;
   THD_init_diskptr_names( qset->dblk->diskptr, NULL,NULL,NULL, qset->view_type, False ) ;
   RETURN(qset) ;
}
#endif  /* USE_SQRTPAIR */

#endif /*(C11)*/ /*###########################################################*/

#if 1
/*============================================================================*/
/* (C12) These functions are for taking stuff from 3dAllineate -nwarp output.
   Since that option is obsolete, these functions are probably useless.       */
/*============================================================================*/

#undef AFF_PARAM
#undef AFF_MATRIX

#define AFF_PARAM  1
#define AFF_MATRIX 2

static int affmode = AFF_PARAM ;

/*---------------------------------------------------------------------------*/
/* Create a warp from a set of parameters, matching a template warp.
   (This is for compatibility with the obsolete 3dAllineate -nwarp option.)
*//*-------------------------------------------------------------------------*/

IndexWarp3D * IW3D_from_poly( int npar, float *par, IndexWarp3D *WW )
{
   GA_warpfunc *wfunc ; char *wname ;
   int nall , ii,jj,kk , nx,ny,nz,nxy,nxyz , pp,qq,qtop ;
   IndexWarp3D *AA ;
   float *xda,*yda,*zda , *xq,*yq,*zq , afpar[12] ;

ENTRY("IW3D_from_poly") ;

   if( par == NULL || WW == NULL ) RETURN(NULL) ;  /* should not happen */

   /* cmat takes ijk -> xyz  ;  imat is cmat's inverse */

   mri_genalign_affine_set_befafter( &(WW->cmat) , &(WW->imat) ) ;

   /* choose the warping function, based on number of parameters */

   switch( npar ){
     default: RETURN(NULL) ;
     case  64: wfunc = mri_genalign_cubic   ; wname = "poly3"  ; break ;
     case 172: wfunc = mri_genalign_quintic ; wname = "poly5"  ; break ;
     case 364: wfunc = mri_genalign_heptic  ; wname = "poly7"  ; break ;
     case 664: wfunc = mri_genalign_nonic   ; wname = "poly9"  ; break ;

     case  12:
       if( affmode == AFF_PARAM ){
         wfunc = mri_genalign_affine ; wname = "affine_param" ;
       } else {
         mat44 wmat, qmat, amat ; float qdet ;       /* create index warp  */
         LOAD_MAT44_AR(amat,par) ;                   /* matrix from coord  */
         wmat = MAT44_MUL(amat,WW->cmat) ;           /* warp matrix, and   */
         qmat = MAT44_MUL(WW->imat,wmat) ;           /* substitute for the */
         qdet = MAT44_DET(qmat) ;                    /* input parameters   */
         if( qdet < 0.025f ){
           WARNING_message("Can't create warp from matrix with determinant=%g",qdet) ;
           RETURN(NULL) ;
         }
         UNLOAD_MAT44_AR(qmat,afpar) ; par = afpar ;
         wfunc = mri_genalign_mat44  ; wname = "affine_matrix" ;
       }
     break ;
   }

   /* setup the output warp */

   nx = WW->nx ; ny = WW->ny ; nz = WW->nz ; nxy = nx*ny ; nxyz = nxy*nz ;
   nxyz = nx*ny*nz ; nall = NALL(nxyz) ;

   AA = IW3D_empty_copy( WW ) ;
   xda = AA->xd ; yda = AA->yd ; zda = AA->zd ;

   xq = (float *)malloc(sizeof(float)*nall) ;  /* workspace */
   yq = (float *)malloc(sizeof(float)*nall) ;
   zq = (float *)malloc(sizeof(float)*nall) ;

   /* send parameters to warping function, for setup */

   if( verb_nww > 1 ) ININFO_message("  - warp name = '%s' has %d parameters",wname,npar) ;

   wfunc( npar , par , 0,NULL,NULL,NULL , NULL,NULL,NULL ) ;

   /* do the work, Jake */

   for( pp=0 ; pp < nxyz ; pp+=nall ){  /* loop over segments */

     qtop = MIN( nxyz , pp+nall ) ;  /* process points from pp to qtop-1 */

     /* input coords are indexes */

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 1111 )
 { int qq , ii,jj,kk ;
#pragma omp for
     for( qq=pp ; qq < qtop ; qq++ ){
       ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
       xq[qq-pp] = ii ; yq[qq-pp] = jj ; zq[qq-pp] = kk ;
     }
 }
 AFNI_OMP_END ;

     /* compute index-to-index warp */

     wfunc( npar , NULL , qtop-pp , xq,yq,zq , xda+pp,yda+pp,zda+pp ) ;

     /* subtract off base indexes to make the result just be displacments */

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 1111 )
 { int qq ;
#pragma omp for
     for( qq=pp ; qq < qtop ; qq++ ){
       xda[qq] -= xq[qq-pp] ; yda[qq] -= yq[qq-pp] ; zda[qq] -= zq[qq-pp] ;
     }
 }
 AFNI_OMP_END ;

   } /* end of loop over segments */

   /* time to trot, Bwana */

   free(zq) ; free(yq) ; free(xq) ;
   IW3D_load_external_slopes(AA) ; RETURN(AA) ;
}

/*---------------------------------------------------------------------------*/
/* Create a 'nonlinear' warp from a matrix;
   needs a dataset to serve as spatial template
*//*-------------------------------------------------------------------------*/

IndexWarp3D * IW3D_from_mat44( mat44 mm , THD_3dim_dataset *mset )
{
   IndexWarp3D *AA , *WW ; float mar[12] ;

   if( !ISVALID_DSET(mset)   ) return NULL ;
   if( MAT44_DET(mm) == 0.0f ) return NULL ;

   WW = IW3D_create_vacant( DSET_NX(mset) , DSET_NY(mset) , DSET_NZ(mset) ) ;
   IW3D_adopt_dataset( WW , mset ) ;
   UNLOAD_MAT44( mm ,
                 mar[0] , mar[1] , mar[2] , mar[3] , mar[ 4] , mar[ 5] ,
                 mar[6] , mar[7] , mar[8] , mar[9] , mar[10] , mar[11]  ) ;
   affmode = AFF_MATRIX ;
   AA = IW3D_from_poly( 12 , mar , WW ) ;  /* kind of crude */
   IW3D_destroy( WW ) ;
   IW3D_load_external_slopes(AA) ; return AA ;
}

#endif /*(C12)*/ /*###########################################################*/

#if 1
/*============================================================================*/
/* (C13) Various functions for interpolating images at arbitrary indexes.     */
/*============================================================================*/

/*--------------------------------------------------------------------------*/
/* interpolate from a float image to a set of indexes (ip,jp,kp);
   this is just a wrapper for functions from mri_genalign_util.c  */

void THD_interp_floatim( MRI_IMAGE *fim ,
                         int np , float *ip , float *jp , float *kp ,
                         int code, float *outar )
{
ENTRY("THD_interp_floatim") ;

   switch( code ){
     case MRI_NN:      GA_interp_NN     ( fim, np,ip,jp,kp, outar ) ; break ;
     case MRI_LINEAR:  GA_interp_linear ( fim, np,ip,jp,kp, outar ) ; break ;
     case MRI_CUBIC:   GA_interp_cubic  ( fim, np,ip,jp,kp, outar ) ; break ;
     default:
     case MRI_QUINTIC: GA_interp_quintic( fim, np,ip,jp,kp, outar ) ; break ;
     case MRI_WSINC5:  GA_interp_wsinc5 ( fim, np,ip,jp,kp, outar ) ; break ;
   }

   /* clipping */

   if( MRI_HIGHORDER(code) ){
     int ii,nn=fim->nvox ; float bot,top , *far=MRI_FLOAT_PTR(fim) ;
     bot = top = far[0] ;
     for( ii=1 ; ii < nn ; ii++ ) if( bot > far[ii] ) bot = far[ii] ;
                             else if( top < far[ii] ) top = far[ii] ;
     for( ii=0 ; ii < np ; ii++ ) if( outar[ii] < bot ) outar[ii] = bot ;
                             else if( outar[ii] > top ) outar[ii] = top ;
   }

   EXRETURN ;
}

/*--------------------------------------------------------------------------*/
/* interpolate from a complex-valued image to a set of indexes (ip,jp,kp) */

void THD_interp_complexim( MRI_IMAGE *fim ,
                           int np , float *ip , float *jp , float *kp ,
                           int code, complex *outar )
{
   MRI_IMARR *rpair ;
   MRI_IMAGE *rim , *iim , *aim , *bim ;
   float     *rar , *iar ;
   complex   *aar ;

ENTRY("THD_interp_complexim") ;

   /* split input into float pair */

   rpair = mri_complex_to_pair( fim ) ;
   if( rpair == NULL ) EXRETURN ;
   aim = IMARR_SUBIM(rpair,0) ;
   bim = IMARR_SUBIM(rpair,1) ;

   /* make float images for outputs */

   rim = mri_new_conforming( fim , MRI_float ) ; rar = MRI_FLOAT_PTR(rim) ;
   iim = mri_new_conforming( fim , MRI_float ) ; iar = MRI_FLOAT_PTR(iim) ;

   /* interpolate into these new images */

   switch( code ){
     case MRI_NN:      GA_interp_NN     ( aim, np,ip,jp,kp, rar ) ;
                       GA_interp_NN     ( bim, np,ip,jp,kp, iar ) ; break ;
     case MRI_LINEAR:  GA_interp_linear ( aim, np,ip,jp,kp, rar ) ;
                       GA_interp_linear ( bim, np,ip,jp,kp, iar ) ; break ;
     case MRI_CUBIC:   GA_interp_cubic  ( aim, np,ip,jp,kp, rar ) ;
                       GA_interp_cubic  ( bim, np,ip,jp,kp, iar ) ; break ;
     default:
     case MRI_QUINTIC: GA_interp_quintic( aim, np,ip,jp,kp, rar ) ;
                       GA_interp_quintic( bim, np,ip,jp,kp, iar ) ; break ;
     case MRI_WSINC5:  GA_interp_wsinc5 ( aim, np,ip,jp,kp, rar ) ;
                       GA_interp_wsinc5 ( bim, np,ip,jp,kp, iar ) ; break ;
   }

   /* toss the input pair */

   DESTROY_IMARR(rpair) ;

   /* convert output pair to single complex image */

   aim = mri_pair_to_complex( rim , iim ) ; mri_free(rim) ; mri_free(iim) ;

   /* copy that data to the user-provided output array */

   aar = MRI_COMPLEX_PTR(aim) ;
   memcpy( outar , aar , sizeof(complex)*np ) ;
   mri_free(aim) ;

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/
/* Apply a warp to a source image 'sim', and stick values into output 'fim'.
   Apply to a sub-volume ibot..itop, jbot..jtop, kbot..ktop (inclusive).
*//*--------------------------------------------------------------------------*/

void IW3D_warp_into_floatim( IndexWarp3D *AA, MRI_IMAGE *sim, MRI_IMAGE *fim,
                             int ibot, int itop ,
                             int jbot, int jtop ,
                             int kbot, int ktop , int code , float fac )
{
   int nx,ny,nz,nxy , nii,njj,nkk , np , ii,jj,kk,ijk , pp ;
   float *ip,*jp,*kp ;
   float *far , *xd,*yd,*zd ;

ENTRY("IW3D_warp_into_floatim") ;

   if( AA  == NULL                           ) EXRETURN ;
   if( sim == NULL || sim->kind != MRI_float ) EXRETURN ;
   if( fim == NULL || fim->kind != MRI_float ) EXRETURN ;

   nx = AA->nx ; ny = AA->ny ; nz = AA->nz ; nxy = nx*ny ;
   if( sim->nx != nx || sim->ny != ny || sim->nz != nz ) EXRETURN ;
   if( fim->nx != nx || fim->ny != ny || fim->nz != nz ) EXRETURN ;

   if( ibot < 0 ) ibot = 0 ; if( itop > nx-1 ) itop = nx-1 ;
   if( jbot < 0 ) jbot = 0 ; if( jtop > ny-1 ) itop = ny-1 ;
   if( kbot < 0 ) kbot = 0 ; if( ktop > nz-1 ) itop = nz-1 ;

   nii = itop - ibot + 1 ; if( nii < 1 ) EXRETURN ;
   njj = jtop - jbot + 1 ; if( njj < 1 ) EXRETURN ;
   nkk = ktop - kbot + 1 ; if( nkk < 1 ) EXRETURN ;

   np = nii*njj*nkk ;
   ip = (float *)malloc(sizeof(float)*np) ;
   jp = (float *)malloc(sizeof(float)*np) ;
   kp = (float *)malloc(sizeof(float)*np) ;

   xd = AA->xd ; yd = AA->yd ; zd = AA->zd ;

   for( pp=0,kk=kbot ; kk <= ktop ; kk++ ){
     for( jj=jbot ; jj <= jtop ; jj++ ){
#pragma ivdep
       for( ii=ibot ; ii <= itop ; ii++,pp++ ){
         ijk = ii + jj*nx + kk*nxy ;
         ip[pp] = ii + xd[ijk] * fac ;
         jp[pp] = jj + yd[ijk] * fac ;
         kp[pp] = kk + zd[ijk] * fac ;
       }
     }
   }

   far = MRI_FLOAT_PTR(fim) ;

   /*-- All of them, Frank? --*/

   if( nii == nx && njj == ny && nkk == nz ){

     THD_interp_floatim( sim , np,ip,jp,kp , code , far ) ;

   } else {  /*-- just some of them, Mother Goose? --*/

     float *val = (float *)malloc(sizeof(float)*np) ;

     THD_interp_floatim( sim , np,ip,jp,kp , code , val ) ;

     for( pp=0,kk=kbot ; kk <= ktop ; kk++ )
       for( jj=jbot ; jj <= jtop ; jj++ )
         for( ii=ibot ; ii <= itop ; ii++,pp++ ) far[ii+jj*nx+kk*nxy] = val[pp];

     free(val) ;
   }

   free(kp) ; free(jp) ; free(ip) ;
   EXRETURN ;
}

/*----------------------------------------------------------------------------*/
/* Warp a source image 'sim' to create an output image */

MRI_IMAGE * IW3D_warp_floatim( IndexWarp3D *AA, MRI_IMAGE *sim, int code, float fac )
{
   MRI_IMAGE *fim ;

ENTRY("IW3D_warp_floatim") ;

   if( AA == NULL || sim == NULL ) RETURN(NULL) ;

   fim = mri_new_conforming( sim , MRI_float ) ;

   IW3D_warp_into_floatim( AA , sim , fim ,
                           0,sim->nx-1 , 0,sim->ny-1 , 0,sim->nz-1 , code , fac ) ;

   if( MRI_HIGHORDER(code) ){  /* clipping */
     double_pair smm = mri_minmax(sim) ;
     float sb=(float)smm.a , st=(float)smm.b ; int qq ;
     float *far=MRI_FLOAT_PTR(fim) ;
     for( qq=0 ; qq < fim->nvox ; qq++ ){
       if( far[qq] < sb ) far[qq] = sb ; else if( far[qq] > st ) far[qq] = st ;
     }
   }

   RETURN(fim) ;
}

/*----------------------------------------------------------------------------*/
/* Source into output (sim to fim) with a matrix instead of an index warp */

void IW3D_mat44_into_floatim( mat44 imat , MRI_IMAGE *sim, MRI_IMAGE *fim,
                              int ibot, int itop ,
                              int jbot, int jtop ,
                              int kbot, int ktop , int code )
{
   int nx,ny,nz,nxy , nii,njj,nkk , np , ii,jj,kk,ijk , pp ;
   float *ip,*jp,*kp ;
   float *far , *xd,*yd,*zd ;

ENTRY("IW3D_mat44_into_floatim") ;

   if( sim == NULL || sim->kind != MRI_float ) EXRETURN ;
   if( fim == NULL || fim->kind != MRI_float ) EXRETURN ;

   nx = fim->nx ; ny = fim->ny ; nz = fim->nz ; nxy = nx*ny ;

   if( ibot < 0 ) ibot = 0 ; if( itop > nx-1 ) itop = nx-1 ;
   if( jbot < 0 ) jbot = 0 ; if( jtop > ny-1 ) itop = ny-1 ;
   if( kbot < 0 ) kbot = 0 ; if( ktop > nz-1 ) itop = nz-1 ;

   nii = itop - ibot + 1 ; if( nii < 1 ) EXRETURN ;
   njj = jtop - jbot + 1 ; if( njj < 1 ) EXRETURN ;
   nkk = ktop - kbot + 1 ; if( nkk < 1 ) EXRETURN ;

   np = nii*njj*nkk ;
   ip = (float *)malloc(sizeof(float)*np) ;
   jp = (float *)malloc(sizeof(float)*np) ;
   kp = (float *)malloc(sizeof(float)*np) ;

   for( pp=0,kk=kbot ; kk <= ktop ; kk++ ){
     for( jj=jbot ; jj <= jtop ; jj++ ){
#pragma ivdep
       for( ii=ibot ; ii <= itop ; ii++,pp++ ){
         MAT44_VEC( imat , ii,jj,kk , ip[pp],jp[pp],kp[pp] ) ;
       }
     }
   }

   far = MRI_FLOAT_PTR(fim) ;

   /*-- All of them, Frank? --*/

   if( nii == nx && njj == ny && nkk == nz ){

     THD_interp_floatim( sim , np,ip,jp,kp , code , far ) ;

   } else {  /*-- just some of them, Mother Goose? --*/

     float *val = (float *)malloc(sizeof(float)*np) ;

     THD_interp_floatim( sim , np,ip,jp,kp , code , val ) ;

     for( pp=0,kk=kbot ; kk <= ktop ; kk++ )
       for( jj=jbot ; jj <= jtop ; jj++ )
         for( ii=ibot ; ii <= itop ; ii++,pp++ ) far[ii+jj*nx+kk*nxy] = val[pp];

     free(val) ;
   }

   free(kp) ; free(jp) ; free(ip) ;
   EXRETURN ;
}

#if 0  /* maybe useful someday? */
/*----------------------------------------------------------------------------*/
/* interpolate from 1 image to another, preserving type */

void THD_interp( MRI_IMAGE *inim ,
                 int np , float *ip , float *jp , float *kp ,
                 int code, void *outar )
{
   MRI_IMAGE *fim=inim ; float *far ; register int ii ;

ENTRY("THD_interp") ;

   switch( fim->kind ){

     default:
       ERROR_message("Illegal input type %d in THD_interp()",(int)fim->kind) ;
     break ;

     /*--------------------*/

     case MRI_float:
       THD_interp_floatim( inim , np,ip,jp,kp , code,(float *)outar ) ;
     break ;

     /*--------------------*/

     case MRI_fvect:{
       int kk , vd=inim->vdim ; float *oar=(float *)outar ;
       far = (float *)malloc(sizeof(float)*np) ;
       for( kk=0 ; kk < vd ; kk++ ){
         fim = mri_fvect_subimage(inim,kk) ;
         THD_interp_floatim( inim , np,ip,jp,kp , code,far ) ;
         for( ii=0 ; ii < np ; ii++ ) oar[ii*vd+kk] = far[ii] ;
         mri_free(fim) ;
       }
       free(far) ;
     }
     break ;

     /*--------------------*/

     case MRI_short:{
       short *sar=(short *)outar ;
       fim = mri_to_float(inim) ; far = (float *)malloc(sizeof(float)*np) ;
       THD_interp_floatim( inim , np,ip,jp,kp , code,far ) ;
       for( ii=0 ; ii < np ;  ii++ ) sar[ii] = SHORTIZE(far[ii]) ;
       free(far) ; mri_free(fim) ;
     }
     break ;

     /*--------------------*/

     case MRI_byte:{
       byte *bar=(byte *)outar ;
       fim = mri_to_float(inim) ; far = (float *)malloc(sizeof(float)*np) ;
       THD_interp_floatim( inim , np,ip,jp,kp , code,far ) ;
       for( ii=0 ; ii < np ;  ii++ ) bar[ii] = BYTEIZE(far[ii]) ;
       free(far) ; mri_free(fim) ;
     }
     break ;

     /*--------------------*/

     case MRI_complex:{
       complex *car=(complex *)outar ; MRI_IMARR *imar ; float *gar ;
       far = (float *)malloc(sizeof(float)*np) ;
       gar = (float *)malloc(sizeof(float)*np) ;
       imar = mri_complex_to_pair(inim) ;
       THD_interp_floatim( IMARR_SUBIM(imar,0) , np,ip,jp,kp , code,far ) ;
       THD_interp_floatim( IMARR_SUBIM(imar,1) , np,ip,jp,kp , code,gar ) ;
       for( ii=0 ; ii < np ; ii++ ){ car[ii].r = far[ii]; car[ii].i = gar[ii]; }
       DESTROY_IMARR(imar) ; free(gar) ; free(far) ;
     }
     break ;

     /*--------------------*/

     case MRI_rgb:{
       MRI_IMARR *imar ; float *gar , *har ; byte *bar=(byte *)outar ;
       far = (float *)malloc(sizeof(float)*np) ;
       gar = (float *)malloc(sizeof(float)*np) ;
       har = (float *)malloc(sizeof(float)*np) ;
       imar = mri_rgb_to_3float(inim) ;
       THD_interp_floatim( IMARR_SUBIM(imar,0) , np,ip,jp,kp , code,far ) ;
       THD_interp_floatim( IMARR_SUBIM(imar,1) , np,ip,jp,kp , code,gar ) ;
       THD_interp_floatim( IMARR_SUBIM(imar,2) , np,ip,jp,kp , code,har ) ;
       for( ii=0 ; ii < np ; ii++ ){
         bar[3*ii  ] = BYTEIZE(far[ii]) ;
         bar[3*ii+1] = BYTEIZE(gar[ii]) ;
         bar[3*ii+2] = BYTEIZE(har[ii]) ;
       }
       DESTROY_IMARR(imar) ; free(har) ; free(gar) ; free(far) ;
     }
     break ;

   }

   EXRETURN ;
}
#endif

#endif /*(C13)*/ /*###########################################################*/

#if 1
/*============================================================================*/
/* (C14) Functions used in 3dNwarpXYZ to warp a limited number of points given
   by x,y,z triples, rather than warping a whole grid at once.                */
/*============================================================================*/

/** Interpolation in 3 displacement fields at once **/

#if 0  /*----------------------------- via linear interp ---------------------*/
int THD_nwarp_im_xyz( MRI_IMAGE *xdim , MRI_IMAGE *ydim , MRI_IMAGE *zdim ,
                      float dfac , int npt ,
                      float *xin , float *yin , float *zin ,
                      float *xut , float *yut , float *zut ,
                      mat44 imat , floatvec *esv )
{
   ES_DECLARE_FLOATS ;
   int nx,ny,nz , nx1,ny1,nz1 , nxy , nx2,ny2,nz2 ;
   float *xdar , *ydar , *zdar ;

ENTRY("THD_nwarp_im_xyz") ;

   if( esv != NULL ) ES_UNPACKVEC(esv->ar) ;

   nx = xdim->nx ; ny = xdim->ny ; nz = zdim->nz ;
   nx1 = nx-1 ; ny1 = ny-1 ; nz1 = nz-1 ; nxy = nx*ny ;
   nx2 = nx-2 ; ny2 = ny-2 ; nz2 = nz-2 ;

   xdar = MRI_FLOAT_PTR(xdim) ;  /* displacement arrays */
   ydar = MRI_FLOAT_PTR(ydim) ;
   zdar = MRI_FLOAT_PTR(zdim) ;

   /* parallel-ized linear interpolation (and extrapolation) */

   AFNI_OMP_START ;
#pragma omp parallel if( npt > 333 )
   { int pp ;
     float xx,yy,zz , fx,fy,fz ; int ix,jy,kz , aem ;
     float eex=0.0f,eey=0.0f,eez=0.0f , Exx=0.0f,Exy=0.0f,Exz=0.0f ,
           Eyx=0.0f,Eyy=0.0f,Eyz=0.0f , Ezx=0.0f,Ezy=0.0f,Ezz=0.0f , uex,vex,wex ;
     int ix_00,ix_p1 , jy_00,jy_p1 , kz_00,kz_p1 ;
     float wt_00,wt_p1 ;
     float f_j00_k00, f_jp1_k00, f_j00_kp1, f_jp1_kp1, f_k00, f_kp1 ;
     float g_j00_k00, g_jp1_k00, g_j00_kp1, g_jp1_kp1, g_k00, g_kp1 ;
     float h_j00_k00, h_jp1_k00, h_j00_kp1, h_jp1_kp1, h_k00, h_kp1 ;

     /* loop over input points, transforming them to indexes, interpolating
        to that location, then adding the displacments to get the outputs  */

#pragma omp for
     for( pp=0 ; pp < npt ; pp++ ){
       MAT44_VEC( imat , xin[pp],yin[pp],zin[pp] , xx,yy,zz ) ;  /* convert to index coords */
       /* find location in or out of dataset grid, and deal with external slopes if need be */
       aem=0 ;  /* flag for usage of external slopes */
            if( xx < 0.0f ){ eex = xx    ; ix = 0      ; fx = 0.0f; aem++; Exx=es_xd_xm; Eyx=es_yd_xm; Ezx=es_zd_xm; }
       else if( xx < nx1  ){ eex = 0.0f  ; ix = (int)xx; fx = xx-ix;       }
       else                { eex = xx-nx1; ix = nx2    ; fx = 1.0f; aem++; Exx=es_xd_xp; Eyx=es_yd_xp; Ezx=es_zd_xp; }
            if( yy < 0.0f ){ eey = yy    ; jy = 0      ; fy = 0.0f; aem++; Exy=es_xd_ym; Eyy=es_yd_ym; Ezy=es_zd_ym; }
       else if( yy < ny1  ){ eey = 0.0f  ; jy = (int)yy; fy = yy-jy;       }
       else                { eey = yy-ny1; jy = ny2    ; fy = 1.0f; aem++; Exy=es_xd_yp; Eyy=es_yd_yp; Ezy=es_zd_yp; }
            if( zz < 0.0f ){ eez = zz    ; kz = 0      ; fz = 0.0f; aem++; Exz=es_xd_zm; Eyz=es_yd_zm; Ezz=es_zd_zm; }
       else if( zz < nz1  ){ eez = 0.0f  ; kz = (int)zz; fz = zz-kz;       }
       else                { eez = zz-nz1; kz = nz2    ; fz = 1.0f; aem++; Exz=es_xd_zp; Eyz=es_yd_zp; Ezz=es_zd_zp; }
       if( aem ){
         uex = Exx*eex + Exy*eey + Exz*eez ;  /* eex = how far past edge of warp, in x-direction (etc.) */
         vex = Eyx*eex + Eyy*eey + Eyz*eez ;  /* Eab = external slope for 'a' displacement, */
         wex = Ezx*eex + Ezy*eey + Ezz*eez ;  /*       along the 'b' direction, for a and b = x or y or z */
         uex *= dfac ; vex *= dfac ; wex *= dfac ;
       } else {
         uex = vex = wex = 0.0f ;
       }
       /* now linearly interpolate displacements inside the dataset grid */
       ix_00 = ix ; ix_p1 = ix_00+1 ;  /* at this point, we are 'fx' between indexes ix_00 and ix_p1 */
       jy_00 = jy ; jy_p1 = jy_00+1 ;  /* et cetera */
       kz_00 = kz ; kz_p1 = kz_00+1 ;
       wt_00 = (1.0f-fx)*dfac ; wt_p1 = fx*dfac ;  /* weights for ix_00 and ix_p1 points for linear interp */

#undef  IJK  /* 1D index from 3D index */
#define IJK(i,j,k) ((i)+(j)*nx+(k)*nxy)

#undef  XINT  /* linear interpolation macro in array 'aaa' at plane jk */
#define XINT(aaa,j,k) wt_00*aaa[IJK(ix_00,j,k)]+wt_p1*aaa[IJK(ix_p1,j,k)]

       /* interpolate to location ix+fx at each jy,kz level */

       f_j00_k00 = XINT(xdar,jy_00,kz_00) ; f_jp1_k00 = XINT(xdar,jy_p1,kz_00) ;
       f_j00_kp1 = XINT(xdar,jy_00,kz_p1) ; f_jp1_kp1 = XINT(xdar,jy_p1,kz_p1) ;
       g_j00_k00 = XINT(ydar,jy_00,kz_00) ; g_jp1_k00 = XINT(ydar,jy_p1,kz_00) ;
       g_j00_kp1 = XINT(ydar,jy_00,kz_p1) ; g_jp1_kp1 = XINT(ydar,jy_p1,kz_p1) ;
       h_j00_k00 = XINT(zdar,jy_00,kz_00) ; h_jp1_k00 = XINT(zdar,jy_p1,kz_00) ;
       h_j00_kp1 = XINT(zdar,jy_00,kz_p1) ; h_jp1_kp1 = XINT(zdar,jy_p1,kz_p1) ;

       /* interpolate to jy+fy at each kz level */

       wt_00 = 1.0f-fy ; wt_p1 = fy ;
       f_k00 =  wt_00 * f_j00_k00 + wt_p1 * f_jp1_k00 ;
       f_kp1 =  wt_00 * f_j00_kp1 + wt_p1 * f_jp1_kp1 ;
       g_k00 =  wt_00 * g_j00_k00 + wt_p1 * g_jp1_k00 ;
       g_kp1 =  wt_00 * g_j00_kp1 + wt_p1 * g_jp1_kp1 ;
       h_k00 =  wt_00 * h_j00_k00 + wt_p1 * h_jp1_k00 ;
       h_kp1 =  wt_00 * h_j00_kp1 + wt_p1 * h_jp1_kp1 ;

       /* interpolate to kz+fz to get output, plus add in original coords */

       xut[pp] = (1.0f-fz) * f_k00 + fz * f_kp1 + uex + xin[pp] ;  /* note add-in of values */
       yut[pp] = (1.0f-fz) * g_k00 + fz * g_kp1 + vex + yin[pp] ;  /* from external slope */
       zut[pp] = (1.0f-fz) * h_k00 + fz * h_kp1 + wex + zin[pp] ;  /* calculation above */

     } /* end of loop over input/output points */
   } /* end of parallel code */
   AFNI_OMP_END ;

   RETURN(npt) ;
}
#else  /*------------------------ via quintic interp -------------------------*/
int THD_nwarp_im_xyz( MRI_IMAGE *xdim , MRI_IMAGE *ydim , MRI_IMAGE *zdim ,
                      float dfac , int npt ,
                      float *xin , float *yin , float *zin ,
                      float *xut , float *yut , float *zut ,
                      mat44 imat , floatvec *esv )
{
   ES_DECLARE_FLOATS ;
   int nx,ny,nz , nx1,ny1,nz1 , nxy , nx2,ny2,nz2 ;
   float *xdar , *ydar , *zdar ;

ENTRY("THD_nwarp_im_xyz") ;

   if( esv != NULL ) ES_UNPACKVEC(esv->ar) ;

   nx = xdim->nx ; ny = xdim->ny ; nz = zdim->nz ;
   nx1 = nx-1 ; ny1 = ny-1 ; nz1 = nz-1 ; nxy = nx*ny ;
   nx2 = nx-2 ; ny2 = ny-2 ; nz2 = nz-2 ;

   xdar = MRI_FLOAT_PTR(xdim) ;  /* displacement arrays */
   ydar = MRI_FLOAT_PTR(ydim) ;
   zdar = MRI_FLOAT_PTR(zdim) ;

   /* quintic interpolation */

 AFNI_OMP_START ;
#pragma omp parallel if( npt > 333 )
 { int pp ;
   float xx,yy,zz , fx,fy,fz ; int ix,jy,kz , aem ;
   float eex=0.0f,eey=0.0f,eez=0.0f , Exx=0.0f,Exy=0.0f,Exz=0.0f ,
         Eyx=0.0f,Eyy=0.0f,Eyz=0.0f , Ezx=0.0f,Ezy=0.0f,Ezz=0.0f , uex,vex,wex ;
   int nx1=nx-1,ny1=ny-1,nz1=nz-1 ;
   int nx2=nx-2,ny2=ny-2,nz2=nz-2 ;
   int ix_m2,ix_m1,ix_00,ix_p1,ix_p2,ix_p3 ; /* interpolation indices */
   int jy_m2,jy_m1,jy_00,jy_p1,jy_p2,jy_p3 ; /* (input image) */
   int kz_m2,kz_m1,kz_00,kz_p1,kz_p2,kz_p3 ;

   float wt_m2,wt_m1,wt_00,wt_p1,wt_p2,wt_p3 ; /* interpolation weights */

   float f_jm2_km2, f_jm1_km2, f_j00_km2, f_jp1_km2, f_jp2_km2, f_jp3_km2,
         f_jm2_km1, f_jm1_km1, f_j00_km1, f_jp1_km1, f_jp2_km1, f_jp3_km1,
         f_jm2_k00, f_jm1_k00, f_j00_k00, f_jp1_k00, f_jp2_k00, f_jp3_k00,
         f_jm2_kp1, f_jm1_kp1, f_j00_kp1, f_jp1_kp1, f_jp2_kp1, f_jp3_kp1,
         f_jm2_kp2, f_jm1_kp2, f_j00_kp2, f_jp1_kp2, f_jp2_kp2, f_jp3_kp2,
         f_jm2_kp3, f_jm1_kp3, f_j00_kp3, f_jp1_kp3, f_jp2_kp3, f_jp3_kp3,
         f_km2    , f_km1    , f_k00    , f_kp1    , f_kp2    , f_kp3     ;
   float g_jm2_km2, g_jm1_km2, g_j00_km2, g_jp1_km2, g_jp2_km2, g_jp3_km2,
         g_jm2_km1, g_jm1_km1, g_j00_km1, g_jp1_km1, g_jp2_km1, g_jp3_km1,
         g_jm2_k00, g_jm1_k00, g_j00_k00, g_jp1_k00, g_jp2_k00, g_jp3_k00,
         g_jm2_kp1, g_jm1_kp1, g_j00_kp1, g_jp1_kp1, g_jp2_kp1, g_jp3_kp1,
         g_jm2_kp2, g_jm1_kp2, g_j00_kp2, g_jp1_kp2, g_jp2_kp2, g_jp3_kp2,
         g_jm2_kp3, g_jm1_kp3, g_j00_kp3, g_jp1_kp3, g_jp2_kp3, g_jp3_kp3,
         g_km2    , g_km1    , g_k00    , g_kp1    , g_kp2    , g_kp3     ;
   float h_jm2_km2, h_jm1_km2, h_j00_km2, h_jp1_km2, h_jp2_km2, h_jp3_km2,
         h_jm2_km1, h_jm1_km1, h_j00_km1, h_jp1_km1, h_jp2_km1, h_jp3_km1,
         h_jm2_k00, h_jm1_k00, h_j00_k00, h_jp1_k00, h_jp2_k00, h_jp3_k00,
         h_jm2_kp1, h_jm1_kp1, h_j00_kp1, h_jp1_kp1, h_jp2_kp1, h_jp3_kp1,
         h_jm2_kp2, h_jm1_kp2, h_j00_kp2, h_jp1_kp2, h_jp2_kp2, h_jp3_kp2,
         h_jm2_kp3, h_jm1_kp3, h_j00_kp3, h_jp1_kp3, h_jp2_kp3, h_jp3_kp3,
         h_km2    , h_km1    , h_k00    , h_kp1    , h_kp2    , h_kp3     ;

#pragma omp for
   for( pp=0 ; pp < npt ; pp++ ){
     MAT44_VEC( imat , xin[pp],yin[pp],zin[pp] , xx,yy,zz ) ;  /* convert to index coords */
     /* find location in or out of dataset grid, and deal with external slopes if need be */
     aem=0 ;  /* flag for usage of external slopes */
          if( xx < 0.0f ){ eex = xx    ; ix = 0      ; fx = 0.0f; aem++; Exx=es_xd_xm; Eyx=es_yd_xm; Ezx=es_zd_xm; }
     else if( xx < nx1  ){ eex = 0.0f  ; ix = (int)xx; fx = xx-ix;       }
     else                { eex = xx-nx1; ix = nx2    ; fx = 1.0f; aem++; Exx=es_xd_xp; Eyx=es_yd_xp; Ezx=es_zd_xp; }
          if( yy < 0.0f ){ eey = yy    ; jy = 0      ; fy = 0.0f; aem++; Exy=es_xd_ym; Eyy=es_yd_ym; Ezy=es_zd_ym; }
     else if( yy < ny1  ){ eey = 0.0f  ; jy = (int)yy; fy = yy-jy;       }
     else                { eey = yy-ny1; jy = ny2    ; fy = 1.0f; aem++; Exy=es_xd_yp; Eyy=es_yd_yp; Ezy=es_zd_yp; }
          if( zz < 0.0f ){ eez = zz    ; kz = 0      ; fz = 0.0f; aem++; Exz=es_xd_zm; Eyz=es_yd_zm; Ezz=es_zd_zm; }
     else if( zz < nz1  ){ eez = 0.0f  ; kz = (int)zz; fz = zz-kz;       }
     else                { eez = zz-nz1; kz = nz2    ; fz = 1.0f; aem++; Exz=es_xd_zp; Eyz=es_yd_zp; Ezz=es_zd_zp; }
     if( aem ){
       uex = Exx*eex + Exy*eey + Exz*eez ;  /* eex = how far past edge of warp, in x-direction (etc.) */
       vex = Eyx*eex + Eyy*eey + Eyz*eez ;  /* Eab = external slope for 'a' displacement, */
       wex = Ezx*eex + Ezy*eey + Ezz*eez ;  /*       along the 'b' direction, for a and b = x or y or z */
       uex *= dfac ; vex *= dfac ; wex *= dfac ;
     } else {
       uex = vex = wex = 0.0f ;
     }

     /* special case of no interpolation needed */

     if( fabsf(fx)+fabsf(fy)+fabsf(fz) < 0.00222f ){    /* 05 Nov 2014 */
       xut[pp] = xdar[IJK(ix,jy,kz)] + uex + xin[pp] ;
       yut[pp] = ydar[IJK(ix,jy,kz)] + vex + yin[pp] ;
       zut[pp] = zdar[IJK(ix,jy,kz)] + wex + zin[pp] ;
       continue ;
     }

     /* compute indexes from which to interpolate (-2,-1,0,+1,+2,+3),
        but clipped to lie inside input image volume                 */

     ix_m1 = ix-1    ; ix_00 = ix      ; ix_p1 = ix+1    ; ix_p2 = ix+2    ;
     CLIP(ix_m1,nx1) ; CLIP(ix_00,nx1) ; CLIP(ix_p1,nx1) ; CLIP(ix_p2,nx1) ;
     ix_m2 = ix-2    ; ix_p3 = ix+3 ;
     CLIP(ix_m2,nx1) ; CLIP(ix_p3,nx1) ;

     jy_m1 = jy-1    ; jy_00 = jy      ; jy_p1 = jy+1    ; jy_p2 = jy+2    ;
     CLIP(jy_m1,ny1) ; CLIP(jy_00,ny1) ; CLIP(jy_p1,ny1) ; CLIP(jy_p2,ny1) ;
     jy_m2 = jy-2    ; jy_p3 = jy+3 ;
     CLIP(jy_m2,ny1) ; CLIP(jy_p3,ny1) ;

     kz_m1 = kz-1    ; kz_00 = kz      ; kz_p1 = kz+1    ; kz_p2 = kz+2    ;
     CLIP(kz_m1,nz1) ; CLIP(kz_00,nz1) ; CLIP(kz_p1,nz1) ; CLIP(kz_p2,nz1) ;
     kz_m2 = kz-2    ; kz_p3 = kz+3 ;
     CLIP(kz_m2,nz1) ; CLIP(kz_p3,nz1) ;

     wt_m1 = Q_M1(fx)*dfac ; wt_00 = Q_00(fx)*dfac ;  /* interpolation weights */
     wt_p1 = Q_P1(fx)*dfac ; wt_p2 = Q_P2(fx)*dfac ;  /* in x-direction        */
     wt_m2 = Q_M2(fx)*dfac ; wt_p3 = Q_P3(fx)*dfac ;

#undef  XINT
#define XINT(aaa,j,k) wt_m2*aaa[IJK(ix_m2,j,k)]+wt_m1*aaa[IJK(ix_m1,j,k)] \
                     +wt_00*aaa[IJK(ix_00,j,k)]+wt_p1*aaa[IJK(ix_p1,j,k)] \
                     +wt_p2*aaa[IJK(ix_p2,j,k)]+wt_p3*aaa[IJK(ix_p3,j,k)]

     /* interpolate to location ix+fx at each jy,kz level */

     f_jm2_km2 = XINT(xdar,jy_m2,kz_m2) ; f_jm1_km2 = XINT(xdar,jy_m1,kz_m2) ;
     f_j00_km2 = XINT(xdar,jy_00,kz_m2) ; f_jp1_km2 = XINT(xdar,jy_p1,kz_m2) ;
     f_jp2_km2 = XINT(xdar,jy_p2,kz_m2) ; f_jp3_km2 = XINT(xdar,jy_p3,kz_m2) ;
     f_jm2_km1 = XINT(xdar,jy_m2,kz_m1) ; f_jm1_km1 = XINT(xdar,jy_m1,kz_m1) ;
     f_j00_km1 = XINT(xdar,jy_00,kz_m1) ; f_jp1_km1 = XINT(xdar,jy_p1,kz_m1) ;
     f_jp2_km1 = XINT(xdar,jy_p2,kz_m1) ; f_jp3_km1 = XINT(xdar,jy_p3,kz_m1) ;
     f_jm2_k00 = XINT(xdar,jy_m2,kz_00) ; f_jm1_k00 = XINT(xdar,jy_m1,kz_00) ;
     f_j00_k00 = XINT(xdar,jy_00,kz_00) ; f_jp1_k00 = XINT(xdar,jy_p1,kz_00) ;
     f_jp2_k00 = XINT(xdar,jy_p2,kz_00) ; f_jp3_k00 = XINT(xdar,jy_p3,kz_00) ;
     f_jm2_kp1 = XINT(xdar,jy_m2,kz_p1) ; f_jm1_kp1 = XINT(xdar,jy_m1,kz_p1) ;
     f_j00_kp1 = XINT(xdar,jy_00,kz_p1) ; f_jp1_kp1 = XINT(xdar,jy_p1,kz_p1) ;
     f_jp2_kp1 = XINT(xdar,jy_p2,kz_p1) ; f_jp3_kp1 = XINT(xdar,jy_p3,kz_p1) ;
     f_jm2_kp2 = XINT(xdar,jy_m2,kz_p2) ; f_jm1_kp2 = XINT(xdar,jy_m1,kz_p2) ;
     f_j00_kp2 = XINT(xdar,jy_00,kz_p2) ; f_jp1_kp2 = XINT(xdar,jy_p1,kz_p2) ;
     f_jp2_kp2 = XINT(xdar,jy_p2,kz_p2) ; f_jp3_kp2 = XINT(xdar,jy_p3,kz_p2) ;
     f_jm2_kp3 = XINT(xdar,jy_m2,kz_p3) ; f_jm1_kp3 = XINT(xdar,jy_m1,kz_p3) ;
     f_j00_kp3 = XINT(xdar,jy_00,kz_p3) ; f_jp1_kp3 = XINT(xdar,jy_p1,kz_p3) ;
     f_jp2_kp3 = XINT(xdar,jy_p2,kz_p3) ; f_jp3_kp3 = XINT(xdar,jy_p3,kz_p3) ;

     g_jm2_km2 = XINT(ydar,jy_m2,kz_m2) ; g_jm1_km2 = XINT(ydar,jy_m1,kz_m2) ;
     g_j00_km2 = XINT(ydar,jy_00,kz_m2) ; g_jp1_km2 = XINT(ydar,jy_p1,kz_m2) ;
     g_jp2_km2 = XINT(ydar,jy_p2,kz_m2) ; g_jp3_km2 = XINT(ydar,jy_p3,kz_m2) ;
     g_jm2_km1 = XINT(ydar,jy_m2,kz_m1) ; g_jm1_km1 = XINT(ydar,jy_m1,kz_m1) ;
     g_j00_km1 = XINT(ydar,jy_00,kz_m1) ; g_jp1_km1 = XINT(ydar,jy_p1,kz_m1) ;
     g_jp2_km1 = XINT(ydar,jy_p2,kz_m1) ; g_jp3_km1 = XINT(ydar,jy_p3,kz_m1) ;
     g_jm2_k00 = XINT(ydar,jy_m2,kz_00) ; g_jm1_k00 = XINT(ydar,jy_m1,kz_00) ;
     g_j00_k00 = XINT(ydar,jy_00,kz_00) ; g_jp1_k00 = XINT(ydar,jy_p1,kz_00) ;
     g_jp2_k00 = XINT(ydar,jy_p2,kz_00) ; g_jp3_k00 = XINT(ydar,jy_p3,kz_00) ;
     g_jm2_kp1 = XINT(ydar,jy_m2,kz_p1) ; g_jm1_kp1 = XINT(ydar,jy_m1,kz_p1) ;
     g_j00_kp1 = XINT(ydar,jy_00,kz_p1) ; g_jp1_kp1 = XINT(ydar,jy_p1,kz_p1) ;
     g_jp2_kp1 = XINT(ydar,jy_p2,kz_p1) ; g_jp3_kp1 = XINT(ydar,jy_p3,kz_p1) ;
     g_jm2_kp2 = XINT(ydar,jy_m2,kz_p2) ; g_jm1_kp2 = XINT(ydar,jy_m1,kz_p2) ;
     g_j00_kp2 = XINT(ydar,jy_00,kz_p2) ; g_jp1_kp2 = XINT(ydar,jy_p1,kz_p2) ;
     g_jp2_kp2 = XINT(ydar,jy_p2,kz_p2) ; g_jp3_kp2 = XINT(ydar,jy_p3,kz_p2) ;
     g_jm2_kp3 = XINT(ydar,jy_m2,kz_p3) ; g_jm1_kp3 = XINT(ydar,jy_m1,kz_p3) ;
     g_j00_kp3 = XINT(ydar,jy_00,kz_p3) ; g_jp1_kp3 = XINT(ydar,jy_p1,kz_p3) ;
     g_jp2_kp3 = XINT(ydar,jy_p2,kz_p3) ; g_jp3_kp3 = XINT(ydar,jy_p3,kz_p3) ;

     h_jm2_km2 = XINT(zdar,jy_m2,kz_m2) ; h_jm1_km2 = XINT(zdar,jy_m1,kz_m2) ;
     h_j00_km2 = XINT(zdar,jy_00,kz_m2) ; h_jp1_km2 = XINT(zdar,jy_p1,kz_m2) ;
     h_jp2_km2 = XINT(zdar,jy_p2,kz_m2) ; h_jp3_km2 = XINT(zdar,jy_p3,kz_m2) ;
     h_jm2_km1 = XINT(zdar,jy_m2,kz_m1) ; h_jm1_km1 = XINT(zdar,jy_m1,kz_m1) ;
     h_j00_km1 = XINT(zdar,jy_00,kz_m1) ; h_jp1_km1 = XINT(zdar,jy_p1,kz_m1) ;
     h_jp2_km1 = XINT(zdar,jy_p2,kz_m1) ; h_jp3_km1 = XINT(zdar,jy_p3,kz_m1) ;
     h_jm2_k00 = XINT(zdar,jy_m2,kz_00) ; h_jm1_k00 = XINT(zdar,jy_m1,kz_00) ;
     h_j00_k00 = XINT(zdar,jy_00,kz_00) ; h_jp1_k00 = XINT(zdar,jy_p1,kz_00) ;
     h_jp2_k00 = XINT(zdar,jy_p2,kz_00) ; h_jp3_k00 = XINT(zdar,jy_p3,kz_00) ;
     h_jm2_kp1 = XINT(zdar,jy_m2,kz_p1) ; h_jm1_kp1 = XINT(zdar,jy_m1,kz_p1) ;
     h_j00_kp1 = XINT(zdar,jy_00,kz_p1) ; h_jp1_kp1 = XINT(zdar,jy_p1,kz_p1) ;
     h_jp2_kp1 = XINT(zdar,jy_p2,kz_p1) ; h_jp3_kp1 = XINT(zdar,jy_p3,kz_p1) ;
     h_jm2_kp2 = XINT(zdar,jy_m2,kz_p2) ; h_jm1_kp2 = XINT(zdar,jy_m1,kz_p2) ;
     h_j00_kp2 = XINT(zdar,jy_00,kz_p2) ; h_jp1_kp2 = XINT(zdar,jy_p1,kz_p2) ;
     h_jp2_kp2 = XINT(zdar,jy_p2,kz_p2) ; h_jp3_kp2 = XINT(zdar,jy_p3,kz_p2) ;
     h_jm2_kp3 = XINT(zdar,jy_m2,kz_p3) ; h_jm1_kp3 = XINT(zdar,jy_m1,kz_p3) ;
     h_j00_kp3 = XINT(zdar,jy_00,kz_p3) ; h_jp1_kp3 = XINT(zdar,jy_p1,kz_p3) ;
     h_jp2_kp3 = XINT(zdar,jy_p2,kz_p3) ; h_jp3_kp3 = XINT(zdar,jy_p3,kz_p3) ;

     /* interpolate to jy+fy at each kz level */

     wt_m1 = Q_M1(fy) ; wt_00 = Q_00(fy) ; wt_p1 = Q_P1(fy) ;
     wt_p2 = Q_P2(fy) ; wt_m2 = Q_M2(fy) ; wt_p3 = Q_P3(fy) ;

     f_km2 =  wt_m2 * f_jm2_km2 + wt_m1 * f_jm1_km2 + wt_00 * f_j00_km2
            + wt_p1 * f_jp1_km2 + wt_p2 * f_jp2_km2 + wt_p3 * f_jp3_km2 ;
     f_km1 =  wt_m2 * f_jm2_km1 + wt_m1 * f_jm1_km1 + wt_00 * f_j00_km1
            + wt_p1 * f_jp1_km1 + wt_p2 * f_jp2_km1 + wt_p3 * f_jp3_km1 ;
     f_k00 =  wt_m2 * f_jm2_k00 + wt_m1 * f_jm1_k00 + wt_00 * f_j00_k00
            + wt_p1 * f_jp1_k00 + wt_p2 * f_jp2_k00 + wt_p3 * f_jp3_k00 ;
     f_kp1 =  wt_m2 * f_jm2_kp1 + wt_m1 * f_jm1_kp1 + wt_00 * f_j00_kp1
            + wt_p1 * f_jp1_kp1 + wt_p2 * f_jp2_kp1 + wt_p3 * f_jp3_kp1 ;
     f_kp2 =  wt_m2 * f_jm2_kp2 + wt_m1 * f_jm1_kp2 + wt_00 * f_j00_kp2
            + wt_p1 * f_jp1_kp2 + wt_p2 * f_jp2_kp2 + wt_p3 * f_jp3_kp2 ;
     f_kp3 =  wt_m2 * f_jm2_kp3 + wt_m1 * f_jm1_kp3 + wt_00 * f_j00_kp3
            + wt_p1 * f_jp1_kp3 + wt_p2 * f_jp2_kp3 + wt_p3 * f_jp3_kp3 ;

     g_km2 =  wt_m2 * g_jm2_km2 + wt_m1 * g_jm1_km2 + wt_00 * g_j00_km2
            + wt_p1 * g_jp1_km2 + wt_p2 * g_jp2_km2 + wt_p3 * g_jp3_km2 ;
     g_km1 =  wt_m2 * g_jm2_km1 + wt_m1 * g_jm1_km1 + wt_00 * g_j00_km1
            + wt_p1 * g_jp1_km1 + wt_p2 * g_jp2_km1 + wt_p3 * g_jp3_km1 ;
     g_k00 =  wt_m2 * g_jm2_k00 + wt_m1 * g_jm1_k00 + wt_00 * g_j00_k00
            + wt_p1 * g_jp1_k00 + wt_p2 * g_jp2_k00 + wt_p3 * g_jp3_k00 ;
     g_kp1 =  wt_m2 * g_jm2_kp1 + wt_m1 * g_jm1_kp1 + wt_00 * g_j00_kp1
            + wt_p1 * g_jp1_kp1 + wt_p2 * g_jp2_kp1 + wt_p3 * g_jp3_kp1 ;
     g_kp2 =  wt_m2 * g_jm2_kp2 + wt_m1 * g_jm1_kp2 + wt_00 * g_j00_kp2
            + wt_p1 * g_jp1_kp2 + wt_p2 * g_jp2_kp2 + wt_p3 * g_jp3_kp2 ;
     g_kp3 =  wt_m2 * g_jm2_kp3 + wt_m1 * g_jm1_kp3 + wt_00 * g_j00_kp3
            + wt_p1 * g_jp1_kp3 + wt_p2 * g_jp2_kp3 + wt_p3 * g_jp3_kp3 ;

     h_km2 =  wt_m2 * h_jm2_km2 + wt_m1 * h_jm1_km2 + wt_00 * h_j00_km2
            + wt_p1 * h_jp1_km2 + wt_p2 * h_jp2_km2 + wt_p3 * h_jp3_km2 ;
     h_km1 =  wt_m2 * h_jm2_km1 + wt_m1 * h_jm1_km1 + wt_00 * h_j00_km1
            + wt_p1 * h_jp1_km1 + wt_p2 * h_jp2_km1 + wt_p3 * h_jp3_km1 ;
     h_k00 =  wt_m2 * h_jm2_k00 + wt_m1 * h_jm1_k00 + wt_00 * h_j00_k00
            + wt_p1 * h_jp1_k00 + wt_p2 * h_jp2_k00 + wt_p3 * h_jp3_k00 ;
     h_kp1 =  wt_m2 * h_jm2_kp1 + wt_m1 * h_jm1_kp1 + wt_00 * h_j00_kp1
            + wt_p1 * h_jp1_kp1 + wt_p2 * h_jp2_kp1 + wt_p3 * h_jp3_kp1 ;
     h_kp2 =  wt_m2 * h_jm2_kp2 + wt_m1 * h_jm1_kp2 + wt_00 * h_j00_kp2
            + wt_p1 * h_jp1_kp2 + wt_p2 * h_jp2_kp2 + wt_p3 * h_jp3_kp2 ;
     h_kp3 =  wt_m2 * h_jm2_kp3 + wt_m1 * h_jm1_kp3 + wt_00 * h_j00_kp3
            + wt_p1 * h_jp1_kp3 + wt_p2 * h_jp2_kp3 + wt_p3 * h_jp3_kp3 ;

     /* interpolate to kz+fz to get output */

     wt_m1 = Q_M1(fz) ; wt_00 = Q_00(fz) ; wt_p1 = Q_P1(fz) ;
     wt_p2 = Q_P2(fz) ; wt_m2 = Q_M2(fz) ; wt_p3 = Q_P3(fz) ;

     xut[pp] =  wt_m2 * f_km2 + wt_m1 * f_km1 + wt_00 * f_k00
              + wt_p1 * f_kp1 + wt_p2 * f_kp2 + wt_p3 * f_kp3 + uex + xin[pp] ;

     yut[pp] =  wt_m2 * g_km2 + wt_m1 * g_km1 + wt_00 * g_k00
              + wt_p1 * g_kp1 + wt_p2 * g_kp2 + wt_p3 * g_kp3 + vex + yin[pp] ;

     zut[pp] =  wt_m2 * h_km2 + wt_m1 * h_km1 + wt_00 * h_k00
              + wt_p1 * h_kp1 + wt_p2 * h_kp2 + wt_p3 * h_kp3 + wex + zin[pp] ;
   }

 } /* end OpenMP */
 AFNI_OMP_END ;

 RETURN(npt) ;
}
#endif

/*----------------------------------------------------------------------------*/
/*  Forward warp a collection of DICOM xyz coordinates.
    Return value is number of points computed (should be npt).
    Negative is an error code.
*//*--------------------------------------------------------------------------*/

int THD_nwarp_forward_xyz( THD_3dim_dataset *dset_nwarp ,
                           float dfac , int npt ,
                           float *xin , float *yin , float *zin ,
                           float *xut , float *yut , float *zut  )
{
   int pp ;
   floatvec *esv ;
   mat44 nwarp_cmat , nwarp_imat ;
   MRI_IMAGE *xdim , *ydim , *zdim ;

ENTRY("THD_nwarp_forward_xyz") ;

   /* check inputs */

   if( npt <= 0 ) RETURN(-1) ;

   if( xin == NULL || yin == NULL || zin == NULL ||
       xut == NULL || yut == NULL || zut == NULL   ) RETURN(-2) ;

   /* get external slopes and check if dataset is any good */

   esv = THD_nwarp_external_slopes( dset_nwarp ) ;
   if( esv == NULL ) RETURN(-3) ;

   /* matrices */

   nwarp_cmat = dset_nwarp->daxes->ijk_to_dicom ;  /* convert ijk to xyz */
   nwarp_imat = MAT44_INV(nwarp_cmat) ;            /* convert xyz to ijk */

   xdim = DSET_BRICK(dset_nwarp,0) ;  /* displacement images */
   ydim = DSET_BRICK(dset_nwarp,1) ;
   zdim = DSET_BRICK(dset_nwarp,2) ;

   /* outsource all the actual work */

   pp = THD_nwarp_im_xyz( xdim , ydim , zdim ,
                          dfac , npt ,
                          xin , yin , zin , xut , yut , zut , nwarp_imat , esv ) ;

   RETURN(pp) ;
}

/*----------------------------------------------------------------------------*/
/*  Inverse warp a collection of DICOM xyz coordinates.
    Return value is number of points computed (should be npt).
    Negative is an error code.
    This code is now just used as the initialization for the final iteration
    to find the best inverse point.  The method used herein is backward
    tracing, nstep short steps.
*//*--------------------------------------------------------------------------*/

int THD_nwarp_inverse_xyz_step( MRI_IMAGE *xdim , MRI_IMAGE *ydim , MRI_IMAGE *zdim ,
                                float dfac , int npt ,
                                float *xin , float *yin , float *zin ,
                                float *xut , float *yut , float *zut ,
                                mat44 imat , floatvec *esv , int nstep )
{
   float qfac ;
   float *qx , *qy , *qz ; int qq ;

ENTRY("THD_nwarp_inverse_xyz_step") ;

   /* check inputs */

   if( npt <= 0 ) RETURN(-1) ;

   if( xin == NULL || yin == NULL || zin == NULL ||
       xut == NULL || yut == NULL || zut == NULL   ) RETURN(-2) ;

   if( nstep < 1 ) nstep = 1 ;
   qfac = -dfac / nstep ;      /* short step size */

   qx = (float *)malloc(sizeof(float)*npt) ; memcpy(qx,xin,sizeof(float)*npt) ;
   qy = (float *)malloc(sizeof(float)*npt) ; memcpy(qy,yin,sizeof(float)*npt) ;
   qz = (float *)malloc(sizeof(float)*npt) ; memcpy(qz,zin,sizeof(float)*npt) ;

   for( qq=0 ; qq < nstep ; qq++ ){       /* take nstep short steps backwards */
      THD_nwarp_im_xyz( xdim , ydim , zdim ,
                        qfac , npt ,
                        qx , qy , qz , xut , yut , zut , imat , esv ) ;
      if( qq < nstep-1 ){
        memcpy(qx,xut,sizeof(float)*npt) ;  /* copy output into input */
        memcpy(qy,yut,sizeof(float)*npt) ;  /* for next step backwards */
        memcpy(qz,zut,sizeof(float)*npt) ;
      }
   }

   free(qz); free(qy); free(qx) ;
   RETURN(npt) ;
}

/*----------------------------------------------------------------------------*/

static double ww_xtarg , ww_ytarg , ww_ztarg ;
static MRI_IMAGE *ww_xdim , *ww_ydim , *ww_zdim ;
static mat44 ww_imat ;
static floatvec *ww_esv ;
static float ww_tol ;

/* Cost function for better pointwise inversion:
   forward warp input, see how far it is (squared) from target point */

double NW_invert_costfunc( int npar , double *par )
{
   float xin=par[0] , yin=par[1] , zin=par[2] , xut,yut,zut , a,b,c ;

   THD_nwarp_im_xyz( ww_xdim , ww_ydim , ww_zdim ,
                     1.0f , 1 ,
                     &xin,&yin,&zin, &xut,&yut,&zut , ww_imat , ww_esv ) ;

   a = ww_xtarg-xut ; b = ww_ytarg-yut ; c = ww_ztarg-zut ;
   return (double)(a*a+b*b+c*c) ;
}

/*----------------------------------------------------------------------------*/
/* Use Powell's NEWUOA to invert a warp (for one point) */

float_triple NW_invert_xyz( float xg , float yg , float zg ,
                            MRI_IMAGE *xdim , MRI_IMAGE *ydim , MRI_IMAGE *zdim ,
                            mat44 imat , floatvec *esv )
{
   float xin,yin,zin , xut,yut,zut ;
   double par[3] ; float_triple xyz ;
   int pp ;

ENTRY("NW_invert_xyz") ;

   /* initialize by backwards stream tracing with 10 steps */

   xin = xg ; yin = yg ; zin = zg ;

   THD_nwarp_inverse_xyz_step( xdim,ydim,zdim , -1.0f , 1 ,
                               &xin,&yin,&zin , &xut,&yut,&zut , imat,esv , 10 ) ;

   /* setup and use Powell for the better result */

   ww_xtarg = xg  ; ww_ytarg = yg  ; ww_ztarg = zg  ;
   par[0]   = xut ; par[1]   = yut ; par[2]   = zut ;

   ww_xdim = xdim ; ww_ydim = ydim ; ww_zdim = zdim ; ww_imat = imat ; ww_esv = esv ;

   pp = powell_newuoa( 3 , par , 0.555 , ww_tol , 66 , NW_invert_costfunc ) ;
#if 0
INFO_message("powell count = %d",pp) ;
#endif

   /* return the results */

   xyz.a = par[0] ; xyz.b = par[1] ; xyz.c = par[2] ; RETURN(xyz) ;
}

/*----------------------------------------------------------------------------*/
/* Find the inverse warp of the set of input x,y,z coordinates.
   NOTE that dfac is not used in this implementation (assumed to be 1)!!!
*//*--------------------------------------------------------------------------*/

int THD_nwarp_inverse_xyz( THD_3dim_dataset *dset_nwarp ,
                           float dfac , int npt ,
                           float *xin , float *yin , float *zin ,
                           float *xut , float *yut , float *zut  )
{
   floatvec *esv ;
   mat44 nwarp_cmat , nwarp_imat ;
   MRI_IMAGE *xdim , *ydim , *zdim ;
   float vx,vy,vz ;

ENTRY("THD_nwarp_inverse_xyz") ;

   /* check inputs */

   if( npt <= 0 ) RETURN(-1) ;

   if( xin == NULL || yin == NULL || zin == NULL ||
       xut == NULL || yut == NULL || zut == NULL   ) RETURN(-2) ;

   /* get external slopes and check if dataset is any good */

   esv = THD_nwarp_external_slopes( dset_nwarp ) ;
   if( esv == NULL ) RETURN(-3) ;

   /* matrices */

   nwarp_cmat = dset_nwarp->daxes->ijk_to_dicom ;  /* convert ijk to xyz */
   nwarp_imat = MAT44_INV(nwarp_cmat) ;            /* convert xyz to ijk */

   xdim = DSET_BRICK(dset_nwarp,0) ;  /* displacement images */
   ydim = DSET_BRICK(dset_nwarp,1) ;
   zdim = DSET_BRICK(dset_nwarp,2) ;

   vx = DSET_DX(dset_nwarp) ;
   vy = DSET_DY(dset_nwarp) ;
   vz = DSET_DZ(dset_nwarp) ;

#if 0   /*--- the old way, using only backward stream tracing ---*/
   { float *qx,*qy,*qz , *px,*py,*pz ; int qq , nstep ;
     float pqdif , vq , tol ;
     px = (float *)malloc(sizeof(float)*npt) ; qx = (float *)malloc(sizeof(float)*npt) ;
     py = (float *)malloc(sizeof(float)*npt) ; qy = (float *)malloc(sizeof(float)*npt) ;
     pz = (float *)malloc(sizeof(float)*npt) ; qz = (float *)malloc(sizeof(float)*npt) ;

     tol = (vx*vx + vy*vy + vz*vz) * 1.e-5f ;

     THD_nwarp_inverse_xyz_step( xdim,ydim,zdim , dfac,npt ,
                                 xin,yin,zin , qx,qy,qz , nwarp_imat,esv , 4 ) ;

     for( nstep=6 ; nstep < 99 ; nstep = (int)rint(1.37*nstep) ){
       memcpy(px,qx,sizeof(float)*npt) ;
       memcpy(py,qy,sizeof(float)*npt) ;
       memcpy(pz,qz,sizeof(float)*npt) ;
       THD_nwarp_inverse_xyz_step( xdim,ydim,zdim , dfac,npt ,
                                   xin,yin,zin , qx,qy,qz , nwarp_imat,esv , nstep ) ;
       pqdif = 0.0f ;
       for( qq=0 ; qq < npt ; qq++ ){
         vx = qx[qq]-px[qq] ; vy = qy[qq]-py[qq] ; vz = qz[qq]-pz[qq] ;
         vq = vx*vx + vy*vy + vz*vz ;
         if( vq > pqdif ) pqdif = vq ;
       }
       if( pqdif <= tol ) break ;
     }

     memcpy(xut,qx,sizeof(float)*npt) ;
     memcpy(yut,qy,sizeof(float)*npt) ;
     memcpy(zut,qz,sizeof(float)*npt) ;
     free(qz); free(qy); free(qx) ; free(pz); free(py); free(px) ;
   }
#else  /*--- the new way, using Powell's NEWUOA to solve for the inverse ---*/
   { float_triple xyz ; int qq ;
     ww_tol = (fabsf(vx)+fabsf(vy)+fabsf(vz)) * (0.0111f/3.0f) ;
     for( qq=0 ; qq < npt ; qq++ ){
       xyz = NW_invert_xyz( xin[qq] , yin[qq] , zin[qq] ,
                            xdim , ydim , zdim , nwarp_imat , esv ) ;
       xut[qq] = xyz.a ; yut[qq] = xyz.b ; zut[qq] = xyz.c ;
     }
   }
#endif
   RETURN(npt) ;
}

#endif /*(C14)*/ /*###########################################################*/

#if 1
/*============================================================================*/
/* (C15) Functions to warp a dataset: a setup function and then the actual
   warper.  Note that index warps are not really used in this computation!    */
/*============================================================================*/

/*----------------------------------------------------------------------------*/
/* Take a warp dataset, and resample it to a new grid.
   The new grid is defined by a geometry string -- see function
   EDIT_get_geometry_string().  The format of the string is
     "MATRIX(b11,b12,b13,b14,b21,b22,b23,b24,b31,b32,b33,b34):nx,ny,nz"
   where the bij values comprise the 3x4 matrix that takes indexs (i,j,k)
   to coordinates (x,y,z).
*//*--------------------------------------------------------------------------*/

THD_3dim_dataset * THD_nwarp_regrid( THD_3dim_dataset *inwarp, char *geomstring )
{
   THD_3dim_dataset *outwarp=NULL ;
   float *xdout,*ydout,*zdout , *xin,*yin,*zin ;
   int    nx,ny,nz,nxy,nxyz ;
   mat44  cmat_out ;

ENTRY("THD_nwarp_regrid") ;

   /*-- some simple checks for user sanity --*/

   if( !ISVALID_DSET(inwarp) || DSET_NVALS(inwarp) < 3 || geomstring == NULL )
     RETURN(NULL) ;

   DSET_load(inwarp) ; if( !DSET_LOADED(inwarp) ) RETURN(NULL) ;

   outwarp = EDIT_geometry_constructor( geomstring , "RegridWarp") ;
   if( outwarp == NULL ) RETURN(NULL) ;

   /*-- patch up the output dataset --*/

   EDIT_dset_items( outwarp ,
                      ADN_nvals     , 3 ,
                      ADN_datum_all , MRI_float ,
                    ADN_none ) ;
   EDIT_substitute_brick( outwarp , 0 , MRI_float , NULL ) ;
   EDIT_substitute_brick( outwarp , 1 , MRI_float , NULL ) ;
   EDIT_substitute_brick( outwarp , 2 , MRI_float , NULL ) ;
   xdout = DSET_BRICK_ARRAY(outwarp,0) ; nx = DSET_NX(outwarp) ;
   ydout = DSET_BRICK_ARRAY(outwarp,1) ; ny = DSET_NY(outwarp) ; nxy  = nx*ny ;
   zdout = DSET_BRICK_ARRAY(outwarp,2) ; nz = DSET_NZ(outwarp) ; nxyz = nxy*nz;
   cmat_out = outwarp->daxes->ijk_to_dicom_real ;

   /*-- load xyz coordinates of output grid points --*/

   xin = (float *)malloc(sizeof(float)*nxyz) ;
   yin = (float *)malloc(sizeof(float)*nxyz) ;
   zin = (float *)malloc(sizeof(float)*nxyz) ;

 AFNI_OMP_START ;
#pragma omp parallel if( nxyz > 6666 )
 { int ii,jj,kk,ijk ;
#pragma omp for
   for( ijk=0 ; ijk < nxyz ; ijk++ ){
     ii = ijk % nx ; kk = ijk / nxy ; jj = (ijk-kk*nxy) / nx ;
     MAT44_VEC(cmat_out,ii,jj,kk,xin[ijk],yin[ijk],zin[ijk]) ;
   }
 }

   /*-- compute forward warp of each point --*/

   (void)THD_nwarp_forward_xyz( inwarp , 1.0f , nxyz ,
                                xin   , yin   , zin  ,
                                xdout , ydout , zdout ) ;

   /*-- subtract off the input xyz coords --*/

 AFNI_OMP_START ;
#pragma omp parallel if( nxyz > 6666 )
{ int ijk ;
#pragma omp for
   for( ijk=0 ; ijk < nxyz ; ijk++ ){
     xdout[ijk] -= xin[ijk] ; ydout[ijk] -= yin[ijk] ; zdout[ijk] -= zin[ijk] ;
   }
 }
 AFNI_OMP_END ;

   /*-- toss the trash, and then it's Miller Time (no endorsement implied) --*/

   free(zin) ; free(yin) ; free(xin) ;
   DSET_superlock(outwarp) ;
   RETURN(outwarp) ;
}

/*----------------------------------------------------------------------------*/
/* Do the setup to warp images given
     bimar    = array of DICOM (x,y,z) deltas
                  = 3D warp displacment function in base image space;
     use_amat = if nonzero, use the amat matrix
     amat     = matrix to transform (x,y,z) AFTER the bimar deltas
     cmat_bim = matrix to transform indexes (ib,jb,kb) to DICOM (xb,yb,zb),
                  in base image space
     cmat_src = similar matrix for source dataset (to be warped from)
     cmat_out = similar matrix for output dataset (to be warped to);
                  for most purposes, cmat_out will either be cmat_bim
                  (to get the source image warped to the base grid)
                  or will be cmat_src (warp the source image on its own grid)

   foreach (io,jo,ko) in output dataset do {
     (xo,yo,zo) =    [cmat_out](io,jo,ko)     -- transform indexes to coords
     (ib,jb,kb) = inv[cmat_bim](xo,yo,zo)     -- transform coords to indexes
     (xs,ys,zs) =  (xo,yo,zo)                 -- compute warped coords
                 + bimar interpolated at (ib,jb,kb)
     (is,js,ks) = inv[cmat_src](xs,ys,zs)     -- compute warped indexes
   }

   The output is the array of images of (is,js,ks) = indexes in the source
   dataset, for each point to interpolate to in the output dataset (io,jo,ko).
   (N.B.: this is NOT an IndexWarp3D struct!).
   This set of images can be used, in turn, to interpolate a src grid image
   to an output grid warped image via THD_interp_floatim().
*//*--------------------------------------------------------------------------*/

MRI_IMARR * THD_setup_nwarp( MRI_IMARR *bimar,
                             int use_amat    , mat44 amat ,
                             mat44 cmat_bim  ,
                             int incode      , float wfac ,
                             mat44 cmat_src  ,
                             mat44 cmat_out  ,
                             int nx_out      , int ny_out , int nz_out  )
{
   int nx,ny,nz,nxy,nxyz ;
   float *xp, *yp, *zp , *wx, *wy, *wz ;
   MRI_IMAGE *wxim, *wyim, *wzim ; MRI_IMARR *wimar ; mat44 tmat ;

ENTRY("THD_setup_nwarp") ;

   nx = nx_out ; ny = ny_out ; nz = nz_out ; nxy = nx*ny ; nxyz = nxy*nz ;

   /* make space for indexes/coordinates in output space */

   xp = (float *)malloc(sizeof(float)*nxyz) ;
   yp = (float *)malloc(sizeof(float)*nxyz) ;
   zp = (float *)malloc(sizeof(float)*nxyz) ;

   /* compute indexes of each point in output image
      (the _out grid) in the warp space (the _bim grid) */

   if( !MAT44_FLEQ(cmat_bim,cmat_out) ){ /* output & base grids not the same */
     mat44 imat_out_to_bim ;

     /* cmat_out takes (i,j,k):out to (x,y,z)
        tmat     takes (x,y,z)     to (i,j,k):base
        so imat_out_to_bim takes (i,j,k):out to (i,j,k):base */

     tmat = MAT44_INV(cmat_bim) ; imat_out_to_bim = MAT44_MUL(tmat,cmat_out) ;

 AFNI_OMP_START ;
#pragma omp parallel if( nxyz > 1111 )
 { int qq , ii,jj,kk ;
#pragma omp for
     for( qq=0 ; qq < nxyz ; qq++ ){
       ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
       MAT44_VEC( imat_out_to_bim , ii,jj,kk , xp[qq],yp[qq],zp[qq] ) ;
     }
 }
 AFNI_OMP_END ;

   } else {   /* case where cmat_bim and cmat_out are equal */
                       /* so (i,j,k):out == (i,j,k):base */
 AFNI_OMP_START ;
#pragma omp parallel if( nxyz > 1111 )
 { int qq , ii,jj,kk ;
#pragma omp for
     for( qq=0 ; qq < nxyz ; qq++ ){
       ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
       xp[qq] = ii ; yp[qq] = jj ; zp[qq] = kk ;
     }
 }
 AFNI_OMP_END ;

   }

   /* now interpolate the warp delta volumes from the bim grid
      to the out grid, using the indexes computed just above;
      note that these deltas are still in mm, not in indexes! */

   wxim = mri_new_vol(nx,ny,nz,MRI_float) ; wx = MRI_FLOAT_PTR(wxim) ;
   wyim = mri_new_vol(nx,ny,nz,MRI_float) ; wy = MRI_FLOAT_PTR(wyim) ;
   wzim = mri_new_vol(nx,ny,nz,MRI_float) ; wz = MRI_FLOAT_PTR(wzim) ;

   THD_interp_floatim( IMARR_SUBIM(bimar,0), nxyz,xp,yp,zp, incode, wx ) ;
   THD_interp_floatim( IMARR_SUBIM(bimar,1), nxyz,xp,yp,zp, incode, wy ) ;
   THD_interp_floatim( IMARR_SUBIM(bimar,2), nxyz,xp,yp,zp, incode, wz ) ;

   /* affinely transform these deltas, if ordered to use amat we are */

   if( use_amat ){
     mat44 aamat=amat , aimat=amat , iimat ;
     /* aimat = amat - Identity */
     aimat.m[0][0] -= 1.0f ; aimat.m[1][1] -= 1.0f ; aimat.m[2][2] -= 1.0f ;
     /* iimat = matrix that takes (i,j,k) to (x,y,z) and then transforms via aimat */
     iimat = MAT44_MUL(aimat,cmat_out) ;
 AFNI_OMP_START ;
#pragma omp parallel if( nxyz > 1111 )
     { int qq , ii,jj,kk ; float xb,yb,zb , xm,ym,zm ;
#pragma omp for
         for( qq=0 ; qq < nxyz ; qq++ ){
           ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
           MAT33_VEC(aamat,wx[qq],wy[qq],wz[qq],xb,yb,zb) ;  /* just the 3x3 part */
           MAT44_VEC(iimat,ii    ,jj    ,kk    ,xm,ym,zm) ;  /* all of the matrix */
           wx[qq] = xb+xm ; wy[qq] = yb+ym ; wz[qq] = zb+zm ; /* add pieces parts */
         }
     }
 AFNI_OMP_END ;
   }

   free(zp) ; free(yp) ; free(xp) ;

   /* now convert to index warp from src to out space */

   tmat = MAT44_INV(cmat_src) ;  /* takes (x,y,z) to (i,j,k) in src space */

 AFNI_OMP_START ;
#pragma omp parallel if( nxyz > 1111 )
 { int qq,ii,jj,kk ; float xx,yy,zz , fac ;
   fac = (wfac == 0.0f) ? 1.0f : wfac ;
#pragma omp for
   for( qq=0 ; qq < nxyz ; qq++ ){
     ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
     MAT44_VEC( cmat_out , ii,jj,kk , xx,yy,zz ) ;         /* compute (xo,yo,zo) */
     xx += fac*wx[qq]; yy += fac*wy[qq]; zz += fac*wz[qq]; /* add in the deltas */
     MAT44_VEC( tmat, xx,yy,zz, wx[qq],wy[qq],wz[qq] ) ;   /* ==> to (is,js,ks) */
   }
 }
 AFNI_OMP_END ;

   /* package results for delivery to the (ab)user */

   INIT_IMARR(wimar) ;
   ADDTO_IMARR(wimar,wxim) ; ADDTO_IMARR(wimar,wyim) ; ADDTO_IMARR(wimar,wzim) ;

   RETURN(wimar) ;
}

/*----------------------------------------------------------------------------*/
/* Warp a dataset dset_src using the dset_nwarp dataset to control the
   displacements, patterning the output after dset_mast.
*//*--------------------------------------------------------------------------*/

THD_3dim_dataset * THD_nwarp_dataset( THD_3dim_dataset *dset_nwarp ,
                                      THD_3dim_dataset *dset_src   ,
                                      THD_3dim_dataset *dset_mast  ,
                                      char *prefix , int wincode , int dincode ,
                                      float dxyz_mast, float wfac, int nvlim,
                                      MRI_IMAGE *amatim                      )
{
   MRI_IMARR *imar_nwarp=NULL , *im_src=NULL ;
   mat44 src_cmat, nwarp_cmat, mast_cmat ;
   THD_3dim_dataset *dset_out , *dset_qwarp ;
   MRI_IMAGE *fim , *wim ; float *ip=NULL,*jp=NULL,*kp=NULL ;
   int nx,ny,nz,nxyz , nvals , kk,iv , next ;
   float *amatar=NULL ; int nxa=0,nya=0 ; mat44 amat ;
   int vp ;

ENTRY("THD_nwarp_dataset") ;

   /*-------- check inputs to see if the user is completely demented ---------*/

   if( dset_nwarp == NULL || dset_src == NULL ) RETURN(NULL) ;

   DSET_load(dset_nwarp) ; if( !DSET_LOADED(dset_nwarp) ) RETURN(NULL) ;
   DSET_load(dset_src)   ; if( !DSET_LOADED(dset_src)   ) RETURN(NULL) ;

   nvals = DSET_NVALS(dset_src) ; if( nvals > nvlim && nvlim > 0 ) nvals = nvlim ;

   LOAD_IDENT_MAT44(amat) ;
   if( amatim != NULL ){
     amatar = MRI_FLOAT_PTR(amatim) ;
     nxa    = amatim->nx ;
     nya    = amatim->ny ;
     if( nxa < 12 ){ nya = 0 ; amatar = NULL ; } /* this is bad */
     else if( nya > nvals ){ nya = nvals ; }     /* this is OK */
   }

   if( dset_mast == NULL ) dset_mast = dset_src ;  /* default master */

   if( prefix == NULL || *prefix == '\0' ){ /* fake up a prefix */
     char *cpt ;
     prefix = (char *)malloc(sizeof(char)*THD_MAX_NAME) ;
     strcpy( prefix , DSET_PREFIX(dset_src) ) ;
     cpt = strcasestr(prefix,".nii") ; if( cpt != NULL ) *cpt = '\0' ;
     strcat( prefix , "_nwarp" ) ; if( cpt != NULL ) strcat(prefix,".nii") ;
   }

   /*----- extend the warp dataset to allow for outliers [15 Apr 2014] -----*/
   /*........ ((( this is kind of arbitrary, but helps sometimes ))) .......*/

   next = (int)AFNI_numenv("AFNI_NWARP_EXTEND") ;  /* 18 Aug 2014 */
   if( next < 32 ) next = 32 ;

   dset_qwarp = THD_nwarp_extend( dset_nwarp , next,next,next,next,next,next ) ;
   if( dset_qwarp == NULL ){  /* should never happen */
     ERROR_message("Can't extend nwarp dataset ?!?") ; RETURN(NULL) ;
   }

   /*---------- manufacture the empty shell of the output dataset ----------*/

   if( !ISVALID_MAT44(dset_src->daxes->ijk_to_dicom) )
     THD_daxes_to_mat44(dset_src->daxes) ;
   src_cmat = dset_src->daxes->ijk_to_dicom ;

   if( !ISVALID_MAT44(dset_qwarp->daxes->ijk_to_dicom) )
     THD_daxes_to_mat44(dset_qwarp->daxes) ;
   nwarp_cmat = dset_qwarp->daxes->ijk_to_dicom ;

   if( dxyz_mast > 0.0f ){
     THD_3dim_dataset *qset ; double dxyz = (double)dxyz_mast ;
     qset = r_new_resam_dset( dset_mast , NULL ,
                              dxyz,dxyz,dxyz ,
                              NULL , RESAM_NN_TYPE , NULL , 0 , 0) ;
     if( qset != NULL ){
       dset_mast = qset ;
       THD_daxes_to_mat44(dset_mast->daxes) ;
     }
   }

   if( !ISVALID_MAT44(dset_mast->daxes->ijk_to_dicom) ) /* make sure have */
     THD_daxes_to_mat44(dset_mast->daxes) ;      /* index-to-DICOM matrix */

   mast_cmat = dset_mast->daxes->ijk_to_dicom ;

   dset_out = EDIT_empty_copy( dset_mast ) ;  /* create the output dataset! */
   EDIT_dset_items( dset_out ,                /* and patch it up */
                      ADN_prefix    , prefix ,
                      ADN_nvals     , nvals ,
                      ADN_datum_all , MRI_float ,
                    ADN_none ) ;
   if( DSET_NUM_TIMES(dset_src) > 1 && nvals > 1 )
     EDIT_dset_items( dset_out ,
                        ADN_ntt   , nvals ,
                        ADN_ttdel , DSET_TR(dset_src) ,
                        ADN_tunits, UNITS_SEC_TYPE ,
                        ADN_nsl   , 0 ,
                      ADN_none ) ;
   else
     EDIT_dset_items( dset_out ,
                        ADN_func_type , ISANAT(dset_out) ? ANAT_BUCK_TYPE
                                                         : FUNC_BUCK_TYPE ,
                      ADN_none ) ;

   /* copy brick info into output */

   THD_copy_datablock_auxdata( dset_src->dblk , dset_out->dblk ) ;
   for( kk=0 ; kk < nvals ; kk++ )
     EDIT_BRICK_FACTOR(dset_out,kk,0.0) ;

   THD_daxes_to_mat44(dset_out->daxes) ;           /* save coord transforms */

   /*----- create warping indexes from warp dataset -----*/

   INIT_IMARR(imar_nwarp) ;
   fim = THD_extract_float_brick(0,dset_qwarp) ; ADDTO_IMARR(imar_nwarp,fim) ;
   fim = THD_extract_float_brick(1,dset_qwarp) ; ADDTO_IMARR(imar_nwarp,fim) ;
   fim = THD_extract_float_brick(2,dset_qwarp) ; ADDTO_IMARR(imar_nwarp,fim) ;
   DSET_delete(dset_qwarp) ;

   nx = DSET_NX(dset_out) ;
   ny = DSET_NY(dset_out) ;
   nz = DSET_NZ(dset_out) ; nxyz = nx*ny*nz ;

   /* the actual work of setting up the warp (for all sub-bricks) */

   if( amatar == NULL || nya == 1 ){
     if( amatar != NULL ){
       LOAD_MAT44(amat,amatar[0],amatar[1],amatar[ 2],amatar[ 3],
                       amatar[4],amatar[5],amatar[ 6],amatar[ 7],
                       amatar[8],amatar[9],amatar[10],amatar[11]) ;
     }
     im_src = THD_setup_nwarp( imar_nwarp, nya,amat ,
                               nwarp_cmat, wincode , wfac ,
                               src_cmat , mast_cmat , nx , ny , nz ) ;
     ip = MRI_FLOAT_PTR( IMARR_SUBIM(im_src,0) ) ;
     jp = MRI_FLOAT_PTR( IMARR_SUBIM(im_src,1) ) ;
     kp = MRI_FLOAT_PTR( IMARR_SUBIM(im_src,2) ) ;
     DESTROY_IMARR(imar_nwarp) ;
   }

   /*----- warp each sub-brick of the input -----*/

   vp = 1 + (nvals/40) ;
   for( iv=0 ; iv < nvals ; iv++ ){
     fim = THD_extract_float_brick(iv,dset_src) ; DSET_unload_one(dset_src,iv) ;
     wim = mri_new_vol(nx,ny,nz,MRI_float) ;
     if( nya > 1 ){                     /* warp setup for just this sub-brick */
       int im = nxa * MIN(iv,nya-1) ;
       LOAD_MAT44(amat,amatar[0+im],amatar[1+im],amatar[ 2+im],amatar[ 3+im],
                       amatar[4+im],amatar[5+im],amatar[ 6+im],amatar[ 7+im],
                       amatar[8+im],amatar[9+im],amatar[10+im],amatar[11+im]) ;
       im_src = THD_setup_nwarp( imar_nwarp, nya,amat ,
                                 nwarp_cmat, wincode, wfac ,
                                 src_cmat , mast_cmat , nx , ny , nz ) ;
       ip = MRI_FLOAT_PTR( IMARR_SUBIM(im_src,0) ) ;
       jp = MRI_FLOAT_PTR( IMARR_SUBIM(im_src,1) ) ;
       kp = MRI_FLOAT_PTR( IMARR_SUBIM(im_src,2) ) ;
     }
     if( verb_nww && iv == 0 ) fprintf(stderr,"++ Warping dataset: ") ;
     THD_interp_floatim( fim, nxyz,ip,jp,kp, dincode, MRI_FLOAT_PTR(wim) ) ;
#if 0
     if( MRI_HIGHORDER(dincode) ){ /* clipping */
       double_pair fmm = mri_minmax(fim) ;
       float fb=(float)fmm.a , ft=(float)fmm.b ; int qq ;
       float *war=MRI_FLOAT_PTR(wim) ;
       for( qq=0 ; qq < wim->nvox ; qq++ ){
         if( war[qq] < fb ) war[qq] = fb ; else if( war[qq] > ft ) war[qq] = ft ;
       }
     }
#endif
     EDIT_substitute_brick( dset_out , iv , MRI_float , MRI_FLOAT_PTR(wim) ) ;
     mri_clear_and_free(wim) ; mri_free(fim) ;
     if( nya > 1 ){ DESTROY_IMARR(im_src) ; }  /* will be re-computed */
     if( verb_nww && iv%vp == 0 ) fprintf(stderr,".") ;
   }

   if( imar_nwarp != NULL ) DESTROY_IMARR(imar_nwarp) ;
   if( im_src     != NULL ) DESTROY_IMARR(im_src) ;
   DSET_unload(dset_src) ;
   if( verb_nww ) fprintf(stderr,"\n") ;
   RETURN(dset_out) ;
}

/*----------------------------------------------------------------------------*/
/* Warp a dataset dset_src using the Nwarp_catlist nwc to control the
   displacements, patterning the output after dset_mast.  [21 Oct 2014]
*//*--------------------------------------------------------------------------*/

THD_3dim_dataset_array * THD_nwarp_dataset_array(
                             Nwarp_catlist          *nwc       ,
                             THD_3dim_dataset_array *dset_src  ,
                             THD_3dim_dataset       *dset_mast ,
                             char                  **prefix    ,
                             int wincode, int dincode,
                             float dxyz_mast, float wfac, int nvlim )
{
   THD_3dim_dataset_array *dset_out=NULL ;
   MRI_IMARR *imar_nwarp=NULL , *imar_src=NULL ;
   mat44 src_cmat, nwarp_cmat, mast_cmat , amat ;
   THD_3dim_dataset *dset_qwarp=NULL , *dset_nwarp=NULL , *dset_sss=NULL,*dset_ooo=NULL;
   MRI_IMAGE *fim=NULL , *wim=NULL ; float *ip=NULL,*jp=NULL,*kp=NULL ;
   int nx,ny,nz,nxyz , nvals , kk,iv , next , kds,numds,nvmax=0,newprefix=0 ;
   int vp , reuse=0 ;
   char *gs=NULL,*hs=NULL ;

ENTRY("THD_nwarp_dataset_array") ;

   /*-------- check inputs to see if the user is completely demented ---------*/

   if( nwc == NULL || dset_src == NULL ) RETURN(NULL) ;

   numds = dset_src->num ; if( numds <= 0 ) RETURN(NULL) ;

   /* some elementary setup and checking stuff */

   for( kds=0 ; kds < numds ; kds++ ){         /* loop over input datasets */
     dset_sss = DSET_IN_3DARR(dset_src,kds) ;
     if( verb_nww > 1 )
       INFO_message("Loading '%s' for warping",DSET_HEADNAME(dset_sss)) ;
     DSET_load(dset_sss) ;
     if( verb_nww > 1 )
       ININFO_message(" DSET_LOAD returns") ;
     if( !DSET_LOADED(dset_sss) ){
       ERROR_message("Can't load dataset '%s' for warping",DSET_HEADNAME(dset_sss)) ;
       RETURN(NULL) ;
     }
     if( kds == 0 ){     /* get first dataset's geometry */
       gs = EDIT_get_geometry_string(dset_sss) ;
       if( verb_nww > 1 )
         ININFO_message(" dataset geometry = %s",gs) ;
     } else {            /* check later datasets to see if they match */
       hs = EDIT_get_geometry_string(dset_sss) ;
       if( EDIT_geometry_string_diff(gs,hs) > 0.01f ){
         ERROR_message("Can't warp multiple datasets because they have different grids!") ;
         free(hs) ; free(gs) ; RETURN(NULL) ;
       }
       free(hs) ;  /* don't need this any more */
     }
     /* find the longest dataset ("time" axis, that is) */
     nvals = DSET_NVALS(dset_sss) ; if( nvals > nvmax ) nvmax = nvals ;
     if( !ISVALID_MAT44(dset_sss->daxes->ijk_to_dicom) )
       THD_daxes_to_mat44(dset_sss->daxes) ;
     NI_sleep(1) ;
   }
   free(gs) ; gs = hs = NULL ;

   if( nvmax > nvlim && nvlim > 0 ) nvmax = nvlim ;

   if( dset_mast == NULL ) dset_mast = DSET_IN_3DARR(dset_src,0) ;  /* default master */

   src_cmat = DSET_IN_3DARR(dset_src,0)->daxes->ijk_to_dicom ;  /* source coordinate matrix */

   if( prefix == NULL ){  /* fake up some prefixes */
     char *cpt ;
     prefix = (char **)malloc(sizeof(char *)*numds) ; newprefix = 1 ;
     for( kds=0 ; kds < numds ; kds++ ){
       prefix[kds] = (char *)malloc(sizeof(char)*THD_MAX_NAME) ;
       dset_sss = DSET_IN_3DARR(dset_src,kds) ;
       strcpy( prefix[kds] , DSET_PREFIX(dset_sss) ) ;
       cpt = strcasestr(prefix[kds],".nii") ; if( cpt != NULL ) *cpt = '\0' ;
       strcat( prefix[kds] , "_Nwarp" ) ; if( cpt != NULL ) strcat(prefix[kds],".nii") ;
     }
   }

   /*----- for creating warps and dataset sub-bricks -----*/

   next = (int)AFNI_numenv("AFNI_NWARP_EXTEND") ;  /* 18 Aug 2014 */
#if 0
   if( next < 32 ) next = 32 ;   /* warp extension, just for luck */
#endif

   if( dxyz_mast > 0.0f ){  /* if altering output grid spacings */
     THD_3dim_dataset *qset ; double dxyz = (double)dxyz_mast ;
     qset = r_new_resam_dset( dset_mast , NULL ,
                              dxyz,dxyz,dxyz ,
                              NULL , RESAM_NN_TYPE , NULL , 0 , 0) ;
     if( qset != NULL ){
       dset_mast = qset ;
       THD_daxes_to_mat44(dset_mast->daxes) ;
     }
   }

   if( !ISVALID_MAT44(dset_mast->daxes->ijk_to_dicom) ) /* make sure have */
     THD_daxes_to_mat44(dset_mast->daxes) ;      /* index-to-DICOM matrix */
   mast_cmat = dset_mast->daxes->ijk_to_dicom ;  /* master coordinate matrix */

   LOAD_IDENT_MAT44(amat) ;  /* not actually used in this function */

   /*--- create output dataset headers and patch them up ---*/

   INIT_3DARR(dset_out) ;

   for( kds=0 ; kds < numds ; kds++ ){
     dset_sss = DSET_IN_3DARR(dset_src,kds) ;
     nvals = DSET_NVALS(dset_sss) ; if( nvals > nvmax ) nvals = nvmax ;
     dset_ooo = EDIT_empty_copy( dset_mast ) ;
     if( verb_nww > 1 )
       ININFO_message(" creating empty output dataset '%s'",prefix[kds]) ;
     EDIT_dset_items( dset_ooo ,
                        ADN_prefix    , prefix[kds] ,
                        ADN_nvals     , nvals ,
                        ADN_datum_all , MRI_float ,
                      ADN_none ) ;
     if( DSET_NUM_TIMES(dset_sss) > 1 && nvals > 1 )
       EDIT_dset_items( dset_ooo ,
                          ADN_ntt   , nvals ,
                          ADN_ttdel , DSET_TR(dset_sss) ,
                          ADN_tunits, UNITS_SEC_TYPE ,
                          ADN_nsl   , 0 ,
                        ADN_none ) ;
     else
       EDIT_dset_items( dset_ooo ,
                          ADN_func_type , ISANAT(dset_ooo) ? ANAT_BUCK_TYPE
                                                           : FUNC_BUCK_TYPE ,
                        ADN_none ) ;

     /* copy brick info into output (e.g., preserve sub-brick labels) */

     THD_copy_datablock_auxdata( dset_sss->dblk , dset_ooo->dblk ) ;
     for( kk=0 ; kk < nvals ; kk++ )
       EDIT_BRICK_FACTOR(dset_ooo,kk,0.0f) ;

     THD_daxes_to_mat44(dset_ooo->daxes) ;           /* save coord transforms */

     ADDTO_3DARR(dset_out,dset_ooo) ;
     NI_sleep(1) ;
   }

   nx = DSET_NX(dset_mast) ;  /* 3D grid sizes */
   ny = DSET_NY(dset_mast) ;
   nz = DSET_NZ(dset_mast) ; nxyz = nx*ny*nz ;

   vp = 1 + (nvmax/50) ;              /* how often to print a '.' progress meter */
   if( verb_nww ) fprintf(stderr,"++ Warping:") ;  /* start progress meter */

   /****** Loop over output sub-bricks,
           create the warp dataset for that 'time' index, then apply it *****/

#ifdef DEBUG_CATLIST
NI_sleep(1) ;
if( verb_nww > 1 ) fprintf(stderr,"[nvar=%d]",nwc->nvar) ;
#endif

   for( iv=0 ; iv < nvmax ; iv++ ){

     /*--- create the warp dataset into dset_qwarp (if needed) ----*/

     if( iv < nwc->nvar ){        /* don't do duplicates if past */
                                  /* end of 'time' inside nwc */
       /*** toss the old warps */
#ifdef DEBUG_CATLIST
if( verb_nww > 1 ) fprintf(stderr," a") ;
#endif
       DSET_delete  (dset_nwarp) ; DSET_delete  (dset_qwarp) ;
       DESTROY_IMARR(imar_nwarp) ; DESTROY_IMARR(imar_src  ) ;
       /*** get the iv-th warp from the list, and pad it */
#ifdef DEBUG_CATLIST
if( verb_nww > 1 ) fprintf(stderr,"b") ;
#endif
       dset_nwarp = IW3D_from_nwarp_catlist( nwc , iv ) ; /* get the iv-th warp */
       if( dset_nwarp == NULL ){  /* should never happen */
         ERROR_message("Can't acquire/compute nwarp dataset #%d ?!?",iv); RETURN(NULL) ;
       }
#ifdef DEBUG_CATLIST
if( verb_nww > 1 ) fprintf(stderr,"'") ;
#endif
       if( next > 0 ){
         dset_qwarp = THD_nwarp_extend( dset_nwarp , next,next,next,next,next,next ) ;
         if( dset_qwarp == NULL ){
           ERROR_message("Can't extend nwarp dataset #%d ?!?",iv) ; RETURN(NULL) ;
         }
       } else {
         dset_qwarp = EDIT_full_copy( dset_nwarp , "ZharksRevenge" ) ;
         if( dset_qwarp == NULL ){
           ERROR_message("Can't copy nwarp dataset #%d ?!?",iv) ; RETURN(NULL) ;
         }
       }

       if( !ISVALID_MAT44(dset_qwarp->daxes->ijk_to_dicom) )
         THD_daxes_to_mat44(dset_qwarp->daxes) ;
       nwarp_cmat = dset_qwarp->daxes->ijk_to_dicom ; /* coordinates of warp */

       /* create warping indexes from warp dataset */

       INIT_IMARR(imar_nwarp) ;
#ifdef DEBUG_CATLIST
if( verb_nww > 1 ) fprintf(stderr,"c") ;
#endif
       fim = THD_extract_float_brick(0,dset_qwarp) ; ADDTO_IMARR(imar_nwarp,fim) ;
       fim = THD_extract_float_brick(1,dset_qwarp) ; ADDTO_IMARR(imar_nwarp,fim) ;
       fim = THD_extract_float_brick(2,dset_qwarp) ; ADDTO_IMARR(imar_nwarp,fim) ;

       /* the actual work of setting up the warp for this sub-brick */

#ifdef DEBUG_CATLIST
if( verb_nww > 1 ) fprintf(stderr,"d") ;
#endif
       imar_src = THD_setup_nwarp( imar_nwarp, 0,amat ,
                                   nwarp_cmat, wincode , wfac ,
                                   src_cmat , mast_cmat , nx , ny , nz ) ;

       ip = MRI_FLOAT_PTR( IMARR_SUBIM(imar_src,0) ) ;  /* warped indexes, */
       jp = MRI_FLOAT_PTR( IMARR_SUBIM(imar_src,1) ) ;  /* for interpolation */
       kp = MRI_FLOAT_PTR( IMARR_SUBIM(imar_src,2) ) ;  /* of dataset values */

       if( iv == 0 ){  /* save the warped space label, if present */
         for( kds=0 ; kds < numds ; kds++ ){
           dset_ooo = DSET_IN_3DARR(dset_out,kds) ;
           MCW_strncpy( dset_ooo->atlas_space , dset_nwarp->atlas_space , THD_MAX_NAME ) ;
         }
       }

     } else if( !reuse & verb_nww ){
       reuse = 1 ; fprintf(stderr,"[R]") ;  /* flag that re-use has started */
     }

     /*----- warp this iv-th sub-brick of the input -----*/

     for( kds=0 ; kds < numds ; kds++ ){  /* loop over input datasets */
       dset_sss = DSET_IN_3DARR(dset_src,kds) ;
       if( DSET_NVALS(dset_sss) < iv ) continue ;  /* dataset is done already */
       dset_ooo = DSET_IN_3DARR(dset_out,kds) ;
#ifdef DEBUG_CATLIST
if( verb_nww > 1 ) fprintf(stderr,"-") ;
#endif
       if( DSET_BRICK_TYPE(dset_sss,iv) != MRI_complex ){
         fim = THD_extract_float_brick(iv,dset_sss) ; DSET_unload_one(dset_sss,iv) ;
         wim = mri_new_vol(nx,ny,nz,MRI_float) ;
         /*** the actual warping is done in the function below! ***/
#ifdef DEBUG_CATLIST
if( verb_nww > 1 ) fprintf(stderr,"+") ;
#endif
         THD_interp_floatim( fim, nxyz,ip,jp,kp, dincode, MRI_FLOAT_PTR(wim) ) ;

#if 0
         if( MRI_HIGHORDER(dincode) ){ /* clipping output values */
           double_pair fmm = mri_minmax(fim) ;
           float fb=(float)fmm.a , ft=(float)fmm.b ; int qq ;
           float *war=MRI_FLOAT_PTR(wim) ;
#ifdef DEBUG_CATLIST
if( verb_nww > 1 ) fprintf(stderr,"*") ;
#endif
           for( qq=0 ; qq < wim->nvox ; qq++ ){
             if( war[qq] < fb ) war[qq] = fb ; else if( war[qq] > ft ) war[qq] = ft ;
           }
         }
#endif
         EDIT_substitute_brick( dset_ooo, iv, MRI_float, MRI_FLOAT_PTR(wim) ) ;
         mri_free(fim) ;                           /* is a copy, so delete it */
       } else {  /* <<<-------------------------- complex image [27 Mar 2018] */
         fim = DSET_BRICK(dset_sss,iv) ;
         wim = mri_new_vol(nx,ny,nz,MRI_complex) ;
#ifdef DEBUG_CATLIST
if( verb_nww > 1 ) fprintf(stderr,"+") ;
#endif
         THD_interp_complexim( fim, nxyz,ip,jp,kp, dincode, MRI_COMPLEX_PTR(wim) ) ;
         EDIT_substitute_brick( dset_ooo, iv, MRI_complex, MRI_COMPLEX_PTR(wim) ) ;
         /* fim here is NOT a copy, so don't delete it */
       }
#ifdef DEBUG_CATLIST
if( verb_nww > 1 ) fprintf(stderr,"!") ;
#endif
       mri_clear_and_free(wim) ;  /* clear and free won't delete the data, just the shell */
     } /* end of loop over input datasets */

     if( verb_nww && iv%vp == 0 ) fprintf(stderr,".") ;  /* progress meter */

   } /*--- end of loop over output sub-bricks */

   /* toss the final warps */
#ifdef DEBUG_CATLIST
if( verb_nww > 1 ) fprintf(stderr," z") ;
#endif
   DSET_delete  (dset_nwarp) ; DSET_delete  (dset_qwarp) ;
   DESTROY_IMARR(imar_nwarp) ; DESTROY_IMARR(imar_src  ) ;
   for( kds=0 ; kds < numds ; kds++ ){
     dset_sss = DSET_IN_3DARR(dset_src,kds) ; DSET_unload(dset_sss) ;
     if( newprefix ) free(prefix[kds]) ;
   }
   if( newprefix ) free(prefix) ;

   if( verb_nww ) fprintf(stderr,"Z\n") ;  /* end of progress */

   RETURN(dset_out) ;
}

/*----------------------------------------------------------------------------*/
/* Shell to make calling the above function simpler for just 1 dataset.
*//*--------------------------------------------------------------------------*/

THD_3dim_dataset * THD_nwarp_dataset_NEW(
                             Nwarp_catlist    *nwc       ,
                             THD_3dim_dataset *dset_src  ,
                             THD_3dim_dataset *dset_mast ,
                             char             *prefix    ,
                             int wincode, int dincode,
                             float dxyz_mast, float wfac, int nvlim )
{
   THD_3dim_dataset_array *dset_sar , *dset_oar ;
   THD_3dim_dataset *dset_out=NULL ;

ENTRY("THD_nwarp_dataset_NEW") ;

   INIT_3DARR(dset_sar) ; ADDTO_3DARR(dset_sar,dset_src) ;

   dset_oar = THD_nwarp_dataset_array( nwc , dset_sar , dset_mast ,
                                       (prefix != NULL) ? &prefix : NULL ,
                                       wincode, dincode, dxyz_mast, wfac, nvlim ) ;

   if( dset_oar != NULL ){
     dset_out = DSET_IN_3DARR(dset_oar,0) ;
     FREE_3DARR(dset_oar) ;
   }
   FREE_3DARR(dset_sar) ; RETURN(dset_out) ;
}

#endif /*(C15)*/ /*###########################################################*/

#if 1
/*============================================================================*/
/* (C16) Reverse Polish Notation warp calculator (3dNwarpCalc program).       */
/*============================================================================*/

#undef  KILL_iwstk
#define KILL_iwstk                                                   \
 do{ if( iwstk != NULL ){                                            \
       int qq; for( qq=0; qq < nstk; qq++ ) IW3D_destroy(iwstk[qq]); \
       free(iwstk) ;                                                 \
 } } while(0)

#undef  ERREX
#define ERREX(sss)                                                     \
  do{ ERROR_message("NwarpCalcRPN('%s')\n"                             \
         "           at '%s': %s" , expr,cmd,sss );                    \
      KILL_iwstk; NI_delete_str_array(sar); FREEIFNN(geomstring);      \
      RETURN(NULL);                                                    \
  } while(0)

#undef  ADDTO_iwstk
#define ADDTO_iwstk(W)                                                       \
 do{ iwstk = (IndexWarp3D **)realloc(iwstk,sizeof(IndexWarp3D *)*(nstk+1)) ; \
     iwstk[nstk] = W ; nstk++ ;                                              \
 } while(0)


/*---------------------------------------------------------------------------*/
/* nwarp RPN calculator function (cf. 3dNwarpCalc program) */

THD_3dim_dataset * NwarpCalcRPN( char *expr, char *prefix, int icode, int acode )
{
   NI_str_array *sar ;
   char *cmd , acmd[4096] , mess[4096] ;
   IndexWarp3D **iwstk=NULL ;
   int            nstk=0 , ii , ss ;
   IndexWarp3D *AA , *BB ;
   THD_3dim_dataset *oset=NULL ;
   int nx=0,ny=0,nz=0 ;
   mat44 cmat , imat ;      /* cmat: i->x ; imat: x->i */
   char *geomstring=NULL , *sname=NULL ;

ENTRY("NwarpCalcRPN") ;

   /**----- break string into sub-strings, delimited by whitespace -----**/

   sar = NI_decode_string_list( expr , "`" ) ;
   if( sar == NULL ) RETURN(NULL) ;
   AAmemset(&imat,0,sizeof(mat44)); AAmemset(&cmat,0,sizeof(mat44));
   if( acode < 0 ) acode = icode ;

   /**----- loop thru and process commands -----**/

   if(verb_nww)INFO_message("NwarpCalcRPN('%s')",expr) ;

   for( ss=0 ; ss < sar->num ; ss++ ){

     cmd = sar->str[ss] ;

     if(verb_nww)ININFO_message(" + stack size=%d  next operation='%s'",nstk,cmd) ;

     if( *cmd == '\0' ) continue ;  /* WTF?! */

     /*--- munge command? ---*/

     if( *cmd == '%' || *cmd == '@' ){                    /* a cheap trick */
       *cmd = '&' ;
     } else if( *cmd != '&' ){
       acmd[0] = '&' ; strcpy(acmd+1,cmd) ; cmd = acmd ;  /* another cheap trick */
     }

     /*--- read warp from a dataset ---*/

     if( strncasecmp(cmd,"&readnwarp(",11) == 0 ||
         strncasecmp(cmd,"&readwarp(" ,10) == 0   ){
       char *buf , *bp=strchr(cmd,'(') ; THD_3dim_dataset *dset ;
       buf = strdup(bp+1) ;
       for( bp=buf ; *bp != '\0' && *bp != ')' ; bp++ ) ; /*nada*/
       if( *bp == ')' ) *bp = '\0' ;  /* delete trailing ) */
       dset = THD_open_dataset(buf) ; DSET_COPYOVER_REAL(dset) ;
       if( dset == NULL ){
         sprintf(mess,"Can't read file '%s'",buf); free(buf); ERREX(mess);
       }
       AA = IW3D_from_dataset(dset,0,0) ; DSET_delete(dset) ;
       if( AA == NULL ){
         sprintf(mess,"Can't make warp from '%s'",buf); free(buf); ERREX(mess);
       }
       if( geomstring == NULL ){
         geomstring = strdup(AA->geomstring) ;
         sname      = strdup(dset->atlas_space) ;
         nx = AA->nx; ny = AA->ny; nz = AA->nz; cmat = AA->cmat; imat = AA->imat;
       } else if( AA->nx != nx || AA->ny != ny || AA->nz != nz ){
         sprintf(mess,"non-conforming warp from '%s'",buf); free(buf); ERREX(mess);
       }
       ADDTO_iwstk(AA) ; free(buf) ;
     }

     /*--- make identity warp from a dataset ---*/

     else if( strncasecmp(cmd,"&identwarp(",11) == 0 ){
       char *buf=strdup(cmd+11) , *bp ; THD_3dim_dataset *dset ;
       for( bp=buf ; *bp != '\0' && *bp != ')' ; bp++ ) ; /*nada*/
       if( *bp == ')' ) *bp = '\0' ;  /* delete trailing ) */
       dset = THD_open_dataset(buf) ; DSET_COPYOVER_REAL(dset) ;
       if( dset == NULL ){
         sprintf(mess,"Can't read file '%s'",buf); free(buf); ERREX(mess);
       }
       AA = IW3D_from_dataset(dset,1,0) ; DSET_delete(dset) ;
       if( AA == NULL ){
         sprintf(mess,"Can't make identwarp from '%s'",buf); free(buf); ERREX(mess);
       }
       if( geomstring == NULL ){
         geomstring = strdup(AA->geomstring) ;
         nx = AA->nx; ny = AA->ny; nz = AA->nz; cmat = AA->cmat; imat = AA->imat;
       } else if( AA->nx != nx || AA->ny != ny || AA->nz != nz ){
         sprintf(mess,"non-conforming warp from '%s'",buf); free(buf); ERREX(mess);
       }
       ADDTO_iwstk(AA) ; free(buf) ;
     }

     /*--- create a warp from a set of polynomial parameters ---*/

     else if( strncasecmp(cmd,"&readpoly(",10) == 0 ){
       char *buf=strdup(cmd+10) , *bp ; MRI_IMAGE *qim,*fim ; float *far ;
       if( nstk < 1 ){ free(buf); ERREX("nothing on stack -- needed for template"); }
       for( bp=buf ; *bp != '\0' && *bp != ')' ; bp++ ) ; /*nada*/
       if( *bp == ')' ) *bp = '\0' ;  /* delete trailing ) */
       qim = mri_read_1D(buf) ;
       if( qim == NULL ){
         sprintf(mess,"Can't read file '%s'",buf); free(buf); ERREX(mess);
       }
       fim = mri_transpose(qim) ; mri_free(qim) ; far = MRI_FLOAT_PTR(fim) ;
       affmode = AFF_PARAM ;
       AA = IW3D_from_poly( fim->nx , far , iwstk[nstk-1] ) ;
       if( AA == NULL ){
         sprintf(mess,"Can't use file '%s' -- num param=%d",buf,fim->nx);
         mri_free(fim); free(buf); ERREX(mess);
       }
       mri_free(fim) ; ADDTO_iwstk(AA) ; free(buf) ;
     }

     /*--- create a warp from a matrix ---*/

     else if( strncasecmp(cmd,"&read4x4(",9) == 0 ){
       char *buf=strdup(cmd+9) , *bp ; MRI_IMAGE *qim,*fim ; float *far ;
       if( nstk < 1 ){ free(buf); ERREX("nothing on stack -- needed for template"); }
       for( bp=buf ; *bp != '\0' && *bp != ')' ; bp++ ) ; /*nada*/
       if( *bp == ')' ) *bp = '\0' ;  /* delete trailing ) */
       qim = mri_read_1D(buf) ;
       if( qim == NULL ){
         sprintf(mess,"Can't read file '%s'",buf); free(buf); ERREX(mess);
       }
       fim = mri_transpose(qim) ; mri_free(qim) ; far = MRI_FLOAT_PTR(fim) ;
       if( fim->nvox < 12 ){
         sprintf(mess,"file '%s' has fewer than 12 numbers",buf);
         free(buf) ; mri_free(fim) ; ERREX(mess) ;
       }
       affmode = AFF_MATRIX ;
       AA = IW3D_from_poly( 12 , far , iwstk[nstk-1] ) ;
       if( AA == NULL ){
         sprintf(mess,"Can't make matrix from file '%s'",buf);
         mri_free(fim); free(buf); ERREX(mess);
       }
       mri_free(fim) ; ADDTO_iwstk(AA) ; free(buf) ;
     }

     /*--- write it out, babee ---*/

     else if( strncasecmp(cmd,"&write(",7) == 0 ){
       char *buf=strdup(cmd+7) , *bp ; THD_3dim_dataset *dset ;
       if( nstk < 1 ){ free(buf); ERREX("nothing on stack"); }
       for( bp=buf ; *bp != '\0' && *bp != ')' ; bp++ ) ; /*nada*/
       if( *bp == ')' ) *bp = '\0' ;  /* delete trailing ) */
       AA = iwstk[nstk-1] ; FREEIFNN(AA->geomstring) ;
       AA->geomstring = strdup(geomstring) ; AA->cmat = cmat ; AA->imat = imat ;
       dset = IW3D_to_dataset( AA , buf ) ;
       if( sname != NULL ) MCW_strncpy( dset->atlas_space , sname , THD_MAX_NAME ) ;
       DSET_write(dset) ;
       if( verb_nww ) ININFO_message(" -- wrote dataset %s",DSET_BRIKNAME(dset)) ;
       DSET_delete(dset) ; free(buf) ;
     }

     /*--- duplication ---*/

     else if( strcasecmp(cmd,"&dup") == 0 ){
       if( nstk < 1 ) ERREX("nothing on stack") ;
       AA = IW3D_copy( iwstk[nstk-1] , 1.0f ) ;
       ADDTO_iwstk(AA) ;
     }

     /*--- pop tart time! ---*/

     else if( strcasecmp(cmd,"&pop") == 0 ){
        if( nstk < 1 ) ERREX("nothing on stack") ;
        IW3D_destroy( iwstk[nstk-1] ) ;
        nstk-- ;
     }

     /*--- swap-eroni ---*/

     else if( strncasecmp(cmd,"&swap",5) == 0 ){  /* modified 06 May 2013 */
       char *bp=strchr(cmd,'(') ;                 /* to allow (a,b) args */
       int nAA=1 , nBB=0 ;
       if( bp != NULL ){
         nAA = nBB = -666 ;
         sscanf(bp+1,"%d,%d",&nAA,&nBB) ;
         if( nAA < 0 || nBB < 0 || nAA == nBB ) ERREX("illegal values in &swap") ;
       }
       nAA++ ; nBB++ ;
       if( nstk < MAX(nAA,nBB) ) ERREX("stack too short for &swap") ;
       AA = iwstk[nstk-nAA] ; BB = iwstk[nstk-nBB] ;
       iwstk[nstk-nAA] = BB ; iwstk[nstk-nBB] = AA ;
     }

     /*--- go to Australia (viz., invert) ---*/

     else if( strcasecmp(cmd,"&invert") == 0 || strcasecmp(cmd,"&inverse") == 0 ){
        double ct = COX_cpu_time() ;
        if( nstk < 1 ) ERREX("nothing on stack") ;
        AA = IW3D_invert( iwstk[nstk-1] , NULL , icode ) ;
        if( AA == NULL ) ERREX("inversion failed :-(") ;
        IW3D_destroy( iwstk[nstk-1] ) ; iwstk[nstk-1] = AA ;
        if( verb_nww )
          ININFO_message(" -- invert CPU time = %.1f s",COX_cpu_time()-ct) ;
     }

     /*--- inverse square root ---*/

     else if( strcasecmp(cmd,"&sqrtinv") == 0 || strcasecmp(cmd,"&invsqrt") == 0 ){
        double ct = COX_cpu_time() ;
        if( nstk < 1 ) ERREX("nothing on stack") ;
#ifndef USE_SQRTPAIR
        AA = IW3D_sqrtinv( iwstk[nstk-1] , NULL , icode ) ;
#else
        { IndexWarp3D_pair *YZ = IW3D_sqrtpair(iwstk[nstk-1],icode) ;
          AA = YZ->iwarp ; IW3D_destroy(YZ->fwarp) ; free(YZ) ;
        }
#endif
        if( AA == NULL ) ERREX("inverse square root failed :-(") ;
        IW3D_destroy( iwstk[nstk-1] ) ; iwstk[nstk-1] = AA ;
        if( verb_nww )
          ININFO_message(" -- inverse square root CPU time = %.1f s",COX_cpu_time()-ct) ;
     }

     /*--- square root ---*/

     else if( strcasecmp(cmd,"&sqrt") == 0 ){
        double ct = COX_cpu_time() ;
        if( nstk < 1 ) ERREX("nothing on stack") ;
#ifndef USE_SQRTPAIR
        AA = IW3D_sqrtinv( iwstk[nstk-1] , NULL , icode ) ;
        if( AA == NULL ) ERREX("inverse square root failed :-(") ;
        BB = IW3D_invert( AA , NULL , icode ) ; IW3D_destroy(AA) ;
        if( BB == NULL ) ERREX("inversion after sqrtinv failed :-(") ;
#else
        { IndexWarp3D_pair *YZ = IW3D_sqrtpair(iwstk[nstk-1],icode) ;
          BB = YZ->fwarp ; IW3D_destroy(YZ->iwarp) ; free(YZ) ;
        }
#endif
        IW3D_destroy( iwstk[nstk-1] ) ; iwstk[nstk-1] = BB ;
        if( verb_nww )
          ININFO_message(" -- square root CPU time = %.1f s",COX_cpu_time()-ct) ;
     }

     /*--- sqrtpair ---*/

     else if( strcasecmp(cmd,"&sqrtpair") == 0 ){
        double ct = COX_cpu_time() ;
        if( nstk < 1 ) ERREX("nothing on stack") ;
#ifndef USE_SQRTPAIR
        AA = IW3D_sqrtinv( iwstk[nstk-1] , NULL , icode ) ;
        if( AA == NULL ) ERREX("inverse square root failed :-(") ;
        BB = IW3D_invert( AA , NULL , icode ) ;
        if( BB == NULL ) ERREX("inversion after sqrtinv failed :-(") ;
#else
        { IndexWarp3D_pair *YZ = IW3D_sqrtpair(iwstk[nstk-1],icode) ;
          BB = YZ->fwarp ; AA = YZ->iwarp ; free(YZ) ;
        }
#endif
        IW3D_destroy( iwstk[nstk-1] ) ; iwstk[nstk-1] = AA ; ADDTO_iwstk(BB) ;
        if( verb_nww )
          ININFO_message(" -- square root pair CPU time = %.1f s",COX_cpu_time()-ct) ;
     }

     /*--- compose ---*/

     else if( strcasecmp(cmd,"&compose") == 0 || strcasecmp(cmd,"&*") == 0 ||
              strcasecmp(cmd,"&mult")    == 0                                ){
        double ct = COX_cpu_time() ;
        if( nstk < 2 ) ERREX("stack too short") ;
        AA = IW3D_compose( iwstk[nstk-1] , iwstk[nstk-2] , icode ) ;
        if( AA == NULL ) ERREX("composition failed :-(") ;
        IW3D_destroy( iwstk[nstk-1] ) ; IW3D_destroy( iwstk[nstk-2] ) ;
        iwstk[nstk-2] = AA ; nstk-- ;
        if( verb_nww )
          ININFO_message(" -- compose CPU time = %.1f s",COX_cpu_time()-ct) ;
     }

     /*--- totally square, man ---*/

     else if( strcasecmp(cmd,"&sqr") == 0 || strcasecmp(cmd,"&square") == 0 ){
        double ct = COX_cpu_time() ;
        if( nstk < 1 ) ERREX("nothing on stack") ;
        AA = IW3D_compose( iwstk[nstk-1] , iwstk[nstk-1] , icode ) ;
        if( AA == NULL ) ERREX("composition failed :-(") ;
        IW3D_destroy( iwstk[nstk-1] ) ;
        iwstk[nstk-1] = AA ;
        if( verb_nww )
          ININFO_message(" -- sqr CPU time = %.1f s",COX_cpu_time()-ct) ;
     }

     /*--- scale ---*/

     else if( strncasecmp(cmd,"&scale(",7) == 0 ){
       char *buf=strdup(cmd+7) , *bp ; float val ;
       if( nstk < 1 ){ free(buf); ERREX("nothing on stack"); }
       for( bp=buf ; *bp != '\0' && *bp != ')' ; bp++ ) ; /*nada*/
       if( *bp == ')' ) *bp = '\0' ;  /* delete trailing ) */
       val = (float)strtod(buf,NULL) ; free(buf) ;
       IW3D_scale( iwstk[nstk-1] , val ) ;
     }

     /*--- sum ---*/

     else if( strncasecmp(cmd,"&sum",4) == 0 ){
       char *bp=strchr(cmd,'(') ;
       float alpha=1.0f , beta=1.0f ;
       if( nstk < 2 ) ERREX("stack is too small") ;
       if( bp != NULL ) sscanf(bp+1,"%f,%f",&alpha,&beta) ;
       AA = IW3D_sum( iwstk[nstk-1],alpha , iwstk[nstk-2],beta ) ;
       IW3D_destroy( iwstk[nstk-2] ) ; IW3D_destroy( iwstk[nstk-1] ) ;
       nstk-- ; iwstk[nstk-1] = AA ;
     }

     /*--- apply ---*/

     else if( strncasecmp(cmd,"&apply(",7) == 0 ){
       char *buf=strdup(cmd+7) , *bp , *pref ;
       THD_3dim_dataset *wset , *iset , *oset ;
       if( nstk < 1 ){ free(buf); ERREX("nothing on stack"); }
       for( bp=buf ; *bp != '\0' && *bp != ')' ; bp++ ) ; /*nada*/
       if( *bp == ')' ) *bp = '\0' ;  /* delete trailing ) */
       for( bp=buf ; *bp != '\0' && *bp != ',' ; bp++ ) ; /*nada*/
       if( *bp != ',' ){ free(buf); ERREX("no comma for prefix"); }
       *bp = '\0' ; pref = bp+1 ;     /* delete comma */
       if( !THD_filename_ok(pref) ){ free(buf); ERREX("illegal prefix"); }
       AA = iwstk[nstk-1] ; FREEIFNN(AA->geomstring) ;
       AA->geomstring = strdup(geomstring) ; AA->cmat = cmat ; AA->imat = imat ;
       iset = THD_open_dataset(buf) ; DSET_COPYOVER_REAL(iset) ;
       if( iset == NULL ){
         sprintf(mess,"Can't read file '%s'",buf); free(buf); ERREX(mess);
       }
       wset = IW3D_to_dataset( AA , buf ) ;
       oset = THD_nwarp_dataset( wset, iset, NULL, pref, icode,acode, 0.0f, 1.0f, 999999999 , NULL ) ;
                                               tross_Copy_History  (iset,oset) ;
       sprintf(mess,"NwarpCalcRPN '%s'",cmd) ; tross_Append_History(oset,mess) ;
       if( sname != NULL ) MCW_strncpy(oset->atlas_space,sname,THD_MAX_NAME) ;
       DSET_delete(iset) ; DSET_delete(wset) ; DSET_write(oset) ;
       if( verb_nww ) ININFO_message(" -- wrote dataset %s",DSET_BRIKNAME(oset)) ;
       DSET_delete(oset) ; free(buf) ;
     }

     /*--- No worst, there is none ---*/

     else {
       ERREX("unknown operation :-((") ;
     }

   } /*----- end of loop over operations -----*/

   if(verb_nww)INFO_message("end of evaluation loop") ;

   if( nstk > 0 ){
     AA = iwstk[nstk-1] ;
     FREEIFNN(AA->geomstring) ;
     AA->geomstring = strdup(geomstring) ; AA->cmat = cmat ; AA->imat = imat ;
     oset = IW3D_to_dataset( AA , prefix ) ;
   }

   KILL_iwstk ; NI_delete_str_array(sar) ; FREEIFNN(geomstring) ; FREEIFNN(sname) ;

   RETURN(oset) ;
}

#endif /*(C16)*/ /*###########################################################*/

#if 1
/*============================================================================*/
/* (C17) Functions for reading warps and inverting/catenating them right away */
/*    (for the '-nwarp' input of various 3dNwarp programs)                    */
/*============================================================================*/

#define CW_NMAX 99  /* max number of warps allowed in input expression */

static int          CW_nwtop=0 ;
static IndexWarp3D *CW_iwarp[CW_NMAX] ;
static float        CW_iwfac[CW_NMAX] ;  /* always 1.0 at present */
static mat44       *CW_awarp[CW_NMAX] ;
static mat44_vec   *CW_vwarp[CW_NMAX] ;
static int CW_nx=0,CW_ny=0,CW_nz=0 ; static char *CW_geomstring=NULL ;
static mat44 CW_cmat , CW_imat ;

static float CW_dxal=0.0f , CW_dyal=0.0f , CW_dzal=0.0f ;
static float CW_dx  =1.0f , CW_dy  =1.0f , CW_dz  =1.0f ;

static THD_3dim_dataset *CW_inset=NULL ;

/*----------------------------------------------------------------------------*/
static char *CW_saved_geomstring = NULL ;
static int   CW_saved_expad      = 0 ;
static int   CW_no_expad         = 1 ;
static int   CW_extra_pad        = 0 ;
static int   CW_interp           = MRI_WSINC5 ;

char * CW_get_saved_geomstring(void){ return CW_saved_geomstring ; }
int    CW_get_saved_expad     (void){ return CW_saved_expad ;      }

/*----------------------------------------------------------------------------*/
/* Erase the above static data */

static void CW_clear_data(void)
{
   int ii ;
   for( ii=0 ; ii < CW_NMAX ; ii++ ){
     CW_iwfac[ii] = 1.0f ;          /* usage is '#if 0'-ed out at present */
     if( CW_iwarp[ii] != NULL ){
       IW3D_destroy(CW_iwarp[ii]) ; CW_iwarp[ii] = NULL ;
     }
     if( CW_awarp[ii] != NULL ){
       free(CW_awarp[ii]) ; CW_awarp[ii] = NULL ;
     }
     if( CW_vwarp[ii] != NULL ){
       DESTROY_mat44_vec(CW_vwarp[ii]) ; CW_vwarp[ii] = NULL ;
     }
   }
   CW_nwtop = CW_nx = CW_ny = CW_nz = 0.0f ;
   if( CW_geomstring != NULL ){
     free(CW_geomstring) ; CW_geomstring = NULL ;
   }
   if( CW_inset != NULL ){
     DSET_delete(CW_inset) ; CW_inset = NULL ;
   }
   ZERO_MAT44(CW_imat) ; ZERO_MAT44(CW_cmat) ;
   CW_dxal = CW_dyal = CW_dzal = 0.0f ;
   CW_dx   = CW_dy   = CW_dz   = 1.0f ;

   return ;
}

/*----------------------------------------------------------------------------*/

float_triple M44_max_shifts( mat44_vec *mvv )
{
   float_triple xyz = {0.0f,0.0f,0.0f} ;
   mat44 mmm ; int ii ; float dxm,dym,dzm , dx,dy,dz ;

   if( mvv == NULL || mvv->nmar == 0 || mvv->mar == NULL ) return xyz ;

   dxm = dym = dzm = 0.0f ;
   for( ii=0 ; ii < mvv->nmar ; ii++ ){
     mmm = mvv->mar[ii] ;
     dx  = fabsf(mmm.m[0][3]) ; if( dx > dxm ) dxm = dx ;
     dy  = fabsf(mmm.m[1][3]) ; if( dy > dym ) dym = dy ;
     dz  = fabsf(mmm.m[2][3]) ; if( dz > dzm ) dzm = dz ;
   }

   xyz.a = dxm ; xyz.b = dym ; xyz.c = dzm ; return xyz ;
}

/*----------------------------------------------------------------------------*/

static mat44_vec * CW_read_affine_warp( char *cp )
{
   mat44 mmm ; mat44_vec *mvv=NULL ;
   MRI_IMAGE *qim ; float *qar, *tar ; char *wp , *ocp ;
   int do_inv=0 , do_sqrt=0 , ii , nmat ;

ENTRY("CW_read_affine_warp") ;

   if( cp == NULL || *cp == '\0' ) RETURN(mvv) ;
   ocp = cp ;

#if 0
INFO_message("Enter CW_read_affine_warp( %s )",cp) ;
#endif

   if( strncasecmp(cp,"INV(",4) == 0 ){                 /* set inversion flag */
     cp += 4 ; do_inv = 1 ;
   } else if( strncasecmp(cp,"INVERT(",7) == 0 ){
     cp += 7 ; do_inv = 1 ;
   } else if( strncasecmp(cp,"INVERSE(",8) == 0 ){
     cp += 8 ; do_inv = 1 ;
   } else if( strncasecmp(cp,"SQRT(",5) == 0 ){        /* set squareroot flag */
     cp += 5 ; do_sqrt = 1 ;
   } else if( strncasecmp(cp,"SQRTINV(",8) == 0 || strncasecmp(cp,"INVSQRT(",8) == 0 ){
     cp += 8 ; do_inv = do_sqrt = 1 ;                       /* set both flags */
   }
   wp = strdup(cp) ; ii = strlen(wp) ;
   if( ii < 4 ){
     ERROR_message("input filename '%s' to CW_read_affine_warp is too short :-((") ;
     free(wp) ; RETURN(mvv) ;
   }
   if( wp[ii-1] == ')' ) wp[ii-1] = '\0' ;

   qim = mri_read_1D(wp) ;

   /* bad data? */

   if( qim == NULL || qim->nvox < 12 ){
     ERROR_message("Cannot read affine warp from file '%s'",wp); free(wp); RETURN(mvv);
   }

   if( qim->nx == 3 && qim->ny == 4 ){        /* single matrix in 3x4 'Xat.1D' format? */
     MRI_IMAGE *tim = mri_rowmajorize_1D(qim); mri_free(qim); qim = tim; /* make it 12x1 */
   } else {
     MRI_IMAGE *tim ;
     if( qim->ny != 12 ){
       ERROR_message("Affine warp file '%s': have %d, not 12, values per row",wp,qim->ny) ;
       free(wp) ; RETURN(mvv) ;
     }
     tim = mri_transpose(qim) ; mri_free(qim) ; qim = tim ;  /* flip to column major order */
   }

   if( qim->nx != 12 ){
     ERROR_message("CW_read_affine_warp: nx == %d != 12 (this message should never happen!)",qim->nx) ;
     free(wp); RETURN(mvv);
   }

   /* at this point, qim->nx = 12, and qim->ny = number of matrices */

   nmat = qim->ny ;
   mvv  = (mat44_vec *)malloc(sizeof(mat44_vec)) ;
   mvv->nmar = nmat ;
   mvv->mar  = (mat44 *)malloc(sizeof(mat44)*nmat) ;

   qar = MRI_FLOAT_PTR(qim) ;
   for( ii=0 ; ii < nmat ; ii++ ){
     tar = qar + ii*12 ;
     LOAD_MAT44(mmm,tar[0],tar[1],tar[2] ,tar[3],
                    tar[4],tar[5],tar[6] ,tar[7],
                    tar[8],tar[9],tar[10],tar[11]) ;

     if( do_inv  ){ mat44 imm=MAT44_INV(mmm)     ; mmm=imm; } /* invert */
     if( do_sqrt ){ mat44 smm=THD_mat44_sqrt(mmm); mmm=smm; } /* sqrt */

     mvv->mar[ii] = mmm ;
   }

#if 0
ININFO_message("output Matrix[%d]",nmat) ;
#endif

   mri_free(qim) ; free(wp) ;

   NI_strncpy(mvv->fname,ocp,128) ; RETURN(mvv) ;
}

/*----------------------------------------------------------------------------*/

static mat44 CW_read_affine_warp_OLD( char *cp )
{
   mat44_vec *mvv ; mat44 mmm ;

   mvv = CW_read_affine_warp(cp) ;

   if( mvv == NULL || mvv->nmar <= 0 || mvv->mar == NULL ){  /* bad bad bad */
     ERROR_message("Failed to read affine warp from file '%s' -- using identity matrix",cp) ;
     LOAD_IDENT_MAT44(mmm) ; return mmm ;
   } else if( mvv->nmar > 1 ){
     WARNING_message("Affine warp file '%s' has %d matrices: using only first one",cp,mvv->nmar) ;
   }

   mmm = mvv->mar[0] ; DESTROY_mat44_vec(mvv) ;
   return mmm ;
}

/*----------------------------------------------------------------------------*/

THD_3dim_dataset * CW_read_dataset_warp( char *cp )
{
   char *wp ; int do_inv=0 , do_sqrt=0 , do_empty=0 , ii ;
   THD_3dim_dataset *dset, *eset=NULL ;

ENTRY("CW_read_dataset_warp") ;

   /* Does the user order some weird processing on the warp?? */

   if( strncasecmp(cp,"INV(",4) == 0 ){                 /* set inversion flag */
     cp += 4 ; do_inv = 1 ;
   } else if( strncasecmp(cp,"INVERT(",7) == 0 ){
     cp += 7 ; do_inv = 1 ;
   } else if( strncasecmp(cp,"INVERSE(",8) == 0 ){
     cp += 8 ; do_inv = 1 ;
   } else if( strncasecmp(cp,"SQRT(",5) == 0 ){        /* set squareroot flag */
     cp += 5 ; do_sqrt = 1 ;
   } else if( strncasecmp(cp,"SQRTINV(",8) == 0 || strncasecmp(cp,"INVSQRT(",8) == 0 ){
     cp += 8 ; do_inv = do_sqrt = 1 ;                       /* set both flags */
   } else if( strncasecmp(cp,"IDENT(",6) == 0 ){
     cp += 6 ; do_empty = 1 ;                             /* this one is easy */
   }
   wp = strdup(cp) ; ii = strlen(wp) ;
   if( ii < 4 ){
     ERROR_message("input string to CW_read_dataset_warp is too short :-((") ;
     free(wp) ; RETURN(NULL) ;
   }
   if( wp[ii-1] == ')' ) wp[ii-1] = '\0' ;

   /* Check for special case of uni-directional warp from 1 sub-brick:
        RL: or LR: for x-direction only
        AP: or PA: for y-direction only
        IS: or SI: for z-direction only
        VEC:a,b,c: for arbitrary direction
        Can also have have a scale factor for the dataset, as in
          RL:0.7:datasetname   or   VEC:1,1,1:-0.5:datasetname
        Note that vector (a,b,c) will be L2-normalized to a unit direction */

   if( strncasecmp(wp,"RL:",3) == 0 || strncasecmp(wp,"LR:",3) == 0 ||
       strncasecmp(wp,"AP:",3) == 0 || strncasecmp(wp,"PA:",3) == 0 ||
       strncasecmp(wp,"IS:",3) == 0 || strncasecmp(wp,"SI:",3) == 0 ||
       strncasecmp(wp,"VEC:",4)== 0 || strncasecmp(wp,"UNI:",4)== 0   ){

     float vx=0.0f,vy=0.0f,vz=0.0f,vm=0.0f ;
     char *up=strchr(wp,':')+1 , *vp ;
     MRI_IMAGE *dim ; float *dar , *xar,*yar,*zar ; int nvox ;

     /* set unit vector for direction of warp displacements in 3D */

     switch( toupper(*wp) ){
       case 'R': case 'L':  vx = 1.0f ; vy = vz = 0.0f ; break ;
       case 'A': case 'P':  vy = 1.0f ; vx = vz = 0.0f ; break ;
       case 'I': case 'S':  vz = 1.0f ; vx = vy = 0.0f ; break ;

       case 'V': default:
         sscanf(up,"%f,%f,%f",&vx,&vy,&vz) ;
         vm = sqrtf(vx*vx+vy*vy+vz*vz) ;
         if( vm < 1.e-6f ){
           ERROR_message("warp '%s' :-) direction/factors unclear",wp) ;
           free(wp) ; RETURN(NULL) ;
         }
         if( toupper(*wp) == 'V' ){ vx /= vm ; vy /= vm ; vz /= vm ; }
         vp = strchr(up,':') ;
         if( vp == NULL || vp[1] == '\0' ){
           ERROR_message("warp '%s' :-) no dataset to read?",wp) ;
           free(wp) ; RETURN(NULL) ;
         }
         up = vp+1 ;
       break ;
     }

     /* check if there is a scale factor (need one more ':') */

     vp = strchr(up,':') ;
     if( vp != NULL && isnumeric(*up) ){
       float wfac = (float)strtod(up,NULL) ;
       if( wfac == 0.0f ){
         ERROR_message("uni-directional warp '%s' :-) scale factor = 0?",wp) ;
         free(wp) ; RETURN(NULL) ;
       }
       up = vp+1 ;
       vx *= wfac ; vy *= wfac ; vz *= wfac ;
     }

     /* now read 1-brick dataset and do surgery on it to create a 3-brick dataset */

     eset = THD_open_dataset(up) ; DSET_COPYOVER_REAL(eset) ;
     if( eset == NULL ){
       ERROR_message("Can't open dataset from file '%s'",up); free(wp); RETURN(NULL);
     }
     DSET_load(eset) ;
     if( !DSET_LOADED(eset) ){
       ERROR_message("Can't load dataset from file '%s'",up); free(wp); DSET_delete(eset); RETURN(NULL);
     }
     dim = THD_extract_float_brick(0,eset); dar = MRI_FLOAT_PTR(dim); DSET_unload(eset);
     nvox = dim->nvox ;
     xar = (float *)calloc(sizeof(float),nvox) ; /* bricks for output 3-brick dataset */
     yar = (float *)calloc(sizeof(float),nvox) ;
     zar = (float *)calloc(sizeof(float),nvox) ;
     dset = EDIT_empty_copy(eset) ;              /* create 'true' 3D warp dataset */
     EDIT_dset_items( dset ,
                        ADN_nvals , 3 ,
                        ADN_ntt   , 0 ,
                        ADN_datum_all , MRI_float ,
                      ADN_none ) ;
     EDIT_BRICK_FACTOR(dset,0,0.0) ; EDIT_substitute_brick(dset,0,MRI_float,xar) ;
     EDIT_BRICK_FACTOR(dset,1,0.0) ; EDIT_substitute_brick(dset,1,MRI_float,yar) ;
     EDIT_BRICK_FACTOR(dset,2,0.0) ; EDIT_substitute_brick(dset,2,MRI_float,zar) ;
     for( ii=0 ; ii < nvox ; ii++ ){ /* scale displacements appropriately */
       xar[ii] = vx * dar[ii]; yar[ii] = vy * dar[ii]; zar[ii] = vz * dar[ii];
     }
     mri_free(dim) ; DSET_delete(eset) ; eset = NULL ;

   } else if( strncasecmp(wp,"FAC:",4) == 0 ){ /* special case: 3D scaling */

     /* FAC:a,b,c:datasetname
        = scale 3-brick input dataset, each direction separately */

     MRI_IMAGE *aim,*bim,*cim ; float *aar,*bar,*car , *xar,*yar,*zar ; int nvox ;
     float xfac=0.0f,yfac=0.0f,zfac=0.0f ;
     char *up=strchr(wp,':')+1 , *vp ;

     sscanf(up,"%f,%f,%f",&xfac,&yfac,&zfac) ;
     if( fabsf(xfac)+fabsf(yfac)+fabsf(zfac) < 0.001f )
       WARNING_message("warp '%s': factors are small",wp) ;
     vp = strchr(up,':') ;
     if( vp == NULL ){
       ERROR_message("warp '%s': no dataset to read?",wp) ;
       free(wp) ; RETURN(NULL) ;
     }
     eset = THD_open_dataset(vp+1) ; DSET_COPYOVER_REAL(eset) ;
     if( eset == NULL ){
       ERROR_message("Can't open dataset from file '%s'",wp); free(wp); RETURN(NULL);
     } else if( DSET_NVALS(eset) < 3 ){
       ERROR_message("warp '%s': does not have 3 sub-bricks",wp) ;
       DSET_delete(eset) ; free(wp) ; RETURN(NULL) ;
     }
     dset = EDIT_empty_copy(eset) ;
     EDIT_dset_items( dset ,
                        ADN_nvals , 3 ,
                        ADN_ntt   , 0 ,
                        ADN_datum_all , MRI_float ,
                        ADN_none ) ;
     EDIT_BRICK_FACTOR(dset,0,0.0) ; EDIT_substitute_brick(dset,0,MRI_float,NULL) ;
     EDIT_BRICK_FACTOR(dset,1,0.0) ; EDIT_substitute_brick(dset,1,MRI_float,NULL) ;
     EDIT_BRICK_FACTOR(dset,2,0.0) ; EDIT_substitute_brick(dset,2,MRI_float,NULL) ;
     aim = THD_extract_float_brick(0,eset); aar = MRI_FLOAT_PTR(aim);
     bim = THD_extract_float_brick(1,eset); bar = MRI_FLOAT_PTR(bim);
     cim = THD_extract_float_brick(2,eset); car = MRI_FLOAT_PTR(cim);
     DSET_delete(eset); eset = NULL;
     xar = DSET_ARRAY(dset,0); yar = DSET_ARRAY(dset,1); zar = DSET_ARRAY(dset,2);
     nvox = aim->nvox ;
     for( ii=0 ; ii < nvox ; ii++ ){
       xar[ii] = xfac*aar[ii] ; yar[ii] = yfac*bar[ii] ; zar[ii] = zfac*car[ii] ;
     }
     mri_free(cim); mri_free(bim); mri_free(aim);

   } else {  /*--- standard 3-brick warp (almost always the case) ---*/

     dset = THD_open_dataset(wp) ; DSET_COPYOVER_REAL(dset) ;
     if( dset == NULL ){
       ERROR_message("Can't open dataset from file '%s'",wp); free(wp); RETURN(NULL);
     }

     if( do_empty ){    /* replace dset with a 0-filled copy */
       eset = EDIT_empty_copy(dset) ;
       EDIT_dset_items( eset ,
                          ADN_nvals , 3 ,
                          ADN_ntt   , 0 ,
                          ADN_datum_all , MRI_float ,
                        ADN_none ) ;
       EDIT_BRICK_FACTOR(eset,0,0.0) ; EDIT_substitute_brick(eset,0,MRI_float,NULL) ;
       EDIT_BRICK_FACTOR(eset,1,0.0) ; EDIT_substitute_brick(eset,1,MRI_float,NULL) ;
       EDIT_BRICK_FACTOR(eset,2,0.0) ; EDIT_substitute_brick(eset,2,MRI_float,NULL) ;
       DSET_delete(dset) ; dset = eset ; eset = NULL ;

     } else if( DSET_NVALS(dset) < 3 ){  /* bad bad Leroy Brown */
       ERROR_message("warp '%s': does not have 3 sub-bricks",wp) ;
       DSET_delete(dset) ; free(wp) ; RETURN(NULL) ;
     }

     DSET_load(dset) ;

     if( !DSET_LOADED(dset) ){
       ERROR_message("warp '%s': cannot load data!",wp) ;
       DSET_delete(dset) ; free(wp) ; RETURN(NULL) ;
     }
     if( DSET_BRICK_TYPE(dset,0) != MRI_float ){
       ERROR_message("warp '%s': is not stored as floats",wp) ;
       DSET_delete(dset) ; free(wp) ; RETURN(NULL) ;
     }

   }

   /**--- do any functional processing of the warp ---**/

   if( do_sqrt ){        /* user wants SQRT or SQRTINV */
#ifndef USE_SQRTPAIR
     ERROR_exit("mri_nwarp compiled without dataset SQRT :-(") ;
#else
     eset = THD_nwarp_sqrt(dset,do_inv) ;
     DSET_delete(dset) ; dset = eset ; eset = NULL ;
#endif
   } else if( do_inv ){  /* user wants INV */
     eset = THD_nwarp_invert(dset) ;
     DSET_delete(dset) ; dset = eset ; eset = NULL ;
   }

   free(wp) ; RETURN(dset) ;
}

/*----------------------------------------------------------------------------*/
/* Load one warp into the nn-th static data, inverting it if necessary (etc.) */

static void CW_load_one_warp( int nn , char *cp )
{
   THD_3dim_dataset *dset ; IndexWarp3D *AA ;

ENTRY("CW_load_one_warp") ;

   if( nn <= 0 || nn > CW_NMAX || cp == NULL || *cp == '\0' ){
     ERROR_message("bad inputs to CW_load_one_warp: nn=%d cp=%s",nn,cp) ; EXRETURN ;
   }

   if( nn > CW_nwtop ) CW_nwtop = nn ;  /* CW_nwtop = largest index thus far */

   /* Deal with input of a matrix (from a .1D or .txt file) */

   if( strcasestr(cp,".1D") != NULL || strcasestr(cp,".txt") != NULL ){
     mat44_vec *mvv ; mat44 mmm ;
     mvv = CW_read_affine_warp(cp) ;  /* does INV() etc. */
     if( mvv == NULL || mvv->nmar <= 0 || mvv->mar == NULL ){  /* bad bad bad */
       ERROR_message("Failed to read affine warp from file '%s'",cp) ; EXRETURN ;
     } else if( mvv->nmar == 1 ){                            /* only 1 matrix */
#if 0
INFO_message("CW_load_one_warp: single matrix") ;
#endif
       CW_awarp[nn-1] = (mat44 *)malloc(sizeof(mat44)) ;
       mmm = mvv->mar[0] ;
       AAmemcpy(CW_awarp[nn-1],&mmm,sizeof(mat44)) ;
       CW_dxal += fabsf(mmm.m[0][3]) ;  /* accumulate shifts */
       CW_dyal += fabsf(mmm.m[1][3]) ;  /* for padding later */
       CW_dzal += fabsf(mmm.m[2][3]) ;
     } else {                                            /* multiple matrices */
       float dx,dy,dz , dxm=0.0f,dym=0.0f,dzm=0.0f ; int ii ;
#if 0
INFO_message("CW_load_one_warp: matrix vector") ;
#endif
       CW_vwarp[nn-1] = mvv ;
       NI_strncpy(CW_vwarp[nn-1]->fname,cp,128) ;
       for( ii=0 ; ii < mvv->nmar ; ii++ ){
         mmm = mvv->mar[ii] ;
         dx  = fabsf(mmm.m[0][3]) ; if( dx > dxm ) dxm = dx ;
         dy  = fabsf(mmm.m[1][3]) ; if( dy > dym ) dym = dy ;
         dz  = fabsf(mmm.m[2][3]) ; if( dz > dzm ) dzm = dz ;
       }
       CW_dxal += dxm ; CW_dyal += dym ; CW_dzal += dzm ;
     }
     EXRETURN ;
   }

   /*--- Below here is a dataset (nonlinear) warp ---*/

   dset = CW_read_dataset_warp(cp) ;  /* read it */
   if( dset == NULL ){
     ERROR_message("Failed to read 3D warp from '%s'",cp) ; EXRETURN ;
   }

   AA = IW3D_from_dataset(dset,0,0) ;  /* convert to index warp */
   if( AA == NULL ){
     ERROR_message("Can't make warp from dataset '%s'",cp); EXRETURN;
   }

   /* first dataset ==> set geometry globals */

   if( CW_geomstring == NULL ){
     CW_geomstring = strdup(AA->geomstring) ; CW_saved_geomstring = strdup(CW_geomstring) ;
     CW_nx = AA->nx; CW_ny = AA->ny; CW_nz = AA->nz; CW_cmat = AA->cmat; CW_imat = AA->imat;
     CW_dx = fabsf(DSET_DX(dset)); CW_dy = fabsf(DSET_DY(dset)); CW_dz = fabsf(DSET_DZ(dset));

   /* later dataset ==> check it against the first */

   } else if( EDIT_geometry_string_diff(CW_geomstring,AA->geomstring) > 0.01f ){
     ERROR_message("warp from dataset '%s' doesn't match earlier input geometry",cp) ;
     EXRETURN ;
   }

   /* save first dataset as template */

   if( CW_inset == NULL ){
     DSET_unload(dset) ; CW_inset = dset ;
   } else {
     DSET_delete(dset) ;
   }

   IW3D_load_external_slopes(AA) ;

   /* push this warp onto the stack we are creating */

   CW_iwarp[nn-1] = AA ; EXRETURN ;
}

/*----------------------------------------------------------------------------*/
/* Read in a string like
     "warp1 warp2 warp3"
   and return the dataset that instantiates warp3(warp2(warp1(x))).
   This is how the '-nwarp' option in 3dNwarp*.c is implemented.
*//*--------------------------------------------------------------------------*/

THD_3dim_dataset * IW3D_read_catenated_warp( char *cstr )
{
   char *prefix = "NwarpCat" ;
   mat44        wmat      , tmat , smat , qmat ;
   IndexWarp3D *warp=NULL , *tarp=NULL ;
   THD_3dim_dataset *oset ;
   NI_str_array *csar ; int ii ;

ENTRY("IW3D_read_catenated_warp") ;

   if( cstr == NULL || *cstr == '\0' ) RETURN(NULL) ;

   CW_clear_data() ;

   csar = NI_decode_string_list(cstr,";") ;
   if( csar == NULL || csar->num < 1 ) RETURN(NULL) ;

   /*-- simple case of a single dataset input --*/

   if( csar->num == 1 && strchr(csar->str[0],'(') == NULL && strchr(csar->str[0],':') == NULL ){
     oset = THD_open_dataset(csar->str[0]) ; DSET_COPYOVER_REAL(oset) ;
     if( oset == NULL ){
       ERROR_message("Can't open warp dataset '%s'",csar->str[0]) ;
       NI_delete_str_array(csar) ; RETURN(NULL) ;
     }
     if( DSET_NVALS(oset) < 3 ){
       ERROR_message("Warp dataset '%s' has < 3 sub-bricks",csar->str[0]) ;
       NI_delete_str_array(csar) ; DSET_delete(oset) ; RETURN(NULL) ;
     }
     DSET_load(oset) ;
     if( !DSET_LOADED(oset) ){
       ERROR_message("Warp dataset '%s' can't be loaded into memory",csar->str[0]) ;
       NI_delete_str_array(csar) ; DSET_delete(oset) ; RETURN(NULL) ;
     }
     RETURN(oset) ;
   }

   /*-- multiple input datasets (or INV operations, etc.) --*/

   for( ii=0 ; ii < csar->num ; ii++ )           /* read them all in */
     CW_load_one_warp( ii+1 , csar->str[ii] ) ;

   NI_delete_str_array(csar) ;

   if( CW_geomstring == NULL ){ /* didn't get a real warp to use */
     ERROR_message("Can't compute nonlinear warp from string '%s'",cstr) ;
     CW_clear_data() ; RETURN(NULL) ;
   }

   /*-- pad the nonlinear warps present [22 Aug 2014] --*/

   if( CW_dxal > 0.0f || CW_dyal > 0.0f || CW_dzal > 0.0f ){
     float dm , xx,yy,zz ; int pp ;
     dm = MIN(CW_dx,CW_dy); dm = MIN(CW_dz,dm) ;
     xx = CW_dxal/dm ; yy = CW_dyal/dm ; zz = CW_dzal/dm ;
     dm = MAX(xx,yy) ; dm = MAX(dm,zz) ;
     pp = (int)rintf(1.1111f*dm) ; CW_saved_expad = MAX(pp,16) ;
   } else {
     CW_saved_expad = 16 ;
   }
   if( CW_no_expad ) CW_saved_expad = 0 ;
   CW_saved_expad += CW_extra_pad ;
#if 0
   { int qq = AFNI_numenv("CW_EXPAD") ; if( qq >= 0 ) CW_saved_expad = qq ; }
#endif
   if( CW_saved_expad > 0 ){
     IndexWarp3D *EE ; THD_3dim_dataset *QQ ; int first=1 ;
     QQ = THD_zeropad( CW_inset ,
                       CW_saved_expad, CW_saved_expad, CW_saved_expad,
                       CW_saved_expad, CW_saved_expad, CW_saved_expad,
                       "ZharkExtends" , ZPAD_IJK | ZPAD_EMPTY ) ;
     DSET_delete(CW_inset) ; CW_inset = QQ ;
     for( ii=0 ; ii < CW_nwtop ; ii++ ){
       if( CW_iwarp[ii] != NULL ){
         EE = IW3D_extend( CW_iwarp[ii] ,
                           CW_saved_expad, CW_saved_expad, CW_saved_expad,
                           CW_saved_expad, CW_saved_expad, CW_saved_expad, 0 ) ;
         IW3D_destroy(CW_iwarp[ii]) ; CW_iwarp[ii] = EE ;
         if( first ){
           CW_cmat = EE->cmat; CW_imat = EE->imat; first = 0;
         }
       }
     }
   }

   /*-- cat (compose) them --*/

   LOAD_IDENT_MAT44(wmat) ;

   for( ii=0 ; ii < CW_nwtop ; ii++ ){

     if( CW_awarp[ii] != NULL ){  /* matrix to apply */

       qmat = *(CW_awarp[ii]) ;          /* convert from xyz warp to ijk warp */
       tmat = MAT44_MUL(qmat,CW_cmat) ;
       smat = MAT44_MUL(CW_imat,tmat) ;

       if( warp == NULL ){                         /* thus far, only matrices */
         qmat = MAT44_MUL(smat,wmat) ; wmat = qmat ;
       } else {                             /* apply matrix to nonlinear warp */
         tarp = IW3D_compose_w1m2(warp,smat,CW_interp) ;
         IW3D_destroy(warp) ; warp = tarp ;
       }

       free(CW_awarp[ii]) ; CW_awarp[ii] = NULL ;

     } else if( CW_iwarp[ii] != NULL ){            /* nonlinear warp to apply */

#if 0
       if( CW_iwfac[ii] != 1.0f ) IW3D_scale( CW_iwarp[ii] , CW_iwfac[ii] ) ;
#endif

       if( warp == NULL ){             /* create nonlinear warp at this point */
         if( ii == 0 ){   /* first one ==> don't compose with identity matrix */
           warp = IW3D_copy(CW_iwarp[ii],1.0f) ;
         } else {                             /* compose with previous matrix */
           warp = IW3D_compose_m1w2(wmat,CW_iwarp[ii],CW_interp) ;
         }
       } else {           /* already have nonlinear warp, apply new one to it */
         tarp = IW3D_compose(warp,CW_iwarp[ii],CW_interp) ;
         IW3D_destroy(warp) ; warp = tarp ;
       }

       IW3D_destroy(CW_iwarp[ii]) ; CW_iwarp[ii] = NULL ;

     } else if( CW_vwarp[ii] != NULL ){  /* bad user */

       WARNING_message(
           "Multi-line matrix file '%s' input when only one matrix is allowed!",
           CW_vwarp[ii]->fname ) ;

       qmat = CW_vwarp[ii]->mar[0] ;   /* convert from xyz warp to ijk warp */
       tmat = MAT44_MUL(qmat,CW_cmat) ;
       smat = MAT44_MUL(CW_imat,tmat) ;

       if( warp == NULL ){                         /* thus far, only matrices */
         qmat = MAT44_MUL(smat,wmat) ; wmat = qmat ;
       } else {                             /* apply matrix to nonlinear warp */
         tarp = IW3D_compose_w1m2(warp,smat,CW_interp) ;
         IW3D_destroy(warp) ; warp = tarp ;
       }

       DESTROY_mat44_vec(CW_vwarp[ii]) ; CW_vwarp[ii] = NULL ;

     }

   } /* end of loop over input transformations */

   /*--- create output dataset ---*/

   if( warp == NULL ){
     ERROR_message("This message should never appear!!") ;
     CW_clear_data() ; RETURN(NULL) ;
   }

   IW3D_adopt_dataset( warp , CW_inset ) ;  /* this is why template was saved */
   oset = IW3D_to_dataset( warp , prefix ) ;

   IW3D_destroy(warp) ; CW_clear_data() ;

   RETURN(oset) ;
}

/*----------------------------------------------------------------------------*/
/* The following set of functions are for dealing with the Nwarp_catlist      */
/* struct, which is for use in 3dNwarpApply.  Here, there is a chain of       */
/* transformations -- warps (datasets) and matrices -- with the wrinkle       */
/* that there may be multiple matrices in one transformation slot:            */
/*   NetWarp[i] = Warp Matrix Matrix[i] Warp Matrix[i] ...                    */
/* where [i] means the i-th transformation (i is 'time')                      */
/*----------------------------------------------------------------------------*/

void IW3D_destroy_nwarp_catlist( Nwarp_catlist *nwc )
{
   int ii ;
   if( nwc == NULL ) return ;
   if( nwc->nwarp != NULL ){
     for( ii=0 ; ii < nwc->ncat ; ii++ )
       if( nwc->nwarp[ii] != NULL ) DSET_delete(nwc->nwarp[ii]) ;
     free(nwc->nwarp) ;
   }
   if( nwc->awarp != NULL ){
     for( ii=0 ; ii < nwc->ncat ; ii++ )
       if( nwc->awarp[ii] != NULL ) DESTROY_mat44_vec(nwc->awarp[ii]) ;
   }
   if( nwc->actual_geomstring != NULL ) free(nwc->actual_geomstring) ;
   if( nwc->master_geomstring != NULL ) free(nwc->master_geomstring) ;
   free(nwc) ; return ;
}

/*----------------------------------------------------------------------------*/
/* Set the 'master' geometry of the catlist.  Please note that the actual
   geometry of the warps inside will be padded from this.  If gs is NULL,
   then the function will make something up instead, which you don't want.
*//*--------------------------------------------------------------------------*/

void IW3D_set_geometry_nwarp_catlist( Nwarp_catlist *nwc , char *gsin )
{
   int ii , nw=0 , xpad,ypad,zpad , first=1 ;
   char  *gs , *gg , *hs ;
   float_triple delxyz ; float del ;
   THD_3dim_dataset *qset ;

ENTRY("IW3D_set_geometry_nwarp_catlist") ;

   if( nwc == NULL || nwc->ncat < 1 ) EXRETURN ; /* bad */

   /* count number of nonlinear warps */

   for( ii=0 ; ii < nwc->ncat ; ii++ )
     if( ISVALID_DSET(nwc->nwarp[ii]) ) nw++ ;
   if( nw == 0 ) EXRETURN ;                 /* also bad */

   /*--- must we make gs up from empty air? ---*/

   if( gsin == NULL ) gs = strdup("\0") ;
   else               gs = strdup(gsin) ;

   if( !ISVALID_GEOMETRY_STRING(gs) ){
     THD_3dim_dataset **nset=NULL ; char **gset=NULL ; int jj ;

     /* make pointers to nonlinear warp datasets and their geometry strings */

     nset = (THD_3dim_dataset **)malloc(sizeof(THD_3dim_dataset *)*nw) ;
     gset = (char             **)malloc(sizeof(char             *)*nw) ;
     for( ii=jj=0 ; ii < nwc->ncat ; ii++ ){
       if( ISVALID_DSET(nwc->nwarp[ii]) ){
         nset[jj]   = nwc->nwarp[ii] ;
         gset[jj++] = EDIT_get_geometry_string(nset[jj]) ;
       }
     }

     /* check for grid mismatch if have multiple warps */

     for( jj=1 ; jj < nw ; jj++ ){
       if( EDIT_geometry_string_diff(gset[0],gset[jj]) > 0.01f ) break ;
     }
     if( jj == nw ){ /* no mismatch ==> keep this box */
       free(gs) ; gs = gset[0] ;
       for( jj=1 ; jj < nw ; jj++ ) free(gset[jj]) ;
       free(gset) ; gset = NULL ;
     } else {        /* mismatch ==> build a big box */
       free(gs) ;
       gs = EDIT_geomstring_from_collection( nw , gset ) ;
       for( jj=0 ; jj < nw ; jj++ ) free(gset[jj]) ;
       free(gset) ; gset = NULL ;
     }
     free(nset) ;

   } /*-- end of creating gs from nothing --*/

   /*--- determine padding for gs from the internal shifts ---*/

   if( nwc->xshift > 0.0f || nwc->yshift > 0.0f || nwc->zshift > 0.0f || CW_extra_pad > 0 ){
     delxyz = EDIT_geometry_string_to_delxyz(gs) ;
     del    = MIN(delxyz.a,delxyz.b) ; del = MIN(del,delxyz.c) ;
     xpad   = (int)rint( nwc->xshift / del ) ;
     ypad   = (int)rint( nwc->yshift / del ) ;
     zpad   = (int)rint( nwc->zshift / del ) ;
     if( xpad < ypad ) xpad = ypad ;
     if( xpad < zpad ) xpad = zpad ;
     xpad += CW_extra_pad ;
     if( xpad > 0 ) hs = EDIT_geometry_string_pad(gs,xpad) ;
     else           hs = strdup(gs) ;
     nwc->xshift = nwc->yshift = nwc->zshift = 0.0f ;
     nwc->xpad = nwc->ypad = nwc->zpad = xpad ;
#ifdef DEBUG_CATLIST
if( verb_nww > 1 ) INFO_message("IW3D_set_geometry_nwarp_catlist: padding = %d",xpad) ;
#endif
   } else {
     hs = strdup(gs) ;
   }

   /*-- set the internal strings in the catlist --*/

   nwc->master_geomstring = gs ;  /* the one we were told to use */
   nwc->actual_geomstring = hs ;  /* the one we actually use */

#ifdef DEBUG_CATLIST
if( verb_nww > 1 ) ININFO_message("results: master_geomstring = %s  actual_geomstring = %s",gs,hs) ;
#endif

   /* Now regrid each dataset in the catlist (if needed) */

   for( ii=0 ; ii < nwc->ncat ; ii++ ){
     if( !ISVALID_DSET(nwc->nwarp[ii]) ) continue ;
     gg = EDIT_get_geometry_string(nwc->nwarp[ii]) ;
#ifdef DEBUG_CATLIST
if( verb_nww > 1 ) ININFO_message("-- warp #%d geometry = %s",ii,gg) ;
#endif
     if( EDIT_geometry_string_diff(hs,gg) > 0.01f ){
#ifdef DEBUG_CATLIST
if( verb_nww > 1 ) ININFO_message("   regridding warp #%d to geometry = %s",ii,hs) ;
#endif
       qset = THD_nwarp_regrid( nwc->nwarp[ii] , hs ) ;
       DSET_delete(nwc->nwarp[ii]) ;
       nwc->nwarp[ii] = qset ;
     }
#ifdef DEBUG_CATLIST
else if( verb_nww > 1 ) ININFO_message("   do not need to regrid warp #%d",ii) ;
#endif

     if( first && ISVALID_DSET(nwc->nwarp[ii]) ){
       nwc->actual_cmat = nwc->nwarp[ii]->daxes->ijk_to_dicom ;  /* 04 Dec 2014 */
       nwc->actual_imat = MAT44_INV(nwc->actual_cmat) ;          /* [oopsie] */
       first = 0 ;
#ifdef DEBUG_CATLIST
if( verb_nww > 1 ){
  DUMP_MAT44("nwarp_catlist actual_cmat",nwc->actual_cmat) ;
  DUMP_MAT44("nwarp_catlist actual_imat",nwc->actual_imat) ;
}
#endif
     }

   }

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/
/* Go over a Nwarp_catlist struct and collapse/reduce operations that
   aren't dependent on the 'time' index.
   Note that if there aren't any multi-matrix 'time dependent' transformations,
   this should reduce the result down to a single warp dataset.
*//*--------------------------------------------------------------------------*/

int IW3D_reduce_nwarp_catlist( Nwarp_catlist *nwc )
{
   int ii,jj , ndone=0 , totaldone=0 ;
   int doall=0 ;
   mat44 cmat , imat ;

ENTRY("IW3D_reduce_nwarp_catlist") ;

   if( nwc == NULL || nwc->ncat < 1 ) RETURN(totaldone) ;

   if( nwc->actual_geomstring == NULL )  /* has to be set to something */
     IW3D_set_geometry_nwarp_catlist( nwc , NULL ) ;  /* so set it now */

   cmat = nwc->actual_cmat ;  /* matrix to convert indexes to coords */
   imat = nwc->actual_imat ;  /* and vice-versa */

#ifdef DEBUG_CATLIST
if( verb_nww > 1 ){
   fprintf(stderr,"+++++ initial structure of Nwarp_catlist:") ;
   for( ii=0 ; ii < nwc->ncat ; ii++ ){
     if( NWC_nwarp(nwc,ii) != NULL ){
       fprintf(stderr," Nwarp ;") ;
     } else if( NWC_awarp(nwc,ii) != NULL ){
       fprintf(stderr," Matrix[%d] ;",nwc->awarp[ii]->nmar) ;
     } else {
       fprintf(stderr," (NULL) ;") ;
     }
   }
   fprintf(stderr,"\n") ;
}
#endif

   /* collapse ii and jj neighbors if possible;
      note that any single matrix will be collapsed;
      on the first pass (doall==0), only matrix-matrix reduction is done */

Try_It_Again_Dude:  /* target for loop back when ndone > 0 */

#ifdef DEBUG_CATLIST
if( verb_nww > 1 ) INFO_message("-- IW3D_reduce_nwarp_catlist -- start loop") ;
#endif

   for( ndone=ii=0 ; ii < nwc->ncat-1 ; ii=jj ){

     /* if this entry is empty (doubly NULL), skip it */

     if( NWC_null(nwc,ii) ){
#ifdef DEBUG_CATLIST
if( verb_nww > 1 ) ININFO_message("  nwc[%d] is doubly NULL",ii) ;
#endif
     jj = ii+1 ; continue ; }

     /* search for next non-empty neighbor above (jj) */
#ifdef DEBUG_CATLIST
if( verb_nww > 1 ) ININFO_message("  looking at nwc[%d]",ii) ;
#endif
     for( jj=ii+1 ; jj < nwc->ncat ; jj++ ) if( !NWC_null(nwc,jj) ) break ;
     if( jj == nwc->ncat ) break ;  /* nothing above, so we are done */

#ifdef DEBUG_CATLIST
if( verb_nww > 1 ) ININFO_message(".. #%d and #%d are neighbors",ii,jj) ;
#endif

     /* neighbors are both matrices (matrix vectors?) ==> multiply them */

     if( NWC_awarp(nwc,ii) != NULL && NWC_awarp(nwc,jj) != NULL ){
       mat44_vec *mvii=nwc->awarp[ii] , *mvjj=nwc->awarp[jj] , *mvnn ;
       mat44 mii , mjj , mkk ;
       int nii=mvii->nmar , njj=mvjj->nmar , ntop=MAX(nii,njj) , kk ;
#ifdef DEBUG_CATLIST
if( verb_nww > 1 ) ININFO_message(".. IW3D_reduce_nwarp_catlist: Matrix[%d]-Matrix[%d] ii=%d jj=%d",nii,njj,ii,jj) ;
#endif
       mvnn = (mat44_vec *)malloc(sizeof(mat44_vec)) ;
       mvnn->nmar = ntop ;
       mvnn->mar  = (mat44 *)malloc(sizeof(mat44)*ntop) ;
       for( kk=0 ; kk < ntop ; kk++ ){
         mii = M44V_mat(mvii,kk) ;
         mjj = M44V_mat(mvjj,kk) ;
         mkk = MAT44_MUL(mjj,mii) ; /* note that mjj pre-multiplies mii */
         mvnn->mar[kk] = mkk ;      /* substitute into output vwarp */
       }
       /* now, delete #ii and #jj, and substitute #jj with the new mvnn */
       DESTROY_mat44_vec(mvii) ; nwc->awarp[ii] = NULL ;
       DESTROY_mat44_vec(mvjj) ; nwc->awarp[jj] = mvnn ;
       nwc->nwarp[ii] = nwc->nwarp[jj] = NULL ;  /* nugatory */
       ndone++ ; continue ;
     }

     /* neighbors are both nonlinear 3D warps ==> compose them */

     if( doall && NWC_nwarp(nwc,ii) != NULL && NWC_nwarp(nwc,jj) != NULL ){
       THD_3dim_dataset *ids=nwc->nwarp[ii] , *jds=nwc->nwarp[jj] , *kds ;
       IndexWarp3D *iww , *jww , *kww ; char prefix[64] ;
#ifdef DEBUG_CATLIST
if( verb_nww > 1 ) ININFO_message(".. IW3D_reduce_nwarp_catlist: warp-warp ii=%d jj=%d",ii,jj) ;
if( verb_nww > 1 ) ININFO_message("   ii=%d: geometry = %s",ii,EDIT_get_geometry_string(ids)) ;
if( verb_nww > 1 ) ININFO_message("   jj=%d: geometry = %s",jj,EDIT_get_geometry_string(jds)) ;
#endif
       iww = IW3D_from_dataset(ids,0,0) ;
       jww = IW3D_from_dataset(jds,0,0) ;
       kww = IW3D_compose(iww,jww,CW_interp) ;
       IW3D_adopt_dataset(kww,ids) ; sprintf(prefix,"Nwarp#%02d",jj+1) ;
       kds = IW3D_to_dataset(kww,prefix) ; DSET_superlock(kds) ;
       IW3D_destroy(iww) ; IW3D_destroy(jww) ; IW3D_destroy(kww) ;
       DSET_delete(ids) ;
       DSET_delete(jds) ; nwc->nwarp[jj] = kds ;
       nwc->awarp[ii] = nwc->awarp[jj] = NULL ; nwc->nwarp[ii] = NULL ;
       ndone++ ; continue ;
     }

     /* neighbors are single matrix and nonlinear warp (in that order) */

     if( doall && NWC_awarp(nwc,ii) != NULL && NWC_nwarp(nwc,jj) != NULL ){
       mat44_vec *mvii=nwc->awarp[ii] ;
       THD_3dim_dataset *jds=nwc->nwarp[jj] , *kds ;
       IndexWarp3D *jww , *kww ; char prefix[64] ;
       mat44 mii , qmat,tmat ;
       if( mvii->nmar == 1 ){
#ifdef DEBUG_CATLIST
if( verb_nww > 1 ) ININFO_message(".. IW3D_reduce_nwarp_catlist: Matrix[1]-warp ii=%d jj=%d",ii,jj) ;
#endif
         qmat = mvii->mar[0] ; tmat = MAT44_MUL(qmat,cmat) ; mii = MAT44_MUL(imat,tmat) ;
         jww = IW3D_from_dataset(jds,0,0) ;
         kww = IW3D_compose_m1w2(mii,jww,CW_interp) ;
         IW3D_adopt_dataset(kww,jds) ; sprintf(prefix,"Nwarp#%02d",jj+1) ;
         kds = IW3D_to_dataset(kww,prefix) ; DSET_superlock(kds) ;
         DESTROY_mat44_vec(mvii) ; IW3D_destroy(kww) ; DSET_delete(jds) ;
         nwc->nwarp[jj] = kds ;
         nwc->awarp[ii] = nwc->awarp[jj] = NULL ; nwc->nwarp[ii] = NULL ;
         ndone++ ;
       }
#ifdef DEBUG_CATLIST
else if( verb_nww > 1 ) ININFO_message(".. IW3D_reduce_nwarp_catlist: Matrix[%d]-warp ii=%d jj=%d -- skipped",mvii->nmar,ii,jj) ;
#endif
       continue ;
     }

     /* neighbors are nonlinear warp and single matrix (in that order) */

     if( doall && NWC_nwarp(nwc,ii) != NULL && NWC_awarp(nwc,jj) != NULL ){
       mat44_vec *mvjj=nwc->awarp[jj] ;
       THD_3dim_dataset *ids=nwc->nwarp[ii] , *kds ;
       IndexWarp3D *iww , *kww ; char prefix[64] ;
       mat44 mjj , qmat,tmat ;
       if( mvjj->nmar == 1 ){
#ifdef DEBUG_CATLIST
if( verb_nww > 1 ) ININFO_message("IW3D_reduce_nwarp_catlist: warp-Matrix[1] ii=%d jj=%d",ii,jj) ;
#endif
         qmat = mvjj->mar[0] ; tmat = MAT44_MUL(qmat,cmat) ; mjj = MAT44_MUL(imat,tmat) ;
         iww = IW3D_from_dataset(ids,0,0) ;
         kww = IW3D_compose_w1m2(iww,mjj,CW_interp) ;
         IW3D_adopt_dataset(kww,ids) ; sprintf(prefix,"Nwarp#%02d",jj+1) ;
         kds = IW3D_to_dataset(kww,prefix) ; DSET_superlock(kds) ;
         DESTROY_mat44_vec(mvjj) ; IW3D_destroy(kww) ; DSET_delete(ids) ;
         nwc->nwarp[jj] = kds ;
         nwc->awarp[ii] = nwc->awarp[jj] = NULL ; nwc->nwarp[ii] = NULL ;
         ndone++ ;
       }
#ifdef DEBUG_CATLIST
else if( verb_nww > 1 ) ININFO_message("IW3D_reduce_nwarp_catlist: warp-Matrix[%d] ii=%d jj=%d -- skipped",mvjj->nmar,ii,jj) ;
#endif
       continue ;
     }

     /* no other case on which to operate */
   }

#ifdef DEBUG_CATLIST
if( verb_nww > 1 ) ININFO_message("-- loop reduction operation count = %d",ndone) ;
#endif
   totaldone += ndone ;
   if( ndone > 0 || !doall ){ doall = 1 ; goto Try_It_Again_Dude ; }

   /*--- at this point, go thru and eliminate the doubly NULL entries ---*/

   for( ii=0 ; ii < nwc->ncat ; ii++ ){
     if( ! NWC_null(nwc,ii) ) continue ;  /* not doubly NULL */

     /* move upper entries down by one */

     for( jj=ii+1 ; jj < nwc->ncat ; jj++ ){
       if( nwc->awarp != NULL ) nwc->awarp[jj-1] = nwc->awarp[jj] ;
       if( nwc->nwarp != NULL ) nwc->nwarp[jj-1] = nwc->nwarp[jj] ;
     }

     nwc->ncat-- ;  /* one less entry now */
   }

   /*--- if have only 1 warp left, and inversion is indicated, do it now ---*/

   if( nwc->ncat == 1 && (nwc->flags & NWC_INVERT_MASK) ){
     THD_3dim_dataset *pset = NWC_nwarp(nwc,0) ;
     THD_3dim_dataset *qset = THD_nwarp_invert(pset) ;
     DSET_delete(pset) ; nwc->nwarp[0] = qset ; DSET_superlock(qset) ;
     nwc->flags &= ~(int)NWC_INVERT_MASK ;  /* turn off invert flag */
     totaldone++ ;
   }

   /** More general way to deal with inversion would be to
        (a) reverse the order of all the warps, and
        (b) invert them all (including multi-line matrices)
       This would save having to do inversions in IW3D_from_nwarp_catlist **/

#ifdef DEBUG_CATLIST
if( verb_nww > 1 ){
   fprintf(stderr,"+++++ final structure of Nwarp_catlist:") ;
   for( ii=0 ; ii < nwc->ncat ; ii++ ){
     if( NWC_nwarp(nwc,ii) != NULL ){
       fprintf(stderr," Nwarp ;") ;
     } else if( NWC_awarp(nwc,ii) != NULL ){
       fprintf(stderr," Matrix[%d] ;",nwc->awarp[ii]->nmar) ;
     } else {
       fprintf(stderr," (NULL) ;") ;
     }
   }
   fprintf(stderr,"\n") ;
}
#endif

   RETURN(totaldone) ;
}

/*----------------------------------------------------------------------------*/
/* Return the vind-th 'time' warp from the list. */

THD_3dim_dataset * IW3D_from_nwarp_catlist( Nwarp_catlist *nwc , int vind )
{
   int ii , do_regrid=0 ;
   char *prefix = "NwarpFromCatlist" ;
   mat44        wmat      , tmat , smat , qmat , cmat,imat ;
   IndexWarp3D *warp=NULL , *tarp=NULL , *qarp=NULL ;
   THD_3dim_dataset *oset , *iset=NULL , *qset=NULL ;

ENTRY("IW3D_from_nwarp_catlist") ;

   if( nwc == NULL ) RETURN(NULL) ;
   if( vind < 0 ) vind = 0 ;  /* stupid user */

   /* a single transformation ==> should be easy */

   if( nwc->ncat == 1 ){       /* return a copy, so it can be deleted */
     oset = NWC_nwarp(nwc,0) ;
     if( nwc->flags & NWC_INVERT_MASK ) qset = THD_nwarp_invert(oset) ; /* not possible */
     else                               qset = EDIT_full_copy(oset,"Copy_Of_Nwarp") ;
     DSET_superlock(qset) ; RETURN(qset) ;
   }

   /* otherwise, need to cat the list of transformations */
   /* start by initializing current state of cumulative warp */

   LOAD_IDENT_MAT44(wmat) ; warp = NULL ;
   cmat = nwc->actual_cmat ;  /* matrix to convert indexes to coords */
   imat = nwc->actual_imat ;  /* and vice-versa */

#ifdef DEBUG_CATLIST
if( verb_nww > 1 ) fprintf(stderr,"{nwarp_catlist[%d]",vind) ;
#endif

   for( ii=0 ; ii < nwc->ncat ; ii++ ){  /* warp catenation loop */

     if( NWC_awarp(nwc,ii) != NULL ){  /* matrix to apply */
       mat44_vec *mvii=nwc->awarp[ii] ;
       qmat = M44V_mat(mvii,vind) ;            /* pick out the vind-th matrix */
       tmat = MAT44_MUL(qmat,cmat) ;
       smat = MAT44_MUL(imat,tmat) ;     /* convert from xyz warp to ijk warp */
if( verb_nww > 2 ){
  float a11,a12,a13,a14,a21,a22,a23,a24,a31,a32,a33,a34 ;
  UNLOAD_MAT44(smat,a11,a12,a13,a14,a21,a22,a23,a24,a31,a32,a33,a34) ;
  fprintf(stderr," [new mat=%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f]",a11,a12,a13,a14,a21,a22,a23,a24,a31,a32,a33,a34);
}

       if( warp == NULL ){                     /* thus far, only matrix warps */
#ifdef DEBUG_CATLIST
if( verb_nww > 1 ) fprintf(stderr," [matrix*matrix]") ;
#endif
         qmat = MAT44_MUL(smat,wmat) ; wmat = qmat ;  /* so multiply matrices */

       } else {                    /* instead, apply matrix to nonlinear warp */
#ifdef DEBUG_CATLIST
if( verb_nww > 1 ) fprintf(stderr," [warp*matrix]") ;
#endif
         tarp = IW3D_compose_w1m2(warp,smat,CW_interp) ;
         IW3D_destroy(warp) ; warp = tarp ;
       }

     } else if( NWC_nwarp(nwc,ii) != NULL ){       /* nonlinear warp to apply */

       qarp = IW3D_from_dataset(nwc->nwarp[ii],0,0) ;
       if( iset == NULL ) iset = nwc->nwarp[ii] ;

       if( warp == NULL ){             /* create nonlinear warp at this point */
         if( ISIDENT_MAT44(wmat) ){     /* don't compose with identity matrix */
#ifdef DEBUG_CATLIST
if( verb_nww > 1 ) fprintf(stderr," [warp]") ;
#endif
           warp = qarp ; qarp = NULL ;
         } else {                             /* compose with previous matrix */
#ifdef DEBUG_CATLIST
if( verb_nww > 1 ) fprintf(stderr," [matrix*warp]") ;
#endif
           warp = IW3D_compose_m1w2(wmat,qarp,CW_interp) ;
         }
       } else {           /* already have nonlinear warp, apply new one to it */
#ifdef DEBUG_CATLIST
if( verb_nww > 1 ) fprintf(stderr," [warp*warp]") ;
#endif
         tarp = IW3D_compose(warp,qarp,CW_interp) ;
         IW3D_destroy(warp) ; warp = tarp ;
       }

       IW3D_destroy(qarp) ;

     } /* end of processing nonlinear warp */

     /* a null entry (which could have been reduced away) is just skipped */
#ifdef DEBUG_CATLIST
else if( verb_nww > 1 ) fprintf(stderr," [NULL entry?]") ;
#endif

   }  /* end of loop over input warps */

#ifdef DEBUG_CATLIST
if( verb_nww > 1 ) fprintf(stderr,"}") ;
#endif

   /*--- create output dataset ---*/

   if( warp == NULL ){
     ERROR_message("IW3D_from_nwarp_catlist: this message should never appear!!") ;
     RETURN(NULL) ;
   }

   IW3D_adopt_dataset( warp , iset ) ;
   oset = IW3D_to_dataset( warp , prefix ) ;
   IW3D_destroy(warp) ;
   if( nwc->flags & NWC_INVERT_MASK ){
     qset = THD_nwarp_invert(oset) ;
     DSET_delete(qset) ; oset = qset ;
   }

   DSET_superlock(oset) ; RETURN(oset) ;
}

/*----------------------------------------------------------------------------*/
/* Construct an Nwarp_catlist from a list of strings (filename). */

Nwarp_catlist * IW3D_read_nwarp_catlist( char *cstr )
{
   char prefix[64] , *cp ;
   mat44        tmat , smat , qmat ;
   THD_3dim_dataset *nset ;
   NI_str_array *csar ; int ii,kk ;
   Nwarp_catlist *nwc=NULL ;
   char *geomstring=NULL , *hs ;
   int nvar=1 , do_regrid=0 , nbad=0 , gs_mismatch=0 ;
   mat44_vec *mvv ; mat44 mmm ;
   float dxmax=0.0f,dymax=0.0f,dzmax=0.0f ; float_triple dxyz ;

ENTRY("IW3D_read_nwarp_catlist") ;

   if( cstr == NULL || *cstr == '\0' ) RETURN(NULL) ;

#ifdef DEBUG_CATLIST
if( verb_nww > 1 ) INFO_message("Enter IW3D_read_nwarp_catlist( %s )",cstr) ;
#endif

   csar = NI_decode_string_list(cstr,";") ;
   if( csar == NULL || csar->num < 1 ) RETURN(NULL) ;

   if( csar->num > CW_NMAX ){
     ERROR_message("Too many components (%d > %d) for IW3D_read_nwarp_catlist",
                   csar->num , CW_NMAX ) ;
     NI_delete_str_array(csar) ;
     RETURN(NULL) ;
   }

   /*-------- simple case of a single dataset input --------*/

   if( csar->num == 1 ){
#ifdef DEBUG_CATLIST
if( verb_nww > 1 ) ININFO_message("Open single warp dataset %s",csar->str[0]) ;
#endif
     cp = csar->str[0] ;
     if( strcasestr(cp,".1D") != NULL || strcasestr(cp,".txt") != NULL ){
       ERROR_message("Warp dataset '%s' name is too much like a matrix text file :-?",cp) ;
       NI_delete_str_array(csar) ; RETURN(NULL) ;
     }
     nset = CW_read_dataset_warp(cp) ;  /* does INV() etc. */
     if( nset == NULL ){
       ERROR_message("Can't open 3D warp dataset '%s'",cp) ;
       NI_delete_str_array(csar) ; RETURN(NULL) ;
     }
     DSET_load(nset) ;
     if( !DSET_LOADED(nset) ){
       ERROR_message("Warp dataset '%s' can't be loaded into memory",cp) ;
       NI_delete_str_array(csar) ; DSET_delete(nset) ; RETURN(NULL) ;
     }

     /* manufacture the very short output list */

     nwc = (Nwarp_catlist *)malloc(sizeof(Nwarp_catlist)) ;
     nwc->ncat       = nwc->nvar = 1 ;
     nwc->nwarp      = (THD_3dim_dataset **)malloc(sizeof(THD_3dim_dataset *)) ;
     nwc->nwarp[0]   = nset ;
     nwc->awarp      = NULL ;  /* trivial case == no matrices */
     nwc->flags      = 0 ;

     nwc->xshift = nwc->yshift = nwc->zshift = 0.0f ;
     hs = EDIT_get_geometry_string(nset) ;
     IW3D_set_geometry_nwarp_catlist( nwc , hs ) ;

     NI_delete_str_array(csar) ;
     RETURN(nwc) ;
   }

   /*-------- multiple input datasets / matrices --------*/

   nwc = (Nwarp_catlist *)malloc(sizeof(Nwarp_catlist)) ;
   nwc->nvar  = 1 ;                 /* no matrices yet: update in loop below */
   nwc->ncat  = csar->num ;
   nwc->awarp = (mat44_vec        **)calloc( sizeof(mat44_vec        *),csar->num ) ;
   nwc->nwarp = (THD_3dim_dataset **)calloc( sizeof(THD_3dim_dataset *),csar->num ) ;
   nwc->flags = 0 ;
   nwc->actual_geomstring = NULL ;  /* must be computed later */
   nwc->master_geomstring = NULL ;
   nwc->xshift = nwc->yshift = nwc->zshift = 0.0f ;
   nwc->xpad = nwc->ypad = nwc->zpad = 0 ;

   for( ii=0 ; ii < csar->num ; ii++ ){          /* read them all in, Frank */
     cp = csar->str[ii] ;

     /*-- read in a vector of matrices (technically a "free module" element) --*/

     if( strcasestr(cp,".1D") != NULL || strcasestr(cp,".txt") != NULL ){
#ifdef DEBUG_CATLIST
if( verb_nww > 1 ) ININFO_message("Open matrix file %s",cp) ;
#endif
       mvv = CW_read_affine_warp(cp) ;
       if( mvv == NULL || mvv->nmar <= 0 || mvv->mar == NULL ){  /* bad bad bad */
         ERROR_message("Failed to read affine warp from '%s'",cp) ;
         nbad++ ; continue ;
       }
       dxyz = M44_max_shifts(mvv) ;
#ifdef DEBUG_CATLIST
if( verb_nww > 1 ) ININFO_message("  max shifts = %g %g %g",dxyz.a,dxyz.b,dxyz.c) ;
#endif
       dxmax += dxyz.a ; dymax += dxyz.b ; dzmax += dxyz.c ;
       if( mvv->nmar > nwc->nvar ) nwc->nvar = mvv->nmar ;  /* update of max */

       nwc->awarp[ii] = mvv  ;  /* insert into output */
       nwc->nwarp[ii] = NULL ;

     } else { /*-- read in a nonlinear 3D warp (from a dataset) --*/

#ifdef DEBUG_CATLIST
if( verb_nww > 1 ) ININFO_message("Open dataset warp %s",cp) ;
#endif
       nset = CW_read_dataset_warp(cp) ;
       if( nset == NULL ){
         ERROR_message("Can't open/process 3D warp dataset '%s'",cp) ;
         nbad++ ; continue ;
       }

       dxyz = THD_nwarp_maxdisp(nset) ;
#ifdef DEBUG_CATLIST
if( verb_nww > 1 ) ININFO_message("  max displacments = %g %g %g",dxyz.a,dxyz.b,dxyz.c) ;
#endif
       dxmax += dxyz.a ; dymax += dxyz.b ; dzmax += dxyz.c ;

       hs = EDIT_get_geometry_string(nset) ; ;
       if( geomstring == NULL ){
         geomstring = hs ;
       } else if( EDIT_geometry_string_diff(geomstring,hs) > 0.01f ){
#ifdef DEBUG_CATLIST
if( verb_nww > 1 ) ININFO_message(" found geometry mismatch: orig=%s new=%s",geomstring,hs) ;
#endif
         gs_mismatch++ ;
       }

       nwc->nwarp[ii] = nset ;  /* insert into output */
       nwc->awarp[ii] = NULL ;
     }

   } /* end of loop over input sub-strings */

   NI_delete_str_array(csar) ;  /* toss the jetsam */

   if( geomstring == NULL || nbad > 0 ){  /* bad inputs */
     ERROR_message("Can't compute nonlinear 3D warp from string '%s'",cstr) ;
     IW3D_destroy_nwarp_catlist(nwc) ; RETURN(NULL) ;
   }

   /* save the sum of maximum shifts encountered */

   nwc->xshift = dxmax ; nwc->yshift = dymax ; nwc->zshift = dzmax ;
#ifdef DEBUG_CATLIST
if( verb_nww > 1 ) ININFO_message("--- Totalized max displacments = %g %g %g",dxmax,dymax,dzmax) ;
#endif

   /* if all nonlinear warps had same grid, save it in the catlist */

   if( !gs_mismatch ){
     IW3D_set_geometry_nwarp_catlist( nwc , geomstring ) ;
     ii = IW3D_reduce_nwarp_catlist( nwc ) ;  /* and can 'reduce' it now */
#ifdef DEBUG_CATLIST
if( verb_nww > 1 && ii > 0 ) ININFO_message("Reduced catlist by %d steps",ii) ;
#endif
   }

   RETURN(nwc) ;
}

#endif /*(C17)*/ /*###########################################################*/

/******************************************************************************/
/******************************************************************************/
#ifdef ALLOW_QWARP       /** the following code is for 3dQwarp (duh) **/
/******************************************************************************/
/******************************************************************************/

#if 1
/*============================================================================*/
/** (Q1) Introduction to 3dQwarp, and Global variables for 3dQwarp-ing       **/
/*============================================================================*/

/**--------------------------------------------------------------------------**/
/** The basic 3dQwarp run is an invocation of function IW3D_warp_s2bim()
    (s2bim == "source to base image"), which returns a struct combining the
    final index warp and warped source image.  The outline of function calls
    follows (some minor calls are omitted -- I'm sure you'll figure them out):

     (0) 3dQwarp main() == sets up the global variables (cf. infra) from
                           the options and inputs, calls IW3D_warp_s2bim(),
                           and then writes the outputs
      (1) IW3D_warp_s2bim() == driver function for IW3D_warpomatic()
                               and applies the warp to the source image
       (2a) IW3D_warpomatic() == produces the optimized warp, by driving
                                 IW3D_improve_warp() over global and then
                                 local (overlapping) patches
        (3a) IW3D_setup_for_improvement() == sets up data for warp optimizing
         (4) IW3D_load_energy() == compute 'energy' fields for global warp
                                   as it is now, for warp penalty later
                                   -- if the inital warp (S2BIM_iwarp) is
                                      not NULL, then this step is important
        (3b) IW3D_improve_warp() == called many many times over shrinking
                                    patches; this function optimizes the
                                    incremental warp Hwarp and then updates
                                    the global warp Haawarp -- the eventual
                                    output of IW3D_warpomatic()
         (4a) H?warp_setup_basis() == sets up basis polynomials for patch warp
                                      (? == Q for quintic, C for cubic)
         (4b) INCOR_create() == sets up struct for incremental 'correlation'
                                calculations -- first computes with the data
                                outside the optimizing patch and later gets
                                finalized each time the patch is updated
         (4c) HPEN_addup() == initialize warp penalty sum for points outside
                              the current patch (from the 'energy' fields)
         (4c) powell_newuoa_con() == Powell NEWUOA constrained optimizer
                                     which actually optimizes Hwarp by
                                     playing with the coefficients of
                                     the warp basis polynomials
          (5) IW3D_scalar_costfun() == function being optimized
           (5a) Hwarp_apply() == computes patch warp Hwarp, composes it with
                                 current global warp Haawarp, and then applies
                                 it to source image to produce the patch data
                                 (using linear interpolation)
            (6) H?warp_eval_X() == evaluate the patch warp given the current
                                   parameters (? == Q or C; X == A or B,
                                   depending on the size of the patch); uses
                                   data setup from H?warp_setup_basis()
           (5b) INCOR_evaluate() == completes the 'incomplete' correlation
                                    using the patch data just computed;
                                    this is the measurement of image matching
           (5c) HPEN_penalty() == finalizes the penalty given the patch warp
            (6) IW3D_load_energy() = computes the 'energy' of the patch warp
        (3c) IW3D_cleanup_improvement() == free workspaces created in
                                           IW3D_setup_for_improvement()
       (2b) IW3D_warp_floatim() == produces the warped source image
                                   (using wsinc5 interpolation)

    Parallel functions for duplo [starting at IW3D_warp_s2bim_duplo()] and
    plusminus [starting at IW3D_warp_s2bim_plusminus()] exist, but their
    structure is the same as above, with only details to set them apart.

    There is an unfinished function THD_warpomatic() which was intended to
    provide a simple way to warp one dataset to another, but that was never
    completed.  At present, you have to use IW3D_warp_s2bim(), which requires
    MRI_IMAGEs as the inputs, not datasets.

    3dQwarp is *supposed* to work with 2D images (nz=1), and there is code
    to allow for that case -- but it has never been tested! (OK, once.)

    The INCOR_* functions are in thd_incorrelate.c (which is #include-d far
    far above), and handle "incomplete correlation" calculations, where part
    of the data is fixed (the part outside the patch being optimized), and
    part is changing (the part inside the current patch).  For speed, the
    computations for the fixed part are only done once -- via INCOR_addto()
    in function IW3D_improve_warp() before the patch optimization is done.
    The "correlation" (cost function) is completed via INCOR_evaluate() in
    function IW3D_scalar_costfun() -- which is what powell_newuoa_con()
    tries to minimize.
*//**------------------------------------------------------------------------**/

/**--------------------------------------------------------------------------**/
/** Many (but not all) global variables below start with 'H',
    which is my notation for the warp patch being optimized;
    however, not all 'H' variables are about the patch itself;
    some of these get set in 3dQwarp.c to control the warp optimization **/
/**--------------------------------------------------------------------------**/

/*--- flag masks for types of displacement allowed (for Hflags, etc.) ---*/

#define NWARP_NOXDIS_FLAG  1  /* no displacment in X direction? */
#define NWARP_NOYDIS_FLAG  2
#define NWARP_NOZDIS_FLAG  4

#define NWARP_NODISP_FLAG (NWARP_NOXDIS_FLAG | NWARP_NOYDIS_FLAG | NWARP_NOZDIS_FLAG)

#define NWARP_NOXDEP_FLAG  8  /* no functional dependence of displacment on X? */
#define NWARP_NOYDEP_FLAG 16
#define NWARP_NOZDEP_FLAG 32

/*** We allow 2 types of polynomial patches: cubics and quintics.
     Quintic patches have 3*3*3*3=81 parameters, and so
     take much more time to evaluate and optimize than
     cubic patches, which have only 3*2*2*2 = 24 parameters.
     Quintic patches are C2 (2 continuous derivatives),
     while cubic patches are C1 (1 continuous derivative). ***/

/*--- Hermite polynomial basis arrays for each direction: x,y,z. ---*/
      /* ('c' for cubic, 'q' for quintic) */

static int nbcx=0, nbcy=0, nbcz=0 , nbcxy ;  /* dimensions of patch */
static int nbqx=0, nbqy=0, nbqz=0 , nbqxy ;  /* dimensions of patch */

/* 1D basis arrays for 2 cubics (#0 and #1) */

static float *bc0x=NULL, *bc1x=NULL,             dxci=0.0f ;
static float *bc0y=NULL, *bc1y=NULL,             dyci=0.0f ;
static float *bc0z=NULL, *bc1z=NULL,             dzci=0.0f ;

static float *bc2x=NULL, *bc3x=NULL, *bc4x=NULL ;  /* 05 Nov 2015 */
static float *bc2y=NULL, *bc3y=NULL, *bc4y=NULL ;
static float *bc2z=NULL, *bc3z=NULL, *bc4z=NULL ;
static float *bcxx[5]  , *bcyy[5]  , *bczz[5] ;

/* 1D basis arrays for 3 quintics (#0, #1, and #2) */

static float *bq0x=NULL, *bq1x=NULL, *bq2x=NULL, dxqi=0.0f ;
static float *bq0y=NULL, *bq1y=NULL, *bq2y=NULL, dyqi=0.0f ;
static float *bq0z=NULL, *bq1z=NULL, *bq2z=NULL, dzqi=0.0f ;

/* 3D basis arrays for cubics and quintics (used for 'small' patches) */

static int    nbbcxyz = 0 ;
static int    nbbbcar = 0 ;
static float **bbbcar = NULL ;
static int    nbbqxyz = 0 ;
static float **bbbqar = NULL ;

#undef  MEGA
#define MEGA 1048576   /* 2^20 = definition of 'small' */

/*--- local (small) warp over region we are optimizing ---*/

static IndexWarp3D *Hwarp   = NULL ; /* Hermite patch increment = H(x) = 'patch warp' */
static IndexWarp3D *AHwarp  = NULL ; /* local version of global warp = A(H(x)) */
static int          need_AH = 1 ;    /* flag to compute AHwarp (not always needed) */
static int          Hflags  = 0 ;          /* flags for this patch */
static int          Hgflags = 0 ;          /* flags for global warp */
static int          Himeth  = MRI_LINEAR ; /* interpolation method for warped data */
                                           /* MRI_LINEAR or MRI_NN */

static int Hnx=0,Hny=0,Hnz=0,Hnxy=0,Hnxyz=0 ;  /* dimensions of base image */

/* defines the patch region index ranges (inclusive);
   e.g., Hibot and Hitop are in range 0..Hnx-1, and so forth */

static int Hibot,Hitop , Hjbot,Hjtop , Hkbot,Hktop ;

static int Hmatch_code  = 0 ;  /* how 'correlation' is computed (INCOR_) */
static int Hbasis_code  = 0 ;  /* quintic or cubic patches? */

#define ALLOW_BASIS5           /* allow use of the 'basis5' functions */

#define MRI_CUBIC_PLUS_1  301  /* 05 Nov 2015 -- extra basis function codes */
#define MRI_CUBIC_PLUS_2  302                 /* for the 'basis5' methods  */
#define MRI_CUBIC_PLUS_3  303

static int H5final = 0 ;       /* Use basis5 at final level (1 or 2 or 3) */

static int H4zero  = 0 ;       /* 12 Apr 2016 */

static double Hbasis_parmax = 0.0 ;  /* max warp parameter allowed */

static floatvec *Hmpar = NULL ;      /* parameters for 'correlation' match */

static const int Hlocalstat = 0 ;    /* this should not be used !!! */

/* these values are for the purpose of printing out some statistics */

static int Hskipped   = 0 ;          /* number of patches skipped */
static int Hdone      = 0 ;          /* number of patches optimized */

/*--- Other stuff for incremental warping ---*/

#undef USE_HLOADER  /* Define this for 'all-at-once' Hwarp load vs. incremental  */
                    /* tests show incremental is about 10% faster, with OpenMP.  */
                    /* (incremental means to compute as needed, vs. pre-compute) */
                    /* Note that USE_HLOADER does NOT work with the basis5 funcs */

#ifdef USE_HLOADER
static void (*Hloader)(float *) = NULL ; /* function to make warp from params */
#undef ALLOW_BASIS5
#endif

static int          Hnpar       = 0    ; /* num params for warp */
static int          Hnpar_sum   = 0    ; /* total num params used in course of run */
static float       *Hpar        = NULL ; /* param vector for local warp definition */
static float       *Hxpar ;              /* sub-array of params for x displacement */
static float       *Hypar ;
static float       *Hzpar ;
static int          Hdox ;               /* do we compute x displacements? */
static int          Hdoy ;
static int          Hdoz ;
static int         *Hparmap     = NULL ; /* map from active to all params */
static int          Hnparmap    = 0    ;
static int          Hnegate     = 0    ; /* negate correlation function? */
static int          Hnval       = 0    ; /* number of voxels in local patch */
static float       *Hwval       = NULL ; /* warped image values in local patch */
static float       *Haawt       = NULL ; /* weight iamge (sic) in local patch */
static float       *Hbval       = NULL ; /* base image in local patch */
static MRI_IMAGE   *Hsrcim      = NULL ; /* source image to warp (global) */
static MRI_IMAGE   *Hsrcim_blur = NULL ; /* blurred source image */
static float        Hblur_b     = 0.0f ; /* amount to blur base */
static float        Hblur_s     = 0.0f ; /* amount to blur source */
static float        Hpblur_b    = 0.0f ; /* progressive blur fraction: base */
static float        Hpblur_s    = 0.0f ; /* progressive blur fraction: source */
static int          Hforce      = 0    ; /* force an iterative warp update? */
static int          Hzeasy      = 0    ; /* take it easy at the zero level? */
static int          Hznoq       = 0    ; /* don't do quintic warp at the zero level? */
static float        Hfactor     = 0.44f; /* fraction of maximum warp size allowed */
static float        Hfactor_q   = 1.0f ; /* used in 3dQwarp */
static float        Hshrink     = 0.749999f ; /* shrink factor for patches between levels */
static int          Hngmin      = 25 ;   /* min patch size allowed in current run */
static IndexWarp3D *Haawarp     = NULL ; /* global warp we are improving = A(x) */
                                         /* AHwarp above is the patch copy of this */
static void        *Hincor      = NULL ; /* INCOR 'correlation' struct */
static MRI_IMAGE   *Haasrcim    = NULL ; /* warped source image (global) */

static MRI_IMAGE   *Hbasim      = NULL ; /* base image (global) */
static MRI_IMAGE   *Hbasim_blur = NULL ; /* blurred base image */
static MRI_IMAGE   *Hwtim       = NULL ; /* weight image (global) */
static float        Hwbar       = 0.0f ; /* average weight value */
static byte        *Hbmask      = NULL ; /* mask for base image (global) */
static byte        *Hemask      = NULL ; /* mask of voxels to EXCLUDE */

/*** these variables are inputs from the -xyzmatch option of 3dQwarp ***/

static int    Hxyzmatch_num    = 0   ; /* num xyz triples for pointwise match */
static double Hxyzmatch_fac    = 0.0 ; /* factor for sum */
static int    Hxyzmatch_pow    = 2   ; /* 1 for L1, 2 for L2 */
static double Hxyzmatch_cost   = 0.0 ;
static float *Hxyzmatch_bas[3] = {NULL,NULL,NULL} ; /* xyz triples in base */
static float *Hxyzmatch_src[3] = {NULL,NULL,NULL} ; /* xyz triples in source */
static float *Hxyzmatch_b2s[3] = {NULL,NULL,NULL} ; /* warped base triples */

/*** these variables affect how the iteration starts, stops, and proceeds ***/

static int Hlev_start =   0 ;  /* initial level of patches */
static int Hlev_end   = 666 ;  /* final level of patches to head towards */
static int Hlev_final =   0 ;  /* final level actually reached */
static int Hlev_now   =   0 ;  /* the level we are playing with at the moment */
static int Hduplo     =   0 ;  /* duplo mode? (faster, somewhat less accurate) */
static int Hfinal     =   0 ;  /* is this the final level we are working on now? */
static int Hworkhard1 =   0 ;  /* workhard stuff (but who wants to work hard?) */
static int Hworkhard2 =  -1 ;

static int Hopt_ball  =   0 ;  /* 'BALL' optimization strategy? [13 Jan 2015] */

static int  Hgridlist_num = 0 ;     /* 31 Dec 2014 */
static int *Hgridlist     = NULL ;
#define HAVE_HGRID ( Hgridlist_num > 0 && Hgridlist != NULL )
#define HGRID(gg) \
  ( ( HAVE_HGRID && (gg) >= 0 && (gg) < Hgridlist_num ) ? Hgridlist[gg] : -666 )

static int           Hsave_allwarps = 0 ;    /* 02 Jan 2015 */
static int           Hsave_num      = 0 ;
static IndexWarp3D **Hsave_iwarp    = NULL ;
static char        **Hsave_iname    = NULL ;

static void (*Hsave_callback_func)(IndexWarp3D * , char *) = NULL ;  /* 13 Mar 2018 */

#define HSAVE_DESTROY                                                         \
 do{ if( Hsave_num > 0 ){                                                     \
       if( Hsave_iwarp != NULL ){                                             \
         int ii ;                                                             \
         for( ii=0 ; ii < Hsave_num ; ii++ ) IW3D_destroy(Hsave_iwarp[ii]) ;  \
         free(Hsave_iwarp) ; Hsave_iwarp = NULL ;                             \
       }                                                                      \
       Hsave_num = 0 ;                                                        \
     } } while(0)

#define HSAVE_ADDTO(iww,inn)                                                                     \
 do{ if( Hsave_callback_func != NULL ){                                                          \
       Hsave_callback_func(iww,inn) ;                                                            \
     } else {                                                                                    \
       Hsave_iwarp = (IndexWarp3D **)realloc(Hsave_iwarp,sizeof(IndexWarp3D *)*(Hsave_num+1)) ;  \
       Hsave_iname = (char        **)realloc(Hsave_iname,sizeof(char        *)*(Hsave_num+1)) ;  \
       Hsave_iwarp[Hsave_num] = IW3D_copy(iww,1.0f) ;                                            \
       Hsave_iname[Hsave_num] = strdup(inn) ; Hsave_num++ ;                                      \
     }                                                                                           \
 } while(0)

static int   Hfirsttime = 0 ;    /* for fun only (to print stuff out in cost func) */
static float Hfirstcost = 666.0f;

static int Hsuperhard1 =  0 ;    /* Oh come on, who want to work super hard? */
static int Hsuperhard2 = -1 ;    /* Certainly not us government employees!!! */

static float Hstopcost = -666666.6f ; /* stop if 'correlation' cost goes below this */
static int   Hstopped  = 0 ;          /* indicate that iterations were stopped */
static int Hquitting   = 0 ;          /* set by signal handler to indicate 'quit NOW' */

/** macro ALLOW_QMODE (set or not in 3dQwarp.c) determines if quintic
    patches are allowed to be specified at various levels past Hlev_now=0 **/

#ifdef ALLOW_QMODE
static int Hqfinal  = 0 ;  /* do quintic at the final level? */
static int Hqonly   = 0 ;  /* do quintic at all levels? (very slow) */
static int Hqhard   = 0 ;  /* do quintic in second pass of 'workhard'? */
#else
# define   Hqfinal    0
# define   Hqonly     0
# define   Hqhard     0
#endif

#undef  WORKHARD  /* work hard at level #lll? */
#define WORKHARD(lll) ( !Hduplo && (lll) >= Hworkhard1 && (lll) <= Hworkhard2 )

#undef  SUPERHARD /* work superhard at level #lll? */
#define SUPERHARD(lll) ( !Hduplo && (lll) >= Hsuperhard1 && (lll) <= Hsuperhard2 )

static float Hcost  = 666.666f ;  /* current 'correlation' cost */
static float Hpenn  = 0.0f ;
static float Hcostt = 0.0f ;

#undef  SRCIM /* macro for which source image to use */
#define SRCIM ( (Hsrcim_blur != NULL ) ? Hsrcim_blur : Hsrcim )

#undef  BASIM /* macro for which base image to use */
#define BASIM ( (Hbasim_blur != NULL ) ? Hbasim_blur : Hbasim )

/*---------- Code and variables for '-inedge' enhancement [Jul 2018] ---------*/

#define ALLOW_INEDGE

#ifdef ALLOW_INEDGE
# include "mri_intedge.c"
  static int   Hinedge_erode = 4 ;
  static float Hinedge_frac  = 0.222f ;
  static int   Hinedge_doit  = 0 ;
#endif

/*----------------------------------------------------------------------------*/
/* Process the QUIT signal, as in 'kill -s QUIT <processID>' */

#include <signal.h>
void IW3D_signal_quit(int sig)
{
   static volatile int fff=0 ;
   if( fff ) _exit(1) ; else fff = 1 ;
   fprintf(stderr,"\n** quit signal received -- trying to die gracefully **\n");
   Hquitting = 1 ;
   return ;
}

/*-----*/

void IW3D_setup_signal_quit(void){ signal(SIGQUIT,IW3D_signal_quit); return; }

/*----------------------------------------------------------------------------*/
/* Make the displacement flags coherent and legal.  If impossible, return -1. */

int IW3D_munge_flags( int nx , int ny , int nz , int flags )
{
   int iflags = flags ;

   if( nx < 1 || ny < 1 || nz < 1 ) return -1 ;     /* bad bad bad */

   /* don't allow x-displacments if x size is too small,
      or if displacements aren't allowed to depend on x coordinate */

   if( nx < NGMIN || (flags & NWARP_NOXDEP_FLAG) )
     flags |= (NWARP_NOXDIS_FLAG | NWARP_NOXDEP_FLAG) ;

   /* same for y and z */

   if( ny < NGMIN || (flags & NWARP_NOYDEP_FLAG) )
     flags |= (NWARP_NOYDIS_FLAG | NWARP_NOYDEP_FLAG) ;

   if( nz < NGMIN || (flags & NWARP_NOZDEP_FLAG) )
     flags |= (NWARP_NOZDIS_FLAG | NWARP_NOZDEP_FLAG) ;

   /* set flags to -1 (indicating error) if nothing is left */

   if( (flags & NWARP_NODISP_FLAG) == NWARP_NODISP_FLAG ) flags = -1 ;

#if 0
   if( Hverb && iflags != flags )
     ININFO_message("      Flags:: input: x=%c y=%c z=%c  output: x=%c y=%c z=%c",
                    (iflags & NWARP_NOXDIS_FLAG) ? 'N' : 'Y' ,
                    (iflags & NWARP_NOYDIS_FLAG) ? 'N' : 'Y' ,
                    (iflags & NWARP_NOZDIS_FLAG) ? 'N' : 'Y' ,
                    ( flags & NWARP_NOXDIS_FLAG) ? 'N' : 'Y' ,
                    ( flags & NWARP_NOYDIS_FLAG) ? 'N' : 'Y' ,
                    ( flags & NWARP_NOZDIS_FLAG) ? 'N' : 'Y'  ) ;
#endif

   return flags ;
}

#endif /*(Q1)*/ /*############################################################*/

#if 1
/*============================================================================*/
/** (Q2) Basis functions (cubic and quintic) for warping                     **/
/*============================================================================*/

/*----------------------------------------------------------------------------*/
/* C1 Hermite cubic basis functions over [-1..1] -- that is, the 2 functions
   and their first derivatives go to 0 at x=1 and x=-1.
     The first function (ee.a) is such that f(0)=1, and f'(0)=0.
     The second function (ee.b) is such that f(0)=0, and f'(0)=6.75.
   Scale factors are adjusted so that the functions' peak values are all 1.
   Return value is a float_pair comprising the 2 function values.
*//*--------------------------------------------------------------------------*/

static INLINE float_pair HCwarp_eval_basis( float x )
{
   register float aa , bb ; float_pair ee ;

   aa = fabsf(x) ;
   if( aa >= 1.0f ){               /* outside range ==> 0 */
     ee.a = ee.b = 0.0f ;
   } else {
     bb = 1.0f - aa ; bb = bb*bb ;
     ee.a = bb * (1.0f+2.0f*aa) ;  /* f(0)  = 1 ; */
     ee.b = bb * x * 6.75f ;       /* f'(0) = 1 * 6.75 */
   }
   return ee ;
}

#ifdef ALLOW_BASIS5
/*----------------------------------------------------------------------------*/
/* The following functions add 3 more functions to the C1 basis set,
   for 'p' type refinement of the warp -- HCwarp_eval_'basisX' provides
   X basis functions, for X=3, 4, 5.  These additional functions are chosen
   to provide more oscillations in the basic [-1..1] interval.
     function #1 (ee.a) = no internal zero crossings
     function #2 (ee.b) =  1 internal zero crossing
     function #3 (ee.c) =  2 internal zero crossings
     function #4 (ee.d) =  3 internal zero crossings
     function #5 (ee.e) =  4 internal zero crossings
   Functions #3-5 integrate to 0 over the interval [0..1] (orthogonal to 1).
   Function #5 is also orthogonal to x over [0..1].              [02 Nov 2015]
*//*--------------------------------------------------------------------------*/

#if 0  /* the basis3 and basis4 functions aren't actually needed */
static INLINE float_triple HCwarp_eval_basis3( float x )
{
   register float aa , bb ; float_triple ee ;

   aa = fabsf(x) ;
   if( aa >= 1.0f ){               /* outside range ==> 0 */
     ee.a = ee.b = ee.c = 0.0f ;
   } else {
     bb = 1.0f - aa ; bb = bb*bb ;
     ee.a = bb * (1.0f+2.0f*aa) ;  /* f(0)  = 1 ; */
     ee.b = bb * x * 6.75f ;       /* f'(0) = 1 * 6.75 */
     ee.c = bb * (1.0f+2.0f*aa-15.0f*aa*aa) ;
   }
   return ee ;
}

/*............................................................*/

static INLINE float_quad HCwarp_eval_basis4( float x )
{
   register float aa , bb ; float_quad ee ;

   aa = fabsf(x) ;
   if( aa >= 1.0f ){               /* outside range ==> 0 */
     ee.a = ee.b = ee.c = ee.d = 0.0f ;
   } else {
     bb = 1.0f - aa ; bb = bb*bb ;
     ee.a = bb * (1.0f+2.0f*aa) ;  /* f(0)  = 1 ; */
     ee.b = bb * x * 6.75f ;       /* f'(0) = 1 * 6.75 */
     ee.c = bb * (1.0f+2.0f*aa-aa*aa*15.0f) ;
     ee.d = bb * x * (1.0f-aa*aa*5.0f) * 9.75f ;
   }
   return ee ;
}
#endif

/*............................................................*/

static INLINE float_quint HCwarp_eval_basis5( float x )
{
   register float aa , bb ; float_quint ee ;

   aa = fabsf(x) ;
   if( aa >= 1.0f ){               /* outside range ==> 0 */
     ee.a = ee.b = ee.c = ee.d = ee.e = 0.0f ;
   } else {
     bb = 1.0f - aa ; bb = bb*bb ;
     ee.a = bb * (1.0f+2.0f*aa) ;  /* f(0)  = 1 ; */
     ee.b = bb * x * 6.75f ;       /* f'(0) = 1 * 6.75 */
     ee.c = bb * (1.0f+2.0f*aa-aa*aa*15.0f) ;
     ee.d = bb * x * (1.0f-aa*aa*5.0f) * 9.75f ;
     ee.e = bb * (1.0f+2.0f*aa-aa*aa*57.0f+aa*aa*aa*84.0f) ;
   }
   return ee ;
}
#endif /* ALLOW_BASIS5 */

/*----------------------------------------------------------------------------*/
/* C2 Hermite quintic basis functions over [-1..1] -- that is, the 3 functions
   and their first 2 derivatives go to 0 at x=1 and x=-1.
     The first function (ee.a) is such that f(0) != 0, and f'(0)=f''(0)=0.
     The second function (ee.b) is such that f(0)=f''(0)=0, and f'(0) != 0.
     The third function (ee.c) is such fhat f(0)=f'(0)=0, and f''(0) != 0.
   Scale factors are adjusted so that the functions' peak values are all 1.
   Return value is a float_triple comprising the 3 function values.
*//*--------------------------------------------------------------------------*/

static INLINE float_triple HQwarp_eval_basis( float x )
{
   register float aa , bb , aq ; float_triple ee ;

   aa = fabsf(x) ;
   if( aa >= 1.0f ){                                /* outside range ==> 0 */
     ee.a = ee.b = ee.c = 0.0f ;
   } else {
     bb = 1.0f - aa ; bb = bb*bb*bb ; aq = aa*aa ;
     ee.a = bb * ( (6.0f*aq+3.0f)*aa + 1.0f ) ;     /* f(0)   = 1 */
     ee.b = bb * x * (3.0f*aa+1.0f) * 5.0625f ;     /* f'(0)  = 1 * 5.0625 */
     ee.c = aq * bb * 28.935f ;                     /* f''(0) = 1 * 28.935 */
   }
   return ee ;
}

/*----------------------------------------------------------------------------*/
/* Macro to compute grid location coefficients inside interval x in [-1,1].
   * The leftmost index (that maps to x=-1) is ILEFT.
   * The rightmost index (that maps to x=+1) is IRGHT(n), where 'n' is the
     number of grid points, indexed from i=0 to i=n-1.
   * Output is ca and cb such that x = ca +cb*i.
   * This function is so we can use the cubic and quintic eval_basis()
     funcs above, which are defined over the domain [-1,1], when setting
     up the warp basis arrays over a range of integer indexes.
*//*--------------------------------------------------------------------------*/

/* Rational possibilities here are:
            ILEFT = 0    IRGHT = n-1    :: 0 at last grid point
            ILEFT = -1/2 IRGHT = n-1/2  :: 0 halfway past last grid point
            ILEFT = -1   IRGHT = n      :: 0 at next grid point outside patch
   Note that all the basis functions are 0 when evaluated at x=-1 or +1.      */

#define ILEFT    -0.5f        /* and these are the choices made by Zhark */
#define IRGHT(n) ((n)-0.5f)

#define COMPUTE_CAB(n)                                                   \
  do{ cb = 2.0f / (IRGHT(n)-ILEFT) ; ca = -1.0f - cb*ILEFT ; } while(0)

/*----------------------------------------------------------------------------*/
/* Setup cubic basis arrays for each dimension for patch = nx X ny X nz */

void HCwarp_setup_basis( int nx , int ny , int nz , int flags )
{
   float_pair ee ; int ii ; float ca,cb,ccc ;

ENTRY("HCwarp_setup_basis") ;

   /* if not going to use all 3D displacements,
      create map from active set of parameters to total set of parameters:
        Hparmap[j] = index into list 0..23 of the 'true' parameter location;
      We have to do this since the Powell optimization function requires
      input of all parameters in a contiguous array, but we want to keep
      them in their logical place even if some of them aren't being used,
      so this mapping will let us translate between these arrays (if needed) */

   Hflags = IW3D_munge_flags(nx,ny,nz,flags) ;
   FREEIFNN(Hparmap) ;

   if( (Hflags & NWARP_NODISP_FLAG) != 0 ){  /* not all params being used */
     int pm = 0 ;
     Hparmap = (int *)calloc(sizeof(int),24) ;
     if( !(Hflags & NWARP_NOXDIS_FLAG) ){
       for( ii=0 ; ii < 8 ; ii++ ) Hparmap[pm++] = ii ;     /* x params */
     }
     if( !(Hflags & NWARP_NOYDIS_FLAG) ){
       for( ii=0 ; ii < 8 ; ii++ ) Hparmap[pm++] = ii+8 ;   /* y params */
     }
     if( !(Hflags & NWARP_NOYDIS_FLAG) ){
       for( ii=0 ; ii < 8 ; ii++ ) Hparmap[pm++] = ii+16 ;  /* z params */
     }
     Hnparmap = pm ;
     if( Hnparmap == 24 ){ free(Hparmap) ; Hparmap = NULL ; }
   } else {
     Hnparmap = 24 ; Hparmap = NULL ;  /* no translation needed */
   }

   /* if everything is already cool, just zero out warp patches and exit */

   if( nx == nbcx      && ny == nbcy      && nz == nbcz      &&
       Hwarp != NULL   && AHwarp != NULL  &&
       nx == Hwarp->nx && ny == Hwarp->ny && nz == Hwarp->nz   ){
     IW3D_zero_fill(Hwarp) ; IW3D_zero_fill(AHwarp) ; EXRETURN ;
   }

   /* cleanup old stuff (recall that the 'c' arrays are for cubics) */

   if(  Hwarp != NULL ){ IW3D_destroy( Hwarp);  Hwarp = NULL; }
   if( AHwarp != NULL ){ IW3D_destroy(AHwarp); AHwarp = NULL; }

   FREEIFNN(bc0x); FREEIFNN(bc1x); nbcx=0;
   FREEIFNN(bc0y); FREEIFNN(bc1y); nbcy=0;
   FREEIFNN(bc0z); FREEIFNN(bc1z); nbcz=0;
   FREEIFNN(bc2x); FREEIFNN(bc3x); FREEIFNN(bc4x);
   FREEIFNN(bc2y); FREEIFNN(bc3y); FREEIFNN(bc4y);
   FREEIFNN(bc2z); FREEIFNN(bc3z); FREEIFNN(bc4z);

   if( bbbcar != NULL ){
     for( ii=0 ; ii < nbbbcar ; ii++ ) FREEIFNN(bbbcar[ii]) ;
     free(bbbcar) ; nbbcxyz = nbbbcar = 0 ; bbbcar = NULL ;
   }

   if( Hflags < 0 ) EXRETURN ;  /* this should not happen */

   /* create new stuff */

   nbcx = nx ;
   bc0x = (float *)malloc(sizeof(float)*nbcx) ;  /* 1D basis arrays */
   bc1x = (float *)malloc(sizeof(float)*nbcx) ;
   nbcy = ny ;
   bc0y = (float *)malloc(sizeof(float)*nbcy) ;
   bc1y = (float *)malloc(sizeof(float)*nbcy) ;
   nbcz = nz ;
   bc0z = (float *)malloc(sizeof(float)*nbcz) ;
   bc1z = (float *)malloc(sizeof(float)*nbcz) ;

   nbcxy = nbcx*nbcy ;  /* for indexing */

   /* fill arrays for x direction */

   if( Hflags & NWARP_NOXDEP_FLAG ){
     dxci = 0.0f ;
     for( ii=0 ; ii < nbcx ; ii++ ){
       bc0x[ii] = 1.0f ; bc1x[ii] = 0.0f ;
     }
   } else {
     COMPUTE_CAB(nbcx) ; dxci = 1.0f/cb ;   /* dxci = half-width of patch */
     for( ii=0 ; ii < nbcx ; ii++ ){
       ccc = ca + ii*cb ; ee = HCwarp_eval_basis(ccc) ;
       bc0x[ii] = ee.a ; bc1x[ii] = ee.b ;
     }
   }

   /* fill arrays for y direction */

   if( Hflags & NWARP_NOYDEP_FLAG ){
     dyci = 0.0f ;
     for( ii=0 ; ii < nbcy ; ii++ ){
       bc0y[ii] = 1.0f ; bc1y[ii] = 0.0f ;
     }
   } else {
     COMPUTE_CAB(nbcy) ; dyci = 1.0f/cb ;
     for( ii=0 ; ii < nbcy ; ii++ ){
       ccc = ca + ii*cb ; ee = HCwarp_eval_basis(ccc) ;
       bc0y[ii] = ee.a ; bc1y[ii] = ee.b ;
     }
   }

   /* fill arrays for z direction */

   if( Hflags & NWARP_NOZDEP_FLAG ){
     dzci = 0.0f ;
     for( ii=0 ; ii < nbcz ; ii++ ){
       bc0z[ii] = 1.0f ; bc1z[ii] = 0.0f ;
     }
   } else {
     COMPUTE_CAB(nbcz) ; dzci = 1.0f/cb ;
     for( ii=0 ; ii < nbcz ; ii++ ){
       ccc = ca + ii*cb ; ee = HCwarp_eval_basis(ccc) ;
       bc0z[ii] = ee.a ; bc1z[ii] = ee.b ;
     }
   }

   /* 3D versions for small enough warp fields (will be faster) */

   /** 8*MEGA times 8 arrays = 8*4*8 = 256 Megabytes of RAM **/

   nbbcxyz = nbcx * nbcy * nbcz ;  /* size of 3D patch */
   if( nbbcxyz <= 8*MEGA ){        /* max size of 3D patch storage */
     int jj , kk , qq ;
     bbbcar = (float **)malloc(sizeof(float *)*8) ; nbbbcar = 8 ;
     for( ii=0 ; ii < 8 ; ii++ )
       bbbcar[ii] = (float *)malloc(sizeof(float)*nbbcxyz) ;
     for( qq=kk=0 ; kk < nbcz ; kk++ ){
      for( jj=0 ; jj < nbcy ; jj++ ){
#pragma ivdep  /* for Intel icc compiler */
        for( ii=0 ; ii < nbcx ; ii++,qq++ ){
          bbbcar[0][qq] = bc0z[kk]*bc0y[jj]*bc0x[ii] ; /* this saves */
          bbbcar[1][qq] = bc1z[kk]*bc0y[jj]*bc0x[ii] ; /* having to */
          bbbcar[2][qq] = bc0z[kk]*bc1y[jj]*bc0x[ii] ; /* do all these */
          bbbcar[3][qq] = bc1z[kk]*bc1y[jj]*bc0x[ii] ; /* multiplies */
          bbbcar[4][qq] = bc0z[kk]*bc0y[jj]*bc1x[ii] ; /* over and over */
          bbbcar[5][qq] = bc1z[kk]*bc0y[jj]*bc1x[ii] ; /* when iterating */
          bbbcar[6][qq] = bc0z[kk]*bc1y[jj]*bc1x[ii] ;
          bbbcar[7][qq] = bc1z[kk]*bc1y[jj]*bc1x[ii] ;
     }}}
   }

   /* create empty patch warp, to be populated in HCwarp_load,
      given these basis function arrays and the warp parameters */

   Hwarp  = IW3D_create(nbcx,nbcy,nbcz) ; /* incremental patch warp */
   AHwarp = IW3D_create(nbcx,nbcy,nbcz) ; /* global warp(patch warp) in patch */

   EXRETURN ;
}

#ifdef ALLOW_BASIS5
/*----------------------------------------------------------------------------*/
/* Setup cubic basis arrays for each dimension for patch = nx X ny X nz,
   but with some extra basis functions (1 or 2 or 3) given by nplus.
   This function ALWAYS sets up 3D basis arrays for speed, so only should
   be used on small-ish patches -- say 25x25x25 or smaller.      [05 Nov 2015]
*//*--------------------------------------------------------------------------*/

void HCwarp_setup_basis5( int nx , int ny , int nz , int flags , int nplus )
{
   float_quint ee ; int ii,jj,kk,pp,qq,rr,ss,hh,nb5,nparm ; float ca,cb,ccc ;

ENTRY("HCwarp_setup_basis5") ;

   if( nplus <= 0 || nplus > 3 )
     ERROR_exit("nplus=%d in call to HCwarp_setup_basis5 :-(",nplus) ;

   nb5 = 2+nplus ; nparm = nb5*nb5*nb5 ;

   /* if not going to use all 3D displacements,
      create map from active set of parameters to total set of parameters:
        Hparmap[j] = index into list 0..3*nb5*nb5*nb5-1 of the 'true'
                     parameter location;
      We have to do this since the Powell optimization function requires
      input of all parameters in a contiguous array, but we want to keep
      them in their logical place even if some of them aren't being used,
      so this mapping will let us translate between these arrays (if needed) */

   Hflags = IW3D_munge_flags(nx,ny,nz,flags) ;
   FREEIFNN(Hparmap) ;

   if( (Hflags & NWARP_NODISP_FLAG) != 0 ){  /* not all params being used */
     int pm = 0 ;
     Hparmap = (int *)calloc(sizeof(int),3*nparm) ;
     if( !(Hflags & NWARP_NOXDIS_FLAG) ){
       for( ii=0 ; ii < nparm ; ii++ ) Hparmap[pm++] = ii ;         /* x params */
     }
     if( !(Hflags & NWARP_NOYDIS_FLAG) ){
       for( ii=0 ; ii < nparm ; ii++ ) Hparmap[pm++] = ii+nparm ;   /* y params */
     }
     if( !(Hflags & NWARP_NOYDIS_FLAG) ){
       for( ii=0 ; ii < nparm ; ii++ ) Hparmap[pm++] = ii+2*nparm ; /* z params */
     }
     Hnparmap = pm ;
     if( Hnparmap == 3*nparm ){ free(Hparmap) ; Hparmap = NULL ; }
   } else {
     Hnparmap = 3*nparm ; Hparmap = NULL ;  /* no translation needed */
   }

   /* if everything is already cool, just zero out warp patches and exit */

   if( nx == nbcx      && ny == nbcy      && nz == nbcz       &&
       Hwarp != NULL   && AHwarp != NULL  && nparm == nbbbcar &&
       nx == Hwarp->nx && ny == Hwarp->ny && nz == Hwarp->nz    ){
STATUS("everything is cool") ;
     IW3D_zero_fill(Hwarp) ; IW3D_zero_fill(AHwarp) ; EXRETURN ;
   }

   /* cleanup old stuff (recall that the 'c' arrays are for cubics) */

STATUS("cleanup old stuff") ;
   if(  Hwarp != NULL ){ IW3D_destroy( Hwarp);  Hwarp = NULL; }
   if( AHwarp != NULL ){ IW3D_destroy(AHwarp); AHwarp = NULL; }

   FREEIFNN(bc0x); FREEIFNN(bc1x); nbcx=0;
   FREEIFNN(bc0y); FREEIFNN(bc1y); nbcy=0;
   FREEIFNN(bc0z); FREEIFNN(bc1z); nbcz=0;
   FREEIFNN(bc2x); FREEIFNN(bc3x); FREEIFNN(bc4x);
   FREEIFNN(bc2y); FREEIFNN(bc3y); FREEIFNN(bc4y);
   FREEIFNN(bc2z); FREEIFNN(bc3z); FREEIFNN(bc4z);

   if( bbbcar != NULL ){
     for( ii=0 ; ii < nbbbcar ; ii++ ) FREEIFNN(bbbcar[ii]) ;
     free(bbbcar) ; nbbcxyz = nbbbcar = 0 ; bbbcar = NULL ;
   }

   if( Hflags < 0 ) EXRETURN ;  /* this should not happen */

   /* create new stuff */

   nbcx = nx ;
   bc0x = (float *)malloc(sizeof(float)*nbcx) ;  /* 1D basis arrays */
   bc1x = (float *)malloc(sizeof(float)*nbcx) ;
   bc2x = (float *)malloc(sizeof(float)*nbcx) ;
   bc3x = (float *)malloc(sizeof(float)*nbcx) ;
   bc4x = (float *)malloc(sizeof(float)*nbcx) ;
   nbcy = ny ;
   bc0y = (float *)malloc(sizeof(float)*nbcy) ;
   bc1y = (float *)malloc(sizeof(float)*nbcy) ;
   bc2y = (float *)malloc(sizeof(float)*nbcy) ;
   bc3y = (float *)malloc(sizeof(float)*nbcy) ;
   bc4y = (float *)malloc(sizeof(float)*nbcy) ;
   nbcz = nz ;
   bc0z = (float *)malloc(sizeof(float)*nbcz) ;
   bc1z = (float *)malloc(sizeof(float)*nbcz) ;
   bc2z = (float *)malloc(sizeof(float)*nbcz) ;
   bc3z = (float *)malloc(sizeof(float)*nbcz) ;
   bc4z = (float *)malloc(sizeof(float)*nbcz) ;

   nbcxy = nbcx*nbcy ;  /* for indexing */

   /* fill arrays for x direction */

STATUS("fill arrays") ;
   if( Hflags & NWARP_NOXDEP_FLAG ){
     dxci = 0.0f ;
     for( ii=0 ; ii < nbcx ; ii++ ){
       bc0x[ii] = 1.0f ; bc1x[ii] = bc2x[ii] = bc3x[ii] = bc4x[ii] = 0.0f ;
     }
   } else {
     COMPUTE_CAB(nbcx) ; dxci = 1.0f/cb ;   /* dxci = half-width of patch */
/* INFO_message("basis5 for nbcx=%d",nbcx) ; */
     for( ii=0 ; ii < nbcx ; ii++ ){
       ccc = ca + ii*cb ; ee = HCwarp_eval_basis5(ccc) ;
       bc0x[ii] = ee.a ; bc1x[ii] = ee.b ;
       bc2x[ii] = ee.c ; bc3x[ii] = ee.d ; bc4x[ii] = ee.e ;
/* ININFO_message(" x=%7.4f  %8.4f %8.4f %8.4f %8.4f %8.4f",ccc,ee.a,ee.b,ee.c,ee.d,ee.e) ; */
     }
   }

   /* fill arrays for y direction */

   if( Hflags & NWARP_NOYDEP_FLAG ){
     dyci = 0.0f ;
     for( ii=0 ; ii < nbcy ; ii++ ){
       bc0y[ii] = 1.0f ; bc1y[ii] = bc2y[ii] = bc3y[ii] = bc4y[ii] = 0.0f ;
     }
   } else {
     COMPUTE_CAB(nbcy) ; dyci = 1.0f/cb ;
/* INFO_message("basis5 for nbcy=%d",nbcy) ; */
     for( ii=0 ; ii < nbcy ; ii++ ){
       ccc = ca + ii*cb ; ee = HCwarp_eval_basis5(ccc) ;
       bc0y[ii] = ee.a ; bc1y[ii] = ee.b ;
       bc2y[ii] = ee.c ; bc3y[ii] = ee.d ; bc4y[ii] = ee.e ;
/* ININFO_message(" y=%7.4f  %8.4f %8.4f %8.4f %8.4f %8.4f",ccc,ee.a,ee.b,ee.c,ee.d,ee.e) ; */
     }
   }

   /* fill arrays for z direction */

   if( Hflags & NWARP_NOZDEP_FLAG ){
     dzci = 0.0f ;
     for( ii=0 ; ii < nbcz ; ii++ ){
       bc0z[ii] = 1.0f ; bc1z[ii] = bc2z[ii] = bc3z[ii] = bc4z[ii] = 0.0f ;
     }
   } else {
     COMPUTE_CAB(nbcz) ; dzci = 1.0f/cb ;
/* INFO_message("basis5 for nbcz=%d",nbcz) ; */
     for( ii=0 ; ii < nbcz ; ii++ ){
       ccc = ca + ii*cb ; ee = HCwarp_eval_basis5(ccc) ;
       bc0z[ii] = ee.a ; bc1z[ii] = ee.b ;
       bc2z[ii] = ee.c ; bc3z[ii] = ee.d ; bc4z[ii] = ee.e ;
/* ININFO_message(" z=%7.4f  %8.4f %8.4f %8.4f %8.4f %8.4f",ccc,ee.a,ee.b,ee.c,ee.d,ee.e) ; */
     }
   }

   bcxx[0] = bc0x; bcxx[1] = bc1x; bcxx[2] = bc2x; bcxx[3] = bc3x; bcxx[4] = bc4x;
   bcyy[0] = bc0y; bcyy[1] = bc1y; bcyy[2] = bc2y; bcyy[3] = bc3y; bcyy[4] = bc4y;
   bczz[0] = bc0z; bczz[1] = bc1z; bczz[2] = bc2z; bczz[3] = bc3z; bczz[4] = bc4z;

   /* always use 3D arrays */

   nbbcxyz = nbcx * nbcy * nbcz ;  /* size of 3D patch */
   nbbbcar = nparm ;
/* INFO_message("allocate %d 3D arrays of size %d voxels",nbbbcar,nbbcxyz) ; */
   bbbcar  = (float **)malloc(sizeof(float *)*nbbbcar) ;
   for( ii=0 ; ii < nbbbcar ; ii++ )
     bbbcar[ii] = (float *)malloc(sizeof(float)*nbbcxyz) ;

STATUS("fill bbbcar") ;
   for( hh=kk=0 ; kk < nbcz ; kk++ ){       /* 3 loops over z,y,x */
    for( jj=0 ; jj < nbcy ; jj++ ){
     for( ii=0 ; ii < nbcx ; ii++,hh++ ){
          for( ss=rr=0 ; rr < nb5 ; rr++ ){ /* 3 loops over basis func order */
           for( qq=0 ; qq < nb5 ; qq++ ){   /* along each direction (x,y,z) */
            for( pp=0 ; pp < nb5 ; pp++,ss++ ){
              bbbcar[ss][hh] = bczz[pp][kk] * bcyy[qq][jj] * bcxx[rr][ii] ;
          }}}
   }}}

   /* create empty patch warp, to be populated in HCwarp_load,
      given these basis function arrays and the warp parameters */

STATUS("create empty warps") ;
   Hwarp  = IW3D_create(nbcx,nbcy,nbcz) ; /* incremental patch warp */
   AHwarp = IW3D_create(nbcx,nbcy,nbcz) ; /* global warp(patch warp) in patch */

   EXRETURN ;
}
#endif /* ALLOW_BASIS5 */

/*----------------------------------------------------------------------------*/
/*! Setup quintic basis arrays: like HCwarp_setup_basis(), but bigger */

void HQwarp_setup_basis( int nx , int ny , int nz , int flags )
{
   float_triple ee ; int ii ; float ca,cb,ccc ;

ENTRY("HQwarp_setup_basis") ;

   Hflags = IW3D_munge_flags(nx,ny,nz,flags) ;
   FREEIFNN(Hparmap) ;

   if( (Hflags & NWARP_NODISP_FLAG) != 0 ){
     int pm = 0 ;
     Hparmap = (int *)calloc(sizeof(int),81) ;
     if( !(Hflags & NWARP_NOXDIS_FLAG) ){
       for( ii=0 ; ii < 27 ; ii++ ) Hparmap[pm++] = ii ;
     }
     if( !(Hflags & NWARP_NOYDIS_FLAG) ){
       for( ii=0 ; ii < 27 ; ii++ ) Hparmap[pm++] = ii+27 ;
     }
     if( !(Hflags & NWARP_NOYDIS_FLAG) ){
       for( ii=0 ; ii < 27 ; ii++ ) Hparmap[pm++] = ii+54 ;
     }
     Hnparmap = pm ;
     if( Hnparmap == 81 ){ free(Hparmap) ; Hparmap = NULL ; }
   } else {
     Hnparmap = 81 ; Hparmap = NULL ;
   }

   if( nx == nbqx      && ny == nbqy      && nz == nbqz      &&
       Hwarp != NULL   && AHwarp != NULL  &&
       nx == Hwarp->nx && ny == Hwarp->ny && nz == Hwarp->nz   ){

     IW3D_zero_fill(Hwarp) ; IW3D_zero_fill(AHwarp) ; EXRETURN ;
   }

   if( Hwarp  != NULL ){ IW3D_destroy( Hwarp);  Hwarp = NULL; }
   if( AHwarp != NULL ){ IW3D_destroy(AHwarp); AHwarp = NULL; }

   FREEIFNN(bq0x); FREEIFNN(bq1x); FREEIFNN(bq2x); nbqx=0;
   FREEIFNN(bq0y); FREEIFNN(bq1y); FREEIFNN(bq2y); nbqy=0;
   FREEIFNN(bq0z); FREEIFNN(bq1z); FREEIFNN(bq2z); nbqz=0;

   if( bbbqar != NULL ){
     for( ii=0 ; ii < 27 ; ii++ ) FREEIFNN(bbbqar[ii]) ;
     free(bbbqar) ; nbbqxyz = 0 ; bbbqar = NULL ;
   }

   if( Hflags < 0 ) EXRETURN ;

   nbqx = nx ;
   bq0x = (float *)malloc(sizeof(float)*nbqx) ;
   bq1x = (float *)malloc(sizeof(float)*nbqx) ;
   bq2x = (float *)malloc(sizeof(float)*nbqx) ;
   nbqy = ny ;
   bq0y = (float *)malloc(sizeof(float)*nbqy) ;
   bq1y = (float *)malloc(sizeof(float)*nbqy) ;
   bq2y = (float *)malloc(sizeof(float)*nbqy) ;
   nbqz = nz ;
   bq0z = (float *)malloc(sizeof(float)*nbqz) ;
   bq1z = (float *)malloc(sizeof(float)*nbqz) ;
   bq2z = (float *)malloc(sizeof(float)*nbqz) ;

   nbqxy = nbqx*nbqy ;

   if( Hflags & NWARP_NOXDEP_FLAG ){
     dxqi = 0.0f ;
     for( ii=0 ; ii < nbqx ; ii++ ){
       bq0x[ii] = 1.0f ; bq1x[ii] = bq2x[ii] = 0.0f ;
     }
   } else {
     COMPUTE_CAB(nbqx) ; dxqi = 1.0f/cb ;
     for( ii=0 ; ii < nbqx ; ii++ ){
       ccc = ca + ii*cb ; ee = HQwarp_eval_basis(ccc) ;
       bq0x[ii] = ee.a ; bq1x[ii] = ee.b ; bq2x[ii] = ee.c ;
     }
   }

   if( Hflags & NWARP_NOYDEP_FLAG ){
     dyqi = 0.0f ;
     for( ii=0 ; ii < nbqy ; ii++ ){
       bq0y[ii] = 1.0f ; bq1y[ii] = bq2y[ii] = 0.0f ;
     }
   } else {
     COMPUTE_CAB(nbqy) ; dyqi = 1.0f/cb ;
     for( ii=0 ; ii < nbqy ; ii++ ){
       ccc = ca + ii*cb ; ee = HQwarp_eval_basis(ccc) ;
       bq0y[ii] = ee.a ; bq1y[ii] = ee.b ; bq2y[ii] = ee.c ;
     }
   }

   if( Hflags & NWARP_NOZDEP_FLAG ){
     dzqi = 0.0f ;
     for( ii=0 ; ii < nbqz ; ii++ ){
       bq0z[ii] = 1.0f ; bq1z[ii] = bq2z[ii] = 0.0f ;
     }
   } else {
     COMPUTE_CAB(nbqz) ; dzqi = 1.0f/cb ;
     for( ii=0 ; ii < nbqz ; ii++ ){
       ccc = ca + ii*cb ; ee = HQwarp_eval_basis(ccc) ;
       bq0z[ii] = ee.a ; bq1z[ii] = ee.b ; bq2z[ii] = ee.c ;
     }
   }

   /* 3D versions? */

   /** 2*MEGA times 27 arrays = 2*4*27 = 216 Megabytes of RAM **/

   nbbqxyz = nbqx * nbqy * nbqz ;
   if( nbbqxyz <= 2*MEGA ){
     int jj , kk , qq ;
     bbbqar = (float **)malloc(sizeof(float *)*27) ;
     for( ii=0 ; ii < 27 ; ii++ )
       bbbqar[ii] = (float *)malloc(sizeof(float)*nbbqxyz) ;
     for( qq=kk=0 ; kk < nbqz ; kk++ ){
      for( jj=0 ; jj < nbqy ; jj++ ){
#pragma ivdep  /* for Intel icc compiler */
        for( ii=0 ; ii < nbqx ; ii++,qq++ ){
          bbbqar[ 0][qq] = bq0z[kk]*bq0y[jj]*bq0x[ii];
          bbbqar[ 1][qq] = bq1z[kk]*bq0y[jj]*bq0x[ii];
          bbbqar[ 2][qq] = bq2z[kk]*bq0y[jj]*bq0x[ii];
          bbbqar[ 3][qq] = bq0z[kk]*bq1y[jj]*bq0x[ii];
          bbbqar[ 4][qq] = bq1z[kk]*bq1y[jj]*bq0x[ii];
          bbbqar[ 5][qq] = bq2z[kk]*bq1y[jj]*bq0x[ii];
          bbbqar[ 6][qq] = bq0z[kk]*bq2y[jj]*bq0x[ii];
          bbbqar[ 7][qq] = bq1z[kk]*bq2y[jj]*bq0x[ii];
          bbbqar[ 8][qq] = bq2z[kk]*bq2y[jj]*bq0x[ii];
          bbbqar[ 9][qq] = bq0z[kk]*bq0y[jj]*bq1x[ii];
          bbbqar[10][qq] = bq1z[kk]*bq0y[jj]*bq1x[ii];
          bbbqar[11][qq] = bq2z[kk]*bq0y[jj]*bq1x[ii];
          bbbqar[12][qq] = bq0z[kk]*bq1y[jj]*bq1x[ii];
          bbbqar[13][qq] = bq1z[kk]*bq1y[jj]*bq1x[ii];
          bbbqar[14][qq] = bq2z[kk]*bq1y[jj]*bq1x[ii];
          bbbqar[15][qq] = bq0z[kk]*bq2y[jj]*bq1x[ii];
          bbbqar[16][qq] = bq1z[kk]*bq2y[jj]*bq1x[ii];
          bbbqar[17][qq] = bq2z[kk]*bq2y[jj]*bq1x[ii];
          bbbqar[18][qq] = bq0z[kk]*bq0y[jj]*bq2x[ii];
          bbbqar[19][qq] = bq1z[kk]*bq0y[jj]*bq2x[ii];
          bbbqar[20][qq] = bq2z[kk]*bq0y[jj]*bq2x[ii];
          bbbqar[21][qq] = bq0z[kk]*bq1y[jj]*bq2x[ii];
          bbbqar[22][qq] = bq1z[kk]*bq1y[jj]*bq2x[ii];
          bbbqar[23][qq] = bq2z[kk]*bq1y[jj]*bq2x[ii];
          bbbqar[24][qq] = bq0z[kk]*bq2y[jj]*bq2x[ii];
          bbbqar[25][qq] = bq1z[kk]*bq2y[jj]*bq2x[ii];
          bbbqar[26][qq] = bq2z[kk]*bq2y[jj]*bq2x[ii];
     }}}
   }

    Hwarp = IW3D_create(nbqx,nbqy,nbqz) ;
   AHwarp = IW3D_create(nbqx,nbqy,nbqz) ;

   EXRETURN ;
}

#endif /*(Q2)*/ /*############################################################*/

#if 1
/*============================================================================*/
/** (Q3) Functions to create a patch warp from basis functions and parameters */
/** These functions look complicated, partly because there are 2 sets of
    functions -- with and without USE_HLOADER #define-d -- and partly because
    there are separate cases for quintic and cubic bases, with special
    sub-cases further broken out for increased efficiency.  But the underlying
    code is actually pretty simple:

       H(x,y,z) = sum { param#p * basisfunc#p(x,y,z) }
                   p                                                          */
/*============================================================================*/

/*----------------------------------------------------------------------------*/
/** Note that USE_HLOADER is not defined any more;
    this older code loads the entire 3D warp patch given the parameters;
    the newer way is to compute the patch displacement only as needed,
    which is somewhat faster when compiled with OpenMP.
**//*-------------------------------------------------------------------------*/

#ifdef USE_HLOADER  /*HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH*/
/*----------------------------------------------------------------------------*/
/*! Load the Hwarp[] arrays, given a set of 24 = 2x2x2x3 cubic parameters:
    2 for each direction (the cubic basis functions), and then 3 directions */

void HCwarp_load( float *par )  /* 24 elements in par */
{
   int nxy,nxyz , dox,doy,doz ; float *xx,*yy,*zz ;

ENTRY("HCwarp_load") ;

   if( Hwarp == NULL || par == NULL ) EXRETURN ;       /* bad inputs */

   xx = Hwarp->xd ; yy = Hwarp->yd ; zz = Hwarp->zd ;  /* arrays to fill */

   nxy = nbcx*nbcy ; nxyz = nxy*nbcz ;

   dox = !(Hflags & NWARP_NOXDIS_FLAG) ;  /* do the x direction? */
   doy = !(Hflags & NWARP_NOYDIS_FLAG) ;  /* y? */
   doz = !(Hflags & NWARP_NOZDIS_FLAG) ;  /* z? */

   if( !dox ) AAmemset( xx , 0 , sizeof(float)*nxyz ) ;  /* no x => zero fill */
   if( !doy ) AAmemset( yy , 0 , sizeof(float)*nxyz ) ;
   if( !doz ) AAmemset( zz , 0 , sizeof(float)*nxyz ) ;

   AFNI_do_nothing() ; /* fprintf(stderr,"a") ; */

   if( bbbcar == NULL ){ /*----------------------------------*/
     AFNI_OMP_START ;
#pragma omp parallel
     { int ii,jj,kk,qq ; float *xpar, *ypar, *zpar ;
       float b0zb0yb0x,b1zb0yb0x, b0zb1yb0x,b1zb1yb0x,
             b0zb0yb1x,b1zb0yb1x, b0zb1yb1x,b1zb1yb1x ;
       xpar = par ; ypar = par+8 ; zpar = par+16 ;
#pragma omp for
       for( qq=0 ; qq < nxyz ; qq++ ){          /* parallel-ized loop over grid */
         ii = qq % nbcx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nbcx ; /* 3D indexes */

         /* calculate all 8=2x2x2 tensor products of basis functions */

         b0zb0yb0x = bc0z[kk]*bc0y[jj]*bc0x[ii]; b1zb0yb0x = bc1z[kk]*bc0y[jj]*bc0x[ii];
         b0zb1yb0x = bc0z[kk]*bc1y[jj]*bc0x[ii]; b1zb1yb0x = bc1z[kk]*bc1y[jj]*bc0x[ii];
         b0zb0yb1x = bc0z[kk]*bc0y[jj]*bc1x[ii]; b1zb0yb1x = bc1z[kk]*bc0y[jj]*bc1x[ii];
         b0zb1yb1x = bc0z[kk]*bc1y[jj]*bc1x[ii]; b1zb1yb1x = bc1z[kk]*bc1y[jj]*bc1x[ii];

         /* scale functions by half-size of grid (dxi, dyi, dzi) */

         if( dox ) xx[qq] = dxci *
                    (  b0zb0yb0x*xpar[0] + b1zb0yb0x*xpar[1] + b0zb1yb0x*xpar[2]
                     + b1zb1yb0x*xpar[3] + b0zb0yb1x*xpar[4] + b1zb0yb1x*xpar[5]
                     + b0zb1yb1x*xpar[6] + b1zb1yb1x*xpar[7]                     ) ;
         if( doy ) yy[qq] = dyci *
                    (  b0zb0yb0x*ypar[0] + b1zb0yb0x*ypar[1] + b0zb1yb0x*ypar[2]
                     + b1zb1yb0x*ypar[3] + b0zb0yb1x*ypar[4] + b1zb0yb1x*ypar[5]
                     + b0zb1yb1x*ypar[6] + b1zb1yb1x*ypar[7]                     ) ;
         if( doz ) zz[qq] = dzci *
                    (  b0zb0yb0x*zpar[0] + b1zb0yb0x*zpar[1] + b0zb1yb0x*zpar[2]
                     + b1zb1yb0x*zpar[3] + b0zb0yb1x*zpar[4] + b1zb0yb1x*zpar[5]
                     + b0zb1yb1x*zpar[6] + b1zb1yb1x*zpar[7]                     ) ;
       } /* end of for loop */
     }  /* end of parallel stuff */
     AFNI_OMP_END ;

   } else { /*------------------------------------------------------------------*/

     AFNI_OMP_START ;
#pragma omp parallel
     { int qq ; float *xpar, *ypar, *zpar ;
       float b0zb0yb0x,b1zb0yb0x, b0zb1yb0x,b1zb1yb0x,
             b0zb0yb1x,b1zb0yb1x, b0zb1yb1x,b1zb1yb1x ;
       xpar = par ; ypar = par+8 ; zpar = par+16 ;
#pragma omp for
       for( qq=0 ; qq < nxyz ; qq++ ){          /* parallel-ized loop over grid */

         b0zb0yb0x = bbbcar[0][qq] ; b1zb0yb0x = bbbcar[1][qq] ;
         b0zb1yb0x = bbbcar[2][qq] ; b1zb1yb0x = bbbcar[3][qq] ;
         b0zb0yb1x = bbbcar[4][qq] ; b1zb0yb1x = bbbcar[5][qq] ;
         b0zb1yb1x = bbbcar[6][qq] ; b1zb1yb1x = bbbcar[7][qq] ;

         /* scale functions by half-size of grid (dxi, dyi, dzi) */

         if( dox ) xx[qq] = dxci *
                    (  b0zb0yb0x*xpar[0] + b1zb0yb0x*xpar[1] + b0zb1yb0x*xpar[2]
                     + b1zb1yb0x*xpar[3] + b0zb0yb1x*xpar[4] + b1zb0yb1x*xpar[5]
                     + b0zb1yb1x*xpar[6] + b1zb1yb1x*xpar[7]                     ) ;
         if( doy ) yy[qq] = dyci *
                    (  b0zb0yb0x*ypar[0] + b1zb0yb0x*ypar[1] + b0zb1yb0x*ypar[2]
                     + b1zb1yb0x*ypar[3] + b0zb0yb1x*ypar[4] + b1zb0yb1x*ypar[5]
                     + b0zb1yb1x*ypar[6] + b1zb1yb1x*ypar[7]                     ) ;
         if( doz ) zz[qq] = dzci *
                    (  b0zb0yb0x*zpar[0] + b1zb0yb0x*zpar[1] + b0zb1yb0x*zpar[2]
                     + b1zb1yb0x*zpar[3] + b0zb0yb1x*zpar[4] + b1zb0yb1x*zpar[5]
                     + b0zb1yb1x*zpar[6] + b1zb1yb1x*zpar[7]                     ) ;
       } /* end of for loop */
     }  /* end of parallel stuff */
     AFNI_OMP_END ;

   }

   AFNI_do_nothing() ; /* fprintf(stderr,"A") ; */
   EXRETURN ;
}

/*----------------------------------------------------------------------------*/
/*! Load the Hwarp[] array, given a set of 81 = 3x3x3x3 quintic parameters:
    3 for each direction (the quintic basis functions), and then 3 directions */

void HQwarp_load( float *par )  /* 81 elements in par */
{
   int nxy,nxyz , dox,doy,doz ; float *xx,*yy,*zz ;

ENTRY("HQwarp_load") ;

   if( Hwarp == NULL || par == NULL ) EXRETURN ;

   xx = Hwarp->xd ; yy = Hwarp->yd ; zz = Hwarp->zd ;

   nxy = nbqx*nbqy ; nxyz = nxy*nbqz ;

   dox = !(Hflags & NWARP_NOXDIS_FLAG) ;
   doy = !(Hflags & NWARP_NOYDIS_FLAG) ;
   doz = !(Hflags & NWARP_NOZDIS_FLAG) ;

   if( !dox ) AAmemset( xx , 0 , sizeof(float)*nxyz ) ;
   if( !doy ) AAmemset( yy , 0 , sizeof(float)*nxyz ) ;
   if( !doz ) AAmemset( zz , 0 , sizeof(float)*nxyz ) ;

   if( bbbqar == NULL ){ /*----------------------------------*/
     AFNI_OMP_START ;
#pragma omp parallel
     { int ii,jj,kk,qq ; float *xpar=par , *ypar=par+27 , *zpar=par+54 ;
       float b0zb0yb0x,b1zb0yb0x, b2zb0yb0x,b0zb1yb0x, b1zb1yb0x,b2zb1yb0x,
             b0zb2yb0x,b1zb2yb0x, b2zb2yb0x,b0zb0yb1x, b1zb0yb1x,b2zb0yb1x,
             b0zb1yb1x,b1zb1yb1x, b2zb1yb1x,b0zb2yb1x, b1zb2yb1x,b2zb2yb1x,
             b0zb0yb2x,b1zb0yb2x, b2zb0yb2x,b0zb1yb2x, b1zb1yb2x,b2zb1yb2x,
             b0zb2yb2x,b1zb2yb2x, b2zb2yb2x ;
#pragma omp for
       for( qq=0 ; qq < nxyz ; qq++ ){
         ii = qq % nbqx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nbqx ;

         /* all 27=3x3x3 tensor products of basis functions */

         b0zb0yb0x = bq0z[kk]*bq0y[jj]*bq0x[ii]; b1zb0yb0x = bq1z[kk]*bq0y[jj]*bq0x[ii];
         b2zb0yb0x = bq2z[kk]*bq0y[jj]*bq0x[ii]; b0zb1yb0x = bq0z[kk]*bq1y[jj]*bq0x[ii];
         b1zb1yb0x = bq1z[kk]*bq1y[jj]*bq0x[ii]; b2zb1yb0x = bq2z[kk]*bq1y[jj]*bq0x[ii];
         b0zb2yb0x = bq0z[kk]*bq2y[jj]*bq0x[ii]; b1zb2yb0x = bq1z[kk]*bq2y[jj]*bq0x[ii];
         b2zb2yb0x = bq2z[kk]*bq2y[jj]*bq0x[ii]; b0zb0yb1x = bq0z[kk]*bq0y[jj]*bq1x[ii];
         b1zb0yb1x = bq1z[kk]*bq0y[jj]*bq1x[ii]; b2zb0yb1x = bq2z[kk]*bq0y[jj]*bq1x[ii];
         b0zb1yb1x = bq0z[kk]*bq1y[jj]*bq1x[ii]; b1zb1yb1x = bq1z[kk]*bq1y[jj]*bq1x[ii];
         b2zb1yb1x = bq2z[kk]*bq1y[jj]*bq1x[ii]; b0zb2yb1x = bq0z[kk]*bq2y[jj]*bq1x[ii];
         b1zb2yb1x = bq1z[kk]*bq2y[jj]*bq1x[ii]; b2zb2yb1x = bq2z[kk]*bq2y[jj]*bq1x[ii];
         b0zb0yb2x = bq0z[kk]*bq0y[jj]*bq2x[ii]; b1zb0yb2x = bq1z[kk]*bq0y[jj]*bq2x[ii];
         b2zb0yb2x = bq2z[kk]*bq0y[jj]*bq2x[ii]; b0zb1yb2x = bq0z[kk]*bq1y[jj]*bq2x[ii];
         b1zb1yb2x = bq1z[kk]*bq1y[jj]*bq2x[ii]; b2zb1yb2x = bq2z[kk]*bq1y[jj]*bq2x[ii];
         b0zb2yb2x = bq0z[kk]*bq2y[jj]*bq2x[ii]; b1zb2yb2x = bq1z[kk]*bq2y[jj]*bq2x[ii];
         b2zb2yb2x = bq2z[kk]*bq2y[jj]*bq2x[ii];

         if( dox ) xx[qq] = dxqi *
          (  b0zb0yb0x*xpar[ 0] + b1zb0yb0x*xpar[ 1] + b2zb0yb0x*xpar[ 2]
           + b0zb1yb0x*xpar[ 3] + b1zb1yb0x*xpar[ 4] + b2zb1yb0x*xpar[ 5]
           + b0zb2yb0x*xpar[ 6] + b1zb2yb0x*xpar[ 7] + b2zb2yb0x*xpar[ 8]
           + b0zb0yb1x*xpar[ 9] + b1zb0yb1x*xpar[10] + b2zb0yb1x*xpar[11]
           + b0zb1yb1x*xpar[12] + b1zb1yb1x*xpar[13] + b2zb1yb1x*xpar[14]
           + b0zb2yb1x*xpar[15] + b1zb2yb1x*xpar[16] + b2zb2yb1x*xpar[17]
           + b0zb0yb2x*xpar[18] + b1zb0yb2x*xpar[19] + b2zb0yb2x*xpar[20]
           + b0zb1yb2x*xpar[21] + b1zb1yb2x*xpar[22] + b2zb1yb2x*xpar[23]
           + b0zb2yb2x*xpar[24] + b1zb2yb2x*xpar[25] + b2zb2yb2x*xpar[26] ) ;
         if( doy ) yy[qq] = dyqi *
          (  b0zb0yb0x*ypar[ 0] + b1zb0yb0x*ypar[ 1] + b2zb0yb0x*ypar[ 2]
           + b0zb1yb0x*ypar[ 3] + b1zb1yb0x*ypar[ 4] + b2zb1yb0x*ypar[ 5]
           + b0zb2yb0x*ypar[ 6] + b1zb2yb0x*ypar[ 7] + b2zb2yb0x*ypar[ 8]
           + b0zb0yb1x*ypar[ 9] + b1zb0yb1x*ypar[10] + b2zb0yb1x*ypar[11]
           + b0zb1yb1x*ypar[12] + b1zb1yb1x*ypar[13] + b2zb1yb1x*ypar[14]
           + b0zb2yb1x*ypar[15] + b1zb2yb1x*ypar[16] + b2zb2yb1x*ypar[17]
           + b0zb0yb2x*ypar[18] + b1zb0yb2x*ypar[19] + b2zb0yb2x*ypar[20]
           + b0zb1yb2x*ypar[21] + b1zb1yb2x*ypar[22] + b2zb1yb2x*ypar[23]
           + b0zb2yb2x*ypar[24] + b1zb2yb2x*ypar[25] + b2zb2yb2x*ypar[26] ) ;
         if( doz ) zz[qq] = dzqi *
          (  b0zb0yb0x*zpar[ 0] + b1zb0yb0x*zpar[ 1] + b2zb0yb0x*zpar[ 2]
           + b0zb1yb0x*zpar[ 3] + b1zb1yb0x*zpar[ 4] + b2zb1yb0x*zpar[ 5]
           + b0zb2yb0x*zpar[ 6] + b1zb2yb0x*zpar[ 7] + b2zb2yb0x*zpar[ 8]
           + b0zb0yb1x*zpar[ 9] + b1zb0yb1x*zpar[10] + b2zb0yb1x*zpar[11]
           + b0zb1yb1x*zpar[12] + b1zb1yb1x*zpar[13] + b2zb1yb1x*zpar[14]
           + b0zb2yb1x*zpar[15] + b1zb2yb1x*zpar[16] + b2zb2yb1x*zpar[17]
           + b0zb0yb2x*zpar[18] + b1zb0yb2x*zpar[19] + b2zb0yb2x*zpar[20]
           + b0zb1yb2x*zpar[21] + b1zb1yb2x*zpar[22] + b2zb1yb2x*zpar[23]
           + b0zb2yb2x*zpar[24] + b1zb2yb2x*zpar[25] + b2zb2yb2x*zpar[26] ) ;
       } /* end of for loop */
     } /* end of parallel stuff */
     AFNI_OMP_END ;

   } else {   /*--------------------------------------------------------------*/

     AFNI_OMP_START ;
#pragma omp parallel
     { int qq ; float *xpar=par , *ypar=par+27 , *zpar=par+54 ;
       float b0zb0yb0x,b1zb0yb0x, b2zb0yb0x,b0zb1yb0x, b1zb1yb0x,b2zb1yb0x,
             b0zb2yb0x,b1zb2yb0x, b2zb2yb0x,b0zb0yb1x, b1zb0yb1x,b2zb0yb1x,
             b0zb1yb1x,b1zb1yb1x, b2zb1yb1x,b0zb2yb1x, b1zb2yb1x,b2zb2yb1x,
             b0zb0yb2x,b1zb0yb2x, b2zb0yb2x,b0zb1yb2x, b1zb1yb2x,b2zb1yb2x,
             b0zb2yb2x,b1zb2yb2x, b2zb2yb2x ;
#pragma omp for
       for( qq=0 ; qq < nxyz ; qq++ ){

         b0zb0yb0x = bbbqar[ 0][qq] ; b1zb0yb0x = bbbqar[ 1][qq] ; b2zb0yb0x = bbbqar[ 2][qq] ;
         b0zb1yb0x = bbbqar[ 3][qq] ; b1zb1yb0x = bbbqar[ 4][qq] ; b2zb1yb0x = bbbqar[ 5][qq] ;
         b0zb2yb0x = bbbqar[ 6][qq] ; b1zb2yb0x = bbbqar[ 7][qq] ; b2zb2yb0x = bbbqar[ 8][qq] ;
         b0zb0yb1x = bbbqar[ 9][qq] ; b1zb0yb1x = bbbqar[10][qq] ; b2zb0yb1x = bbbqar[11][qq] ;
         b0zb1yb1x = bbbqar[12][qq] ; b1zb1yb1x = bbbqar[13][qq] ; b2zb1yb1x = bbbqar[14][qq] ;
         b0zb2yb1x = bbbqar[15][qq] ; b1zb2yb1x = bbbqar[16][qq] ; b2zb2yb1x = bbbqar[17][qq] ;
         b0zb0yb2x = bbbqar[18][qq] ; b1zb0yb2x = bbbqar[19][qq] ; b2zb0yb2x = bbbqar[20][qq] ;
         b0zb1yb2x = bbbqar[21][qq] ; b1zb1yb2x = bbbqar[22][qq] ; b2zb1yb2x = bbbqar[23][qq] ;
         b0zb2yb2x = bbbqar[24][qq] ; b1zb2yb2x = bbbqar[25][qq] ; b2zb2yb2x = bbbqar[26][qq] ;

         if( dox ) xx[qq] = dxqi *
                            (  b0zb0yb0x*xpar[ 0] + b1zb0yb0x*xpar[ 1] + b2zb0yb0x*xpar[ 2]
                             + b0zb1yb0x*xpar[ 3] + b1zb1yb0x*xpar[ 4] + b2zb1yb0x*xpar[ 5]
                             + b0zb2yb0x*xpar[ 6] + b1zb2yb0x*xpar[ 7] + b2zb2yb0x*xpar[ 8]
                             + b0zb0yb1x*xpar[ 9] + b1zb0yb1x*xpar[10] + b2zb0yb1x*xpar[11]
                             + b0zb1yb1x*xpar[12] + b1zb1yb1x*xpar[13] + b2zb1yb1x*xpar[14]
                             + b0zb2yb1x*xpar[15] + b1zb2yb1x*xpar[16] + b2zb2yb1x*xpar[17]
                             + b0zb0yb2x*xpar[18] + b1zb0yb2x*xpar[19] + b2zb0yb2x*xpar[20]
                             + b0zb1yb2x*xpar[21] + b1zb1yb2x*xpar[22] + b2zb1yb2x*xpar[23]
                             + b0zb2yb2x*xpar[24] + b1zb2yb2x*xpar[25] + b2zb2yb2x*xpar[26] ) ;
         if( doy ) yy[qq] = dyqi *
                            (  b0zb0yb0x*ypar[ 0] + b1zb0yb0x*ypar[ 1] + b2zb0yb0x*ypar[ 2]
                             + b0zb1yb0x*ypar[ 3] + b1zb1yb0x*ypar[ 4] + b2zb1yb0x*ypar[ 5]
                             + b0zb2yb0x*ypar[ 6] + b1zb2yb0x*ypar[ 7] + b2zb2yb0x*ypar[ 8]
                             + b0zb0yb1x*ypar[ 9] + b1zb0yb1x*ypar[10] + b2zb0yb1x*ypar[11]
                             + b0zb1yb1x*ypar[12] + b1zb1yb1x*ypar[13] + b2zb1yb1x*ypar[14]
                             + b0zb2yb1x*ypar[15] + b1zb2yb1x*ypar[16] + b2zb2yb1x*ypar[17]
                             + b0zb0yb2x*ypar[18] + b1zb0yb2x*ypar[19] + b2zb0yb2x*ypar[20]
                             + b0zb1yb2x*ypar[21] + b1zb1yb2x*ypar[22] + b2zb1yb2x*ypar[23]
                             + b0zb2yb2x*ypar[24] + b1zb2yb2x*ypar[25] + b2zb2yb2x*ypar[26] ) ;
         if( doz ) zz[qq] = dzqi *
                            (  b0zb0yb0x*zpar[ 0] + b1zb0yb0x*zpar[ 1] + b2zb0yb0x*zpar[ 2]
                             + b0zb1yb0x*zpar[ 3] + b1zb1yb0x*zpar[ 4] + b2zb1yb0x*zpar[ 5]
                             + b0zb2yb0x*zpar[ 6] + b1zb2yb0x*zpar[ 7] + b2zb2yb0x*zpar[ 8]
                             + b0zb0yb1x*zpar[ 9] + b1zb0yb1x*zpar[10] + b2zb0yb1x*zpar[11]
                             + b0zb1yb1x*zpar[12] + b1zb1yb1x*zpar[13] + b2zb1yb1x*zpar[14]
                             + b0zb2yb1x*zpar[15] + b1zb2yb1x*zpar[16] + b2zb2yb1x*zpar[17]
                             + b0zb0yb2x*zpar[18] + b1zb0yb2x*zpar[19] + b2zb0yb2x*zpar[20]
                             + b0zb1yb2x*zpar[21] + b1zb1yb2x*zpar[22] + b2zb1yb2x*zpar[23]
                             + b0zb2yb2x*zpar[24] + b1zb2yb2x*zpar[25] + b2zb2yb2x*zpar[26] ) ;
       } /* end of for loop */
     }
     AFNI_OMP_END ;

   }

   EXRETURN ;
}

#else /* not USE_HLOADER */  /*HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH*/

/*** Compute warp displacements at one voxel (index = qq);
     There are 4 (2x2) routines here:
       for cubic and quintic;
       for using 1D basis arrays or 3D basis arrays. **/

/*** Add routine for basis5 functions,
     which always use 3D basis arrays [05 Nov 2015] **/

/*-----=====-----=====-----=====-----=====-----=====-----=====-----=====-----*/
/* evaluate cubic warp the slower way (from 1D basis arrays) */

static void HCwarp_eval_A( int qq , float *xx , float *yy , float *zz )
{
   int ii,jj,kk ;
   float b0zb0yb0x,b1zb0yb0x, b0zb1yb0x,b1zb1yb0x,
         b0zb0yb1x,b1zb0yb1x, b0zb1yb1x,b1zb1yb1x ;

   /* in this function, the 'b' values (3D warp components) are
      evaluated as tensor products of the underlying 1D functions */

   /* convert 1D index to 3D index */

   ii = qq % nbcx ; kk = qq / nbcxy ; jj = (qq-kk*nbcxy) / nbcx ;

   /* multiply 1D basis functions to get 3D basis functions (2x2x2=8 of them) */

   b0zb0yb0x = bc0z[kk]*bc0y[jj]*bc0x[ii]; b1zb0yb0x = bc1z[kk]*bc0y[jj]*bc0x[ii];
   b0zb1yb0x = bc0z[kk]*bc1y[jj]*bc0x[ii]; b1zb1yb0x = bc1z[kk]*bc1y[jj]*bc0x[ii];
   b0zb0yb1x = bc0z[kk]*bc0y[jj]*bc1x[ii]; b1zb0yb1x = bc1z[kk]*bc0y[jj]*bc1x[ii];
   b0zb1yb1x = bc0z[kk]*bc1y[jj]*bc1x[ii]; b1zb1yb1x = bc1z[kk]*bc1y[jj]*bc1x[ii];

   /* multiply by parameters and sum up */

   if( Hdox ) *xx = dxci *
                  (  b0zb0yb0x*Hxpar[0] + b1zb0yb0x*Hxpar[1] + b0zb1yb0x*Hxpar[2]
                   + b1zb1yb0x*Hxpar[3] + b0zb0yb1x*Hxpar[4] + b1zb0yb1x*Hxpar[5]
                   + b0zb1yb1x*Hxpar[6] + b1zb1yb1x*Hxpar[7]                     ) ; else *xx = 0.0f ;
   if( Hdoy ) *yy = dyci *
                  (  b0zb0yb0x*Hypar[0] + b1zb0yb0x*Hypar[1] + b0zb1yb0x*Hypar[2]
                   + b1zb1yb0x*Hypar[3] + b0zb0yb1x*Hypar[4] + b1zb0yb1x*Hypar[5]
                   + b0zb1yb1x*Hypar[6] + b1zb1yb1x*Hypar[7]                     ) ; else *yy = 0.0f ;
   if( Hdoz ) *zz = dzci *
                  (  b0zb0yb0x*Hzpar[0] + b1zb0yb0x*Hzpar[1] + b0zb1yb0x*Hzpar[2]
                   + b1zb1yb0x*Hzpar[3] + b0zb0yb1x*Hzpar[4] + b1zb0yb1x*Hzpar[5]
                   + b0zb1yb1x*Hzpar[6] + b1zb1yb1x*Hzpar[7]                     ) ; else *zz = 0.0f ;
   return ;
}

/*----------------------------------------------------------------------------*/
/* evaluate cubic warp the faster way (from pre-computed 3D basis arrays) */

static void HCwarp_eval_B( int qq , float *xx , float *yy , float *zz )
{
   float b0zb0yb0x,b1zb0yb0x, b0zb1yb0x,b1zb1yb0x,
         b0zb0yb1x,b1zb0yb1x, b0zb1yb1x,b1zb1yb1x ;

   /* in this function, the 8 'b' values (warp components) were
      pre-evaluated in the bbbcar arrays, and so just need to
      be extracted; this method is faster, but obviously takes more memory */

   b0zb0yb0x = bbbcar[0][qq] ; b1zb0yb0x = bbbcar[1][qq] ;
   b0zb1yb0x = bbbcar[2][qq] ; b1zb1yb0x = bbbcar[3][qq] ;
   b0zb0yb1x = bbbcar[4][qq] ; b1zb0yb1x = bbbcar[5][qq] ;
   b0zb1yb1x = bbbcar[6][qq] ; b1zb1yb1x = bbbcar[7][qq] ;

   if( Hdox ) *xx = dxci *
                  (  b0zb0yb0x*Hxpar[0] + b1zb0yb0x*Hxpar[1] + b0zb1yb0x*Hxpar[2]
                   + b1zb1yb0x*Hxpar[3] + b0zb0yb1x*Hxpar[4] + b1zb0yb1x*Hxpar[5]
                   + b0zb1yb1x*Hxpar[6] + b1zb1yb1x*Hxpar[7]                     ) ; else *xx = 0.0f ;
   if( Hdoy ) *yy = dyci *
                  (  b0zb0yb0x*Hypar[0] + b1zb0yb0x*Hypar[1] + b0zb1yb0x*Hypar[2]
                   + b1zb1yb0x*Hypar[3] + b0zb0yb1x*Hypar[4] + b1zb0yb1x*Hypar[5]
                   + b0zb1yb1x*Hypar[6] + b1zb1yb1x*Hypar[7]                     ) ; else *yy = 0.0f ;
   if( Hdoz ) *zz = dzci *
                  (  b0zb0yb0x*Hzpar[0] + b1zb0yb0x*Hzpar[1] + b0zb1yb0x*Hzpar[2]
                   + b1zb1yb0x*Hzpar[3] + b0zb0yb1x*Hzpar[4] + b1zb0yb1x*Hzpar[5]
                   + b0zb1yb1x*Hzpar[6] + b1zb1yb1x*Hzpar[7]                     ) ; else *zz = 0.0f ;
   return ;
}

/*-----=====-----=====-----=====-----=====-----=====-----=====-----=====-----*/
/* evaluate quintic warp the slower way (from 1D basis arrays) */

static void HQwarp_eval_A( int qq , float *xx , float *yy , float *zz )
{
   int ii,jj,kk ;
   float b0zb0yb0x,b1zb0yb0x, b2zb0yb0x,b0zb1yb0x, b1zb1yb0x,b2zb1yb0x,
         b0zb2yb0x,b1zb2yb0x, b2zb2yb0x,b0zb0yb1x, b1zb0yb1x,b2zb0yb1x,
         b0zb1yb1x,b1zb1yb1x, b2zb1yb1x,b0zb2yb1x, b1zb2yb1x,b2zb2yb1x,
         b0zb0yb2x,b1zb0yb2x, b2zb0yb2x,b0zb1yb2x, b1zb1yb2x,b2zb1yb2x,
         b0zb2yb2x,b1zb2yb2x, b2zb2yb2x ;

   ii = qq % nbqx; kk = qq / nbqxy; jj = (qq-kk*nbqxy) / nbqx; /* get 3D index */

   /* multiply 3x3x3 1D basis functions to get the 27 3D basis functions */

   b0zb0yb0x = bq0z[kk]*bq0y[jj]*bq0x[ii]; b1zb0yb0x = bq1z[kk]*bq0y[jj]*bq0x[ii];
   b2zb0yb0x = bq2z[kk]*bq0y[jj]*bq0x[ii]; b0zb1yb0x = bq0z[kk]*bq1y[jj]*bq0x[ii];
   b1zb1yb0x = bq1z[kk]*bq1y[jj]*bq0x[ii]; b2zb1yb0x = bq2z[kk]*bq1y[jj]*bq0x[ii];
   b0zb2yb0x = bq0z[kk]*bq2y[jj]*bq0x[ii]; b1zb2yb0x = bq1z[kk]*bq2y[jj]*bq0x[ii];
   b2zb2yb0x = bq2z[kk]*bq2y[jj]*bq0x[ii]; b0zb0yb1x = bq0z[kk]*bq0y[jj]*bq1x[ii];
   b1zb0yb1x = bq1z[kk]*bq0y[jj]*bq1x[ii]; b2zb0yb1x = bq2z[kk]*bq0y[jj]*bq1x[ii];
   b0zb1yb1x = bq0z[kk]*bq1y[jj]*bq1x[ii]; b1zb1yb1x = bq1z[kk]*bq1y[jj]*bq1x[ii];
   b2zb1yb1x = bq2z[kk]*bq1y[jj]*bq1x[ii]; b0zb2yb1x = bq0z[kk]*bq2y[jj]*bq1x[ii];
   b1zb2yb1x = bq1z[kk]*bq2y[jj]*bq1x[ii]; b2zb2yb1x = bq2z[kk]*bq2y[jj]*bq1x[ii];
   b0zb0yb2x = bq0z[kk]*bq0y[jj]*bq2x[ii]; b1zb0yb2x = bq1z[kk]*bq0y[jj]*bq2x[ii];
   b2zb0yb2x = bq2z[kk]*bq0y[jj]*bq2x[ii]; b0zb1yb2x = bq0z[kk]*bq1y[jj]*bq2x[ii];
   b1zb1yb2x = bq1z[kk]*bq1y[jj]*bq2x[ii]; b2zb1yb2x = bq2z[kk]*bq1y[jj]*bq2x[ii];
   b0zb2yb2x = bq0z[kk]*bq2y[jj]*bq2x[ii]; b1zb2yb2x = bq1z[kk]*bq2y[jj]*bq2x[ii];
   b2zb2yb2x = bq2z[kk]*bq2y[jj]*bq2x[ii];

   /* multiply by parameters and add up */

   if( Hdox ) *xx = dxqi *
          (  b0zb0yb0x*Hxpar[ 0] + b1zb0yb0x*Hxpar[ 1] + b2zb0yb0x*Hxpar[ 2]
           + b0zb1yb0x*Hxpar[ 3] + b1zb1yb0x*Hxpar[ 4] + b2zb1yb0x*Hxpar[ 5]
           + b0zb2yb0x*Hxpar[ 6] + b1zb2yb0x*Hxpar[ 7] + b2zb2yb0x*Hxpar[ 8]
           + b0zb0yb1x*Hxpar[ 9] + b1zb0yb1x*Hxpar[10] + b2zb0yb1x*Hxpar[11]
           + b0zb1yb1x*Hxpar[12] + b1zb1yb1x*Hxpar[13] + b2zb1yb1x*Hxpar[14]
           + b0zb2yb1x*Hxpar[15] + b1zb2yb1x*Hxpar[16] + b2zb2yb1x*Hxpar[17]
           + b0zb0yb2x*Hxpar[18] + b1zb0yb2x*Hxpar[19] + b2zb0yb2x*Hxpar[20]
           + b0zb1yb2x*Hxpar[21] + b1zb1yb2x*Hxpar[22] + b2zb1yb2x*Hxpar[23]
           + b0zb2yb2x*Hxpar[24] + b1zb2yb2x*Hxpar[25] + b2zb2yb2x*Hxpar[26] ) ; else *xx = 0.0f ;
   if( Hdoy ) *yy = dyqi *
          (  b0zb0yb0x*Hypar[ 0] + b1zb0yb0x*Hypar[ 1] + b2zb0yb0x*Hypar[ 2]
           + b0zb1yb0x*Hypar[ 3] + b1zb1yb0x*Hypar[ 4] + b2zb1yb0x*Hypar[ 5]
           + b0zb2yb0x*Hypar[ 6] + b1zb2yb0x*Hypar[ 7] + b2zb2yb0x*Hypar[ 8]
           + b0zb0yb1x*Hypar[ 9] + b1zb0yb1x*Hypar[10] + b2zb0yb1x*Hypar[11]
           + b0zb1yb1x*Hypar[12] + b1zb1yb1x*Hypar[13] + b2zb1yb1x*Hypar[14]
           + b0zb2yb1x*Hypar[15] + b1zb2yb1x*Hypar[16] + b2zb2yb1x*Hypar[17]
           + b0zb0yb2x*Hypar[18] + b1zb0yb2x*Hypar[19] + b2zb0yb2x*Hypar[20]
           + b0zb1yb2x*Hypar[21] + b1zb1yb2x*Hypar[22] + b2zb1yb2x*Hypar[23]
           + b0zb2yb2x*Hypar[24] + b1zb2yb2x*Hypar[25] + b2zb2yb2x*Hypar[26] ) ; else *yy = 0.0f ;
   if( Hdoz ) *zz = dzqi *
          (  b0zb0yb0x*Hzpar[ 0] + b1zb0yb0x*Hzpar[ 1] + b2zb0yb0x*Hzpar[ 2]
           + b0zb1yb0x*Hzpar[ 3] + b1zb1yb0x*Hzpar[ 4] + b2zb1yb0x*Hzpar[ 5]
           + b0zb2yb0x*Hzpar[ 6] + b1zb2yb0x*Hzpar[ 7] + b2zb2yb0x*Hzpar[ 8]
           + b0zb0yb1x*Hzpar[ 9] + b1zb0yb1x*Hzpar[10] + b2zb0yb1x*Hzpar[11]
           + b0zb1yb1x*Hzpar[12] + b1zb1yb1x*Hzpar[13] + b2zb1yb1x*Hzpar[14]
           + b0zb2yb1x*Hzpar[15] + b1zb2yb1x*Hzpar[16] + b2zb2yb1x*Hzpar[17]
           + b0zb0yb2x*Hzpar[18] + b1zb0yb2x*Hzpar[19] + b2zb0yb2x*Hzpar[20]
           + b0zb1yb2x*Hzpar[21] + b1zb1yb2x*Hzpar[22] + b2zb1yb2x*Hzpar[23]
           + b0zb2yb2x*Hzpar[24] + b1zb2yb2x*Hzpar[25] + b2zb2yb2x*Hzpar[26] ) ; else *zz = 0.0f ;
   return ;
}

/*----------------------------------------------------------------------------*/
/* evaluate quintic warp the faster way (from 3D basis arrays) */

static void HQwarp_eval_B( int qq , float *xx , float *yy , float *zz )
{

#if 1
   float t1,t2,t3 ; int jj ;

   t1 = t2 = t3 = 0.0f ;
   for( jj=0 ; jj < 27 ; jj+=3 ){
     t1 += bbbqar[jj][qq]*Hxpar[jj] + bbbqar[jj+1][qq]*Hxpar[jj+1] + bbbqar[jj+2][qq]*Hxpar[jj+2] ;
     t2 += bbbqar[jj][qq]*Hypar[jj] + bbbqar[jj+1][qq]*Hypar[jj+1] + bbbqar[jj+2][qq]*Hypar[jj+2] ;
     t3 += bbbqar[jj][qq]*Hzpar[jj] + bbbqar[jj+1][qq]*Hzpar[jj+1] + bbbqar[jj+2][qq]*Hzpar[jj+2] ;
   }
   *xx = (Hdox) ? t1 : 0.0f ;
   *yy = (Hdoy) ? t2 : 0.0f ;
   *zz = (Hdoz) ? t3 : 0.0f ;
#else
   float b0zb0yb0x,b1zb0yb0x, b2zb0yb0x,b0zb1yb0x, b1zb1yb0x,b2zb1yb0x,
         b0zb2yb0x,b1zb2yb0x, b2zb2yb0x,b0zb0yb1x, b1zb0yb1x,b2zb0yb1x,
         b0zb1yb1x,b1zb1yb1x, b2zb1yb1x,b0zb2yb1x, b1zb2yb1x,b2zb2yb1x,
         b0zb0yb2x,b1zb0yb2x, b2zb0yb2x,b0zb1yb2x, b1zb1yb2x,b2zb1yb2x,
         b0zb2yb2x,b1zb2yb2x, b2zb2yb2x ;

   b0zb0yb0x = bbbqar[ 0][qq] ; b1zb0yb0x = bbbqar[ 1][qq] ; b2zb0yb0x = bbbqar[ 2][qq] ;
   b0zb1yb0x = bbbqar[ 3][qq] ; b1zb1yb0x = bbbqar[ 4][qq] ; b2zb1yb0x = bbbqar[ 5][qq] ;
   b0zb2yb0x = bbbqar[ 6][qq] ; b1zb2yb0x = bbbqar[ 7][qq] ; b2zb2yb0x = bbbqar[ 8][qq] ;
   b0zb0yb1x = bbbqar[ 9][qq] ; b1zb0yb1x = bbbqar[10][qq] ; b2zb0yb1x = bbbqar[11][qq] ;
   b0zb1yb1x = bbbqar[12][qq] ; b1zb1yb1x = bbbqar[13][qq] ; b2zb1yb1x = bbbqar[14][qq] ;
   b0zb2yb1x = bbbqar[15][qq] ; b1zb2yb1x = bbbqar[16][qq] ; b2zb2yb1x = bbbqar[17][qq] ;
   b0zb0yb2x = bbbqar[18][qq] ; b1zb0yb2x = bbbqar[19][qq] ; b2zb0yb2x = bbbqar[20][qq] ;
   b0zb1yb2x = bbbqar[21][qq] ; b1zb1yb2x = bbbqar[22][qq] ; b2zb1yb2x = bbbqar[23][qq] ;
   b0zb2yb2x = bbbqar[24][qq] ; b1zb2yb2x = bbbqar[25][qq] ; b2zb2yb2x = bbbqar[26][qq] ;

   if( Hdox ) *xx = dxqi *
          (  b0zb0yb0x*Hxpar[ 0] + b1zb0yb0x*Hxpar[ 1] + b2zb0yb0x*Hxpar[ 2]
           + b0zb1yb0x*Hxpar[ 3] + b1zb1yb0x*Hxpar[ 4] + b2zb1yb0x*Hxpar[ 5]
           + b0zb2yb0x*Hxpar[ 6] + b1zb2yb0x*Hxpar[ 7] + b2zb2yb0x*Hxpar[ 8]
           + b0zb0yb1x*Hxpar[ 9] + b1zb0yb1x*Hxpar[10] + b2zb0yb1x*Hxpar[11]
           + b0zb1yb1x*Hxpar[12] + b1zb1yb1x*Hxpar[13] + b2zb1yb1x*Hxpar[14]
           + b0zb2yb1x*Hxpar[15] + b1zb2yb1x*Hxpar[16] + b2zb2yb1x*Hxpar[17]
           + b0zb0yb2x*Hxpar[18] + b1zb0yb2x*Hxpar[19] + b2zb0yb2x*Hxpar[20]
           + b0zb1yb2x*Hxpar[21] + b1zb1yb2x*Hxpar[22] + b2zb1yb2x*Hxpar[23]
           + b0zb2yb2x*Hxpar[24] + b1zb2yb2x*Hxpar[25] + b2zb2yb2x*Hxpar[26] ) ; else *xx = 0.0f ;
   if( Hdoy ) *yy = dyqi *
          (  b0zb0yb0x*Hypar[ 0] + b1zb0yb0x*Hypar[ 1] + b2zb0yb0x*Hypar[ 2]
           + b0zb1yb0x*Hypar[ 3] + b1zb1yb0x*Hypar[ 4] + b2zb1yb0x*Hypar[ 5]
           + b0zb2yb0x*Hypar[ 6] + b1zb2yb0x*Hypar[ 7] + b2zb2yb0x*Hypar[ 8]
           + b0zb0yb1x*Hypar[ 9] + b1zb0yb1x*Hypar[10] + b2zb0yb1x*Hypar[11]
           + b0zb1yb1x*Hypar[12] + b1zb1yb1x*Hypar[13] + b2zb1yb1x*Hypar[14]
           + b0zb2yb1x*Hypar[15] + b1zb2yb1x*Hypar[16] + b2zb2yb1x*Hypar[17]
           + b0zb0yb2x*Hypar[18] + b1zb0yb2x*Hypar[19] + b2zb0yb2x*Hypar[20]
           + b0zb1yb2x*Hypar[21] + b1zb1yb2x*Hypar[22] + b2zb1yb2x*Hypar[23]
           + b0zb2yb2x*Hypar[24] + b1zb2yb2x*Hypar[25] + b2zb2yb2x*Hypar[26] ) ; else *yy = 0.0f ;
   if( Hdoz ) *zz = dzqi *
          (  b0zb0yb0x*Hzpar[ 0] + b1zb0yb0x*Hzpar[ 1] + b2zb0yb0x*Hzpar[ 2]
           + b0zb1yb0x*Hzpar[ 3] + b1zb1yb0x*Hzpar[ 4] + b2zb1yb0x*Hzpar[ 5]
           + b0zb2yb0x*Hzpar[ 6] + b1zb2yb0x*Hzpar[ 7] + b2zb2yb0x*Hzpar[ 8]
           + b0zb0yb1x*Hzpar[ 9] + b1zb0yb1x*Hzpar[10] + b2zb0yb1x*Hzpar[11]
           + b0zb1yb1x*Hzpar[12] + b1zb1yb1x*Hzpar[13] + b2zb1yb1x*Hzpar[14]
           + b0zb2yb1x*Hzpar[15] + b1zb2yb1x*Hzpar[16] + b2zb2yb1x*Hzpar[17]
           + b0zb0yb2x*Hzpar[18] + b1zb0yb2x*Hzpar[19] + b2zb0yb2x*Hzpar[20]
           + b0zb1yb2x*Hzpar[21] + b1zb1yb2x*Hzpar[22] + b2zb1yb2x*Hzpar[23]
           + b0zb2yb2x*Hzpar[24] + b1zb2yb2x*Hzpar[25] + b2zb2yb2x*Hzpar[26] ) ; else *zz = 0.0f ;
#endif

   return ;
}

#ifdef ALLOW_BASIS5
/*----------------------------------------------------------------------------*/
/* The _basis3, _basis4, _basis5 functions below had their basic elements
   generated by the following tcsh script, followed by manual editing to
   make things look good.  The example below is for basis5 (nb5=5) -- the way
   3dQwarp is set up at present, only basis5 can be executed (-5final).

       #!/bin/tcsh

       set nb5 = 5
       @ nbb = $nb5 - 1

       echo "float"
       foreach rr ( `count -dig 1 0 $nbb` )
         foreach qq ( `count -dig 1 0 $nbb` )
           foreach pp ( `count -dig 1 0 $nbb` )
             echo " b${pp}zb${qq}yb${rr}x,"
           end
         end
       end

       echo

       foreach rr ( `count -dig 1 0 $nbb` )
         foreach qq ( `count -dig 1 0 $nbb` )
           foreach pp ( `count -dig 1 0 $nbb` )
             @ ss = $pp + $qq * $nb5 + $rr * $nb5 * $nb5
             echo "b${pp}zb${qq}yb${rr}x = bbbcar[$ss][qq] ;"
           end
         end
       end

       echo

       foreach rr ( `count -dig 1 0 $nbb` )
         foreach qq ( `count -dig 1 0 $nbb` )
           foreach pp ( `count -dig 1 0 $nbb` )
             @ ss = $pp + $qq * $nb5 + $rr * $nb5 * $nb5
             echo " + b${pp}zb${qq}yb${rr}x*Hxpar[$ss]"
           end
         end
       end
*//*--------------------------------------------------------------------------*/

/*............................................................................*/
/* evaluate basis3 warp the faster way (from 3D basis arrays) */

static void HCwarp_eval_B_basis3( int qq , float *xx , float *yy , float *zz )
{
   float b0zb0yb0x,b1zb0yb0x, b2zb0yb0x,b0zb1yb0x, b1zb1yb0x,b2zb1yb0x,
         b0zb2yb0x,b1zb2yb0x, b2zb2yb0x,b0zb0yb1x, b1zb0yb1x,b2zb0yb1x,
         b0zb1yb1x,b1zb1yb1x, b2zb1yb1x,b0zb2yb1x, b1zb2yb1x,b2zb2yb1x,
         b0zb0yb2x,b1zb0yb2x, b2zb0yb2x,b0zb1yb2x, b1zb1yb2x,b2zb1yb2x,
         b0zb2yb2x,b1zb2yb2x, b2zb2yb2x ;

   b0zb0yb0x = bbbcar[ 0][qq] ; b1zb0yb0x = bbbcar[ 1][qq] ; b2zb0yb0x = bbbcar[ 2][qq] ;
   b0zb1yb0x = bbbcar[ 3][qq] ; b1zb1yb0x = bbbcar[ 4][qq] ; b2zb1yb0x = bbbcar[ 5][qq] ;
   b0zb2yb0x = bbbcar[ 6][qq] ; b1zb2yb0x = bbbcar[ 7][qq] ; b2zb2yb0x = bbbcar[ 8][qq] ;
   b0zb0yb1x = bbbcar[ 9][qq] ; b1zb0yb1x = bbbcar[10][qq] ; b2zb0yb1x = bbbcar[11][qq] ;
   b0zb1yb1x = bbbcar[12][qq] ; b1zb1yb1x = bbbcar[13][qq] ; b2zb1yb1x = bbbcar[14][qq] ;
   b0zb2yb1x = bbbcar[15][qq] ; b1zb2yb1x = bbbcar[16][qq] ; b2zb2yb1x = bbbcar[17][qq] ;
   b0zb0yb2x = bbbcar[18][qq] ; b1zb0yb2x = bbbcar[19][qq] ; b2zb0yb2x = bbbcar[20][qq] ;
   b0zb1yb2x = bbbcar[21][qq] ; b1zb1yb2x = bbbcar[22][qq] ; b2zb1yb2x = bbbcar[23][qq] ;
   b0zb2yb2x = bbbcar[24][qq] ; b1zb2yb2x = bbbcar[25][qq] ; b2zb2yb2x = bbbcar[26][qq] ;

   if( Hdox ) *xx = dxci *
          (  b0zb0yb0x*Hxpar[ 0] + b1zb0yb0x*Hxpar[ 1] + b2zb0yb0x*Hxpar[ 2]
           + b0zb1yb0x*Hxpar[ 3] + b1zb1yb0x*Hxpar[ 4] + b2zb1yb0x*Hxpar[ 5]
           + b0zb2yb0x*Hxpar[ 6] + b1zb2yb0x*Hxpar[ 7] + b2zb2yb0x*Hxpar[ 8]
           + b0zb0yb1x*Hxpar[ 9] + b1zb0yb1x*Hxpar[10] + b2zb0yb1x*Hxpar[11]
           + b0zb1yb1x*Hxpar[12] + b1zb1yb1x*Hxpar[13] + b2zb1yb1x*Hxpar[14]
           + b0zb2yb1x*Hxpar[15] + b1zb2yb1x*Hxpar[16] + b2zb2yb1x*Hxpar[17]
           + b0zb0yb2x*Hxpar[18] + b1zb0yb2x*Hxpar[19] + b2zb0yb2x*Hxpar[20]
           + b0zb1yb2x*Hxpar[21] + b1zb1yb2x*Hxpar[22] + b2zb1yb2x*Hxpar[23]
           + b0zb2yb2x*Hxpar[24] + b1zb2yb2x*Hxpar[25] + b2zb2yb2x*Hxpar[26] ) ; else *xx = 0.0f ;
   if( Hdoy ) *yy = dyci *
          (  b0zb0yb0x*Hypar[ 0] + b1zb0yb0x*Hypar[ 1] + b2zb0yb0x*Hypar[ 2]
           + b0zb1yb0x*Hypar[ 3] + b1zb1yb0x*Hypar[ 4] + b2zb1yb0x*Hypar[ 5]
           + b0zb2yb0x*Hypar[ 6] + b1zb2yb0x*Hypar[ 7] + b2zb2yb0x*Hypar[ 8]
           + b0zb0yb1x*Hypar[ 9] + b1zb0yb1x*Hypar[10] + b2zb0yb1x*Hypar[11]
           + b0zb1yb1x*Hypar[12] + b1zb1yb1x*Hypar[13] + b2zb1yb1x*Hypar[14]
           + b0zb2yb1x*Hypar[15] + b1zb2yb1x*Hypar[16] + b2zb2yb1x*Hypar[17]
           + b0zb0yb2x*Hypar[18] + b1zb0yb2x*Hypar[19] + b2zb0yb2x*Hypar[20]
           + b0zb1yb2x*Hypar[21] + b1zb1yb2x*Hypar[22] + b2zb1yb2x*Hypar[23]
           + b0zb2yb2x*Hypar[24] + b1zb2yb2x*Hypar[25] + b2zb2yb2x*Hypar[26] ) ; else *yy = 0.0f ;
   if( Hdoz ) *zz = dzci *
          (  b0zb0yb0x*Hzpar[ 0] + b1zb0yb0x*Hzpar[ 1] + b2zb0yb0x*Hzpar[ 2]
           + b0zb1yb0x*Hzpar[ 3] + b1zb1yb0x*Hzpar[ 4] + b2zb1yb0x*Hzpar[ 5]
           + b0zb2yb0x*Hzpar[ 6] + b1zb2yb0x*Hzpar[ 7] + b2zb2yb0x*Hzpar[ 8]
           + b0zb0yb1x*Hzpar[ 9] + b1zb0yb1x*Hzpar[10] + b2zb0yb1x*Hzpar[11]
           + b0zb1yb1x*Hzpar[12] + b1zb1yb1x*Hzpar[13] + b2zb1yb1x*Hzpar[14]
           + b0zb2yb1x*Hzpar[15] + b1zb2yb1x*Hzpar[16] + b2zb2yb1x*Hzpar[17]
           + b0zb0yb2x*Hzpar[18] + b1zb0yb2x*Hzpar[19] + b2zb0yb2x*Hzpar[20]
           + b0zb1yb2x*Hzpar[21] + b1zb1yb2x*Hzpar[22] + b2zb1yb2x*Hzpar[23]
           + b0zb2yb2x*Hzpar[24] + b1zb2yb2x*Hzpar[25] + b2zb2yb2x*Hzpar[26] ) ; else *zz = 0.0f ;
   return ;
}

/*............................................................................*/
/* evaluate basis4 warp the faster way (from 3D basis arrays) */

static void HCwarp_eval_B_basis4( int qin , float *xx , float *yy , float *zz )
{
   float t1,t2,t3,t4,t5,t6,t7 ; int qq=qin , jj ;

#if 1
   t1 = t2 = t3 = 0.0f ;
   for( jj=0 ; jj < 64 ; jj+=2 ){
     t1 += bbbcar[jj][qq]*Hxpar[jj] + bbbcar[jj+1][qq]*Hxpar[jj+1] ;
     t2 += bbbcar[jj][qq]*Hypar[jj] + bbbcar[jj+1][qq]*Hypar[jj+1] ;
     t3 += bbbcar[jj][qq]*Hzpar[jj] + bbbcar[jj+1][qq]*Hzpar[jj+1] ;
   }
   *xx = (Hdox) ? t1 : 0.0f ;
   *yy = (Hdoy) ? t2 : 0.0f ;
   *zz = (Hdoz) ? t3 : 0.0f ;

#else
   if( Hdox ){
     t1 =  bbbcar[ 0][qq]*Hxpar[ 0] + bbbcar[ 1][qq]*Hxpar[ 1] + bbbcar[ 2][qq]*Hxpar[ 2]
         + bbbcar[ 3][qq]*Hxpar[ 3] + bbbcar[ 4][qq]*Hxpar[ 4] + bbbcar[ 5][qq]*Hxpar[ 5]
         + bbbcar[ 6][qq]*Hxpar[ 6] + bbbcar[ 7][qq]*Hxpar[ 7] + bbbcar[ 8][qq]*Hxpar[ 8] ;
     t2 =  bbbcar[ 9][qq]*Hxpar[ 9] + bbbcar[10][qq]*Hxpar[10] + bbbcar[11][qq]*Hxpar[11]
         + bbbcar[12][qq]*Hxpar[12] + bbbcar[13][qq]*Hxpar[13] + bbbcar[14][qq]*Hxpar[14]
         + bbbcar[15][qq]*Hxpar[15] + bbbcar[16][qq]*Hxpar[16] + bbbcar[17][qq]*Hxpar[17] ;
     t3 =  bbbcar[18][qq]*Hxpar[18] + bbbcar[19][qq]*Hxpar[19] + bbbcar[20][qq]*Hxpar[20]
         + bbbcar[21][qq]*Hxpar[21] + bbbcar[22][qq]*Hxpar[22] + bbbcar[23][qq]*Hxpar[23]
         + bbbcar[24][qq]*Hxpar[24] + bbbcar[25][qq]*Hxpar[25] + bbbcar[26][qq]*Hxpar[26] ;
     t4 =  bbbcar[27][qq]*Hxpar[27] + bbbcar[28][qq]*Hxpar[28] + bbbcar[29][qq]*Hxpar[29]
         + bbbcar[30][qq]*Hxpar[30] + bbbcar[31][qq]*Hxpar[31] + bbbcar[32][qq]*Hxpar[32]
         + bbbcar[33][qq]*Hxpar[33] + bbbcar[34][qq]*Hxpar[34] + bbbcar[35][qq]*Hxpar[35] ;
     t5 =  bbbcar[36][qq]*Hxpar[36] + bbbcar[37][qq]*Hxpar[37] + bbbcar[38][qq]*Hxpar[38]
         + bbbcar[39][qq]*Hxpar[39] + bbbcar[40][qq]*Hxpar[40] + bbbcar[41][qq]*Hxpar[41]
         + bbbcar[42][qq]*Hxpar[42] + bbbcar[43][qq]*Hxpar[43] + bbbcar[44][qq]*Hxpar[44] ;
     t6 =  bbbcar[45][qq]*Hxpar[45] + bbbcar[46][qq]*Hxpar[46] + bbbcar[47][qq]*Hxpar[47]
         + bbbcar[48][qq]*Hxpar[48] + bbbcar[49][qq]*Hxpar[49] + bbbcar[50][qq]*Hxpar[50]
         + bbbcar[51][qq]*Hxpar[51] + bbbcar[52][qq]*Hxpar[52] + bbbcar[53][qq]*Hxpar[53] ;
     t7 =  bbbcar[54][qq]*Hxpar[54] + bbbcar[55][qq]*Hxpar[55] + bbbcar[56][qq]*Hxpar[56]
         + bbbcar[57][qq]*Hxpar[57] + bbbcar[58][qq]*Hxpar[58] + bbbcar[59][qq]*Hxpar[59]
         + bbbcar[60][qq]*Hxpar[60] + bbbcar[61][qq]*Hxpar[61] + bbbcar[62][qq]*Hxpar[62]
         + bbbcar[63][qq]*Hxpar[63]                                                       ;
     *xx = dxci * (t1+t2+t3+t4+t5+t6+t7) ;
   } else {
     *xx = 0.0f ;
   }

   if( Hdoy ){
     t1 =  bbbcar[ 0][qq]*Hypar[ 0] + bbbcar[ 1][qq]*Hypar[ 1] + bbbcar[ 2][qq]*Hypar[ 2]
         + bbbcar[ 3][qq]*Hypar[ 3] + bbbcar[ 4][qq]*Hypar[ 4] + bbbcar[ 5][qq]*Hypar[ 5]
         + bbbcar[ 6][qq]*Hypar[ 6] + bbbcar[ 7][qq]*Hypar[ 7] + bbbcar[ 8][qq]*Hypar[ 8] ;
     t2 =  bbbcar[ 9][qq]*Hypar[ 9] + bbbcar[10][qq]*Hypar[10] + bbbcar[11][qq]*Hypar[11]
         + bbbcar[12][qq]*Hypar[12] + bbbcar[13][qq]*Hypar[13] + bbbcar[14][qq]*Hypar[14]
         + bbbcar[15][qq]*Hypar[15] + bbbcar[16][qq]*Hypar[16] + bbbcar[17][qq]*Hypar[17] ;
     t3 =  bbbcar[18][qq]*Hypar[18] + bbbcar[19][qq]*Hypar[19] + bbbcar[20][qq]*Hypar[20]
         + bbbcar[21][qq]*Hypar[21] + bbbcar[22][qq]*Hypar[22] + bbbcar[23][qq]*Hypar[23]
         + bbbcar[24][qq]*Hypar[24] + bbbcar[25][qq]*Hypar[25] + bbbcar[26][qq]*Hypar[26] ;
     t4 =  bbbcar[27][qq]*Hypar[27] + bbbcar[28][qq]*Hypar[28] + bbbcar[29][qq]*Hypar[29]
         + bbbcar[30][qq]*Hypar[30] + bbbcar[31][qq]*Hypar[31] + bbbcar[32][qq]*Hypar[32]
         + bbbcar[33][qq]*Hypar[33] + bbbcar[34][qq]*Hypar[34] + bbbcar[35][qq]*Hypar[35] ;
     t5 =  bbbcar[36][qq]*Hypar[36] + bbbcar[37][qq]*Hypar[37] + bbbcar[38][qq]*Hypar[38]
         + bbbcar[39][qq]*Hypar[39] + bbbcar[40][qq]*Hypar[40] + bbbcar[41][qq]*Hypar[41]
         + bbbcar[42][qq]*Hypar[42] + bbbcar[43][qq]*Hypar[43] + bbbcar[44][qq]*Hypar[44] ;
     t6 =  bbbcar[45][qq]*Hypar[45] + bbbcar[46][qq]*Hypar[46] + bbbcar[47][qq]*Hypar[47]
         + bbbcar[48][qq]*Hypar[48] + bbbcar[49][qq]*Hypar[49] + bbbcar[50][qq]*Hypar[50]
         + bbbcar[51][qq]*Hypar[51] + bbbcar[52][qq]*Hypar[52] + bbbcar[53][qq]*Hypar[53] ;
     t7 =  bbbcar[54][qq]*Hypar[54] + bbbcar[55][qq]*Hypar[55] + bbbcar[56][qq]*Hypar[56]
         + bbbcar[57][qq]*Hypar[57] + bbbcar[58][qq]*Hypar[58] + bbbcar[59][qq]*Hypar[59]
         + bbbcar[60][qq]*Hypar[60] + bbbcar[61][qq]*Hypar[61] + bbbcar[62][qq]*Hypar[62]
         + bbbcar[63][qq]*Hypar[63]                                                       ;
     *yy = dyci * (t1+t2+t3+t4+t5+t6+t7) ;
   } else {
     *yy = 0.0f ;
   }

   if( Hdoz ){
     t1 =  bbbcar[ 0][qq]*Hzpar[ 0] + bbbcar[ 1][qq]*Hzpar[ 1] + bbbcar[ 2][qq]*Hzpar[ 2]
         + bbbcar[ 3][qq]*Hzpar[ 3] + bbbcar[ 4][qq]*Hzpar[ 4] + bbbcar[ 5][qq]*Hzpar[ 5]
         + bbbcar[ 6][qq]*Hzpar[ 6] + bbbcar[ 7][qq]*Hzpar[ 7] + bbbcar[ 8][qq]*Hzpar[ 8] ;
     t2 =  bbbcar[ 9][qq]*Hzpar[ 9] + bbbcar[10][qq]*Hzpar[10] + bbbcar[11][qq]*Hzpar[11]
         + bbbcar[12][qq]*Hzpar[12] + bbbcar[13][qq]*Hzpar[13] + bbbcar[14][qq]*Hzpar[14]
         + bbbcar[15][qq]*Hzpar[15] + bbbcar[16][qq]*Hzpar[16] + bbbcar[17][qq]*Hzpar[17] ;
     t3 =  bbbcar[18][qq]*Hzpar[18] + bbbcar[19][qq]*Hzpar[19] + bbbcar[20][qq]*Hzpar[20]
         + bbbcar[21][qq]*Hzpar[21] + bbbcar[22][qq]*Hzpar[22] + bbbcar[23][qq]*Hzpar[23]
         + bbbcar[24][qq]*Hzpar[24] + bbbcar[25][qq]*Hzpar[25] + bbbcar[26][qq]*Hzpar[26] ;
     t4 =  bbbcar[27][qq]*Hzpar[27] + bbbcar[28][qq]*Hzpar[28] + bbbcar[29][qq]*Hzpar[29]
         + bbbcar[30][qq]*Hzpar[30] + bbbcar[31][qq]*Hzpar[31] + bbbcar[32][qq]*Hzpar[32]
         + bbbcar[33][qq]*Hzpar[33] + bbbcar[34][qq]*Hzpar[34] + bbbcar[35][qq]*Hzpar[35] ;
     t5 =  bbbcar[36][qq]*Hzpar[36] + bbbcar[37][qq]*Hzpar[37] + bbbcar[38][qq]*Hzpar[38]
         + bbbcar[39][qq]*Hzpar[39] + bbbcar[40][qq]*Hzpar[40] + bbbcar[41][qq]*Hzpar[41]
         + bbbcar[42][qq]*Hzpar[42] + bbbcar[43][qq]*Hzpar[43] + bbbcar[44][qq]*Hzpar[44] ;
     t6 =  bbbcar[45][qq]*Hzpar[45] + bbbcar[46][qq]*Hzpar[46] + bbbcar[47][qq]*Hzpar[47]
         + bbbcar[48][qq]*Hzpar[48] + bbbcar[49][qq]*Hzpar[49] + bbbcar[50][qq]*Hzpar[50]
         + bbbcar[51][qq]*Hzpar[51] + bbbcar[52][qq]*Hzpar[52] + bbbcar[53][qq]*Hzpar[53] ;
     t7 =  bbbcar[54][qq]*Hzpar[54] + bbbcar[55][qq]*Hzpar[55] + bbbcar[56][qq]*Hzpar[56]
         + bbbcar[57][qq]*Hzpar[57] + bbbcar[58][qq]*Hzpar[58] + bbbcar[59][qq]*Hzpar[59]
         + bbbcar[60][qq]*Hzpar[60] + bbbcar[61][qq]*Hzpar[61] + bbbcar[62][qq]*Hzpar[62]
         + bbbcar[63][qq]*Hzpar[63]                                                       ;
     *zz = dzci * (t1+t2+t3+t4+t5+t6+t7) ;
   } else {
     *zz = 0.0f ;
   }
#endif

   return ;
}

/*............................................................................*/
/* evaluate basis5 warp the faster way (from 3D basis arrays) */

static void HCwarp_eval_B_basis5( int qq , float *xx , float *yy , float *zz )
{

#if 1
   float t1,t2,t3 ; int jj ;
   t1 = bbbcar[0][qq]*Hxpar[0] ;
   t2 = bbbcar[0][qq]*Hypar[0] ;
   t3 = bbbcar[0][qq]*Hzpar[0] ;
   for( jj=1 ; jj < 125 ; jj+=4 ){
     t1 += bbbcar[jj  ][qq]*Hxpar[jj  ] + bbbcar[jj+1][qq]*Hxpar[jj+1] +
           bbbcar[jj+2][qq]*Hxpar[jj+2] + bbbcar[jj+3][qq]*Hxpar[jj+3]  ;
     t2 += bbbcar[jj  ][qq]*Hypar[jj  ] + bbbcar[jj+1][qq]*Hypar[jj+1] +
           bbbcar[jj+2][qq]*Hypar[jj+2] + bbbcar[jj+3][qq]*Hypar[jj+3]  ;
     t3 += bbbcar[jj  ][qq]*Hzpar[jj  ] + bbbcar[jj+1][qq]*Hzpar[jj+1] +
           bbbcar[jj+2][qq]*Hzpar[jj+2] + bbbcar[jj+3][qq]*Hzpar[jj+3]  ;
   }
   *xx = (Hdox) ? t1 : 0.0f ;
   *yy = (Hdoy) ? t2 : 0.0f ;
   *zz = (Hdoz) ? t3 : 0.0f ;

#else
   float b0zb0yb0x, b1zb0yb0x, b2zb0yb0x, b3zb0yb0x, b4zb0yb0x, b0zb1yb0x, b1zb1yb0x,
         b2zb1yb0x, b3zb1yb0x, b4zb1yb0x, b0zb2yb0x, b1zb2yb0x, b2zb2yb0x, b3zb2yb0x,
         b4zb2yb0x, b0zb3yb0x, b1zb3yb0x, b2zb3yb0x, b3zb3yb0x, b4zb3yb0x, b0zb4yb0x,
         b1zb4yb0x, b2zb4yb0x, b3zb4yb0x, b4zb4yb0x, b0zb0yb1x, b1zb0yb1x, b2zb0yb1x,
         b3zb0yb1x, b4zb0yb1x, b0zb1yb1x, b1zb1yb1x, b2zb1yb1x, b3zb1yb1x, b4zb1yb1x,
         b0zb2yb1x, b1zb2yb1x, b2zb2yb1x, b3zb2yb1x, b4zb2yb1x, b0zb3yb1x, b1zb3yb1x,
         b2zb3yb1x, b3zb3yb1x, b4zb3yb1x, b0zb4yb1x, b1zb4yb1x, b2zb4yb1x, b3zb4yb1x,
         b4zb4yb1x, b0zb0yb2x, b1zb0yb2x, b2zb0yb2x, b3zb0yb2x, b4zb0yb2x, b0zb1yb2x,
         b1zb1yb2x, b2zb1yb2x, b3zb1yb2x, b4zb1yb2x, b0zb2yb2x, b1zb2yb2x, b2zb2yb2x,
         b3zb2yb2x, b4zb2yb2x, b0zb3yb2x, b1zb3yb2x, b2zb3yb2x, b3zb3yb2x, b4zb3yb2x,
         b0zb4yb2x, b1zb4yb2x, b2zb4yb2x, b3zb4yb2x, b4zb4yb2x, b0zb0yb3x, b1zb0yb3x,
         b2zb0yb3x, b3zb0yb3x, b4zb0yb3x, b0zb1yb3x, b1zb1yb3x, b2zb1yb3x, b3zb1yb3x,
         b4zb1yb3x, b0zb2yb3x, b1zb2yb3x, b2zb2yb3x, b3zb2yb3x, b4zb2yb3x, b0zb3yb3x,
         b1zb3yb3x, b2zb3yb3x, b3zb3yb3x, b4zb3yb3x, b0zb4yb3x, b1zb4yb3x, b2zb4yb3x,
         b3zb4yb3x, b4zb4yb3x, b0zb0yb4x, b1zb0yb4x, b2zb0yb4x, b3zb0yb4x, b4zb0yb4x,
         b0zb1yb4x, b1zb1yb4x, b2zb1yb4x, b3zb1yb4x, b4zb1yb4x, b0zb2yb4x, b1zb2yb4x,
         b2zb2yb4x, b3zb2yb4x, b4zb2yb4x, b0zb3yb4x, b1zb3yb4x, b2zb3yb4x, b3zb3yb4x,
         b4zb3yb4x, b0zb4yb4x, b1zb4yb4x, b2zb4yb4x, b3zb4yb4x, b4zb4yb4x ;

   float t01,t02,t03,t04,t05,t06,t07,t08,t09,t10,t11 ;

   b0zb0yb0x = bbbcar[ 0][qq] ; b1zb0yb0x = bbbcar[ 1][qq] ; b2zb0yb0x = bbbcar[ 2][qq] ;
   b3zb0yb0x = bbbcar[ 3][qq] ; b4zb0yb0x = bbbcar[ 4][qq] ; b0zb1yb0x = bbbcar[ 5][qq] ;
   b1zb1yb0x = bbbcar[ 6][qq] ; b2zb1yb0x = bbbcar[ 7][qq] ; b3zb1yb0x = bbbcar[ 8][qq] ;
   b4zb1yb0x = bbbcar[ 9][qq] ; b0zb2yb0x = bbbcar[10][qq] ; b1zb2yb0x = bbbcar[11][qq] ;
   b2zb2yb0x = bbbcar[12][qq] ; b3zb2yb0x = bbbcar[13][qq] ; b4zb2yb0x = bbbcar[14][qq] ;
   b0zb3yb0x = bbbcar[15][qq] ; b1zb3yb0x = bbbcar[16][qq] ; b2zb3yb0x = bbbcar[17][qq] ;
   b3zb3yb0x = bbbcar[18][qq] ; b4zb3yb0x = bbbcar[19][qq] ; b0zb4yb0x = bbbcar[20][qq] ;
   b1zb4yb0x = bbbcar[21][qq] ; b2zb4yb0x = bbbcar[22][qq] ; b3zb4yb0x = bbbcar[23][qq] ;
   b4zb4yb0x = bbbcar[24][qq] ; b0zb0yb1x = bbbcar[25][qq] ; b1zb0yb1x = bbbcar[26][qq] ;
   b2zb0yb1x = bbbcar[27][qq] ; b3zb0yb1x = bbbcar[28][qq] ; b4zb0yb1x = bbbcar[29][qq] ;
   b0zb1yb1x = bbbcar[30][qq] ; b1zb1yb1x = bbbcar[31][qq] ; b2zb1yb1x = bbbcar[32][qq] ;
   b3zb1yb1x = bbbcar[33][qq] ; b4zb1yb1x = bbbcar[34][qq] ; b0zb2yb1x = bbbcar[35][qq] ;
   b1zb2yb1x = bbbcar[36][qq] ; b2zb2yb1x = bbbcar[37][qq] ; b3zb2yb1x = bbbcar[38][qq] ;
   b4zb2yb1x = bbbcar[39][qq] ; b0zb3yb1x = bbbcar[40][qq] ; b1zb3yb1x = bbbcar[41][qq] ;
   b2zb3yb1x = bbbcar[42][qq] ; b3zb3yb1x = bbbcar[43][qq] ; b4zb3yb1x = bbbcar[44][qq] ;
   b0zb4yb1x = bbbcar[45][qq] ; b1zb4yb1x = bbbcar[46][qq] ; b2zb4yb1x = bbbcar[47][qq] ;
   b3zb4yb1x = bbbcar[48][qq] ; b4zb4yb1x = bbbcar[49][qq] ; b0zb0yb2x = bbbcar[50][qq] ;
   b1zb0yb2x = bbbcar[51][qq] ; b2zb0yb2x = bbbcar[52][qq] ; b3zb0yb2x = bbbcar[53][qq] ;
   b4zb0yb2x = bbbcar[54][qq] ; b0zb1yb2x = bbbcar[55][qq] ; b1zb1yb2x = bbbcar[56][qq] ;
   b2zb1yb2x = bbbcar[57][qq] ; b3zb1yb2x = bbbcar[58][qq] ; b4zb1yb2x = bbbcar[59][qq] ;
   b0zb2yb2x = bbbcar[60][qq] ; b1zb2yb2x = bbbcar[61][qq] ; b2zb2yb2x = bbbcar[62][qq] ;
   b3zb2yb2x = bbbcar[63][qq] ; b4zb2yb2x = bbbcar[64][qq] ; b0zb3yb2x = bbbcar[65][qq] ;
   b1zb3yb2x = bbbcar[66][qq] ; b2zb3yb2x = bbbcar[67][qq] ; b3zb3yb2x = bbbcar[68][qq] ;
   b4zb3yb2x = bbbcar[69][qq] ; b0zb4yb2x = bbbcar[70][qq] ; b1zb4yb2x = bbbcar[71][qq] ;
   b2zb4yb2x = bbbcar[72][qq] ; b3zb4yb2x = bbbcar[73][qq] ; b4zb4yb2x = bbbcar[74][qq] ;
   b0zb0yb3x = bbbcar[75][qq] ; b1zb0yb3x = bbbcar[76][qq] ; b2zb0yb3x = bbbcar[77][qq] ;
   b3zb0yb3x = bbbcar[78][qq] ; b4zb0yb3x = bbbcar[79][qq] ; b0zb1yb3x = bbbcar[80][qq] ;
   b1zb1yb3x = bbbcar[81][qq] ; b2zb1yb3x = bbbcar[82][qq] ; b3zb1yb3x = bbbcar[83][qq] ;
   b4zb1yb3x = bbbcar[84][qq] ; b0zb2yb3x = bbbcar[85][qq] ; b1zb2yb3x = bbbcar[86][qq] ;
   b2zb2yb3x = bbbcar[87][qq] ; b3zb2yb3x = bbbcar[88][qq] ; b4zb2yb3x = bbbcar[89][qq] ;
   b0zb3yb3x = bbbcar[90][qq] ; b1zb3yb3x = bbbcar[91][qq] ; b2zb3yb3x = bbbcar[92][qq] ;
   b3zb3yb3x = bbbcar[93][qq] ; b4zb3yb3x = bbbcar[94][qq] ; b0zb4yb3x = bbbcar[95][qq] ;
   b1zb4yb3x = bbbcar[96][qq] ; b2zb4yb3x = bbbcar[97][qq] ; b3zb4yb3x = bbbcar[98][qq] ;
   b4zb4yb3x = bbbcar[99][qq] ; b0zb0yb4x = bbbcar[100][qq] ; b1zb0yb4x = bbbcar[101][qq] ;
   b2zb0yb4x = bbbcar[102][qq] ; b3zb0yb4x = bbbcar[103][qq] ; b4zb0yb4x = bbbcar[104][qq] ;
   b0zb1yb4x = bbbcar[105][qq] ; b1zb1yb4x = bbbcar[106][qq] ; b2zb1yb4x = bbbcar[107][qq] ;
   b3zb1yb4x = bbbcar[108][qq] ; b4zb1yb4x = bbbcar[109][qq] ; b0zb2yb4x = bbbcar[110][qq] ;
   b1zb2yb4x = bbbcar[111][qq] ; b2zb2yb4x = bbbcar[112][qq] ; b3zb2yb4x = bbbcar[113][qq] ;
   b4zb2yb4x = bbbcar[114][qq] ; b0zb3yb4x = bbbcar[115][qq] ; b1zb3yb4x = bbbcar[116][qq] ;
   b2zb3yb4x = bbbcar[117][qq] ; b3zb3yb4x = bbbcar[118][qq] ; b4zb3yb4x = bbbcar[119][qq] ;
   b0zb4yb4x = bbbcar[120][qq] ; b1zb4yb4x = bbbcar[121][qq] ; b2zb4yb4x = bbbcar[122][qq] ;
   b3zb4yb4x = bbbcar[123][qq] ; b4zb4yb4x = bbbcar[124][qq] ;

   /* break 1 big statement with 125 multiply-adds into 11 sub-statements,
      hoping that the optimizer will treat this more efficiently :-) [06 Apr 2016] */

   if( Hdox ){
      t01 =   b0zb0yb0x*Hxpar[ 0] + b1zb0yb0x*Hxpar[ 1] + b2zb0yb0x*Hxpar[ 2]
            + b3zb0yb0x*Hxpar[ 3] + b4zb0yb0x*Hxpar[ 4] + b0zb1yb0x*Hxpar[ 5]
            + b1zb1yb0x*Hxpar[ 6] + b2zb1yb0x*Hxpar[ 7] + b3zb1yb0x*Hxpar[ 8]
            + b4zb1yb0x*Hxpar[ 9] + b0zb2yb0x*Hxpar[10] + b1zb2yb0x*Hxpar[11] ;
      t02 =   b2zb2yb0x*Hxpar[12] + b3zb2yb0x*Hxpar[13] + b4zb2yb0x*Hxpar[14]
            + b0zb3yb0x*Hxpar[15] + b1zb3yb0x*Hxpar[16] + b2zb3yb0x*Hxpar[17]
            + b3zb3yb0x*Hxpar[18] + b4zb3yb0x*Hxpar[19] + b0zb4yb0x*Hxpar[20]
            + b1zb4yb0x*Hxpar[21] + b2zb4yb0x*Hxpar[22] + b3zb4yb0x*Hxpar[23] ;
      t03 =   b4zb4yb0x*Hxpar[24] + b0zb0yb1x*Hxpar[25] + b1zb0yb1x*Hxpar[26]
            + b2zb0yb1x*Hxpar[27] + b3zb0yb1x*Hxpar[28] + b4zb0yb1x*Hxpar[29]
            + b0zb1yb1x*Hxpar[30] + b1zb1yb1x*Hxpar[31] + b2zb1yb1x*Hxpar[32]
            + b3zb1yb1x*Hxpar[33] + b4zb1yb1x*Hxpar[34] + b0zb2yb1x*Hxpar[35] ;
      t04 =   b1zb2yb1x*Hxpar[36] + b2zb2yb1x*Hxpar[37] + b3zb2yb1x*Hxpar[38]
            + b4zb2yb1x*Hxpar[39] + b0zb3yb1x*Hxpar[40] + b1zb3yb1x*Hxpar[41]
            + b2zb3yb1x*Hxpar[42] + b3zb3yb1x*Hxpar[43] + b4zb3yb1x*Hxpar[44]
            + b0zb4yb1x*Hxpar[45] + b1zb4yb1x*Hxpar[46] + b2zb4yb1x*Hxpar[47] ;
      t05 =   b3zb4yb1x*Hxpar[48] + b4zb4yb1x*Hxpar[49] + b0zb0yb2x*Hxpar[50]
            + b1zb0yb2x*Hxpar[51] + b2zb0yb2x*Hxpar[52] + b3zb0yb2x*Hxpar[53]
            + b4zb0yb2x*Hxpar[54] + b0zb1yb2x*Hxpar[55] + b1zb1yb2x*Hxpar[56]
            + b2zb1yb2x*Hxpar[57] + b3zb1yb2x*Hxpar[58] + b4zb1yb2x*Hxpar[59] ;
      t06 =   b0zb2yb2x*Hxpar[60] + b1zb2yb2x*Hxpar[61] + b2zb2yb2x*Hxpar[62]
            + b3zb2yb2x*Hxpar[63] + b4zb2yb2x*Hxpar[64] + b0zb3yb2x*Hxpar[65]
            + b1zb3yb2x*Hxpar[66] + b2zb3yb2x*Hxpar[67] + b3zb3yb2x*Hxpar[68]
            + b4zb3yb2x*Hxpar[69] + b0zb4yb2x*Hxpar[70] + b1zb4yb2x*Hxpar[71] ;
      t07 =   b2zb4yb2x*Hxpar[72] + b3zb4yb2x*Hxpar[73] + b4zb4yb2x*Hxpar[74]
            + b0zb0yb3x*Hxpar[75] + b1zb0yb3x*Hxpar[76] + b2zb0yb3x*Hxpar[77]
            + b3zb0yb3x*Hxpar[78] + b4zb0yb3x*Hxpar[79] + b0zb1yb3x*Hxpar[80]
            + b1zb1yb3x*Hxpar[81] + b2zb1yb3x*Hxpar[82] + b3zb1yb3x*Hxpar[83] ;
      t08 =   b4zb1yb3x*Hxpar[84] + b0zb2yb3x*Hxpar[85] + b1zb2yb3x*Hxpar[86]
            + b2zb2yb3x*Hxpar[87] + b3zb2yb3x*Hxpar[88] + b4zb2yb3x*Hxpar[89]
            + b0zb3yb3x*Hxpar[90] + b1zb3yb3x*Hxpar[91] + b2zb3yb3x*Hxpar[92]
            + b3zb3yb3x*Hxpar[93] + b4zb3yb3x*Hxpar[94] + b0zb4yb3x*Hxpar[95] ;
      t09 =   b1zb4yb3x*Hxpar[96] + b2zb4yb3x*Hxpar[97] + b3zb4yb3x*Hxpar[98]
            + b4zb4yb3x*Hxpar[99] + b0zb0yb4x*Hxpar[100] + b1zb0yb4x*Hxpar[101]
            + b2zb0yb4x*Hxpar[102] + b3zb0yb4x*Hxpar[103] + b4zb0yb4x*Hxpar[104]
            + b0zb1yb4x*Hxpar[105] + b1zb1yb4x*Hxpar[106] + b2zb1yb4x*Hxpar[107] ;
      t10 =   b3zb1yb4x*Hxpar[108] + b4zb1yb4x*Hxpar[109] + b0zb2yb4x*Hxpar[110]
            + b1zb2yb4x*Hxpar[111] + b2zb2yb4x*Hxpar[112] + b3zb2yb4x*Hxpar[113]
            + b4zb2yb4x*Hxpar[114] + b0zb3yb4x*Hxpar[115] + b1zb3yb4x*Hxpar[116]
            + b2zb3yb4x*Hxpar[117] + b3zb3yb4x*Hxpar[118] + b4zb3yb4x*Hxpar[119] ;
      t11 =   b0zb4yb4x*Hxpar[120] + b1zb4yb4x*Hxpar[121] + b2zb4yb4x*Hxpar[122]
            + b3zb4yb4x*Hxpar[123] + b4zb4yb4x*Hxpar[124]                        ;
     *xx = dxci * ( t01+t02+t03+t04+t05+t06+t07+t08+t09+t10+t11 ) ;
   } else {
     *xx = 0.0f ;
   }

   if( Hdoy ){
      t01 =   b0zb0yb0x*Hypar[ 0] + b1zb0yb0x*Hypar[ 1] + b2zb0yb0x*Hypar[ 2]
            + b3zb0yb0x*Hypar[ 3] + b4zb0yb0x*Hypar[ 4] + b0zb1yb0x*Hypar[ 5]
            + b1zb1yb0x*Hypar[ 6] + b2zb1yb0x*Hypar[ 7] + b3zb1yb0x*Hypar[ 8]
            + b4zb1yb0x*Hypar[ 9] + b0zb2yb0x*Hypar[10] + b1zb2yb0x*Hypar[11] ;
      t02 =   b2zb2yb0x*Hypar[12] + b3zb2yb0x*Hypar[13] + b4zb2yb0x*Hypar[14]
            + b0zb3yb0x*Hypar[15] + b1zb3yb0x*Hypar[16] + b2zb3yb0x*Hypar[17]
            + b3zb3yb0x*Hypar[18] + b4zb3yb0x*Hypar[19] + b0zb4yb0x*Hypar[20]
            + b1zb4yb0x*Hypar[21] + b2zb4yb0x*Hypar[22] + b3zb4yb0x*Hypar[23] ;
      t03 =   b4zb4yb0x*Hypar[24] + b0zb0yb1x*Hypar[25] + b1zb0yb1x*Hypar[26]
            + b2zb0yb1x*Hypar[27] + b3zb0yb1x*Hypar[28] + b4zb0yb1x*Hypar[29]
            + b0zb1yb1x*Hypar[30] + b1zb1yb1x*Hypar[31] + b2zb1yb1x*Hypar[32]
            + b3zb1yb1x*Hypar[33] + b4zb1yb1x*Hypar[34] + b0zb2yb1x*Hypar[35] ;
      t04 =   b1zb2yb1x*Hypar[36] + b2zb2yb1x*Hypar[37] + b3zb2yb1x*Hypar[38]
            + b4zb2yb1x*Hypar[39] + b0zb3yb1x*Hypar[40] + b1zb3yb1x*Hypar[41]
            + b2zb3yb1x*Hypar[42] + b3zb3yb1x*Hypar[43] + b4zb3yb1x*Hypar[44]
            + b0zb4yb1x*Hypar[45] + b1zb4yb1x*Hypar[46] + b2zb4yb1x*Hypar[47] ;
      t05 =   b3zb4yb1x*Hypar[48] + b4zb4yb1x*Hypar[49] + b0zb0yb2x*Hypar[50]
            + b1zb0yb2x*Hypar[51] + b2zb0yb2x*Hypar[52] + b3zb0yb2x*Hypar[53]
            + b4zb0yb2x*Hypar[54] + b0zb1yb2x*Hypar[55] + b1zb1yb2x*Hypar[56]
            + b2zb1yb2x*Hypar[57] + b3zb1yb2x*Hypar[58] + b4zb1yb2x*Hypar[59] ;
      t06 =   b0zb2yb2x*Hypar[60] + b1zb2yb2x*Hypar[61] + b2zb2yb2x*Hypar[62]
            + b3zb2yb2x*Hypar[63] + b4zb2yb2x*Hypar[64] + b0zb3yb2x*Hypar[65]
            + b1zb3yb2x*Hypar[66] + b2zb3yb2x*Hypar[67] + b3zb3yb2x*Hypar[68]
            + b4zb3yb2x*Hypar[69] + b0zb4yb2x*Hypar[70] + b1zb4yb2x*Hypar[71] ;
      t07 =   b2zb4yb2x*Hypar[72] + b3zb4yb2x*Hypar[73] + b4zb4yb2x*Hypar[74]
            + b0zb0yb3x*Hypar[75] + b1zb0yb3x*Hypar[76] + b2zb0yb3x*Hypar[77]
            + b3zb0yb3x*Hypar[78] + b4zb0yb3x*Hypar[79] + b0zb1yb3x*Hypar[80]
            + b1zb1yb3x*Hypar[81] + b2zb1yb3x*Hypar[82] + b3zb1yb3x*Hypar[83] ;
      t08 =   b4zb1yb3x*Hypar[84] + b0zb2yb3x*Hypar[85] + b1zb2yb3x*Hypar[86]
            + b2zb2yb3x*Hypar[87] + b3zb2yb3x*Hypar[88] + b4zb2yb3x*Hypar[89]
            + b0zb3yb3x*Hypar[90] + b1zb3yb3x*Hypar[91] + b2zb3yb3x*Hypar[92]
            + b3zb3yb3x*Hypar[93] + b4zb3yb3x*Hypar[94] + b0zb4yb3x*Hypar[95] ;
      t09 =   b1zb4yb3x*Hypar[96] + b2zb4yb3x*Hypar[97] + b3zb4yb3x*Hypar[98]
            + b4zb4yb3x*Hypar[99] + b0zb0yb4x*Hypar[100] + b1zb0yb4x*Hypar[101]
            + b2zb0yb4x*Hypar[102] + b3zb0yb4x*Hypar[103] + b4zb0yb4x*Hypar[104]
            + b0zb1yb4x*Hypar[105] + b1zb1yb4x*Hypar[106] + b2zb1yb4x*Hypar[107] ;
      t10 =   b3zb1yb4x*Hypar[108] + b4zb1yb4x*Hypar[109] + b0zb2yb4x*Hypar[110]
            + b1zb2yb4x*Hypar[111] + b2zb2yb4x*Hypar[112] + b3zb2yb4x*Hypar[113]
            + b4zb2yb4x*Hypar[114] + b0zb3yb4x*Hypar[115] + b1zb3yb4x*Hypar[116]
            + b2zb3yb4x*Hypar[117] + b3zb3yb4x*Hypar[118] + b4zb3yb4x*Hypar[119] ;
      t11 =   b0zb4yb4x*Hypar[120] + b1zb4yb4x*Hypar[121] + b2zb4yb4x*Hypar[122]
            + b3zb4yb4x*Hypar[123] + b4zb4yb4x*Hypar[124]                        ;
     *yy = dyci * ( t01+t02+t03+t04+t05+t06+t07+t08+t09+t10+t11 ) ;
   } else {
     *yy = 0.0f ;
   }

   if( Hdoz ){
      t01 =   b0zb0yb0x*Hzpar[ 0] + b1zb0yb0x*Hzpar[ 1] + b2zb0yb0x*Hzpar[ 2]
            + b3zb0yb0x*Hzpar[ 3] + b4zb0yb0x*Hzpar[ 4] + b0zb1yb0x*Hzpar[ 5]
            + b1zb1yb0x*Hzpar[ 6] + b2zb1yb0x*Hzpar[ 7] + b3zb1yb0x*Hzpar[ 8]
            + b4zb1yb0x*Hzpar[ 9] + b0zb2yb0x*Hzpar[10] + b1zb2yb0x*Hzpar[11] ;
      t02 =   b2zb2yb0x*Hzpar[12] + b3zb2yb0x*Hzpar[13] + b4zb2yb0x*Hzpar[14]
            + b0zb3yb0x*Hzpar[15] + b1zb3yb0x*Hzpar[16] + b2zb3yb0x*Hzpar[17]
            + b3zb3yb0x*Hzpar[18] + b4zb3yb0x*Hzpar[19] + b0zb4yb0x*Hzpar[20]
            + b1zb4yb0x*Hzpar[21] + b2zb4yb0x*Hzpar[22] + b3zb4yb0x*Hzpar[23] ;
      t03 =   b4zb4yb0x*Hzpar[24] + b0zb0yb1x*Hzpar[25] + b1zb0yb1x*Hzpar[26]
            + b2zb0yb1x*Hzpar[27] + b3zb0yb1x*Hzpar[28] + b4zb0yb1x*Hzpar[29]
            + b0zb1yb1x*Hzpar[30] + b1zb1yb1x*Hzpar[31] + b2zb1yb1x*Hzpar[32]
            + b3zb1yb1x*Hzpar[33] + b4zb1yb1x*Hzpar[34] + b0zb2yb1x*Hzpar[35] ;
      t04 =   b1zb2yb1x*Hzpar[36] + b2zb2yb1x*Hzpar[37] + b3zb2yb1x*Hzpar[38]
            + b4zb2yb1x*Hzpar[39] + b0zb3yb1x*Hzpar[40] + b1zb3yb1x*Hzpar[41]
            + b2zb3yb1x*Hzpar[42] + b3zb3yb1x*Hzpar[43] + b4zb3yb1x*Hzpar[44]
            + b0zb4yb1x*Hzpar[45] + b1zb4yb1x*Hzpar[46] + b2zb4yb1x*Hzpar[47] ;
      t05 =   b3zb4yb1x*Hzpar[48] + b4zb4yb1x*Hzpar[49] + b0zb0yb2x*Hzpar[50]
            + b1zb0yb2x*Hzpar[51] + b2zb0yb2x*Hzpar[52] + b3zb0yb2x*Hzpar[53]
            + b4zb0yb2x*Hzpar[54] + b0zb1yb2x*Hzpar[55] + b1zb1yb2x*Hzpar[56]
            + b2zb1yb2x*Hzpar[57] + b3zb1yb2x*Hzpar[58] + b4zb1yb2x*Hzpar[59] ;
      t06 =   b0zb2yb2x*Hzpar[60] + b1zb2yb2x*Hzpar[61] + b2zb2yb2x*Hzpar[62]
            + b3zb2yb2x*Hzpar[63] + b4zb2yb2x*Hzpar[64] + b0zb3yb2x*Hzpar[65]
            + b1zb3yb2x*Hzpar[66] + b2zb3yb2x*Hzpar[67] + b3zb3yb2x*Hzpar[68]
            + b4zb3yb2x*Hzpar[69] + b0zb4yb2x*Hzpar[70] + b1zb4yb2x*Hzpar[71] ;
      t07 =   b2zb4yb2x*Hzpar[72] + b3zb4yb2x*Hzpar[73] + b4zb4yb2x*Hzpar[74]
            + b0zb0yb3x*Hzpar[75] + b1zb0yb3x*Hzpar[76] + b2zb0yb3x*Hzpar[77]
            + b3zb0yb3x*Hzpar[78] + b4zb0yb3x*Hzpar[79] + b0zb1yb3x*Hzpar[80]
            + b1zb1yb3x*Hzpar[81] + b2zb1yb3x*Hzpar[82] + b3zb1yb3x*Hzpar[83] ;
      t08 =   b4zb1yb3x*Hzpar[84] + b0zb2yb3x*Hzpar[85] + b1zb2yb3x*Hzpar[86]
            + b2zb2yb3x*Hzpar[87] + b3zb2yb3x*Hzpar[88] + b4zb2yb3x*Hzpar[89]
            + b0zb3yb3x*Hzpar[90] + b1zb3yb3x*Hzpar[91] + b2zb3yb3x*Hzpar[92]
            + b3zb3yb3x*Hzpar[93] + b4zb3yb3x*Hzpar[94] + b0zb4yb3x*Hzpar[95] ;
      t09 =   b1zb4yb3x*Hzpar[96] + b2zb4yb3x*Hzpar[97] + b3zb4yb3x*Hzpar[98]
            + b4zb4yb3x*Hzpar[99] + b0zb0yb4x*Hzpar[100] + b1zb0yb4x*Hzpar[101]
            + b2zb0yb4x*Hzpar[102] + b3zb0yb4x*Hzpar[103] + b4zb0yb4x*Hzpar[104]
            + b0zb1yb4x*Hzpar[105] + b1zb1yb4x*Hzpar[106] + b2zb1yb4x*Hzpar[107] ;
      t10 =   b3zb1yb4x*Hzpar[108] + b4zb1yb4x*Hzpar[109] + b0zb2yb4x*Hzpar[110]
            + b1zb2yb4x*Hzpar[111] + b2zb2yb4x*Hzpar[112] + b3zb2yb4x*Hzpar[113]
            + b4zb2yb4x*Hzpar[114] + b0zb3yb4x*Hzpar[115] + b1zb3yb4x*Hzpar[116]
            + b2zb3yb4x*Hzpar[117] + b3zb3yb4x*Hzpar[118] + b4zb3yb4x*Hzpar[119] ;
      t11 =   b0zb4yb4x*Hzpar[120] + b1zb4yb4x*Hzpar[121] + b2zb4yb4x*Hzpar[122]
            + b3zb4yb4x*Hzpar[123] + b4zb4yb4x*Hzpar[124]                        ;
     *zz = dzci * ( t01+t02+t03+t04+t05+t06+t07+t08+t09+t10+t11 ) ;
   } else {
     *zz = 0.0f ;
   }
#endif

   return ;
}
#endif /* ALLOW_BASIS5 */

#endif /* USE_HLOADER */  /*HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH*/

#endif /*(Q3)*/ /*############################################################*/

#if 1
/*============================================================================*/
/** (Q4) Evaluate the incrementally warped source image at the combination
    of the current global warp and the patch warp

 --: This function is a key part of the 3dQwarp empire -- it is where the  :--
 --:   source image is warped so it can be compared to the base image,     :--
 --:   so it will be called ** A LOT **.                                   :--
 --: It was cloned into Hwarp_apply_plusminus() for warping the base and   :--
 --:   source images together (in opposite directions).  If one wanted to  :--
 --:   match vector-valued images in some way, one central necessity would :--
 --:   be to generalize this function to deal with such beasts.            :--*/
/*============================================================================*/

/*----------------------------------------------------------------------------*/
/* Evaluate Hsrcim[ Haawarp(Hwarp(x)) ] into the val[] array.
     Note that Haawarp is a global warp for Hsrcim, whereas Hwarp is just
     a local patch.
   On output, the val[] array contains the linearly interpolated warped image
     values over this patch.
   Also evaluate the composed warp Haawarp[Hwarp(x)] into AHwarp for future
     utility.  AHwarp is a local patch that fits into Haawarp later, when
     it needs to be updated at the end of a patch optimization step.
     (Actually, AHwarp will not necessarily be computed if need_AH is zero.)
*//*--------------------------------------------------------------------------*/

void Hwarp_apply( float *val )
{
   int   nbx,nby,nbz , nbxy,nbxyz , nAx,nAy,nAz , nAx1,nAy1,nAz1 , nAxy ;
   float nAxh,nAyh,nAzh ;
   float *hxd,*hyd,*hzd , *Axd,*Ayd,*Azd , *sar , *bxd,*byd,*bzd ;
#ifndef USE_HLOADER
   void (*Heval)(int,float *,float *,float *) = NULL ;  /* compute Hwarp at one index */
#endif

ENTRY("Hwarp_apply") ;

   /* bad inputs? */

   if( Hsrcim == NULL || Haawarp == NULL || val == NULL || Hwarp == NULL ) EXRETURN ;

   /* get local pointers to the various displacement arrays */

   hxd = Hwarp->xd  ; hyd = Hwarp->yd  ; hzd = Hwarp->zd  ; /* Hwarp delta (patch) */
   Axd = Haawarp->xd; Ayd = Haawarp->yd; Azd = Haawarp->zd; /* Haawarp (global) */
   bxd = AHwarp->xd ; byd = AHwarp->yd ; bzd = AHwarp->zd ; /* AHwarp delta (patch) */

   /* decide if this is a cubic or quintic warp, load patch grid sizes */

   if( Hbasis_code == MRI_QUINTIC ){ nbx = nbqx ; nby = nbqy ; nbz = nbqz ; }
   else                            { nbx = nbcx ; nby = nbcy ; nbz = nbcz ; }

   nbxy = nbx*nby ; nbxyz = nbxy*nbz ;

   /* choose which parameter-to-patch warp function to use */

#ifndef USE_HLOADER
   if( Hbasis_code == MRI_CUBIC ){
     Heval = (bbbcar == NULL) ? HCwarp_eval_A : HCwarp_eval_B ;
   } else if( Hbasis_code == MRI_QUINTIC ){
     Heval = (bbbqar == NULL) ? HQwarp_eval_A : HQwarp_eval_B ;
#ifdef ALLOW_BASIS5
   } else if( Hbasis_code == MRI_CUBIC_PLUS_1 ){
     Heval = HCwarp_eval_B_basis3 ;
   } else if( Hbasis_code == MRI_CUBIC_PLUS_2 ){
     Heval = HCwarp_eval_B_basis4 ;
   } else if( Hbasis_code == MRI_CUBIC_PLUS_3 ){
     Heval = HCwarp_eval_B_basis5 ;
#endif
   }
#endif

   /* global warp dimensions */

   nAx  = Haawarp->nx; nAy  = Haawarp->ny; nAz  = Haawarp->nz; nAxy = nAx*nAy;
   nAx1 = nAx-1      ; nAy1 = nAy-1      ; nAz1 = nAz-1      ;
   nAxh = nAx-0.501f ; nAyh = nAy-0.501f ; nAzh = nAz-0.501f ;

   sar = MRI_FLOAT_PTR(SRCIM) ;  /* source image array (to be warped) */

STATUS("start loop") ;

#undef  IJK   /* 3D index to 1D index */
#define IJK(i,j,k) ((i)+(j)*nAx+(k)*nAxy)

#undef  XINT  /* linear interpolation in array aaa[] */
#define XINT(aaa,j,k) wt_00*aaa[IJK(ix_00,j,k)]+wt_p1*aaa[IJK(ix_p1,j,k)]

AFNI_OMP_START ;
#pragma omp parallel
 { int ii,jj,kk , qq , need_val ;
   float xq,yq,zq ;
   float fx,fy,fz , ix,jy,kz ;
   int   ix_00,ix_p1 , jy_00,jy_p1 , kz_00,kz_p1 ;
   float wt_00,wt_p1 ;
   float f_j00_k00, f_jp1_k00, f_j00_kp1, f_jp1_kp1, f_k00, f_kp1 ;
   float g_j00_k00, g_jp1_k00, g_j00_kp1, g_jp1_kp1, g_k00, g_kp1 ;
   float h_j00_k00, h_jp1_k00, h_j00_kp1, h_jp1_kp1, h_k00, h_kp1 ;
#ifdef USE_OMP
   int ith = omp_get_thread_num() ;  /* thread index (not used here) */
#else
   int ith = 0 ;
#endif

#pragma ivdep  /* for Intel icc compiler */
#pragma omp for
   for( qq=0 ; qq < nbxyz ; qq++ ){            /* for each voxel in the patch */
     ii = qq % nbx; kk = qq / nbxy; jj = (qq-kk*nbxy) / nbx; /* patch indexes */

     /* determine if we actually need this value (is it in the mask?) */

     need_val = ( Hbmask[IJK(ii+Hibot,jj+Hjbot,kk+Hkbot)] != 0 ) ;

     if( !need_val && !need_AH ){ val[qq] = 0.0f; continue; }  /* that was easy */

#ifndef USE_HLOADER
     Heval(qq,hxd+qq,hyd+qq,hzd+qq) ;  /* if warp not loaded, evaluate it now */
#endif

     /* get Hwarp-ed indexes into the global warp Haawarp; e.g.,
          xq = Hibot + ii + hxd[qq]
        because the Hwarp output index warp location is computed as
          Hwarp_x(x,y,z) = x + hxd
        and we also have to add in Hibot to get a global index for use in Haawarp */

#if 0
     xq = Hibot + ii + hxd[qq] ; if( xq < -0.499f ) xq = -0.499f; else if( xq > nAxh ) xq = nAxh;
     yq = Hjbot + jj + hyd[qq] ; if( yq < -0.499f ) yq = -0.499f; else if( yq > nAyh ) yq = nAyh;
     zq = Hkbot + kk + hzd[qq] ; if( zq < -0.499f ) zq = -0.499f; else if( zq > nAzh ) zq = nAzh;
     ix = floorf(xq) ;  fx = xq - ix ;
     jy = floorf(yq) ;  fy = yq - jy ;
     kz = floorf(zq) ;  fz = zq - kz ;
     ix_00 = ix ; ix_p1 = ix_00+1 ; CLIP(ix_00,nAx1) ; CLIP(ix_p1,nAx1) ;
     jy_00 = jy ; jy_p1 = jy_00+1 ; CLIP(jy_00,nAy1) ; CLIP(jy_p1,nAy1) ;
     kz_00 = kz ; kz_p1 = kz_00+1 ; CLIP(kz_00,nAz1) ; CLIP(kz_p1,nAz1) ;
#else
     xq = Hibot + ii + hxd[qq] ; ix = (int)(xq) ; fx = xq - ix ;
     yq = Hjbot + jj + hyd[qq] ; jy = (int)(yq) ; fy = yq - jy ;
     zq = Hkbot + kk + hzd[qq] ; kz = (int)(zq) ; fz = zq - kz ;
     ix_00 = ix ; ix_p1 = ix_00+1 ; QLIP(ix_00,nAx1) ; QLIP(ix_p1,nAx1) ;
     jy_00 = jy ; jy_p1 = jy_00+1 ; QLIP(jy_00,nAy1) ; QLIP(jy_p1,nAy1) ;
     kz_00 = kz ; kz_p1 = kz_00+1 ; QLIP(kz_00,nAz1) ; QLIP(kz_p1,nAz1) ;
#endif

     /* linearly interpolate in Haawarp to get Haawarp
        displacements at this Hwarp warped location; later,
        we then interpolate at THESE displacments to get the warped image value */

     wt_00 = 1.0f-fx ; wt_p1 = fx ;   /* x interpolations of Axd, Ayd, Azd */
     f_j00_k00 = XINT(Axd,jy_00,kz_00) ; f_jp1_k00 = XINT(Axd,jy_p1,kz_00) ;
     f_j00_kp1 = XINT(Axd,jy_00,kz_p1) ; f_jp1_kp1 = XINT(Axd,jy_p1,kz_p1) ;
     g_j00_k00 = XINT(Ayd,jy_00,kz_00) ; g_jp1_k00 = XINT(Ayd,jy_p1,kz_00) ;
     g_j00_kp1 = XINT(Ayd,jy_00,kz_p1) ; g_jp1_kp1 = XINT(Ayd,jy_p1,kz_p1) ;
     h_j00_k00 = XINT(Azd,jy_00,kz_00) ; h_jp1_k00 = XINT(Azd,jy_p1,kz_00) ;
     h_j00_kp1 = XINT(Azd,jy_00,kz_p1) ; h_jp1_kp1 = XINT(Azd,jy_p1,kz_p1) ;

     wt_00 = 1.0f-fy ;                /* y interpolations */
     f_k00 = wt_00 * f_j00_k00 + fy * f_jp1_k00 ;
     f_kp1 = wt_00 * f_j00_kp1 + fy * f_jp1_kp1 ;
     g_k00 = wt_00 * g_j00_k00 + fy * g_jp1_k00 ;
     g_kp1 = wt_00 * g_j00_kp1 + fy * g_jp1_kp1 ;
     h_k00 = wt_00 * h_j00_k00 + fy * h_jp1_k00 ;
     h_kp1 = wt_00 * h_j00_kp1 + fy * h_jp1_kp1 ;

     wt_00 = 1.0f-fz ;                /* z interpolations */

     /* bxd = x-displacments for AHwarp = Awarp(Hwarp())
        xq  = index in srcim for output interpolation to get val */

     bxd[qq] = wt_00 * f_k00 + fz * f_kp1 + hxd[qq] ;  /* saved into AHwarp */
     byd[qq] = wt_00 * g_k00 + fz * g_kp1 + hyd[qq] ;  /* at this point */
     bzd[qq] = wt_00 * h_k00 + fz * h_kp1 + hzd[qq] ;

     /* if not in the global mask, don't bother to interpolate val */

     if( !need_val ){ val[qq] = 0.0f; continue; }

     /* locations at which to interpolate to source image to get val */

     xq = bxd[qq]+ii+Hibot ; yq = byd[qq]+jj+Hjbot ; zq = bzd[qq]+kk+Hkbot ;

     /** ABOVE: since Awarp_x[x,y,z] = x + Axd, then
           Awarp_x[ Hwarp(x,y,z) ] = Hwarp_x(x,y,z) + Axd(interpolated)
                                   = Hibot + ii + hxd + Axd(interpolated)
         so the above formula for xq includes not just the interpolated
         values from Axd (the first 2 terms) but the Hwarp stuff again, also */

     /* interpolate source image at (xq,yq,zq) indexes in sar to get val */

     if( xq < -0.499f ) xq = -0.499f; else if( xq > nAxh ) xq = nAxh;
     if( yq < -0.499f ) yq = -0.499f; else if( yq > nAyh ) yq = nAyh;
     if( zq < -0.499f ) zq = -0.499f; else if( zq > nAzh ) zq = nAzh;

     if( Himeth == MRI_NN ){             /* special case of NN interp of data */
       ix_00   = (int)(xq+0.5f) ;                       /* which is real easy */
       jy_00   = (int)(yq+0.5f) ;
       kz_00   = (int)(zq+0.5f) ;
       val[qq] = sar[IJK(ix_00,jy_00,kz_00)] ;
     } else {                                 /* normal case of linear interp */
       ix = floorf(xq) ;  fx = xq - ix ;       /* which is a little more work */
       jy = floorf(yq) ;  fy = yq - jy ;
       kz = floorf(zq) ;  fz = zq - kz ;
       ix_00 = ix ; ix_p1 = ix_00+1 ; CLIP(ix_00,nAx1) ; CLIP(ix_p1,nAx1) ;
       jy_00 = jy ; jy_p1 = jy_00+1 ; CLIP(jy_00,nAy1) ; CLIP(jy_p1,nAy1) ;
       kz_00 = kz ; kz_p1 = kz_00+1 ; CLIP(kz_00,nAz1) ; CLIP(kz_p1,nAz1) ;

       wt_00 = 1.0f-fx ; wt_p1 = fx ;                     /* x interpolations */
       f_j00_k00 = XINT(sar,jy_00,kz_00) ; f_jp1_k00 = XINT(sar,jy_p1,kz_00) ;
       f_j00_kp1 = XINT(sar,jy_00,kz_p1) ; f_jp1_kp1 = XINT(sar,jy_p1,kz_p1) ;
       wt_00 = 1.0f-fy ;                                  /* y interpolations */
       f_k00 = wt_00 * f_j00_k00 + fy * f_jp1_k00 ;
       f_kp1 = wt_00 * f_j00_kp1 + fy * f_jp1_kp1 ;
       val[qq] = (1.0f-fz) * f_k00 + fz * f_kp1 ; /* z interpolation = output */
     }

   } /* end of parallelized for loop over voxels */
 } /* end of parallel stuff */
AFNI_OMP_END ;

   AFNI_do_nothing() ; /* fprintf(stderr,"B") ; */

   EXRETURN ;
}

#endif /*(Q4)*/ /*############################################################*/

#if 1
/*============================================================================*/
/** (Q5) Functions to evaluate the warp penalty and cost function            **/
/*============================================================================*/

/*----------------------------------------------------------------------------*/
/** Penalty parameters **/

#define Hpen_fbase 0.033333         /* increased by factor of 5 [23 Sep 2013] */

static double Hpen_fac = Hpen_fbase ;
static double Hpen_fff = Hpen_fbase ;     /* increases with lev [20 Sep 2013] */
static double Hpen_sum = 0.0 ;
static int    Hpen_num = 0 ;
static int    Hpen_use = 1 ;
static int    Hpen_old = 0 ;              /* don't increase with lev */

/*----------------------------------------------------------------------------*/
/* Compute the penalty as the pre-computed penalty for the part of the
   warp outside the current patch (Hpen_sum), plus the penalty in the current
   patch, as computed from AHwarp.  Hpen_sum is initialized in function
   IW3D_improve_warp().
*//*-------------------------------------------------------------------------*/

double HPEN_penalty(void)
{
   double hsum ;
   hsum = Hpen_sum + (double)IW3D_load_energy(AHwarp) ;
   if( hsum > 0.0 ) hsum = Hpen_fff * pow( hsum , 0.25 ) ;
   return hsum ;
}

/*----------------------------------------------------------------------------*/
/* Utility func: if everything is the same, we usually don't like this array */

static INLINE int is_float_array_constant( int n , float *v )
{
   int ii ;
   for( ii=1 ; ii < n && v[ii] == v[0] ; ii++ ) ; /*nada*/
   return (ii==n) ;
}

/*----------------------------------------------------------------------------*/
/* Clear the internal data for the -xyzmatch option */

void IW3D_xyzmatch_clear(void)
{
   FREEIFNN(Hxyzmatch_bas[0]) ; FREEIFNN(Hxyzmatch_bas[1]) ; FREEIFNN(Hxyzmatch_bas[2]) ;
   FREEIFNN(Hxyzmatch_src[0]) ; FREEIFNN(Hxyzmatch_src[1]) ; FREEIFNN(Hxyzmatch_src[2]) ;
   FREEIFNN(Hxyzmatch_b2s[0]) ; FREEIFNN(Hxyzmatch_b2s[1]) ; FREEIFNN(Hxyzmatch_b2s[2]) ;
   Hxyzmatch_num  = 0 ; Hxyzmatch_cost = 0.0 ;
   return ;
}

/*----------------------------------------------------------------------------*/
/* Convert two sets of vectors to the -xyzmatch internal data.
*//*--------------------------------------------------------------------------*/

int IW3D_xyzmatch_internalize( THD_3dim_dataset *dset , int npt ,
                               float *xb , float *yb , float *zb ,
                               float *xs , float *ys , float *zs  )
{
   int ii , nn ;
   float ib,jb,kb , is,js,ks , nx1,ny1,nz1 ;
   mat44 imat ;

   ENTRY("IW3D_xyzmatch_internalize") ;

   IW3D_xyzmatch_clear() ;

   if( dset == NULL || npt <  1    ||
       xb   == NULL || yb  == NULL || zb == NULL ||
       xs   == NULL || ys  == NULL || zs == NULL   ) RETURN(0) ;

   imat = MAT44_INV(dset->daxes->ijk_to_dicom) ; /* takes xyz to ijk */

   nx1 = DSET_NX(dset)-1.0f ; ny1 = DSET_NY(dset)-1.0f ; nz1 = DSET_NZ(dset)-1.0f ;

   Hxyzmatch_bas[0] = (float *)malloc(sizeof(float)*npt) ;
   Hxyzmatch_bas[1] = (float *)malloc(sizeof(float)*npt) ;
   Hxyzmatch_bas[2] = (float *)malloc(sizeof(float)*npt) ;
   Hxyzmatch_src[0] = (float *)malloc(sizeof(float)*npt) ;
   Hxyzmatch_src[1] = (float *)malloc(sizeof(float)*npt) ;
   Hxyzmatch_src[2] = (float *)malloc(sizeof(float)*npt) ;
   Hxyzmatch_b2s[0] = (float *)malloc(sizeof(float)*npt) ;
   Hxyzmatch_b2s[1] = (float *)malloc(sizeof(float)*npt) ;
   Hxyzmatch_b2s[2] = (float *)malloc(sizeof(float)*npt) ;

   for( nn=ii=0 ; ii < npt ; ii++ ){
     MAT44_VEC(imat,xb[ii],yb[ii],zb[ii],ib,jb,kb) ;  /* convert to indexes */
     MAT44_VEC(imat,xs[ii],ys[ii],zs[ii],is,js,ks) ;
     if( ib < 0.0f || ib >= nx1 ||
         jb < 0.0f || jb >= ny1 || kb < 0.0f || kb >= nz1 ) continue ;
     if( is < 0.0f || is >= nx1 ||
         js < 0.0f || js >= ny1 || ks < 0.0f || ks >= nz1 ) continue ;

     Hxyzmatch_bas[0][nn] = ib; Hxyzmatch_bas[1][nn] = jb; Hxyzmatch_bas[2][nn] = kb;
     Hxyzmatch_src[0][nn] = is; Hxyzmatch_src[1][nn] = js; Hxyzmatch_src[2][nn] = ks;
     nn++ ;
   }

   Hxyzmatch_num = nn ; RETURN(nn) ;
}

/*----------------------------------------------------------------------------*/
/* Warp the input xyz indexes given Haawarp and AHwarp.
   The input indexes will have been edited to be inside the Haawarp box,
     so external slopes are not needed.
   Note that the input points are the base image locations, which will
     be warped to the source image locations for comparison.
   The only tricky part is to use AHwarp inside its box, otherwise use Haawarp.
*//*--------------------------------------------------------------------------*/

void IW3D_nwarp_xyzmatch( int npt ,
                          float *xin , float *yin , float *zin ,
                          float *xut , float *yut , float *zut  )
{
   int nx,ny,nz,nxy , nbx,nby ;
   float *xdar , *ydar , *zdar ;
   float *xqar , *yqar , *zqar ;
   float *xxar[2], *yyar[2] , *zzar[2] ;

   nx   = Hnx         ; ny   = Hny         ; nz   = Hnz         ; nxy = Hnxy ;
   xdar = Haawarp->xd ; ydar = Haawarp->yd ; zdar = Haawarp->zd ;

   nbx  = AHwarp->nx ; nby  = AHwarp->ny ;
   xqar = AHwarp->xd ; yqar = AHwarp->yd ; zqar = AHwarp->zd ;

   /* pointers to displacement arrays for case 0 = outside AHwarp box */

   xxar[0] = xdar ; yyar[0] = ydar ; zzar[0] = zdar ;

   /* for case 1 = inside AHwarp box (cf. AHIND macro below);
      use pointer arithmetic fu to make them seem based at (i,j,k)=(0,0,0)
      [Note: could easily fail badly if on a segmented memory architecture] */

   xxar[1] = xqar - (Hibot + Hjbot*nbx + Hkbot*nbx*nby) ;
   yyar[1] = yqar - (Hibot + Hjbot*nbx + Hkbot*nbx*nby) ;
   zzar[1] = zqar - (Hibot + Hjbot*nbx + Hkbot*nbx*nby) ;

   /* parallel-ized linear interpolation [parallelizing probably useless] */

   AFNI_OMP_START ;
#pragma omp parallel if( npt > 111 )
   { int pp ;
     float xx,yy,zz , fx,fy,fz ; int ix,jy,kz ;
     int ix_00,ix_p1 , jy_00,jy_p1 , kz_00,kz_p1 ;
     int dc_00_00_00 , dc_p1_00_00 , dc_00_p1_00 , dc_p1_p1_00 ,
         dc_00_00_p1 , dc_p1_00_p1 , dc_00_p1_p1 , dc_p1_p1_p1  ;
     float wt_00,wt_p1 ;
     float f_j00_k00, f_jp1_k00, f_j00_kp1, f_jp1_kp1, f_k00, f_kp1 ;
     float g_j00_k00, g_jp1_k00, g_j00_kp1, g_jp1_kp1, g_k00, g_kp1 ;
     float h_j00_k00, h_jp1_k00, h_j00_kp1, h_jp1_kp1, h_k00, h_kp1 ;

#pragma omp for
     for( pp=0 ; pp < npt ; pp++ ){                 /* loop over input points */
       xx = xin[pp] ; yy = yin[pp] ; zz = zin[pp] ;
       ix = (int)xx; fx = xx-ix; jy = (int)yy; fy = yy-jy; kz = (int)zz; fz = zz-kz;
       /* now linearly interpolate displacements inside the dataset grid */
       ix_00 = ix ; ix_p1 = ix_00+1 ;  /* at this point, we are 'fx' between indexes ix_00 and ix_p1 */
       jy_00 = jy ; jy_p1 = jy_00+1 ;  /* et cetera */
       kz_00 = kz ; kz_p1 = kz_00+1 ;
       wt_00 = (1.0f-fx) ; wt_p1 = fx ;  /* weights for ix_00 and ix_p1 points for linear interp */

#undef  AHIND  /* == 0 for outside of AHwarp, == 1 for inside AHwarp */
#define AHIND(i,j,k) ( (i >= Hibot) && (i <= Hitop) &&  \
                       (j >= Hjbot) && (j <= Hjtop) &&  \
                       (k >= Hkbot) && (k <= Hktop)   )

       dc_00_00_00 = AHIND(ix_00,jy_00,kz_00) ; /* select case 0 or 1 */
       dc_p1_00_00 = AHIND(ix_p1,jy_00,kz_00) ; /* for each of the 8 */
       dc_00_p1_00 = AHIND(ix_00,jy_p1,kz_00) ; /* points used for  */
       dc_p1_p1_00 = AHIND(ix_p1,jy_p1,kz_00) ; /* interpolation   */
       dc_00_00_p1 = AHIND(ix_00,jy_00,kz_p1) ; /* ['dc' == displacement case] */
       dc_p1_00_p1 = AHIND(ix_p1,jy_00,kz_p1) ;
       dc_00_p1_p1 = AHIND(ix_00,jy_p1,kz_p1) ;
       dc_p1_p1_p1 = AHIND(ix_p1,jy_p1,kz_p1) ;

#undef  IJK  /* convert 3D index to 1D index */
#define IJK(i,j,k) ((i)+(j)*nx+(k)*nxy)

#undef  XINT  /* linear interpolation at plane jk, for cases dca,dcb */
#define XINT(aaa,j,k,dca,dcb) wt_00*aaa[dca][IJK(ix_00,j,k)]+wt_p1*aaa[dcb][IJK(ix_p1,j,k)]

       /* interpolate to location ix+fx at each jy,kz level */

       f_j00_k00 = XINT(xxar,jy_00,kz_00,dc_00_00_00,dc_p1_00_00) ;
       f_jp1_k00 = XINT(xxar,jy_p1,kz_00,dc_00_p1_00,dc_p1_p1_00) ;
       f_j00_kp1 = XINT(xxar,jy_00,kz_p1,dc_00_00_p1,dc_p1_00_p1) ;
       f_jp1_kp1 = XINT(xxar,jy_p1,kz_p1,dc_00_p1_p1,dc_p1_p1_p1) ;
       g_j00_k00 = XINT(yyar,jy_00,kz_00,dc_00_00_00,dc_p1_00_00) ;
       g_jp1_k00 = XINT(yyar,jy_p1,kz_00,dc_00_p1_00,dc_p1_p1_00) ;
       g_j00_kp1 = XINT(yyar,jy_00,kz_p1,dc_00_00_p1,dc_p1_00_p1) ;
       g_jp1_kp1 = XINT(yyar,jy_p1,kz_p1,dc_00_p1_p1,dc_p1_p1_p1) ;
       h_j00_k00 = XINT(zzar,jy_00,kz_00,dc_00_00_00,dc_p1_00_00) ;
       h_jp1_k00 = XINT(zzar,jy_p1,kz_00,dc_00_p1_00,dc_p1_p1_00) ;
       h_j00_kp1 = XINT(zzar,jy_00,kz_p1,dc_00_00_p1,dc_p1_00_p1) ;
       h_jp1_kp1 = XINT(zzar,jy_p1,kz_p1,dc_00_p1_p1,dc_p1_p1_p1) ;

       /* interpolate to jy+fy at each kz level */

       wt_00 = 1.0f-fy ; wt_p1 = fy ;
       f_k00 =  wt_00 * f_j00_k00 + wt_p1 * f_jp1_k00 ;
       f_kp1 =  wt_00 * f_j00_kp1 + wt_p1 * f_jp1_kp1 ;
       g_k00 =  wt_00 * g_j00_k00 + wt_p1 * g_jp1_k00 ;
       g_kp1 =  wt_00 * g_j00_kp1 + wt_p1 * g_jp1_kp1 ;
       h_k00 =  wt_00 * h_j00_k00 + wt_p1 * h_jp1_k00 ;
       h_kp1 =  wt_00 * h_j00_kp1 + wt_p1 * h_jp1_kp1 ;

       /* interpolate to kz+fz to get output, plus add in original coords */

       xut[pp] = (1.0f-fz) * f_k00 + fz * f_kp1 + xin[pp] ;
       yut[pp] = (1.0f-fz) * g_k00 + fz * g_kp1 + yin[pp] ;
       zut[pp] = (1.0f-fz) * h_k00 + fz * h_kp1 + zin[pp] ;

     } /* end of loop over input/output points */
   } /* end of parallel code */
   AFNI_OMP_END ;

   return ;
}

/*----------------------------------------------------------------------------*/
/* Part of the penalty computed from the -xyzmatch option in 3dQwarp:
   a sum over distances between discrete points.
*//*--------------------------------------------------------------------------*/

double IW3D_xyzmatch_sum(void)
{
   int ii ; double sum = 0.0 ;

   IW3D_nwarp_xyzmatch( Hxyzmatch_num ,
                        Hxyzmatch_bas[0], Hxyzmatch_bas[1], Hxyzmatch_bas[2],
                        Hxyzmatch_b2s[0], Hxyzmatch_b2s[1], Hxyzmatch_b2s[2] ) ;

   switch ( Hxyzmatch_pow ){

     case 2:  /* L2 sum = RMS distance */
     default:
       for( ii=0 ; ii < Hxyzmatch_num ; ii++ ){
         sum +=   SQR( Hxyzmatch_b2s[0][ii]-Hxyzmatch_src[0][ii] )
                + SQR( Hxyzmatch_b2s[1][ii]-Hxyzmatch_src[1][ii] )
                + SQR( Hxyzmatch_b2s[2][ii]-Hxyzmatch_src[2][ii] ) ;
       }
       sum = sqrt(sum/Hxyzmatch_num) ;
     break ;

     case 1:  /* L1 sum = average taxicab distance */
       for( ii=0 ; ii < Hxyzmatch_num ; ii++ ){
         sum +=   fabs( Hxyzmatch_b2s[0][ii]-Hxyzmatch_src[0][ii] )
                + fabs( Hxyzmatch_b2s[1][ii]-Hxyzmatch_src[1][ii] )
                + fabs( Hxyzmatch_b2s[2][ii]-Hxyzmatch_src[2][ii] ) ;
       }
       sum /= Hxyzmatch_num ;
     break ;
   }

   return (sum * Hxyzmatch_fac) ;
}

/*----------------------------------------------------------------------------*/
/* This is the function which will actually be minimized
   (via Powell's NEWUOA); and as such is the very core of 3dQwarp!
*//*--------------------------------------------------------------------------*/

double IW3D_scalar_costfun( int npar , double *dpar )
{
   double cost=0.0 ; int ii ;

   /* Step 1: setup compute Hwarp given the params */

   if( Hparmap != NULL ){  /* expand via the parameter map to a full list */
                            /* since all the warp eval functions need all */
                                           /* the parameters, used or not */

     for( ii=0 ; ii < Hnpar ; ii++ ) Hpar[ii] = 0.0f ;
     for( ii=0 ; ii < npar  ; ii++ ) Hpar[ Hparmap[ii] ] = (float)dpar[ii] ;

   } else {                /* we received a full list of parameters */

     for( ii=0 ; ii < Hnpar ; ii++ ){
       Hpar[ii] = (float)dpar[ii] ;
       if( !isfinite(Hpar[ii]) ){
         ERROR_message("bad Hpar[%d]=%g dpar=%g",ii,Hpar[ii],dpar[ii]) ;
         Hpar[ii] = dpar[ii] = 0.0 ;
       }
     }

   }

#ifdef USE_HLOADER
   Hloader(Hpar) ;  /* loads Hwarp prior to Hwarp_apply() */
#endif

   /* Step 2: compute warped image over the patch, into Hwval array */

   Hwarp_apply(Hwval) ;

#if 0
  if( is_float_array_constant(Hnval,Hwval) )
    fprintf(stderr," costfun: Hwval is constant %g\n",Hwval[0]) ;
#endif

   /* Step 3: compute the actual cost function */
   /* -- the first case in the '?' expressions is when the patch
         is smaller than the whole volume;
      -- the second case is when the patch covers the entire universe */

   cost = INCOR_evaluate( Hincor , Hnval ,
                          (Hbval != NULL ) ? Hbval     /* base image in patch */
                                           : MRI_FLOAT_PTR(BASIM),
                          Hwval ,             /* warped source image in patch */
                          (Haawt != NULL ) ? Haawt   /* weight image in patch */
                                           : MRI_FLOAT_PTR(Hwtim) ) ;

   if( Hnegate ) cost = -cost ;  /* change the sign? (for minimization) */

   if( !isfinite(cost) ){  /* bad bad Leroy Brown */
     ERROR_message("bad Warpomatic cost = %g -- input parameters:",cost) ;
     for( ii=0 ; ii < npar ; ii++ ) fprintf(stderr," %g",dpar[ii]) ;
     fprintf(stderr,"\n") ;
   }

   Hcostt = cost ;  /* store 'pure' cost globally for reporting purposes */

   /* Step 4: add the penalty function into the output cost */

   if( Hpen_use ){
     Hpenn = HPEN_penalty() ; cost += Hpenn ;  /* penalty is saved in Hpenn */
   } else {
     Hpenn = 0.0f ;
   }

   if( Hxyzmatch_num > 0 ){
     Hxyzmatch_cost = IW3D_xyzmatch_sum() ; cost += Hxyzmatch_cost ;
   }

   if( Hfirsttime ){  /* just for fun fun fun in the sun sun sun */
     if( Hverb ) fprintf(stderr,"[first cost=%.5f]%c",cost , ((Hverb>1) ? '\n' : ' ') ) ;
     Hfirsttime = 0 ; Hfirstcost = (float)cost ;
   }

   return cost ;
}

#endif /*(Q5)*/ /*############################################################*/

#if 1
/*============================================================================*/
/** (Q6) Functions to setup global variables for warp optimization process   **/
/*============================================================================*/

/*----------------------------------------------------------------------------*/
/* Delete various workspaces created for warp improvement */

void IW3D_cleanup_improvement(void)
{
ENTRY("IW3D_cleanup_improvement") ;

   mri_free(Hbasim)   ; Hbasim   = NULL ;
   mri_free(Hsrcim)   ; Hsrcim   = NULL ;
   mri_free(Hwtim)    ; Hwtim    = NULL ; FREEIFNN(Hbmask) ;
   mri_free(Haasrcim) ; Haasrcim = NULL ;

   mri_free(Hsrcim_blur) ; Hsrcim_blur = NULL ;
   mri_free(Hbasim_blur) ; Hbasim_blur = NULL ;

   IW3D_destroy(Hwarp)   ; Hwarp   = NULL ;
   IW3D_destroy(AHwarp)  ; AHwarp  = NULL ;
   IW3D_destroy(Haawarp) ; Haawarp = NULL ;

   IW3D_xyzmatch_clear() ;     /* 15 Aug 2014 */

   INCOR_set_lpc_mask(NULL) ;  /* 25 Jun 2014 */
   INCOR_destroy(Hincor) ; Hincor = NULL ; KILL_floatvec(Hmpar) ;
   FREEIFNN(Hpar) ; FREEIFNN(Hwval) ; FREEIFNN(Haawt) ; FREEIFNN(Hbval) ;
   FREEIFNN(Hparmap) ; Hnparmap = Hnpar = 0 ; Hbasis_code = -666 ;

   FREEIFNN(bc0x); FREEIFNN(bc1x); nbcx=0;
   FREEIFNN(bc0y); FREEIFNN(bc1y); nbcy=0;
   FREEIFNN(bc0z); FREEIFNN(bc1z); nbcz=0;
   FREEIFNN(bc2x); FREEIFNN(bc3x); FREEIFNN(bc4x);
   FREEIFNN(bc2y); FREEIFNN(bc3y); FREEIFNN(bc4y);
   FREEIFNN(bc2z); FREEIFNN(bc3z); FREEIFNN(bc4z);

   FREEIFNN(bq0x); FREEIFNN(bq1x); FREEIFNN(bq2x); nbqx=0;
   FREEIFNN(bq0y); FREEIFNN(bq1y); FREEIFNN(bq2y); nbqy=0;
   FREEIFNN(bq0z); FREEIFNN(bq1z); FREEIFNN(bq2z); nbqz=0;

   if( bbbcar != NULL ){
     int ii ;
     for( ii=0 ; ii < nbbbcar ; ii++ ) FREEIFNN(bbbcar[ii]) ;
     free(bbbcar) ; nbbcxyz = nbbbcar = 0 ; bbbcar = NULL ;
   }

   if( bbbqar != NULL ){
     int ii ;
     for( ii=0 ; ii < 27 ; ii++ ) FREEIFNN(bbbqar[ii]) ;
     free(bbbqar) ; nbbqxyz = 0 ; bbbqar = NULL ;
   }

   Hstopcost = -666666.6f ;
   Hstopped  = 0 ;
   Hfinal    = 0 ;

   HSAVE_DESTROY ;

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/
/* Median filter specialized for 3dQwarp and for OpenMP. */

MRI_IMAGE *IW3D_medianfilter( MRI_IMAGE *imin, float irad )
{
   MRI_IMAGE *imout ;
   float *fin, *fout , dz ;
   short *di , *dj  , *dk ;
   int nd, nx,ny,nz,nxy,nxyz ;
   MCW_cluster *cl ;

ENTRY("IW3D_medianfilter") ;

   if( imin == NULL || imin->kind != MRI_float ) RETURN(NULL) ;

   if( irad < 1.01f ) irad = 1.01f ;
   dz = (imin->nz == 1) ? 6666.0f : 1.0f ;
   cl = MCW_build_mask( 1.0f,1.0f,dz , irad ) ;

   if( cl == NULL || cl->num_pt < 6 ){ KILL_CLUSTER(cl); RETURN(NULL); }

   ADDTO_CLUSTER(cl,0,0,0,0) ;

   di = cl->i   ; dj = cl->j   ; dk = cl->k   ; nd  = cl->num_pt;
   nx = imin->nx; ny = imin->ny; nz = imin->nz; nxy = nx*ny     ; nxyz = nxy*nz;

   imout = mri_new_conforming( imin , MRI_float ) ;
   fout  = MRI_FLOAT_PTR( imout ) ;
   fin   = MRI_FLOAT_PTR( imin ) ;

 AFNI_OMP_START ;
#pragma omp parallel if( nxyz > 6666 )
 { int ii,jj,kk,ijk , ip,jp,kp , nt,dd ; float *tmp ;
#pragma omp critical
   { tmp = (float *)malloc(sizeof(float)*nd) ; }
#pragma omp for
   for( ijk=0 ; ijk < nxyz ; ijk++ ){
     ii = ijk % nx ; kk = ijk / nxy ; jj = (ijk-kk*nxy) / nx ;
     for( nt=dd=0 ; dd < nd ; dd++ ){ /* extract neighborhood values */
       ip = ii+di[dd] ; if( ip < 0 || ip >= nx ) continue ;
       jp = jj+dj[dd] ; if( jp < 0 || jp >= ny ) continue ;
       kp = kk+dk[dd] ; if( kp < 0 || kp >= nz ) continue ;
       tmp[nt++] = fin[ip+jp*nx+kp*nxy] ;
     }
     if( nt > 0 ) fout[ijk] = qmed_float( nt , tmp ) ;  /* cf. cs_qmed.c */
   }
#pragma omp critical
   { free(tmp) ; }
 }
AFNI_OMP_END ;

   KILL_CLUSTER(cl);
   RETURN(imout) ;
}

/*----------------------------------------------------------------------------*/
/* Function for blurring a volume in 1 of 2 ways */

MRI_IMAGE * IW3D_blurim( float rad , MRI_IMAGE *inim , char *label )
{
   MRI_IMAGE *outim = NULL ;
ENTRY("IW3D_blurim") ;
   if( rad >= 0.5f ){
     if( Hverb > 1 && label != NULL )
       ININFO_message("  blurring %s image %.2f voxels FWHM",label,rad) ;
     outim = mri_float_blur3D( FWHM_TO_SIGMA(rad) , inim ) ;
   } else if( rad <= -1.0f ){
     if( Hverb > 1 && label != NULL )
       ININFO_message("  median-ating %s image %.2f voxels",label,-rad) ;
     outim = IW3D_medianfilter( inim , -rad ) ;
   }
   RETURN(outim) ;
}

/*----------------------------------------------------------------------------*/
/* Macro for actual pblur radius, given image dimensions */

#define PBLUR(pb,qx,qy,qz) \
 ( ((pb) <= 0.0f)          \
  ? 0.0f                   \
  : ( ( ((qz) >= NGMIN) ? cbrtf((qx)*(qy)*(qz)) : sqrtf((qx)*(qy)) ) * (pb) ) )

/* Macro to compute actual blur radius from the
   progressive blur (pb) and the fixed blur (fb) radii */

#define ACTUAL_BLUR(pb,fb) \
  ( sqrtf( (pb)*(pb) + (fb)*(fb) ) * ( ((fb) >= 0.0f) ? 1.0f : -1.0f ) )

/*----------------------------------------------------------------------------*/
/* function to do the blurring */

MRI_IMAGE * IW3D_do_blurring( float fixed_blur, float prog_blur,
                              float scalex, float scaley, float scalez,
                              MRI_IMAGE *inim , char *lab )
{
   float pblur,ablur ; MRI_IMAGE *outim ;

ENTRY("IW3D_do_blurring") ;
   if( fixed_blur == 0.0f && prog_blur == 0.0f ) RETURN(NULL) ;
   pblur = PBLUR(prog_blur,scalex,scaley,scalez) ;
   ablur = ACTUAL_BLUR(pblur,fixed_blur) ;
   outim = IW3D_blurim( ablur , inim , lab ) ;
   RETURN(outim) ;
}

/*------------------- Macros for blurring images as needed -------------------*/

#define PBLUR_BASE(i1,i2,j1,j2,k1,k2)                                                \
 do{ if( Hpblur_b > 0.0f ){                                                          \
       mri_free(Hbasim_blur) ;                                                       \
       Hbasim_blur = IW3D_do_blurring( Hblur_b, Hpblur_b,                            \
                                       i2-i1+1, j2-j1+1, k2-k1+1, Hbasim, "base" ) ; \
     } } while(0)

#define PBLUR_SOURCE(i1,i2,j1,j2,k1,k2)                                                \
 do{ if( Hpblur_s > 0.0f ){                                                            \
       mri_free(Hsrcim_blur) ;                                                         \
       Hsrcim_blur = IW3D_do_blurring( Hblur_s, Hpblur_s,                              \
                                       i2-i1+1, j2-j1+1, k2-k1+1, Hsrcim, "source" ) ; \
     } } while(0)

/*----------------------------------------------------------------------------*/
/* Sets a bunch of global workspace variables, prior to
   iteratively improving the warp with function IW3D_improve_warp() */

void IW3D_setup_for_improvement( MRI_IMAGE *bim, MRI_IMAGE *wbim, MRI_IMAGE *sim,
                                 IndexWarp3D *Iwarp,
                                 int meth_code, int warp_flags )
{
   int iii , nmask ;

ENTRY("IW3D_setup_for_improvement") ;

   /*-- check for errorosities --*/

   if( bim == NULL )
     ERROR_exit("IW3D_setup_for_improvement: bad bim input") ;

   if( sim == NULL )
     ERROR_exit("IW3D_setup_for_improvement: bad sim input") ;

   if( sim->nx != bim->nx || sim->ny != bim->ny || sim->nz != bim->nz )
     ERROR_exit("IW3D_setup_for_improvement: bim and sim grids don't match") ;

   /*-- eliminate old stuff (if any) --*/

   IW3D_cleanup_improvement() ;

   /*-- copy base and source images --*/

   Hnx = bim->nx; Hny = bim->ny; Hnz = bim->nz; Hnxy=Hnx*Hny; Hnxyz = Hnxy*Hnz;
   Hbasim = mri_to_float(bim) ;
   Hsrcim = mri_to_float(sim);

#ifdef ALLOW_INEDGE /* Jul 2018 */
   if( Hinedge_doit ){
     if( Hverb > 1 ) ININFO_message("  enhancing interior edges of base and source") ;
     mri_interior_edgeize( Hbasim , Hinedge_erode , Hinedge_frac ) ;
     mri_interior_edgeize( Hsrcim , Hinedge_erode , Hinedge_frac ) ;
   }
#endif

   if( Hpblur_b > 0.0f && Hblur_b == 0.0f ) Hblur_b = 0.1f ;
   if( Hpblur_s > 0.0f && Hblur_s == 0.0f ) Hblur_s = 0.1f ;

   Hbasim_blur = IW3D_do_blurring( Hblur_b , Hpblur_b ,
                                   0.5f*Hnx,0.5f*Hny,0.5f*Hnz, Hbasim, "base"   ) ;
   Hsrcim_blur = IW3D_do_blurring( Hblur_s , Hpblur_s ,
                                   0.5f*Hnx,0.5f*Hny,0.5f*Hnz, Hsrcim, "source" ) ;

   /*-- copy or create base weight image (included Hemask for exclusion) --*/

   if( wbim != NULL ){                           /*-- user supplied weight --*/

     int ii,nwb,nexc ; float *wbfar ;
     if( wbim->kind != MRI_float ||
         wbim->nx != Hnx || wbim->ny != Hny || wbim->nz != Hnz )
       ERROR_exit("IW3D_setup_for_improvement: bad wbim input") ;

     Hwtim = mri_to_float(wbim) ; wbfar = MRI_FLOAT_PTR(Hwtim) ;
     Hbmask = (byte *)malloc(sizeof(byte)*Hnxyz) ;
     for( Hwbar=nwb=nexc=ii=0 ; ii < Hnxyz ; ii++ ){
       if( Hemask != NULL && Hemask[ii] && wbfar[ii] > 0.0f ){  /* 29 Oct 2012 */
         nexc++ ; wbfar[ii] = 0.0f ;
       }
       Hbmask[ii] = (wbfar[ii] > 0.0f) ;
       if( Hbmask[ii] ){ Hwbar += wbfar[ii] ; nwb++ ; }
       else            { wbfar[ii] = 0.0f ; }
     }
     if( Hwbar == 0.0f || nwb == 0 )
       ERROR_exit("IW3D_setup_for_improvement: all zero wbim input") ;
     if( Hverb > 1 ) ININFO_message("   %d voxels in mask (out of %d = %.2f%%)",
                                     nwb,Hnxyz,(100.0*nwb)/Hnxyz ) ;
     Hwbar /= nwb ;  /* average value of all nonzero weights */
     nmask = nwb ;
     if( nexc > 0 ) ININFO_message("-emask excluded %d voxels",nexc) ;

   } else {                               /*-- make weight up from nowhere --*/

     int ii,nwb,nexc ; float *wbfar ;
     Hwtim = mri_new_vol(Hnx,Hny,Hnz,MRI_float); wbfar = MRI_FLOAT_PTR(Hwtim);
     Hbmask = (byte *)malloc(sizeof(byte)*Hnxyz) ;
     for( Hwbar=nwb=nexc=ii=0 ; ii < Hnxyz ; ii++ ){
       if( Hemask != NULL && Hemask[ii] ){ wbfar[ii] = 0.0f; Hbmask[ii] = 0; nexc++; }
       else                              { wbfar[ii] = 1.0f; Hbmask[ii] = 1; }
       if( Hbmask[ii] ){ Hwbar += wbfar[ii] ; nwb++ ; }
     }
     if( Hwbar == 0.0f || nwb == 0 )
       ERROR_exit("IW3D_setup_for_improvement: all zero mask!?") ;
     Hwbar /= nwb ;  /* average value of all nonzero weights */
     nmask = nwb ;
     if( nexc > 0 ) ININFO_message("-emask excluded %d voxels",nexc) ;

   }

   /*-- setup codes and other stuff for the 'correlation' (INCOR_) function --*/

   Hmatch_code = meth_code ;
   iii = INCOR_check_meth_code(meth_code) ;  /* iii tells what to setup */
   if( iii == 0 )
     ERROR_exit("IW3D_setup_for_improvement: bad meth_code input=%d",meth_code) ;

   /* decide if the cost function needs to be negated before minimization */

   switch( meth_code ){
     default:                           Hnegate = 0 ; break ;

     case GA_MATCH_PEARSON_LOCALA:      /* lpa (but NOT lpc) */
     case GA_MATCH_HELLINGER_SCALAR:    /* hel */
     case GA_MATCH_CRAT_USYM_SCALAR:    /* correlation ratio */
     case GA_MATCH_CRAT_SADD_SCALAR:
     case GA_MATCH_CORRATIO_SCALAR:
     case GA_MATCH_KULLBACK_SCALAR:     /* mutual info */
     case GA_MATCH_PEARCLP_SCALAR:      /* clipped Pearson */
     case GA_MATCH_PEARSON_SCALAR:      /* pure Pearson */
                                        Hnegate = 1 ; break ;
   }

   /* special case: don't try to over-minimize the Pearson costs
      (e.g., the base and source images are essentially identical) */

   if( meth_code == GA_MATCH_PEARCLP_SCALAR || meth_code == GA_MATCH_PEARSON_SCALAR )
     Hstopcost = -3.991f ;

   /*-- INCOR method uses 2Dhist functions (iii==2),
        or is the clipped Pearson method (iii==3),
        or is a local Pearson method (iii==4),
        so setup some parameters (into Hmpar) for later use --*/

   if( iii == 2 || iii == 3 ){
     float *xar,*yar , *bar,*sar ; int jj,kk ;
     float_quad xyc , xym ;
     bar = MRI_FLOAT_PTR(BASIM) ; sar = MRI_FLOAT_PTR(SRCIM) ;
     if( nmask == Hnxyz ){  /* entire volume is in mask! */
       xar = bar ; yar = sar ; kk = Hnxyz ;
     } else {
       xar = (float *)malloc(sizeof(float)*nmask) ;
       yar = (float *)malloc(sizeof(float)*nmask) ;
       for( jj=kk=0 ; jj < Hnxyz ; jj++ ){
         if( Hbmask[jj] ){ xar[kk] = bar[jj] ; yar[kk++] = sar[jj] ; }
       }
     }
     xym = INCOR_2Dhist_minmax( kk , xar , yar ) ;
     xyc = INCOR_2Dhist_xyclip( kk , xar , yar ) ;
     if( xar != bar ){ free(xar) ; free(yar) ; }
     MAKE_floatvec(Hmpar,9) ;
     if( iii == 2 ){              /* histogram parameter setup */
       INCOR_setup_good(Hnxyz) ;
       Hmpar->ar[0] = (float)INCOR_2Dhist_compute_nbin(nmask) ;
       Hmpar->ar[1] = xym.a ; Hmpar->ar[2] = xym.b ;  /* xbot  xtop  */
       Hmpar->ar[3] = xym.c ; Hmpar->ar[4] = xym.d ;  /* ybot  ytop  */
       Hmpar->ar[5] = xyc.a ; Hmpar->ar[6] = xyc.b ;  /* xcbot xctop */
       Hmpar->ar[7] = xyc.c ; Hmpar->ar[8] = xyc.d ;  /* ycbot yctop */
       if( Hverb > 1 ){
         ININFO_message("   2Dhist: nbin=%d",(int)Hmpar->ar[0]) ;
         ININFO_message("           xbot=%g xcbot=%g xctop=%g xtop=%g",
                        Hmpar->ar[1], Hmpar->ar[5], Hmpar->ar[6], Hmpar->ar[2] ) ;
         ININFO_message("           ybot=%g ycbot=%g yctop=%g ytop=%g",
                        Hmpar->ar[3], Hmpar->ar[7], Hmpar->ar[8], Hmpar->ar[4] ) ;
       }
     } else if( iii == 3 ){       /* clipped Pearson setup */
       float d1 , d2 , dif ;
       d2 = 0.05f*(xyc.b-xyc.a) ; /* 5% of x clip range */
       d1 = 0.5f*(xyc.a-xym.a) ;  /* half of x clip bot to x min */
                                 dif = MIN(d1,d2) ; Hmpar->ar[1] = xyc.a-dif ; /* xdbot */
       d1 = 0.5f*(xym.b-xyc.b) ; dif = MIN(d1,d2) ; Hmpar->ar[2] = xyc.b+dif ; /* xdtop */
       d2 = 0.05f*(xyc.d-xyc.c) ;
       d1 = 0.5f*(xyc.c-xym.c) ; dif = MIN(d1,d2) ; Hmpar->ar[3] = xyc.c-dif ; /* ydbot */
       d1 = 0.5f*(xym.d-xyc.d) ; dif = MIN(d1,d2) ; Hmpar->ar[4] = xyc.d+dif ; /* ydtop */
       Hmpar->ar[5] = xyc.a ; Hmpar->ar[6] = xyc.b ;                     /* xcbot xctop */
       Hmpar->ar[7] = xyc.c ; Hmpar->ar[8] = xyc.d ;                     /* ycbot yctop */
#if 0
       if( Hverb ){
         ININFO_message("  PEARCLP: xdbot=%g xcbot=%g xctop=%g xdtop=%g",
                        Hmpar->ar[1], Hmpar->ar[5], Hmpar->ar[6], Hmpar->ar[2] ) ;
         ININFO_message("           ydbot=%g ycbot=%g yctop=%g ydtop=%g",
                        Hmpar->ar[3], Hmpar->ar[7], Hmpar->ar[8], Hmpar->ar[4] ) ;
       }
#endif
     }
   } else if( iii == 4 ){         /*--- Local Pearson setup [25 Jun 2014] ---*/
     INCOR_set_lpc_mask(Hbmask) ;
     MAKE_floatvec(Hmpar,9) ;     /* to be filled in later, for each patch */
   }

   /*-- set global flags for displacements (what is allowed to move) --*/

   Hgflags = IW3D_munge_flags(Hnx,Hny,Hnz,warp_flags) ;
   if( Hgflags < 0 )
     ERROR_exit("IW3D_setup_for_improvement: bad warp_flags input") ;

   /*-- copy/create initial warp, and warp the source image that way --*/
   /*** [10 Aug 2014 -- Haasrcim will be created later] ***/

   if( Iwarp != NULL ){
     if( Iwarp->nx != Hnx || Iwarp->ny != Hny || Iwarp->nz != Hnz )
       ERROR_exit("IW3D_setup_for_improvement: bad Iwarp input") ;

     Haawarp  = IW3D_copy(Iwarp,1.0f) ;     /* copy it */
   } else {
     Haawarp  = IW3D_create(Hnx,Hny,Hnz) ;  /* initialized to 0 displacements */
   }

   (void)IW3D_load_energy(Haawarp) ;  /* initialize energy field for penalty use */

   EXRETURN ;
}

#endif /*(Q6)*/ /*############################################################*/

#if 1
/*============================================================================*/
/** (Q7) Function that actually optimizes one incremental patch warp         **/
/*============================================================================*/

/*----------------------------------------------------------------------------*/
/* Given a global warp Haawarp, improve it locally over a rectangular patch.
   The patch is ibot..itop X jbot..jtop X kbot..ktop (inclusive).
   Also, keep up-to-date the copy of the warped source image Haasrcim.
*//*--------------------------------------------------------------------------*/

#define SC_BOX  1
#define SC_BALL 2

#define BALLOPT                                         \
  do{ int bc = powell_newuoa_get_con() ;                \
      if( bc != SC_BALL ){                              \
        powell_newuoa_set_con_ball() ;                  \
        if( Hverb > 1 ) fprintf(stderr,"[ballopt]\n") ; \
      }                                                 \
  } while(0)

#define BOXOPT                                          \
  do{ int bc = powell_newuoa_get_con() ;                \
      if( bc != SC_BOX  ){                              \
        powell_newuoa_set_con_box() ;                   \
        if( Hverb > 1 ) fprintf(stderr,"[boxopt]\n") ;  \
      }                                                 \
  } while(0)

int IW3D_improve_warp( int warp_code ,
                       int ibot, int itop, int jbot, int jtop, int kbot, int ktop )
{
   MRI_IMAGE *warpim ;
   int nxh,nyh,nzh , ii,jj,kk , iter,itmax,qq,pp , nwb , nball ;
   float *wbfar , wsum ; double prad ;
   double *parvec, *xbot,*xtop ;
   float *sar , *Axd,*Ayd,*Azd,*Aje,*Ase , *bxd,*byd,*bzd,*bje,*bse , jt,st ;
   int ballopt = (SC_BALL == powell_newuoa_get_con()) ;  /* 30 Oct 2015 */

ENTRY("IW3D_improve_warp") ;

   /*-- setup local region for Hwarp --*/

   CLIP(ibot,Hnx-1) ; CLIP(itop,Hnx-1) ;
   CLIP(jbot,Hny-1) ; CLIP(jtop,Hny-1) ;
   CLIP(kbot,Hnz-1) ; CLIP(ktop,Hnz-1) ;

   nxh = itop-ibot+1 ; nyh = jtop-jbot+1 ; nzh = ktop-kbot+1 ;  /* sizes */

   /* can't do something if patch is TOO small */

   if( nxh < NGMIN && nyh < NGMIN && nzh < NGMIN ){ Hskipped++ ;  RETURN(0) ; }

   Hibot = ibot ; Hitop = itop ; /* index range of the patch we're working on */
   Hjbot = jbot ; Hjtop = jtop ; /* (global variables) */
   Hkbot = kbot ; Hktop = ktop ;

   /* test if this region has enough "weight" to process */

   Hnval = nxh*nyh*nzh ;  /* number of points in this patch */

   nball = (warp_code==MRI_CUBIC) ? 15 : 19 ;
   if( nxh < nball || nyh < nball || nzh < nball ){      /* 10 Feb 2017 */
     ballopt = 1 ; BALLOPT ;
   }

   wbfar = MRI_FLOAT_PTR(Hwtim) ; wsum = 0.0f ;  /* sum of weights in the patch */
   for( nwb=0,kk=kbot ; kk <= ktop ; kk++ ){
     for( jj=jbot ; jj <= jtop ; jj++ ){
       for( ii=ibot ; ii <= itop ; ii++ ){
         qq = ii + jj*Hnx + kk*Hnxy ;
         if( Hbmask[qq] ){ wsum += wbfar[qq] ; nwb++ ; }
   }}}

   if( !Hforce && (nwb < 0.333f*Hnval || wsum < 0.166f*Hnval*Hwbar) ){ /* too light for us */
     if( Hverb > 2 )
       ININFO_message(
         "     %s patch %03d..%03d %03d..%03d %03d..%03d : skipping (%.1f%% inmask %.1f%% weight)" ,
                       WARP_CODE_STRING(warp_code) ,
                       ibot,itop, jbot,jtop, kbot,ktop ,
                       (100.0f*nwb)/Hnval , (100.0f*wsum)/(Hnval*Hwbar) ) ;
     Hskipped++ ; RETURN(0) ;
   }

   /*-- setup the basis functions for Hwarp-ing --*/

   /*-- The max displacment scales (0.033 for cubic, 0.007 for quintic)
        are set from the combination of parameters that gave non-singular
        patch warps with "OK" levels of distortion (delta-volume about 50%).
        Hfactor allows the iteration to scale these down at finer levels,
        but that wasn't needed after the introduction of the warp penalty. --*/

   /*-- If ball optimization is on, the max displacement scales are larger! --*/

   switch( warp_code ){
     default:
     case MRI_CUBIC:
       Hbasis_code   = MRI_CUBIC ;                   /* 3rd order polynomials */
       Hbasis_parmax = 0.0444*Hfactor ;   /* max displacement from 1 function */
       if( ballopt ) Hbasis_parmax = 0.0777*Hfactor ;          /* 13 Jan 2015 */
       Hnpar         = 24 ;                /* number of params for local warp */
       prad          = 0.333 ;                       /* NEWUOA initial radius */
       HCwarp_setup_basis( nxh,nyh,nzh, Hgflags ) ;      /* setup HCwarp_load */
#ifdef USE_HLOADER
       Hloader       = HCwarp_load ;   /* func to make local warp from params */
#endif
     break ;

     case MRI_QUINTIC:
       Hbasis_code   = MRI_QUINTIC ;                 /* 5th order polynomials */
       Hbasis_parmax = 0.0088*Hfactor ;
       if( ballopt ) Hbasis_parmax = 0.066*Hfactor ;           /* 13 Jan 2015 */
       Hnpar         = 81 ;
       prad          = 0.333 ;
       HQwarp_setup_basis( nxh,nyh,nzh, Hgflags ) ;
#ifdef USE_HLOADER
       Hloader       = HQwarp_load ;
#endif
     break ;

#ifdef ALLOW_BASIS5  /* 05 Nov 2015 */
     case MRI_CUBIC_PLUS_1:  /* basis3 */
       BALLOPT ; ballopt = 1 ;
       Hbasis_code = MRI_CUBIC_PLUS_1 ;
       Hbasis_parmax = 0.0432*Hfactor ;
       Hnpar         = 81 ;
       prad          = 0.333 ;
       HCwarp_setup_basis5( nxh,nyh,nzh, Hgflags , 1 ) ;
     break ;

     case MRI_CUBIC_PLUS_2:  /* basis4 */
       BALLOPT ; ballopt = 1 ;
       Hbasis_code = MRI_CUBIC_PLUS_2 ;
       Hbasis_parmax = 0.0222*Hfactor ;
       Hnpar         = 192 ;
       prad          = 0.333 ;
       HCwarp_setup_basis5( nxh,nyh,nzh, Hgflags , 2 ) ;
     break ;

     case MRI_CUBIC_PLUS_3:  /* basis5 */
       BALLOPT ; ballopt = 1 ;
       Hbasis_code = MRI_CUBIC_PLUS_3 ;
       Hbasis_parmax = 0.0155*Hfactor ;
       Hnpar         = 375 ;
       prad          = 0.333 ;
       HCwarp_setup_basis5( nxh,nyh,nzh, Hgflags , 3 ) ;
     break ;
#endif
   }

   /* skip if not enough points for number of parameters [07 Apr 2016] */

   if( nwb < 2*Hnparmap ){  /* Hnparmap was set in a *setup_basis* just above */
     if( Hverb > 2 )
       ININFO_message(
         "     %s patch %03d..%03d %03d..%03d %03d..%03d : skipping (%d voxels inmask vs %d parameters)" ,
                       WARP_CODE_STRING(warp_code) ,
                       ibot,itop, jbot,jtop, kbot,ktop ,
                       nwb , Hnparmap ) ;
     Hskipped++ ; RETURN(0) ;
   }

   /* mark what is allowed to be warped */

   Hdox = !(Hflags & NWARP_NOXDIS_FLAG) ;  /* do the x direction? */
   Hdoy = !(Hflags & NWARP_NOYDIS_FLAG) ;  /* y? */
   Hdoz = !(Hflags & NWARP_NOZDIS_FLAG) ;  /* z? */

   /* create parameters that define Hwarp (these are to be optimized) */

   Hpar  = (float *)realloc(Hpar,sizeof(float)*Hnpar) ; /* parameter array */
   for( ii=0 ; ii < Hnpar ; ii++ ) Hpar[ii] = 0.0f ;
   Hxpar = Hpar ;               /* sub-array for x displacements */
   Hypar = Hxpar + (Hnpar/3) ;
   Hzpar = Hypar + (Hnpar/3) ;

   /*-- create space for local warped source image values --*/

   Hwval = (float *)realloc(Hwval,sizeof(float)*Hnval) ;

   /*-- setup to do incremental 'correlation' on the local region --*/

   if( INCOR_check_meth_code(Hmatch_code) == 4 ){ /* set LPC bounds [25 Jun 2014] */
     Hmpar->ar[0] = (float)Hnx; Hmpar->ar[1] = (float)Hny; Hmpar->ar[2] = (float)Hnz;
     Hmpar->ar[3] = (float)Hibot ; Hmpar->ar[4] = (float)Hitop ;
     Hmpar->ar[5] = (float)Hjbot ; Hmpar->ar[6] = (float)Hjtop ;
     Hmpar->ar[7] = (float)Hkbot ; Hmpar->ar[8] = (float)Hktop ;
   }

   INCOR_destroy(Hincor) ;
   Hincor = INCOR_create( Hmatch_code , Hmpar ) ; /* struct for correlations */

   FREEIFNN(Haawt) ; FREEIFNN(Hbval) ;

   need_AH = Hpen_use ;
   if( Hpen_use ) Hpen_sum = 0.0 ;

#undef  RESTORE_WBFAR  /* macro to fix wbfar after we break it below */
#define RESTORE_WBFAR                           \
 do{ for( pp=0,kk=kbot ; kk <= ktop ; kk++ )    \
      for( jj=jbot ; jj <= jtop ; jj++ )        \
       for( ii=ibot ; ii <= itop ; ii++,pp++ )  \
        wbfar[ii+jj*Hnx+kk*Hnxy] = Haawt[pp] ;  \
 } while(0)

   if( Hnval < Hnxyz ){                               /* initialize correlation from   */
     float *wbfar=MRI_FLOAT_PTR(Hwtim) ;              /* non-changing part of Haasrcim */
     float *bar  =MRI_FLOAT_PTR(BASIM) ;

     Haawt = (float *)malloc(sizeof(float)*Hnval) ;
     Hbval = (float *)malloc(sizeof(float)*Hnval) ;
     for( pp=0,kk=kbot ; kk <= ktop ; kk++ ){      /* extract weights  */
       for( jj=jbot ; jj <= jtop ; jj++ ){         /* and base image   */
         for( ii=ibot ; ii <= itop ; ii++,pp++ ){  /* for patch region */
           qq        = ii + jj*Hnx + kk*Hnxy ;
           Haawt[pp] = wbfar[qq] ;  /* copy weight image vals */
           Hbval[pp] =   bar[qq] ;  /* copy base image vals */
           wbfar[qq] = 0.0f ;       /* 0 out temp weight (fixed by RESTORE_WBFAR) */
     }}}

     if( is_float_array_constant(Hnval,Hbval) ){ /* can't correlate with this */
       if( Hverb > 2 )
         ININFO_message(
           "     %7s patch %03d..%03d %03d..%03d %03d..%03d : skipping (base=const=%g)" ,
                         WARP_CODE_STRING(warp_code) ,
                         ibot,itop, jbot,jtop, kbot,ktop , Hbval[0] ) ;
       RESTORE_WBFAR ; Hskipped++ ; RETURN(0) ;
     }

     /* initialize the 'correlation' from the data that won't
        be changing (i.e., data from outside the local patch)
        -- note wbfar was set to 0 above for voxels INSIDE the local patch */

     if( !Hlocalstat )  /* Hlocalstat should be 0 */
     INCOR_addto( Hincor , Hnxyz ,
                  MRI_FLOAT_PTR(BASIM) , MRI_FLOAT_PTR(Haasrcim) , wbfar ) ;
     RESTORE_WBFAR ;  /* fix wbfar inside local patch */

     /* init penalty (Hpen_sum) from non-changing part of Haawarp, if needed */

     if( Hpen_use ){
       float *je , *se ;
       je = Haawarp->je ; se = Haawarp->se ;
       for( kk=kbot ; kk <= ktop ; kk++ )
        for( jj=jbot ; jj <= jtop ; jj++ )
         for( ii=ibot ; ii <= itop ; ii++ )
          je[ii+jj*Hnx+kk*Hnxy] = se[ii+jj*Hnx+kk*Hnxy] = 0.0f ;
       Hpen_sum = HPEN_addup(Hnxyz,je,se) ;
     }

   } /* end of special setup over a patch smaller than the universe */

   /*--- OK, let's do the optimization of warp parameters (finally!) ---*/

   parvec = (double *)malloc(sizeof(double)*Hnparmap) ; /* Hnparmap was */
   xbot   = (double *)malloc(sizeof(double)*Hnparmap) ; /* set in func */
   xtop   = (double *)malloc(sizeof(double)*Hnparmap) ; /* H?warp_setup_basis */
   for( ii=0 ; ii < Hnparmap ; ii++ ){
     parvec[ii] = 0.0 ;
     xbot[ii]   = -Hbasis_parmax ;
     xtop[ii]   =  Hbasis_parmax ;
   }

   if( 1 || Hnval > 6666 )                /* at present, this is always on, since */
     powell_set_mfac( 1.001f , 2.001f ) ; /* it gives slightly better results (IMHO) */
   else
     powell_set_mfac( 2.001f , 1.001f ) ; /* so this is always OFF */

   /***** HERE is the actual optimization! (the point of all this code) *****/

   /** set maximum number of iterations allowed in the NEWUOA code **/

   itmax = (Hduplo) ? 6*Hnparmap+29 : 8*Hnparmap+31 ;
#if 0
   if( WORKHARD(Hlev_now) || SUPERHARD(Hlev_now) ) itmax -= Hnparmap ;
#endif

   if( Hverb > 3 ){
/* INFO_message("itmax = %d",itmax) ; */
     powell_set_verbose(Hverb-3) ;
   }

   /******* do it babee!! ***********************************/

   iter = powell_newuoa_con( Hnparmap , parvec,xbot,xtop , 0 ,
                             prad,0.009*prad , itmax , IW3D_scalar_costfun ) ;

   /******* iter = number of iterations actually used *******/

   if( iter > 0 ) Hnpar_sum += Hnparmap ; /* number of parameters used so far */

   if( Hverb > 3 ) powell_set_verbose(0) ;

   /***** cleanup and exit phase ***/

   free(xtop) ; free(xbot) ;

   if( iter <= 0 ){  /* something bad happened */
     ININFO_message(
       "     %s patch %03d..%03d %03d..%03d %03d..%03d : skipping - powell_newuoa_con() failure code=%d" ,
                     WARP_CODE_STRING(warp_code) ,
                     ibot,itop, jbot,jtop, kbot,ktop , iter ) ;
     ININFO_message(
      "powell_newuoa_con( ndim=%d  x=%p  xbot=%p  xtop=%p  nrand=%d  rstart=%f  rend=%f  maxcall=%d  ufunc=%p",
      Hnparmap , (void *)parvec , (void *)xbot , (void *)xtop , 0 , prad , 0.009*prad , itmax ,
      (void *)IW3D_scalar_costfun ) ;
     free(parvec); Hskipped++ ; RETURN(0);
   }

   /* load optimized warped image and warp into their patches */

   need_AH = 1 ;                     /* force the computation of AHwarp this time */
   Hcost = IW3D_scalar_costfun( Hnparmap , parvec ) ; /* evaluate at final params */
   (void)IW3D_load_energy(AHwarp) ;                     /* for Hverb output below */

   /* AHwarp gets loaded into Haawarp and Hwval into Haasrcim;
      the patch local energy fields get loaded into Haawarp's energy fields */

   sar = MRI_FLOAT_PTR(Haasrcim) ;
   Axd = Haawarp->xd; Ayd = Haawarp->yd; Azd = Haawarp->zd; Aje = Haawarp->je; Ase = Haawarp->se;
   bxd = AHwarp->xd ; byd = AHwarp->yd ; bzd = AHwarp->zd ; bje = AHwarp->je ; bse = AHwarp->se ;

   jt= bje[0] ; st = bse[0] ;
   for( pp=0,kk=kbot ; kk <= ktop ; kk++ ){
     for( jj=jbot ; jj <= jtop ; jj++ ){
       for( ii=ibot ; ii <= itop ; ii++,pp++ ){
         qq = ii + jj*Hnx + kk*Hnxy ;
         sar[qq] = Hwval[pp];                    /* load into warped source image */
         Axd[qq] = bxd[pp]; Ayd[qq] = byd[pp]; Azd[qq] = bzd[pp]; /* into Haawarp */
         Aje[qq] = bje[pp]; Ase[qq] = bse[pp];         /* into penalty 'energies' */
         if( Aje[qq] > jt ) jt = Aje[qq];     /* largest 'j' penalty in the patch */
         if( Ase[qq] > st ) st = Ase[qq];     /* largest 's' penalty */
   }}}

   if( Hverb > 1 ){  /* detailed overview of what went down in this patch */
     char cbuf[32] ;
     if( Hxyzmatch_num > 0 ) sprintf(cbuf,"xyzmatch pen=%g",Hxyzmatch_cost) ;
     else                    strcpy(cbuf,"\0") ;
     ININFO_message(
       "     %7s patch %03d..%03d %03d..%03d %03d..%03d : cost=%.5f iter=%d : energy=%.3f:%.3f pen=%g pure=%g %s",
                      WARP_CODE_STRING(Hbasis_code) ,
                      ibot,itop, jbot,jtop, kbot,ktop , Hcost  , iter , jt,st , Hpenn , Hcostt , cbuf ) ;

   } else if( Hverb == 1 && (Hlev_now<=2 || lrand48()%(Hlev_now*Hlev_now*Hlev_now/9)==0) ){
     fprintf(stderr,".") ;  /* just a pacifier */
   }

   /* ZOMG -- let's vamoose the ranchette */

   free(parvec) ; Hdone++ ; RETURN(iter) ;
}

#endif /*(Q7)*/ /*############################################################*/

#if 1
/*============================================================================*/
/** (Q8) Functions that drive the warp searching process                     **/
/*============================================================================*/

static IndexWarp3D *WO_iwarp = NULL ;
static int         *WO_ilev  = 0 ;

/*----------------------------------------------------------------------------*/
/* Optimize the warp over a sequence of ever smaller patches;
     bim      = base image
    wbim      = weight for base image (can be NULL)
     sim      = source image (to be warped)
    meth_code = matching code for INCOR
   warp_flags = special cases (e.g., no x-displacements)
   Also, a vast number of H... global variables can be set to affect the way
     this function works.
   This function is called from IW3D_warp_s2bim(), which is the function
     called in turn from 3dQwarp.c -- the face to the world at large.
   Return value is the index warp as optimized.
*//*-------------------------------------------------------------------------*/

IndexWarp3D * IW3D_warpomatic( MRI_IMAGE *bim, MRI_IMAGE *wbim, MRI_IMAGE *sim,
                               int meth_code, int warp_flags                   )
{
   int lev,levs,leve , xwid,ywid,zwid , xdel,ydel,zdel , iter , hgzero=0 ;
   int ibot,itop,idon , jbot,jtop,jdon , kbot,ktop,kdon , dox,doy,doz , iii ;
   float flev , glev , Hcostold , Hcostmid=0.0f,Hcostend=0.0f,Hcostbeg=999.9f ;
   int imin,imax , jmin,jmax, kmin,kmax , ibbb,ittt , jbbb,jttt , kbbb,kttt ;
   int dkkk,djjj,diii , ngmin=0 , levdone=0 , pcon , do_qfinal=0 ;
   int qmode=MRI_CUBIC , qmode2=MRI_CUBIC , qmodeX , nlevr , nsup,isup ;
   IndexWarp3D *OutWarp ;  /* the return value */
   char warplab[64] ;      /* 02 Jan 2015 */

ENTRY("IW3D_warpomatic") ;

   Hfirsttime = 1 ;  /* for fun printouts on first pass */

   /* set up a lot of things (mostly in global H... variables) */

   IW3D_setup_for_improvement( bim, wbim, sim, WO_iwarp, meth_code, warp_flags ) ;

   /* compute range of indexes over which to warp */
   /* imin..imax jmin..jmax kmin..kmax = autobox = contains all nonzeros */

   MRI_autobbox( Hwtim , &imin,&imax , &jmin,&jmax , &kmin,&kmax ) ;

   /* do global warping first (lev=0) */

   /* we want patch from imin..imax (etc), BUT must extend it a little
      on each edge to allow for the warp going to 0 at the edges; thus, we
      add xwid voxels on each end (if possible) -- etc for the other directions */

   xwid = (imax-imin)/8       ; ywid = (jmax-jmin)/8       ; zwid = (kmax-kmin)/8       ;
   ibbb = MAX(0,imin-xwid)    ; jbbb = MAX(0,jmin-ywid)    ; kbbb = MAX(0,kmin-zwid)    ;
   ittt = MIN(Hnx-1,imax+xwid); jttt = MIN(Hny-1,jmax+ywid); kttt = MIN(Hnz-1,kmax+zwid);

   /* actual warp at lev=0 is over domain ibbb..ittt X jbbb..jttt X kbbb..kttt */

   diii = ittt-ibbb+1 ; djjj = jttt-jbbb+1 ; dkkk = kttt-kbbb+1 ; /* sizes of this patch */
   iter = MAX(diii,djjj) ; iter = MAX(iter,dkkk) ;       /* largest size of a patch edge */
   if( iter < NGMIN ){
     ERROR_message("Can't warpomatic such a small volume: %d x %d x %d",diii,djjj,dkkk) ;
     RETURN(NULL) ;
   }

   /* announce the birth of a new warping procedure */

   if( Hverb )
     INFO_message("AFNI warpomatic: %d x %d x %d volume ; autobbox = %d..%d %d..%d %d..%d [clock=%s]",
                  Hnx,Hny,Hnz, imin,imax,jmin,jmax,kmin,kmax,
                  nice_time_string(NI_clock_time())          ) ;

   /* do the top level (global warps) */

   pcon = powell_newuoa_get_con() ;  /* 30 Oct 2015 */

   if( Hverb > 1 && Hworkhard2 >= Hworkhard1 )
     INFO_message("WORKHARD from %d to %d",Hworkhard1,Hworkhard2) ;
   if( Hverb > 1 && Hsuperhard2 >= Hsuperhard1 )
     INFO_message("SUPERHARD from %d to %d",Hsuperhard1,Hsuperhard2) ;

   if( Hlev_start == 0 || HGRID(0) == 0 ){
     /* nlevr = number of times to try the global quintic patch */
     /* [reduced by 1 on 24 May 2016, since repetition had little effect] */
#ifdef ALLOW_BASIS5
     nlevr = 1 ;
#else
     nlevr = ( WORKHARD(0) || SUPERHARD(0) || Hduplo ) ? 2 : 1 ;
#endif
     /* force the warp to happen, but don't use any penalty */
     Hforce = 1 ; Hfactor = Hfactor_q ; Hpen_use = 0 ; Hlev_now = 0 ;
     PBLUR_BASE  (ibbb,ittt,jbbb,jttt,kbbb,kttt) ;  /* progressive blur, if ordered */
     PBLUR_SOURCE(ibbb,ittt,jbbb,jttt,kbbb,kttt) ;
     mri_free(Haasrcim) ;                /* At this point, create the warped  */
     if( IW3D_is_zero(Haawarp) )         /* source image Haasrcim, which will */
       Haasrcim = mri_to_float(SRCIM) ;  /* be updated in IW3D_improve_warp() */
     else
       Haasrcim = IW3D_warp_floatim( Haawarp, SRCIM, Himeth , 1.0f ) ;
     if( Hverb == 1 ) fprintf(stderr,"lev=0 %d..%d %d..%d %d..%d: ",ibbb,ittt,jbbb,jttt,kbbb,kttt) ;
     /* always start with 2 cubic steps */
     BOXOPT ;
     (void)IW3D_improve_warp( MRI_CUBIC  , ibbb,ittt,jbbb,jttt,kbbb,kttt );
     if( Hquitting ) goto DoneDoneDone ;  /* signal to quit was sent */
#if 0
     BALLOPT ;
     (void)IW3D_improve_warp( MRI_CUBIC  , ibbb,ittt,jbbb,jttt,kbbb,kttt );
     if( Hquitting ) goto DoneDoneDone ;  /* signal to quit was sent */
#endif
     if( SUPERHARD(0) )
       (void)IW3D_improve_warp( MRI_CUBIC  , ibbb,ittt,jbbb,jttt,kbbb,kttt );
     if( Hquitting ) goto DoneDoneDone ;  /* signal to quit was sent */
          if( Hznoq  ) nlevr = 0 ;
     else if( Hzeasy ) nlevr = 1 ;
     for( iii=0 ; iii < nlevr ; iii++ ){  /* and some quintic steps */
       Hcostold = Hcost ;
       if( iii%2 == 0 ) BOXOPT ;
       else             BALLOPT ;
       (void)IW3D_improve_warp( MRI_QUINTIC, ibbb,ittt,jbbb,jttt,kbbb,kttt );
       if( Hquitting ) goto DoneDoneDone ;  /* signal to quit was sent */
       if( iii < nlevr-1 && Hcostold-Hcost < 0.00444f ){
         if( Hverb > 1 )
           ININFO_message("       --> too little improvement: breaking out of lev=0 iterates") ;
         break ;
       }
     }
#ifdef ALLOW_BASIS5
     if( (!Hznoq && !Hzeasy) && (H4zero || WORKHARD(0) || SUPERHARD(0)) ){
       BALLOPT ;
       (void)IW3D_improve_warp( MRI_CUBIC_PLUS_2, ibbb,ittt,jbbb,jttt,kbbb,kttt );
     }
     if( Hquitting ) goto DoneDoneDone ;  /* signal to quit was sent */
#endif
     if( Hsave_allwarps ){           /* 02 Jan 2015 */
       sprintf(warplab,"Lev0.%04dx%04dx%04d",ittt-ibbb+1,jttt-jbbb+1,kttt-kbbb+1) ;
       HSAVE_ADDTO(Haawarp,warplab) ;
     }
     if( Hverb == 1 ) fprintf(stderr," done [cost:%.5f==>%.5f]\n",Hfirstcost,Hcost) ;
   } else {
     Hcost = 666.666f ;  /* a beastly thing to do [no lev=0 optimization] */
   }

   powell_newuoa_set_con(pcon) ;

   /* for further steps, don't force things, and use the penalty */

   Hforce = 0 ; Hlev_final = 0 ;
   Hcostmid = Hcostend = Hcostbeg = Hcost ;

   if( !HAVE_HGRID ){
     if( Hngmin > 0 ){  /* is min patch size set from user? */
       ngmin = Hngmin ;
       if( Hduplo ){ ngmin = ngmin/2 + 1 ; if( ngmin < 11 ) ngmin = 11 ; }
     }

          if( ngmin   <  NGMIN ) ngmin = NGMIN ; /* can't go below this! */
     else if( ngmin%2 == 0     ) ngmin-- ;       /* must be odd */
   } else {
     ngmin = NGMIN ;   /* 31 Dec 2014 */
   }

   /** this should not happen, but if it does, then we're too small **/

   if( ngmin >= Hnx && ngmin >= Hny && ngmin >= Hnz ) goto DoneDoneDone ;

   /* inter-level shrinkage factor (used to be set-able by user) */

   if( Hshrink > 1.0f                       ) Hshrink = 1.0f / Hshrink ;
   if( Hshrink < 0.444f || Hshrink > 0.888f ) Hshrink = 0.749999f ;

   /*------ Now, iterate down to finer and finer patches ------*/

   levs = MAX(1,Hlev_start) ;  /* start level */
   leve = Hlev_end ;
   if( HAVE_HGRID && Hgridlist_num-1 < leve ) leve = Hgridlist_num-1 ;

   if( HAVE_HGRID ){
     xwid = (Hnx+1)*0.75f ; if( xwid%2 == 0 ) xwid++ ;
     ywid = (Hny+1)*0.75f ; if( ywid%2 == 0 ) ywid++ ;
     zwid = (Hnz+1)*0.75f ; if( zwid%2 == 0 ) zwid++ ;
     if( xwid > ngmin                  ) hgzero = xwid ;
     if( ywid > ngmin && ywid < hgzero ) hgzero = ywid ;
     if( zwid > ngmin && zwid < hgzero ) hgzero = zwid ;
     if( hgzero == 0 ) hgzero = ngmin ; /* should not happen */
   }

   for( lev=levs ; lev <= leve && !levdone ; lev++ ){

     /* set penalty factor for this level */

     flev = (Hpen_old) ? 1.0f : powf( (float)(lev-levs+1) , 0.333f ) ; ;
     Hpen_fff = Hpen_fac * MIN(2.22f,flev) ;  /* 20 Sep 2013 */

     Hpen_use = (Hpen_fff > 0.0f) && (lev > 2) ;

     /* compute width of rectangles at this level */

     if( ! HAVE_HGRID ){  /* the olden way */
       flev = powf(Hshrink,(float)lev) ;                 /* shrinkage fraction */
       xwid = (Hnx+1)*flev ; if( xwid%2 == 0 ) xwid++ ;  /* patch sizes must be odd */
       ywid = (Hny+1)*flev ; if( ywid%2 == 0 ) ywid++ ;
       zwid = (Hnz+1)*flev ; if( zwid%2 == 0 ) zwid++ ;
     } else {             /* the new-fangled way [31 Dec 2014] */
       xwid = ywid = zwid = HGRID(lev) ;
       if( xwid == 0 ) xwid = ywid = zwid = hgzero ;
       if( xwid < NGMIN ) goto DoneDoneDone ;  /* past the end of the Hgridlist array? */
     }

     /* decide if we are doing things in x, y, and/or z */

     dox = (xwid >= ngmin) && !(Hgflags & NWARP_NOXDEP_FLAG) ;
     doy = (ywid >= ngmin) && !(Hgflags & NWARP_NOYDEP_FLAG) ;
     doz = (zwid >= ngmin) && !(Hgflags & NWARP_NOZDEP_FLAG) ;

     if( !dox && !doy && !doz ){  /* exit immediately if nothing to do (shrank too far) */
       if( Hverb > 1 )
         ININFO_message("  ---------  lev=%d xwid=%d ywid=%d zwid=%d -- BREAK",lev,xwid,ywid,zwid) ;
       break ;
     }

     /* here, we are doing something, so don't let any width go below threshold */

     Hlev_now = Hlev_final = lev ;  /* in case we leave this loop somewhere below */

     if( xwid < ngmin ) xwid = MIN(Hnx,ngmin);
     if( ywid < ngmin ) ywid = MIN(Hny,ngmin);
     if( zwid < ngmin ) zwid = MIN(Hnz,ngmin);

     /* if we are almost to the smallest allowed patch, jump down to that size now */

     if( !HAVE_HGRID ){  /* only if user didn't specify the grid schedule */
       flev = xwid / (float)ngmin ;                                  /* flev is the */
       glev = ywid / (float)ngmin ; if( flev < glev ) flev = glev ;  /* largest ratio */
       glev = zwid / (float)ngmin ; if( flev < glev ) flev = glev ;  /* of ?wid to ngmin */
       if( flev > 1.0f && flev*Hshrink <= 1.00001f ){
         if( xwid > ngmin ) xwid = ngmin ;
         if( ywid > ngmin ) ywid = ngmin ;
         if( zwid > ngmin ) zwid = ngmin ;
         levdone = 1 ;   /* signal to exit when loop finishes */
       } else {
         iter = MAX(xwid,ywid) ; iter = MAX(iter,zwid) ; levdone = (iter == ngmin) ;
       }
       Hfinal = (levdone && !Hduplo) ;  /* is this the final level? */
     } else {
       levdone = Hfinal = (lev == Hgridlist_num-1) ;
     }

     /* step sizes for shifting the patches (half widths) */

     xdel = (xwid-1)/2 ; if( xdel == 0 ) xdel = 1 ;
     ydel = (ywid-1)/2 ; if( ydel == 0 ) ydel = 1 ;
     zdel = (zwid-1)/2 ; if( zdel == 0 ) zdel = 1 ;

     diii = xdel ; djjj = ydel ; dkkk = zdel ;

     /* bbbottom and tttop indexes to warp over */

     ibbb = imin-xdel/2-1 ; if( ibbb <  0   ) ibbb = 0 ;
     jbbb = jmin-ydel/2-1 ; if( jbbb <  0   ) jbbb = 0 ;
     kbbb = kmin-zdel/2-1 ; if( kbbb <  0   ) kbbb = 0 ;
     ittt = imax+xdel/2+1 ; if( ittt >= Hnx ) ittt = Hnx-1 ;
     jttt = jmax+ydel/2+1 ; if( jttt >= Hny ) jttt = Hny-1 ;
     kttt = kmax+zdel/2+1 ; if( kttt >= Hnz ) kttt = Hnz-1 ;

#if 0
#define HHH 0.333f
#define BBB 0.888f
     Hfactor = (1.0f-HHH) + HHH*powf(BBB,(float)(lev-1)) ;  /* max displacement allowed */
#else
     Hfactor = Hfactor_q ;  /* always allow full-size patch warps */
#endif

     qmode = qmode2 = MRI_CUBIC ;  /* cubic patches from here on down */
#ifdef ALLOW_QMODE                 /* or just maybe we'll do quintic? */
     do_qfinal = (Hfinal && Hqfinal) ;
     if( do_qfinal || Hqonly )   { qmode = qmode2 = MRI_QUINTIC ; }
     else if( Hqhard && !Hduplo ){ qmode2 = MRI_QUINTIC ; }
     if( xwid < NGMIN_Q || ywid < NGMIN_Q || zwid < NGMIN_Q )  /* 28 Oct 2015 */
       qmode = qmode2 = MRI_CUBIC ;
#endif
#ifdef ALLOW_BASIS5
     if( (Hfinal && H5final) && xwid*ywid*zwid < NVOXMAX_PLUS ){
       if(        xwid*ywid*zwid >= NGMIN_PLUS_3*NGMIN_PLUS_3*NGMIN_PLUS_3 && H5final >= 3 ){
         qmode = qmode2 = MRI_CUBIC_PLUS_3 ;
       } else if( xwid*ywid*zwid >= NGMIN_PLUS_2*NGMIN_PLUS_2*NGMIN_PLUS_2 && H5final >= 2 ){
         qmode = qmode2 = MRI_CUBIC_PLUS_2 ;
       } else if( xwid*ywid*zwid >= NGMIN_PLUS_1*NGMIN_PLUS_1*NGMIN_PLUS_1 && H5final >= 1 ){
         qmode = qmode2 = MRI_CUBIC_PLUS_1 ;
       }
     }
#endif

     (void)IW3D_load_energy(Haawarp) ;  /* initialize energy field for penalty use */

     nlevr = WORKHARD(lev)  ? 2 : 1 ; /* number of iterations over this level */
     nsup  = SUPERHARD(lev) ? 2 : 1 ; /* number of iterations in each sweep */

     /* announce the start of a new level! */

     PBLUR_BASE  (1,xwid,1,ywid,1,zwid) ;  /* progressive blur, if ordered */
     PBLUR_SOURCE(1,xwid,1,ywid,1,zwid) ;
     if( Hpblur_b > 0.0f || Hpblur_b > 0.0f ) Hfirsttime = 1 ;
     mri_free(Haasrcim) ;  /* re-create the warped source image Haasrcim */
     Haasrcim = IW3D_warp_floatim( Haawarp, SRCIM, Himeth , 1.0f ) ;

     if( Hverb > 1 )
       ININFO_message("  .........  lev=%d xwid=%d ywid=%d zwid=%d Hfac=%g penfac=%g %s %s [clock=%s]" ,
                      lev,xwid,ywid,zwid,Hfactor,Hpen_fff , (levdone   ? "FINAL"  : "\0") ,
                                                   (nlevr > 1 ? "WORKHARD" : "\0") ,
                      nice_time_string(NI_clock_time()) ) ;
     else if( Hverb == 1 )
       fprintf(stderr,"lev=%d patch=%dx%dx%d [clock=%s]",lev,xwid,ywid,zwid,nice_time_string(NI_clock_time()) ) ;

     Hdone = Hskipped = 0 ;

     /* alternate the direction of sweeping at different levels;
        the purpose of this is so that one side of the box doesn't get favored */

     if( lev%2 == 1 || nlevr > 1 ){  /* sweep from bot to top, ijk order */
           if( do_qfinal ) BALLOPT ;
      else if( nlevr > 1 ) BOXOPT ;
      for( isup=0 ; isup < nsup ; isup++ ){  /* working superhard? do this twice! */
       for( kdon=0,kbot=kbbb ; !kdon ; kbot += dkkk ){  /* loop over z direction of patches */
         ktop = kbot+zwid-1;  /* top edge of patch: maybe edit it down or up */
              if( ktop >= kttt )       { ktop = kttt; kbot = ktop+1-zwid; kdon=1; }
         else if( ktop >= kttt-zwid/4 ){ ktop = kttt; kdon=1; }  /* extend patch to end */
         for( jdon=0,jbot=jbbb ; !jdon ; jbot += djjj ){ /* y direction loop */
           jtop = jbot+ywid-1;
                if( jtop >= jttt        ){ jtop = jttt; jbot = jtop+1-ywid; jdon=1; }
           else if( jtop >= jttt-ywid/4 ){ jtop = jttt; jdon=1; }
           for( idon=0,ibot=ibbb ; !idon ; ibot += diii ){ /* x direction loop */
             itop = ibot+xwid-1;
                  if( itop >= ittt        ){ itop = ittt; ibot = itop+1-xwid; idon=1; }
             else if( itop >= ittt-xwid/4 ){ itop = ittt; idon=1; }
             Hcostold = Hcost ;  /* the last cost we saw */
             /*** actually try to make things better ***/
             iter = IW3D_improve_warp( qmode  , ibot,itop , jbot,jtop , kbot,ktop ) ;
             if( Hquitting ) goto DoneDoneDone ;  /* signal to quit was sent */
#if 0  /* this stuff was an attempt to iterate again under some circumstances */
             if( Hcost > Hcostold+0.001f ){
               if( Hverb > 1 ) ININFO_message(" -- rerun --") ;
               iter = IW3D_improve_warp( MRI_CUBIC  , ibot,itop , jbot,jtop , kbot,ktop ) ;
             }
#if 0
             else if( iter > 144 && qmode > 0 && Hcostold-Hcost > 0.00002f )
               iter = IW3D_improve_warp( qmode    , ibot,itop , jbot,jtop , kbot,ktop ) ;
#endif
#endif
             if( Hcost < Hstopcost ){                  /* whoa? */
               if( Hverb == 1 ) fprintf(stderr,"\n") ;
               ININFO_message("  ######### cost has reached stopping value") ;
               goto DoneDoneDone ;
             }
           } /* end of sweep over i-direction (x) patches */
         } /* end of sweep over j-direction (y) patches */
       } /* end of sweep over k-direction (z) patches */
      } /* isup loop (1 or 2) */
      Hcostmid = Hcostend = Hcost ;
     } /* end of bot to top, ijk order */

     /* reverse direction */

     if( lev%2 == 0 || nlevr > 1 ){ /* sweep from top to bot, kji order */
       if( nlevr > 1 && Hverb == 1 ) fprintf(stderr,":[cost=%.5f]:",Hcost) ;
       if( do_qfinal || nlevr > 1 ) BALLOPT ;
       qmodeX = (nlevr > 1) ? qmode2 : qmode ;
      for( isup=0 ; isup < nsup ; isup++ ){  /* superhard? */
       for( idon=0,itop=ittt ; !idon ; itop -= diii ){
         ibot = itop+1-xwid;
              if( ibot <= ibbb        ){ ibot = ibbb; itop = ibot+xwid-1; idon=1; }
         else if( ibot <= ibbb+xwid/4 ){ ibot = ibbb; idon=1; }
         for( jdon=0,jtop=jttt ; !jdon ; jtop -= djjj ){
           jbot = jtop+1-ywid;
                if( jbot <= jbbb        ){ jbot = jbbb; jtop = jbot+ywid-1; jdon=1; }
           else if( jbot <= jbbb+ywid/4 ){ jbot = jbbb; jdon=1; }
           for( kdon=0,ktop=kttt ; !kdon ; ktop -= dkkk ){
             kbot = ktop+1-zwid;
                  if( kbot <= kbbb        ){ kbot = kbbb; ktop = kbot+zwid-1; kdon=1; }
             else if( kbot <= kbbb+zwid/4 ){ kbot = kbbb; kdon=1; }
             Hcostold = Hcost ;
             iter = IW3D_improve_warp( qmodeX  , ibot,itop , jbot,jtop , kbot,ktop ) ;
             if( Hquitting ) goto DoneDoneDone ;  /* signal to quit was sent */
#if 0
             if( Hcost > Hcostold+0.001f ){
               if( Hverb > 1 ) ININFO_message(" -- rerun --") ;
               iter = IW3D_improve_warp( MRI_CUBIC  , ibot,itop , jbot,jtop , kbot,ktop ) ;
             }
#if 0
             else if( iter > 144 && qmodeX > 0 && Hcostold-Hcost > 0.00002f )
               iter = IW3D_improve_warp( qmodeX    , ibot,itop , jbot,jtop , kbot,ktop ) ;
#endif
#endif
              if( Hcost < Hstopcost ){
                if( Hverb == 1 ) fprintf(stderr,"\n") ;
                ININFO_message("  ######### cost has reached stopping value") ;
                goto DoneDoneDone ;
              }
           }
         }
       }
      } /* isup loop */
      Hcostend = Hcost ;
     } /* end of top to bot, kji order */

     /* at this point, we have just about finished with this level's patches */

     if( Hdone == 0 ){  /* if nothing was done at this level, try -something- */
       ibot = (imin+imax-xwid)/2 ; if( ibot < 0 ) ibot = 0 ;   /* centered on */
       jbot = (jmin+jmax-ywid)/2 ; if( jbot < 0 ) jbot = 0 ;   /* the autobox */
       kbot = (kmin+kmax-zwid)/2 ; if( kbot < 0 ) kbot = 0 ;
       itop = ibot+xwid-1        ; if( itop >= Hnx ) itop = Hnx-1 ;
       jtop = jbot+ywid-1        ; if( jtop >= Hny ) jtop = Hny-1 ;
       ktop = kbot+zwid-1        ; if( ktop >= Hnz ) ktop = Hnz-1 ;
       Hforce = 1 ;
       iter = IW3D_improve_warp( qmode , ibot,itop , jbot,jtop , kbot,ktop ) ;
       if( Hquitting ) goto DoneDoneDone ;  /* signal to quit was sent */
       Hforce = 0 ;
       Hcostend = Hcost ;
     }

     /* print some summary of what happened at this level */

     if( Hsave_allwarps ){           /* 02 Jan 2015 */
       sprintf(warplab,"Lev%d.%04dx%04dx%04d",lev,xwid,ywid,zwid) ;
       HSAVE_ADDTO(Haawarp,warplab) ;
     }

     if( Hcostbeg > 666.0f ) Hcostbeg = Hfirstcost ;
     if( Hverb == 1 ){
       if( Hdone > 0 )
         fprintf(stderr," done [cost:%.5f==>%.5f ; %d patches optimized, %d skipped]\n",Hcostbeg,Hcost,Hdone,Hskipped) ;
       else
         fprintf(stderr," done [cost:%.5f ; all patches skipped]\n",Hcost) ;
     }
     Hcostbeg = Hcost ;

   } /*-- end of loop over levels of refinement --*/

DoneDoneDone:  /* breakout */

   OutWarp = IW3D_copy( Haawarp , 1.0f ) ;
   IW3D_cleanup_improvement() ;

   RETURN(OutWarp) ;
}

/*----------------------------------------------------------------------------*/
/* This is the function called by 3dQwarp to warp source to base (s2bim)!
   It returns the index warp AND the warped source image in a gift wrapping.
*//*--------------------------------------------------------------------------*/

static void *S2BIM_iwarp = NULL ;  /* initial warp? */
static int   S2BIM_ilev  = 0 ;     /* initial level */
static int   S2BIM_mlev  = 666 ;    /* final level */

Image_plus_Warp * IW3D_warp_s2bim( MRI_IMAGE *bim , MRI_IMAGE *wbim , MRI_IMAGE *sim,
                                   int interp_code , int meth_code , int warp_flags  )
{
   IndexWarp3D *Swarp ;
   MRI_IMAGE *outim ;
   Image_plus_Warp *imww ;

ENTRY("IW3D_warp_s2bim") ;

   WO_iwarp = S2BIM_iwarp ; Hlev_start = S2BIM_ilev ; Hlev_end = S2BIM_mlev ;
   Hnpar_sum = 0 ; Hduplo = 0 ;

   /* the user can set the Hshrink factor (default=0.749999) but why bother? */

   Hshrink = AFNI_numenv("AFNI_WARPOMATIC_SHRINK") ;
   if( Hshrink > 1.0f                       ) Hshrink = 1.0f / Hshrink ;
   if( Hshrink < 0.444f || Hshrink > 0.888f ) Hshrink = 0.749999f ;
   else                                       ININFO_message("  -- Hshrink set to %.6f",Hshrink) ;

   /*----- virtually all the CPU time is spent in this next puppy -----*/

   Swarp = IW3D_warpomatic( bim , wbim , sim , meth_code , warp_flags ) ;

   /*-- compute the warped source image --*/

   outim = IW3D_warp_floatim( Swarp, sim , interp_code , 1.0f ) ;

   /*-- wrap them up in a nice Hanukkah package and send them home --*/

   imww       = (Image_plus_Warp *)malloc(sizeof(Image_plus_Warp)) ;
   imww->im   = outim ;
   imww->warp = Swarp ;

   RETURN(imww) ;
}

#if 0
/*----------------------------------------------------------------------------*/
/*** THIS CODE IS NOT READY YET (maybe never) ***/

#define WOMA_NONE  0
#define WOMA_MAT44 1
#define WOMA_DSET  2

THD_3dim_dataset * THD_warpomatic( THD_3dim_dataset *bset ,
                                   THD_3dim_dataset *sset ,
                                   int inicode , void *iniwarp ,
                                   byte *bmask , byte *emask    )
{
   int nx,ny,nz ;
   IndexWarp3D *iniwww=NULL , *outwww=NULL ;
   MRI_IMAGE *bim , *sim , *wbim ;
   THD_3dim_dataset *outdset=NULL ;

ENTRY("THD_warpomatic") ;

   if( !ISVALID_DSET(bset) || !ISVALID_DSET(sset) || EQUIV_DSETS(bset,sset) ){
     ERROR_message("bad input datasets to THD_warpomatic") ;
     RETURN(NULL) ;
   }

   nx = DSET_NX(bset) ; ny = DSET_NY(bset) ; nz = DSET_NZ(bset) ;

   if( (nz == 1 && DSET_NZ(sset) >  1) ||
       (nz >  1 && DSET_NZ(sset) == 1)   ){
     ERROR_message("exactly 1 input dataset to THD_warpomatic is 2D") ;
     RETURN(NULL) ;
   }

   switch( inicode ){
     default:
       ERROR_message("bad inicode in THD_warpomatic: %d",inicode) ;
       RETURN(NULL) ;
     break ;

     case WOMA_NONE:
        iniwww = IW3D_create(nx,ny,nz) ;    /* identity warp */
        IW3D_adopt_dataset(iniwww,bset) ;
     break ;

     case WOMA_DSET:{
       THD_3dim_dataset *iniset = (THD_3dim_dataset *)iniwarp ;
       if( !ISVALID_DSET(iniset) || DSET_NVALS(iniset) < 3 ){
         ERROR_message("bad iniwarp dataset in THD_warpomatic") ; RETURN(NULL) ;
       }
       if( !EQUIV_GRIDS(bset,iniset) ){
         ERROR_message("bad iniwarp dataset grid in THD_warpomatic") ; RETURN(NULL) ;
       }
       iniwww = IW3D_from_dataset( iniset , 0 , 0 ) ;
       DSET_unload(iniset) ;
       if( iniwww == NULL ){
         ERROR_message("Can't use iniwarp dataset in THD_warpomatic") ; RETURN(NULL) ;
       }
     }
     break ;

     case WOMA_MAT44:{
       mat44 *inimat = (mat44 *)iniwarp ;
       iniwww = IW3D_from_mat44( *inimat , bset ) ;
       if( iniwww == NULL ){
         ERROR_message("Can't use iniwarp mat44 in THD_warpomatic") ; RETURN(NULL) ;
       }
     }
     break ;
   }

   WO_iwarp = iniwww ;

   DSET_load(bset) ;
   if( !DSET_LOADED(bset) ){
     IW3D_destroy(iniwww) ; WO_iwarp = NULL ;
     ERROR_message("Can't load base dataset in THD_warpomatic") ; RETURN(NULL) ;
   }
   bim = THD_extract_float_brick(0,bset) ; DSET_unload(bset) ;

   DSET_load(sset) ;
   if( !DSET_LOADED(sset) ){
     IW3D_destroy(iniwww) ; WO_iwarp = NULL ; mri_free(bim) ;
     ERROR_message("Can't load source dataset in THD_warpomatic") ; RETURN(NULL) ;
   }
   if( EQUIV_GRIDS(bset,sset) ){
     sim = THD_extract_float_brick(0,sset) ;
   } else {
     THD_3dim_dataset *wset , *oset ;
     wset = IW3D_to_dataset( iniwww, "Qadqop" ) ;
     oset = THD_nwarp_dataset( wset, sset, bset, "Mercotan", MRI_LINEAR,Himeth, 0.0f, 1.0f, 1 , NULL ) ;
     DSET_delete(wset) ;
     sim = mri_copy(DSET_BRICK(oset,0)) ;
     DSET_delete(oset) ;
   }

   RETURN(outdset) ;
}
#endif

#endif /*(Q8)*/ /*############################################################*/

#ifdef ALLOW_DUPLO
/*============================================================================*/
/** (Q9) Functions for duplo-ing a warp or image (up and down in size)       **/
/*============================================================================*/

/*---------------------------------------------------------------------------*/
/* Make a half-size copy of a warp (scaling displacements by 0.5 as well). */

IndexWarp3D * IW3D_duplo_down( IndexWarp3D *AA )
{
   IndexWarp3D *BB ;
   int nxa,nya,nza , nxb,nyb,nzb , nxya,nxyb , ii,jj,kk ;
   float *xda, *yda, *zda , *xdb, *ydb, *zdb ;

   nxa = AA->nx ; nya = AA->ny ; nza = AA->nz  ;

   nxb = nxa / 2 ; if( nxb < 1 ) nxb = 1 ;
   nyb = nya / 2 ; if( nyb < 1 ) nyb = 1 ;
   nzb = nza / 2 ; if( nzb < 1 ) nzb = 1 ;

   BB = IW3D_create(nxb,nyb,nzb) ; if( BB == NULL ) return NULL ;

   xda = AA->xd ; yda = AA->yd ; zda = AA->zd ; nxya = nxa*nya ;
   xdb = BB->xd ; ydb = BB->yd ; zdb = BB->zd ; nxyb = nxb*nyb ;

   /* index displacements are scaled by 0.5 since each change
      of index by 1 now corresponds to 2 voxels in the old grid */

   for( kk=0 ; kk < nzb ; kk++ ){
    for( jj=0 ; jj < nyb ; jj++ ){
      for( ii=0 ; ii < nxb ; ii++ ){
        FSUB(xdb,ii,jj,kk,nxb,nxyb) = 0.5f*FSUB(xda,2*ii,2*jj,2*kk,nxa,nxya) ;
        FSUB(ydb,ii,jj,kk,nxb,nxyb) = 0.5f*FSUB(yda,2*ii,2*jj,2*kk,nxa,nxya) ;
        FSUB(zdb,ii,jj,kk,nxb,nxyb) = 0.5f*FSUB(zda,2*ii,2*jj,2*kk,nxa,nxya) ;
   }}}
   IW3D_load_external_slopes(BB) ;

   return BB ;
}

/*---------------------------------------------------------------------------*/
/* Make a double-size copy of a warp (scaling displacements by 2.0 as well). */

IndexWarp3D * IW3D_duplo_up( IndexWarp3D *AA , int xadd,int yadd,int zadd)
{
   IndexWarp3D *BB ;
   int nxa,nya,nza , nxb,nyb,nzb , nxya,nxyb , ii,jj,kk , im,jm,km,ip,jp,kp ;
   float *xda, *yda, *zda , *xdb, *ydb, *zdb ;

ENTRY("IW3D_duplo_up") ;

   nxa = AA->nx ; nya = AA->ny ; nza = AA->nz  ;

   nxb = (nxa == 1) ? 1 : (2*nxa+(xadd != 0)) ;
   nyb = (nya == 1) ? 1 : (2*nya+(yadd != 0)) ;
   nzb = (nza == 1) ? 1 : (2*nza+(zadd != 0)) ;

   BB = IW3D_create(nxb,nyb,nzb) ; if( BB == NULL ) RETURN(NULL) ;

   xda = AA->xd ; yda = AA->yd ; zda = AA->zd ; nxya = nxa*nya ;
   xdb = BB->xd ; ydb = BB->yd ; zdb = BB->zd ; nxyb = nxb*nyb ;

   /* in the following:
        note that linear interpolation would need a scale factor of 0.125
        (from 8 points), then the doubling is the factor of 0.250 = 0.125 * 2  */

   for( kk=0 ; kk < nzb ; kk++ ){
    kp = km = kk/2 ; if( kk%2 ) kp++;
    if( kp >= nza ) kp = nza-1; if( km >= nza ) km = nza-1;
    for( jj=0 ; jj < nyb ; jj++ ){
      jp = jm = jj/2 ; if( jj%2 ) jp++;
      if( jp >= nya ) jp = nya-1; if( jm >= nya ) jm = nya-1;
      for( ii=0 ; ii < nxb ; ii++ ){
        ip = im = ii/2 ; if( ii%2 ) ip++;
        if( ip >= nxa ) ip = nxa-1; if( im >= nxa ) im = nxa-1;
        FSUB(xdb,ii,jj,kk,nxb,nxyb) =
          0.250f * ( FSUB(xda,im,jm,km,nxa,nxya) + FSUB(xda,ip,jm,km,nxa,nxya)
                    +FSUB(xda,im,jp,km,nxa,nxya) + FSUB(xda,ip,jp,km,nxa,nxya)
                    +FSUB(xda,im,jm,kp,nxa,nxya) + FSUB(xda,ip,jm,kp,nxa,nxya)
                    +FSUB(xda,im,jp,kp,nxa,nxya) + FSUB(xda,ip,jp,kp,nxa,nxya) ) ;
        FSUB(ydb,ii,jj,kk,nxb,nxyb) =
          0.250f * ( FSUB(yda,im,jm,km,nxa,nxya) + FSUB(yda,ip,jm,km,nxa,nxya)
                    +FSUB(yda,im,jp,km,nxa,nxya) + FSUB(yda,ip,jp,km,nxa,nxya)
                    +FSUB(yda,im,jm,kp,nxa,nxya) + FSUB(yda,ip,jm,kp,nxa,nxya)
                    +FSUB(yda,im,jp,kp,nxa,nxya) + FSUB(yda,ip,jp,kp,nxa,nxya) ) ;
        FSUB(zdb,ii,jj,kk,nxb,nxyb) =
          0.250f * ( FSUB(zda,im,jm,km,nxa,nxya) + FSUB(zda,ip,jm,km,nxa,nxya)
                    +FSUB(zda,im,jp,km,nxa,nxya) + FSUB(zda,ip,jp,km,nxa,nxya)
                    +FSUB(zda,im,jm,kp,nxa,nxya) + FSUB(zda,ip,jm,kp,nxa,nxya)
                    +FSUB(zda,im,jp,kp,nxa,nxya) + FSUB(zda,ip,jp,kp,nxa,nxya) ) ;
   }}}
   IW3D_load_external_slopes(BB) ;

   RETURN(BB) ;
}

/*----------------------------------------------------------------------------*/
/* blur an image in place (applied to images being warped by duplo) */

static void blur_inplace( MRI_IMAGE *fim , float fwhm )
{
   float sig = FWHM_TO_SIGMA(fwhm) ;
   FIR_blur_volume_3d( fim->nx,fim->ny,fim->nz , 1.0f,1.0f,1.0f ,
                       MRI_FLOAT_PTR(fim)  , sig,sig,sig         ) ;
   return ;
}

/*---------------------------------------------------------------------------*/
/* cut an image down by halfsies */

#undef  CALLME
#define CALLME(inn,out) (out) = mri_duplo_down_3D(inn)

MRI_IMAGE * mri_duplo_down_3D( MRI_IMAGE *fim )
{
   MRI_IMAGE *gim=NULL , *qim ;
   float *far , *gar ;
   int nxf,nyf,nzf , nxg,nyg,nzg , nxyf,nxyg , ii,jj,kk ;

   if( fim == NULL ) return NULL ;

   if( ISVECTIM(fim) ){ VECTORME(fim,gim) ; return gim ; }

   qim = mri_to_float(fim) ;       /* make a copy */
   blur_inplace( qim , 1.666f ) ;  /* blur it before sub-sampling */

   nxf = qim->nx ; nyf = qim->ny ; nzf = qim->nz ;
   nxg = nxf / 2 ; if( nxg < 1 ) nxg = 1 ;
   nyg = nyf / 2 ; if( nyg < 1 ) nyg = 1 ;
   nzg = nzf / 2 ; if( nzg < 1 ) nzg = 1 ;

   nxyf = nxf*nyf ; nxyg = nxg*nyg ;

   gim = mri_new_vol(nxg,nyg,nzg,MRI_float) ;
   gar = MRI_FLOAT_PTR(gim) ;
   far = MRI_FLOAT_PTR(qim) ;

   for( kk=0 ; kk < nzg ; kk++ ){
    for( jj=0 ; jj < nyg ; jj++ ){
      for( ii=0 ; ii < nxg ; ii++ ){
        FSUB(gar,ii,jj,kk,nxg,nxyg) = FSUB(far,2*ii,2*jj,2*kk,nxf,nxyf) ;
   }}}

   return gim ;
}

/*---------------------------------------------------------------------------*/
/* cut a binary mask image down by halfsies */

MRI_IMAGE * mri_duplo_down_3Dmask( MRI_IMAGE *fim )
{
   MRI_IMAGE *gim=NULL ;
   byte *far , *gar ;
   int nxf,nyf,nzf , nxg,nyg,nzg , nxyf,nxyg , ii,jj,kk ;
   int ip,jp,kp , id,jd,kd , im,jm,km ;
   byte f00 , fxp,fxm , fyp,fym , fzp,fzm , val=0 ;
   int  n00 , nxp,nxm , nyp,nym , nzp,nzm ;

   if( fim == NULL || fim->kind != MRI_byte ) return NULL ;

   nxf = fim->nx ; nyf = fim->ny ; nzf = fim->nz ;
   nxg = nxf / 2 ; if( nxg < 1 ) nxg = 1 ;
   nyg = nyf / 2 ; if( nyg < 1 ) nyg = 1 ;
   nzg = nzf / 2 ; if( nzg < 1 ) nzg = 1 ;

   nxyf = nxf*nyf ; nxyg = nxg*nyg ;

   gim = mri_new_vol(nxg,nyg,nzg,MRI_byte) ;
   gar = MRI_BYTE_PTR(gim) ;
   far = MRI_BYTE_PTR(fim) ;

   for( kk=0 ; kk < nzg ; kk++ ){
    kd = 2*kk ; kp = kd+1 ; if( kp >= nzf ) kp = nzf-1 ;
                km = kd-1 ; if( km <  0   ) km = 0 ;
    for( jj=0 ; jj < nyg ; jj++ ){
      jd = 2*jj ; jp = jd+1 ; if( jp >= nyf ) jp = nyf-1 ;
                  jm = jd-1 ; if( jm <  0   ) jm = 0 ;
      for( ii=0 ; ii < nxg ; ii++ ){
        id = 2*ii ; ip = id+1 ; if( ip >= nxf ) ip = nxf-1 ;
                    im = id-1 ; if( im <  0   ) im = 0 ;
        f00 = FSUB(far,id,jd,kd,nxf,nxyf);
        fxp = FSUB(far,ip,jd,kd,nxf,nxyf); fxm = FSUB(far,im,jd,kd,nxf,nxyf);
        fyp = FSUB(far,id,jp,kd,nxf,nxyf); fym = FSUB(far,id,jm,kd,nxf,nxyf);
        fzp = FSUB(far,id,jd,km,nxf,nxyf); fzm = FSUB(far,id,jd,km,nxf,nxyf);
        val = (f00 > 0) + (fxp > 0) + (fxm > 0)
                        + (fyp > 0) + (fym > 0) + (fzp > 0) + (fzm > 0) ;
        FSUB(gar,ii,jj,kk,nxg,nxyg) = (val > 2) ;
   }}}

   return gim ;
}

#if 0
/*---------------------------------------------------------------------------*/

#undef  CALLME
#define CALLME(inn,out) (out) = mri_duplo_down_3D_NN(inn)

MRI_IMAGE * mri_duplo_down_3D_NN( MRI_IMAGE *fim )
{
   MRI_IMAGE *gim=NULL ;
   float *far , *gar ;
   int nxf,nyf,nzf , nxg,nyg,nzg , nxyf,nxyg , ii,jj,kk ;
   int ip,jp,kp , id,jd,kd , im,jm,km ;
   float f00 , fxp,fxm , fyp,fym , fzp,fzm , val=0.0f ;
   int   n00 , nxp,nxm , nyp,nym , nzp,nzm ;

   if( fim == NULL ) return NULL ;

   if( ISVECTIM(fim) ){ VECTORME(fim,gim) ; return gim ; }

   nxf = fim->nx ; nyf = fim->ny ; nzf = fim->nz ;
   nxg = nxf / 2 ; if( nxg < 1 ) nxg = 1 ;
   nyg = nyf / 2 ; if( nyg < 1 ) nyg = 1 ;
   nzg = nzf / 2 ; if( nzg < 1 ) nzg = 1 ;

   nxyf = nxf*nyf ; nxyg = nxg*nyg ;

   gim = mri_new_vol(nxg,nyg,nzg,MRI_float) ;
   gar = MRI_FLOAT_PTR(gim) ;
   far = MRI_FLOAT_PTR(fim) ;

   for( kk=0 ; kk < nzg ; kk++ ){
    kd = 2*kk ; kp = kd+1 ; if( kp >= nzf ) kp = nzf-1 ;
                km = kd-1 ; if( km <  0   ) km = 0 ;
    for( jj=0 ; jj < nyg ; jj++ ){
      jd = 2*jj ; jp = jd+1 ; if( jp >= nyf ) jp = nyf-1 ;
                  jm = jd-1 ; if( jm <  0   ) jm = 0 ;
      for( ii=0 ; ii < nxg ; ii++ ){
        id = 2*ii ; ip = id+1 ; if( ip >= nxf ) ip = nxf-1 ;
                    im = id-1 ; if( im <  0   ) im = 0 ;
        f00 = FSUB(far,id,jd,kd,nxf,nxyf);
        fxp = FSUB(far,ip,jd,kd,nxf,nxyf); fxm = FSUB(far,im,jd,kd,nxf,nxyf);
        fyp = FSUB(far,id,jp,kd,nxf,nxyf); fym = FSUB(far,id,jm,kd,nxf,nxyf);
        fzp = FSUB(far,id,jd,km,nxf,nxyf); fzm = FSUB(far,id,jd,km,nxf,nxyf);
        n000 = 1 ;
        if( fxp == f00 ) n00++ ; if( fxm == f00 ) n00++ ;
        if( fyp == f00 ) n00++ ; if( fym == f00 ) n00++ ;
        if( fzp == f00 ) n00++ ; if( fzm == f00 ) n00++ ;
        if( n000 > 3 ){ val = f00 ; }
        else {
        }
        FSUB(gar,ii,jj,kk,nxg,nxyg) =
   }}}

   return gim ;
}
#endif

#endif /*(Q9)*/ /*############################################################*/

#ifdef ALLOW_DUPLO
/*============================================================================*/
/** (Q10) Function for warp optimization with duplo-ing                      **/
/*============================================================================*/

/*----------------------------------------------------------------------------*/
/* Similar to the earlier s2bim function, but for the 3dQwarp -duplo option */

Image_plus_Warp * IW3D_warp_s2bim_duplo( MRI_IMAGE *bim , MRI_IMAGE *wbim , MRI_IMAGE *sim,
                                         int interp_code , int meth_code , int warp_flags  )
{
   IndexWarp3D *Swarp , *Dwarp ;
   MRI_IMAGE *outim ;
   MRI_IMAGE *bimd , *wbimd , *simd ;
   int nx,ny,nz , Htemp1, Htemp2 ;
   Image_plus_Warp *imww ;
   byte *emask_big=NULL ; MRI_IMAGE *embim=NULL , *emsim=NULL ;

ENTRY("IW3D_warp_s2bim_duplo") ;

   if( Hverb ) INFO_message("=== Duplo down -- blurring volumes & down-sampling") ;

   WO_iwarp = NULL ;               /* can't start with initial warp for duplo */
   nx = bim->nx ; ny = bim->ny ; nz = bim->nz ;
   bimd  = mri_duplo_down_3D(bim) ;   blur_inplace( bimd , 1.234f ) ;
   wbimd = mri_duplo_down_3D(wbim) ;
   simd  = mri_duplo_down_3D(sim) ;   blur_inplace( simd , 1.234f ) ;

   Hshrink    = 0.749999f ;
   Hlev_start = 0 ; Hlev_end = 3 ;
   Hpen_fac  *= 4.0f ;
   Hduplo     = 1 ; Hnpar_sum = 0 ;

   if( Hemask != NULL ){
     embim = mri_new_vol_empty(nx,ny,nz,MRI_byte) ; emask_big = Hemask ;
     mri_fix_data_pointer(emask_big,embim) ;
     emsim = mri_duplo_down_3Dmask(embim) ;
     Hemask = MRI_BYTE_PTR(emsim) ;
     mri_clear_data_pointer(embim) ; mri_free(embim) ;
   }

   /* the duplo warping of smaller volumes */

   Dwarp = IW3D_warpomatic( bimd , wbimd , simd , meth_code , warp_flags ) ;

   Hpen_fac  /= 4.0f ;
   Hduplo     = 0 ;

   mri_free(simd) ; mri_free(wbimd) ; mri_free(bimd) ;

   if( Hemask != NULL ){ mri_free(emsim) ; Hemask = emask_big ; }

   if( Dwarp == NULL ) RETURN(NULL) ;

   if( Hverb )
     INFO_message("=== Duplo up [clock=%s] -- up-sampling warp",nice_time_string(NI_clock_time())) ;

   WO_iwarp = IW3D_duplo_up( Dwarp, nx%2 , ny%2 , nz%2 ) ;
   IW3D_destroy(Dwarp) ;

   Hshrink = 0.749999f ; Hlev_start = Hlev_final /*-1*/ ; if( Hlev_start < 0 ) Hlev_start = 0 ;
   Htemp1 = Hworkhard1 ; Hworkhard1 = 0 ;
   Htemp2 = Hworkhard2 ; Hworkhard2 = MAX(Htemp2,Hlev_start) ;
   Hlev_end = S2BIM_mlev ;

   /* the full sized warping, using the initial warp from above (in WO_iwarp) */

   Swarp = IW3D_warpomatic( bim , wbim , sim , meth_code , warp_flags ) ;

   IW3D_destroy(WO_iwarp) ; WO_iwarp = NULL ; Hlev_start = 0 ;
   Hworkhard1 = Htemp1 ; Hworkhard2 = Htemp2 ;

   outim = IW3D_warp_floatim( Swarp, sim , interp_code , 1.0f ) ;

   imww       = (Image_plus_Warp *)malloc(sizeof(Image_plus_Warp)) ;
   imww->im   = outim ;
   imww->warp = Swarp ;

   RETURN(imww) ;
}

#endif /*(Q10)*/ /*###########################################################*/

#if 1
/*============================================================================*/
/** (Q11) All the above functions copied and edited for plusminus warping    **/
/*============================================================================*/

/*****--------------------------------------------------------------------*****/
/*****||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||*****/
/*****--------------------------------------------------------------------*****/
#ifdef ALLOW_PLUSMINUS
/*****--------------------------------------------------------------------*****/
/*****||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||*****/
/*****--------------------------------------------------------------------*****/
/*****          Optimization of 'plusminus' warps:                        *****/
/*****            matching base(x-a(x)) = source(x+a(x))                  *****/
/*****          instead of      base(x) = source(x+a(x))                  *****/
/*****--------------------------------------------------------------------*****/
/*---- There is a lot of duplicated+edited code below, mostly uncommented ----*/
/*---- If you want to understand, first grok the parent functions!        ----*/
/*****--------------------------------------------------------------------*****/

static float *Hwval_plus  = NULL ;
static float *Hwval_minus = NULL ;

static MRI_IMAGE *Haasrcim_plus  = NULL ; /* warped source image (global) */
static MRI_IMAGE *Haabasim_minus = NULL ; /* warped base   image (global) */

/*----------------------------------------------------------------------------*/

void Hwarp_apply_plusminus( float *valp , float *valm )
{
   int   nbx,nby,nbz , nbxy,nbxyz , nAx,nAy,nAz , nAx1,nAy1,nAz1 , nAxy ;
   float nAxh,nAyh,nAzh ;
   float *hxd,*hyd,*hzd , *Axd,*Ayd,*Azd , *sarp,*sarm , *bxd,*byd,*bzd ;
#ifndef USE_HLOADER
   void (*Heval)(int,float *,float *,float *) = NULL ;  /* compute Hwarp at one index */
#endif

ENTRY("Hwarp_apply_plusminus") ;

   hxd = Hwarp->xd  ; hyd = Hwarp->yd  ; hzd = Hwarp->zd  ; /* Hwarp delta */
   Axd = Haawarp->xd; Ayd = Haawarp->yd; Azd = Haawarp->zd; /* Haawarp */
   bxd = AHwarp->xd ; byd = AHwarp->yd ; bzd = AHwarp->zd ; /* AHwarp delta */

   if( Hbasis_code == MRI_QUINTIC ){ nbx = nbqx ; nby = nbqy ; nbz = nbqz ; }
   else                            { nbx = nbcx ; nby = nbcy ; nbz = nbcz ; }
   nbxy = nbx*nby ; nbxyz = nbxy*nbz ;

#ifndef USE_HLOADER
   if( Hbasis_code == MRI_CUBIC ){
     Heval = (bbbcar == NULL) ? HCwarp_eval_A : HCwarp_eval_B ;
   } else if( Hbasis_code == MRI_QUINTIC ){
     Heval = (bbbqar == NULL) ? HQwarp_eval_A : HQwarp_eval_B ;
   } else if( Hbasis_code == MRI_CUBIC_PLUS_1 ){
     Heval = HCwarp_eval_B_basis3 ;
   } else if( Hbasis_code == MRI_CUBIC_PLUS_2 ){
     Heval = HCwarp_eval_B_basis4 ;
   } else if( Hbasis_code == MRI_CUBIC_PLUS_3 ){
     Heval = HCwarp_eval_B_basis5 ;
   }
#endif

   nAx  = Haawarp->nx; nAy  = Haawarp->ny; nAz  = Haawarp->nz; nAxy = nAx*nAy;
   nAx1 = nAx-1      ; nAy1 = nAy-1      ; nAz1 = nAz-1      ;
   nAxh = nAx-0.501f ; nAyh = nAy-0.501f ; nAzh = nAz-0.501f ;

   sarp = MRI_FLOAT_PTR(SRCIM) ;  /* source image array */
   sarm = MRI_FLOAT_PTR(BASIM) ;  /* base image array */

STATUS("start loop") ;

#undef  IJK
#define IJK(i,j,k) ((i)+(j)*nAx+(k)*nAxy)

#undef  XINT
#define XINT(aaa,j,k) wt_00*aaa[IJK(ix_00,j,k)]+wt_p1*aaa[IJK(ix_p1,j,k)]

AFNI_OMP_START ;
#pragma omp parallel
 { int ii,jj,kk , qq , need_val ;
   float xq,yq,zq ;
   float fx,fy,fz , ix,jy,kz ;
   int   ix_00,ix_p1 , jy_00,jy_p1 , kz_00,kz_p1 ;
   float wt_00,wt_p1 ;
   float f_j00_k00, f_jp1_k00, f_j00_kp1, f_jp1_kp1, f_k00, f_kp1 ;
   float g_j00_k00, g_jp1_k00, g_j00_kp1, g_jp1_kp1, g_k00, g_kp1 ;
   float h_j00_k00, h_jp1_k00, h_j00_kp1, h_jp1_kp1, h_k00, h_kp1 ;
#ifdef USE_OMP
   int ith = omp_get_thread_num() ;
#else
   int ith = 0 ;
#endif

#pragma omp for
   for( qq=0 ; qq < nbxyz ; qq++ ){            /* for each voxel in the patch */
     ii = qq % nbx; kk = qq / nbxy; jj = (qq-kk*nbxy) / nbx; /* patch indexes */

     /* determine if we actually need this value (is it in the mask?) */

     need_val = ( Hbmask[IJK(ii+Hibot,jj+Hjbot,kk+Hkbot)] != 0 ) ;

     if( !need_val && !need_AH ){ valp[qq] = valm[qq] = 0.0f; continue; }

#ifndef USE_HLOADER
     Heval(qq,hxd+qq,hyd+qq,hzd+qq) ;  /* if warp not loaded, evaluate it now */
#endif

     /* get Hwarp-ed indexes into Haawarp; e.g.,
          xq = Hibot + ii + hxd[qq]
        because the Hwarp output index warp location is computed as
          Hwarp_x(x,y,z) = x + hxd
        and we also have to add in Hibot to get a global index for use in Haawarp */

#if 0
     xq = Hibot + ii + hxd[qq] ; if( xq < -0.499f ) xq = -0.499f; else if( xq > nAxh ) xq = nAxh;
     yq = Hjbot + jj + hyd[qq] ; if( yq < -0.499f ) yq = -0.499f; else if( yq > nAyh ) yq = nAyh;
     zq = Hkbot + kk + hzd[qq] ; if( zq < -0.499f ) zq = -0.499f; else if( zq > nAzh ) zq = nAzh;
     ix = floorf(xq) ;  fx = xq - ix ;
     jy = floorf(yq) ;  fy = yq - jy ;
     kz = floorf(zq) ;  fz = zq - kz ;
     ix_00 = ix ; ix_p1 = ix_00+1 ; CLIP(ix_00,nAx1) ; CLIP(ix_p1,nAx1) ;
     jy_00 = jy ; jy_p1 = jy_00+1 ; CLIP(jy_00,nAy1) ; CLIP(jy_p1,nAy1) ;
     kz_00 = kz ; kz_p1 = kz_00+1 ; CLIP(kz_00,nAz1) ; CLIP(kz_p1,nAz1) ;
#else
     xq = Hibot + ii + hxd[qq] ; ix = (int)(xq) ; fx = xq - ix ;
     yq = Hjbot + jj + hyd[qq] ; jy = (int)(yq) ; fy = yq - jy ;
     zq = Hkbot + kk + hzd[qq] ; kz = (int)(zq) ; fz = zq - kz ;
     ix_00 = ix ; ix_p1 = ix_00+1 ; QLIP(ix_00,nAx1) ; QLIP(ix_p1,nAx1) ;
     jy_00 = jy ; jy_p1 = jy_00+1 ; QLIP(jy_00,nAy1) ; QLIP(jy_p1,nAy1) ;
     kz_00 = kz ; kz_p1 = kz_00+1 ; QLIP(kz_00,nAz1) ; QLIP(kz_p1,nAz1) ;
#endif

     /* linearly interpolate in Haawarp to get Haawarp displacements */

     wt_00 = 1.0f-fx ; wt_p1 = fx ;   /* x interpolations */
     f_j00_k00 = XINT(Axd,jy_00,kz_00) ; f_jp1_k00 = XINT(Axd,jy_p1,kz_00) ;
     f_j00_kp1 = XINT(Axd,jy_00,kz_p1) ; f_jp1_kp1 = XINT(Axd,jy_p1,kz_p1) ;
     g_j00_k00 = XINT(Ayd,jy_00,kz_00) ; g_jp1_k00 = XINT(Ayd,jy_p1,kz_00) ;
     g_j00_kp1 = XINT(Ayd,jy_00,kz_p1) ; g_jp1_kp1 = XINT(Ayd,jy_p1,kz_p1) ;
     h_j00_k00 = XINT(Azd,jy_00,kz_00) ; h_jp1_k00 = XINT(Azd,jy_p1,kz_00) ;
     h_j00_kp1 = XINT(Azd,jy_00,kz_p1) ; h_jp1_kp1 = XINT(Azd,jy_p1,kz_p1) ;

     wt_00 = 1.0f-fy ;                /* y interpolations */
     f_k00 = wt_00 * f_j00_k00 + fy * f_jp1_k00 ;
     f_kp1 = wt_00 * f_j00_kp1 + fy * f_jp1_kp1 ;
     g_k00 = wt_00 * g_j00_k00 + fy * g_jp1_k00 ;
     g_kp1 = wt_00 * g_j00_kp1 + fy * g_jp1_kp1 ;
     h_k00 = wt_00 * h_j00_k00 + fy * h_jp1_k00 ;
     h_kp1 = wt_00 * h_j00_kp1 + fy * h_jp1_kp1 ;

     wt_00 = 1.0f-fz ;                /* z interpolations */

     /* bxd = x-displacments for AHwarp = Awarp(Hwarp())
        xq  = index in srcim for output interpolation to get val */

     bxd[qq] = wt_00 * f_k00 + fz * f_kp1 + hxd[qq] ;
     byd[qq] = wt_00 * g_k00 + fz * g_kp1 + hyd[qq] ;
     bzd[qq] = wt_00 * h_k00 + fz * h_kp1 + hzd[qq] ;

     /* if not in the global mask, don't bother to compute val */

     if( !need_val ){ valp[qq] = valm[qq] = 0.0f; continue; }

     /** interpolate for POSITIVE displacements **/

     xq = bxd[qq]+ii+Hibot ; yq = byd[qq]+jj+Hjbot ; zq = bzd[qq]+kk+Hkbot ;

     if( xq < -0.499f ) xq = -0.499f; else if( xq > nAxh ) xq = nAxh;
     if( yq < -0.499f ) yq = -0.499f; else if( yq > nAyh ) yq = nAyh;
     if( zq < -0.499f ) zq = -0.499f; else if( zq > nAzh ) zq = nAzh;

     if( Himeth == MRI_NN ){
       ix_00   = (int)(xq+0.5f) ;
       jy_00   = (int)(yq+0.5f) ;
       kz_00   = (int)(zq+0.5f) ;
       valp[qq] = sarp[IJK(ix_00,jy_00,kz_00)] ;
     } else {
       ix = floorf(xq) ;  fx = xq - ix ;
       jy = floorf(yq) ;  fy = yq - jy ;
       kz = floorf(zq) ;  fz = zq - kz ;
       ix_00 = ix ; ix_p1 = ix_00+1 ; CLIP(ix_00,nAx1) ; CLIP(ix_p1,nAx1) ;
       jy_00 = jy ; jy_p1 = jy_00+1 ; CLIP(jy_00,nAy1) ; CLIP(jy_p1,nAy1) ;
       kz_00 = kz ; kz_p1 = kz_00+1 ; CLIP(kz_00,nAz1) ; CLIP(kz_p1,nAz1) ;

       wt_00 = 1.0f-fx ; wt_p1 = fx ;                     /* x interpolations */
       f_j00_k00 = XINT(sarp,jy_00,kz_00) ; f_jp1_k00 = XINT(sarp,jy_p1,kz_00) ;
       f_j00_kp1 = XINT(sarp,jy_00,kz_p1) ; f_jp1_kp1 = XINT(sarp,jy_p1,kz_p1) ;
       wt_00 = 1.0f-fy ;                                  /* y interpolations */
       f_k00 = wt_00 * f_j00_k00 + fy * f_jp1_k00 ;
       f_kp1 = wt_00 * f_j00_kp1 + fy * f_jp1_kp1 ;
       valp[qq] = (1.0f-fz) * f_k00 + fz * f_kp1 ; /* z interpolation = output */
     }

     /** duplicate for NEGATIVE displacements **/

     xq = -bxd[qq]+ii+Hibot ; yq = -byd[qq]+jj+Hjbot ; zq = -bzd[qq]+kk+Hkbot ;

     if( xq < -0.499f ) xq = -0.499f; else if( xq > nAxh ) xq = nAxh;
     if( yq < -0.499f ) yq = -0.499f; else if( yq > nAyh ) yq = nAyh;
     if( zq < -0.499f ) zq = -0.499f; else if( zq > nAzh ) zq = nAzh;

     if( Himeth == MRI_NN ){
       ix_00   = (int)(xq+0.5f) ;
       jy_00   = (int)(yq+0.5f) ;
       kz_00   = (int)(zq+0.5f) ;
       valm[qq] = sarm[IJK(ix_00,jy_00,kz_00)] ;
     } else {
       ix = floorf(xq) ;  fx = xq - ix ;
       jy = floorf(yq) ;  fy = yq - jy ;
       kz = floorf(zq) ;  fz = zq - kz ;
       ix_00 = ix ; ix_p1 = ix_00+1 ; CLIP(ix_00,nAx1) ; CLIP(ix_p1,nAx1) ;
       jy_00 = jy ; jy_p1 = jy_00+1 ; CLIP(jy_00,nAy1) ; CLIP(jy_p1,nAy1) ;
       kz_00 = kz ; kz_p1 = kz_00+1 ; CLIP(kz_00,nAz1) ; CLIP(kz_p1,nAz1) ;

       wt_00 = 1.0f-fx ; wt_p1 = fx ;                     /* x interpolations */
       f_j00_k00 = XINT(sarm,jy_00,kz_00) ; f_jp1_k00 = XINT(sarm,jy_p1,kz_00) ;
       f_j00_kp1 = XINT(sarm,jy_00,kz_p1) ; f_jp1_kp1 = XINT(sarm,jy_p1,kz_p1) ;
       wt_00 = 1.0f-fy ;                                  /* y interpolations */
       f_k00 = wt_00 * f_j00_k00 + fy * f_jp1_k00 ;
       f_kp1 = wt_00 * f_j00_kp1 + fy * f_jp1_kp1 ;
       valm[qq] = (1.0f-fz) * f_k00 + fz * f_kp1 ; /* z interpolation = output */
     }

   } /* end of for loop */
 } /* end of parallel stuff */
AFNI_OMP_END ;

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/

double IW3D_scalar_costfun_plusminus( int npar , double *dpar )
{
   double cost=0.0 ; int ii ;

   /* compute Hwarp given the params */

   if( Hparmap != NULL ){
     for( ii=0 ; ii < Hnpar ; ii++ ) Hpar[ii] = 0.0f ;
     for( ii=0 ; ii < npar  ; ii++ ) Hpar[ Hparmap[ii] ] = (float)dpar[ii] ;
   } else {
     for( ii=0 ; ii < Hnpar ; ii++ ){
       Hpar[ii] = (float)dpar[ii] ;
       if( !isfinite(Hpar[ii]) ){
         ERROR_message("bad Hpar[%d]=%g dpar=%g",ii,Hpar[ii],dpar[ii]) ;
         Hpar[ii] = dpar[ii] = 0.0 ;
       }
     }
   }

#ifdef USE_HLOADER
   Hloader(Hpar) ;  /* loads Hwarp */
#endif

   /* compute warped image over the patch, into Hwval array */

   Hwarp_apply_plusminus(Hwval_plus,Hwval_minus) ;

   /* compute the rest of the cost function */

   cost = INCOR_evaluate( Hincor , Hnval , Hwval_minus , Hwval_plus ,
                          (Haawt != NULL ) ? Haawt : MRI_FLOAT_PTR(Hwtim) ) ;
   if( Hnegate ) cost = -cost ;

   if( !isfinite(cost) ){
     ERROR_message("bad Warpomatic cost = %g -- input parameters:",cost) ;
     for( ii=0 ; ii < npar ; ii++ ) fprintf(stderr," %g",dpar[ii]) ;
     fprintf(stderr,"\n") ;
   }

   if( Hpen_use ){
     Hpenn = HPEN_penalty() ; cost += Hpenn ;  /* penalty is saved in Hpenn */
   } else {
     Hpenn = 0.0f ;
   }

   if( Hfirsttime ){
     if( Hverb ) fprintf(stderr,"[first cost=%.5f]%c",cost , ((Hverb>1) ? '\n' : ' ') ) ;
     Hfirsttime = 0 ; Hfirstcost = (float)cost ;
   }

   return cost ;
}

/*----------------------------------------------------------------------------*/

int IW3D_improve_warp_plusminus( int warp_code ,
                                 int ibot, int itop,
                                 int jbot, int jtop, int kbot, int ktop )
{
   MRI_IMAGE *warpim ;
   int nxh,nyh,nzh , ii,jj,kk , iter,itmax,qq,pp , nwb , nball ;
   float *wbfar , wsum ; double prad ;
   double *parvec, *xbot,*xtop ;
   float *sarp,*sarm , *Axd,*Ayd,*Azd,*Aje,*Ase , *bxd,*byd,*bzd,*bje,*bse , jt,st ;
   int ballopt = (SC_BALL == powell_newuoa_get_con()) ;  /* 30 Oct 2015 */

ENTRY("IW3D_improve_warp_plusminus") ;

   /*-- setup local region for Hwarp --*/

   CLIP(ibot,Hnx-1) ; CLIP(itop,Hnx-1) ;
   CLIP(jbot,Hny-1) ; CLIP(jtop,Hny-1) ;
   CLIP(kbot,Hnz-1) ; CLIP(ktop,Hnz-1) ;

   nxh = itop-ibot+1 ; nyh = jtop-jbot+1 ; nzh = ktop-kbot+1 ;

   if( nxh < NGMIN && nyh < NGMIN && nzh < NGMIN ) RETURN(0) ;

   Hibot = ibot ; Hitop = itop ; /* index range of the patch we're working on */
   Hjbot = jbot ; Hjtop = jtop ;
   Hkbot = kbot ; Hktop = ktop ;

   /* test if this region has enough "weight" to process */

   Hnval = nxh*nyh*nzh ;

   nball = (warp_code==MRI_CUBIC) ? 15 : 19 ;
   if( nxh < nball || nyh < nball || nzh < nball ){      /* 10 Feb 2017 */
     ballopt = 1 ; BALLOPT ;
   }

   wbfar = MRI_FLOAT_PTR(Hwtim) ; wsum = 0.0f ;
   for( nwb=0,kk=kbot ; kk <= ktop ; kk++ ){
     for( jj=jbot ; jj <= jtop ; jj++ ){
       for( ii=ibot ; ii <= itop ; ii++ ){
         qq = ii + jj*Hnx + kk*Hnxy ;
         if( Hbmask[qq] ){ wsum += wbfar[qq] ; nwb++ ; }
   }}}
   if( !Hforce && (nwb < 0.333f*Hnval || wsum < 0.222f*Hnval*Hwbar) ){ /* too light for us */
     if( Hverb > 2 )
       ININFO_message(
         "     %7s patch %03d..%03d %03d..%03d %03d..%03d : skipping (%.1f%% inmask %.1f%% weight)" ,
                       WARP_CODE_STRING(warp_code) ,
                       ibot,itop, jbot,jtop, kbot,ktop ,
                       (100.0f*nwb)/Hnval , (100.0f*wsum)/(Hnval*Hwbar) ) ;
     RETURN(0) ;
   }

   /*-- setup the basis functions for Hwarping --*/

   switch( warp_code ){
     default:
     case MRI_CUBIC:
       Hbasis_code   = MRI_CUBIC ;                   /* 3rd order polynomials */
       Hbasis_parmax = 0.044*Hfactor ;    /* max displacement from 1 function */
       if( ballopt ) Hbasis_parmax = 0.077*Hfactor ;           /* 13 Jan 2015 */
       Hnpar         = 24 ;                /* number of params for local warp */
       prad          = 0.333 ;                       /* NEWUOA initial radius */
       HCwarp_setup_basis( nxh,nyh,nzh, Hgflags ) ;      /* setup HCwarp_load */
#ifdef USE_HLOADER
       Hloader       = HCwarp_load ;   /* func to make local warp from params */
#endif
     break ;

     case MRI_QUINTIC:
       Hbasis_code   = MRI_QUINTIC ;                 /* 5th order polynomials */
       Hbasis_parmax = 0.0088*Hfactor ;
       if( ballopt ) Hbasis_parmax = 0.066*Hfactor ;           /* 13 Jan 2015 */
       Hnpar         = 81 ;
       prad          = 0.333 ;
       HQwarp_setup_basis( nxh,nyh,nzh, Hgflags ) ;
#ifdef USE_HLOADER
       Hloader       = HQwarp_load ;
#endif
     break ;

#ifdef ALLOW_BASIS5  /* 05 Nov 2015 */
     case MRI_CUBIC_PLUS_1:  /* basis3 */
       BALLOPT ; ballopt = 1 ;
       Hbasis_code = MRI_CUBIC_PLUS_1 ;
       Hbasis_parmax = 0.0432*Hfactor ;
       Hnpar         = 81 ;
       prad          = 0.333 ;
       HCwarp_setup_basis5( nxh,nyh,nzh, Hgflags , 1 ) ;
     break ;

     case MRI_CUBIC_PLUS_2:  /* basis4 */
       BALLOPT ; ballopt = 1 ;
       Hbasis_code = MRI_CUBIC_PLUS_2 ;
       Hbasis_parmax = 0.0222*Hfactor ;
       Hnpar         = 192 ;
       prad          = 0.333 ;
       HCwarp_setup_basis5( nxh,nyh,nzh, Hgflags , 2 ) ;
     break ;

     case MRI_CUBIC_PLUS_3:  /* basis5 */
       BALLOPT ; ballopt = 1 ;
       Hbasis_code = MRI_CUBIC_PLUS_3 ;
       Hbasis_parmax = 0.0155*Hfactor ;
       Hnpar         = 375 ;
       prad          = 0.333 ;
       HCwarp_setup_basis5( nxh,nyh,nzh, Hgflags , 3 ) ;
     break ;
#endif
   }

   /* skip if not enough points for number of parameters [07 Apr 2016] */

   if( nwb < 2*Hnparmap ){  /* Hnparmap was set in a *setup_basis* just above */
     if( Hverb > 2 )
       ININFO_message(
         "     %s patch %03d..%03d %03d..%03d %03d..%03d : skipping (%d voxels inmask vs %d parameters)" ,
                       WARP_CODE_STRING(warp_code) ,
                       ibot,itop, jbot,jtop, kbot,ktop ,
                       nwb , Hnparmap ) ;
     Hskipped++ ; RETURN(0) ;
   }

   Hdox = !(Hflags & NWARP_NOXDIS_FLAG) ;  /* do the x direction? */
   Hdoy = !(Hflags & NWARP_NOYDIS_FLAG) ;  /* y? */
   Hdoz = !(Hflags & NWARP_NOZDIS_FLAG) ;  /* z? */

   Hpar  = (float *)realloc(Hpar,sizeof(float)*Hnpar) ;
   for( ii=0 ; ii < Hnpar ; ii++ ) Hpar[ii] = 0.0f ;
   Hxpar = Hpar ;
   Hypar = Hxpar + (Hnpar/3) ;
   Hzpar = Hypar + (Hnpar/3) ;

   /*-- create space for local warped image values --*/

   Hwval_plus  = (float *)realloc(Hwval_plus ,sizeof(float)*Hnval) ;
   Hwval_minus = (float *)realloc(Hwval_minus,sizeof(float)*Hnval) ;

   /*-- setup to do incremental 'correlation' on the local region --*/

   INCOR_destroy(Hincor) ;
   Hincor = INCOR_create( Hmatch_code , Hmpar ) ;

   FREEIFNN(Haawt) ;

   need_AH = Hpen_use ;
   if( Hpen_use ) Hpen_sum = 0.0 ;

#undef  RESTORE_WBFAR
#define RESTORE_WBFAR                           \
 do{ for( pp=0,kk=kbot ; kk <= ktop ; kk++ )    \
      for( jj=jbot ; jj <= jtop ; jj++ )        \
       for( ii=ibot ; ii <= itop ; ii++,pp++ )  \
        wbfar[ii+jj*Hnx+kk*Hnxy] = Haawt[pp] ;  \
 } while(0)

   if( Hnval < Hnxyz ){                               /* initialize correlation from   */
     float *wbfar=MRI_FLOAT_PTR(Hwtim) ;              /* non-changing part of Haasrcim */
     float *bar  =MRI_FLOAT_PTR(BASIM) ;

     Haawt = (float *)malloc(sizeof(float)*Hnval) ;
     for( pp=0,kk=kbot ; kk <= ktop ; kk++ ){      /* extract weights  */
       for( jj=jbot ; jj <= jtop ; jj++ ){         /* and base image   */
         for( ii=ibot ; ii <= itop ; ii++,pp++ ){  /* for patch region */
           qq        = ii + jj*Hnx + kk*Hnxy ;
           Haawt[pp] = wbfar[qq] ;  /* copy weight image vals */
           wbfar[qq] = 0.0f ;       /* 0 out temp weight */
     }}}

     /* initialize the 'correlation' from the data that won't
        be changing (i.e., data from outside the local patch) */

     if( !Hlocalstat )
     INCOR_addto( Hincor , Hnxyz ,
                  MRI_FLOAT_PTR(Haabasim_minus) , MRI_FLOAT_PTR(Haasrcim_plus) , wbfar ) ;
     RESTORE_WBFAR ;

     /* also init penalty from non-changing part of Haawarp, if needed */

     if( Hpen_use ){
       float *je , *se ;
       je = Haawarp->je ; se = Haawarp->se ;
       for( kk=kbot ; kk <= ktop ; kk++ )
        for( jj=jbot ; jj <= jtop ; jj++ )
         for( ii=ibot ; ii <= itop ; ii++ )
          je[ii+jj*Hnx+kk*Hnxy] = se[ii+jj*Hnx+kk*Hnxy] = 0.0f ;
       Hpen_sum = HPEN_addup(Hnxyz,je,se) ;
     }
   }

   /* optimization of warp parameters */

   parvec = (double *)malloc(sizeof(double)*Hnparmap) ;
   xbot   = (double *)malloc(sizeof(double)*Hnparmap) ;
   xtop   = (double *)malloc(sizeof(double)*Hnparmap) ;
   for( ii=0 ; ii < Hnparmap ; ii++ ){
     parvec[ii] = 0.0 ;
     xbot[ii]   = -Hbasis_parmax ;
     xtop[ii]   =  Hbasis_parmax ;
   }

   powell_set_mfac( 1.001f , 2.001f ) ;

   /***** HERE is the actual optimization! *****/

#if 1
   itmax = (Hduplo) ? 6*Hnparmap+29 : 8*Hnparmap+31 ;
#else
   itmax = 8*Hnparmap+31 ;
#endif
   if( WORKHARD(Hlev_now) || SUPERHARD(Hlev_now) ) itmax -= Hnparmap ;

   if( Hverb > 3 ) powell_set_verbose(1) ;

   iter = powell_newuoa_con( Hnparmap , parvec,xbot,xtop , 0 ,
                             prad,0.009*prad , itmax , IW3D_scalar_costfun_plusminus ) ;

   if( iter > 0 ) Hnpar_sum += Hnparmap ;

   if( Hverb > 3 ) powell_set_verbose(0) ;

   /***** cleanup and exit phase ***/

   free(xtop) ; free(xbot) ;

   if( iter <= 0 ){  /* something bad happened */
     ININFO_message(
       "     %s patch %03d..%03d %03d..%03d %03d..%03d : skipping - powell_newuoa_con() failure code=%d" ,
                     WARP_CODE_STRING(warp_code) ,
                     ibot,itop, jbot,jtop, kbot,ktop , iter ) ;
     ININFO_message(
      "powell_newuoa_con( ndim=%d  x=%p  xbot=%p  xtop=%p  nrand=%d  rstart=%f  rend=%f  maxcall=%d  ufunc=%p",
      Hnparmap , (void *)parvec , (void *)xbot , (void *)xtop , 0 , prad , 0.009*prad , itmax ,
      (void *)IW3D_scalar_costfun ) ;
     free(parvec); RETURN(0);
   }

   /* load optimized warped image and warp into their patches */

   need_AH = 1 ;
   Hcost = IW3D_scalar_costfun_plusminus( Hnparmap , parvec ) ;  /* evaluate at current results */
   (void)IW3D_load_energy(AHwarp) ;

   /* AHwarp gets loaded into Haawarp and Hwval_plus into Haasrcim_plus
                                      and Hwval_minus into Haabasim_minus */

   sarp = MRI_FLOAT_PTR(Haasrcim_plus) ;
   sarm = MRI_FLOAT_PTR(Haabasim_minus) ;
   Axd = Haawarp->xd; Ayd = Haawarp->yd; Azd = Haawarp->zd; Aje = Haawarp->je; Ase = Haawarp->se;
   bxd = AHwarp->xd ; byd = AHwarp->yd ; bzd = AHwarp->zd ; bje = AHwarp->je ; bse = AHwarp->se ;

   jt= bje[0] ; st = bse[0] ;
   for( pp=0,kk=kbot ; kk <= ktop ; kk++ ){
     for( jj=jbot ; jj <= jtop ; jj++ ){
       for( ii=ibot ; ii <= itop ; ii++,pp++ ){
         qq = ii + jj*Hnx + kk*Hnxy ;
         sarp[qq] = Hwval_plus[pp] ;
         sarm[qq] = Hwval_minus[pp] ;
         Axd[qq] = bxd[pp] ; Ayd[qq] = byd[pp] ; Azd[qq] = bzd[pp] ;
         Aje[qq] = bje[pp] ; Ase[qq] = bse[pp] ;
         if( Aje[qq] > jt ) jt = Aje[qq] ;
         if( Ase[qq] > st ) st = Ase[qq] ;
   }}}

   if( Hverb > 1 ){
     ININFO_message(
       "     %7s patch %03d..%03d %03d..%03d %03d..%03d : cost=%.5f iter=%d : energy=%.3f:%.3f pen=%g",
                     WARP_CODE_STRING(Hbasis_code) ,
                           ibot,itop, jbot,jtop, kbot,ktop , Hcost  , iter , jt,st , Hpenn ) ;
   } else if( Hverb == 1 && (Hlev_now<=2 || lrand48()%(Hlev_now*Hlev_now*Hlev_now/9)==0) ){
     fprintf(stderr,".") ;
   }

   /* vamoose the ranch */

   free(parvec) ; RETURN(iter) ;
}

/*----------------------------------------------------------------------------*/

void IW3D_cleanup_improvement_plusminus(void)
{
ENTRY("IW3D_cleanup_improvement_plusminus") ;

   mri_free(Hbasim)   ; Hbasim   = NULL ;
   mri_free(Hsrcim)   ; Hsrcim   = NULL ;
   mri_free(Hwtim)    ; Hwtim    = NULL ; FREEIFNN(Hbmask) ;
   mri_free(Haasrcim) ; Haasrcim = NULL ;
   mri_free(Haasrcim_plus) ; Haasrcim_plus  = NULL ;
   mri_free(Haabasim_minus); Haabasim_minus = NULL ;

   mri_free(Hsrcim_blur) ; Hsrcim_blur = NULL ;
   mri_free(Hbasim_blur) ; Hbasim_blur = NULL ;

   IW3D_destroy(Hwarp)   ; Hwarp   = NULL ;
   IW3D_destroy(AHwarp)  ; AHwarp  = NULL ;
   IW3D_destroy(Haawarp) ; Haawarp = NULL ;

   INCOR_set_lpc_mask(NULL) ;  /* 25 Jun 2014 */
   INCOR_destroy(Hincor) ; Hincor = NULL ; KILL_floatvec(Hmpar) ;
   FREEIFNN(Hpar) ; FREEIFNN(Hwval) ; FREEIFNN(Haawt) ; FREEIFNN(Hbval) ;
   FREEIFNN(Hparmap) ; Hnparmap = Hnpar = 0 ; Hbasis_code = -666 ;
   FREEIFNN(Hwval_plus) ; FREEIFNN(Hwval_minus) ;

   FREEIFNN(bc0x); FREEIFNN(bc1x); nbcx=0;
   FREEIFNN(bc0y); FREEIFNN(bc1y); nbcy=0;
   FREEIFNN(bc0z); FREEIFNN(bc1z); nbcz=0;
   FREEIFNN(bc2x); FREEIFNN(bc3x); FREEIFNN(bc4x);
   FREEIFNN(bc2y); FREEIFNN(bc3y); FREEIFNN(bc4y);
   FREEIFNN(bc2z); FREEIFNN(bc3z); FREEIFNN(bc4z);

   FREEIFNN(bq0x); FREEIFNN(bq1x); FREEIFNN(bq2x); nbqx=0;
   FREEIFNN(bq0y); FREEIFNN(bq1y); FREEIFNN(bq2y); nbqy=0;
   FREEIFNN(bq0z); FREEIFNN(bq1z); FREEIFNN(bq2z); nbqz=0;

   if( bbbcar != NULL ){
     int ii ;
     for( ii=0 ; ii < nbbbcar ; ii++ ) FREEIFNN(bbbcar[ii]) ;
     free(bbbcar) ; nbbcxyz = nbbbcar = 0 ; bbbcar = NULL ;
   }

   if( bbbqar != NULL ){
     int ii ;
     for( ii=0 ; ii < 27 ; ii++ ) FREEIFNN(bbbqar[ii]) ;
     free(bbbqar) ; nbbqxyz = 0 ; bbbqar = NULL ;
   }

   Hstopcost = -666666.6f ;
   Hstopped  = 0 ;
   Hfinal    = 0 ;

   HSAVE_DESTROY ;

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/

void IW3D_setup_for_improvement_plusminus(
                                 MRI_IMAGE *bim, MRI_IMAGE *wbim, MRI_IMAGE *sim,
                                 IndexWarp3D *Iwarp,
                                 int meth_code, int warp_flags )
{
   int iii , nmask ;

ENTRY("IW3D_setup_for_improvement_plusminus") ;

   /*-- eliminate old stuff --*/

   IW3D_cleanup_improvement_plusminus() ;

   /*-- copy base and source images --*/

   Hnx = bim->nx; Hny = bim->ny; Hnz = bim->nz; Hnxy=Hnx*Hny; Hnxyz = Hnxy*Hnz;
   Hbasim = mri_to_float(bim) ;
   Hsrcim = mri_to_float(sim) ;

#ifdef ALLOW_INEDGE /* Jul 2018 */
   if( Hinedge_doit ){
     if( Hverb > 1 ) ININFO_message("  enhancing interior edges of base and source") ;
     mri_interior_edgeize( Hbasim , Hinedge_erode , Hinedge_frac ) ;
     mri_interior_edgeize( Hsrcim , Hinedge_erode , Hinedge_frac ) ;
   }
#endif

   if( Hpblur_b > 0.0f && Hblur_b == 0.0f ) Hblur_b = 0.1f ;
   if( Hpblur_s > 0.0f && Hblur_s == 0.0f ) Hblur_s = 0.1f ;

   Hbasim_blur = IW3D_do_blurring( Hblur_b , Hpblur_b ,
                                   0.5f*Hnx,0.5f*Hny,0.5f*Hnz, Hbasim, "base"   ) ;
   Hsrcim_blur = IW3D_do_blurring( Hblur_s , Hpblur_s ,
                                   0.5f*Hnx,0.5f*Hny,0.5f*Hnz, Hsrcim, "source" ) ;

   /*-- and copy or create base weight image --*/

   if( wbim != NULL ){               /*-- user supplied weight --*/

     int ii,nwb,nexc ; float *wbfar ;
     if( wbim->kind != MRI_float ||
         wbim->nx != Hnx || wbim->ny != Hny || wbim->nz != Hnz )
       ERROR_exit("IW3D_setup_for_improvement_plusminus: bad wbim input") ;

     Hwtim = mri_to_float(wbim) ; wbfar = MRI_FLOAT_PTR(Hwtim) ;
     Hbmask = (byte *)malloc(sizeof(byte)*Hnxyz) ;
     for( Hwbar=nwb=nexc=ii=0 ; ii < Hnxyz ; ii++ ){
       if( Hemask != NULL && Hemask[ii] && wbfar[ii] > 0.0f ){  /* 29 Oct 2012 */
         nexc++ ; wbfar[ii] = 0.0f ;
       }
       Hbmask[ii] = (wbfar[ii] > 0.0f) ;
       if( Hbmask[ii] ){ Hwbar += wbfar[ii] ; nwb++ ; }
       else            { wbfar[ii] = 0.0f ; }
     }
     if( Hwbar == 0.0f || nwb == 0 )
       ERROR_exit("IW3D_setup_for_improvement_plusminus: all zero wbim input") ;
     if( Hverb > 1 ) ININFO_message("   %d voxels in mask (out of %d = %.2f%%)",
                                     nwb,Hnxyz,(100.0*nwb)/Hnxyz ) ;
     Hwbar /= nwb ;  /* average value of all nonzero weights */
     nmask = nwb ;
     if( nexc > 0 ) ININFO_message("-emask excluded %d voxels",nexc) ;

   } else {                          /*-- make weight up from nowhere --*/

     int ii,nwb,nexc ; float *wbfar ;
     Hwtim = mri_new_vol(Hnx,Hny,Hnz,MRI_float); wbfar = MRI_FLOAT_PTR(Hwtim);
     Hbmask = (byte *)malloc(sizeof(byte)*Hnxyz) ;
     for( Hwbar=nwb=nexc=ii=0 ; ii < Hnxyz ; ii++ ){
       if( Hemask != NULL && Hemask[ii] ){ wbfar[ii] = 0.0f; Hbmask[ii] = 0; nexc++; }
       else                              { wbfar[ii] = 1.0f; Hbmask[ii] = 1; }
       if( Hbmask[ii] ){ Hwbar += wbfar[ii] ; nwb++ ; }
     }
     if( Hwbar == 0.0f || nwb == 0 )
       ERROR_exit("IW3D_setup_for_improvement_plusminus: all zero mask!?") ;
     Hwbar /= nwb ;  /* average value of all nonzero weights */
     nmask = nwb ;
     if( nexc > 0 ) ININFO_message("-emask excluded %d voxels",nexc) ;

   }

   /*-- set operating codes --*/

   Hmatch_code = meth_code ; iii = INCOR_check_meth_code(meth_code) ;
   if( iii == 0 )
     ERROR_exit("IW3D_setup_for_improvement_plusminus: bad meth_code input=%d",meth_code) ;

   switch( meth_code ){
     default:                           Hnegate = 0 ; break ;

     case GA_MATCH_PEARSON_LOCALA:
     case GA_MATCH_HELLINGER_SCALAR:
     case GA_MATCH_CRAT_USYM_SCALAR:
     case GA_MATCH_CRAT_SADD_SCALAR:
     case GA_MATCH_CORRATIO_SCALAR:
     case GA_MATCH_KULLBACK_SCALAR:
     case GA_MATCH_PEARCLP_SCALAR:
     case GA_MATCH_PEARSON_SCALAR:      Hnegate = 1 ; break ;
   }

   if( meth_code == GA_MATCH_PEARCLP_SCALAR || meth_code == GA_MATCH_PEARSON_SCALAR )
     Hstopcost = -3.995f ;

   if( iii == 2 || iii == 3 ){  /* uses 2Dhist functions, so setup some parameters */
     float *xar,*yar , *bar,*sar ; int jj,kk ;
     float_quad xyc , xym ;
     bar = MRI_FLOAT_PTR(BASIM) ; sar = MRI_FLOAT_PTR(SRCIM) ;
     if( nmask == Hnxyz ){
       xar = bar ; yar = sar ; kk = Hnxyz ;
     } else {
       xar = (float *)malloc(sizeof(float)*nmask) ;
       yar = (float *)malloc(sizeof(float)*nmask) ;
       for( jj=kk=0 ; jj < Hnxyz ; jj++ ){
         if( Hbmask[jj] ){ xar[kk] = bar[jj] ; yar[kk++] = sar[jj] ; }
       }
     }
     xym = INCOR_2Dhist_minmax( kk , xar , yar ) ;
     xyc = INCOR_2Dhist_xyclip( kk , xar , yar ) ;
     if( xar != bar ){ free(xar) ; free(yar) ; }
     MAKE_floatvec(Hmpar,9) ;
     if( iii == 2 ){
       INCOR_setup_good(Hnxyz) ;
       Hmpar->ar[0] = (float)INCOR_2Dhist_compute_nbin(nmask) ;
       Hmpar->ar[1] = xym.a ; Hmpar->ar[2] = xym.b ;  /* xbot  xtop  */
       Hmpar->ar[3] = xym.c ; Hmpar->ar[4] = xym.d ;  /* ybot  ytop  */
       Hmpar->ar[5] = xyc.a ; Hmpar->ar[6] = xyc.b ;  /* xcbot xctop */
       Hmpar->ar[7] = xyc.c ; Hmpar->ar[8] = xyc.d ;  /* ycbot yctop */
#if 1
       if( Hverb > 1 ){
         ININFO_message("   2Dhist: nbin=%d",(int)Hmpar->ar[0]) ;
         ININFO_message("           xbot=%g xcbot=%g xctop=%g xtop=%g",
                        Hmpar->ar[1], Hmpar->ar[5], Hmpar->ar[6], Hmpar->ar[2] ) ;
         ININFO_message("           ybot=%g ycbot=%g yctop=%g ytop=%g",
                        Hmpar->ar[3], Hmpar->ar[7], Hmpar->ar[8], Hmpar->ar[4] ) ;
       }
#endif
     } else if( iii == 3 ){
       float d1 , d2 , dif ;
       d2 = 0.05f*(xyc.b-xyc.a) ; /* 5% of x clip range */
       d1 = 0.5f*(xyc.a-xym.a) ;  /* half of x clip bot to x min */
                                 dif = MIN(d1,d2) ; Hmpar->ar[1] = xyc.a-dif ; /* xdbot */
       d1 = 0.5f*(xym.b-xyc.b) ; dif = MIN(d1,d2) ; Hmpar->ar[2] = xyc.b+dif ; /* xdtop */
       d2 = 0.05f*(xyc.d-xyc.c) ;
       d1 = 0.5f*(xyc.c-xym.c) ; dif = MIN(d1,d2) ; Hmpar->ar[3] = xyc.c-dif ; /* ydbot */
       d1 = 0.5f*(xym.d-xyc.d) ; dif = MIN(d1,d2) ; Hmpar->ar[4] = xyc.d+dif ; /* ydtop */
       Hmpar->ar[5] = xyc.a ; Hmpar->ar[6] = xyc.b ;                     /* xcbot xctop */
       Hmpar->ar[7] = xyc.c ; Hmpar->ar[8] = xyc.d ;                     /* ycbot yctop */
#if 0
       if( Hverb ){
         ININFO_message("  PEARCLP: xdbot=%g xcbot=%g xctop=%g xdtop=%g",
                        Hmpar->ar[1], Hmpar->ar[5], Hmpar->ar[6], Hmpar->ar[2] ) ;
         ININFO_message("           ydbot=%g ycbot=%g yctop=%g ydtop=%g",
                        Hmpar->ar[3], Hmpar->ar[7], Hmpar->ar[8], Hmpar->ar[4] ) ;
       }
#endif
     }
   } else if( iii == 4 ){   /* Local Pearson setup [25 Jun 2014] */
     INCOR_set_lpc_mask(Hbmask) ;
     MAKE_floatvec(Hmpar,9) ;     /* to be filled in later */
   }

   Hgflags = IW3D_munge_flags(Hnx,Hny,Hnz,warp_flags) ;
   if( Hgflags < 0 )
     ERROR_exit("IW3D_setup_for_improvement: bad warp_flags input") ;

   /*-- copy/create initial warp, and warp the source images --*/
   /*** [10 Aug 2014] Haasrcim_plus and Haabasim_minus are created later ***/

   if( Iwarp != NULL ){
     if( Iwarp->nx != Hnx || Iwarp->ny != Hny || Iwarp->nz != Hnz )
       ERROR_exit("IW3D_setup_for_improvement: bad Iwarp input") ;

     Haawarp = IW3D_copy(Iwarp,1.0f) ;     /* copy it */
   } else {
     Haawarp = IW3D_create(Hnx,Hny,Hnz) ;  /* initialize to 0 displacements */
   }
   (void)IW3D_load_energy(Haawarp) ;  /* initialize energy field for penalty use */

   EXRETURN ;
}

#ifdef USE_PLUSMINUS_INITIALWARP
/*----------------------------------------------------------------------------*/
/* Create an initial warp to the middle by coarse level warping
   of source to base, then by a quick warp square-root-ization.
*//*--------------------------------------------------------------------------*/

IndexWarp3D * IW3D_initialwarp_plusminus( MRI_IMAGE *bim ,
                                          MRI_IMAGE *wbim, MRI_IMAGE *sim,
                                          int meth_code  , int warp_flags  )
{
   IndexWarp3D *Owarp ; IndexWarp3D_pair *Spair ;
   int lstart,lend ; double pfac ;
   int hw1,hw2 , hs1,hs2 ;
   MRI_IMAGE *qwbim ; byte *mask ; int ii ; float *wbar ;

ENTRY("IW3D_initialwarp_plusminus") ;

   qwbim = mri_to_float( (wbim==NULL) ? bim : wbim ) ;
   blur_inplace(qwbim,2.3456f); mask = mri_automask_image(qwbim);
   wbar = MRI_FLOAT_PTR(qwbim);
   for( ii=0 ; ii < wbim->nvox ; ii++ ) if( !mask[ii] ) wbar[ii] = 0.0f ;
   free(mask) ;

   lstart     = Hlev_start ; lend     = Hlev_end ; pfac     = Hpen_fac ;
   Hlev_start = 0          ; Hlev_end = 1        ; Hpen_fac = 0.0f     ;

   hw1 = Hworkhard1 ; hs1 = Hsuperhard1 ;
   hw2 = Hworkhard2 ; hs2 = Hsuperhard2 ;
   Hworkhard1 = Hsuperhard1 = 0 ; Hworkhard2 = Hsuperhard2 = -666 ;

   Owarp = IW3D_warpomatic( bim , qwbim , sim , meth_code , warp_flags ) ;
   mri_free(qwbim) ;

   Hlev_start = lstart ; Hlev_end = lend ; Hpen_fac = pfac ;
   Hworkhard1 = hw1 ; Hsuperhard1 = hs1 ;
   Hworkhard2 = hw2 ; Hsuperhard2 = hs2 ;

   IW3D_scale(Owarp,0.5f) ;
   RETURN(Owarp) ;
}
#endif

/*----------------------------------------------------------------------------*/

IndexWarp3D * IW3D_warpomatic_plusminus( MRI_IMAGE *bim, MRI_IMAGE *wbim, MRI_IMAGE *sim,
                                         int meth_code, int warp_flags                   )
{
   int lev,levs , xwid,ywid,zwid , xdel,ydel,zdel , iter ;
   int ibot,itop,idon , jbot,jtop,jdon , kbot,ktop,kdon , dox,doy,doz , iii ;
   IndexWarp3D *OutWarp ;
   float flev , glev , Hcostold , Hcostmid=0.0f,Hcostend=0.0f ;
   int imin,imax , jmin,jmax, kmin,kmax , ibbb,ittt , jbbb,jttt , kbbb,kttt ;
   int dkkk,djjj,diii , ngmin=0 , levdone=0 , do_qfinal=0 ;
   int qmode=MRI_CUBIC , nlevr , nsup,isup , myIwarp=0 ;
   int qmode2=MRI_CUBIC , qmodeX ;
   char warplab[64] ;

ENTRY("IW3D_warpomatic_plusminus") ;

   Hfirsttime = 1 ;

#ifdef USE_PLUSMINUS_INITIALWARP
   if( WO_iwarp == NULL ){
     if( Hverb ) INFO_message("Initializing +- warp") ;
     WO_iwarp = IW3D_initialwarp_plusminus( bim, wbim, sim, meth_code, warp_flags ) ;
     myIwarp  = 1 ;
   }
#endif

   IW3D_setup_for_improvement_plusminus( bim, wbim, sim, WO_iwarp, meth_code, warp_flags ) ;

   /* range of indexes over which to warp */

   MRI_autobbox( Hwtim , &imin,&imax , &jmin,&jmax , &kmin,&kmax ) ;

   /* do global warping first */

   xwid = (imax-imin)/8       ; ywid = (jmax-jmin)/8       ; zwid = (kmax-kmin)/8       ;
   ibbb = MAX(0,imin-xwid)    ; jbbb = MAX(0,jmin-ywid)    ; kbbb = MAX(0,kmin-zwid)    ;
   ittt = MIN(Hnx-1,imax+xwid); jttt = MIN(Hny-1,jmax+ywid); kttt = MIN(Hnz-1,kmax+zwid);

   diii = ittt-ibbb+1 ; djjj = jttt-jbbb+1 ; dkkk = kttt-kbbb+1 ;
   iter = MAX(diii,djjj) ; iter = MAX(iter,dkkk) ;
   if( iter < NGMIN ){
     ERROR_message("Can't warpomatic such a small volume: %d x %d x %d",diii,djjj,dkkk) ;
     RETURN(NULL) ;
   }

   if( Hverb )
     INFO_message("AFNI warpomatic start: %d x %d x %d volume ; autobbox = %d..%d %d..%d %d..%d",
                  Hnx,Hny,Hnz, imin,imax,jmin,jmax,kmin,kmax) ;

   if( Hlev_start == 0 ){            /* top level = global warps */
#ifdef USE_PLUSMINUS_INITIALWARP
     nlevr = ( WORKHARD(0) || Hduplo ) ? 4 : 2 ; if( SUPERHARD(0) ) nlevr++ ;
#else
     nlevr = 3 ;
#endif
     Hforce = 1 ; Hfactor = Hfactor_q ; Hpen_use = 0 ; Hlev_now = 0 ;
     PBLUR_BASE  (ibbb,ittt,jbbb,jttt,kbbb,kttt) ;  /* progressive blur, if ordered */
     PBLUR_SOURCE(ibbb,ittt,jbbb,jttt,kbbb,kttt) ;
     mri_free(Haasrcim_plus) ;   /* at this point, create the initial */
     mri_free(Haabasim_minus);   /* warped source and base images */
     if( IW3D_is_zero(Haawarp) ){
       Haasrcim_plus  = mri_to_float(SRCIM) ;     /* 'warped' source image */
       Haabasim_minus = mri_to_float(BASIM) ;     /* 'warped' base image */
     } else {
       Haasrcim_plus  = IW3D_warp_floatim( Haawarp, SRCIM, Himeth ,  1.0f ) ;
       Haabasim_minus = IW3D_warp_floatim( Haawarp, BASIM, Himeth , -1.0f ) ;
     }
     if( Hverb == 1 ) fprintf(stderr,"lev=0 %d..%d %d..%d %d..%d: ",ibbb,ittt,jbbb,jttt,kbbb,kttt) ;
     BOXOPT ;
     (void)IW3D_improve_warp_plusminus( MRI_CUBIC  , ibbb,ittt,jbbb,jttt,kbbb,kttt );
     if( Hquitting ) goto DoneDoneDone ;  /* signal to quit was sent */
#if 0
     (void)IW3D_improve_warp_plusminus( MRI_CUBIC  , ibbb,ittt,jbbb,jttt,kbbb,kttt );
     if( Hquitting ) goto DoneDoneDone ;  /* signal to quit was sent */
#endif
     BALLOPT ;
     (void)IW3D_improve_warp_plusminus( MRI_CUBIC  , ibbb,ittt,jbbb,jttt,kbbb,kttt );
     if( Hquitting ) goto DoneDoneDone ;  /* signal to quit was sent */
          if( Hznoq  ) nlevr = 0 ;
     else if( Hzeasy ) nlevr = 1 ;
     for( iii=0 ; iii < nlevr ; iii++ ){
       Hcostold = Hcost ;
       if( iii%2 == 0 ) BOXOPT ;
       else             BALLOPT ;
       (void)IW3D_improve_warp_plusminus( MRI_QUINTIC, ibbb,ittt,jbbb,jttt,kbbb,kttt );
       if( Hquitting ) goto DoneDoneDone ;  /* signal to quit was sent */
       if( iii > 0 && iii < nlevr-1 && Hcostold-Hcost < 0.00444f ){
         if( Hverb > 1 )
           ININFO_message("       --> too little improvement: breaking out of lev=0 iterates") ;
         break ;
       }
     }
     if( Hverb == 1 ) fprintf(stderr," done [cost=%.5f]\n",Hcost) ;
     if( Hsave_allwarps ){
       sprintf(warplab,"Lev0.%04dx%04dx%04d",ittt-ibbb+1,jttt-jbbb+1,kttt-kbbb+1) ;
       HSAVE_ADDTO(Haawarp,warplab) ;
     }
   } else {
     Hcost = 666.666f ;  /* a beastly thing to do */
   }
   Hforce = 0 ; Hlev_final = 0 ; Hpen_use = (Hpen_fac > 0.0f) ;
   Hcostmid = Hcostend = Hcost ;

   if( Hngmin > 0 ){
     ngmin = Hngmin ;
     if( Hduplo ){ ngmin = ngmin/2 + 1 ; if( ngmin < 11 ) ngmin = 11 ; }
   }

        if( ngmin   <  NGMIN ) ngmin = NGMIN ;
   else if( ngmin%2 == 0     ) ngmin-- ;

   if( ngmin >= Hnx && ngmin >= Hny && ngmin >= Hnz ) goto DoneDoneDone ;

   if( Hshrink > 1.0f                       ) Hshrink = 1.0f / Hshrink ;
   if( Hshrink < 0.444f || Hshrink > 0.888f ) Hshrink = 0.749999f ;

   /* iterate down to finer and finer patches */

   levs = MAX(1,Hlev_start) ;
   for( lev=levs ; lev <= Hlev_end && !levdone ; lev++ ){

     flev = (Hpen_old) ? 1.0f : powf( (float)(lev-levs+1) , 0.333f ) ; ;
     Hpen_fff = Hpen_fac * MIN(2.22f,flev) ;  /* 20 Sep 2013 */

     Hpen_use = (Hpen_fff > 0.0f) && (lev > 2) ;

     /* compute width of rectangles at this level */

     flev = powf(Hshrink,(float)lev) ;                 /* shrinkage fraction */
     xwid = (Hnx+1)*flev ; if( xwid%2 == 0 ) xwid++ ;
     ywid = (Hny+1)*flev ; if( ywid%2 == 0 ) ywid++ ;
     zwid = (Hnz+1)*flev ; if( zwid%2 == 0 ) zwid++ ;

     /* decide if we are doing things in x, y, and/or z */

     dox = (xwid >= ngmin) && !(Hgflags & NWARP_NOXDEP_FLAG) ;
     doy = (ywid >= ngmin) && !(Hgflags & NWARP_NOYDEP_FLAG) ;
     doz = (zwid >= ngmin) && !(Hgflags & NWARP_NOZDEP_FLAG) ;

     if( !dox && !doy && !doz ){  /* exit immediately if nothing to do (shrank too far) */
       if( Hverb > 1 )
         ININFO_message("  ---------  lev=%d xwid=%d ywid=%d zwid=%d -- BREAK",lev,xwid,ywid,zwid) ;
       break ;
     }

     /* here, we are doing something, so don't let any width go below threshold */

     Hlev_now = Hlev_final = lev ;  /* in case we leave this loop somewhere below */

     if( xwid < ngmin ) xwid = MIN(Hnx,ngmin);
     if( ywid < ngmin ) ywid = MIN(Hny,ngmin);
     if( zwid < ngmin ) zwid = MIN(Hnz,ngmin);

     /* if we are almost to the smallest allowed patch, jump down to that size now */

     flev = xwid / (float)ngmin ;                                  /* flev is the */
     glev = ywid / (float)ngmin ; if( flev < glev ) flev = glev ;  /* largest ratio */
     glev = zwid / (float)ngmin ; if( flev < glev ) flev = glev ;  /* of ?wid to ngmin */
     if( flev > 1.0f && flev*Hshrink <= 1.00001f ){
       if( xwid > ngmin ) xwid = ngmin ;
       if( ywid > ngmin ) ywid = ngmin ;
       if( zwid > ngmin ) zwid = ngmin ;
       levdone = 1 ;   /* signal to exit when loop finishes */
     } else {
       iter = MAX(xwid,ywid) ; iter = MAX(iter,zwid) ; levdone = (iter == ngmin) ;
     }
     Hfinal = (levdone && !Hduplo) ;

     /* step sizes for shifting the patches */

     xdel = (xwid-1)/2 ; if( xdel == 0 ) xdel = 1 ;
     ydel = (ywid-1)/2 ; if( ydel == 0 ) ydel = 1 ;
     zdel = (zwid-1)/2 ; if( zdel == 0 ) zdel = 1 ;

     diii = xdel ; djjj = ydel ; dkkk = zdel ;

     /* bbbottom and tttop indexes to warp over */

     ibbb = imin-xdel/2-1 ; if( ibbb <  0   ) ibbb = 0 ;
     jbbb = jmin-ydel/2-1 ; if( jbbb <  0   ) jbbb = 0 ;
     kbbb = kmin-zdel/2-1 ; if( kbbb <  0   ) kbbb = 0 ;
     ittt = imax+xdel/2+1 ; if( ittt >= Hnx ) ittt = Hnx-1 ;
     jttt = jmax+ydel/2+1 ; if( jttt >= Hny ) jttt = Hny-1 ;
     kttt = kmax+zdel/2+1 ; if( kttt >= Hnz ) kttt = Hnz-1 ;

#if 0
#define HHH 0.333f
#define BBB 0.888f
     Hfactor = (1.0f-HHH) + HHH*powf(BBB,(float)(lev-1)) ;  /* max displacement allowed */
#else
     Hfactor = Hfactor_q ;
#endif

     qmode = qmode2 = MRI_CUBIC ;  /* cubic patches from here on down */
#ifdef ALLOW_QMODE
     do_qfinal = (Hfinal && Hqfinal) ;
     if( do_qfinal || Hqonly )   { qmode = qmode2 = MRI_QUINTIC ; }
     else if( Hqhard && !Hduplo ){ qmode2 = MRI_QUINTIC ; }
     if( xwid < NGMIN_Q || ywid < NGMIN_Q || zwid < NGMIN_Q )  /* 28 Oct 2015 */
       qmode = qmode2 = MRI_CUBIC ;
#endif

     (void)IW3D_load_energy(Haawarp) ;  /* initialize energy field for penalty use */

     nlevr = WORKHARD(lev)  ? 2 : 1 ;
     nsup  = SUPERHARD(lev) ? 2 : 1 ;

     /* announce the start of a new level! */

     PBLUR_BASE  (1,xwid,1,ywid,1,zwid) ;  /* progressive blur, if ordered */
     PBLUR_SOURCE(1,xwid,1,ywid,1,zwid) ;
     if( Hpblur_b > 0.0f || Hpblur_b > 0.0f ) Hfirsttime = 1 ;
     mri_free(Haasrcim_plus) ;  /* re-create the warped */
     mri_free(Haabasim_minus);  /* source and base images */
     Haasrcim_plus  = IW3D_warp_floatim( Haawarp, SRCIM, Himeth ,  1.0f ) ;
     Haabasim_minus = IW3D_warp_floatim( Haawarp, BASIM, Himeth , -1.0f ) ;

     if( Hverb > 1 )
       ININFO_message("  ........ +-lev=%d xwid=%d ywid=%d zwid=%d Hfac=%g penfac=%g %s %s" ,
                      lev,xwid,ywid,zwid,Hfactor,Hpen_fff , (levdone   ? "FINAL"  : "\0") ,
                                                   (nlevr > 1 ? "WORKHARD" : "\0") ) ;
     else if( Hverb == 1 )
       fprintf(stderr,"lev=%d patch=%dx%dx%d: ",lev,xwid,ywid,zwid) ;

     /* alternate the direction of sweeping at different levels */

     if( lev%2 == 1 || nlevr > 1 ){  /* bot to top, ijk */
           if( do_qfinal ) BALLOPT ;
      else if( nlevr > 1 ) BOXOPT ;
      for( isup=0 ; isup < nsup ; isup++ ){  /* superhard? */
       for( kdon=0,kbot=kbbb ; !kdon ; kbot += dkkk ){
         ktop = kbot+zwid-1;
              if( ktop >= kttt )       { ktop = kttt; kbot = ktop+1-zwid; kdon=1; }
         else if( ktop >= kttt-zwid/4 ){ ktop = kttt; kdon=1; }
         for( jdon=0,jbot=jbbb ; !jdon ; jbot += djjj ){
           jtop = jbot+ywid-1;
                if( jtop >= jttt        ){ jtop = jttt; jbot = jtop+1-ywid; jdon=1; }
           else if( jtop >= jttt-ywid/4 ){ jtop = jttt; jdon=1; }
           for( idon=0,ibot=ibbb ; !idon ; ibot += diii ){
             itop = ibot+xwid-1;
                  if( itop >= ittt        ){ itop = ittt; ibot = itop+1-xwid; idon=1; }
             else if( itop >= ittt-xwid/4 ){ itop = ittt; idon=1; }
             Hcostold = Hcost ;
             iter = IW3D_improve_warp_plusminus( qmode  , ibot,itop , jbot,jtop , kbot,ktop ) ;
             if( Hquitting ) goto DoneDoneDone ;  /* signal to quit was sent */
#if 0
             if( Hcost > Hcostold+0.001f ){
               if( Hverb > 1 ) ININFO_message(" -- rerun --") ;
               iter = IW3D_improve_warp_plusminus( MRI_CUBIC  , ibot,itop , jbot,jtop , kbot,ktop ) ;
             }
#if 0
             else if( iter > 144 && qmode > 0 && Hcostold-Hcost > 0.00002f )
               iter = IW3D_improve_warp_plusminus( qmode    , ibot,itop , jbot,jtop , kbot,ktop ) ;
#endif
#endif
             if( Hcost < Hstopcost ){
               if( Hverb == 1 ) fprintf(stderr,"\n") ;
               ININFO_message("  ######### cost has reached stopping value") ;
               goto DoneDoneDone ;
             }
           }
         }
       }
      } /* isup loop */
       Hcostmid = Hcostend = Hcost ;
     }

     if( lev%2 == 0 || nlevr > 1 ){ /* top to bot, kji */
       if( nlevr > 1 && Hverb == 1 ) fprintf(stderr,":[cost=%.5f]:",Hcost) ;
       if( do_qfinal || nlevr > 1 ) BALLOPT ;
       qmodeX = (nlevr > 1) ? qmode2 : qmode ;
      for( isup=0 ; isup < nsup ; isup++ ){  /* superhard? */
       for( idon=0,itop=ittt ; !idon ; itop -= diii ){
         ibot = itop+1-xwid;
              if( ibot <= ibbb        ){ ibot = ibbb; itop = ibot+xwid-1; idon=1; }
         else if( ibot <= ibbb+xwid/4 ){ ibot = ibbb; idon=1; }
         for( jdon=0,jtop=jttt ; !jdon ; jtop -= djjj ){
           jbot = jtop+1-ywid;
                if( jbot <= jbbb        ){ jbot = jbbb; jtop = jbot+ywid-1; jdon=1; }
           else if( jbot <= jbbb+ywid/4 ){ jbot = jbbb; jdon=1; }
           for( kdon=0,ktop=kttt ; !kdon ; ktop -= dkkk ){
             kbot = ktop+1-zwid;
                  if( kbot <= kbbb        ){ kbot = kbbb; ktop = kbot+zwid-1; kdon=1; }
             else if( kbot <= kbbb+zwid/4 ){ kbot = kbbb; kdon=1; }
             Hcostold = Hcost ;
             iter = IW3D_improve_warp_plusminus( qmodeX  , ibot,itop , jbot,jtop , kbot,ktop ) ;
             if( Hquitting ) goto DoneDoneDone ;  /* signal to quit was sent */
#if 0
             if( Hcost > Hcostold+0.001f ){
               if( Hverb > 1 ) ININFO_message(" -- rerun --") ;
               iter = IW3D_improve_warp_plusminus( MRI_CUBIC  , ibot,itop , jbot,jtop , kbot,ktop ) ;
             }
#if 0
             else if( iter > 144 && qmodeX > 0 && Hcostold-Hcost > 0.00002f )
               iter = IW3D_improve_warp_plusminus( qmodeX    , ibot,itop , jbot,jtop , kbot,ktop ) ;
#endif
#endif
              if( Hcost < Hstopcost ){
                if( Hverb == 1 ) fprintf(stderr,"\n") ;
                ININFO_message("  ######### cost has reached stopping value") ;
                goto DoneDoneDone ;
              }
           }
         }
       }
      } /* isup loop */
       Hcostend = Hcost ;
     }

     if( Hverb == 1 ) fprintf(stderr," done [cost=%.5f]\n",Hcost) ;

     if( Hsave_allwarps ){
       sprintf(warplab,"Lev%d.%04dx%04dx%04d",lev,xwid,ywid,zwid) ;
       HSAVE_ADDTO(Haawarp,warplab) ;
     }

   } /*-- end of loop over levels of refinement --*/

DoneDoneDone:  /* breakout */

   OutWarp = IW3D_copy( Haawarp , 1.0f ) ;
   IW3D_cleanup_improvement_plusminus() ;
#ifdef USE_PLUSMINUS_INITIALWARP
   if( myIwarp ){ IW3D_destroy(WO_iwarp) ; WO_iwarp = NULL ; }
#endif

   RETURN(OutWarp) ;
}

/*----------------------------------------------------------------------------*/

Image_plus_Warp ** IW3D_warp_s2bim_plusminus(
                                    MRI_IMAGE *bim , MRI_IMAGE *wbim , MRI_IMAGE *sim,
                                    int interp_code , int meth_code , int warp_flags  )
{
   IndexWarp3D *Swarp ;
   Image_plus_Warp *pww , *mww , **sbww ;

ENTRY("IW3D_warp_s2bim_plusminus") ;

   WO_iwarp = S2BIM_iwarp ; Hlev_start = S2BIM_ilev ; Hlev_end = S2BIM_mlev ;
   Hnpar_sum = 0 ; Hduplo = 0 ;

   Hshrink = AFNI_numenv("AFNI_WARPOMATIC_SHRINK") ;
   if( Hshrink > 1.0f                       ) Hshrink = 1.0f / Hshrink ;
   if( Hshrink < 0.444f || Hshrink > 0.888f ) Hshrink = 0.749999f ;
   else                                       ININFO_message("  -- Hshrink set to %.6f",Hshrink) ;

   Swarp = IW3D_warpomatic_plusminus( bim , wbim , sim , meth_code , warp_flags ) ;

   pww       = (Image_plus_Warp *)malloc(sizeof(Image_plus_Warp)) ;
   pww->im   = IW3D_warp_floatim( Swarp, sim , interp_code , 1.0f ) ;
   pww->warp = Swarp ;

   mww       = (Image_plus_Warp *)malloc(sizeof(Image_plus_Warp)) ;
   mww->warp = IW3D_copy( Swarp , -1.0f ) ;
   mww->im   = IW3D_warp_floatim( Swarp, bim , interp_code , -1.0f ) ;

   sbww = (Image_plus_Warp **)malloc(sizeof(Image_plus_Warp *)*2) ;
   sbww[0] = pww ; sbww[1] = mww ;

   RETURN(sbww) ;
}

#if 0
/*----------------------------------------------------------------------------*/
/* Function to compute 'normal' 3dQwarp result from the plusminus warps.
   From the 3dQwarp help:
      Define Wp(x) = x+dis(x) and Wm(x) = x-dis(x). Then since
      base(Wm(x)) matches source(Wp(x)), by substituting INV(Wm(x))
      wherever we see x, we have base(x) matches source(Wp(INV(Wm(x))));
      that is, the warp V(x) that one would get from the 'usual' way
      of running 3dQwarp is V(x) = Wp(INV(Wm(x))).
   The code to do this is in 3dQWarp.c ('-pmBASE' option) and so this
   function is not actually used anywhere.
*//*--------------------------------------------------------------------------*/

IndexWarp3D * IW3D_plusminus_to_direct( IndexWarp3D *pww )
{
   IndexWarp3D *mww , *imww ;

   if( pww == NULL ) return NULL ;

    mww = IW3D_copy( pww , -1.0f ) ;
   imww = IW3D_invert (  mww , NULL , MRI_WSINC5 ) ;
    mww = IW3D_compose( imww , pww  , MRI_WSINC5 ) ;

   IW3D_destroy(imww) ;
   return mww ;
}
#endif

#endif /* ALLOW_PLUSMINUS */
/*****--------------------------------------------------------------------*****/
/*****||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||*****/
/*****--------------------------------------------------------------------*****/

#endif /*(Q11)*/ /*###########################################################*/

#endif /* ALLOW_QWARP */
/******************************************************************************/
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
/******************************************************************************/
