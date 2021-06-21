#ifndef _MRI_NWARP_HEADER_
#define _MRI_NWARP_HEADER_

/********** This file defines stuff for nonlinear warping
            in AFNIland, and is intended to be included into mrilib.h *********/

#ifdef  __cplusplus
extern "C" {
#endif

/*----------------------------------------------------------------------------*/
/* for mri_nwarp.c */

 /* The basic struct for a nonlinear warp in index space,
    with hooks to allow it to be converted to a coordinate space 3D dataset */

 /*----------
    April 2021 - Significant changes:
    Modify warp struct so that the grid displacements are now stored in
    two parts:
      little wiggly stuff (xd,yd,zd),
      affine transformation for long-range stuff/points outside grid (amat)
    The mapping of a point at index (i,j,k) is to index
           [ i + xd(i,j,k) ]
       [M] [ j + yd(i,j,k) ] + [c]
           [ k + zd(i,j,k) ]
    where [amat] is the catenation [M c] of 3x3 matrix M and 3-vector c.

    The intention is that the "pure" index warp of [i,j,k] + [xd,yd,zd]
    is bounded to the 3D grid, and for (i,j,k) outside of the grid, the
    warp transformation is purely affine [M][i,j,k]+[c]. In the past,
    [M] was implicitly the identity and [c] was zero -- that is, all the
    displacements were carried by [xd,yd,zd], which lead to various
    practical problems brought out by DRG and PAT (those devils). ----------*/

typedef struct {
  int    nx ,  ny ,  nz ;  /* Grid dimensions */
  float *xd , *yd , *zd ;  /* Displacments (in index space) */
  float *hv , *je , *se ;  /* Various auxiliary volumes */
  int   use_amat ;         /* Whether to use amat */
  mat44 amat , amati ;     /* Affine component of warp (and its inverse) */
   /*- stuff below here is for conversion to/from 3D dataset format -*/
  mat44 cmat , imat ;      /* cmat: i->x ; imat: x->i */
  char *geomstring ;       /* grid geometry of 3D dataset */
  int view ;               /* view/space of the dataset */
} IndexWarp3D ;

 /* an array of nonlinear warps [Unused at this time] */

typedef struct {
  int nwarp ;          /* number of them */
  IndexWarp3D **warp ;
} IndexWarp3DArray ;

 /* an image plus a warp [Used in 3dQwarp = warped image plus the warp] */

typedef struct {
  MRI_IMAGE *im ;
  IndexWarp3D *warp ;
} Image_plus_Warp ;

 /* a warp and its inverse, packaged together for fun */

typedef struct {
  IndexWarp3D *fwarp ;   /* forward warp */
  IndexWarp3D *iwarp ;   /* inverse warp */
} IndexWarp3D_pair ;

 /* a 4x4 matrix and its inverse [Unused] */

typedef struct {
  mat44 fwarp ;
  mat44 iwarp ;
} mat44_pair ;

 /* a collection of 4x4 matrices (e.g., time dependent rotations) */

typedef struct { /* 17 Oct 2014 */
  int   nmar ;
  char  fname[128] ;
  mat44 *mar ;
} mat44_vec ;

 /* get the iii-th mat44 element from a mat44_vec struct */

#define M44V_mat(mmm,iii) ( ((iii) < (mmm)->nmar) ? (mmm)->mar[iii]             \
                                                  : (mmm)->mar[(mmm)->nmar-1] )

 /* kill a mat44_vec struct (all of it, Frank) */

#define DESTROY_mat44_vec(mv)                  \
 do{ if( (mv)->mar != NULL ) free((mv)->mar) ; \
     free(mv) ;                                \
 } while(0) ;

 /* list of warps to catenate, some nonlinear (nwarp) and some affine (awarp) */

typedef struct { /* 17 Oct 2014 */
  int ncat , nvar , flags ;
  THD_3dim_dataset **nwarp ;
  mat44_vec        **awarp ;
  char              *actual_geomstring ;
  char              *master_geomstring ;
  mat44              actual_cmat , actual_imat ;
  int              xpad  ,ypad  ,zpad ;
  float            xshift,yshift,zshift ;
} Nwarp_catlist ;

#define NWC_INVERT_MASK 1  /* for flags field above, when catenating warps */

 /* macros to get components from a Nwarp_catlist struct */

#define NWC_nwarp(nnn,iii) ( ((nnn)->nwarp != NULL) ? (nnn)->nwarp[iii] : NULL )
#define NWC_awarp(nnn,iii) ( ((nnn)->awarp != NULL) ? (nnn)->awarp[iii] : NULL )
#define NWC_null(nnn,iii)  ( NWC_nwarp(nnn,iii)==NULL && NWC_awarp(nnn,iii)==NULL )

 /* prototypes for warping utilities and functions */

extern THD_3dim_dataset * IW3D_from_nwarp_catlist( Nwarp_catlist * , int ) ;
extern void IW3D_destroy_nwarp_catlist( Nwarp_catlist * ) ;
extern int IW3D_reduce_nwarp_catlist( Nwarp_catlist * ) ;
extern Nwarp_catlist * IW3D_read_nwarp_catlist( char * ) ;
extern void THD_set_nwarp_apply_prefix( char *ppp ) ; /* 15 Mar 2021 */

extern IndexWarp3D * IW3D_create( int nx , int ny , int nz ) ;
extern void IW3D_destroy( IndexWarp3D *AA ) ;
extern float IW3D_normL1  ( IndexWarp3D *AA , IndexWarp3D *BB ) ;
extern float IW3D_normL2  ( IndexWarp3D *AA , IndexWarp3D *BB ) ;
extern float IW3D_normLinf( IndexWarp3D *AA , IndexWarp3D *BB ) ;
extern int_sextet IW3D_warpbox( IndexWarp3D *AA , float fac , float dthr ) ; /* 18 Mar 2021 */
extern IndexWarp3D * IW3D_empty_copy( IndexWarp3D *AA ) ;
extern IndexWarp3D * IW3D_copy( IndexWarp3D *AA , float fac ) ;
extern IndexWarp3D * IW3D_sum( IndexWarp3D *AA, float Afac, IndexWarp3D *BB, float Bfac ) ;
extern void IW3D_scale( IndexWarp3D *AA , float fac ) ;
extern IndexWarp3D * IW3D_from_dataset( THD_3dim_dataset *dset , int empty , int ivs ) ;
extern THD_3dim_dataset * IW3D_to_dataset( IndexWarp3D *AA , char *prefix ) ;
extern float IW3D_load_hexvol( IndexWarp3D *AA , float *hv ) ;
extern float IW3D_load_energy( IndexWarp3D *AA ) ;
extern void IW3D_load_bsv( IndexWarp3D *AA , float,float,float, float *bb , float *ss , float *vv ) ;
extern IndexWarp3D * IW3D_compose( IndexWarp3D *AA , IndexWarp3D *BB     , int icode ) ;
extern IndexWarp3D * IW3D_invert ( IndexWarp3D *AA , IndexWarp3D *BBinit , int icode ) ;
extern IndexWarp3D * IW3D_sqrtinv( IndexWarp3D *AA , int icode ) ;
extern IndexWarp3D * IW3D_sqrt   ( IndexWarp3D *AA , int icode ) ;
extern IndexWarp3D * IW3D_from_poly( int npar, float *par, IndexWarp3D *WW ) ;
extern THD_3dim_dataset * NwarpCalcRPN( char *expr, char *prefix, int icode, int acode ) ;
extern void NwarpCalcRPN_verb(int i) ;

extern void IW3D_bounding_box_clear( IndexWarp3D *AA , float thresh ) ; /* 29 Mar 2021 */
extern int_sextet IW3D_bounding_box( IndexWarp3D *AA , float thresh ) ;

extern void THD_interp_floatim( MRI_IMAGE *fim ,
                                int np , float *ip , float *jp , float *kp ,
                                int code, float *outar ) ;
extern void THD_interp_complexim( MRI_IMAGE *fim ,
                                  int np , float *ip , float *jp , float *kp ,
                                  int code, complex *outar ) ; /* 27 Mar 2018 */
extern MRI_IMARR * THD_setup_nwarp( MRI_IMARR *bimar,
                                    int use_amat    , mat44 amat ,
                                    mat44 cmat_bim  ,
                                    int incode      , float wfac ,
                                    mat44 cmat_src  ,
                                    mat44 cmat_out  ,
                                    int nx_out      , int ny_out , int nz_out  ) ;
extern THD_3dim_dataset * THD_nwarp_dataset( THD_3dim_dataset *dset_nwarp ,
                                             THD_3dim_dataset *dset_src  ,
                                             THD_3dim_dataset *dset_mast ,
                                             char *prefix , int wincode , int dincode ,
                                             float dxyz_mast , float wfac , int nvlim ,
                                             MRI_IMAGE *amatim ) ;

extern THD_3dim_dataset * THD_nwarp_dataset_NEW( Nwarp_catlist    *nwc       ,
                                                 THD_3dim_dataset *dset_src  ,
                                                 THD_3dim_dataset *dset_mast ,
                                                 char *prefix, int wincode, int dincode,
                                                 float dxyz_mast, float wfac, int nvlim ) ;

extern int THD_nwarp_forward_xyz( THD_3dim_dataset *dset_nwarp ,
                                  float dfac , int npt ,
                                  float *xin , float *yin , float *zin ,
                                  float *xut , float *yut , float *zut  ) ;

extern int THD_nwarp_inverse_xyz( THD_3dim_dataset *dset_nwarp ,
                                  float dfac , int npt ,
                                  float *xin , float *yin , float *zin ,
                                  float *xut , float *yut , float *zut  ) ;
/*----------------------------------------------------------------------------*/

#ifdef  __cplusplus
}
#endif

#endif /* _MCW_MRILIB_HEADER_ */
