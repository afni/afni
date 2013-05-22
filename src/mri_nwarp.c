#include "mrilib.h"
#include "r_new_resam_dset.h"

#ifdef USE_OMP
# include <omp.h>
# define NUM_DHARRAY 6
  static int    nthmax=1 ;
  static double *dhaar=NULL ;
  static double *dhbbr=NULL ;
  static double *dhccr=NULL ;
  static double *dhddr=NULL ;
  static double *dheer=NULL ;
  static double *dhffr=NULL ;
#else
# define nthmax 1
  static double dhaar[1] ;
  static double dhbbr[1] ;
  static double dhccr[1] ;
  static double dhddr[1] ;
  static double dheer[1] ;
  static double dhffr[1] ;
#endif

#include "thd_incorrelate.c"

/*---------------------------------------------------------------------------*/

#undef  FREEIFNN
#define FREEIFNN(x) do{ if((x)!=NULL){ free((void *)(x)); (x)=NULL;} } while(0)

static int verb_nww=0 ;
void NwarpCalcRPN_verb(int i){ verb_nww = i; }

#undef  NGMIN
#define NGMIN 9             /* minimum num grid points in a given direction */

#undef  FSUB
#define FSUB(far,i,j,k,ni,nij) far[(i)+(j)*(ni)+(k)*(nij)]

/*----------------------------------------------------------------------------*/

void IW3D_set_emat_from_xyzmat44( IndexWarp3D *AA , mat44 amm )
{
  mat44 cmat , imat , tmat , smat ;

  if( AA == NULL ) return ;
  cmat = AA->cmat ; imat = AA->imat ;
  smat = MAT44_MUL( amm  , cmat ) ;
  tmat = MAT44_MUL( imat , smat ) ;
  MAT44_TO_MAT33( tmat , AA->emat ) ;
  AA->emat.m[0][0] -= 1.0f ;
  AA->emat.m[1][1] -= 1.0f ;
  AA->emat.m[2][2] -= 1.0f ;
  AA->use_emat = ( NORM_MAT33(AA->emat) > 0.001f ) ;
  return ;
}

/*----------------------------------------------------------------------------*/

mat33 IW3D_compute_inverse_emat( mat33 emm )
{
  mat33 imm , qmm ;
  qmm = emm ; qmm.m[0][0] += 1.0f ; qmm.m[1][1] += 1.0f ; qmm.m[2][2] += 1.0f ;
  imm = MAT33_INV(qmm) ;
              imm.m[0][0] -= 1.0f ; imm.m[1][1] -= 1.0f ; imm.m[2][2] -= 1.0f ;
  return imm ;
}

/*----------------------------------------------------------------------------*/

void IW3D_set_emat_raw( IndexWarp3D *AA , mat33 emm )
{
   if( AA != NULL ){ AA->emat = emm ; AA->use_emat = ! ISZERO_MAT33(emm) ; }
   return ;
}

/*----------------------------------------------------------------------------*/

void IW3D_clear_emat( IndexWarp3D *AA )
{
   if( AA != NULL ){ LOAD_ZERO_MAT33(AA->emat) ; AA->use_emat = 0 ; }
   return ;
}

/*---------------------------------------------------------------------------*/
/* Creation ex nihilo! */

IndexWarp3D * IW3D_create( int nx , int ny , int nz )
{
   IndexWarp3D *AA ;

   if( nx < NGMIN && ny < NGMIN && nz < NGMIN ) return NULL ;

   AA = (IndexWarp3D *)calloc(1,sizeof(IndexWarp3D)) ;
   AA->nx = nx ; AA->ny = ny ; AA->nz = nz ;
   AA->xd = (float *)calloc(nx*ny*nz,sizeof(float)) ;
   AA->yd = (float *)calloc(nx*ny*nz,sizeof(float)) ;
   AA->zd = (float *)calloc(nx*ny*nz,sizeof(float)) ;
   AA->hv = NULL ;  /* to be filled in later, maybe */
   AA->je = NULL ;  /* to be filled in later, maybe */
   AA->se = NULL ;  /* to be filled in later, maybe */
   LOAD_IDENT_MAT44(AA->cmat) ;
   LOAD_IDENT_MAT44(AA->imat) ;
   IW3D_clear_emat(AA) ;
   AA->geomstring = NULL ;
   AA->view = VIEW_ORIGINAL_TYPE ;

   return AA ;
}

/*---------------------------------------------------------------------------*/

void IW3D_zero_fill( IndexWarp3D *AA )
{
   size_t nbyt  ;

   if( AA == NULL ) return ;
   nbyt = sizeof(float) * AA->nx * AA->ny * AA->nz ;
   if( AA->xd != NULL ) AAmemset( AA->xd , 0 , nbyt ) ;
   if( AA->yd != NULL ) AAmemset( AA->yd , 0 , nbyt ) ;
   if( AA->zd != NULL ) AAmemset( AA->zd , 0 , nbyt ) ;
   if( AA->hv != NULL ) AAmemset( AA->hv , 0 , nbyt ) ;
   if( AA->je != NULL ) AAmemset( AA->je , 0 , nbyt ) ;
   if( AA->se != NULL ) AAmemset( AA->se , 0 , nbyt ) ;
   return ;
}

/*---------------------------------------------------------------------------*/

IndexWarp3D_pair * IW3D_pair_insert( IndexWarp3D *AA , IndexWarp3D *BB )
{
   IndexWarp3D_pair *PP ;

   PP = (IndexWarp3D_pair *)malloc(sizeof(IndexWarp3D_pair)) ;
   PP->fwarp = AA ;
   PP->iwarp = BB ;
   return PP ;
}

/*---------------------------------------------------------------------------*/

void IW3D_pair_invertify( IndexWarp3D_pair *PP )
{
   if( PP == NULL || PP->fwarp == NULL ) return ;
   IW3D_destroy( PP->iwarp ) ;
   PP->iwarp = IW3D_invert( PP->fwarp , NULL , MRI_QUINTIC ) ;
   return ;
}

/*---------------------------------------------------------------------------*/

void IW3D_pair_swapify( IndexWarp3D_pair *PP )
{
   IndexWarp3D *AA ;
   if( PP == NULL ) return ;
   AA = PP->fwarp ; PP->fwarp = PP->iwarp ; PP->iwarp = AA ;
   return ;
}

/*---------------------------------------------------------------------------*/

IndexWarp3D * IW3D_create_vacant( int nx , int ny , int nz )
{
   IndexWarp3D *AA ;

   if( nx < NGMIN && ny < NGMIN && nz < NGMIN ) return NULL ;

   AA = (IndexWarp3D *)calloc(1,sizeof(IndexWarp3D)) ;
   AA->nx = nx ; AA->ny = ny ; AA->nz = nz ;
   AA->xd = NULL ; AA->yd = NULL ; AA->zd = NULL ;
   AA->hv = NULL ; AA->je = NULL ; AA->se = NULL ;
   LOAD_IDENT_MAT44(AA->cmat) ;
   LOAD_IDENT_MAT44(AA->imat) ;
   IW3D_clear_emat(AA) ;
   AA->geomstring = NULL ;
   AA->view = VIEW_ORIGINAL_TYPE ;

   return AA ;
}

/*---------------------------------------------------------------------------*/
/* Into the valley of death! */

void IW3D_destroy( IndexWarp3D *AA )
{
   if( AA != NULL ){
     FREEIFNN(AA->xd); FREEIFNN(AA->yd); FREEIFNN(AA->zd);
     FREEIFNN(AA->hv); FREEIFNN(AA->je); FREEIFNN(AA->se);
     FREEIFNN(AA->geomstring) ;
     free(AA);
   }
   return ;
}

/*---------------------------------------------------------------------------*/

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
/* If BB == NULL, just the norm of AA.  Otherwise, the norm of AA-BB. */

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
/* If BB == NULL, just the norm of AA.  Otherwise, the norm of AA-BB. */

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
/* If BB == NULL, just the norm of AA.  Otherwise, the norm of AA-BB. */

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
/* Same setup as IW3D_copy(), but contents are zero. */

IndexWarp3D * IW3D_empty_copy( IndexWarp3D *AA )
{
   IndexWarp3D *BB ; int nxyz ;

   if( AA == NULL ) return NULL ;

   BB = IW3D_create( AA->nx , AA->ny , AA->nz ) ;

   BB->cmat = AA->cmat ; BB->imat = AA->imat ;
   BB->emat = AA->emat ; BB->use_emat = AA->use_emat ;

   if( AA->geomstring != NULL )
     BB->geomstring = strdup(AA->geomstring) ;

   BB->view = AA->view ;

   return BB ;
}

/*---------------------------------------------------------------------------*/
/* Make a copy, scaling displacements by fac. */

IndexWarp3D * IW3D_copy( IndexWarp3D *AA , float fac )
{
   IndexWarp3D *BB ; int nxyz ;

   if( AA == NULL ) return NULL ;

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
   MAT33_SCALE(BB->emat,fac) ;

   return BB ;
}

/*---------------------------------------------------------------------------*/

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
/* Scale displacements by fac (in-place). */

void IW3D_scale( IndexWarp3D *AA , float fac )
{
   int nxyz , qq ;

   if( AA == NULL || fac == 1.0f ) return ;

   nxyz = AA->nx * AA->ny * AA->nz ;

   for( qq=0 ; qq < nxyz ; qq++ ){
     AA->xd[qq] *= fac ;
     AA->yd[qq] *= fac ;
     AA->zd[qq] *= fac ;
   }
   MAT33_SCALE(AA->emat,fac) ;

   return ;
}

/*---------------------------------------------------------------------------*/
/* Sum displacements, with factors. */

IndexWarp3D * IW3D_sum( IndexWarp3D *AA, float Afac, IndexWarp3D *BB, float Bfac )
{
   IndexWarp3D *CC ; int nxyz , qq ;

   if( AA == NULL && BB == NULL ) return NULL ;

   if( AA == NULL || Afac == 0.0f ){
     CC = IW3D_copy( BB , Bfac ) ; return CC ;
   } else if( BB == NULL || Bfac == 0.0f ){
     CC = IW3D_copy( AA , Afac ) ; return CC ;
   }

   nxyz = AA->nx * AA->ny * AA->nz ;

   if( BB->nx * BB->ny * BB->nz != nxyz ) return NULL ;

   CC = IW3D_empty_copy(AA) ;  /* all zero displacements */

   for( qq=0 ; qq < nxyz ; qq++ ){
     CC->xd[qq] = Afac * AA->xd[qq] + Bfac * BB->xd[qq] ;
     CC->yd[qq] = Afac * AA->yd[qq] + Bfac * BB->yd[qq] ;
     CC->zd[qq] = Afac * AA->zd[qq] + Bfac * BB->zd[qq] ;
   }
   CC->emat = MAT33_SUM( AA->emat,Afac , BB->emat,Bfac ) ;
   CC->use_emat = (AA->use_emat || BB->use_emat) && (NORM_MAT33(CC->emat) > 0.001f) ;
   if( !CC->use_emat ) IW3D_clear_emat(CC) ;

   return CC ;
}

#if 0
/*----------------------------------------------------------------------------*/
/* smooth locally */

#define M7  0.142857143f
#define M28 0.035714286f
#define M84 0.011904762f

void IW3D_7smooth( IndexWarp3D *AA )
{
}
/*----------------------------------------------------------------------------*/
#endif

/*----------------------------------------------------------------------------*/
/* Make the geometry fields of an index warp match that of a dataset. */

void IW3D_adopt_dataset( IndexWarp3D *AA , THD_3dim_dataset *dset )
{
   mat44 cmat , imat ; char *gstr ;

   if( AA == NULL || !ISVALID_DSET(dset) ) return ;

   if( DSET_NX(dset) != AA->nx || DSET_NY(dset) != AA->ny || DSET_NZ(dset) != AA->nz ){
     ERROR_message("IW3D_adopt_dataset: grid mismatch") ; return ;
   }

   if( !ISVALID_MAT44(dset->daxes->ijk_to_dicom) )
     THD_daxes_to_mat44(dset->daxes) ;

   cmat = dset->daxes->ijk_to_dicom ;  /* takes ijk to xyz */
   imat = MAT44_INV(cmat) ;            /* takes xyz to ijk */

   AA->cmat = cmat ; AA->imat = imat ;
   gstr = EDIT_get_geometry_string(dset) ;
   if( gstr != NULL ) AA->geomstring = strdup(gstr) ;
   AA->view = dset->view_type ;

   return ;
}

/*----------------------------------------------------------------------------*/
/* Convert a 3D dataset of displacments in mm to an index warp.
     empty != 0 ==> displacements will be all zero
     ivs   != 0 ==> extract sub-bricks [ivs..ivs+2] for the displacments
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

   if( !empty ){
     if( DSET_NVALS(dset) < 3+ivs ) RETURN(NULL) ;
     if( !DSET_LOADED(dset) ){
       DSET_load(dset) ; if( !DSET_LOADED(dset) ) RETURN(NULL) ;
     }
   }

   if( !ISVALID_MAT44(dset->daxes->ijk_to_dicom) )
     THD_daxes_to_mat44(dset->daxes) ;

   cmat = dset->daxes->ijk_to_dicom ;  /* takes ijk to xyz */
   imat = MAT44_INV(cmat) ;            /* takes xyz to ijk */

   nx = DSET_NX(dset); ny = DSET_NY(dset); nz = DSET_NZ(dset); nxyz = nx*ny*nz;

   AA = IW3D_create(nx,ny,nz) ;

   AA->cmat = cmat ; AA->imat = imat ;
   gstr = EDIT_get_geometry_string(dset) ;
   if( gstr != NULL ) AA->geomstring = strdup(gstr) ;

   AA->view = dset->view_type ;

   if( !empty ){
     xda = AA->xd ; yda = AA->yd ; zda = AA->zd ;
     xim = THD_extract_float_brick(ivs+0,dset) ; xar = MRI_FLOAT_PTR(xim) ;
     yim = THD_extract_float_brick(ivs+1,dset) ; yar = MRI_FLOAT_PTR(yim) ;
     zim = THD_extract_float_brick(ivs+2,dset) ; zar = MRI_FLOAT_PTR(zim) ;
     DSET_unload(dset) ;

 AFNI_OMP_START ;
#pragma omp parallel if( nxyz > 6666 )
 { int ii ;
#pragma omp for
     for( ii=0 ; ii < nxyz ; ii++ ){  /* convert mm to index displacements */
       MAT33_VEC( imat , xar[ii],yar[ii],zar[ii] , xda[ii],yda[ii],zda[ii] ) ;
     }
 }
 AFNI_OMP_END ;

     mri_free(zim) ; mri_free(yim) ; mri_free(xim) ;
   }

#if 0
   { ATR_float *atr ;
     atr = THD_find_float_atr( dset->dblk , "NWARP_EMAT33") ;
     if( atr != NULL && atr->nfl >= 9 ){
       float *matar = atr->fl ; mat33 emat ;
       LOAD_MAT33( emat, matar[0], matar[1], matar[2],
                         matar[3], matar[4], matar[5],
                         matar[6], matar[7], matar[8] ) ;
       IW3D_set_emat_xyz( AA , emat ) ;
     }
   }
#endif

   RETURN(AA) ;
}

/*----------------------------------------------------------------------------*/

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

/*----------------------------------------------------------------------------*/
/* Convert an index warp to a 3D dataset of spatial displacmements in mm */

static int save_aux_volumes = 0 ;

THD_3dim_dataset * IW3D_to_dataset( IndexWarp3D *AA , char *prefix )
{
   THD_3dim_dataset *dset ;
   float *xar,*yar,*zar,*har,*jar,*sar , *xda,*yda,*zda,*hva,*jea,*sea , hfac ;
   mat44 cmat , imat ;
   int ii , nxyz ;

ENTRY("IW3D_to_dataset") ;

   if( AA == NULL ) RETURN(NULL) ;

   if( AA->geomstring == NULL ){
     char *gstr = EDIT_imat_to_geometry_string(AA->imat,AA->nx,AA->ny,AA->nz) ;
     if( gstr == NULL ) RETURN(NULL) ;  /* should not transpire */
     AA->geomstring = strdup(gstr) ;
   }

STATUS("create dataset") ;
   dset = EDIT_geometry_constructor( AA->geomstring , prefix ) ;

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

   xda = AA->xd ; yda = AA->yd ; zda = AA->zd ;
   nxyz = AA->nx * AA->ny * AA->nz ;
   cmat = AA->cmat ; imat = AA->imat ;

   xar = (float *)malloc(sizeof(float)*nxyz) ;
   yar = (float *)malloc(sizeof(float)*nxyz) ;
   zar = (float *)malloc(sizeof(float)*nxyz) ;
   if( save_aux_volumes ){
     har = (float *)malloc(sizeof(float)*nxyz) ;
     jar = (float *)malloc(sizeof(float)*nxyz) ;
     sar = (float *)malloc(sizeof(float)*nxyz) ;

STATUS("load hexvol") ;
     (void)IW3D_load_hexvol(AA) ;
STATUS("load energy") ;
     (void)IW3D_load_energy(AA) ;
STATUS("done with aux volumes") ;
     jea = AA->je ; sea  = AA->se ;
   } else {
     har = jar = sar = jea = sea = NULL ;
   }
   hva = AA->hv ; hfac = MAT44_DET(cmat) ; hfac = fabsf(hfac) ;

STATUS("transform to displacements in mm") ;
 AFNI_OMP_START ;
#pragma omp parallel if( nxyz > 6666 )
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

   if( AA->use_emat && ! ISZERO_MAT33(AA->emat) ){
     mat33 dmat,smat,tmat ; float matar[9] ;
STATUS("setup emat") ;
     MAT44_TO_MAT33(cmat,tmat) ; smat = MAT33_MUL( tmat , AA->emat ) ;
     MAT44_TO_MAT33(imat,tmat) ; dmat = MAT33_MUL( smat , tmat ) ;
     UNLOAD_MAT33_AR(dmat,matar) ;
     THD_set_float_atr( dset->dblk , "NWARP_EMAT33" , 9 , matar ) ;
   }

STATUS("done") ;
   RETURN(dset) ;
}

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

#undef  DA
#undef  DB
#undef  DC
#define DA(p,q) (p.a-q.a)
#define DB(p,q) (p.b-q.b)
#define DC(p,q) (p.c-q.c)

#undef  TRIPROD
#define TRIPROD(ax,ay,az,bx,by,bz,cx,cy,cz) ( (ax)*((by)*(cz)-(bz)*(cy)) \
                                             +(bx)*((cy)*(az)-(cz)*(ay)) \
                                             +(cx)*((ay)*(bz)-(az)*(by))  )

#undef  IJK
#define IJK(i,j,k) ((i)+(j)*nx+(k)*nxy)

#undef  C2F
#define C2F(p,q,r,xx) ( (xx).a = (p) , (xx).b = (q) , (xx).c = (r) )

#undef  D2F
#define D2F(pqr,xx)   ( (xx).a+=xda[pqr], (xx).b+=yda[pqr], (xx).c+=zda[pqr] )

#undef  E2F
#define E2F(pqr,xx)   ( (xx).a =xda[pqr], (xx).b =yda[pqr], (xx).c =zda[pqr] )

/*----------------------------------------------------------------------------*/
/* Compute the bulk and shear energies from DISPLACEMENTS at corners.
   Loosely based on http://en.wikipedia.org/wiki/Neo-Hookean_solid    */

static INLINE float_pair hexahedron_energy( float_triple d000 , float_triple d100 ,
                                            float_triple d010 , float_triple d110 ,
                                            float_triple d001 , float_triple d101 ,
                                            float_triple d011 , float_triple d111  )
{
   float fxx,fxy,fxz, fyx,fyy,fyz, fzx,fzy,fzz ;
   float II , JJ , VV , jcb ;
   float_pair en ;

   /* load strain matrix */

   fxx = ( DA(d100,d000) + DA(d111,d011) ) * 0.5f + 1.0f ;
   fxy = ( DB(d100,d000) + DB(d111,d011) ) * 0.5f ;
   fxz = ( DC(d100,d000) + DC(d111,d011) ) * 0.5f ;

   fyx = ( DA(d010,d000) + DA(d111,d101) ) * 0.5f ;
   fyy = ( DB(d010,d000) + DB(d111,d101) ) * 0.5f + 1.0f ;
   fyz = ( DC(d010,d000) + DC(d111,d101) ) * 0.5f ;

   fzx = ( DA(d001,d000) + DA(d111,d110) ) * 0.5f ;
   fzy = ( DB(d001,d000) + DB(d111,d110) ) * 0.5f ;
   fzz = ( DC(d001,d000) + DC(d111,d110) ) * 0.5f + 1.0f ;

   /* determinant = bulk volume (1=unchanged) */

   JJ = TRIPROD( fxx,fxy,fxz, fyx,fyy,fyz, fzx,fzy,fzz ) ;
   if( JJ < 0.1f ) JJ = 0.1f ; else if( JJ > 10.0f ) JJ = 10.0f ;

   /* trace of matrix square = shear energy */

   II = (  fxx*fxx + fyy*fyy + fzz*fzz
         + fxy*fxy + fyx*fyx + fxz*fxz
         + fzx*fzx + fyz*fyz + fzy*fzy ) ;

   /* "vorticity" penalty added in, for fun */

   fxx = fyz - fzy ; fyy = fxz - fzx ; fzz = fxy - fyx ;
   VV = 2.0f*( fxx*fxx + fyy*fyy + fzz*fzz ) ;

#if 1
   jcb = cbrtf(JJ*JJ) ;
#else
# define CBQ(x) ((x)*(1.5f-0.5f*(x)))
   if( JJ <= 1.0f ){ jcb = CBQ(JJ) ; }
   else            { jcb = 1.0f / JJ ; jcb = 1.0f / CBQ(jcb) ; }
# undef  CBQ
#endif

   II = (II + VV) / jcb - 3.0f ; if( II < 0.0f ) II = 0.0f ;

   /* compute energies */

   jcb = (JJ-1.0f/JJ) ; en.a = 0.333f*jcb*jcb ; en.b = II ;
   return en ;
}

/*----------------------------------------------------------------------------*/
/* Load the deformation energies for all voxels */

#undef  Hpen_cut
#define Hpen_cut 1.0

float IW3D_load_energy( IndexWarp3D *AA )
{
   float enout=0.0f ;
   float *xda, *yda , *zda , *jea,*sea , jetop,setop ;
   int nx,ny,nz , nxy,nxyz , ii ;

ENTRY("IW3D_load_energy") ;

   if( AA == NULL ) RETURN(enout) ;

   nx = AA->nx ; ny = AA->ny ; nz = AA->nz ; nxy = nx*ny ; nxyz = nxy*nz ;

   xda = AA->xd ; yda = AA->yd ; zda = AA->zd ;

STATUS("get je/se arrays") ;
   jea = AA->je; if( jea == NULL ) jea = AA->je = (float *)calloc(nxyz,sizeof(float));
   sea = AA->se; if( sea == NULL ) sea = AA->se = (float *)calloc(nxyz,sizeof(float));

STATUS("dhhar -> 0") ;
   AAmemset( dhaar , 0 , sizeof(double)*nthmax ) ;

STATUS("start the work") ;
 AFNI_OMP_START ;
#pragma omp parallel
 { float_triple x0,x1,x2,x3,x4,x5,x6,x7 ; float_pair en ;
   int ii,jj,kk , ip,jp,kp , ijk , qq , ith=0 ; float esum=0.0f, ev ;
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
     en  = hexahedron_energy(x0,x1,x2,x3,x4,x5,x6,x7); jea[qq] = en.a; sea[qq] = en.b;
     ev  = jea[qq]-Hpen_cut ; if( ev > 0.0 ) esum += (ev*ev)*(ev*ev) ;
     ev  = sea[qq]-Hpen_cut ; if( ev > 0.0 ) esum += (ev*ev)*(ev*ev) ;
   }
#ifdef USE_OMP
   ith = omp_get_thread_num() ;
#endif
   dhaar[ith] = (double)esum ;
 }
 AFNI_OMP_END ;
STATUS("work is done") ;

  for( ii=0 ; ii < nthmax ; ii++ ) enout += dhaar[ii] ;
  RETURN(enout) ;
}

/*----------------------------------------------------------------------------*/
#ifdef USE_OMP
double HPEN_addup( int njs , float *je , float *se )
{
   double esum ; int qq ;

   AAmemset( dhaar , 0 , sizeof(double)*nthmax ) ;
#pragma omp parallel
   { int ii , ith ; double ev , dh=0.0 ;
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

double HPEN_addup( int njs , float *je , float *se )
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
#ifndef HAVE_HEXVOL
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
   An undistorted voxel will get volumen 1, since AA is a unitless warp. */

float IW3D_load_hexvol( IndexWarp3D *AA )
{
   float *xda, *yda , *zda , *hva , top,bot ;
   int nx,ny,nz , nxy,nxyz , ii ;
   float hvm = 0.0f ;

   if( AA == NULL ) return hvm ;

   nx = AA->nx ; ny = AA->ny ; nz = AA->nz ; nxy = nx*ny ; nxyz = nxy*nz ;

   xda = AA->xd ; yda = AA->yd ; zda = AA->zd ;

   hva = AA->hv ;
   if( hva == NULL ) hva = AA->hv = (float *)calloc(nxyz,sizeof(float)) ;

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

  return hvm ;
}

/*---------------------------------------------------------------------------*/
#if 0
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

/*---------------------------------------------------------------------------*/
/* The following functions are for interpolating all 3 components of an index
   warp at one time, and are shamelessly ripped off from mri_genalign_util.c */

#undef  CLIP
#define CLIP(mm,nn) if((mm) < 0)(mm)=0; else if((mm) > (nn))(mm)=(nn)

#undef  QLIP
#define QLIP(mm,nn) if( (mm) > (nn) ) (mm)=(nn)

#undef  AJK
#define AJK(aaa,j,k) ((aaa)+(j)*nx+(k)*nxy)

/*---------------------------------------------------------------------------*/
/*! Interpolate using linear method */

void IW3D_interp_linear( int nxx , int nyy , int nzz ,
                         float *aar , float *bar , float *car ,
                         int use_emat , mat33 emat ,
                         int npp, float *ip, float *jp, float *kp,
                         float *uar , float *var , float *war     )
{
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
   int uem=use_emat ;
   float Exx,Exy,Exz , Eyx,Eyy,Eyz , Ezx,Ezy,Ezz , uex,vex,wex ;

   UNLOAD_MAT33(emat,Exx,Exy,Exz,Eyx,Eyy,Eyz,Ezx,Ezy,Ezz) ;
   uex = vex = wex = 0.0f ;

#pragma omp for
   for( pp=0 ; pp < npp ; pp++ ){
     xx = ip[pp] ; yy = jp[pp] ; zz = kp[pp] ;
     if( !uem ){
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
       int aem=0 ; float eex,eey,eez ;
            if( xx < 0.0f ){ eex = xx    ; ix = 0      ; fx = 0.0f; aem++; }
       else if( xx < nx1  ){ eex = 0.0f  ; ix = (int)xx; fx = xx-ix;       }
       else                { eex = xx-nx1; ix = nx2    ; fx = 1.0f; aem++; }
            if( yy < 0.0f ){ eey = yy    ; jy = 0      ; fy = 0.0f; aem++; }
       else if( yy < ny1  ){ eey = 0.0f  ; jy = (int)yy; fy = yy-jy;       }
       else                { eey = yy-ny1; jy = ny2    ; fy = 1.0f; aem++; }
            if( zz < 0.0f ){ eez = zz    ; kz = 0      ; fz = 0.0f; aem++; }
       else if( zz < nz1  ){ eez = 0.0f  ; kz = (int)zz; fz = zz-kz;       }
       else                { eez = zz-nz1; kz = nz2    ; fz = 1.0f; aem++; }
       if( aem ){
         uex = Exx*eex + Exy*eey + Exz*eez ;
         vex = Eyx*eex + Eyy*eey + Eyz*eez ;
         wex = Ezx*eex + Ezy*eey + Ezz*eez ;
       } else {
         uex = vex = wex = 0.0f ;
       }
     }

     ix_00 = ix ; ix_p1 = ix_00+1 ;
     jy_00 = jy ; jy_p1 = jy_00+1 ;
     kz_00 = kz ; kz_p1 = kz_00+1 ;

     wt_00 = 1.0f-fx ; wt_p1 = fx ;  /* weights for ix_00 and ix_p1 points */

#undef  XINT
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

     uar[pp] = (1.0f-fz) * f_k00 + fz * f_kp1 + uex ;
     var[pp] = (1.0f-fz) * g_k00 + fz * g_kp1 + vex ;
     war[pp] = (1.0f-fz) * h_k00 + fz * h_kp1 + wex ;
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
#define AW(i) aarjk[iqq[i]]*wtt[i]
#define BW(i) barjk[iqq[i]]*wtt[i]
#define CW(i) carjk[iqq[i]]*wtt[i]

/*---------------------------------------------------------------------------*/
/*! Interpolate using wsinc5 method (slow and accurate) */

void IW3D_interp_wsinc5( int nxx , int nyy , int nzz ,
                         float *aar , float *bar , float *car ,
                         int use_emat , mat33 emat ,
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

   int uem=use_emat , outside=0 ;
   float Exx,Exy,Exz , Eyx,Eyy,Eyz , Ezx,Ezy,Ezz , uex,vex,wex ;

   UNLOAD_MAT33(emat,Exx,Exy,Exz,Eyx,Eyy,Eyz,Ezx,Ezy,Ezz) ;
   uex = vex = wex = 0.0f ;

   /*----- loop over points -----*/

#pragma omp for
   for( pp=0 ; pp < npp ; pp++ ){
     xx = ip[pp] ; yy = jp[pp] ; zz = kp[pp] ;
     if( !uem ){
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
       int aem=0 ; float eex,eey,eez ;
            if( xx < 0.0f ){ eex = xx    ; ix = 0      ; fx = 0.0f; aem++; }
       else if( xx < nx1  ){ eex = 0.0f  ; ix = (int)xx; fx = xx-ix;       }
       else                { eex = xx-nx1; ix = nx2    ; fx = 1.0f; aem++; }
            if( yy < 0.0f ){ eey = yy    ; jy = 0      ; fy = 0.0f; aem++; }
       else if( yy < ny1  ){ eey = 0.0f  ; jy = (int)yy; fy = yy-jy;       }
       else                { eey = yy-ny1; jy = ny2    ; fy = 1.0f; aem++; }
            if( zz < 0.0f ){ eez = zz    ; kz = 0      ; fz = 0.0f; aem++; }
       else if( zz < nz1  ){ eez = 0.0f  ; kz = (int)zz; fz = zz-kz;       }
       else                { eez = zz-nz1; kz = nz2    ; fz = 1.0f; aem++; }
       if( aem ){
         uex = Exx*eex + Exy*eey + Exz*eez ;
         vex = Eyx*eex + Eyy*eey + Eyz*eez ;
         wex = Ezx*eex + Ezy*eey + Ezz*eez ;
       } else {
         uex = vex = wex = 0.0f ;
       }
     }

#if 0
     if( outside ){                 /* use value at nearest edge point */
       qq = ix + jy*nx + kz*nxy ; CLIP(qq,nxyz1) ;
       uar[pp] = aar[qq] ; var[pp] = bar[qq] ; war[pp] = car[qq] ;
       continue ;
     }
#endif

     /*- x interpolations -*/

     for( wsum=0.0f,qq=-IRAD+1 ; qq <= IRAD ; qq++ ){  /* weights */
       xw  = fabsf(fx - qq) ; wt = sinc(xw) ;
       xw /= WRAD ; if( xw > WCUT ) wt *= wtap(xw) ;
       wtt[qq+(IRAD-1)] = wt ; wsum += wt ;
       iq = ix+qq ; CLIP(iq,nx1) ; iqq[qq+(IRAD-1)] = iq ;
     }
     wfac = wsum ;

     for( jj=-IRAD+1 ; jj <= IRAD ; jj++ ){  /* interps */
       jq = jy+jj ; CLIP(jq,ny1) ;
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

     for( wsum=0.0f,qq=-IRAD+1 ; qq <= IRAD ; qq++ ){  /* weights */
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

     for( wsum=0.0f,qq=-IRAD+1 ; qq <= IRAD ; qq++ ){  /* weights */
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
   }

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
/*! Interpolate using quintic method */

void IW3D_interp_quintic( int nxx , int nyy , int nzz ,
                          float *aar , float *bar , float *car ,
                          int use_emat , mat33 emat ,
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
   int uem=use_emat ;
   float Exx,Exy,Exz , Eyx,Eyy,Eyz , Ezx,Ezy,Ezz , uex,vex,wex ;

   UNLOAD_MAT33(emat,Exx,Exy,Exz,Eyx,Eyy,Eyz,Ezx,Ezy,Ezz) ;
   uex = vex = wex = 0.0f ;

#pragma omp for
   for( pp=0 ; pp < npp ; pp++ ){
     xx = ip[pp] ; yy = jp[pp] ; zz = kp[pp] ;
     if( !uem ){
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
       int aem=0 ; float eex,eey,eez ;
            if( xx < 0.0f ){ eex = xx    ; ix = 0      ; fx = 0.0f; aem++; }
       else if( xx < nx1  ){ eex = 0.0f  ; ix = (int)xx; fx = xx-ix;       }
       else                { eex = xx-nx1; ix = nx2    ; fx = 1.0f; aem++; }
            if( yy < 0.0f ){ eey = yy    ; jy = 0      ; fy = 0.0f; aem++; }
       else if( yy < ny1  ){ eey = 0.0f  ; jy = (int)yy; fy = yy-jy;       }
       else                { eey = yy-ny1; jy = ny2    ; fy = 1.0f; aem++; }
            if( zz < 0.0f ){ eez = zz    ; kz = 0      ; fz = 0.0f; aem++; }
       else if( zz < nz1  ){ eez = 0.0f  ; kz = (int)zz; fz = zz-kz;       }
       else                { eez = zz-nz1; kz = nz2    ; fz = 1.0f; aem++; }
       if( aem ){
         uex = Exx*eex + Exy*eey + Exz*eez ;
         vex = Eyx*eex + Eyy*eey + Eyz*eez ;
         wex = Ezx*eex + Ezy*eey + Ezz*eez ;
       } else {
         uex = vex = wex = 0.0f ;
       }
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

#undef  XINT
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
                  int use_emat , mat33 emat ,
                  int npp, float *ip, float *jp, float *kp,
                  float *uar , float *var , float *war     )
{
   switch( icode ){
     case MRI_NN:
     case MRI_LINEAR:
       IW3D_interp_linear( nxx , nyy , nzz , aar , bar , car ,
                           use_emat , emat ,
                           npp , ip  , jp  , kp  , uar , var , war ) ;
     break ;

     case MRI_CUBIC:
     case MRI_QUINTIC:
       IW3D_interp_quintic( nxx , nyy , nzz , aar , bar , car ,
                            use_emat , emat ,
                            npp , ip  , jp  , kp  , uar , var , war ) ;
     break ;

     default:
     case MRI_WSINC5:
       IW3D_interp_wsinc5( nxx , nyy , nzz , aar , bar , car ,
                           use_emat , emat ,
                           npp , ip  , jp  , kp  , uar , var , war ) ;
     break ;
   }

   return ;
}

/*---------------------------------------------------------------------------*/

#undef  NPER
#define NPER 262144  /* 1 Mbyte per float array */

/*---------------------------------------------------------------------------*/
/* B(A(x)) where B = matrix, A = warp
   -- no interpolation is needed for this operation
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
#pragma omp parallel if( nxyz > 6666 )
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

   RETURN(CC) ;
}

/*---------------------------------------------------------------------------*/
/* A(B(x)) where B = matrix, A = warp */

IndexWarp3D * IW3D_compose_m1w2( mat44 BB , IndexWarp3D *AA , int icode )
{
   int nx,ny,nz,nxy,nxyz , nall , pp,qtop;
   float *xda,*yda,*zda , *xdc,*ydc,*zdc , *xq,*yq,*zq ;
   IndexWarp3D *CC=NULL ;
   mat44 BL ;

ENTRY("IW3D_compose_m1w2") ;

   if( AA == NULL ) RETURN(CC) ;

   nx = AA->nx ; ny = AA->ny ; nz = AA->nz ; nxy = nx*ny ; nxyz = nxy*nz ;

   BL = BB ; /* BL = local copy of B */

   CC = IW3D_empty_copy(AA) ;

   xda = AA->xd ; yda = AA->yd ; zda = AA->zd ;
   xdc = CC->xd ; ydc = CC->yd ; zdc = CC->zd ;

   nall = MIN(nxyz,NPER) ;

   xq = (float *)malloc(sizeof(float)*nall) ;
   yq = (float *)malloc(sizeof(float)*nall) ;
   zq = (float *)malloc(sizeof(float)*nall) ;

   for( pp=0 ; pp < nxyz ; pp+=nall ){  /* loop over segments */

     qtop = MIN( nxyz , pp+nall ) ;  /* process points from pp to qtop-1 */

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 6666 )
 { int qq , ii,jj,kk ;
#pragma omp for
     for( qq=pp ; qq < qtop ; qq++ ){
       ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
       MAT44_VEC(BL,ii,jj,kk,xq[qq-pp],yq[qq-pp],zq[qq-pp]) ;
     }
 }
 AFNI_OMP_END ;

     /* Interpolate A() warp index displacments at the B(x) locations */

     IW3D_interp( icode, nx,ny,nz , xda   , yda   , zda      ,
                                    AA->use_emat  , AA->emat ,
                         qtop-pp  , xq    , yq    , zq       ,
                                    xdc+pp, ydc+pp, zdc+pp    ) ;

     /* Add in the B(x) displacments to get the total
        index displacment from each original position: B(x)-x + A(x+B(x)) */

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 6666 )
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

   free(zq) ; free(yq) ; free(xq) ; RETURN(CC) ;
}

/*---------------------------------------------------------------------------*/
/* Compute B(A(x)) */

IndexWarp3D * IW3D_compose( IndexWarp3D *AA , IndexWarp3D *BB , int icode )
{
   int nx,ny,nz,nxy,nxyz , nall , pp,qtop;
   float *xda,*yda,*zda , *xq,*yq,*zq , *xdc,*ydc,*zdc ;
   IndexWarp3D *CC ;

ENTRY("IW3D_compose") ;

        if( AA == NULL ){ CC = IW3D_copy(BB,1.0f) ; RETURN(CC) ; }
   else if( BB == NULL ){ CC = IW3D_copy(AA,1.0f) ; RETURN(CC) ; }

   nx = AA->nx ; ny = AA->ny ; nz = AA->nz ; nxy = nx*ny ; nxyz = nxy*nz ;

   if( nx != BB->nx || ny != BB->ny || nz != BB->nz ) RETURN(NULL) ;

   nall = MIN(nxyz,NPER) ;

   xq = (float *)malloc(sizeof(float)*nall) ;
   yq = (float *)malloc(sizeof(float)*nall) ;
   zq = (float *)malloc(sizeof(float)*nall) ;

   CC = IW3D_empty_copy(AA) ;

   xda = AA->xd ; yda = AA->yd ; zda = AA->zd ;
   xdc = CC->xd ; ydc = CC->yd ; zdc = CC->zd ;

   for( pp=0 ; pp < nxyz ; pp+=nall ){  /* loop over segments */

     qtop = MIN( nxyz , pp+nall ) ;  /* process points from pp to qtop-1 */

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 6666 )
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

     /* Interpolate B() warp index displacments at the A() locations */

     IW3D_interp( icode, nx,ny,nz , BB->xd, BB->yd, BB->zd ,
                                    BB->use_emat , BB->emat ,
                         qtop-pp  , xq    , yq    , zq     ,
                                    xdc+pp, ydc+pp, zdc+pp  ) ;

     /* Add in the A() displacments to get the total
        index displacment from each original position: A(x) + B(x+A(x)) */

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 6666 )
 { int qq ;
#pragma omp for
     for( qq=pp ; qq < qtop ; qq++ ){
       xdc[qq] += xda[qq] ; ydc[qq] += yda[qq] ; zdc[qq] += zda[qq] ;
     }
 }
 AFNI_OMP_END ;

   } /* end of loop over segments of length NPER (or less) */

   free(zq) ; free(yq) ; free(xq) ; RETURN(CC) ;
}

#if 0
/*---------------------------------------------------------------------------*/
/* Compute A^(2^lev) , using linear interpolation only */

IndexWarp3D * IW3D_2pow( IndexWarp3D *AA , int lev )
{
   int nx,ny,nz,nxy,nxyz , nall , pp,qtop , ll ;
   float *xdb,*ydb,*zdb , *xq,*yq,*zq , *xdc,*ydc,*zdc ;
   IndexWarp3D *BB , *CC , *TT ;

ENTRY("IW3D_2pow") ;

   if( AA == NULL ) RETURN(NULL) ;  /* duh */

   /* simple case of squaring */

   if( lev == 1 ){ BB = IW3D_compose(AA,AA,MRI_LINEAR) ; RETURN(BB) ; }

   BB = IW3D_copy(AA,1.0f) ;  /* BB = AA (lev=0 result) */

   if( lev <= 0 ) RETURN(BB) ;

   nx = BB->nx ; ny = BB->ny ; nz = BB->nz ; nxy = nx*ny ; nxyz = nxy*nz ;

   nall = MIN(nxyz,NPER) ;

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
#pragma omp parallel if( qtop-pp > 6666 )
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

       IW3D_interp_linear( nx,ny,nz , xdb   , ydb   , zdb   ,
                           BB->use_emat , BB->emat ,
                           qtop-pp  , xq    , yq    , zq    ,
                                      xdc+pp, ydc+pp, zdc+pp ) ;

        /* Add in the B() displacments to get the total
           index displacment from each original position: B(x) + B(x+B(x)) */

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 6666 )
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
   }

   /* at the end, BB is the result, and CC is trash */

   IW3D_destroy(CC) ; free(zq) ; free(yq) ; free(xq) ; RETURN(BB) ;
}
#endif

/*---------------------------------------------------------------------------*/
/* Compute B( 2x - A(B(x)) ) = Newton step for computing Ainv(x) */

static float inewtfac = 0.5f ;
static int   inewtfix = 0 ;

IndexWarp3D * IW3D_invert_newt( IndexWarp3D *AA, IndexWarp3D *BB, int icode )
{
   int nx,ny,nz,nxy,nxyz , nall , pp,qtop ;
   float *xda,*yda,*zda , *xq,*yq,*zq,*xr,*yr,*zr , *xdc,*ydc,*zdc , *xdb,*ydb,*zdb ;
   IndexWarp3D *CC ;

ENTRY("IW3D_invert_newt") ;

   if( AA == NULL || BB == NULL ) RETURN(NULL) ;  /* stoopidd luser */

   nx = AA->nx ; ny = AA->ny ; nz = AA->nz ; nxy = nx*ny ; nxyz = nxy*nz ;

   if( nx != BB->nx || ny != BB->ny || nz != BB->nz ) RETURN(NULL) ;

   nall = MIN(nxyz,NPER) ;

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

   /* Warps are stored as voxel index displacements, so we have
      A(x)              = x + a(x)
      B(x)              = x + b(x),
      A(B(x))           = x + b(x) + a(x+b(x))
      2x - A(B(x))      = x - b(x) - a(x+b(x))
      B( 2x - A(B(x)) ) = x - b(x) - a(x+b(x)) + b(x-b(x)-a(x+b(x))) */

   for( pp=0 ; pp < nxyz ; pp+=nall ){  /* loop over segments */

     qtop = MIN( nxyz , pp+nall ) ;  /* process points from pp to qtop-1 */

     /* Compute [xq,yq,zq] = x+b(x) */

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 6666 )
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

     /* Compute [xr,yr,zr] = a(x+b(x)) */

     IW3D_interp( icode, nx,ny,nz , xda, yda, zda,
                         AA->use_emat , AA->emat ,
                         qtop-pp  , xq , yq , zq ,
                                    xr , yr , zr  ) ;

     /* Compute [xr,yr,zr] = x - b(x) - a(x+b(x)) */

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 6666 )
 { int qq , ii,jj,kk ;
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

     IW3D_interp( icode, nx,ny,nz , xdb, ydb, zdb,
                         BB->use_emat , BB->emat ,
                         qtop-pp  , xr , yr , zr ,
                                    xq , yq , zq  ) ;

     /* Compute result = -b(x) - a(x+b(x)) + b(x-b(x)-a(x+b(x))) */

     if( inewtfac <= 0.0f ){

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 6666 )
 { int qq , ii,jj,kk ;
#pragma omp for
       for( qq=pp ; qq < qtop ; qq++ ){
         ii = qq % nx ; kk = qq / nxy ; jj = (qq-kk*nxy) / nx ;
         xdc[qq] = xr[qq-pp] - ii + xq[qq-pp] ;
         ydc[qq] = yr[qq-pp] - jj + yq[qq-pp] ;
         zdc[qq] = zr[qq-pp] - kk + zq[qq-pp] ;
       }
 }
 AFNI_OMP_END ;

     } else {

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 6666 )
 { int qq , ii,jj,kk ;
   register float nf , nf1 ;
   nf = inewtfac ; nf1 = 1.0f - nf ;
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

   free(zr); free(yr); free(xr); free(zq); free(yq); free(xq); RETURN(CC);
}

/*---------------------------------------------------------------------------*/
/* Find the inverse warp BB(x) to AA(x).  If not NULL, BBinit is the
   initial estimate for BB(x).  icode tells how to interpolate warps. */

IndexWarp3D * IW3D_invert( IndexWarp3D *AA , IndexWarp3D *BBinit , int icode )
{
   IndexWarp3D *BB , *CC ;
   float normAA , normBC , nrat , orat ;
   int ii , nnewt=0 , nss,nii , jcode=MRI_LINEAR ;
   float switchval=0.001f ;

ENTRY("IW3D_invert") ;

   if( AA == NULL ) RETURN(NULL) ;

   normAA = IW3D_normLinf( AA , NULL ) ;
   if( normAA == 0.0f ){
     BB = IW3D_empty_copy(AA) ; RETURN(BB) ;
   }
   if( icode == MRI_WSINC5 ) icode = MRI_QUINTIC ;

   /* BB = initial guess at inverse */

   if( verb_nww ) ININFO_message(" -- invert max|AA|=%f",normAA) ;

   if( BBinit == NULL ){
     int pp = 1+(int)ceil(log2(normAA)) ; float qq ;
     if( pp < 2 ) pp = 2 ;
     qq = pow(0.5,pp) ;
     if( verb_nww ) ININFO_message("  - init nstep=%d qq=1/2^%d=%f",pp,pp,qq) ;
     BB = IW3D_copy( AA,-qq ) ;
     for( ii=0 ; ii < pp ; ii++ ){
       if( verb_nww > 1 ) ININFO_message("  - init step %d",ii+1) ;
       CC = IW3D_compose(BB,BB,jcode) ; IW3D_destroy(BB) ; BB = CC ;
     }
   } else {
     BB = IW3D_copy( BBinit , 1.0f ) ;
   }

   normAA  = IW3D_normL1( AA , NULL ) ;
   if( !inewtfix ){
     inewtfac = 2.0f / (2.0f+sqrtf(normAA)) ;  /* Newton damping factor */
     if( inewtfac > 0.333f ) inewtfac = 0.333f ;
   }

   if( verb_nww )
     ININFO_message("  - start iterations: normAA=%f inewtfac=%f",normAA,inewtfac) ;

   /* iterate some, until convergence or exhaustion */

#if 0
   if( getenv("AFNI_NWARP_SWITCHVAL") != NULL ){
     switchval = (float)AFNI_numenv("AFNI_NWARP_SWITCHVAL") ;
          if( switchval <= 0.0002f ) switchval = 0.0002f ;
     else if( switchval >= 0.0100f ) switchval = 0.0100f ;
   }
#endif

   nrat = 666.666f ;

   for( nii=nss=ii=0 ; ii < 69 ; ii++ ){

     /* take a Newton step */

     CC = BB ; BB = IW3D_invert_newt(AA,CC,jcode) ;

     /* how close are they now? */

     normBC = IW3D_normL1( BB , CC ) ; IW3D_destroy(CC) ;

     orat = nrat ; nrat = normBC / normAA ;

     if( verb_nww ) ININFO_message("  - iterate %d nrat=%f",++nnewt,nrat) ;

     /* check for convergence of B and C */

     if( jcode != icode && nrat < switchval ){
       jcode = icode ; nss = 0 ;
       if( verb_nww ) ININFO_message("  - switching from linear interp") ;
       continue ;
     }

     if( nrat < 0.0001f ){
       if( verb_nww ) ININFO_message(" -- iteration converged") ;
       RETURN(BB) ;   /* converged */
     }

     if( ii > 3 && nrat > orat ){
       nii++ ; if( nii == 2 ) break ;  /* getting worse?! */
     } else {
       nii = 0 ;
     }

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

   }

   /* failed to converge, return latest result anyhoo */

   WARNING_message("invert: iterations failed to converge") ;
   RETURN(BB) ;
}

/*---------------------------------------------------------------------------*/
/* Iteration step for sqrt:  Bnew(x) = B( 1.5*x - 0.5*A(B(B(x))) )
   This is actually a step to produce the square root of inverse(A). */

static float sstepfac = 0.5f ;

static float sstepfac_MAX = 0.432111f ;
static float sstepfac_MIN = 0.135799f ;

IndexWarp3D * IW3D_sqrtinv_step( IndexWarp3D *AA, IndexWarp3D *BB, int icode )
{
   int nx,ny,nz,nxy,nxyz , nall , pp,qtop ;
   float *xda,*yda,*zda , *xq,*yq,*zq,*xr,*yr,*zr , *xdc,*ydc,*zdc , *xdb,*ydb,*zdb ;
   IndexWarp3D *CC ;

ENTRY("IW3D_sqrtinv_step") ;

   if( AA == NULL || BB == NULL ) RETURN(NULL) ;  /* stoopidd luser */

   nx = AA->nx ; ny = AA->ny ; nz = AA->nz ; nxy = nx*ny ; nxyz = nxy*nz ;

   if( nx != BB->nx || ny != BB->ny || nz != BB->nz ) RETURN(NULL) ;

   nall = MIN(nxyz,NPER) ;

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
#pragma omp parallel if( qtop-pp > 6666 )
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

     IW3D_interp( icode, nx,ny,nz , xdb, ydb, zdb,
                         BB->use_emat , BB->emat ,
                         qtop-pp  , xq , yq , zq ,
                                    xr , yr , zr  ) ;

     /* Compute [xr,yr,zr] = B(B(x)) = B(x) + b(B(x)) */

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 6666 )
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

     IW3D_interp( icode, nx,ny,nz , xda, yda, zda,
                         AA->use_emat , AA->emat ,
                         qtop-pp  , xr , yr , zr ,
                                    xq , yq , zq  ) ;

     /* Compute [xq,yq,zq] = 1.5*x - 0.5*A(B(B(x)))
                           = 1.5*x - 0.5*( B(B(x)) + a(B(B(x))) ) */

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 6666 )
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

     IW3D_interp( icode, nx,ny,nz , xdb, ydb, zdb,
                         BB->use_emat , BB->emat ,
                         qtop-pp  , xq , yq , zq ,
                                    xr , yr , zr  ) ;

     /* Compute the answer: B(1.5*x - 0.5*A(B(B(x))))
                          = 1.5*x - 0.5*A(B(B(x))) + b(1.5*x - 0.5*A(B(B(x)))) */

     if( sstepfac <= 0.0f ){

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 6666 )
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
#pragma omp parallel if( qtop-pp > 6666 )
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

   free(zr); free(yr); free(xr); free(zq); free(yq); free(xq); RETURN(CC);
}

/*---------------------------------------------------------------------------*/
/*** This is bad -- don't use it! ***/
#if 0
IndexWarp3D * IW3D_sqrtinv_stepQ( IndexWarp3D *AA, IndexWarp3D *BB, int icode )
{
   int nx,ny,nz,nxy,nxyz , nall , pp,qtop ;
   float *xda,*yda,*zda , *xq,*yq,*zq,*xr,*yr,*zr , *xdc,*ydc,*zdc , *xdb,*ydb,*zdb ;
   IndexWarp3D *CC ;

ENTRY("IW3D_sqrtinv_stepQ") ;

   if( AA == NULL || BB == NULL ) RETURN(NULL) ;  /* stoopidd luser */

   nx = AA->nx ; ny = AA->ny ; nz = AA->nz ; nxy = nx*ny ; nxyz = nxy*nz ;

   if( nx != BB->nx || ny != BB->ny || nz != BB->nz ) RETURN(NULL) ;

   nall = MIN(nxyz,NPER) ;

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
#pragma omp parallel if( qtop-pp > 6666 )
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

     IW3D_interp( icode, nx,ny,nz , xdb, ydb, zdb,
                         BB->use_emat , BB->emat ,
                         qtop-pp  , xq , yq , zq ,
                                    xr , yr , zr  ) ;

     /* Compute [xr,yr,zr] = B(A(x)) = A(x) + b(A(x)) */

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 6666 )
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

     IW3D_interp( icode, nx,ny,nz , xdb, ydb, zdb,
                         AA->use_emat , AA->emat ,
                         qtop-pp  , xr , yr , zr ,
                                    xq , yq , zq  ) ;

     /* Compute [xq,yq,zq] = 1.5*x - 0.5*B(B(A(x)))
                           = 1.5*x - 0.5*( B(A(x)) + b(B(A(x))) ) */

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 6666 )
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

     IW3D_interp( icode, nx,ny,nz , xdb, ydb, zdb,
                         BB->use_emat , BB->emat ,
                         qtop-pp  , xq , yq , zq ,
                                    xr , yr , zr  ) ;

     /* Compute the answer: B(1.5*x - 0.5*B(B(A(x))))
                          = 1.5*x - 0.5*B(B(A(x))) + b(1.5*x - 0.5*B(B(A(x)))) */

     if( sstepfac <= 0.0f ){

 AFNI_OMP_START ;
#pragma omp parallel if( qtop-pp > 6666 )
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
#pragma omp parallel if( qtop-pp > 6666 )
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

   free(zr); free(yr); free(xr); free(zq); free(yq); free(xq); RETURN(CC);
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
     BB = IW3D_sqrtinv_stepQ(AA,CC,jcode) ;
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

#define USE_SQRTPAIR
#ifdef USE_SQRTPAIR
/*---------------------------------------------------------------------------*/

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
 return (tsum/nxyz) ;
}

/*---------------------------------------------------------------------------*/

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

     if( nrat < 0.001666f              ) break ;
     if( nite > 2 && nrat > orat*0.99f ) break ;
   }

   if( verb_nww ) INFO_message("*** sqrtpair: exit after %d iterations",nite+1) ;

   inewtfix = 0 ;
   return YYZZ ;
}
#endif

/****************************************************************************/
/****************************************************************************/

#undef AFF_PARAM
#undef AFF_MATRIX

#define AFF_PARAM  1
#define AFF_MATRIX 2

static int affmode = AFF_PARAM ;

/*---------------------------------------------------------------------------*/
/* Create a warp from a set of parameters, matching a template warp. */

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
   nxyz = nx*ny*nz ; nall = MIN(nxyz,NPER) ;

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
#pragma omp parallel if( qtop-pp > 6666 )
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
#pragma omp parallel if( qtop-pp > 6666 )
 { int qq ;
#pragma omp for
     for( qq=pp ; qq < qtop ; qq++ ){
       xda[qq] -= xq[qq-pp] ; yda[qq] -= yq[qq-pp] ; zda[qq] -= zq[qq-pp] ;
     }
 }
 AFNI_OMP_END ;

   } /* end of loop over segments */

   /* time to trot, Bwana */

   free(zq) ; free(yq) ; free(xq) ; RETURN(AA) ;
}

/*---------------------------------------------------------------------------*/

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
   AA = IW3D_from_poly( 12 , mar , WW ) ;
   IW3D_destroy( WW ) ;
   return AA ;
}

/****************************************************************************/
/****************************************************************************/
/****************************************************************************/

/*--------------------------------------------------------------------------*/
/* interpolate from a float image to a set of indexes;
   this is just a wrapper for functions from mri_genalign_util.c */

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

/*----------------------------------------------------------------------------*/

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

MRI_IMAGE * IW3D_warp_floatim( IndexWarp3D *AA, MRI_IMAGE *sim, int code, float fac )
{
   MRI_IMAGE *fim ;

ENTRY("IW3D_warp_floatim") ;

   if( AA == NULL || sim == NULL ) RETURN(NULL) ;

   fim = mri_new_conforming( sim , MRI_float ) ;

   IW3D_warp_into_floatim( AA , sim , fim ,
                           0,sim->nx-1 , 0,sim->ny-1 , 0,sim->nz-1 , code , fac ) ;

   RETURN(fim) ;
}

/*----------------------------------------------------------------------------*/

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

#if 0
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

/*----------------------------------------------------------------------------*/
/* Setup to warp images given
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

   /* space for indexes/coordinates in output space */

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
#pragma omp parallel if( nxyz > 6666 )
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
#pragma omp parallel if( nxyz > 6666 )
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
#pragma omp parallel if( nxyz > 6666 )
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
#pragma omp parallel if( nxyz > 6666 )
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
   THD_3dim_dataset *dset_out ;
   MRI_IMAGE *fim , *wim ; float *ip=NULL,*jp=NULL,*kp=NULL ;
   int nx,ny,nz,nxyz , nvals , kk,iv ;
   float *amatar=NULL ; int nxa=0,nya=0 ; mat44 amat ;

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
     cpt = strstr(prefix,".nii") ; if( cpt != NULL ) *cpt = '\0' ;
     strcat( prefix , "_nwarp" ) ; if( cpt != NULL ) strcat(prefix,".nii") ;
   }

   /*---------- manufacture the empty shell of the output dataset ----------*/

   if( !ISVALID_MAT44(dset_src->daxes->ijk_to_dicom) )
     THD_daxes_to_mat44(dset_src->daxes) ;
   src_cmat = dset_src->daxes->ijk_to_dicom ;

   if( !ISVALID_MAT44(dset_nwarp->daxes->ijk_to_dicom) )
     THD_daxes_to_mat44(dset_nwarp->daxes) ;
   nwarp_cmat = dset_nwarp->daxes->ijk_to_dicom ;

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
   fim = THD_extract_float_brick(0,dset_nwarp) ; ADDTO_IMARR(imar_nwarp,fim) ;
   fim = THD_extract_float_brick(1,dset_nwarp) ; ADDTO_IMARR(imar_nwarp,fim) ;
   fim = THD_extract_float_brick(2,dset_nwarp) ; ADDTO_IMARR(imar_nwarp,fim) ;
   DSET_unload(dset_nwarp) ;

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
     if( verb_nww && iv == 0 ) fprintf(stderr,"Warping dataset: ") ;
     THD_interp_floatim( fim, nxyz,ip,jp,kp, dincode, MRI_FLOAT_PTR(wim) ) ;
     EDIT_substitute_brick( dset_out , iv , MRI_float , MRI_FLOAT_PTR(wim) ) ;
     mri_clear_and_free(wim) ;
     if( nya > 1 ){ DESTROY_IMARR(im_src) ; }  /* will be re-computed */
     if( verb_nww ) fprintf(stderr,".") ;
   }

   if( imar_nwarp != NULL ) DESTROY_IMARR(imar_nwarp) ;
   if( im_src     != NULL ) DESTROY_IMARR(im_src) ;
   DSET_unload(dset_src) ;
   if( verb_nww ) fprintf(stderr,"\n") ;
   RETURN(dset_out) ;
}

/****************************************************************************/
/****************************************************************************/
/****************************************************************************/

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
       dset = THD_open_dataset(buf) ;
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
       dset = THD_open_dataset(buf) ;
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
       iset = THD_open_dataset(buf) ;
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

/*----------------------------------------------------------------------------*/
/**** Functions for reading warps and inverting/catenating them right away ****/
/*----------------------------------------------------------------------------*/

#define CW_NMAX 99

static int          CW_nwtop=0 ;
static IndexWarp3D *CW_iwarp[CW_NMAX] ;
static float        CW_iwfac[CW_NMAX] ;
static mat44       *CW_awarp[CW_NMAX] ;
static int CW_nx=0,CW_ny=0,CW_nz=0 ; char *CW_geomstring=NULL ;
static mat44 CW_cmat , CW_imat ;

static THD_3dim_dataset *CW_inset=NULL ;

/*----------------------------------------------------------------------------*/
/* Erase the above static data */

static void CW_clear_data(void)
{
   int ii ;
   for( ii=0 ; ii < CW_NMAX ; ii++ ){
     CW_iwfac[ii] = 1.0f ;
     if( CW_iwarp[ii] != NULL ){
       IW3D_destroy(CW_iwarp[ii]) ; CW_iwarp[ii] = NULL ;
     }
     if( CW_awarp[ii] != NULL ){
       free(CW_awarp[ii]) ; CW_awarp[ii] = NULL ;
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

   return ;
}

/*----------------------------------------------------------------------------*/
/* Load one warp into the static data, inverting it if necessary */

static void CW_load_one_warp( int nn , char *cp )
{
   char *wp ; int do_inv=0 , do_sqrt=0 , do_empty=0 , ii ;

ENTRY("CW_load_one_warp") ;

   if( nn <= 0 || nn > CW_NMAX || cp == NULL || *cp == '\0' ){
     ERROR_message("bad inputs to CW_load_one_warp") ; EXRETURN ;
   }

   if( strncasecmp(cp,"INV(",4) == 0 ){                 /* set inversion flag */
     cp += 4 ; do_inv = 1 ;
   } else if( strncasecmp(cp,"INVERSE(",8) == 0 ){
     cp += 8 ; do_inv = 1 ;
   } else if( strncasecmp(cp,"SQRT(",5) == 0 ){
     cp += 5 ; do_sqrt = 1 ;
   } else if( strncasecmp(cp,"SQRTINV(",8) == 0 || strncasecmp(cp,"INVSQRT(",8) == 0 ){
     cp += 8 ; do_inv = do_sqrt = 1 ;
   } else if( strncasecmp(cp,"IDENT(",6) == 0 ){
     cp += 6 ; do_empty = 1 ;
   }
   wp = strdup(cp) ; ii = strlen(wp) ;
   if( ii < 4 ){
     ERROR_message("input string to CW_load_one_warp is too short :-((") ;
     free(wp) ; EXRETURN ;
   }
   if( wp[ii-1] == ')' ) wp[ii-1] = '\0' ;

   if( nn > CW_nwtop ) CW_nwtop = nn ;  /* CW_nwtop = largest index thus far */

   if( STRING_HAS_SUFFIX_CASE(wp,".1D")  ||
       STRING_HAS_SUFFIX_CASE(wp,".txt")   ){      /*--- affine warp ---*/

     mat44 mmm ; MRI_IMAGE *qim ; float *qar ;
     qim = mri_read_1D(wp) ;
     if( qim == NULL || qim->nvox < 9 ){
       ERROR_message("cannot read matrix from file '%s'",wp); free(wp); EXRETURN ;
     }
     if( qim->nx < 12 && qim->ny > 1 ){
       MRI_IMAGE *tim = mri_transpose(qim) ; mri_free(qim) ; qim = tim ;
     }
     qar = MRI_FLOAT_PTR(qim) ;
     if( qim->nvox < 12 )                           /* presumably a rotation */
       LOAD_MAT44(mmm,qar[0],qar[1],qar[2],0,
                      qar[3],qar[4],qar[5],0,
                      qar[6],qar[7],qar[8],0) ;
     else                                           /* a full matrix */
       LOAD_MAT44(mmm,qar[0],qar[1],qar[2],qar[3],
                      qar[4],qar[5],qar[6],qar[7],
                      qar[8],qar[9],qar[10],qar[11]) ;
     mri_free(qim) ;

     if( do_inv ){
       mat44 imm = MAT44_INV(mmm) ; mmm = imm ;
     }
     if( do_sqrt ){
       mat44 smm = THD_mat44_sqrt(mmm) ; mmm = smm ;
     }

     CW_awarp[nn-1] = (mat44 *)malloc(sizeof(mat44)) ;
     AAmemcpy(CW_awarp[nn-1],&mmm,sizeof(mat44)) ;
     free(wp) ; EXRETURN ;

   } else {                                        /* dataset warp */

     THD_3dim_dataset *dset, *eset=NULL ; IndexWarp3D *AA , *BB ;

     /* check for special case of uni-directional warp from 1 sub-brick [19 Mar 2013] */

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
         default:
           sscanf(up,"%f,%f,%f",&vx,&vy,&vz) ;
           vm = sqrtf(vx*vx+vy*vy+vz*vz) ;
           if( vm < 1.e-9f ){
             ERROR_message("uni-directional warp '%s' :-) direction is unclear",wp) ;
             free(wp) ; EXRETURN ;
           }
           vx /= vm ; vy /= vm ; vz /= vm ;
           vp = strchr(up,':') ;
           if( vp == NULL ){
             ERROR_message("uni-directional warp '%s' :-) no dataset?",wp) ;
             free(wp) ; EXRETURN ;
           }
           up = vp+1 ;
       }

       /* check if there is a scale factor */

       vp = strchr(up,':') ;
       if( vp != NULL && isnumeric(*up) ){
         float wfac = (float)strtod(up,NULL) ;
         if( wfac == 0.0f ){
           ERROR_message("uni-directional warp '%s' :-) scale factor = 0?",wp) ;
           free(wp) ; EXRETURN ;
         }
         up = vp+1 ;
         vx *= wfac ; vy *= wfac ; vz *= wfac ;
       }

       /* now read dataset and do surgery on it */

       eset = THD_open_dataset(up) ;
       if( eset == NULL ){
         ERROR_message("Can't open dataset from file '%s'",up); free(wp); EXRETURN;
       }
       DSET_load(eset) ;
       if( !DSET_LOADED(eset) ){
         ERROR_message("Can't load dataset from file '%s'",up); free(wp); DSET_delete(eset); EXRETURN;
       }
       dim = THD_extract_float_brick(0,eset); dar = MRI_FLOAT_PTR(dim); DSET_unload(eset);
       nvox = dim->nvox ;
       xar = (float *)calloc(sizeof(float),nvox) ; /* bricks for output dataset */
       yar = (float *)calloc(sizeof(float),nvox) ;
       zar = (float *)calloc(sizeof(float),nvox) ;
       dset = EDIT_empty_copy(eset) ;
       EDIT_dset_items( dset ,
                          ADN_nvals , 3 ,
                          ADN_ntt   , 0 ,
                          ADN_datum_all , MRI_float ,
                        ADN_none ) ;
       EDIT_BRICK_FACTOR(dset,0,0.0) ; EDIT_substitute_brick(dset,0,MRI_float,xar) ;
       EDIT_BRICK_FACTOR(dset,1,0.0) ; EDIT_substitute_brick(dset,1,MRI_float,yar) ;
       EDIT_BRICK_FACTOR(dset,2,0.0) ; EDIT_substitute_brick(dset,2,MRI_float,zar) ;
       for( ii=0 ; ii < nvox ; ii++ ){
         xar[ii] = vx * dar[ii]; yar[ii] = vy * dar[ii]; zar[ii] = vz * dar[ii];
       }
       mri_free(dim) ;

     } else {  /*--- standard 3-brick warp ---*/

       dset = THD_open_dataset(wp) ;
       if( dset == NULL ){
         ERROR_message("Can't open dataset from file '%s'",wp); free(wp); EXRETURN;
       }

     }

     /*--- convert dataset to warp ---*/

     AA = IW3D_from_dataset(dset,do_empty,0) ;
     if( AA == NULL ){
       ERROR_message("Can't make warp from dataset '%s'",wp); free(wp); EXRETURN;
     }
     if( CW_geomstring == NULL ){       /* first dataset => set geometry globals */
       CW_geomstring = strdup(AA->geomstring) ;
       CW_nx = AA->nx; CW_ny = AA->ny; CW_nz = AA->nz; CW_cmat = AA->cmat; CW_imat = AA->imat;
     } else if( AA->nx != CW_nx || AA->ny != CW_ny || AA->nz != CW_nz ){ /* check them */
       ERROR_message("warp from dataset '%s' doesn't match earlier inputs in grid size",wp) ;
       free(wp); EXRETURN ;
     }
     if( CW_inset == NULL ){
       if( eset != NULL ){ CW_inset = eset ; DSET_delete(dset) ; }
       else              { DSET_unload(dset) ; CW_inset = dset ; }  /* save as template */
     }
     else                { DSET_delete(dset) ; }

     if( do_sqrt ){
#ifndef USE_SQRTPAIR
       BB = IW3D_sqrtinv(AA,NULL,MRI_LINEAR) ;  /* inverse AND sqrt */
       if( do_inv ){
         IW3D_destroy(AA) ; AA = BB ;
       } else {                                 /* must re-invert */
         AA = IW3D_invert(BB,NULL,MRI_LINEAR) ; IW3D_destroy(BB) ;
       }
#else
       IndexWarp3D_pair *YZ = IW3D_sqrtpair(AA,MRI_LINEAR) ;
       if( do_inv ){ AA = YZ->iwarp ; IW3D_destroy(YZ->fwarp) ; }
       else        { AA = YZ->fwarp ; IW3D_destroy(YZ->iwarp) ; }
       free(YZ) ;
#endif
     } else if( do_inv ){
       BB = IW3D_invert(AA,NULL,MRI_WSINC5); IW3D_destroy(AA); AA = BB;
     }
     AA->use_emat = 0 ;

     CW_iwarp[nn-1] = AA ; free(wp) ; EXRETURN ;
   }

   /* unreachable */

   free(wp); EXRETURN;
}

/*----------------------------------------------------------------------------*/
/* Read in a string like
     "warp1 warp2 warp3"
   and return the dataset that instantiates warp3(warp2(warp1(x))).
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
     oset = THD_open_dataset(csar->str[0]) ;
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

   /*-- multiple input datsets (or INV operations) --*/

   for( ii=0 ; ii < csar->num ; ii++ )           /* read them in */
     CW_load_one_warp( ii+1 , csar->str[ii] ) ;

   NI_delete_str_array(csar) ;

   if( CW_geomstring == NULL ){ /* didn't get a real warp to use */
     ERROR_message("Can't compute nonlinear warp from string '%s'",cstr) ;
     CW_clear_data() ; RETURN(NULL) ;
   }

   /*-- cat them --*/

   LOAD_IDENT_MAT44(wmat) ;

   for( ii=0 ; ii < CW_nwtop ; ii++ ){

     if( CW_awarp[ii] != NULL ){  /* matrix to apply */

       qmat = *(CW_awarp[ii]) ;          /* convert from xyz warp to ijk warp */
       tmat = MAT44_MUL(qmat,CW_cmat) ;
       smat = MAT44_MUL(CW_imat,tmat) ;

       if( warp == NULL ){                         /* thus far, only matrices */
         qmat = MAT44_MUL(smat,wmat) ; wmat = qmat ;
       } else {                             /* apply matrix to nonlinear warp */
         tarp = IW3D_compose_w1m2(warp,smat,MRI_WSINC5) ;
         IW3D_destroy(warp) ; warp = tarp ;
       }

       free(CW_awarp[ii]) ; CW_awarp[ii] = NULL ;

     } else if( CW_iwarp[ii] != NULL ){            /* nonlinear warp to apply */

       if( CW_iwfac[ii] != 1.0f ) IW3D_scale( CW_iwarp[ii] , CW_iwfac[ii] ) ;

       if( warp == NULL ){             /* create nonlinear warp at this point */
         if( ii == 0 ){   /* first one ==> don't compose with identity matrix */
           warp = IW3D_copy(CW_iwarp[ii],1.0f) ;
         } else {                             /* compose with previous matrix */
           warp = IW3D_compose_m1w2(wmat,CW_iwarp[ii],MRI_WSINC5) ;
         }
       } else {           /* already have nonlinear warp, apply new one to it */
         tarp = IW3D_compose(warp,CW_iwarp[ii],MRI_WSINC5) ;
         IW3D_destroy(warp) ; warp = tarp ;
       }

       IW3D_destroy(CW_iwarp[ii]) ; CW_iwarp[ii] = NULL ;

     }

   }

   /*--- create output dataset ---*/

   if( warp == NULL ){
     ERROR_message("This message should never appear!") ;
     CW_clear_data() ; RETURN(NULL) ;
   }

   IW3D_adopt_dataset( warp , CW_inset ) ;
   oset = IW3D_to_dataset( warp , prefix ) ;

   IW3D_destroy(warp) ; CW_clear_data() ;

   RETURN(oset) ;
}

/******************************************************************************/
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
/******************************************************************************/
#ifdef ALLOW_QWARP
/******************************************************************************/
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
/******************************************************************************/

/*============================================================================*/
/********** Functions for optimizing an nwarp for image registration **********/
/*============================================================================*/

/*----------------------------------------------------------------------------*/

/*--- flag masks for types of displacement allowed ---*/

#define NWARP_NOXDIS_FLAG  1  /* no displacment in X direction? */
#define NWARP_NOYDIS_FLAG  2
#define NWARP_NOZDIS_FLAG  4

#define NWARP_NODISP_FLAG (NWARP_NOXDIS_FLAG | NWARP_NOYDIS_FLAG | NWARP_NOZDIS_FLAG)

#define NWARP_NOXDEP_FLAG  8  /* no functional dependence of displacment on X? */
#define NWARP_NOYDEP_FLAG 16
#define NWARP_NOZDEP_FLAG 32

/*--- Hermite polynomial basis arrays for each direction: x,y,z. ---*/

static int nbcx=0, nbcy=0, nbcz=0 , nbcxy ;  /* dimensions of patch */
static int nbqx=0, nbqy=0, nbqz=0 , nbqxy ;  /* dimensions of patch */

static int Hbmode = -666 ;

static float *bc0x=NULL, *bc1x=NULL,             dxci=0.0f ;
static float *bc0y=NULL, *bc1y=NULL,             dyci=0.0f ;
static float *bc0z=NULL, *bc1z=NULL,             dzci=0.0f ;

static float *bq0x=NULL, *bq1x=NULL, *bq2x=NULL, dxqi=0.0f ;
static float *bq0y=NULL, *bq1y=NULL, *bq2y=NULL, dyqi=0.0f ;
static float *bq0z=NULL, *bq1z=NULL, *bq2z=NULL, dzqi=0.0f ;

static int    nbbcxyz = 0 ;
static float **bbbcar = NULL ;
static int    nbbqxyz = 0 ;
static float **bbbqar = NULL ;

/*--- local (small) warp over region we are optimizing ---*/

static IndexWarp3D *Hwarp   = NULL ;
static IndexWarp3D *AHwarp  = NULL ;
static int          need_AH = 1 ;
static int          Hflags  = 0 ;          /* flags for this patch */
static int          Hgflags = 0 ;          /* flags for global warp */
static int          Himeth  = MRI_LINEAR ; /* interpolation method for data */

static int Hibot,Hitop , Hjbot,Hjtop , Hkbot,Hktop ;  /* patch region indexes */

static int Hmatch_code  = 0 ;  /* how 'correlation' is computed */
static int Hbasis_code  = 0 ;  /* quintic or cubic? */

static double Hbasis_parmax = 0.0 ;  /* max warp parameter allowed */

static floatvec *Hmpar = NULL ;

/*--- Other stuff for incremental warping ---*/

#undef USE_HLOADER  /* define this for 'all-at-once' Hwarp load vs. incremental */
                    /* tests show incremental is about 10% faster, with OpenMP */

#ifdef USE_HLOADER
static void (*Hloader)(float *) = NULL ; /* function to make warp from params */
#endif

static int          Hnpar       = 0    ; /* num params for warp */
static int          Hnpar_sum   = 0    ; /* total num params used */
static float       *Hpar        = NULL ; /* param vector for warp */
static float       *Hxpar ;
static float       *Hypar ;
static float       *Hzpar ;
static int          Hdox ;
static int          Hdoy ;
static int          Hdoz ;
static int         *Hparmap     = NULL ;
static int          Hnparmap    = 0    ;
static int          Hnegate     = 0    ; /* negate correlation function? */
static int          Hnval       = 0    ; /* number of values in local patch */
static float       *Hwval       = NULL ; /* warped image values in local patch */
static float       *Haawt       = NULL ; /* weight iamge (sic) in local patch */
static float       *Hbval       = NULL ; /* base image in local patch */
static MRI_IMAGE   *Hsrcim      = NULL ; /* source image to warp (global) */
static MRI_IMAGE   *Hsrcim_blur = NULL ;
static float        Hblur_b     = 0.0f ;
static float        Hblur_s     = 0.0f ;
static int          Hforce      = 0    ;
static float        Hfactor     = 0.44f;
static float        Hshrink     = 0.749999f ;
static int          Hngmin      = 25 ;
static IndexWarp3D *Haawarp     = NULL ; /* initial warp we are modifying (global) */
static void        *Hincor      = NULL ; /* INCOR 'correlation' struct */
static MRI_IMAGE   *Haasrcim    = NULL ; /* warped source image (global) */

static MRI_IMAGE   *Hbasim      = NULL ; /* base image (global) */
static MRI_IMAGE   *Hbasim_blur = NULL ;
static MRI_IMAGE   *Hwtim       = NULL ; /* weight image (global) */
static float        Hwbar       = 0.0f ; /* average weight value */
static byte        *Hbmask      = NULL ; /* mask for base image (global) */
static byte        *Hemask      = NULL ; /* mask of voxels to EXCLUDE */

static float        Hstopcost   = -666666.6f ;
static int          Hstopped    = 0 ;

static int Hlev_start =   0 ;
static int Hlev_end   = 666 ;
static int Hlev_final =   0 ;
static int Hlev_now   =   0 ;
static int Hduplo     =   0 ;
static int Hfinal     =   0 ;
static int Hworkhard1 =   0 ;
static int Hworkhard2 =  -1 ;
static int Hfirsttime =   0 ;  /* for fun only */

static int Hsuperhard1 =  0 ;
static int Hsuperhard2 = -1 ;

#define ALLOW_QFINAL
#ifdef ALLOW_QFINAL
static int Hqfinal  = 0 ;  /* 07 May 2013 */
#else
# define   Hqfinal    0
#endif

#undef  WORKHARD
#define WORKHARD(lll) ( !Hduplo && (lll) >= Hworkhard1 && (lll) <= Hworkhard2 )

#undef  SUPERHARD
#define SUPERHARD(lll) ( !Hduplo && (lll) >= Hsuperhard1 && (lll) <= Hsuperhard2 )

static int Hnx=0,Hny=0,Hnz=0,Hnxy=0,Hnxyz=0 ;  /* dimensions of base image */

static float Hcost = 666.0f ;
static float Hpenn = 0.0f ;

static int Hverb = 1 ;

#undef  SRCIM
#define SRCIM ( (Hsrcim_blur != NULL ) ? Hsrcim_blur : Hsrcim )

#undef  BASIM
#define BASIM ( (Hbasim_blur != NULL ) ? Hbasim_blur : Hbasim )

/*----------------------------------------------------------------------------*/
/* Make the displacement flags coherent.  If impossible, return -1. */

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

/*----------------------------------------------------------------------------*/
/* C1 Hermite cubic basis functions over [-1..1].
   Scale factors are adjusted so that the functions' peak values are all 1.
   Return value is a float_pair comprising the 2 function values.
*//*--------------------------------------------------------------------------*/

static INLINE float_pair HCwarp_eval_basis( float x )
{
   register float aa , bb ; float_pair ee ;

   aa = fabsf(x) ;
   if( aa >= 1.0f ){
     ee.a = ee.b = 0.0f ;
   } else {
     bb = 1.0f - aa ; bb = bb*bb ;
     ee.a = bb * (1.0f+2.0f*aa) ;  /* f(0)  = 1 ; */
     ee.b = bb * x * 6.75f ;       /* f'(0) = 1 * 6.75 */
   }
   return ee ;
}

/*----------------------------------------------------------------------------*/
/* C2 Hermite quintic basis functions over [-1..1].
   Scale factors are adjusted so that the functions' peak values are all 1.
   Return value is a float_triple comprising the 3 function values.
*//*--------------------------------------------------------------------------*/

static INLINE float_triple HQwarp_eval_basis( float x )
{
   register float aa , bb , aq ; float_triple ee ;

   aa = fabsf(x) ;
   if( aa >= 1.0f ){
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
*//*--------------------------------------------------------------------------*/

/* Rational possibilities here are:
            ILEFT = 0    IRGHT = n-1
            ILEFT = -1/2 IRGHT = n-1/2
            ILEFT = -1   IRGHT = n
   Note that all the basis functions are 0 when evaluated at x=-1 or +1. */

#define ILEFT    -0.5f
#define IRGHT(n) ((n)-0.5f)

#define COMPUTE_CAB(n)                                                   \
  do{ cb = 2.0f / (IRGHT(n)-ILEFT) ; ca = -1.0f - cb*ILEFT ; } while(0)

/*----------------------------------------------------------------------------*/
/* Setup cubic basis arrays for each dimension */

void HCwarp_setup_basis( int nx , int ny , int nz , int flags )
{
   float_pair ee ; int ii ; float ca,cb,ccc ;

ENTRY("HCwarp_setup_basis") ;

   /* if not going to use all 3D displacements,
      create map from active set of parameters to total set of parameters:
        Hparmap[j] = index into list 0..23 of the 'true' parameter location */

   Hflags = IW3D_munge_flags(nx,ny,nz,flags) ;
   FREEIFNN(Hparmap) ;

   if( (Hflags & NWARP_NODISP_FLAG) != 0 ){
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
     Hnparmap = 24 ; Hparmap = NULL ;
   }

   /* cleanup old stuff */

   if( nx == nbcx      && ny == nbcy      && nz == nbcz      &&
       Hwarp != NULL   && AHwarp != NULL  &&
       nx == Hwarp->nx && ny == Hwarp->ny && nz == Hwarp->nz   ){
     IW3D_zero_fill(Hwarp) ; IW3D_zero_fill(AHwarp) ; EXRETURN ;
   }

   if(  Hwarp != NULL ){ IW3D_destroy( Hwarp);  Hwarp = NULL; }
   if( AHwarp != NULL ){ IW3D_destroy(AHwarp); AHwarp = NULL; }

   FREEIFNN(bc0x); FREEIFNN(bc1x); nbcx=0;
   FREEIFNN(bc0y); FREEIFNN(bc1y); nbcy=0;
   FREEIFNN(bc0z); FREEIFNN(bc1z); nbcz=0;

   if( bbbcar != NULL ){
     for( ii=0 ; ii < 8 ; ii++ ) FREEIFNN(bbbcar[ii]) ;
     free(bbbcar) ; nbbcxyz = 0 ; bbbcar = NULL ;
   }

   if( Hflags < 0 ) EXRETURN ;

   /* create new stuff */

   nbcx = nx ;
   bc0x = (float *)malloc(sizeof(float)*nbcx) ;
   bc1x = (float *)malloc(sizeof(float)*nbcx) ;
   nbcy = ny ;
   bc0y = (float *)malloc(sizeof(float)*nbcy) ;
   bc1y = (float *)malloc(sizeof(float)*nbcy) ;
   nbcz = nz ;
   bc0z = (float *)malloc(sizeof(float)*nbcz) ;
   bc1z = (float *)malloc(sizeof(float)*nbcz) ;

   nbcxy = nbcx*nbcy ;

   /* arrays for x direction */

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

   /* arrays for y direction */

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

   /* arrays for z direction */

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

   nbbcxyz = nbcx * nbcy * nbcz ;
   if( nbbcxyz <= 1048576 ){
     int jj , kk , qq ;
     bbbcar = (float **)malloc(sizeof(float *)*8) ;
     for( ii=0 ; ii < 8 ; ii++ )
       bbbcar[ii] = (float *)malloc(sizeof(float)*nbbcxyz) ;
     for( qq=kk=0 ; kk < nbcz ; kk++ ){
      for( jj=0 ; jj < nbcy ; jj++ ){
        for( ii=0 ; ii < nbcx ; ii++,qq++ ){
          bbbcar[0][qq] = bc0z[kk]*bc0y[jj]*bc0x[ii] ;
          bbbcar[1][qq] = bc1z[kk]*bc0y[jj]*bc0x[ii] ;
          bbbcar[2][qq] = bc0z[kk]*bc1y[jj]*bc0x[ii] ;
          bbbcar[3][qq] = bc1z[kk]*bc1y[jj]*bc0x[ii] ;
          bbbcar[4][qq] = bc0z[kk]*bc0y[jj]*bc1x[ii] ;
          bbbcar[5][qq] = bc1z[kk]*bc0y[jj]*bc1x[ii] ;
          bbbcar[6][qq] = bc0z[kk]*bc1y[jj]*bc1x[ii] ;
          bbbcar[7][qq] = bc1z[kk]*bc1y[jj]*bc1x[ii] ;
     }}}
   }

   /* create empty warp, to be populated in HCwarp_load,
      given these basis function arrays and the parameters */

   Hwarp  = IW3D_create(nbcx,nbcy,nbcz) ;
   AHwarp = IW3D_create(nbcx,nbcy,nbcz) ;

   EXRETURN ;
}

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

   nbbqxyz = nbqx * nbqy * nbqz ;
   if( nbbqxyz <= 524288 ){
     int jj , kk , qq ;
     bbbqar = (float **)malloc(sizeof(float *)*27) ;
     for( ii=0 ; ii < 27 ; ii++ )
       bbbqar[ii] = (float *)malloc(sizeof(float)*nbbqxyz) ;
     for( qq=kk=0 ; kk < nbqz ; kk++ ){
      for( jj=0 ; jj < nbqy ; jj++ ){
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

/*----------------------------------------------------------------------------*/

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

/*-----=====-----=====-----=====-----=====-----=====-----=====-----=====-----*/

static void HCwarp_eval_A( int qq , float *xx , float *yy , float *zz )
{
   int ii,jj,kk ;
   float b0zb0yb0x,b1zb0yb0x, b0zb1yb0x,b1zb1yb0x,
         b0zb0yb1x,b1zb0yb1x, b0zb1yb1x,b1zb1yb1x ;

   /* in this function, the 'b' values (3D warp components) are
      evaluated as tensor products of the underlying 1D functions */

   ii = qq % nbcx ; kk = qq / nbcxy ; jj = (qq-kk*nbcxy) / nbcx ;
   b0zb0yb0x = bc0z[kk]*bc0y[jj]*bc0x[ii]; b1zb0yb0x = bc1z[kk]*bc0y[jj]*bc0x[ii];
   b0zb1yb0x = bc0z[kk]*bc1y[jj]*bc0x[ii]; b1zb1yb0x = bc1z[kk]*bc1y[jj]*bc0x[ii];
   b0zb0yb1x = bc0z[kk]*bc0y[jj]*bc1x[ii]; b1zb0yb1x = bc1z[kk]*bc0y[jj]*bc1x[ii];
   b0zb1yb1x = bc0z[kk]*bc1y[jj]*bc1x[ii]; b1zb1yb1x = bc1z[kk]*bc1y[jj]*bc1x[ii];

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

static void HCwarp_eval_B( int qq , float *xx , float *yy , float *zz )
{
   float b0zb0yb0x,b1zb0yb0x, b0zb1yb0x,b1zb1yb0x,
         b0zb0yb1x,b1zb0yb1x, b0zb1yb1x,b1zb1yb1x ;

   /* in this function, the 'b' values (warp components) were
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

static void HQwarp_eval_A( int qq , float *xx , float *yy , float *zz )
{
   int ii,jj,kk ;
   float b0zb0yb0x,b1zb0yb0x, b2zb0yb0x,b0zb1yb0x, b1zb1yb0x,b2zb1yb0x,
         b0zb2yb0x,b1zb2yb0x, b2zb2yb0x,b0zb0yb1x, b1zb0yb1x,b2zb0yb1x,
         b0zb1yb1x,b1zb1yb1x, b2zb1yb1x,b0zb2yb1x, b1zb2yb1x,b2zb2yb1x,
         b0zb0yb2x,b1zb0yb2x, b2zb0yb2x,b0zb1yb2x, b1zb1yb2x,b2zb1yb2x,
         b0zb2yb2x,b1zb2yb2x, b2zb2yb2x ;

   ii = qq % nbqx ; kk = qq / nbqxy ; jj = (qq-kk*nbqxy) / nbqx ;

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

static void HQwarp_eval_B( int qq , float *xx , float *yy , float *zz )
{
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
   return ;
}

#endif /* USE_HLOADER */  /*HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH*/

/*----------------------------------------------------------------------------*/
/* Evaluate Hsrcim[ Haawarp(Hwarp(x)) ] into the val[] array.
     Note that Haawarp is a global warp for Hsrcim, whereas Hwarp is just
     a local patch.
   The val[] array contains the linearly interpolated warped image values over
     this patch.
   Also evaluate Haawarp[Hwarp(x)] into AHwarp for future utility.
     AHwarp is a local patch that fits into Haawarp later.
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

   if( Hsrcim == NULL || Haawarp == NULL || val == NULL || Hwarp == NULL ) EXRETURN ;

   hxd = Hwarp->xd  ; hyd = Hwarp->yd  ; hzd = Hwarp->zd  ; /* Hwarp delta */
   Axd = Haawarp->xd; Ayd = Haawarp->yd; Azd = Haawarp->zd; /* Haawarp */
   bxd = AHwarp->xd ; byd = AHwarp->yd ; bzd = AHwarp->zd ; /* AHwarp delta */

   if( Hbasis_code == MRI_CUBIC ){ nbx = nbcx ; nby = nbcy ; nbz = nbcz ; }
   else                          { nbx = nbqx ; nby = nbqy ; nbz = nbqz ; }
   nbxy = nbx*nby ; nbxyz = nbxy*nbz ;

#ifndef USE_HLOADER
   if( Hbasis_code == MRI_CUBIC ){
     Heval = (bbbcar == NULL) ? HCwarp_eval_A : HCwarp_eval_B ;
   } else {
     Heval = (bbbqar == NULL) ? HQwarp_eval_A : HQwarp_eval_B ;
   }
#endif

   nAx  = Haawarp->nx; nAy  = Haawarp->ny; nAz  = Haawarp->nz; nAxy = nAx*nAy;
   nAx1 = nAx-1      ; nAy1 = nAy-1      ; nAz1 = nAz-1      ;
   nAxh = nAx-0.501f ; nAyh = nAy-0.501f ; nAzh = nAz-0.501f ;

   sar = MRI_FLOAT_PTR(SRCIM) ;  /* source image array */

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

     if( !need_val && !need_AH ){ val[qq] = 0.0f; continue; }

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

     if( !need_val ){ val[qq] = 0.0f; continue; }

     xq = bxd[qq]+ii+Hibot ; yq = byd[qq]+jj+Hjbot ; zq = bzd[qq]+kk+Hkbot ;

     /** ABOVE: since Awarp_x[x,y,z] = x + Axd, then
           Awarp_x[ Hwarp(x,y,z) ] = Hwarp_x(x,y,z) + Axd(interpolated)
                                   = Hibot + ii + hxd + Axd(interpolated)
         so the above formula for xq includes not just the interpolated
         values from Axd (the first 2 terms) but the Hwarp stuff again, also */

     /* interpolate at (xq,yq,zq) indexes in sar to get val output */

     if( xq < -0.499f ) xq = -0.499f; else if( xq > nAxh ) xq = nAxh;
     if( yq < -0.499f ) yq = -0.499f; else if( yq > nAyh ) yq = nAyh;
     if( zq < -0.499f ) zq = -0.499f; else if( zq > nAzh ) zq = nAzh;

     if( Himeth == MRI_NN ){             /* special case of NN interp of data */
       ix_00   = (int)(xq+0.5f) ;
       jy_00   = (int)(yq+0.5f) ;
       kz_00   = (int)(zq+0.5f) ;
       val[qq] = sar[IJK(ix_00,jy_00,kz_00)] ;
     } else {                                 /* normal case of linear interp */
       ix = floorf(xq) ;  fx = xq - ix ;
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

   } /* end of for loop */
 } /* end of parallel stuff */
AFNI_OMP_END ;

   AFNI_do_nothing() ; /* fprintf(stderr,"B") ; */

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/

#define Hpen_fbase 0.00666

static double Hpen_fac = Hpen_fbase ;
static double Hpen_sum = 0.0 ;
static int    Hpen_num = 0 ;
static int    Hpen_use = 1 ;

/*----------------------------------------------------------------------------*/

double HPEN_penalty(void)
{
   double hsum ;
   hsum = Hpen_sum + (double)IW3D_load_energy(AHwarp) ;
   if( hsum > 0.0 ) hsum = Hpen_fac * pow( hsum , 0.25 ) ;
   return hsum ;
}

/*----------------------------------------------------------------------------*/

static INLINE int is_float_array_constant( int n , float *v )
{
   int ii ;
   for( ii=1 ; ii < n && v[ii] == v[0] ; ii++ ) ; /*nada*/
   return (ii==n) ;
}

/*----------------------------------------------------------------------------*/
/* Function to actually minimize (via Powell's NEWUOA) */

double IW3D_scalar_costfun( int npar , double *dpar )
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

   Hwarp_apply(Hwval) ;

#if 0
  if( is_float_array_constant(Hnval,Hwval) )
    fprintf(stderr," costfun: Hwval is constant %g\n",Hwval[0]) ;
#endif

   /* compute the rest of the cost function */

   cost = INCOR_evaluate( Hincor , Hnval ,
                          (Hbval != NULL ) ? Hbval : MRI_FLOAT_PTR(Hbasim),
                          Hwval ,
                          (Haawt != NULL ) ? Haawt : MRI_FLOAT_PTR(Hwtim) ) ;
   if( Hnegate ) cost = -cost ;

   if( !isfinite(cost) ){
     ERROR_message("Warpomatic cost = %g -- input parameters:",cost) ;
     for( ii=0 ; ii < npar ; ii++ ) fprintf(stderr," %g",dpar[ii]) ;
     fprintf(stderr,"\n") ;
   }

   if( Hpen_use ){
     Hpenn = HPEN_penalty() ; cost += Hpenn ;  /* penalty is saved in Hpenn */
   } else {
     Hpenn = 0.0f ;
   }

   if( Hfirsttime ){
     fprintf(stderr,"[first cost=%.3f]%c",cost , ((Hverb>1) ? '\n' : ' ') ) ;
     Hfirsttime = 0 ;
   }

   return cost ;
}

/*----------------------------------------------------------------------------*/
/* Delete various workspaces */

void IW3D_cleanup_improvement(void)
{
ENTRY("IW3D_cleanup_improvement") ;

   mri_free(Hbasim)   ; Hbasim   = NULL ;
   mri_free(Hsrcim)   ; Hsrcim   = NULL ;
   mri_free(Hwtim)    ; Hwtim    = NULL ; FREEIFNN(Hbmask) ;
   mri_free(Haasrcim) ; Haasrcim = NULL ;

   mri_free(Hsrcim_blur) ; Hsrcim_blur = NULL ;

   IW3D_destroy(Hwarp)   ; Hwarp   = NULL ;
   IW3D_destroy(AHwarp)  ; AHwarp  = NULL ;
   IW3D_destroy(Haawarp) ; Haawarp = NULL ;

   INCOR_destroy(Hincor) ; Hincor = NULL ; KILL_floatvec(Hmpar) ;
   FREEIFNN(Hpar) ; FREEIFNN(Hwval) ; FREEIFNN(Haawt) ; FREEIFNN(Hbval) ;
   FREEIFNN(Hparmap) ; Hnparmap = Hnpar = 0 ; Hbasis_code = -666 ;

   FREEIFNN(bc0x); FREEIFNN(bc1x); nbcx=0;
   FREEIFNN(bc0y); FREEIFNN(bc1y); nbcy=0;
   FREEIFNN(bc0z); FREEIFNN(bc1z); nbcz=0;

   FREEIFNN(bq0x); FREEIFNN(bq1x); FREEIFNN(bq2x); nbqx=0;
   FREEIFNN(bq0y); FREEIFNN(bq1y); FREEIFNN(bq2y); nbqy=0;
   FREEIFNN(bq0z); FREEIFNN(bq1z); FREEIFNN(bq2z); nbqz=0;

   if( bbbcar != NULL ){
     int ii ;
     for( ii=0 ; ii < 8 ; ii++ ) FREEIFNN(bbbcar[ii]) ;
     free(bbbcar) ; nbbcxyz = 0 ; bbbcar = NULL ;
   }

   if( bbbqar != NULL ){
     int ii ;
     for( ii=0 ; ii < 27 ; ii++ ) FREEIFNN(bbbqar[ii]) ;
     free(bbbqar) ; nbbqxyz = 0 ; bbbqar = NULL ;
   }

   Hstopcost = -666666.6f ;
   Hstopped  = 0 ;
   Hfinal    = 0 ;

   EXRETURN ;
}

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

   /*-- eliminate old stuff --*/

   IW3D_cleanup_improvement() ;

   /*-- copy base and source images --*/

   Hnx = bim->nx; Hny = bim->ny; Hnz = bim->nz; Hnxy=Hnx*Hny; Hnxyz = Hnxy*Hnz;
   Hbasim = mri_to_float(bim) ;
   Hsrcim = mri_to_float(sim);

   if( Hblur_s >= 0.5f ){
     if( Hverb > 1 ) ININFO_message("   blurring source image %.3g voxels FWHM",Hblur_s) ;
     Hsrcim_blur = mri_float_blur3D( FWHM_TO_SIGMA(Hblur_s) , Hsrcim ) ;
   } else if( Hblur_s <= -1.0f ){
     if( Hverb > 1 ) ININFO_message("   median-izing source image %.3g voxels",-Hblur_s) ;
     Hsrcim_blur = mri_medianfilter( Hsrcim , -Hblur_s , NULL , 0 ) ;
   } else {
     Hsrcim_blur = NULL ;
   }

   /*-- and copy or create base weight image --*/

   if( wbim != NULL ){               /*-- user supplied weight --*/

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
     if( Hverb > 1 ) ININFO_message(   "%d voxels in mask (out of %d)",nwb,Hnxyz) ;
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
       ERROR_exit("IW3D_setup_for_improvement: all zero mask!?") ;
     Hwbar /= nwb ;  /* average value of all nonzero weights */
     nmask = nwb ;
     if( nexc > 0 ) ININFO_message("-emask excluded %d voxels",nexc) ;

   }

   /*-- set operating codes --*/

   Hmatch_code = meth_code ; iii = INCOR_check_meth_code(meth_code) ;
   if( iii == 0 )
     ERROR_exit("IW3D_setup_for_improvement: bad meth_code input=%d",meth_code) ;

   switch( meth_code ){
     default:                           Hnegate = 0 ; break ;

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
     bar = MRI_FLOAT_PTR(Hbasim) ; sar = MRI_FLOAT_PTR(Hsrcim) ;
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
       if( Hverb > 1 ){
         ININFO_message("   2Dhist: nbin=%d",(int)Hmpar->ar[0]) ;
         ININFO_message("           xbot=%g xcbot=%g xctop=%g xtop=%g",
                        Hmpar->ar[1], Hmpar->ar[5], Hmpar->ar[6], Hmpar->ar[2] ) ;
         ININFO_message("           ybot=%g ycbot=%g yctop=%g ytop=%g",
                        Hmpar->ar[3], Hmpar->ar[7], Hmpar->ar[8], Hmpar->ar[4] ) ;
       }
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
   }

   Hgflags = IW3D_munge_flags(Hnx,Hny,Hnz,warp_flags) ;
   if( Hgflags < 0 )
     ERROR_exit("IW3D_setup_for_improvement: bad warp_flags input") ;

   /*-- copy/create initial warp, and warp the source image --*/

   if( Iwarp != NULL ){
     if( Iwarp->nx != Hnx || Iwarp->ny != Hny || Iwarp->nz != Hnz )
       ERROR_exit("IW3D_setup_for_improvement: bad Iwarp input") ;

     Haawarp  = IW3D_copy(Iwarp,1.0f) ;     /* copy it */
     Haasrcim = IW3D_warp_floatim( Haawarp, SRCIM, Himeth , 1.0f ) ;
   } else {
     Haawarp  = IW3D_create(Hnx,Hny,Hnz) ;  /* initialize to 0 displacements */
     Haasrcim = mri_to_float(SRCIM) ;       /* 'warped' source image */
   }
   (void)IW3D_load_energy(Haawarp) ;  /* initialize energy field for penalty use */

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/
/* Given a global warp Haawarp, improve it locally over a rectangular patch.
   Also, keep up-to-date the copy of the warped source image Haasrcim.        */

int IW3D_improve_warp( int warp_code ,
                       int ibot, int itop, int jbot, int jtop, int kbot, int ktop )
{
   MRI_IMAGE *warpim ;
   int nxh,nyh,nzh , ii,jj,kk , iter,itmax,qq,pp , nwb ;
   float *wbfar , wsum ; double prad ;
   double *parvec, *xbot,*xtop ;
   float *sar , *Axd,*Ayd,*Azd,*Aje,*Ase , *bxd,*byd,*bzd,*bje,*bse , jt,st ;

ENTRY("IW3D_improve_warp") ;

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

   wbfar = MRI_FLOAT_PTR(Hwtim) ; wsum = 0.0f ;
   for( nwb=0,kk=kbot ; kk <= ktop ; kk++ ){
     for( jj=jbot ; jj <= jtop ; jj++ ){
       for( ii=ibot ; ii <= itop ; ii++ ){
         qq = ii + jj*Hnx + kk*Hnxy ;
         if( Hbmask[qq] ){ wsum += wbfar[qq] ; nwb++ ; }
   }}}
   if( !Hforce && nwb < 0.369f*Hnval || wsum < 0.246f*Hnval*Hwbar ){ /* too light for us */
     if( Hverb > 1 )
       ININFO_message(
         "     %s patch %03d..%03d %03d..%03d %03d..%03d : skipping (%.1f%% inmask %.1f%% weight)" ,
                       (warp_code == MRI_QUINTIC) ? "quintic" : "  cubic" ,
                       ibot,itop, jbot,jtop, kbot,ktop ,
                       (100.0f*nwb)/Hnval , (100.0f*wsum)/(Hnval*Hwbar) ) ;
     RETURN(0) ;
   }

   /*-- setup the basis functions for Hwarping --*/

   switch( warp_code ){
     default:
     case MRI_CUBIC:
       Hbasis_code   = MRI_CUBIC ;                   /* 3rd order polynomials */
       Hbasis_parmax = 0.033*Hfactor ;    /* max displacement from 1 function */
       Hnpar         = 24 ;                /* number of params for local warp */
       prad          = 0.333 ;                       /* NEWUOA initial radius */
       HCwarp_setup_basis( nxh,nyh,nzh, Hgflags ) ;      /* setup HCwarp_load */
#ifdef USE_HLOADER
       Hloader       = HCwarp_load ;   /* func to make local warp from params */
#endif
     break ;

     case MRI_QUINTIC:
       Hbasis_code   = MRI_QUINTIC ;                 /* 5th order polynomials */
       Hbasis_parmax = 0.007*Hfactor ;
       Hnpar         = 81 ;
       prad          = 0.222 ;
       HQwarp_setup_basis( nxh,nyh,nzh, Hgflags ) ;
#ifdef USE_HLOADER
       Hloader       = HQwarp_load ;
#endif
     break ;
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

   Hwval = (float *)realloc(Hwval,sizeof(float)*Hnval) ;

   /*-- setup to do incremental 'correlation' on the local region --*/

   INCOR_destroy(Hincor) ;
   Hincor = INCOR_create( Hmatch_code , Hmpar ) ;

   FREEIFNN(Haawt) ; FREEIFNN(Hbval) ;

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
     float *bar  =MRI_FLOAT_PTR(Hbasim) ;

     Haawt = (float *)malloc(sizeof(float)*Hnval) ;
     Hbval = (float *)malloc(sizeof(float)*Hnval) ;
     for( pp=0,kk=kbot ; kk <= ktop ; kk++ ){      /* extract weights  */
       for( jj=jbot ; jj <= jtop ; jj++ ){         /* and base image   */
         for( ii=ibot ; ii <= itop ; ii++,pp++ ){  /* for patch region */
           qq        = ii + jj*Hnx + kk*Hnxy ;
           Haawt[pp] = wbfar[qq] ;  /* copy weight image vals */
           Hbval[pp] =   bar[qq] ;  /* copy base image vals */
           wbfar[qq] = 0.0f ;       /* 0 out temp weight */
     }}}

     if( is_float_array_constant(Hnval,Hbval) ){
       if( Hverb > 1 )
         ININFO_message(
           "     %s patch %03d..%03d %03d..%03d %03d..%03d : skipping (base=const=%g)" ,
                         (warp_code == MRI_QUINTIC) ? "quintic" : "  cubic" ,
                         ibot,itop, jbot,jtop, kbot,ktop , Hbval[0] ) ;
       RESTORE_WBFAR ; RETURN(0) ;
     }

     /* initialize the 'correlation' from the data that won't
        be changing (i.e., data from outside the local patch) */

     INCOR_addto( Hincor , Hnxyz ,
                  MRI_FLOAT_PTR(Hbasim) , MRI_FLOAT_PTR(Haasrcim) , wbfar ) ;
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
                             prad,0.009*prad , itmax , IW3D_scalar_costfun ) ;

   if( iter > 0 ) Hnpar_sum += Hnparmap ;

   if( Hverb > 3 ) powell_set_verbose(0) ;

   /***** cleanup and exit phase ***/

   free(xtop) ; free(xbot) ;

   if( iter <= 0 ){ free(parvec); RETURN(0); }  /* something bad happened */

   /* load optimized warped image and warp into their patches */

   need_AH = 1 ;
   Hcost = IW3D_scalar_costfun( Hnparmap , parvec ) ;  /* evaluate at current results */
   (void)IW3D_load_energy(AHwarp) ;

   /* AHwarp gets loaded into Haawarp and Hwval into Haasrcim */

   sar = MRI_FLOAT_PTR(Haasrcim) ;
   Axd = Haawarp->xd; Ayd = Haawarp->yd; Azd = Haawarp->zd; Aje = Haawarp->je; Ase = Haawarp->se;
   bxd = AHwarp->xd ; byd = AHwarp->yd ; bzd = AHwarp->zd ; bje = AHwarp->je ; bse = AHwarp->se ;

   jt= bje[0] ; st = bse[0] ;
   for( pp=0,kk=kbot ; kk <= ktop ; kk++ ){
     for( jj=jbot ; jj <= jtop ; jj++ ){
       for( ii=ibot ; ii <= itop ; ii++,pp++ ){
         qq = ii + jj*Hnx + kk*Hnxy ;
         sar[qq] = Hwval[pp] ;
         Axd[qq] = bxd[pp] ; Ayd[qq] = byd[pp] ; Azd[qq] = bzd[pp] ;
         Aje[qq] = bje[pp] ; Ase[qq] = bse[pp] ;
         if( Aje[qq] > jt ) jt = Aje[qq] ;
         if( Ase[qq] > st ) st = Ase[qq] ;
   }}}

   if( Hverb > 1 ){
     ININFO_message(
       "     %s patch %03d..%03d %03d..%03d %03d..%03d : cost=%g iter=%d : energy=%.3f:%.3f pen=%g",
                     (Hbasis_code == MRI_QUINTIC) ? "quintic" : "  cubic" ,
                           ibot,itop, jbot,jtop, kbot,ktop , Hcost  , iter , jt,st , Hpenn ) ;
   } else if( Hverb == 1 && (Hlev_now<=2 || lrand48()%Hlev_now==0) ){
     fprintf(stderr,".") ;
   }

   /* ZOMG -- let's vamoose */

   free(parvec) ; RETURN(iter) ;
}

/*----------------------------------------------------------------------------*/
/**---------- The function that drives the warp searching process! ----------**/
/*----------------------------------------------------------------------------*/

static IndexWarp3D *WO_iwarp = NULL ;
static int         *WO_ilev  = 0 ;

void (*iterfun)(char *,MRI_IMAGE *) = NULL ;

#define ITEROUT(lll)                                                                    \
 do{ if( iterfun != NULL ){                                                              \
       MRI_IMAGE *outim = IW3D_warp_floatim(Haawarp,Hsrcim,MRI_WSINC5,1.0f) ;             \
       char str[256]; sprintf(str,"lev=%d",lll) ;                                          \
       iterfun(str,outim) ; mri_free(outim) ;                                               \
       ININFO_message("  ---QSAVE(%s) -- %s",str,nice_time_string(NI_clock_time())) ;        \
       if( lll == 0 || nlevr < 2 )                                                            \
         ININFO_message("   --(%s)-- final cost = %g",str,Hcostend) ;                          \
       else                                                                                     \
         ININFO_message("   --(%s)-- middle cost = %g  final cost = %g",str,Hcostmid,Hcostend) ; \
   } } while(0)

/*----------------------------------------------------------------------------*/

IndexWarp3D * IW3D_warpomatic( MRI_IMAGE *bim, MRI_IMAGE *wbim, MRI_IMAGE *sim,
                               int meth_code, int warp_flags                   )
{
   int lev,levs , xwid,ywid,zwid , xdel,ydel,zdel , iter ;
   int ibot,itop,idon , jbot,jtop,jdon , kbot,ktop,kdon , dox,doy,doz , iii ;
   IndexWarp3D *OutWarp ;
   float flev , glev , Hcostold , Hcostmid=0.0f,Hcostend=0.0f ;
   int imin,imax , jmin,jmax, kmin,kmax , ibbb,ittt , jbbb,jttt , kbbb,kttt ;
   int dkkk,djjj,diii , ngmin=0 , levdone=0 ;
   int qmode=MRI_CUBIC , nlevr , nsup,isup ;

ENTRY("IW3D_warpomatic") ;

   if( Hverb ) Hfirsttime = 1 ;

   IW3D_setup_for_improvement( bim, wbim, sim, WO_iwarp, meth_code, warp_flags ) ;

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

   if( Hverb ){
         INFO_message("AFNI warpomatic start: %d x %d x %d volume",Hnx,Hny,Hnz) ;
       ININFO_message("            autobbox = %d..%d %d..%d %d..%d",imin,imax,jmin,jmax,kmin,kmax) ;
   }

   if( Hlev_start == 0 ){            /* top level = global warps */
     nlevr = ( WORKHARD(0) || Hduplo ) ? 4 : 2 ; if( SUPERHARD(0) ) nlevr++ ;
     Hforce = 1 ; Hfactor = 1.0f ; Hpen_use = 0 ; Hlev_now = 0 ;
     if( Hverb == 1 ) fprintf(stderr,"lev=0 %d..%d %d..%d %d..%d: ",ibbb,ittt,jbbb,jttt,kbbb,kttt) ;
     for( iii=0 ; iii < nlevr ; iii++ ){
       (void)IW3D_improve_warp( MRI_CUBIC  , ibbb,ittt,jbbb,jttt,kbbb,kttt );
       Hcostold = Hcost ;
       (void)IW3D_improve_warp( MRI_QUINTIC, ibbb,ittt,jbbb,jttt,kbbb,kttt );
       if( iii > 0 && iii < nlevr-1 && Hcostold-Hcost < 0.005f ){
         if( Hverb > 1 )
           ININFO_message("       --> too little improvement: breaking out of WORKHARD iterates") ;
         break ;
       }
     }
     if( Hverb == 1 ) fprintf(stderr," done [cost=%.3f]\n",Hcost) ;
   } else {
     Hcost = 666.666f ;  /* a beastly thing to do */
   }
   Hforce = 0 ; Hlev_final = 0 ; Hpen_use = (Hpen_fac > 0.0f) ;
   Hcostmid = Hcostend = Hcost ;

   if( !Hduplo ) ITEROUT(0) ;

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
     Hfactor = 1.0f ;
#endif

     qmode = MRI_CUBIC ;
#ifdef ALLOW_QFINAL
     if( levdone && !Hduplo && Hqfinal ) qmode = MRI_QUINTIC ;
#endif

     (void)IW3D_load_energy(Haawarp) ;  /* initialize energy field for penalty use */

     nlevr = WORKHARD(lev)  ? 2 : 1 ;
     nsup  = SUPERHARD(lev) ? 2 : 1 ;

     if( Hverb > 1 )
       ININFO_message("  .........  lev=%d xwid=%d ywid=%d zwid=%d Hfac=%g %s %s" ,
                      lev,xwid,ywid,zwid,Hfactor , (levdone   ? "FINAL"  : "\0") ,
                                                   (nlevr > 1 ? "WORKHARD" : "\0") ) ;
     else if( Hverb == 1 )
       fprintf(stderr,"lev=%d patch=%dx%dx%d: ",lev,xwid,ywid,zwid) ;

     /* alternate the direction of sweeping at different levels */

     if( lev%2 == 1 || nlevr > 1 ){  /* bot to top, ijk */
      for( isup=0 ; isup < nsup ; isup++ ){  /* superhard? */
       for( kdon=0,kbot=ibbb ; !kdon ; kbot += dkkk ){
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
             iter = IW3D_improve_warp( qmode  , ibot,itop , jbot,jtop , kbot,ktop ) ;
#if 0
             if( Hcost > Hcostold+0.001f ){
               if( Hverb > 1 ) ININFO_message(" -- rerun --") ;
               iter = IW3D_improve_warp( MRI_CUBIC  , ibot,itop , jbot,jtop , kbot,ktop ) ;
             }
#if 0
             else if( iter > 144 && qmode > 0 && Hcostold-Hcost > 0.00002f )
               iter = IW3D_improve_warp( qmode    , ibot,itop , jbot,jtop , kbot,ktop ) ;
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
       if( nlevr > 1 && Hverb == 1 ) fprintf(stderr,":[cost=%.3f]:",Hcost) ;
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
             iter = IW3D_improve_warp( qmode  , ibot,itop , jbot,jtop , kbot,ktop ) ;
#if 0
             if( Hcost > Hcostold+0.001f ){
               if( Hverb > 1 ) ININFO_message(" -- rerun --") ;
               iter = IW3D_improve_warp( MRI_CUBIC  , ibot,itop , jbot,jtop , kbot,ktop ) ;
             }
#if 0
             else if( iter > 144 && qmode > 0 && Hcostold-Hcost > 0.00002f )
               iter = IW3D_improve_warp( qmode    , ibot,itop , jbot,jtop , kbot,ktop ) ;
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

     if( Hverb == 1 ) fprintf(stderr," done [cost=%.3f]\n",Hcost) ;

     if( !Hduplo ) ITEROUT(lev) ;

   } /*-- end of loop over levels of refinement --*/

DoneDoneDone:  /* breakout */

   OutWarp = IW3D_copy( Haawarp , 1.0f ) ;
   IW3D_cleanup_improvement() ;

   RETURN(OutWarp) ;
}

/*----------------------------------------------------------------------------*/

static void *S2BIM_iwarp = NULL ;
static int   S2BIM_ilev  = 0 ;
static int   S2BIM_mlev  = 666 ;

Image_plus_Warp * IW3D_warp_s2bim( MRI_IMAGE *bim , MRI_IMAGE *wbim , MRI_IMAGE *sim,
                                   int interp_code , int meth_code , int warp_flags  )
{
   IndexWarp3D *Swarp ;
   MRI_IMAGE *outim ;
   Image_plus_Warp *imww ;

ENTRY("IW3D_warp_s2bim") ;

   WO_iwarp = S2BIM_iwarp ; Hlev_start = S2BIM_ilev ; Hlev_end = S2BIM_mlev ;
   Hnpar_sum = 0 ; Hduplo = 0 ;

   Hshrink = AFNI_numenv("AFNI_WARPOMATIC_SHRINK") ;
   if( Hshrink > 1.0f                       ) Hshrink = 1.0f / Hshrink ;
   if( Hshrink < 0.444f || Hshrink > 0.888f ) Hshrink = 0.749999f ;
   else                                       ININFO_message("  -- Hshrink set to %.6f",Hshrink) ;

   Swarp = IW3D_warpomatic( bim , wbim , sim , meth_code , warp_flags ) ;

   outim = IW3D_warp_floatim( Swarp, sim , interp_code , 1.0f ) ;

   imww       = (Image_plus_Warp *)malloc(sizeof(Image_plus_Warp)) ;
   imww->im   = outim ;
   imww->warp = Swarp ;

   RETURN(imww) ;
}

/*----------------------------------------------------------------------------*/
/*** NOT READY YET ***/

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

   for( kk=0 ; kk < nzb ; kk++ ){
    for( jj=0 ; jj < nyb ; jj++ ){
      for( ii=0 ; ii < nxb ; ii++ ){
        FSUB(xdb,ii,jj,kk,nxb,nxyb) = 0.5f*FSUB(xda,2*ii,2*jj,2*kk,nxa,nxya) ;
        FSUB(ydb,ii,jj,kk,nxb,nxyb) = 0.5f*FSUB(yda,2*ii,2*jj,2*kk,nxa,nxya) ;
        FSUB(zdb,ii,jj,kk,nxb,nxyb) = 0.5f*FSUB(zda,2*ii,2*jj,2*kk,nxa,nxya) ;
   }}}
   if( AA->use_emat ){
     BB->emat = AA->emat ; MAT33_SCALE(BB->emat,0.5f) ; BB->use_emat = 1 ;
   }

   return BB ;
}

/*---------------------------------------------------------------------------*/
/* Make a double-size copy of a warp (scaling displacements by 2.0 as well). */

IndexWarp3D * IW3D_duplo_up( IndexWarp3D *AA , int xadd,int yadd,int zadd)
{
   IndexWarp3D *BB ;
   int nxa,nya,nza , nxb,nyb,nzb , nxya,nxyb , ii,jj,kk , im,jm,km,ip,jp,kp ;
   float *xda, *yda, *zda , *xdb, *ydb, *zdb ;

   nxa = AA->nx ; nya = AA->ny ; nza = AA->nz  ;

   nxb = (nxa == 1) ? 1 : (2*nxa+(xadd != 0)) ;
   nyb = (nya == 1) ? 1 : (2*nya+(yadd != 0)) ;
   nzb = (nza == 1) ? 1 : (2*nza+(zadd != 0)) ;

   BB = IW3D_create(nxb,nyb,nzb) ; if( BB == NULL ) return NULL ;

   xda = AA->xd ; yda = AA->yd ; zda = AA->zd ; nxya = nxa*nya ;
   xdb = BB->xd ; ydb = BB->yd ; zdb = BB->zd ; nxyb = nxb*nyb ;

   /* in the following:
        note that linear interpolation would be a scale factor of 0.125
        (from 8 points), then the doubling is the factor of 0.250 = 0.125 * 2  */

   for( kk=0 ; kk < nzb ; kk++ ){
    kp = km = kk/2 ; if( kk%2 ){ kp++; if( kp >= nza ) kp = nza-1; }
    for( jj=0 ; jj < nyb ; jj++ ){
      jp = jm = jj/2 ; if( jj%2 ){ jp++; if( jp >= nya ) jp = nya-1; }
      for( ii=0 ; ii < nxb ; ii++ ){
        ip = im = ii/2 ; if( ii%2 ){ ip++; if( ip >= nxa ) ip = nxa-1; }
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
   if( AA->use_emat ){
     BB->emat = AA->emat ; MAT33_SCALE(BB->emat,2.0f) ; BB->use_emat = 1 ;
   }

   return BB ;
}

/*----------------------------------------------------------------------------*/

static void blur_inplace( MRI_IMAGE *fim , float fwhm )
{
   float sig = FWHM_TO_SIGMA(fwhm) ;
   FIR_blur_volume_3d( fim->nx,fim->ny,fim->nz , 1.0f,1.0f,1.0f ,
                       MRI_FLOAT_PTR(fim)  , sig,sig,sig         ) ;
   return ;
}

/*---------------------------------------------------------------------------*/

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

/*----------------------------------------------------------------------------*/

Image_plus_Warp * IW3D_warp_s2bim_duplo( MRI_IMAGE *bim , MRI_IMAGE *wbim , MRI_IMAGE *sim,
                                         int interp_code , int meth_code , int warp_flags  )
{
   IndexWarp3D *Swarp , *Dwarp ;
   MRI_IMAGE *outim ;
   MRI_IMAGE *bimd , *wbimd , *simd ;
   int nx,ny,nz , Htemp1, Htemp2 , ct ;
   Image_plus_Warp *imww ;
   byte *emask_big=NULL ; MRI_IMAGE *embim=NULL , *emsim=NULL ;

ENTRY("IW3D_warp_s2bim_duplo") ;

   ct = NI_clock_time() ;
   if( Hverb ) INFO_message("=== Duplo down (blurring volumes & resampling)") ;

   WO_iwarp = NULL ;               /* can't start with initial warp for duplo */
   nx = bim->nx ; ny = bim->ny ; nz = bim->nz ;
   bimd  = mri_duplo_down_3D(bim) ;   blur_inplace( bimd , 1.234f ) ;
   wbimd = mri_duplo_down_3D(wbim) ;
   simd  = mri_duplo_down_3D(sim) ;   blur_inplace( simd , 1.234f ) ;

   Hshrink    = 0.749999f ;
   Hlev_start = 0 ;
   Hpen_fac  *= 8.0f ;
   Hduplo     = 1 ; Hnpar_sum = 0 ;

   if( Hemask != NULL ){
     embim = mri_new_vol_empty(nx,ny,nz,MRI_byte) ; emask_big = Hemask ;
     mri_fix_data_pointer(emask_big,embim) ;
     emsim = mri_duplo_down_3Dmask(embim) ;
     Hemask = MRI_BYTE_PTR(emsim) ;
     mri_clear_data_pointer(embim) ; mri_free(embim) ;
   }

   Dwarp = IW3D_warpomatic( bimd , wbimd , simd , meth_code , warp_flags ) ;

   Hpen_fac  /= 8.0f ;
   Hduplo     = 0 ;

   mri_free(simd) ; mri_free(wbimd) ; mri_free(bimd) ;

   if( Hemask != NULL ){ mri_free(emsim) ; Hemask = emask_big ; }

   if( Dwarp == NULL ) RETURN(NULL) ;

   if( Hverb )
     INFO_message("=== Duplo up (clock = %s)",nice_time_string(NI_clock_time()-ct)) ;

   WO_iwarp = IW3D_duplo_up( Dwarp, nx%2 , ny%2 , nz%2 ) ;
   IW3D_destroy(Dwarp) ;

   Hshrink = 0.749999f ; Hlev_start = Hlev_final /*-1*/ ; if( Hlev_start < 0 ) Hlev_start = 0 ;
   Htemp1 = Hworkhard1 ; Hworkhard1 = 0 ;
   Htemp2 = Hworkhard2 ; Hworkhard2 = MAX(Htemp2,Hlev_start) ;
   Swarp = IW3D_warpomatic( bim , wbim , sim , meth_code , warp_flags ) ;
   IW3D_destroy(WO_iwarp) ; WO_iwarp = NULL ; Hlev_start = 0 ;
   Hworkhard1 = Htemp1 ; Hworkhard2 = Htemp2 ;

   outim = IW3D_warp_floatim( Swarp, sim , interp_code , 1.0f ) ;

   imww       = (Image_plus_Warp *)malloc(sizeof(Image_plus_Warp)) ;
   imww->im   = outim ;
   imww->warp = Swarp ;

   RETURN(imww) ;
}

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

   if( Hbasis_code == MRI_CUBIC ){ nbx = nbcx ; nby = nbcy ; nbz = nbcz ; }
   else                          { nbx = nbqx ; nby = nbqy ; nbz = nbqz ; }
   nbxy = nbx*nby ; nbxyz = nbxy*nbz ;

#ifndef USE_HLOADER
   if( Hbasis_code == MRI_CUBIC ){
     Heval = (bbbcar == NULL) ? HCwarp_eval_A : HCwarp_eval_B ;
   } else {
     Heval = (bbbqar == NULL) ? HQwarp_eval_A : HQwarp_eval_B ;
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
     ERROR_message("Warpomatic cost = %g -- input parameters:",cost) ;
     for( ii=0 ; ii < npar ; ii++ ) fprintf(stderr," %g",dpar[ii]) ;
     fprintf(stderr,"\n") ;
   }

   if( Hpen_use ){
     Hpenn = HPEN_penalty() ; cost += Hpenn ;  /* penalty is saved in Hpenn */
   } else {
     Hpenn = 0.0f ;
   }

   if( Hfirsttime ){
     fprintf(stderr,"[first cost=%.3f]%c",cost , ((Hverb>1) ? '\n' : ' ') ) ;
     Hfirsttime = 0 ;
   }

   return cost ;
}

/*----------------------------------------------------------------------------*/

int IW3D_improve_warp_plusminus( int warp_code ,
                                 int ibot, int itop,
                                 int jbot, int jtop, int kbot, int ktop )
{
   MRI_IMAGE *warpim ;
   int nxh,nyh,nzh , ii,jj,kk , iter,itmax,qq,pp , nwb ;
   float *wbfar , wsum ; double prad ;
   double *parvec, *xbot,*xtop ;
   float *sarp,*sarm , *Axd,*Ayd,*Azd,*Aje,*Ase , *bxd,*byd,*bzd,*bje,*bse , jt,st ;

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

   wbfar = MRI_FLOAT_PTR(Hwtim) ; wsum = 0.0f ;
   for( nwb=0,kk=kbot ; kk <= ktop ; kk++ ){
     for( jj=jbot ; jj <= jtop ; jj++ ){
       for( ii=ibot ; ii <= itop ; ii++ ){
         qq = ii + jj*Hnx + kk*Hnxy ;
         if( Hbmask[qq] ){ wsum += wbfar[qq] ; nwb++ ; }
   }}}
   if( !Hforce && nwb < 0.369f*Hnval || wsum < 0.246f*Hnval*Hwbar ){ /* too light for us */
     if( Hverb > 1 )
       ININFO_message(
         "     %s patch %03d..%03d %03d..%03d %03d..%03d : skipping (%.1f%% inmask %.1f%% weight)" ,
                       (warp_code == MRI_QUINTIC) ? "quintic" : "  cubic" ,
                       ibot,itop, jbot,jtop, kbot,ktop ,
                       (100.0f*nwb)/Hnval , (100.0f*wsum)/(Hnval*Hwbar) ) ;
     RETURN(0) ;
   }

   /*-- setup the basis functions for Hwarping --*/

   switch( warp_code ){
     default:
     case MRI_CUBIC:
       Hbasis_code   = MRI_CUBIC ;                   /* 3rd order polynomials */
       Hbasis_parmax = 0.033*Hfactor ;    /* max displacement from 1 function */
       Hnpar         = 24 ;                /* number of params for local warp */
       prad          = 0.333 ;                       /* NEWUOA initial radius */
       HCwarp_setup_basis( nxh,nyh,nzh, Hgflags ) ;      /* setup HCwarp_load */
#ifdef USE_HLOADER
       Hloader       = HCwarp_load ;   /* func to make local warp from params */
#endif
     break ;

     case MRI_QUINTIC:
       Hbasis_code   = MRI_QUINTIC ;                 /* 5th order polynomials */
       Hbasis_parmax = 0.007*Hfactor ;
       Hnpar         = 81 ;
       prad          = 0.222 ;
       HQwarp_setup_basis( nxh,nyh,nzh, Hgflags ) ;
#ifdef USE_HLOADER
       Hloader       = HQwarp_load ;
#endif
     break ;
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
     float *bar  =MRI_FLOAT_PTR(Hbasim) ;

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

   if( iter <= 0 ){ free(parvec); RETURN(0); }  /* something bad happened */

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
       "     %s patch %03d..%03d %03d..%03d %03d..%03d : cost=%g iter=%d : energy=%.3f:%.3f pen=%g",
                     (Hbasis_code == MRI_QUINTIC) ? "quintic" : "  cubic" ,
                           ibot,itop, jbot,jtop, kbot,ktop , Hcost  , iter , jt,st , Hpenn ) ;
   } else if( Hverb == 1 && (Hlev_now<=2 || lrand48()%Hlev_now==0) ){
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

   INCOR_destroy(Hincor) ; Hincor = NULL ; KILL_floatvec(Hmpar) ;
   FREEIFNN(Hpar) ; FREEIFNN(Hwval) ; FREEIFNN(Haawt) ; FREEIFNN(Hbval) ;
   FREEIFNN(Hparmap) ; Hnparmap = Hnpar = 0 ; Hbasis_code = -666 ;
   FREEIFNN(Hwval_plus) ; FREEIFNN(Hwval_minus) ;

   FREEIFNN(bc0x); FREEIFNN(bc1x); nbcx=0;
   FREEIFNN(bc0y); FREEIFNN(bc1y); nbcy=0;
   FREEIFNN(bc0z); FREEIFNN(bc1z); nbcz=0;

   FREEIFNN(bq0x); FREEIFNN(bq1x); FREEIFNN(bq2x); nbqx=0;
   FREEIFNN(bq0y); FREEIFNN(bq1y); FREEIFNN(bq2y); nbqy=0;
   FREEIFNN(bq0z); FREEIFNN(bq1z); FREEIFNN(bq2z); nbqz=0;

   if( bbbcar != NULL ){
     int ii ;
     for( ii=0 ; ii < 8 ; ii++ ) FREEIFNN(bbbcar[ii]) ;
     free(bbbcar) ; nbbcxyz = 0 ; bbbcar = NULL ;
   }

   if( bbbqar != NULL ){
     int ii ;
     for( ii=0 ; ii < 27 ; ii++ ) FREEIFNN(bbbqar[ii]) ;
     free(bbbqar) ; nbbqxyz = 0 ; bbbqar = NULL ;
   }

   Hstopcost = -666666.6f ;
   Hstopped  = 0 ;
   Hfinal    = 0 ;

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
   Hsrcim = mri_to_float(sim);

   if( Hblur_s >= 0.5f ){
     if( Hverb > 1 ) ININFO_message("   blurring source image %.3g voxels FWHM",Hblur_s) ;
     Hsrcim_blur = mri_float_blur3D( FWHM_TO_SIGMA(Hblur_s) , Hsrcim ) ;
   } else if( Hblur_s <= -1.0f ){
     if( Hverb > 1 ) ININFO_message("   median-izing source image %.3g voxels",-Hblur_s) ;
     Hsrcim_blur = mri_medianfilter( Hsrcim , -Hblur_s , NULL , 0 ) ;
   } else {
     Hsrcim_blur = NULL ;
   }

   if( Hblur_b >= 0.5f ){
     if( Hverb > 1 ) ININFO_message("   blurring base image %.3g voxels FWHM",Hblur_b) ;
     Hbasim_blur = mri_float_blur3D( FWHM_TO_SIGMA(Hblur_b) , Hbasim ) ;
   } else if( Hblur_b <= -1.0f ){
     if( Hverb > 1 ) ININFO_message("   median-izing base image %.3g voxels",-Hblur_b) ;
     Hbasim_blur = mri_medianfilter( Hbasim , -Hblur_b , NULL , 0 ) ;
   } else {
     Hbasim_blur = NULL ;
   }

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
     if( Hverb > 1 ) ININFO_message(   "%d voxels in mask (out of %d)",nwb,Hnxyz) ;
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
   }

   Hgflags = IW3D_munge_flags(Hnx,Hny,Hnz,warp_flags) ;
   if( Hgflags < 0 )
     ERROR_exit("IW3D_setup_for_improvement: bad warp_flags input") ;

   /*-- copy/create initial warp, and warp the source images --*/

   if( Iwarp != NULL ){
     if( Iwarp->nx != Hnx || Iwarp->ny != Hny || Iwarp->nz != Hnz )
       ERROR_exit("IW3D_setup_for_improvement: bad Iwarp input") ;

     Haawarp = IW3D_copy(Iwarp,1.0f) ;     /* copy it */
     Haasrcim_plus  = IW3D_warp_floatim( Haawarp, SRCIM, Himeth ,  1.0f ) ;
     Haabasim_minus = IW3D_warp_floatim( Haawarp, BASIM, Himeth , -1.0f ) ;
   } else {
     Haawarp = IW3D_create(Hnx,Hny,Hnz) ;  /* initialize to 0 displacements */
     Haasrcim_plus  = mri_to_float(SRCIM) ;     /* 'warped' source image */
     Haabasim_minus = mri_to_float(BASIM) ;     /* 'warped' base image */
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
   Hlev_start = 0          ; Hlev_end = 1        ; Hpen_fac = 0.0      ;

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
   int dkkk,djjj,diii , ngmin=0 , levdone=0 ;
   int qmode=MRI_CUBIC , nlevr , nsup,isup , myIwarp=0 ;

ENTRY("IW3D_warpomatic_plusminus") ;

   if( Hverb ) Hfirsttime = 1 ;

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

   if( Hverb ){
         INFO_message("AFNI +-warpomatic start: %d x %d x %d volume",Hnx,Hny,Hnz) ;
       ININFO_message("              autobbox = %d..%d %d..%d %d..%d",imin,imax,jmin,jmax,kmin,kmax) ;
   }

   if( Hlev_start == 0 ){            /* top level = global warps */
#ifdef USE_PLUSMINUS_INITIALWARP
     nlevr = ( WORKHARD(0) || Hduplo ) ? 4 : 2 ; if( SUPERHARD(0) ) nlevr++ ;
#else
     nlevr = 4 ;
#endif
     Hforce = 1 ; Hfactor = 1.0f ; Hpen_use = 0 ; Hlev_now = 0 ;
     if( Hverb == 1 ) fprintf(stderr,"lev=0 %d..%d %d..%d %d..%d: ",ibbb,ittt,jbbb,jttt,kbbb,kttt) ;
     for( iii=0 ; iii < nlevr ; iii++ ){
       (void)IW3D_improve_warp_plusminus( MRI_CUBIC  , ibbb,ittt,jbbb,jttt,kbbb,kttt );
       Hcostold = Hcost ;
       (void)IW3D_improve_warp_plusminus( MRI_QUINTIC, ibbb,ittt,jbbb,jttt,kbbb,kttt );
       if( iii > 0 && iii < nlevr-1 && Hcostold-Hcost < 0.005f ){
         if( Hverb > 1 )
           ININFO_message("       --> too little improvement: breaking out of WORKHARD iterates") ;
         break ;
       }
     }
     if( Hverb == 1 ) fprintf(stderr," done [cost=%.3f]\n",Hcost) ;
   } else {
     Hcost = 666.666f ;  /* a beastly thing to do */
   }
   Hforce = 0 ; Hlev_final = 0 ; Hpen_use = (Hpen_fac > 0.0f) ;
   Hcostmid = Hcostend = Hcost ;

   if( !Hduplo ) ITEROUT(0) ;

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
     Hfactor = 1.0f ;
#endif

     qmode = MRI_CUBIC ;
#ifdef ALLOW_QFINAL
     if( levdone && !Hduplo && Hqfinal ) qmode = MRI_QUINTIC ;
#endif

     (void)IW3D_load_energy(Haawarp) ;  /* initialize energy field for penalty use */

     nlevr = WORKHARD(lev)  ? 2 : 1 ;
     nsup  = SUPERHARD(lev) ? 2 : 1 ;

     if( Hverb > 1 )
       ININFO_message("  ........ +-lev=%d xwid=%d ywid=%d zwid=%d Hfac=%g %s %s" ,
                      lev,xwid,ywid,zwid,Hfactor , (levdone   ? "FINAL"  : "\0") ,
                                                   (nlevr > 1 ? "WORKHARD" : "\0") ) ;
     else if( Hverb == 1 )
       fprintf(stderr,"lev=%d patch=%dx%dx%d: ",lev,xwid,ywid,zwid) ;

     /* alternate the direction of sweeping at different levels */

     if( lev%2 == 1 || nlevr > 1 ){  /* bot to top, ijk */
      for( isup=0 ; isup < nsup ; isup++ ){  /* superhard? */
       for( kdon=0,kbot=ibbb ; !kdon ; kbot += dkkk ){
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
       if( nlevr > 1 && Hverb == 1 ) fprintf(stderr,":[cost=%.3f]:",Hcost) ;
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
             iter = IW3D_improve_warp_plusminus( qmode  , ibot,itop , jbot,jtop , kbot,ktop ) ;
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
       Hcostend = Hcost ;
     }

     if( Hverb == 1 ) fprintf(stderr," done [cost=%.3f]\n",Hcost) ;

     if( !Hduplo ) ITEROUT(lev) ;

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
#endif /* ALLOW_PLUSMINUS */
/*****--------------------------------------------------------------------*****/
/*****||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||*****/
/*****--------------------------------------------------------------------*****/

#endif /* ALLOW_QWARP */
/******************************************************************************/
/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
/******************************************************************************/
