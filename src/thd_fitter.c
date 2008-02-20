#include "mrilib.h"

/*------------------------------------------------------------------*/
/* Least squares fitting without constraints. (cf mri_matrix.c) */
/*------------------------------------------------------------------*/

static float * new_lsqfit( int npt  , float *far   ,
                           int nref , float *ref[]  )
{
  int  jj ;
  MRI_IMAGE *rmat,*pmat,*smat ;
  float *rar;

  /* compute pseudo-inverse of matrix into pmat */

  rmat = mri_new(npt,nref,MRI_float ); rar = MRI_FLOAT_PTR(rmat);
  for( jj=0 ; jj < nref ; jj++ )
    memcpy( rar+jj*npt , ref[jj] , sizeof(float)*npt ) ;
  pmat = mri_matrix_psinv(rmat,NULL,0.0f) ;
  mri_free(rmat) ;
  if( pmat == NULL ) return NULL ;  /* should not happen */

  /* create vector of data and multiply by pseudo-inverse */

  rmat = mri_new_vol_empty( npt , 1 , 1 , MRI_float ) ;
  mri_fix_data_pointer( far , rmat ) ;
  smat = mri_matrix_mult( pmat , rmat ) ;
  mri_free(pmat); mri_clear_data_pointer(rmat); mri_free(rmat);
  if( smat == NULL ) return NULL ;  /* should not happen */

  /* get pointer to results array and return it */

  rar = MRI_FLOAT_PTR(smat);
  mri_clear_data_pointer(smat); mri_free(smat);
  return rar ;
}

/*------------------------------------------------------------------*/

#undef  ERREX
#define ERREX(s) do{ ERROR_message(s); return NULL; } while(0)

/*------------------------------------------------------------------*/
/* Fit the npt-long vector far[] to the nref vectors in ref[].
    - meth=1 ==> L1 fit
    - meth=2 ==> L2 fit (any meth besides 1 or 2 is illegal)
    - ccon != NULL ==> ccon[i] is constraint on coef #i
                       ccon[i] = 0 == no constraint
                               > 0 == coef #i must be >= 0
                               < 0 == coef #i must be <= 0
    - Output is vector of coefficiens (nref of them).
    - Output is NULL if some error transpired.
*//*----------------------------------------------------------------*/

floatvec * THD_fitter( int npt , float *far  ,
                       int nref, float *ref[],
                       int meth, float *ccon  )
{
   int jj ;
   float *qfit=NULL, val ;
   floatvec *fv=NULL ;

   /* check inputs */

   if( npt  <= 1 || far == NULL ||
       nref <= 0 || ref == NULL || nref >= npt-1 )
     ERREX("THD_fitter: bad inputs") ;
   for( jj=0 ; jj < nref ; jj++ )
     if( ref[jj] == NULL ) ERREX("THD_fitter: bad ref") ;

   switch( meth ){

     default: ERREX("THD_fitter: bad meth code") ;

     /*-- least squares --*/

     case 2:
       if( ccon == NULL ){                           /* unconstrained */
         qfit = new_lsqfit( npt, far, nref, ref ) ;
       } else {                                      /* constrained */
         qfit = (float *)malloc(sizeof(float)*nref);
         memcpy(qfit,ccon,sizeof(float)*nref) ;
         val = cl2_solve( npt, nref, far, ref, qfit, 1 ) ; /* cf cl2.c */
         if( val < 0.0f ){ free(qfit); qfit = NULL; }      /* bad */
       }
     break ;

     /*-- L1 fitting --*/

     case 1:
       qfit = (float *)malloc(sizeof(float)*nref) ;
       if( ccon != NULL ) memcpy(qfit,ccon,sizeof(float)*nref) ;
       val = cl1_solve( npt,nref, far,ref, qfit, (ccon!=NULL) ); /* cf cl1.c */
       if( val < 0.0f ){ free(qfit); qfit = NULL; }              /* bad */
     break ;
   }

   if( qfit == NULL ) return NULL ;  /* bad */

   MAKE_floatvec(fv,nref) ;
   memcpy( fv->ar, qfit, sizeof(float)*nref ) ;
   free(qfit) ; return fv ;
}
