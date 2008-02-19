#include "mrilib.h"

/*------------------------------------------------------------------*/

static float * new_lsqfit( int npt  , float *far   ,
                           int nref , float *ref[]  )
{
  int  jj ;
  MRI_IMAGE *rmat,*pmat,*smat ;
  float *rar;

  rmat = mri_new(npt,nref,MRI_float ); rar = MRI_FLOAT_PTR(rmat);
  for( jj=0 ; jj < nref ; jj++ )
    memcpy( rar+jj*npt , ref[jj] , sizeof(float)*npt ) ;
  pmat = mri_matrix_psinv(rmat,NULL,0.0f) ;
  mri_free(rmat) ;
  if( pmat == NULL ) return NULL ;

  rmat = mri_new_vol_empty( npt , 1 , 1 , MRI_float ) ;
  mri_fix_data_pointer( far , rmat ) ;
  smat = mri_matrix_mult( pmat , rmat ) ;
  mri_free(pmat); mri_clear_data_pointer(rmat); mri_free(rmat);
  if( smat == NULL ) return NULL ;

  rar = MRI_FLOAT_PTR(smat);
  mri_clear_data_pointer(smat); mri_free(smat);
  return rar ;
}

/*------------------------------------------------------------------*/
/* Does not work! */

#if 0
static float * con_lsqfit( int npt  , float *far   ,
                           int nref , float *ref[] , float *ccon )
{
  float *qfit , **cref , *cfit ;
  int jj , nit , nok ;

  qfit = new_lsqfit(npt,far,nref,ref) ;
  if( ccon == NULL || qfit == NULL ) return qfit ;

  cref = (float **)malloc(sizeof(float *)*nref) ;

  for( nit=0 ; nit < nref ; nit++ ){
    for( nok=jj=0 ; jj < nref ; jj++ ){
      if( qfit[jj]*ccon[jj] >= 0.0f ) cref[nok++] = ref[jj] ;
    }
    if( nok == 0 ){            /* none met the constraints */
      for( jj=0 ; jj < nref ; jj++ ) qfit[jj] = 0.0f ;
      free(cref); return qfit ;
    } else if( nok == nref ){  /* all met the constraints */
      free(cref) ; return qfit ;
    }
    cfit = new_lsqfit(npt,far,nok,cref) ;  /* fit a subset */
    for( nok=jj=0 ; jj < nref ; jj++ ){
      if( qfit[jj]*ccon[jj] >= 0.0f ) qfit[jj] = cfit[nok++] ;
      else                            qfit[jj] = 0.0f ;
    }
    free(cfit) ;
  }
  free(cref) ; free(qfit) ; return NULL ;
}
#endif

/*------------------------------------------------------------------*/

#undef  ERREX
#define ERREX(s) do{ ERROR_message(s); return NULL; } while(0)

/* Fit the npt-long vector far[] to the nref vectors in ref[].
   meth=1 ==> L1 fit
   meth=2 ==> L2 fit
   ccon != NULL ==> con[i] is constraint on coef #i
*/

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

     case 2:
       qfit = new_lsqfit( npt, far, nref, ref ) ;
     break ;

     case 1:
       qfit = (float *)malloc(sizeof(float)*nref) ;
       if( ccon != NULL ) memcpy(qfit,ccon,sizeof(float)*nref) ;
       val = cl1_solve( npt, nref, far, ref, qfit, (ccon!=NULL) ) ;
       if( val < 0.0f ){ free(qfit); qfit = NULL; } /* error */
#if 0
       if( ccon != NULL && qfit != NULL ){
         for( jj=0 ; jj < nref ; jj++ )
           if( ccon[jj]*qfit[jj] < 0.0f )
             ERROR_message("cl1 constraint failure: ccon[%d]=%f qfit=%f",
                           jj,ccon[jj],qfit[jj]) ;
       }
#endif
     break ;
   }

   if( qfit == NULL ) return NULL ;  /* bad */

   MAKE_floatvec(fv,nref) ;
   memcpy( fv->ar, qfit, sizeof(float)*nref ) ;
   free(qfit) ; return fv ;
}
