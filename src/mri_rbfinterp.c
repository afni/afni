#include "mrilib.h"

#undef  RBF_PC2
#undef  RBF_CTPS2a
#undef  RBF_CTPS2b
#undef  Rlog

#define Rlog(x)       (((x)<=0.0f) ? 0.0f : logf(x))

#define RBF_CP2(x)    ((1.0f-(x))*(1.0f-(x))*(4.0f*(x)+1.0f))

#define RBF_CTPS2a(x) (1.0f+(x)*(x)*(-30.0f+(x)*(-10.0f-60.0f*Rlog(x)+(x)*(45.0f-6.0f*(x)))))

#define RBF_CTPS2b(x) (1.0f+(x)*(x)*(-20.0f+(x)*(-80.0f+(x)*(-45.0f+60.0f*Rlog(x)-16.0f*(x)))))

#undef  RBF_func
#define RBF_func RBF_CP2  /* which radial basis function to use */

#undef  RBF_TYPE
#define RBF_TYPE 9737308

typedef struct {
  int type ;
  int nknot ;
  float *xknot, *yknot, *zknot ; /* each is an nknot-long vector */
  float *psmat ;                 /* (nknot+4) X (nknot+4) matrix */
} RBF_struct ;

/*----------------------------------------------------------------------------*/

RBF_struct * RBF_setup( int nk , float *xx , float *yy , float *zz )
{
   RBF_struct *rbs ;
   int ii , jj , nn ;
   MRI_IMAGE *im_mat , *im_psmat ;
   float     *mat , rr ;

ENTRY("RBF_setup") ;

   if( nk < 1 || xx == NULL || yy == NULL || zz == NULL ) RETURN(NULL) ;

#undef  MM
#define MM(i,j) mat[(i)+nn*(j)]

   nn = nk+4 ;
   im_mat = mri_new( nn , nn , MRI_float ) ;
   mat    = MRI_FLOAT_PTR(im_mat) ;          /* zero filled */

   for( ii=0 ; ii < nk ; ii++ ){
     for( jj=0 ; jj < ii ; jj++ ){
       rr = sqrtf( (xx[ii]-xx[jj])*(xx[ii]-xx[jj])
                  +(yy[ii]-yy[jj])*(yy[ii]-yy[jj])
                  +(zz[ii]-zz[jj])*(zz[ii]-zz[jj]) ) ;
       MM(ii,jj) = MM(jj,ii) = RBF_func(rr) ;
     }
     MM(ii,ii)   = MM(ii,nk)   = MM(nk,ii) = 1.0f ;
     MM(ii,nk+1) = MM(nk+1,ii) = xx[ii] ;
     MM(ii,nk+2) = MM(nk+2,ii) = yy[ii] ;
     MM(ii,nk+3) = MM(nk+3,ii) = yy[ii] ;
   }

   im_psmat = mri_matrix_psinv( im_mat , NULL , 0.00001f ) ;
   mri_free(im_mat) ;
   if( im_psmat == NULL ) RETURN(NULL) ;

   rbs = (RBF_struct *)malloc(sizeof(RBF_struct)) ;
   rbs->type  = RBF_TYPE ;
   rbs->nknot = nk ;
   rbs->xknot = (float *)malloc(sizeof(float)*nk) ;
   rbs->yknot = (float *)malloc(sizeof(float)*nk) ;
   rbs->zknot = (float *)malloc(sizeof(float)*nk) ;
   memcpy(rbs->xknot,xx,sizeof(float)*nk) ;
   memcpy(rbs->yknot,yy,sizeof(float)*nk) ;
   memcpy(rbs->zknot,zz,sizeof(float)*nk) ;
   rbs->psmat = MRI_FLOAT_PTR(im_psmat) ;
   mri_fix_data_pointer(NULL,im_psmat) ; mri_free(im_psmat) ;

   RETURN(rbs) ;
}
