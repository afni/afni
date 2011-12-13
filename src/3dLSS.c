#include "mrilib.h"

#undef  PM
#define PM(j)  pp[(j+1)*mm-1]

/*----------------------------------------------------------------------------*/
/* Setup to solve a collection of equations of the form

   [z] = [X] [beta] + gamma [b] + delta [c]

   where [z] = data = N vector
         [X] = fixed N x (M-1) matrix } [X] and [b] together make
         [b] = fixed N vector         } up the N x M matrix [A]
         [c] = variable N vector
   The LSS goal is to find the value of gamma+delta for a bunch of
   different [c] vectors.  To that end, this function returns an
   N vector [s] for each input [c], such that delta = [s] *dot* [z].
*//*--------------------------------------------------------------------------*/

MRI_IMAGE * LSS_setup( MRI_IMAGE *ima , MRI_IMAGE *imc )
{
   int nn, mm, nc, ii, jj, ic ;
   float *cc, *pp, *qq, *qj, *vv, *ss, cj, cvdot, pc ;
   MRI_IMAGE *ims , *imp , *imq ;
   MRI_IMARR *imar ;

ENTRY("LSS_setup") ;

   if( ima == NULL || imc == NULL ) RETURN(NULL) ;

   nn = ima->nx ; mm = ima->ny ; nc = imc->ny ;
   if( imc->nx != nn || nn <= mm+2 ) RETURN(NULL) ;

   imar = mri_matrix_psinv_ortproj( ima , 1 ) ;
   if( imar == NULL ) RETURN(NULL) ;
   imp = IMARR_SUBIM(imar,0) ; pp = MRI_FLOAT_PTR(imp) ;
   imq = IMARR_SUBIM(imar,1) ; qq = MRI_FLOAT_PTR(imq) ;
   cc  = MRI_FLOAT_PTR(imc) ;
   ims = mri_new(nn,nc,MRI_float) ; ss = MRI_FLOAT_PTR(ims) ;

   vv = (float *)malloc(sizeof(float)*nn) ;
   for( ic=0 ; ic < nc ; ic++,cc+=nn,ss+=nn ){
     memset(vv,0,sizeof(float)*nn) ;
     for( jj=0 ; jj < nn ; jj++ ) ss[jj] = PM(jj) ;
     for( jj=0 ; jj < nn ; jj++ ){
       qj = qq + jj*nn ; cj = cc[jj] ;
       for( ii=0 ; ii < nn ; ii++ ) vv[ii] += qj[ii] * cj ;
     }
     for( pc=cj=cvdot=ii=0 ; ii < nn ; ii++ ){
       cvdot += cc[ii]*vv[ii] ; cj += cc[ii]*cc[ii] ; pc += PM(ii)*cc[ii] ;
     }
     if( fabsf(cvdot) < 1.e-5*cj ) continue ;  /* bad bad Leroy Brown */
     pc = (1.0f - pc) / cvdot ;
     for( ii=0 ; ii < nn ; ii++ ) ss[ii] += pc * vv[ii] ;
   }

   DESTROY_IMARR(imar) ; free(vv) ; RETURN(ims) ;
}
