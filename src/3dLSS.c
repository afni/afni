#include "mrilib.h"

/*----------------------------------------------------------------------------*/
/* Setup to solve a collection of equations of the form

   [z] = [X] [beta] + gamma [b] + delta [c]

   where [z] = data = N vector
         [X] = fixed N x (M-1) matrix } [X] and [b] together make
         [b] = fixed N vector         } up the N x M matrix [A]
         [c] = variable N vector
      [beta] = (M-1) vector = fit coefficients of columns of [X]
       gamma = scalar fit coefficient of [b]
       delta = scalar fit coefficient of [c]
   The LSS goal is to find the value of gamma+delta for a bunch of
   different [c] vectors.  To that end, this function returns an
   N vector [s] for each input [c], such that gamma+delta = [s] *dot* [z].
   All the other stuff that COULD be estimated, such as [beta], is ignored
   for the sake of efficiency.

   If NULL is returned, the inputs are illegal.  In particular, the nx element
   (column length) of the 2 input images must be the same, or you will be
   chastised and execrated in public.
*//*--------------------------------------------------------------------------*/

MRI_IMAGE * LSS_setup( MRI_IMAGE *ima , MRI_IMAGE *imc )
{
   int nn, mm, nc, ii, jj, ic ;
   float *cc, *pp, *qq, *qj, *vv, *ss, *pv , cj, cvdot, pc ;
   MRI_IMAGE *ims , *imp , *imq ;
   MRI_IMARR *imar ;

ENTRY("LSS_setup") ;

   if( ima == NULL || imc == NULL ){  /* bad user */
     ERROR_message("LSS_setup: NULL input image?!") ;
     RETURN(NULL) ;
   }

   /* [A] is nn X mm ; [C] is nn x nc */

   nn = ima->nx ; mm = ima->ny ; nc = imc->ny ; cc = MRI_FLOAT_PTR(imc) ;

   if( imc->nx != nn || nn <= mm+2 ){  /* stoopid user */
     ERROR_message("LSS_setup: ima->nx=%d does not equal imc->nx=%d :-(" ,
                   ima->nx,imc->nx) ;
     RETURN(NULL) ;
   }

   /* get imp = [P] = psinv of [A] = mm X nn matrix
          imq = [Q] = ortproj onto column null space of [A] = nn X nn matrix */

   mri_matrix_psinv_svd(1) ;
   imar = mri_matrix_psinv_ortproj( ima , 1 ) ;

   if( imar == NULL ){  /* should not happen */
     ERROR_message("LSS_setup: cannot compute pseudo-inverse :-(") ;
     RETURN(NULL) ;
   }

   imp = IMARR_SUBIM(imar,0) ; pp = MRI_FLOAT_PTR(imp) ;
   imq = IMARR_SUBIM(imar,1) ; qq = MRI_FLOAT_PTR(imq) ;

   /* create output image = [S] = nn X nc
      Each column of [S] is the vector that we
      dot into a data vector [z] to get the estimate of
      gamma+delta for the corresponding column from [C] */

   ims = mri_new(nn,nc,MRI_float) ; ss = MRI_FLOAT_PTR(ims) ;

   /* workspace vectors */

   vv = (float *)malloc(sizeof(float)*nn) ;  /* will be [Q] [c] */

   pv = (float *)malloc(sizeof(float)*nn) ;  /* last row of [P] */
   for( ii=0 ; ii < nn ; ii++ ) pv[ii] = pp[ mm-1 + ii*mm ] ;

   /* loop over columns of [C] (and [S]) */

   for( ic=0 ; ic < nc ; ic++,cc+=nn,ss+=nn ){

     /* compute [v] = [Q] [c] */

     for( ii=0 ; ii < nn ; ii++ ) vv[ii] = 0.0f ;     /* initialize [v] to 0 */
     for( jj=0 ; jj < nn ; jj++ ){                 /* loop over columns of Q */
       qj = qq + jj*nn ;                         /* ptr to jj-th column of Q */
       cj = cc[jj] ;                                   /* jj-th value of [c] */
       for( ii=0 ; ii < nn ; ii++ ) vv[ii] += qj[ii] * cj ;  /* sum into [v] */
     }

     /* compute cvdot = [c] *dot* [v]
                cj    = [c] *dot* [c]
                pc    = [c] *dot* {last row of [P] = pv} */

     for( pc=cj=cvdot=ii=0 ; ii < nn ; ii++ ){
       cvdot += cc[ii]*vv[ii] ; cj += cc[ii]*cc[ii] ; pc += pv[ii]*cc[ii] ;
     }

     /* initialize [s] = last row of [P] */

     for( ii=0 ; ii < nn ; ii++ ) ss[ii] = pv[ii] ;

     /* If cvdot is zero(ish), this means that the extra column [c]
        is collinear(ish) with the columns of [A], and we skip the next step.
        Note that since [Q] is an orthogonal matrix,
        we are guaranteed that L2norm([Q][c]) == L2norm([c]),
        which implies that abs(cvdot) <= abs(cj), by the triangle inequality. */

     if( fabsf(cvdot) >= 1.e-5*cj ){

       /* add the proper fraction of [v] into [s] */

       pc = (1.0f - pc) / cvdot ;
       for( ii=0 ; ii < nn ; ii++ ) ss[ii] += pc * vv[ii] ;
     }

   } /* end of loop over columns of [C] */

   /* toss the trash and return the output set of columns */

   free(pv) ; free(vv) ; DESTROY_IMARR(imar) ; RETURN(ims) ;
}

/*----------------------------------------------------------------------------*/
/* Create 2 new matrix images.
   (0) One where columns jbot..jtop are excised, and the last column is
       the sum of those columns.
   (1) The matrix of the excised columns.
*//*--------------------------------------------------------------------------*/

MRI_IMARR * LSS_mangle_matrix( MRI_IMAGE *ima, int jbot, int jtop )
{
   int ii , jj , jnew , jn , njj , nn,mm ;
   MRI_IMAGE *imb , *imc ; MRI_IMARR *imar ;
   float *aa , *bb , *cc , *acol , *bcol , *ccol ;

ENTRY("LSS_mangle_matrix") ;

   if( ima == NULL || ima->kind != MRI_float ) RETURN(NULL) ;
   nn = ima->nx ; mm = ima->ny ;
   njj = jtop-jbot+1 ; if( njj <= 1 )          RETURN(NULL) ;
   if( jbot < 0 || jtop >= mm )                RETURN(NULL) ;

   imb = mri_new( nn , mm-njj+1 , MRI_float ) ;
   imc = mri_new( nn , njj      , MRI_float ) ;
   aa  = MRI_FLOAT_PTR(ima) ;
   bb  = MRI_FLOAT_PTR(imb) ;
   cc  = MRI_FLOAT_PTR(imc) ;

   /* copy non-excised columns into the new imb */

   for( jn=jj=0 ; jj < mm ; jj++ ){
     if( jj >= jbot && jj <= jtop ) continue ;
     acol = aa + jj*nn ;
     bcol = bb + jn*nn ; jn++ ;
     for( ii=0 ; ii < nn ; ii++ ) bcol[ii] = acol[ii] ;
   }

   /* copy excised columns into the new imc,
      and also add them up into the last column of imb */

   bcol = bb + (mm-njj)*nn ;  /* last col of imb */
   for( jn=0,jj=jbot ; jj <= jtop ; jj++ ){
     acol = aa + jj*nn ;
     ccol = cc + jn*nn ; jn++ ;
     for( ii=0 ; ii < nn ; ii++ ){
       ccol[ii] = acol[ii] ; bcol[ii] += acol[ii] ;
     }
   }

   INIT_IMARR(imar) ; ADDTO_IMARR(imar,imb) ; ADDTO_IMARR(imar,imc) ; RETURN(imar) ;
}
