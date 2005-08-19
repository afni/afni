#include "mrilib.h"

/*--------------------------------------------------------------------------*/

float mri_nstat( int code , MRI_IMAGE *im )
{
   MRI_IMAGE *fim ;
   float     *far , outval=0.0f ;
   int npt ;

   if( im == NULL || im->nvox == 0 ) return outval ;

   if( im->kind != MRI_float ) fim = mri_to_float(im) ;
   else                        fim = im ;
   far = MRI_FLOAT_PTR(fim) ;
   npt = fim->nvox ;

   switch( code ){
     default:
     case NSTAT_MEAN:{
       register int ii ;
       for( outval=0.0,ii=0 ; ii < npt ; ii++ ) outval += far[ii] ;
       outval /= npt ;
     }
     break ;

     case NSTAT_SIGMA:
     case NSTAT_CVAR:
     case NSTAT_VAR:{
       register float mm,vv ; register int ii ;
       if( npt == 1 ) break ;
       for( mm=0.0,ii=0 ; ii < npt ; ii++ ) mm += far[ii] ;
       mm /= npt ;
       for( vv=0.0,ii=0 ; ii < npt ; ii++ ) vv += (far[ii]-mm)*(far[ii]-mm) ;
       vv /= (npt-1) ;
            if( code == NSTAT_SIGMA ) outval = sqrt(vv) ;
       else if( code == NSTAT_VAR   ) outval = vv ;
       else if( mm   >  0.0f        ) outval = sqrt(vv) / mm ;
     }
     break ;

     case NSTAT_MEDIAN:
       qmedmad_float( npt , far , &outval , NULL ) ;
     break ;

     case NSTAT_MAD:
       qmedmad_float( npt , far , NULL , &outval ) ;
     break ;

     case NSTAT_MAX:{
       register int ii ;
       outval = far[0] ;
       for( ii=1 ; ii < npt ; ii++ ) if( far[ii] > outval ) outval = far[ii] ;
     }
     break ;

     case NSTAT_MIN:{
       register int ii ;
       outval = far[0] ;
       for( ii=1 ; ii < npt ; ii++ ) if( far[ii] < outval ) outval = far[ii] ;
     }
     break ;

     case NSTAT_ABSMAX:{
       register int ii ; register float vv ;
       outval = fabs(far[0]) ;
       for( ii=1 ; ii < npt ; ii++ ){
         vv = fabs(far[ii]) ; if( vv > outval ) outval = vv ;
       }
     }
     break ;
   }

   /* cleanup and exit */

   if( fim != im  ) mri_free(fim) ;
   return outval ;
}

/*--------------------------------------------------------------------------*/

MRI_IMAGE * mri_localstat( MRI_IMAGE *im, byte *mask, MCW_cluster *nbhd, int code )
{
   MRI_IMAGE *outim , *nbim ;
   float     *outar ;
   int ii,jj,kk , nx,ny,nz , ijk ;

ENTRY("mri_localstat") ;

   if( im == NULL || nbhd == NULL ) RETURN(NULL) ;

   outim = mri_new_conforming( im , MRI_float ) ;
   outar = MRI_FLOAT_PTR(outim) ;
   nx = outim->nx ; ny = outim->ny ; nz = outim->nz ;

   for( ijk=kk=0 ; kk < nz ; kk++ ){
    for( jj=0 ; jj < ny ; jj++ ){
     for( ii=0 ; ii < nx ; ii++ ){
       nbim = mri_get_nbhd( im , mask , ii,jj,kk , nbhd ) ;
       outar[ijk++] = mri_nstat( code , nbim ) ;
       mri_free(nbim) ;
   }}}

   RETURN(outim) ;
}
