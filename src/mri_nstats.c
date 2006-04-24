#include "mrilib.h"

/*--------------------------------------------------------------------------*/
/*! Input = 1D image, and an NSTAT_ code to compute some statistic.
   Output = statistic's value.
----------------------------------------------------------------------------*/

float mri_nstat( int code , MRI_IMAGE *im )
{
   MRI_IMAGE *fim ;
   float     *far , outval=0.0f ;
   int npt ;

   if( im == NULL || im->nvox == 0 ) return outval ;

   /* convert input to float format, if not already there */

   if( im->kind != MRI_float ) fim = mri_to_float(im) ;
   else                        fim = im ;
   far = MRI_FLOAT_PTR(fim) ;  /* array of values to statisticate */
   npt = fim->nvox ;           /* number of values */

   switch( code ){

     case NSTAT_NUM: outval = (float)npt ; break ;  /* quite easy */

     default:
     case NSTAT_MEAN:{
       register int ii ;
       for( ii=0 ; ii < npt ; ii++ ) outval += far[ii] ;
       outval /= npt ;
     }
     break ;

     case NSTAT_SIGMA:   /* these 3 need the mean and variance sums */
     case NSTAT_CVAR:
     case NSTAT_VAR:{
       register float mm,vv ; register int ii ;
       if( npt == 1 ) break ;                     /* will return 0.0 */
       for( mm=0.0,ii=0 ; ii < npt ; ii++ ) mm += far[ii] ;
       mm /= npt ;
       for( vv=0.0,ii=0 ; ii < npt ; ii++ ) vv += (far[ii]-mm)*(far[ii]-mm) ;
       vv /= (npt-1) ;
            if( code == NSTAT_SIGMA ) outval = sqrt(vv) ;
       else if( code == NSTAT_VAR   ) outval = vv ;
       else if( mm   !=  0.0f        ) outval = sqrt(vv) / fabsf(mm) ;
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
       outval = fabsf(far[0]) ;
       for( ii=1 ; ii < npt ; ii++ ){
         vv = fabsf(far[ii]) ; if( vv > outval ) outval = vv ;
       }
     }
     break ;
   }

   /* cleanup and exit */

   if( fim != im  ) mri_free(fim) ;
   return outval ;
}

/*--------------------------------------------------------------------------*/
/*! Compute a local statistic at each voxel of an image, possibly with
    a mask; 'local' is defined with a neighborhood; 'statistic' is defined
    by an NSTAT_ code.
----------------------------------------------------------------------------*/

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

/*--------------------------------------------------------------------------*/

static int verb=0 , vn=0 ;
void THD_localstat_verb(int i){ verb=i; vn=0; }

static void vstep_print(void)
{
   static char xx[10] = "0123456789" ;
   fprintf(stderr , "%c" , xx[vn%10] ) ;
   if( vn%10 == 9) fprintf(stderr,".") ;
   vn++ ;
}

/*--------------------------------------------------------------------------*/

THD_3dim_dataset * THD_localstat( THD_3dim_dataset *dset , byte *mask ,
                                  MCW_cluster *nbhd , int ncode, int *code )
{
   THD_3dim_dataset *oset ;
   MRI_IMAGE *nbim ;
   int iv,cc , nvin,nvout , nx,ny,nz,nxyz , ii,jj,kk,ijk ;
   float **aar ;
   int vstep ;

ENTRY("THD_localstat") ;

   if( dset == NULL || nbhd == NULL || ncode < 1 || code == NULL ) RETURN(NULL);

   oset  = EDIT_empty_copy( dset ) ;
   nvin  = DSET_NVALS( dset ) ;
   nvout = nvin * ncode ;
   EDIT_dset_items( oset ,
                      ADN_nvals     , nvout       ,
                      ADN_datum_all , MRI_float   ,
                      ADN_nsl       , 0           ,
                      ADN_brick_fac , NULL        ,
                      ADN_prefix    , "localstat" ,
                    ADN_none ) ;

   nx = DSET_NX(dset) ;
   ny = DSET_NY(dset) ;
   nz = DSET_NZ(dset) ; nxyz = nx*ny*nz ;

   vstep = (verb && nxyz > 99999) ? nxyz/50 : 0 ;
   if( vstep ) fprintf(stderr,"++ voxel loop:") ;

   aar = (float **)malloc(sizeof(float *)*ncode) ;

   for( iv=0 ; iv < nvin ; iv++ ){
     for( cc=0 ; cc < ncode ; cc++ ){
       aar[cc] = (float *)malloc(sizeof(float)*nxyz) ;
       if( aar[cc] == NULL )
         ERROR_exit("THD_localstat: out of memory at iv=%d cc=%d",iv,cc);
     }

     for( ijk=kk=0 ; kk < nz ; kk++ ){
      for( jj=0 ; jj < ny ; jj++ ){
       for( ii=0 ; ii < nx ; ii++,ijk++ ){
         if( vstep && ijk%vstep==vstep-1 ) vstep_print() ;
         nbim = THD_get_dset_nbhd( dset,iv , mask,ii,jj,kk , nbhd ) ;
         for( cc=0 ; cc < ncode ; cc++ )
           aar[cc][ijk] = mri_nstat( code[cc] , nbim ) ;
         mri_free(nbim) ;
     }}}

     for( cc=0 ; cc < ncode ; cc++ )
       EDIT_substitute_brick( oset , iv*ncode+cc , MRI_float , aar[cc] ) ;
   }

   if( vstep ) fprintf(stderr,"\n") ;
   free((void *)aar) ;
   RETURN(oset) ;
}
