#include "mrilib.h"

THD_3dim_dataset * THD_mean_dataset( int nds, THD_3dim_dataset **dsin, int ivbot, int ivtop, int verb )
{
   THD_3dim_dataset *dsout ;
   int nbr,dd , nx,ny,nz,nxyz , vv,kk , ndd , ii ;
   float **br , *bb , fac ;

ENTRY("THD_mean_dataset") ;

   /* check inputs */

   if( nds <= 0 || dsin == NULL || !ISVALID_DSET(dsin[0]) ) RETURN(NULL) ;
   if( ivbot < 0 ) ivbot = 0 ;
   if( ivtop < ivbot || ivtop >= DSET_NVALS(dsin[0]) ) ivtop = DSET_NVALS(dsin[0])-1 ;
   if( ivtop < ivbot ) RETURN(NULL) ;

   nbr = ivtop-ivbot+1 ;

   nx = DSET_NX(dsin[0]) ;
   ny = DSET_NY(dsin[0]) ;
   nz = DSET_NZ(dsin[0]) ; nxyz = nx*ny*nz ;

   for( dd=0 ; dd < nds ; dd++ ){
     if( !ISVALID_DSET(dsin[dd]) ) RETURN(NULL) ;
     if( ivtop >= DSET_NVALS(dsin[dd]) ) RETURN(NULL) ;
     if( DSET_NX(dsin[dd]) != nx ||
         DSET_NY(dsin[dd]) != ny || DSET_NZ(dsin[dd]) != nz ) RETURN(NULL) ;
   }

   /* create output dataset */

   dsout = EDIT_empty_copy(dsin[0]) ;
   EDIT_dset_items( dsout ,
                      ADN_nvals     , nbr  ,
                      ADN_brick_fac , NULL ,
                    ADN_none ) ;
   br = (float **)malloc(sizeof(float *)*nbr) ;
   for( vv=0 ; vv < nbr ; vv++ ){
     br[vv] = (float *)calloc(sizeof(float),nxyz) ;
     EDIT_substitute_brick( dsout , vv , MRI_float , br[vv] ) ;
   }

   /* loop over input datasets */

   for( ndd=dd=0 ; dd < nds ; dd++ ){
     DSET_load(dsin[dd]) ;
     if( !DSET_LOADED(dsin[dd]) ){
       ERROR_message("Can't load dataset %s in THD_mean_dataset()",DSET_BRIKNAME(dsin[dd])) ;
       continue ;
     }
     if( verb ) fprintf(stderr,".") ;

     for( vv=0 ; vv < nbr ; vv++ ){
       kk = vv+ivbot ; bb = br[vv] ;
       switch( DSET_BRICK_TYPE(dsin[dd],kk) ){
         default:
           ERROR_message("THD_mean_dataset(): can't use sub-brick with datum=%d", DSET_BRICK_TYPE(dsin[dd],kk));
           ndd-- ;
         break ;

         case MRI_float:{
           float *pp = (float *) DSET_ARRAY(dsin[dd],kk) ;
           fac = DSET_BRICK_FACTOR(dsin[dd],kk) ; if( fac == 0.0f ) fac = 1.0f ;
           for( ii=0 ; ii < nxyz ; ii++ ) bb[ii] += fac * pp[ii] ;
         }
         break ;

         case MRI_short:{
           short *pp = (short *) DSET_ARRAY(dsin[dd],kk) ;
           fac = DSET_BRICK_FACTOR(dsin[dd],kk) ; if( fac == 0.0f ) fac = 1.0f ;
           for( ii=0 ; ii < nxyz ; ii++ ) bb[ii] += fac * pp[ii] ;
         }
         break ;

         case MRI_byte:{
           byte *pp = (byte *) DSET_ARRAY(dsin[dd],kk) ;
           fac = DSET_BRICK_FACTOR(dsin[dd],kk) ; if( fac == 0.0f ) fac = 1.0f ;
           for( ii=0 ; ii < nxyz ; ii++ ) bb[ii] += fac * pp[ii] ;
         }
         break ;
       } /* end of switch on sub-brick type */
     } /* end of sub-brick loop */

     ndd++ ;  DSET_unload(dsin[dd]) ; /* one more done */
   }

   if( ndd == 0 ) RETURN(dsout) ;

   /* scale to be mean */

   fac = 1.0f / ndd ;
   for( vv=0 ; vv < nbr ; vv++ ){
     bb = br[vv] ;
     for( ii=0 ; ii < nxyz ; ii++ ) bb[ii] *= fac ;
   }

   free(br) ; RETURN(dsout) ;
}
