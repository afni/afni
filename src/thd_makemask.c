/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*---------------------------------------------------------------------*/
/*! Make a byte mask from mask dataset:
     miv = sub-brick of input
     if( mask_bot <= mask_top ) then
       only nonzero values in this range will be used
     else
       all nonzero values in the mask will be used
   The input dataset should be byte-, short-, or float-valued.

   The output is a byte array with 1s in "hit" locations and 0s in
   other locations.  The number of bytes is DSET_NVOX(mask_dset).
   This array should be free()-d someday.  If NULL is returned,
   some grotesque error transpired.
-----------------------------------------------------------------------*/

byte * THD_makemask( THD_3dim_dataset *mask_dset ,
                     int miv , float mask_bot , float mask_top )
{
   byte *mmm = NULL ;
   int nvox , ii ;

   if( !ISVALID_DSET(mask_dset)    ||
       miv < 0                     ||
       miv >= DSET_NVALS(mask_dset)  ) return NULL ;

   nvox = DSET_NVOX(mask_dset) ;

   DSET_load(mask_dset) ; if( !DSET_LOADED(mask_dset) ) return NULL ;

   mmm = (byte *) calloc( sizeof(byte) * nvox , 1 ) ;

   switch( DSET_BRICK_TYPE(mask_dset,miv) ){
      default:
         free(mmm) ; DSET_unload(mask_dset) ; return NULL ;

      case MRI_short:{
         short mbot , mtop ;
         short *mar = (short *) DSET_ARRAY(mask_dset,miv) ;
         float mfac = DSET_BRICK_FACTOR(mask_dset,miv) ;
         if( mfac == 0.0 ) mfac = 1.0 ;
         if( mask_bot <= mask_top ){
            mbot = SHORTIZE(mask_bot/mfac) ;
            mtop = SHORTIZE(mask_top/mfac) ;
         } else {
            mbot = (short) -MRI_TYPE_maxval[MRI_short] ;
            mtop = (short)  MRI_TYPE_maxval[MRI_short] ;
         }
         for( ii=0 ; ii < nvox ; ii++ )
            if( mar[ii] >= mbot && mar[ii] <= mtop && mar[ii] != 0 ) mmm[ii]=1;
      }
      break ;

      case MRI_byte:{
         byte mbot , mtop ;
         byte *mar = (byte *) DSET_ARRAY(mask_dset,miv) ;
         float mfac = DSET_BRICK_FACTOR(mask_dset,miv) ;
         if( mfac == 0.0 ) mfac = 1.0 ;
         if( mask_bot <= mask_top && mask_top > 0.0 ){
            mbot = BYTEIZE(mask_bot/mfac) ;
            mtop = BYTEIZE(mask_top/mfac) ;
         } else {
            mbot = 0 ;
            mtop = (byte) MRI_TYPE_maxval[MRI_short] ;
         }
         for( ii=0 ; ii < nvox ; ii++ )
            if( mar[ii] >= mbot && mar[ii] <= mtop && mar[ii] != 0 ) mmm[ii]=1;
      }
      break ;

      case MRI_float:{
         float mbot , mtop ;
         float *mar = (float *) DSET_ARRAY(mask_dset,miv) ;
         float mfac = DSET_BRICK_FACTOR(mask_dset,miv) ;
         if( mfac == 0.0 ) mfac = 1.0 ;
         if( mask_bot <= mask_top ){
            mbot = (float) (mask_bot/mfac) ;
            mtop = (float) (mask_top/mfac) ;
         } else {
            mbot = -WAY_BIG ;
            mtop =  WAY_BIG ;
         }
         for( ii=0 ; ii < nvox ; ii++ )
            if( mar[ii] >= mbot && mar[ii] <= mtop && mar[ii] != 0 ) mmm[ii]=1;
      }
      break ;
   }

   return mmm ;
}

/*! 
   Similar to THD_makemask except that it turns the dset itself to mask values
   returns (-1) if it fails, number of non-zero voxels if OK
*/
int THD_makedsetmask( THD_3dim_dataset *mask_dset ,
                     int miv , float mask_bot , float mask_top,
                     byte *cmask )
{
   int nvox , ii, nonzero=-1 ;

   if( !ISVALID_DSET(mask_dset)    ||
       miv < 0                     ||
       miv >= DSET_NVALS(mask_dset)  ) return (-1) ;

   nvox = DSET_NVOX(mask_dset) ;
   
   DSET_mallocize(mask_dset); /* do this or else it could be a read only dset! */
   DSET_load(mask_dset) ; if( !DSET_LOADED(mask_dset) ) return (-1) ;

   nonzero = 0;
   switch( DSET_BRICK_TYPE(mask_dset,miv) ){
      default:
         DSET_unload(mask_dset) ; return (-1) ;

      case MRI_short:{
         short mbot , mtop ;
         short *mar = (short *) DSET_ARRAY(mask_dset,miv) ;
         float mfac = DSET_BRICK_FACTOR(mask_dset,miv) ;
         if( mfac == 0.0 ) mfac = 1.0 ;
         if( mask_bot <= mask_top ){
            mbot = SHORTIZE(mask_bot/mfac) ;
            mtop = SHORTIZE(mask_top/mfac) ;
         } else {
            mbot = (short) -MRI_TYPE_maxval[MRI_short] ;
            mtop = (short)  MRI_TYPE_maxval[MRI_short] ;
         }
         if (cmask)  {
            for( ii=0 ; ii < nvox ; ii++ )
               if( mar[ii] >= mbot && mar[ii] <= mtop && mar[ii] != 0 && cmask[ii]) { mar[ii]=1; ++nonzero; }
               else { mar[ii] = 0; }
         } else {
            for( ii=0 ; ii < nvox ; ii++ )
               if( mar[ii] >= mbot && mar[ii] <= mtop && mar[ii] != 0 ) { mar[ii]=1; ++nonzero; }
               else { mar[ii] = 0; }
         }
      }
      break ;

      case MRI_byte:{
         byte mbot , mtop ;
         byte *mar = (byte *) DSET_ARRAY(mask_dset,miv) ;
         float mfac = DSET_BRICK_FACTOR(mask_dset,miv) ;
         if( mfac == 0.0 ) mfac = 1.0 ;
         if( mask_bot <= mask_top && mask_top > 0.0 ){
            mbot = BYTEIZE(mask_bot/mfac) ;
            mtop = BYTEIZE(mask_top/mfac) ;
         } else {
            mbot = 0 ;
            mtop = (byte) MRI_TYPE_maxval[MRI_short] ;
         }
         if (cmask) {
            for( ii=0 ; ii < nvox ; ii++ )
               if( mar[ii] >= mbot && mar[ii] <= mtop && mar[ii] != 0 && cmask[ii]){ mar[ii]=1; ++nonzero; }
               else { mar[ii] = 0; }
         } else {
            for( ii=0 ; ii < nvox ; ii++ )
               if( mar[ii] >= mbot && mar[ii] <= mtop && mar[ii] != 0 ){ mar[ii]=1; ++nonzero; }
               else { mar[ii] = 0; }
         }
      }
      break ;

      case MRI_float:{
         float mbot , mtop ;
         float *mar = (float *) DSET_ARRAY(mask_dset,miv) ;
         float mfac = DSET_BRICK_FACTOR(mask_dset,miv) ;
         if( mfac == 0.0 ) mfac = 1.0 ;
         if( mask_bot <= mask_top ){
            mbot = (float) (mask_bot/mfac) ;
            mtop = (float) (mask_top/mfac) ;
         } else {
            mbot = -WAY_BIG ;
            mtop =  WAY_BIG ;
         }
         if (cmask) {
            for( ii=0 ; ii < nvox ; ii++ )
               if( mar[ii] >= mbot && mar[ii] <= mtop && mar[ii] != 0 && cmask[ii]) { mar[ii]=1; ++nonzero; }
               else { mar[ii] = 0; }
         } else {
            for( ii=0 ; ii < nvox ; ii++ )
               if( mar[ii] >= mbot && mar[ii] <= mtop && mar[ii] != 0 ) { mar[ii]=1; ++nonzero; }
               else { mar[ii] = 0; }
         }
      }
      break ;
   }

   return (nonzero) ;
}
/*! 
   Returns a list of the unique values in a dataset.
*/
extern int * UniqueInt (int *y, int ysz, int *kunq, int Sorted );
int *THD_unique_vals( THD_3dim_dataset *mask_dset ,
                        int miv,
                        int *n_unique , 
                        byte *cmask)
{
   int nvox , ii, *unq = NULL, *vals=NULL;

   *n_unique = 0;
   unq = NULL ;
   
   if( !ISVALID_DSET(mask_dset)    ||
       miv < 0                     ||
       miv >= DSET_NVALS(mask_dset)  ) {
   
      fprintf(stderr,"** Bad mask_dset or sub-brick index.\n");
      return (unq) ;
   
   }
   nvox = DSET_NVOX(mask_dset) ;

   DSET_load(mask_dset) ; if( !DSET_LOADED(mask_dset) ) return (unq) ;

   vals = (int *)malloc(sizeof(int)*nvox);
   if (!vals) {
      fprintf(stderr,"** Failed to allocate.\n");
      return (unq) ;
   }
   
   switch( DSET_BRICK_TYPE(mask_dset,miv) ){
      default:
         fprintf(stderr,"** Bad dset type for unique operation.\nOnly Byte and Short dsets are allowed.\n");
         DSET_unload(mask_dset) ; if (vals) free(vals); return (unq) ;

      case MRI_short:{
         short *mar = (short *) DSET_ARRAY(mask_dset,miv) ;
         float mfac = DSET_BRICK_FACTOR(mask_dset,miv) ;
         if( mfac == 0.0 ) mfac = 1.0 ;
         
         if (cmask) {
            for( ii=0 ; ii < nvox ; ii++ )
               if (cmask[ii]) vals[ii] = (int)(mar[ii]*mfac); else vals[ii] = 0;
         } else {
            for( ii=0 ; ii < nvox ; ii++ )
               vals[ii] = (int)(mar[ii]*mfac);
         }
         
      }
      break ;

      case MRI_byte:{
         byte *mar = (byte *) DSET_ARRAY(mask_dset,miv) ;
         float mfac = DSET_BRICK_FACTOR(mask_dset,miv) ;
         if( mfac == 0.0 ) mfac = 1.0 ;
         
         if (cmask) {
            for( ii=0 ; ii < nvox ; ii++ )
               if (cmask[ii]) vals[ii] = (int)(mar[ii]*mfac); else vals[ii] = 0;
         } else {
            for( ii=0 ; ii < nvox ; ii++ )
               vals[ii] = (int)(mar[ii]*mfac);
         }

      }
      break ;

      #if 0 /* bad idea */
      case MRI_float:{
         float *mar = (float *) DSET_ARRAY(mask_dset,miv) ;
         float mfac = DSET_BRICK_FACTOR(mask_dset,miv) ;
         if( mfac == 0.0 ) mfac = 1.0 ;
         
         if (cmask) {
            for( ii=0 ; ii < nvox ; ii++ )
               if (cmask[ii]) vals[ii] = (int)(mar[ii]*mfac); else vals[ii] = 0;
         } else {
            for( ii=0 ; ii < nvox ; ii++ )
               vals[ii] = (int)(mar[ii]*mfac);
         }

      }
      break ;
      #endif
   }

   /* unique */
   unq = UniqueInt (vals, nvox, n_unique, 0 );
   
   free(vals); vals = NULL;
   
   return (unq) ;
}
/*---------------------------------------------------------------------*/
/*! Count the number of nonzero voxels in a mask.
-----------------------------------------------------------------------*/

int THD_countmask( int nvox , byte *mmm )
{
   int ii,mc ;

   if( nvox <= 0 || mmm == NULL ) return 0 ;

   for( ii=mc=0 ; ii < nvox ; ii++ ) if( mmm[ii] ) mc++ ;

   return mc ;
}
