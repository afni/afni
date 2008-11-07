/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*---------------------------------------------------------------------------*/
/*!  Routine to substitute one brick array for another in a 3D dataset.
     - dset  = dataset to be edited
     - ival  = index of sub-brick to be replaced
     - ftype = datum type of substitute array
     - fim   = array of substitute data -- it may be NULL.  If it is not NULL,
               then should contain mri_datum_size(ftype) * nxx*nyy*nzz bytes.
               If it is NULL, then space will be calloc-ed.

   Notes:
     - The original brick (an MRI_IMAGE within an MRI_IMARR) is deleted
       from memory.  A new brick is put in its place.
     - This can only be done on a brick that is malloc-ed, not mmap-ed!
     - The brick_bytes and total_bytes field of the datablock are
       patched, but the brick_fac field is NOT changed here.
-----------------------------------------------------------------------------*/

void EDIT_substitute_brick(THD_3dim_dataset *dset, int ival, int ftype,void *fim)
{
   THD_datablock *dblk ;
   MRI_IMAGE *newim , *oldim ;
   int nbytes , nullfim = (fim == NULL) ;

ENTRY("EDIT_substitute_brick") ;

   /**-- Sanity Checks --**/

   if( ! ISVALID_3DIM_DATASET(dset) )                   EXRETURN; /* error! */
   if( dset->dblk->brick == NULL )                      EXRETURN; /* ditto! */
   if( dset->dblk->malloc_type != DATABLOCK_MEM_MALLOC )EXRETURN; /* ditto! */
   if( ival >= dset->dblk->nvals || ival < 0 )          EXRETURN; /* ditto! */
   if( ftype < 0 || ftype > LAST_MRI_TYPE )             EXRETURN; /* ditto! */

   oldim = DSET_BRICK(dset,ival) ; if( oldim == NULL )  EXRETURN; /* ditto! */

   newim  = mri_empty_conforming( oldim , ftype ) ;      /* new sub-brick */
   nbytes = newim->nvox * newim->pixel_size ;            /* how big it is */
   mri_free( oldim ) ;                                   /* kill old one  */

   if( nullfim ){                                        /* if needed, */
      fim = calloc( 1,nbytes ) ;                         /* make array */
      if( fim == NULL ){
        fprintf(stderr,"\n*** malloc error for dataset sub-brick\n") ; EXIT(1) ;
      }
   }
   mri_fix_data_pointer( fim , newim ) ;                 /* attach new data */
   DSET_BRICK(dset,ival) = newim ;                       /* put in dataset  */

   /** change the byte count for this sub-brick and the total dataset **/

   dset->dblk->total_bytes      += (nbytes - dset->dblk->brick_bytes[ival]) ;
   dset->dblk->brick_bytes[ival] = nbytes ;

   DSET_CRUSH_BSTAT(dset,ival) ;

   THD_patch_dxyz_one(dset,ival) ;  /* 05 Jun 2007 */

   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/*! Similar to EDIT_substitute_brick(), but allows the data type to be
    converted/scaled.  The brick_fac field will be set.  [18 Sep 2006]
-----------------------------------------------------------------------------*/

void EDIT_substscale_brick(THD_3dim_dataset *dset, int ival,
                           int ftype,void *fim , int stype,float fac )
{
   float *far ;
   int ii,nvox ;

ENTRY("EDIT_substscale_brick") ;

   /**-- Sanity Checks --**/

   if( ! ISVALID_3DIM_DATASET(dset) )                   EXRETURN; /* error! */
   if( dset->dblk->brick == NULL )                      EXRETURN; /* ditto! */
   if( dset->dblk->malloc_type != DATABLOCK_MEM_MALLOC )EXRETURN; /* ditto! */
   if( ival >= dset->dblk->nvals || ival < 0 )          EXRETURN; /* ditto! */
   if( ftype < 0 || ftype > LAST_MRI_TYPE )             EXRETURN; /* ditto! */

   /** the trivial operation? **/

   if( stype < 0 || stype > LAST_MRI_TYPE || stype == ftype ){
     EDIT_substitute_brick( dset,ival,ftype,fim ) ;
     EDIT_BRICK_FACTOR( dset,ival,0.0f ) ;
     EXRETURN ;
   }
   if( fim == NULL ){
     EDIT_substitute_brick( dset,ival,stype,NULL ) ;
     EDIT_BRICK_FACTOR( dset,ival,0.0f ) ;
     EXRETURN ;
   }

   /** at this time, can only scale float inputs to shorts or bytes **/

   if( ftype != MRI_float ){
     ERROR_message("EDIT_substscale_brick: non-float input!"); EXRETURN;
   }
   if( stype != MRI_short && stype != MRI_byte ){
     ERROR_message("EDIT_substscale_brick: non-short/byte output!"); EXRETURN;
   }

   far = (float *)fim ; nvox = DSET_NVOX(dset) ;

   /** compute factor, if not supplied by user **/

   if( fac <= 0.0f ){
     float bot,top , abot,atop,mmm ; int isin ;
     bot = top = far[0] ;
     for( ii=1 ; ii < nvox ; ii++ ){
            if( far[ii] < bot ) bot = far[ii] ;
       else if( far[ii] > top ) top = far[ii] ;
     }
     abot = fabsf(bot); atop = fabsf(top); mmm = MAX(abot,atop);
     if( mmm == 0.0f ){  /** data values are all zero! **/
       fac = 1.0f ;
     } else if( stype == MRI_short ){
       isin = is_integral_data( nvox , MRI_float , far ) ;
       fac = (isin && mmm <= 32767.0f) ? 1.0f : 32767.0f / mmm ;
     } else if( stype == MRI_byte ){
       if( bot < 0.0f ){
         for( ii=0 ; ii < nvox ; ii++ ) if( far[ii] < 0.0f ) far[ii] = 0.0f ;
       }
       if( top > 0.0f ){
         isin = is_integral_data( nvox , MRI_float , far ) ;
         fac = (isin && top <= 255.0f) ? 1.0f : 255.0f / top ;
       } else {
         WARNING_message("EDIT_substscale_brick: no positive data for -> byte");
         fac = 1.0f ;
       }
     }
   }

   /*-- now do the scaling and substitution --*/

   if( stype == MRI_short ){

     short *sar = (short *)malloc(sizeof(short)*nvox) ;
     float val ;
     if( fac == 1.0f ){
       STATUS("storing to shorts") ;
       for( ii=0 ; ii < nvox ; ii++ ){
         sar[ii] = SHORTIZE(far[ii]) ;
       }
     } else {
       STATUS("scaling to shorts") ;
       for( ii=0 ; ii < nvox ; ii++ ){
         val = fac*far[ii] ; sar[ii] = SHORTIZE(val) ;
       }
     }
     STATUS("putting into dataset") ;
     EDIT_substitute_brick( dset,ival,MRI_short,sar ) ;
     fac = (fac==1.0f) ? 0.0f : 1.0f/fac ;
     STATUS("setting scale factor") ;
     EDIT_BRICK_FACTOR( dset,ival,fac ) ;

     EDIT_misfit_report( DSET_FILECODE(dset), ival,
                         nvox , fac , sar , far  ) ;

   } else if( stype == MRI_byte ){

     byte *bar = (byte *)malloc(sizeof(byte)*nvox) ;
     float val ;
     if( fac == 1.0f ){
       for( ii=0 ; ii < nvox ; ii++ ) bar[ii] = BYTEIZE(far[ii]) ;
     } else {
       for( ii=0 ; ii < nvox ; ii++ ){
         val = fac*far[ii] ; bar[ii] = BYTEIZE(val) ;
       }
     }
     EDIT_substitute_brick( dset,ival,MRI_byte,bar ) ;
     fac = (fac==1.0f) ? 0.0f : 1.0f/fac ;
     EDIT_BRICK_FACTOR( dset,ival,fac ) ;
   }

   EXRETURN ;
}
