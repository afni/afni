/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

#undef  AFNI_DEBUG
#undef  CLUST_DEBUG
#define STATUS(x) /* nada */
#define ENTRY(x)  /* nada */
#define EXRETURN  return
#define RETURN(x) return(x)

/*---------------------------------------------------------------------------
   Routine to substitute one brick array for another in a 3D dataset.
     dset  = dataset to be edited
     ival  = index of sub-brick to be replaced
     ftype = datum type of substitute array
     fim   = array of substitute data -- it may be NULL.  If it is not NULL,
             then should contain mri_datum_size(ftype) * nxx*nyy*nzz bytes.
             If it is NULL, then space will be malloc-ed.

   Notes: 1) The original brick (an MRI_IMAGE within an MRI_IMARR) is deleted
             from memory.  A new brick is put in its place.
          2) This can only be done on a brick that is malloc-ed, not mmap-ed!
          3) The brick_bytes and total_bytes field of the datablock are
             patched, but the brick_fac field is not changed here.
-----------------------------------------------------------------------------*/

void EDIT_substitute_brick(THD_3dim_dataset * dset, int ival, int ftype,void * fim)
{
   THD_datablock * dblk ;
   MRI_IMAGE * newim , * oldim ;
   int nbytes , nullfim = (fim == NULL) ;

   /**-- Sanity Checks --**/

   if( ! ISVALID_3DIM_DATASET(dset) )                    return ; /* error! */
   if( dset->dblk->brick == NULL )                       return ; /* ditto! */
   if( dset->dblk->malloc_type != DATABLOCK_MEM_MALLOC ) return ; /* ditto! */
   if( ival >= dset->dblk->nvals || ival < 0 )           return ; /* ditto! */
   if( ftype < 0 || ftype > LAST_MRI_TYPE )              return ; /* ditto! */

   oldim = DSET_BRICK(dset,ival) ; if( oldim == NULL )   return ; /* ditto! */

   newim  = mri_empty_conforming( oldim , ftype ) ;      /* new sub-brick */
   nbytes = newim->nvox * newim->pixel_size ;            /* how big it is */
   mri_free( oldim ) ;                                   /* kill old one  */

   if( nullfim ){                                        /* if needed, */
      fim = malloc( nbytes ) ;                           /* make array */
      if( fim == NULL ){
         fprintf(stderr,"\n*** malloc error for dataset sub-brick\n") ; exit(1) ;
      }
   }
   mri_fix_data_pointer( fim , newim ) ;                 /* attach new data */
   DSET_BRICK(dset,ival) = newim ;                       /* put in dataset  */

   /** change the byte count for this sub-brick and the total dataset **/

   dset->dblk->total_bytes      += (nbytes - dset->dblk->brick_bytes[ival]) ;
   dset->dblk->brick_bytes[ival] = nbytes ;

   DSET_CRUSH_BSTAT(dset,ival) ;

   return ;
}
