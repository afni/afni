/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "thd.h"

static int use_3D_format = 0 ;

void THD_use_3D_format( int uu ){ use_3D_format = uu; }

/*----------------------------------------------------------------*/
/*! This routine writes all the header data in the struct to
    the datablock attributes, then writes the dataset to
    disk.

   29 Apr 1998: erase attributes that are unused, so that
                  they won't be left over from a previous life

   09 May 2005: attributes are now set in function
                THD_set_dataset_attributes() rather than here
------------------------------------------------------------------*/

Boolean THD_write_3dim_dataset( char *new_sessname , char *new_prefixname ,
                                THD_3dim_dataset *dset , Boolean write_brick )
{
   THD_datablock *blk ;
   int ii ;

ENTRY("THD_write_3dim_dataset") ;

   /*-- sanity checks --*/

   if( ! ISVALID_3DIM_DATASET(dset)    ||
       ! ISVALID_DATABLOCK(dset->dblk) ||
       ! ISVALID_DISKPTR(dset->dblk->diskptr) ) RETURN(False) ;

   blk = dset->dblk ;

   /* Can only write AFNI formatted datasets */

   if( DSET_IS_MINC(dset)     ) RETURN(False) ;  /* 29 Oct 2001 */
   if( DSET_IS_MASTERED(dset) ) RETURN(False) ;  /* 11 Jan 1999 */
   if( DSET_IS_ANALYZE(dset)  ) RETURN(False) ;  /* 27 Aug 2002 */
   if( DSET_IS_NIFTI(dset)    ) RETURN(False) ;  /* 28 Aug 2003 */
   if( DSET_IS_CTFMRI(dset)   ) RETURN(False) ;  /* 05 Dec 2002 */
   if( DSET_IS_CTFSAM(dset)   ) RETURN(False) ;  /* 05 Dec 2002 */
   if( DSET_IS_TCAT(dset)     ) RETURN(False) ;  /* 05 Aug 2004 */

   if( DSET_IS_VOLUMES(dset) && write_brick ) RETURN(False) ;  /* 20 Jun 2002 */

   if( DSET_IS_1D(dset) ||
       ( DSET_NY(dset)==1 && DSET_NZ(dset)==1 ) ){            /* 04 Mar 2003 */

     THD_write_1D( new_sessname , new_prefixname , dset ) ;
     RETURN(True) ;
   }

   /*------------------------------*/
   /*-----  change filenames? -----*/

   THD_init_diskptr_names( blk->diskptr ,
                           new_sessname , NULL , new_prefixname ,
                           dset->view_type , True ) ;

   /*----- 09 May 2005: set attribute structs in the datablock -----*/

   THD_set_dataset_attributes( dset ) ;

   if( DSET_IS_3D(dset) || use_3D_format ){                   /* 21 Mar 2003 */
     THD_write_3D( NULL, NULL, dset ) ; RETURN(True) ;
   }

   /*----- write datablock to disk in AFNI .HEAD/.BRIK format -----*/

   RETURN( THD_write_datablock(blk,write_brick) ) ;
}
