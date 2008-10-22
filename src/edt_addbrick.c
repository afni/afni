/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*---------------------------------------------------------------------------*/
/*! Routine to attach sub-bricks to the end of an already existing 3D dataset.
    - nbr = number of extra bricks
    - tbr = array of data types in each brick (defaults to MRI_short)
    - fbr = array of new brick_fac factors in each brick
             if NULL, new factors will be set to zero
    - sbr = array of pointers to data in each brick
             if sbr[i]==NULL, then the i'th extra brick will not have data
             if sbr==NULL, then all new extra bricks will not have data
               (in which case you will have to use mri_fix_data_pointer to
                attach data at a later time, if you plan to use the data)

  Note that this can only be done on a brick that is malloc-ed, not mmap-ed!
-----------------------------------------------------------------------------*/

void EDIT_add_bricklist( THD_3dim_dataset *dset ,
                         int nbr, int *tbr, float *fbr , void *sbr[] )
{
   int ibr , typ , nx,ny,nz , nvals,new_nvals ;
   THD_datablock *dblk ;
   MRI_IMAGE *qim ;
   char str[32] ;

ENTRY("EDIT_add_bricklist") ;

   /**-- Sanity Checks --**/

   if( ! ISVALID_3DIM_DATASET(dset) || nbr <= 0 )       EXRETURN; /* error! */
   if( dset->dblk->brick == NULL )                      EXRETURN; /* error! */
   if( dset->dblk->malloc_type != DATABLOCK_MEM_MALLOC )EXRETURN; /* error! */

   dblk  = dset->dblk ;
   nvals = dblk->nvals ;
   nx    = dblk->diskptr->dimsizes[0] ;
   ny    = dblk->diskptr->dimsizes[1] ;
   nz    = dblk->diskptr->dimsizes[2] ;

   /**-- reallocate the brick control information --**/

   new_nvals = nvals + nbr ;
   dblk->brick_bytes = (int *) XtRealloc( (char *) dblk->brick_bytes ,
                                          sizeof(int) * new_nvals ) ;

   dblk->brick_fac = (float *) XtRealloc( (char *) dblk->brick_fac ,
                                          sizeof(float) * new_nvals ) ;

   dblk->nvals = dblk->diskptr->nvals = new_nvals ;

   /** allocate new sub-brick images **/

   for( ibr=0 ; ibr < nbr ; ibr++ ){
      typ = (tbr != NULL ) ? tbr[ibr] : MRI_short ;
      qim = mri_new_vol_empty( nx,ny,nz , typ ) ;  /* image with no data */

      if( sbr != NULL && sbr[ibr] != NULL )        /* attach data to image */
         mri_fix_data_pointer( sbr[ibr] , qim ) ;

      ADDTO_IMARR( dblk->brick , qim ) ;           /* attach image to dset */

      dblk->brick_fac[nvals+ibr]   = (fbr != NULL) ? fbr[ibr] : 0.0 ;
      dblk->brick_bytes[nvals+ibr] = qim->pixel_size * qim->nvox ;
      dblk->total_bytes           += dblk->brick_bytes[ibr] ;
   }

   /** allocate new sub-brick auxiliary data: labels **/

   if( dblk->brick_lab == NULL )
      THD_init_datablock_labels( dblk ) ;
   else
      dblk->brick_lab = (char **) XtRealloc( (char *) dblk->brick_lab ,
                                             sizeof(char *) * new_nvals ) ;
   for( ibr=0 ; ibr < nbr ; ibr++ ){
      sprintf( str , "#%d" , nvals+ibr ) ;
      dblk->brick_lab[nvals+ibr] = NULL ;
      THD_store_datablock_label( dblk , nvals+ibr , str ) ;
   }

   /** keywords **/

   if( dblk->brick_keywords == NULL )
      THD_init_datablock_keywords( dblk ) ;
   else
      dblk->brick_keywords = (char **) XtRealloc( (char *) dblk->brick_keywords ,
                                                  sizeof(char *) * new_nvals ) ;
   for( ibr=0 ; ibr < nbr ; ibr++ ){
      dblk->brick_keywords[nvals+ibr] = NULL ;
      THD_store_datablock_keywords( dblk , nvals+ibr , NULL ) ;
   }

   /** stataux **/

   if( dblk->brick_statcode != NULL ){
      dblk->brick_statcode = (int *) XtRealloc( (char *) dblk->brick_statcode ,
                                                sizeof(int) * new_nvals        ) ;
      dblk->brick_stataux  = (float **) XtRealloc( (char *) dblk->brick_stataux ,
                                                   sizeof(float *) * new_nvals ) ;

      for( ibr=0 ; ibr < nbr ; ibr++ ){
         dblk->brick_statcode[nvals+ibr] = 0 ;
         dblk->brick_stataux[nvals+ibr]  = NULL ;
      }
   }

   /** fdrcurve **/

   if( dblk->brick_fdrcurve != NULL ){
     dblk->brick_fdrcurve = (floatvec **) realloc( (void *)dblk->brick_fdrcurve ,
                                                  sizeof(floatvec *) * new_nvals ) ;
     for( ibr=0 ; ibr < nbr ; ibr++ ) dblk->brick_fdrcurve[nvals+ibr] = NULL ;
   }

   if( dblk->brick_mdfcurve != NULL ){  /* 22 Oct 2008 */
     dblk->brick_mdfcurve = (floatvec **) realloc( (void *)dblk->brick_mdfcurve ,
                                                  sizeof(floatvec *) * new_nvals ) ;
     for( ibr=0 ; ibr < nbr ; ibr++ ) dblk->brick_mdfcurve[nvals+ibr] = NULL ;
   }

   EXRETURN;
}

/*---------------------------------------------------------------------------*/
/*! Add one brick to the end of a dataset. */

void EDIT_add_brick( THD_3dim_dataset *dset, int typ , float fac , void *br )
{
   int   ttt = typ ;
   float fff = fac ;
   void *bbb = br ;

   EDIT_add_bricklist( dset , 1 , &ttt , &fff , &bbb ) ;
   return ;
}
