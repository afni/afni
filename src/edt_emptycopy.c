#include "mrilib.h"

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/


#undef  AFNI_DEBUG
#undef  CLUST_DEBUG
#define STATUS(x) /* nada */
#define ENTRY(x)  /* nada */
#define EXRETURN  return
#define RETURN(x) return(x)


/*****************  New routines for AFNI-96  ***************************/

#define DUMMY_NAME "Elvis"

/*--------------------------------------------------------------------
  Routine to make a dataset that is an empty copy of an input dataset.
  Will usually be followed by EDIT_dset_items to alter some internals.
  Note that old_dset can be NULL, in which case EDIT_dset_items
  will definitely need to be called before the new dataset is of use.
----------------------------------------------------------------------*/

THD_3dim_dataset * EDIT_empty_copy( THD_3dim_dataset * old_dset )
{
   THD_3dim_dataset * new_dset ;
   THD_datablock    * new_dblk ;
   THD_dataxes      * new_daxes ;
   THD_timeaxis     * new_taxis ;
   THD_diskptr      * new_dkptr ;
   int                new_nvals , old_good ;

   old_good = ISVALID_3DIM_DATASET(old_dset) ;

   /** make some new places to store stuff **/

   new_dset                      = myXtNew( THD_3dim_dataset ) ;
   new_dblk  = new_dset->dblk    = myXtNew( THD_datablock ) ;
   new_daxes = new_dset->daxes   = myXtNew( THD_dataxes ) ;
   new_dkptr = new_dblk->diskptr = myXtNew( THD_diskptr ) ;

   INIT_KILL(new_dset->kl) ; INIT_KILL(new_dblk->kl) ;
   ADDTO_KILL(new_dset->kl,new_dblk)  ;
   ADDTO_KILL(new_dset->kl,new_daxes) ;
   ADDTO_KILL(new_dset->kl,new_dkptr) ;

   new_dset->wod_daxes         = myXtNew(THD_dataxes) ;
   new_dset->wod_daxes->parent = (XtPointer) new_dset ;
   new_dset->wod_flag          = False ;

   ADDTO_KILL(new_dset->kl,new_dset->wod_daxes) ;

#ifndef OMIT_DATASET_IDCODES
   new_dset->idcode = MCW_new_idcode() ;
   ZERO_IDCODE(new_dset->anat_parent_idcode) ;
   ZERO_IDCODE(new_dset->warp_parent_idcode) ;
#endif

   if( old_good ){
      new_dset->type      = old_dset->type ;      /* data types */
      new_dset->func_type = old_dset->func_type ;
      new_dset->view_type = old_dset->view_type ;
      new_nvals           = old_dset->dblk->nvals ;
   } else {
      new_dset->type      = HEAD_ANAT_TYPE ;
      new_dset->func_type = ANAT_SPGR_TYPE ;
      new_dset->view_type = VIEW_ORIGINAL_TYPE ;
      new_nvals           = ANAT_nvals[new_dset->func_type] ;
   }

   new_dset->warp        = NULL ;
   new_dset->anat_parent = NULL ;
   new_dset->markers     = NULL ;
   new_dset->warp_parent = NULL ;

   new_dset->vox_warp       = myXtNew( THD_warp ) ;  /* create a voxel warp */
   new_dset->vox_warp->type = ILLEGAL_TYPE ;       /* but don't put anything in it */

   new_dset->warp_parent_name[0] = '\0' ;
   new_dset->anat_parent_name[0] = '\0' ;

   MCW_strncpy( new_dset->self_name , DUMMY_NAME , THD_MAX_NAME  ) ;
   MCW_strncpy( new_dset->label1    , DUMMY_NAME , THD_MAX_LABEL ) ;
   MCW_strncpy( new_dset->label2    , DUMMY_NAME , THD_MAX_LABEL ) ;

   new_dset->merger_list = NULL ;
   new_dset->death_mark  = 0 ;
   new_dset->pts         = NULL ;

   new_dkptr->type         = DISKPTR_TYPE ;
   new_dkptr->rank         = 3 ;
   new_dkptr->nvals        = new_nvals ;
   new_dkptr->storage_mode = STORAGE_BY_BRICK ;
   new_dkptr->byte_order   = THD_get_write_order() ;  /* 25 April 1998 */
   if( old_good ){
      new_dkptr->dimsizes[0]  = old_dset->daxes->nxx ;
      new_dkptr->dimsizes[1]  = old_dset->daxes->nyy ;
      new_dkptr->dimsizes[2]  = old_dset->daxes->nzz ;
   } else {
      new_dkptr->dimsizes[0]  = 2 ;
      new_dkptr->dimsizes[1]  = 2 ;
      new_dkptr->dimsizes[2]  = 2 ;
   }

   if( old_good )
      THD_init_diskptr_names( new_dkptr ,
                              old_dset->dblk->diskptr->directory_name ,
                              NULL , DUMMY_NAME ,
                              new_dset->view_type , True ) ;
   else
      THD_init_diskptr_names( new_dkptr ,
                              "./" , NULL , DUMMY_NAME ,
                              new_dset->view_type , True ) ;

   new_dblk->type        = DATABLOCK_TYPE ;
   new_dblk->nvals       = new_nvals ;
   new_dblk->malloc_type = DATABLOCK_MEM_MALLOC ;
   new_dblk->natr        = new_dblk->natr_alloc = 0 ;
   new_dblk->atr         = NULL ;
   new_dblk->parent      = (XtPointer) new_dset ;

   DBLK_unlock(new_dblk) ;  /* Feb 1998 */

   new_dblk->brick_fac   = NULL ;
   new_dblk->brick_bytes = NULL ;
   new_dblk->brick       = NULL ;
   if( old_good )
      THD_init_datablock_brick( new_dblk , -1 , old_dset->dblk ) ;
   else
      THD_init_datablock_brick( new_dblk , MRI_short , NULL ) ;

   if( old_good && old_dset->keywords != NULL )
      THD_store_dataset_keywords( new_dset , old_dset->keywords ) ;
   else
      new_dset->keywords = NULL ;

   THD_null_datablock_auxdata( new_dblk ) ;
   if( old_good ) THD_copy_datablock_auxdata( old_dset->dblk , new_dblk ) ;

   if( old_good )
      *new_daxes  = *(old_dset->daxes) ;    /* copy all contents */
   else {
      new_daxes->type = DATAXES_TYPE ;      /* make up contents */

      new_daxes->nxx = new_dkptr->dimsizes[0] ;
      new_daxes->nyy = new_dkptr->dimsizes[1] ;
      new_daxes->nzz = new_dkptr->dimsizes[2] ;

      new_daxes->xxorg = new_daxes->yyorg = new_daxes->zzorg = -0.5 ;
      new_daxes->xxdel = new_daxes->yydel = new_daxes->zzdel =  1.0 ;

      new_daxes->xxorient = ORI_R2L_TYPE ;
      new_daxes->yyorient = ORI_A2P_TYPE ;
      new_daxes->zzorient = ORI_I2S_TYPE ;
      LOAD_DIAG_MAT(new_daxes->to_dicomm,1,1,1) ;

      new_daxes->xxmin = new_daxes->yymin = new_daxes->zzmin = -0.5 ;
      new_daxes->xxmax = new_daxes->yymax = new_daxes->zzmax =  0.5 ;
   }
   new_daxes->parent = (XtPointer) new_dset ;

   new_dset->stats   = NULL ;
   new_dset->parent  = NULL ;

   if( old_good )
      INIT_STAT_AUX( new_dset , MAX_STAT_AUX , old_dset->stat_aux ) ;
   else
      ZERO_STAT_AUX( new_dset ) ;

   if( old_good && ISVALID_TIMEAXIS(old_dset->taxis) ){
      new_taxis = new_dset->taxis = myXtNew( THD_timeaxis ) ;

      *new_taxis = *old_dset->taxis ;  /* copy contents */

      if( new_taxis->nsl > 0 ){        /* copy toff_sl array, if present */
         int isl ;
         new_taxis->toff_sl = (float *) XtMalloc( sizeof(float) * new_taxis->nsl ) ;
         for( isl = 0 ; isl < new_taxis->nsl ; isl++ )
            new_taxis->toff_sl[isl] = old_dset->taxis->toff_sl[isl] ;
      } else {
         new_taxis->toff_sl = NULL ;
      }
   } else {
      new_dset->taxis = NULL ;
   }

   return new_dset ;
}
