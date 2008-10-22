/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*****************  New routines for AFNI-96  ***************************/

#define DUMMY_NAME "zyxt"  /* 07 Mar 2000: changed from Elvis */

/*--------------------------------------------------------------------
  Routine to make a dataset that is an empty copy of an input dataset.
  Will usually be followed by EDIT_dset_items to alter some internals.
  Note that old_dset can be NULL, in which case EDIT_dset_items
  will definitely need to be called before the new dataset is of use.
----------------------------------------------------------------------*/

THD_3dim_dataset * EDIT_empty_copy( THD_3dim_dataset *old_dset )
{
   THD_3dim_dataset *new_dset ;
   THD_datablock    *new_dblk ;
   THD_dataxes      *new_daxes ;
   THD_timeaxis     *new_taxis ;
   THD_diskptr      *new_dkptr ;
   int               new_nvals , old_good ;

ENTRY("EDIT_empty_copy") ; /* 29 Aug 2001 */

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

   new_dset->idcode = MCW_new_idcode() ;
   ZERO_IDCODE(new_dset->anat_parent_idcode) ;
   ZERO_IDCODE(new_dset->warp_parent_idcode) ;

   if( old_good ){
      new_dset->type      = old_dset->type ;      /* data types */
      new_dset->func_type = old_dset->func_type ;
      new_dset->view_type = old_dset->view_type ;
      new_nvals           = old_dset->dblk->nvals ;
   } else {
      new_dset->type      = HEAD_ANAT_TYPE ;
      new_dset->func_type = ANAT_SPGR_TYPE ;
      new_dset->view_type = VIEW_ORIGINAL_TYPE ;
      new_nvals           = 1 ;
   }

   new_dset->warp        = NULL ;
   new_dset->anat_parent = NULL ;
   new_dset->markers     = NULL ;
   new_dset->warp_parent = NULL ;

   /*-- 14 Dec 1999: copy the anat_parent, too --*/

   if( old_good ){
     new_dset->anat_parent = old_dset->anat_parent  ; /* actual link, for use now */
     EDIT_COPY_ANATOMY_PARENT_ID(new_dset,old_dset) ; /* idcode, for HEAD file later */
   }

   /*-- end of anat_parent copy --*/

   new_dset->vox_warp       = myXtNew( THD_warp ) ;  /* create a voxel warp */
   new_dset->vox_warp->type = ILLEGAL_TYPE ;         /* but don't put anything in it */
   new_dset->self_warp      = NULL ;                 /* 26 Aug 2002 */

   new_dset->warp_parent_name[0] = '\0' ;
   new_dset->anat_parent_name[0] = '\0' ;

   MCW_strncpy( new_dset->self_name , DUMMY_NAME , THD_MAX_NAME  ) ;
   MCW_strncpy( new_dset->label1    , DUMMY_NAME , THD_MAX_LABEL ) ;
   MCW_strncpy( new_dset->label2    , DUMMY_NAME , THD_MAX_LABEL ) ;

   new_dset->death_mark  = 0 ;
   new_dset->tcat_list   = NULL ;
   new_dset->tcat_num    = 0 ;
   new_dset->tcat_len    = NULL ;
#ifdef ALLOW_DATASET_VLIST
   new_dset->pts         = NULL ;
#endif
   new_dset->tagset      = NULL ;  /* Oct 1998 */

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

   new_dblk->vedim = NULL ; /* 05 Sep 2006 */

   new_dblk->brick_fdrcurve = NULL ; /* 23 Jan 2008 */
   new_dblk->brick_mdfcurve = NULL ; /* 22 Oct 2008 */

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

   new_dblk->master_nvals = 0 ;     /* 11 Jan 1999 */
   new_dblk->master_ival  = NULL ;  /* Copy does not inherit mastery */
   new_dblk->master_bytes = NULL ;

   if( old_good ){
     *new_daxes  = *(old_dset->daxes) ;    /* copy all contents */
     if( !ISVALID_MAT44(new_daxes->ijk_to_dicom) )  /* 15 Dec 2005 */
       THD_daxes_to_mat44(new_daxes) ;
   } else {
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
     THD_daxes_to_mat44(new_daxes) ;
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

   RETURN( new_dset );
}

/*-----------------------------------------------------------------------*/
/*! Create a simple empty datablock, to be filled in later. */

THD_datablock * EDIT_empty_datablock(void)
{
   THD_datablock *new_dblk ;
   THD_diskptr   *new_dkptr ;

ENTRY("EDIT_empty_datablock") ;

   /** make some new places to store stuff **/

   new_dblk                 = myXtNew( THD_datablock ) ;
   new_dblk->type           = DATABLOCK_TYPE ;
   new_dblk->brick          = NULL ;
   new_dblk->brick_bytes    = NULL ;
   new_dblk->brick_fac      = NULL ;
   new_dblk->total_bytes    = 0    ;
   new_dblk->malloc_type    = DATABLOCK_MEM_UNDEFINED ;
   new_dblk->parent         = NULL ;
   new_dblk->brick_lab      = NULL ;
   new_dblk->brick_keywords = NULL ;
   new_dblk->brick_statcode = NULL ;
   new_dblk->brick_stataux  = NULL ;
   new_dblk->master_nvals   = 0    ;
   new_dblk->master_ival    = NULL ;
   new_dblk->master_bytes   = NULL ;
   new_dblk->master_bot     = 1.0  ;
   new_dblk->master_top     = 0.0  ;
   new_dblk->shm_idcode[0]  = '\0' ;
   new_dblk->nvals          = 1 ;
   new_dblk->natr           = new_dblk->natr_alloc = 0 ;
   new_dblk->atr            = NULL ;
   new_dblk->nnodes         = 0 ;       /* 12 July 2006 [rickr] */
   new_dblk->node_list      = NULL ;

   new_dblk->vedim = NULL ; /* 05 Sep 2006 */

   new_dblk->brick_fdrcurve = NULL ; /* 23 Jan 2008 */
   new_dblk->brick_mdfcurve = NULL ; /* 22 Oct 2008 */

   new_dkptr = new_dblk->diskptr = myXtNew( THD_diskptr ) ;

   new_dkptr->type         = DISKPTR_TYPE ;
   new_dkptr->rank         = 3 ;
   new_dkptr->nvals        = 1 ;
   new_dkptr->storage_mode = STORAGE_UNDEFINED ;
   new_dkptr->byte_order   = THD_get_write_order() ;
   new_dkptr->dimsizes[0]  = 2 ;
   new_dkptr->dimsizes[1]  = 2 ;
   new_dkptr->dimsizes[2]  = 2 ;

   THD_init_diskptr_names( new_dkptr ,
                           "./" , NULL , DUMMY_NAME ,
                           VIEW_ORIGINAL_TYPE , True ) ;

   INIT_KILL(new_dblk->kl) ;
   ADDTO_KILL(new_dblk->kl,new_dkptr) ;

   DBLK_unlock(new_dblk) ;
   THD_null_datablock_auxdata( new_dblk ) ;

   RETURN( new_dblk ) ;
}

/*-----------------------------------------------------------------------*/
/*! Create an empty marker set.                      13 Sep 2005 [rickr] */
/*  (copied from 3drefit.c)                                              */

THD_marker_set * create_empty_marker_set(void)
{
   THD_marker_set * markers = myXtNew( THD_marker_set ) ;
   int              ii, jj ;

   if( !markers ) return NULL;

   markers->numdef = 0 ;

   for( ii=0 ; ii < MARKS_MAXNUM ; ii++ ){       /* null all data out */
      markers->valid[ii] = 0 ;
      for( jj=0 ; jj < MARKS_MAXLAB  ; jj++ )
         markers->label[ii][jj] = '\0';
      for( jj=0 ; jj < MARKS_MAXHELP ; jj++ )
         markers->help[ii][jj]  = '\0';
   }

   for( ii=0 ; ii < NMARK_ALIGN ; ii++ ){       /* copy strings in */
      MCW_strncpy( &(markers->label[ii][0]) ,
                   THD_align_label[ii] , MARKS_MAXLAB ) ;
      MCW_strncpy( &(markers->help[ii][0]) ,
                   THD_align_help[ii] , MARKS_MAXHELP ) ;
   }

   for( ii=0 ; ii < MARKS_MAXFLAG ; ii++ )     /* copy flags in */
      markers->aflags[ii] = THD_align_aflags[ii] ;

   return markers ;
}

/*------------------------------------------------------------------*/

int okay_to_add_markers(THD_3dim_dataset * dset)
{
   if( !dset )         return 0 ;

/*    if( dset->markers ) return 0 ;  allow deletion   9 Aug 2006 [rickr] */

   /* test comes from 3drefit.c */
   if( dset->type           == HEAD_ANAT_TYPE     &&
       dset->view_type      == VIEW_ORIGINAL_TYPE &&
       DSET_NUM_TIMES(dset) == 1                  &&
       DSET_NVALS(dset)     == 1 )
     return 1 ;

   return 0 ;
}
