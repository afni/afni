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

/*-----------------------------------------------------------------------
  Edit some internals of a dataset.  Notice that it is possible to
  create a dataset which is inconsistent.  Note also that some operations
  cannot be performed on a dataset containing actual data -- for example,
  you cannot change the types of sub-bricks if they already are attached
  to data arrays!

  This routine uses the <stdarg.h> variable argument interface.
  The first argument is a pointer to the dataset to edit.
  All succeeding arguments are pairs indicating which internal item
  to edit, and the new value for that item.  The first entry of a pair
  must be one of the ADN_ entries defined in editvol.h.  The type of the
  second entry of a pair depends on the particular item.
  [*** WARNING: due to C automatic 'type promotion', you cannot use
       float as the type to extract from the list -- you must use double!]

  Finally, the end of the argument list is found by the last argument
  being the special code ADN_none.

  The return value is the number of errors detected.  If this is > 0,
  the input dataset is unchanged.
-------------------------------------------------------------------------*/

#ifdef THD_DEBUG
#  define EDERR(str) \
     do{ fprintf(stderr,"*** EDIT_dset_items: %s\n",str) ; errnum++ ; } while(0)
#else
/* #  define EDERR(str) errnum++ */
#  define EDERR(str) \
     do{ fprintf(stderr,"*** EDIT_dset_items: %s\n",str) ; errnum++ ; } while(0)
#endif

int EDIT_dset_items( THD_3dim_dataset * dset , ... )
{
   va_list vararg_ptr ;
   int     flag_arg , errnum = 0 ;
   int     redo_bricks , redo_daxes , redo_taxis , ii ;
   void *  dummy ;
   int     iarg ;

   /**----- variables to flag and store presence of arguments -----**/

   int new_prefix         = 0 ; char *              prefix         = NULL ;
   int new_directory_name = 0 ; char *              directory_name = NULL ;
   int new_brick_fac      = 0 ; float *             brick_fac      = NULL ;
   int new_malloc_type    = 0 ; int                 malloc_type    = ILLEGAL_TYPE ;
   int new_datum_all      = 0 ; int                 datum_all      = ILLEGAL_TYPE ;
   int new_datum_array    = 0 ; int *               datum_array    = NULL ;
   int new_nvals          = 0 ; int                 nvals          = 0 ;
   int new_nxyz           = 0 ; THD_ivec3           nxyz           ;
   int new_xyzdel         = 0 ; THD_fvec3           xyzdel         ;
   int new_xyzorg         = 0 ; THD_fvec3           xyzorg         ;
   int new_xyzorient      = 0 ; THD_ivec3           xyzorient      ;
   int new_to_dicomm      = 0 ; THD_mat33           to_dicomm      ;
   int new_ntt            = 0 ; int                 ntt            = 0 ;
   int new_ttorg          = 0 ; float               ttorg          = 0.0 ;
   int new_ttdel          = 0 ; float               ttdel          = 0.0 ;
   int new_ttdur          = 0 ; float               ttdur          = 0.0 ;
   int new_nsl            = 0 ; int                 nsl            = 0 ;
   int new_zorg_sl        = 0 ; float               zorg_sl        = 0.0 ;
   int new_dz_sl          = 0 ; float               dz_sl          = 0.0 ;
   int new_toff_sl        = 0 ; float *             toff_sl        = NULL ;
   int new_type           = 0 ; int                 type           = ILLEGAL_TYPE ;
   int new_view_type      = 0 ; int                 view_type      = ILLEGAL_TYPE ;
   int new_func_type      = 0 ; int                 func_type      = ILLEGAL_TYPE ;
   int new_label1         = 0 ; char *              label1         = NULL ;
   int new_label2         = 0 ; char *              label2         = NULL ;
   int new_self_name      = 0 ; char *              self_name      = NULL ;
   int new_warp_parent    = 0 ; THD_3dim_dataset *  warp_parent    = NULL ;
   int new_anat_parent    = 0 ; THD_3dim_dataset *  anat_parent    = NULL ;
   int new_stat_aux       = 0 ; float *             stat_aux       = NULL ;
   int new_warp           = 0 ; THD_warp *          warp           = NULL ;
   int new_tunits         = 0 ; int                 tunits         = ILLEGAL_TYPE ;
   int new_keywords       = 0 ; char *              keywords       = NULL ;

   /* 30 Nov 1997 */

   int new_brick_label_one = 0 ; char * brick_label_one    = NULL ;
                                 int    brick_label_one_iv = -1 ;

   int new_brick_fac_one = 0 ; float brick_fac_one    = 0.0 ;
                               int   brick_fac_one_iv = -1 ;

   int new_brick_stataux_one = 0 ; float * brick_stataux_one    = NULL ;
                                   int     brick_stataux_one_iv = -1 ;

   int new_brick_keywords_one = 0 ; char * brick_keywords_one    = NULL ;
                                    int    brick_keywords_one_iv = -1 ;

   /****---------------------- Sanity Check ----------------------****/

   if( ! ISVALID_3DIM_DATASET(dset) ) return 1 ;  /* bad data */

   /****----------- Scan input argument list;
                  - Load data into locals (va_arg);
                  - Check for legal values;
                  - Flag its presence (the new_ variables);
                  - Carry out simple processing that doesn't
                      depend on the presence of other arguments. ---------****/

   va_start( vararg_ptr , dset ) ;              /** Initialize arg reading **/
   iarg = 1 ;

   do{
      flag_arg = va_arg( vararg_ptr , int ) ;   /** Get next arg  **/
      if( flag_arg == ADN_none ) break ;        /** No more args! **/
#if 0
fprintf(stderr,"EDIT_dset_items: iarg=%d flag_arg=%d\n",iarg,flag_arg) ;
#endif
      iarg++ ;

      switch( flag_arg ){

         default:{
            int iv ;
            char str[128] ;

            /** 30 Nov 1997: check for special cases **/

            iv = flag_arg - ADN_brick_label_one ;
            if( iv >= 0 && iv < ADN_ONE_STEP ){
               brick_label_one_iv  = iv ;
               brick_label_one     = va_arg( vararg_ptr , char * ) ;
               if( brick_label_one != NULL ) new_brick_label_one = 1 ;
               break ; /* exit switch */
            }

            iv = flag_arg - ADN_brick_fac_one ;
            if( iv >= 0 && iv < ADN_ONE_STEP ){
               brick_fac_one_iv  = iv ;
               brick_fac_one     = va_arg( vararg_ptr , double ) ;
               new_brick_fac_one = 1 ;
               break ; /* exit switch */
            }

            iv = flag_arg - ADN_brick_stataux_one ;
            if( iv >= 0 && iv < ADN_ONE_STEP ){
               brick_stataux_one_iv = iv ;
               brick_stataux_one    = va_arg( vararg_ptr , float * ) ;
               if( brick_stataux_one != NULL ) new_brick_stataux_one = 1 ;
               break ; /* exit switch */
            }

            iv = flag_arg - ADN_brick_keywords_replace_one ;
            if( iv >= 0 && iv < ADN_ONE_STEP ){
               brick_keywords_one_iv  = iv ;
               brick_keywords_one     = va_arg( vararg_ptr , char * ) ;
               new_brick_keywords_one = 1 ;
               break ; /* exit switch */
            }

            iv = flag_arg - ADN_brick_keywords_append_one ;
            if( iv >= 0 && iv < ADN_ONE_STEP ){
               brick_keywords_one_iv  = iv ;
               brick_keywords_one     = va_arg( vararg_ptr , char * ) ;
               new_brick_keywords_one = 2 ;
               break ; /* exit switch */
            }

            /** not a special case? error! **/

            sprintf(str,"illegal opcode = %d at arg #%d",flag_arg,iarg) ;
            EDERR(str) ; if( errnum > 9 ) return errnum ;
            dummy = va_arg( vararg_ptr , void * ) ;  /* skip next arg */
         }
         break ;

         /** these two commands affect the disk file names **/

         case ADN_prefix:  /* processed later */
            prefix = va_arg( vararg_ptr , char * ) ;
            if( prefix != NULL ) new_prefix = 1 ;
            else EDERR("illegal new prefix") ;
         break ;

         case ADN_directory_name:  /* processed later */
            directory_name = va_arg( vararg_ptr , char * ) ;
            if( directory_name != NULL ) new_directory_name = 1 ;
            else EDERR("illegal new directory_name") ;
         break ;

         /** change the memory allocation type (mmap or malloc) **/

         case ADN_malloc_type:  /* processed now */
            malloc_type = va_arg( vararg_ptr , int ) ;
            if( ISVALID_MEM_CODE(malloc_type) ){
               new_malloc_type = 1 ;
               THD_force_malloc_type( dset->dblk , malloc_type ) ;
            }
            else EDERR("illegal new malloc_type") ;
         break ;

         /** these commands affect the data in the bricks **/

         case ADN_brick_fac:  /* processed later */
            brick_fac = va_arg( vararg_ptr , float * ) ;
            new_brick_fac = 1 ;
         break ;

         case ADN_datum_all:  /* processed later */
            datum_all = va_arg( vararg_ptr , int ) ;
            if( AFNI_GOOD_DTYPE(datum_all) ) new_datum_all = 1 ;
            else EDERR("illegal new datum_all") ;
         break ;

         case ADN_datum_array:  /* processed later */
            datum_array = va_arg( vararg_ptr , int * ) ;
            if( datum_array != NULL ) new_datum_array = 1 ;
            else EDERR("illegal new datum_array") ;
         break ;

         case ADN_nvals:  /* processed later */
            nvals = va_arg( vararg_ptr , int ) ;
            if( nvals > 0 ) new_nvals = 1 ;
            else EDERR("illegal new nvals") ;
         break ;

         /** these commands affect the spatial axes,
             which may also influence size of the data bricks **/

         case ADN_nxyz:  /* processed later */
            nxyz = va_arg( vararg_ptr , THD_ivec3 ) ;
            if( nxyz.ijk[0] > 1 && nxyz.ijk[1] > 1 && nxyz.ijk[2] > 1 )
               new_nxyz = 1 ;
               else EDERR("illegal new nxyz") ;
         break ;

         case ADN_xyzdel:  /* processed later */
            xyzdel = va_arg( vararg_ptr , THD_fvec3 ) ;
            if( xyzdel.xyz[0]!=0.0 && xyzdel.xyz[1]!=0.0 && xyzdel.xyz[2]!=0.0 )
               new_xyzdel = 1 ;
               else EDERR("illegal new xyzdel") ;
         break ;

         case ADN_xyzorg:  /* processed later */
            xyzorg = va_arg( vararg_ptr , THD_fvec3 ) ;
            new_xyzorg = 1 ;
         break ;

         case ADN_xyzorient:  /* processed later */
            xyzorient = va_arg( vararg_ptr , THD_ivec3 ) ;
            if( xyzorient.ijk[0] >= FIRST_ORIENT_TYPE &&
                xyzorient.ijk[0] <= LAST_ORIENT_TYPE  &&
                xyzorient.ijk[1] >= FIRST_ORIENT_TYPE &&
                xyzorient.ijk[1] <= LAST_ORIENT_TYPE  &&
                xyzorient.ijk[2] >= FIRST_ORIENT_TYPE &&
                xyzorient.ijk[2] <= LAST_ORIENT_TYPE  &&
                  ORIENT_xyzint[xyzorient.ijk[0]]
                + ORIENT_xyzint[xyzorient.ijk[1]]
                + ORIENT_xyzint[xyzorient.ijk[2]] == 6  )

               new_xyzorient = 1 ;
            else EDERR("illegal new xyzorient") ;
         break ;

         case ADN_to_dicomm:   /* illegal at this time */
            EDERR("to_dicomm not implemented") ;
         break ;

         /** these commands affect the time axis of the dataset **/

         case ADN_ntt:  /* processed later */
            ntt = va_arg( vararg_ptr , int ) ;
            if( ntt >= 0 ) new_ntt = 1 ;
            else EDERR("illegal new taxis ntt") ;
         break ;

         case ADN_tunits:  /* processed later */
            tunits = va_arg( vararg_ptr , int ) ;
            if( tunits >= 0 ) new_tunits = 1 ;
            else EDERR("illegal new taxis tunits") ;
         break ;

         case ADN_nsl:  /* processed later */
            nsl = va_arg( vararg_ptr , int ) ;
            if( nsl >= 0 ) new_nsl = 1 ;
            else EDERR("illegal new taxis nsl") ;
         break ;

         case ADN_ttorg:  /* processed later */
            ttorg = (float) va_arg( vararg_ptr , double ) ;
            new_ttorg = 1 ;
         break ;

         case ADN_ttdel:  /* processed later */
            ttdel = (float) va_arg( vararg_ptr , double ) ;
            new_ttdel = 1 ;
         break ;

         case ADN_ttdur:  /* processed later */
            ttdur = (float) va_arg( vararg_ptr , double ) ;
            new_ttdur = 1 ;
         break ;

         case ADN_zorg_sl:  /* processed later */
            zorg_sl = (float) va_arg( vararg_ptr , double ) ;
            new_zorg_sl = 1 ;
         break ;

         case ADN_dz_sl:  /* processed later */
            dz_sl = (float) va_arg( vararg_ptr , double ) ;
            if( dz_sl != 0.0 ) new_dz_sl = 1 ;
            else EDERR("illegal new taxis dz_sl") ;
         break ;

         case ADN_toff_sl:  /* processed later */
            toff_sl = va_arg( vararg_ptr , float * ) ;
            new_toff_sl = 1 ;
         break ;

         /** these commands affect the interpretation of the dataset
             (e.g., is it functional or anatomical, which view type, ...) **/

         case ADN_type:  /* processed later */
            type = va_arg( vararg_ptr , int ) ;
            if( type >= FIRST_3DIM_TYPE && type <= LAST_3DIM_TYPE )
               new_type = 1 ;
            else EDERR("illegal new type") ;
         break ;

         case ADN_view_type:  /* processed later */
            view_type = va_arg( vararg_ptr , int ) ;
            if( view_type >= FIRST_VIEW_TYPE && view_type <= LAST_VIEW_TYPE )
               new_view_type = 1 ;
            else EDERR("illegal new view_type") ;
         break ;

         case ADN_func_type:  /* processed later */
            func_type = va_arg( vararg_ptr , int ) ;
            if( func_type >= 0 ) new_func_type = 1 ;
            else EDERR("illegal new func_type") ;
         break ;

         /** auxiliary statistical data, for interpretation of functions **/

         case ADN_stat_aux:  /* processed now */
            stat_aux = va_arg( vararg_ptr , float * ) ;
            if( stat_aux != NULL ){
               new_stat_aux = 1 ;
               for( ii=0 ; ii < MAX_STAT_AUX ; ii++ )
                  dset->stat_aux[ii] = stat_aux[ii] ;
            } else EDERR("illegal new stat_aux") ;
         break ;

         /** dataset keywords **/

         case ADN_keywords_replace: /* processed now */
            keywords = va_arg( vararg_ptr , char * ) ;
            new_keywords = 1 ;
            THD_store_dataset_keywords( dset , keywords ) ;
         break ;

         case ADN_keywords_append: /* processed now */
            keywords = va_arg( vararg_ptr , char * ) ;
            new_keywords = 1 ;
            THD_append_dataset_keywords( dset , keywords ) ;
         break ;

         /** various labeling options **/

         case ADN_label1:  /* processed now */
            label1 = va_arg( vararg_ptr , char * ) ;
            if( label1 != NULL ){
               MCW_strncpy( dset->label1 , label1 , THD_MAX_LABEL ) ;
               new_label1 = 1 ;
            }
            else EDERR("illegal new label1") ;
         break ;

         case ADN_label2:  /* processed now */
            label2 = va_arg( vararg_ptr , char * ) ;
            if( label2 != NULL ){
               MCW_strncpy( dset->label2 , label2 , THD_MAX_LABEL ) ;
               new_label2 = 1 ;
            }
            else EDERR("illegal new label2") ;
         break ;

         case ADN_self_name:  /* processed now */
            self_name = va_arg( vararg_ptr , char * ) ;
            if( self_name != NULL ){
               MCW_strncpy( dset->self_name , self_name , THD_MAX_NAME ) ;
               new_self_name = 1 ;
            }
            else EDERR("illegal new self_name") ;
         break ;

         /** relationships to other datasets **/

         case ADN_warp_parent:  /* processed now */
            warp_parent = va_arg( vararg_ptr , THD_3dim_dataset * ) ;
            if( ISVALID_3DIM_DATASET(warp_parent) ){
               new_warp_parent = 1 ;
               dset->warp_parent = warp_parent ;
               MCW_strncpy(dset->warp_parent_name,warp_parent->self_name,THD_MAX_NAME) ;
#ifndef OMIT_DATASET_IDCODES
               dset->warp_parent_idcode = warp_parent->idcode ;
#endif
            }
            else EDERR("illegal new warp_parent") ;
         break ;

         case ADN_warp:  /* processed now */
            warp = va_arg( vararg_ptr , THD_warp * ) ;
            if( ISVALID_WARP(warp) ){
               new_warp = 1 ;
               if( dset->warp == NULL ) dset->warp = myXtNew(THD_warp) ;
               *(dset->warp) =* warp ;
            } else EDERR("illegal new warp") ;
         break ;

         case ADN_anat_parent:  /* processed now */
            anat_parent = va_arg( vararg_ptr , THD_3dim_dataset * ) ;
            if( ISVALID_3DIM_DATASET(anat_parent) ){
               new_anat_parent = 1 ;
               dset->anat_parent = anat_parent ;
               MCW_strncpy(dset->anat_parent_name,anat_parent->self_name,THD_MAX_NAME) ;
#ifndef OMIT_DATASET_IDCODES
               dset->anat_parent_idcode = anat_parent->idcode ;
#endif
            }
            else EDERR("illegal new anat_parent") ;
         break ;

      }

      iarg++ ;
   } while( 1 ) ;  /* end of loop over arguments */
   va_end( vararg_ptr ) ;
   if( errnum > 0 ) return errnum ;

   /**** carry out edits that were flagged above ****/

   /**---------- Need to reset the disk filename? ----------**/

   if( new_prefix || new_directory_name || new_view_type )
      THD_init_diskptr_names( dset->dblk->diskptr ,
                              directory_name , NULL ,
                              prefix , view_type , True ) ;

   /**----------- Need to reconfigure the spatial axes? -----------**/
   /**    Most of this code is from routine THD_3dim_from_block    **/

   redo_daxes = ( new_nxyz || new_xyzorg || new_xyzdel || new_xyzorient ) ;

   if( redo_daxes ){
      THD_dataxes * daxes = dset->daxes ;
      THD_diskptr * dkptr = dset->dblk->diskptr ;

      /** copy new stuff into the daxes structure **/

      if( new_nxyz ){
         daxes->nxx  = dkptr->dimsizes[0] = nxyz.ijk[0] ;
         daxes->nyy  = dkptr->dimsizes[1] = nxyz.ijk[1] ;
         daxes->nzz  = dkptr->dimsizes[2] = nxyz.ijk[2] ;
      }

      if( new_xyzorg ){
         daxes->xxorg = xyzorg.xyz[0] ;
         daxes->yyorg = xyzorg.xyz[1] ;
         daxes->zzorg = xyzorg.xyz[2] ;
      }

      if( new_xyzdel ){
         daxes->xxdel = xyzdel.xyz[0] ;
         daxes->yydel = xyzdel.xyz[1] ;
         daxes->zzdel = xyzdel.xyz[2] ;
      }

      if( new_xyzorient ){
         daxes->xxorient = xyzorient.ijk[0] ;
         daxes->yyorient = xyzorient.ijk[1] ;
         daxes->zzorient = xyzorient.ijk[2] ;
      }

      /*---------------------------------------*/
      /*-- set bounding box for this dataset --*/
      /*---------------------------------------*/

      daxes->xxmin = daxes->xxorg ;
      daxes->xxmax = daxes->xxorg + (daxes->nxx-1) * daxes->xxdel ;
      if( daxes->xxmin > daxes->xxmax ){
         float temp   = daxes->xxmin ;
         daxes->xxmin = daxes->xxmax ; daxes->xxmax = temp ;
      }

      daxes->yymin = daxes->yyorg ;
      daxes->yymax = daxes->yyorg + (daxes->nyy-1) * daxes->yydel ;
      if( daxes->yymin > daxes->yymax ){
         float temp   = daxes->yymin ;
         daxes->yymin = daxes->yymax ; daxes->yymax = temp ;
      }

      daxes->zzmin = daxes->zzorg ;
      daxes->zzmax = daxes->zzorg + (daxes->nzz-1) * daxes->zzdel ;
      if( daxes->zzmin > daxes->zzmax ){
         float temp   = daxes->zzmin ;
         daxes->zzmin = daxes->zzmax ; daxes->zzmax = temp ;
      }

#ifdef EXTEND_BBOX
      daxes->xxmin -= 0.5 * daxes->xxdel ;  /* pushes edges back by 1/2  */
      daxes->xxmax += 0.5 * daxes->xxdel ;  /* voxel dimensions (the box */
      daxes->yymin -= 0.5 * daxes->yydel ;  /* defined above is based on */
      daxes->yymax += 0.5 * daxes->yydel ;  /* voxel centers, not edges) */
      daxes->zzmin -= 0.5 * daxes->zzdel ;
      daxes->zzmax += 0.5 * daxes->zzdel ;
#endif

      /*----------------------------------------------------------------*/
      /*--  matrix that transforms to Dicom (left-posterior-superior) --*/
      /*----------------------------------------------------------------*/

      LOAD_ZERO_MAT(daxes->to_dicomm) ;

      switch( daxes->xxorient ){
         case ORI_R2L_TYPE:
         case ORI_L2R_TYPE: daxes->to_dicomm.mat[0][0] = 1.0 ; break ;
         case ORI_P2A_TYPE:
         case ORI_A2P_TYPE: daxes->to_dicomm.mat[1][0] = 1.0 ; break ;
         case ORI_I2S_TYPE:
         case ORI_S2I_TYPE: daxes->to_dicomm.mat[2][0] = 1.0 ; break ;
      }

      switch( daxes->yyorient ){
         case ORI_R2L_TYPE:
         case ORI_L2R_TYPE: daxes->to_dicomm.mat[0][1] = 1.0 ; break ;
         case ORI_P2A_TYPE:
         case ORI_A2P_TYPE: daxes->to_dicomm.mat[1][1] = 1.0 ; break ;
         case ORI_I2S_TYPE:
         case ORI_S2I_TYPE: daxes->to_dicomm.mat[2][1] = 1.0 ; break ;
      }

      switch( daxes->zzorient ){
         case ORI_R2L_TYPE:
         case ORI_L2R_TYPE: daxes->to_dicomm.mat[0][2] = 1.0 ; break ;
         case ORI_P2A_TYPE:
         case ORI_A2P_TYPE: daxes->to_dicomm.mat[1][2] = 1.0 ; break ;
         case ORI_I2S_TYPE:
         case ORI_S2I_TYPE: daxes->to_dicomm.mat[2][2] = 1.0 ; break ;
      }
   }

   /**---------- Need to reconfigure the sub-bricks? ----------**/

   if( new_datum_all && new_datum_array ){
       EDERR("datum_all and datum_array can't be used together") ;
       return errnum ;
   }

   redo_bricks = ( new_datum_all || new_datum_array ||
                   new_nvals     || new_nxyz          ) ;

   if( redo_bricks && THD_count_databricks(dset->dblk) > 0 ){
      EDERR("cannot reconfigure bricks that already are full") ;
      return errnum ;
   }

   if( redo_bricks ){
      int old_nvals = dset->dblk->nvals ;
#if 0
fprintf(stderr,"EDIT_dset_items: about to redo_bricks\n") ;
#endif
      if( ! new_nvals ) nvals = old_nvals ;

      /** make an array of data types, if one not provided **/

      if( ! new_datum_array ){
         datum_array = (int *) XtMalloc( sizeof(int) * nvals ) ;

#if 0
fprintf(stderr,"EDIT_dset_items: about to make datum_array\n") ;
#endif
         for( ii=0 ; ii < nvals ; ii++ )
            datum_array[ii] =  (new_datum_all)  ? datum_all
                             : (ii < old_nvals) ? DSET_BRICK_TYPE(dset,ii)
                                                : MRI_short ;
      }

      if( new_nvals ){
         if( dset->dblk->nvals != nvals )
            THD_copy_datablock_auxdata( NULL , dset->dblk ) ; /* 30 Nov 1997 */

         myXtFree( dset->dblk->brick_bytes ) ;
         myXtFree( dset->dblk->brick_fac   ) ;

         dset->dblk->nvals = dset->dblk->diskptr->nvals = nvals ;
      }

      THD_init_datablock_brick( dset->dblk , nvals , datum_array ) ;

      if( ! new_datum_array ) myXtFree(datum_array) ;
   }

   /**---------- Need to add new brick_fac values? ----------**/

   if( new_brick_fac ){
      if( brick_fac != NULL ){
         for( ii=0 ; ii < dset->dblk->nvals ; ii++ )
            dset->dblk->brick_fac[ii] = brick_fac[ii] ;
      } else {
         for( ii=0 ; ii < dset->dblk->nvals ; ii++ )
            dset->dblk->brick_fac[ii] = 0.0 ;
      }
   }

   /** 30 Nov 1997: do just one brick_fac value **/

   if( new_brick_fac_one ){
      if( brick_fac_one_iv < 0 || brick_fac_one_iv >= dset->dblk->nvals ){
         EDERR("illegal index for ADN_brick_fac_one") ;
         return errnum ;
      }
      dset->dblk->brick_fac[ brick_fac_one_iv ] = brick_fac_one ;
   }

   /**--------- 30 Nov 1997: add a single brick label value --------**/

   if( new_brick_label_one ){
      if( brick_label_one_iv < 0 || brick_label_one_iv >= dset->dblk->nvals ){
         EDERR("illegal index for ADN_brick_label_one") ;
         return errnum ;
      }

      THD_store_datablock_label( dset->dblk, brick_label_one_iv, brick_label_one ) ;
   }

   /*---- add a single brick keywords value ----*/

   if( new_brick_keywords_one ){
      if( brick_keywords_one_iv < 0 || brick_keywords_one_iv >= dset->dblk->nvals ){
         EDERR("illegal index for ADN_brick_keywords_one") ;
         return errnum ;
      }

      if( new_brick_keywords_one == 1 )
         THD_store_datablock_keywords( dset->dblk, brick_keywords_one_iv,
                                                   brick_keywords_one    );
      else if( new_brick_keywords_one == 2 )
         THD_append_datablock_keywords( dset->dblk, brick_keywords_one_iv,
                                                    brick_keywords_one    );
   }

   /*---- Add a single brick stataux value.
          The input is a float array formatted like so:
            <statcode> <npar> <value> ... <value>
          where <statcode> is a FUNC_*_TYPE code
                <npar>     is the number of values to follow (may be 0);
                             normally is FUNC_need_stat_aux[<statcode>]
                <value>    is an auxiliary statistical parameter needed
                             for data of type <statcode>                ----*/

   if( new_brick_stataux_one ){
      int jv , npar , kv , iv ;

      iv = brick_stataux_one_iv ;

      if( iv < 0 || iv >= dset->dblk->nvals ){
         EDERR("illegal index for ADN_brick_stataux_one") ;
         return errnum ;
      }

      jv = brick_stataux_one[0] ;  /* statcode */

      npar = brick_stataux_one[1] ;  /* # of values present */
      if( npar < 0 ){
         EDERR("illegal npar for ADN_brick_stataux_one") ;
         return errnum ;
      }

      kv = FUNC_need_stat_aux[jv] ;  /* # of values needed */
      if( npar > kv ) npar = kv ;

      THD_store_datablock_stataux( dset->dblk ,
                                   iv , jv , npar , brick_stataux_one + 2 ) ;
   }

   /**---------- Need to reconfigure the time axis? ----------**/

   redo_taxis = ( new_ntt   || new_nsl     || new_ttorg || new_ttdel ||
                  new_ttdur || new_zorg_sl || new_dz_sl || new_toff_sl ) ;

   if( ! new_ntt ) ntt = ISVALID_TIMEAXIS(dset->taxis) ? dset->taxis->ntt : 0 ;

   if( ntt == 0 && dset->taxis != NULL ){
      myXtFree( dset->taxis->toff_sl ) ;
      myXtFree( dset->taxis ) ;
      dset->taxis = NULL ;
   }

   redo_taxis = ( redo_taxis && ntt > 0 ) ;

   if( (new_nsl && nsl > 0) && !new_toff_sl ){    /* if we have new slice count */
      EDERR("have new_nsl but not new_toff_sl") ; /* but no new slice offsets */
      return errnum ;
   }

   if( redo_taxis ){
      THD_timeaxis * taxis = dset->taxis ;

      if( taxis == NULL ){
         taxis          = dset->taxis     = myXtNew( THD_timeaxis ) ;
         taxis->type    = TIMEAXIS_TYPE ;
         taxis->toff_sl = NULL ;
         taxis->nsl     = 0 ;
         taxis->ttorg   = taxis->ttdel = taxis->ttdur = 0.0 ;
         taxis->ntt     = ntt ;
      }

      if( new_ntt     ) taxis->ntt     = ntt ;
      if( new_ttorg   ) taxis->ttorg   = ttorg ;
      if( new_ttdel   ) taxis->ttdel   = ttdel ;
      if( new_ttdur   ) taxis->ttdur   = ttdur ;
      if( new_zorg_sl ) taxis->zorg_sl = zorg_sl ;
      if( new_dz_sl   ) taxis->dz_sl   = dz_sl ;

      if( new_nsl ){
         taxis->nsl = nsl ;
         if( nsl > 0 )
            taxis->toff_sl = (float *) XtRealloc( (char *) taxis->toff_sl ,
                                                  sizeof(float) * nsl      ) ;
         else
            myXtFree(taxis->toff_sl) ;
      }

      if( new_toff_sl )
         for( ii=0 ; ii < taxis->nsl ; ii++ ) taxis->toff_sl[ii] = toff_sl[ii] ;
   }

   if( new_tunits ){
      THD_timeaxis * taxis = dset->taxis ;

      if( taxis == NULL ){
         EDERR("have new_tunits but have no time axis") ;
         return errnum ;
      }

      taxis->units_type = tunits ;
   }

   /**--------------- Need to redo dataset type codes? ------------**/
   /**  Note that changing the type codes by themselves won't fix  **/
   /**  nvals or other such stuff -- that must be done separately. **/

   if( new_type      ) dset->type      = type ;
   if( new_view_type ) dset->view_type = view_type ;

   if( new_func_type ){
      if( (ISANAT(dset) && func_type <= LAST_ANAT_TYPE) ||
          (ISFUNC(dset) && func_type <= LAST_FUNC_TYPE)   ){

         dset->func_type = func_type ;

      } else{
         EDERR("illegal new_func type combination") ; return errnum ;
      }
   }

   /****--------------- hopefully, we are done! ---------------****/

   return errnum ;
}
