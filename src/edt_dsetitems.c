/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/** Error messaging **/

#undef  EDERR
#define EDERR(str) \
 do{ ERROR_message("EDIT_dset_items[%d]: %s\n",ncall,str); errnum++; } while(0)

#undef  EDERR_int
#define EDERR_int(str,zqq) \
 do{ ERROR_message("EDIT_dset_items[%d]: %s : %d\n",ncall,str,zqq); errnum++; } while(0)


/* Remove +view.BRIK ness ZSS Feb 2012 */
#define STRING_DEVIEW_DEEXT_BRICK(fname) {   \
   if (STRING_HAS_SUFFIX((fname),"+orig.BRIK") || \
      STRING_HAS_SUFFIX((fname),"+acpc.BRIK") ||  \
      STRING_HAS_SUFFIX((fname),"+tlrc.BRIK") ) { /* ZSS Nov 2011 */   \
      (fname)[ll-10] = '\0' ;   \
   } else if (STRING_HAS_SUFFIX((fname),"+orig.BRIK.gz") ||   \
             STRING_HAS_SUFFIX((fname),"+acpc.BRIK.gz") || \
             STRING_HAS_SUFFIX((fname),"+tlrc.BRIK.gz") ) {/* ZSS Feb 2012 */   \
     (fname)[ll-13] = '\0' ; \
   }  \
}

/*-----------------------------------------------------------------------*/
/*! Edit some internals of a dataset.  Notice that it is possible to
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
  being the special code ADN_none (0).

  The return value is the number of errors detected (hopefully, 0).
-------------------------------------------------------------------------*/

int EDIT_dset_items( THD_3dim_dataset *dset , ... )
{
   va_list vararg_ptr ;
   int     flag_arg , errnum = 0 ;
   int     redo_bricks , redo_daxes , redo_taxis , ii ;
   void   *dummy ;
   int     iarg ;
   static int ncall=0 ;  /* 20 Dec 2005 */

   /**----- variables to flag and store presence of arguments -----**/

   int new_prefix        = 0; char *            prefix         = NULL ;
   int new_directory_name= 0; char *            directory_name = NULL ;
   int new_brick_fac     = 0; float *           brick_fac      = NULL ;
   int new_malloc_type   = 0; int               malloc_type    = ILLEGAL_TYPE;
   int new_datum_all     = 0; int               datum_all      = ILLEGAL_TYPE;
   int new_datum_array   = 0; int *             datum_array    = NULL ;
   int new_nvals         = 0; int               nvals          = 0 ;
   int new_nxyz          = 0; THD_ivec3         nxyz           ;
   int new_xyzdel        = 0; THD_fvec3         xyzdel         ;
   int new_xyzorg        = 0; THD_fvec3         xyzorg         ;
   int new_xyzorient     = 0; THD_ivec3         xyzorient      ;
   int new_to_dicomm     = 0; THD_mat33         to_dicomm      ;
   int new_ntt           = 0; int               ntt            = 0 ;
   int new_ttorg         = 0; float             ttorg          = 0.0 ;
   int new_ttdel         = 0; float             ttdel          = 0.0 ;
   int new_ttdur         = 0; float             ttdur          = 0.0 ;
   int new_nsl           = 0; int               nsl            = 0 ;
   int new_zorg_sl       = 0; float             zorg_sl        = 0.0 ;
   int new_dz_sl         = 0; float             dz_sl          = 0.0 ;
   int new_toff_sl       = 0; float *           toff_sl        = NULL ;
   int new_type          = 0; int               type           = ILLEGAL_TYPE;
   int new_view_type     = 0; int               view_type      = ILLEGAL_TYPE;
   int new_func_type     = 0; int               func_type      = ILLEGAL_TYPE;
   int new_label1        = 0; char *            label1         = NULL ;
   int new_label2        = 0; char *            label2         = NULL ;
   int new_self_name     = 0; char *            self_name      = NULL ;
   int new_warp_parent   = 0; THD_3dim_dataset *warp_parent    = NULL ;
   int new_anat_parent   = 0; THD_3dim_dataset *anat_parent    = NULL ;
   int new_stat_aux      = 0; float *           stat_aux       = NULL ;
   int new_warp          = 0; THD_warp *        warp           = NULL ;
   int new_tunits        = 0; int               tunits         = ILLEGAL_TYPE;
   int new_keywords      = 0; char *            keywords       = NULL ;

   /* 30 Nov 1997 */

   int new_brick_label_one = 0 ; char * brick_label_one    = NULL ;
                                 int    brick_label_one_iv = -1 ;

   int new_brick_fac_one = 0 ; float brick_fac_one    = 0.0 ;
                               int   brick_fac_one_iv = -1 ;

   int new_brick_stataux_one = 0 ; float * brick_stataux_one    = NULL ;
                                   int     brick_stataux_one_iv = -1 ;

   int new_brick_keywords_one = 0 ; char * brick_keywords_one    = NULL ;
                                    int    brick_keywords_one_iv = -1 ;

   /* 19 Dec 2005 */

   int new_ijk_to_dicom = 0; mat44 ijk_to_dicom ;

   /* 14 July 2006 */
   int cmode = COMPRESS_NOFILE;   /* check compression mode for NIFTI separately */
    
   int new_smode=STORAGE_UNDEFINED; /* ZSS Feb 2012 */


   /****---------------------- Sanity Check ----------------------****/

ENTRY("EDIT_dset_items") ;

   ncall++ ;  /* 20 Dec 2005: to keep track of number of calls for EDERR */

   if( ! ISVALID_3DIM_DATASET(dset) ){                /* bad data */
     EDERR("invalid input dataset"); RETURN(errnum);
   }

   /****----------- Scan input argument list;
                  - Load data into locals (va_arg);
                  - Check for legal values;
                  - Flag its presence (the new_ variables);
                  - Carry out simple processing that doesn't
                      depend on the presence of other arguments. ---------****/

   va_start( vararg_ptr , dset ) ;              /** Initialize arg reading **/
   iarg = 1 ;
   memset(&ijk_to_dicom, 0, sizeof(mat44)); 
   memset(&xyzorient, 0, sizeof(THD_ivec3)); 
   memset(&xyzorg, 0, sizeof(THD_fvec3)); 
   memset(&xyzdel, 0, sizeof(THD_fvec3)); 
   memset(&nxyz, 0, sizeof(THD_ivec3));
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
            EDERR(str) ; if( errnum > 9 ) RETURN(errnum) ;
            dummy = va_arg( vararg_ptr , void * ) ;  /* skip next arg */
         }
         break ;

         /** these two commands affect the disk file names **/

         case ADN_prefix:  /* processed later */
            prefix = va_arg( vararg_ptr , char * ) ;
            if( prefix != NULL ) {
               new_prefix = 1 ;
               new_smode = storage_mode_from_prefix(prefix);
               if (dset->dblk && dset->dblk->diskptr) { 
                  /* if defined by prefix, apply, else if current is not a
                     surface, clear (i.e. assume AFNI type) */
                  if (new_smode != STORAGE_UNDEFINED)
                     dset->dblk->diskptr->storage_mode = new_smode;
                  else if ( ! is_surface_storage_mode(DSET_STORAGE_MODE(dset)) )
                     dset->dblk->diskptr->storage_mode = STORAGE_BY_BRICK;
               }
            } else EDERR("illegal new prefix") ;
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

         case ADN_ijk_to_dicom:  /* processed later [19 Dec 2005] */
           ijk_to_dicom = va_arg( vararg_ptr , mat44 ) ;
           if( ISVALID_MAT44(ijk_to_dicom) ) new_ijk_to_dicom = 1 ;
           else EDERR("illegal new ijk_to_dicom") ;
         break ;

         case ADN_nxyz:  /* processed later */
           nxyz = va_arg( vararg_ptr , THD_ivec3 ) ;
           if( nxyz.ijk[0] >= 1 && nxyz.ijk[1] >= 1 && nxyz.ijk[2] >= 1 )
             new_nxyz = 1 ;
           else
             EDERR("illegal new nxyz") ;
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
            if( view_type >= FIRST_VIEW_TYPE && view_type <= LAST_VIEW_TYPE ) {
               new_view_type = 1 ;
               if (dset->dblk && dset->dblk->diskptr) { /* ZSS Feb 2012 */
                  dset->dblk->diskptr->storage_mode = STORAGE_BY_BRICK;
               }
            } else EDERR("illegal new view_type") ;
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
               dset->warp_parent_idcode = warp_parent->idcode ;
            }
            else EDERR("illegal new warp_parent") ;
         break ;

         case ADN_warp:  /* processed now */
            warp = va_arg( vararg_ptr , THD_warp * ) ;
            if( ISVALID_WARP(warp) ){
               new_warp = 1 ;
               if( dset->warp == NULL ) dset->warp = myRwcNew(THD_warp) ;
               *(dset->warp) =* warp ;
            } else EDERR("illegal new warp") ;
         break ;

         case ADN_anat_parent:  /* processed now */
            anat_parent = va_arg( vararg_ptr , THD_3dim_dataset * ) ;
            if( ISVALID_3DIM_DATASET(anat_parent) ){
               new_anat_parent = 1 ;
               dset->anat_parent = anat_parent ;
               MCW_strncpy(dset->anat_parent_name,anat_parent->self_name,THD_MAX_NAME) ;
               dset->anat_parent_idcode = anat_parent->idcode ;
            }
            else EDERR("illegal new anat_parent") ;
         break ;

         case ADN_anatpar_idcode:{ /* processed now [13 Dec 1999] */
            MCW_idcode * idc ;
            idc = va_arg( vararg_ptr , MCW_idcode * ) ;
            if( idc != NULL )
               dset->anat_parent_idcode = *idc ;
            else
               EDERR("illegal new anatpar_idcode") ;
         }
         break ;

      }  /*- end of switch on flag_arg -*/

      iarg++ ;
   } while( 1 ) ;  /* end of loop over arguments */
   va_end( vararg_ptr ) ;
   if( errnum > 0 ) RETURN(errnum) ;

   /**** carry out edits that were flagged above ****/

   /**---------- Need to reset the disk filename? ------------**/
   /** 22 Nov 2002: remove +orig etc. from prefix, if present **/

   if( new_prefix || new_directory_name || new_view_type ){
      char *nprefix = THD_deplus_prefix( prefix ) ;
      int   smode ;

      THD_init_diskptr_names( dset->dblk->diskptr ,
                              directory_name , NULL ,
                              nprefix , view_type , True ) ;

      /* if the storage mode has been specified via the prefix, apply it */
      /*                                             21 Aug 2008 [rickr] */
      smode = storage_mode_from_filename(prefix);
      if( smode != STORAGE_UNDEFINED )
         dset->dblk->diskptr->storage_mode = smode;

      if( DSET_IS_1D(dset) || DSET_IS_3D(dset) ){         /* 21 Mar 2003 */
        char *fname = dset->dblk->diskptr->brick_name ;
        int  ll = strlen(fname) ;
        STRING_DEVIEW_DEEXT_BRICK(fname);
        if( DSET_IS_1D(dset) || (DSET_NY(dset)==1 && DSET_NZ(dset)==1) ) {
          if( !STRING_HAS_SUFFIX(fname,".1D") &&
              !STRING_HAS_SUFFIX(fname,".1D.dset")) strcat(fname,".1D");
          fname = dset->dblk->diskptr->prefix ; /* also adjust prefix */
          if( !STRING_HAS_SUFFIX(fname,".1D") &&
              !STRING_HAS_SUFFIX(fname,".1D.dset")) strcat(fname,".1D");
        } else {
          strcat(fname,".3D");
        }
      }

      if( DSET_IS_NIML(dset) ){         /* 07 Jun 2006 [rickr] */
        char *fname = dset->dblk->diskptr->brick_name ;
        int  ll = strlen(fname) ;
        STRING_DEVIEW_DEEXT_BRICK(fname);
        if( !STRING_HAS_SUFFIX(fname,".niml") ) strcat(fname,".niml");
        fname = dset->dblk->diskptr->prefix ; /* also adjust prefix */
        if( !STRING_HAS_SUFFIX(fname,".niml") ) strcat(fname,".niml");
      }

      if( DSET_IS_NI_SURF_DSET(dset) ){ /* 28 Jun 2006 [rickr] */
        char *fname = dset->dblk->diskptr->brick_name ;
        int  ll = strlen(fname) ;
        STRING_DEVIEW_DEEXT_BRICK(fname);
        fname = dset->dblk->diskptr->brick_name ;
        if( !STRING_HAS_SUFFIX(fname,".niml.dset") ) strcat(fname,".niml.dset");
        fname = dset->dblk->diskptr->prefix ; /* also adjust prefix */
        if( !STRING_HAS_SUFFIX(fname,".niml.dset") ) strcat(fname,".niml.dset");
      }

      if( DSET_IS_GIFTI(dset) ){ /* 13 Feb 2008 [rickr] */
        char *fname = dset->dblk->diskptr->brick_name ;
        int  ll = strlen(fname) ;
        STRING_DEVIEW_DEEXT_BRICK(fname);
        if( ! STRING_HAS_SUFFIX(fname,".gii") &&
            ! STRING_HAS_SUFFIX(fname,".gii.dset") )
           strcat(fname,".gii");
        fname = dset->dblk->diskptr->prefix ; /* also adjust prefix */
        if( ! STRING_HAS_SUFFIX(fname,".gii") &&
            ! STRING_HAS_SUFFIX(fname,".gii.dset") )
           strcat(fname,".gii");
      }

      /** output of NIfTI-1.1 dataset: 06 May 2005 **/
      /* if the prefix ends in .nii or .nii.gz, change filename in brick_name */
      if( nprefix != NULL && ( STRING_HAS_SUFFIX(nprefix,".nii") ||
                               STRING_HAS_SUFFIX(nprefix,".nii.gz") ) ){
        char *fname = dset->dblk->diskptr->brick_name ;
        int  ll = strlen(fname) ;
        STRING_DEVIEW_DEEXT_BRICK(fname);
        if( STRING_HAS_SUFFIX(nprefix,".nii") ) {  /* 22 Jun 2006 mod drg */

          cmode = THD_get_write_compression() ; 
                           /* check env. variable for compression*/
          if(cmode==0) { /* have to compress this NIFTI data, 
                            add .gz to prefix */
             sprintf(DSET_PREFIX(dset),"%s.gz",nprefix); 
                           /* add .gz on to prefix initialized in */
                           /* THD_init_diskptr_names just above */
             if(STRING_HAS_SUFFIX(fname,".nii")) {
                                       /* if filename ends with .nii */
               strcat(fname,".gz") ;   /* add the .gz extension */
             } else {
               if(!(STRING_HAS_SUFFIX(fname,".nii.gz"))) { 
                                       /* compressed NIFTI extension*/
                 strcat(fname,".nii.gz") ;
               }
             }
          } else {
             if(!(STRING_HAS_SUFFIX(fname,".nii"))) { 
                                        /* if filename doesn't end with .nii */
               strcat(fname,".nii") ;   /* add the .nii extension */
             }
          }
        } else {
           if(!(STRING_HAS_SUFFIX(fname,".nii.gz"))) {
                                       /* compressed NIFTI extension*/
             strcat(fname,".nii.gz") ;
           }
        }
        if (dset->dblk->diskptr->header_name) { /* ZSS: April 26 2012 */
            /* header_name should be just like brick_name */
            strcpy(dset->dblk->diskptr->header_name, fname);
        }
      }

      /** NIfTI-1.1 with .hdr suffix (2 file format) [08 May 2008] **/

      if( nprefix != NULL && STRING_HAS_SUFFIX(nprefix,".hdr") ){

        char *fname = dset->dblk->diskptr->brick_name ;
        int  ll = strlen(fname) ;
        STRING_DEVIEW_DEEXT_BRICK(fname);
        if( !strcmp(fname+ll-14,".hdr") ) fname[ll-14] = '\0';
        if (dset->dblk->diskptr->header_name) {/* and adjust header name 
                                                  ZSS April 26 2012 */
            strcpy(dset->dblk->diskptr->header_name, fname);
            strcat(dset->dblk->diskptr->header_name,".hdr") ;
        }
        if( !STRING_HAS_SUFFIX(fname,".img") ) strcat(fname,".img") ;
         
        /* and override the BRICK mode */
        dset->dblk->diskptr->storage_mode = STORAGE_BY_NIFTI;
      }
      
      if( nprefix != NULL ) free(nprefix) ;
   }

   /**----------- Need to reconfigure the spatial axes? -----------**/
   /**    Much of this code is from routine THD_3dim_from_block    **/

   redo_daxes = ( new_xyzorg || new_xyzdel || new_xyzorient ) ;

   redo_bricks = ( new_datum_all || new_datum_array ||
                   new_nvals     || new_nxyz          ) ;

   if( new_nvals && nvals == 1 ){  /* 02 Dec 2009 */
     new_ntt = 1 ; ntt = 0 ;
   }

   /*----- ye newe waye for ye axes change [19 Dec 2005] -----*/

   /* check for conflicts */

   if( new_xyzorg && new_ijk_to_dicom )
     EDERR("can't set ijk_to_dicom and xyzorg at same time");
   if( new_xyzdel && new_ijk_to_dicom )
     EDERR("can't set ijk_to_dicom and xyzdel at same time");
   if( new_xyzorient && new_ijk_to_dicom )
     EDERR("can't set ijk_to_dicom and xyzorient at same time");

   if( redo_bricks && THD_count_databricks(dset->dblk) > 0 )
     EDERR("cannot reconfigure bricks that already are full") ;

   if( errnum > 0 ) RETURN(errnum) ;

   /* set new data grid size now */

   if( new_nxyz ){
     THD_dataxes *daxes = dset->daxes ;
     THD_diskptr *dkptr = dset->dblk->diskptr ;

     daxes->nxx  = dkptr->dimsizes[0] = nxyz.ijk[0] ;
     daxes->nyy  = dkptr->dimsizes[1] = nxyz.ijk[1] ;
     daxes->nzz  = dkptr->dimsizes[2] = nxyz.ijk[2] ;

     if( !redo_daxes && !new_ijk_to_dicom ){
       THD_set_daxes_bbox(daxes) ;
       THD_daxes_to_mat44(daxes) ;
       dset->daxes->ijk_to_dicom_real = dset->daxes->ijk_to_dicom ;
       THD_set_dicom_box (daxes) ;
     }
   }

   /* set the new matrix transform between index and space */

   if( new_ijk_to_dicom ){
     THD_dataxes *daxes = dset->daxes ;

     daxes->ijk_to_dicom = ijk_to_dicom ;
     daxes->dicom_to_ijk = nifti_mat44_inverse( ijk_to_dicom ) ;
     THD_set_dicom_box( daxes ) ;
     (void)THD_daxes_from_mat44( daxes ) ;  /* set the old stuff */
   }

   /*------ ye olde waye for ye axes change -----*/

   if( redo_daxes ){
     THD_dataxes *daxes = dset->daxes ;
#if 0
INFO_message("old geometry string = %s",EDIT_get_geometry_string(dset)) ;
#endif

     /** copy new stuff into the daxes structure **/

     if( new_xyzorg ){
#if 0
INFO_message("old daxes org = %g %g %g",daxes->xxorg,daxes->yyorg,daxes->zzorg) ;
#endif
       daxes->xxorg = xyzorg.xyz[0] ;
       daxes->yyorg = xyzorg.xyz[1] ;
       daxes->zzorg = xyzorg.xyz[2] ;
#if 0
INFO_message("new daxes org = %g %g %g",daxes->xxorg,daxes->yyorg,daxes->zzorg) ;
#endif
     }

     if( new_xyzdel ){
#if 0
INFO_message("old daxes del = %g %g %g",daxes->xxdel,daxes->yydel,daxes->zzdel) ;
#endif
       daxes->xxdel = xyzdel.xyz[0] ;
       daxes->yydel = xyzdel.xyz[1] ;
       daxes->zzdel = xyzdel.xyz[2] ;
#if 0
INFO_message("new daxes del = %g %g %g",daxes->xxdel,daxes->yydel,daxes->zzdel) ;
#endif
     }

     if( new_xyzorient ){
#if 0
INFO_message("old daxes orient = %d %d %d",daxes->xxorient,daxes->yyorient,daxes->zzorient) ;
#endif
       daxes->xxorient = xyzorient.ijk[0] ;
       daxes->yyorient = xyzorient.ijk[1] ;
       daxes->zzorient = xyzorient.ijk[2] ;
#if 0
INFO_message("new daxes orient = %d %d %d",daxes->xxorient,daxes->yyorient,daxes->zzorient) ;
#endif
     }

     /*-- set bounding box and to_dicomm matrix for this dataset --*/

     THD_set_daxes_bbox     ( daxes ) ; /* 20 Dec 2005 */
     THD_daxes_to_mat44     ( daxes ) ; /* 19 Dec 2005 */
     dset->daxes->ijk_to_dicom_real = dset->daxes->ijk_to_dicom ;
     THD_set_dicom_box      ( daxes ) ;
     THD_set_daxes_to_dicomm( daxes ) ; /* 20 Dec 2005 */

#if 0
INFO_message("new geometry string = %s",EDIT_get_geometry_string(dset)) ;
#endif
   }

   /**---------- Need to reconfigure the sub-bricks? ----------**/

   if( new_datum_all && new_datum_array ){
     EDERR("datum_all and datum_array can't be used together") ;
     RETURN(errnum) ;
   }

   if( redo_bricks ){
      int old_nvals = dset->dblk->nvals ;
#if 0
fprintf(stderr,"EDIT_dset_items: about to redo_bricks\n") ;
#endif
      if( ! new_nvals ) nvals = old_nvals ;

      /** make an array of data types, if one not provided **/

      if( ! new_datum_array ){
         datum_array = (int *) RwcMalloc( sizeof(int) * nvals ) ;

#if 0
fprintf(stderr,"EDIT_dset_items: about to make datum_array\n") ;
#endif
         for( ii=0 ; ii < nvals ; ii++ )
            datum_array[ii] =  (new_datum_all)  ? datum_all
                             : (ii < old_nvals) ? DSET_BRICK_TYPE(dset,ii)
                                                : DSET_BRICK_TYPE(dset,0) ;
      }                                           /* 06 Apr 2005 [rickr] */

      if( new_nvals ){
         if( dset->dblk->nvals != nvals )
            THD_copy_datablock_auxdata( NULL , dset->dblk ) ; /* 30 Nov 1997 */

         myRwcFree( dset->dblk->brick_bytes ) ;
         myRwcFree( dset->dblk->brick_fac   ) ;

         dset->dblk->nvals = dset->dblk->diskptr->nvals = nvals ;
      }

      THD_init_datablock_brick( dset->dblk , nvals , datum_array ) ;

      if( ! new_datum_array ) myRwcFree(datum_array) ;
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
         EDERR_int("illegal index for ADN_brick_fac_one",brick_fac_one_iv) ;
         RETURN(errnum) ;
      }
      dset->dblk->brick_fac[ brick_fac_one_iv ] = brick_fac_one ;
   }

   /**--------- 30 Nov 1997: add a single brick label value --------**/

   if( new_brick_label_one ){
      if( brick_label_one_iv < 0 || brick_label_one_iv >= dset->dblk->nvals ){
         EDERR_int("illegal index for ADN_brick_label_one",brick_label_one_iv) ;
         RETURN(errnum) ;
      }

      THD_store_datablock_label( dset->dblk, brick_label_one_iv, brick_label_one ) ;
   }

   /*---- add a single brick keywords value ----*/

   if( new_brick_keywords_one ){
      if( brick_keywords_one_iv < 0 || brick_keywords_one_iv >= dset->dblk->nvals ){
         EDERR_int("illegal index for ADN_brick_keywords_one",brick_keywords_one_iv) ;
         RETURN(errnum) ;
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

#if 0
fprintf(stderr,"stataux_one:  iv=%d bso[0]=%g bso[1]=%g bso[2]=%g\n",
        iv, brick_stataux_one[0],  brick_stataux_one[1], brick_stataux_one[2] ) ;
#endif

      if( iv < 0 || iv >= dset->dblk->nvals ){
         EDERR("illegal index for ADN_brick_stataux_one") ;
         RETURN(errnum) ;
      }

      jv = brick_stataux_one[0] ;  /* statcode */

      npar = brick_stataux_one[1] ;  /* # of values present */
      if( npar < 0 ){
         EDERR("illegal npar for ADN_brick_stataux_one") ;
         RETURN(errnum) ;
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
      myRwcFree( dset->taxis->toff_sl ) ;
      myRwcFree( dset->taxis ) ;
      dset->taxis = NULL ;
   }

   redo_taxis = ( redo_taxis && ntt > 0 ) ;

   if( (new_nsl && nsl > 0) && !new_toff_sl ){    /* if we have new slice count */
      EDERR("have new_nsl but not new_toff_sl") ; /* but no new slice offsets */
      RETURN(errnum) ;
   }

   if( redo_taxis ){
      THD_timeaxis *taxis = dset->taxis ;

      if( taxis == NULL ){
         taxis          = dset->taxis     = myRwcNew( THD_timeaxis ) ;
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
            taxis->toff_sl = (float *) RwcRealloc( (char *) taxis->toff_sl ,
                                                  sizeof(float) * nsl      ) ;
         else
            myRwcFree(taxis->toff_sl) ;
      }

      if( new_toff_sl )
         for( ii=0 ; ii < taxis->nsl ; ii++ ) taxis->toff_sl[ii] = toff_sl[ii] ;
   }

   if( new_tunits ){
      THD_timeaxis * taxis = dset->taxis ;

      if( taxis == NULL) {
         if (DSET_NVALS(dset) > 1) {
            EDERR("have new_tunits but have no time axis") ;
            RETURN(errnum) ;
         }
      } else {
         taxis->units_type = tunits ;
      }
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
         EDERR("illegal new_func type combination") ; RETURN(errnum) ;
      }
   }

   /****--------------- hopefully, we are done! ---------------****/

   RETURN(errnum) ;
}

/*!
   Scale dataset's dxyz, same as 3drefit's -xyzscale
*/
int THD_volDXYZscale( THD_dataxes  * daxes, float xyzscale, int reuse_shift)
{   
   static char FuncName[]={"THD_volDXYZscale"};
   float dxp, dyp, dzp;
   int   rl, ap , is;
   float xop , yop , zop ;
   static float shift[3] ;

   ENTRY("THD_volDXYZscale");

   if (!daxes) RETURN(0);

   if( xyzscale > 0.0f ){  /* 18 Jul 2006 */
     dxp = daxes->xxdel * xyzscale ;  /* new grid */
     dyp = daxes->yydel * xyzscale ;  /* spacings */
     dzp = daxes->zzdel * xyzscale ;
     rl  = abs(THD_get_axis_direction(daxes,ORI_R2L_TYPE)) ;
     ap  = abs(THD_get_axis_direction(daxes,ORI_A2P_TYPE)) ;
     is  = abs(THD_get_axis_direction(daxes,ORI_I2S_TYPE)) ;

     if( rl == 0 || ap == 0 || is == 0 )
       ERROR_exit("-xyzscale: Indeterminate axis directions!") ;

     if( !reuse_shift ){  /* to calculate shift */
       float op[3] , oo[3] ;
       op[0] = xop = daxes->xxorg + 
                     (daxes->xxdel-dxp)*0.5f*(daxes->nxx-1) ;
       op[1] = yop = daxes->yyorg + 
                     (daxes->yydel-dyp)*0.5f*(daxes->nyy-1) ;
       op[2] = zop = daxes->zzorg + 
                     (daxes->zzdel-dzp)*0.5f*(daxes->nzz-1) ;
       oo[0] = daxes->xxorg ;
       oo[1] = daxes->yyorg ;
       oo[2] = daxes->zzorg ;
       shift[0] = op[rl-1] - xyzscale * oo[rl-1] ;   /* RL shift */
       shift[1] = op[ap-1] - xyzscale * oo[ap-1] ;   /* AP shift */
       shift[2] = op[is-1] - xyzscale * oo[is-1] ;   /* IS shift */

     } else {           /* to apply pre calculated shift */

       xop = xyzscale * daxes->xxorg + shift[daxes->xxorient/2] ;
       yop = xyzscale * daxes->yyorg + shift[daxes->yyorient/2] ;
       zop = xyzscale * daxes->zzorg + shift[daxes->zzorient/2] ;
     }

     daxes->xxdel = dxp ; daxes->yydel = dyp ; daxes->zzdel = dzp ;
     daxes->xxorg = xop ; daxes->yyorg = yop ; daxes->zzorg = zop ;
   }
   RETURN(1);
}

/*-------------------------------------------------------------------*/
#undef DPOLD  /* 22 Sep 2014 == Bilbo's birthday */

/*-------------------------------------------------------------------*/
/*! Remove any +???? suffix from a prefix, returning a new one.
    -- 22 Nov 2002 - RWCox
---------------------------------------------------------------------*/

char * THD_deplus_prefix( char *prefix )
{
#ifdef DPOLD
   static char *plussers[] = {
      "+orig", "+orig.", "+orig.HEAD", "+orig.BRIK", "+orig.BRIK.gz",
      "+acpc", "+acpc.", "+acpc.HEAD", "+acpc.BRIK", "+acpc.BRIK.gz",
      "+tlrc", "+tlrc.", "+tlrc.HEAD", "+tlrc.BRIK", "+tlrc.BRIK.gz"
      };
#else
   static char *plussers[] = { "+orig" , "+acpc" , "+tlrc" } ;
#endif
   char *newprefix , *cpt ;
   int nn, N_nn;

   if( prefix == NULL ) return NULL ;

   newprefix = strdup(prefix);
   
   N_nn = sizeof(plussers)/sizeof(char *);
   for (nn=0; nn<N_nn; ++nn) {
#ifdef DPOLD
      if( STRING_HAS_SUFFIX(newprefix, plussers[nn]) ) {
         newprefix[strlen(newprefix)-strlen(plussers[nn])] = '\0';
         return newprefix ;
      }
#else
      cpt = strstr(newprefix,plussers[nn]) ;
      if( cpt != NULL ) *cpt = '\0' ;
#endif
   }
   
   return newprefix ;
}
