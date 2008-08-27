/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "thd.h"

/*---------------------------------------------------------------------
   Given a directory name, read in all the datasets and make
   a session data structure from them.

   [28 Jul 2003] Modified to use new THD_session struct, wherein all
                 datasets are in one array (no anat/func distinction).
-----------------------------------------------------------------------*/

THD_session * THD_init_session( char * sessname )
{
   THD_session            *sess ;
   XtPointer_array        *dblk_arrarr ;
   THD_datablock_array    *dblk_arr ;
   THD_3dim_dataset       *dset=NULL ;
   THD_3dim_dataset_array *dset_arr ;

   int ibar , idset , iview  , nds ;

ENTRY("THD_init_session") ;

   /*-- sanity check --*/

   if( sessname == NULL || strlen(sessname) == 0 || !THD_is_directory(sessname) )
     RETURN( NULL ) ;

   /*-- initialize session --*/

   sess         = myXtNew( THD_session ) ;
   sess->type   = SESSION_TYPE ;
   sess->parent = NULL ;
   BLANK_SESSION(sess) ;  /* null out all entries */

   /* save directory name, with a trailing slash */

   MCW_strncpy( sess->sessname , sessname , THD_MAX_NAME ) ;
   iview = strlen(sess->sessname) ;
   if( sess->sessname[iview-1] != '/' ){  /* tack trailing / onto sessname */
     sess->sessname[iview]   = '/' ;
     sess->sessname[iview+1] = '\0' ;
   } else {
     iview-- ;  /* iview now points to last non-NUL character in string */
   }

   /* save last name from sessname */
#if 1
   { char * env = my_getenv( "AFNI_SESSTRAIL" ) ; int tt = 0 ;
     if( env != NULL ) tt = strtol(env,NULL,10) ;
     env = THD_trailname(sess->sessname,tt) ;
     tt = 1+strlen(env) - THD_MAX_NAME ; if( tt < 0 ) tt = 0 ;
     strcpy( sess->lastname , env+tt ) ;
   }
#else
     for( iview-- ; iview >= 0 ; iview-- )
       if( sess->sessname[iview] == '/' ) break ;
     MCW_strncpy( sess->lastname, &(sess->sessname[iview+1]), THD_MAX_NAME ) ;
#endif

   /*-- read all datablocks --*/

   dblk_arrarr = THD_init_alldir_datablocks( sess->sessname ) ;

   /*-- for each datablock array ... --*/

   for( ibar=0 ; ibar < dblk_arrarr->num ; ibar++ ){

      /*-- get the current array of datablocks --*/

      dblk_arr = (THD_datablock_array *) dblk_arrarr->ar[ibar] ;
      if( dblk_arr == NULL || dblk_arr->num <= 0 ) continue ;    /* huh? */

      /*-- convert it into an array of datasets --*/

      dset_arr = THD_array_3dim_from_block( dblk_arr ) ;
      if( dset_arr == NULL || dset_arr->num <= 0 ) continue ;

      /*-- place it into the next row of the THD_session [28 Jul 2003] --*/

      nds = sess->num_dsset ;

      if( nds >= THD_MAX_SESSION_SIZE ){   /* bad! */
        fprintf(stderr,
         "\n*** Session %s table overflow with dataset %s ***\n",
             sessname , dset_arr->ar[0]->self_name) ;
        for( idset=0 ; idset < dset_arr->num ; idset++ )
          THD_delete_3dim_dataset( dset_arr->ar[idset] , False ) ;
        FREE_3DARR(dset_arr) ;
        continue ;  /* skip to next dblk_arr (ibar loop) */
      }

      /*-- put each dataset into this row in its view place --*/

      for( idset=0 ; idset < dset_arr->num ; idset++ ){
        dset  = dset_arr->ar[idset] ;
        iview = dset->view_type ;

        if( sess->dsset[nds][iview] != NULL ){  /* should never happen */
          fprintf(stderr,
           "\n*** Session %s has duplicate dataset views of %s ***\n",
           sessname , dset->self_name) ;
          THD_delete_3dim_dataset( dset , False ) ;
        } else {
          sess->dsset[nds][iview] = dset ;       /* should always happen */
        }
      }

      sess->num_dsset ++ ;  /* one more row */

      FREE_3DARR(dset_arr) ;

   } /* end of loop over each datablock array (ibar) */

   /*-- throw away the datablock arrays at this point --*/

   STATUS("trashing dblk_arrarr") ;

   for( ibar=0 ; ibar < dblk_arrarr->num ; ibar++ ){
     dblk_arr = (THD_datablock_array *) dblk_arrarr->ar[ibar] ;
     FREE_DBARR( dblk_arr ) ;
   }
   FREE_XTARR( dblk_arrarr ) ;

   /*-- 29 Oct 2001: try to read .mnc "datasets" --*/

   if( !AFNI_noenv("AFNI_MINC_DATASETS") ){
     char ename[THD_MAX_NAME] , **fn_minc , *eee ;
     int num_minc , ii ;

     STATUS("looking for MINC files") ;

     strcpy(ename,sess->sessname) ; strcat(ename,"*.mnc") ;
     eee = ename ;
     MCW_file_expand( 1,&eee , &num_minc,&fn_minc ) ;  /* find files */

     if( num_minc > 0 ){                               /* got some! */
       STATUS("opening MINC files") ;
       for( ii=0 ; ii < num_minc ; ii++ ){             /* loop over files */
         dset = THD_open_minc( fn_minc[ii] ) ;         /* try it on */
         if( !ISVALID_DSET(dset) ) continue ;          /* doesn't fit? */
         nds = sess->num_dsset ;
         if( nds >= THD_MAX_SESSION_SIZE ){
           fprintf(stderr,
             "\n*** Session %s table overflow with MINC dataset %s ***\n",
             sessname , fn_minc[ii] ) ;
           THD_delete_3dim_dataset( dset , False ) ;
           break ; /* out of for loop */
         }
         iview = dset->view_type ;
         sess->dsset[nds][iview] = dset ;
         sess->num_dsset ++ ;
       } /* end of loop over files */
       MCW_free_expand( num_minc , fn_minc ) ;
     } /* end of if we found MINC files */
   }

   /*-- 06 Apr 2005: try to read NIfTI-1 files [KRH and RWC] --*/

   if( !AFNI_noenv("AFNI_NIFTI_DATASETS") ){
     char *ename[2] , **fn_nifti ;
     int num_nifti , ii ;

     STATUS("looking for NIFTI files") ;

     ename[0] = AFMALL(char, THD_MAX_NAME) ;
     ename[1] = AFMALL(char, THD_MAX_NAME) ;
     strcpy(ename[0],sess->sessname) ; strcat(ename[0],"*.nii") ;
     strcpy(ename[1],sess->sessname) ; strcat(ename[1],"*.nii.gz") ;
     MCW_file_expand( 2,ename , &num_nifti,&fn_nifti ) ;  /* find files */
     free(ename[0]) ; free(ename[1]) ;

     if( num_nifti > 0 ){                               /* got some! */
       STATUS("opening NIFTI files") ;
       for( ii=0 ; ii < num_nifti ; ii++ ){             /* loop over files */
         dset = THD_open_nifti( fn_nifti[ii] ) ;        /* try it on */
         if( !ISVALID_DSET(dset) ) continue ;           /* doesn't fit? */
         nds = sess->num_dsset ;
         if( nds >= THD_MAX_SESSION_SIZE ){
           fprintf(stderr,
             "\n*** Session %s table overflow with NIfTI dataset %s ***\n",
             sessname , fn_nifti[ii] ) ;
           THD_delete_3dim_dataset( dset , False ) ;
           break ; /* out of for loop */
         }
         iview = dset->view_type ;
         sess->dsset[nds][iview] = dset ;
         sess->num_dsset ++ ;
       } /* end of loop over files */
       MCW_free_expand( num_nifti , fn_nifti ) ;
     } /* end of if we found NIFTI files */
   }

   /*-- 27 Aug 2002: try to read any ANALYZE "datasets" here --*/

   if( !AFNI_noenv("AFNI_ANALYZE_DATASETS") ){
     char *ename[2] , **fn_anlz ;
     int num_anlz , ii , nee ;
#ifdef ALLOW_FSL_FEAT
     int feat_exf=-1 , feat_hrs=-1 , feat_std=-1 ;
     int feat_nds_start=sess->num_dsset ;
#endif

     STATUS("looking for ANALYZE files") ;

     ename[0] = AFMALL(char, THD_MAX_NAME) ;
     strcpy(ename[0],sess->sessname) ; strcat(ename[0],"*.hdr") ;
     nee = 1 ;
#ifdef ALLOW_FSL_FEAT
     ename[1] = AFMALL(char, THD_MAX_NAME) ;
     strcpy(ename[1],sess->sessname) ; strcat(ename[1],"stats/*stat*.hdr") ;
     nee++ ;
#endif
     MCW_file_expand( nee,ename , &num_anlz,&fn_anlz ) ;  /* find files */
     for( ii=0 ; ii < nee ; ii++ ) free(ename[ii]) ;

     if( num_anlz > 0 ){                               /* got some! */
       STATUS("opening ANALYZE files") ;
       for( ii=0 ; ii < num_anlz ; ii++ ){             /* loop over files */
         dset = THD_open_analyze( fn_anlz[ii] ) ;      /* try it on */
         if( !ISVALID_DSET(dset) ) continue ;          /* doesn't fit? */
         nds = sess->num_dsset ;
         if( nds >= THD_MAX_SESSION_SIZE ){
           fprintf(stderr,
             "\n*** Session %s table overflow with ANALYZE dataset %s ***\n",
             sessname , fn_anlz[ii] ) ;
           THD_delete_3dim_dataset( dset , False ) ;
           break ; /* out of for loop */
        }
        iview = dset->view_type ;
        sess->dsset[nds][iview] = dset ;
        sess->num_dsset ++ ;
#ifdef ALLOW_FSL_FEAT
             if( strcmp(DSET_PREFIX(dset),"example_func.hdr") == 0 ) feat_exf = nds;
        else if( strcmp(DSET_PREFIX(dset),"highres.hdr")      == 0 ) feat_hrs = nds;
        else if( strcmp(DSET_PREFIX(dset),"standard.hdr")     == 0 ) feat_std = nds;
#endif
       } /* end of loop over files */

       MCW_free_expand( num_anlz , fn_anlz ) ;  /* free file list */

       /* now read in linear mappings (.mat files) for FSL FEAT */

#ifdef ALLOW_FSL_FEAT
       if( feat_exf >= 0                        &&
           sess->num_dsset - feat_nds_start > 0 &&
           (feat_hrs >= 0 || feat_std >= 0)       ){  /* found FEAT files */

         THD_3dim_dataset *dset_exf=NULL, *dset_hrs=NULL, *dset_std=NULL ;
         char fnam[THD_MAX_NAME] ;
         FILE *fp ;
         float a11,a12,a13,s1 , a21,a22,a23,s2 , a31,a32,a33,s3 ;
         THD_warp *warp_exf_hrs=NULL , *warp_exf_std=NULL , *warp_std_hrs=NULL ;

                             dset_exf = sess->dsset[feat_exf][0] ;  /* Must have this. */
         if( feat_hrs >= 0 ) dset_hrs = sess->dsset[feat_hrs][0] ;  /* And at least   */
         if( feat_std >= 0 ) dset_std = sess->dsset[feat_std][0] ;  /* one of these. */

         /* try to read the warp from example_func (EPI) to standard */

         if( dset_std != NULL ){
           strcpy(fnam,sess->sessname) ; strcat(fnam,"example_func2standard.mat") ;
           fp = fopen(fnam,"r") ;
           if( fp != NULL ){
             ii = fscanf(fp,"%f%f%f%f %f%f%f%f %f%f%f%f" ,
                         &a11,&a12,&a13,&s1 , &a21,&a22,&a23,&s2 ,
                                              &a31,&a32,&a33,&s3   ) ;
             if( ii == 12 )
               warp_exf_std = AFNI_make_affwarp_12( a11,a12,a13,s1 ,
                                                    a21,a22,a23,s2 ,
                                                    a31,a32,a33,s3   ) ;
             fclose(fp) ;
           }

           /* 28 Aug 2002:
               (i)  correct orientation of example_func
               (ii) correct warp for non-DICOM order of coords in datasets      */

           if( warp_exf_std != NULL ){
             THD_mat33 mmm,nnn ;
             int   ix , iy , iz ;
             float ax , ay , az ;

#if 0
printf("warp_exf_std BEFORE:") ; DUMP_LMAP(warp_exf_std->rig_bod.warp) ;
#endif

#undef FIX_EXF
#ifdef FIX_EXF
             /* make matrix that transforms standard to example_func */

             LOAD_MAT(mmm,a11,a12,a13,a21,a22,a23,a31,a32,a33) ;
             nnn = MAT_INV(mmm) ;
             UNLOAD_MAT(nnn,a11,a12,a13,a21,a22,a23,a31,a32,a33) ;

             /* for each for, find index and value of largest element */

             ix = 1 ; ax = a11 ;
             if( fabs(a12) > fabs(ax) ){ ix = 2 ; ax = a12 ; }
             if( fabs(a13) > fabs(ax) ){ ix = 3 ; ax = a13 ; }

             iy = 1 ; ay = a21 ;
             if( fabs(a22) > fabs(ay) ){ iy = 2 ; ay = a22 ; }
             if( fabs(a23) > fabs(ay) ){ iy = 3 ; ay = a23 ; }

             iz = 1 ; az = a31 ;
             if( fabs(a32) > fabs(az) ){ iz = 2 ; az = a32 ; }
             if( fabs(a33) > fabs(az) ){ iz = 3 ; az = a33 ; }
#else
             ix = 1 ; iy = 2 ; iz = 3 ;
#endif

             if( ix+iy+iz == 6 ){  /* (ix,iy,iz) must be a permutation of (1,2,3) */
               THD_ivec3 orixyz ;
               THD_fvec3 dxyz ;
               THD_dataxes *daxes = dset_exf->daxes ;
               THD_warp *from_exf , *to_std ;

#ifdef FIX_EXF
               /** fix orientation of dset_exf dataset **/

               switch(ix){
                 case 1:    /* example_func x-axis is mainly same
                               as standard x-axis (ax>0) or its opposite (ax<0) */
                   if( ax > 0 ) ix = dset_std->daxes->xxorient ;
                   else         ix = ORIENT_OPPOSITE(dset_std->daxes->xxorient) ;
                 break ;

                 case 2:    /* example_func x-axis is mainly same
                               as standard y-axis (ax>0) or its opposite (ax<0) */
                   if( ax > 0 ) ix = dset_std->daxes->yyorient ;
                   else         ix = ORIENT_OPPOSITE(dset_std->daxes->yyorient) ;
                 break ;

                 case 3:    /* example_func x-axis is mainly same
                               as standard z-axis (ax>0) or its opposite (ax<0) */
                   if( ax > 0 ) ix = dset_std->daxes->zzorient ;
                   else         ix = ORIENT_OPPOSITE(dset_std->daxes->zzorient) ;
                 break ;
               }
               switch(iy){
                 case 1: if( ay > 0 ) iy = dset_std->daxes->xxorient ;
                         else         iy = ORIENT_OPPOSITE(dset_std->daxes->xxorient) ;
                 break ;
                 case 2: if( ay > 0 ) iy = dset_std->daxes->yyorient ;
                         else         iy = ORIENT_OPPOSITE(dset_std->daxes->yyorient) ;
                 break ;
                 case 3: if( ay > 0 ) iy = dset_std->daxes->zzorient ;
                         else         iy = ORIENT_OPPOSITE(dset_std->daxes->zzorient) ;
                 break ;
               }
               switch(iz){
                 case 1: if( az > 0 ) iz = dset_std->daxes->xxorient ;
                         else         iz = ORIENT_OPPOSITE(dset_std->daxes->xxorient) ;
                 break ;
                 case 2: if( az > 0 ) iz = dset_std->daxes->yyorient ;
                         else         iz = ORIENT_OPPOSITE(dset_std->daxes->yyorient) ;
                 break ;
                 case 3: if( az > 0 ) iz = dset_std->daxes->zzorient ;
                         else         iz = ORIENT_OPPOSITE(dset_std->daxes->zzorient) ;
                 break ;
               }
               orixyz.ijk[0] = ix ; orixyz.ijk[1] = iy ; orixyz.ijk[2] = iz ;
               dxyz.xyz[0] = fabs(daxes->xxdel) ;
               dxyz.xyz[1] = fabs(daxes->yydel) ;
               dxyz.xyz[2] = fabs(daxes->zzdel) ;
               if( ORIENT_sign[ix] == '-' ) dxyz.xyz[0] = -dxyz.xyz[0] ;
               if( ORIENT_sign[iy] == '-' ) dxyz.xyz[1] = -dxyz.xyz[1] ;
               if( ORIENT_sign[iz] == '-' ) dxyz.xyz[2] = -dxyz.xyz[2] ;
               EDIT_dset_items( dset_exf , ADN_xyzorient,orixyz, ADN_xyzdel,dxyz, ADN_none ) ;
#endif

               /** now fix warp from example_func to standard to do it in DICOM coords **/

               nnn = SNGL_mat_to_dicomm( dset_exf ) ; mmm = TRANSPOSE_MAT(nnn) ;
               from_exf = AFNI_make_affwarp_mat( mmm ) ;
               nnn = SNGL_mat_to_dicomm( dset_std ) ;
               to_std = AFNI_make_affwarp_mat( nnn ) ;
               AFNI_concatenate_warp( warp_exf_std , from_exf ) ;
               AFNI_concatenate_warp( to_std , warp_exf_std ) ;
               myXtFree(warp_exf_std) ; myXtFree(from_exf) ;
               warp_exf_std = to_std ;

#if 0
printf("warp_exf_std AFTER:") ; DUMP_LMAP(warp_exf_std->rig_bod.warp) ;
#endif

             } /* end of if warp had reasonable axis combinations */
           } /* end of if we got a good warp */
         }

         /* try to read the warp from example_func (EPI) to highres */

         if( dset_hrs != NULL ){
           strcpy(fnam,sess->sessname) ; strcat(fnam,"example_func2highres.mat") ;
           fp = fopen(fnam,"r") ;
           if( fp != NULL ){
             ii = fscanf(fp,"%f%f%f%f %f%f%f%f %f%f%f%f" ,
                         &a11,&a12,&a13,&s1 , &a21,&a22,&a23,&s2 ,
                                              &a31,&a32,&a33,&s3   ) ;
             if( ii == 12 )
               warp_exf_hrs = AFNI_make_affwarp_12( a11,a12,a13,s1 ,
                                                    a21,a22,a23,s2 ,
                                                    a31,a32,a33,s3   ) ;
             fclose(fp) ;
           }

           /* 28 Aug 2002: correct warp for non-DICOM order of coords in datasets */

           if( warp_exf_hrs != NULL ){
             THD_mat33 mmm,nnn ;
             THD_warp *from_exf , *to_hrs ;

#if 0
printf("warp_exf_hrs BEFORE:") ; DUMP_LMAP(warp_exf_hrs->rig_bod.warp) ;
#endif

             nnn = SNGL_mat_to_dicomm( dset_exf ) ; mmm = TRANSPOSE_MAT(nnn) ;
             from_exf = AFNI_make_affwarp_mat( mmm ) ;
             nnn = SNGL_mat_to_dicomm( dset_hrs ) ;
             to_hrs = AFNI_make_affwarp_mat( nnn ) ;
             AFNI_concatenate_warp( warp_exf_hrs , from_exf ) ;
             AFNI_concatenate_warp( to_hrs , warp_exf_hrs ) ;
             myXtFree(warp_exf_hrs) ; myXtFree(from_exf) ;
             warp_exf_hrs = to_hrs ;

#if 0
printf("warp_exf_hrs AFTER:") ; DUMP_LMAP(warp_exf_hrs->rig_bod.warp) ;
#endif
           }
         }

         /* try to read the warp from standard to highres */

         if( dset_hrs != NULL && dset_std != NULL ){
           strcpy(fnam,sess->sessname) ; strcat(fnam,"standard2highres.mat") ;
           fp = fopen(fnam,"r") ;
           if( fp != NULL ){
             ii = fscanf(fp,"%f%f%f%f %f%f%f%f %f%f%f%f" ,
                         &a11,&a12,&a13,&s1 , &a21,&a22,&a23,&s2 ,
                                              &a31,&a32,&a33,&s3   ) ;
             if( ii == 12 )
               warp_std_hrs = AFNI_make_affwarp_12( a11,a12,a13,s1 ,
                                                    a21,a22,a23,s2 ,
                                                    a31,a32,a33,s3   ) ;
             fclose(fp) ;
           }

           /* 28 Aug 2002: correct warp for non-DICOM order of coords in datasets */

           if( warp_std_hrs != NULL ){
             THD_mat33 mmm,nnn ;
             THD_warp *from_std , *to_hrs ;

#if 0
printf("warp_std_hrs BEFORE:") ; DUMP_LMAP(warp_std_hrs->rig_bod.warp) ;
#endif

             nnn = SNGL_mat_to_dicomm( dset_std ) ; mmm = TRANSPOSE_MAT(nnn) ;
             from_std = AFNI_make_affwarp_mat( mmm ) ;
             nnn = SNGL_mat_to_dicomm( dset_hrs ) ;
             to_hrs = AFNI_make_affwarp_mat( nnn ) ;
             AFNI_concatenate_warp( warp_std_hrs , from_std ) ;
             AFNI_concatenate_warp( to_hrs , warp_std_hrs ) ;
             myXtFree(warp_std_hrs) ; myXtFree(from_std) ;
             warp_std_hrs = to_hrs ;

#if 0
printf("warp_std_hrs AFTER:") ; DUMP_LMAP(warp_std_hrs->rig_bod.warp) ;
#endif
           }
         }

         /* if we have these warps,
            then build a hashtable describing their use in
            transforming funcs (in exf coords) to hrs and std coords */

         if( warp_exf_hrs != NULL || warp_exf_std != NULL ){

           if( sess->warptable == NULL )
              sess->warptable = new_Htable(0) ;  /* use minimum table size */

           if( warp_exf_hrs != NULL ){
             for( ii=feat_nds_start ; ii < sess->num_dsset ; ii++ ){
               if( ISFUNC(sess->dsset[ii][0]) ){
                 sprintf(fnam,"%s,%s",dset_hrs->idcode.str,sess->dsset[ii][0]->idcode.str) ;
                 addto_Htable( fnam , warp_exf_hrs , sess->warptable ) ;
               }
             }
             for( ii=feat_nds_start ; ii < sess->num_dsset ; ii++ ){
               if( ii != feat_hrs && ii != feat_std && ISANAT(sess->dsset[ii][0]) ){
                 sprintf(fnam,"%s,%s",dset_hrs->idcode.str,sess->dsset[ii][0]->idcode.str) ;
                 addto_Htable( fnam , warp_exf_hrs , sess->warptable ) ;
               }
             }
           }

           if( warp_exf_std != NULL ){
             for( ii=feat_nds_start ; ii < sess->num_dsset ; ii++ ){
               if( ISFUNC(sess->dsset[ii][0]) ){
                 sprintf(fnam,"%s,%s",dset_std->idcode.str,sess->dsset[ii][0]->idcode.str) ;
                 addto_Htable( fnam , warp_exf_std , sess->warptable ) ;
               }
             }
             for( ii=feat_nds_start ; ii < sess->num_dsset ; ii++ ){
               if( ii != feat_hrs && ii != feat_std && ISANAT(sess->dsset[ii][0]) ){
                 sprintf(fnam,"%s,%s",dset_std->idcode.str,sess->dsset[ii][0]->idcode.str) ;
                 addto_Htable( fnam , warp_exf_std , sess->warptable ) ;
               }
             }
           }

           if( warp_std_hrs != NULL ){
             sprintf(fnam,"%s,%s",dset_hrs->idcode.str,dset_std->idcode.str) ;
             addto_Htable( fnam , warp_std_hrs , sess->warptable ) ;
           }

         } /* end of making warptable */
       } /* end of FEATing */
#endif

     } /* end of if we found ANALYZE files */
   }

   /*-- 04 Dec 2002: try to read CTF .mri and .svl "datasets" --*/

   if( !AFNI_noenv("AFNI_CTF_DATASETS") ){
     char *ename[2] , **fn_ctf ;
     int num_ctf , ii ;

     STATUS("looking for CTF files") ;

     ename[0] = AFMALL(char, THD_MAX_NAME) ;
     ename[1] = AFMALL(char, THD_MAX_NAME) ;
     strcpy(ename[0],sess->sessname) ; strcat(ename[0],"*.mri") ;
     strcpy(ename[1],sess->sessname) ; strcat(ename[1],"*.svl") ;
     MCW_file_expand( 2,ename , &num_ctf,&fn_ctf ) ;  /* find files */
     free(ename[0]) ; free(ename[1]) ;

     if( num_ctf > 0 ){                               /* got some files! */
       STATUS("opening CTF files") ;
       for( ii=0 ; ii < num_ctf ; ii++ ){             /* loop over files */

         if( strstr(fn_ctf[ii],".mri") != NULL )      /* try to read: */
           dset = THD_open_ctfmri( fn_ctf[ii] ) ;     /*   as MRI */
         else if( strstr(fn_ctf[ii],".svl") != NULL )
           dset = THD_open_ctfsam( fn_ctf[ii] ) ;     /*   as SAM */

         if( !ISVALID_DSET(dset) ) continue ;         /* doesn't read? */
         nds = sess->num_dsset ;
         if( nds >= THD_MAX_SESSION_SIZE ){
           fprintf(stderr,
            "\n*** Session %s table overflow with dataset %s ***\n",
            sessname , fn_ctf[ii] ) ;
           THD_delete_3dim_dataset( dset , False ) ;
           break ; /* out of for loop */
         }
         iview = dset->view_type ;
         sess->dsset[nds][iview] = dset ;
         sess->num_dsset ++ ;
       } /* end of loop over files */
       MCW_free_expand( num_ctf , fn_ctf ) ;
     } /* end of if we found CTF files */
   }

   /*-- 03 Dec 2001: try to read MPEG "datasets" --*/

   if( !AFNI_noenv("AFNI_MPEG_DATASETS") ){
     char ename[4*THD_MAX_NAME+64] , **fn_mpeg ;
     int num_mpeg , ii ;

     STATUS("looking for MPEG files") ;

     sprintf(ename,"%s*.mpg %s*.mpeg %s*.MPEG %s*.MPG" , 
             sess->sessname, sess->sessname, sess->sessname, sess->sessname ) ;
     MCW_wildcards( ename , &num_mpeg , &fn_mpeg ) ;   /* find files */

     if( num_mpeg > 0 ){                               /* got some! */
       STATUS("opening MPEG files") ;
       for( ii=0 ; ii < num_mpeg ; ii++ ){             /* loop over files */
         dset = THD_open_mpeg( fn_mpeg[ii] ) ;         /* try it on */
         if( !ISVALID_DSET(dset) ) continue ;          /* doesn't fit? */
         nds = sess->num_dsset ;
         if( nds >= THD_MAX_SESSION_SIZE ){
           fprintf(stderr,
             "\n*** Session %s table overflow with MPEG dataset %s ***\n",
             sessname , fn_mpeg[ii] ) ;
           THD_delete_3dim_dataset( dset , False ) ;
           break ; /* out of for loop */
         }
         iview = dset->view_type ;
         sess->dsset[nds][iview] = dset ;
         sess->num_dsset ++ ;
       } /* end of loop over files */
       MCW_free_expand( num_mpeg , fn_mpeg ) ;
     } /* end of if we found MPEG files */
   }

   /*-- done! --*/

   if( sess->num_dsset == 0 ){
      myXtFree( sess ) ; RETURN( NULL ) ;
   }

   /*-- 29 Jul 2003: order dataset rows --*/

   THD_order_session( sess ) ;

   RETURN( sess ) ;
}

/*-------------------------------------------------------------------------*/
/*! Order the datasets within a session (anats first, funcs last).
---------------------------------------------------------------------------*/

void THD_order_session( THD_session *sess )
{
   THD_3dim_dataset *qset[THD_MAX_SESSION_SIZE][LAST_VIEW_TYPE+1] ;
   THD_3dim_dataset *dset ;
   int iview , ids , nds ;

ENTRY("THD_order_session") ;
   if( sess == NULL || sess->num_dsset <= 1 ) EXRETURN ;

   /* put anats into qset */

   nds = 0 ;
   for( ids=0 ; ids < sess->num_dsset ; ids++ ){
     for( iview=0 ; iview <= LAST_VIEW_TYPE ; iview++ ){
       dset = sess->dsset[ids][iview] ;
       if( dset != NULL && ISANAT(dset) ) break ;
     }
     if( iview <= LAST_VIEW_TYPE ){
       for( iview=0 ; iview <= LAST_VIEW_TYPE ; iview++ )
         qset[nds][iview] = sess->dsset[ids][iview] ;
       nds++ ;
     }
   }

   /* put funcs into qset */

   for( ids=0 ; ids < sess->num_dsset ; ids++ ){
     for( iview=0 ; iview <= LAST_VIEW_TYPE ; iview++ ){
       dset = sess->dsset[ids][iview] ;
       if( dset != NULL && ISFUNC(dset) ) break ;
     }
     if( iview <= LAST_VIEW_TYPE ){
       for( iview=0 ; iview <= LAST_VIEW_TYPE ; iview++ )
         qset[nds][iview] = sess->dsset[ids][iview] ;
       nds++ ;
     }
   }

   /* copy qset back into sess, overwriting dsset */

   for( ids=0 ; ids < nds ; ids++ )
     for( iview=0 ; iview <= LAST_VIEW_TYPE ; iview++ )
       sess->dsset[ids][iview] = qset[ids][iview] ;

   sess->num_dsset = nds ;  /* shouldn't change */
   EXRETURN ;
}
