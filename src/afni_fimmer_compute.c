

/*-------------------------------------------------------------------
  Actually carry out the FIM computations:
    im3d
---------------------------------------------------------------------*/

#ifndef FIM_THR
#define FIM_THR  0.0999
#endif

/** Jan 1998:    code = combinations of the FIM_*_MASK values      **/
/** 30 May 1999: allow polort to be variable (formerly fixed at 1) **/
/** 08 Sep 1999: added PAVE and AVER options                       **/
/** 03 Jan 2000: added PTOP, TOPL, and SIGM options                **/
/** 01 Feb 2000: added ucode, for user written functions           **/

THD_3dim_dataset * AFNI_fimmer_compute( Three_D_View * im3d ,
                                        THD_3dim_dataset * dset_time ,
                                        MRI_IMAGE * ref_ts , MRI_IMAGE * ort_ts ,
                                        THD_session * sess , int code, int ucode )
{
   THD_3dim_dataset * new_dset=NULL ;
   char new_prefix[THD_MAX_PREFIX] ;
   char old_prefix[THD_MAX_PREFIX] ;
   THD_slist_find fff ;
   int ifim , it,iv , nvox , ngood_ref , ntime , it1 , dtyp , nxyz , itbot ;
   float * vval, * tsar, * aval, * rbest, * abest, * pbest, * pval, * bbest, * bval;
   int   * indx ;
   short * bar ;
   short * ibest ;  /* 15 Dec 1997 */
   void  * ptr ;
   float stataux[MAX_STAT_AUX] ;
   float fthr , topval ;
   int nx_ref , ny_ref , ivec , nnow ;
   PCOR_references ** pc_ref ;
   PCOR_voxel_corr ** pc_vc ;

   int fim_nref , nx_ort , ny_ort , internal_ort ;
   float * ortar ;
   static float * ref_vec = NULL ;
   static int    nref_vec = -666 ;

   int ibr_best , ibr_perc , ibr_fim , ibr_corr , ibr_base , nbrik ;

   int polort = im3d->fimdata->polort , ip ;  /* 30 May 1999 */

   float top_perc = 0.0 ;                     /* 30 Aug 1999 */

   int ibr_pave , ibr_aver ;                        /* 08 Sep 1999 */
   float * paval , * avval , * pabest , * avbest ;  /* 08 Sep 1999 */

   int ibr_ptop , ibr_topl , ibr_sigm ;             /* 03 Jan 2000 */
   float * ptval , * tlval , *sgval ,
         * ptbest, * tlbest, *sgbest ;

#ifndef DONT_USE_METER
   Widget meter = NULL ;
   int meter_perc , meter_pold ;
#endif

   int nupdt      = 0 ,  /* number of updates done yet */
       min_updt   = 5 ,  /* min number needed for display */
       first_updt = 1 ;  /* flag to indicate that first update is yet to be displayed */

ENTRY("AFNI_fimmer_compute") ;

   /*--- check for legal inputs ---*/

   if( ! DSET_GRAPHABLE(dset_time)    ||
       ref_ts == NULL                 ||
       ref_ts->kind != MRI_float      ||
       ! IM3D_OPEN(im3d)              ||
       im3d->type != AFNI_3DDATA_VIEW ||
       (code == 0 && ucode == 0)      ||           /* Jan 1998 & Feb 2000 */
       ref_ts->nx < DSET_NUM_TIMES(dset_time) ){

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"illegal inputs: ntime=%d num_ts=%d",
          DSET_NUM_TIMES(dset_time), (ref_ts==NULL) ? (0) : (ref_ts->nx) ) ;
  STATUS(str) ; }

      RETURN(NULL) ;
   }

   /** 13 Nov 1996: allow for orts **/

   if( ort_ts != NULL ){
      nx_ort = ort_ts->nx ;
      ny_ort = ort_ts->ny ;
      ortar  = MRI_FLOAT_PTR(ort_ts) ;

      internal_ort = (nx_ort < DSET_NUM_TIMES(dset_time)) ;
   } else {
      internal_ort = 1 ;
   }
   fim_nref = (internal_ort) ? (polort+2) : (ny_ort+polort+2) ;

   if( nref_vec < fim_nref ){
       ref_vec = (float *) XtRealloc( (char *)ref_vec, sizeof(float)*fim_nref ) ;
      nref_vec = fim_nref ;
   }

   itbot     = im3d->fimdata->init_ignore ;
   nx_ref    = ref_ts->nx ;
   ny_ref    = ref_ts->ny ;
   ntime     = DSET_NUM_TIMES(dset_time) ;
   ngood_ref = 0 ;
   it1       = -1 ;
   for( ivec=0 ; ivec < ny_ref ; ivec++ ){
      tsar = MRI_FLOAT_PTR(ref_ts) + (ivec*nx_ref) ;
      ifim = 0 ;
      for( it=itbot ; it < ntime ; it++ ){
         if( tsar[it] < WAY_BIG ){ ifim++ ; if( it1 < 0 ) it1 = it ; }
      }

      if( ifim < min_updt ){
         STATUS("ref_ts has too few good entries!") ;
         RETURN(NULL) ;
      }

      ngood_ref = MAX( ifim , ngood_ref ) ;
   }

   /** at this point, ngood_ref = max number of good reference points,
       and                  it1 = index of first point used in first reference **/

   dtyp = DSET_BRICK_TYPE(dset_time,it1) ;
   if( ! AFNI_GOOD_FUNC_DTYPE(dtyp) ){
      STATUS("illegal input data type!") ;
      RETURN(NULL) ;
   }

   /*--- Create a new prefix ---*/

   MCW_strncpy( old_prefix , DSET_PREFIX(dset_time) , THD_MAX_PREFIX-3 ) ;

   if( ! ISVALID_SESSION(sess) ){
      sprintf( new_prefix , "%s@%d" , old_prefix , 1 ) ;
   } else {
      for( ifim=1 ; ifim < 99 ; ifim++ ){
         sprintf( new_prefix , "%s@%d" , old_prefix , ifim ) ;
         fff = THD_dset_in_session( FIND_PREFIX , new_prefix , sess ) ;
         if( fff.dset == NULL ) break ;
      }
      if( ifim == 99 ){
         STATUS("can't create new prefix!") ;
         RETURN(NULL) ;  /* can't make a new prefix! */
      }
   }

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"new prefix = %s",new_prefix) ; STATUS(str) ; }

   /*--- FIM: find values above threshold to fim ---*/

   THD_load_datablock( dset_time->dblk , AFNI_purge_unused_dsets ) ;

   nxyz =  dset_time->dblk->diskptr->dimsizes[0]
         * dset_time->dblk->diskptr->dimsizes[1]
         * dset_time->dblk->diskptr->dimsizes[2] ;

   /** find the mean of the first array,
       compute the threshold (fthr) from it,
       make indx[i] be the 3D index of the i-th voxel above threshold **/

   switch( dtyp ){

      case MRI_short:{
         short * dar = (short *) DSET_ARRAY(dset_time,it1) ;
         for( iv=0,fthr=0.0 ; iv < nxyz ; iv++ ) fthr += abs(dar[iv]) ;
         fthr = FIM_THR * fthr / nxyz ;

if(PRINT_TRACING)
{ char str[256] ; sprintf(str,"fthr = %g",fthr) ; STATUS(str) ; }

         for( iv=0,nvox=0 ; iv < nxyz ; iv++ )
            if( abs(dar[iv]) > fthr ) nvox++ ;
         indx = (int *) malloc( sizeof(int) * nvox ) ;
         if( indx == NULL ){
            fprintf(stderr,"\n*** indx malloc failure in AFNI_fimmer_compute\n") ;
            RETURN(NULL) ;
         }
         for( iv=0,nvox=0 ; iv < nxyz ; iv++ )
            if( abs(dar[iv]) > fthr ) indx[nvox++] = iv ;
      }
      break ;

      case MRI_float:{
         float * dar = (float *) DSET_ARRAY(dset_time,it1) ;
         for( iv=0,fthr=0.0 ; iv < nxyz ; iv++ ) fthr += fabs(dar[iv]) ;
         fthr = FIM_THR * fthr / nxyz ;

if(PRINT_TRACING)
{ char str[256] ; sprintf(str,"fthr = %g",fthr) ; STATUS(str) ; }

         for( iv=0,nvox=0 ; iv < nxyz ; iv++ )
            if( fabs(dar[iv]) > fthr ) nvox++ ;
         indx = (int *) malloc( sizeof(int) * nvox ) ;
         if( indx == NULL ){
            fprintf(stderr,"\n*** indx malloc failure in AFNI_fimmer_compute\n") ;
            RETURN(NULL) ;
         }
         for( iv=0,nvox=0 ; iv < nxyz ; iv++ )
            if( fabs(dar[iv]) > fthr ) indx[nvox++] = iv ;
      }
      break ;

      case MRI_byte:{
         byte * dar = (byte *) DSET_ARRAY(dset_time,it1) ;
         for( iv=0,fthr=0.0 ; iv < nxyz ; iv++ ) fthr += dar[iv] ;
         fthr = FIM_THR * fthr / nxyz ;

if(PRINT_TRACING)
{ char str[256] ; sprintf(str,"fthr = %g",fthr) ; STATUS(str) ; }

         for( iv=0,nvox=0 ; iv < nxyz ; iv++ )
            if( dar[iv] > fthr ) nvox++ ;
         indx = (int *) malloc( sizeof(int) * nvox ) ;
         if( indx == NULL ){
            fprintf(stderr,"\n*** indx malloc failure in AFNI_fimmer_compute\n") ;
            RETURN(NULL) ;
         }
         for( iv=0,nvox=0 ; iv < nxyz ; iv++ )
            if( dar[iv] > fthr ) indx[nvox++] = iv ;
      }
      break ;
   }

if(PRINT_TRACING)
{ char str[256] ; sprintf(str,"number of voxels = %d",nvox) ; STATUS(str) ; }

   /** allocate space for voxel values **/

   vval = (float *) malloc( sizeof(float) * nvox) ;
   if( vval == NULL ){
      fprintf(stderr,"\n*** vval malloc failure in AFNI_fimmer_compute\n") ;
      free(indx) ; RETURN(NULL) ;
   }

   /** compute number of output bricks **/

   ibr_fim=ibr_corr=ibr_best=ibr_perc=ibr_base = -1 ; nbrik = 0 ;
   ibr_pave = ibr_aver = -1 ;
   ibr_ptop = ibr_topl = ibr_sigm = -1 ;

   if( (code & FIM_ALPHA_MASK)!= 0)              { ibr_fim  = nbrik; nbrik++; }
   if( (code & FIM_BEST_MASK) != 0 && ny_ref > 1){ ibr_best = nbrik; nbrik++; }
   if( (code & FIM_PERC_MASK) != 0)              { ibr_perc = nbrik; nbrik++; }
   if( (code & FIM_PAVE_MASK) != 0)              { ibr_pave = nbrik; nbrik++; }
   if( (code & FIM_BASE_MASK) != 0)              { ibr_base = nbrik; nbrik++; }
   if( (code & FIM_AVER_MASK) != 0)              { ibr_aver = nbrik; nbrik++; }
   if( (code & FIM_CORR_MASK) != 0)              { ibr_corr = nbrik; nbrik++; }
   if( (code & FIM_PTOP_MASK) != 0)              { ibr_ptop = nbrik; nbrik++; }
   if( (code & FIM_TOPL_MASK) != 0)              { ibr_topl = nbrik; nbrik++; }
   if( (code & FIM_SIGM_MASK) != 0)              { ibr_sigm = nbrik; nbrik++; }

   /** 01 Feb 2000: if no normal FIM stuff (code), skip to the ucode stuff **/

if(PRINT_TRACING)
{ char str[256] ; sprintf(str,"number of bricks = %d",nbrik) ; STATUS(str) ; }

   if( nbrik == 0 ){

#ifndef DONT_USE_METER
   meter = MCW_popup_meter( im3d->vwid->top_shell , METER_TOP_WIDE ) ;
   meter_pold = 0 ;
#endif

      goto ucode_stuff ;  /* way below */
   }

   /** normal case: do the normal recursive FIMming **/

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"code of FIM_MASKs = %d",code) ; STATUS(str) ; }

   /** allocate extra space for comparing results from multiple ref vectors **/

   if( ny_ref > 1 ){
      aval  = (float *) malloc(sizeof(float) * nvox) ;
      rbest = (float *) malloc(sizeof(float) * nvox) ;
      abest = (float *) malloc(sizeof(float) * nvox) ;
      ibest = (short *) malloc(sizeof(short) * nvox) ;  /* 15 Dec 1997 */
      pbest = (float *) malloc(sizeof(float) * nvox) ;  /* 16 Jan 1998 */
      bbest = (float *) malloc(sizeof(float) * nvox) ;  /* 16 Jan 1998 */
      pval  = (float *) malloc(sizeof(float) * nvox) ;  /* 16 Jan 1998 */
      bval  = (float *) malloc(sizeof(float) * nvox) ;  /* 16 Jan 1998 */

      paval  = (float *) malloc(sizeof(float) * nvox) ; /* 08 Sep 1999 */
      avval  = (float *) malloc(sizeof(float) * nvox) ; /* 08 Sep 1999 */
      pabest = (float *) malloc(sizeof(float) * nvox) ; /* 16 Jan 1998 */
      avbest = (float *) malloc(sizeof(float) * nvox) ; /* 16 Jan 1998 */

      ptval  = (float *) malloc(sizeof(float) * nvox) ; /* 03 Jan 2000 */
      tlval  = (float *) malloc(sizeof(float) * nvox) ; /* 03 Jan 2000 */
      sgval  = (float *) malloc(sizeof(float) * nvox) ; /* 03 Jan 2000 */
      ptbest = (float *) malloc(sizeof(float) * nvox) ; /* 03 Jan 2000 */
      tlbest = (float *) malloc(sizeof(float) * nvox) ; /* 03 Jan 2000 */
      sgbest = (float *) malloc(sizeof(float) * nvox) ; /* 03 Jan 2000 */

      if( sgbest == NULL ){
         fprintf(stderr,"\n*** 'best' malloc failure in AFNI_fimmer_compute\n") ;
         free(vval) ; free(indx) ;
         if( aval  != NULL ) free(aval)  ;
         if( rbest != NULL ) free(rbest) ;
         if( abest != NULL ) free(abest) ;
         if( ibest != NULL ) free(ibest) ;  /* 15 Dec 1997 */
         if( pbest != NULL ) free(pbest) ;  /* 16 Jan 1998 */
         if( bbest != NULL ) free(bbest) ;  /* 16 Jan 1998 */
         if( pval  != NULL ) free(pval)  ;  /* 16 Jan 1998 */
         if( bval  != NULL ) free(bval)  ;  /* 16 Jan 1998 */
         if( paval != NULL ) free(paval) ;  /* 08 Sep 1999 */
         if( avval != NULL ) free(avval) ;  /* 08 Sep 1999 */
         if( pabest!= NULL ) free(pabest);  /* 08 Sep 1999 */
         if( avbest!= NULL ) free(avbest);  /* 08 Sep 1999 */
         if( ptval != NULL ) free(ptval) ;  /* 03 Jan 2000 */
         if( tlval != NULL ) free(tlval) ;  /* 03 Jan 2000 */
         if( sgval != NULL ) free(sgval) ;  /* 03 Jan 2000 */
         if( ptbest!= NULL ) free(ptbest);  /* 03 Jan 2000 */
         if( tlbest!= NULL ) free(tlbest);  /* 03 Jan 2000 */
         if( sgbest!= NULL ) free(sgbest);  /* 03 Jan 2000 */
         RETURN(NULL) ;
      }
   } else {
      aval = rbest = abest = pbest = bbest = pval = bval = NULL ;
      paval = avval = pabest = avbest = NULL ;  /* 08 Sep 1999 */
      ptval = tlval = ptbest = tlbest = NULL ;  /* 03 Jan 2000 */
      sgval = sgbest = NULL ;                   /* 03 Jan 2000 */
      ibest = NULL ;                            /* 15 Dec 1997 */
   }

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"nxyz = %d  nvox = %d",nxyz,nvox) ; STATUS(str) ; }

   /*--- FIM: initialize recursive updates ---*/

   pc_ref = (PCOR_references **) malloc( sizeof(PCOR_references *) * ny_ref ) ;
   pc_vc  = (PCOR_voxel_corr **) malloc( sizeof(PCOR_voxel_corr *) * ny_ref ) ;

   if( pc_ref == NULL || pc_vc == NULL ){
      free(vval) ; free(indx) ; free(pc_ref) ; free(pc_vc) ;
      if( aval  != NULL ) free(aval) ;
      if( rbest != NULL ) free(rbest) ;
      if( abest != NULL ) free(abest) ;
      if( ibest != NULL ) free(ibest) ;  /* 15 Dec 1997 */
      if( pbest != NULL ) free(pbest) ;  /* 16 Jan 1998 */
      if( bbest != NULL ) free(bbest) ;  /* 16 Jan 1998 */
      if( pval  != NULL ) free(pval)  ;  /* 16 Jan 1998 */
      if( bval  != NULL ) free(bval)  ;  /* 16 Jan 1998 */
      if( paval != NULL ) free(paval) ;  /* 08 Sep 1999 */
      if( avval != NULL ) free(avval) ;  /* 08 Sep 1999 */
      if( pabest!= NULL ) free(pabest);  /* 08 Sep 1999 */
      if( avbest!= NULL ) free(avbest);  /* 08 Sep 1999 */
      if( ptval != NULL ) free(ptval) ;  /* 03 Jan 2000 */
      if( tlval != NULL ) free(tlval) ;  /* 03 Jan 2000 */
      if( sgval != NULL ) free(sgval) ;  /* 03 Jan 2000 */
      if( ptbest!= NULL ) free(ptbest);  /* 03 Jan 2000 */
      if( tlbest!= NULL ) free(tlbest);  /* 03 Jan 2000 */
      if( sgbest!= NULL ) free(sgbest);  /* 03 Jan 2000 */
      fprintf(stderr,"\n*** FIM initialization fails in AFNI_fimmer_compute\n") ;
      RETURN(NULL) ;
   }

   ifim = 0 ;
   for( ivec=0 ; ivec < ny_ref ; ivec++ ){
      pc_ref[ivec] = new_PCOR_references( fim_nref ) ;
      pc_vc[ivec]  = new_PCOR_voxel_corr( nvox , fim_nref ) ;
      if( pc_ref[ivec] == NULL || pc_vc[ivec] == NULL ) ifim++ ;
   }

   if( ifim > 0 ){
      for( ivec=0 ; ivec < ny_ref ; ivec++ ){
         free_PCOR_references(pc_ref[ivec]) ;
         free_PCOR_voxel_corr(pc_vc[ivec]) ;
      }
      free(vval) ; free(indx) ; free(pc_ref) ; free(pc_vc) ;
      if( aval  != NULL ) free(aval) ;
      if( rbest != NULL ) free(rbest) ;
      if( abest != NULL ) free(abest) ;
      if( ibest != NULL ) free(ibest) ;  /* 15 Dec 1997 */
      if( pbest != NULL ) free(pbest) ;  /* 16 Jan 1998 */
      if( bbest != NULL ) free(bbest) ;  /* 16 Jan 1998 */
      if( pval  != NULL ) free(pval)  ;  /* 16 Jan 1998 */
      if( bval  != NULL ) free(bval)  ;  /* 16 Jan 1998 */
      if( paval != NULL ) free(paval) ;  /* 08 Sep 1999 */
      if( avval != NULL ) free(avval) ;  /* 08 Sep 1999 */
      if( pabest!= NULL ) free(pabest);  /* 08 Sep 1999 */
      if( avbest!= NULL ) free(avbest);  /* 08 Sep 1999 */
      if( ptval != NULL ) free(ptval) ;  /* 03 Jan 2000 */
      if( tlval != NULL ) free(tlval) ;  /* 03 Jan 2000 */
      if( sgval != NULL ) free(sgval) ;  /* 03 Jan 2000 */
      if( ptbest!= NULL ) free(ptbest);  /* 03 Jan 2000 */
      if( tlbest!= NULL ) free(tlbest);  /* 03 Jan 2000 */
      if( sgbest!= NULL ) free(sgbest);  /* 03 Jan 2000 */
      fprintf(stderr,"\n*** FIM initialization fails in AFNI_fimmer_compute\n") ;
      RETURN(NULL) ;
   }

   /*--- Make a new dataset to hold the output ---*/

STATUS("making new dataset") ;

   new_dset = EDIT_empty_copy( dset_time ) ;

   if( nbrik == 1 && ucode == 0 ){           /* 1 brick out --> a 'fim' dataset */
      it = EDIT_dset_items( new_dset ,
                               ADN_prefix      , new_prefix ,
                               ADN_malloc_type , DATABLOCK_MEM_MALLOC ,
                               ADN_type        , ISHEAD(dset_time)
                                                 ? HEAD_FUNC_TYPE : GEN_FUNC_TYPE ,
                               ADN_func_type   , FUNC_FIM_TYPE ,
                               ADN_nvals       , 1 ,
                               ADN_datum_all   , MRI_short ,
                               ADN_ntt         , 0 ,
                            ADN_none ) ;
                                             /* 2 bricks, 2nd corr --> 'fico' */
   } else if( nbrik == 2 && ibr_corr == 1 && ucode == 0 ){
      it = EDIT_dset_items( new_dset ,
                               ADN_prefix      , new_prefix ,
                               ADN_malloc_type , DATABLOCK_MEM_MALLOC ,
                               ADN_type        , ISHEAD(dset_time)
                                                 ? HEAD_FUNC_TYPE : GEN_FUNC_TYPE ,
                               ADN_func_type   , FUNC_COR_TYPE ,
                               ADN_nvals       , 2 ,
                               ADN_datum_all   , MRI_short ,
                               ADN_ntt         , 0 ,
                            ADN_none ) ;

   } else if( nbrik > 0 ){                   /* otherwise --> 'fbuc' (bucket) */
      it = EDIT_dset_items( new_dset ,
                               ADN_prefix      , new_prefix ,
                               ADN_malloc_type , DATABLOCK_MEM_MALLOC ,
                               ADN_type        , ISHEAD(dset_time)
                                                 ? HEAD_FUNC_TYPE : GEN_FUNC_TYPE ,
                               ADN_func_type   , FUNC_BUCK_TYPE ,
                               ADN_nvals       , nbrik ,
                               ADN_datum_all   , MRI_short ,
                               ADN_ntt         , 0 ,
                            ADN_none ) ;
   } else {
      it = 999 ;
   }

   if( it > 0 ){
      fprintf(stderr,
              "\n*** EDIT_dset_items error %d in AFNI_fimmer_compute\n",it) ;
      THD_delete_3dim_dataset( new_dset , False ) ;
      for( ivec=0 ; ivec < ny_ref ; ivec++ ){
         free_PCOR_references(pc_ref[ivec]) ;
         free_PCOR_voxel_corr(pc_vc[ivec]) ;
      }
      free(vval) ; free(indx) ; free(pc_ref) ; free(pc_vc) ;
      if( aval  != NULL ) free(aval) ;
      if( rbest != NULL ) free(rbest) ;
      if( abest != NULL ) free(abest) ;
      if( ibest != NULL ) free(ibest) ;  /* 15 Dec 1997 */
      if( pbest != NULL ) free(pbest) ;  /* 16 Jan 1998 */
      if( bbest != NULL ) free(bbest) ;  /* 16 Jan 1998 */
      if( pval  != NULL ) free(pval)  ;  /* 16 Jan 1998 */
      if( bval  != NULL ) free(bval)  ;  /* 16 Jan 1998 */
      if( paval != NULL ) free(paval) ;  /* 08 Sep 1999 */
      if( avval != NULL ) free(avval) ;  /* 08 Sep 1999 */
      if( pabest!= NULL ) free(pabest);  /* 08 Sep 1999 */
      if( avbest!= NULL ) free(avbest);  /* 08 Sep 1999 */
      if( ptval != NULL ) free(ptval) ;  /* 03 Jan 2000 */
      if( tlval != NULL ) free(tlval) ;  /* 03 Jan 2000 */
      if( sgval != NULL ) free(sgval) ;  /* 03 Jan 2000 */
      if( ptbest!= NULL ) free(ptbest);  /* 03 Jan 2000 */
      if( tlbest!= NULL ) free(tlbest);  /* 03 Jan 2000 */
      if( sgbest!= NULL ) free(sgbest);  /* 03 Jan 2000 */
      RETURN(NULL) ;
   }

   /* modify labels for each brick */

   if( ibr_fim >= 0 )
      EDIT_BRICK_LABEL( new_dset , ibr_fim  , "Fit Coef" ) ;
   if( ibr_corr >= 0 )
      EDIT_BRICK_LABEL( new_dset , ibr_corr , "Correlation" ) ;
   if( ibr_best >= 0 )
      EDIT_BRICK_LABEL( new_dset , ibr_best , "Best Index" ) ;
   if( ibr_perc >= 0 )
      EDIT_BRICK_LABEL( new_dset , ibr_perc , "% Change" ) ;
   if( ibr_base >= 0 )
      EDIT_BRICK_LABEL( new_dset , ibr_base , "Baseline" ) ;

   if( ibr_pave >= 0 )
      EDIT_BRICK_LABEL( new_dset , ibr_pave , "% From Ave" ) ;  /* 08 Sep 1999 */
   if( ibr_aver >= 0 )
      EDIT_BRICK_LABEL( new_dset , ibr_aver , "Average" ) ;

   if( ibr_ptop >= 0 )
      EDIT_BRICK_LABEL( new_dset , ibr_ptop , "% From Top" ) ;  /* 03 Jan 2000 */
   if( ibr_topl >= 0 )
      EDIT_BRICK_LABEL( new_dset , ibr_topl , "Topline" ) ;
   if( ibr_sigm >= 0 )
      EDIT_BRICK_LABEL( new_dset , ibr_sigm , "Sigma Resid" ) ;

   /*-- 30 Aug 1999: set limits on percent change --*/

   if( ibr_perc >= 0 || ibr_pave >= 0 || ibr_ptop >= 0 ){
      char * cp = my_getenv("AFNI_FIM_PERCENT_LIMIT") ;
      if( cp != NULL ){
         float tp = strtod(cp,NULL) ;
         if( tp > 0.0 ) top_perc = tp ;
      }
   }

   /* create bricks */

STATUS("making output bricks") ;

   for( iv=0 ; iv < new_dset->dblk->nvals ; iv++ ){
      ptr = malloc( DSET_BRICK_BYTES(new_dset,iv) ) ;
      mri_fix_data_pointer( ptr ,  DSET_BRICK(new_dset,iv) ) ;
   }

   if( THD_count_databricks(new_dset->dblk) < new_dset->dblk->nvals ){
      fprintf(stderr,
              "\n*** failure to malloc new bricks in AFNI_fimmer_compute\n") ;
      THD_delete_3dim_dataset( new_dset , False ) ;
      for( ivec=0 ; ivec < ny_ref ; ivec++ ){
         free_PCOR_references(pc_ref[ivec]) ;
         free_PCOR_voxel_corr(pc_vc[ivec]) ;
      }
      free(vval) ; free(indx) ; free(pc_ref) ; free(pc_vc) ;
      if( aval  != NULL ) free(aval) ;
      if( rbest != NULL ) free(rbest) ;
      if( abest != NULL ) free(abest) ;
      if( ibest != NULL ) free(ibest) ;  /* 15 Dec 1997 */
      if( pbest != NULL ) free(pbest) ;  /* 16 Jan 1998 */
      if( bbest != NULL ) free(bbest) ;  /* 16 Jan 1998 */
      if( pval  != NULL ) free(pval)  ;  /* 16 Jan 1998 */
      if( bval  != NULL ) free(bval)  ;  /* 16 Jan 1998 */
      if( paval != NULL ) free(paval) ;  /* 08 Sep 1999 */
      if( avval != NULL ) free(avval) ;  /* 08 Sep 1999 */
      if( pabest!= NULL ) free(pabest);  /* 08 Sep 1999 */
      if( avbest!= NULL ) free(avbest);  /* 08 Sep 1999 */
      if( ptval != NULL ) free(ptval) ;  /* 03 Jan 2000 */
      if( tlval != NULL ) free(tlval) ;  /* 03 Jan 2000 */
      if( sgval != NULL ) free(sgval) ;  /* 03 Jan 2000 */
      if( ptbest!= NULL ) free(ptbest);  /* 03 Jan 2000 */
      if( tlbest!= NULL ) free(tlbest);  /* 03 Jan 2000 */
      if( sgbest!= NULL ) free(sgbest);  /* 03 Jan 2000 */
      RETURN(NULL) ;
   }

   /*---------------------------------*/
   /*--- FIM: do recursive updates ---*/

#ifndef DONT_USE_METER
   meter = MCW_popup_meter( im3d->vwid->top_shell , METER_TOP_WIDE ) ;
   meter_pold = 0 ;
#endif

STATUS("starting recursive least squares") ;

   for( it=itbot ; it < ntime ; it++ ){  /* loop over time */

      nnow = 0 ;  /* number of updates done at this time point */

      for( ivec=0 ; ivec < ny_ref ; ivec++ ){  /* loop over ref vects */

         tsar = MRI_FLOAT_PTR(ref_ts) + (ivec*nx_ref) ; /* ptr to vect */
         if( tsar[it] >= WAY_BIG ) continue ;           /* skip this */

         ref_vec[0] = 1.0 ;         /* we always supply ort for constant */
         for( ip=1 ; ip <= polort ; ip++ )              /* 30 May 1999:    */
            ref_vec[ip] = ref_vec[ip-1] * ((float)it) ; /* and polynomials */

         if( internal_ort ){         /* no external orts */
            ref_vec[ip] = tsar[it] ; /* ref value */
         } else {
            for( iv=0 ; iv < ny_ort ; iv++ )             /* external */
               ref_vec[iv+ip] = ortar[it + iv*nx_ort] ;  /* orts */

            ref_vec[ny_ort+ip] = tsar[it] ;              /* ref value */
         }

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"time index=%d  ideal[%d]=%f" , it,ivec,tsar[it] ) ;
  STATUS(str) ; }

         /* process the ort+ref update */

         update_PCOR_references( ref_vec , pc_ref[ivec] ) ;

         /* first time thru: load data from dataset */

         if( nnow == 0 ){
            switch( dtyp ){
               case MRI_short:{
                  short * dar = (short *) DSET_ARRAY(dset_time,it) ;
                  for( iv=0; iv < nvox; iv++ ) vval[iv] = (float) dar[indx[iv]];
               }
               break ;

               case MRI_float:{
                  float * dar = (float *) DSET_ARRAY(dset_time,it) ;
                  for( iv=0; iv < nvox; iv++ ) vval[iv] = (float) dar[indx[iv]];
               }
               break ;

               case MRI_byte:{
                  byte * dar = (byte *) DSET_ARRAY(dset_time,it) ;
                  for( iv=0; iv < nvox; iv++ ) vval[iv] = (float) dar[indx[iv]];
               }
               break ;
            }
         }

         /* process the data update */

         PCOR_update_float( vval , pc_ref[ivec] , pc_vc[ivec] ) ;
         nnow++ ;  /* one more update at this time point */
      }  /* end of loop over ref vects */

      if( nnow > 0 ) nupdt++ ;  /* number of time points that had updates */

#ifndef DONT_USE_METER
      meter_perc = (int) ( 100.0 * nupdt / ngood_ref ) ;
      if( meter_perc != meter_pold ){
         MCW_set_meter( meter , meter_perc ) ;
         meter_pold = meter_perc ;
      }
#endif

   }  /* end of loop over time */

   /*-------------------------------------------*/
   /*--- Load final results into the dataset ---*/

   /*--- set the statistical parameters ---*/

   stataux[0] = nupdt ;               /* number of points used */
   stataux[1] = (ny_ref==1) ? 1 : 2 ; /* number of references  */
   stataux[2] = fim_nref - 1 ;        /* number of orts        */
   for( iv=3 ; iv < MAX_STAT_AUX ; iv++ ) stataux[iv] = 0.0 ;

   if( ibr_corr >= 0 ){
      if( new_dset->func_type == FUNC_COR_TYPE )
         EDIT_dset_items( new_dset, ADN_stat_aux, stataux, ADN_none ) ;

      EDIT_BRICK_TO_FICO( new_dset, ibr_corr, stataux[0],stataux[1],stataux[2] ) ;
   }

#ifndef DONT_USE_METER
# define METERIZE(ib) do { meter_perc = (int) ( 100.0 * (ib) / nbrik ) ; \
                           if( meter_perc != meter_pold ){               \
                              MCW_set_meter( meter , meter_perc ) ;      \
                              meter_pold = meter_perc ;                  \
                           } } while(0)
#else
# define METERIZE(ib) /*nada*/
#endif

   /*** Compute brick arrays for new dataset ***/
   /*  [load scale factors into stataux, too]  */

   if( ny_ref == 1 ){

   /*** Just 1 ref vector --> load values directly into dataset ***/

      if( ibr_fim >= 0 ){

STATUS("getting 1 ref alpha") ;

         PCOR_get_coef( pc_ref[0] , pc_vc[0] , vval ) ;

         topval = 0.0 ;
         for( iv=0 ; iv < nvox ; iv++ )
            if( fabs(vval[iv]) > topval ) topval = fabs(vval[iv]) ;

         bar = DSET_ARRAY( new_dset , ibr_fim ) ;
         memset( bar , 0 , sizeof(short)*nxyz ) ;

         if( topval > 0.0 ){
            topval = MRI_TYPE_maxval[MRI_short] / topval ;
            for( iv=0 ; iv < nvox ; iv++ )
               bar[indx[iv]] = (short)(topval * vval[iv] + 0.499) ;

            stataux[ibr_fim] = 1.0/topval ;
         } else {
            stataux[ibr_fim] = 0.0 ;
         }

         METERIZE(ibr_fim) ;
      }

      if( ibr_corr >= 0 ){

STATUS("getting 1 ref pcor") ;

         PCOR_get_pcor( pc_ref[0] , pc_vc[0] , vval ) ;

         bar = DSET_ARRAY( new_dset , ibr_corr ) ;
         memset( bar , 0 , sizeof(short)*nxyz ) ;

         for( iv=0 ; iv < nvox ; iv++ )
            bar[indx[iv]] = (short)(FUNC_COR_SCALE_SHORT * vval[iv] + 0.499) ;

         stataux[ibr_corr] = 1.0 / FUNC_COR_SCALE_SHORT ;

         METERIZE(ibr_corr) ;
      }

      if( ibr_perc >= 0 ){

STATUS("getting 1 ref perc") ;

         PCOR_get_perc( pc_ref[0] , pc_vc[0] , vval , NULL , 0 ) ;

         if( top_perc > 0.0 ) EDIT_clip_float( top_perc , nvox , vval ) ;

         topval = 0.0 ;
         for( iv=0 ; iv < nvox ; iv++ )
            if( fabs(vval[iv]) > topval ) topval = fabs(vval[iv]) ;

         bar = DSET_ARRAY( new_dset , ibr_perc ) ;
         memset( bar , 0 , sizeof(short)*nxyz ) ;

         if( topval > 0.0 ){
            topval = MRI_TYPE_maxval[MRI_short] / topval ;
            for( iv=0 ; iv < nvox ; iv++ )
               bar[indx[iv]] = (short)(topval * vval[iv] + 0.499) ;

            stataux[ibr_perc] = 1.0/topval ;
         } else {
            stataux[ibr_perc] = 0.0 ;
         }

         METERIZE(ibr_perc) ;
      }

      if( ibr_pave >= 0 ){  /* 08 Sep 1999 */

STATUS("getting 1 ref pave") ;

         PCOR_get_perc( pc_ref[0] , pc_vc[0] , vval , NULL , 1 ) ;

         if( top_perc > 0.0 ) EDIT_clip_float( top_perc , nvox , vval ) ;

         topval = 0.0 ;
         for( iv=0 ; iv < nvox ; iv++ )
            if( fabs(vval[iv]) > topval ) topval = fabs(vval[iv]) ;

         bar = DSET_ARRAY( new_dset , ibr_pave ) ;
         memset( bar , 0 , sizeof(short)*nxyz ) ;

         if( topval > 0.0 ){
            topval = MRI_TYPE_maxval[MRI_short] / topval ;
            for( iv=0 ; iv < nvox ; iv++ )
               bar[indx[iv]] = (short)(topval * vval[iv] + 0.499) ;

            stataux[ibr_pave] = 1.0/topval ;
         } else {
            stataux[ibr_pave] = 0.0 ;
         }

         METERIZE(ibr_pave) ;
      }

      if( ibr_ptop >= 0 ){  /* 03 Jan 2000 */

STATUS("getting 1 ref ptop") ;

         PCOR_get_perc( pc_ref[0] , pc_vc[0] , vval , NULL , 2 ) ;

         if( top_perc > 0.0 ) EDIT_clip_float( top_perc , nvox , vval ) ;

         topval = 0.0 ;
         for( iv=0 ; iv < nvox ; iv++ )
            if( fabs(vval[iv]) > topval ) topval = fabs(vval[iv]) ;

         bar = DSET_ARRAY( new_dset , ibr_ptop ) ;
         memset( bar , 0 , sizeof(short)*nxyz ) ;

         if( topval > 0.0 ){
            topval = MRI_TYPE_maxval[MRI_short] / topval ;
            for( iv=0 ; iv < nvox ; iv++ )
               bar[indx[iv]] = (short)(topval * vval[iv] + 0.499) ;

            stataux[ibr_ptop] = 1.0/topval ;
         } else {
            stataux[ibr_ptop] = 0.0 ;
         }

         METERIZE(ibr_ptop) ;
      }

      if( ibr_base >= 0 ){

STATUS("getting 1 ref base") ;

         PCOR_get_perc( pc_ref[0] , pc_vc[0] , NULL , vval , 0 ) ;

         topval = 0.0 ;
         for( iv=0 ; iv < nvox ; iv++ )
            if( fabs(vval[iv]) > topval ) topval = fabs(vval[iv]) ;

         bar = DSET_ARRAY( new_dset , ibr_base ) ;
         memset( bar , 0 , sizeof(short)*nxyz ) ;

         if( topval > 0.0 ){
            topval = MRI_TYPE_maxval[MRI_short] / topval ;
            for( iv=0 ; iv < nvox ; iv++ )
               bar[indx[iv]] = (short)(topval * vval[iv] + 0.499) ;

            stataux[ibr_base] = 1.0/topval ;
         } else {
            stataux[ibr_base] = 0.0 ;
         }

         METERIZE(ibr_base) ;
      }

      if( ibr_aver >= 0 ){  /* 08 Sep 1999 */

STATUS("getting 1 ref aver") ;

         PCOR_get_perc( pc_ref[0] , pc_vc[0] , NULL , vval , 1 ) ;

         topval = 0.0 ;
         for( iv=0 ; iv < nvox ; iv++ )
            if( fabs(vval[iv]) > topval ) topval = fabs(vval[iv]) ;

         bar = DSET_ARRAY( new_dset , ibr_aver ) ;
         memset( bar , 0 , sizeof(short)*nxyz ) ;

         if( topval > 0.0 ){
            topval = MRI_TYPE_maxval[MRI_short] / topval ;
            for( iv=0 ; iv < nvox ; iv++ )
               bar[indx[iv]] = (short)(topval * vval[iv] + 0.499) ;

            stataux[ibr_aver] = 1.0/topval ;
         } else {
            stataux[ibr_aver] = 0.0 ;
         }

         METERIZE(ibr_aver) ;
      }

      if( ibr_topl >= 0 ){  /* 03 Jan 2000 */

STATUS("getting 1 ref topl") ;

         PCOR_get_perc( pc_ref[0] , pc_vc[0] , NULL , vval , 2 ) ;

         topval = 0.0 ;
         for( iv=0 ; iv < nvox ; iv++ )
            if( fabs(vval[iv]) > topval ) topval = fabs(vval[iv]) ;

         bar = DSET_ARRAY( new_dset , ibr_topl ) ;
         memset( bar , 0 , sizeof(short)*nxyz ) ;

         if( topval > 0.0 ){
            topval = MRI_TYPE_maxval[MRI_short] / topval ;
            for( iv=0 ; iv < nvox ; iv++ )
               bar[indx[iv]] = (short)(topval * vval[iv] + 0.499) ;

            stataux[ibr_topl] = 1.0/topval ;
         } else {
            stataux[ibr_topl] = 0.0 ;
         }

         METERIZE(ibr_topl) ;
      }

      if( ibr_sigm >= 0 ){  /* 03 Jan 2000 */

STATUS("getting 1 ref sigm") ;

         PCOR_get_stdev( pc_vc[0] , vval ) ;

         topval = 0.0 ;
         for( iv=0 ; iv < nvox ; iv++ )
            if( fabs(vval[iv]) > topval ) topval = fabs(vval[iv]) ;

         bar = DSET_ARRAY( new_dset , ibr_sigm ) ;
         memset( bar , 0 , sizeof(short)*nxyz ) ;

         if( topval > 0.0 ){
            topval = MRI_TYPE_maxval[MRI_short] / topval ;
            for( iv=0 ; iv < nvox ; iv++ )
               bar[indx[iv]] = (short)(topval * vval[iv] + 0.499) ;

            stataux[ibr_sigm] = 1.0/topval ;
         } else {
            stataux[ibr_sigm] = 0.0 ;
         }

         METERIZE(ibr_sigm) ;
      }

   } else {

   /*** Multiple references --> find best correlation at each voxel ***/

      /*--- get first ref results into abest and rbest (best so far) ---*/

STATUS("getting first ref results") ;

      PCOR_get_coef( pc_ref[0] , pc_vc[0] , abest ) ;
      PCOR_get_pcor( pc_ref[0] , pc_vc[0] , rbest ) ;
      PCOR_get_perc( pc_ref[0] , pc_vc[0] , pbest , bbest , 0 ) ;
      PCOR_get_perc( pc_ref[0] , pc_vc[0] , pabest, avbest, 1 ) ;
      PCOR_get_perc( pc_ref[0] , pc_vc[0] , ptbest, tlbest, 2 ) ;
      PCOR_get_stdev( pc_vc[0] , sgbest ) ;

      for( iv=0 ; iv < nvox ; iv++ ) ibest[iv] = 1 ;  /* 15 Dec 1997 */

      /*--- for each succeeding ref vector,
            get results into aval and vval,
            if |vval| > |rbest|, then use that result instead ---*/

      for( ivec=1 ; ivec < ny_ref ; ivec++ ){

STATUS(" == getting results for next ref") ;

         PCOR_get_coef( pc_ref[ivec] , pc_vc[ivec] , aval ) ;
         PCOR_get_pcor( pc_ref[ivec] , pc_vc[ivec] , vval ) ;
         PCOR_get_perc( pc_ref[ivec] , pc_vc[ivec] , pval , bval , 0 ) ;
         PCOR_get_perc( pc_ref[ivec] , pc_vc[ivec] , paval, avval, 1 ) ;
         PCOR_get_perc( pc_ref[ivec] , pc_vc[ivec] , ptval, tlval, 2 ) ;
         PCOR_get_stdev( pc_vc[ivec] , sgval ) ;

STATUS(" == and finding the best results") ;

         for( iv=0 ; iv < nvox ; iv++ ){
            if( fabs(vval[iv]) > fabs(rbest[iv]) ){
               rbest[iv] = vval[iv] ;
               abest[iv] = aval[iv] ;
               ibest[iv] = (ivec+1) ;   /* 15 Dec 1997 */
               pbest[iv] = pval[iv] ;   /* Jan 1998 */
               bbest[iv] = bval[iv] ;
               pabest[iv]= paval[iv] ;  /* 08 Sep 1999 */
               avbest[iv]= avval[iv] ;
               ptbest[iv]= ptval[iv] ;  /* 03 Jan 1999 */
               tlbest[iv]= tlval[iv] ;
               sgbest[iv]= sgval[iv] ;
            }
         }
      }

      /*--- at this point, abest and rbest are the best
            results, so scale them into the dataset bricks ---*/

      /** fim brick **/

      if( ibr_fim >= 0 ){

if(PRINT_TRACING)
{ char str[256]; sprintf(str,"getting ibr_fim=%d",ibr_fim); STATUS(str); }

         topval = 0.0 ;
         for( iv=0 ; iv < nvox ; iv++ )
            if( fabs(abest[iv]) > topval ) topval = fabs(abest[iv]) ;

         bar = DSET_ARRAY( new_dset , ibr_fim ) ;
         memset( bar , 0 , sizeof(short)*nxyz ) ;

         if( topval > 0.0 ){
            topval = MRI_TYPE_maxval[MRI_short] / topval ;
            for( iv=0 ; iv < nvox ; iv++ )
               bar[indx[iv]] = (short)(topval * abest[iv] + 0.499) ;

            stataux[ibr_fim] = 1.0/topval ;
         } else {
            stataux[ibr_fim] = 0.0 ;
         }

         METERIZE(ibr_fim) ;
      }

      /** threshold brick **/

      if( ibr_corr >= 0 ){

if(PRINT_TRACING)
{ char str[256]; sprintf(str,"getting ibr_corr=%d",ibr_corr); STATUS(str); }

         bar = DSET_ARRAY( new_dset , ibr_corr ) ;
         memset( bar , 0 , sizeof(short)*nxyz ) ;

         for( iv=0 ; iv < nvox ; iv++ )
            bar[indx[iv]] = (short)(FUNC_COR_SCALE_SHORT * rbest[iv] + 0.499) ;

         stataux[ibr_corr] = 1.0 / FUNC_COR_SCALE_SHORT ;

         METERIZE(ibr_corr) ;
      }

      /** best index brick (15 Dec 1997) */

      if( ibr_best >= 0 ){

if(PRINT_TRACING)
{ char str[256]; sprintf(str,"getting ibr_best=%d",ibr_best); STATUS(str); }

         bar = DSET_ARRAY( new_dset , ibr_best ) ;
         memset( bar , 0 , sizeof(short)*nxyz ) ;
         for( iv=0 ; iv < nvox ; iv++ ) bar[indx[iv]] = ibest[iv] ;
         stataux[ibr_best] = 0.0 ;  /* no scaling */

         METERIZE(ibr_best) ;
      }

      /** perc brick */

      if( ibr_perc >= 0 ){

if(PRINT_TRACING)
{ char str[256]; sprintf(str,"getting ibr_perc=%d",ibr_perc); STATUS(str); }

         if( top_perc > 0.0 ) EDIT_clip_float( top_perc , nvox , pbest ) ;

         topval = 0.0 ;
         for( iv=0 ; iv < nvox ; iv++ )
            if( fabs(pbest[iv]) > topval ) topval = fabs(pbest[iv]) ;

         bar = DSET_ARRAY( new_dset , ibr_perc ) ;
         memset( bar , 0 , sizeof(short)*nxyz ) ;

         if( topval > 0.0 ){
            topval = MRI_TYPE_maxval[MRI_short] / topval ;
            for( iv=0 ; iv < nvox ; iv++ )
               bar[indx[iv]] = (short)(topval * pbest[iv] + 0.499) ;

            stataux[ibr_perc] = 1.0/topval ;
         } else {
            stataux[ibr_perc] = 0.0 ;
         }

         METERIZE(ibr_perc) ;
      }

      /** pave brick [08 Sep 1999] */

      if( ibr_pave >= 0 ){

if(PRINT_TRACING)
{ char str[256]; sprintf(str,"getting ibr_pave=%d",ibr_pave); STATUS(str); }

         if( top_perc > 0.0 ) EDIT_clip_float( top_perc , nvox , pabest ) ;

         topval = 0.0 ;
         for( iv=0 ; iv < nvox ; iv++ )
            if( fabs(pabest[iv]) > topval ) topval = fabs(pabest[iv]) ;

         bar = DSET_ARRAY( new_dset , ibr_pave ) ;
         memset( bar , 0 , sizeof(short)*nxyz ) ;

         if( topval > 0.0 ){
            topval = MRI_TYPE_maxval[MRI_short] / topval ;
            for( iv=0 ; iv < nvox ; iv++ )
               bar[indx[iv]] = (short)(topval * pabest[iv] + 0.499) ;

            stataux[ibr_pave] = 1.0/topval ;
         } else {
            stataux[ibr_pave] = 0.0 ;
         }

         METERIZE(ibr_pave) ;
      }

      /** ptop brick [03 Jan 2000] */

      if( ibr_ptop >= 0 ){

if(PRINT_TRACING)
{ char str[256]; sprintf(str,"getting ibr_ptop=%d",ibr_ptop); STATUS(str); }

         if( top_perc > 0.0 ) EDIT_clip_float( top_perc , nvox , ptbest ) ;

         topval = 0.0 ;
         for( iv=0 ; iv < nvox ; iv++ )
            if( fabs(ptbest[iv]) > topval ) topval = fabs(ptbest[iv]) ;

         bar = DSET_ARRAY( new_dset , ibr_ptop ) ;
         memset( bar , 0 , sizeof(short)*nxyz ) ;

         if( topval > 0.0 ){
            topval = MRI_TYPE_maxval[MRI_short] / topval ;
            for( iv=0 ; iv < nvox ; iv++ )
               bar[indx[iv]] = (short)(topval * ptbest[iv] + 0.499) ;

            stataux[ibr_ptop] = 1.0/topval ;
         } else {
            stataux[ibr_ptop] = 0.0 ;
         }

         METERIZE(ibr_ptop) ;
      }

      /** base brick */

      if( ibr_base >= 0 ){

if(PRINT_TRACING)
{ char str[256]; sprintf(str,"getting ibr_base=%d",ibr_base); STATUS(str); }

         topval = 0.0 ;
         for( iv=0 ; iv < nvox ; iv++ )
            if( fabs(bbest[iv]) > topval ) topval = fabs(bbest[iv]) ;

         bar = DSET_ARRAY( new_dset , ibr_base ) ;
         memset( bar , 0 , sizeof(short)*nxyz ) ;

         if( topval > 0.0 ){
            topval = MRI_TYPE_maxval[MRI_short] / topval ;
            for( iv=0 ; iv < nvox ; iv++ )
               bar[indx[iv]] = (short)(topval * bbest[iv] + 0.499) ;

            stataux[ibr_base] = 1.0/topval ;
         } else {
            stataux[ibr_base] = 0.0 ;
         }

         METERIZE(ibr_base) ;
      }

      /** aver brick [08 Sep 1999] */

      if( ibr_aver >= 0 ){

if(PRINT_TRACING)
{ char str[256]; sprintf(str,"getting ibr_aver=%d",ibr_aver); STATUS(str); }

         topval = 0.0 ;
         for( iv=0 ; iv < nvox ; iv++ )
            if( fabs(avbest[iv]) > topval ) topval = fabs(avbest[iv]) ;

         bar = DSET_ARRAY( new_dset , ibr_aver ) ;
         memset( bar , 0 , sizeof(short)*nxyz ) ;

         if( topval > 0.0 ){
            topval = MRI_TYPE_maxval[MRI_short] / topval ;
            for( iv=0 ; iv < nvox ; iv++ )
               bar[indx[iv]] = (short)(topval * avbest[iv] + 0.499) ;

            stataux[ibr_aver] = 1.0/topval ;
         } else {
            stataux[ibr_aver] = 0.0 ;
         }

         METERIZE(ibr_aver) ;
      }

      /** topl brick [03 Jan 2000] */

      if( ibr_topl >= 0 ){

if(PRINT_TRACING)
{ char str[256]; sprintf(str,"getting ibr_topl=%d",ibr_topl); STATUS(str); }

         topval = 0.0 ;
         for( iv=0 ; iv < nvox ; iv++ )
            if( fabs(tlbest[iv]) > topval ) topval = fabs(tlbest[iv]) ;

         bar = DSET_ARRAY( new_dset , ibr_topl ) ;
         memset( bar , 0 , sizeof(short)*nxyz ) ;

         if( topval > 0.0 ){
            topval = MRI_TYPE_maxval[MRI_short] / topval ;
            for( iv=0 ; iv < nvox ; iv++ )
               bar[indx[iv]] = (short)(topval * tlbest[iv] + 0.499) ;

            stataux[ibr_topl] = 1.0/topval ;
         } else {
            stataux[ibr_topl] = 0.0 ;
         }

         METERIZE(ibr_topl) ;
      }

      /** sigm brick [03 Jan 2000]**/

      if( ibr_sigm >= 0 ){

if(PRINT_TRACING)
{ char str[256]; sprintf(str,"getting ibr_sigm=%d",ibr_sigm); STATUS(str); }

         topval = 0.0 ;
         for( iv=0 ; iv < nvox ; iv++ )
            if( fabs(sgbest[iv]) > topval ) topval = fabs(sgbest[iv]) ;

         bar = DSET_ARRAY( new_dset , ibr_sigm ) ;
         memset( bar , 0 , sizeof(short)*nxyz ) ;

         if( topval > 0.0 ){
            topval = MRI_TYPE_maxval[MRI_short] / topval ;
            for( iv=0 ; iv < nvox ; iv++ )
               bar[indx[iv]] = (short)(topval * sgbest[iv] + 0.499) ;

            stataux[ibr_sigm] = 1.0/topval ;
         } else {
            stataux[ibr_sigm] = 0.0 ;
         }

         METERIZE(ibr_sigm) ;
      }

   }  /* end of multiple reference case */

   /*** Set the brick factors for the new dataset,
        no matter how it was computed above.       ***/

STATUS("setting brick_fac") ;

   (void) EDIT_dset_items( new_dset , ADN_brick_fac , stataux , ADN_none ) ;

#ifndef DONT_USE_METER
   MCW_set_meter( meter , 100 ) ;
#endif

   /*--- End of recursive updates; now free temporary workspaces ---*/

   for( ivec=0 ; ivec < ny_ref ; ivec++ ){
      free_PCOR_references(pc_ref[ivec]) ;
      free_PCOR_voxel_corr(pc_vc[ivec]) ;
   }
   free(pc_ref) ; free(pc_vc) ;
   if( aval  != NULL ) free(aval) ;
   if( rbest != NULL ) free(rbest) ;
   if( abest != NULL ) free(abest) ;
   if( ibest != NULL ) free(ibest) ;  /* 15 Dec 1997 */
   if( pbest != NULL ) free(pbest) ;  /* 16 Jan 1998 */
   if( bbest != NULL ) free(bbest) ;  /* 16 Jan 1998 */
   if( pval  != NULL ) free(pval) ;   /* 16 Jan 1998 */
   if( bval  != NULL ) free(bval) ;   /* 16 Jan 1998 */
   if( paval != NULL ) free(paval) ;  /* 08 Sep 1999 */
   if( avval != NULL ) free(avval) ;  /* 08 Sep 1999 */
   if( pabest!= NULL ) free(pabest);  /* 08 Sep 1999 */
   if( avbest!= NULL ) free(avbest);  /* 08 Sep 1999 */
   if( ptval != NULL ) free(ptval) ;  /* 03 Jan 2000 */
   if( tlval != NULL ) free(tlval) ;  /* 03 Jan 2000 */
   if( sgval != NULL ) free(sgval) ;  /* 03 Jan 2000 */
   if( ptbest!= NULL ) free(ptbest);  /* 03 Jan 2000 */
   if( tlbest!= NULL ) free(tlbest);  /* 03 Jan 2000 */
   if( sgbest!= NULL ) free(sgbest);  /* 03 Jan 2000 */

   /*-----------------------------------------------------*/
   /*--- 01 Feb 2000: execute user specified functions ---*/

ucode_stuff:

#define MAXUFUN 64  /* should be at least sizeof(int) */
#define MAXTS   32  /* number of timeseries to process at once */

   if( ucode != 0 ){
      MCW_function_list * rlist = &(GLOBAL_library.registered_fim) ;
      int uuse[MAXUFUN] , nbrik[MAXUFUN] , brik1[MAXUFUN] ;
      void * udata[MAXUFUN] ;
      generic_func * ufunc[MAXUFUN] ;
      int nuse , uu , newbrik , oldbrik ;
      FIMdata fd ;
      MRI_IMAGE * tsim ;
      float     * tsar , * val , ** vbr ;
      short     * sar ;
      int         nts , jts ;
      MRI_IMARR * imar ;

      /* mark which ones to execute */

      for( newbrik=nuse=uu=0 ; uu < rlist->num ; uu++ ){
         if( (ucode & (1<<uu)) != 0 ){
            uuse [nuse] = uu ;
            ufunc[nuse] = rlist->funcs[uu] ;     /* user_func for this func */
            nbrik[nuse] = rlist->flags[uu] ;     /* new bricks for this func */
            udata[nuse] = rlist->func_data[uu] ; /* user_data for this func */
            brik1[nuse] = newbrik ;              /* index of 1st brick */
            newbrik    += nbrik[nuse] ;          /* total number of new bricks */
            nuse++ ;
         }
      }

      if( nuse == 0 ) goto final_exit ; /* shouldn't happen */

      /* do the initialization calls to the user_func functions */

      fd.ref_ts = ref_ts ;
      fd.ort_ts = ort_ts ;
      fd.nvox   = nvox   ;
      fd.ignore = im3d->fimdata->init_ignore ;
      fd.polort = polort ;

#ifndef DONT_USE_METER
      MCW_set_meter( meter , 0 ) ; meter_pold = 0.0 ;
#endif

      for( uu=0 ; uu < nuse ; uu++ )
         ufunc[uu]( ntime , NULL , udata[uu] , nbrik[uu] , (void *)(&fd) ) ;

      /* loop over voxels,
         assemble time series,
         call functions to put results in val[],
         store float outputs in vbr[][]          */

      vbr = (float **) malloc(sizeof(float *)*newbrik) ;
      for( iv=0 ; iv < newbrik ; iv++ )
         vbr[iv] = (float *) malloc(sizeof(float)*nvox) ;

      val = (float *) malloc(sizeof(float)*newbrik) ;

      for( iv=0 ; iv < nvox ; iv+=MAXTS ){
         nts  = MIN( MAXTS , nvox-iv ) ;
         imar = THD_extract_many_series( nts,indx+iv , dset_time ) ;

         for( jts=0 ; jts < nts ; jts++ ){
            tsim = IMARR_SUBIMAGE(imar,jts) ;                     /* data */
            tsar = MRI_FLOAT_PTR(tsim) ;

            for( uu=0 ; uu < nuse ; uu++ ){
               ufunc[uu]( ntime , tsar ,                          /* func */
                          udata[uu] , nbrik[uu] , (void *) val ) ;

               for( it=0 ; it < nbrik[uu] ; it++ )             /* storage */
                  vbr[it+brik1[uu]][iv+jts] = val[it] ;
            }
         }

         DESTROY_IMARR(imar) ;                        /* garbage disposal */

#ifndef DONT_USE_METER
         meter_perc = (int) ( 100.0 * iv / nvox ) ;
         if( meter_perc != meter_pold ){
            MCW_set_meter( meter , meter_perc ) ;
            meter_pold = meter_perc ;
         }
#endif
      }
      free(val) ;  /* no longer needed */
#ifndef DONT_USE_METER
      MCW_set_meter( meter , 100 ) ;
#endif

      /* if necessary, make the new dataset now */

      if( new_dset != NULL ){
         oldbrik = DSET_NVALS(new_dset) ;  /* number of bricks it has now */
      } else {
         oldbrik = 0 ;

         new_dset = EDIT_empty_copy( dset_time ) ;

         EDIT_dset_items( new_dset ,
                             ADN_prefix      , new_prefix ,
                             ADN_malloc_type , DATABLOCK_MEM_MALLOC ,
                             ADN_type        , ISHEAD(dset_time)
                                               ? HEAD_FUNC_TYPE : GEN_FUNC_TYPE ,
                             ADN_func_type   , FUNC_BUCK_TYPE ,
                             ADN_nvals       , newbrik ,
                             ADN_datum_all   , MRI_short ,
                             ADN_ntt         , 0 ,
                          ADN_none ) ;
      }

      /* for each output brick:
           make short space for it,
           scale and stored floats into this space ,
           attach it to the output dataset as a new brick */

      for( iv=0 ; iv < newbrik ; iv++ ){
         tsar   = vbr[iv] ;              /* float data */
         topval = 0.0 ;                  /* find range of data */
         for( it=0 ; it < nvox ; it++ )
            if( fabs(tsar[it]) > topval ) topval = fabs(tsar[it]) ;

         sar = (short *) calloc(sizeof(short),nxyz) ;  /* new brick */

         if( topval > 0.0 ){                           /* scale to shorts */
            topval = MRI_TYPE_maxval[MRI_short] / topval ;
            for( it=0 ; it < nvox ; it++ )
               sar[indx[it]] = (short)(topval * tsar[it] + 0.499) ;

            topval = 1.0/topval ;  /* scale factor */
         }

         free(tsar) ;

         if( oldbrik > 0 ){
            EDIT_add_brick( new_dset , MRI_short , topval , sar ) ;
         } else {
            mri_fix_data_pointer( sar , DSET_BRICK(new_dset,iv) ) ;
            EDIT_BRICK_FACTOR( new_dset , iv , topval ) ;
         }
      }
      free(vbr) ;

      /* do the ending calls to user_func */

      for( uu=0 ; uu < nuse ; uu++ )
         ufunc[uu]( -(brik1[uu]+oldbrik) , NULL ,
                    udata[uu] , nbrik[uu] , (void *) new_dset ) ;

   } /* 01 Feb 2000: end of user_func addition to FIMming */

final_exit:
   free(vval) ; free(indx) ;  /* can finally free these */

   /*--- Return new dataset ---*/

#ifndef DONT_USE_METER
   MCW_popdown_meter(meter) ;
#endif

   RETURN(new_dset) ;
}
