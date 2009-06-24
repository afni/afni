/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*---------------------------------------------------------------------------*/
/*
  This program calculates a functional image from an AFNI 3D+time data set, 
  by calculating the correlation between the image time series and one or
  more reference ("ideal") time series.
  
  File:     3dfim.c
  Date:     06 September 1996

  Incorporated "ort" time series into 3dfim program.
  BDW       12 December 1996
 
  Added -percent option for calculating relative effect of reference waveform
  upon observed time series.
  BDW       19 May 1997

  Corrected reference to "ort" time series data structure.
  BDW       05 Sept 1997

  Print a more explicit error message when ideal or "ort" time series are not 
  of sufficient length.
  BDW       14 January 1998

  Mod:     Added changes for incorporating History notes.
  Date:    10 September 1999

  Mod:     Added call to AFNI_logger.
  Date:    15 August 2001

*/

/*---------------------------------------------------------------------------*/

#define PROGRAM_NAME "3dfim"                         /* name of this program */
#define PROGRAM_AUTHOR "R. W. Cox and B. D. Ward"          /* program author */
#define PROGRAM_INITIAL "06 Sept 1996"    /* date of initial program release */
#define PROGRAM_LATEST  "15 August 2001"  /* date of latest program revision */

/*---------------------------------------------------------------------------*/

#define SO_BIG 33333

#include "afni.h"
#include "mrilib.h"
#include "ts.h"
#include "afni_pcor.c"

/*-------------------------------------------------------------------
   Lots of CPU work in here!
---------------------------------------------------------------------*/

#undef  FIM_THR
#define FIM_THR  0.0999


THD_3dim_dataset * fim3d_fimmer_compute ( THD_3dim_dataset * dset_time ,
   time_series_array * ref_ts , time_series_array * ort_ts , 
   int itbot, char * new_prefix, 
   float max_percent        /* 19 May 1997 */ ) 
{
   THD_3dim_dataset * new_dset ;
   int ifim , it,iv , nvox , ngood_ref , ntime , it1 , dtyp , nxyz;
   float * vval , * tsar , * aval , * rbest , * abest ;
   int   * indx ;
   short * bar ;
   void  * ptr ;
   float stataux[MAX_STAT_AUX];
   float fthr , topval ;
   int nx_ref , ny_ref , ivec , nnow ;
   PCOR_references ** pc_ref ;
   PCOR_voxel_corr ** pc_vc ;
   int save_resam ;

   int fim_nref , nx_ort , ny_ort , internal_ort ;    /* 10 Dec 1996 */
   static float * ref_vec = NULL ;
   static int    nref_vec = -666 ;

   float * ref_ts_min = NULL, 
         * ref_ts_max = NULL, 
         * baseline   = NULL;      /* 19 May 1997 */

   int i;
   
   int nupdt      = 0 ,  /* number of updates done yet */
       min_updt   = 5 ;  /* min number needed for display */


   /*--- check for legal inputs ---*/      /* 14 Jan 1998 */

   if (!DSET_GRAPHABLE(dset_time)) 
     {
       fprintf (stderr, "Error:  Invalid 3d+time input data file \n");
       RETURN (NULL);
     }
   
   if (ref_ts == NULL)
     {
       fprintf (stderr, "Error:  No ideal time series \n");
       RETURN (NULL);
     }

   for (i = 0;  i < ref_ts->num;  i++)
     if (ref_ts->tsarr[i]->len < DSET_NUM_TIMES(dset_time))
       { 
	 fprintf (stderr,
	   "Error:  ideal time series is too short: ntime=%d num_ts=%d \n",
		  DSET_NUM_TIMES(dset_time), 
		  ref_ts->tsarr[i]->len);
	 RETURN (NULL) ;
       }


   /** 10 Dec 1996: allow for orts **/

   if( ort_ts->num > 0 )      /** 05 Sept 1997 **/
     {
       internal_ort = 0;
       ny_ort = ort_ts->num;
       for (i = 0;  i < ny_ort;  i++)
	 {
	   nx_ort = ort_ts->tsarr[i]->len ;
	   if (nx_ort < DSET_NUM_TIMES(dset_time))   /* 14 Jan 1998 */
	     { 
	       fprintf (stderr,
		 "Error:  ort time series is too short: ntime=%d ort_ts=%d \n",
			DSET_NUM_TIMES(dset_time), 
			ort_ts->tsarr[i]->len);
	       RETURN (NULL) ;
	     }	   
	 }
     } 
   else 
     {
       internal_ort = 1 ;
     }
   fim_nref = (internal_ort) ? 3 : (ny_ort+3) ;

   if( nref_vec < fim_nref )
     {
       ref_vec = (float *) malloc (sizeof(float)*fim_nref) ;
       nref_vec = fim_nref;
     }


   /* arrays to store maximum change in the ideal time series */
   if (max_percent > 0.0)    /* 19 May 1997 */
     {
       ref_ts_max = (float *) malloc (sizeof(float) * (ref_ts->num));
       ref_ts_min = (float *) malloc (sizeof(float) * (ref_ts->num));
     }


   nx_ref    = ref_ts->tsarr[0]->len;
   ny_ref    = ref_ts->num;
   ntime     = DSET_NUM_TIMES(dset_time) ;
   ngood_ref = 0 ;
   it1      = -1 ;
   for( ivec=0 ; ivec < ny_ref ; ivec++ ){
      tsar = ref_ts->tsarr[ivec]->ts;
      ifim = 0 ;

      if (max_percent > 0.0)       /* 19 May 1997 */
	{
	  ref_ts_min[ivec] = (float) SO_BIG;              
	  ref_ts_max[ivec] = - (float) SO_BIG;
	}

      for( it=itbot ; it < ntime ; it++ )
	{
         if( tsar[it] < SO_BIG )
	   { 
	     ifim++ ; 
	     if( it1 < 0 ) it1 = it ;

	     if (max_percent > 0.0)      /* 19 May 1997 */
	       {
		 if (tsar[it] > ref_ts_max[ivec])  ref_ts_max[ivec] = tsar[it];
		 if (tsar[it] < ref_ts_min[ivec])  ref_ts_min[ivec] = tsar[it];
	       }
	   }
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


#ifdef AFNI_DEBUG
{ char str[256] ;
  sprintf(str,"new prefix = %s",new_prefix) ; STATUS(str) ; }
#endif

   /*--- FIM: find values above threshold to fim ---*/

   DSET_load(dset_time); CHECK_LOAD_ERROR(dset_time);

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
         for( iv=0,nvox=0 ; iv < nxyz ; iv++ )
            if( abs(dar[iv]) > fthr ) nvox++ ;
         indx = (int *) malloc( sizeof(int) * nvox ) ;
         if( indx == NULL ){
            fprintf(stderr,"\n*** indx malloc failure in fim3d_fimmer_compute\n") ;
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
         for( iv=0,nvox=0 ; iv < nxyz ; iv++ )
            if( fabs(dar[iv]) > fthr ) nvox++ ;
         indx = (int *) malloc( sizeof(int) * nvox ) ;
         if( indx == NULL ){
            fprintf(stderr,"\n*** indx malloc failure in fim3d_fimmer_compute\n") ;
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
         for( iv=0,nvox=0 ; iv < nxyz ; iv++ )
            if( dar[iv] > fthr ) nvox++ ;
         indx = (int *) malloc( sizeof(int) * nvox ) ;
         if( indx == NULL ){
            fprintf(stderr,"\n*** indx malloc failure in fim3d_fimmer_compute\n") ;
            RETURN(NULL) ;
         }
         for( iv=0,nvox=0 ; iv < nxyz ; iv++ )
            if( dar[iv] > fthr ) indx[nvox++] = iv ;
      }
      break ;
   }

   /** allocate space for voxel values **/

   vval = (float *) malloc( sizeof(float) * nvox) ;
   if( vval == NULL ){
      fprintf(stderr,"\n*** vval malloc failure in fim3d_fimmer_compute\n") ;
      free(indx) ; RETURN(NULL) ;
   }

  
   /*----- allocate space for baseline values -----*/
   if (max_percent > 0.0)    /* 19 May 1997 */
     {
       baseline = (float *) malloc (sizeof(float) * nvox);
       if (baseline == NULL)
	 {
	   fprintf(stderr,
		   "\n*** baseline malloc failure in fim3d_fimmer_compute\n") ;
	   free(indx) ; free(vval); RETURN(NULL) ;
	 }
       else  /* initialize baseline values to zero */
	 for (iv = 0;  iv < nvox;  iv++)
	   baseline[iv] = 0.0;
     } 


   /** allocate extra space for comparing results from multiple ref vectors **/

   if( ny_ref > 1 ){
      aval  = (float *) malloc( sizeof(float) * nvox) ;
      rbest = (float *) malloc( sizeof(float) * nvox) ;
      abest = (float *) malloc( sizeof(float) * nvox) ;
      if( aval==NULL || rbest==NULL || abest==NULL ){
         fprintf(stderr,"\n*** abest malloc failure in fim3d_fimmer_compute\n") ;
         free(vval) ; free(indx) ;
         if( aval  != NULL ) free(aval) ;
         if( rbest != NULL ) free(rbest) ;
         if( abest != NULL ) free(abest) ;
         RETURN(NULL) ;
      }
   } else {
      aval = rbest = abest = NULL ;
   }

#ifdef AFNI_DEBUG
{ char str[256] ;
  sprintf(str,"nxyz = %d  nvox = %d",nxyz,nvox) ; STATUS(str) ; }
#endif

   /*--- FIM: initialize recursive updates ---*/

   pc_ref = (PCOR_references **) malloc( sizeof(PCOR_references *) * ny_ref ) ;
   pc_vc  = (PCOR_voxel_corr **) malloc( sizeof(PCOR_voxel_corr *) * ny_ref ) ;

   if( pc_ref == NULL || pc_vc == NULL ){
      free(vval) ; free(indx) ; free(pc_ref) ; free(pc_vc) ;
      if( aval  != NULL ) free(aval) ;
      if( rbest != NULL ) free(rbest) ;
      if( abest != NULL ) free(abest) ;
      fprintf(stderr,"\n*** FIM initialization fails in fim3d_fimmer_compute\n") ;
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
      fprintf(stderr,"\n*** FIM initialization fails in fim3d_fimmer_compute\n") ;
      RETURN(NULL) ;
   }

   /*--- Make a new dataset to hold the output ---*/

   new_dset = EDIT_empty_copy( dset_time ) ;

   it = EDIT_dset_items( new_dset ,
                            ADN_prefix      , new_prefix ,
                            ADN_malloc_type , DATABLOCK_MEM_MALLOC ,
                            ADN_type        , ISHEAD(dset_time)
                                              ? HEAD_FUNC_TYPE : GEN_FUNC_TYPE ,
                            ADN_func_type   , FUNC_COR_TYPE ,
                            ADN_nvals       , FUNC_nvals[FUNC_COR_TYPE] ,
                            ADN_datum_all   , MRI_short ,
                            ADN_ntt         , 0 ,
                         ADN_none ) ;

   if( it > 0 ){
      fprintf(stderr,
              "\n*** EDIT_dset_items error %d in fim3d_fimmer_compute\n",it) ;
      THD_delete_3dim_dataset( new_dset , False ) ;
      for( ivec=0 ; ivec < ny_ref ; ivec++ ){
         free_PCOR_references(pc_ref[ivec]) ;
         free_PCOR_voxel_corr(pc_vc[ivec]) ;
      }
      free(vval) ; free(indx) ; free(pc_ref) ; free(pc_vc) ;
      if( aval  != NULL ) free(aval) ;
      if( rbest != NULL ) free(rbest) ;
      if( abest != NULL ) free(abest) ;
      RETURN(NULL) ;
   }

   for( iv=0 ; iv < new_dset->dblk->nvals ; iv++ ){
      ptr = malloc( DSET_BRICK_BYTES(new_dset,iv) ) ;
      mri_fix_data_pointer( ptr ,  DSET_BRICK(new_dset,iv) ) ;
   }

   if( THD_count_databricks(new_dset->dblk) < new_dset->dblk->nvals ){
      fprintf(stderr,
              "\n*** failure to malloc new bricks in fim3d_fimmer_compute\n") ;
      THD_delete_3dim_dataset( new_dset , False ) ;
      for( ivec=0 ; ivec < ny_ref ; ivec++ ){
         free_PCOR_references(pc_ref[ivec]) ;
         free_PCOR_voxel_corr(pc_vc[ivec]) ;
      }
      free(vval) ; free(indx) ; free(pc_ref) ; free(pc_vc) ;
      if( aval  != NULL ) free(aval) ;
      if( rbest != NULL ) free(rbest) ;
      if( abest != NULL ) free(abest) ;
      RETURN(NULL) ;
   }


   /*--- FIM: do recursive updates ---*/

   for( it=itbot ; it < ntime ; it++ ){

      nnow = 0 ;
      for( ivec=0 ; ivec < ny_ref ; ivec++ ){
         tsar = ref_ts->tsarr[ivec]->ts ;
         if( tsar[it] >= SO_BIG ) continue ;  /* skip this */

         ref_vec[0] = 1.0 ;         /* we always supply orts */
         ref_vec[1] = (float) it ;  /* for mean and linear trend */

         if (internal_ort)          /* 10 Dec 1996 */
	   {
	     ref_vec[2] = tsar[it] ;
	   } 
	 else 
	   {
	     for( iv=0 ; iv < ny_ort ; iv++ )
               ref_vec[iv+2] = ort_ts->tsarr[iv]->ts[it];
	     ref_vec[ny_ort+2] = tsar[it] ;
	   }


#ifdef AFNI_DEBUG
{ char str[256] ;
  sprintf(str,"time index=%d  ideal[%d]=%f" , it,ivec,tsar[it] ) ;
  if (ivec == 0) STATUS(str) ; }
#endif


         update_PCOR_references( ref_vec , pc_ref[ivec] ) ;

         switch( dtyp ){
            case MRI_short:{
               short * dar = (short *) DSET_ARRAY(dset_time,it) ;
               for( iv=0 ; iv < nvox ; iv++ ) vval[iv] = (float) dar[indx[iv]] ;
            }
            break ;

            case MRI_float:{
               float * dar = (float *) DSET_ARRAY(dset_time,it) ;
               for( iv=0 ; iv < nvox ; iv++ ) vval[iv] = (float) dar[indx[iv]] ;
            }
            break ;

            case MRI_byte:{
               byte * dar = (byte *) DSET_ARRAY(dset_time,it) ;
               for( iv=0 ; iv < nvox ; iv++ ) vval[iv] = (float) dar[indx[iv]] ;
            }
            break ;
         }

         PCOR_update_float( vval , pc_ref[ivec] , pc_vc[ivec] ) ;
         nnow++ ;

	 /*----- update baseline value calculation -----*/
	 if (max_percent > 0.0)    /* 19 May 1997 */
	   if (ivec == 0)
	     for (iv = 0;  iv < nvox;  iv++)
	       baseline[iv] += vval[iv] / ngood_ref;
 
      }
      if( nnow > 0 ) nupdt++ ;


      /*--- Load results into the dataset and redisplay it ---*/

      if( nupdt == ngood_ref ) 
      {
         /*--- set the statistical parameters ---*/

         stataux[0] = nupdt ;               /* number of points used */
         stataux[1] = (ny_ref==1) ? 1 : 2 ; /* number of references  */
         stataux[2] = fim_nref - 1 ;     /* number of orts */  /* 12 Dec 96 */
         for( iv=3 ; iv < MAX_STAT_AUX ; iv++ ) stataux[iv] = 0.0 ;

STATUS("setting statistical parameters") ;

         (void) EDIT_dset_items( new_dset ,
                                    ADN_stat_aux , stataux ,
                                 ADN_none ) ;

         /*** Compute brick arrays for new dataset ***/

         if( ny_ref == 1 ){

         /*** Just 1 ref vector --> load values directly into dataset ***/

            /*--- get alpha (coef) into vval,
                  find max value, scale into brick array ---*/

STATUS("getting 1 ref alpha") ;

            PCOR_get_coef( pc_ref[0] , pc_vc[0] , vval ) ;

	    /*--- replace alpha with percentage change, if so requested ---*/
	    if (max_percent > 0.0)    /* 19 May 1997 */
	      {
		for (iv = 0;  iv < nvox;  iv++)
		  {
		    vval[iv] *= 100.0 * (ref_ts_max[0] - ref_ts_min[0]);
		    if (fabs(vval[iv]) < max_percent * fabs(baseline[iv]))
		      vval[iv] = fabs( vval[iv] / baseline[iv] );
		    else
		      vval[iv] = max_percent;
		  }
		topval = max_percent;
	      }
	    else 
	      {
		topval = 0.0 ;
		for( iv=0 ; iv < nvox ; iv++ )
		  if( fabs(vval[iv]) > topval ) topval = fabs(vval[iv]) ;
	      }

            bar = DSET_ARRAY( new_dset , FUNC_ival_fim[FUNC_COR_TYPE] ) ;
            memset( bar , 0 , sizeof(short)*nxyz ) ;

            if( topval > 0.0 ){
               topval = MRI_TYPE_maxval[MRI_short] / topval ;
               for( iv=0 ; iv < nvox ; iv++ )
                  bar[indx[iv]] = (short)(topval * vval[iv] + 0.499) ;

               stataux[0] = 1.0/topval ;
            } else {
               stataux[0] = 0.0 ;
            }

            /*--- get correlation coefficient (pcor) into vval,
                  scale into brick array (with fixed scaling factor) ---*/

STATUS("getting 1 ref pcor") ;

            PCOR_get_pcor( pc_ref[0] , pc_vc[0] , vval ) ;

            bar = DSET_ARRAY( new_dset , FUNC_ival_thr[FUNC_COR_TYPE] ) ;
            memset( bar , 0 , sizeof(short)*nxyz ) ;

            for( iv=0 ; iv < nvox ; iv++ )
               bar[indx[iv]] = (short)(FUNC_COR_SCALE_SHORT * vval[iv] + 0.499) ;

            stataux[1] = 1.0 / FUNC_COR_SCALE_SHORT ;

         } else {

         /*** Multiple references --> find best correlation at each voxel ***/

            /*--- get first ref results into abest and rbest (best so far) ---*/

            PCOR_get_coef( pc_ref[0] , pc_vc[0] , abest ) ;

	    /*--- modify alpha for percentage change calculation ---*/
	    if (max_percent > 0.0)    /* 19 May 1997 */
	      for (iv = 0;  iv < nvox;  iv++)
		abest[iv] *= 100.0 * (ref_ts_max[0] - ref_ts_min[0]);	       
	      
            PCOR_get_pcor( pc_ref[0] , pc_vc[0] , rbest ) ;

            /*--- for each succeeding ref vector,
                  get results into aval and vval,
                  if |vval| > |rbest|, then use that result instead ---*/

            for( ivec=1 ; ivec < ny_ref ; ivec++ ){

               PCOR_get_coef( pc_ref[ivec] , pc_vc[ivec] , aval ) ;

               PCOR_get_pcor( pc_ref[ivec] , pc_vc[ivec] , vval ) ;

               for( iv=0 ; iv < nvox ; iv++ ){
                  if( fabs(vval[iv]) > fabs(rbest[iv]) ){
                     rbest[iv] = vval[iv] ;
                     abest[iv] = aval[iv] ;

		     /*--- modify alpha for percentage change calculation ---*/
		     if (max_percent > 0.0)    /* 19 May 1997 */
		       abest[iv] *= 100.0 *
			 (ref_ts_max[ivec] - ref_ts_min[ivec]);

                  }
               }

            }

            /*--- at this point, abest and rbest are the best
                  results, so scale them into the dataset bricks ---*/

	    /*--- finish percentage change calculation, if so requested ---*/
	    if (max_percent > 0.0)    /* 19 May 1997 */
	      {
		for (iv = 0;  iv < nvox;  iv++)
		  {
		    if (fabs(abest[iv]) < max_percent * fabs(baseline[iv]))
		      abest[iv] = fabs( abest[iv] / baseline[iv] );
		    else
		      abest[iv] = max_percent;
		  }
		topval = max_percent;
	      }
	    else
	      {
		topval = 0.0 ;
		for( iv=0 ; iv < nvox ; iv++ )
		  if( fabs(abest[iv]) > topval ) topval = fabs(abest[iv]) ;
	      }

            bar = DSET_ARRAY( new_dset , FUNC_ival_fim[FUNC_COR_TYPE] ) ;
            memset( bar , 0 , sizeof(short)*nxyz ) ;

            if( topval > 0.0 ){
               topval = MRI_TYPE_maxval[MRI_short] / topval ;
               for( iv=0 ; iv < nvox ; iv++ )
                  bar[indx[iv]] = (short)(topval * abest[iv] + 0.499) ;

               stataux[0] = 1.0/topval ;
            } else {
               stataux[0] = 0.0 ;
            }

            bar = DSET_ARRAY( new_dset , FUNC_ival_thr[FUNC_COR_TYPE] ) ;
            memset( bar , 0 , sizeof(short)*nxyz ) ;

            for( iv=0 ; iv < nvox ; iv++ )
               bar[indx[iv]] = (short)(FUNC_COR_SCALE_SHORT * rbest[iv] + 0.499) ;

            stataux[1] = 1.0 / FUNC_COR_SCALE_SHORT ;

         }

STATUS("setting brick_fac") ;

         (void) EDIT_dset_items( new_dset ,
                                    ADN_brick_fac , stataux ,
                                 ADN_none ) ;

      }
   }

 
   /*--- End of recursive updates; now free temporary workspaces ---*/

   for( ivec=0 ; ivec < ny_ref ; ivec++ ){
      free_PCOR_references(pc_ref[ivec]) ;
      free_PCOR_voxel_corr(pc_vc[ivec]) ;
   }
   free(vval) ; free(indx) ; free(pc_ref) ; free(pc_vc) ;
   if( aval  != NULL ) free(aval) ;
   if( rbest != NULL ) free(rbest) ;
   if( abest != NULL ) free(abest) ;

   if (ref_ts_min != NULL)  free (ref_ts_min);    /* 19 May 1997 */
   if (ref_ts_max != NULL)  free (ref_ts_max);
   if (baseline != NULL)    free (baseline);


   /* --- load the statistics --- */
   THD_load_statistics (new_dset);
   
   /*--- Return new dataset ---*/

   RETURN(new_dset) ;
}

/*--------------------------------------------------------------------------*/

/** structure type to hold results from scanning the command line options **/

typedef struct 
{
   char prefix_name[THD_MAX_NAME];    /* filename root for output */
   THD_3dim_dataset * dset;           /* input 3d+time data set */
   time_series_array * idts;          /* array of ideal time series */
   time_series_array * ortts;         /* array of ort time series */ 
   int first_im;                      /* first time series point to be used */ 
   float max_percent;                 /* max. percentage change due to ideal
                                         time series,  19 May 1997 */
} line_opt ;


/*** internal prototype ***/

void Syntax( char * ) ;

/*--------------------------------------------------------------------------*/

/*** read and interpret command line arguments ***/


#define OPENERS "[{/%"
#define CLOSERS "]}/%"

#define IS_OPENER(sss) (strlen((sss))==1 && strstr(OPENERS,(sss))!=NULL)
#define IS_CLOSER(sss) (strlen((sss))==1 && strstr(CLOSERS,(sss))!=NULL)


void get_line_opt( int argc , char *argv[] , line_opt *opt )
{
  
   int nopt ;
   float ftemp ;
   MRI_IMAGE *im ;
   time_series *ideal;
   time_series *ort;
   char err_note[128];
   Boolean ok;
   char input_filename[THD_MAX_NAME];


   /* --- give help if needed --- */
   if( argc < 2 || strncmp(argv[1],"-help",4) == 0 ) Syntax(NULL) ;
  
   /*----- add to program log -----*/
   AFNI_logger (PROGRAM_NAME,argc,argv); 

   /* --- setup opt with defaults --- */
   strcpy (opt->prefix_name, "");   /* no prefix name yet */
   opt->first_im = 0 ;              /* first image to actually use */
   opt->max_percent = 0.0;

   /* --- initialize array of time series data --- */
   INIT_TSARR(opt->idts) ;

   /* --- initialize array of ort time series data --- */  /* 10 Dec 1996 */
   INIT_TSARR (opt->ortts);

   /* --- set defaults in local variables --- */
   ideal    = NULL ;                /* time_series of ideal response vector */
   strcpy (input_filename, "");     /* no input file name yet */


   /* --- read command line arguments and process them:
      coding technique is to examine each argv, and if it matches
      something we expect (e.g., -ideal), process it, then skip to
      the next loop through ('continue' statements in each strncmp if).
      Note that the 'while' will increment the argument index (nopt)  --- */

   nopt = 1 ;
   
   do{ 

#ifdef DEBUG
#  define DBOPT(str) fprintf(stderr,"at option %s: %s\n",argv[nopt],str)
#else
#  define DBOPT(str) /* nothing */
#endif

      /* ---  -im1 #  ==>  index (1...) of first image to actually use  --- */
      if( strncmp(argv[nopt],"-im1",4) == 0 )
      {
         DBOPT("-im1") ;
         if( ++nopt >= argc ) Syntax("-im1 needs an argument") ;
         ftemp = strtod( argv[nopt] , NULL ) ;
         if( ftemp < 1.0 )
         {
            sprintf( err_note , "illegal -im1 value: %f" , ftemp ) ;
            Syntax(err_note) ;
         }
         opt->first_im = (int)(ftemp+0.499) - 1;
         continue ;
      }


      /* --- -ideal file  ==>  ideal response vector time series --- */
      if( strncmp(argv[nopt],"-ideal",5) == 0 )
      {
         DBOPT("-ideal") ;
         if( ++nopt >= argc ) Syntax("-ideal needs a filename") ;

         /** July 1995: read a bunch of ideals using [ a b c ... ] format **/

         if( ! IS_OPENER(argv[nopt]) )
         {  /* --- one file --- */
            ideal = RWC_read_time_series( argv[nopt] ) ;
            if( ideal == NULL )  Syntax ("Unable to read ideal time series.");
            ADDTO_TSARR( opt->idts , ideal ) ;
         } 
         else 
         {  /* --- many files --- */
            for( nopt++ ; !IS_CLOSER(argv[nopt]) && nopt<argc ; nopt++ )
            {
               ideal = RWC_read_time_series( argv[nopt] ) ;
               if( ideal == NULL )  Syntax ("Unable to read ideal time series.");
               ADDTO_TSARR( opt->idts , ideal ) ;
            }
            if( nopt == argc ) Syntax("-ideal never finishes!") ;
         }
         continue ;
      }


      /*-----  -percent p   Calculate percentage change from baseline -----*/
      if( strncmp(argv[nopt],"-percent",8) == 0 )
      {
         DBOPT("-percent") ;
         if( ++nopt >= argc ) Syntax("-percent needs an argument") ;
         ftemp = strtod( argv[nopt] , NULL ) ;
         if( ftemp <= 0.0 )
         {
            sprintf( err_note , "illegal -percent value: %f" , ftemp ) ;
            Syntax(err_note) ;
         }
         opt->max_percent = ftemp;
         continue ;
      }


      /* --- -ort file  ==>  ort vector time series --- */   /* 10 Dec 1996 */
      if( strncmp(argv[nopt],"-ort",4) == 0 )
      {
         DBOPT("-ort") ;
         if( ++nopt >= argc ) Syntax("-ort needs a filename") ;

         /**  read a bunch of orts using [ a b c ... ] format **/

         if( ! IS_OPENER(argv[nopt]) )
         {  /* --- one file --- */
            ort = RWC_read_time_series( argv[nopt] ) ;
            if( ort == NULL )  Syntax ("Unable to read ort time series.");
            ADDTO_TSARR( opt->ortts , ort ) ;
         } 
         else 
         {  /* --- many files --- */
            for( nopt++ ; !IS_CLOSER(argv[nopt]) && nopt<argc ; nopt++ )
            {
               ort = RWC_read_time_series( argv[nopt] ) ;
               if( ort == NULL )  Syntax ("Unable to read ort time series.");
               ADDTO_TSARR( opt->ortts , ort ) ;
            }
            if( nopt == argc ) Syntax("-ort never finishes!") ;
         }
         continue ;
      }


      /* ---  -prefix name  ==>  prefix name of output file --- */
      if( strncmp(argv[nopt], "-prefix", 5) == 0 ){
         DBOPT("-prefix") ;
         if( ++nopt >= argc ) Syntax("-prefix needs a name") ;
         if( strcmp(opt->prefix_name, "") )  
            Syntax("Cannot have two prefix output names!" ) ;
         strcpy (opt->prefix_name, argv[nopt]) ;
         DBOPT("stored as prefix") ; 
         continue ;
      }

      /* ---  -input fname  ==>  file name of input 3d+time data --- */

      if( strncmp(argv[nopt], "-input", 5) == 0 )
      {
         DBOPT("-input") ;
         if( ++nopt >= argc ) Syntax("-input needs a name") ;
         if( strcmp(input_filename, "") )
            Syntax ("Cannot have two input names!" ) ;
         strcpy (input_filename, argv[nopt] );
         /* open 3d data set */
         opt->dset = THD_open_one_dataset( argv[nopt] ) ;
         if( opt->dset == NULL ) 
         {
            sprintf (err_note, "Unable to open 3d+time data file: %s", argv[nopt]);
            Syntax (err_note); 
         }
         continue ;
      }

      /* unidentified option */
      sprintf( err_note , "Unknown option %s" , argv[nopt] ) ;
      Syntax(err_note) ;
      
   } while( ++nopt < argc ) ;  /* loop until all args are read, or we break */


   /* --- check for valid user inputs --- */
   if (!strcmp(input_filename,""))  Syntax ("No input file name was given.");
   if( opt->idts->num == 0 ) Syntax("No ideal response vector was defined!") ; 
   if (!strcmp(opt->prefix_name,""))  Syntax ("No prefix name was specified.");


   /* --- Done! --- */

   return ;
}

/*----------------------------------------------------------------------------*/

/* --- give some help and exit --- */

void Syntax( char *note )
{
   static char *mesg[] = {
   "Program:   3dfim \n\n"
   "Purpose:   Calculate functional image from 3d+time data file. \n"
   "Usage:     3dfim  [-im1 num]  -input fname  -prefix name \n"
   "              -ideal fname  [-ideal fname] [-ort fname] \n"
   " " ,
   "options are:",
   "-im1 num        num   = index of first image to be used in time series ",
   "                        correlation; default is 1  ",
   " ",
   "-input fname    fname = filename of 3d + time data file for input",
   " " ,
   "-prefix name    name  = prefix of filename for saving functional data",
   " ",
   "-ideal fname    fname = filename of a time series to which the image data",
   "                        is to be correlated. ",
   " ",
   "-percent p      Calculate percentage change due to the ideal time series ",
   "                p     = maximum allowed percentage change from baseline ",
   "                        Note: values greater than p are set equal to p. ",
   " ",
   "-ort fname      fname = filename of a time series to which the image data",
   "                        is to be orthogonalized ",
   " ", 
   "            N.B.: It is possible to specify more than",
   "            one ideal time series file. Each one is separately correlated",
   "            with the image time series and the one most highly correlated",
   "            is selected for each pixel.  Multiple ideals are specified",
   "            using more than one '-ideal fname' option, or by using the",
   "            form '-ideal [ fname1 fname2 ... ]' -- this latter method",
   "            allows the use of wildcarded ideal filenames.",
   "            The '[' character that indicates the start of a group of",
   "            ideals can actually be any ONE of these: " OPENERS ,
   "            and the ']' that ends the group can be:  " CLOSERS ,
   " ",
   "            [Format of ideal time series files:",
   "            ASCII; one number per line;",
   "            Same number of lines as images in the time series;",
   "            Value over 33333 --> don't use this image in the analysis]",
   " ",
   "            N.B.: It is also possible to specify more than",
   "            one ort time series file.  The image time series is  ",
   "            orthogonalized to each ort time series.  Multiple orts are ",
   "            specified by using more than one '-ort fname' option, ",
   "            or by using the form '-ort [ fname1 fname2 ... ]'.  This ",
   "            latter method allows the use of wildcarded ort filenames.",
   "            The '[' character that indicates the start of a group of",
   "            ideals can actually be any ONE of these: " OPENERS ,
   "            and the ']' that ends the group can be:  " CLOSERS ,
   " ",
   "            [Format of ort time series files:",
   "            ASCII; one number per line;",
   "            At least same number of lines as images in the time series]",
   " ",
   " ",
   } ;

   int nsize , ii ;

   if( note != NULL && (nsize=strlen(note)) > 0 ){
      fprintf(stderr,"\n") ;
      for(ii=0;ii<nsize+9;ii++) fprintf(stderr,"*") ;
      fprintf(stderr,"\a\n Error: %s\n", note ) ;
      for(ii=0;ii<nsize+9;ii++) fprintf(stderr,"*") ;
      fprintf(stderr,"\nTry 3dfim -help for instructions\a\n") ;
      exit(-1) ;
   }

   for( ii=0 ; ii < sizeof(mesg)/sizeof(char *) ; ii++ ){
      printf( " %s\n" , mesg[ii] );
   }
   PRINT_COMPILE_DATE ; exit(0) ;
}


/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   line_opt  opt ;               /* holds data constructed from command line */
   THD_3dim_dataset * new_dset;  /* functional data set to be calculated */
   Boolean ok;                   /* true if 3d write is successful */
   

   /*----- Identify software -----*/
#if 0
   printf ("\n\n");
   printf ("Program: %s \n", PROGRAM_NAME);
   printf ("Author:  %s \n", PROGRAM_AUTHOR);
   printf ("Initial Release:  %s \n", PROGRAM_INITIAL);
   printf ("Latest Revision:  %s \n", PROGRAM_LATEST);
   printf ("\n");
#endif

   PRINT_VERSION("3dfim") ; AUTHOR(PROGRAM_AUTHOR) ;
   mainENTRY("3dfim main") ; machdep() ;

   /*-- 20 Apr 2001: addto the arglist, if user wants to [RWCox] --*/

   { int new_argc ; char ** new_argv ;
     addto_args( argc , argv , &new_argc , &new_argv ) ;
     if( new_argv != NULL ){ argc = new_argc ; argv = new_argv ; }
   }


   /* --- read command line --- */
   get_line_opt( argc , argv , &opt ) ;

   /* --- calculate functional image --- */
   new_dset = fim3d_fimmer_compute (opt.dset, opt.idts, opt.ortts, 
				    opt.first_im, opt.prefix_name, 
				    opt.max_percent);  /* 19 May 1997 */

   /*----- Record history of dataset -----*/
   tross_Copy_History( opt.dset , new_dset ) ;
   tross_Make_History( PROGRAM_NAME , argc , argv , new_dset ) ;
   
   /* --- write 3d functional image data --- */
   ok = THD_write_3dim_dataset ("", opt.prefix_name, new_dset, TRUE);
   if (!ok)  Syntax ("Failure to write functional data set.");
   
   /* --- cleanup --- */
   THD_delete_3dim_dataset (new_dset, FALSE);
   
   return (0);
}

/*---------------------------------------------------------------------------*/
