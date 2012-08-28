#include "mrilib.h"

/*---------------------------------------------------------------------------*/
/*! Open a dataset that is an impromptu catenation of multiple dataset.      */
/*---------------------------------------------------------------------------*/

THD_3dim_dataset * THD_open_tcat( char *dlist )
{
   THD_3dim_dataset *dset_out , **dset_in ;
   int ndset_in , dd , nerr , new_nvals, sb=0 ;
   NI_str_array *sar ;
   double angle=0.0;
   char *dp, *dlocal = dlist;   /* local dlist, in case it is altered */
   
ENTRY("THD_open_tcat") ;

   if( dlocal == NULL || *dlocal == '\0' ) RETURN(NULL) ;

   /* allow file list to be read from a file   23 Jul 2012 [rickr] */
   if( ! strncmp(dlocal, "filelist:", 9) ) {
      dlocal = AFNI_suck_file(dlocal+9) ;
      if ( ! dlocal ) {
         ERROR_message("THD_open_tcat: failed to open '%s' as filelist",
                       dlocal+9);
         RETURN(NULL) ;
      }
      /* make it look more like expected */
      for( dd=0, dp=dlocal; dd < strlen(dlocal); dd++, dp++ )
         if( *dp == '\n' || *dp  == '\r' ) *dp = ' ';
   }

   if( strchr(dlocal,' ') == NULL ){
     dset_out = THD_open_dataset(dlocal) ; RETURN(dset_out) ;
   }

   sar = NI_decode_string_list( dlocal , "~" ) ;
   if( sar == NULL ) RETURN(NULL) ;

   ndset_in = sar->num ;
   dset_in  = (THD_3dim_dataset **)malloc(sizeof(THD_3dim_dataset *)*sar->num) ;
   for( nerr=dd=0 ; dd < ndset_in ; dd++ ){
     dset_in[dd] = THD_open_dataset( sar->str[dd] ) ;
     if( dset_in[dd] == NULL ){
       fprintf(stderr,"** THD_open_tcat: can't open dataset %s\n",sar->str[dd]) ;
       nerr++ ;
     }
   }
   if( nerr > 0 ){
     for( dd=0 ; dd < ndset_in ; dd++ )
       if( dset_in[dd] != NULL ) DSET_delete(dset_in[dd]) ;
     free((void *)dset_in) ;
     NI_delete_str_array(sar) ;
     RETURN(NULL) ;
   }
   if( ndset_in == 1 ){
     dset_out = dset_in[0] ;
     free((void *)dset_in) ;
     NI_delete_str_array(sar) ;
     RETURN(dset_out) ;
   }

   (void)THD_check_for_duplicates( sar->num , sar->str , 1 ) ;  /* 31 May 2007 */

   for( nerr=0,dd=1 ; dd < ndset_in ; dd++ ){
     if( DSET_NX(dset_in[0]) != DSET_NX(dset_in[dd]) ||
         DSET_NY(dset_in[0]) != DSET_NY(dset_in[dd]) ||
         DSET_NZ(dset_in[0]) != DSET_NZ(dset_in[dd])   ){
       ERROR_message(
               "THD_open_tcat: %s [%dx%dx%d] doesn't match %s [%dx%dx%d]\n",
               sar->str[0] ,DSET_NX(dset_in[0]) ,
                            DSET_NY(dset_in[0]) ,DSET_NZ(dset_in[0]) ,
               sar->str[dd],DSET_NX(dset_in[dd]),
                            DSET_NY(dset_in[dd]),DSET_NZ(dset_in[dd]) ) ;
       nerr++ ;
     } else {
       if( !EQUIV_DATAXES(dset_in[dd]->daxes,dset_in[0]->daxes) ){
         WARNING_message(
                  "THD_open_tcat: %s grid mismatch with %s\n",
                  sar->str[0] , sar->str[dd] ) ;  /* don't increment nerr! */
       }
       angle = dset_obliquity_angle_diff(dset_in[dd], dset_in[0], -1.0);
       if (angle > 0.0) {
         WARNING_message(
            "dataset %s has an obliquity difference of %f degress with %s\n",
            dset_in[dd] ,
            angle, dset_in[0] );
       }
     }
   }
   if( nerr > 0 ){
     for( dd=0 ; dd < ndset_in ; dd++ )
       if( dset_in[dd] != NULL ) DSET_delete(dset_in[dd]) ;
     free((void *)dset_in) ;
     NI_delete_str_array(sar) ;
     RETURN(NULL) ;
   }

   /*-- Check for type problems                    ZSS: Aug 27 2012 --*/
   for (nerr=0,dd=0; dd < ndset_in ; dd++) {
      for (sb=0; sb < DSET_NVALS(dset_in[dd]); ++sb) {
         if ( DSET_BRICK_TYPE(dset_in[0],0) != 
              DSET_BRICK_TYPE(dset_in[dd],sb) ) {
            ++nerr;    
         }
      }
   }
   if (nerr > 0) { /* don't die, just complain */
      WARNING_message(
      "Command-line catenated dataset has %d sub-bricks that differ \n"
      "  in data type from the first sub-brick of the first set.\n"
      "  Mme Irma sees potential for grief if you go down that path. \n"
      "  Use 3dinfo -datum on each input to understand why this is happening.\n"
      "  You can use 3dcalc's -datum option to rewrite the dataset with \n"
      "  all sub-bricks set to the same type then start over.\n\n",
            nerr);
      nerr=0;
   }
   
   /*-- OK, start making new dataset --*/

   new_nvals = 0 ;
   for( dd=0 ; dd < ndset_in ; dd++ )
     new_nvals += DSET_NVALS(dset_in[dd]) ;

   for( dd=0 ; dd < ndset_in ; dd++ )
      if( DSET_TIMESTEP(dset_in[dd]) > 0.0 ) break ;  /* 1st 3D+time */
   if( dd == ndset_in ) dd = 0 ;

   dset_out = EDIT_empty_copy( dset_in[dd] ) ;

   /* since this is basically an input dataset, set the storage_mode
    * to match                                   27 Jul 2010 [rickr] */
   if( DSET_ONDISK(dset_out) && IS_VALID_NON_AFNI_DSET(dset_in[dd]) )
      THD_set_storage_mode(dset_out, dset_in[dd]->dblk->diskptr->storage_mode);

   EDIT_dset_items( dset_out ,
                      ADN_prefix    , "tcat" ,
                      ADN_func_type , ISANAT(dset_in[dd]) ? ANAT_EPI_TYPE
                                                          : FUNC_FIM_TYPE ,
                      ADN_ntt       , new_nvals ,
                      ADN_nvals     , new_nvals ,
                    ADN_none ) ;
   DSET_mallocize( dset_out ) ;

   /* check if we have a valid time axis; if not, make one up */

   if( DSET_TIMESTEP(dset_out) <= 0.0f ){
      float TR=1.0f , torg=0.0f , tdur=0.0f ;
      int tunits=UNITS_SEC_TYPE ;
      EDIT_dset_items( dset_out ,
                          ADN_tunits , tunits ,
                          ADN_ttdel  , TR ,
                          ADN_ttorg  , torg ,
                          ADN_ttdur  , tdur ,
                       ADN_none ) ;
   }

   dset_out->tcat_list = strdup( dlocal ) ;
   dset_out->tcat_num  = ndset_in ;
   dset_out->tcat_len  = (int *)malloc(sizeof(int)*ndset_in) ;
   for( dd=0 ; dd < ndset_in ; dd++ ){
     dset_out->tcat_len[dd] = DSET_NVALS(dset_in[dd]) ;
     DSET_delete(dset_in[dd]) ;
   }
   free((void *)dset_in) ;
   NI_delete_str_array(sar) ;

#if 0
fprintf(stderr,"THD_open_tcat('%s'):",dset_out->tcat_list);
for(dd=0;dd<ndset_in;dd++)fprintf(stderr," %d",dset_out->tcat_len[dd]);
fprintf(stderr,"\n");
#endif

   RETURN(dset_out) ;
}

/*---------------------------------------------------------------------------*/
/*! Load from disk the impromptu catenation.                                 */
/*---------------------------------------------------------------------------*/

void THD_load_tcat( THD_datablock *dblk )
{
   int ivout , dd , iv ;
   THD_3dim_dataset *dset_in , *dset_out ;
   NI_str_array *sar ;

ENTRY("THD_load_tcat") ;

   if( !ISVALID_DBLK(dblk) ) EXRETURN ;
   dset_out = (THD_3dim_dataset *)dblk->parent ;
   if( !ISVALID_DSET(dset_out) ) EXRETURN ;
   sar = NI_decode_string_list( dset_out->tcat_list , "~" ) ;
   if( sar == NULL ) EXRETURN ;
   if( sar->num != dset_out->tcat_num ){ NI_delete_str_array(sar); EXRETURN; }

   ivout = 0 ;
   for( dd=0 ; dd < sar->num ; dd++ ){
     dset_in = THD_open_dataset( sar->str[dd] ) ;
     if( dset_in == NULL ){
       NI_delete_str_array(sar) ; DSET_unload(dset_out) ;
       EXRETURN ;
     }
     DSET_mallocize(dset_in) ; DSET_load(dset_in) ;
     if( !DSET_LOADED(dset_in) ){
       NI_delete_str_array(sar) ; DSET_unload(dset_out) ; DSET_delete(dset_in) ;
       EXRETURN ;
     }

     for( iv=0 ; iv < DSET_NVALS(dset_in) ; iv++ ){
       EDIT_substitute_brick( dset_out , ivout ,
                              DSET_BRICK_TYPE(dset_in,iv), DSET_ARRAY(dset_in,iv) );
       mri_fix_data_pointer( NULL , DSET_BRICK(dset_in,iv) ) ;
       EDIT_BRICK_FACTOR( dset_out , ivout , DSET_BRICK_FACTOR(dset_in,iv) ) ;
       EDIT_BRICK_LABEL(dset_out, ivout, 
                        DSET_BRICK_LABEL(dset_in, iv)); /* ZSS Aug. 27 2012 */
       ivout++ ;
     }
     DSET_delete(dset_in) ;
   }

   NI_delete_str_array(sar) ; EXRETURN ;
}
