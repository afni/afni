#include "mrilib.h"

/*---------------------------------------------------------------------------*/
/*! Open a dataset that is an impromptu catenation of multiple dataset.      */
/*---------------------------------------------------------------------------*/

THD_3dim_dataset * THD_open_tcat( char *dlist )
{
   THD_3dim_dataset *dset_out , **dset_in ;
   int ndset_in , dd , nerr , new_nvals ;
   NI_str_array *sar ;

ENTRY("THD_open_tcat") ;

   if( dlist == NULL || *dlist == '\0' ) RETURN(NULL) ;

   if( strchr(dlist,' ') == NULL ){
     dset_out = THD_open_dataset(dlist) ; RETURN(dset_out) ;
   }

   sar = NI_decode_string_list( dlist , "~" ) ;
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

   for( nerr=0,dd=1 ; dd < ndset_in ; dd++ ){
     if( DSET_NX(dset_in[0]) != DSET_NX(dset_in[dd]) ||
         DSET_NY(dset_in[0]) != DSET_NY(dset_in[dd]) ||
         DSET_NZ(dset_in[0]) != DSET_NZ(dset_in[dd])   ){
       fprintf(stderr,
               "** THD_open_tcat: %s [%dx%dx%d] doesn't match %s [%dx%dx%d]\n",
               sar->str[0] ,DSET_NX(dset_in[0]) ,
                            DSET_NY(dset_in[0]) ,DSET_NZ(dset_in[0]) ,
               sar->str[dd],DSET_NX(dset_in[dd]),
                            DSET_NY(dset_in[dd]),DSET_NZ(dset_in[dd]) ) ;
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

   /*-- OK, start making new dataset --*/

   new_nvals = 0 ;
   for( dd=0 ; dd < ndset_in ; dd++ )
     new_nvals += DSET_NVALS(dset_in[dd]) ;

   for( dd=0 ; dd < ndset_in ; dd++ )
      if( DSET_TIMESTEP(dset_in[dd]) > 0.0 ) break ;  /* 1st 3D+time */
   if( dd == ndset_in ) dd = 0 ;

   dset_out = EDIT_empty_copy( dset_in[dd] ) ;

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

   dset_out->tcat_list = strdup( dlist ) ;
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
       ivout++ ;
     }
     DSET_delete(dset_in) ;
   }

   NI_delete_str_array(sar) ; EXRETURN ;
}
