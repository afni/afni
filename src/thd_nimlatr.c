#include "mrilib.h"
#include "thd.h"
#include "niml.h"

/*---------------------------------------------------------------------*/
/*! Write all the attributes for a datablock into a set of NIML data
    elements, stored in a NIML group element.
-----------------------------------------------------------------------*/

NI_group * THD_nimlize_dsetatr( THD_3dim_dataset *dset )
{
   THD_datablock *blk ;
   ATR_any *atr_any ;
   NI_element *nel ;
   int ia , ii ;
   NI_group *ngr = NULL ;   /* will be output */

ENTRY("THD_nimlize_dsetatr") ;

   /*--- sanity checks ---*/

   if( !ISVALID_DSET(dset) ) RETURN(ngr) ;
   blk = dset->dblk ;
   if( blk == NULL || blk->natr == 0 || blk->atr == NULL ) RETURN(ngr) ;

   /* create empty output group */

   ngr = NI_new_group_element() ;

   /* make a data element for each attribute ... */

   for( ia=0 ; ia < blk->natr ; ia++ ){

      atr_any = &(blk->atr[ia]) ;
      if( atr_any == NULL ) continue ;   /* bad attribute */

      switch( atr_any->type ){

         case ATR_FLOAT_TYPE:{
            ATR_float *atr_flo = (ATR_float *) atr_any ;

            nel = NI_new_data_element( atr_flo->name , atr_flo->nfl ) ;
            NI_set_attribute( nel , "AFNI_atr" , "float" ) ;
            NI_add_column( nel , NI_FLOAT , atr_flo->fl ) ;
            NI_add_to_group( ngr , nel ) ;
         }
         break ;

         case ATR_INT_TYPE:{
            ATR_int *atr_int = (ATR_int *) atr_any ;

            nel = NI_new_data_element( atr_int->name , atr_int->nin ) ;
            NI_set_attribute( nel , "AFNI_atr" , "int" ) ;
            NI_add_column( nel , NI_INT , atr_int->in ) ;
            NI_add_to_group( ngr , nel ) ;
         }
         break ;

         case ATR_STRING_TYPE:{
            ATR_string *atr_str = (ATR_string *) atr_any ;
            char *str ;  /* create string to hold all data to send */

            nel = NI_new_data_element( atr_str->name , 1 ) ;
            NI_set_attribute( nel , "AFNI_atr" , "string" ) ;

            str = malloc( atr_str->nch + 4 ) ;           /* convert from */
            memcpy( str , atr_str->ch , atr_str->nch ) ; /* char array   */
            THD_zblock( atr_str->nch , str ) ;           /* to C string  */
            str[ atr_str->nch ] = '\0' ;

            NI_add_column( nel , NI_STRING , &str ) ;
            NI_add_to_group( ngr , nel ) ;

            free((void *)str) ;
         }
         break ;

      } /* end of switch on atr type */

   } /* end of loop over all atr's */

   /*--- done ---*/

   RETURN(ngr) ;
}

/*---------------------------------------------------------------------*/
/*! Given a NIML group element, read AFNI attribute elements from it
    and load these into a datablock.
-----------------------------------------------------------------------*/

void THD_dsetatr_from_niml( NI_group *ngr , THD_3dim_dataset *dset )
{
   THD_datablock *blk ;
   ATR_any       *atr ;
   NI_element    *nel ;
   int            ip  ;

ENTRY("THD_dsetatr_from_niml") ;

   if( ngr                  == NULL          ||
       NI_element_type(ngr) != NI_GROUP_TYPE ||
       dset                 == NULL          ||
       dset->dblk           == NULL            ) EXRETURN ;

   blk = dset->dblk ;

   for( ip=0 ; ip < ngr->part_num ; ip++ ){

     switch( ngr->part_typ[ip] ){

       /*--- a sub-group ==> recursion! ---*/

       case NI_GROUP_TYPE:
         THD_dsetatr_from_niml( (NI_group *)ngr->part[ip] , dset ) ;
       break ;

       /*- data ==> see if is marked as an AFNI_atr and has exactly 1 column
                    if so, then extract that column and load into datablock  -*/

       case NI_ELEMENT_TYPE:{ /* data ==> see if is an AFNI attribute */
         NI_element *nel = (NI_element *)ngr->part[ip] ;
         char       *rhs = NI_get_attribute( nel , "AFNI_atr" ) ;

         if( nel->vec_num == 1 && nel->vec_len > 0 && rhs != NULL ){
           switch( nel->vec_typ[0] ){
             case NI_FLOAT:
               THD_set_float_atr( blk , nel->name ,
                                  nel->vec_len , (float *)nel->vec[0] ) ;
             break ;

             case NI_INT:
               THD_set_int_atr( blk , nel->name ,
                                nel->vec_len , (int *)nel->vec[0] ) ;
             break ;

             case NI_STRING:{
               char **sar = (char **)nel->vec[0] , *str ;
               int nch ;
               str = strdup(sar[0]) ; nch = strlen(str) ;
               THD_unzblock( nch+1 , str ) ;
               THD_set_char_atr( blk , nel->name , nch+1 , str ) ;
               free(str) ;
             }
             break ;
           }
         }
       }
       break ;
     }
   }

   EXRETURN ;
}
