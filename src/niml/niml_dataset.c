#include "niml_private.h"

/*-----------------------------------------------------------*/
/*! Transpose a dataset, so that rows are columns and vice-
    versa.
     - Requires that all vectors (columns) have the same type
     - Requires that all vectors do NOT have statistical
       distribution types attached.
     - Returns pointer to a new dataset (NULL for errors).
-------------------------------------------------------------*/

void * NI_dataset_transpose( void *ndd )
{
   NI_dataset *ndnew , *nd = (NI_dataset *)ndd ;
   NI_index_t ii,jj , nvec_old,nvec_new , len_old,len_new ;
   int tt , lt ;

   if( nd       == NULL            ||
       nd->type != NI_DATASET_TYPE ||
       nd->vec  == NULL              ) return NULL ;  /* bad input */

   /* check if all columns have same type, etc. */

   nvec_old = NI_dataset_vecnum(nd) ;
   len_old  = NI_dataset_veclen(nd) ;

   if( nvec_old <= 0 || len_old <= 0 ) return NULL ;
   if( nd->vec[0]            == NULL ) return NULL ;
   if( nd->vec[0]->statistic != NULL ) return NULL ;

   tt = nd->vec[0]->vec_typ ;
   lt = NI_datatype_size(tt) ;
   for( ii=1 ; ii < nvec_old ; ii++ ){
     if( nd->vec[ii]            == NULL ) return NULL ;
     if( nd->vec[ii]->vec_typ   != tt   ) return NULL ;
     if( nd->vec[ii]->statistic != NULL ) return NULL ;
   }

   /* create output struct */

   ndnew = NI_new(NI_dataset) ;
   COPY_BASIC_STRUCT(ndnew,nd) ;
   ndnew->num_node = nd->num_node ;
   ndnew->num_val  = nd->num_val  ;
   ndnew->order    = NI_opposite_order(nd->order) ;   /* flipped */
   ndnew->domain   = NI_pointto_struct(nd->domain) ;  /* same domain */

   /* create new vectors */

   nvec_new = NI_dataset_vecnum(ndnew) ;
   len_new  = NI_dataset_veclen(ndnew) ;

   ndnew->vec = NI_malloc( sizeof(NI_vector *) * nvec_new ) ;
   for( ii=0 ; ii < nvec_new ; ii++ )
     ndnew->vec[ii] = NI_new_vector( tt , len_new ) ;

   /* copy data from old vectors to new vectors */

   if( tt != NI_STRING ){                 /* copy fixed length content */

     char *vnew , *vold ;
     for( ii=0 ; ii < nvec_new ; ii++ ){
       vnew = (char *)ndnew->vec[ii]->vec ;
       for( jj=0 ; jj < nvec_old ; jj++ ){
         vold = (char *)nd->vec[jj]->vec ;
         memcpy( vnew+lt*jj , vold+lt*ii , lt ) ;
       }
     }

     for( ii=0 ; ii < nvec_new ; ii++ )
       NI_set_vector_range( ndnew->vec[ii] ) ;

   } else {                                 /* duplicate strings */

     char **vnew , **vold ;
     for( ii=0 ; ii < nvec_new ; ii++ ){
       vnew = (char **)ndnew->vec[ii]->vec ;
       for( jj=0 ; jj < nvec_old ; jj++ ){
         vold = (char **)nd->vec[jj]->vec ;
         vnew[jj] = NI_strdup( vold[ii] ) ;
       }
     }

   }

   /** done **/

   return (void *)ndnew ;
}
