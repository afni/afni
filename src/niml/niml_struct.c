#include "niml_private.h"

/*-----------------------------------------------------------*/
/*! Holds the table of all registered structs. */

static Htable *ni_struct_table=NULL ;

/*-----------------------------------------------------------*/
/*! Register a struct by its idcode (if it has one). */

void NI_register_struct( void *ndd )
{
   NI_struct *nd = (NI_struct *)ndd ;
   void *vp ;

   /* can't register without idcode */

   if( nd == NULL || nd->idcode == NULL ) return ;

   /* 1st time in ==> create hash table */

   if( ni_struct_table == NULL )
     ni_struct_table = new_Htable( 1031 ) ;

   /* see if it already is registered */

   vp = findin_Htable( nd->idcode , ni_struct_table ) ;
   if( vp != NULL ) return ;  /* duplicate entry */

   /* OK, add it to the table */

   addto_Htable( nd->idcode , nd , ni_struct_table ) ;
   return ;
}

/*-----------------------------------------------------------*/
/*! Find a struct by its idcode. */

void * NI_find_struct( char *idcode )
{
   void *vp ;
   if( idcode == NULL ) return NULL ; /* nothing to do */
   vp = findin_Htable( idcode , ni_struct_table ) ;
   return vp ;
}

/*-----------------------------------------------------------*/
/*! Remove a struct from the table. */

void NI_unregister_struct( void *ndd )
{
   NI_struct *nd = (NI_struct *)ndd ;
   if( nd == NULL || nd->idcode == NULL ) return ;
   removefrom_Htable( nd->idcode , ni_struct_table ) ;
   return ;
}

/*-----------------------------------------------------------*/
/*! Return a copy of the pointer to the struct,
    also incrementing its reference counter.    */

void * NI_pointto_struct( void *ndd )
{
   NI_struct *nd = (NI_struct *)ndd ;
   if( nd == NULL ) return NULL ;
   nd->nref ++ ;
   return (void *)nd ;
}

/*-----------------------------------------------------------*/
/* This macro does the basic stuff necessary to delete a
   struct from the hash table and from memory.  It is used
   at the very end of the NI_free_struct() function.
   Type specific code is also needed to delete any memory
   used by sub-structs or sub-arrays.
-------------------------------------------------------------*/

#undef  DELETE_STRUCT
#define DELETE_STRUCT(nq)              \
 do{ NI_unregister_struct(nq);         \
     NI_free(nq->idcode) ;             \
     NI_free(nq->name)   ;             \
     NI_free(nq)         ; } while(0)

/*-----------------------------------------------------------*/
/*! Decrement the reference counter, and destroy the struct
    (recursively in some cases) if the counter goes to zero.
-------------------------------------------------------------*/

void NI_free_struct( void *ndd )
{
   NI_struct *nd = (NI_struct *)ndd ;
   if( nd == NULL ) return ;

   /* decrementation */

   nd->nref -- ;
   if( nd->nref > 0 ) return ;      /* keep it */

   /* OK, blot it from the universe */

   switch( nd->type ){              /* N.B.: there is no default */

     case NI_STRUCT_TYPE:         /* These types have no sub-structs */
     case NI_FLOAT_ONE_TYPE:      /* or sub-arrays that need deleting */
     case NI_AFFINE_3DMAP_TYPE:
     case NI_RECT_DOMAIN_TYPE:
       DELETE_STRUCT(nd) ;
     break ;

     case NI_STATISTIC_TYPE:{
       NI_statistic *ns = (NI_statistic *)nd ;
       NI_index_t ii ;
       if( ns->param != NULL ){
         for( ii=0 ; ii < ns->param_num ; ii++ )
           NI_free_struct( ns->param[ii] ) ;    /* recursion */
         NI_free(ns->param) ;
       }
     }
     DELETE_STRUCT(nd) ;
     break ;

     case NI_VECTOR_TYPE:
     case NI_BYTE_VECTOR_TYPE:
     case NI_SHORT_VECTOR_TYPE:
     case NI_INT_VECTOR_TYPE:
     case NI_FLOAT_VECTOR_TYPE:
     case NI_DOUBLE_VECTOR_TYPE:
     case NI_COMPLEX_VECTOR_TYPE:
     case NI_RGB_VECTOR_TYPE:
     case NI_RGBA_VECTOR_TYPE:{
       NI_vector *nv = (NI_vector *)nd ;
       NI_free( nv->vec ) ;
       NI_free( nv->vec_range ) ;
       NI_free( nv->statistic ) ;
     }
     DELETE_STRUCT(nd) ;
     break ;

     case NI_STRING_VECTOR_TYPE:{
       NI_string_vector *nv = (NI_string_vector *)nd ;
       NI_index_t ii ;
       if( nv->vec != NULL ){
         for( ii=0 ; ii < nv->vec_len ; ii++ )
           NI_free( nv->vec[ii] ) ;
         NI_free( nv->vec ) ;
       }
       /* vec_range not used for string vectors */
       /* statistic not used for string vectors */
     }
     DELETE_STRUCT(nd) ;
     break ;

     case NI_POINTS_DOMAIN_TYPE:{
       NI_points_domain *np = (NI_points_domain *)nd ;
       NI_free( np->id ) ;
       NI_free( np->x  ) ;
       NI_free( np->y  ) ;
       NI_free( np->z  ) ;
     }
     DELETE_STRUCT(nd) ;
     break ;

     case NI_DATASET_TYPE:{
       NI_dataset *nn = (NI_dataset *)nd ;
       if( nn->vec != NULL ){
         NI_index_t nv , ii ;
         nv = NI_dataset_vecnum(nn) ;
         for( ii=0 ; ii < nv ; ii++ )
           NI_free_struct( nn->vec[ii] ) ;  /* recursion */
         NI_free( nn->vec ) ;
       }
       NI_free_struct( nn->domain ) ;       /* recursion */
     }
     DELETE_STRUCT(nd) ;
     break ;

   }

   return ;
}

/*-----------------------------------------------------------*/
/* This macro copies the basic elements of a struct,
   from struct qold to struct qnew.  Of course, the new
   struct gets a new idcode.  This macro may be used after
   creating a new struct with NI_new(), for example.
-------------------------------------------------------------*/

#undef  COPY_BASIC_STRUCT
#define COPY_BASIC_STRUCT(qnew,qold)               \
 do{ (qnew)->type = (qold)->type ;                 \
     (qnew)->nref = 1 ;                            \
     (qnew)->idcode = UNIQ_idcode() ;              \
     NI_register_struct( (qnew) ) ;                \
     (qnew)->name = NI_strdup((qold)->name) ;      \
 } while(0)

/*-----------------------------------------------------------*/
/* This macro makes a new struct of type TTYPE, copies
   the basic elements, and points ndnew to the new struct,
   for eventual return from NI_copy_struct().  This macro
   is used only in that function.
-------------------------------------------------------------*/

#undef DUPLICATE_STRUCT
#define DUPLICATE_STRUCT(TTYPE)                    \
   TTYPE *nn = NI_new(TTYPE) ;                     \
   TTYPE *qq = (TTYPE *)nd ;                       \
   COPY_BASIC_STRUCT(nn,qq) ;                      \
   ndnew = (NI_struct *)nn

/*-----------------------------------------------------------*/
/*! Make a copy of a struct, as opposed to a new
    reference (which is what NI_pointto_struct() does).
-------------------------------------------------------------*/

void * NI_copy_struct( void *ndd )
{
   NI_struct *nd = (NI_struct *)ndd ;
   NI_struct *ndnew=NULL ;

   if( nd == NULL ) return NULL ;  /* bad input :-( */

   switch( nd->type ){                 /* N.B.: there is no default */

     case NI_STRUCT_TYPE:{
       DUPLICATE_STRUCT(NI_struct) ;
     }
     break ;

     case NI_FLOAT_ONE_TYPE:{
       DUPLICATE_STRUCT(NI_float_one) ;
       nn->val = qq->val ;
     }
     break ;

     case NI_AFFINE_3DMAP_TYPE:{
       DUPLICATE_STRUCT(NI_affine_3dmap) ;
       nn->mat[0][0] = qq->mat[0][0]; nn->mat[0][1] = qq->mat[0][1];
       nn->mat[0][2] = qq->mat[0][2]; nn->mat[0][3] = qq->mat[0][3];
       nn->mat[1][0] = qq->mat[1][0]; nn->mat[1][1] = qq->mat[1][1];
       nn->mat[1][2] = qq->mat[1][2]; nn->mat[1][3] = qq->mat[1][3];
       nn->mat[2][0] = qq->mat[2][0]; nn->mat[2][1] = qq->mat[2][1];
       nn->mat[2][2] = qq->mat[2][2]; nn->mat[2][3] = qq->mat[2][3];
       nn->mat[3][0] = qq->mat[3][0]; nn->mat[3][1] = qq->mat[3][1];
       nn->mat[3][2] = qq->mat[3][2]; nn->mat[3][3] = qq->mat[3][3];
     }
     break ;

     case NI_RECT_DOMAIN_TYPE:{
       DUPLICATE_STRUCT(NI_rect_domain) ;
       nn->nx = qq->nx; nn->ny = qq->ny; nn->nz = qq->nz; nn->nt = qq->nt;
       nn->dx = qq->dx; nn->dy = qq->dy; nn->dz = qq->dz; nn->dt = qq->dt;
       nn->xo = qq->xo; nn->yo = qq->yo; nn->zo = qq->zo; nn->to = qq->to;
     }
     break ;

     case NI_STATISTIC_TYPE:{
       NI_index_t ii ;
       DUPLICATE_STRUCT(NI_statistic) ;
       nn->statcode = qq->statcode ;
       nn->param_num = qq->param_num ;
       if( qq->param != NULL ){
         nn->param = NI_malloc(NI_struct*, sizeof(NI_struct *)*nn->param_num) ;
         for( ii=0 ; ii < nn->param_num ; ii++ )
           nn->param[ii] = NI_copy_struct( qq->param[ii] ) ; /* recursion */
       } else {
         nn->param = NULL ;
       }
     }
     break ;

     case NI_VECTOR_TYPE:
     case NI_BYTE_VECTOR_TYPE:
     case NI_SHORT_VECTOR_TYPE:
     case NI_INT_VECTOR_TYPE:
     case NI_FLOAT_VECTOR_TYPE:
     case NI_DOUBLE_VECTOR_TYPE:
     case NI_COMPLEX_VECTOR_TYPE:
     case NI_RGB_VECTOR_TYPE:
     case NI_RGBA_VECTOR_TYPE:{
       NI_index_t ii ;
       DUPLICATE_STRUCT(NI_vector) ;
       nn->vec_len = qq->vec_len ;
       nn->vec_typ = qq->vec_typ ;
       if( qq->vec != NULL ){                                /* copy array */
         ii = nn->vec_len * NI_datatype_size(nn->vec_typ) ;
         nn->vec = NI_malloc(void, ii) ;
         memcpy( nn->vec , qq->vec , ii ) ;
       } else {
         nn->vec = NULL ;
       }
       if( qq->vec_range != NULL ){                          /* copy array */
         ii = 2 * NI_datatype_size(nn->vec_typ) ;
         nn->vec_range = NI_malloc(void, ii) ;
         memcpy( nn->vec_range , qq->vec_range , ii ) ;
       } else {
         nn->vec_range = NULL ;
       }
       nn->statistic = NI_copy_struct( qq->statistic ) ;   /* recursion */
     }
     break ;

     case NI_STRING_VECTOR_TYPE:{
       NI_index_t ii ;
       DUPLICATE_STRUCT(NI_string_vector) ;
       nn->vec_len = qq->vec_len ;
       nn->vec_typ = qq->vec_typ ;
       if( qq->vec != NULL ){                                /* copy array */
         nn->vec = NI_malloc(char*, sizeof(char *)*nn->vec_len) ;
         for( ii=0 ; ii < nn->vec_len ; ii++ )
           nn->vec[ii] = NI_strdup(qq->vec[ii]) ;
       } else {
         nn->vec = NULL ;
       }
       nn->vec_range = NULL ;  /* string vectors don't use vec_range */
       nn->statistic = NULL ;
     }
     break ;

     case NI_POINTS_DOMAIN_TYPE:{
       NI_index_t ii ;
       DUPLICATE_STRUCT(NI_points_domain) ;
       nn->num_node = ii = qq->num_node ;
       if( qq->id != NULL ){                                 /* copy array */
         nn->id = NI_malloc(NI_index_t, ii*sizeof(NI_index_t)) ;
         memcpy( nn->id , qq->id , ii*sizeof(NI_index_t) ) ;
       }
       if( qq->x != NULL ){                                  /* copy array */
         nn->x = NI_malloc(float, ii*sizeof(float)) ;
         memcpy( nn->x , qq->x , ii*sizeof(float) ) ;
       }
       if( qq->y != NULL ){                                  /* copy array */
         nn->y = NI_malloc(float, ii*sizeof(float)) ;
         memcpy( nn->y , qq->y , ii*sizeof(float) ) ;
       }
       if( qq->z != NULL ){                                  /* copy array */
         nn->z = NI_malloc(float, ii*sizeof(float)) ;
         memcpy( nn->z , qq->z , ii*sizeof(float) ) ;
       }
       nn->seq = qq->seq; nn->seqbase = qq->seqbase; nn->sorted = qq->sorted;
     }
     break ;

     case NI_DATASET_TYPE:{
       DUPLICATE_STRUCT(NI_dataset) ;
       nn->num_node = qq->num_node ;
       nn->num_val  = qq->num_val  ;
       nn->order    = qq->order    ;
       if( qq->vec != NULL ){
         NI_index_t nv , ii ;
         nv = NI_dataset_vecnum(nn) ;
         nn->vec = NI_malloc(NI_vector*, sizeof(NI_vector *)*nv) ;
         for( ii=0 ; ii < nv ; ii++ )
           nn->vec[ii] = NI_copy_struct( qq->vec[ii] ) ;   /* recursion */
       } else {
         nn->vec = NULL ;
       }
       nn->domain = NI_copy_struct( qq->domain ) ;         /* recursion */
     }
     break ;

   }

   return (void *)ndnew ;
}
#undef DUPLICATE_STRUCT
