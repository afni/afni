#include "nids.h"

#undef INLINE
#ifdef __GNUC__
# define INLINE inline
#else
# define INLINE /*nada*/
#endif

/*-----------------------------------------------------------*/
/*! Return the size in bytes of an atomic datatype. */

int NIDS_datatype_size( int dtyp )
{
   switch( dtyp ){
    case NIDS_BYTE:        return sizeof(byte);
    case NIDS_SHORT:       return sizeof(short);
    case NIDS_INT:         return sizeof(int);
    case NIDS_FLOAT:       return sizeof(float);
    case NIDS_DOUBLE:      return sizeof(double);
    case NIDS_COMPLEX:     return sizeof(complex);
    case NIDS_RGB:         return sizeof(rgb);
    case NIDS_RGBA:        return sizeof(rgba);
  }
  return 0;
}

/*-----------------------------------------------------------*/
/*! Holds the table of all registered structs. */

static Htable *nids_struct_table=NULL ;

/*-----------------------------------------------------------*/
/*! Register a struct by its idcode (if it has one). */

void NIDS_register_struct( void *ndd )
{
   NIDS_struct *nd = (NIDS_struct *)ndd ;
   void *vp ;

   /* can't register without idcode */

   if( nd == NULL || nd->idcode == NULL ) return ;

   /* 1st time in ==> create hash table */

   if( nids_struct_table == NULL )
     nids_struct_table = new_Htable( 1031 ) ;

   /* see if it already is registered */

   vp = findin_Htable( nd->idcode , nids_struct_table ) ;
   if( vp != NULL ) return ;  /* duplicate entry */

   /* OK, add it to the table */

   addto_Htable( nd->idcode , nd , nids_struct_table ) ;
   return ;
}

/*-----------------------------------------------------------*/
/*! Find a struct by its idcode. */

void * NIDS_find_struct( char *idcode )
{
   void *vp ;
   if( idcode == NULL ) return NULL ; /* nothing to do */
   vp = findin_Htable( idcode , nids_struct_table ) ;
   return vp ;
}

/*-----------------------------------------------------------*/
/*! Remove a struct from the table. */

void NIDS_unregister_struct( void *ndd )
{
   NIDS_struct *nd = (NIDS_struct *)ndd ;
   if( nd == NULL || nd->idcode == NULL ) return ;
   removefrom_Htable( nd->idcode , nids_struct_table ) ;
   return ;
}

/*-----------------------------------------------------------*/
/*! Return a copy of the pointer to the struct,
    also incrementing its reference counter.    */

void * NIDS_pointto_struct( void *ndd )
{
   NIDS_struct *nd = (NIDS_struct *)ndd ;
   if( nd == NULL ) return NULL ;
   nd->nref ++ ;
   return (void *)nd ;
}

/*-----------------------------------------------------------*/
/* This macro does the basic stuff necessary to delete a
   struct from the hash table and from memory.  It is used
   at the very end of the NIDS_free_struct() function.
   Type specific code is also needed to delete any memory
   used by sub-structs or sub-arrays.
-------------------------------------------------------------*/

#undef  DELETE_STRUCT
#define DELETE_STRUCT(nq)                \
 do{ NIDS_unregister_struct(nq);         \
     NIDS_free(nq->idcode) ;             \
     NIDS_free(nq->name)   ;             \
     NIDS_free(nq)         ; } while(0)

/*-----------------------------------------------------------*/
/*! Decrement the reference counter, and destroy the struct
    (recursively in some cases) if the counter goes to zero.
-------------------------------------------------------------*/

void NIDS_free_struct( void *ndd )
{
   NIDS_struct *nd = (NIDS_struct *)ndd ;
   if( nd == NULL ) return ;

   /* decrementation */

   nd->nref -- ;
   if( nd->nref > 0 ) return ;         /* keep it */

   /* OK, blot it from the universe */

   switch( nd->type ){                 /* N.B.: there is no default */

     case NIDS_STRUCT_TYPE:            /* These types have no sub-structs */
     case NIDS_FLOAT_ONE_TYPE:         /* or sub-arrays that need deleting */
     case NIDS_AFFINE_3DMAP_TYPE:
     case NIDS_RECT_DOMAIN_TYPE:
       DELETE_STRUCT(nd) ;
     break ;

     case NIDS_STATISTIC_TYPE:{
       NIDS_statistic *ns = (NIDS_statistic *)nd ;
       NIDS_index_t ii ;
       if( ns->param != NULL ){
         for( ii=0 ; ii < ns->param_num ; ii++ )
           NIDS_free_struct( ns->param[ii] ) ;    /* recursion */
         NIDS_free(ns->param) ;
       }
     }
     DELETE_STRUCT(nd) ;
     break ;

     case NIDS_VECTOR_TYPE:
     case NIDS_BYTE_VECTOR_TYPE:
     case NIDS_SHORT_VECTOR_TYPE:
     case NIDS_INT_VECTOR_TYPE:
     case NIDS_FLOAT_VECTOR_TYPE:
     case NIDS_DOUBLE_VECTOR_TYPE:
     case NIDS_COMPLEX_VECTOR_TYPE:
     case NIDS_RGB_VECTOR_TYPE:
     case NIDS_RGBA_VECTOR_TYPE:{
       NIDS_vector *nv = (NIDS_vector *)nd ;
       NIDS_free( nv->vec ) ;
       NIDS_free( nv->vec_range ) ;
       NIDS_free( nv->statistic ) ;
     }
     DELETE_STRUCT(nd) ;
     break ;

     case NIDS_STRING_VECTOR_TYPE:{
       NIDS_string_vector *nv = (NIDS_string_vector *)nd ;
       NIDS_index_t ii ;
       if( nv->vec != NULL ){
         for( ii=0 ; ii < nv->vec_len ; ii++ )
           NIDS_free( nv->vec[ii] ) ;
         NIDS_free( nv->vec ) ;
       }
       /* vec_range not used for string vectors */
       /* statistic not used for string vectors */
     }
     DELETE_STRUCT(nd) ;
     break ;

     case NIDS_POINTS_DOMAIN_TYPE:{
       NIDS_points_domain *np = (NIDS_points_domain *)nd ;
       NIDS_free( np->id ) ;
       NIDS_free( np->x  ) ;
       NIDS_free( np->y  ) ;
       NIDS_free( np->z  ) ;
     }
     DELETE_STRUCT(nd) ;
     break ;

     case NIDS_DATASET_TYPE:{
       NIDS_dataset *nn = (NIDS_dataset *)nd ;
       if( nn->vec != NULL ){
         NIDS_index_t nv , ii ;
         nv = NIDS_dataset_vecnum(nn) ;
         for( ii=0 ; ii < nv ; ii++ )
           NIDS_free_struct( nn->vec[ii] ) ;  /* recursion */
         NIDS_free( nn->vec ) ;
       }
       NIDS_free_struct( nn->domain ) ;       /* recursion */
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
   creating a new struct with NIDS_new(), for example.
-------------------------------------------------------------*/

#undef  COPY_BASIC_STRUCT
#define COPY_BASIC_STRUCT(qnew,qold)                 \
 do{ (qnew)->type = (qold)->type ;                   \
     (qnew)->nref = 1 ;                              \
     (qnew)->idcode = UNIQ_idcode() ;                \
     NIDS_register_struct( (qnew) ) ;                \
     (qnew)->name = NIDS_strdup((qold)->name) ;      \
 } while(0)

/*-----------------------------------------------------------*/
/* This macro makes a new struct of type TTYPE, copies
   the basic elements, and points ndnew to the new struct,
   for eventual return from NIDS_copy_struct().  This macro
   is used only in that function.
-------------------------------------------------------------*/

#undef DUPLICATE_STRUCT
#define DUPLICATE_STRUCT(TTYPE)                      \
   TTYPE *nn = NIDS_new(TTYPE) ;                     \
   TTYPE *qq = (TTYPE *)nd ;                         \
   COPY_BASIC_STRUCT(nn,qq) ;                        \
   ndnew = (NIDS_struct *)nn

/*-----------------------------------------------------------*/
/*! Make a copy of a struct, as opposed to a new
    reference (which is what NIDS_pointto_struct() does).
-------------------------------------------------------------*/

void * NIDS_copy_struct( void *ndd )
{
   NIDS_struct *nd = (NIDS_struct *)ndd ;
   NIDS_struct *ndnew=NULL ;

   if( nd == NULL ) return NULL ;  /* bad input :-( */

   switch( nd->type ){                 /* N.B.: there is no default */

     case NIDS_STRUCT_TYPE:{
       DUPLICATE_STRUCT(NIDS_struct) ;
     }
     break ;

     case NIDS_FLOAT_ONE_TYPE:{
       DUPLICATE_STRUCT(NIDS_float_one) ;
       nn->val = qq->val ;
     }
     break ;

     case NIDS_AFFINE_3DMAP_TYPE:{
       DUPLICATE_STRUCT(NIDS_affine_3dmap) ;
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

     case NIDS_RECT_DOMAIN_TYPE:{
       DUPLICATE_STRUCT(NIDS_rect_domain) ;
       nn->nx = qq->nx; nn->ny = qq->ny; nn->nz = qq->nz; nn->nt = qq->nt;
       nn->dx = qq->dx; nn->dy = qq->dy; nn->dz = qq->dz; nn->dt = qq->dt;
       nn->xo = qq->xo; nn->yo = qq->yo; nn->zo = qq->zo; nn->to = qq->to;
     }
     break ;

     case NIDS_STATISTIC_TYPE:{
       NIDS_index_t ii ;
       DUPLICATE_STRUCT(NIDS_statistic) ;
       nn->statcode = qq->statcode ;
       nn->param_num = qq->param_num ;
       if( qq->param != NULL ){
         nn->param = NIDS_malloc(sizeof(NIDS_struct *)*nn->param_num) ;
         for( ii=0 ; ii < nn->param_num ; ii++ )
           nn->param[ii] = NIDS_copy_struct( qq->param[ii] ) ; /* recursion */
       } else {
         nn->param = NULL ;
       }
     }
     break ;

     case NIDS_VECTOR_TYPE:
     case NIDS_BYTE_VECTOR_TYPE:
     case NIDS_SHORT_VECTOR_TYPE:
     case NIDS_INT_VECTOR_TYPE:
     case NIDS_FLOAT_VECTOR_TYPE:
     case NIDS_DOUBLE_VECTOR_TYPE:
     case NIDS_COMPLEX_VECTOR_TYPE:
     case NIDS_RGB_VECTOR_TYPE:
     case NIDS_RGBA_VECTOR_TYPE:{
       NIDS_index_t ii ;
       DUPLICATE_STRUCT(NIDS_vector) ;
       nn->vec_len = qq->vec_len ;
       nn->vec_typ = qq->vec_typ ;
       if( qq->vec != NULL ){                                 /* copy array */
         ii = nn->vec_len * NIDS_datatype_size(nn->vec_typ) ;
         nn->vec = NIDS_malloc(ii) ;
         memcpy( nn->vec , qq->vec , ii ) ;
       } else {
         nn->vec = NULL ;
       }
       if( qq->vec_range != NULL ){                           /* copy array */
         ii = 2 * NIDS_datatype_size(nn->vec_typ) ;
         nn->vec_range = NIDS_malloc(ii) ;
         memcpy( nn->vec_range , qq->vec_range , ii ) ;
       } else {
         nn->vec_range = NULL ;
       }
       nn->statistic = NIDS_copy_struct( qq->statistic ) ;    /* recursion */
     }
     break ;

     case NIDS_STRING_VECTOR_TYPE:{
       NIDS_index_t ii ;
       DUPLICATE_STRUCT(NIDS_string_vector) ;
       nn->vec_len = qq->vec_len ;
       nn->vec_typ = qq->vec_typ ;
       if( qq->vec != NULL ){                                 /* copy array */
         nn->vec = NIDS_malloc(sizeof(char *)*nn->vec_len) ;
         for( ii=0 ; ii < nn->vec_len ; ii++ )
           nn->vec[ii] = NIDS_strdup(qq->vec[ii]) ;
       } else {
         nn->vec = NULL ;
       }
       nn->vec_range = NULL ;  /* string vectors don't use vec_range */
       nn->statistic = NULL ;
     }
     break ;

     case NIDS_POINTS_DOMAIN_TYPE:{
       NIDS_index_t ii ;
       DUPLICATE_STRUCT(NIDS_points_domain) ;
       nn->num_node = ii = qq->num_node ;
       if( qq->id != NULL ){                                  /* copy array */
         nn->id = NIDS_malloc(ii*sizeof(NIDS_index_t)) ;
         memcpy( nn->id , qq->id , ii*sizeof(NIDS_index_t) ) ;
       }
       if( qq->x != NULL ){                                   /* copy array */
         nn->x = NIDS_malloc(ii*sizeof(float)) ;
         memcpy( nn->x , qq->x , ii*sizeof(float) ) ;
       }
       if( qq->y != NULL ){                                   /* copy array */
         nn->y = NIDS_malloc(ii*sizeof(float)) ;
         memcpy( nn->y , qq->y , ii*sizeof(float) ) ;
       }
       if( qq->z != NULL ){                                   /* copy array */
         nn->z = NIDS_malloc(ii*sizeof(float)) ;
         memcpy( nn->z , qq->z , ii*sizeof(float) ) ;
       }
       nn->seq = qq->seq; nn->seqbase = qq->seqbase; nn->sorted = qq->sorted;
     }
     break ;

     case NIDS_DATASET_TYPE:{
       DUPLICATE_STRUCT(NIDS_dataset) ;
       nn->num_node = qq->num_node ;
       nn->num_val  = qq->num_val  ;
       nn->order    = qq->order    ;
       if( qq->vec != NULL ){
         NIDS_index_t nv , ii ;
         nv = NIDS_dataset_vecnum(nn) ;
         nn->vec = NIDS_malloc(sizeof(NIDS_vector *)*nv) ;
         for( ii=0 ; ii < nv ; ii++ )
           nn->vec[ii] = NIDS_copy_struct( qq->vec[ii] ) ;    /* recursion */
       } else {
         nn->vec = NULL ;
       }
       nn->domain = NIDS_copy_struct( qq->domain ) ;          /* recursion */
     }
     break ;

   }

   return (void *)ndnew ;
}
#undef DUPLICATE_STRUCT

/*-----------------------------------------------------------*/
/*! Create a new vector, of the given type and length.
    Returns NULL if an error occurs.  Otherwise, the vec
    and vec_range arrays are calloc()-ed, and the statistic
    struct is set to NULL.
-------------------------------------------------------------*/

void * NIDS_new_vector( int dtyp , NIDS_index_t len )
{
   NIDS_vector *nv ;
   NIDS_index_t ii ;

   if( len <= 0 ) return NULL ;
   if( dtyp < NIDS_FIRST_DATATYPE ||
       dtyp > NIDS_LAST_DATATYPE    ) return NULL ;

   nv = NIDS_new(NIDS_vector) ;
   nv->type    = NIDS_VECTOR_TYPE + dtyp + 1 ;  /* type patched */
   nv->vec_typ = dtyp ;

   if( dtyp != NIDS_STRING ){
     nv->vec       = NIDS_malloc( NIDS_datatype_size(dtyp) * len ) ;
     nv->vec_range = NIDS_malloc( NIDS_datatype_size(dtyp) * 2   ) ;
   } else {
     nv->vec       = NIDS_malloc( sizeof(char *) * len ) ;
     nv->vec_range = NULL ;  /* string vectors don't use vec_range */
   }
   nv->statistic = NULL ;
   return (void *)nv ;
}

/*-----------------------------------------------------------*/
/*! Transpose a dataset, so that rows are columns and vice-
    versa.
     - Requires that all vectors (columns) have the same type
     - Requires that all vectors do NOT have statistical
       distribution types attached.
     - Returns pointer to a new dataset (NULL for errors).
-------------------------------------------------------------*/

void * NIDS_dataset_transpose( void *ndd )
{
   NIDS_dataset *ndnew , *nd = (NIDS_dataset *)ndd ;
   NIDS_index_t ii,jj , nvec_old,nvec_new , len_old,len_new ;
   int tt , lt ;

   if( nd       == NULL              ||
       nd->type != NIDS_DATASET_TYPE ||
       nd->vec  == NULL                ) return NULL ;  /* bad input */

   /* check if all columns have same type, etc. */

   nvec_old = NIDS_dataset_vecnum(nd) ;
   len_old  = NIDS_dataset_veclen(nd) ;

   if( nvec_old <= 0 || len_old <= 0 ) return NULL ;
   if( nd->vec[0]            == NULL ) return NULL ;
   if( nd->vec[0]->statistic != NULL ) return NULL ;

   tt = nd->vec[0]->vec_typ ;
   lt = NIDS_datatype_size(tt) ;
   for( ii=1 ; ii < nvec_old ; ii++ ){
     if( nd->vec[ii]            == NULL ) return NULL ;
     if( nd->vec[ii]->vec_typ   != tt   ) return NULL ;
     if( nd->vec[ii]->statistic != NULL ) return NULL ;
   }

   /* create output struct */

   ndnew = NIDS_new(NIDS_dataset) ;
   COPY_BASIC_STRUCT(ndnew,nd) ;
   ndnew->num_node = nd->num_node ;
   ndnew->num_val  = nd->num_val  ;
   ndnew->order    = NIDS_opposite_order(nd->order) ;   /* flipped */
   ndnew->domain   = NIDS_pointto_struct(nd->domain) ;  /* same domain */

   /* create new vectors */

   nvec_new = NIDS_dataset_vecnum(ndnew) ;
   len_new  = NIDS_dataset_veclen(ndnew) ;

   ndnew->vec = NIDS_malloc( sizeof(NIDS_vector *) * nvec_new ) ;
   for( ii=0 ; ii < nvec_new ; ii++ )
     ndnew->vec[ii] = NIDS_new_vector( tt , len_new ) ;

   /* copy data from old vectors to new vectors */

   if( tt != NIDS_STRING ){                      /* copy numbers */

     char *vnew , *vold ;
     for( ii=0 ; ii < nvec_new ; ii++ ){
       vnew = (char *)ndnew->vec[ii]->vec ;
       for( jj=0 ; jj < nvec_old ; jj++ ){
         vold = (char *)nd->vec[jj]->vec ;
         memcpy( vnew+lt*jj , vold+lt*ii , lt ) ;
       }
     }

     for( ii=0 ; ii < nvec_new ; ii++ )
       NIDS_set_vector_range( ndnew->vec[ii] ) ;

   } else {                                        /* duplicate strings */

     char **vnew , **vold ;
     for( ii=0 ; ii < nvec_new ; ii++ ){
       vnew = (char **)ndnew->vec[ii]->vec ;
       for( jj=0 ; jj < nvec_old ; jj++ ){
         vold = (char **)nd->vec[jj]->vec ;
         vnew[jj] = NIDS_strdup( vold[ii] ) ;
       }
     }

   }

   /** done **/

   return (void *)ndnew ;
}

/*-----------------------------------------------------------*/
/*! Set the range in a vector struct. */

void NIDS_set_vector_range( void *nvv )
{
   NIDS_vector *nv = (NIDS_vector *)nvv ;
   NIDS_index_t len, ii ;

   if( nv == NULL                     ||
       !NIDS_is_vector_type(nv->type) ||
       nv->vec_typ == NIDS_STRING       ) return ;

   len = nv->vec_len ; if( len <= 0 ) return ;

   if( nv->vec_range == NULL )
     nv->vec_range = NIDS_malloc( 2*NIDS_datatype_size(nv->vec_typ) ) ;

   switch( nv->vec_typ ){  /* no default */

     case NIDS_BYTE:{
       byte *vv = (byte *)nv->vec ;
       byte *vr = (byte *)nv->vec_range ;
       byte vbot=vv[0], vtop=vv[0] ;
       for( ii=1 ; ii < len ; ii++ )
              if( vv[ii] < vbot ) vbot = vv[ii] ;
         else if( vv[ii] > vtop ) vtop = vv[ii] ;
       vr[0] = vbot ; vr[1] = vtop ;
     }
     break ;

     case NIDS_SHORT:{
       short *vv = (short *)nv->vec ;
       short *vr = (short *)nv->vec_range ;
       short vbot=vv[0], vtop=vv[0] ;
       for( ii=1 ; ii < len ; ii++ )
              if( vv[ii] < vbot ) vbot = vv[ii] ;
         else if( vv[ii] > vtop ) vtop = vv[ii] ;
       vr[0] = vbot ; vr[1] = vtop ;
     }
     break ;

     case NIDS_INT:{
       int *vv = (int *)nv->vec ;
       int *vr = (int *)nv->vec_range ;
       int vbot=vv[0], vtop=vv[0] ;
       for( ii=1 ; ii < len ; ii++ )
              if( vv[ii] < vbot ) vbot = vv[ii] ;
         else if( vv[ii] > vtop ) vtop = vv[ii] ;
       vr[0] = vbot ; vr[1] = vtop ;
     }
     break ;

     case NIDS_FLOAT:{
       float *vv = (float *)nv->vec ;
       float *vr = (float *)nv->vec_range ;
       float vbot=vv[0], vtop=vv[0] ;
       for( ii=1 ; ii < len ; ii++ )
              if( vv[ii] < vbot ) vbot = vv[ii] ;
         else if( vv[ii] > vtop ) vtop = vv[ii] ;
       vr[0] = vbot ; vr[1] = vtop ;
     }
     break ;

     case NIDS_DOUBLE:{
       double *vv = (double *)nv->vec ;
       double *vr = (double *)nv->vec_range ;
       double vbot=vv[0], vtop=vv[0] ;
       for( ii=1 ; ii < len ; ii++ )
              if( vv[ii] < vbot ) vbot = vv[ii] ;
         else if( vv[ii] > vtop ) vtop = vv[ii] ;
       vr[0] = vbot ; vr[1] = vtop ;
     }
     break ;

     case NIDS_COMPLEX:{
       complex *vv = (complex *)nv->vec ;
       complex *vr = (complex *)nv->vec_range ;
       complex vbot=vv[0], vtop=vv[0] ;
       for( ii=1 ; ii < len ; ii++ ){
              if( vv[ii].r < vbot.r ) vbot.r = vv[ii].r ;
         else if( vv[ii].r > vtop.r ) vtop.r = vv[ii].r ;
              if( vv[ii].i < vbot.i ) vbot.i = vv[ii].i ;
         else if( vv[ii].i > vtop.i ) vtop.i = vv[ii].i ;
       }
       vr[0] = vbot ; vr[1] = vtop ;
     }
     break ;

     case NIDS_RGB:{
       rgb *vv = (rgb *)nv->vec ;
       rgb *vr = (rgb *)nv->vec_range ;
       rgb vbot=vv[0], vtop=vv[0] ;
       for( ii=1 ; ii < len ; ii++ ){
              if( vv[ii].r < vbot.r ) vbot.r = vv[ii].r ;
         else if( vv[ii].r > vtop.r ) vtop.r = vv[ii].r ;
              if( vv[ii].g < vbot.g ) vbot.g = vv[ii].g ;
         else if( vv[ii].g > vtop.g ) vtop.g = vv[ii].g ;
              if( vv[ii].b < vbot.b ) vbot.b = vv[ii].b ;
         else if( vv[ii].b > vtop.b ) vtop.b = vv[ii].b ;
       }
       vr[0] = vbot ; vr[1] = vtop ;
     }
     break ;

     case NIDS_RGBA:{
       rgba *vv = (rgba *)nv->vec ;
       rgba *vr = (rgba *)nv->vec_range ;
       rgba vbot=vv[0], vtop=vv[0] ;
       for( ii=1 ; ii < len ; ii++ ){
              if( vv[ii].r < vbot.r ) vbot.r = vv[ii].r ;
         else if( vv[ii].r > vtop.r ) vtop.r = vv[ii].r ;
              if( vv[ii].g < vbot.g ) vbot.g = vv[ii].g ;
         else if( vv[ii].g > vtop.g ) vtop.g = vv[ii].g ;
              if( vv[ii].b < vbot.b ) vbot.b = vv[ii].b ;
         else if( vv[ii].b > vtop.b ) vtop.b = vv[ii].b ;
              if( vv[ii].a < vbot.a ) vbot.a = vv[ii].a ;
         else if( vv[ii].a > vtop.a ) vtop.a = vv[ii].a ;
       }
       vr[0] = vbot ; vr[1] = vtop ;
     }
     break ;

   }

   return ;
}
