#include "niml_private.h"

/*-----------------------------------------------------------*/
/*! Create a new vector, of the given type and length.
    Returns NULL if an error occurs.  Otherwise, the vec
    and vec_range arrays are calloc()-ed, and the statistic
    struct is set to NULL.
-------------------------------------------------------------*/

void * NI_new_vector( int dtyp , NI_index_t len )
{
   NI_vector *nv ;
   NI_index_t ii ;
   int siz ;

   if( len <= 0 ) return NULL ;

   siz = NI_datatype_size( dtyp ) ;
   if( dtyp != NI_STRING && siz <= 0 ) return NULL ;

   nv = NI_new(NI_vector) ;
   if( NI_is_builtin_type(dtyp) )
     nv->type  = NI_VECTOR_TYPE + dtyp + 1 ;  /* type patched */
   else
     nv->type  = NI_VECTOR_TYPE ;             /* generic type */
   nv->vec_typ = dtyp ;

   if( dtyp != NI_STRING ){
     nv->vec       = NI_malloc(void,  NI_datatype_size(dtyp) * len ) ;
     nv->vec_range = NI_malloc(void, NI_datatype_size(dtyp) * 2   ) ;
   } else {
     nv->vec       = NI_malloc(void, sizeof(char *) * len ) ;
     nv->vec_range = NULL ;  /* string vectors don't use vec_range */
   }
   nv->statistic = NULL ;
   return (void *)nv ;
}

/*-----------------------------------------------------------*/
/*! Set the range in a vector struct. */

void NI_set_vector_range( void *nvv )
{
   NI_vector *nv = (NI_vector *)nvv ;
   NI_index_t len, ii ;

   if( nv == NULL                       ||
       !NI_is_vector_type(nv->type)     ||
       !NI_is_builtin_type(nv->vec_typ) ||
       nv->vec_typ == NI_STRING           ) return ;

   len = nv->vec_len ; if( len <= 0 ) return ;

   if( nv->vec_range == NULL )
     nv->vec_range = NI_malloc(void, 2*NI_datatype_size(nv->vec_typ) ) ;

   switch( nv->vec_typ ){  /* no default */

     case NI_BYTE:{
       byte *vv = (byte *)nv->vec ;
       byte *vr = (byte *)nv->vec_range ;
       byte vbot=vv[0], vtop=vv[0] ;
       for( ii=1 ; ii < len ; ii++ )
              if( vv[ii] < vbot ) vbot = vv[ii] ;
         else if( vv[ii] > vtop ) vtop = vv[ii] ;
       vr[0] = vbot ; vr[1] = vtop ;
     }
     break ;

     case NI_SHORT:{
       short *vv = (short *)nv->vec ;
       short *vr = (short *)nv->vec_range ;
       short vbot=vv[0], vtop=vv[0] ;
       for( ii=1 ; ii < len ; ii++ )
              if( vv[ii] < vbot ) vbot = vv[ii] ;
         else if( vv[ii] > vtop ) vtop = vv[ii] ;
       vr[0] = vbot ; vr[1] = vtop ;
     }
     break ;

     case NI_INT:{
       int *vv = (int *)nv->vec ;
       int *vr = (int *)nv->vec_range ;
       int vbot=vv[0], vtop=vv[0] ;
       for( ii=1 ; ii < len ; ii++ )
              if( vv[ii] < vbot ) vbot = vv[ii] ;
         else if( vv[ii] > vtop ) vtop = vv[ii] ;
       vr[0] = vbot ; vr[1] = vtop ;
     }
     break ;

     case NI_FLOAT:{
       float *vv = (float *)nv->vec ;
       float *vr = (float *)nv->vec_range ;
       float vbot=vv[0], vtop=vv[0] ;
       for( ii=1 ; ii < len ; ii++ )
              if( vv[ii] < vbot ) vbot = vv[ii] ;
         else if( vv[ii] > vtop ) vtop = vv[ii] ;
       vr[0] = vbot ; vr[1] = vtop ;
     }
     break ;

     case NI_DOUBLE:{
       double *vv = (double *)nv->vec ;
       double *vr = (double *)nv->vec_range ;
       double vbot=vv[0], vtop=vv[0] ;
       for( ii=1 ; ii < len ; ii++ )
              if( vv[ii] < vbot ) vbot = vv[ii] ;
         else if( vv[ii] > vtop ) vtop = vv[ii] ;
       vr[0] = vbot ; vr[1] = vtop ;
     }
     break ;

     case NI_COMPLEX:{
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

     case NI_RGB:{
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

     case NI_RGBA:{
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
