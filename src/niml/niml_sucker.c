#include "niml_private.h"

/*---------------------------------------------------------------------------*/
/*! Return a pointer to the idcode of a NIML element (group or data),
    if it has one.  Otherwise, return NULL.
-----------------------------------------------------------------------------*/

char * NI_self_idcode( void *nini )
{
   char *rhs ;

   rhs = NI_get_attribute( nini , "self_idcode" ) ;
   if( rhs != NULL ) return rhs ;

   rhs = NI_get_attribute( nini , "idcode" ) ;
   if( rhs != NULL ) return rhs ;

   rhs = NI_get_attribute( nini , "AFNI_idcode" ) ;
   if( rhs != NULL ) return rhs ;

   return NULL ;
}

/*---------------------------------------------------------------------------*/
/*! Open a stream, read all NIML stuff possible from it, close it.
-----------------------------------------------------------------------------*/

void NI_suck_stream( char *sname, int msec, int *ndc, NI_datacontainer ***dc )
{
   NI_stream ns ;
   int nn , start_msec=NI_clock_time() ;
   NI_datacontainer *mdc ;
   void *nini ;
   char *rhs ;

   /*-- startup and sanity checks --*/

   if( ndc == NULL ) return ;
   *ndc = 0 ;
   if( dc == NULL ) return ;
   *dc = NULL ;

   ns = NI_stream_open( sname , "r" ) ;
   if( ns == NULL ) return ;

   NI_add_trusted_host(NULL) ;
   if( msec < 0 ) msec = 999999999 ;

   /*-- wait for connection to be good --*/

   while(1){
     nn = NI_stream_goodcheck( ns , 100 ) ;
     if( nn == 1 ) break ;
     if( nn <  0 || NI_clock_time()-start_msec > msec ) return ;
   }

   /*-- get a new NI element (group or data) --*/

GetElement:
   nini = NI_read_element( ns , msec ) ;
   if( nini == NULL ) return ;            /** the way out **/

   nn  = NI_element_type(nini) ;
   rhs = NI_self_idcode (nini) ;
   mdc = (NI_datacontainer *)calloc(1,sizeof(NI_datacontainer)) ;

   mdc->self = nini ;
   NI_strncpy( mdc->self_idcode , rhs , IDCODE_LEN) ;

   if( nn == NI_ELEMENT_TYPE ){
     NI_element *nel = (NI_element *)nini ;

     NI_strncpy( mdc->typename  , "NI_ELEMENT" , IDCODE_LEN ) ;
     NI_strncpy( mdc->self_name , nel->name    , IDCODE_LEN ) ;

   } else if( nn == NI_GROUP_TYPE ){
     NI_group *ngr = (NI_group *)nini ;

     NI_strncpy( mdc->typename  , "NI_GROUP" , IDCODE_LEN ) ;
     NI_strncpy( mdc->self_name , ngr->name  , IDCODE_LEN ) ;

   } else {  /** should never happen */

     fprintf(stderr,"\n** ERROR: non-NIML data on stream '%s' !!\n",sname) ;
     free((void *)mdc) ;
     goto GetElement ;

   }

   /*-- add new element to output list --*/

   NI_convert_elm_to_obj( mdc ) ; /* convert to struct in-place, if possible */

   (*ndc)++ ;
   (*dc) = (NI_datacontainer **)realloc( (void *)(*dc) ,
                                         sizeof(NI_datacontainer *) * (*ndc) );
   (*dc)[(*ndc)-1] = mdc ;

   goto GetElement ;
}

/*---------------------------------------------------------------------------*/
/*! See if we can convert an element to an object.
-----------------------------------------------------------------------------*/

void NI_convert_elm_to_obj( NI_datacontainer *dc )
{
   if( dc == NULL ) return ;
   return ;
}
