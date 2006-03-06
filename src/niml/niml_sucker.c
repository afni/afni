#include "niml_private.h"

/*---------------------------------------------------------------------------*/
/*! Return a pointer to the idcode of a NIML element (group or data),
    if it has one.  Otherwise, return NULL.  Do not modify or free()
    this string, since it points into the NIML element struct.
-----------------------------------------------------------------------------*/

char * NI_self_idcode( void *nini )
{
   char *rhs ;
   int ii ;
   static char *iname[] = { "self_idcode" ,
                            "AFNI_idcode" ,
                            "ni_idcode"   ,
                            "idcode"      ,
                            NULL           } ;

   for( ii=0 ; iname[ii] != NULL ; ii++ ){
     rhs = NI_get_attribute( nini , iname[ii] ) ;
     if( rhs != NULL ) return rhs ;
   }

   return NULL ;
}

/*---------------------------------------------------------------------------*/
/*! Open a stream, read all NIML stuff possible from it, close it.
    - Returns an array of (*ndc) object containers in (*dc).
    - Max time delay allowed at any I/O step is msec.
    - If (*ndc)==0 on return, then nothing good happened.
-----------------------------------------------------------------------------*/

void NI_suck_stream( char *sname, int msec, int *ndc, NI_objcontainer ***dc )
{
   NI_stream ns ;
   int nn , start_msec=NI_clock_time() ;
   NI_objcontainer *mdc ;
   void *nini ;
   char *rhs ;

   /*-- startup and sanity checks --*/

   if( ndc == NULL ) return ;  /* clueless caller */
   *ndc = 0 ;                  /* number of objects found thus far */
   if( dc == NULL ) return ;   /* stupid caller */
   *dc = NULL ;                /* array of objects found thus far */

   ns = NI_stream_open( sname , "r" ) ;
   if( ns == NULL ) return ;                /* not so good */

   NI_add_trusted_host(NULL) ;
        if( msec == 0 ) msec = 1 ;                /* short waits */
   else if( msec <  0 ) msec = 999999999 ;        /* long waits */

   /*-- wait for connection to be good --*/

   nn = NI_stream_goodcheck( ns , msec ) ;
   if( nn <= 0 ){ NI_stream_closenow(ns); return; }

   /*-- loopback point to get a new NI element (group or data) --*/

 GetElement:
   nini = NI_read_element( ns , msec ) ;
   if( nini == NULL ){ NI_stream_closenow(ns); return; } /*** the way out ***/

   nn  = NI_element_type(nini) ;
   rhs = NI_self_idcode (nini) ;
   mdc = (NI_objcontainer *)calloc(1,sizeof(NI_objcontainer)) ;

   mdc->self_data = nini ;
   NI_strncpy( mdc->self_idcode , rhs , IDCODE_LEN ) ;

   if( nn == NI_ELEMENT_TYPE ){
     NI_element *nel = (NI_element *)nini ;

     NI_strncpy( mdc->type_name , "NI_ELEMENT" , IDCODE_LEN ) ;
     NI_strncpy( mdc->self_name , nel->name    , IDCODE_LEN ) ;

   } else if( nn == NI_GROUP_TYPE ){
     NI_group *ngr = (NI_group *)nini ;

     NI_strncpy( mdc->type_name , "NI_GROUP" , IDCODE_LEN ) ;
     NI_strncpy( mdc->self_name , ngr->name  , IDCODE_LEN ) ;

   } else {  /** should never happen */

     fprintf(stderr,"\n** ERROR: non-NIML data on stream '%s' !!\n",sname) ;
     free((void *)mdc) ;
     goto GetElement ;

   }

   /*-- add new element to output list --*/

   NI_convert_elm_to_obj( mdc ) ; /* convert to struct in-place, if possible */

   (*ndc)++ ;
   (*dc) = (NI_objcontainer **)realloc( (void *)(*dc) ,
                                         sizeof(NI_objcontainer *) * (*ndc) ) ;
   (*dc)[(*ndc)-1] = mdc ;

   goto GetElement ;
}

/*---------------------------------------------------------------------------*/

typedef struct {
   char self_name[IDCODE_LEN] ;
   NI_objconverter_func to_obj , to_elm ;
} NI_converterstruct ;

static int             num_converters = 0    ;
static NI_converterstruct *converters = NULL ;

/*---------------------------------------------------------------------------*/

void NI_register_objconverters( char *self_name ,
                                NI_objconverter_func elm_to_obj ,
                                NI_objconverter_func obj_to_elm )
{
   int cc ;

   if( self_name == NULL || *self_name == '\0' ) return ;
   if( elm_to_obj == (NI_objconverter_func)NULL  ) return ;

   for( cc=0 ; cc < num_converters ; cc++ )
     if( strcmp(converters[cc].self_name,self_name) == 0 ) break ;

   if( cc == num_converters ){
     num_converters++ ;
     converters = (NI_converterstruct *)
                    realloc( (void *)converters ,
                             sizeof(NI_converterstruct)*num_converters ) ;
   }

   NI_strncpy( converters[cc].self_name , self_name , IDCODE_LEN ) ;
   converters[cc].to_obj = elm_to_obj ;
   converters[cc].to_elm = obj_to_elm ;
   return ;
}

/*---------------------------------------------------------------------------*/
/*! See if we can convert an element to an object.
    On input:
     - dc->type_name should be "NI_ELEMENT" or "NI_GROUP"
     - conversion is based on dc->self_name
    On output
     - dc->type_name will be set to dc->self_name
     - data in dc->self_data will be altered, and the NIML element
       will have been destroyed
-----------------------------------------------------------------------------*/

void NI_convert_elm_to_obj( NI_objcontainer *dc )
{
   int cc , nn ;

   if( dc == NULL ) return ;

   if( strcmp(dc->type_name,"NI_ELEMENT") != 0 &&
       strcmp(dc->type_name,"NI_GROUP"  ) != 0   ) return ;

   for( cc=0 ; cc < num_converters ; cc++ )
     if( strcmp(converters[cc].self_name,dc->self_name) == 0 ) break ;

   if( cc == num_converters ) return ;

   nn = converters[cc].to_obj( dc ) ;
   if( nn > 0 )
     NI_strncpy( dc->type_name , dc->self_name , IDCODE_LEN ) ;

   return ;
}
