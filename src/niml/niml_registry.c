#include "niml_private.h"

static Dtable *registry_dtable = NULL ;  /* idcode <-> ptr  */
static Htable *registry_htable = NULL ;  /* idcode  -> name */

/*-------------------------------------------------------------------*/

static void init_registry(void)
{
   if( registry_dtable == NULL ){
     registry_dtable = new_Dtable(127) ;
     registry_htable = new_Htable(127) ;
   }
   return ;
}

/*-------------------------------------------------------------------*/

void * NI_registry_create( char *idcode , char *name , size_t len )
{
   char *cpt ; void *vpt ;

   init_registry() ;

   if( idcode == NULL || *idcode == '\0' || len == 0 ) return NULL ;

   /* check to see if already have this idcode */

   vpt = findin_Htable( idcode , registry_htable ) ;
   if( vpt != NULL ) return NULL ;               /* bad */

   /* allocate space for result */

   vpt = calloc(1,len) ;
   if( vpt == NULL ) return NULL ;               /* bad */

   /* create copy of name string, put in Htable */

   if( name != NULL ) cpt = strdup(name)   ;
   else               cpt = strdup("NONE") ;
   if( cpt == NULL ){ free(vpt); return NULL; }  /* bad */

   addto_Htable( idcode , (void *)cpt , registry_htable ) ;

   /* make string from pointer, put into Dtable */

   cpt = (char *)malloc(32) ;
   sprintf( cpt , "%p" , vpt ) ;

   addto_Dtable( idcode , cpt , registry_dtable ) ;

   return vpt ;
}

/*-------------------------------------------------------------------*/

void * NI_registry_realloc( void *ptr , size_t newlen )
{
}

/*-------------------------------------------------------------------*/

void * NI_registry_free( void *ptr )
{
}

/*-------------------------------------------------------------------*/

void * NI_registry_idcode_to_ptr( char *idcode )
{
   char *cpt ; void *vpt=NULL ;

   cpt = findin_Dtable_a( idcode , registry_dtable ) ;
   if( cpt == NULL || *cpt == '\0' ) return NULL ;
   sscanf(cpt,"%p",&vpt) ;
   return vpt ;
}

/*-------------------------------------------------------------------*/

char * NI_registry_idcode_to_name( char *idcode )
{
   char *cpt ;
   cpt = (char *) findin_Htable( idcode , registry_htable ) ;
   return cpt ;
}

/*-------------------------------------------------------------------*/

char * NI_registry_ptr_to_idcode( void *vpt )
{
   char *cpt , *dpt ;

   if( registry_dtable == NULL || vpt == NULL ) return NULL ;

   cpt = (char *)malloc(32) ;
   sprintf( cpt , "%p" , vpt ) ;
   dpt = findin_Dtable_b( cpt , registry_dtable ) ;
   free((void *)cpt) ;
   return dpt ;
}

/*-------------------------------------------------------------------*/

char * NI_registry_ptr_to_name( void *vpt )
{
   char *idc , *cpt ;

   idc = NI_registry_ptr_to_idcode( vpt ) ;
   cpt = (char *) findin_Htable( idc , registry_htable ) ;
   return cpt ;
}
