#include "niml_private.h"

#undef INLINE
#ifdef __GNUC__
# define INLINE inline
#else
# define INLINE /*nada*/
#endif

/*---------------------------------------------------------------*/
/*! Struct to hold on registered entry of a (idcode,pointer,name) triple. */

typedef struct {
  char   idc[32];    /*!< idcode */
  char   ipt[32];    /*!< string representation of a pointer   */
  size_t vlen   ;    /*!< number of bytes stored in vpt        */
  void  *vpt    ;    /*!< the pointer to data (equiv to ipt)   */
  char *name    ;    /*!< arbitrary name associated with above */
} registry_entry ;

#undef  FREE_registry_entry
#define FREE_registry_entry(rr) do{ free(        (rr)->vpt ) ;           \
                                    free((void *)(rr)->name) ;           \
                                    free((void *)(rr)      ) ; } while(0)

/*---------------------------------------------------------------*/

static Htable *registry_htable_idc = NULL ;  /* index by idcode */
static Htable *registry_htable_ipt = NULL ;  /* index by pointer */

/*---------------------------------------------------------------*/
/*! Convert pointer to a string representation. */

static INLINE void vpt_to_char( void *vpt , char *cpt )
{
   sprintf( cpt , "%p" , vpt ) ;
}

/*---------------------------------------------------------------*/
/*! Convert string representation to a pointer. */

static INLINE void * char_to_vpt( char *cpt )
{
   void *vpt=NULL  ;
   if( cpt == NULL ) return NULL ;
   sscanf(cpt,"%p",&vpt) ;
   return vpt ;
}

/*-------------------------------------------------------------------*/
/*! Create the (empty) underlying hash tables
    for indexing the list of registry elements. */

static void init_registry(void)
{
   if( registry_htable_ipt == NULL ){
     registry_htable_idc = new_Htable(131) ;
     registry_htable_ipt = new_Htable(131) ;
   }
   return ;
}

/*-------------------------------------------------------------------*/
/*! Allocate memory with calloc(),
    and associate it with a given idcode and name string.
    Return is NULL is idcode is already used, len is 0,
    or if calloc() fails.
---------------------------------------------------------------------*/

void * NI_registry_malloc( char *idcode , char *name , size_t len )
{
   char *cpt ;
   void *vpt ;
   registry_entry *rent ;  /* pay this or be evicted */

   init_registry() ;       /* setup empty hash tables, if needed */

   if( idcode == NULL || *idcode == '\0' || len == 0 ) return NULL ;

   /* check to see if already have this idcode */

   vpt = findin_Htable( idcode , registry_htable_idc ) ;
   if( vpt != NULL ) return NULL ;               /* bad */

   /* allocate space for result of this function */

   vpt = calloc(1,len) ;
   if( vpt == NULL ) return NULL ;               /* bad */

   rent = calloc(1,sizeof(registry_entry)) ;
   NI_strncpy( rent->idc , idcode , 32 ) ;
   rent->vpt  = vpt ;
   rent->vlen = len ;
   vpt_to_char( vpt , rent->ipt ) ;    /* string version of new pointer */
   if( name == NULL ) name = "NONE" ;
   rent->name = strdup(name) ;

   addto_Htable( rent->idc , (void *)rent , registry_htable_idc ) ;
   addto_Htable( rent->ipt , (void *)rent , registry_htable_ipt ) ;

   return vpt ;
}

/*-------------------------------------------------------------------*/

void * NI_registry_realloc( void *vpt , size_t newlen )
{
   char ipt[32] ;
   void *vpt_new ;
   registry_entry *rent ;

   if( vpt == NULL || registry_htable_ipt == NULL ) return NULL ;

   if( newlen == 0 ){ NI_registry_free( vpt ); return NULL; }

   vpt_to_char( vpt , ipt ) ;
   rent = (registry_entry *) findin_Htable( ipt , registry_htable_ipt ) ;
   if( rent == NULL ) return NULL ;

   vpt_new = realloc( vpt , newlen ) ;  /* get new allocation */
   if( vpt_new == NULL ) return NULL ;
   if( vpt_new == vpt  ) return vpt  ;  /* no change! */

   removefrom_Htable( ipt , registry_htable_ipt ) ;

   rent->vpt = vpt_new ;
   vpt_to_char( vpt , rent->ipt ) ;

   addto_Htable( rent->ipt , (void *)rent , registry_htable_ipt ) ;

   return vpt_new ;
}

/*-------------------------------------------------------------------*/

void NI_registry_free( void *vpt )
{
   char ipt[32] ;
   registry_entry *rent ;

   if( vpt == NULL || registry_htable_ipt == NULL ) return ;

   vpt_to_char( vpt , ipt ) ;
   rent = (registry_entry *) findin_Htable( ipt , registry_htable_ipt ) ;
   if( rent == NULL ) return ;

   removefrom_Htable( rent->ipt , registry_htable_ipt ) ;
   removefrom_Htable( rent->idc , registry_htable_idc ) ;
   FREE_registry_entry( rent ) ;
   return ;
}

/*-------------------------------------------------------------------*/

void * NI_registry_idcode_to_ptr( char *idcode )
{
   registry_entry *rent ;

   rent = (registry_entry *) findin_Htable( idcode , registry_htable_idc ) ;
   if( rent == NULL ) return NULL ;
   return rent->vpt ;
}

/*-------------------------------------------------------------------*/

char * NI_registry_idcode_to_name( char *idcode )
{
   registry_entry *rent ;

   rent = (registry_entry *) findin_Htable( idcode , registry_htable_idc ) ;
   if( rent == NULL ) return NULL ;
   return rent->name ;
}

/*-------------------------------------------------------------------*/

char * NI_registry_ptr_to_idcode( void *vpt )
{
   char ipt[32] ;
   registry_entry *rent ;

   if( vpt == NULL || registry_htable_ipt == NULL ) return ;

   vpt_to_char( vpt , ipt ) ;
   rent = (registry_entry *) findin_Htable( ipt , registry_htable_ipt ) ;
   if( rent == NULL ) return ;
   return rent->idc ;
}

/*-------------------------------------------------------------------*/

char * NI_registry_ptr_to_name( void *vpt )
{
   char ipt[32] ;
   registry_entry *rent ;

   if( vpt == NULL || registry_htable_ipt == NULL ) return ;

   vpt_to_char( vpt , ipt ) ;
   rent = (registry_entry *) findin_Htable( ipt , registry_htable_ipt ) ;
   if( rent == NULL ) return ;
   return rent->name ;
}

/*-------------------------------------------------------------------*/

void NI_registry_idcode_altername( char *idcode , char *newname )
{
   registry_entry *rent ;

   rent = (registry_entry *) findin_Htable( idcode , registry_htable_idc ) ;
   if( rent == NULL ) return ;
   free((void *)rent->name) ;
   if( newname == NULL ) newname = "NONE" ;
   rent->name = strdup(newname) ;
   return ;
}

/*-------------------------------------------------------------------*/

void NI_registry_ptr_altername( void *vpt , char *newname )
{
   char ipt[32] ;
   registry_entry *rent ;

   if( vpt == NULL || registry_htable_ipt == NULL ) return ;

   vpt_to_char( vpt , ipt ) ;
   rent = (registry_entry *) findin_Htable( ipt , registry_htable_ipt ) ;
   if( rent == NULL ) return ;
   free((void *)rent->name) ;
   if( newname == NULL ) newname = "NONE" ;
   rent->name = strdup(newname) ;
   return ;
}
