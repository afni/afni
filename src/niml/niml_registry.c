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
  char   idc[32];    /*!< idcode string                        */
  char   ipt[32];    /*!< string representation of a pointer   */
  size_t vlen   ;    /*!< number of bytes stored in vpt        */
  int    flags  ;    /*!< various bit flags                    */
  void  *vpt    ;    /*!< the pointer to data (equiv to ipt)   */
  char *name    ;    /*!< arbitrary name associated with above */
} registry_entry ;

/* Some masks and macros for "flags" */

#undef  NIREG_PRIVATE_MALLOC
#define NIREG_PRIVATE_MALLOC (1<<0)

#undef  NIREG_isprivate
#define NIREG_isprivate(rr) (((rr)->flags & NIREG_PRIVATE_MALLOC) != 0)

#undef  NIREG_free
#define NIREG_free(rr)                                \
  do{ if( !NIREG_isprivate(rr) ) free( (rr)->vpt ) ;  \
      free((void *)(rr)->name) ;                      \
      free((void *)(rr)) ;                            \
  } while(0)

/*---------------------------------------------------------------*/
/* These Htables index registry_entry by idcode and pointer.     */

static Htable *registry_htable_idc = NULL ;  /* index by idcode */
static Htable *registry_htable_ipt = NULL ;  /* index by pointer */

/*---------------------------------------------------------------*/
/*! Convert pointer to a string representation. */

static INLINE void vpt_to_char( void *vpt , char *cpt )
{
   sprintf( cpt , "%p" , vpt ) ;
}

/*---------------------------------------------------------------*/
/*! Convert string representation to a pointer;
    this is the inverse function to vpt_to_char(). */

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
    - Return is NULL is idcode is already used, or if calloc() fails.
    - If len=0, then 1 byte will be malloc-ed, but this special
      case is a flag that no data will actually be available at
      this location -- cf. functions NI_registry_idcode_to_len()
      and NI_registry_ptr_to_len.
    - If name==NULL, the empty string "\0" is actually stored.
---------------------------------------------------------------------*/

void * NI_registry_malloc( char *idcode , char *name , size_t len )
{
   void *vpt ;
   int   lll ;
   registry_entry *rent ;  /* pay this or be evicted */

   init_registry() ;       /* setup empty hash tables, if needed */

   if( idcode == NULL || *idcode == '\0' ) return NULL ;

   /* check to see if already have this idcode */

   vpt = findin_Htable( idcode , registry_htable_idc ) ;
   if( vpt != NULL ) return NULL ;               /* bad */

   /* allocate space for result of this function */

   lll = (len == 0) ? 4 : len ;
   vpt = calloc(1,lll) ;
   if( vpt == NULL ) return NULL ;               /* bad */

   if( len == 0 ){ char *cpt=(char *)vpt; *cpt = '\0'; }

   /* make the registry entry for this doohicky */

   rent = calloc(1,sizeof(registry_entry)) ;
   NI_strncpy( rent->idc , idcode , 32 ) ;               /* copy idcode */
   rent->vpt  = vpt ;                              /* copy data pointer */
   rent->vlen = len ;                                    /* save length */
   vpt_to_char( vpt , rent->ipt ) ;   /* string version of data pointer */
   if( name == NULL ) name = "\0" ;
   rent->name  = strdup(name) ;                            /* copy name */
   rent->flags = 0 ;                                      /* init flags */

   /* and index this new registry entry under the idcode and the pointer */

   addto_Htable( rent->idc , (void *)rent , registry_htable_idc ) ;
   addto_Htable( rent->ipt , (void *)rent , registry_htable_ipt ) ;

   return vpt ;   /* give the user the pointer he asked for */
}

/*-------------------------------------------------------------------*/
/*! Associate a given pointer (non-NULL) with idcode and name string.
     - Return value is vpt if things are OK, NULL if they are not.
     - Not OK if vpt==NULL, or idcode is already in table.
     - If vpt is already in table, then it's old entry will.
       be lost and the new idcode will win (that is, you can't have
       two different idcodes associated with the same data pointer).
---------------------------------------------------------------------*/

void * NI_registry_add( char *idcode , char *name , void *vpt )
{
   void *xpt ;
   registry_entry *rent ;  /* pay this or be evicted */

   init_registry() ;       /* setup empty hash tables, if needed */

   if( idcode == NULL || *idcode == '\0' || vpt == NULL ) return NULL ;

   /* check to see if already have this idcode */

   xpt = findin_Htable( idcode , registry_htable_idc ) ;
   if( xpt != NULL ) return NULL ;  /* bad */

   /* make the registry entry for this doohicky */

   rent = calloc(1,sizeof(registry_entry)) ;
   NI_strncpy( rent->idc , idcode , 32 ) ;               /* copy idcode */
   rent->vpt  = vpt ;                              /* copy data pointer */
   rent->vlen = 0   ;                                     /* set length */
   vpt_to_char( vpt , rent->ipt ) ;   /* string version of data pointer */
   if( name == NULL ) name = "\0" ;
   rent->name  = strdup(name) ;                            /* copy name */
   rent->flags = NIREG_PRIVATE_MALLOC ;                   /* init flags */

   /* and index this new registry entry under the idcode and the pointer */

   addto_Htable( rent->idc , (void *)rent , registry_htable_idc ) ;
   addto_Htable( rent->ipt , (void *)rent , registry_htable_ipt ) ;

   return vpt ;   /* give the user the pointer he asked for */
}

/*-------------------------------------------------------------------*/
/*! Like realloc(), but also updates the indexes.
    - However, you can't call this with vpt==NULL,
      since you aren't supplying an idcode.
    - Calling this with newlen==0 is like calling NI_registry_malloc()
      with len==0.
    - Calling this with an entry created via NI_registry_add()
      will return NULL, since that function is used for registering
      things that aren't to be malloc-ed by this library.
---------------------------------------------------------------------*/

void * NI_registry_realloc( void *vpt , size_t newlen )
{
   char ipt[32] ;
   void *vpt_new ;
   int lll ;
   registry_entry *rent ;

   if( vpt == NULL || registry_htable_ipt == NULL ) return NULL ;

   /* look up the pointer in the index */

   vpt_to_char( vpt , ipt ) ;
   rent = (registry_entry *) findin_Htable( ipt , registry_htable_ipt ) ;
   if( rent == NULL          ) return NULL ;   /* not found!? */
   if( NIREG_isprivate(rent) ) return NULL ;   /* bad user */

   lll = (newlen == 0) ? 4 : newlen ;
   vpt_new = realloc( vpt , lll ) ;  /* get new allocation */
   if( vpt_new == NULL ) return NULL ;  /* bad */
   if( vpt_new == vpt  ) return vpt  ;  /* no change! */

   /* remove the pointer-based entry from the index,
      then make a new pointer index                 */

   removefrom_Htable( ipt , registry_htable_ipt ) ;

   rent->vpt  = vpt_new ;
   rent->vlen = newlen ;
   vpt_to_char( vpt , rent->ipt ) ;
   addto_Htable( rent->ipt , (void *)rent , registry_htable_ipt ) ;

   return vpt_new ;  /* give back the new pointer */
}

/*-------------------------------------------------------------------*/
/*! For something added with NI_registry_add(), lets you replace
    the pointer with some other pointer.
---------------------------------------------------------------------*/

void * NI_registry_replace( void *vpt , void *vpt_new )
{
   char ipt[32] ;
   registry_entry *rent ;

   if( vpt == NULL || vpt_new == NULL ||
       registry_htable_ipt == NULL      ) return NULL ;

   if( vpt == vpt_new ) return vpt ;

   /* look up the pointer in the index */

   vpt_to_char( vpt , ipt ) ;
   rent = (registry_entry *) findin_Htable( ipt , registry_htable_ipt ) ;
   if( rent == NULL ) return NULL ;   /* not found!? */

   if( !NIREG_isprivate(rent) ) free((void *)vpt) ;

   /* remove the pointer-based entry from the index,
      then make a new pointer index                 */

   removefrom_Htable( ipt , registry_htable_ipt ) ;

   rent->vpt  = vpt_new ;
   rent->vlen = 0 ;                   /* len is unknown here */
   vpt_to_char( vpt , rent->ipt ) ;
   addto_Htable( rent->ipt , (void *)rent , registry_htable_ipt ) ;
   rent->flags = NIREG_PRIVATE_MALLOC ;

   return vpt_new ;  /* give back the new pointer */
}

/*-------------------------------------------------------------------*/
/* Like free() but also de-indexes the thing. */

void NI_registry_free( void *vpt )
{
   char ipt[32] ;
   registry_entry *rent ;

   if( vpt == NULL || registry_htable_ipt == NULL ) return ;

   /* look for the pointer in the index */

   vpt_to_char( vpt , ipt ) ;
   rent = (registry_entry *) findin_Htable( ipt , registry_htable_ipt ) ;
   if( rent == NULL ) return ;   /* stupid users must be punished somehow */

   removefrom_Htable( rent->ipt , registry_htable_ipt ) ;
   removefrom_Htable( rent->idc , registry_htable_idc ) ;
   NIREG_free( rent ) ;
   return ;
}

/*-------------------------------------------------------------------*/
/*! Given an idcode, get the data pointer that goes with it. */

void * NI_registry_idcode_to_ptr( char *idcode )
{
   registry_entry *rent ;

   rent = (registry_entry *) findin_Htable( idcode , registry_htable_idc ) ;
   if( rent == NULL ) return NULL ;
   return rent->vpt ;
}

/*-------------------------------------------------------------------*/
/*! Given an idcode, get the data length that goes with it.
    Note that 0 is returned if the data ptr was setup with len=0
    *OR* if the idcode can't be found in the registry.
---------------------------------------------------------------------*/

size_t NI_registry_idcode_to_len( char *idcode )
{
   registry_entry *rent ;

   rent = (registry_entry *) findin_Htable( idcode , registry_htable_idc ) ;
   if( rent == NULL ) return 0 ;
   return rent->vlen ;
}

/*-------------------------------------------------------------------*/
/*! Given a data pointer, get the data length that goes with it.
    Note that 0 is returned if the data ptr was setup with len=0
    *OR* if the data ptr can't be found in the registry.
---------------------------------------------------------------------*/

size_t NI_registry_ptr_to_len( void *vpt )
{
   char ipt[32] ;
   registry_entry *rent ;

   if( vpt == NULL || registry_htable_ipt == NULL ) return 0 ;

   vpt_to_char( vpt , ipt ) ;
   rent = (registry_entry *) findin_Htable( ipt , registry_htable_ipt ) ;
   if( rent == NULL ) return 0 ;
   return rent->vlen ;
}

/*-------------------------------------------------------------------*/
/*! Given an idcode, get the name string that went with it.
    This is the pointer into the internal registry_entry struct, so
    don't modify it!
---------------------------------------------------------------------*/

char * NI_registry_idcode_to_name( char *idcode )
{
   registry_entry *rent ;

   rent = (registry_entry *) findin_Htable( idcode , registry_htable_idc ) ;
   if( rent == NULL ) return NULL ;
   return rent->name ;
}

/*-------------------------------------------------------------------*/
/*! Given a data pointer, return a pointer to the idcode that
    corresponds.  Don't modify this!
---------------------------------------------------------------------*/

char * NI_registry_ptr_to_idcode( void *vpt )
{
   char ipt[32] ;
   registry_entry *rent ;

   if( vpt == NULL || registry_htable_ipt == NULL ) return NULL ;

   vpt_to_char( vpt , ipt ) ;
   rent = (registry_entry *) findin_Htable( ipt , registry_htable_ipt ) ;
   if( rent == NULL ) return NULL ;
   return rent->idc ;
}

/*-------------------------------------------------------------------*/
/*! Given a data pointer, get the name string that corresponds. */

char * NI_registry_ptr_to_name( void *vpt )
{
   char ipt[32] ;
   registry_entry *rent ;

   if( vpt == NULL || registry_htable_ipt == NULL ) return NULL ;

   vpt_to_char( vpt , ipt ) ;
   rent = (registry_entry *) findin_Htable( ipt , registry_htable_ipt ) ;
   if( rent == NULL ) return NULL ;
   return rent->name ;
}

/*-------------------------------------------------------------------*/
/*! Given an idcode, modify the name string that goes with it. */

void NI_registry_idcode_altername( char *idcode , char *newname )
{
   registry_entry *rent ;

   rent = (registry_entry *) findin_Htable( idcode , registry_htable_idc ) ;
   if( rent == NULL ) return ;
   free((void *)rent->name) ;
   if( newname == NULL ) newname = "\0" ;
   rent->name = strdup(newname) ;
   return ;
}

/*-------------------------------------------------------------------*/
/*! Given a data pointer, alter the name string that goes with it. */

void NI_registry_ptr_altername( void *vpt , char *newname )
{
   char ipt[32] ;
   registry_entry *rent ;

   if( vpt == NULL || registry_htable_ipt == NULL ) return ;

   vpt_to_char( vpt , ipt ) ;
   rent = (registry_entry *) findin_Htable( ipt , registry_htable_ipt ) ;
   if( rent == NULL ) return ;
   free((void *)rent->name) ;
   if( newname == NULL ) newname = "\0" ;
   rent->name = strdup(newname) ;
   return ;
}
