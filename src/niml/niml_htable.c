#include "niml_private.h"

/******************************************************************************/
/*********** Hash table functions (string/pointer pair) [26 Aug 2002] *********/
/******************************************************************************/

#undef  UINT
#define UINT unsigned int

#undef INLINE
#ifdef __GNUC__
# define INLINE inline
#else
# define INLINE /*nada*/
#endif

static int vtkill=0 ;
void Htable_set_vtkill( int vt ){ vtkill = vt ; }

/*---------------------------------------------------------*/
/*! Compute a non-negative integer key from a string.
-----------------------------------------------------------*/

static INLINE UINT hashkey( char *str )
{
  char *p ;
  unsigned int h=32003 ;

  for( p=str ; *p != '\0' ; p++ )
    h = ( h << 5 ) - h + *p ;

#if 0
  h =   ((h & 0xf0f0f0f0) >> 4)   /* swap nibbles */
      | ((h & 0x0f0f0f0f) << 4) ; /* just for fun */
#endif

  return h;
}

/*-------------------------------------------------------*/
/*! Create a new Htable, with len slots.
---------------------------------------------------------*/

Htable * new_Htable( int len )
{
   Htable *ht ;

        if( len   <= 7 ) len = 7 ;  /* smallest allowed */
   else if( len%2 == 0 ) len++   ;  /* mustn't be even */

   ht = (Htable *) calloc( 1 , sizeof(Htable) ) ;

   ht->len  = len ;
   ht->vtab = (void ***) calloc( len , sizeof(void **) ) ;
   ht->ctab = (char ***) calloc( len , sizeof(char **) ) ;
   ht->ntab = (int *)    calloc( len , sizeof(int)     ) ;

   return ht ;
}

/*---------------------------------------------------------*/
/*! Delete a Htable forever.
-----------------------------------------------------------*/

void destroy_Htable( Htable *ht )
{
   int jj , kk ;

   if( ht == NULL ) return ;

   for( jj=0 ; jj < ht->len ; jj++ ){
     if( ht->vtab[jj] != NULL ){
       if( vtkill ){
         for( kk=0 ; kk < ht->ntab[jj] ; kk++ )
           if( ht->vtab[jj][kk] != NULL ) free(ht->vtab[jj][kk]) ;
       }
       free(ht->vtab[jj]) ;
     }
     if( ht->ctab[jj] != NULL ){
       for( kk=0 ; kk < ht->ntab[jj] ; kk++ )
         if( ht->ctab[jj][kk] != NULL ) free(ht->ctab[jj][kk]) ;
       free(ht->ctab[jj]) ;
     }
   }
   free(ht->vtab) ; free(ht->ctab) ; free(ht->ntab) ; free(ht) ;
   return ;
}

/*-------------------------------------------------------*/
/* Find a string in the hash table;
   - Returns the pointer it was deposited with, or NULL.
   - Also returns NULL if inputs are illegal.
---------------------------------------------------------*/

void * findin_Htable( char *str , Htable *ht )
{
   UINT jj ;
   int kk , ntab ;
   char *key , **ctab ;
   void ***vtab ;

   if( str == NULL || ht == NULL || ht->ntot == 0 ) return NULL ;

   jj = hashkey(str) % ht->len ;      /* hash table row */

   vtab = ht->vtab ;

   if( vtab[jj] == NULL ) return NULL ;  /* nothing there */

   key = str ;

   ctab = ht->ctab[jj] ; ntab = ht->ntab[jj] ;

   for( kk=0 ; kk < ntab ; kk++ )   /* scan for match of key to ctab */
     if( ctab[kk] != NULL && strcmp(key,ctab[kk]) == 0 )
       return vtab[jj][kk];

   return NULL ; /* no match found */
}

/*--------------------------------------------------------*/
/*! Add a string/pointer pair to a hash table.
    - If the ptr is NULL, this will remove the string/ptr
      pair from the table.
    - If you insert with the same string twice, then the
      second time will overwrite the 1st time.
----------------------------------------------------------*/

void addto_Htable( char *str , void *vpt , Htable *ht )
{
   UINT jj ;
   int kk , ll=-1 ;
   char *key ;

   /* check for bad inputs */

   if( str == NULL || ht == NULL ) return ;

   if( vpt == NULL ){ removefrom_Htable( str , ht ) ; return ; }

   jj = hashkey(str) % ht->len ;      /* hash table row */

   key = strdup(str) ;                /* internal key string */

   if( ht->vtab[jj] == NULL ){        /* create this row in table */

     ht->vtab[jj] = (void **) calloc(3,sizeof(void *)) ;
     ht->ctab[jj] = (char **) calloc(3,sizeof(char *)) ;
     ht->ntab[jj] = 3 ;    /* made 2 extra entries */

     ht->vtab[jj][0] = vpt ;   /* save pointer */
     ht->ctab[jj][0] = key ;   /* save key string */
     ht->ntot ++ ;             /* 1 more in table */

   } else {                           /* search this row */

     for( kk=0 ; kk < ht->ntab[jj] ; kk++ ){
            if( ht->ctab[jj][kk] == NULL         ){ if(ll < 0) ll=kk; } /* add here? */
       else if( strcmp(key,ht->ctab[jj][kk]) == 0 ) break ;             /* found it? */
     }

     if( kk == ht->ntab[jj] ){   /* didn't find str in row already */

       if( ll >= 0 ){         /* have a NULL slot from scan above */

         ht->vtab[jj][ll] = vpt ;  /* save ptr */
         ht->ctab[jj][ll] = key ;  /* save key string */
         ht->ntot ++ ;             /* 1 more in table */

       } else {               /* must make row longer */

         ht->vtab[jj] = (void **) realloc( ht->vtab[jj] , (kk+3)*sizeof(void *)) ;
         ht->ctab[jj] = (char **) realloc( ht->ctab[jj] , (kk+3)*sizeof(char *)) ;
         ht->ntab[jj] = kk+3 ;

         ht->vtab[jj][kk] = vpt ;  /* save ptr */
         ht->ctab[jj][kk] = key ;  /* save key string */
         ht->ntot ++ ;             /* 1 more in table */

         ht->vtab[jj][kk+1] = ht->vtab[jj][kk+2] = NULL ;  /* created 2 extra */
         ht->ctab[jj][kk+1] = ht->ctab[jj][kk+2] = NULL ;  /* elements above */

       }

     } else {                    /* found str in row at index kk */

       if( vtkill && ht->vtab[jj][kk] != NULL ) free(ht->vtab[jj][kk]) ;

       ht->vtab[jj][kk] = vpt ;  /* replace old ptr with new */
       free(key) ;               /* don't need this */
     }
   }
}

/*--------------------------------------------------------*/
/*! Remove an entry from a Htable.
----------------------------------------------------------*/

void removefrom_Htable( char *str , Htable *ht )
{
   UINT jj ;
   int kk ;
   char *key ;
   void ***vtab ;
   char **ctab ;
   int  ntab ;

   if( str == NULL || ht == NULL || ht->ntot == 0 ) return ;

   jj = hashkey(str) % ht->len ;      /* hash table row */

   vtab = ht->vtab ;

   if( vtab[jj] == NULL ) return ;    /* nothing there */

   key = str ;

   ctab = ht->ctab[jj] ; ntab = ht->ntab[jj] ;

   for( kk=0 ; kk < ntab ; kk++ )   /* scan for match of key to ctab */
     if( ctab[kk] != NULL && strcmp(key,ctab[kk]) == 0 ){
       free(ctab[kk]); ctab[kk] = NULL;
       if( vtkill && vtab[jj][kk] != NULL ) free(vtab[jj][kk]) ;
       vtab[jj][kk] = NULL; ht->ntot--; break;
     }

   return ;
}

/*-----------------------------------------------------------------*/
/*! Profile a Htable to stdout.
-------------------------------------------------------------------*/

void profile_Htable( char *str, Htable *ht )
{
   int jj, kk , nn ;

   printf("\n----- Htable profile: %s\n",(str != NULL) ? str : "" ) ;
   if( ht == NULL ){
     printf("++ EMPTY ++\n") ; return ;
   }

   printf("Rows=%d  Ntot=%d\n",ht->len,ht->ntot) ;

   for( jj=0 ; jj < ht->len ; jj++ ){
     printf(" #%05d: ",jj) ;
     if( ht->vtab[jj] == NULL ){
       printf("++ EMPTY ++\n") ;
     } else {
       for( nn=kk=0 ; kk < ht->ntab[jj] ; kk++ ){
         if( ht->ctab[jj][kk] != NULL ){ printf("*") ; nn++ ; }
         else                          { printf(".") ;        }
       }
       printf(" [ntab=%d nn=%d]\n",ht->ntab[jj],nn) ;
     }
   }
   fflush(stdout) ;
}

/*-----------------------------------------------------------------*/
/*! Put contents of htold into htnew.
-------------------------------------------------------------------*/

void subsume_Htable( Htable *htold , Htable *htnew )
{
   int kk,jj ;

   /* check inputs for sanity */

   if( htold == NULL || htold->ntot == 0 || htnew == NULL ) return ;

   for( jj=0 ; jj < htold->len ; jj++ ){
     if( htold->vtab[jj] != NULL ){
       for( kk=0 ; kk < htold->ntab[jj] ; kk++ )
         if( htold->ctab[jj][kk] != NULL )
           addto_Htable( htold->ctab[jj][kk] , htold->vtab[jj][kk] , htnew ) ;
     }
   }
}

/*-----------------------------------------------------------------*/
/*! Resize the guts of a Htable [28 Feb 2005]
-------------------------------------------------------------------*/

void resize_Htable( int newlen , Htable *ht )
{
   Htable *htnew ;
   int jj , kk ;

   if( ht == NULL ) return ;

   /* auto-resize? */

   if( newlen == 0 ){
     if( ht->ntot <= 131 * ht->len ) return ;
     newlen = ht->ntot / 37 ;
   }

   /* create new Htable, copy contents of this one into it */

   htnew = new_Htable( newlen ) ;
   if( htnew == NULL ) return ;

   subsume_Htable( ht , htnew ) ;

   /* erase contents of this one now */

   for( jj=0 ; jj < ht->len ; jj++ ){
     if( ht->vtab[jj] != NULL ) free(ht->vtab[jj]) ;

     if( ht->ctab[jj] != NULL ){
       for( kk=0 ; kk < ht->ntab[jj] ; kk++ )
         if( ht->ctab[jj][kk] != NULL ) free(ht->ctab[jj][kk]) ;
       free(ht->ctab[jj]) ;
     }
   }
   free(ht->vtab) ; free(ht->ctab) ; free(ht->ntab) ;

   /* copy guts of new Htable over the guts of this one */

   *ht = *htnew ;

   /* free the shell of the new Htable and exit */

   free((void *)htnew) ; return ;
}
