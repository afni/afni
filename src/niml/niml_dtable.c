#include "niml_private.h"
#include <math.h>

/******************************************************************/
/**** Dtable: string-string pairs (two Htables) -- 15 Oct 2003 ****/
/******************************************************************/

/*-----------------------------------------------------------------*/
/*! Create a Dtable with len slots.
-------------------------------------------------------------------*/

Dtable * new_Dtable( int len )
{
   Dtable *dt ;
   dt = (Dtable *) calloc( 1 , sizeof(Dtable) ) ;
   dt->hta = new_Htable( len ) ;
   dt->htb = new_Htable( len ) ;
   return dt ;
}

/*-----------------------------------------------------------------*/
/*! Death and destruction of a Dtable.
-------------------------------------------------------------------*/

void destroy_Dtable( Dtable *dt )
{
   if( dt == NULL ) return ;
   Htable_set_vtkill(1) ;
   destroy_Htable( dt->hta ) ;
   destroy_Htable( dt->htb ) ;
   Htable_set_vtkill(0) ; ;
   return ;
}

/*-----------------------------------------------------------------*/
/*! Insert string pair str_a,str_b into the Dtable.
    Copies of the strings are made.
-------------------------------------------------------------------*/

void addto_Dtable( char *str_a , char *str_b , Dtable *dt )
{
   char *sa , *sb ;
   if( dt == NULL || str_a == NULL || str_b == NULL ) return ;
   sa = strdup(str_a) ; sb = strdup(str_b) ;
   addto_Htable( sa , (void *)sb , dt->hta ) ;
   addto_Htable( sb , (void *)sa , dt->htb ) ;
   return ;
}

/*-----------------------------------------------------------------*/

char * findin_Dtable_a( char *str_a , Dtable *dt )
{
   if( dt == NULL || str_a == NULL ) return NULL ;
   return (char *)findin_Htable( str_a , dt->hta ) ;
}

/*-----------------------------------------------------------------*/

char * findin_Dtable_b( char *str_b , Dtable *dt )
{
   if( dt == NULL || str_b == NULL ) return NULL ;
   return (char *)findin_Htable( str_b , dt->htb ) ;
}

/*-----------------------------------------------------------------*/

void removefrom_Dtable_a( char *str_a , Dtable *dt )
{
   char *str_bb , *str_aa ;
   if( dt == NULL ) return ;
   str_bb = (char *)findin_Htable( str_a , dt->hta ) ;
   if( str_bb == NULL ) return ;
   str_aa = (char *)findin_Htable( str_bb, dt->htb ) ;
   removefrom_Htable( str_a , dt->hta ) ;
   removefrom_Htable( str_bb, dt->htb ) ;

   /* must also remove dangling targets from each Htable */

   free((void *)str_bb) ; if( str_aa != NULL ) free((void *)str_aa) ;
   return ;
}

/*-----------------------------------------------------------------*/

void removefrom_Dtable_b( char *str_b , Dtable *dt )
{
   char *str_aa , *str_bb ;
   if( dt == NULL ) return ;
   str_aa = (char *)findin_Htable( str_b , dt->htb ) ;
   if( str_aa == NULL ) return ;
   str_bb = (char *)findin_Htable( str_aa, dt->hta ) ;
   removefrom_Htable( str_b , dt->htb ) ;
   removefrom_Htable( str_aa, dt->hta ) ;

   free((void *)str_aa) ; if( str_bb != NULL ) free((void *)str_bb) ;
   return ;
}

/*-----------------------------------------------------------------*/
/* Create lists of the pointers to the strings directly inside
   the Dtable.  Return value is number of string pairs.  Since
   these are pointers INTO the Dtable, don't free() these!
   However, you should free(*list_a) and free(*list_b) when done.
-------------------------------------------------------------------*/

int listize_Dtable( Dtable *dt , char ***list_a , char ***list_b )
{
   char **la=NULL , **lb=NULL , *sa,*sb ;
   int jj,kk,nn ;
   Htable *ht ;

   if( dt == NULL || list_a == NULL || list_b == NULL ) return 0 ;

   ht = dt->hta ;

   for( nn=jj=0 ; jj < ht->len ; jj++ ){
     if( ht->vtab[jj] == NULL ) continue ;
     for( kk=0 ; kk < ht->ntab[jj] ; kk++ ){
       sa = (char *) ht->ctab[jj][kk] ; if( sa == NULL ) continue ;
       sb = (char *) ht->vtab[jj][kk] ; if( sb == NULL ) continue ;
       la = (char **) realloc( (void *)la , sizeof(char *)*(nn+1) ) ;
       lb = (char **) realloc( (void *)lb , sizeof(char *)*(nn+1) ) ;
       la[nn] = sa ; lb[nn] = sb ; nn++ ;
     }
   }
   *list_a = la ; *list_b = lb ; return nn ;
}

/*---------------------------------------------------------------------------*/
/* Create a string that encodes all of a Dtable.  free() this when done.
-----------------------------------------------------------------------------*/

char * Dtable_to_nimlstring( Dtable *dt , char *name )
{
   int nn , ii ;
   char **la , **lb , *stout ;
   NI_element *nel ;
   NI_stream ns ;

   nn = listize_Dtable( dt , &la , &lb ) ;
   if( nn == 0 || la == NULL || lb == NULL ) return (char *)NULL ;

   if( name == NULL || *name == '\0' ) name = "Dtable" ;

   nel = NI_new_data_element( name , nn ) ;
   NI_add_column( nel , NI_STRING , la ) ;
   NI_add_column( nel , NI_STRING , lb ) ;
   free(la) ; free(lb) ;

   ns = NI_stream_open( "str:" , "w" ) ;
   (void) NI_write_element( ns , nel , NI_TEXT_MODE ) ;
   NI_free_element( nel ) ;
   stout = strdup( NI_stream_getbuf(ns) ) ;
   NI_stream_close( ns ) ;
   nn = strlen(stout) ;
   for( ii=nn-1 ; ii > 0 && isspace(stout[ii]) ; ii-- ) ; /* trailing blanks */
   stout[ii+1] = '\0' ;
   return stout ;
}

/*---------------------------------------------------------------------------*/

Dtable * Dtable_from_nimlstring( char *nstr )
{
   NI_stream ns ;
   NI_element *nel ;
   int nn , ii ;
   Dtable *dt ;
   char **la , **lb ;

   if (!(nel = (NI_element *)NI_read_element_fromstring(nstr))) {
      return NULL;
   }

   /* see if element is OK for this purpose */

   if( NI_element_type(nel) != NI_ELEMENT_TYPE ){
     NI_free_element(nel) ; return NULL ;
   }

   if( nel->vec_len    <  1         ||  /* empty element?             */
       nel->vec_filled <  1         ||  /* no data was filled in?     */
       nel->vec_num    <  2         ||  /* less than 4 columns?       */
       nel->vec_typ[0] != NI_STRING ||  /* must be String, String     */
       nel->vec_typ[1] != NI_STRING   ){

     NI_free_element(nel) ; return NULL ;
   }

   la = (char **) nel->vec[0] ;  /* first column of String */
   lb = (char **) nel->vec[1] ;  /* second column of String */

   nn = nel->vec_filled ;
   ii = rint(sqrt(2*nn+1.0l)) ;
   if( ii < 7 ) ii = 7 ; else if( ii%2 == 0 ) ii++ ;

   /* make table, insert strings */

   dt = new_Dtable( ii ) ;
   for( ii=0 ; ii < nn ; ii++ )
     addto_Dtable( la[ii] , lb[ii] , dt ) ;

   NI_free_element(nel) ; return dt ;
}
