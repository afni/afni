#include "niml_private.h"

/**** Tables for ni_do callbacks *****/

static int       doer_num  = 0    ;
static char    **doer_verb = NULL ;
static NI_doer **doer_func = NULL ;

/*---------------------------------------------------------------------------*/
/*! Register a callback for a "ni_do" verb.  [12 Feb 2003]
-----------------------------------------------------------------------------*/

void NI_register_doer( char *verb , NI_doer *func )
{
   int ii ;

   if( verb == NULL || *verb == '\0' ) return ;

   /* see if verb already in table */

   for( ii=0 ; ii < doer_num ; ii++ )
     if( strcmp(verb,doer_verb[ii]) == 0 ) break ;

   /* if was in table, replace func */

   if( ii < doer_num ){
     doer_func[ii] = func ; return ;
   }

   if( func == NULL ) return ;   /* quit if no func */

   /* expand table */

   ii = doer_num ; doer_num++ ;

   doer_verb = NI_realloc( doer_verb , sizeof(char *)*doer_num ) ;
   doer_verb[ii] = NI_strdup(verb) ;

   doer_func = NI_realloc( doer_func , sizeof(NI_doer *)*doer_num ) ;
   doer_func[ii] = func ;
   return ;
}

/*---------------------------------------------------------------------------*/
/*! Carry out an action ordered by a "ni_do" element received on
    the input stream.  Actions we know about:

   - ni_verb='reopen_this' => open the stream anew and replace it [23 Aug 2002]
   - ni_verb='close_this'  => close this stream down              [20 Dec 2002]

    Return value is -1 if an error occurs, 0 if things are cool.
-----------------------------------------------------------------------------*/

int NI_do( NI_stream_type *ns , NI_element *nel )
{
   char *verb , *object ;
   int ii ;

   /*- check inputs for OK-ositiness -*/

   if( ns == NULL || nel == NULL || nel->type != NI_ELEMENT_TYPE ) return -1 ;

   if( strcmp(nel->name,"ni_do") != 0 ) return -1 ;

   verb   = NI_get_attribute( nel , "ni_verb"   ) ;
   object = NI_get_attribute( nel , "ni_object" ) ;

   if( verb == NULL || verb[0] == '\0' ) return -1 ;        /* need a verb;  */
                                                           /* but not always */
                                                          /* need an object  */
   /***********************************/
   /*---- check for various verbs ----*/
   /***********************************/

   if( strcmp(verb,"reopen_this") == 0 ){  /****----- reopen stream ------****/

     NI_stream_type *nsnew ;

     if( object == NULL || object[0] == '\0' ) return -1 ;  /* bad */

     nsnew = NI_stream_open( object , "r" ) ;             /* open new stream */
     if( nsnew == NULL ) return -1 ;                                  /* bad */

     NI_stream_close_keep(ns,0) ;                        /* trash old stream */
     *ns = *nsnew; NI_free(nsnew);                       /* replace old guts */
     return 0 ;

   } /****------------------------- end reopen --------------------------*****/

   if( strcmp(verb,"close_this") == 0 ){  /****---- close this stream ----****/

     NI_stream_close_keep(ns,0);                   /* close and mark as dead */
     return 0 ;

   } /****------------------------ end close_this ------------------------****/

   if( strcmp(verb,"typedef") == 0 ){    /****---- define a NIML type ----****/

     char tnam[256] , tdef[8200] ;
     int tt ;

     if( object == NULL || object[0] == '\0' ) return -1 ;  /* bad */

     tnam[0] = tdef[0] = '\0' ;
     sscanf(object,"%255s %8199s",tnam,tdef) ;
     tt = NI_rowtype_define( tnam , tdef ) ;
     return (tt >0) ? 0 : -1 ;

   } /****------------------------ end typedef ---------------------------****/

   /**** Here, check for user-defined callbacks ****/

   for( ii=0 ; ii < doer_num ; ii++ ){
     if( strcmp(verb,doer_verb[ii]) == 0 ){
       if( doer_func[ii] != NULL ) doer_func[ii]( nel ) ;
       return 0 ;
     }
   }

   /*--- if we get here, we got a verb we don't recognize ---*/

   return -1 ;
}
