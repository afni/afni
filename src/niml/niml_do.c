#include "niml_private.h"

/**** Tables for ni_do callbacks *****/

static int           doer_num  = 0    ;
static char        **doer_verb = NULL ;
static NI_voidfunc **doer_func = NULL ;

/*---------------------------------------------------------------------------*/
/*! Register a callback for a "ni_do" verb.  [12 Feb 2003]

    The function will be called like so
      - func( char *object , NI_stream_type *ns , NI_element *nel ) ;
      - object = RHS of ni_object attribute (may be NULL)
      - ns = stream that sent the message (so you can reply, if you like)
      - nel = the data element that contained the message (if you need it)

    Calling with the same verb will replace the function - you can't have
    two callbacks for the same verb.  If func is input as NULL, then this
    will remove a callback, if it was defined earlier; if func is NULL and
    verb was not previously defined, then nothing happens.
-----------------------------------------------------------------------------*/

void NI_register_doer( char *verb , NI_voidfunc *func )
{
   int ii ;

   if( verb == NULL || *verb == '\0' ) return ;

   /* see if verb already in table */

   for( ii=0 ; ii < doer_num ; ii++ )
     if( strcmp(verb,doer_verb[ii]) == 0 ) break ;

   /* if was in table, replace func (may be NULL) */

   if( ii < doer_num ){
     doer_func[ii] = func ; return ;
   }

   /* defining a new verb */

   if( func == NULL ) return ;   /* quit if no func */

   /* expand tables of verbs and funcs */

   ii = doer_num++ ;

   doer_verb = NI_realloc( doer_verb, char*, sizeof(char *)*doer_num ) ;
   doer_verb[ii] = NI_strdup(verb) ;

   doer_func = NI_realloc( doer_func , NI_voidfunc*, sizeof(NI_voidfunc *)*doer_num ) ;
   doer_func[ii] = func ;
   return ;
}

/*---------------------------------------------------------------------------*/
/*! Carry out an action ordered by a "ni_do" element received on
    the input stream.  Actions we know about:

   - ni_verb='reopen_this' => open the stream anew and replace it [23 Aug 2002]
   - ni_verb='close_this'  => close this stream down              [20 Dec 2002]
   - ni_verb='typedef'     => define a NI_rowtype                 [12 Feb 2003]
   - user-defined verbs can be added using NI_register_doer()     [12 Feb 2003]

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
   /*******************************************/
   /*---- check for various builtin verbs ----*/
   /*******************************************/

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
                                         /****     [12 Feb 2003]          ****/
     char tnam[256] , tdef[8200] ;
     int tt ;

     if( object == NULL || object[0] == '\0' ) return -1 ;  /* bad */

     tnam[0] = tdef[0] = '\0' ;
     sscanf(object,"%255s %8199s",tnam,tdef) ;
     tt = NI_rowtype_define( tnam , tdef ) ;
     return (tt >0) ? 0 : -1 ;

   } /****------------------------ end typedef ---------------------------****/

   /**************************************************************/
   /**** Here, check for user-defined callbacks [12 Feb 2003] ****/

   for( ii=0 ; ii < doer_num ; ii++ ){
     if( strcmp(verb,doer_verb[ii]) == 0 ){
       if( doer_func[ii] != NULL ) doer_func[ii]( object , ns , nel ) ;
       return 0 ;
     }
   }

   /*--- if we get here, we got a verb we don't recognize ---*/

   return -1 ;
}
