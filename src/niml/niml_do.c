#include "niml_private.h"

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

   /*--- if we get here, we got a verb we don't recognize ---*/

   return -1 ;
}
