#include "niml_private.h"

/*---------------------------------------------------------------------------*/
/*! Carry out an action ordered by a "ni_do" element received on
    the input stream.  Actions we know about:

     - ni_verb='reopen' => open the stream anew and replace it [23 Aug 2002]

    Return value is -1 if an error occurs.
-----------------------------------------------------------------------------*/

int NI_do( NI_stream_type *ns , NI_element *nel )
{
   char *verb , *object ;

   /*- check inputs for OK-ositiness -*/

#ifdef NIML_DEBUG
NI_dpr("NI_do: enter\n") ;
#endif

   if( ns == NULL || nel == NULL || nel->type != NI_ELEMENT_TYPE ) return -1 ;

   if( strcmp(nel->name,"ni_do") != 0 ) return -1 ;

   verb   = NI_get_attribute( nel , "ni_verb"   ) ;
   object = NI_get_attribute( nel , "ni_object" ) ;

#ifdef NIML_DEBUG
NI_dpr("NI_do: got verb and object\n") ;
#endif

   if( verb == NULL || verb[0] == '\0' ) return -1 ;  /* need a verb */

#ifdef NIML_DEBUG
NI_dpr("verb=%s\n",verb) ;
#endif

   /*---- check for various verbs ----*/

   if( strcmp(verb,"reopen") == 0 ){  /**** reopen stream ****/

     NI_stream_type *nsnew ;

#ifdef NIML_DEBUG
NI_dpr("object=%s\n",object) ;
#endif

     if( object == NULL || object[0] == '\0' ) return -1 ;  /* bad */

#ifdef NIML_DEBUG
NI_dpr("NI_do: reopen %s\n",object) ;
#endif

     nsnew = NI_stream_open( object , "r" ) ;   /* open new stream */
     if( nsnew == NULL ) return -1 ;                        /* bad */

#ifdef NIML_DEBUG
NI_dpr("NI_do: closing old stream\n") ;
#endif

     NI_stream_close_keep(ns) ;                /* trash old stream */
     *ns = *nsnew; NI_free(nsnew); return 0;  /* replace old stuff */

   } /* end reopen */

   /*--- if we get here, we got a verb we don't recognize ---*/

   return -1 ;
}
