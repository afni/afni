#include "mrilib.h"
#include "niml.h"

/**************************************/
/** global data for NIML connections **/
/**************************************/

/*---------------------------------------*/
/*! Number of streams on which to listen */
#define NUM_NIML   1

/*--------------------------------------*/
/*! Array of streams on which to listen */

static NI_stream ns_listen[NUM_NIML] ;

/*------------------------*/
/*! Array of stream names */

static char ns_name[NUM_NIML][64] ;

/*-------------------------------------*/
/*! The SUMA stream index in ns_listen */

#define NS_SUMA 0

/*-------------------------*/

#ifndef SUMA_TCP_PORT
#define SUMA_TCP_PORT 53211
#endif

/*-----------------------------------------------*/
/*! Flag to tell if NIML things are initialized. */

static int started = 0 ;

/*---------------------*/
/* Internal prototypes */

static Boolean AFNI_niml_workproc( XtPointer ) ;

/*-----------------------------------------------------------------------*/
/*! Routine executed at AFNI exit: shutdown all open NI_stream.
-------------------------------------------------------------------------*/

static void AFNI_niml_exit( void )
{
   int cc ;
   for( cc=0 ; cc < NUM_NIML ; cc++ )        /* close any open sockets */
      NI_stream_close( ns_listen[cc] ) ;
   return ;
}

/*-----------------------------------------------------------------------*/
/*! Initialize NIML listening.
-------------------------------------------------------------------------*/

void AFNI_init_niml( void )
{
   int cc ;

ENTRY("AFNI_init_niml") ;

   if( started ) EXRETURN ;

fprintf(stderr,"AFNI_init_niml\n") ;

   PLUTO_register_workproc( AFNI_niml_workproc , NULL ) ;
   atexit( AFNI_niml_exit ) ;

   for( cc=0 ; cc < NUM_NIML ; cc++ ) ns_listen[cc] = NULL ;

   sprintf(ns_name[0] , "tcp:host:%d" , SUMA_TCP_PORT ) ;

   started = 1 ; EXRETURN ;
}

/*-----------------------------------------------------------------------*/
/*! Debug printout of a NIML element.
-------------------------------------------------------------------------*/

static void NIML_to_stderr( void *nini )
{
   NI_stream ns_err ;
   ns_err = NI_stream_open( "fd:2" , "w" ) ;
   if( ns_err != NULL ){
     NI_write_element( ns_err , nel , NI_TEXT_MODE ) ;
     NI_stream_close( ns_err ) ;
   }
}

/*-----------------------------------------------------------------------*/
/*! NIML workprocess.
    - Listen for new incoming connections on any non-open connections.
    - Read and process any new data from open connections.

  (If the return is True, that means don't call this workproc again.
   If the return is False, that means call this workproc again.......)
-------------------------------------------------------------------------*/

static Boolean AFNI_niml_workproc( XtPointer elvis )
{
   int cc , nn ;
   void *nini ;

   /** loop over input NIML streams **/

   for( cc=0 ; cc < NUM_NIML ; cc++ ){

     /* open streams that aren't open */

     if( ns_listen[cc] == NULL ){
fprintf(stderr,"AFNI_niml_workproc: opening %s\n",ns_name[cc]) ;
       ns_listen[cc] = NI_stream_open( ns_name[cc] , "r" ) ;
     }

     /* check if stream has gone bad */

     nn = NI_stream_goodcheck( ns_listen[cc] , 1 ) ;

     if( nn < 0 ){                          /* is bad */
       NI_stream_close( ns_listen[cc] ) ;
       ns_listen[cc] = NULL ;
       continue ;              /* skip to next stream */
     }

     if( nn == 0 ) continue ;  /* waiting: skip to next stream */

     /* if here, stream is good;
        see if there is any data to be read */

     nn = NI_stream_readcheck( ns_listen[cc] , 1 ) ;

     if( nn > 0 ){                                   /* has data */
       nini = NI_read_element( ns_listen[cc] , 1 ) ;  /* read it */

       switch( NI_element_type(nini) ){            /* process it */
         default: break ;                     /* NULL or unknown */

         case NI_ELEMENT_TYPE:{                  /* data element */
           NI_element *nel = (NI_element *) nini ;

           NIML_to_stderr( nel ) ;
           NI_free_element( nel ) ;
         }
         break ;

         case NI_GROUP_TYPE:{                   /* group element */
           NI_element *ngr = (NI_element *) nini ;

           NIML_to_stderr( ngr ) ;
           NI_free_element( ngr ) ;
         }
         break ;
       }
     }
   }

   return False ;
}
