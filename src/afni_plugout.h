#ifndef _AFNI_PLUGOUT_HEADER_
#define _AFNI_PLUGOUT_HEADER_

void AFNI_init_plugouts( void ) ;  /* prototype */

#ifdef ALLOW_PLUGINS

#include "afni.h"
#include "iochan.h"
#include <ctype.h>
#include <unistd.h>

#define TCP_PLUGOUT_CONTROL "tcp:*:7955"   /* TCP/IP control channel for plugouts */

#define POACKSIZE       4
#define PO_ACK_BAD(ic)  iochan_sendall( (ic) , "BAD" , POACKSIZE )
#define PO_ACK_OK(ic)   iochan_sendall( (ic) , "OK!" , POACKSIZE )
#define PO_ACK_GOOD     PO_ACK_OK
#define PO_SEND(ic,str) iochan_sendall( (ic) , (str) , strlen((str))+1 )

#define SHORT_DELAY      2            /* msec */
#define LONG_DELAY      10
#define VLONG_DELAY    100

#define PO_MAXMODES 16

#define POMODE_TT_XYZ_OUT     1
#define POMODE_DICOM_XYZ_OUT  2

typedef struct {
   IOCHAN * ioc ;
   int ioc_ready ;
   char ioc_name[128] ;
   char po_name[128] ;

   int npomode ;
   int pomode[PO_MAXMODES] ;

   /** things to recall about the current state **/

   float xi , yj , zk ;     /* DICOM coordinates of viewpoint */
   int   time_index ;
   int   view_type ;
   int   sess_num , anat_num , func_num ;
   float func_threshold ;

} PLUGOUT_spec ;

#define DESTROY_PLUGOUT(po)  \
  do{ if( (po) != NULL ){    \
         IOCHAN_CLOSE((po)->ioc) ; free((po)) ; (po) = NULL ; } } while(0)

/** prototypes **/

Boolean AFNI_plugout_workproc( XtPointer ) ;
PLUGOUT_spec * new_PLUGOUT_spec( int , char * ) ;
int AFNI_process_plugout( PLUGOUT_spec * ) ;
void AFNI_plugout_exit(void) ;

#endif /* ALLOW_PLUGINS */
#endif /* _AFNI_PLUGOUT_HEADER_ */
