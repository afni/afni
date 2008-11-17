/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#ifndef _AFNI_PLUGOUT_HEADER_
#define _AFNI_PLUGOUT_HEADER_

#define PLUGOUT_COM_LENGTH (4*1024)   /* max length of command (to allow for ziad's 
                                                    beautiful and inspired commands) */
#define PLUGOUT_SHM_SIZE_K 4      /* size of shm used by plugout_drive. 
                                     modify along with PLUGOUT_COM_LENGTH */
                                     
extern void AFNI_init_plugouts( void ) ;  /* prototypes */
extern int AFNI_have_plugouts( void ) ;   /* 07 Nov 2001 */
extern void AFNI_plugout_verb( int ) ; /* 14 Oct 2008 */

#ifdef ALLOW_PLUGINS

#include "afni.h"
#include <ctype.h>
#include <unistd.h>

#define NUM_TCP_CONTROL  5     /* 21 Nov 2001: number of TCP/IP control ports */
#define BASE_TCP_CONTROL 7955  /* control via ports number 7955 .. 7959       */

#define POACKSIZE       4   /* size of all plugout acknowledgment strings */

#define PO_ACK_BAD(ic)                                                  \
   do{ if( verbose ) fprintf(stderr,"PO: sending 'BAD' to plugout\n") ; \
       iochan_sendall( (ic) , "BAD" , POACKSIZE ) ; } while(0)

#define PO_ACK_OK(ic)                                                   \
   do{ if( verbose ) fprintf(stderr,"PO: sending 'OK!' to plugout\n") ; \
       iochan_sendall( (ic) , "OK!" , POACKSIZE ) ; } while(0)

#define PO_ACK_GOOD     PO_ACK_OK

#define PO_SEND(ic,str)                                                       \
   do{ int ll = strlen((str))+1 ;                                             \
       if( verbose ) fprintf(stderr,"PO: sending %d bytes to plugout\n",ll) ; \
       iochan_sendall( (ic) , (str) , ll ) ; } while(0)

#define SHORT_DELAY      2   /* msec */
#define LONG_DELAY      10
#define VLONG_DELAY    100

#define PO_MAXMODES 16       /* max I/O modes allowed for one plugout */

#define POMODE_TT_XYZ_DELTA     1  /* I/O mode codes */
#define POMODE_DICOM_XYZ_DELTA  2
#define POMODE_DSET_IJK_DELTA   3

#define POMODE_SURFID_DELTA    21  /* 05 Sep 2001 */
#define POMODE_UNDERLAY_DELTA  31  /* 11 Jan 2002 */
#define POMODE_OVERLAY_DELTA   32

typedef struct {
   IOCHAN * ioc ;                  /* how to talk to plugout */
   int ioc_ready ;                 /* is talking ready? */
   char ioc_name[128] ;
   char po_name[128] ;             /* identifier for plugout */

   int npomode ;
   int pomode[PO_MAXMODES] ;       /* I/O modes for this plugout */

   /** things to recall about the current state **/

   float xi , yj , zk ;     /* DICOM coordinates of viewpoint */
   int   ix , jy , kz ;     /* dataset indices of viewpoint */
   int   time_index ;
   int   view_type ;
   int   sess_num , anat_num , func_num ;
   float func_threshold ;
   int   surfindex ;        /* 06 Sep 2001 */

   int do_ack ;             /* acknowledgements? 06 Sep 2001 */

   THD_3dim_dataset *dset_underlay ;  /* 11 Jan 2002 */
   THD_3dim_dataset *dset_overlay  ;

} PLUGOUT_spec ;

/* Remember the movie Death Wish? */

#define DESTROY_PLUGOUT(po)                       \
  do{ if( (po) != NULL ){                         \
         iochan_set_cutoff((po)->ioc) ;           \
         IOCHAN_CLOSE((po)->ioc) ;                \
         free((po)) ; (po) = NULL ; } } while(0)

/** prototypes **/

Boolean AFNI_plugout_workproc( XtPointer ) ;
PLUGOUT_spec * new_PLUGOUT_spec( int , char * ) ;
int AFNI_process_plugout( PLUGOUT_spec * ) ;
void AFNI_plugout_exit(void) ;

#endif /* ALLOW_PLUGINS */
#endif /* _AFNI_PLUGOUT_HEADER_ */
