/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "def_epi.h"
#include "jp_afni.h"

EXT struct im_info imX[] ;  /* the image data */

#if defined(AFNI_SIGNA_KECK)
   extern int   AJxres, AJreps, AJovs, AJtopovs, AJyres, epramp, opentry, oppos;
   extern int   opspf, opplane, opobplane, AJopte, opte2, opti, optr, opslquant;
   extern float opfov, opslthick, opslspace, opnex, filt_band, sl_pos[];
   extern int   Signa_info ;
#endif

static char AFNI_infobuf[1024] = "\0" ;  /* initialize to nothing */

/*--- 23 May 2001: allow for forking to send data to AFNI ---*/

#define USE_FORK    /* turn this off to disable forkage */

#include <unistd.h>
#include <sys/types.h>
#include <signal.h>
#include <sys/wait.h>
static int   use_fork = 0 ;
static pid_t pid_fork = 0 ;

/*****************************************************************************/

#include <signal.h>

static void AFNI_sigfunc(int sig)  /** signal handler for fatal errors **/
{
   char * sname ;
   switch(sig){
      default:      sname = "unknown" ; break ;
      case SIGINT:  sname = "SIGINT"  ; break ;
      case SIGPIPE: sname = "SIGPIPE" ; break ;
      case SIGSEGV: sname = "SIGSEGV" ; break ;
      case SIGBUS:  sname = "SIGBUS"  ; break ;
      case SIGTERM: sname = "SIGTERM" ; break ;
   }
   fprintf(stderr,"\nFatal Signal %d (%s) received\n",sig,sname) ;
   fprintf(stderr,"*** Program Abort ***\n") ; fflush(stderr) ;
   exit(1) ;
}

/*****************************************************************************/

void AFNI_exit(void)                   /* Function to be called to make sure */
{                                      /* the AFNI data channels get closed. */
   iochan_close(AFNI_ioc) ;

#ifdef USE_FORK
   if( use_fork && pid_fork != 0 && pid_fork != (pid_t)(-1) ){
      int kk = kill( pid_fork , SIGTERM ) ;
      if( kk == 0 ){
         pid_t qpid=0 ;
         for( kk=0 ; kk < 10 && qpid == 0 ; kk++ ){
            iochan_sleep(5) ;
            qpid = waitpid( pid_fork , NULL , WNOHANG ) ;
         }
      }
   }
#endif

   return ;
}

/*****************************************************************************
  Do I/O startup stuff; nim images are in the imX buffer at this time.

  At any given moment, this routine is in one of a number of modes
  (the AFNI_mode variable).
  The first time in, AFNI_mode == AFNI_OPEN_CONTROL_MODE.  In each mode,
  certain tasks must be accomplished and this program must be synchronized
  with AFNI.  When the necessary deeds are done, the routine advances to
  the next mode.  If the deeds cannot be done when this routine is called,
  then it will stay in the same mode, and the next time it is called it
  will try to do them again.  This routine should be called repeatedly
  until it progresses to the last mode (AFNI_CONTINUE_MODE), which is for
  normal transmission of images (one at a time) to AFNI.  This operation
  is handled in the separate routine AFNI_send_image (infra).

  If an error occurs, so that this program can no longer talk to AFNI, then
  AFNI_mode is set to 0, which means "do nothing further".  The rest of
  the data acquisition software will continue, but these routines will
  be stopped dead.
******************************************************************************/

void AFNI_start_io( int nim )
{
   int ii ;

   /***** Check for illegal conditions *****/

   if( AFNI_mode <= 0 || AFNI_mode == AFNI_CONTINUE_MODE ) return ;

   /***** If we are at the first time in,
          try to open a control socket to talk to AFNI *****/

   if( AFNI_mode == AFNI_OPEN_CONTROL_MODE ){

      sprintf( AFNI_iochan , "tcp:%s:%d" , AFNI_host , AFNI_CONTROL_PORT ) ;

      if( AFNI_verbose )
         fprintf(stderr,"Opening control channel %s to AFNI.\n",AFNI_iochan) ;

      AFNI_ioc = iochan_init( AFNI_iochan , "w" ) ;

      if( AFNI_ioc == NULL ){
         fprintf(stderr,"Can't open control channel %s to AFNI!\a\n",AFNI_iochan) ;
         AFNI_mode = 0 ; return ;
      } else {
         if( AFNI_verbose ) fprintf(stderr,"Entering AFNI_WAIT_CONTROL_MODE.\n") ;
         AFNI_mode = AFNI_WAIT_CONTROL_MODE ;  /* begin waiting for AFNI connection */
      }

      signal( SIGTERM , AFNI_sigfunc ) ; /* 23 May 2001 */
      signal( SIGSEGV , AFNI_sigfunc ) ;
      signal( SIGINT  , AFNI_sigfunc ) ;
   }

   /***** Check if the control socket is connected to AFNI *****/

   if( AFNI_mode == AFNI_WAIT_CONTROL_MODE ){

      ii = iochan_writecheck( AFNI_ioc , 1 ) ;  /* Check; wait at most 1 msec */

      /** if ii == 0, then the channel is still pending,
          so do nothing; otherwise, take some action.    **/

      if( ii < 0 ){
         fprintf(stderr,"Control channel to AFNI failed!\a\n") ;
         IOCHAN_CLOSE(AFNI_ioc) ;
         AFNI_mode = 0 ; return ;
      } else if( ii > 0 ){
         if( AFNI_verbose ) fprintf(stderr,"Control channel connected to AFNI."
                                           "  Entering AFNI_OPEN_DATA_MODE.\n") ;
         AFNI_mode = AFNI_OPEN_DATA_MODE ;  /* prepare to send data to AFNI */
      }
   }

   /***** Send the control information, which says
          how we will talk to AFNI in the future (shmem or TCP/IP),
          then close the control channel and open this new data channel *****/

   if( AFNI_mode == AFNI_OPEN_DATA_MODE ){

      /** decide name of data channel: it can be TCP/IP or shared memory **/

      if( AFNI_use_tcp ) sprintf(AFNI_iochan,"tcp:%s:%d",AFNI_host,AFNI_TCP_PORT) ;
      else               strcpy(AFNI_iochan,"shm:eps:8M") ;

      strcpy(AFNI_buf,AFNI_iochan) ;     /* tell AFNI where to read data */
      if( AFNI_infocom[0] != '\0' ){
         strcat(AFNI_buf,"\n") ;
         strcat(AFNI_buf,AFNI_infocom) ; /* tell it where to get 3T info */
      }

      if( AFNI_verbose )
         fprintf(stderr,"Sending control information to AFNI:\n%s\n",AFNI_buf) ;

      ii = iochan_sendall( AFNI_ioc , AFNI_buf , strlen(AFNI_buf)+1 ) ;

      /** A negative return is bad news **/

      if( ii < 0 ){
         fprintf(stderr,"Transmission of control data to AFNI failed!\a\n") ;
         IOCHAN_CLOSE(AFNI_ioc) ;
         AFNI_mode = 0 ; return ;
      } else {
         while( ! iochan_clearcheck(AFNI_ioc,2) ) /* wait for control data to clear */
            iochan_sleep(2) ;
         IOCHAN_CLOSE(AFNI_ioc) ;                 /* close control channel */

         if( AFNI_verbose )
            fprintf(stderr,"Opening data channel %s to AFNI.\n",AFNI_iochan) ;

#ifdef USE_FORK
         use_fork = AFNI_use_tcp ;
#endif

         if( use_fork ){  /* fork a relay process to talk to AFNI */

            AFNI_ioc = iochan_init( "shm:forkage:8M" , "create" ) ;
            if( AFNI_ioc == NULL ){
               fprintf(stderr,"Can't open shm:forkage:8M for relay to AFNI!\n");
               use_fork = 0 ; AFNI_mode = 0 ; return ;
            }
            pid_fork = iochan_fork_relay( "shm:forkage:8M" , AFNI_iochan ) ;
            if( pid_fork == (pid_t)(-1) || pid_fork == 0 ){
               fprintf(stderr,"Can't fork for relay to AFNI!\n") ;
               IOCHAN_CLOSE(AFNI_ioc) ; pid_fork = 0 ; use_fork = 0 ;
               AFNI_mode = 0 ; return ;
            }

         } else {  /* directly handle I/O to AFNI ourselves */

            AFNI_ioc = iochan_init( AFNI_iochan , "w" ) ; /* open data channel */
            if( AFNI_ioc == NULL ){
               fprintf(stderr,"Can't open data channel %s to AFNI!\a\n",AFNI_iochan) ;
               AFNI_mode = 0 ; return ;
            }
         }

         if( AFNI_verbose ) fprintf(stderr,"Entering AFNI_CATCHUP_MODE.\n") ;
         AFNI_mode = AFNI_CATCHUP_MODE ;
      }
   }

   /***** Wait for the data channel to be connected to AFNI,
          and then send any images that are reconstructed and ready to go *****/

   if( AFNI_mode == AFNI_CATCHUP_MODE ){

      ii = iochan_writecheck( AFNI_ioc , 1 ) ;  /* wait at most 1 msec */
      if( ii < 0 ){
         fprintf(stderr,"AFNI data channel aborted before any data was sent!\a\n") ;
         IOCHAN_CLOSE( AFNI_ioc ) ;
         use_fork = 0 ; AFNI_mode = 0 ; return ;
      } else if( ii > 0 ){                      /* can now send data to AFNI! */
         if( AFNI_verbose )
            fprintf(stderr,"AFNI data channel %s is connected.\n",AFNI_iochan) ;

         /*** final preparation code depends on which scanner we are using ***/

#if defined(AFNI_BRUKER_BRI)

         /*-- for the Bruker, don't need to check anything else
              (external information comes from program 3T_toafni) --*/

         if( AFNI_verbose )
            fprintf(stderr,"Entering AFNI_CONTINUE_MODE.\n") ;
         AFNI_mode = AFNI_CONTINUE_MODE ;

#elif defined(AFNI_SIGNA_KECK)

         /*-- for the Keck Signa, we must have the external information now --*/

         if( !Signa_info ){
            double tt = COX_clock_time() ;

            fprintf(stderr,
                    "\n\a"
                    "*** WARNING: Reading Signa info for AFNI ('load')\n");

            read_Signa_cvs() ;

            if( !Signa_info ){  /* failed! */
               fprintf(stderr,
                       "\n\a"
                       "*** ERROR:   Signa info is not 'load'-ed!\n"
                       "***          Closing connection to AFNI.\n") ;

               IOCHAN_CLOSE( AFNI_ioc ) ;
               use_fork = 0 ; AFNI_mode = 0 ; return ;
            }

            tt = COX_clock_time() - tt ;
            fprintf(stderr,
                    "***          Signa info transferred in %.1f s\n",tt) ;
         }

         /*-- have external info, so compose it into the AFNI format --*/

         /* info that never changes (at least yet) */

         ii = strlen(AFNI_infobuf) ;
         sprintf( AFNI_infobuf+ii , "ZORDER alt\n"
                                    "ACQUISITION_TYPE 2D+zt\n" ) ;

         /* field of view */

         ii = strlen(AFNI_infobuf) ;
         sprintf( AFNI_infobuf+ii , "XYFOV %.2f 0 0\n" , opfov ) ;

         /* number of slices */

         ii = strlen(AFNI_infobuf) ;
         sprintf( AFNI_infobuf+ii , "ZNUM %d\n" , opslquant ) ;

         /* slice thickness */

         ii = strlen(AFNI_infobuf) ;
         sprintf( AFNI_infobuf+ii , "ZDELTA %.2f\n" , opslthick+opslspace ) ;

         /* slice orientation and offset */

         { static char *zzz[3]={"S","R","A"} ;  /* positive z-axis for each orientation */
           char *axx , *ayy , *azz ;            /* strings for each axis orientation   */
           int jj = opplane ;                   /* orientation code from Signa        */

           if( jj < 1 || jj > 3 ){
              static char * pn[3] = { "Axial" , "Sagittal" , "Coronal" } ;
              jj = opobplane ; if( jj < 1 || jj > 3 ) jj = 1 ;
              fprintf(stderr,
                      "\n\a"
                      "*** WARNING: oblique slices; AFNI orientation=%s\n",pn[jj-1]) ;
           }

           /* 07 Mar 2000: modified some of these codes, per Lloyd Estkowski */

           axx = ayy = azz = "???" ;  /* just to keep the compiler happy */
           switch( jj ){
             case 1:                                              /* Axial */
               if( opspf == 0 ){ axx = "A-P" ; ayy = "R-L" ; }
               else            { axx = "L-R" ; ayy = "A-P" ; }

               if( sl_pos[0] < sl_pos[1] ) azz = "I-S" ;
               else                        azz = "S-I" ;
             break ;

             case 2:                                              /* Sagittal */
               if( opspf == 0 ){ axx = "S-I" ; ayy = "A-P" ; }
               else            { axx = "P-A" ; ayy = "S-I" ; }

               if( sl_pos[0] < sl_pos[1] ) azz = "L-R" ;
               else                        azz = "R-L" ;
             break ;

             case 3:                                              /* Coronal */
               if( opspf == 0 ){ axx = "I-S" ; ayy = "R-L" ; }
               else            { axx = "R-L" ; ayy = "S-I" ; }

               if( sl_pos[0] < sl_pos[1] ) azz = "P-A" ;
               else                        azz = "A-P" ;
             break ;
           }

           ii = strlen(AFNI_infobuf) ;
           sprintf( AFNI_infobuf+ii , "XYZAXES %s %s %s\n" , axx,ayy,azz ) ;

fprintf(stderr,"AFNI Signa info: plane=%d spf=%d\n" ,jj,opspf) ;      /* debugging */
fprintf(stderr,"              => XYZAXES %s %s %s\n",axx,ayy,azz) ;
fprintf(stderr,"              => ZFIRST %.2f%s\n"   ,sl_pos[0],zzz[jj-1] ) ;

           ii = strlen(AFNI_infobuf) ;
           sprintf( AFNI_infobuf+ii , "ZFIRST %.2f%s\n" , sl_pos[0],zzz[jj-1] ) ;
         }

         /* repetition time */

         ii = strlen(AFNI_infobuf) ;
         sprintf( AFNI_infobuf+ii , "TR %.3f\n" , 1.e-6*optr ) ;

         /* ready to rock-n-roll */

         if( AFNI_verbose )
            fprintf(stderr,"Entering AFNI_CONTINUE_MODE.\n") ;
         AFNI_mode = AFNI_CONTINUE_MODE ;

#endif   /*** end of scanner dependent preparation code ***/

         /*** if there are any images already accumulated, send them now! ***/

         if( AFNI_mode == AFNI_CONTINUE_MODE ){
            if( nim > 0 && AFNI_verbose )
               fprintf(stderr,"Playing AFNI catchup with %d images.\n",nim) ;

            for( ii=0 ; ii < nim ; ii++ ) AFNI_send_image( ii ) ;
         }
      }
   }

   return ;
}

/*******************************************************************************/

/** send image in imX[nim] to AFNI **/

void AFNI_send_image( int nim )
{
   int lx = imX[nim].x , ly = imX[nim].y , nbytes = 2*lx*ly , soff , jj ;

   if( AFNI_mode != AFNI_CONTINUE_MODE ) return ;

   if ( (lx*ly) == 65536 ) soff = OFFSET;  /* for Signa */
   else                    soff = 0;

   /** before very 1st image, send data type and matrix size **/

   if( nim == 0 ){

      if( AFNI_verbose ) fprintf(stderr,"Sending 1st info+image to AFNI.\n") ;

      if( AFNI_infobuf[0] == '\0' )
         sprintf( AFNI_buf , "DATUM short\nXYMATRIX %d %d\n" , lx,ly ) ;
      else
         sprintf( AFNI_buf , "%s\nDATUM short\nXYMATRIX %d %d\n" ,
                             AFNI_infobuf , lx,ly ) ;

      iochan_sendall( AFNI_ioc , AFNI_buf , strlen(AFNI_buf)+1 ) ;

   } else if( AFNI_verbose ){

      fprintf(stderr,"Sending image %d to AFNI.\n",nim+1) ;

   }

   jj = iochan_writecheck( AFNI_ioc , 1 ) ;
   if( jj <= 0 ){
      fprintf(stderr,"Image transmission to AFNI impossible at #%d.\a\n",nim) ;
      if( AFNI_ioc->type == SHM_IOCHAN )
         fprintf(stderr," shm: bstart=%d  bend=%d\n",
                *(AFNI_ioc->bstart),*(AFNI_ioc->bend) ) ;
      iochan_sleep(1) ;
      IOCHAN_CLOSE(AFNI_ioc) ;
      use_fork = 0 ; AFNI_mode = 0 ;
      fprintf(stderr,"Closed connection to AFNI\n") ;
      return ;
   }
   if( AFNI_ioc->type == SHM_IOCHAN && jj < nbytes ){
      fprintf(stderr,"Image transmission to AFNI incomplete at #%d.\a\n",nim) ;
      if( AFNI_ioc->type == SHM_IOCHAN )
         fprintf(stderr," shm: bstart=%d  bend=%d\n",
                *(AFNI_ioc->bstart),*(AFNI_ioc->bend) ) ;
      iochan_sleep(1) ;
      IOCHAN_CLOSE(AFNI_ioc) ;
      use_fork = 0 ; AFNI_mode = 0 ;
      fprintf(stderr,"Closed connection to AFNI\n") ;
      return ;
   }

   jj = iochan_sendall( AFNI_ioc , imX[nim].arr + soff , nbytes ) ;

   /** if the data channel failed, stop **/

   if( jj < 0 ){
      fprintf(stderr,"Image transmission to AFNI fails at #%d.\a\n",nim) ;
      if( AFNI_ioc->type == SHM_IOCHAN )
         fprintf(stderr," shm: bstart=%d  bend=%d\n",
                *(AFNI_ioc->bstart),*(AFNI_ioc->bend) ) ;
      iochan_sleep(1) ;
      IOCHAN_CLOSE(AFNI_ioc) ;
      use_fork = 0 ; AFNI_mode = 0 ;
      fprintf(stderr,"Closed connection to AFNI\n") ;
      return ;
   }

   return ;
}
