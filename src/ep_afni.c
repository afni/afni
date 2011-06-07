/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "def_epi.h"
#include "ep_afni.h"

EXT struct im_info imX[] ;

/*****************************************************************************/

void AFNI_exit(void)                   /* Function to be called to make sure */
{                                      /* the AFNI data channels get closed. */
   iochan_close(AFNI_ioc) ;
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
   int ii , jj ;

   /***** Check for illegal conditions *****/

   if( AFNI_mode <= 0 || AFNI_mode == AFNI_CONTINUE_MODE ) return ;

   /***** If we are at the first time in,
          try to open a control socket to talk to AFNI *****/

   if( AFNI_mode == AFNI_OPEN_CONTROL_MODE ){

      sprintf( AFNI_iochan , "tcp:%s:%d" , 
               AFNI_host , get_port_named("AFNI_CONTROL_PORT") ) ;

      if( AFNI_verbose )
         fprintf(stderr,"Opening control channel %s to AFNI.\n",AFNI_iochan) ;

      AFNI_ioc = iochan_init( AFNI_iochan , "w" ) ;

      if( AFNI_ioc == NULL ){
         fprintf(stderr,"Can't open control channel %s to AFNI!\a\n",AFNI_iochan) ;
         AFNI_mode = 0 ;                       /* disable AFNI */
         return ;
      } else {
         if( AFNI_verbose ) fprintf(stderr,"Entering AFNI_WAIT_CONTROL_MODE.\n") ;
         AFNI_mode = AFNI_WAIT_CONTROL_MODE ;  /* begin waiting for AFNI connection */
      }
   }

   /***** Check if the control socket is connected to AFNI *****/

   if( AFNI_mode == AFNI_WAIT_CONTROL_MODE ){

      ii = iochan_writecheck( AFNI_ioc , 1 ) ;  /* Check; wait at most 1 msec */

      /** if ii == 0, then the channel is still pending,
          so do nothing; otherwise, take some action.    **/

      if( ii < 0 ){
         fprintf(stderr,"Control channel to AFNI failed!\a\n") ;
         IOCHAN_CLOSE(AFNI_ioc) ;
         AFNI_mode = 0 ;                    /* disable AFNI */
         return ;
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

      /* decide name of data channel: it can be TCP/IP or shared memory */

      if( AFNI_use_tcp ) sprintf(AFNI_iochan,"tcp:%s:%d",
                                 AFNI_host,get_port_named("AFNI_TCP_PORT")) ;
      else               strcpy(AFNI_iochan,"shm:eps:1M") ;

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
         AFNI_mode = 0 ;
         return ;
      } else {
         while( ! iochan_clearcheck(AFNI_ioc,2) ) /* wait for control data to clear */
            iochan_sleep(2) ;
         IOCHAN_CLOSE(AFNI_ioc) ;                 /* close control channel */

         if( AFNI_verbose )
            fprintf(stderr,"Opening data channel %s to AFNI.\n",AFNI_iochan) ;

         AFNI_ioc = iochan_init( AFNI_iochan , "w" ) ; /* open data channel */
         if( AFNI_ioc == NULL ){
            fprintf(stderr,"Can't open data channel %s to AFNI!\a\n",AFNI_iochan) ;
            AFNI_mode = 0 ;
            return ;
         } else {
            if( AFNI_verbose ) fprintf(stderr,"Entering AFNI_CATCHUP_MODE.\n") ;
            AFNI_mode = AFNI_CATCHUP_MODE ;
         }
      }
   }

   /***** Wait for the data channel to be connected to AFNI,
          and then send any images that are reconstructed and ready to go *****/

   if( AFNI_mode == AFNI_CATCHUP_MODE ){

      ii = iochan_writecheck( AFNI_ioc , 1 ) ;  /* wait at most 1 msec */
      if( ii < 0 ){
         fprintf(stderr,"AFNI data channel aborted before any data was sent!\a\n") ;
         IOCHAN_CLOSE( AFNI_ioc ) ;
         AFNI_mode = 0 ;
         return ;
      } else if( ii > 0 ){                      /* can now send data to AFNI! */
         if( AFNI_verbose )
            fprintf(stderr,"AFNI data channel %s is connected.\n"
                           "Entering AFNI_CONTINUE_MODE.\n" , AFNI_iochan) ;
         AFNI_mode = AFNI_CONTINUE_MODE ;

         /* if there are any images already accumulated, send them now! */

         if( nim > 0 && AFNI_verbose )
            fprintf(stderr,"Playing AFNI catchup with %d images.\n",nim) ;

         for( ii=0 ; ii < nim ; ii++ ) AFNI_send_image( ii ) ;
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
      sprintf( AFNI_buf , "DATUM short\nXYMATRIX %d %d\n" , lx,ly ) ;
      iochan_sendall( AFNI_ioc , AFNI_buf , strlen(AFNI_buf)+1 ) ;
   } else if( AFNI_verbose ){
      fprintf(stderr,"Sending image %d to AFNI.\n",nim+1) ;
   }

   jj = iochan_sendall( AFNI_ioc , imX[nim].arr + soff , nbytes ) ;

   /** if the data channel failed, stop **/

   if( jj < 0 ){
      fprintf(stderr,"Image transmission to AFNI fails at #%d.\a\n",nim) ;
      IOCHAN_CLOSE(AFNI_ioc) ;
      AFNI_mode = 0 ;
   }

   return ;
}
