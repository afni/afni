/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
/***************************************************************
  Sample plugout program to send the dataset (i,j,k)
  indices for viewing.
****************************************************************/

/***** Header file for communication routines *****/

#include "thd_iochan.h"

/***** Global variable determining on which system AFNI runs.  *****/
/***** [default is the current system, can be changed by user] *****/

static char afni_host[128] = "." ;
static char afni_name[128] = "\0" ;
static int  afni_port      = 8009 ;
static int  afni_verbose   = 0 ;  /* print out debug info? */

/***** Prototype *****/

int afni_io(void) ;

/*===================================================================
   Main program:
     Read command line for options
     Call afni_io routine forever
=====================================================================*/

int main( int argc , char * argv[] )
{
   int narg , ii ;

   /***** See if the pitiful user wants help *****/

   if( argc == 2 && strncmp(argv[1],"-help",5) == 0 ){
      printf("Usage: plugout_ijk [-host name] [-v]\n"
             "This program connects to AFNI and send (i,j,k)\n"
             "dataset indices to control the viewpoint.\n\n"
             "Options:\n"
             "  -host name  Means to connect to AFNI running on the\n"
             "                computer 'name' using TCP/IP.  The default is to\n"
             "                connect on the current host using shared memory.\n"
             "  -v          Verbose mode.\n"
             "  -port pp    Use TCP/IP port number 'pp'.  The default is\n"
             "                8009, but if two plugins are running on the\n"
             "                same computer, they must use different ports.\n"
             "  -name sss   Use the string 'sss' for the name that AFNI assigns\n"
             "                to this plugout.  The default is something stupid.\n"
            ) ;
      exit(0) ;
   }

   /***** Process command line options *****/

   narg = 1 ;
   while( narg < argc ){

      /** -host name **/

      if( strncmp(argv[narg],"-host",5) == 0 ){
         narg++ ;
         if( narg >= argc ){
            fprintf(stderr,"-host needs a following name!\a\n"); exit(1);
         }
         strcpy( afni_host , argv[narg] ) ;
         narg++ ; continue ;
      }

      /** -name sss **/

      if( strncmp(argv[narg],"-name",5) == 0 ){
         narg++ ;
         if( narg >= argc ){
            fprintf(stderr,"-name needs a following string!\a\n"); exit(1);
         }
         strcpy( afni_name , argv[narg] ) ;
         narg++ ; continue ;
      }

      /** -v **/

      if( strncmp(argv[narg],"-v",2) == 0 ){
         afni_verbose = 1 ;
         narg++ ; continue ;
      }

      /** -port pp **/

      if( strncmp(argv[narg],"-port",4) == 0 ){
         narg++ ;
         if( narg >= argc ){
            fprintf(stderr,"-port needs a following argument!\a\n"); exit(1);
         }
         afni_port = strtol( argv[narg] , NULL , 10 ) ;
         if( afni_port <= 0 ){
            fprintf(stderr,"-port needs a positive argument!\a\n"); exit(1);
         }
         if( strcmp(afni_host,".") == 0 ) strcpy(afni_host,"localhost") ;
         narg++ ; continue ;
      }

      /** Je ne sais pas **/

      fprintf(stderr,"Unrecognized option: %s\a\n",argv[narg]) ;
      exit(1) ;
   }

   /***** Loop and check in with AFNI every 100 msec *****/

   while( 1 ){
      ii = afni_io() ;        /* commune with AFNI  */
      if( ii < 0 ) exit(0) ;  /* bad trip? then die */
      iochan_sleep(100) ;     /* perchance to dream */
   }

}

/*===================================================================
  This routine handles all communications with AFNI.
  The only input is the global variable afni_host, which determines
  on which system AFNI is running.
  The output is -1 if an error occured, 0 if everything is OK.
=====================================================================*/

/***** Mode flags for determining what afni_io does.
       The routine progress through 5 stages:

       1) Open a control connection to AFNI;
     then
       2) Wait until AFNI also opens the control connection;
     then
       3) Send a control string to AFNI saying what kind of
            information we want, wait for an acknowledgment,
            close the control connection, and open a data
            connection to AFNI;
     then
       4) Wait for AFNI to also open the data connection;
     then
       5) Send data to AFNI!                                *****/

#define AFNI_OPEN_CONTROL_MODE  1  /* 1st time thru: open control channel */
#define AFNI_WAIT_CONTROL_MODE  2  /* wait for AFNI to open control chan  */
#define AFNI_OPEN_DATA_MODE     3  /* now can open data channel to AFNI   */
#define AFNI_WAIT_DATA_MODE     4  /* waiting for AFNI to open data chan  */
#define AFNI_CONTINUE_MODE      5  /* at last! data channel is ready!     */

/***** macros to send acknowledgement strings to AFNI *****/

#define POACKSIZE       4  /* length of acknowledgement strings */

#define PO_ACK_BAD(ic)  iochan_sendall( (ic) , "BAD" , POACKSIZE )
#define PO_ACK_OK(ic)   iochan_sendall( (ic) , "OK!" , POACKSIZE )
#define PO_SEND(ic,str) iochan_sendall( (ic) , (str) , strlen((str))+1 )

int afni_io(void)
{
   static int afni_mode = AFNI_OPEN_CONTROL_MODE ;  /* status variable */
   static IOCHAN * afni_ioc = NULL ;                /* connection to AFNI */
   int ii ;

   /***************************************************************/
   /***** Check to see if status is OK before we proceed.     *****/
   /***** (if an error occurs below, afni_mode gets set to 0) *****/

   if( afni_mode <= 0 ) return -1 ;

   /***********************************************************************/
   /***** First time into this routine?  Open control channel to AFNI *****/

   if( afni_mode == AFNI_OPEN_CONTROL_MODE ){
      char afni_iocname[128] ;           /* will hold name of I/O channel */

      /** Note that the control channel is always a
          TCP/IP channel to port # 7955 on the AFNI host system **/

      if( strcmp(afni_host,".") == 0 )
         sprintf( afni_iocname , "tcp:%s:7955" , "localhost" ); /* make name */
      else
         sprintf( afni_iocname , "tcp:%s:7955" , afni_host ) ;  /* make name */

      afni_ioc = iochan_init( afni_iocname , "create" ) ;    /* create it */
      if( afni_ioc == NULL ){
         fprintf(stderr,
                 "Can't create control channel %s to AFNI!\n",afni_iocname) ;
         afni_mode = 0 ;
         return -1 ;
      }
      afni_mode = AFNI_WAIT_CONTROL_MODE ; /* waiting for AFNI connection */
      if( afni_verbose )
         fprintf(stderr,"AFNI control channel created\n") ;
   }

   /****************************************************/
   /**** Check if AFNI control channel is connected ****/

   if( afni_mode == AFNI_WAIT_CONTROL_MODE ){
      ii = iochan_writecheck( afni_ioc , 5 ) ;     /* wait at most 5 msec */

      /** the iochan_*check() routines return
             -1 for an error,
              0 if not ready,
             >0 if the I/O channel is ready. **/

      if( ii < 0 ){
         fprintf(stderr,"Control channel to AFNI failed!\a\n") ;
         IOCHAN_CLOSE(afni_ioc) ;
         afni_mode = 0 ;
         return -1 ;
      } else if( ii > 0 ){
         afni_mode = AFNI_OPEN_DATA_MODE ;        /* prepare to send data */
         if( afni_verbose )
            fprintf(stderr,"AFNI control channel connected\n");
      } else {
         return 0 ;                                /* try again next time */
      }
   }

   /**********************************************************/
   /**** Send control data to AFNI, and open data channel ****/

   if( afni_mode == AFNI_OPEN_DATA_MODE ){
      char afni_iocname[128] ;
      char afni_buf[256] ;

      /** decide name of data channel:
            use shared memory (shm:) on ".",
            use TCP/IP (tcp:) on other computer systems;
        * Note that the TCP/IP port number can be
           anything that isn't already in use;
        * Note that the shm control name (here "test_plugout")
           is a string that will be converted to an IPC
           key (in function string_to_key in iochan.c).       **/

      if( strcmp(afni_host,".") == 0 )
         strcpy( afni_iocname , "shm:test_plugout:1K+1K" ) ;
      else
         sprintf( afni_iocname , "tcp:%s:%d" , afni_host , afni_port ) ;

      /** write the command to AFNI into the buffer:
            * each command ends with a newline character,
                except (possibly) the last command;
            * the command buffer is a C string, which ends
                with an ASCII NUL character;
            * PONAME means 'use this string for informative messages';
            * IOCHAN means 'use this I/O channel from now on'. **/

      if( afni_name[0] == '\0' ) strcpy(afni_name,"aHorseCalledMan") ;

      sprintf( afni_buf , "PONAME %s\n"
                          "IOCHAN %s" ,
               afni_name , afni_iocname ) ;

      if( afni_verbose )
         fprintf(stderr,"Sending control information to AFNI\n") ;

      /** note that the ASCII NUL at the end of the buffer is sent **/

      ii = iochan_sendall( afni_ioc , afni_buf , strlen(afni_buf)+1 ) ;

      /** the return value is the number of bytes sent,
          or -1 indicating a fatal error transpired.    **/

      if( ii < 0 ){
         fprintf(stderr,"Transmission of control data to AFNI failed!\a\n") ;
         IOCHAN_CLOSE(afni_ioc) ;
         afni_mode = 0 ;
         return -1 ;

      } else {

         /** wait for the acknowledgment from AFNI, then close channel **/

         ii = iochan_recvall( afni_ioc , afni_buf , POACKSIZE ) ;
         IOCHAN_CLOSE(afni_ioc) ;

         if( ii < 0 || strncmp(afni_buf,"OK!",3) != 0 ){
            fprintf(stderr,"AFNI didn't like control information!\a\n") ;
            afni_mode = 0 ;
            return -1 ;
         }

         /** now open data channel to AFNI **/

         afni_ioc = iochan_init( afni_iocname , "create" ) ;
         if( afni_ioc == NULL ){
            fprintf(stderr,
                    "Can't open data channel %s to AFNI!\a\n",afni_iocname) ;
            afni_mode = 0 ;
            return -1 ;
         } else {
            afni_mode = AFNI_WAIT_DATA_MODE ;
            if( afni_verbose ) fprintf(stderr,"AFNI data channel created\n") ;
         }
      }
   }

   /****************************************************/
   /***** See if data channel is connected to AFNI *****/

   if( afni_mode == AFNI_WAIT_DATA_MODE ){

      ii = iochan_goodcheck( afni_ioc , 5 ) ;  /* wait at most 5 msec */
      if( ii < 0 ){
         fprintf(stderr,
                 "AFNI data channel aborted before any data was sent!\a\n") ;
         IOCHAN_CLOSE( afni_ioc ) ;
         afni_mode = 0 ;
         return -1 ;
      } else if( ii > 0 ){                     /* ready to go! */
         afni_mode = AFNI_CONTINUE_MODE ;
         if( afni_verbose ) fprintf(stderr,"AFNI data channel is open\n") ;
      } else {
         return 0 ;                            /* try again next time */
      }
   }

   /**************************************************************/
   /***** The "normal" state of affairs:  AFNI is connected. *****/
   /***** See if the user wants to send i,j,k to AFNI.       *****/

   if( afni_mode == AFNI_CONTINUE_MODE ){
      char afni_buf[256] ;
      int   ix , jy , kz ;

      /* get user input */

      printf("Enter i j k: ") ; fflush(stdout) ; fgets(afni_buf,256,stdin) ;
      ii = sscanf(afni_buf,"%d %d %d",&ix,&jy,&kz) ;
      if( ii < 3 ){
         printf("** Warning -- planetary meltdown will occur in 10 seconds!\a\n") ;
         iochan_sleep(1000) ;
         ii = iochan_writecheck(afni_ioc,5) ; if( ii < 0 ) return -1 ;
         return 0 ;
      }

      /* send input to AFNI (essentially unedited) */

      sprintf(afni_buf,"DSET_IJK_SET %d %d %d",ix,jy,kz) ;
      ii = iochan_sendall( afni_ioc , afni_buf , strlen(afni_buf)+1 ) ;

      if( ii > 0 ){  /* send was OK; wait for acknowledgment */
         ii = iochan_recvall( afni_ioc , afni_buf , POACKSIZE ) ;
      }

      if( ii < 0 ){   /* send or acknowledgment failed */
         fprintf(stderr,"AFNI data channel aborted!\a\n") ;
         IOCHAN_CLOSE(afni_ioc) ;
         afni_mode = 0 ;
         return -1 ;
      }
      return 0 ;
   }

   return 0 ;
}
