/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
/***************************************************************
  Sample plugout program, that registers with AFNI
  to be notified every time the Talairach-Tournoux
  coordinates change.  Note that this will only
  occur when the lowest numbered active AFNI
  controller is in the Talairach view.
  T-T coordinates are
      -x axis = L   +x axis = R
      -y axis = P   +y axis = A
      -z axis = I   +z axis = S
  Note that x and y are each flipped from the
  DICOM standard, which AFNI uses internally.
  The values reported to this plugout program
  are in the T-T system, not the DICOM system.
                                          RWCox, June 1997
****************************************************************
  Usage: plugout_tt [-host name] [-v]
  Options:
    -host name  Means to connect to AFNI running on the
                  remote computer 'name'.  The default is
                  to connect on the current host.  If the
                  connection is to "localhost" (the default),
                  then shared memory is used, otherwise
                  a TCP/IP socket is used.
    -v          Verbose mode: prints out lots of stuff.

  Note that AFNI must be run with the "-yesplugouts"
  option to allow it to talk to this program.  See the
  output of "afni -help" for (a little) more information.
****************************************************************
  The file "thd_trusthost" controls which systems are
  allowed to connect to AFNI.  This is controlled by
  setting the environment variables
    AFNI_TRUSTHOST_1 through AFNI_TRUSTHOST_99
  to the IP addresses (not names) of hosts from which
  AFNI should accept plugout connections.  For example,
    setenv AFNI_TRUSTHOST_1 123.45.67.89
  Note that 127.0.0.1 (localhost) is always trusted.
****************************************************************
  Compilation:
    If using the Makefile that came with AFNI, then
        make plugout_tt
    Otherwise
        cc -o plugout_tt -O plugout_tt.c thd_iochan.c -I.

  "thd_iochan.c" contains the routines that do the TCP/IP
  socket and IPC shared memory stuff.
****************************************************************/

/***** Header file for communication routines *****/

#include "thd_iochan.h"

/***** Global variable determining on which system AFNI runs.  *****/
/***** [default is the current system, can be changed by user] *****/

static char afni_host[128] = "." ;
static char afni_name[128] = "\0" ;
static int  afni_port      = 8001 ;

static int  afni_verbose = 0 ;  /* print out debug info? */
static int  afni_do_ijk  = 0 ;  /* do IJK instead of TT? */

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
      printf("Usage: plugout_tt [-host name] [-v]\n"
             "This program connects to AFNI and receives notification\n"
             "whenever the user changes Talairach coordinates.\n\n"
             "Options:\n"
             "  -host name  Means to connect to AFNI running on the\n"
             "                computer 'name' using TCP/IP.  The default is to\n"
             "                connect on the current host using shared memory.\n"
             "  -ijk        Means to get voxel indices from AFNI, rather\n"
             "                than Talairach coordinates.\n"
             "  -v          Verbose mode: prints out lots of stuff.\n"
             "  -port pp    Use TCP/IP port number 'pp'.  The default is\n"
             "                8001, but if two copies of this are running on\n"
             "                the same computer, they must use different ports.\n"
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

      /** -ijk **/

      if( strncmp(argv[narg],"-ijk",4) == 0 ){
         afni_do_ijk = 1 ;
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
       5) See if AFNI sends any data, and if so, process it!  *****/

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
            * TT_XYZ_DELTA means 'send me T-T coordinates when they change';
            * DSET_IJK_DELTA means 'send me IJK voxel indices when they change';
            * PONAME means 'use this string for informative messages';
            * IOCHAN means 'use this I/O channel from now on'. **/

      if( afni_name[0] == '\0' ) strcpy(afni_name,"aManCalledHorse") ;

      if( afni_do_ijk ){
         sprintf( afni_buf , "DSET_IJK_DELTA\n"
                             "PONAME %s\n"
                             "IOCHAN %s" ,
                  afni_name , afni_iocname ) ;
      } else {
         sprintf( afni_buf , "TT_XYZ_DELTA\n"
                             "PONAME %s\n"
                             "IOCHAN %s" ,
                  afni_name , afni_iocname ) ;
      }

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

   /************************************************************/
   /***** The "normal" state of affairs:                   *****/
   /***** AFNI is connected.  See if any data is arriving. *****/

   if( afni_mode == AFNI_CONTINUE_MODE ){
      char afni_buf[256] ;
      float xx , yy , zz ;
      int   ix , jy , kz ;

      ii = iochan_readcheck( afni_ioc , 0 ) ;  /* don't wait */

      /** ii <  0  ==>  a fatal error has happened
          ii == 0  ==>  no data is ready
          ii >  0  ==>  data is ready to read from the channel **/

      if( ii < 0 ){
         fprintf(stderr,"AFNI data channel aborted!\a\n") ;
         IOCHAN_CLOSE(afni_ioc) ;
         afni_mode = 0 ;
         return -1 ;
      } else if( ii == 0 ){
         return 0 ;       /* no data ==> try again next time */
      }

      /** at this point, data is incoming from AFNI **/

      ii = iochan_recv( afni_ioc , afni_buf , 256 ) ;

      if( ii <= 0 ){
         fprintf(stderr,"AFNI data channel recv failed!\a\n") ;
         IOCHAN_CLOSE(afni_ioc) ;
         afni_mode = 0 ;
         return -1 ;
      }

      /** at last! "process" the data from AFNI
                   (in this case, just print it out) **/

      if( afni_do_ijk )
         ii = sscanf( afni_buf , "DSET_IJK %d %d %d" , &ix,&jy,&kz ) ;
      else
         ii = sscanf( afni_buf , "TT_XYZ %f %f %f"   , &xx,&yy,&zz ) ;

      /** also, AFNI will wait until we send an acknowledgment;
          acknowledgment messages are always 4 (POACKSIZE) bytes long **/

      if( ii < 3 ){
         fprintf(stderr,"AFNI sent bad data: %s\a\n",afni_buf) ;
         PO_ACK_BAD(afni_ioc) ;
      } else if( afni_do_ijk ){
         fprintf(stderr,"AFNI sent indices: %d %d %d\n",ix,jy,kz) ;
         PO_ACK_OK(afni_ioc) ;
      } else {
         fprintf(stderr,"AFNI sent coords: %9.3f %9.3f %9.3f\n",xx,yy,zz) ;
         PO_ACK_OK(afni_ioc) ;
      }
   }

   return 0 ;
}
