/******************************************************************
  Sample plugout program to send AFNI commands that drive the AFNI
  interface remotely.  Current commands [07 Nov 2001] are

    RESCAN_THIS  Q
    SET_SESSION  Q.dirname
    SET_ANATOMY  Q.prefix
    SET_FUNCTION Q.prefix
    OPEN_WINDOW  Q.windowname
    CLOSE_WINDOW Q.windowname
    QUIT

  where "Q" is the letter for the AFNI controller you are
  driving (Q=A,B,C,D, or E).  "Q." can be omitted, in which
  case you are driving controller A.  Other parameters are:

    dirname    = name of directory that AFNI should switch to
                   (must already be read into AFNI)
    prefix     = prefix part of dataset name that AFNI should
                   switch to (must be in the current session)
    windowname = axialimage OR coronalimage OR sagittalimage
                 OR axialgraph OR coronalgraph OR sagittalgraph

  Doing OPEN_WINDOW or CLOSE_WINDOW on just the single letter Q
  will open or close controller Q itself.  However, you cannot
  use CLOSE_WINDOW to kill AFNI - it won't close the last
  controller window.

  See README.driver for up-to-date information on what commands
  are available for driving Miss AFNI.

*******************************************************************/

/***** Header file for communication routines *****/
#include "afni_environ.h"
#include "afni_plugout.h"
#include "thd_iochan.h"
#include "niml.h"
#include "cs.h"

/***** Global variable determining on which system AFNI runs.  *****/
/***** [default is the current system, can be changed by user] *****/

static char afni_host[128] = "localhost" ;
static char afni_name[128] = "\0" ;
static int  afni_port      = 0 ;  /* Init. before parsing command line
                                    ZSS June 2011 */
static int  afni_verbose   = 0 ;  /* print out debug info? */
static int  DontWait = 0;
static int  N_com = 0;
static int  I_com = 0;    /* 22 Mar 2005 - RWCox */
static char *com[1024];
/***** Prototype *****/

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

static int afni_mode = AFNI_OPEN_CONTROL_MODE ;  /* status variable */

static int exit_status = 1 ;  /* 1 if never connected, 0 if connected */

static float maxwait = 9.0f ; /* 27 Dec 2010 */

int afni_io(void)
{
   static IOCHAN *afni_ioc = NULL ;                /* connection to AFNI */
   static float delta_t = -1.0;                    /* time spent waiting */
   float Time_Fact = 1000000.0;           /* one meeelllion microseconds */
   static struct  timeval  tw;
   struct  timeval  tn;
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
          TCP/IP channel to port #get_port_named("AFNI_PLUGOUT_TCP_0")
          (used to be #7955) on the AFNI host system **/

      if( strcmp(afni_host,".") == 0 )
         sprintf( afni_iocname , "tcp:%s:%d" ,
            "localhost", get_port_named("AFNI_PLUGOUT_TCP_0") ); /* make name */
      else
         sprintf( afni_iocname , "tcp:%s:%d" ,
            afni_host, get_port_named("AFNI_PLUGOUT_TCP_0") ) ;  /* make name */

      afni_ioc = iochan_init( afni_iocname , "create" ) ;    /* create it */
      if( afni_ioc == NULL ){
         fprintf(stderr,
                 "** Can't create control channel %s to AFNI!\n",afni_iocname) ;
         afni_mode = 0 ;
         return -1 ;
      }
      afni_mode = AFNI_WAIT_CONTROL_MODE ; /* waiting for AFNI connection */
      gettimeofday(&tw, NULL);   /* keep track of time that you began waiting */

      if( afni_verbose )
         fprintf(stderr,"++ AFNI control channel %s created\n", afni_iocname) ;
   }

   /****************************************************/
   /**** Check if AFNI control channel is connected ****/

   if( afni_mode == AFNI_WAIT_CONTROL_MODE ){
      ii = iochan_writecheck( afni_ioc , 5 ) ;     /* wait at most 5 msec */
      gettimeofday(&tn, NULL);   /* what time is it now? */

      /** the iochan_*check() routines return
             -1 for an error,
              0 if not ready,
             >0 if the I/O channel is ready. **/

      if( ii < 0 ){
         fprintf(stderr,"** Control channel to AFNI failed!\a\n") ;
         IOCHAN_CLOSENOW(afni_ioc) ;
         afni_mode = 0 ;
         return -1 ;
      } else if( ii > 0 ){
         afni_mode = AFNI_OPEN_DATA_MODE ;        /* prepare to send data */
         if( afni_verbose )
            fprintf(stderr,"++ AFNI control channel connected\n");
      } else {
         delta_t = (((float)(tn.tv_sec  - tw.tv_sec )*Time_Fact) +
                                                      /* time spent waiting */
                     (float)(tn.tv_usec - tw.tv_usec))/Time_Fact ;
         if (delta_t > maxwait) {
            fprintf(stderr,
                     "** Waited %g seconds to no avail ==> I quit.\n",maxwait);
            IOCHAN_CLOSENOW(afni_ioc) ;
            afni_mode = 0 ;
            return -1 ;
         } else {
            return 0 ;                                /* try again next time */
         }
      }
   }

   /**********************************************************/
   /**** Send control data to AFNI, and open data channel ****/

   if( afni_mode == AFNI_OPEN_DATA_MODE ){
      char afni_iocname[128] ;
      char afni_buf[256] ;
      char shmstr[256];

      exit_status = 0 ;

      /** decide name of data channel:
            use shared memory (shm:) on ".",
            use TCP/IP (tcp:) on other computer systems;
        * Note that the TCP/IP port number can be
           anything that isn't already in use;
        * Note that the shm control name (here "test_plugout")
           is a string that will be converted to an IPC
           key (in function string_to_key in iochan.c).       **/

      if( strcmp(afni_host,".") == 0 ) {
         sprintf( shmstr, "shm:test_plugout:%dK+%dK",
                     PLUGOUT_SHM_SIZE_K, PLUGOUT_SHM_SIZE_K);
         strcpy( afni_iocname , shmstr ) ;
      } else
         sprintf( afni_iocname , "tcp:%s:%d" , afni_host , afni_port ) ;

      /** write the command to AFNI into the buffer:
            * each command ends with a newline character,
                except (possibly) the last command;
            * the command buffer is a C string, which ends
                with an ASCII NUL character;
            * PONAME means 'use this string for informative messages';
            * IOCHAN means 'use this I/O channel from now on'. **/

      if( afni_name[0] == '\0' ) strcpy(afni_name,"plugout_drive") ;

      sprintf( afni_buf , "PONAME %s\n"
                          "IOCHAN %s" ,
               afni_name , afni_iocname ) ;

      if( afni_verbose )
         fprintf(stderr,"++ Sending control information to AFNI\n   %s\n",
                afni_buf) ;

      /** note that the ASCII NUL at the end of the buffer is sent **/

      ii = iochan_sendall( afni_ioc , afni_buf , strlen(afni_buf)+1 ) ;

      /** the return value is the number of bytes sent,
          or -1 indicating a fatal error transpired.    **/

      if( ii < 0 ){
         fprintf(stderr,"** Transmission of control data to AFNI failed!\a\n") ;
         IOCHAN_CLOSENOW(afni_ioc) ;
         afni_mode = 0 ;
         return -1 ;

      } else {

         /** wait for the acknowledgment from AFNI, then close channel **/

         ii = iochan_recvall( afni_ioc , afni_buf , POACKSIZE ) ;
         IOCHAN_CLOSENOW(afni_ioc) ;

         if( ii < 0 || strncmp(afni_buf,"OK!",3) != 0 ){
            fprintf(stderr,"** AFNI didn't like control information!\a\n") ;
            afni_mode = 0 ;
            return -1 ;
         }

         /** now open data channel to AFNI **/

         afni_ioc = iochan_init( afni_iocname , "create" ) ;
         if( afni_ioc == NULL ){
            fprintf(stderr,
                    "** Can't open data channel %s to AFNI!\a\n",afni_iocname) ;
            afni_mode = 0 ;
            return -1 ;
         } else {
            afni_mode = AFNI_WAIT_DATA_MODE ;
            if( afni_verbose ) fprintf(stderr,"++ AFNI data channel created\n") ;
         }
      }
   }

   /****************************************************/
   /***** See if data channel is connected to AFNI *****/

   if( afni_mode == AFNI_WAIT_DATA_MODE ){

      ii = iochan_goodcheck( afni_ioc , 5 ) ;  /* wait at most 5 msec */
      if( ii < 0 ){
         fprintf(stderr,
                 "** AFNI data channel aborted before any data was sent!\a\n") ;
         IOCHAN_CLOSENOW( afni_ioc ) ;
         afni_mode = 0 ;
         return -1 ;
      } else if( ii > 0 ){                     /* ready to go! */
         afni_mode = AFNI_CONTINUE_MODE ;
         if( afni_verbose ) fprintf(stderr,"++ AFNI data channel is open\n") ;
      } else {
         return 0 ;                            /* try again next time */
      }
   }

   /**************************************************************/
   /***** The "normal" state of affairs:  AFNI is connected. *****/
   /***** See if the user wants to drive AFNI.               *****/

   if( afni_mode == AFNI_CONTINUE_MODE ){
      char cmd_buf[PLUGOUT_COM_LENGTH] , afni_buf[PLUGOUT_COM_LENGTH+56];

      if( I_com < N_com ){                   /* send the I_com'th command */
         strcpy(afni_buf, "DRIVE_AFNI ") ;
         strcat(afni_buf, com[I_com]   ) ; strcpy(cmd_buf,com[I_com]) ;
         if (afni_verbose) {
            fprintf(stderr,"Command String %d Echo: '%s'\n",
                           I_com, afni_buf); fflush(stderr) ;
         }
         I_com++ ;
      } else {
         char *qpt ;
         if (DontWait) exit(0);
         /* get user input */

         printf("Enter command: ") ; fflush(stdout) ;
         qpt = afni_fgets(cmd_buf,PLUGOUT_COM_LENGTH,stdin) ;
         if( qpt == NULL ) exit(0) ;  /* 18 Mar 2007 */

         /* make command to AFNI */

         strcpy(afni_buf,"DRIVE_AFNI ") ;
         strcat(afni_buf,cmd_buf) ;
      }

      /* send command to AFNI */

      ii = iochan_sendall( afni_ioc , afni_buf , strlen(afni_buf)+1 ) ;

      if( strcmp(cmd_buf,"QUIT") == 0 ){  /* 28 Jul 2005 */
        iochan_sleep(222) ; exit(0) ;
      }

      if( ii > 0 ){  /* send was OK; wait for acknowledgment */
         ii = iochan_recvall( afni_ioc , afni_buf , POACKSIZE ) ;
         if( ii > 0 && afni_verbose )
           printf("++ AFNI response string: %s\n",afni_buf) ;
      }

      if( ii < 0 ){   /* send or acknowledgment failed */
         fprintf(stderr,"** AFNI data channel aborted!\a\n") ;
         IOCHAN_CLOSENOW(afni_ioc) ;
         afni_mode = 0 ;
         return -1 ;
      }
      return 0 ;
   }

   return 0 ;
}

extern char *SUMA_Offset_SLines(char *, int);

void usage_plugout_drive(int detail)
{
      printf("\n"
"Usage: plugout_drive [-host name] [-v]\n\n"
"This program connects to AFNI and sends commands\n"
" that the user specifies interactively or on command line\n"
" over to AFNI to be executed.\n"
"\n"
"NOTE:\n"
" If you quit plugout_drive and then re-start it immediately\n"
" (as in a script), you might run into problems re-connecting\n"
" to AFNI. The reason is that the TCP/IP system doesn't hang\n"
" up a socket instantly when commanded to do so; the socket\n"
" takes about a second to close down completely. If you are\n"
" writing a script that starts plugout_drive repeatedly, you\n"
" should insert a command 'sleep 1' between each start, to\n"
" give the operating system time to clean the socket up.\n"
" Otherwise, AFNI might not be able to open the socket,\n"
" and plugout_drive will output an error message:\n"
"   ** AFNI didn't like control information!\n"
"\n"
"OPTIONS:\n"
"  -host name    Means to connect to AFNI running on the computer\n"
"                'name' using TCP/IP.  The default is to connect\n"
"                on the current host 'localhost' using TCP/IP.\n"
"\n"
"  -shm          Means to connect to the current host using shared\n"
"                memory.  There is no reason to do this unless\n"
"                you are transferring huge quantities of data.\n"
"                N.B.:  '-host .' is equivalent to '-shm'.\n"
"\n"
"  -v            Verbose mode.\n"
"\n"
"  -port pp      Use TCP/IP port number 'pp'.  The default is\n"
"                %d, but if two plugouts are running on the\n"
"                same computer, they must use different ports.\n"
"                For a list of currently used ports use afni -list_ports\n"
"\n"
"  -maxwait t    Wait a maximum of 't' seconds for AFNI to connect;\n"
"                if the connection doesn't happen in that time, exit.\n"
"                [default wait time is 9 seconds]\n"
"\n"
"  -name sss     Use the string 'sss' for the name that AFNI assigns\n"
"                to this plugout.  The default is something stupid.\n"
"\n"
"  -com 'ACTION DATA'  Execute the following command. For example:\n"
"                       -com 'SET_FUNCTION SomeFunction'\n"
"                       will switch AFNI's function (overlay) to\n"
"                       dataset with prefix SomeFunction. \n"
"                      Make sure ACTION and DATA are together enclosed\n"
"                       in one pair of single quotes.\n"
"                      There are numerous actions listed in AFNI's\n"
"                       README.driver file.\n"
"                      You can use the option -com repeatedly. \n"
"\n"
"  -quit         Quit after you are done with all the -com commands.\n"
"                The default is for the program to wait for more\n"
"                commands to be typed at the terminal's prompt.\n"
"\n"
"NOTES:\n"
"You will need to turn plugouts on in AFNI using one of the\n"
"following methods: \n"
" 1. Including '-yesplugouts' as an option on AFNI's command line\n"
" 2. From AFNI GUI: Define Datamode->Misc->Start Plugouts\n"
" 3. From AFNI GUI: Press the 'NIML+PO' button (near 'Overlay')\n"
" 4. Set environment variable AFNI_YESPLUGOUTS to YES in .afnirc\n"
"Otherwise, AFNI won't be listening for a plugout connection.\n"
"  [AFNI does't listen for socket connections, unless]\n"
"  [it is told to,  in order to avoid the overhead of]\n"
"  [checking for incoming data every few milliseconds]\n"
"\n"
"This program's exit status will be 1 if it couldn't connect\n"
"to AFNI at all.  Otherwise, the exit status will be 0.\n"
"You could use this feature in a script to check if a copy of\n"
"AFNI is ready to rumble, and if not then start one, as in the\n"
"following csh fragment:\n"
"    plugout_drive -maxwait 1 -com 'OPEN_WINDOW axialimage'\n"
"    if( $status == 1 )then\n"
"      afni -yesplugouts &\n"
"      sleep 2 ; plugout_drive -com 'OPEN_WINDOW axialimage'\n"
"    endif\n"
"\n"
"To have different plugout_* programs talking to different\n"
"AFNI, use the -np* options below\n"
"%s\n"
"Global Options (available to all AFNI/SUMA programs)\n"
"%s\n%s"
"Example 1:\n"
"    afni -yesplugouts\n"
"    plugout_drive  -com 'SWITCH_SESSION A.afni'                       \\\n"
"                   -com 'OPEN_WINDOW A.axialimage geom=600x600+416+44 \\\n"
"                         ifrac=0.8 opacity=9'                         \\\n"
"                   -com 'OPEN_WINDOW A.sagittalimage geom=+45+430     \\\n"
"                         ifrac=0.8 opacity=9'                         \\\n"
"                   -com 'SWITCH_UNDERLAY anat'                        \\\n"
"                   -com 'SWITCH_OVERLAY strip'                        \\\n"
"                   -com 'SEE_OVERLAY +'                               \\\n"
"                   -com 'SET_DICOM_XYZ 7 12 2'                        \\\n"
"                   -com 'OPEN_WINDOW A.axialimage keypress=v'         \\\n"
"                   -quit             \n"
"\n"
"More help in: README.driver\n"
"More Demos is: @DriveAfni\n"
"\n"
   , afni_port, get_np_help(), SUMA_Offset_SLines(get_help_help(),2),
    get_gopt_help() );
   return;
}

/*===================================================================
   Main program:
     Read command line for options
     Call afni_io routine forever
=====================================================================*/

int main( int argc , char *argv[] )
{
   int narg , ii;

   (void)AFNI_prefilter_args(&argc,argv);

   afni_port = get_port_named("PLUGOUT_DRIVE_PORT"); /* ZSS June 2011 */

   /***** See if the pitiful user wants help *****/

   if( argc == 2 && (
      strcasecmp(argv[1],"-help") == 0 ||
      strcasecmp(argv[1],"-h")    == 0)  ){
      usage_plugout_drive(strlen(argv[1]) > 3 ? 2:1);
      exit (0);
   }

   /***** Process command line options *****/

   N_com = 0;
   DontWait = 0;
   narg = 1 ;
   while( narg < argc ){

      /** -maxwait [27 Dec 2010] **/

      if( strcmp(argv[narg],"-maxwait") == 0 ){
        if( ++narg >= argc ){
           fprintf(stderr,"** -maxwait needs a following number!\a\n"); exit(1);
        }
        maxwait = (float)strtod(argv[narg],NULL) ;
        if( maxwait <= 0.0f ) maxwait = 9.0f ;
        narg++ ; continue ;
      }

      /** -shm [20 May 2008] **/

      if( strcmp(argv[narg],"-shm") == 0 ){
        strcpy( afni_host , "." ) ;
        narg++ ; continue ;
      }

      /** -host name **/

      if( strncmp(argv[narg],"-host",5) == 0 ){
         narg++ ;
         if( narg >= argc ){
            fprintf(stderr,"** -host needs a following name!\a\n"); exit(1);
         }
         strcpy( afni_host , argv[narg] ) ;
         narg++ ; continue ;
      }

      /** -name sss **/

      if( strncmp(argv[narg],"-name",5) == 0 ){
         narg++ ;
         if( narg >= argc ){
            fprintf(stderr,"** -name needs a following string!\a\n"); exit(1);
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
            fprintf(stderr,"** -port needs a following argument!\a\n"); exit(1);
         }
         afni_port = strtol( argv[narg] , NULL , 10 ) ;
         if( afni_port <= 1024 ){
            fprintf(stderr,"** -port needs an argument > 1024!\a\n"); exit(1);
         }
         if( strcmp(afni_host,".") == 0 ) strcpy(afni_host,"localhost") ;
         narg++ ; continue ;
      }

      /*** -com 'command this' */
      if( strncmp(argv[narg],"-com",4) == 0 ){
         narg++ ;
         if( narg >= argc ){
            fprintf(stderr,"** -com needs a following argument!\a\n"); exit(1);
         }

         if (argv[narg] && strlen(argv[narg]) >= PLUGOUT_COM_LENGTH) {
            fprintf(stderr,
               "** Command length must be smaller than %d characters.\n"
               "   If you really need a longer command let us know.\n"
               "   Your command is %d characters long.\n" ,
               PLUGOUT_COM_LENGTH, (int)strlen(argv[narg]));
            exit(1);
         }

         if (N_com < 1024) {
            com[N_com] = argv[narg];
            ++N_com;
         } else {
            fprintf( stderr,
                     "** Only 1024 -com options allowed. Are you nuts?\a\n");
            exit(1);
         }

         narg++ ; continue ;
      }

      /*** -quit */
      if( strncmp(argv[narg],"-quit",5) == 0 ){
         DontWait = 1 ;
         narg++ ; continue ;
      }

      /** Je ne sais pas **/

      fprintf(stderr,"** Unrecognized option: %s\a\n",argv[narg]) ;
      suggest_best_prog_option(argv[0], argv[narg]);
      exit(1) ;
   }

   if (DontWait && !N_com) {
      fprintf( stderr,
               "** WARNING: -quit option is meaningless without -com option.\n");
      DontWait = 0;
   }

   /***** Loop and check in with AFNI every 100 msec *****/

   iochan_enable_perror(0) ;  /* 27 Dec 2010 */

   while( 1 ){
      ii = afni_io() ;                  /* commune with AFNI  */
      if( ii < 0 ) exit(exit_status) ;  /* bad trip? then die */
      iochan_sleep(100) ;               /* perchance to dream */
   }

}
