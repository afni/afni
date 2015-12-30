/***************************************************************
  Sample plugout program to send/receive node id's to AFNI
****************************************************************/

/***** Header file for communication routines *****/

#include "thd_iochan.h"
#include "niml/niml.h"

/***** Global variable determining on which system AFNI runs.  *****/
/***** [default is the current system, can be changed by user] *****/

static char afni_host[128] = "." ;
static char afni_name[128] = "\0" ;
static int  afni_port      = 0 ;  /* Init. before parsing command line  
                                    ZSS June 2011 */
static int  afni_verbose   = 0 ;  /* print out debug info? */

/***** Prototypes *****/

int afni_io(void) ;     /* talk to AFNI */

int got_key(char *) ;   /* keyboard functions (at end of file) */
void setup_key(void) ;
void restore_key(void) ;

/*===================================================================
   Main program:
     Read command line for options
     Call afni_io routine forever, and ever, and ever, and ...
=====================================================================*/

int main( int argc , char * argv[] )
{
   int narg , ii ;

   (void)AFNI_prefilter_args(&argc,argv);

   afni_port = get_port_named("PLUGOUT_SURF_PORT"); /* ZSS June 2011 */
   
   /***** See if the pitiful user wants help *****/

   if( argc == 2 && strncmp(argv[1],"-help",5) == 0 ){
      printf("Usage: plugout_surf [-host name] [-v]\n"
             "This program connects to AFNI and sends/gets surface\n"
             "node IDs to control/know the AFNI viewpoint.\n\n"
             "Options:\n"
             "  -host name  Means to connect to AFNI running on the\n"
             "                computer 'name' using TCP/IP.  The default is to\n"
             "                connect on the current host using shared memory.\n"
             "  -v          Verbose mode.\n"
             "  -port pp    Use TCP/IP port number 'pp'.  The default is\n"
             "                %d, but if two plugouts are running on the\n"
             "                same computer, they must use different ports.\n"
             "                For a list of currently used ports use afni -list_ports\n"
             "  -name sss   Use the string 'sss' for the name that AFNI assigns\n"
             "                to this plugout.  The default is something stupid.\n"
             "\n"
             "To have different plugout_* programs talking to different\n"
             "AFNI, use the -np* options below\n"
             "%s\n"
            , afni_port, get_np_help()) ;
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
      ii = afni_io() ;      /* commune with AFNI  */
      if( ii < 0 ) break ;  /* bad trip? then die */
      iochan_sleep(100) ;   /* perchance to dream */
   }

   restore_key() ; exit(0) ;
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
       5) Send data to AFNI or receive data from AFNI!      *****/

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
   int need_setup_key=1 ;

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
        * Note that the shm control name (here "surf_plugout")
           is a string that will be converted to an IPC
           key (in function string_to_key in iochan.c).       **/

      if( strcmp(afni_host,".") == 0 )
         strcpy( afni_iocname , "shm:surf_plugout:1K+1K" ) ;
      else
         sprintf( afni_iocname , "tcp:%s:%d" , afni_host , afni_port ) ;

      /** write the command to AFNI into the buffer:
            * each command ends with a newline character,
                except (possibly) the last command;
            * the command buffer is a C string, which ends
                with an ASCII NUL character;
            * PONAME means 'use this string for informative messages';
            * IOCHAN means 'use this I/O channel from now on'. **/

      if( afni_name[0] == '\0' ) strcpy(afni_name,"SurfaceHack") ;

      sprintf( afni_buf , "SURFID_DELTA\n"
                          "NO_ACK\n"
                          "PONAME %s\n"
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

      } else {  /* things are cool, real cool */

         /** close channel **/

         IOCHAN_CLOSE(afni_ioc) ;

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
         printf("\rEnter node ID: "); fflush(stdout);
      } else {
         return 0 ;                            /* try again next time */
      }
   }

   /**************************************************************/
   /***** The "normal" state of affairs:  AFNI is connected. *****/
   /***** See if the user wants to send node ID to AFNI,     *****/
   /***** or if AFNI has sent a node ID back to us.          *****/

#define BUF 256

   if( afni_mode == AFNI_CONTINUE_MODE ){
      static char kbuf[BUF] ;  /* input keystroke buffer */
      static int  nkey=0 ;     /* number of keys */
      char afni_buf[BUF] ;     /* text to send to AFNI */
      int  id ;                /* Surface node ID code */

      if( need_setup_key ){ setup_key(); need_setup_key = 0; }

      /* see if AFNI sent anything */

      ii = iochan_readcheck( afni_ioc , 0 ) ;  /* don't wait */

      /** ii <  0  ==>  a fatal error has happened
          ii == 0  ==>  no data is ready
          ii >  0  ==>  data is ready to read from the channel **/

      if( ii < 0 ){        /** bad news on the I/O front **/

         fprintf(stderr,"\nAFNI data channel aborted!\a\n") ;
         IOCHAN_CLOSE(afni_ioc) ;
         afni_mode = 0 ;
         return -1 ;

      } else if( ii > 0 ){ /** at this point, data is incoming from AFNI **/

         ii = iochan_recv( afni_ioc , afni_buf , BUF ) ; /* get data */

         if( ii <= 0 ){    /** receive failed? */
            fprintf(stderr,"\nAFNI data channel recv failed!\a\n") ;
            IOCHAN_CLOSE(afni_ioc) ;
            afni_mode = 0 ;
            return -1 ;
         }

         /** at last! "process" the data from AFNI
                      (in this case, just print it out) **/

#if 1
         printf("AFNI sent: %s\n",afni_buf) ;
#else
         ii = sscanf( afni_buf , "SURFID %d" , &id ) ;  /* extract node id */
         if( ii < 1 ){
            fprintf(stderr,"AFNI sent bad data: %s\a\n",afni_buf) ;
         } else {
            fprintf(stderr,"AFNI sent id: %d\n",id) ;
         }
#endif

         /* give prompt for keyboard input again
            (if had keyboard input, it will be thrown away now) */

         printf("\rEnter node ID: "); fflush(stdout); nkey = 0;

      } /* end of processing input from AFNI */

      /*-- check for user input --*/

      while( got_key(kbuf+nkey) ){ /* loop while we have keystrokes */

         if( isdigit(kbuf[nkey]) ){                    /* save numerals */

            printf("%c",kbuf[nkey++]); fflush(stdout);

         } else if( nkey > 0 && kbuf[nkey] == '\n' ){  /* end of string */

            printf("\n"); fflush(stdout); kbuf[nkey++]='\0'; break;

         }

         if( nkey == BUF ){                            /* stupid user */
            fprintf(stderr,"\nBad boy.\n\a"); exit(1);
         }
      }

      /* check if we have a whole string;
         if not, return; otherwise, send index to AFNI */

      if( nkey == 0 || kbuf[nkey-1] != '\0' ) return 0 ; /* incomplete */

      id = strtol( kbuf , NULL , 10 ) ;
      sprintf(afni_buf,"SURFID %d",id) ;
      ii = iochan_sendall( afni_ioc , afni_buf , strlen(afni_buf)+1 ) ;

      if( ii < 0 ){   /* send failed */
         fprintf(stderr,"AFNI data channel aborted!\a\n") ;
         IOCHAN_CLOSE(afni_ioc) ;
         afni_mode = 0 ;
         return -1 ;
      }

      printf("\rEnter node ID: "); fflush(stdout); nkey = 0;
      return 0 ;
   }

   return 0 ;
}

/*****************************************************************************/

#include <errno.h>
#include <stdlib.h>
#include <strings.h>
#include <curses.h>

/*****************************************************************************/

void setup_key(void)
{
  stdscr = initscr();    /* Initialise curses library */
  noecho();              /* Turn off echo of keypresses to screen */
  cbreak();              /* Set terminal to one-character-at-a-time mode */
  nodelay(stdscr, TRUE); /* Set keyboard to no-delay mode */
}

/*****************************************************************************/

void restore_key(void)
{
  nodelay(stdscr, FALSE); /* Set keyboard to normal (blocking) mode */
  nocbreak();             /* Set terminal to "wait until press return" buffer mode */
  echo();                 /* Restore echo of keypresses to screen */
  endwin();               /* Close down curses library */
}

/*****************************************************************************/
/* This function returns TRUE if a key has been pressed, FALSE otherwise.    */
/* If a key has been pressed, it is put in the supplied pointer location.    */
/*****************************************************************************/

int got_key(char *char_pressed)
{
  int return_code = 0;
  *char_pressed = getch();
  if (*char_pressed != ERR) return_code = TRUE;
  return(return_code);
}
