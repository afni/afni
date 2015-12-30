/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include <math.h>
#include "thd_iochan.h"
#include "niml/niml.h"

/***** Global variable determining on which system AFNI runs.  *****/
/***** [default is the current system, can be changed by user] *****/

static char afni_host[128] = "." ;
static char afni_name[128] = "\0" ;
static int  afni_port      = 0 ;  /* Init. before parsing command line  
                                    ZSS June 2011 */

static int  afni_verbose = 0 ;  /* print out debug info? */

static char * url = NULL ;
static char * uff = NULL ;

/***** Prototype *****/

int afni_io(void) ;
void handle_tta( float xx , float yy , float zz ) ;

void exit_me_baby( void ){
   if( uff != NULL ) unlink(uff) ;
   return ;
}

#include <signal.h>
void sigfunc(int sig)   /** signal handler for fatal errors **/
{
   exit(1) ;
}

/*===================================================================
   Main program:
     Read command line for options
     Call afni_io routine forever
=====================================================================*/

int main( int argc , char * argv[] )
{
   int narg , ii ;

   (void)AFNI_prefilter_args(&argc,argv);

   afni_port = get_port_named("PLUGOUT_TTA_PORT"); /* ZSS June 2011 */
   
   /***** See if the pitiful user wants help *****/

   if( argc == 2 && strncmp(argv[1],"-help",5) == 0 ){
      printf("Usage: plugout_tta [options]\n"
             "This program connects to AFNI and receives notification\n"
             "whenever the user changes Talairach coordinates.\n"
             "It then drives Netscape to display the closest figures\n"
             "from the Talairach-Tournoux atlas.  Note that Netscape must\n"
             "be running on the same computer as this plugout, since it\n"
             "communicates with Netscape using a temporary URL file.\n"
             "Options:\n"
             "  -host name  Means to connect to AFNI running on the\n"
             "                computer 'name' using TCP/IP.  The default is to\n"
             "                connect on the current host using shared memory.\n"
             "                To connect to the current host with TCP/IP, use\n"
             "                '-host localhost', or use the '-port' option.\n"
             "              For a list of currently used ports use afni -list_ports\n"
             "  -v          Verbose mode: prints out progress reports.\n"
             "  -port pp    Use TCP/IP port number 'pp'; default is %d.\n"
             "The environment variable AFNI_TTAHOME controls where the atlas\n"
             "images are loaded from.  If not given, a default value is used.\n"
             "\n"
             "To have different plugout_* programs talking to different\n"
             "AFNI, use the -np* options below\n"
             "%s\n"
            , afni_port, get_np_help()) ;
      exit(0) ;
   }

   /* set up to delete the temporary URL file when the program ends */

   atexit(exit_me_baby) ;
   signal(SIGINT ,sigfunc) ;
   signal(SIGBUS ,sigfunc) ;
   signal(SIGSEGV,sigfunc) ;
   signal(SIGTERM,sigfunc) ;

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

      if( afni_name[0] == '\0' ) strcpy(afni_name,"T-T-A") ;

      sprintf( afni_buf , "TT_XYZ_DELTA\n"
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

      ii = sscanf( afni_buf , "TT_XYZ %f %f %f"   , &xx,&yy,&zz ) ;

      /** also, AFNI will wait until we send an acknowledgment;
          acknowledgment messages are always 4 (POACKSIZE) bytes long **/

      if( ii < 3 ){
         fprintf(stderr,"AFNI sent bad data: %s\a\n",afni_buf) ;
         PO_ACK_BAD(afni_ioc) ;
      } else {
         PO_ACK_OK(afni_ioc) ;
         if( afni_verbose )
            fprintf(stderr,"AFNI sent TT coords %9.3f %9.3f %9.3f\n",xx,yy,zz) ;
         handle_tta(xx,yy,zz) ;
      }
   }

   return 0 ;
}

/*------------------------------------------------------------------*/

#define CORONAL_NUM 38
static float coronal_yy[] = {
  -100, -95, -90, -85, -80, -75, -70, -65,
   -60, -55, -50, -45, -40, -35, -32, -28,
   -24, -20, -16, -12, -8,  -4,   0,   4,
     8,  12,  16,  20, 24,  28,  32,  35,
    40,  45,  50,  55, 60,  65
} ;
static char * coronal_ff[] = {
   "tt_corm99.gif" , "tt_corm95.gif" , "tt_corm90.gif" , "tt_corm85.gif" ,
   "tt_corm80.gif" , "tt_corm75.gif" , "tt_corm70.gif" , "tt_corm65.gif" ,
   "tt_corm60.gif" , "tt_corm55.gif" , "tt_corm50.gif" , "tt_corm45.gif" ,
   "tt_corm40.gif" , "tt_corm35.gif" , "tt_corm32.gif" , "tt_corm28.gif" ,
   "tt_corm24.gif" , "tt_corm20.gif" , "tt_corm16.gif" , "tt_corm12.gif" ,
   "tt_corm08.gif" , "tt_corm04.gif" , "tt_corp00.gif" , "tt_corp04.gif" ,
   "tt_corp08.gif" , "tt_corp12.gif" , "tt_corp16.gif" , "tt_corp20.gif" ,
   "tt_corp24.gif" , "tt_corp28.gif" , "tt_corp32.gif" , "tt_corp35.gif" ,
   "tt_corp40.gif" , "tt_corp45.gif" , "tt_corp50.gif" , "tt_corp55.gif" ,
   "tt_corp60.gif" , "tt_corp65.gif"
} ;

#define SAGITTAL_NUM 18
static float sagittal_xx[] = {
   0, 3, 5, 9, 13, 17, 21, 25, 29, 33 ,
   37, 41, 43, 47, 51, 55, 59, 61
} ;
static char * sagittal_ff[] = {
   "tt_sag00g.gif", "tt_sag03.gif" , "tt_sag05.gif" , "tt_sag09.gif" ,
   "tt_sag13.gif" , "tt_sag17.gif" , "tt_sag21.gif" , "tt_sag25.gif" ,
   "tt_sag29.gif" , "tt_sag33.gif" , "tt_sag37.gif" , "tt_sag41.gif" ,
   "tt_sag43.gif" , "tt_sag47.gif" , "tt_sag51.gif" , "tt_sag55.gif" ,
   "tt_sag59.gif" , "tt_sag61.gif"
} ;

#define AXIAL_NUM 27
static float axial_zz[] = {
   -40 , -36 , -32 , -28 , -24 , -20 , -16 , -12 , -8 , -4 ,
   -1 , 1 , 4 , 8 , 12 , 16 , 20 , 24 , 28 , 32 , 35 ,
   40 , 45 , 50 , 55 , 60 , 65
} ;
static char * axial_ff[] = {
   "tt_horm40.gif" , "tt_horm36.gif" , "tt_horm32.gif" , "tt_horm28.gif" ,
   "tt_horm24.gif" , "tt_horm20.gif" , "tt_horm16.gif" , "tt_horm12.gif" ,
   "tt_horm08.gif" , "tt_horm04.gif" , "tt_horm01.gif" , "tt_horp01.gif" ,
   "tt_horp04.gif" , "tt_horp08.gif" , "tt_horp12.gif" , "tt_horp16.gif" ,
   "tt_horp20.gif" , "tt_horp24.gif" , "tt_horp28.gif" , "tt_horp32.gif" ,
   "tt_horp35.gif" , "tt_horp40.gif" , "tt_horp45.gif" , "tt_horp50.gif" ,
   "tt_horp55.gif" , "tt_horp60.gif" , "tt_horp65.gif"
} ;

#define TTAHOME "http://varda.biophysics.mcw.edu/~cox/TTA/"

void handle_tta( float xx , float yy , float zz )
{
   char * tname ;
   int ii,jj,kk , qqq ;
   static int ii_old=-1 , jj_old=-1 , kk_old=-1 ;
   FILE * fp ;
   static char nbuf[444] ;
   static char * ttahome ;

   /* create temporary URL */

   if( url == NULL ){
      tname = tempnam(NULL,"tta") ;
      url   = (char *) malloc(strlen(tname)+16) ;
      uff   = url + 5 ;
      strcpy(url,"file:") ; strcat(url,tname) ; strcat(url,".html") ;
      free(tname) ;
      sprintf(nbuf,"netscape -remote 'openURL(%s)'" , url ) ;
      fprintf(stderr,"Temporary URL file is %s\n",uff) ;

      ttahome = getenv( "AFNI_TTAPATH" ) ;
      if( ttahome == NULL ) ttahome = TTAHOME ;
   }

   /* find sagittal image */

   xx = fabs(xx) ;
   if( xx <= sagittal_xx[0] ){
      ii = 0 ;
   } else if( xx >= sagittal_xx[SAGITTAL_NUM-1] ){
      ii = SAGITTAL_NUM - 1 ;
   } else {
      for( ii=1 ; ii < SAGITTAL_NUM && xx > sagittal_xx[ii] ; ii++ ) ; /* nada */
      if( fabs(xx-sagittal_xx[ii-1]) < fabs(xx-sagittal_xx[ii]) ) ii-- ;
   }

   /* find coronal image */

   if( yy <= coronal_yy[0] ){
      jj = 0 ;
   } else if( yy >= coronal_yy[CORONAL_NUM-1] ){
      jj = CORONAL_NUM - 1 ;
   } else {
      for( jj=1 ; jj < CORONAL_NUM && yy > coronal_yy[jj] ; jj++ ) ; /* nada */
      if( fabs(yy-coronal_yy[jj-1]) < fabs(yy-coronal_yy[jj]) ) jj-- ;
   }

   /* find axial image */

   if( zz <= axial_zz[0] ){
      kk = 0 ;
   } else if( zz >= axial_zz[AXIAL_NUM-1] ){
      kk = AXIAL_NUM - 1 ;
   } else {
      for( kk=1 ; kk < AXIAL_NUM && zz > axial_zz[kk] ; kk++ ) ; /* nada */
      if( fabs(zz-axial_zz[kk-1]) < fabs(zz-axial_zz[kk]) ) kk-- ;
   }

   if( ii == ii_old && jj == jj_old && kk == kk_old ) return ;

   /* write out url file */

   fp = fopen( uff , "w" ) ;
   if( fp == NULL ){ fprintf(stderr,"Can't write URL file\n") ; return ; }

   fprintf(fp , "<HEAD>\n"
                "<title>T-T Atlas Pages</title>\n"
                "</HEAD>\n"
                "<BODY>\n"
                "<img align=middle src=\"%s%s\"><p>\n"
                "<img align=middle src=\"%s%s\"><p>\n"
                "<img align=middle src=\"%s%s\"><p>\n"
                "</BODY>\n" ,
           ttahome , axial_ff[kk] ,
           ttahome , coronal_ff[jj] ,
           ttahome , sagittal_ff[ii] ) ;

   fclose(fp) ;

   if( afni_verbose )
      fprintf(stderr,"Axial=%s  Coronal=%s  Sagittal=%s\n",
              axial_ff[kk] , coronal_ff[jj] , sagittal_ff[ii] ) ;

   /* send message to Netscape */

   qqq = system(nbuf) ;
   if( qqq != 0 )
      fprintf(stderr,"Can't send command to Netscape - is it running?\n") ;

   ii_old = ii ; jj_old = jj ; kk_old = kk ;
   return ;
}
