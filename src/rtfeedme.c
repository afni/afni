/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

#define MAX_CHAN 32  /* 02 Aug 2002: cf. plug_realtime.c */

static THD_3dim_dataset * RT_dset[MAX_CHAN] ;
static float              RT_dt             = 0.0 ;
static int                RT_3D             = 0 ;
static int                RT_swap2          = 0 ;
static char               RT_buf[32768] , RT_com[1024] ;
static int                RT_mega = 1 ;

/*=============================================================================*/

#define AFNI_CONTROL_PORT  7954         /* always send control data to AFNI    */
#define AFNI_TCP_PORT      7953         /* maybe send image data to AFNI       */

#define AFNI_OPEN_CONTROL_MODE   1      /* 1st time thru: open control channel */
#define AFNI_WAIT_CONTROL_MODE   2      /* waiting for AFNI to open control    */
#define AFNI_OPEN_DATA_MODE      3      /* now can open data channel to AFNI   */
#define AFNI_CATCHUP_MODE        4      /* waiting for AFNI to open data       */
#define AFNI_CONTINUE_MODE       5      /* at last! data channel is ready!     */

/*-- global control variables --*/

int      AFNI_mode        = 0 ;           /* if > 0, then means AFNI is active  */
int      AFNI_use_tcp     = 0 ;           /* if > 0, use TCP/IP to send images */
char     AFNI_host[128]   = "localhost" ; /* hostname of CPU AFNI is on       */
char     AFNI_iochan[128] = "\0" ;        /* I/O channel name to AFNI        */
IOCHAN * AFNI_ioc         = NULL ;        /* ptr to I/O channel itself      */
char     AFNI_buf[1024]          ;        /* temporary space               */
int      AFNI_verbose     = 0    ;        /* debugging mode               */

char     AFNI_infocom[256]= "\0" ;        /* command for AFNI info */

/*-- prototypes --*/

extern void AFNI_start_io(void) ;
extern void AFNI_exit(void) ;

/*-- how to execute a command on another system --*/

#ifdef HP
# define RSH "remsh"
#else
# define RSH "rsh"
#endif

/*=============================================================================*/

void AFNI_exit(void)                   /* Function to be called to make sure */
{                                      /* the AFNI data channels get closed. */
   iochan_close(AFNI_ioc) ;
   return ;
}

/*****************************************************************************
  Do I/O startup stuff.

  At any given moment, this routine is in one of a number of modes
  (the AFNI_mode variable).
  The first time in, AFNI_mode == AFNI_OPEN_CONTROL_MODE.  In each mode,
  certain tasks must be accomplished and this program must be synchronized
  with AFNI.  When the necessary deeds are done, the routine advances to
  the next mode.  If the deeds cannot be done when this routine is called,
  then it will stay in the same mode, and the next time it is called it
  will try to do them again.  This routine should be called repeatedly
  until it progresses to the last mode (AFNI_CONTINUE_MODE), which is for
  normal transmission of images (one at a time) to AFNI.

  If an error occurs, so that this program can no longer talk to AFNI, then
  AFNI_mode is set to 0, which means "do nothing further".  The rest of
  the data acquisition software will continue, but these routines will
  be stopped dead.
******************************************************************************/

void AFNI_start_io( void )
{
   int ii , jj ;

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
#if 0
         AFNI_mode = 0 ;                       /* disable AFNI */
#endif
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

      if( AFNI_use_tcp ) sprintf(AFNI_iochan,"tcp:%s:%d",AFNI_host,AFNI_TCP_PORT) ;
      else               sprintf(AFNI_iochan,"shm:grv:%dM",RT_mega) ;

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
      }
   }

   return ;
}

/*****************************************************************************/

int main( int argc , char * argv[] )
{
   int iarg=1 , ii,tt,kk , nbytes , nbslice , ntran , nzfake=0 ;
   char * bar , * qar , * sar ;
   double start_time , left_time , xtime ;
   char * drive_afni[128] ;
   int   ndrive=0 ;
   int   num_chan , cur_chan , cc ;
   char * note[128] ;   /* 02 Oct 2002 */
   int   num_note=0 ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf(
        "Usage: rtfeedme [options] dataset [dataset ...]\n"
        "Test the real-time plugin by sending all the bricks in 'dataset' to AFNI.\n"
        " * 'dataset' may include a sub-brick selector list.\n"
        " * If more than one dataset is given, multiple channel acquisition\n"
        "    will be simulated.  Each dataset must then have the same datum\n"
        "    and dimensions.\n"
        "\n"
        "Options:\n"
        "  -host sname =  Send data, via TCP/IP, to AFNI running on the\n"
        "                 computer system 'sname'.  By default, uses the\n"
        "                 current system, and transfers data using shared\n"
        "                 memory.  To send on the current system using\n"
        "                 TCP/IP, use the system 'localhost'.\n"
        "\n"
        "  -dt ms      =  Tries to maintain an inter-transmit interval of\n"
        "                 'ms' milliseconds.  The default is to send data\n"
        "                 as fast as possible.\n"
        "\n"
        "  -3D         =  Sends data in 3D bricks.  By default, sends in\n"
        "                 2D slices.\n"
        "\n"
        "  -buf m      =  When using shared memory, sets the interprocess\n"
        "                 communications buffer to 'm' megabytes.  Has no\n"
        "                 effect if using TCP/IP.  Default is m = 1.\n"
        "\n"
        "  -verbose    =  Be talkative about actions.\n"
        "  -swap2      =  Swap byte pairs before sending data.\n"
        "\n"
        "  -nzfake nz  =  Send 'nz' as the value of nzz (for debugging).\n"
        "\n"
        "  -drive cmd  =  Send 'cmd' as a DRIVE_AFNI command; e.g.,\n"
        "                   -drive 'OPEN_WINDOW A.axialimage'\n"
        "                 If cmd contains blanks, it must be in 'quotes'.\n"
        "                 Multiple -drive options may be used.\n"
        "\n"
        "  -note sss   =  Send 'sss' as a NOTE to the realtime plugin.\n"
        "                 Multiple -note options may be used.\n"
      ) ;
      exit(0) ;
   }

   mainENTRY("rtfeedme") ;

   /*-- scan arguments --*/

   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strcmp(argv[iarg],"-drive") == 0 ){   /* 30 Jul 2002 */
         drive_afni[ndrive++] = argv[++iarg] ;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-note") == 0 ){    /* 02 Oct 2002 */
         note[num_note++] = argv[++iarg] ;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-nzfake") == 0 ){
         nzfake = (int) strtod( argv[++iarg] , NULL ) ;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-buf") == 0 ){
         RT_mega = (int) strtod( argv[++iarg] , NULL ) ;
         if( RT_mega < 1 ){
            fprintf(stderr,"*** Illegal value after -buf\n") ; exit(1) ;
         }
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-host") == 0 ){
         strcpy( AFNI_host , argv[++iarg] ) ;
         AFNI_use_tcp = 1 ;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-dt") == 0 ){
         RT_dt = strtod( argv[++iarg] , NULL ) * 0.001 ;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-3D") == 0 ){
         RT_3D = 1 ;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-swap2") == 0 ){
         RT_swap2 = 1 ;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-verbose") == 0 ){
         AFNI_verbose = 1 ;
         iarg++ ; continue ;
      }

      fprintf(stderr,"*** Unrecognized option: %s\n",argv[iarg]) ;
      exit(1) ;
   }

   /* count number of remaining arguments (each one a dataset) */

   num_chan = argc - iarg ;

   if( num_chan <= 0 ){
     fprintf(stderr,"*** No dataset argument?\n"); exit(1);
   }

   if( num_chan > MAX_CHAN ){
     fprintf(stderr,"*** Too many datasets on command line!\n"); exit(1);
   }

   /*-- read the input dataset(s) --*/

   for( cc=0 ; cc < num_chan ; cc++ ){

     RT_dset[cc] = THD_open_dataset( argv[iarg+cc] ) ;

     if( RT_dset[cc] == NULL ){
       fprintf(stderr,"*** Can't open dataset %s\n",argv[iarg+cc]); exit(1);
     }

     if( cc > 0 ){  /* check for compatibility with #0 */

#define ERREX(ee)                                                           \
  do { fprintf(stderr,"*** " ee ":%s and %s\n",argv[iarg],argv[iarg+cc]) ;  \
       exit(1) ; } while(0)

       if( DSET_NX   (RT_dset[0]) != DSET_NX   (RT_dset[cc]) ) ERREX("nx mismatch") ;
       if( DSET_NY   (RT_dset[0]) != DSET_NY   (RT_dset[cc]) ) ERREX("ny mismatch") ;
       if( DSET_NZ   (RT_dset[0]) != DSET_NZ   (RT_dset[cc]) ) ERREX("nz mismatch") ;

       if( DSET_NVALS(RT_dset[0]) != DSET_NVALS(RT_dset[cc]) ) ERREX("nvals mismatch") ;

       if( DSET_BRICK_TYPE(RT_dset[0],0) != DSET_BRICK_TYPE(RT_dset[cc],0) )
                                                               ERREX("datum mismatch") ;
     }

     /* load from disk */

     DSET_load(RT_dset[cc]) ;
     if( !DSET_LOADED(RT_dset[cc]) ){
       fprintf(stderr,"*** Can't load dataset %s\n",argv[iarg+cc]); exit(1);
     }
   } /* end of loop over channels (datasets) */

   /*-- initiate communications with AFNI --*/

   if( AFNI_verbose ) fprintf(stderr,"--- Starting I/O to AFNI\n") ;

   atexit(AFNI_exit) ;                   /* call this when program ends */
   AFNI_mode = AFNI_OPEN_CONTROL_MODE ;
   AFNI_start_io() ;

   ii = 1 ;
   while( AFNI_mode > 0 && AFNI_mode != AFNI_CONTINUE_MODE && ii < 1000 ){
      iochan_sleep( 30 ) ;  /* 30 msec wait */
      AFNI_start_io() ;
      ii++ ;
   }

   if( AFNI_mode != AFNI_CONTINUE_MODE ){
      fprintf(stderr,"\n*** Can't connect to AFNI?!\n") ; exit(1) ;
   }

   if( AFNI_verbose )
     fprintf(stderr,"\n--- Connection to AFNI is ready after %d tries\n",ii) ;

   /*-- Send dataset control information --*/

#define ADDTO_BUF ( strcat(RT_buf,RT_com) , strcat(RT_buf,"\n") )

   RT_buf[0] = '\0' ;   /* string to hold commands to AFNI realtime plugin */

   /*** Number of channels [Aug 2002] ***/

   if( num_chan > 1 ){
     sprintf(RT_com,"NUM_CHAN %d",num_chan) ;
     ADDTO_BUF ;
   }

   /*** How the data will be sent ***/

   strcpy(RT_com,"ACQUISITION_TYPE ") ;
   if( DSET_NVALS(RT_dset[0]) == 1 ){
      if( RT_3D ) strcat(RT_com,"3D") ;
      else        strcat(RT_com,"2D+z") ;
   } else {
      if( RT_3D ) strcat(RT_com,"3D+t") ;
      else        strcat(RT_com,"2D+zt") ;
   }
   ADDTO_BUF ;

   /*** Time step, if needed ***/

   if( DSET_NVALS(RT_dset[0]) > 1 && DSET_TR(RT_dset[0]) > 0.0 ){
      float TR = DSET_TR(RT_dset[0]) ;
      if( DSET_TIMEUNITS(RT_dset[0]) == UNITS_MSEC_TYPE ) TR *= 0.001 ;
      sprintf( RT_com , "TR %f" , TR ) ;
      ADDTO_BUF ;
   }

   /*** Volume dimensions ***/

   sprintf( RT_com, "XYFOV %f %f %f", fabs(DSET_DX(RT_dset[0]) * DSET_NX(RT_dset[0])) ,
                                      fabs(DSET_DY(RT_dset[0]) * DSET_NY(RT_dset[0])) ,
                                      fabs(DSET_DZ(RT_dset[0]) * DSET_NZ(RT_dset[0]))  ) ;
   ADDTO_BUF ;

   /*** Matrix sizes ***/

   if( nzfake <= 0 ){
      sprintf( RT_com , "XYMATRIX %d %d %d" , DSET_NX(RT_dset[0]) ,
                                              DSET_NY(RT_dset[0]) ,
                                              DSET_NZ(RT_dset[0])  ) ;
      ADDTO_BUF ;
   } else {
      sprintf( RT_com , "XYMATRIX %d %d" , DSET_NX(RT_dset[0]) ,
                                           DSET_NY(RT_dset[0])  ) ;
      ADDTO_BUF ;
      sprintf( RT_com , "ZNUM %d" , nzfake ) ;
      ADDTO_BUF ;
   }

   /*** Data type ***/

   sprintf( RT_com , "DATUM %s" , MRI_TYPE_name[ DSET_BRICK_TYPE(RT_dset[0],0) ] ) ;
   ADDTO_BUF ;

   /*** Slice order ***/

   if( ! RT_3D ){
      strcpy( RT_com , "ZORDER seq" ) ;
      ADDTO_BUF ;
   }

   /*** Axes orientation ***/

   sprintf( RT_com , "XYZAXES %s %s %s" ,
            ORIENT_shortstr[ RT_dset[0]->daxes->xxorient ] ,
            ORIENT_shortstr[ RT_dset[0]->daxes->yyorient ] ,
            ORIENT_shortstr[ RT_dset[0]->daxes->zzorient ]  ) ;
   ADDTO_BUF ;

   /*** DRIVE_AFNI commands [Jul 2002] ***/

   for( ii=0 ; ii < ndrive ; ii++ ){
     sprintf( RT_com , "DRIVE_AFNI %s" , drive_afni[ii] ) ;
     ADDTO_BUF ;
   }

   /*** NOTE commands [02 Oct 2002] ***/

   for( ii=0 ; ii < num_note ; ii++ ){
     sprintf( RT_com , "NOTE %s" , note[ii] ) ;
     ADDTO_BUF ;
   }

   /*** send to AFNI ***/

   if( AFNI_verbose )
      fprintf(stderr,"--- Dataset control info for AFNI:\n%s",RT_buf) ;

   ii = iochan_sendall( AFNI_ioc , RT_buf , strlen(RT_buf)+1 ) ;
   if( ii < 0 ){
      fprintf(stderr,"*** Error sending dataset control info to AFNI\n") ;
      exit(1) ;
   }

   iochan_sleep(128) ;

   /*--- send slices or volumes to AFNI ---*/

   nbslice = nbytes = mri_datum_size( DSET_BRICK_TYPE(RT_dset[0],0) )
                      * DSET_NX(RT_dset[0]) * DSET_NY(RT_dset[0]) ;

   if( RT_3D ) nbytes *= DSET_NZ(RT_dset[0]) ;

   if( RT_swap2 ) qar = (char *) malloc( sizeof(char) * nbytes ) ;
   else           qar = NULL ;

   xtime = COX_clock_time() ;
   ntran = DSET_NVALS(RT_dset[0]) * num_chan ;
   if( !RT_3D ) ntran *= DSET_NZ(RT_dset[0]) ;

   for( tt=0 ; tt < DSET_NVALS(RT_dset[0]) ; tt++ ){  /* loop over time points */

      if( RT_3D ){                          /* send 3D arrays */

       for( cc=0 ; cc < num_chan ; cc++ ){  /* loop over channels (datasets) */

         bar = DSET_ARRAY(RT_dset[cc],tt) ; /* array to send */

         if( AFNI_verbose )
           fprintf(stderr,"--- Sending brick %d, channel %02d\n",tt,cc+1) ;

         if( RT_dt > 0.0 ) start_time = COX_clock_time() ;

         sar = bar ;
         if( RT_swap2 ){                    /* swap bytes? */
            memcpy(qar,sar,nbytes) ; sar = qar ;
            mri_swap2( nbytes/2 , (short *) sar ) ;
         }

         /* send the whole 3D array */

         ii = iochan_sendall( AFNI_ioc , sar , nbytes ) ;
         if( ii < 0 ){
            fprintf(stderr,
                    "*** Error sending brick %d, channel %02d, to AFNI\n",
                    tt,cc+1) ;
            exit(1) ;
         }

         if( RT_dt > 0.0 ){  /* wait for prescribed transmission time */
            left_time = RT_dt - ( COX_clock_time() - start_time ) ;
            if( left_time >= 0.001 ){
               ii = (int) (1000.0 * left_time) ;
               iochan_sleep( ii ) ;
            }
         }
       } /* end of loop over channels */

      /* send 2D slices from each channel in turn */

      } else {

         for( kk=0 ; kk < DSET_NZ(RT_dset[0]) ; kk++ ){  /* loop over slices */
          for( cc=0 ; cc < num_chan ; cc++ ){            /* loop over channels */

            bar = DSET_ARRAY(RT_dset[cc],tt) ;  /* 3D array to get slice from */

            if( AFNI_verbose )
              fprintf(stderr,"--- Sending brick %d, slice %d, channel %02d\n",
                             tt,kk,cc+1) ;

            if( RT_dt > 0.0 ) start_time = COX_clock_time() ;

            sar = bar+(kk*nbslice) ;  /* pointer to start of slice data */

            if( RT_swap2 ){           /* byte swapping */
               memcpy(qar,sar,nbslice) ; sar = qar ;
               mri_swap2( nbslice/2 , (short *) sar ) ;
            }

            /* send slice data */

            ii = iochan_sendall( AFNI_ioc , sar , nbslice ) ;

            if( ii < 0 ){
              fprintf(stderr,
                      "*** Error sending slice brick %d, slice %d, channel %02d to AFNI\n",
                      tt,kk,cc+1) ;
              exit(1) ;
            }

            if( RT_dt > 0.0 ){  /* wait for prescribed transmission time */
              left_time = RT_dt - ( COX_clock_time() - start_time ) ;
              if( left_time >= 0.001 ){
                 ii = (int) (1000.0 * left_time) ;
                 iochan_sleep( ii ) ;
              }
            }

          } /* end of loop over channels */
         } /* end of loop over slices */
      }

      /** unload dataset bricks we just sent **/

      for( cc=0 ; cc < num_chan ; cc++ )
        DSET_unload_one( RT_dset[cc] , tt ) ;

   } /* end of loop over time points */

   /* cleanup */

   xtime = COX_clock_time() - xtime ;  /* total transmit time */

   for( cc=0 ; cc < num_chan ; cc++ )  /* unload all datasets */
      DSET_delete( RT_dset[cc] ) ;

   if( qar != NULL ) free(qar) ;       /* free swap space */

   /* make sure all data is transmitted to AFNI */

   if( AFNI_verbose ) fprintf(stderr,"--- Clearing buffer") ;
   iochan_sleep(100) ;
   while( ! iochan_clearcheck(AFNI_ioc,100) ){
      if( AFNI_verbose ) fprintf(stderr,".") ;
   }
   if( AFNI_verbose ) fprintf(stderr,"\n") ;

   fprintf(stderr,
           "--- Elapsed transmit time = %f s (%f per transmit)\n",
           xtime,xtime/ntran) ;
   exit(0) ;
}
