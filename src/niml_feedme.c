/***
  This sample program shows how to send volumes of data to AFNI
  through a TCP/IP socket, using the NIML interface.  RWCox - Jul 2009
***/

#include "mrilib.h"

#define AFNI_NIML_PORT 53212            /* TCP/IP port that AFNI uses */

NI_stream NF_stream = (NI_stream)NULL ;

/*=============================================================================*/

void NF_exit(void)                   /* Function to be called to make sure */
{                                    /* the AFNI data channel gets closed. */
   fprintf(stderr,"*** niml_feedme exits: closing socket to AFNI\n") ;
   NI_stream_close(NF_stream) ;
   return ;
}

/*-----------------------------------------------------------------------------*/

#include <signal.h>

void NF_sigfunc(int sig)   /** signal handler for fatal errors **/
{
   char *sname ;
   static volatile int fff=0 ;
   if( fff ) _exit(1) ; else fff = 1 ;
   switch(sig){
      default:      sname = "unknown" ; break ;
      case SIGINT:  sname = "SIGINT"  ; break ;
      case SIGPIPE: sname = "SIGPIPE" ; break ;
      case SIGSEGV: sname = "SIGSEGV" ; break ;
      case SIGBUS:  sname = "SIGBUS"  ; break ;
      case SIGTERM: sname = "SIGTERM" ; break ;
   }
   fprintf(stderr,"\n*** Fatal Signal %d (%s) received\n",sig,sname) ;
   exit(1) ;
}

/*-----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   char *drive_afni[128] ;
   int   ndrive=0 , iarg=1 ;

   char host[1024]="localhost", nsname[2048], *geomstr, *cpt, temp[32] ;
   int dt=1000 , ctold,ctnew , ctzero ;
   int verbose=0 , kk,nn , nvox,nval , do_accum=0 ;
   THD_3dim_dataset *dset ;
   NI_element *nel ;
   MRI_IMAGE *fim ; float *far ;
   char *targname="niml_feedme" ;

   /*-- help the ignorant user --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf(
        "Usage: niml_feedme [options] dataset\n"
        "\n"
        "* Sends volumes from the dataset to AFNI via the NIML socket interface.\n"
        "* You must run AFNI with the command 'afni -niml' so that the program\n"
        "  will be listening for the socket connection.\n"
        "* Inside AFNI, the transmitted dataset will be named 'niml_feedme'.\n"
        "* For another way to send image data to AFNI, see progam rtfeedme.\n"
        "* At present, there is no way to attach statistical parameters to\n"
        "  a transmitted volume.\n"
        "* This program sends all volumes in float format, simply because\n"
        "  that's easy for me.  But you can also send byte, short, and\n"
        "  complex valued volumes.\n"
        "* This program is really just a demo; it has little practical use.\n"
        "\n"
        "OPTIONS:\n"
        "  -host sname =  Send data, via TCP/IP, to AFNI running on the\n"
        "                 computer system 'sname'.  By default, uses the\n"
        "                 current system (localhost), if you don't use this\n"
        "                 option.\n"
        "\n"
        "  -dt ms      =  Tries to maintain an inter-transmit interval of 'ms'\n"
        "                 milliseconds.  The default is 1000 msec per volume.\n"
        "\n"
        "  -verb       =  Be (very) talkative about actions.\n"
        "\n"
        "  -accum      =  Send sub-bricks so that they accumulate in AFNI.\n"
        "                 The default is to create only a 1 volume dataset\n"
        "                 inside AFNI, and each sub-brick just replaces\n"
        "                 that one volume when it is received.\n"
        "\n"
        "  -target nam =  Change the dataset name transmitted to AFNI from\n"
        "                 'niml_feedme' to 'nam'.\n"
        "\n"
        "  -drive cmd  =  Send 'cmd' as a DRIVE_AFNI command.\n"
        "                * If cmd contains blanks, it must be in 'quotes'.\n"
        "                * Multiple -drive options may be used.\n"
        "                * These commands will be sent to AFNI just after\n"
        "                  the first volume is transmitted.\n"
        "                * See file README.driver for a list of commands.\n"
        "\n"
        "EXAMPLE: Send volumes from a 3D+time dataset to AFNI:\n"
        "\n"
        "  niml_feedme -dt 1000 -verb -accum -target Elvis \\\n"
        "              -drive 'OPEN_WINDOW axialimage'     \\\n"
        "              -drive 'OPEN_WINDOW axialgraph'     \\\n"
        "              -drive 'SWITCH_UNDERLAY Elvis'      \\\n"
        "              timeseries+orig\n"
        "\n"
        "Author: RW Cox -- July 2009\n"
      ) ;
      PRINT_COMPILE_DATE ;
      exit(0) ;
   }

   mainENTRY("niml_feedme") ;

   /*-- scan arguments --*/

   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strncmp(argv[iarg],"-target",5) == 0 ){
        targname = strdup(argv[++iarg]) ; iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-drive") == 0 ){
        drive_afni[ndrive++] = argv[++iarg] ; iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-host") == 0 ){
        strcpy( host , argv[++iarg] ) ; iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-dt") == 0 ){
        dt = (int)strtod(argv[++iarg],NULL) ; if( dt < 9 ) dt = 9 ;
        iarg++ ; continue ;
      }

      if( strncmp(argv[iarg],"-verbose",4) == 0 ){
        verbose = 1 ; iarg++ ; continue ;
      }

      if( strncmp(argv[iarg],"-accum",4) == 0 ){
        do_accum = 1 ; iarg++ ; continue ;
      }

      ERROR_exit("Unrecognized option: %s",argv[iarg]) ;
   }

   if( iarg >= argc ) ERROR_exit("No dataset on command line?!") ;

   /*-- read in the dataset --*/

   dset = THD_open_dataset( argv[iarg] ) ;
   if( dset == NULL ) ERROR_exit("Can't open dataset '%s'",argv[iarg]) ;
   DSET_load(dset) ;
   if( !DSET_LOADED(dset) ) ERROR_exit("Can't load dataset '%s'",argv[iarg]) ;

   cpt     = EDIT_get_geometry_string(dset) ;
   geomstr = strdup(cpt) ;  /* describes geometry of dataset grid */

   if( verbose ) INFO_message("geometry string = '%s'",geomstr) ;

   nvox = DSET_NVOX(dset);  /* number of voxels in dataset */
   nval = DSET_NVALS(dset); /* number of sub-bricks in dataset */

   /*-- this stuff is one-time-only setup of the I/O to AFNI --*/

   atexit(NF_exit) ;             /* call this when program ends */

   signal(SIGINT ,NF_sigfunc) ;  /* setup signal handler */
   signal(SIGBUS ,NF_sigfunc) ;  /* for fatal errors */
   signal(SIGSEGV,NF_sigfunc) ;
   signal(SIGTERM,NF_sigfunc) ;

   /* name of NIML stream (socket) to open */

   sprintf( nsname , "tcp:%s:%d" , host , AFNI_NIML_PORT ) ;

   /* open the socket (i.e., dial the telephone call) */

   fprintf(stderr,"opening NIML stream '%s' ",nsname) ;
   NF_stream = NI_stream_open( nsname , "w" ) ;

   /* loop until AFNI connects (answers the call),
      printing a '.' every 1/2 second to keep the user happy */

   while(1){
     kk = NI_stream_writecheck( NF_stream , 500 ) ;
     if( kk == 1 ){ fprintf(stderr," connected!\n") ; break ; }
     if( kk <  0 ){ fprintf(stderr," ** connection fails **\n") ; exit(1) ; }
     fprintf(stderr,".") ;
   }

   /*-- Create VOLUME_DATA NIML element to hold the brick data --*/

   nel = NI_new_data_element( "VOLUME_DATA" , nvox ) ;

   /* add attributes to the element to help AFNI construct the dataset */

     /* define the grid of the dataset */
   NI_set_attribute( nel , "geometry_string" , geomstr ) ;

     /* define the name of the dataset */
   NI_set_attribute( nel , "target_name"     , targname ) ;

     /* all sub-bricks in the input dataset will be sent to be
        sub-brick #0 in the dataset inside AFNI
        -- if you don't want this behavior, and want the dataset
           inside AFNI to keep growing, then don't set this attribute! */

   if( !do_accum ) NI_set_attribute( nel , "index" , "0" ) ;

     /* +tlrc view?  [default in AFNI is +orig view] */
   if( dset->view_type == VIEW_TALAIRACH_TYPE )
     NI_set_attribute( nel , "view" , "tlrc" ) ;

   /**-- loop over sub-bricks and send them to AFNI --*/

   ctzero = NI_clock_time() ;  /* for later reference */

   if( verbose ) INFO_message("Starting sub-brick loop") ;

   for( kk=0 ; kk < nval ; kk++ ){

     ctold = NI_clock_time() ;   /* clock time at start of work (ms) */

     /* get a float copy of the kk-th sub-brick */

     fim = THD_extract_float_brick( kk , dset ) ;

     DSET_unload_one(dset,kk) ;  /* unload this sub-brick now */

     if( fim == NULL ){  /* should never happen */
       ERROR_message("Can't get sub-brick #%d?? -- skipping",kk) ;
       NI_sleep(dt) ; continue ;
     }

     /* copy the float data into the NIML element for transmission */

     far = MRI_FLOAT_PTR(fim) ;
     if( kk == 0 )               /* first time: create data column in element */
       NI_add_column( nel , NI_FLOAT , far ) ;
     else                        /* later times: overwrite nel data column */
       memcpy( nel->vec[0] , far , sizeof(float)*nvox ) ;

     mri_free(fim) ;  /* done with this now [data all copied to nel] */

     /* set sub-brick index in AFNI if doing accumulation */

     if( do_accum ){
       sprintf(temp,"%d",kk) ; NI_set_attribute( nel , "index" , temp ) ;
     }

     /*** send the data element to AFNI ***/

     nn = NI_write_element( NF_stream , nel , NI_BINARY_MODE ) ;

     /* if something bad happened in the transmission, report it */

     if( nn <= 0 ){
       ERROR_message("Can't write sub-brick #%d to AFNI!",kk) ; break ;
     }

     /*** first time through ==>
          do the '-drive' commands now by sending processing instructions ***/

     if( kk == 0 && ndrive > 0 ){
       int ii ; NI_procins *npi ;
       if( verbose )
         ININFO_message("Sending %d 'drive_afni' elements now",ndrive) ;
       npi = NI_new_processing_instruction( "DRIVE_AFNI" ) ;
       NI_sleep(1) ;    /* give AFNI a msec to digest the data */
       for( ii=0 ; ii < ndrive ; ii++ ){
         NI_set_attribute( npi , "cmd" , drive_afni[ii] ) ;
         (void)NI_write_element( NF_stream , npi , NI_TEXT_MODE ) ;
       }
       NI_free_element(npi) ; /* delete this struct from the world! */
     }

     ctnew = NI_clock_time() ;  /* clock time now */

     if( verbose ) ININFO_message("Sent %d bytes for sub-brick #%d in %d ms",
                                  nn , kk , ctnew-ctold ) ;

     NI_sleep( dt - (ctnew-ctold) ) ;  /* sleep so that time delay is right */

   } /* end of loop over sub-bricks */

   /** summarize, do some cleanup, and exit stage left **/

   if( verbose && kk > 0 ){
     float dtav = (NI_clock_time()-ctzero) / (float)kk ;
     INFO_message("Transmission finished: %.1f ms = average time per volume",dtav) ;
   }

   NI_free_element(nel) ;  /* destroy the data element */
   DSET_delete(dset) ;     /* destroy the dataset */

   exit(0) ;
}
