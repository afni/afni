#include "afni.h"

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

#ifdef AFNI_DEBUG
#  define USE_TRACING
#  define PRINT_TRACING
#endif
#include "dbtrace.h"

#if defined(USE_TRACING) && defined(MALLOC_TRACE)
# define VMCHECK                                                \
  do{ if(verbose){                                              \
         fprintf(stderr,"RT check_malloc: ") ; fflush(stderr) ; \
         fprintf(stderr,"%s\n",check_malloc()) ; } } while(0)
#else
# define VMCHECK /* nada */
#endif

#define TCP_CONTROL "tcp:*:7954"      /* control channel specification */
#define INFO_SIZE  (16*1024)          /* change this ==> change SHM_CHILD below */
#define SHM_CHILD  "shm:afnibahn:16K" /* for data from the child */

#define SHORT_DELAY      1            /* msec */
#define LONG_DELAY      10

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

#define ALLOW_REGISTRATION  /* not fully implemented yet */

/***********************************************************************
  Plugin to accept data from an external process and assemble
  it into a dataset.  Initial implementation for the Bruker 3T/60
  system at the Medical College of Wisconsin.
************************************************************************/

/**************************************************************************/
/*********************** struct for reading data **************************/

#include <sys/types.h>
#include <unistd.h>
#include <sys/wait.h>
#include <signal.h>
#include <ctype.h>

#define DTYPE_2DZ   77
#define DTYPE_2DZT  78
#define DTYPE_3D    79
#define DTYPE_3DT   80

#define ZORDER_ALT  33
#define ZORDER_SEQ  34
#define ZORDER_EXP  39
#define NZMAX       1024

#define DEFAULT_XYFOV    240.0
#define DEFAULT_XYMATRIX  64
#define DEFAULT_ZDELTA     5.0
#define DEFAULT_ZNUM       2
#define DEFAULT_TR         1.0

#define RT_NBUF          INFO_SIZE
#define NNAME            128

#define ORCODE(aa) \
  ( (aa)=='R' ? ORI_R2L_TYPE : (aa)=='L' ? ORI_L2R_TYPE : \
    (aa)=='P' ? ORI_P2A_TYPE : (aa)=='A' ? ORI_A2P_TYPE : \
    (aa)=='I' ? ORI_I2S_TYPE : (aa)=='S' ? ORI_S2I_TYPE : ILLEGAL_TYPE )

typedef struct {

   int info_ok , no_data ;        /* status flags */

   char     name_data[NNAME] ;    /* where to get image data */
   IOCHAN * ioc_data ;            /* IO channel for image data */

   char     name_info[NNAME] ;    /* where to get control data */
   IOCHAN * ioc_info ;            /* IO channel for control data */
   pid_t    child_info ;          /* pid of child that will give me control information */

   char prefix[THD_MAX_PREFIX] ;  /* name of dataset */

   int   nxx  ,nyy  ,nzz  ,       /* matrix */
         orcxx,orcyy,orczz ;      /* orientations */

   float xxfov , yyfov , zzfov ;  /* field of view */
   float dxx  ,dyy  ,dzz ,        /* voxel size */
         xxorg,yyorg,zzorg ;      /* offsets */
   int   zzdcode ;                /* direction code for z offset */

   int   xcen ,ycen ,zcen  ;      /* centering of axes? */

   float tr ;                     /* TR */
   int   nr ;                     /* expected # volumes */

   int   dtype ;                  /* dataset type: a DTYPE code */
   int   zorder ;                 /* 2D slice ordering: a ZORDER code */
   int   nzseq , zseq[NZMAX] ;    /* slice input order */
   int   datum ;                  /* a MRI_type code from mrilib.h */

   int   nbuf ;                   /* current buffer size */
   char  buf[RT_NBUF] ;           /* buffer for reading command strings */

   int afni_status ;              /* does AFNI know about this dataset yet? */
   THD_3dim_dataset * dset ;      /* AFNI dataset under construction */
   Three_D_View * im3d ;          /* AFNI controller which gets it */
   THD_session * sess ;           /* AFNI session which gets it */
   char * sbr ;                   /* sub-brick under construction */
   char * im ;                    /* place for next slice to be read */
   int sbr_size ;                 /* # bytes per sub-brick */
   int nvol ;                     /* # volumes read so far */
   int nsl ;                      /* # slices read so far */
   int imsize ;                   /* # bytes per image */
   MRI_IMARR * bufar ;            /* buffer for input images */

   THD_3dim_dataset * func_dset ; /* functional dataset, if any */
   int func_status ;              /* does AFNI know about this dataset yet? */
   int func_code ;                /* how to compute function */
   int func_condit ;              /* condition that function computation is in now */
   int_func * func_func ;         /* function to compute the function */

#ifdef ALLOW_REGISTRATION
   /*-- 07 Apr and 01 Aug 1998: real-time image registration stuff --*/

   THD_3dim_dataset * reg_dset ;      /* registered dataset, if any */
   MRI_2dalign_basis ** reg_2dbasis ; /* stuff for each slice */
   int reg_base_index ;               /* where to start? */
   int reg_mode ;                     /* how to register? */
   int reg_status ;                   /* does AFNI know about this dataset yet? */
   int reg_nvol ;                     /* number of volumes registered so far */
#endif

   double elapsed , cpu ;         /* times */
   double last_elapsed ;
   int    last_nvol ;

} RT_input ;

#define SHOW_TIMES                                              \
  fprintf(stderr,"RT: cpu time = %.2f  elapsed time = %.2f\n" , \
          PLUTO_cpu_time()-rtin->cpu , PLUTO_elapsed_time()-rtin->elapsed )

/**************************************************************************/

static char helpstring[] =
   " Purpose: Assigning a root prefix to real-time datasets.\n"
   " Input:\n"
   " Root     = Root prefix to be used for new datasets.\n"
   "              The actual prefix will be of the form Root#001\n\n"
   " Update   = How often AFNI's display will be updated\n"
   "             = How many 3D bricks are gathered before\n"
   "               the images and graphs are redrawn:\n"
   "                0 --> don't update until all data is acquired\n"
   "                1 --> redraw images and graphs every time\n"
   "                2 --> redraw every other brick, etc.\n\n"
   " Function = Controls what kind of function analysis is done\n"
   "              during realtime input of time dependent datasets.\n\n"
   " Verbose  = If set to 'Yes', the plugin will print out progress\n"
   "              reports as information flows into it.\n"
#ifdef ALLOW_REGISTRATION
   "\n"
   " Registration = If activated, image registration will take place\n"
   "                  either in realtime or at the end of image acquisition.\n"
   "                  The un-registered dataset will be named something like\n"
   "                  Root#001 and the aligned dataset Root#001%reg.\n"
   " Base Image   = The value sets the time index to which the alignment\n"
   "                  will take place.\n"
#endif
;

/** variables encoding the state of options from the plugin interface **/

static char root[THD_MAX_PREFIX] = "rt." ;  /* default prefix */
static int  update               = 10 ;     /* default update frequency */

#define NFUNC  2
static char * FUNC_strings[NFUNC] = { "None" , "FIM" } ;
static int func_code = 0 ;

#define FUNC_NONE  0  /* symbolic values for the func_code */
#define FUNC_RTFIM 1

#define INIT_MODE     -1  /* modes for RT_fim_recurse (AKA func_func) */
#define UPDATE_MODE   -2
#define FINAL_MODE    -3

int RT_fim_recurse( RT_input * , int ) ;

int_func * FUNC_funcs[NFUNC] = { NULL , RT_fim_recurse } ;

#define NVERB 3
static char * VERB_strings[NVERB] = { "No" , "Yes" , "Very" } ;
static int verbose = 1 ;

#ifdef ALLOW_REGISTRATION
  static int regtime = 3 ;  /* index of base time for registration */
  static int regmode = 0 ;  /* index into REG_strings */

# define NREG 3
  static char * REG_strings[NREG] = { "None" , "2D: realtime" , "2D: at end" } ;

# define REGMODE_NONE      0
# define REGMODE_2D_RTIME  1
# define REGMODE_2D_ATEND  2
#endif

/************ global data for reading data *****************/

static IOCHAN * ioc_control = NULL ;     /* control channel */
static RT_input * rtinp = NULL ;         /* only read 1 stream at a time */
static PLUGIN_interface * plint = NULL ; /* AFNI plugin structure */

/************ prototypes ***********/

PLUGIN_interface * PLUGIN_init( int ) ;
char * RT_main( PLUGIN_interface * ) ;
Boolean RT_worker( XtPointer ) ;
RT_input * new_RT_input(void) ;
int RT_check_listen(void) ;
int RT_acquire_info( char * ) ;
int RT_process_info( int , char * , RT_input * ) ;
void RT_start_dataset( RT_input * ) ;
void RT_read_image( RT_input * , char * ) ;
int RT_process_data( RT_input * ) ;
void RT_process_image( RT_input * ) ;
void RT_finish_dataset( RT_input * ) ;
void RT_tell_afni( RT_input * , int ) ;
void RT_start_child( RT_input * ) ;

#ifdef ALLOW_REGISTRATION
  void RT_registration_2D_atend( RT_input * rtin ) ;
  void RT_registration_2D_setup( RT_input * rtin ) ;
  void RT_registration_2D_close( RT_input * rtin ) ;
  void RT_registration_2D_onevol( RT_input * rtin , int tt ) ;
  void RT_registration_2D_realtime( RT_input * rtin ) ;
#endif

#define TELL_NORMAL  0
#define TELL_WRITE   1
#define TELL_FINAL   2

#define WRITE_INTERVAL 7.954  /* seconds */

#define REAP_CHILDREN
#ifdef REAP_CHILDREN
#  define GRIM_REAPER waitpid(-1,NULL,WNOHANG)
#else
#  define GRIM_REAPER /* nada */
#endif

/***********************************************************************
   Set up the interface to the user
************************************************************************/

PLUGIN_interface * PLUGIN_init( int ncall )
{
   if( ncall > 0 )         return NULL ;  /* only one interface */
   if( ! ALLOW_real_time ) return NULL ;  /* do nothing if not allowed */

   /*-- set titles and call point --*/

   plint = PLUTO_new_interface( "RT Options" , "Set Real-Time Acquisition Options" ,
                                helpstring , PLUGIN_CALL_VIA_MENU , RT_main  ) ;

   PLUTO_add_hint( plint , "Set Real-Time Acquisition Options" ) ;

   /*-- first line of input: Prefix for output dataset --*/

   PLUTO_add_option( plint , "" , "Root" , FALSE ) ;
   PLUTO_add_string( plint , "Root" , 0,NULL , 19 ) ;

   /*-- second line of input: Update frequency --*/

   PLUTO_add_option( plint , "" , "Update" , FALSE ) ;
   PLUTO_add_number( plint , "Update" , 0,59,0 , update , FALSE ) ;

   /*-- third line of input: Function computation --*/

   PLUTO_add_option( plint , "" , "Function" , FALSE ) ;
   PLUTO_add_string( plint , "Function" , NFUNC , FUNC_strings , func_code ) ;

   /*-- fourth line of input: Verbosity flag --*/

   PLUTO_add_option( plint , "" , "Verbose" , FALSE ) ;
   PLUTO_add_string( plint , "Verbose" , NVERB , VERB_strings , verbose ) ;

#ifdef ALLOW_REGISTRATION
   /*-- fifth line of input: registration mode --*/

   PLUTO_add_option( plint , "" , "Registration" , FALSE ) ;
   PLUTO_add_string( plint , "Registration" , NREG , REG_strings , regmode ) ;
   PLUTO_add_number( plint , "Base Image" , 0,59,0 , regtime , FALSE ) ;
#endif

   /***** Register a work process *****/

   PLUTO_register_workproc( RT_worker , NULL ) ;

   /***** go home *****/

   ALLOW_real_time = 1 ;  /* flag to AFNI that realtime work is started */
   return plint ;
}

/***************************************************************************
  Main routine for this plugin (will be called from AFNI).
****************************************************************************/

char * RT_main( PLUGIN_interface * plint )
{
   char * tag , * str , * new_prefix ;
   static char buf[256] ;

   if( plint == NULL )
      return "*********************\n"
             "RT_main:  NULL input\n"
             "*********************"  ;

   /** loop over input from AFNI **/

#ifdef ALLOW_REGISTRATION
   regmode = 0 ;
#endif

   while( (tag=PLUTO_get_optiontag(plint)) != NULL ){

      if( strcmp(tag,"Root") == 0 ){
         new_prefix = PLUTO_get_string(plint) ;
         if( ! THD_filename_ok(new_prefix) )
            return "**************************\n"
                   "RT_main:  bad root prefix\n"
                   "**************************"  ;
         strcpy(root,new_prefix) ;
         continue ;
      }

      if( strcmp(tag,"Update") == 0 ){
         update = PLUTO_get_number(plint) ;
         continue ;
      }

      if( strcmp(tag,"Function") == 0 ){
         str       = PLUTO_get_string(plint) ;
         func_code = PLUTO_string_index( str , NFUNC , FUNC_strings ) ;
         continue ;
      }

      if( strcmp(tag,"Verbose") == 0 ){
         str     = PLUTO_get_string(plint) ;
         verbose = PLUTO_string_index( str , NVERB , VERB_strings ) ;
         continue ;
      }

#ifdef ALLOW_REGISTRATION
      if( strcmp(tag,"Registration") == 0 ){
         str     = PLUTO_get_string(plint) ;
         regmode = PLUTO_string_index( str , NREG , REG_strings ) ;
         regtime = PLUTO_get_number(plint) ;
         continue ;
      }
#endif

      /** How the hell did this happen? **/

      sprintf(buf,"*****************\n"
                  "Illegal optiontag: %s\n"
                  "*****************"      , tag ) ;
      return buf ;

   }  /* end of loop over active input options */

   return NULL ;  /* nothing bad happened */
}

/***************************************************************************
   Listen for an incoming real-time data control connection.
   Return 1 if the connection is established,
   return 0 if no connection is ready, return -1 if an error occurs.
****************************************************************************/

int RT_check_listen(void)
{
   int jj ;
   static int newcon = 1 ;

   /** see if we need to start listening **/

   if( ioc_control == NULL ){
      if( verbose )
         fprintf(stderr,"RT: starting to listen for control channel.\n") ;
      ioc_control = iochan_init( TCP_CONTROL , "accept" ) ;
      newcon      = 1 ;
      if( ioc_control == NULL ){
         fprintf(stderr,"RT: can't listen for control channel\a\n") ;
         return -1 ;
      }
   }

   /** Check if the network channel is active **/

   jj = iochan_goodcheck(ioc_control,SHORT_DELAY) ;

   if( jj == 1 ){  /* external connection! */

      if( newcon ){
         fprintf(stderr,"RT:---------------------------------------\n") ;
         fprintf(stderr,"RT: connected to control channel %s\n",ioc_control->name) ;
         newcon = 0 ;
      } else {
/**
         if( verbose == 2 )
            fprintf(stderr,"RT: still waiting for control data.\n") ;
**/
      }

      if( ! TRUST_host(ioc_control->name) ){
         fprintf(stderr,"RT: illegal host connection!\a\n") ;
         IOCHAN_CLOSE(ioc_control) ;
         return 0 ;
      }

      jj = iochan_readcheck(ioc_control,0) ;  /* is something ready to read? */

      if( jj > 0 && verbose == 2 ) fprintf(stderr,"RT: control data is present!\n") ;

      return jj ;

   } else if( jj == -1 ){  /* something bad! */

      fprintf(stderr,"RT: failure while listening for control channel!\a\n") ;
      IOCHAN_CLOSE(ioc_control) ;
      return 0 ;
   }

   return 0 ;  /* no external connection yet */
}

/***************************************************************************/
/**             macro to close down the realtime input stream             **/

#if 0
# define CLEANUP                                                         \
  do { IOCHAN_CLOSE(rtinp->ioc_data) ;                                   \
       IOCHAN_CLOSE(rtinp->ioc_info) ;                                   \
       DESTROY_IMARR(rtinp->bufar) ;                                     \
       if( rtinp->sbr != NULL ) free( rtinp->sbr ) ;                     \
       if( rtinp->child_info > 0 ) kill( rtinp->child_info , SIGTERM ) ; \
       free(rtinp) ; rtinp = NULL ;  ioc_control = NULL ; GRIM_REAPER ;  \
  } while(0)
#else
# define CLEANUP cleanup_rtinp()
#endif

/**------------ 01 Aug 1998: replace the macro with a function -----------**/

void cleanup_rtinp(void)
{
   IOCHAN_CLOSE(rtinp->ioc_data) ;         /* close any open I/O channels */
   IOCHAN_CLOSE(rtinp->ioc_info) ;

   if( rtinp->child_info > 0 )                   /* destroy child process */
      kill( rtinp->child_info , SIGTERM ) ;

   DESTROY_IMARR(rtinp->bufar) ;           /* destroy any buffered images */

   if( rtinp->sbr != NULL ) free( rtinp->sbr ) ; /* destroy buffered data */

#ifdef ALLOW_REGISTRATION
   if( rtinp->reg_2dbasis != NULL ){        /* destroy registration setup */
      int kk ;
      for( kk=0 ; kk < rtinp->nzz ; kk++ )
         mri_2dalign_cleanup( rtinp->reg_2dbasis[kk] ) ;
      free( rtinp->reg_2dbasis ) ;
   }
#endif

   free(rtinp) ; rtinp = NULL ;                 /* destroy data structure */
   ioc_control = NULL ;                          /* ready to listen again */
   GRIM_REAPER ;                               /* reap dead child, if any */
}

/*********************************************************************
  Real time routine called every so often by Xt.
  This handles connections from other processes that want to
  send AFNI data.

  This is a work process -- it returns False if it wants to be
  called again, and return True if it is done.  For real-time
  AFNIfying, this should always return False.
**********************************************************************/

Boolean RT_worker( XtPointer elvis )
{
   int jj ;

   /**--------------------------------------------------------------------**/
   /** if we are waiting for a connection, check to see if it is good yet **/

   if( rtinp == NULL ){         /* not doing real-time input at this time */
      jj = RT_check_listen() ;             /* see if someone wants to talk */
      if( jj < 0 ) return True ;           /* quit if there's an error */
      if( jj == 0 ) return False ;         /* try later if no connection now */
      rtinp = new_RT_input() ;             /* try to make a new input struct */
      IOCHAN_CLOSE( ioc_control ) ;        /* not needed any more */
      if( rtinp == NULL ) return False ;   /* try later (?) */
   }

   /**---------------------------------------------------------------**/
   /**---- at this point, the rtinp struct is good,              ----**/
   /**---- meaning that we are in the business of acquiring data ----**/
   /**---------------------------------------------------------------**/

   /**-----------------------------------------------------------------------**/
   /** if input data stream has gone bad, close down the real-time operation **/

   if( iochan_goodcheck(rtinp->ioc_data,0) != 1 ){

      if( rtinp->dset != NULL ){       /* if we started a dataset */
         if( verbose == 2 )
            fprintf(stderr,"RT: data channel closed down.\n") ;
         VMCHECK ;
         RT_finish_dataset( rtinp ) ;  /* then we can finish it */
      } else {
         fprintf(stderr,"RT: data channel closed dataset was fully defined!\a\n") ;
      }

      CLEANUP ; return False ;
   }

   /**-----------------------------------------------------------------**/
   /**---- See if any data is waiting to be read from the child   ----**/
   /**---- process that is supposed to supply control information ----**/

   if( rtinp->child_info > 0 ){

      jj = iochan_readcheck( rtinp->ioc_info , SHORT_DELAY ) ;  /** data ready? **/

      if( jj < 0 ){   /** something bad happened? **/

         fprintf(stderr,"RT: child info channel closed prematurely!\a\n") ;
         CLEANUP ; return False ;

      } else if( jj > 0 ){   /** something good happened? **/

         char * info = (char *) malloc( sizeof(char) * INFO_SIZE ) ;
         int   ninfo , ii ;

         /* read all data possible from the info channel */

         if( verbose )
            fprintf(stderr,"RT: receiving data from child process\n") ;
         VMCHECK ;

         ninfo = 0 ;
         while(1){
            ii = iochan_recv( rtinp->ioc_info , info+ninfo , INFO_SIZE-ninfo ) ;
            if( ii < 1 ) break ;
            ninfo += ii ;
            if( ninfo >= INFO_SIZE ){
               fprintf(stderr,"RT: child process sent too much info!\a\n") ;
               break ;
            }
            ii = iochan_readcheck( rtinp->ioc_info , SHORT_DELAY ) ;
            if( ii < 1 ) break ;
         }
         IOCHAN_CLOSE( rtinp->ioc_info ) ; rtinp->child_info = 0 ;

         if( ninfo <= 0 ){
            fprintf(stderr,"RT: child info channel returned no data!\a\n") ;
            CLEANUP ; return False ;
         }

         if( verbose == 2 )
            fprintf(stderr,"RT: child info channel returned %d bytes.\n",ninfo) ;
         VMCHECK ;

         /* process the info and store it in the real-time struct */

         jj = RT_process_info( ninfo , info , rtinp ) ; free(info) ;

         /* check if data was processed OK */

         if( jj <= 0 ){
            fprintf(stderr,"RT: child info was badly formatted!\a\n") ;
            CLEANUP ; return False ;
         }

         /* if we already received the first image data,
            then all the setup info should be OK.  if not, quit */

         if( ! rtinp->no_data && ! rtinp->info_ok ){
            fprintf(stderr,"RT: child info was incomplete!\a\n") ;
            CLEANUP ; return FALSE ;
         }

         /* if all the setup info is OK, we can create the dataset now */

         if( rtinp->dset == NULL && rtinp->info_ok ){
            if( verbose == 2 )
               fprintf(stderr,"RT: info complete --> creating dataset.\n") ;
            VMCHECK ;
            RT_start_dataset( rtinp ) ;
         }
      }

      /** if nothing happened (good or bad),
          will try to read it again on the next time thru **/
   }

   /**---------------------------------------------**/
   /** See if any image data is waiting to be read **/

   jj = iochan_readcheck( rtinp->ioc_data , SHORT_DELAY ) ;

   if( jj == 0 ){     /** no data **/

#ifdef WRITE_INTERVAL
      double delt ;

      /** if there is no image data for a long time,
          and there is something new to write out,
          then write the datasets out again.         **/

      if( !rtinp->no_data                                                  &&
          rtinp->nvol > rtinp->last_nvol                                   &&
          (delt=PLUTO_elapsed_time()-rtinp->last_elapsed)  > WRITE_INTERVAL   ){

         int cmode ;

         fprintf(stderr,"RT: no image data for %g seconds --> saving to disk.\n",delt) ;

         PLUTO_popup_transient( plint , " \n"
                                        " Pause in input data stream:\n"
                                        " Saving current dataset to disk.\n" ) ;

         RT_tell_afni(rtinp,TELL_NORMAL) ;

         cmode = THD_get_write_compression() ;      /* 20 Mar 1998 */
         THD_set_write_compression(COMPRESS_NONE) ;
         SHOW_AFNI_PAUSE ;

         THD_write_3dim_dataset( NULL,NULL , rtinp->dset , True ) ;
         if( rtinp->func_dset != NULL )
            THD_write_3dim_dataset( NULL,NULL , rtinp->func_dset , True ) ;

         THD_set_write_compression(cmode) ;
         SHOW_AFNI_READY ;

         rtinp->last_nvol    = rtinp->nvol ;
         rtinp->last_elapsed = PLUTO_elapsed_time() ;
      }
#endif

      return False ;
   }

   if( jj < 0 ){                 /* something bad happened to data channel */
      if( verbose == 2 )
         fprintf(stderr,"RT: data channel closed down.\n") ;
      VMCHECK ;
      if( rtinp->dset != NULL ) RT_finish_dataset( rtinp ) ;
      CLEANUP ; return False ;
   }

   /**-----------------------------------------------------**/
   /** If this is the first time reading the data channel, **/
   /** read and process any header info present there.     **/
   /** (Will read the image data a little farther below.)  **/

   if( rtinp->no_data ){

      /** if so ordered, start a child process to get header info **/

      if( strlen(rtinp->name_info) > 0 ) RT_start_child( rtinp ) ;

      /** read some stuff from the data channel **/

      fprintf(stderr,"RT: receiving first data=") ; fflush(stderr) ;

      rtinp->nbuf = iochan_recv( rtinp->ioc_data , rtinp->buf , RT_NBUF ) ;

      fprintf(stderr,"%d bytes\n",rtinp->nbuf) ;

      if( rtinp->nbuf <= 0 ){
         fprintf(stderr,"RT: recv on data channel fails on first try!\a\n") ;
         CLEANUP ; return False ;
      }

      PLUTO_beep() ;
      PLUTO_popup_transient( plint , " \n"
                                     "       Heads Up!\n"
                                     "Incoming realtime data!\n" ) ;

      /** process header info in the data channel;
          note that there must be SOME header info this first time **/

      jj = RT_process_info( rtinp->nbuf , rtinp->buf , rtinp ) ;

      if( jj <= 0 ){                  /* header info not OK */

         fprintf(stderr,"RT: initial image data is badly formatted!\a\n") ;
         CLEANUP ; return False ;

      } else if( jj < rtinp->nbuf ){  /* data bytes left after header info */

         memmove( rtinp->buf , rtinp->buf + jj , rtinp->nbuf - jj ) ;
         rtinp->nbuf = rtinp->nbuf - jj ;

      } else {                        /* no data left after header info */
         rtinp->nbuf = 0 ;
      }

      if( verbose == 2 )
         fprintf(stderr,"RT: processed %d bytes of header info from data channel\n",jj) ;
      VMCHECK ;

      rtinp->no_data = 0 ;  /* can't say we never received data */

      /* if we have already received the child process info,
         or if there is no child process info, then all should be OK now */

      if( rtinp->child_info == 0 && ! rtinp->info_ok ){
         fprintf(stderr,"RT: image header info was incomplete!\a\n") ;
         CLEANUP ; return FALSE ;
      }
   }

   /**-------------------------------**/
   /** At last -- process image data **/

   jj = RT_process_data( rtinp ) ;

   if( jj < 0 ){
      fprintf(stderr,"RT: data channel aborted during image read!\n") ;
      if( rtinp->dset != NULL ) RT_finish_dataset( rtinp ) ;
      CLEANUP ; return False ;
   }

   rtinp->last_elapsed = PLUTO_elapsed_time() ;  /* record this time */
   return False ;
}

/*--------------------------------------------------------------------------
   Initialize a new RT_input structure; returns pointer to new structure.
   Will read from the control channel, which will tell it what data
   channel to open, and if it needs to fork a child process to gather
   header info.
----------------------------------------------------------------------------*/

RT_input * new_RT_input(void)
{
   RT_input * rtin ;
   int ncon , ii ;
   char * con , * ptr ;
   Three_D_View * im3d ;

   /** wait until data can be read, or something terrible happens **/

   if( iochan_readcheck(ioc_control,-1) <= 0 ){
      fprintf(stderr,"RT: control channel fails readcheck!\a\n") ;
      return NULL ;
   }

   /** make new structure **/

   rtin = (RT_input *) malloc( sizeof(RT_input) ) ;
   con  = (char *)     malloc( INFO_SIZE ) ;

   if( rtin == NULL || con == NULL ){
      fprintf(stderr,"RT: malloc fails in new_RT_input!\a\n") ; exit(1) ;
   }

   /** read all data possible from control channel **/

   ncon = 0 ;  /* read all data possible from the control channel */
   while(1){
      ii = iochan_recv( ioc_control , con+ncon , INFO_SIZE-ncon ) ;
      if( ii < 1 ) break ;
      ncon += ii ;
      if( ncon >= INFO_SIZE ){
         fprintf(stderr,"RT: control channel buffer overflow!\a\n") ;
         break ;
      }
      iochan_sleep( SHORT_DELAY ) ;
   }

   if( ncon < 1 ){
      fprintf(stderr,"RT: control channel sends no data!\a\n") ;
      free(rtin) ; free(con) ; return NULL ;
   }

   /** The data we now have in 'con' should be of the form
          image_channel_spec\n
          info_command\n\0
       where 'image_channel_spec' is an IOCHAN specifier saying
         how AFNI should read the image data
       and 'info_command' is the name of a command to run that
         will write the dataset control info to stdout (where
         a child of AFNI will get it using the 'popen' routine).
         If no 'info_command' is needed (all the dataset info will
         come thru on the image channel), then the info_command
         should just be skipped.
       Each string should be less than NNAME characters long!
   **/

   ncon = MIN( ncon , INFO_SIZE-2 ) ;
   con[ncon+1] = '\0' ;               /* make sure it is NUL terminated */

   /** copy first string -- this is image_channel_spec **/

   ii = 0 ;
   for( ptr=con ; ii < NNAME && *ptr != '\0' && *ptr != '\n' ; ptr++ ){
      rtin->name_data[ii++] = *ptr ;
   }
   if( ii >= NNAME ){
      fprintf(stderr,"RT: control image_channel_spec buffer overflow!\a\n") ;
      ii = NNAME - 1 ;
   }
   rtin->name_data[ii] = '\0' ;

   /** open data channel **/

   rtin->ioc_data = iochan_init( rtin->name_data , "accept" ) ;
   if( rtin->ioc_data == NULL ){
      fprintf(stderr,"RT: failure to open data IOCHAN %s\a\n",rtin->name_data) ;
      free(rtin) ; free(con) ; return NULL ;
   }

   if( verbose == 2 )
      fprintf(stderr,"RT: opened data channel %s\n",rtin->name_data) ;
   VMCHECK ;

   /** if more follows, that is a command for a child process
       (which will not be started until the first image data arrives) **/

   ii = 0 ;
   if( *ptr != '\0' ){
      for( ptr++ ; ii < NNAME && *ptr != '\0' && *ptr != '\n' ; ptr++ ){
         rtin->name_info[ii++] = *ptr ;
      }
      if( ii >= NNAME ){
         fprintf(stderr,"RT: control info_command buffer overflow!\a\n") ;
         ii = NNAME - 1 ;
      }
   }
   rtin->name_info[ii] = '\0' ;

   if( verbose == 2 ){
      if( strlen(rtin->name_info) > 0 )
         fprintf(stderr,"RT: info command for child will be '%s'\n",rtin->name_info) ;
      else
         fprintf(stderr,"RT: no info command given.\n") ;
   }
   VMCHECK ;

   /** the command (if any) will be run later **/

   rtin->ioc_info   = NULL ;
   rtin->child_info = 0 ;

   /** wait until the data channel is good **/

   if( verbose )
      fprintf(stderr,"RT: waiting for data channel to open.\n") ;
   VMCHECK ;

   while(1){
      ii = iochan_goodcheck(rtin->ioc_data,1000) ;             /* wait up to 1000 msec */
           if( ii >  0 )                 break ;               /* is good! */
      else if( ii == 0 && verbose == 2 ) fprintf(stderr,".") ; /* not good yet */
      else {                                                   /* is bad! */
         fprintf(stderr,"RT: data channel fails to become good!\a\n") ;
         IOCHAN_CLOSE(rtin->ioc_data) ; free(rtin) ; free(con) ;
         return NULL ;
      }
   }

   if( verbose == 2 )
      fprintf(stderr,"RT: data channel is opened.\n") ;
   VMCHECK ;

   /** initialize internal constants in the struct **/

   rtin->info_ok = 0 ; rtin->no_data = 1 ;

   strcpy(rtin->prefix,root) ;

#if 0
   rtin->nxx = DEFAULT_XYMATRIX ;
   rtin->nyy = DEFAULT_XYMATRIX ;
   rtin->nzz = DEFAULT_ZNUM ;

   rtin->orcxx = ORI_S2I_TYPE ;
   rtin->orcyy = ORI_A2P_TYPE ;
   rtin->orczz = ORI_L2R_TYPE ;

   rtin->xxfov = DEFAULT_XYFOV ;
   rtin->yyfov = DEFAULT_XYFOV ;
   rtin->zzfov = -1.0 ;
   rtin->dxx   = DEFAULT_XYFOV / DEFAULT_XYMATRIX ;
   rtin->dyy   = DEFAULT_XYFOV / DEFAULT_XYMATRIX ;
   rtin->dzz   = DEFAULT_ZDELTA ;

   rtin->xxorg = 0.5 * (rtin->nxx - 1) * rtin->dxx ; rtin->xcen = 1 ;
   rtin->yyorg = 0.5 * (rtin->nyy - 1) * rtin->dyy ; rtin->ycen = 1 ;
   rtin->zzorg = 0.5 * (rtin->nzz - 1) * rtin->dzz ; rtin->zcen = 1 ;
   rtin->zzdcode = ILLEGAL_TYPE ;

   rtin->tr     = DEFAULT_TR ;
   rtin->nr     = 1 ;
   rtin->dtype  = DTYPE_2DZT ;
   rtin->zorder = ZORDER_ALT ;
   rtin->datum  = MRI_short  ;
#else
   rtin->nxx = -1 ;
   rtin->nyy = -1 ;
   rtin->nzz = -1 ;

   rtin->orcxx = -1 ;
   rtin->orcyy = -1 ;
   rtin->orczz = -1 ;

   rtin->xxfov = 0.0 ;
   rtin->yyfov = 0.0 ;
   rtin->zzfov = 0.0 ;
   rtin->dxx   = 0.0 ;
   rtin->dyy   = 0.0 ;
   rtin->dzz   = 0.0 ;

   rtin->xxorg = 0.0 ; rtin->xcen = 1 ;
   rtin->yyorg = 0.0 ; rtin->ycen = 1 ;
   rtin->zzorg = 0.0 ; rtin->zcen = 1 ; rtin->zzdcode = ILLEGAL_TYPE ;

   rtin->tr     = DEFAULT_TR ;
   rtin->nr     = 1 ;
   rtin->dtype  = DTYPE_2DZT ;
   rtin->zorder = ZORDER_ALT ;
   rtin->datum  = MRI_short  ;
   rtin->nzseq  = 0 ;
#endif

   rtin->nbuf   = 0    ;     /* no input buffer data yet */
   rtin->dset   = NULL ;     /* no dataset yet */
   rtin->sbr    = NULL ;     /* no brick space yet */
   rtin->im     = NULL ;
   rtin->imsize = 0 ;        /* don't know how many bytes/image yet */
   rtin->bufar  = NULL ;     /* no input image buffer yet */

   /** define connection to AFNI **/

   rtin->afni_status = 0 ;   /* AFNI is ignorant of the dataset */

   im3d = plint->im3d ;                           /* default controller */
   if( !IM3D_OPEN(im3d) ){                        /* may not be open */
      for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ ){  /* so find an open one */
         im3d = GLOBAL_library.controllers[ii] ;
         if( IM3D_OPEN(im3d) ) break ;
      }
      if( ii == MAX_CONTROLLERS ){                /* should not be possible */
         fprintf(stderr,"RT: no open controllers in AFNI!?\a\n") ;
         exit(1) ;
      }
   }
   rtin->im3d = im3d ;
   rtin->sess = GLOBAL_library.sslist->ssar[im3d->vinfo->sess_num] ;

   /** setup realtime function evaluation **/

   rtin->func_dset   = NULL ;  /* no function yet (if ever) */
   rtin->func_status = 0 ;     /* AFNI is ignorant */
   rtin->func_condit = 0 ;     /* no condition yet */

   /* function evaluation is not done on time-independent datasets */

   rtin->func_code   = (rtin->dtype == DTYPE_2DZ || rtin->dtype == DTYPE_3D)
                       ? FUNC_NONE : func_code ;

   rtin->func_func   = FUNC_funcs[rtin->func_code] ; /* where to evaluate */

#ifdef ALLOW_REGISTRATION
   rtin->reg_base_index = regtime ;  /* save these now, in case the evil */
   rtin->reg_mode       = regmode ;  /* user changes them on the fly!    */
   rtin->reg_dset       = NULL ;
   rtin->reg_2dbasis    = NULL ;
   rtin->reg_status     = 0 ;        /* AFNI knows nothing. NOTHING.     */
   rtin->reg_nvol       = 0 ;        /* number volumes registered so far */
#endif

   /** record the times for later reportage **/

   rtin->elapsed = PLUTO_elapsed_time() ;  rtin->last_elapsed = rtin->elapsed ;
   rtin->cpu     = PLUTO_cpu_time() ;      rtin->nvol = rtin->last_nvol = 0 ;

   free(con) ; return rtin ;
}

/*-------------------------------------------------------------------------
   Create a child process to run a command that will send header info
   back to the parent AFNI process.
---------------------------------------------------------------------------*/

void RT_start_child( RT_input * rtin )
{
   pid_t child_pid ;

   if( rtin == NULL || strlen(rtin->name_info) == 0 ) return ;  /* sanity */

   child_pid = fork() ;             /* AKA bifurcation */

   if( child_pid == (pid_t)(-1) ){  /* real bad news */
      fprintf(stderr,"RT: can't fork child process!\a\n") ; exit(1) ;
   }

   if( child_pid > 0 ){              /** I'm the parent **/

      if( verbose == 2 )
         fprintf(stderr,"RT: forked a child process to execute '%s'\n",rtin->name_info) ;
      VMCHECK ;

      /** open a channel to communicate with the child **/

      rtin->child_info = child_pid ;
      rtin->ioc_info   = iochan_init( SHM_CHILD , "accept" ) ;
      if( rtinp->ioc_info == NULL ){
         kill( child_pid , SIGTERM ) ;
         fprintf(stderr,"RT: can't create read channel from child!\a\n") ;
         exit(1) ;
      }
      /** next time thru RT_worker will try to read data from this channel **/

   } else {                          /** I'm the child **/

      RT_acquire_info( rtin->name_info ) ;  /* get info, send back to parent */
      _exit(0) ;
   }

   return ;
}

/*-------------------------------------------------------------------------
---------------------------------------------------------------------------
   This routine is only called from the child process:
     Run a command, get its output, send it back to the parent, exit.
     The entire existence of the child is confined to this routine.
---------------------------------------------------------------------------
---------------------------------------------------------------------------*/

int RT_acquire_info( char * command )
{
   FILE * fp ;
   char * info = (char *) malloc( sizeof(char) * INFO_SIZE ) ; int ninfo = 0 ;
   IOCHAN * ioc ;
   int jj ;

   /** create channel back to parent **/

   ioc = iochan_init( SHM_CHILD , "create" ) ;
   if( ioc == NULL ){
      fprintf(stderr,"RT: child fails to open channel back to parent!\a\n") ;
      _exit(1) ;
   }

   /** run command, read its output into this pipe **/

   fp = popen( command , "r" ) ;
   if( fp == NULL ){
      fprintf(stderr,"RT: child fails to open pipe to command=%s\a\n",command) ;
      IOCHAN_CLOSE(ioc) ; _exit(1) ;
   }

   /** read pipe until nothing more **/

   while( fgets(info+ninfo,INFO_SIZE-ninfo,fp) != NULL ){
      ninfo = strlen(info) ;
   }
   pclose(fp) ;

   /** send output back to parent **/

   jj = iochan_writecheck(ioc,-1) ;  /* wait until ready */
   if( jj < 0 ){
      fprintf(stderr,"RT: child can't write IOCHAN to parent!\a\n") ;
      IOCHAN_CLOSE(ioc) ; _exit(1) ;
   }

   iochan_sendall( ioc , info , ninfo+1 ) ;        /* include the NUL character */
   iochan_sleep(LONG_DELAY) ;                      /* wait a bit */
   while( ! iochan_clearcheck(ioc,LONG_DELAY) )    /* loop until cleared */
      iochan_sleep(LONG_DELAY) ;

   iochan_sleep(LONG_DELAY) ;                      /* once more, just for luck */

                                                   /**************************/
   free(info) ; IOCHAN_CLOSE(ioc) ; _exit(0) ;     /** END OF CHILD PROCESS **/
}                                                  /**************************/

/*---------------------------------------------------------------------------
   Process command strings in the buffer, up to the first '\0' character.
   Returns the number of characters processed, which will be one more than
   the index of the first '\0'.  If an error occurs, -1 is returned.
-----------------------------------------------------------------------------*/

#define BADNEWS     fprintf(stderr,"RT: illegal header info=%s\a\n",buf)
#define STARTER(st) (strncmp(buf,st,strlen(st)) == 0)
#define NBUF        256

int RT_process_info( int ninfo , char * info , RT_input * rtin )
{
   int ii , jj , nstart,nend , nuse , nbuf ;
   char buf[NBUF] ;

   if( rtin == NULL || info == NULL || ninfo == 0 ) return -1 ;

   for( nend=0 ; nend < ninfo && info[nend] != '\0' ; nend++ ) ; /* nada */
   if( nend == ninfo ){
      fprintf(stderr,"RT: info string not NUL-terminated!\a\n") ;
      return -1 ;
   }

   /******************************************/
   /** Scan ahead until next command string **/

   nstart = 0 ;
   while( nstart < nend ){

      /** skip whitespace **/

      for( ; nstart < nend && isspace(info[nstart]) ; nstart++ ) ; /* nada */
      if( nstart >= nend ) break ;

      /** copy characters into buf until the next '\n' or '\0' **/

      for( nbuf=0,jj=nstart ; nbuf < NBUF && info[jj] != '\n' && info[jj] != '\0' ; ){
         buf[nbuf++] = info[jj++] ;
      }
      if( nbuf == NBUF ){
         fprintf(stderr,"RT: line buffer overflow in control information!\a\n") ;
         nbuf-- ;
      }
      buf[nbuf] = '\0' ; nstart = jj ;

      if( verbose == 2 )
         fprintf(stderr,"RT: info line buffer=%s\n",buf) ;
      VMCHECK ;

      /************************************/
      /*** Scan for legal input strings ***/

      if( STARTER("ACQUISITION_TYPE") ){
         char typ[32] ;

         sscanf( buf , "ACQUISITION_TYPE %31s" , typ ) ;

              if( strcmp(typ,"2D+z")  == 0 ) rtin->dtype = DTYPE_2DZ ;
         else if( strcmp(typ,"2D+zt") == 0 ) rtin->dtype = DTYPE_2DZT ;
         else if( strcmp(typ,"3D")    == 0 ) rtin->dtype = DTYPE_3D ;
         else if( strcmp(typ,"3D+t")  == 0 ) rtin->dtype = DTYPE_3DT ;
         else
              BADNEWS ;

      } else if( STARTER("NAME") ){
         char npr[THD_MAX_PREFIX] = "\0" ;
         sscanf( buf , "NAME %31s" , npr ) ;
         if( THD_filename_ok(npr) ) strcpy( rtin->prefix , npr ) ;
         else
              BADNEWS ;

      } else if( STARTER("NUMVOL") ){
         int val = 0 ;
         sscanf( buf , "NUMVOL %d" , &val ) ;
         if( val > 0 ) rtin->nr = val ;
         else
              BADNEWS ;

      } else if( STARTER("TR") ){
         float val = 0.0 ;
         sscanf( buf , "TR %f" , &val ) ;
         if( val > 0.0 ) rtin->tr = val ;
         else
              BADNEWS ;

      } else if( STARTER("ZDELTA") ){
         float val = 0.0 ;
         sscanf( buf , "ZDELTA %f" , &val ) ;
         if( val > 0.0 ) rtin->dzz = val ;
         else
              BADNEWS ;
         if( verbose == 2 )
            fprintf(stderr,"RT: dzz = %g\n",rtin->dzz) ;
         VMCHECK ;

      } else if( STARTER("ZFIRST") ){
         float val = 0.0 ;
         char dcode = ' ' ;
         sscanf( buf , "ZFIRST %f%c" , &val,&dcode ) ;
         rtin->zzorg   = val ;
         rtin->zcen    = 0 ;
         rtin->zzdcode = ORCODE(dcode) ;
         if( verbose == 2 )
            fprintf(stderr,"RT: zzorg = %g%c\n" ,
                    rtin->zzorg , (rtin->zzdcode < 0) ? ' '
                                                      : ORIENT_first[rtin->zzdcode] ) ;
         VMCHECK ;

      } else if( STARTER("XYFOV") ){
         float xval = 0.0 , yval = 0.0 , zval = 0.0 ;
         sscanf( buf , "XYFOV %f %f %f" , &xval , &yval , &zval ) ;
         if( xval > 0.0 ){
            rtin->xxfov = xval ;
            rtin->yyfov = (yval > 0.0) ? yval : xval ;
            if( zval > 0.0 ) rtin->zzfov = zval ;
         } else
                BADNEWS ;
         if( verbose == 2 )
            fprintf(stderr,"RT: fov = %g %g %g\n",rtin->xxfov,rtin->yyfov,rtin->zzfov) ;
         VMCHECK ;

      } else if( STARTER("XYMATRIX") ){
         int xval = 0 , yval = 0 , zval = 0 ;
         sscanf( buf , "XYMATRIX %d %d %d" , &xval , &yval , &zval ) ;
         if( xval > 0 ){
            rtin->nxx = xval ;
            rtin->nyy = (yval > 0) ? yval : xval ;
            if( zval > 0 ) rtin->nzz = zval ;
         } else
                BADNEWS ;
         if( verbose == 2 )
            fprintf(stderr,"RT: matrix = %d %d %d\n",rtin->nxx,rtin->nyy,rtin->nzz) ;
         VMCHECK ;

      } else if( STARTER("ZNUM") ){
         int zval = 0 ;
         sscanf( buf , "ZNUM %d" , &zval ) ;
         if( zval > 0 ){
             rtin->nzz = zval ;
             if( rtin->nzz < 2 ) fprintf(stderr,"RT: # slices = 1!\a\n") ;
         }
         else
              BADNEWS ;
         if( verbose == 2 )
            fprintf(stderr,"RT: # slices = %d\n",rtin->nzz) ;
         VMCHECK ;

      } else if( STARTER("DATUM") ){
         int ii ;
         char tstr[32] = "\0" ;
         sscanf( buf , "DATUM %31s" , tstr ) ;
         for( ii=0 ; ii <= LAST_MRI_TYPE ; ii++ )
            if( strcmp(tstr,MRI_TYPE_name[ii]) == 0 ) break ;

         if( AFNI_GOOD_DTYPE(ii) ) rtin->datum = ii ;
         else
              BADNEWS ;
         if( verbose == 2 )
            fprintf(stderr,"RT: datum code = %d\n",rtin->datum) ;
         VMCHECK ;

      } else if( STARTER("ZORDER") ){
         char str[32] = "\0" ; int nord=0 , nb = 0 , nq ;
         sscanf( buf , "ZORDER %31s%n" , str , &nb ) ;
              if( strcmp(str,"alt") == 0 ) rtin->zorder = ZORDER_ALT ;
         else if( strcmp(str,"seq") == 0 ) rtin->zorder = ZORDER_SEQ ;
         else if( strcmp(str,"explicit") == 0 ){
            rtin->zorder = ZORDER_EXP ;
            do{                                  /* get all numbers */
               rtin->zseq[nord] = -1 ; nq = -1 ;
               sscanf(buf+nb , "%d%n" , &(rtin->zseq[nord]) , &nq) ;
               if( nq < 1 ) break ;              /* failed to get */
               nb += nq ; nord++ ;               /* otherwise, increment */
            } while( nb < nbuf ) ;               /* until end of buffer */
            rtin->nzseq = nord ;                 /* number found */
            if( nord < 1 || nord > NZMAX ) BADNEWS ;
         }
         else
              BADNEWS ;

      } else if( STARTER("XYZAXES") ){
         int ii , orx=-1,ory=-1,orz=-1 ;
         char xstr[32] = "\0" , ystr[32] = "\0" , zstr[32] = "\0" ;

         sscanf( buf , "XYZAXES %31s %31s %31s" , xstr,ystr,zstr ) ;

#define OR3OK(x,y,z) ( ((x)&6) + ((y)&6) + ((z)&6) == 6 )

         for( ii=0 ; ii < 6 ; ii++ ){
            if( strcmp(xstr,ORIENT_shortstr[ii]) == 0 ||
                strcmp(xstr,ORIENT_typestr[ii])  == 0   ) orx = ii ;

            if( strcmp(ystr,ORIENT_shortstr[ii]) == 0 ||
                strcmp(ystr,ORIENT_typestr[ii])  == 0   ) ory = ii ;

            if( strcmp(zstr,ORIENT_shortstr[ii]) == 0 ||
                strcmp(zstr,ORIENT_typestr[ii])  == 0   ) orz = ii ;
         }

         if( orx >= 0 && ory >= 0 && orz >= 0 && OR3OK(orx,ory,orz) ){
            rtin->orcxx = orx ;
            rtin->orcyy = ory ;
            rtin->orczz = orz ;
         } else
                BADNEWS ;

      } else {
         BADNEWS ;
      }
   }  /* end of loop over command buffers */

   /** now, determine if enough information exists to create a dataset **/

   rtin->info_ok = ( rtin->dtype > 0 )                            &&
                   ( THD_filename_ok(rtin->prefix) )              &&
                   ( strlen(rtin->prefix) < THD_MAX_PREFIX )      &&
                   ( rtin->tr > 0 )                               &&
                   ( rtin->dzz > 0 || rtin->zzfov > 0 )           &&
                   ( rtin->xxfov > 0 )                            &&
                   ( rtin->yyfov > 0 )                            &&
                   ( rtin->nxx > 0 )                              &&
                   ( rtin->nyy > 0 )                              &&
                   ( rtin->nzz > 0 )                              &&
                   ( AFNI_GOOD_DTYPE(rtin->datum) )               &&
                   ( rtin->zorder > 0 )                           &&
                   ( rtin->orcxx >= 0 )                           &&
                   ( rtin->orcyy >= 0 )                           &&
                   ( rtin->orczz >= 0 )                           &&
                   ( OR3OK(rtin->orcxx,rtin->orcyy,rtin->orczz) )    ;

   /** if possible, now compute the number of bytes in each input image **/

   if( AFNI_GOOD_DTYPE(rtin->datum) && rtin->nxx > 0 && rtin->nyy > 0 ){
      int n1 = mri_datum_size( rtin->datum ) ;

      if( rtin->dtype == DTYPE_2DZT || rtin->dtype == DTYPE_2DZ )
         rtin->imsize = rtin->nxx * rtin->nyy * n1 ;
      else if( rtin->nzz > 0 )
         rtin->imsize = rtin->nxx * rtin->nyy * rtin->nzz * n1 ;
   }

   /** return the number of characters processed **/

   return (nend+1) ;
}

/*--------------------------------------------------------------------
   Create the dataset and prepare to receive image data.  Some of this
   code is taken shamelessly from to3d.c (I wrote it, I can steal it).
   This routine should only be called when rtin->info_ok is true.
----------------------------------------------------------------------*/

void RT_start_dataset( RT_input * rtin )
{
   THD_ivec3 nxyz , orixyz;
   THD_fvec3 dxyz , orgxyz ;
   int nvox , npix , n1 , ii ;
   char npr[THD_MAX_PREFIX] ;

   /***************************/
   /** Let there be dataset! **/

   rtin->dset = EDIT_empty_copy(NULL) ;

   /********************************/
   /** make a good dataset prefix **/

   for( ii=1 ; ; ii++ ){
      sprintf( npr , "%.31s#%03d" , rtin->prefix , ii ) ;
      if( PLUTO_prefix_ok(npr) ) break ;
   }

   /**********************************************************/
   /** correct the spatial information for axes orientation **/

   rtin->dxx = rtin->xxfov / rtin->nxx ;
   rtin->dyy = rtin->yyfov / rtin->nyy ;

   if( rtin->zzfov > 0 ) rtin->dzz = rtin->zzfov / rtin->nzz ;

   if( rtin->xcen ) rtin->xxorg = 0.5 * (rtin->nxx - 1) * rtin->dxx ;
   if( rtin->ycen ) rtin->yyorg = 0.5 * (rtin->nyy - 1) * rtin->dyy ;
   if( rtin->zcen ) rtin->zzorg = 0.5 * (rtin->nzz - 1) * rtin->dzz ;

   /* if axis direction is a 'minus', voxel increments are negative */

   if( ORIENT_sign[rtin->orcxx] == '-' ) rtin->dxx = - rtin->dxx ;
   if( ORIENT_sign[rtin->orcyy] == '-' ) rtin->dyy = - rtin->dyy ;
   if( ORIENT_sign[rtin->orczz] == '-' ) rtin->dzz = - rtin->dzz ;

   /* if axis direction is a 'plus',
      then the origin is in a 'minus' direction,
      so the origin offset must have its sign changed */

   if( ORIENT_sign[rtin->orcxx] == '+' ) rtin->xxorg = - rtin->xxorg ;
   if( ORIENT_sign[rtin->orcyy] == '+' ) rtin->yyorg = - rtin->yyorg ;

   /* z-origin is complex, because the remote program might
      have given a direction code, or it might not have.
      We must set the sign of the z-origin based on the
      sign of the axis direction if no specific
      direction code is given, otherwise it should be
      based on the specific direction code (rtin->zzdcode). */

   if( rtin->zzdcode < 0 ){  /* no specific code */

      if( ORIENT_sign[rtin->orczz] == '+' ) rtin->zzorg = - rtin->zzorg ;

   } else {  /* have code */

      /* check if code matches the axis direction
         (or is the opposite, which is also OK).  */

      if( rtin->orczz != rtin->zzdcode                 &&
          rtin->orczz != ORIENT_OPPOSITE(rtin->zzdcode)  ){  /* this is bad */

         fprintf(stderr,"RT: ZFIRST direction code = %c but Z axis = %s!\a\n",
                 ORIENT_first[rtin->zzdcode] , ORIENT_shortstr[rtin->orczz] ) ;

         if( ORIENT_sign[rtin->orczz] == '+' ) rtin->zzorg = - rtin->zzorg ;

      } else {  /* things are OK */

         if( ORIENT_sign[rtin->zzdcode] == '+' ) rtin->zzorg = - rtin->zzorg ;
      }
   }

   /************************************************/
   /** add the spatial information to the dataset **/

   nxyz.ijk[0]   = rtin->nxx   ; dxyz.xyz[0]   = rtin->dxx ;
   nxyz.ijk[1]   = rtin->nyy   ; dxyz.xyz[1]   = rtin->dyy ;
   nxyz.ijk[2]   = rtin->nzz   ; dxyz.xyz[2]   = rtin->dzz ;

   orixyz.ijk[0] = rtin->orcxx ; orgxyz.xyz[0] = rtin->xxorg ;
   orixyz.ijk[1] = rtin->orcyy ; orgxyz.xyz[1] = rtin->yyorg ;
   orixyz.ijk[2] = rtin->orczz ; orgxyz.xyz[2] = rtin->zzorg ;

   EDIT_dset_items( rtin->dset ,
                       ADN_prefix      , npr ,
                       ADN_datum_all   , rtin->datum ,
                       ADN_nxyz        , nxyz ,
                       ADN_xyzdel      , dxyz ,
                       ADN_xyzorg      , orgxyz ,
                       ADN_xyzorient   , orixyz ,
                       ADN_malloc_type , DATABLOCK_MEM_MALLOC ,
                       ADN_nvals       , 1 ,
                       ADN_type        , HEAD_ANAT_TYPE ,
                       ADN_view_type   , VIEW_ORIGINAL_TYPE ,
                       ADN_func_type   , ANAT_EPI_TYPE ,
                    ADN_none ) ;

   /******************************************/
   /** add time axis information, if needed **/

   if( rtin->dtype == DTYPE_3DT ){  /* all slices simultaneous */

      EDIT_dset_items( rtin->dset ,
                          ADN_ntt      , 1 ,
                          ADN_ttorg    , 0.0 ,
                          ADN_ttdel    , rtin->tr ,
                          ADN_ttdur    , 0.0 ,
                          ADN_tunits   , UNITS_SEC_TYPE ,
                       ADN_none ) ;

   } else if( rtin->dtype == DTYPE_2DZT ){  /* slices at different times */

      float * tpattern  = (float *) malloc( sizeof(float) * rtin->nzz ) ;
      float   tframe    = rtin->tr / rtin->nzz ;
      float   tsl ;
      int     ii ;

      if( rtin->zorder == ZORDER_ALT ){        /* alternating +z direction */
         tsl = 0.0 ;
         for( ii=0 ; ii < rtin->nzz ; ii+=2 ){
            tpattern[ii] = tsl ; tsl += tframe ;
         }
         for( ii=1 ; ii < rtin->nzz ; ii+=2 ){
            tpattern[ii] = tsl ; tsl += tframe ;
         }
      } else if( rtin->zorder == ZORDER_SEQ ){ /* sequential +z direction */
         tsl = 0.0 ;
         for( ii=0 ; ii < rtin->nzz ; ii++ ){
            tpattern[ii] = tsl ; tsl += tframe ;
         }
      }

      EDIT_dset_items( rtin->dset ,
                          ADN_ntt      , 1 ,
                          ADN_ttorg    , 0.0 ,
                          ADN_ttdel    , rtin->tr ,
                          ADN_ttdur    , 0.0 ,
                          ADN_tunits   , UNITS_SEC_TYPE ,
                          ADN_nsl      , rtin->nzz ,
                          ADN_zorg_sl  , rtin->zzorg ,
                          ADN_dz_sl    , rtin->dzz ,
                          ADN_toff_sl  , tpattern ,
                       ADN_none ) ;

      free( tpattern ) ;
   }

   rtin->afni_status = 0 ;  /* uninformed at this time */
   DSET_lock(rtin->dset) ;  /* 20 Mar 1998 */

#ifdef ALLOW_REGISTRATION
   /*---- Make a dataset for registration, if need be ----*/

   if( rtin->reg_mode > 0 &&
       ((rtin->dtype==DTYPE_2DZT) || (rtin->dtype==DTYPE_3DT)) ){

      rtin->reg_dset = EDIT_empty_copy( rtin->dset ) ;

      strcat(npr,"%reg") ;
      EDIT_dset_items( rtin->reg_dset , ADN_prefix , npr , ADN_none ) ;

      rtin->reg_status = 0 ;
      rtin->reg_nvol   = 0 ;
      DSET_lock(rtin->reg_dset) ;
   }
#endif

   /***********************************************/
   /** now prepare space for incoming image data **/

   nvox = rtin->nxx * rtin->nyy * rtin->nzz ;
   n1   = mri_datum_size( rtin->datum ) ;

   rtin->sbr_size = nvox * n1 ;                /* size of sub-brick */
   rtin->sbr      = malloc( rtin->sbr_size ) ; /* space to hold sub-brick */
   if( rtin->sbr == NULL ){
      fprintf(stderr,"RT: can't malloc space for real-time dataset!\a\n") ;
      exit(1) ;
   }

   if( rtin->dtype == DTYPE_2DZT || rtin->dtype == DTYPE_2DZ )
      rtin->imsize = rtin->nxx * rtin->nyy * n1 ;
   else
      rtin->imsize = nvox * n1 ;

   rtin->im   = rtin->sbr ;  /* place to put first image in sub-brick */
   rtin->nsl  = 0 ;          /* number of slices gathered so far */
   rtin->nvol = 0 ;          /* number of volumes gathered so far */

   /** if there is already data stored in the temporary buffer
       (acquired before this routine was called), then we want
       to place it into the dataset now.                       **/

   if( rtin->bufar != NULL ){
      int ii , upsave ;
      MRI_IMAGE * ibb ;
      char * bbb ;

      if( verbose == 2 )
         fprintf(stderr,"RT: putting %d buffered images into dataset\n" ,
                        IMARR_COUNT(rtin->bufar) ) ;
      VMCHECK ;

      upsave = update ;  /* don't do updates to AFNI during this step */
      update = 0 ;

      for( ii=0 ; ii < IMARR_COUNT(rtin->bufar) ; ii++ ){
         ibb = IMARR_SUBIMAGE(rtin->bufar,ii) ;     /* next buffered image */
         bbb = (char *) MRI_BYTE_PTR( ibb ) ;       /* pointer to its data */
         memcpy( rtin->im , bbb , rtin->imsize ) ;  /* copy into sub-brick */
         mri_free( ibb ) ;                          /* free buffered image */
         RT_process_image( rtin ) ;                 /* send into dataset   */
      }
      FREE_IMARR( rtin->bufar ) ;   /* throw this away like a used Kleenex */

      update = upsave ;

      if( verbose == 2 )
         fprintf(stderr,"RT: buffered images all placed into dataset\n") ;
      VMCHECK ;
   }

   /** dataset now created and ready to boogie! **/

   return ;
}

/*--------------------------------------------------------------------
  Read one image (or volume) into the space pointed to by im.
----------------------------------------------------------------------*/

void RT_read_image( RT_input * rtin , char * im )
{
   int need , have , nbuffed ;

   /** sanity checks **/

   if( rtin == NULL || im == NULL ){
      fprintf(stderr,"RT: illegal inputs to RT_read_image!\a\n") ;
      exit(1) ;
   }

   if( rtin->imsize <= 0 ){
      fprintf(stderr,"RT: image data present, but don't know its size!\a\n") ;
      exit(1) ;
   }

   /** see if any data in buffer already **/

   have = rtin->nbuf ;

   /** if we have data already, use it first **/

   if( have > 0 ){

      nbuffed = MIN( have , rtin->imsize ) ;  /* how much to read from buffer */
      memcpy( im , rtin->buf , nbuffed ) ;    /* copy it into image */

      if( nbuffed < have){                    /* didn't use up all of buffer */
         memmove( rtin->buf , rtin->buf + nbuffed , rtin->nbuf - nbuffed ) ;
         rtin->nbuf = rtin->nbuf - nbuffed ;
      } else {                                /* used up all of buffer */
         rtin->nbuf = 0 ;
      }
   } else {
      nbuffed = 0 ;
   }

   /** at this point, nbuffed is the number of bytes read from the buffer.
       if we need to read more bytes from the I/O channel, then do so now. **/

   need = rtin->imsize - nbuffed ;  /* number of bytes left to fill image */

   if( need > 0 )
      iochan_recvall( rtin->ioc_data , im + nbuffed , need ) ;

   return ;
}

/*--------------------------------------------------------------------
   Read images and put them into place.  Note that if there is any
   data left in the rtin buffer, it will go into the image first.
----------------------------------------------------------------------*/

int RT_process_data( RT_input * rtin )
{
   int vdone ;

   /** can we create a dataset yet? **/

   if( rtin->dset == NULL && rtin->info_ok ){
      if( verbose == 2 )
         fprintf(stderr,"RT: info complete --> creating dataset.\n") ;
      VMCHECK ;
      RT_start_dataset( rtin ) ;
   }

   /** read images as long as there is data to suck up **/

   while( rtin->nbuf > 0 || iochan_readcheck(rtin->ioc_data,0) > 0 ){

      if( rtin->im != NULL ){  /** process data into dataset directly **/

         if( verbose == 2 )
            fprintf(stderr,"RT: reading image into dataset sub-brick.\n") ;
         VMCHECK ;

         RT_read_image( rtin , rtin->im ) ;  /* read into dataset buffer */
         RT_process_image( rtin ) ;          /* process it for the dataset */

      } else {                 /** read data into temporary buffer space **/
                               /** (will have to process it later, dude) **/
         MRI_IMAGE * newim ;
         char * newbuf ;

         if( rtin->imsize <= 0 ){
            fprintf(stderr,"RT: image data present, but don't know its size!\a\n") ;
            exit(1) ;
         }

         if( rtin->bufar == NULL )    /* initialize buffer for input images */
            INIT_IMARR(rtin->bufar) ;

         if( verbose == 2 && rtin->bufar->num % 10 == 0 ){
            fprintf(stderr,"RT: reading image into buffer[%d]\n",rtin->bufar->num) ;
            VMCHECK ;
         }

         newim  = mri_new( rtin->imsize , 1 , MRI_byte ) ; /* make space for next image */
         newbuf = (char *) MRI_BYTE_PTR(newim) ;           /* pointer to image data */
         ADDTO_IMARR( rtin->bufar , newim ) ;              /* add to saved image list */
         RT_read_image( rtin , newbuf ) ;                  /* read image data */
      }

   }  /* end of loop over reading images as long as we can */

   /** return an error code if the input data channel has gone bad **/

/**
   if( iochan_goodcheck(rtin->ioc_data,0) <= 0 ) return -1 ;
**/

   return 1 ;
}

/*-------------------------------------------------------------------
   Given image data stored in rtin->im, process it into the dataset
   that is under construction.  This routine should only be called
   after RT_start_dataset has been executed.
---------------------------------------------------------------------*/

#define MIN_TO_GRAPH 2

void RT_process_image( RT_input * rtin )
{
   int vdone ;

   /** check if new image completes the current volume **/

   if( rtin->dtype == DTYPE_2DZT || rtin->dtype == DTYPE_2DZ ){

      rtin->nsl ++ ;                     /* 1 more slice */
      vdone = (rtin->nsl == rtin->nzz) ; /* have all slices? */

   } else if( rtin->dtype == DTYPE_3DT || rtin->dtype == DTYPE_3D ){

      vdone = 1 ;                        /* 3D gets all data at once */
   }

   /** if volume is done, add it to the dataset **/

   if( vdone ){

      rtin->nvol ++ ;        /* 1 more volume is acquired! */

      if( verbose == 2 )
         fprintf(stderr,"RT: now have %d complete sub-bricks.\n",rtin->nvol) ;
      VMCHECK ;

      /* first time: put this volume in as "substitute" for empty 1st brick
         later:      add new volume at end of chain                         */

      if( rtin->nvol == 1 )
         EDIT_substitute_brick( rtin->dset , 0 , rtin->datum , rtin->sbr ) ;
      else
         EDIT_add_brick( rtin->dset , rtin->datum , 0.0 , rtin->sbr ) ;

      VMCHECK ;
      if( verbose == 2 )
         fprintf(stderr,"RT: added brick to dataset\n") ;
      VMCHECK ;

      /* must also change the number of times recorded
         [EDIT_add_brick does 'nvals' correctly, but not 'ntt'] */

      if( rtin->dtype == DTYPE_3DT || rtin->dtype == DTYPE_2DZT ){
         EDIT_dset_items( rtin->dset , ADN_ntt , rtin->nvol , ADN_none ) ;
         if( verbose == 2 )
            fprintf(stderr,"RT: altered ntt in dataset header\n") ;
         VMCHECK ;
      } else if( rtin->nvol > 1 ){
         fprintf(stderr,"RT: have %d bricks for time-independent dataset!\a\n",
                 rtin->nvol) ;
         VMCHECK ;
      }

      /** compute function, maybe? **/

      if( rtin->func_code > 0 ){
         int jj ;

         /** if needed, initialize the function computations **/

         if( rtin->func_condit == 0 ){
            jj = rtin->func_func( rtin , INIT_MODE ) ;
            if( jj < 0 ){ rtin->func_code = 0 ; rtin->func_func = NULL ; }
            rtin->func_condit = 1 ;  /* initialized */
         }

         /** do the function computations for this volume **/

         if( rtin->func_code > 0 )
            rtin->func_func( rtin , rtin->nvol - 1 ) ;
      }

#ifdef ALLOW_REGISTRATION
      if( rtin->reg_mode == REGMODE_2D_RTIME )
         RT_registration_2D_realtime( rtin ) ;
#endif /* ALLOW_REGISTRATION */

      /** make space for next sub-brick to arrive **/

      if( verbose == 2 )
         fprintf(stderr,"RT: malloc-ing %d bytes for next volume\n",rtin->sbr_size) ;
      VMCHECK ;

      rtin->sbr = malloc( rtin->sbr_size ) ;
      if( rtin->sbr == NULL ){
         fprintf(stderr,"RT: can't malloc real-time brick %d\a\n",rtin->nvol+1) ;
         exit(1) ;
      }
      if( verbose == 2 )
         fprintf(stderr,"RT: malloc succeeded\n") ;
      VMCHECK ;

      rtin->im  = rtin->sbr ;  /* location of slice #0 within sub-brick */
      rtin->nsl = 0 ;          /* number of slices gathered so far */

      /** tell AFNI about this dataset, maybe **/

      if( update > 0 ){  /* if we want updates every so often */
         int doit ;

         if( verbose == 2 )
            fprintf(stderr,"RT: checking for update status\n") ;
         VMCHECK ;

         doit = ( (rtin->dtype==DTYPE_3DT || rtin->dtype==DTYPE_2DZT) &&
                  (rtin->nvol == MIN_TO_GRAPH ||
                   (rtin->nvol > MIN_TO_GRAPH && rtin->nvol % update == 0)) ) ;

         if( doit ){
            if( verbose == 2 )
               fprintf(stderr,"RT: about to tell AFNI about this dataset.\n") ;
            VMCHECK ;
            RT_tell_afni(rtin,TELL_NORMAL) ;
         }
      }

   } else {  /** need to add more slices before volume is done **/

      int noff ;  /* slice position in output brick */

      if( rtin->zorder == ZORDER_SEQ ){  /* sequential:       */
                                         /* slice position is */
         noff = rtin->nsl ;              /* just slice number */

      } else if ( rtin->zorder == ZORDER_ALT ){  /* alternating: */

         int nhalf = (rtin->nzz + 1)/2 ;         /* number in first 1/2 */

         if( rtin->nsl < nhalf )
            noff = 2 * rtin->nsl ;               /* first half of slices */
         else
            noff = 1 + 2 * (rtin->nsl - nhalf) ; /* second half of slices */
      }

      rtin->im = rtin->sbr + (noff * rtin->imsize) ; /* where next slice goes */

   }

   return ;
}

/*-------------------------------------------------------------------
   Tell AFNI about the dataset we are building.
---------------------------------------------------------------------*/

void RT_tell_afni( RT_input * rtin , int mode )
{
   Three_D_View * im3d ;
   THD_session * sess ;
   THD_3dim_dataset * old_anat ;
   int ii , id , afni_init=0 ;
   static char clabel[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ;
   char clll ;
   MCW_arrowval * tav ;

   /** sanity check **/

   if( rtin == NULL || ! ISVALID_3DIM_DATASET(rtin->dset) ) return ;

   im3d = rtin->im3d ;                       /* AFNI controller */
   sess = rtin->sess ;                       /* AFNI session */
   tav  = im3d->vwid->imag->time_index_av ;  /* time index widget */

   ii   = AFNI_controller_index( im3d ) ;
   clll = clabel[ii] ;          /* controller label */

   old_anat = im3d->anat_now ;  /* the default */

   /**--- deal with the input dataset ---**/

   if( rtin->afni_status == 0 ){  /** first time for this dataset **/

      if( verbose )
         fprintf(stderr , "RT: sending dataset %s with %d bricks\n"
                          "    to AFNI controller [%c] session %s\n" ,
                 DSET_FILECODE(rtin->dset) , rtin->nvol ,
                 clll , sess->sessname ) ;
      VMCHECK ;

      EDIT_dset_items( rtin->dset, ADN_directory_name,sess->sessname, ADN_none ) ;

      THD_load_statistics( rtin->dset ) ;

      /** put it into the current session in the current controller **/

      if( ISANAT(rtin->dset) ){

         if( GLOBAL_library.have_dummy_dataset ) UNDUMMYIZE ;

         id = sess->num_anat ;

         if( id >= THD_MAX_SESSION_ANAT ){
            fprintf(stderr,"RT: max number of anat datasets exceeded!\a\n") ;
            exit(1) ;
         }
         sess->anat[id][VIEW_ORIGINAL_TYPE] = rtin->dset ; sess->num_anat = id+1 ;
         POPDOWN_strlist_chooser ;

      } else if( ISFUNC(rtin->dset) ){

         if( GLOBAL_library.have_dummy_dataset )
            fprintf(stderr,"RT: input of functional dataset into dummy session!\a\n") ;

         id = sess->num_func ;
         if( id >= THD_MAX_SESSION_FUNC ){
            fprintf(stderr,"RT: max number of func datasets exceeded!\a\n") ;
            exit(1) ;
         }
         sess->func[id][VIEW_ORIGINAL_TYPE] = rtin->dset ; (sess->num_func)++ ;
         AFNI_force_adoption( sess , False ) ;
         POPDOWN_strlist_chooser ;

      } else {
         fprintf(stderr,"RT: bizarre dataset type error!\a\n") ;
         exit(1) ;
      }

      /** tell AFNI to jump to this dataset **/

      if( ISANAT(rtin->dset) )
         im3d->vinfo->anat_num = sess->num_anat - 1 ;
      else
         im3d->vinfo->func_num = sess->num_func - 1 ;

      im3d->vinfo->time_index = 0 ;
      AV_assign_ival( tav , 0 ) ;

      afni_init         = 1 ;   /* below: will initialize AFNI to see this dataset */
      rtin->afni_status = 1 ;   /* AFNI knows now */

   } else {  /** 2nd or later call for this dataset **/

      THD_update_statistics( rtin->dset ) ;

      if( verbose )
         fprintf(stderr,"RT: update with %d bricks to AFNI [%c]\n",rtin->nvol,clll) ;
      VMCHECK ;

      tav->fmax = tav->imax = im3d->vinfo->time_index = rtin->nvol - 1  ;
      AV_assign_ival( tav , tav->imax ) ;
   }

   /**--- Deal with the computed function, if any ---**/

   if( rtin->func_dset != NULL ){

      rtin->func_func( rtin , UPDATE_MODE ) ;  /* put data into dataset */

      if( rtin->func_condit > 1 ){             /* data actually inside */

         THD_load_statistics( rtin->func_dset ) ; /* statistickizification */

         if( rtin->func_status == 0 ){  /** first time for this dataset **/

            if( verbose )
               fprintf(stderr , "RT: sending dataset %s with %d bricks\n"
                                "    to AFNI controller [%c] session %s\n" ,
                       DSET_FILECODE(rtin->func_dset) , DSET_NVALS(rtin->func_dset) ,
                       clll , sess->sessname ) ;
            VMCHECK ;

            EDIT_dset_items( rtin->func_dset, ADN_directory_name,sess->sessname, ADN_none ) ;
            id = sess->num_func ;
            if( id >= THD_MAX_SESSION_FUNC ){
               fprintf(stderr,"RT: max number of func datasets exceeded!\a\n") ;
               exit(1) ;
            }
            sess->func[id][VIEW_ORIGINAL_TYPE] = rtin->func_dset ; (sess->num_func)++ ;
            AFNI_force_adoption( sess , False ) ;
            POPDOWN_strlist_chooser ;

            im3d->vinfo->func_num = sess->num_func - 1 ;

            AFNI_SETUP_FUNC_ON(im3d) ;
            rtin->func_status = 1 ;   /* AFNI knows now */
            afni_init = 1 ;           /* below: tell AFNI to look at this function */

         } else {  /** 2nd or later call for this dataset **/

            /**--- actually, there's nothing to do here ---**/

         }
      }  /* end of if functional dataset actually has data */
   }  /* end of if functional dataset exists */

#ifdef ALLOW_REGISTRATION
   /**--- Deal with the registered dataset, if any ---**/

   if( rtin->reg_dset != NULL && rtin->reg_nvol > 0 ){

      if( rtin->reg_status == 0 ){  /** first time for this dataset **/

         THD_load_statistics( rtin->reg_dset ) ;

         if( verbose )
               fprintf(stderr , "RT: sending dataset %s with %d bricks\n"
                                "    to AFNI controller [%c] session %s\n" ,
                       DSET_FILECODE(rtin->reg_dset) , DSET_NVALS(rtin->reg_dset) ,
                       clll , sess->sessname ) ;
         VMCHECK ;

         EDIT_dset_items( rtin->reg_dset, ADN_directory_name,sess->sessname, ADN_none ) ;

         id = sess->num_anat ;
         if( id >= THD_MAX_SESSION_ANAT ){
            fprintf(stderr,"RT: max number of anat datasets exceeded!\a\n") ;
            exit(1) ;
         }
         sess->anat[id][VIEW_ORIGINAL_TYPE] = rtin->reg_dset ; sess->num_anat = id+1 ;
         POPDOWN_strlist_chooser ;

         rtin->reg_status = 1 ;   /* AFNI knows about this dataset now */

      } else {  /** 2nd or later call for this dataset **/

         THD_update_statistics( rtin->reg_dset ) ;
      }
   }
#endif

   /**--- actually talk to AFNI now ---**/

   if( afni_init ){  /* tell AFNI to view new dataset(s) */
      AFNI_SETUP_VIEW(im3d,VIEW_ORIGINAL_TYPE) ;
      if( EQUIV_DSETS(rtin->dset,old_anat) ) THD_update_statistics( rtin->dset ) ;
      else                                   THD_load_statistics  ( rtin->dset ) ;

      AFNI_initialize_view( old_anat , im3d ) ;  /* Geronimo! */

   } else {          /* just tell AFNI to refresh the images/graphs */
      Three_D_View * qq3d ;
      int review ;

      /* check all controllers to see if they are looking at the same datasets */

      for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ ){
         qq3d = GLOBAL_library.controllers[ii] ;
         if( !IM3D_OPEN(qq3d) ) break ;  /* skip this one */

         review = (qq3d == im3d)                         ||   /* same viewer?  */
                  EQUIV_DSETS(rtin->dset,qq3d->anat_now) ||   /* or same anat? */
                  ( rtin->func_dset != NULL   &&              /* or same func? */
                    qq3d->vinfo->func_visible &&
                    EQUIV_DSETS(rtin->func_dset,qq3d->fim_now) ) ;

#ifdef ALLOW_REGISTRATION
         review = review || ( rtin->reg_dset != NULL &&       /* or same reg?  */
                              rtin->reg_nvol > 0     &&
                              EQUIV_DSETS(rtin->reg_dset,qq3d->anat_now) ) ;
#endif

         if( review ){
            if( verbose == 2 )
               fprintf(stderr,"RT: send redraw message to controller [%c]\n",
                       clabel[ii] ) ;
            VMCHECK ;
            AFNI_modify_viewing( qq3d , False ) ;  /* Crazy Horse! */
         }
      }
   }
   XmUpdateDisplay( im3d->vwid->top_shell ) ;

   /**--- if this is the final call, do some cleanup stuff ---**/

   if( mode == TELL_FINAL ){

      int cmode ;

      if( verbose == 2 )
         fprintf(stderr,"RT: finalizing dataset to AFNI (including disk output).\n") ;
      VMCHECK ;

      fprintf(stderr , "RT: sending dataset %s with %d bricks\n"
                       "    to AFNI controller [%c] session %s\n" ,
              DSET_FILECODE(rtin->dset) , rtin->nvol ,
              clll , sess->sessname ) ;

      { char qbuf[256] ;
        sprintf( qbuf , " \n"
                        " Acquisition Terminated\n\n"
                        " Dataset saved as %s\n"
                        " with %d bricks\n" ,
                 DSET_FILECODE(rtin->dset) , rtin->nvol ) ;

        PLUTO_beep() ;
        PLUTO_popup_transient( plint , qbuf ) ;
      }

/**
      THD_load_statistics( rtin->dset ) ;
**/

      /* 20 Mar 1998: write in uncompressed mode */

      cmode = THD_get_write_compression() ;
      THD_set_write_compression(COMPRESS_NONE) ;
      SHOW_AFNI_PAUSE ;

      THD_write_3dim_dataset( NULL,NULL , rtin->dset , True ) ;
      DSET_unlock( rtin->dset ) ;  /* 20 Mar 1998 */

      if( rtin->func_dset != NULL ){
         rtin->func_func( rtin , FINAL_MODE ) ;
         THD_write_3dim_dataset( NULL,NULL , rtin->func_dset , True ) ;
         DSET_unlock( rtin->func_dset ) ;  /* 20 Mar 1998 */
         THD_force_malloc_type( rtin->func_dset->dblk , DATABLOCK_MEM_ANY ) ;
      }

#ifdef ALLOW_REGISTRATION
      if( rtin->reg_dset != NULL && rtin->reg_nvol > 0 ){
         THD_write_3dim_dataset( NULL,NULL , rtin->reg_dset , True ) ;
         DSET_unlock( rtin->reg_dset ) ;
         THD_force_malloc_type( rtin->reg_dset->dblk , DATABLOCK_MEM_ANY ) ;
      }
#endif

      THD_set_write_compression(cmode) ;  /* restore compression mode */
      SHOW_AFNI_READY ;

      AFNI_force_adoption( sess , GLOBAL_argopt.warp_4D ) ;
      AFNI_make_descendants( GLOBAL_library.sslist ) ;
      THD_force_malloc_type( rtin->dset->dblk , DATABLOCK_MEM_ANY ) ;

      AFNI_purge_unused_dsets() ;
   }

   if( verbose == 2 ) SHOW_TIMES ;
   VMCHECK ;

   return ;
}

/*--------------------------------------------------------------------------
  This routine is called when no more data will be added to the
  incoming dataset.
----------------------------------------------------------------------------*/

void RT_finish_dataset( RT_input * rtin )
{
   int ii ;

   if( ! ISVALID_3DIM_DATASET(rtin->dset) ) return ;

   if( rtin->nvol < 1 ){
     fprintf(stderr,"RT: attempt to finish dataset with 0 completed bricks!\a\n") ;
     DSET_delete( rtin->dset ) ; rtin->dset = NULL ;
     if( rtin->func_dset != NULL ){
        DSET_delete( rtin->func_dset ) ; rtin->func_dset = NULL ;
     }
#ifdef ALLOW_REGISTRATION
     if( rtin->reg_dset != NULL ){
        DSET_delete( rtin->reg_dset ) ; rtin->reg_dset = NULL ;
     }
#endif
     return ;
   }

   if( rtin->nsl > 0 )
      fprintf(stderr,"RT: finish dataset with %d slices unused!\a\n",rtin->nsl);

   fprintf(stderr,"RT: finish dataset with %d bricks completed.\n",rtin->nvol) ;

   if( verbose ) SHOW_TIMES ;
   VMCHECK ;

#ifdef ALLOW_REGISTRATION
   /*--- Do the "at end" registration, if ordered and if possible ---*/

   if( rtin->reg_mode == REGMODE_2D_ATEND )
      RT_registration_2D_atend( rtin ) ;
#endif

   /** tell afni about it one last time **/

   RT_tell_afni(rtin,TELL_FINAL) ;

   return ;
}

#ifdef ALLOW_REGISTRATION
/*---------------------------------------------------------------------------
  Do pieces of 2D registration during realtime.
-----------------------------------------------------------------------------*/

void RT_registration_2D_realtime( RT_input * rtin )
{
   int tt , ntt ;

   if( rtin->reg_dset == NULL ) return ;

   /*-- check to see if we need to setup first --*/

   if( rtin->reg_2dbasis == NULL ){  /* need to setup */

      /* check if enough data to setup */

      if( rtin->reg_base_index >= rtin->nvol ) return ;  /* can't setup */

      /* setup the registration process */

      if( verbose == 2 )
         fprintf(stderr,"RT: setting up 2D registration 'realtime'\n") ;

      RT_registration_2D_setup( rtin ) ;

      if( rtin->reg_2dbasis == NULL ){
         fprintf(stderr,"RT: can't setup %s registration!\a\n",
                 REG_strings[REGMODE_2D_RTIME] ) ;
         DSET_delete( rtin->reg_dset ) ; rtin->reg_dset = NULL ;
         rtin->reg_mode = REGMODE_NONE ;
         return ;
      }
   }

   /*-- register all sub-bricks that aren't done yet --*/

   ntt = DSET_NUM_TIMES( rtin->dset ) ;
   for( tt=rtin->reg_nvol ; tt < ntt ; tt++ )
      RT_registration_2D_onevol( rtin , tt ) ;

   /*-- my work here is done --*/

   return ;
}

/*---------------------------------------------------------------------------
  Do 2D registration of all slices at once, when the realtime dataset
  is all present and accounted for.
-----------------------------------------------------------------------------*/

void RT_registration_2D_atend( RT_input * rtin )
{
   int tt , ntt ;

   /* check if have enough data to register as ordered */

   if( rtin->reg_base_index >= rtin->nvol ){
      fprintf(stderr,"RT: can't do %s registration: not enough 3D volumes!\a\n",
              REG_strings[REGMODE_2D_ATEND] ) ;
      DSET_delete( rtin->reg_dset ) ; rtin->reg_dset = NULL ;
      rtin->reg_mode = REGMODE_NONE ;
      return ;
   }

   /* set up the registration process */

   if( verbose == 2 )
      fprintf(stderr,"RT: setting up 2D registration 'at end'\n") ;

   RT_registration_2D_setup( rtin ) ;

   if( rtin->reg_2dbasis == NULL ){
      fprintf(stderr,"RT: can't setup %s registration!\a\n",
              REG_strings[REGMODE_2D_ATEND] ) ;
      DSET_delete( rtin->reg_dset ) ; rtin->reg_dset = NULL ;
      rtin->reg_mode = REGMODE_NONE ;
      return ;
   }

   /* register each volume into the new dataset */

   ntt = DSET_NUM_TIMES( rtin->dset ) ;
   for( tt=0 ; tt < ntt ; tt++ )
      RT_registration_2D_onevol( rtin , tt ) ;

   /* un-setup the registration process */

   RT_registration_2D_close( rtin ) ;
   if( verbose == 2 ) SHOW_TIMES ;
   return ;
}

/*-----------------------------------------------------------------------
   Setup the 2D registration data for each slice
-------------------------------------------------------------------------*/

void RT_registration_2D_setup( RT_input * rtin )
{
   int ibase = rtin->reg_base_index ;
   int kk , nx,ny,nz , kind , nbar ;
   MRI_IMAGE * im ;
   char * bar ;

   nx   = DSET_NX( rtin->dset ) ;
   ny   = DSET_NY( rtin->dset ) ;
   nz   = DSET_NZ( rtin->dset ) ;
   kind = DSET_BRICK_TYPE( rtin->dset , ibase ) ;

   rtin->reg_nvol  = 0 ;

   rtin->reg_2dbasis = (MRI_2dalign_basis **)
                         malloc( sizeof(MRI_2dalign_basis *) * nz ) ;

   im   = mri_new_vol_empty( nx,ny,1 , kind ) ;     /* fake image for slices */
   bar  = DSET_BRICK_ARRAY( rtin->dset , ibase ) ;  /* ptr to base volume    */
   nbar = im->nvox * im->pixel_size ;               /* offset for each slice */

   for( kk=0 ; kk < nz ; kk++ ){                         /* loop over slices */
      mri_fix_data_pointer( bar + kk*nbar , im ) ;             /* base image */
      rtin->reg_2dbasis[kk] = mri_2dalign_setup( im , NULL ) ;
   }

   mri_fix_data_pointer( NULL , im ) ; mri_free( im ) ;   /* get rid of this */
   return ;
}

/*---------------------------------------------------------------------------
    Undo the routine just above!
-----------------------------------------------------------------------------*/

void RT_registration_2D_close( RT_input * rtin )
{
   int kk , nz ;

   nz = DSET_NZ( rtin->dset ) ;
   for( kk=0 ; kk < nz ; kk++ )
      mri_2dalign_cleanup( rtin->reg_2dbasis[kk] ) ;

   free( rtin->reg_2dbasis ) ; rtin->reg_2dbasis = NULL ;
   return ;
}

/*-----------------------------------------------------------------------
   Carry out the 2D registration for each slice in the tt-th sub-brick
-------------------------------------------------------------------------*/

void RT_registration_2D_onevol( RT_input * rtin , int tt )
{
   int kk , nx,ny,nz , kind , nbar ;
   MRI_IMAGE * im , * rim , * qim ;
   char * bar , * rar , * qar ;

   /*-- sanity check --*/

   if( rtin->dset == NULL || rtin->reg_dset == NULL ) return ;

   nx   = DSET_NX( rtin->dset ) ;
   ny   = DSET_NY( rtin->dset ) ;
   nz   = DSET_NZ( rtin->dset ) ;
   kind = DSET_BRICK_TYPE( rtin->dset , 0 ) ;

   im   = mri_new_vol_empty( nx,ny,1 , kind ) ;     /* fake image for slices */
   bar  = DSET_BRICK_ARRAY( rtin->dset , tt ) ;     /* ptr to input volume   */
   nbar = im->nvox * im->pixel_size ;               /* offset for each slice */

   /* make space for new sub-brick in reg_dset */

   if( verbose == 2 )
      fprintf(stderr,"RT: registering sub-brick %d",tt) ;

   rar = (char *) malloc( sizeof(char) * nx*ny*nz * im->pixel_size ) ;

   if( rar == NULL ){
      fprintf(stderr,"RT: can't malloc space for registered dataset!\a\n") ;
      DSET_delete( rtin->reg_dset ) ; rtin->reg_dset = NULL ;
      rtin->reg_mode = REGMODE_NONE ;
      return ;
   }

   /*-- loop over slices --*/

   for( kk=0 ; kk < nz ; kk++ ){

      if( verbose == 2 ) fprintf(stderr,".") ;

      mri_fix_data_pointer( bar + kk*nbar , im ) ;  /* image to register */

      /* registration! */

      rim = mri_2dalign_one( rtin->reg_2dbasis[kk] , im , NULL,NULL,NULL ) ;

      /* convert output image to desired type;
         set qar to point to data in the converted registered image */

      switch( kind ){
         case MRI_float:                        /* rim is already floats */
            qar = (char *) MRI_FLOAT_PTR(rim) ;
         break ;

         case MRI_short:
            qim = mri_to_short(1.0,rim) ; mri_free(rim) ; rim = qim ;
            qar = (char *) MRI_SHORT_PTR(rim) ;
         break ;

         case MRI_byte:
            qim = mri_to_byte(rim) ; mri_free(rim) ; rim = qim ;
            qar = (char *) MRI_BYTE_PTR(rim) ;
         break ;

         default:
            fprintf(stderr,"RT: can't do registration on %s images!\a\n",
                    MRI_TYPE_name[kind] ) ;
            DSET_delete( rtin->reg_dset ) ; rtin->reg_dset = NULL ;
            rtin->reg_mode = REGMODE_NONE ;
            free(rar) ;
            mri_free(rim) ; mri_fix_data_pointer(NULL,im) ; mri_free(im) ;
         return ;
      }

      /* copy data from registered image into output sub-brick */

      memcpy( rar + kk*nbar , qar , nbar ) ;

      mri_free(rim) ; /* don't need this no more no how */
   }

   mri_fix_data_pointer(NULL,im) ; mri_free(im) ;   /* get rid of this */

   /*-- attach rar to reg_dset --*/

   if( tt == 0 )
      EDIT_substitute_brick( rtin->reg_dset , 0 , rtin->datum , rar ) ;
   else
      EDIT_add_brick( rtin->reg_dset , rtin->datum , 0.0 , rar ) ;

   rtin->reg_nvol = tt+1 ;

   EDIT_dset_items( rtin->reg_dset , ADN_ntt , rtin->reg_nvol ,  ADN_none ) ;

   if( verbose == 2 ) fprintf(stderr,"\n") ;
   return ;
}
/*---------------------------------------------------------------------------*/
#endif /* ALLOW_REGISTRATION */

#define FIM_THR  0.0999
#define MIN_UPDT 5

/*--------------------------------------------------------------------------
  Recursively update the functional image computation.
    mode = (if >= 0) index of sub-brick to process this time
           (if <  0) one of the MODE codes above:
             INIT_MODE:   create a new dataset, and initialize workspaces
             UPDATE_MODE: put values into dataset bricks
             FINAL_MODE:  clean up workspaces

  The return value is 1 if all is OK, -1 if something bad happens.

  Adapted 27 Apr 1997 from AFNI_fimmer_compute in afni_fimmer.c -- only
  the computational stuff is left; the display stuff is relegated to
  another routine.
-----------------------------------------------------------------------------*/

#define IFree(x) do{ if( (x)!=NULL ){ free(x) ; (x)=NULL ; } } while(0)

int RT_fim_recurse( RT_input * rtin , int mode )
{
   static Three_D_View * im3d ;
   static THD_3dim_dataset * dset_time ;
   static MRI_IMAGE * ref_ts ;
   static MRI_IMAGE * ort_ts ;

   static THD_3dim_dataset * new_dset ;
   static int nvox , ngood_ref , it1 , dtyp , nxyz , nupdt ;
   static float * vval=NULL , * aval=NULL , * rbest=NULL , * abest=NULL ;
   static int   * indx=NULL ;
   static PCOR_references ** pc_ref=NULL ;
   static PCOR_voxel_corr ** pc_vc=NULL ;
   static int nx_ref , ny_ref ;

   static int fim_nref , nx_ort , ny_ort , internal_ort ;
   static float * ortar ;
   static float * ref_vec = NULL ;
   static int    nref_vec = -666 ;

   static char new_prefix[THD_MAX_PREFIX] ;
   static char old_prefix[THD_MAX_PREFIX] ;

   int ifim, it, iv, ivec, nnow, itbot ;
   float fthr , topval , stataux[MAX_STAT_AUX] ;
   float * tsar ;
   short * bar ;
   void  * ptr ;

   /******************/
   /** sanity check **/

   if( rtin == NULL ) return -1 ;

   /****************************/
   /***** initialize stuff *****/

   if( mode == INIT_MODE ){

      im3d      = rtin->im3d ;            if( im3d      == NULL ) return -1 ;
      dset_time = rtin->dset ;            if( dset_time == NULL ) return -1 ;
      ref_ts    = im3d->fimdata->fimref ; if( ref_ts    == NULL ) return -1 ;
      ort_ts    = im3d->fimdata->fimort ; /* NULL is legal here */
      nupdt     = 0 ;                     /* number of updates done yet */

      if( ort_ts != NULL ){               /* load some dimensions */
         nx_ort = ort_ts->nx ;
         ny_ort = ort_ts->ny ;
         ortar  = MRI_FLOAT_PTR(ort_ts) ;
         internal_ort = 0 ;
      } else {
         nx_ort = ny_ort = 0 ;
         ortar  = NULL ;
         internal_ort = 1 ;
      }
      fim_nref = (internal_ort) ? 3 : (ny_ort+3) ;

      if( nref_vec < fim_nref ){  /* allocate space for ref work vector */
          ref_vec = (float *) XtRealloc( (char *)ref_vec, sizeof(float)*fim_nref ) ;
         nref_vec = fim_nref ;
      }

      itbot     = im3d->fimdata->init_ignore ;  /* certainly don't use these points! */
      nx_ref    = ref_ts->nx ;                  /* load dimensions */
      ny_ref    = ref_ts->ny ;
      ngood_ref =  0 ;
      it1       = -1 ;
      for( ivec=0 ; ivec < ny_ref ; ivec++ ){           /* scan ref vectors */
         tsar = MRI_FLOAT_PTR(ref_ts) + (ivec*nx_ref) ; /* ptr to ivec-th vector */
         ifim = 0 ;                                     /* count # good entries */
         for( it=itbot ; it < nx_ref ; it++ ){
            if( tsar[it] < WAY_BIG ){ ifim++ ; if( it1 < 0 ) it1 = it ; }
         }

         if( ifim < MIN_UPDT ){
            fprintf(stderr,"RTfim: ideal timeseries has too few good entries!\a\n") ;
            return -1 ;
         }

         ngood_ref = MAX( ifim , ngood_ref ) ;
      }

      /** at this point, ngood_ref = max number of good reference points,
          and                  it1 = index of first point used in first reference **/

      dtyp = DSET_BRICK_TYPE(dset_time,0) ;  /* type of each voxel */
      if( ! AFNI_GOOD_FUNC_DTYPE(dtyp) ){
         fprintf(stderr,"RTfim: input dataset has unsupported datum %s!\a\n",
                 MRI_TYPE_name[dtyp] ) ;
         return -1 ;
      }

      if( nx_ort > 0 && nx_ort < nx_ref ){  /* check for compatibility */
         fprintf(stderr,
                 "RTfim: WARNING -- ort length=%d  ideal length=%d\n",
                 nx_ort , nx_ref ) ;
      }

      /*--- Create a new prefix ---*/

      MCW_strncpy( old_prefix , DSET_PREFIX(dset_time) , THD_MAX_PREFIX-3 ) ;

      for( ifim=1 ; ifim < 99 ; ifim++ ){
         sprintf( new_prefix , "%s@%d" , old_prefix , ifim ) ;
         if( PLUTO_prefix_ok(new_prefix) ) break ;
      }
      if( ifim == 99 ){
         fprintf(stderr,"RTfim: can't create new prefix!\a\n") ;
         return -1 ;
      }

      nxyz =  dset_time->dblk->diskptr->dimsizes[0]    /* total voxel count */
            * dset_time->dblk->diskptr->dimsizes[1]
            * dset_time->dblk->diskptr->dimsizes[2] ;

      return 1 ;
   }

   /**************************/
   /***** finalize stuff *****/

   if( mode == FINAL_MODE ){

      /*--- End of recursive updates; now free temporary workspaces ---*/

      if( pc_ref != NULL ){
         for( ivec=0 ; ivec < ny_ref ; ivec++ ){
            free_PCOR_references(pc_ref[ivec]) ;
            free_PCOR_voxel_corr(pc_vc[ivec]) ;
         }
      }
      IFree(vval) ; IFree(indx)  ; IFree(pc_ref) ; IFree(pc_vc)  ;
      IFree(aval) ; IFree(rbest) ; IFree(abest)  ;

      return 1 ;
   }

   /*********************************************/
   /***** update the dataset under creation *****/

   if( mode == UPDATE_MODE ){

      if( nupdt < MIN_UPDT ) return 1 ;  /* can't do anything yet */

      rtin->func_condit = 2 ;  /* updated at least once */

      /*--- set the statistical parameters ---*/

      stataux[0] = nupdt ;               /* number of points used */
      stataux[1] = (ny_ref==1) ? 1 : 2 ; /* number of references  */
      stataux[2] = fim_nref - 1 ;        /* number of orts        */
      for( iv=3 ; iv < MAX_STAT_AUX ; iv++ ) stataux[iv] = 0.0 ;

      (void) EDIT_dset_items( new_dset, ADN_stat_aux, stataux, ADN_none ) ;

      /*** Compute brick arrays for new dataset ***/

      if( ny_ref == 1 ){

      /*** Just 1 ref vector --> load values directly into dataset ***/

         /*--- get alpha (coef) into vval,
               find max value, scale into brick array ---*/

         PCOR_get_coef( pc_ref[0] , pc_vc[0] , vval ) ;

         topval = 0.0 ;
         for( iv=0 ; iv < nvox ; iv++ )
            if( fabs(vval[iv]) > topval ) topval = fabs(vval[iv]) ;

         bar = DSET_ARRAY( new_dset , FUNC_ival_fim[FUNC_COR_TYPE] ) ;

#ifdef DONT_USE_MEMCPY
         for( iv=0 ; iv < nxyz ; iv++ ) bar[iv] = 0 ;
#else
         memset( bar , 0 , sizeof(short)*nxyz ) ;
#endif

         if( topval > 0.0 ){
            topval = MRI_TYPE_maxval[MRI_short] / topval ;
            for( iv=0 ; iv < nvox ; iv++ )
               bar[indx[iv]] = (short)(topval * vval[iv] + 0.499) ;

            stataux[0] = 1.0/topval ;
         } else {
            stataux[0] = 0.0 ;
         }

         /*--- get correlation coefficient (pcor) into vval,
               scale into brick array (with fixed scaling factor) ---*/

         PCOR_get_pcor( pc_ref[0] , pc_vc[0] , vval ) ;

         bar = DSET_ARRAY( new_dset , FUNC_ival_thr[FUNC_COR_TYPE] ) ;

#ifdef DONT_USE_MEMCPY
         for( iv=0 ; iv < nxyz ; iv++ ) bar[iv] = 0 ;
#else
         memset( bar , 0 , sizeof(short)*nxyz ) ;
#endif

         for( iv=0 ; iv < nvox ; iv++ )
            bar[indx[iv]] = (short)(FUNC_COR_SCALE_SHORT * vval[iv] + 0.499) ;

         stataux[1] = 1.0 / FUNC_COR_SCALE_SHORT ;

      } else {

      /*** Multiple references --> find best correlation at each voxel ***/

         /*--- get first ref results into abest and rbest (best so far) ---*/

         PCOR_get_coef( pc_ref[0] , pc_vc[0] , abest ) ;
         PCOR_get_pcor( pc_ref[0] , pc_vc[0] , rbest ) ;

         /*--- for each succeeding ref vector,
               get results into aval and vval,
               if |vval| > |rbest|, then use that result instead ---*/

         for( ivec=1 ; ivec < ny_ref ; ivec++ ){

            PCOR_get_coef( pc_ref[ivec] , pc_vc[ivec] , aval ) ;
            PCOR_get_pcor( pc_ref[ivec] , pc_vc[ivec] , vval ) ;

            for( iv=0 ; iv < nvox ; iv++ ){
               if( fabs(vval[iv]) > fabs(rbest[iv]) ){
                  rbest[iv] = vval[iv] ;
                  abest[iv] = aval[iv] ;
               }
            }
         }

         /*--- at this point, abest and rbest are the best
               results, so scale them into the dataset bricks ---*/

         topval = 0.0 ;
         for( iv=0 ; iv < nvox ; iv++ )
            if( fabs(abest[iv]) > topval ) topval = fabs(abest[iv]) ;

         bar = DSET_ARRAY( new_dset , FUNC_ival_fim[FUNC_COR_TYPE] ) ;

#ifdef DONT_USE_MEMCPY
         for( iv=0 ; iv < nxyz ; iv++ ) bar[iv] = 0 ;
#else
         memset( bar , 0 , sizeof(short)*nxyz ) ;
#endif

         if( topval > 0.0 ){
            topval = MRI_TYPE_maxval[MRI_short] / topval ;
            for( iv=0 ; iv < nvox ; iv++ )
               bar[indx[iv]] = (short)(topval * abest[iv] + 0.499) ;

            stataux[0] = 1.0/topval ;
         } else {
            stataux[0] = 0.0 ;
         }

         bar = DSET_ARRAY( new_dset , FUNC_ival_thr[FUNC_COR_TYPE] ) ;

#ifdef DONT_USE_MEMCPY
         for( iv=0 ; iv < nxyz ; iv++ ) bar[iv] = 0 ;
#else
         memset( bar , 0 , sizeof(short)*nxyz ) ;
#endif

         for( iv=0 ; iv < nvox ; iv++ )
            bar[indx[iv]] = (short)(FUNC_COR_SCALE_SHORT * rbest[iv] + 0.499) ;

         stataux[1] = 1.0 / FUNC_COR_SCALE_SHORT ;
      }

      /* set scaling factors for each sub-brick */

      (void) EDIT_dset_items( new_dset, ADN_brick_fac, stataux, ADN_none ) ;
      return 1 ;
   }

   /*********************************************************/
   /***** at this point, must process time point # mode *****/

   if( mode < it1 ) return 1 ;  /* nothing to do before first time */

   /*---- at first time, find values above threshold to fim:
            find the mean of the first array,
            compute the threshold (fthr) from it,
            make indx[i] be the 3D index of the i-th voxel above threshold ----*/

   if( mode == it1 ){  /* am exactly at the first time relevant to fim */

      switch( dtyp ){  /* process each datum type separately */

         case MRI_short:{
            short * dar = (short *) DSET_ARRAY(dset_time,it1) ;
            for( iv=0,fthr=0.0 ; iv < nxyz ; iv++ ) fthr += abs(dar[iv]) ;
            fthr = FIM_THR * fthr / nxyz ;
            for( iv=0,nvox=0 ; iv < nxyz ; iv++ )
               if( abs(dar[iv]) > fthr ) nvox++ ;
            indx = (int *) malloc( sizeof(int) * nvox ) ;
            if( indx == NULL ){
               fprintf(stderr,"RTfim: indx malloc failure!\a\n") ;
               exit(1) ;
            }
            for( iv=0,nvox=0 ; iv < nxyz ; iv++ )
               if( abs(dar[iv]) > fthr ) indx[nvox++] = iv ;
         }
         break ;

         case MRI_float:{
            float * dar = (float *) DSET_ARRAY(dset_time,it1) ;
            for( iv=0,fthr=0.0 ; iv < nxyz ; iv++ ) fthr += fabs(dar[iv]) ;
            fthr = FIM_THR * fthr / nxyz ;
            for( iv=0,nvox=0 ; iv < nxyz ; iv++ )
               if( fabs(dar[iv]) > fthr ) nvox++ ;
            indx = (int *) malloc( sizeof(int) * nvox ) ;
            if( indx == NULL ){
               fprintf(stderr,"RTfim: indx malloc failure!\a\n") ;
               exit(1) ;
            }
            for( iv=0,nvox=0 ; iv < nxyz ; iv++ )
               if( fabs(dar[iv]) > fthr ) indx[nvox++] = iv ;
         }
         break ;

         case MRI_byte:{
            byte * dar = (byte *) DSET_ARRAY(dset_time,it1) ;
            for( iv=0,fthr=0.0 ; iv < nxyz ; iv++ ) fthr += dar[iv] ;
            fthr = FIM_THR * fthr / nxyz ;
            for( iv=0,nvox=0 ; iv < nxyz ; iv++ )
               if( dar[iv] > fthr ) nvox++ ;
            indx = (int *) malloc( sizeof(int) * nvox ) ;
            if( indx == NULL ){
               fprintf(stderr,"RTfim: indx malloc failure!\a\n") ;
               exit(1) ;
            }
            for( iv=0,nvox=0 ; iv < nxyz ; iv++ )
               if( dar[iv] > fthr ) indx[nvox++] = iv ;
         }
         break ;
      }

      /** allocate space for voxel values that are above the threshold **/
      /** (nvox = # voxels above threshold;  nxyz = total # voxels)    **/

      if( verbose == 2 )
         fprintf(stderr,"RTfim: %d/%d voxels being FIMmed.\n",nvox,nxyz) ;
      VMCHECK ;

      vval = (float *) malloc( sizeof(float) * nvox) ;
      if( vval == NULL ){
         fprintf(stderr,"RTfim: vval malloc failure!\a\n") ;
         exit(1) ;
      }

      /** allocate extra space for comparing results from multiple ref vectors **/

      if( ny_ref > 1 ){
         aval  = (float *) malloc( sizeof(float) * nvox) ;
         rbest = (float *) malloc( sizeof(float) * nvox) ;
         abest = (float *) malloc( sizeof(float) * nvox) ;
         if( aval==NULL || rbest==NULL || abest==NULL ){
            fprintf(stderr,"RTfim: abest malloc failure!\a\n") ;
            exit(1) ;
         }
      } else {
         aval = rbest = abest = NULL ;
      }

      /*--- initialize recursive updates ---*/

      pc_ref = (PCOR_references **) malloc( sizeof(PCOR_references *) * ny_ref ) ;
      pc_vc  = (PCOR_voxel_corr **) malloc( sizeof(PCOR_voxel_corr *) * ny_ref ) ;

      if( pc_ref == NULL || pc_vc == NULL ){
         fprintf(stderr,"RTfim: can't malloc recursion space!\a\n") ;
         exit(1) ;
      }

      for( ivec=0 ; ivec < ny_ref ; ivec++ ){
         pc_ref[ivec] = new_PCOR_references( fim_nref ) ;
         pc_vc[ivec]  = new_PCOR_voxel_corr( nvox , fim_nref ) ;
         if( pc_ref[ivec] == NULL || pc_vc[ivec] == NULL ){
            fprintf(stderr,"RTfim: can't malloc refs and corr!\a\n") ;
            exit(1) ;
         }
      }

      /*--- Make a new dataset to hold the output ---*/

      rtin->func_dset = new_dset = EDIT_empty_copy( dset_time ) ;
      if( new_dset == NULL ){
         fprintf(stderr,"RTfim: can't create empty dataset!\a\n") ;
         exit(1) ;
      }

      it = EDIT_dset_items( new_dset ,
                               ADN_prefix      , new_prefix ,
                               ADN_malloc_type , DATABLOCK_MEM_MALLOC ,
                               ADN_type        , ISHEAD(dset_time)
                                                 ? HEAD_FUNC_TYPE : GEN_FUNC_TYPE ,
                               ADN_func_type   , FUNC_COR_TYPE ,
                               ADN_nvals       , FUNC_nvals[FUNC_COR_TYPE] ,
                               ADN_datum_all   , MRI_short ,
                               ADN_ntt         , 0 ,
                            ADN_none ) ;

      if( it > 0 )
         fprintf(stderr,"RTfim: WARNING -- %d errors in dataset creation!\a\n",it) ;

      /** attach bricks to this dataset **/

      for( iv=0 ; iv < new_dset->dblk->nvals ; iv++ ){
         ptr = malloc( DSET_BRICK_BYTES(new_dset,iv) ) ;
         if( ptr == NULL ){
            fprintf(stderr,"RTfim: can't malloc dataset bricks!\a\n") ;
            exit(1) ;
         }
         mri_fix_data_pointer( ptr ,  DSET_BRICK(new_dset,iv) ) ;
      }

      DSET_lock( rtin->func_dset ) ; /* 20 Mar 1998 */

   }  /*---- end of initializing at first important time point ----*/

   /*---- do recursive update for this time point ----*/

   it   = mode ;  /* time index */
   nnow = 0 ;     /* number of refs used this time */

   if( it >= nx_ref ) return 1 ;  /* can't update without references! */

   for( ivec=0 ; ivec < ny_ref ; ivec++ ){           /* for each ideal time series */
      tsar = MRI_FLOAT_PTR(ref_ts) + (ivec*nx_ref) ;
      if( tsar[it] >= WAY_BIG ) continue ;           /* skip this time series */

      ref_vec[0] = 1.0 ;              /* we always supply orts */
      ref_vec[1] = (float) it ;       /* for mean and linear trend */

      if( internal_ort ){
         ref_vec[2] = tsar[it] ;                     /* ideal */
      } else {
         for( iv=0 ; iv < ny_ort ; iv++ )            /* any other orts? */
            ref_vec[iv+2] = (it < nx_ort) ? ortar[it+iv*nx_ort]
                                          : 0.0 ;

         ref_vec[ny_ort+2] = tsar[it] ;              /* ideal */
      }

      update_PCOR_references( ref_vec , pc_ref[ivec] ) ;  /* Choleski refs */

      /* load data from dataset into local float array vval */

      switch( dtyp ){
         case MRI_short:{
            short * dar = (short *) DSET_ARRAY(dset_time,it) ;
            for( iv=0 ; iv < nvox ; iv++ ) vval[iv] = (float) dar[indx[iv]] ;
         }
         break ;

         case MRI_float:{
            float * dar = (float *) DSET_ARRAY(dset_time,it) ;
            for( iv=0 ; iv < nvox ; iv++ ) vval[iv] = (float) dar[indx[iv]] ;
         }
         break ;

         case MRI_byte:{
            byte * dar = (byte *) DSET_ARRAY(dset_time,it) ;
            for( iv=0 ; iv < nvox ; iv++ ) vval[iv] = (float) dar[indx[iv]] ;
         }
         break ;
      }

      PCOR_update_float( vval , pc_ref[ivec] , pc_vc[ivec] ) ;  /* Choleski data */
      nnow++ ;

   }
   if( nnow > 0 ) nupdt++ ;  /* at least one ideal vector was used */

   return 1 ;
}
