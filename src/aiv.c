/*------------------------------------------------------------------------*/
/*--- AFNI Image Viewer - quicky adaptation from afni_plugin.c stuff. ----*/
/*------------------------------------------------------------------------*/

#include "mrilib.h"
#include "imseq.h"

/*------------------------------------------------------------------------*/

typedef struct {
   MCW_imseq *seq ;
   MRI_IMARR *imar ;
   int rgb_count ;
   generic_func *kill_func ;
   void *kill_data ;
} AIVVVV_imseq ;

static void * AIVVV_imseq_popup( MRI_IMARR *, generic_func *, void * ) ;
static void   AIVVV_imseq_retitle( void * , char * ) ;
static XtPointer AIVVV_imseq_getim( int , int , XtPointer ) ;
static void AIVVV_imseq_send_CB( MCW_imseq * , XtPointer , ISQ_cbs * ) ;

/*------------------------------------------------------------------------*/

static char *FALLback[] =
  {   "AFNI*fontList:              9x15bold=charset1"    ,
      "AFNI*pbar*fontList:         6x10=charset1"        ,
      "AFNI*imseq*fontList:        7x13=charset1"        ,
      "AFNI*background:            gray20"               ,
      "AFNI*menu*background:       gray10"               ,
      "AFNI*borderColor:           gray20"               ,
      "AFNI*foreground:            yellow"               ,
      "AFNI*borderWidth:           0"                    ,
      "AFNI*troughColor:           blue"                 ,
      "AFNI*XmLabel.translations:  #override<Btn2Down>:" , /* Motif 2.0 bug */
      "AFNI*help*background:       black"                ,
      "AFNI*help*foreground:       yellow"               ,
      "AFNI*help*helpborder:       False"                ,
      "AFNI*help*waitPeriod:       1066"                 ,
      "AFNI*help*fontList:         9x15bold=charset1"    ,
      "AFNI*cluefont:              9x15bold"             ,
      "AFNI*help*cancelWaitPeriod: 50"                   ,
   NULL } ;

static MCW_DC       *MAIN_dc ;
static XtAppContext  MAIN_app ;
static MRI_IMARR    *MAIN_imar ;

#define DEFAULT_NCOLOVR 20

static char *INIT_colovr[DEFAULT_NCOLOVR] = {
   "#ffff00" , "#ffcc00"   , "#ff9900"  , "#ff6900" , "#ff4400" , "#ff0000" ,
   "#0000ff" , "#0044ff"   , "#0069ff"  , "#0099ff" , "#00ccff" , "#00ffff" ,
   "green"   , "limegreen" , "violet"   , "hotpink" ,
   "white"   , "#dddddd"   , "#bbbbbb"  , "black"
} ;

static char *INIT_labovr[DEFAULT_NCOLOVR] = {
   "yellow" , "yell-oran" , "oran-yell" , "orange"   , "oran-red" , "red"   ,
   "dk-blue", "blue"      , "lt-blue1"  , "lt-blue2" , "blue-cyan", "cyan"  ,
   "green"  , "limegreen" , "violet"    , "hotpink"  ,
   "white"  , "gry-dd"    , "gry-bb"    , "black"
} ;

/*------------------------------------------------------------------------*/

static void killer( void *pt ){ exit(0); }
static void AFNI_handler(char *msg){ return ; }

/*------------------------------------------------------------------------*/

static void timeout_CB( XtPointer client_data , XtIntervalId *id )
{
   AIVVV_imseq_popup( MAIN_imar , killer , NULL ) ;
}

/*------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int ii , verb=0 , iarg=1 , jj ;
   MRI_IMAGE *im ;
   MRI_IMARR *qar ;
   Widget shell ;
   int gnim ; char **gname ;   /* 23 Dec 2002: glob filenames */

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: aiv [-v] image ...\n"
      "AFNI Image Viewer program.\n"
      "Shows the 2D images on the command line in an AFNI-like image viewer.\n"
      "Image formats are those supported by to3d:\n"
      " * various MRI formats\n"
      " * raw PPM or PGM\n"
      " * JPEG (if djpeg is in the path)\n"
      " * GIF, TIFF, BMP, and PNG (if netpbm is in the path)\n"
      "The '-v' option will make aiv print out the image filenames\n"
      "as it reads them - this can be a useful progress meter if\n"
      "the program starts up slowly.\n"
     ) ;
     exit(0) ;
   }

   /* verbose? */

   if( strncmp(argv[iarg],"-v",2) == 0 ){ verb=1; iarg++; }

   /* read images */

   MCW_file_expand( argc-iarg , argv+iarg , &gnim , &gname ) ;
   if( gnim == 0 ){
     fprintf(stderr,"** No filenames on command line?! **\n"); exit(1);
   }

   INIT_IMARR(MAIN_imar) ;

   for( ii=0 ; ii < gnim ; ii++ ){
     if( !THD_filename_ok(gname[ii]) ) continue ;  /* 23 Apr 2003 */
     if( verb ) fprintf(stderr,"+") ;
     qar = mri_read_file( gname[ii] ) ;  /* may have more than 1 image */
     if( qar == NULL || IMARR_COUNT(qar) < 1 ){
       fprintf(stderr,"\n** Can't read file %s - skipping!",gname[ii]) ;
       continue ;
     } else if( verb ){
       fprintf(stderr,"%s",gname[ii]) ;
     }

     for( jj=0 ; jj < IMARR_COUNT(qar) ; jj++ ){
       im = IMARR_SUBIM(qar,jj) ;
       if( im != NULL ) ADDTO_IMARR( MAIN_imar , im ) ;
     }
     FREE_IMARR(qar) ;
   }

   if( IMARR_COUNT(MAIN_imar) == 0 ){
     fprintf(stderr,"\n** NO IMAGES FOUND!? **\n") ; exit(1) ;
   }
   if( verb ) fprintf(stderr," = ") ;
   if( IMARR_COUNT(MAIN_imar) == 1 )
     fprintf(stderr,"1 image") ;
   else
     fprintf(stderr,"%d images\n",IMARR_COUNT(MAIN_imar)) ;

   MCW_free_expand( gnim , gname ) ;


   /* connect to X11 */

   shell = XtVaAppInitialize( &MAIN_app , "AFNI" , NULL , 0 ,
                              &argc , argv , FALLback , NULL ) ;

   if( shell == NULL ){
     fprintf(stderr,"** Can't initialize X11 ***\n") ; exit(1) ;
   }

   (void) XtAppSetWarningHandler(MAIN_app,AFNI_handler) ;

   MAIN_dc = MCW_new_DC( shell, 128,
                         DEFAULT_NCOLOVR, INIT_colovr, INIT_labovr, 1.0, 0 ) ;


   srand48((long)time(NULL)) ;

   (void) XtAppAddTimeOut( MAIN_app, 234, timeout_CB, NULL ) ;
   XtAppMainLoop(MAIN_app) ;
}

/*-------------------------------------------------------------------------*/

static void * AIVVV_imseq_popup( MRI_IMARR *imar, generic_func *kfunc, void *kdata )
{
   int ntot , ii ;
   MRI_IMAGE *im , *cim ;
   AIVVVV_imseq *psq ;

   if( imar == NULL || IMARR_COUNT(imar) == 0 ) return NULL ;

   ntot = IMARR_COUNT(imar) ;

   psq = (AIVVVV_imseq *) calloc(1,sizeof(AIVVVV_imseq)) ;
   if( psq == NULL ) return NULL ;

   psq->imar = imar ;

   psq->kill_func = kfunc ;
   psq->kill_data = kdata ;
   psq->rgb_count = 0 ;

   psq->seq = open_MCW_imseq( MAIN_dc , AIVVV_imseq_getim , psq ) ;

   drive_MCW_imseq( psq->seq , isqDR_clearstat , NULL ) ;

   { ISQ_options opt ;       /* change some options from the defaults */

     ISQ_DEFAULT_OPT(opt) ;
     opt.save_one = False ;  /* change to Save:bkg */
     opt.save_pnm = False ;
     drive_MCW_imseq( psq->seq , isqDR_options      , (XtPointer) &opt ) ;
     drive_MCW_imseq( psq->seq , isqDR_periodicmont , (XtPointer) 0    ) ;
     drive_MCW_imseq( psq->seq , isqDR_penbbox      , (XtPointer) 0    ) ;
   }

   /* make it popup */

   drive_MCW_imseq( psq->seq , isqDR_realize, NULL ) ;
   drive_MCW_imseq( psq->seq , isqDR_title, "Images" ) ;

   if( ntot == 1 )
     drive_MCW_imseq( psq->seq , isqDR_onoffwid , (XtPointer) isqDR_offwid );
   else
     drive_MCW_imseq( psq->seq , isqDR_onoffwid , (XtPointer) isqDR_onwid  );

   drive_MCW_imseq( psq->seq , isqDR_display, (XtPointer)0 ) ;

   return (void *) psq ;
}

/*-----------------------------------------------------------------------*/

static void AIVVV_imseq_retitle( void *handle , char *title )
{
   AIVVVV_imseq *psq = (AIVVVV_imseq *) handle ;

   if( psq == NULL || psq->seq == NULL || title == NULL ) return ;
   drive_MCW_imseq( psq->seq , isqDR_title, title ) ;
   return ;
}

/*------------------------------------------------------------------
   Routine to provide data to the imseq.
   Just returns the control information, or the selected image.
--------------------------------------------------------------------*/

static XtPointer AIVVV_imseq_getim( int n, int type, XtPointer handle )
{
   AIVVVV_imseq *psq = (AIVVVV_imseq *) handle ;
   int ntot = 0 ;

   if( psq->imar != NULL ) ntot = IMARR_COUNT(psq->imar) ;
   if( ntot < 1 ) ntot = 1 ;

   /*--- send control info ---*/

   if( type == isqCR_getstatus ){
     MCW_imseq_status *stat = myXtNew( MCW_imseq_status ) ; /* will be freed */
                                                            /* when imseq is */
                                                            /* destroyed    */
     stat->num_total  = ntot ;
     stat->num_series = ntot ;
     stat->send_CB    = AIVVV_imseq_send_CB ;
     stat->parent     = NULL ;
     stat->aux        = NULL ;

     stat->transforms0D = NULL ;
     stat->transforms2D = NULL ;
     stat->slice_proj   = NULL ;

     return (XtPointer) stat ;
   }

   /*--- no overlay, never ---*/

   if( type == isqCR_getoverlay ) return NULL ;

   /*--- return a copy of an image
         (since the imseq will delete it when it is done) ---*/

   if( type == isqCR_getimage || type == isqCR_getqimage ){
     MRI_IMAGE *im = NULL , *rim ;

     if( psq->imar != NULL ){
       if( n < 0 ) n = 0 ; else if( n >= ntot ) n = ntot-1 ;
       rim = IMARR_SUBIMAGE(psq->imar,n) ;
       if( psq->rgb_count > 0 )
         im = mri_to_rgb( rim ) ;
       else
         im = mri_copy( rim ) ;
     }
     return (XtPointer) im ;
   }

   return NULL ; /* should not occur, but who knows? */
}

/*---------------------------------------------------------------------------
   Routine called when the imseq wants to send a message.
   In this case, all we need to handle is the destroy message,
   so that we can free some memory.
-----------------------------------------------------------------------------*/

static void AIVVV_imseq_send_CB( MCW_imseq *seq, XtPointer handle, ISQ_cbs *cbs )
{
   AIVVVV_imseq *psq = (AIVVVV_imseq *) handle ;

   switch( cbs->reason ){
     case isqCR_destroy:{
       myXtFree(psq->seq) ;
       DESTROY_IMARR( psq->imar ) ;

       if( psq->kill_func != NULL )
         psq->kill_func( psq->kill_data ) ;
         free(psq) ;
     }
     break ;

     case isqCR_newimage:{
       drive_MCW_imseq( psq->seq, isqDR_display, (XtPointer)cbs->nim );
     }
     break ;
   }
   return ;
}
