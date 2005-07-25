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

static AIVVVV_imseq *psq_global ;
static XtAppContext AIVVV_appcontext ;

static NI_stream AIVVV_stream = (NI_stream)NULL ;
static char      AIVVV_strnam[64] ;

static void * AIVVV_imseq_popup( MRI_IMARR *, generic_func *, void * ) ;
static void   AIVVV_imseq_retitle( void * , char * ) ;
static XtPointer AIVVV_imseq_getim( int , int , XtPointer ) ;
static void AIVVV_imseq_send_CB( MCW_imseq * , XtPointer , ISQ_cbs * ) ;
static void AIVVV_imseq_addto( MRI_IMAGE *im ) ;  /* 25 Jul 2005 */
static Boolean AIVVV_workproc( XtPointer ) ;      /* 25 Jul 2005 */

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
   (void) AIVVV_imseq_popup( MAIN_imar , killer , NULL ) ;
   if( AIVVV_stream != (NI_stream)NULL )
     XtAppAddWorkProc( AIVVV_appcontext, AIVVV_workproc, NULL ) ;
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
      "Usage: aiv [-v] [-p xxxx ] image ...\n"
      "AFNI Image Viewer program.\n"
      "Shows the 2D images on the command line in an AFNI-like image viewer.\n"
      "Image formats are those supported by to3d:\n"
      " * various MRI formats\n"
      " * raw PPM or PGM\n"
      " * JPEG (if djpeg is in the path)\n"
      " * GIF, TIFF, BMP, and PNG (if netpbm is in the path)\n"
      "\n"
      "The '-v' option will make aiv print out the image filenames\n"
      "as it reads them - this can be a useful progress meter if\n"
      "the program starts up slowly.\n"
      "\n"
      "The '-p xxxx' option will make aiv listen to TCP/IP port 'xxxx'\n"
      "for incoming images in the NIML '<MRI_IMAGE ...>' format.  The\n"
      "port number must be between 1024 and 65535, inclusive.\n"
     ) ;
     exit(0) ;
   }

   /* verbose? */

   while( iarg < argc && argv[iarg][0] == '-' ){
     if( strncmp(argv[iarg],"-v",2) == 0 ){ verb=1; iarg++; continue; }
     if( strncmp(argv[iarg],"-p",2) == 0 ){
       int port = (int)strtol(argv[++iarg],NULL,10) ;
       if( port <= 1023 ){
         ERROR_message("Illegal value after -p") ;
       } else {
         sprintf(AIVVV_strnam,"tcp:x:%d",port) ;
         AIVVV_stream = NI_stream_open( AIVVV_strnam , "r" ) ;
         if( AIVVV_stream == (NI_stream)NULL ){
           ERROR_message("Can't listen to port %d!",port) ;
         } else {
           int nn ;
           nn = NI_stream_goodcheck(AIVVV_stream,66) ;
           if( nn > 0 ) INFO_message("Connected to port %d",port) ;
           else         INFO_message("Listening to port %d",port) ;
         }
       }
       iarg++ ; continue ;
     }
     ERROR_message("Unknown option: %s",argv[iarg]) ;
   }

   /* read images */

   MCW_file_expand( argc-iarg , argv+iarg , &gnim , &gname ) ;
   if( gnim == 0 && AIVVV_stream==(NI_stream)NULL )
     ERROR_exit("No filenames on command line?!") ;

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

   if( IMARR_COUNT(MAIN_imar) == 0 && AIVVV_stream==(NI_stream)NULL )
     ERROR_exit("NO IMAGES FOUND!?") ;
   if( verb ) fprintf(stderr," = ") ;
   if( IMARR_COUNT(MAIN_imar) == 1 )
     fprintf(stderr,"1 image") ;
   else if( IMARR_COUNT(MAIN_imar) > 1 )
     fprintf(stderr,"%d images\n",IMARR_COUNT(MAIN_imar)) ;

   MCW_free_expand( gnim , gname ) ;

   /* connect to X11 */

   shell = XtVaAppInitialize( &MAIN_app , "AFNI" , NULL , 0 ,
                              &argc , argv , FALLback , NULL ) ;

   if( shell == NULL )
     ERROR_exit("Can't initialize X11") ;

   AIVVV_appcontext = XtWidgetToApplicationContext(shell) ;

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

   if( imar == NULL ) return NULL ;

   ntot = IMARR_COUNT(imar) ;
   if( ntot == 0 ){
#define QQ_NXYZ 16
     static byte xxx[QQ_NXYZ*QQ_NXYZ] = {
       0,0,21,131,135,135,135,8,0,0,3,100,135,135,135,128,
       0,0,0,108,255,255,255,86,0,0,115,255,255,255,255,121,
       0,0,0,21,216,255,255,213,0,19,223,255,255,255,187,5,
       0,0,0,0,92,244,255,255,114,114,255,255,255,234,58,0,
       0,0,0,0,0,174,255,255,252,230,255,255,255,130,0,0,
       0,0,0,0,0,58,244,255,255,255,255,255,228,29,0,0,
       0,0,0,0,0,0,118,255,255,255,255,255,74,0,0,0,
       0,0,0,0,0,0,55,248,255,255,255,199,3,0,0,0,
       0,0,0,0,0,5,170,255,255,255,255,227,32,0,0,0,
       0,0,0,0,0,104,255,255,255,255,255,255,140,5,0,0,
       0,0,0,0,13,217,255,255,252,215,255,255,255,67,0,0,
       0,0,0,0,159,255,255,255,212,23,233,255,255,187,7,0,
       0,0,0,81,241,255,255,255,85,0,72,255,255,255,66,0,
       0,0,16,206,255,255,255,212,0,0,8,193,255,255,237,12,
       0,0,94,255,255,255,255,86,0,0,0,73,255,255,255,121,
       0,14,129,134,134,134,85,1,0,0,0,3,106,134,134,127 } ;
     byte *ar ; MRI_IMAGE *xim ;

     ar = (byte *)malloc(sizeof(byte)*QQ_NXYZ*QQ_NXYZ) ;
     memcpy(ar,xxx,sizeof(byte)*QQ_NXYZ*QQ_NXYZ) ;
     xim = mri_new_vol_empty( QQ_NXYZ,QQ_NXYZ,1 , MRI_byte ) ;
     mri_fix_data_pointer( ar , xim ) ;
     ADDTO_IMARR(imar,xim) ; ntot = 1 ;
   }

   psq = psq_global = (AIVVVV_imseq *) calloc(1,sizeof(AIVVVV_imseq)) ;
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

/*---------------------------------------------------------------------------*/

static void AIVVV_imseq_addto( MRI_IMAGE *im )
{
   int ntot ;
   AIVVVV_imseq *psq = psq_global ;

   if( im == NULL ) return ;

   ADDTO_IMARR(psq->imar,im) ;
   if( im->kind == MRI_rgb ) psq->rgb_count++ ;

   drive_MCW_imseq( psq->seq , isqDR_newseq , psq ) ;

   ntot = IMARR_COUNT(psq->imar) ;
   if( ntot == 1 )
     drive_MCW_imseq( psq->seq , isqDR_onoffwid , (XtPointer) isqDR_offwid ) ;
   else
     drive_MCW_imseq( psq->seq , isqDR_onoffwid , (XtPointer) isqDR_onwid ) ;

   drive_MCW_imseq( psq->seq , isqDR_reimage , (XtPointer)(ntot-1) ) ;
   return ;
}

/*---------------------------------------------------------------------------*/

static Boolean AIVVV_workproc( XtPointer fred )
{
   int nn ;
   NI_element *nel ;
   MRI_IMAGE *im ;

   nn = NI_stream_goodcheck(AIVVV_stream,3) ;
   if( nn < 0 ){
     NI_stream_closenow(AIVVV_stream) ;
     NI_sleep(9) ;
     AIVVV_stream = NI_stream_open( AIVVV_strnam , "r" ) ;
     return False ;
   }

   nn = NI_stream_hasinput(AIVVV_stream,9) ;
   if( nn <= 0 ) return False ;                /* no data */

   nel = (NI_element *)NI_read_element(AIVVV_stream,99) ;
   if( nel == NULL ) return False ;
   im = niml_to_mri( nel ) ;
   NI_free_element( nel ) ;
   AIVVV_imseq_addto( im ) ;
   return False ;
}
