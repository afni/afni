/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "afni.h"

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   %%% Some of this file is "frivolities" and could be removed from AFNI %%%
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */

#if defined(NO_FRIVOLITIES) || !defined(ALLOW_PLUGINS)

void AFNI_splashdown (void){ return; }  /* for party poopers */
void AFNI_splashup   (void){ return; }
void AFNI_splashraise(void){ return; }
void AFNI_faceup     (void){ return; }
void AFNI_allsplash  (void){ return; }

/* moved the functions into "FRIVOLTIES" section  30 Jun 2005 [rickr] */
void AFNI_broutim_CB (Widget w, XtPointer cd, XtPointer cbs){ return; }
void AFNI_broutext_CB(Widget w, XtPointer cd, XtPointer cbs){ return; }
static int AFNI_find_todays_face(void){ return -1; }

#else  /*=============================== for party animals !!!!!!!!!!!!!!!!!!*/

#include "afni_splash.h"     /* contains the RLE image data */

static void * SPLASH_popup_image( void * , MRI_IMAGE * ) ;
static MRI_IMAGE * SPLASH_decode26( int , int , int , char ** ) ;
static MRI_IMAGE * SPLASH_decodexx( int , int , int , int ,
                                    byte *, byte *, byte * , char ** ) ;

static MRI_IMAGE *imspl = NULL ;
static void *handle = NULL ;

/*----------------------------------------------------------------------------*/
static int num_splashup = 0 ;  /* 14 Nov 2005 */
int AFNI_splash_isopen(void)
{
   PLUGIN_impopper *ppp = (PLUGIN_impopper *)handle ;
   return (ppp != NULL && ISQ_REALZ(ppp->seq)) ? num_splashup : 0 ;
}
/*----------------------------------------------------------------------------*/
int AFNI_gcd( int m , int n ){
  while( m > 0 ){
    if( n > m ){ int t=m; m=n; n=t; } /* swap */
    m -= n;
  }
  return n;
}
int AFNI_find_relprime_random( int n ) /* find one relatively prime to n */
{
   int dj , n5=n/5 ;
   if( n5 < 2 ) return 1 ;
   do{ dj = lrand48() % n + 1 ; } while( AFNI_gcd(n,dj) > 1 ) ;
   return dj ;
}
int AFNI_find_relprime_fixed( int n )  /* find number relatively prime to n */
{
   int dj , n5=n/5 ;
   if( n5 < 2 ) return 1 ;
   for( dj=n5 ; AFNI_gcd(n,dj) > 1 ; dj++ ) ; /*nada*/
   return dj ;
}
/*---------------------------------------------------------------------------*/

#define USE_FADING

#define USE_WRITING     /* 26 Feb 2001 */
static int do_write=2 ;

static int AFNI_find_jpegs( char *, char ***) ;  /* 26 Nov 2003 */
static int AFNI_find_todays_face(void) ;         /* 30 Mar 2005 */

/*----------------------------------------------------------------------------*/

void AFNI_splashraise(void) /* 25 Sep 2000: bring splash window to the top */
{
   PLUGIN_impopper *ppp = (PLUGIN_impopper *)handle ;

   if( ppp != NULL && ISQ_REALZ(ppp->seq) )
     XMapRaised( XtDisplay(ppp->seq->wtop) , XtWindow(ppp->seq->wtop) ) ;

   return ;
}

/*----------------------------------------------------------------------------*/

void AFNI_splashdown(void)
{
   PLUGIN_impopper *ppp = (PLUGIN_impopper *)handle ;

ENTRY("AFNI_splashdown") ;

   if( ppp != NULL ){
#ifdef USE_FADING
    if( ISQ_REALZ(ppp->seq) && imspl != NULL ){  /* fade gently away */
      if( AFNI_yesenv("AFNI_SPLASH_MELT") ){
        MCW_melt_widget( ppp->seq->wform ) ;
      } else {
        byte *bspl ; int ii , nv , kk ; double et ;
        bspl = mri_data_pointer(imspl) ;
        nv   = (imspl->pixel_size) * (imspl->nvox) ;
        et   = COX_clock_time() ;
        do_write = 0 ;
        for( kk=0 ; kk < 10 ; kk++ ){
          for( ii=0 ; ii < nv ; ii++ ) bspl[ii] = (15*bspl[ii]) >> 4 ;
          SPLASH_popup_image(handle,imspl) ;
          drive_MCW_imseq( ppp->seq , isqDR_reimage , (XtPointer) 0 ) ;
          if( COX_clock_time()-et > 1.234 ) break ;
        }
      }
    }
#endif
    SPLASH_popup_image(handle,NULL);
   }
   mri_free(imspl) ; imspl = NULL ;
   do_write = ( (lrand48() >> 8) % 3 == 0 ) ? 2 : 1 ;
   EXRETURN ;
}

/*----------------------------------------------------------------------------*/

static int    num_splash   =  0 ;   /* 26 Nov 2003 */
static int    first_splash = -1 ;
static int    first_face   = -1 ;   /* 21 Sep 2005 */
static char **fname_splash = NULL ;

static int    num_face     =  0 ;   /* 28 Mar 2003 */
static char **fname_face   = NULL ;

static int    index_splash = -1 ;   /* 13 Sep 2007 */
static int    delta_splash =  1 ;

/*----------------------------------------------------------------------------*/
/* Change the images in the splash window [13 Sep 2007] */

static int dont_change = 0 ;

static int AFNI_change_splash(void)
{
   MRI_IMAGE *imov ;
   PLUGIN_impopper *ppp = (PLUGIN_impopper *)handle ;

   if( imspl == NULL  || index_splash < 0 || dont_change         ||
       num_splash < 2 || handle == NULL   || !ISQ_REALZ(ppp->seq)  ) return 0;

   /* get new top splash image */

   index_splash = (index_splash+delta_splash+num_splash)%(num_splash) ;
   imov = mri_read_stuff( fname_splash[index_splash] ) ;

   if( imov != NULL ){ /* overlay new splash image */

     mri_overlay_2D( imspl , imov , 0,0 ) ; mri_free(imov) ;

     /* get new face and overlay it as well */

     if( num_face > 1 ){
       static int ff=-1 , df=1 ;
       int nxov,nyov,ee,dd ;
       if( ff >= 0 ) ff = (ff+df) % num_face ;
       else {        ff = (lrand48() >> 8) % num_face ;
                     df = AFNI_find_relprime_random(num_face) ;
       }
       imov = mri_read_stuff( fname_face[ff] ) ;
       if( imov != NULL ){
         nxov = imov->nx ; nyov = imov->ny ;
         if( nxov <= MAX_XOVER && nyov <= MAX_YOVER ){
           if( nxov < MAX_XOVER || nyov < MAX_YOVER ){
             MRI_IMAGE *qim ;
             dd = (MAX_XOVER-nxov)/2 ; ee = (MAX_YOVER-nyov)/2 ;
             qim = mri_zeropad_2D( dd , MAX_XOVER-nxov-dd ,
                                   ee , MAX_YOVER-nyov-ee , imov ) ;
             mri_free(imov) ; imov = qim ; nxov = qim->nx ; nyov = qim->ny ;
           }
           dd = IXOVER + (MAX_XOVER-nxov)/2 ;
           ee = JYOVER + (MAX_YOVER-nyov)/2 ;
           mri_overlay_2D( imspl, imov, dd,ee );
         }
         mri_free(imov);
       }
     }

     /* change images in splash window and redraw */

     SPLASH_popup_image(handle,imspl) ;
     drive_MCW_imseq( ppp->seq , isqDR_reimage , (XtPointer) 0 ) ;
     return 1 ;
   }
   return 0 ;
}

/*----------------------------------------------------------------------------*/
/* Change the images in the splash window every so often. [13 Sep 2007] */

#define SPLASH_TIMEOUT 1954  /* an ominous date in history */

static void AFNI_splash_timer_CB( XtPointer cd, XtIntervalId *id )
{
   Widget w=(Widget)cd ;

   if( dont_change || w == (Widget)NULL || AFNI_change_splash() == 0 ) return;
   (void) XtAppAddTimeOut( XtWidgetToApplicationContext(w) ,
                           SPLASH_TIMEOUT , AFNI_splash_timer_CB , w ) ;
   return ;
}

/*----------------------------------------------------------------------------*/

void AFNI_splashup(void)
{
   PLUGIN_impopper *ppp=NULL ;
   MRI_IMAGE *imov=NULL ;
   int    dd=0,ee=0 ;
   char   bb=0 ;
   byte *bspl=NULL ;
   int   sxx=0,syy=0 ;
   char *sen=NULL ;
   static int ncall=0 , nov=0 , dnov=0 , nm=-1 ;

ENTRY("AFNI_splashup") ;

   /*--- create splash image ---*/

   if( ! PLUTO_popup_open(handle) ){  /* not open at this time? */

      int nxov=0,nyov=0,ff=0 ;

      num_splashup++ ;  /* 14 Nov 2005 */

      /* get some fun stuff, first time in */

      if( ncall == 0 ){
        int np ;
        num_face   = AFNI_find_jpegs( "face_"   , &fname_face   ) ;
        num_splash = AFNI_find_jpegs( "splash_" , &fname_splash ) ;

        if( num_splash > 0 ){
          char *targ=(lrand48()%5 == 0) ? "bobkarl" : NULL ;
          if( targ != NULL ){
            for( np=0 ; np < num_splash ; np++ )
              if( strstr(fname_splash[np],targ) != NULL ) break ;
            if( np < num_splash ) first_splash = np ;
          } else {
            first_splash = (lrand48()>>8) % num_splash ;
          }
        }
      }

      /* create basic splash image */

      mri_free(imspl) ;
      imspl = SPLASH_decodexx( NX_blank, NY_blank, NLINE_blank, NC_blank,
                               RMAP_blank,GMAP_blank,BMAP_blank, BAR_blank ) ;

      if( ncall == 0 ){                         /* initialize random */
        nov  =    (lrand48() >> 8) % NOVER  ;   /* sub-image overlay */
        dnov = 2*((lrand48() >> 8) % 2) - 1 ;   /* index & direction */
      }

      /* Facial overlay: */
      /*  if have face jpegs, use them; else, use builtin faces [28 Mar 2003] */

      imov = NULL ; ff = 0 ;
      if( num_face > 0 ){                      /* external face_*.jpg files */
        static int *dold=NULL, ndold=0 ; int qq ;
        if( ndold == 0 && num_face > 1 ){
          ndold = num_face/2 ;
          dold  = (int *) malloc(sizeof(int)*ndold) ;
          for( qq=0 ; qq < ndold ; qq++ ) dold[qq] = -1 ;
        }
        dd = AFNI_find_todays_face() ;                 /* 30 Mar 2005: find */
        if( dd >= 0 ){ first_face=ff=-1; goto Have_dd; }    /* today's face */
      Retry_dd:
        dd = (lrand48() >> 8) % num_face ;              /* pick random file */
        if( num_face > 1 ){                       /* check if used recently */
          for( qq=0 ; qq < ndold && dold[qq] != dd ; qq++ ) ;       /* nada */
          if( qq < ndold ) goto Retry_dd ;                    /* was recent */
          for( qq=1 ; qq < ndold ; qq++ )        /* wasn't, so save in list */
            dold[qq-1] = dold[qq] ;
          dold[ndold-1] = dd ;
        }
      Have_dd:
        imov = mri_read_stuff( fname_face[dd] ) ;              /* read file */
        if( imov != NULL && (imov->nx > MAX_XOVER || imov->ny > MAX_YOVER) ){
          float xfac=MAX_XOVER/(float)(imov->nx),
                yfac=MAX_YOVER/(float)(imov->ny) ;    /* rescale if too big */
          int nxnew,nynew ; MRI_IMAGE *imq ;
          if( xfac > yfac ) xfac = yfac ;
          nxnew = (int)(xfac*imov->nx) ; nynew = (int)(xfac*imov->ny) ;
          imq = mri_resize( imov , nxnew,nynew ) ;          /* kind of slow */
          mri_free(imov); imov = imq;        /* replace with rescaled image */
          STATUS("loaded face") ;
        }
        if( ff == 0 && imov != NULL ){       /* ff = 2 for me, 1 for everyone else */
          ff = (strstr(fname_face[dd],"_rwcox") != NULL) ? 2 : 1 ;
        }
      }
      if( imov == NULL ){                  /* if didn't get face jpeg above */
        STATUS("didn't load face") ;
        nov  = (nov+dnov+NOVER) % NOVER ;
        imov = SPLASH_decode26( xover[nov], yover[nov], lover[nov], bover[nov] ) ;
      }
      nxov = imov->nx ; nyov = imov->ny ;          /* size of overlay image */
      dd = IXOVER + (MAX_XOVER-nxov)/2 ;          /* and location to put it */
      ee = JYOVER + (MAX_YOVER-nyov)/2 ;
      mri_overlay_2D( imspl, imov, dd,ee ); mri_free(imov);
      if( ff > 0 ){                             /* overlay title under face */
        imov = SPLASH_decodexx( NX_facetitle,NY_facetitle,NLINE_facetitle,
                                NC_facetitle,RMAP_facetitle,
                                RMAP_facetitle,RMAP_facetitle ,
                                BAR_facetitle ) ;
        if( ff == 2 ) mri_invert_inplace( imov ) ;           /* for me only */
        dd = IXOVER + (MAX_XOVER-imov->nx)/2 ; ee += nyov+1 ;
        mri_overlay_2D( imspl, imov, dd,ee ) ; mri_free(imov) ;
        STATUS("loaded face label") ;
      }

      /* possibly replace the splash image at the top [26 Nov 2003] */

      if( (ncall > 0 || first_splash >= 0) && num_splash > 0 &&
          (ncall <= num_splash  || ((lrand48() >> 8 ) % 7) != 0) ){

        if( index_splash < 0 || ncall < 2 ){
          index_splash = (first_splash >= 0) ? first_splash
                                             : (lrand48() >> 8) % num_splash ;
          delta_splash = 2*((lrand48() >> 8)%2)-1 ;  /* -1 or +1 */
          delta_splash *= AFNI_find_relprime_random(num_splash) ; /* 13 Sep 2007 */
        } else {
          index_splash = (index_splash+delta_splash+num_splash)%(num_splash) ;
        }
        imov = mri_read_stuff( fname_splash[index_splash] ) ;
        if( imov != NULL ){
#if 0
          reload_DC_colordef( GLOBAL_library.dc ) ;
          if( imov->nx != NX_TOPOVER || imov->ny != NY_TOPOVER ){
            MRI_IMAGE *imq ;
            imq = mri_resize( imov , NX_TOPOVER,NY_TOPOVER ) ; /* kind of slow */
            if( imq != NULL ){ mri_free(imov); imov = imq; }
          }
#endif
          STATUS("overlaying splash image") ;
          mri_overlay_2D( imspl , imov , 0,0 ) ; mri_free(imov) ;
        }
        if( index_splash == first_splash && first_face >= 0 ){   /* 21 Sep 2005 */
          imov = mri_read_stuff( fname_face[first_face] ) ;
          if( imov != NULL ){
            nxov = imov->nx ; nyov = imov->ny ;
            dd = IXOVER + (MAX_XOVER-nxov)/2 ;
            ee = JYOVER + (MAX_YOVER-nyov)/2 ;
            STATUS("re-overlaying first_face") ;
            mri_overlay_2D( imspl, imov, dd,ee ); mri_free(imov);
          }
        }

      } /* end of replacing splash image */

      /*-- show the image at last! --*/

      handle = SPLASH_popup_image( handle, imspl ) ;

      /* modify image display properties */

      ppp = (PLUGIN_impopper *)handle ;

      if( ncall==-1){ dd = MWM_DECOR_BORDER ;
                      ee = 0 ;
      } else        { dd = MWM_DECOR_BORDER | MWM_DECOR_TITLE | MWM_DECOR_MENU ;
                      ee = MWM_FUNC_MOVE | MWM_FUNC_CLOSE ;
      }

      /* 21 Sep 2000 -- allow user to control splash position */

      sxx = (GLOBAL_library.dc->width-4*NX_blank)/2 ; if( sxx < 0 ) sxx = 0 ;
      syy = 99 ;
      sen = getenv("AFNI_SPLASH_XY") ;
      if( sen != NULL ){
        int n,x,y ;
        n = sscanf(sen,"%d:%d",&x,&y) ;
        if( n == 2 && x >= 0 && x < GLOBAL_library.dc->width &&
                      y >= 0 && y < GLOBAL_library.dc->height  ){

          sxx = x ; syy = y ;
        }
      }

      XtVaSetValues( ppp->seq->wtop ,
                       XmNx , sxx ,
                       XmNy , syy ,
                       XmNmwmDecorations , dd ,
                       XmNmwmFunctions   , ee ,
                  /**  XmNoverrideRedirect , True , **/
                     NULL ) ;

      /* actually popup image display */

      drive_MCW_imseq( ppp->seq , isqDR_realize   , NULL                    );
      drive_MCW_imseq( ppp->seq , isqDR_onoffwid  , (XtPointer)isqDR_offwid );
      drive_MCW_imseq( ppp->seq , isqDR_clearstat , NULL                    );

      NORMAL_cursorize( ppp->seq->wimage ) ; /* 07 Dec 2001 */

      if( ncall > 0 ){
        drive_MCW_imseq( ppp->seq , isqDR_title , (XtPointer) "AFNI!" ) ;
        drive_MCW_imseq( ppp->seq , isqDR_reimage , (XtPointer)0 ) ;
      }

      if( ncall > 0 && !AFNI_noenv("AFNI_SPLASH_ANIMATE") ){
        Widget w = ppp->seq->wtop ;
        (void) XtAppAddTimeOut( XtWidgetToApplicationContext(w) ,
                                SPLASH_TIMEOUT , AFNI_splash_timer_CB , w ) ;
      }

   /*--- destroy splash image ---*/

   } else {
     ppp = (PLUGIN_impopper *)handle ;

     /* bring splash window to the top */

     if( ppp != NULL && ISQ_REALZ(ppp->seq) )
       XMapRaised( XtDisplay(ppp->seq->wtop) , XtWindow(ppp->seq->wtop) ) ;

     AFNI_splashdown() ;  /* off with their heads */
   }

   ncall++ ; EXRETURN ;
}

/*------------------------------------------------------------------
   The following is adapted from PLUGIN_imseq_getim (afni_plugin.c)
   -- 26 Feb 2001 -- RWCox
--------------------------------------------------------------------*/

static XtPointer SPLASH_imseq_getim( int n, int type, XtPointer handle )
{
   PLUGIN_impopper *imp = (PLUGIN_impopper *) handle ;

ENTRY("SPLASH_imseq_getim") ;

   if( imp == NULL ) RETURN(NULL) ;  /* bad */

   if( type == isqCR_destroy ){
     mri_free(imspl) ; imspl = NULL ; dont_change = 1 ;
   }

   /*--- control info ---*/

   if( type == isqCR_getstatus ){
      MCW_imseq_status *stat = myXtNew( MCW_imseq_status ) ;
      stat->num_total  = 1 ;
      stat->num_series = 1 ;
      stat->send_CB    = PLUGIN_seq_send_CB ;
      stat->parent     = (XtPointer)imp  ;
      stat->aux        = NULL ;

      stat->transforms0D = NULL ;  /* 31 Jan 2002: remove all functions */
      stat->transforms2D = NULL ;
      stat->slice_proj   = NULL ;

      RETURN((XtPointer) stat) ;
   }

   /*--- no overlay image in this usage ---*/

   if( type == isqCR_getoverlay ) RETURN(NULL) ;

   /*--- return a copy of the image
         (since the imseq will delete it when it is done) ---*/

   if( type == isqCR_getimage || type == isqCR_getqimage ){
      MRI_IMAGE *im = NULL ;
#ifndef USE_WRITING
      if( imp->im != NULL ) im = mri_copy( imp->im ) ;
#else
      if( imp->im != NULL ){
        if( do_write ) im = mri_zeropad_2D( 0,0,0,50 , imp->im ) ; /* 26 Feb 2001 */
        else           im = mri_copy( imp->im ) ;
      }
#endif
      RETURN((XtPointer) im) ;
   }

#ifdef USE_WRITING
   /*--- 26 Feb 2001: a line plot (for some more fun!)---*/

   if( do_write && type == isqCR_getmemplot ){
      int ii ;
      ii = create_memplot_surely("SPLASH memplot",1.0) ;
      if( ii == 0 ){
         MEM_plotdata *mp = get_active_memplot() ;

         set_thick_memplot(0.003) ;    /* slightly thick lines */

         if( do_write == 2 || 1 ){
           char *sf = AFNI_get_date_trivia() ;
           int   nn = strlen(sf) , ss=28 ; float hh=0.061f ;
           if( nn > 37 ) ss = (int)(27*37.0/nn)+1 ;
           if( strncmp(sf,"Thanks ",7) != 0 ){
             set_color_memplot(1.0,1.0,0.7) ;           /* whitish */
             plotpak_pwritf( 0.51,0.089 , "Today is:"  , 30 , 0 , 0 ) ;
             hh = 0.033f ;
           }
           set_color_memplot(1.0,1.0,0.1) ;             /* yellow */
           plotpak_pwritf( 0.51,hh , sf , ss , 0 , 0 ) ;
         } else {
           char *sf = AFNI_get_friend() ;
           char *mf = strstr(sf," for ") ;
           int    nn = strlen(sf) ;
           set_color_memplot(1.0,1.0,0.5) ;         /* orangish */
           if( nn < 36 || mf == NULL ){
              plotpak_pwritf( 0.5,0.060 , sf , 28 , 0 , 0 ) ;
           } else {
              *mf = '\0' ;
              plotpak_pwritf( 0.5,0.089 , sf  , 28 , 0 , 0 ) ;
              plotpak_pwritf( 0.5,0.033 , mf+1, 28 , 0 , 0 ) ;
           }
         }
         set_thick_memplot(0.0) ;
         RETURN((XtPointer)mp) ;  /* will be deleted by imseq */
      }
   }
#endif

   RETURN(NULL) ; /* default action = return nothing */
}

/*-----------------------------------------------------------------------
  The following is adapted from PLUTO_popup_image() in afni_plugin.c
-------------------------------------------------------------------------*/

static void * SPLASH_popup_image( void *handle , MRI_IMAGE *im )
{
   PLUGIN_impopper *imp = (PLUGIN_impopper *) handle ;

ENTRY("SPLASH_popup_image") ;

   /*-- input image is NULL ==> popdown, if applicable --*/

   if( im == NULL ){
     if( imp != NULL ){
       STATUS("unrealizing splash window") ;
       drive_MCW_imseq( imp->seq , isqDR_unrealize , NULL ) ;
       dont_change = 1 ;
     }

     RETURN ((void *) imp) ;
   }

   /*-- input = no popper handle ==> create one --*/

   if( imp == NULL ){
     imp      = myXtNew(PLUGIN_impopper) ;
     imp->seq = NULL ; imp->im  = NULL ;
   }

   /*-- input = non-null image ==> replace image --*/

   STATUS("replacing splash image") ;
   mri_free( imp->im ) ;      /* toss old copy */
   imp->im = mri_copy( im ) ; /* make new copy */

   /*-- input = inactive popper handle ==> activate it --*/

   if( imp->seq == NULL ){
     STATUS("opening new splash window") ;
     imp->seq = open_MCW_imseq( GLOBAL_library.dc ,
                                SPLASH_imseq_getim , (XtPointer) imp ) ;
   } else {
     STATUS("realizing old splash window") ;
     drive_MCW_imseq( imp->seq , isqDR_realize , NULL ) ;
   }

   /*-- unlike PLUTO_popup_image, actual popup is left to caller --*/

   dont_change = 0 ;
   RETURN ((void *) imp) ;
}

/*--------------------------------------------------------------------------
  Decode the 26 data into an image
----------------------------------------------------------------------------*/

static byte map26[26] =
  {  30,  50,  70,  90, 106, 118, 130, 140, 146, 152, 158, 164, 170,
    176, 182, 190, 198, 206, 212, 218, 224, 230, 236, 242, 248, 254 } ;

static MRI_IMAGE * SPLASH_decode26( int nx, int ny , int nl , char **im26 )
{
   return SPLASH_decodexx( nx, ny, nl, 26,map26,map26,map26,im26 ) ;
}

/*--------------------------------------------------------------------------
  Decode the 'xx' data into an image (cf. program toxx.c to make the data).
----------------------------------------------------------------------------*/

#define MMAX 82                                               /* max num colors */
static char alpha[MMAX] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"        /* codes for colors */
                          "abcdefghijklmnopqrstuvwxyz"        /* [0] .. [MMAX-1] */
                          ",<.>/?;:'[{]}|=+-_)(*&^%$#@!`~" ;

static MRI_IMAGE * SPLASH_decodexx( int nx, int ny, int nl, int nmap,
                                    byte *rmap, byte *gmap, byte *bmap ,
                                    char **imxx )
{
   MRI_IMAGE *im ;
   byte *bim ;
   int ii,jj , cc,qq , dd,ee , kk ;
   char bb ;
   static int first=1 , ainv[256] ;

ENTRY("SPLASH_decodexx") ;

   if( nmap == 0 ){                /* defaults from old to26.c program */
     nmap = 26 ;
     rmap = bmap = gmap = map26 ;
   }

   if( nx < 3       || ny < 3       || nl < 3 ||
       rmap == NULL || gmap == NULL ||
       bmap == NULL || imxx == NULL             ) RETURN(NULL) ;

   if( first ){
     for( ii=0 ; ii < 256 ; ii++ ) ainv[ii] = -1 ;
     for( ii=0 ; ii < MMAX ; ii++ ){
       bb = alpha[ii] ; ainv[bb] = ii ;
     }
     first = 0 ;
   }

   im  = mri_new( nx , ny , MRI_rgb ) ;
   bim = MRI_RGB_PTR(im) ;

   /* decode the RLE image data into a real image array */

   cc = qq = 0 ;
   for( ii=0 ; ii < 3*im->nvox && qq < nl ; ){
     bb = imxx[qq][cc++] ; if( bb == '\0' ) break ;
     jj = ainv[bb] ;
     if( jj >= 0 ){
       bim[ii++] = rmap[jj]; bim[ii++] = gmap[jj]; bim[ii++] = bmap[jj];
     } else {
       dd = bb - '0' ;
       bb = imxx[qq][cc++] ; if( bb == '\0' ) break ;
       jj = ainv[bb] ;
       for( ee=0 ; ee < dd && ii < 3*im->nvox ; ee++ ){
         bim[ii++] = rmap[jj]; bim[ii++] = gmap[jj]; bim[ii++] = bmap[jj];
       }
     }
     if( imxx[qq][cc] == '\0' ){ cc = 0 ; qq++ ; }
   }

   RETURN(im) ;
}

/*----------------------------------------------------------------------------*/
/*! Find all files of form 'prefix*.jpg' and 'prefix*.JPG' files in the path. */

int AFNI_find_jpegs( char *prefix , char ***fname )  /* 26 Nov 2003 */
{
   char *epath , *elocal , *eee ;
   char edir[THD_MAX_NAME] , fdir[THD_MAX_NAME] , *ename , *udir ;
   int epos , ll , ii , id , nfile , nx,ny , num_file=0 ;
   char **ffile , **fflist=NULL ;

ENTRY("AFNI_find_jpegs") ;

   if( prefix == NULL || *prefix == '\0' || fname == NULL ) RETURN(-1) ;

   /*----- get path to search -----*/

                       epath = getenv("AFNI_PLUGINPATH") ;
   if( epath == NULL ) epath = getenv("AFNI_PLUGIN_PATH") ;
   if( epath == NULL ) epath = getenv("PATH") ;
   if( epath == NULL ) epath = getenv("HOME") ;
   if( epath == NULL ){ RETURN(-1) ; }

   /*----- copy path list into local memory -----*/

   ll = strlen(epath) ;
   elocal = AFMALL( char,  sizeof(char) * (ll+2) ) ;

   /*----- put a blank at the end -----*/

   strcpy( elocal , epath ) ; elocal[ll] = ' ' ; elocal[ll+1] = '\0' ;

   /*----- replace colons (if any) with blanks -----*/

   for( ii=0 ; ii < ll ; ii++ )
     if( elocal[ii] == ':' ) elocal[ii] = ' ' ;

   /*----- extract blank delimited strings from elocal;
           use as directory names to look for matching files -----*/

   ename = (char *) malloc( 2*THD_MAX_NAME+32 ) ;  /* string for wildcards */
   epos  = 0 ;                              /* scanning position in elocal */

   do{
     ii = sscanf( elocal+epos , "%s%n" , edir , &id ); /* next substring */
     if( ii < 1 ) break ;                              /* none -> done   */

     /** check if edir occurs earlier in elocal **/

     eee = strstr( elocal , edir ) ;
     if( eee != NULL && (eee-elocal) < epos ){ epos += id ; continue ; }

     epos += id ;                                 /* char after last scanned */

     ii = strlen(edir) ;                          /* make sure dirname has */
     if( edir[ii-1] != '/' ){                     /* a trailing '/' on it */
       edir[ii]  = '/' ; edir[ii+1] = '\0' ;
     }

     strcpy(fdir,edir) ; strcat(fdir,"funstuff") ;  /* 07 Oct 2011 */
     if( THD_is_directory(fdir) ){ strcat(fdir,"/"); udir = fdir; } else { udir = edir; }

     /* create wildcard for JPEG files in this directory */

     sprintf(ename,"%s%s*.jpg %s%s*.JPG" , udir,prefix,udir,prefix ) ;

     MCW_wildcards( ename , &nfile , &ffile ) ;  /* find matching files */
     if( nfile <= 0 ) continue ;                 /* no files found */

      /** add files we found to list **/

     fflist = (char **)realloc(fflist,sizeof(char *)*(num_file+nfile));
     for( ii=0 ; ii < nfile ; ii++ ){
       if( strstr(ffile[ii],"face_wildman.jpg")   != NULL ||  /* 20 May 2005 */
           strstr(ffile[ii],"face_akanevsky.jpg") != NULL   ) /* 09 Nov 2011 */
         remove(ffile[ii]) ;
       else
         fflist[num_file++] = strdup(ffile[ii]) ;

       if( udir == fdir ){         /* 07 Oct 2011 */
         char qnam[THD_MAX_NAME] ;
         strcpy(qnam,edir) ; strcat(qnam,THD_trailname(ffile[ii],0)) ;
         if( THD_is_file(qnam) ) remove(qnam) ;
       }
     }

     MCW_free_wildcards( nfile , ffile ) ;  /* toss the junk */

   } while( epos < ll ) ;  /* scan until 'epos' is after end of epath */

   free(elocal) ; free(ename) ;             /* toss more junk */

   if( num_file == 0 ) num_file = -1 ;      /* flag that nothing was found */
   *fname = fflist ;                        /* list of found files */
   RETURN(num_file) ;                       /* return number of files found */
}

/*---------------------------------------------------------------------------*/

static void *face_phan=NULL ;
void AFNI_facedown( void *kd ){ face_phan = NULL; }

#undef  NXY
#define NXY 128  /* expected size of face images; trim or pad, as needed */

#undef DO_DELAY

/*---------------------------------------------------------------------------*/

void AFNI_faceup(void)   /* 17 Dec 2004 */
{
   MRI_IMAGE *im , *fim ;
   int ii , nx,ny , nxdown,nxup , nydown,nyup ;
   int ctold,ctnew,mmss , ddss ;
   int jj , j0,dj=1 ;

ENTRY("AFNI_faceup") ;

   if( num_face <  0 ){ BEEPIT; WARNING_message("no faces!?"); EXRETURN; }
   if( num_face == 0 ){
     num_face = AFNI_find_jpegs( "face_" , &fname_face ) ;
     if( num_face <= 0 ){ BEEPIT; WARNING_message("no faces?!"); EXRETURN; }
   }
   if( face_phan != NULL ){
     PLUGIN_imseq *ph = (PLUGIN_imseq *)face_phan ;
     XMapRaised( XtDisplay(ph->seq->wtop) , XtWindow(ph->seq->wtop) ) ;
     EXRETURN ;
   }

   ctold = NI_clock_time() ;

   ddss = num_face + 16449/num_face ; if( ddss > 222 ) ddss = 222 ;

   if( num_face > 4 ) dj = AFNI_find_relprime_random(num_face) ;
   j0 = lrand48() % num_face ;

   for( ii=0 ; ii < num_face ; ii++ ){
     jj = (j0 + ii*dj) % num_face ;
     im = mri_read_stuff( fname_face[jj] ) ;
     if( im == NULL ) continue ;
     nx = im->nx ; ny = im->ny ;

     nxdown = (NXY-nx) / 2 ; nxup = NXY - nx - nxdown ;
     nydown = (NXY-ny) / 2 ; nyup = NXY - ny - nydown ;
     if( nxdown != 0 || nydown != 0 || nxup != 0 || nyup != 0 ){
       fim = mri_zeropad_2D( nxdown,nxup , nydown,nyup , im ) ;
       if( fim != NULL ){ mri_free(im) ; im = fim ; }
     }
     fim = mri_dup2D(2,im) ; mri_free(im) ;  /* double size for fun */
     if( face_phan == NULL ){
       int sxx,syy ; char *sen ; PLUGIN_imseq *ph ;

       face_phan = PLUTO_imseq_popim( fim,(generic_func *)AFNI_facedown,NULL );
       sxx = (GLOBAL_library.dc->width-9*NXY)/2 ; if( sxx < 1 ) sxx = 1 ;
       syy = 100 ;
       sen = getenv("AFNI_SPLASH_XY") ;
       if( sen != NULL ){
         int n,x,y ;
         n = sscanf(sen,"%d:%d",&x,&y) ;
         if( n == 2 && x >= 0 && x < GLOBAL_library.dc->width &&
                       y >= 0 && y < GLOBAL_library.dc->height  ){
            sxx = x ; syy = y ;
         }
       }
       ph = (PLUGIN_imseq *)face_phan ;
       XtVaSetValues( ph->seq->wtop , XmNx,sxx , XmNy,syy , NULL ) ;
       drive_MCW_imseq( ph->seq , isqDR_record_disable , (XtPointer)0 ) ;
       drive_MCW_imseq( ph->seq , isqDR_periodicmont   , (XtPointer)1 ) ;

     } else {
       PLUTO_imseq_addto( face_phan , fim ) ;
     }
     mri_free(fim) ;

#ifdef DO_DELAY
     ctnew = NI_clock_time() ;      /* show 1 image every ddss ms [27 Dec 2004] */
     mmss  = ddss - (ctnew-ctold) ;
     ctold = ctnew ; ddss-- ;
     NI_sleep(mmss) ;
#endif
   }
   if( face_phan != NULL ){
     PLUTO_imseq_retitle( face_phan , "Faces of AFNI" ) ;
     PLUTO_imseq_setim( face_phan , 0 ) ;
   } else {
     BEEPIT ;
   }

   EXRETURN ;
}

/*---------------------------------------------------------------------------*/

static void *splash_phan=NULL ;
void AFNI_allsplashdown( void *kd ){ splash_phan = NULL; }

void AFNI_allsplash(void)   /* 12 Sep 2007 */
{
   MRI_IMAGE *im , *fim ;
   int ii , nx,ny , nxdown,nxup , nydown,nyup ;
   int ctold,ctnew,mmss , ddss ;
   int jj , j0,dj=1 ;

ENTRY("AFNI_allsplash") ;

   if( num_splash <  0 ){ BEEPIT; WARNING_message("no splashes!?"); EXRETURN; }
   if( num_splash == 0 ){
     num_splash = AFNI_find_jpegs( "splash_" , &fname_splash ) ;
     if( num_splash <= 0 ){ BEEPIT; WARNING_message("no splashes?!"); EXRETURN; }
   }
   if( splash_phan != NULL ){
     PLUGIN_imseq *ph = (PLUGIN_imseq *)splash_phan ;
     XMapRaised( XtDisplay(ph->seq->wtop) , XtWindow(ph->seq->wtop) ) ;
     EXRETURN ;
   }

   ctold = NI_clock_time() ;

   ddss = num_splash + 16449/num_splash ; if( ddss > 222 ) ddss = 222 ;

   if( num_splash > 4 ){
     ii = num_splash / 2 ;
     do{ dj = 1 + lrand48() % ii ; } while( AFNI_gcd(num_splash,dj) > 1 ) ;
   }
   j0 = lrand48() % num_splash ;

   for( ii=0 ; ii < num_splash ; ii++ ){
     jj = (j0 + ii*dj) % num_splash ;
     im = mri_read_stuff( fname_splash[jj] ) ;
     if( im == NULL ) continue ;
     nx = im->nx ; ny = im->ny ;

     nxdown = (NX_TOPOVER-nx) / 2 ; nxup = NX_TOPOVER - nx - nxdown ;
     nydown = (NY_TOPOVER-ny) / 2 ; nyup = NY_TOPOVER - ny - nydown ;
     if( nxdown != 0 || nydown != 0 || nxup != 0 || nyup != 0 ){
       fim = mri_zeropad_2D( nxdown,nxup , nydown,nyup , im ) ;
       if( fim != NULL ){ mri_free(im) ; im = fim ; }
     }
     if( GLOBAL_library.dc->width >= 3*NX_TOPOVER ){  /* screen is big? */
       fim = mri_dup2D(2,im) ; mri_free(im) ;    /* double size for fun */
       im = mri_sharpen_rgb(0.27f,fim) ; mri_free(fim); fim = im ;
     } else {
       fim = im ;                                   /* small screen :-( */
     }
     if( splash_phan == NULL ){
       int sxx,syy ; char *sen ; PLUGIN_imseq *ph ;

       splash_phan = PLUTO_imseq_popim( fim,(generic_func *)AFNI_allsplashdown,NULL );
       sxx = (GLOBAL_library.dc->width-4*fim->nx)/2 ; if( sxx < 1 ) sxx = 1 ;
       syy = 99 ;
       sen = getenv("AFNI_SPLASH_XY") ;
       if( sen != NULL ){
         int n,x,y ;
         n = sscanf(sen,"%d:%d",&x,&y) ;
         if( n == 2 && x >= 0 && x < GLOBAL_library.dc->width &&
                       y >= 0 && y < GLOBAL_library.dc->height  ){
            sxx = x ; syy = y ;
            if( sxx > 100 ) sxx -= 100 ;
         }
       }
       ph = (PLUGIN_imseq *)splash_phan ;
       XtVaSetValues( ph->seq->wtop , XmNx,sxx , XmNy,syy , NULL ) ;
       drive_MCW_imseq( ph->seq , isqDR_record_disable , (XtPointer)0 ) ;
       drive_MCW_imseq( ph->seq , isqDR_periodicmont   , (XtPointer)1 ) ;

     } else {
       PLUTO_imseq_addto( splash_phan , fim ) ;
     }
     mri_free(fim) ;

#ifdef DO_DELAY
     ctnew = NI_clock_time() ;      /* show 1 image every ddss ms [27 Dec 2004] */
     mmss  = ddss - (ctnew-ctold) ;
     ctold = ctnew ; ddss-- ;
     NI_sleep(mmss) ;
#endif
   }
   if( splash_phan != NULL ){
     PLUTO_imseq_retitle( splash_phan , "Splashings of AFNI" ) ;
     PLUTO_imseq_setim( splash_phan , 0 ) ;
   } else {
     BEEPIT ;
   }

   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
#include <time.h>
#define JAN  1
#define FEB  2
#define MAR  3
#define APR  4
#define MAY  5
#define JUN  6
#define JUL  7
#define AUG  8
#define SEP  9
#define OCT 10
#define NOV 11
#define DEC 12

typedef struct { int mon,day; char *label; } mday ;
#define NTMAX 9

static mday facials[] = {
 {MAR,30,"face_vincent" } ,
 {FEB,12,"face_lincoln" } ,
 {JAN, 3,"face_tolkien" } ,
 {MAR,14,"face_einstein"} ,
 {APR,27,"face_grant"   } ,
 {JUL,22,"face_rbirn"   } ,
 {SEP, 7,"face_rwcox"   } ,
 {OCT,17,"face_mmk"     } ,
 {NOV,17,"face_brodman" } ,
{0,0,NULL} } ;  /* last element = flag to stop searching */
/*---------------------------------------------------------------------------*/

static int AFNI_find_todays_face(void)
{
   time_t tt ;
   struct tm *lt ;
   int ii , ntar , dd , tar[NTMAX] ;
   static int iold=-1 ;
   char *flab ;

   if( num_face <= 0 || fname_face == NULL ) return -1 ;  /* bad */
   if( num_face == 1 )                       return  0 ;  /* duh */

   /* find if this day is in the 'facials' list */

   tt = time(NULL) ;         /* seconds since 01 Jan 1970 */
   lt = localtime( &tt ) ;   /* break into pieces */
   for( ii=0 ; facials[ii].day > 0 ; ii++ )
     if( facials[ii].mon == lt->tm_mon+1 && facials[ii].day == lt->tm_mday ) break ;
   if( facials[ii].day <= 0 ) return -1 ;  /* today is not special */

   /* OK, find face names that match */

   flab = facials[ii].label ;

   for( ntar=dd=0 ; ntar < NTMAX && dd < num_face ; dd++ )
     if( strstr(fname_face[dd],flab) != NULL ) tar[ntar++] = dd ;

   if( ntar == 0 ) return -1 ;
   if( ntar == 1 ) return tar[0] ;
   ii = (lrand48()>>8) % ntar ;
   if( ii == iold ) ii = (ii+1)%ntar ;
   iold = ii ; return tar[ii] ;
}

/*---------------------------------------------------------------------------*/

#include "afni_broutim.h"

void AFNI_broutim_CB( Widget w, XtPointer cd, XtPointer cbs ) /* 06 Jun 2005 */
{
   MRI_IMAGE *imbr ;
   imbr = SPLASH_decodexx( NX_broutim,NY_broutim,NLINE_broutim,NC_broutim,
                           RMAP_broutim,GMAP_broutim,BMAP_broutim,BAR_broutim);
   (void) PLUTO_popup_image( NULL , imbr ) ;
   mri_free(imbr) ; return ;
}

/*---------------------------------------------------------------------------*/

#include "afni_broutext.h"

void AFNI_broutext_CB( Widget w, XtPointer cd, XtPointer cbs ) /* 21 Dec 2005 */
{
   char *inf=NULL ; int ii ;
   Three_D_View *im3d = (Three_D_View *)cd ;

   if( IM3D_OPEN(im3d) ){
     for( ii=0 ; afni_broutext[ii] != NULL ; ii++ )
       inf = THD_zzprintf( inf , "%s" , afni_broutext[ii] ) ;
     (void) new_MCW_textwin( im3d->vwid->imag->topper, inf, TEXT_READONLY ) ;
     free(inf) ;
   }
   return ;
}

#endif /* NO_FRIVOLITIES */

/*---------------------------------------------------------------------------*/

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   %%%%% The stuff below here is required (i.e., must not be removed)! %%%%%
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */

/*---------------------------------------------------------------------------*/

#define NLBUF 4096
static char *linbuf ;  /* must be malloc()-ed before use */

static int get_linbuf( char *str )
{
   int ii=0 , jj ;

   if( str == NULL || str[0] == '\0' ){ linbuf[0] = '\0'; return 0; }

   /* skip whitespace to find start of new line */

 SKIP_WHITESPACE:
   for( ; str[ii] != '\0' && isspace(str[ii]) ; ii++ ) ; /* nada */

   if( str[ii] == '\0' ){ linbuf[0] = '\0'; return ii; } /* at end of input */

   if( str[ii] == '!' || (str[ii]=='/' && str[ii+1]=='/') ){  /* skip comment */
      for( ; str[ii] != '\0' && str[ii] != '\n' ; ii++ ) ;    /* nada: skip to eol */
      goto SKIP_WHITESPACE ;                                  /* augh! a GOTO */
   }

   /* copy into linbuf */

   for( jj=0 ; jj < NLBUF      &&
               str[ii] != '\0' &&
               str[ii] != '\n' &&
               str[ii] != '!'  &&
               !(str[ii]=='/' && str[ii+1]=='/')
             ; ii++ , jj++                       ) linbuf[jj] = str[ii] ;

   linbuf[jj] = '\0' ; return ii ;
}

/*---------------------------------------------------------------------------*/

void AFNI_decode_geom( char *geom , int *ww, int *hh , int *xx, int *yy )
{
   int has_x , has_plus ;

   *ww = *hh = *xx = *yy = -1 ;
   if( geom == NULL || geom[0] == '\0' ) return ;

   has_x    = strstr(geom,"x") != NULL ;
   has_plus = strstr(geom,"+") != NULL ;

   if( has_x && has_plus )
      sscanf(geom,"%dx%d+%d+%d",ww,hh,xx,yy) ;
   else if( has_x )
      sscanf(geom,"%dx%d",ww,hh) ;
   else if( has_plus )
      sscanf(geom,"+%d+%d",xx,yy) ;

   return ;
}

/*===========================================================================
  This routine, which is NOT a frivolity, creates the layout of new windows
  at the start of the program - 23 Sep 2000
=============================================================================*/

#define ISTARRED(s) ( (s)[0]=='*' && (s)[1]=='*' && (s)[2]=='*' )

#define NGMAX 32

#define ACTIVATE_MASK  (1)
#define NULL_MASK      (0)

#define NWBUF 128

void AFNI_startup_layout_CB( XtPointer client_data , XtIntervalId *id )
{
   char *fname = (char *) client_data ;
   int   nbuf , ii , goslow ;
   char *fbuf=NULL , *fptr ;
   char lword[NWBUF] ;

   int  controller_mask[MAX_CONTROLLERS]           ;
   char controller_geom[MAX_CONTROLLERS][NGMAX]    ;

   int  image_mask     [MAX_CONTROLLERS][3]        ;
   char image_geom     [MAX_CONTROLLERS][3][NGMAX] ;
   float image_ifrac   [MAX_CONTROLLERS][3]        ;
   char image_mont     [MAX_CONTROLLERS][3][NGMAX] ;

   int  graph_mask     [MAX_CONTROLLERS][3]        ;
   char graph_geom     [MAX_CONTROLLERS][3][NGMAX] ;
   int  graph_matrix   [MAX_CONTROLLERS][3]        ;
   int  graph_pinnum   [MAX_CONTROLLERS][3]        ;

   int cc,ww , gww,ghh,gxx,gyy ;
   char *e_asp ;
   int   e_turnoff=0 ;

   int  * plugin_cont = NULL ;
   char **plugin_geom = NULL ;
   int ipl ;
   char def_layout[]={"\n"
                      " ***LAYOUT\n"
                      "  A geom=+0+44\n"
                     };
   Three_D_View *im3d         = GLOBAL_library.controllers[0] ; /* already open */

#ifdef ALLOW_PLUGINS
   int      npbut              = im3d->vwid->nplugbut ;      /* how many plugins */
   char **  pluglab            = im3d->vwid->pluglab ;       /* their labels     */
   PLUGIN_interface ** plugint = im3d->vwid->plugint ;       /* their interfaces */
#endif

ENTRY("AFNI_startup_layout_CB") ;

   if( fname == NULL || fname[0] == '\0' ){ AFNI_splashdown(); EXRETURN; }

   /* read layout file */
   if( strcmp(fname,"GIMME_SOMETHING") != 0 ){
      fbuf = AFNI_suck_file(fname);
   } else if ( ALLOW_realtime ) {
      AFNI_splashdown(); EXRETURN;  /* no default in RT   4 Jan 2011 [rickr] */
   } else {    /* ZSS Dec 2010 ++ RWC Apr 2016 */
      int xxx = 3 ;
      fbuf = (char *)malloc(strlen(def_layout)+1024);
      strcpy(fbuf, def_layout);
      if( !AFNI_noenv("AFNI_OPEN_AXIAL") ){
        sprintf(fbuf+strlen(fbuf),
               "  A.axialimage geom=+%d+455 ifrac=0.8\n" , xxx ) ;
        xxx += 288 ;
      }
      if( !AFNI_noenv("AFNI_OPEN_SAGITTAL") ){
        sprintf(fbuf+strlen(fbuf),
                "  A.sagittalimage geom=+%d+455 ifrac=0.8\n" , xxx ) ;
        xxx += 333 ;
      }
      if( !AFNI_noenv("AFNI_OPEN_CORONAL") ){
        sprintf(fbuf+strlen(fbuf),
                "  A.coronalimage geom=+%d+455 ifrac=0.8\n",xxx) ;
        xxx += 288 ;
      }
      strcat(fbuf,"\n") ;
   }
   if( fbuf == NULL ){ AFNI_splashdown(); EXRETURN; }
   nbuf = strlen(fbuf) ;         if( nbuf == 0    ){ AFNI_splashdown(); EXRETURN; }

   fptr = fbuf ; linbuf = (char *) malloc(sizeof(char)*(NLBUF+1)) ;

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"Reading AFNI layout file = %s (%d bytes)",fname,nbuf) ;
  STATUS(str);}

   /*-- read lines until find "***LAYOUT" --*/

   do {
      ii = get_linbuf( fptr ) ; fptr += ii ;
      if( linbuf[0] == '\0' || fptr-fbuf >= nbuf ){  /* didn't find it */
STATUS("no ***LAYOUT found") ;
         fprintf(stderr,"\n*** LAYOUT not found in layout file %s\n",fname);
         free(fbuf); free(linbuf); AFNI_splashdown(); EXRETURN;
      }
   } while( strncmp(linbuf,"***LAYOUT",9) != 0 ) ;

   goslow = (strstr(linbuf,"slow") != NULL) ;  /* slow down the startup? */

   /*-- initialize controllers, images, and graphs to do nothing --*/

   for( cc=0 ; cc < MAX_CONTROLLERS ; cc++ ){
      controller_geom[cc][0] =
       image_geom[cc][0][0] = image_geom[cc][1][0] = image_geom[cc][2][0] =
       image_mont[cc][0][0] = image_mont[cc][1][0] = image_mont[cc][2][0] =
       graph_geom[cc][0][0] = graph_geom[cc][1][0] = graph_geom[cc][1][0] = '\0' ;

      controller_mask[cc] =
       image_mask[cc][0] = image_mask[cc][1] = image_mask[cc][2] =
       graph_mask[cc][0] = graph_mask[cc][1] = graph_mask[cc][2] = NULL_MASK ;

      image_ifrac[cc][0] = image_ifrac[cc][1] = image_ifrac[cc][2] = 0.0 ;

      graph_pinnum[cc][0] = graph_pinnum[cc][1] = graph_pinnum[cc][2] =
      graph_matrix[cc][0] = graph_matrix[cc][1] = graph_matrix[cc][2] = 0 ;
   }

   /*-- initialize list of plugins --*/

#ifdef ALLOW_PLUGINS
   if( npbut > 0 ){
      plugin_cont = (int *)   malloc(sizeof(int)   *npbut) ;
      plugin_geom = (char **) malloc(sizeof(char *)*npbut) ;
      for( ipl=0 ; ipl < npbut ; ipl++ ){
         plugin_cont[ipl] = -1 ;      /* controller index to start it with */
         plugin_geom[ipl] = NULL ;    /* geometry string to start it with */
      }
   }
#endif

   /*-- read and process further lines until a "***" line is found, or the end --*/

   do {
      ii = get_linbuf( fptr ) ; fptr += ii ;
      if( linbuf[0] == '\0' || ISTARRED(linbuf) ) break ;  /* end of layout commands */

      /*-- first character of command determines controller --*/

      cc = linbuf[0] - 'A' ;
      if( cc < 0 || cc >= MAX_CONTROLLERS ) continue ;     /* illegal value */

      controller_mask[cc] |= ACTIVATE_MASK ;               /* must activate this one */

      if( linbuf[1] == '\0' ) continue;                    /* nothing more to do */

      /*-- do controller options --*/

      if( isspace(linbuf[1]) ){

         int jj=1 , nn ;

         do {                                              /* scan option words */
            nn = 0 ; lword[0] = '\0' ;
            sscanf(linbuf+jj,"%s%n",lword,&nn) ;
            if( nn == 0 || lword[0] == '\0' ) break ;        /* no more options */
            jj += nn ;                                       /* position for next word */

            if( strncmp(lword,"geom=",5) == 0 ){             /* geom= */
               strcpy(controller_geom[cc],lword+5) ;
            } else {                                         /* ILLEGAL */
               fprintf(stderr,"\n** Illegal LAYOUT controller option: %s\n",lword) ;
               continue ;
            }
         } while(1) ;

      /*-- do sub-windows (images and graphs) --*/

      } else if( linbuf[1] == '.' && strncmp(linbuf+1,".plugin.",8) != 0 ){

         int jj=2 , nn ;

         nn = 0 ; lword[0] = '\0' ;                          /* get window name */
         sscanf(linbuf+jj,"%s%n",lword,&nn) ;
         if( nn == 0 || lword[0] == '\0' ) continue ;        /* not present -> goto next line */
         jj += nn ;

              if( strncmp(lword,"axial"   ,5) == 0 ) ww = 0 ; /* get window orientation */
         else if( strncmp(lword,"sagittal",8) == 0 ) ww = 1 ;
         else if( strncmp(lword,"coronal" ,7) == 0 ) ww = 2 ;
         else {
            fprintf(stderr,"\n** Illegal LAYOUT sub-window line: %s\n",linbuf) ;
            continue ;
         }

         /*-- images --*/

         if( strstr(lword,"image") != NULL ){

            image_mask[cc][ww] |= ACTIVATE_MASK ;              /* mark to open */

            do {                                               /* scan option words */
               nn = 0 ; lword[0] = '\0' ;
               sscanf(linbuf+jj,"%s%n",lword,&nn) ;
               if( nn == 0 || lword[0] == '\0' ) break ;           /* no more options */
               jj += nn ;

               if( strncmp(lword,"geom=",5) == 0 ){                /* geom= */
                  strcpy(image_geom[cc][ww],lword+5) ;
               } else if( strncmp(lword,"ifrac=",6) == 0 ){        /* ifrac= */
                  image_ifrac[cc][ww] = strtod( lword+6 , NULL ) ;
               } else if( strncmp(lword,"mont=",5) == 0 ){         /* mont= */
                  strcpy(image_mont[cc][ww],lword+5) ;
               } else {
                  fprintf(stderr,"\n** Illegal LAYOUT image option: %s\n",lword) ;
                  continue ;
               }
            } while(1) ;

         /*-- graphs --*/

         } else if( strstr(lword,"graph") != NULL ){         /* a graph window */

            graph_mask[cc][ww] |= ACTIVATE_MASK ;              /* mark to open */

            do {                                               /* scan option words */
               nn = 0 ; lword[0] = '\0' ;
               sscanf(linbuf+jj,"%s%n",lword,&nn) ;
               if( nn == 0 || lword[0] == '\0' ) break ;           /* no more options */
               jj += nn ;

               if( strncmp(lword,"geom=",5) == 0 ){                /* geom= */
                  strcpy(graph_geom[cc][ww],lword+5) ;
               } else if( strncmp(lword,"matrix=",7) == 0 ){       /* matrix= */
                  graph_matrix[cc][ww] = (int) strtod( lword+7 , NULL ) ;
               } else if( strncmp(lword,"pinnum=",7) == 0 ){       /* pinnum= */
                  graph_pinnum[cc][ww] = (int) strtod( lword+7 , NULL ) ;
               } else {
                  fprintf(stderr,"\n** Illegal LAYOUT image option: %s\n",lword) ;
                  continue ;
               }
            } while(1) ;

         } else {                                            /* ILLEGAL */
            fprintf(stderr,"\n** Illegal LAYOUT line: %s\n",linbuf) ;
            continue ;
         }

      /*-- plugin windows --*/

      } else if( strncmp(linbuf+1,".plugin.",8) == 0 ){

#ifdef ALLOW_PLUGINS
         char *pname = linbuf+9 ;
         int pl,ll,qq , jj,nn ;

         /* check name after .plugin. to match a plugin
            (but skip blanks in the plugin's label)     */

         pl = strlen(pname) ;
         if( pl < 1 ){
            fprintf(stderr,"\n** LAYOUT: unknown plugin line: %s\n",linbuf) ;
            continue ;
         }

         for( ipl=0 ; ipl < npbut ; ipl++ ){
            for( ll=strlen(pluglab[ipl]) ;   /* truncate trailing blanks */
                 ll >= 0 && isspace(pluglab[ipl][ll]) ; ll-- ) ; /* nada */
            if( ll < 0 ) continue ;                        /* all blanks?! ERROR */
            if( pl < ll ) continue ;                       /* too short to match */
            for( qq=0 ; qq < ll ; qq++ )                   /* match each nonblank */
               if( !isspace(pluglab[ipl][qq]) && pname[qq]!=pluglab[ipl][qq] ) break ;
            if( qq == ll ) break ;  /* found a match */
         }
         if( ipl >= npbut ){
            fprintf(stderr,"\n** LAYOUT: unknown plugin line: %s\n",linbuf) ;

            if( strncmp(pname,"RT Options",10) == 0 ||
                strncmp(pname,"RT_Options",10) == 0   )
              fprintf(stderr,"**         [Realtime plugin is not active!]\n") ;

            continue ;
         }

         /* mark plugin to be turned on by this controller */

         if( plugin_cont[ipl] >= 0 )
            fprintf(stderr,"\n** LAYOUT: 2nd callout for this plugin: %s\n",linbuf) ;

         plugin_cont[ipl] = cc ;
         if( plugin_geom[ipl] != NULL ){  /* in case was set earlier */
            free(plugin_geom[ipl]) ; plugin_geom[ipl] = NULL ;
         }

         jj = 9+ll ;
         do {                                              /* scan option words */
            nn = 0 ; lword[0] = '\0' ;
            sscanf(linbuf+jj,"%s%n",lword,&nn) ;
            if( nn == 0 || lword[0] == '\0' ) break ;        /* no more options */
            jj += nn ;                                       /* position for next word */

            if( strncmp(lword,"geom=",5) == 0 ){             /* geom= */
               plugin_geom[ipl] = strdup(lword+5) ;
            } else {                                         /* ILLEGAL */
               fprintf(stderr,"\n** Illegal LAYOUT .plugin. option: %s\n",lword) ;
               continue ;
            }
         } while(1) ;
#endif

      /*-- Quien Sabe? --*/

      } else {                                             /* bad news city, Arizona */

         fprintf(stderr,"\n** Illegal LAYOUT line: %s\n",linbuf) ;
         continue ;
      }

   } while(1) ; /* end of loop over layout command lines */

   free(fbuf) ; free(linbuf) ;  /* toss the trash */

#if 0
   /*-- if any image geom commands are present,
        we might need to enforce the aspect ratio --*/

   e_asp = my_getenv("AFNI_ENFORCE_ASPECT") ;
   if( !YESSISH(e_asp) ){
     putenv("AFNI_ENFORCE_ASPECT=YES") ;
     e_turnoff = 1 ;
   }
#endif

   /*-- now do the commanded work --*/

   for( cc=0 ; cc < MAX_CONTROLLERS ; cc++ ){  /* loop over controllers */

      MCW_imseq   *isq ;
      MCW_grapher *gra ;
      int          singleton=0 ;

      /*-- determine if this controller is to be active --*/

      if( (controller_mask[cc] & ACTIVATE_MASK) == 0 ) continue ; /* skip */

      AFNI_make_controller( cc ) ;  /* open this controller (if not already open) */

      if( GLOBAL_library.controllers[cc] == NULL ) continue ;   /* ERROR */

      if(goslow || PRINT_TRACING) sleep(1);

      /* set location (ignore size part of geometry, if present) */

      if( controller_geom[cc][0] != '\0' ){
         AFNI_decode_geom( controller_geom[cc] , &gww,&ghh,&gxx,&gyy ) ;
         if( gxx >= 0 && gyy >= 0 )
            XtVaSetValues( GLOBAL_library.controllers[cc]->vwid->top_shell ,
                           XmNx , gxx , XmNy , gyy , NULL ) ;
      }

      AFNI_splashraise() ;

      if(goslow || PRINT_TRACING) sleep(1);

      /*-- loop over image windows for this controller --*/

      for( ww=0 ; ww < 3 ; ww++ ){

         /* determine if this image is to be activated */

         if( (image_mask[cc][ww] & ACTIVATE_MASK) == 0 ) continue ;  /* skip */

         /* simulate the button press */

         AFNI_view_xyz_CB( (ww == 0) ? GLOBAL_library.controllers[cc]->vwid->imag->image_xyz_pb
                          :(ww == 1) ? GLOBAL_library.controllers[cc]->vwid->imag->image_yzx_pb
                          :            GLOBAL_library.controllers[cc]->vwid->imag->image_zxy_pb ,
                          GLOBAL_library.controllers[cc] , NULL ) ;

         isq = (ww == 0) ? GLOBAL_library.controllers[cc]->s123     /* get the image */
              :(ww == 1) ? GLOBAL_library.controllers[cc]->s231     /* viewer struct */
              :            GLOBAL_library.controllers[cc]->s312 ;

         if( isq == NULL ) continue ;   /* ERROR */

         if(goslow || PRINT_TRACING) sleep(1);

         /* change the image fraction? */

         if( image_ifrac[cc][ww] >= FRAC_MIN && image_ifrac[cc][ww] <= 1.0 ){

            drive_MCW_imseq( isq , isqDR_setifrac , (XtPointer) &(image_ifrac[cc][ww]) ) ;

            if(goslow || PRINT_TRACING) sleep(1);
         }

         /* change the image montage layout? */

         if( image_mont[cc][ww][0] != '\0' ){  /* decode montage WxH[:spacing[:gap[:color]]] */
            int mww=-1 , mhh=-1 , msp=-1 , mgap=-1 , nn ;
            char mcol[128] = "\0" ;

            nn = sscanf( image_mont[cc][ww] , "%dx%d:%d:%d:%s" , &mww,&mhh,&msp,&mgap,mcol ) ;

            if( nn >= 2 && mww >= 1 && mww <= MONT_NMAX && mhh >= 1 && mhh <= MONT_NMAX ){
               int mp[5] ;
               mp[0] = mww ; mp[1] = mhh ; mp[2] = msp ; mp[3] = mgap ;
               mp[4] = DC_find_closest_overlay_color(GLOBAL_library.controllers[cc]->dc,mcol);
               drive_MCW_imseq( isq , isqDR_setmontage , (XtPointer) mp ) ;

               if( msp == 1 ) singleton++ ;

               if(goslow || PRINT_TRACING) sleep(1);
            }
         }

         /* change the image geometry? */

         if( image_geom[cc][ww][0] != '\0' ){
            AFNI_decode_geom( image_geom[cc][ww] , &gww,&ghh,&gxx,&gyy ) ;
            if( gxx >= 0 && gyy >= 0 )
               XtVaSetValues( isq->wtop , XmNx , gxx , XmNy , gyy , NULL ) ;
            if( gww > 0 && ghh > 0 )
               XtVaSetValues( isq->wtop , XmNwidth , gww , XmNheight , ghh , NULL ) ;

               if(goslow || PRINT_TRACING) sleep(1);
         }

         AFNI_splashraise() ;

      } /* end of loop over images */

      /* 11 Oct 2000: change crosshairs if any mont spacing=1 */

      if( singleton ){
         AV_assign_ival(
           GLOBAL_library.controllers[cc]->vwid->imag->crosshair_av ,
           AFNI_XHAIRS_SINGLE ) ;

         AFNI_crosshair_visible_CB(
           GLOBAL_library.controllers[cc]->vwid->imag->crosshair_av ,
           GLOBAL_library.controllers[cc] ) ;
      }

      /*-- loop over graphs --*/

      for( ww=0 ; ww < 3 ; ww++ ){

         /* is this graph to be opened? */

         if( (graph_mask[cc][ww] & ACTIVATE_MASK) == 0 ) continue ; /* skip */

         /* simulate the button press */

         AFNI_view_xyz_CB( (ww == 0) ? GLOBAL_library.controllers[cc]->vwid->imag->graph_xyz_pb
                          :(ww == 1) ? GLOBAL_library.controllers[cc]->vwid->imag->graph_yzx_pb
                          :            GLOBAL_library.controllers[cc]->vwid->imag->graph_zxy_pb ,
                          GLOBAL_library.controllers[cc] , NULL ) ;

         gra = (ww == 0) ? GLOBAL_library.controllers[cc]->g123    /* get the graph */
              :(ww == 1) ? GLOBAL_library.controllers[cc]->g231    /* viewer struct */
              :            GLOBAL_library.controllers[cc]->g312 ;

         if( gra == NULL ) continue ;   /* ERROR */

         if(goslow || PRINT_TRACING) sleep(1);

         /* change the graph matrix (i.e., how many sub-graphs)? */

         if( graph_matrix[cc][ww] > 0 ){
           drive_MCW_grapher( gra, graDR_setmatrix,
                             (XtPointer)ITOP(graph_matrix[cc][ww]) );
           if(goslow || PRINT_TRACING) sleep(1);
         }

         /* make the graph length pinned? */

         if( graph_pinnum[cc][ww] > 1 ){
           drive_MCW_grapher( gra, graDR_setpinnum,
                              (XtPointer)ITOP(graph_pinnum[cc][ww]) );
           if(goslow || PRINT_TRACING) sleep(1);
         }

         /* change the graph window geometry? */

         if( graph_geom[cc][ww][0] != '\0' ){
            AFNI_decode_geom( graph_geom[cc][ww] , &gww,&ghh,&gxx,&gyy ) ;
            if( gxx >= 0 && gyy >= 0 )
               XtVaSetValues( gra->fdw_graph , XmNx , gxx , XmNy , gyy , NULL ) ;
            if( gww > 0 && ghh > 0 )
               XtVaSetValues( gra->fdw_graph, XmNwidth, gww, XmNheight, ghh, NULL ) ;
            if(goslow || PRINT_TRACING) sleep(1);
         }

         AFNI_splashraise() ;

      } /* end of loop over graphs */

#if 0
      XmUpdateDisplay( im3d->vwid->top_shell ) ;
#endif

   } /* end of loop over controllers */

#ifdef ALLOW_PLUGINS
   /*-- now loop over plugins --*/

   for( ipl=0 ; ipl < npbut ; ipl++ ){

      cc = plugin_cont[ipl] ; if( cc < 0 ) continue ;

      if( GLOBAL_library.controllers[cc] == NULL ) continue ;  /* ERROR */

      PLUG_startup_plugin_CB( GLOBAL_library.controllers[cc]->vwid->plugbut[ipl] ,
                              GLOBAL_library.controllers[cc]->vwid->plugint[ipl] ,
                              NULL ) ;

      if(goslow || PRINT_TRACING) sleep(1);

      /* 13 Nov 2001: don't check interface widgets for customized plugins */

      if( GLOBAL_library.controllers[cc]->vwid->plugint[ipl]->call_method == PLUGIN_CALL_IMMEDIATELY )
        continue ;

      /* check interface widgets to see if plugin opened */

      if( GLOBAL_library.controllers[cc]->vwid->plugint[ipl]->wid        == NULL ||
          GLOBAL_library.controllers[cc]->vwid->plugint[ipl]->wid->shell == NULL   ){

         fprintf(stderr,"\n** LAYOUT: couldn't start plugin %s\n",pluglab[ipl]) ;
         continue ;
      }

      /* set the location of the interface widgets */

      if( plugin_geom[ipl] != NULL ){
            AFNI_decode_geom( plugin_geom[ipl] , &gww,&ghh,&gxx,&gyy ) ;
            if( gxx >= 0 && gyy >= 0 ){
               XtVaSetValues(
                  GLOBAL_library.controllers[cc]->vwid->plugint[ipl]->wid->shell,
                  XmNx , gxx , XmNy , gyy , NULL ) ;
               if(goslow || PRINT_TRACING) sleep(1);
            }
      }

      AFNI_splashraise() ;

   } /* end of loop over plugins */
#endif

   /*--- done ---*/

#ifdef ALLOW_PLUGINS
   if( npbut > 0 ){
      for( ipl=0 ; ipl < npbut ; ipl++ )
         if( plugin_geom[ipl] != NULL ) free(plugin_geom[ipl]) ;
      free(plugin_cont) ; free(plugin_geom) ;
   }
#endif

#if 0
   if( e_turnoff ) putenv("AFNI_ENFORCE_ASPECT=NO") ;
#endif

   AFNI_splashdown() ; EXRETURN ;
}

/*---------------------------------------------------------------------------
   Save the current layout to a file
-----------------------------------------------------------------------------*/

void AFNI_save_layout_CB( Widget w , XtPointer cd , XtPointer cbs )
{
   Three_D_View *im3d = (Three_D_View *) cd ;

ENTRY("AFNI_save_layout_CB") ;

   if( !IM3D_OPEN(im3d) ) EXRETURN ;

   MCW_choose_string( im3d->vwid->picture ,
                      "Layout filename [blank => .afni.startup_script]:" ,
                      NULL , AFNI_finalsave_layout_CB , cd ) ;
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/

void AFNI_finalsave_layout_CB( Widget w , XtPointer cd , MCW_choose_cbs *cbs )
{
   Three_D_View *im3d = (Three_D_View *) cd ;
   int cc,ww , gww,ghh,gxx,gyy ;
   FILE *fp=NULL , *gp=NULL ;
   MCW_imseq   *isq ;
   MCW_grapher *gra ;
   float ifrac ;
   char mont[128] ;
   int matrix , pinnum ;

   char *abet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ;
   char *wnam[3] = { "axial" , "sagittal" , "coronal" } ;

   int ipl , qq , ll ;
   char *plab ;

   Three_D_View *qm3d         = GLOBAL_library.controllers[0]; /* exists now */

#ifdef ALLOW_PLUGINS
   int      npbut             = qm3d->vwid->nplugbut;    /* how many plugins */
   char **  pluglab           = qm3d->vwid->pluglab;     /* their labels     */
   PLUGIN_interface **plugint = qm3d->vwid->plugint;     /* their interfaces */
#endif

   MCW_DCOV     *ovc          = GLOBAL_library.dc->ovc ;   /* 22 Jan 2003 */
   Three_D_View *zm3d ;

ENTRY("AFNI_finalsave_layout_CB") ;

   if( strcmp(cbs->cval,".afnirc") == 0 ){ /* 12 Oct 2000 */
     BEEPIT; WARNING_message("Won't over-write .afnirc"); EXRETURN;
   }

   /*-- 23 Jan 2003: open layout file if name is OK, else don't use it --*/

   if( THD_filename_ok(cbs->cval) ){
     fp = fopen( cbs->cval , "w" ) ;
     if( fp == NULL ){ BEEPIT; WARNING_message("Can't open output file"); EXRETURN; }
   }
   if( fp != NULL && strstr(cbs->cval,"script") != NULL ){  /* 05 Dec 2007 */
     gp = fp ; fp = NULL ;                  /* write as a driver script */
   }
   if( fp != NULL ) fprintf(fp,"\n***LAYOUT\n") ;

   /*-- 22 Jan 2002: maybe write a startup script to do same things --*/

   if( fp == NULL && gp == NULL ) gp = fopen( ".afni.startup_script" , "w" ) ;

   if( gp != NULL ){  /* start with a comment */

     fprintf(gp,"// AFNI startup script, from Datamode->Misc->Save Layout\n") ;

     /* put in any extra overlay colors */

     for( qq=DEFAULT_NCOLOVR+1 ; qq < ovc->ncol_ov ; qq++ )
       fprintf(gp,"ADD_OVERLAY_COLOR %s %s\n",
               ovc->name_ov[qq] , ovc->label_ov[qq] ) ;
   } else {
     if( fp == NULL ){ BEEPIT; WARNING_message("Can't open output file"); EXRETURN; }  /* no fp and no gp == Error! */
   }

   /*----- loop over open controllers -----*/

   for( cc=0 ; cc < MAX_CONTROLLERS ; cc++ ){

      /*-- controller open? */

      zm3d = GLOBAL_library.controllers[cc] ;

      if( !IM3D_OPEN(zm3d) ) continue ; /* skip this controller */

      /* print controller info */

      MCW_widget_geom( zm3d->vwid->top_shell , NULL,NULL , &gxx,&gyy ) ;

      if( fp != NULL ) fprintf(fp,"  %c geom=+%d+%d\n" , abet[cc] , gxx,gyy ) ;

      /*-- 22 Jan 2003: parallel output for startup script --*/

      if( gp != NULL ){
        MCW_pbar *pbar = zm3d->vwid->func->inten_pbar ;

        fprintf(gp,"OPEN_WINDOW %c geom=+%d+%d\n" , abet[cc] , gxx,gyy ) ;

        if( XtIsManaged(zm3d->vwid->func->frame) )
          fprintf(gp,"OPEN_PANEL %c.Define_Overlay\n" , abet[cc] ) ;

        fprintf(gp,"SET_THRESHOLD %c.%04d %d\n" , abet[cc] ,
                    (int)(zm3d->vinfo->func_threshold/THR_factor) ,
                    (int)(log10(zm3d->vinfo->func_thresh_top)+.01) ) ;

        if( !pbar->bigmode ){
          fprintf(gp,"SET_PBAR_ALL %c.%c%d" , abet[cc] ,
                     (pbar->mode) ? '+' : '-' , pbar->num_panes ) ;
          for( qq=0 ; qq < pbar->num_panes ; qq++ )
            fprintf(gp," %s=%s",
                       AV_uformat_fval(pbar->pval[qq]) ,
                       ovc->label_ov[pbar->ov_index[qq]] ) ;
          fprintf(gp,"\n") ;
        } else {
          fprintf(gp,"SET_PBAR_ALL %c.%c%d %f %s\n" , abet[cc] ,
                     (pbar->mode) ? '+' : '-' , 99 ,
                     pbar->bigtop , PBAR_get_bigmap(pbar) ) ;
        }

        fprintf(gp,"SET_FUNC_VISIBLE %c.%c\n" , abet[cc] ,
                (zm3d->vinfo->func_visible) ? '+' : '-'   ) ;

        fprintf(gp,"SET_FUNC_RESAM %c.%s.%s\n" , abet[cc] ,
                RESAM_shortstr[zm3d->vinfo->func_resam_mode] ,
                RESAM_shortstr[zm3d->vinfo->thr_resam_mode]   ) ;

        if( zm3d->vinfo->use_autorange )
          fprintf(gp,"SET_FUNC_AUTORANGE %c.+\n" , abet[cc] ) ;
        else
          fprintf(gp,"SET_FUNC_RANGE %c.%f\n" , abet[cc] ,
                  zm3d->vwid->func->range_av->fval        ) ;
        
        #if 0 /* Do we need this here ? */
        if( zm3d->cont_perc_thr )  /* ZSS April 27 2012 */
          fprintf(gp,"SET_FUNC_PERCENTILE %c.+\n" , abet[cc] ) ;
        else
          fprintf(gp,"SET_FUNC_PERCENTILE %c.-\n" , abet[cc] ) ;
        #endif
         
        if( ISVALID_DSET(zm3d->anat_now) ){          /* 27 Dec 2006 */
          char *pp = DSET_PREFIX(zm3d->anat_now) ;
          if( pp == NULL || *pp == '\0' ) pp = DSET_IDCODE_STR(zm3d->anat_now);
          fprintf(gp,"SET_UNDERLAY %c.%s %d\n",
                  abet[cc], pp, zm3d->vinfo->anat_index ) ;
        }
        if( ISVALID_DSET(zm3d->fim_now) ){           /* 27 Dec 2006 */
          char *pp = DSET_PREFIX(zm3d->fim_now) ;
          if( pp == NULL || *pp == '\0' ) pp = DSET_IDCODE_STR(zm3d->fim_now);
          fprintf(gp,"SET_OVERLAY %c.%s %d %d\n",
                  abet[cc], pp, zm3d->vinfo->fim_index,zm3d->vinfo->thr_index );
        }
        fprintf(gp,"SET_DICOM_XYZ %c %f %f %f\n",    /* 27 Dec 2006 */
                abet[cc], zm3d->vinfo->xi, zm3d->vinfo->yj, zm3d->vinfo->zk );

      } /* end of startup script stuff */

      /*-- loop over image viewers in this controller --*/

      for( ww=0 ; ww < 3 ; ww++ ){

         isq = (ww == 0) ? zm3d->s123     /* get the image */
              :(ww == 1) ? zm3d->s231     /* viewer struct */
              :            zm3d->s312 ;

         if( isq == NULL ) continue ;   /* skip */

         /* get and print image viewer info */

         MCW_widget_geom( isq->wtop , &gww,&ghh , &gxx,&gyy ) ;

         ifrac = (isq->onoff_state) ? isq->image_frac : 1.0 ;

         if( isq->mont_nx > 1 || isq->mont_ny > 1 ){
            sprintf(mont,"%dx%d:%d:%d:%s" ,
                    isq->mont_nx , isq->mont_ny , isq->mont_skip+1 , isq->mont_gap ,
                    zm3d->dc->ovc->label_ov[isq->mont_gapcolor]);
         } else {
            mont[0] = '\0' ;
         }

         if( fp != NULL ){
           if( mont[0] == '\0' ){
              fprintf(fp, "  %c.%simage geom=%dx%d+%d+%d ifrac=%s\n" ,
                          abet[cc], wnam[ww], gww,ghh,gxx,gyy, AV_uformat_fval(ifrac) ) ;
           } else {
              fprintf(fp, "  %c.%simage geom=%dx%d+%d+%d ifrac=%s mont=%s\n" ,
                          abet[cc], wnam[ww], gww,ghh,gxx,gyy, AV_uformat_fval(ifrac), mont ) ;
           }
         }

         /*-- 22 Jan 2003: startup script stuff for image viewers --*/

         if( gp != NULL ){
           int opval=9 , iar[4] ;
           drive_MCW_imseq( isq, isqDR_getopacity, (XtPointer)&opval ) ;
           if( mont[0] == '\0' ){
             fprintf(gp, "OPEN_WINDOW %c.%simage geom=%dx%d+%d+%d ifrac=%s opacity=%d\n" ,
                     abet[cc], wnam[ww], gww,ghh, gxx,gyy, AV_uformat_fval(ifrac), opval ) ;
           } else {
             fprintf(gp,"OPEN_WINDOW %c.%simage geom=%dx%d+%d+%d ifrac=%s mont=%s opacity=%d\n",
                     abet[cc], wnam[ww], gww,ghh, gxx,gyy, AV_uformat_fval(ifrac), mont, opval ) ;
           }

           /* 03 May 2007: save crop info as well */

           iar[0] = -1 ;
           drive_MCW_imseq( isq , isqDR_get_crop , (XtPointer)iar ) ;
           if( iar[0] >= 0 )
             fprintf(gp, "ALTER_WINDOW %c.%simage crop=%d:%d,%d:%d\n",
                     abet[cc], wnam[ww] , iar[0],iar[1],iar[2],iar[3] ) ;
        }
      }

      /*-- loop over graph viewers --*/

      for( ww=0 ; ww < 3 ; ww++ ){

         gra = (ww == 0) ? zm3d->g123    /* get the graph */
              :(ww == 1) ? zm3d->g231    /* viewer struct */
              :            zm3d->g312 ;

         if( gra == NULL ) continue ;   /* ERROR */

         MCW_widget_geom( gra->fdw_graph , &gww,&ghh , &gxx,&gyy ) ;

         pinnum = (gra->pin_top < MIN_PIN) ? 0 : gra->pin_top ;
         matrix = gra->mat ;

         if( fp != NULL ){
           if( pinnum > 0 ){
              fprintf(fp , "  %c.%sgraph geom=%dx%d+%d+%d matrix=%d pinnum=%d\n" ,
                           abet[cc] , wnam[ww] , gww,ghh,gxx,gyy , matrix,pinnum ) ;
           } else {
              fprintf(fp , "  %c.%sgraph geom=%dx%d+%d+%d matrix=%d\n" ,
                           abet[cc] , wnam[ww] , gww,ghh,gxx,gyy , matrix ) ;
           }
         }

         /*-- 22 Jan 2003: startup script stuff for graph viewers --*/

         if( gp != NULL ){
           if( pinnum > 0 ){
             fprintf(gp,"OPEN_WINDOW %c.%sgraph geom=%dx%d+%d+%d matrix=%d pinnum=%d\n" ,
                        abet[cc], wnam[ww], gww,ghh,gxx,gyy, matrix,pinnum ) ;
           } else {
             fprintf(gp,"OPEN_WINDOW %c.%sgraph geom=%dx%d+%d+%d matrix=%d\n" ,
                        abet[cc], wnam[ww], gww,ghh,gxx,gyy, matrix ) ;
           }
         }
      }

   } /* end of loop over controllers */

#ifdef ALLOW_PLUGINS
   /*-- loop over plugins --*/

   for( ipl=0 ; ipl < npbut ; ipl++ ){

      if( plugint[ipl]->wid == NULL || plugint[ipl]->wid->shell == NULL ) continue ;

      cc = AFNI_controller_index( plugint[ipl]->im3d ) ;

      if( cc < 0 || cc >= MAX_CONTROLLERS || !IM3D_OPEN(plugint[ipl]->im3d) ) continue ;

      if( ! MCW_widget_visible(plugint[ipl]->wid->shell) ) continue ;

      /* it passed all the test, so get its location */

      MCW_widget_geom( plugint[ipl]->wid->shell , &gww,&ghh , &gxx,&gyy ) ;

      /* make a label for the plugin, stripping trailing
         blanks and replacing interior blanks with underscores */

      plab = strdup( pluglab[ipl] ) ;
      for( ll=strlen(plab) ;                         /* truncate trailing blanks */
           ll >= 0 && isspace(plab[ll]) ; ll-- ) plab[ll] = '\0' ;
      if( ll < 0 ) continue ;                        /* all blanks?! ERROR */
      ll++ ;
      for( qq=0 ; qq < ll ; qq++ ) if( isspace(plab[qq]) ) plab[qq] = '_' ;

      if( fp != NULL ) fprintf(fp , "  %c.plugin.%s geom=+%d+%d\n" ,
                                    abet[cc] , plab , gxx,gyy ) ;
   }
#endif

   /*-- finito! --*/

   if( fp != NULL ) fclose(fp) ;
   if( gp != NULL ) fclose(gp) ;
   EXRETURN ;
}

/*--------------------------------------------------------------------------*/
/*! Run the startup script [21 Jan 2003]. */

void AFNI_startup_script_CB( XtPointer client_data , XtIntervalId *id )
{
   char *fname = (char *)client_data ;
   char *fbuf , *fptr ;
   int ii , nbuf ;

ENTRY("AFNI_startup_script_CB") ;

   if( fname == NULL ) EXRETURN ;

   if( strchr(fname,' ') != NULL ){  /* if contains a blank, */
     AFNI_driver(fname) ;            /* execute a single command */
     EXRETURN ;
   }

   fbuf = AFNI_suck_file(fname); if( fbuf == NULL ) EXRETURN ;
   nbuf = strlen(fbuf) ;         if( nbuf == 0    ) EXRETURN ;

   fptr = fbuf ; linbuf = (char *) malloc(sizeof(char)*(NLBUF+1)) ;

   GLOBAL_library.ignore_lock = 1 ;  /* 06 Feb 2004 */

   while(1){
     ii = get_linbuf( fptr ) ; fptr += ii ;
     if( linbuf[0] == '\0' || fptr-fbuf >= nbuf ){ free(linbuf); break; }
     AFNI_driver( linbuf ) ;
   }

   GLOBAL_library.ignore_lock = 0 ; EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/*! Get a filename to run as an AFNI script.  22 Jan 2003 - RWCox.
-----------------------------------------------------------------------------*/

void AFNI_run_script_CB( Widget w , XtPointer cd , XtPointer cbs )
{
   Three_D_View *im3d = (Three_D_View *) cd ;

ENTRY("AFNI_run_script_CB") ;

   if( !IM3D_OPEN(im3d) ) EXRETURN ;

   MCW_choose_string( im3d->vwid->picture ,
                      "Enter AFNI script filename:" ,
                      NULL , AFNI_finalrun_script_CB , cd ) ;
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/

void AFNI_finalrun_script_CB( Widget w , XtPointer cd , MCW_choose_cbs *cbs )
{
   Three_D_View *im3d = (Three_D_View *) cd ;

ENTRY("AFNI_finalrun_script_CB") ;

   if( !IM3D_OPEN(im3d) ) EXRETURN ;

   AFNI_startup_script_CB( (XtPointer) cbs->cval , NULL ) ;
   EXRETURN ;
}
