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

#else  /*=============================== for party animals !!!!!!!!!!!!!!!!!!*/

#include "afni_splash.h"     /* contains the RLE image data */

static void * SPLASH_popup_image( void * , MRI_IMAGE * ) ;
static MRI_IMAGE * SPLASH_decode26( int , int , int , char ** ) ;
static MRI_IMAGE * SPLASH_decodexx( int , int , int , int ,
                                    byte *, byte *, byte * , char ** ) ;

static MRI_IMAGE * imspl = NULL ;
static void * handle = NULL ;

#define USE_FADING

#define USE_WRITING     /* 26 Feb 2001 */
static int do_write=2 ;

static int    num_ppms  =0 ;     /* 17 Sep 2001 */
static char **fname_ppms=NULL ;
static void AFNI_find_splash_ppms(void) ;

static int    num_face  = 0 ;    /* 28 Mar 2003 */
static char **fname_face=NULL ;
static void AFNI_find_face_jpegs(void) ;

/*----------------------------------------------------------------------------*/

void AFNI_splashraise(void) /* 25 Sep 2000: bring splash window to the top */
{
   PLUGIN_impopper * ppp = (PLUGIN_impopper *) handle ;

   if( ppp != NULL && ISQ_REALZ(ppp->seq) )
      XMapRaised( XtDisplay(ppp->seq->wtop) , XtWindow(ppp->seq->wtop) ) ;

   return ;
}

/*----------------------------------------------------------------------------*/

void AFNI_splashdown(void)
{
   PLUGIN_impopper * ppp = (PLUGIN_impopper *) handle ;

ENTRY("AFNI_splashdown") ;

   if( handle != NULL ){
#ifdef USE_FADING
      float max_splash = 3.0 ;
      char * hh = getenv("AFNI_SPLASHTIME") ;
      if( hh != NULL ) max_splash = strtod(hh,NULL) ;
      if( max_splash > 0.0 ){
         if( imspl != NULL ){  /* fade gently away */
            byte * bspl ; int ii , nv , kk ; double et ;
            bspl = mri_data_pointer(imspl) ;
            nv   = (imspl->pixel_size) * (imspl->nvox) ;
            et   = COX_clock_time() ;
            do_write = 0 ;
            for( kk=0 ; kk < 10 ; kk++ ){
#if 0
               for( ii=0 ; ii < nv ; ii++ ) bspl[ii] *= 0.92 ;
#else
               for( ii=0 ; ii < nv ; ii++ ) bspl[ii] = (15*bspl[ii]) >> 4 ;
#endif
               SPLASH_popup_image(handle,imspl) ;
               drive_MCW_imseq( ppp->seq , isqDR_reimage , (XtPointer) 0 ) ;
               if( COX_clock_time()-et > 2.1 ) break ;
            }
         }
      }
#endif
      SPLASH_popup_image(handle,NULL); myXtFree(handle) ; /* get rid of window */
   }
   mri_free(imspl) ; imspl = NULL ;
   do_write = ( (lrand48() >> 8) % 3 == 0 ) ? 2 : 1 ;
   EXRETURN ;
}

/*----------------------------------------------------------------------------*/

void AFNI_splashup(void)
{
   PLUGIN_impopper * ppp ;
   MRI_IMAGE * imov ;
   int    dd,ee ;
   char   bb ;
   byte * bspl ;
   int   sxx,syy ;
   char * sen ;
   static int ncall=0 , nov , dnov , nm=-1 ;

ENTRY("AFNI_splashup") ;

   /*--- create splash image ---*/

   if( ! PLUTO_popup_open(handle) ){

      /* get some fun stuff, first time in */

      if( ncall == 0 ){
        AFNI_find_splash_ppms() ; /* 17 Sep 2001 */
        AFNI_find_face_jpegs()  ; /* 28 Mar 2003 */
      }

      /* create basic image */

      mri_free(imspl) ;
      imspl = SPLASH_decodexx( NX_blank, NY_blank, NLINE_blank,
                               NC_blank, RMAP_blank,GMAP_blank,BMAP_blank, BAR_blank ) ;

      if( ncall==0 ){                           /* initialize random */
        nov  =    (lrand48() >> 8) % NOVER  ;   /* sub-image overlay */
        dnov = 2*((lrand48() >> 8) % 2) - 1 ;   /* index & direction */
      }

      /* Facial overlay: */
      /*  if have face jpegs, use them; else, use builtin faces [28 Mar 2003] */

      imov = NULL ;
      if( num_face > 0 ){
        dd = (lrand48() >> 8) % num_face ;
        imov = mri_read_stuff( fname_face[dd] ) ;
        if( imov != NULL && (imov->nx > MAX_XOVER || imov->ny > MAX_YOVER) ){
            mri_free(imov) ; imov == NULL ;
        }
      }
      if( imov == NULL ){
        nov  = (nov+dnov+NOVER) % NOVER ;
        imov = SPLASH_decode26( xover[nov], yover[nov], lover[nov], bover[nov] ) ;
      }
      dd = IXOVER + (MAX_XOVER-imov->nx)/2 ;
      ee = JYOVER + (MAX_YOVER-imov->ny)/2 ;
      mri_overlay_2D( imspl, imov, dd,ee ) ; mri_free(imov) ;

      /* possibly replace the splash image at the top */

      if( ncall > 0 || AFNI_yesenv("AFNI_SPLASH_OVERRIDE") || num_ppms > 0 ){ /* 07 Jun 2000 */
        int good=0 , qq,nq=0 , sov=0 ;
        char *ufname , *qname[10] , str[32] ;

#ifdef NMAIN
        sov = (ncall == 1) ;
#endif
        if( !sov )
          sov = ( (lrand48() >> 8 ) % 4 ) == 0 ;   /* 13 Nov 2002: skip overlay? */

        /* select a user specified main image name, if any */

        if( AFNI_yesenv("AFNI_SPLASH_OVERRIDE") && !sov ){
          ufname = getenv("AFNI_IMAGE_PGMFILE") ;
          if( ufname != NULL ) qname[nq++] = ufname ;
          for( qq=1 ; qq < 10 ; qq++ ){
            sprintf(str,"AFNI_IMAGE_PGMFILE_%d",qq) ;
            ufname = getenv(str); if( ufname != NULL) qname[nq++] = ufname;
          }
        }

        switch( nq ){
          case 0:  ufname = NULL                           ; break ;
          case 1:  ufname = qname[0]                       ; break ;
          default: ufname = qname[ (lrand48() >> 8) % nq ] ; break ;
        }

        /* popup user specified main image, if any */

        if( ufname != NULL ){         /* 08 & 20 Jun 2000 */
          imov = mri_read(ufname) ;  /* popup user-supplied image */
          if( imov != NULL ){
            if( imov->nx != NX_TOPOVER || imov->ny != NY_TOPOVER ){
              MRI_IMAGE * imq = mri_resize(imov,NX_TOPOVER,NY_TOPOVER) ;
              mri_free(imov) ; imov = imq ;
            }
            reload_DC_colordef( GLOBAL_library.dc ) ;
            mri_overlay_2D( imspl , imov , 0,0 ) ;
            mri_free(imov) ; good = 1 ;
          }
        }

        if( !good ){    /* no user specified image ==> use my own */

          if( num_ppms > 0 && !sov ){  /* 17 Sep 2001: external image */
            static int np=-1 ;
            if( np < 0 ) np = (lrand48() >> 8) % num_ppms ;
            else         np = (np+1)%(num_ppms) ;
            imov = mri_read_ppm(fname_ppms[np]) ;
            if( imov != NULL ){
              reload_DC_colordef( GLOBAL_library.dc ) ;
              mri_overlay_2D( imspl , imov , 0,0 ) ;
              mri_free(imov) ; good = 1 ;
            }
          }

#ifdef NMAIN
          if( !good ){                            /* internal image */
            nm = (nm+1)%(NMAIN+1) ;
            if( nm < NMAIN ){                     /* don't always overlay */
              if( rmapm[nm] == NULL ){             /* grayscale overlay */
                imov = SPLASH_decode26( xmain[nm],ymain[nm],lmain[nm],bmain[nm] ) ;
              } else {                             /* color overlay */
                imov = SPLASH_decodexx( xmain[nm],ymain[nm],lmain[nm],nmapm[nm],
                                        rmapm[nm],gmapm[nm],bmapm[nm],bmain[nm] ) ;
              }
              reload_DC_colordef( GLOBAL_library.dc ) ;
              mri_overlay_2D( imspl , imov , 0,0 ) ;
              mri_free(imov) ; good = 1 ;
            }
          } /* end of internal image */
#endif

        } /* end of "my own" image */
      } /* end of replacing splash image */

      /*-- show the image at last! --*/

      handle = SPLASH_popup_image( handle, imspl ) ;
#ifndef USE_FADING
      mri_free(imspl) ; imspl = NULL ;
#endif

      /* modify image display properties */

      ppp = (PLUGIN_impopper *) handle ;

      if( ncall==0 ){ dd = MWM_DECOR_BORDER ;
                      ee = 0 ;
      } else        { dd = MWM_DECOR_BORDER | MWM_DECOR_TITLE | MWM_DECOR_MENU ;
                      ee = MWM_FUNC_MOVE | MWM_FUNC_CLOSE ;
      }

      /* 21 Sep 2000 -- allow user to control splash position */

      sxx = (GLOBAL_library.dc->width-NX_blank)/2 ;
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

      XtVaSetValues( ppp->seq->wtop ,
                       XmNx , sxx ,
                       XmNy , syy ,
                       XmNmwmDecorations , dd ,
                       XmNmwmFunctions   , ee ,
                  /**  XmNoverrideRedirect , True , **/
                     NULL ) ;

      /* actually popup image display */

      drive_MCW_imseq( ppp->seq , isqDR_realize   , NULL                     ) ;
      drive_MCW_imseq( ppp->seq , isqDR_onoffwid  , (XtPointer) isqDR_offwid ) ;
      drive_MCW_imseq( ppp->seq , isqDR_clearstat , NULL                     ) ;
#if 0
      drive_MCW_imseq( ppp->seq , isqDR_reimage   , (XtPointer) 0            ) ;
#endif

      NORMAL_cursorize( ppp->seq->wimage ) ; /* 07 Dec 2001 */

      /* some super-frivolities */

      if( ncall==0 ){
         drive_MCW_imseq( ppp->seq , isqDR_title , (XtPointer) "AFNI!" ) ;
         drive_MCW_imseq( ppp->seq , isqDR_imhelptext,
                          (XtPointer)
                           " \n"
                           " Thou art indeed just, Lord, if I contend\n"
                           "  With thee; but, sir, so what I plead is just.\n"
                           "  Why do sinners' ways prosper? and why must\n"
                           "  Disappointment all I endeavour end?\n"
                           " Wert thou my enemy, O thou my friend,\n"
                           "  How wouldst thou worse, I wonder, than thou dost\n"
                           "  Defeat, thwart me? Oh, the sots and thralls of lust\n"
                           "  Do in spare hours more thrive than I that spend,\n"
                           "  Sir, life upon thy cause. See, banks and brakes\n"
                           "  Now, leaved how thick! laced they are again\n"
                           "  With fretty chervil, look, and fresh wind shakes\n"
                           "  Them; birds build -- but not I build; no, but strain,\n"
                           "  Time's eunuch, and not breed one work that wakes.\n"
                           " Mine, O thou lord of life, send my roots rain.\n"
                        ) ;

#if 0
        {                      /* 21 Jun 2000 -- turn sharpening on */
         ISQ_options opt ;
         drive_MCW_imseq( ppp->seq , isqDR_getoptions , (XtPointer) &opt ) ;
         opt.improc_code |= ISQ_IMPROC_SHARP ;
         drive_MCW_imseq( ppp->seq , isqDR_options    , (XtPointer) &opt ) ;
        }
#endif
      }

   /*--- destroy splash image ---*/

   } else {
      ppp = (PLUGIN_impopper *) handle ;

      /* bring splash window to the top */

      if( ISQ_REALZ(ppp->seq) )
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
   PLUGIN_impopper * imp = (PLUGIN_impopper *) handle ;

ENTRY("SPLASH_imseq_getim") ;

   if( imp == NULL ) RETURN(NULL) ;

   /*--- control info ---*/

   if( type == isqCR_getstatus ){
      MCW_imseq_status * stat = myXtNew( MCW_imseq_status ) ;
      stat->num_total  = 1 ;
      stat->num_series = 1 ;
      stat->send_CB    = PLUGIN_seq_send_CB ;
      stat->parent     = (XtPointer) imp  ;
      stat->aux        = NULL ;

      stat->transforms0D = NULL ;  /* 31 Jan 2002: remove all functions */
      stat->transforms2D = NULL ;
      stat->slice_proj   = NULL ;

      RETURN((XtPointer) stat) ;
   }

   /*--- no overlay ---*/

   if( type == isqCR_getoverlay ) RETURN(NULL) ;

   /*--- return a copy of the image
         (since the imseq will delete it when it is done) ---*/

   if( type == isqCR_getimage || type == isqCR_getqimage ){
      MRI_IMAGE * im = NULL ;
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
         MEM_plotdata * mp = get_active_memplot() ;

         set_thick_memplot(0.003) ;    /* slightly thick lines */

         if( do_write == 2 || 1 ){
           char *sf = AFNI_get_date_trivia() ;
           int   nn = strlen(sf) , ss=28 ;
           if( nn > 38 ) ss = (int)(28.0*38.0/nn) ;
           set_color_memplot(1.0,1.0,0.0) ;           /* yellow */
           plotpak_pwritf( 0.5,0.089 , "Today is:"  , 30 , 0 , 0 ) ;
           plotpak_pwritf( 0.5,0.033 , sf           , ss , 0 , 0 ) ;
         } else {
           char * sf = AFNI_get_friend() ;
           char * mf = strstr(sf," for ") ;
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

static void * SPLASH_popup_image( void * handle , MRI_IMAGE * im )
{
   PLUGIN_impopper * imp = (PLUGIN_impopper *) handle ;

ENTRY("SPLASH_popup_image") ;

   /*-- input image is NULL ==> popdown, if applicable --*/

   if( im == NULL ){
      if( imp != NULL )
         drive_MCW_imseq( imp->seq , isqDR_destroy , NULL ) ;

      RETURN ((void *) imp) ;
   }

   /*-- input = no popper handle ==> create one --*/

   if( imp == NULL ){
      imp      = myXtNew(PLUGIN_impopper) ;
      imp->seq = NULL ; imp->im  = NULL ;
   }

   /*-- input = non-null image ==> replace image --*/

   mri_free( imp->im ) ;      /* toss old copy */
   imp->im = mri_copy( im ) ; /* make new copy */

   /*-- input = inactive popper handle ==> activate it --*/

   if( imp->seq == NULL )
      imp->seq = open_MCW_imseq( GLOBAL_library.dc ,
                                 SPLASH_imseq_getim , (XtPointer) imp ) ;

   /*-- unlike PLUTO_popup_image, actual popup is left to caller --*/

   RETURN ((void *) imp) ;
}

/*--------------------------------------------------------------------------
  Decode the 26 data into an image
----------------------------------------------------------------------------*/

static byte map26[26] =
  {  30,  50,  70,  90, 106, 118, 130, 140, 146, 152, 158, 164, 170,
    176, 182, 190, 198, 206, 212, 218, 224, 230, 236, 242, 248, 254 } ;

static MRI_IMAGE * SPLASH_decode26( int nx, int ny , int nl , char ** im26 )
{
   return SPLASH_decodexx( nx, ny, nl, 26,map26,map26,map26,im26 ) ;
}

/*--------------------------------------------------------------------------
  Decode the 'xx' data into an image
----------------------------------------------------------------------------*/

#define MMAX 82                                               /* max num colors */
static char alpha[MMAX] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"        /* codes for colors */
                          "abcdefghijklmnopqrstuvwxyz"        /* [0] .. [MMAX-1] */
                          ",<.>/?;:'[{]}|=+-_)(*&^%$#@!`~" ;

static MRI_IMAGE * SPLASH_decodexx( int nx, int ny, int nl, int nmap,
                                    byte *rmap, byte *gmap, byte *bmap ,
                                    char ** imxx )
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

/*--------------------------------------------------------------------------*/

void AFNI_find_splash_ppms(void)  /* 17 Sep 2001 */
{
   char *epath , *elocal , *eee ;
   char edir[THD_MAX_NAME] , **ename ;
   int epos , ll , ii , id , nppm , nx,ny ;
   char **fppm ;

ENTRY("AFNI_find_splash_ppms") ;

   if( num_ppms > 0 ) EXRETURN ; /* should never happen */

   /*----- get path to search -----*/

                       epath = getenv("AFNI_PLUGINPATH") ;
   if( epath == NULL ) epath = getenv("AFNI_PLUGIN_PATH") ;
   if( epath == NULL ) epath = getenv("PATH") ;
   if( epath == NULL ) EXRETURN ;                          /* bad */

   /*----- copy path list into local memory -----*/

   ll = strlen(epath) ;
   elocal = malloc( sizeof(char) * (ll+2) ) ;

   /*----- put a blank at the end -----*/

   strcpy( elocal , epath ) ; elocal[ll] = ' ' ; elocal[ll+1] = '\0' ;

   /*----- replace colons with blanks -----*/

   for( ii=0 ; ii < ll ; ii++ )
      if( elocal[ii] == ':' ) elocal[ii] = ' ' ;

   /*----- extract blank delimited strings;
           use as directory names to look for files -----*/

   ename    = (char **) malloc(sizeof(char *)*2) ;
   ename[0] = (char *)  malloc(THD_MAX_NAME) ;
   ename[1] = (char *)  malloc(THD_MAX_NAME) ;

   epos = 0 ;

   do{
      ii = sscanf( elocal+epos , "%s%n" , edir , &id ); /* next substring */
      if( ii < 1 ) break ;                              /* none -> done   */

      /** check if edir occurs earlier in elocal **/

      eee = strstr( elocal , edir ) ;
      if( eee != NULL && (eee-elocal) < epos ){ epos += id ; continue ; }

      epos += id ;                                 /* char after last scanned */

      ii = strlen(edir) ;                          /* make sure name has   */
      if( edir[ii-1] != '/' ){                     /* a trailing '/' on it */
          edir[ii]  = '/' ; edir[ii+1] = '\0' ;
      }
      strcpy(ename[0],edir) ;
      strcat(ename[0],".afnisplash*.ppm") ;        /* add filenname pattern */
      strcpy(ename[1],edir) ;
      strcat(ename[1],"afnisplash*.ppm") ;         /* add filenname pattern */

      MCW_file_expand( 2,ename, &nppm , &fppm );   /* find files that match */
      if( nppm <= 0 ) continue ;                   /* no files found */

      /** add files we found to list, if they are good **/

      for( ii=0 ; ii < nppm ; ii++ ){
         mri_read_ppm_header( fppm[ii] , &nx , &ny ) ;
         if( nx == NX_TOPOVER || ny == NY_TOPOVER ){      /* PPM file of good size? */
            if( fname_ppms == NULL )
               fname_ppms = (char **)malloc(sizeof(char *)) ;
            else
               fname_ppms = (char **)realloc(fname_ppms,sizeof(char *)*(num_ppms+1));

            STATUS(fppm[ii]) ;

            fname_ppms[num_ppms++] = strdup(fppm[ii]) ;
         }
      }

      MCW_free_expand( nppm , fppm ) ;

   } while( epos < ll ) ;  /* scan until 'epos' is after end of epath */

   free(elocal) ; free(ename[0]) ; free(ename[1]) ; free(ename) ;
   EXRETURN ;
}

/*--------------------------------------------------------------------------*/

void AFNI_find_face_jpegs(void)  /* 28 Mar 2003 */
{
   char *epath , *elocal , *eee ;
   char edir[THD_MAX_NAME] , **ename ;
   int epos , ll , ii , id , nface , nx,ny ;
   char **fface ;

ENTRY("AFNI_find_face_jpegs") ;

   if( num_face != 0 ) EXRETURN ; /* should never happen */

   /*----- get path to search -----*/

                       epath = getenv("AFNI_PLUGINPATH") ;
   if( epath == NULL ) epath = getenv("AFNI_PLUGIN_PATH") ;
   if( epath == NULL ) epath = getenv("PATH") ;
   if( epath == NULL ){ num_face=-1; EXRETURN ; }

   /*----- copy path list into local memory -----*/

   ll = strlen(epath) ;
   elocal = malloc( sizeof(char) * (ll+2) ) ;

   /*----- put a blank at the end -----*/

   strcpy( elocal , epath ) ; elocal[ll] = ' ' ; elocal[ll+1] = '\0' ;

   /*----- replace colons with blanks -----*/

   for( ii=0 ; ii < ll ; ii++ )
      if( elocal[ii] == ':' ) elocal[ii] = ' ' ;

   /*----- extract blank delimited strings;
           use as directory names to look for files -----*/

   ename    = (char **) malloc(sizeof(char *)*2) ;
   ename[0] = (char *)  malloc(THD_MAX_NAME) ;
   ename[1] = (char *)  malloc(THD_MAX_NAME) ;

   epos = 0 ;

   do{
      ii = sscanf( elocal+epos , "%s%n" , edir , &id ); /* next substring */
      if( ii < 1 ) break ;                              /* none -> done   */

      /** check if edir occurs earlier in elocal **/

      eee = strstr( elocal , edir ) ;
      if( eee != NULL && (eee-elocal) < epos ){ epos += id ; continue ; }

      epos += id ;                                 /* char after last scanned */

      ii = strlen(edir) ;                          /* make sure name has   */
      if( edir[ii-1] != '/' ){                     /* a trailing '/' on it */
          edir[ii]  = '/' ; edir[ii+1] = '\0' ;
      }
      strcpy(ename[0],edir) ;
      strcat(ename[0],"face_*.ppm") ;        /* add filenname pattern */
      strcpy(ename[1],edir) ;
      strcat(ename[1],"face_*.jpg") ;        /* add filenname pattern */

      MCW_file_expand( 2,ename, &nface , &fface );   /* find files that match */
      if( nface <= 0 ) continue ;                   /* no files found */

      /** add files we found to list **/

      if( fname_face == NULL )
        fname_face = (char **)malloc(sizeof(char *)*nface) ;
      else
        fname_face = (char **)realloc(fname_face,sizeof(char *)*(num_face+nface));

      for( ii=0 ; ii < nface ; ii++ )
        fname_face[num_face++] = strdup(fface[ii]) ;

      MCW_free_expand( nface , fface ) ;

   } while( epos < ll ) ;  /* scan until 'epos' is after end of epath */

   free(elocal) ; free(ename[0]) ; free(ename[1]) ; free(ename) ;

   if( num_face == 0 ) num_face = -1 ;
   EXRETURN ;
}

#endif /* NO_FRIVOLITIES */

/*---------------------------------------------------------------------------*/

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   %%%%% The stuff below here is required (i.e., must not be removed)! %%%%%
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */

/*---------------------------------------------------------------------------*/

#define NLBUF 4096
static char * linbuf ;  /* must be malloc()-ed before use */

static int get_linbuf( char * str )
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

void AFNI_decode_geom( char * geom , int *ww, int *hh , int *xx, int *yy )
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

void AFNI_startup_layout_CB( XtPointer client_data , XtIntervalId * id )
{
   char * fname = (char *) client_data ;
   int    nbuf , ii , goslow ;
   char * fbuf , * fptr ;
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
   char * e_asp ;
   int    e_turnoff=0 ;

   int *   plugin_cont = NULL ;
   char ** plugin_geom = NULL ;
   int ipl ;

   Three_D_View * im3d         = GLOBAL_library.controllers[0] ; /* already open */

#ifdef ALLOW_PLUGINS
   int      npbut              = im3d->vwid->nplugbut ;      /* how many plugins */
   char **  pluglab            = im3d->vwid->pluglab ;       /* their labels     */
   PLUGIN_interface ** plugint = im3d->vwid->plugint ;       /* their interfaces */
#endif

ENTRY("AFNI_startup_layout_CB") ;

   if( fname == NULL || fname[0] == '\0' ){ AFNI_splashdown(); EXRETURN; }

   /* read layout file */

   fbuf = AFNI_suck_file(fname); if( fbuf == NULL ){ AFNI_splashdown(); EXRETURN; }
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
         char * pname = linbuf+9 ;
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

   /*-- if any image geom commands are present,
        we might need to enforce the aspect ratio --*/

   e_asp = my_getenv("AFNI_ENFORCE_ASPECT") ;
   if( !YESSISH(e_asp) ){
      putenv("AFNI_ENFORCE_ASPECT=YES") ;
      e_turnoff = 1 ;
   }

   /*-- now do the commanded work --*/

   for( cc=0 ; cc < MAX_CONTROLLERS ; cc++ ){  /* loop over controllers */

      MCW_imseq   * isq ;
      MCW_grapher * gra ;
      int           singleton=0 ;

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
               mp[4] = DC_find_overlay_color(GLOBAL_library.controllers[cc]->dc,mcol);
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
            drive_MCW_grapher( gra , graDR_setmatrix , (XtPointer) graph_matrix[cc][ww] ) ;
            if(goslow || PRINT_TRACING) sleep(1);
         }

         /* make the graph length pinned? */

         if( graph_pinnum[cc][ww] > 1 ){
            drive_MCW_grapher( gra , graDR_setpinnum , (XtPointer) graph_pinnum[cc][ww] ) ;
            if(goslow || PRINT_TRACING) sleep(1);
         }

         /* change the graph window geometry? */

         if( graph_geom[cc][ww][0] != '\0' ){
            AFNI_decode_geom( graph_geom[cc][ww] , &gww,&ghh,&gxx,&gyy ) ;
            if( gxx >= 0 && gyy >= 0 )
               XtVaSetValues( gra->fdw_graph , XmNx , gxx , XmNy , gyy , NULL ) ;
            if( gww > 0 && ghh > 0 )
               XtVaSetValues( gra->fdw_graph , XmNwidth , gww , XmNheight , ghh , NULL ) ;
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

   if( e_turnoff ) putenv("AFNI_ENFORCE_ASPECT=NO") ;

   AFNI_splashdown() ; EXRETURN ;
}

/*---------------------------------------------------------------------------
   Save the current layout to a file
-----------------------------------------------------------------------------*/

void AFNI_save_layout_CB( Widget w , XtPointer cd , XtPointer cbs )
{
   Three_D_View * im3d = (Three_D_View *) cd ;

ENTRY("AFNI_save_layout_CB") ;

   MCW_choose_string( im3d->vwid->picture ,
                      "Layout filename [blank => .afni.startup_script]:" ,
                      NULL , AFNI_finalsave_layout_CB , cd ) ;
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/

void AFNI_finalsave_layout_CB( Widget w , XtPointer cd , MCW_choose_cbs * cbs )
{
   Three_D_View * im3d = (Three_D_View *) cd ;
   int cc,ww , gww,ghh,gxx,gyy ;
   FILE * fp , * gp ;
   MCW_imseq   * isq ;
   MCW_grapher * gra ;
   float ifrac ;
   char mont[128] ;
   int matrix , pinnum ;

   char * abet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ;
   char * wnam[3] = { "axial" , "sagittal" , "coronal" } ;

   int ipl , qq , ll ;
   char * plab ;

   Three_D_View * qm3d         = GLOBAL_library.controllers[0]; /* already open */

#ifdef ALLOW_PLUGINS
   int      npbut              = qm3d->vwid->nplugbut;      /* how many plugins */
   char **  pluglab            = qm3d->vwid->pluglab;       /* their labels     */
   PLUGIN_interface ** plugint = qm3d->vwid->plugint;       /* their interfaces */
#endif

   MCW_DCOV     * ovc          = GLOBAL_library.dc->ovc ;   /* 22 Jan 2003 */
   Three_D_View * zm3d ;

ENTRY("AFNI_finalsave_layout_CB") ;


   if( strcmp(cbs->cval,".afnirc") == 0 ){ BEEPIT; EXRETURN; } /* 12 Oct 2000 */

   /*-- 23 Jan 2003: open layout file if name is "OK", else don't use it --*/

   if( THD_filename_ok(cbs->cval) ){
     fp = fopen( cbs->cval , "w" ) ;
     if( fp == NULL ){ BEEPIT; EXRETURN; }
   } else {
     fp = NULL ;
   }

   if( fp != NULL ) fprintf(fp,"\n***LAYOUT\n") ;

   /*-- 22 Jan 2002: maybe write a startup script to do same things --*/

   if( fp == NULL )
     gp = fopen( ".afni.startup_script" , "w" ) ;
   else
     gp = NULL ;

   if( gp != NULL ){

     fprintf(gp,"// AFNI startup script, from Datamode->Misc->Save Layout\n") ;

     /* put in any extra overlay colors */

     for( qq=DEFAULT_NCOLOVR+1 ; qq < ovc->ncol_ov ; qq++ )
       fprintf(gp,"ADD_OVERLAY_COLOR %s %s\n",
               ovc->name_ov[qq] , ovc->label_ov[qq] ) ;
   } else {
     if( fp == NULL ){ BEEPIT; EXRETURN; }
   }

   /*-- loop over controllers --*/

   for( cc=0 ; cc < MAX_CONTROLLERS ; cc++ ){

      /*-- controller open? */

      zm3d = GLOBAL_library.controllers[cc] ;

      if( !IM3D_OPEN(zm3d) ) continue ; /* skip */

      /* print controller info */

      MCW_widget_geom( zm3d->vwid->top_shell ,
                       NULL,NULL , &gxx,&gyy ) ;

      if( fp != NULL ) fprintf(fp,"  %c geom=+%d+%d\n" , abet[cc] , gxx,gyy ) ;

      /*-- 22 Jan 2003: parallel output for startup script --*/

      if( gp != NULL ){
        MCW_pbar *pbar = zm3d->vwid->func->inten_pbar ;

        fprintf(gp,"OPEN_WINDOW %c geom=+%d+%d\n" , abet[cc] , gxx,gyy ) ;

        if( XtIsManaged(zm3d->vwid->func->frame) )
          fprintf(gp,"OPEN_PANEL %c.Define_Function\n" , abet[cc] ) ;

        fprintf(gp,"SET_THRESHOLD %c.%04d %d\n" , abet[cc] ,
                    (int)(zm3d->vinfo->func_threshold/THR_FACTOR) ,
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

        if( im3d->vinfo->use_autorange )
          fprintf(gp,"SET_FUNC_AUTORANGE %c.+\n" , abet[cc] ) ;
        else
          fprintf(gp,"SET_FUNC_RANGE %c.%f\n" , abet[cc] ,
                  im3d->vwid->func->range_av->fval        ) ;

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
                          abet[cc] , wnam[ww] , gww,ghh,gxx,gyy , AV_uformat_fval(ifrac) ) ;
           } else {
              fprintf(fp, "  %c.%simage geom=%dx%d+%d+%d ifrac=%s mont=%s\n" ,
                          abet[cc] , wnam[ww] , gww,ghh,gxx,gyy , AV_uformat_fval(ifrac) , mont ) ;
           }
         }

         /*-- 22 Jan 2003: startup script stuff for image viewers --*/

         if( gp != NULL ){
           int opval=9 ;
           drive_MCW_imseq( isq , isqDR_getopacity , &opval ) ;
           if( mont[0] == '\0' ){
              fprintf(gp, "OPEN_WINDOW %c.%simage geom=+%d+%d ifrac=%s opacity=%d\n" ,
                          abet[cc] , wnam[ww] , gxx,gyy , AV_uformat_fval(ifrac) , opval ) ;
           } else {
              fprintf(gp, "OPEN_WINDOW %c.%simage geom=+%d+%d ifrac=%s mont=%s opacity=%d\n" ,
                          abet[cc] , wnam[ww] , gxx,gyy , AV_uformat_fval(ifrac) , mont , opval ) ;
           }
        }
      }

      /*-- loop over graph viewers --*/

      for( ww=0 ; ww < 3 ; ww++ ){

         gra = (ww == 0) ? zm3d->g123    /* get the graph */
              :(ww == 1) ? zm3d->g231    /* viewer struct */
              :            zm3d->g312 ;

         if( gra == NULL ) continue ;   /* ERROR */

         MCW_widget_geom( gra->fdw_graph , &gww,&ghh , &gxx,&gyy ) ;

         pinnum = (gra->pin_num < MIN_PIN) ? 0 : gra->pin_num ;
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
              fprintf(gp , "OPEN_WINDOW %c.%sgraph geom=%dx%d+%d+%d matrix=%d pinnum=%d\n" ,
                           abet[cc] , wnam[ww] , gww,ghh,gxx,gyy , matrix,pinnum ) ;
           } else {
              fprintf(gp , "OPEN_WINDOW %c.%sgraph geom=%dx%d+%d+%d matrix=%d\n" ,
                           abet[cc] , wnam[ww] , gww,ghh,gxx,gyy , matrix ) ;
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

void AFNI_startup_script_CB( XtPointer client_data , XtIntervalId * id )
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

   while(1){
     ii = get_linbuf( fptr ) ; fptr += ii ;
     if( linbuf[0] == '\0' || fptr-fbuf >= nbuf ){ free(linbuf); EXRETURN; }
     AFNI_driver( linbuf ) ;
   } /* can't exit this loop except as above */
}

/*---------------------------------------------------------------------------*/
/*! Get a filename to run as an AFNI script.  22 Jan 2003 - RWCox.
-----------------------------------------------------------------------------*/

void AFNI_run_script_CB( Widget w , XtPointer cd , XtPointer cbs )
{
   Three_D_View * im3d = (Three_D_View *) cd ;

ENTRY("AFNI_run_script_CB") ;

   MCW_choose_string( im3d->vwid->picture ,
                      "Enter AFNI script filename:" ,
                      NULL , AFNI_finalrun_script_CB , cd ) ;
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/

void AFNI_finalrun_script_CB( Widget w , XtPointer cd , MCW_choose_cbs * cbs )
{
   Three_D_View * im3d = (Three_D_View *) cd ;

ENTRY("AFNI_finalrun_script_CB") ;

   AFNI_startup_script_CB( (XtPointer) cbs->cval , NULL ) ;
   EXRETURN ;
}
