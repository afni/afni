#include "afni.h"

#ifdef NO_FRIVOLITIES

void AFNI_splashdown(void){ return; }  /* for party poopers */
void AFNI_splashup  (void){ return; }

#else  /*================================ for party animals !!!!!!!!!!!!!!!!!!*/

#include "afni_splash.h"     /* contains the RLE image data */

static void * SPLASH_popup_image( void * , MRI_IMAGE * ) ;
static MRI_IMAGE * SPLASH_decode26( int , int , int , char ** ) ;

static MRI_IMAGE * imspl = NULL ;
static void * handle = NULL ;

#define USE_FADING

#ifdef AFNI_DEBUG
#  define USE_TRACING
#endif
#include "dbtrace.h"

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
            for( kk=0 ; kk < 10 ; kk++ ){
#if 0
               for( ii=0 ; ii < nv ; ii++ ) bspl[ii] *= 0.92 ;
#else
               for( ii=0 ; ii < nv ; ii++ ) bspl[ii] = (15*bspl[ii])>>4 ;
#endif
               SPLASH_popup_image(handle,imspl) ;
               drive_MCW_imseq( ppp->seq , isqDR_reimage , (XtPointer) 0 ) ;
               if( COX_clock_time()-et > 1.1 ) break ;
            }
         }
      }
#endif
      SPLASH_popup_image(handle,NULL); myXtFree(handle) ; /* get rid of window */
   }
   mri_free(imspl) ; imspl = NULL ;
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
   static int first=1 , nov , dnov , nm=-1 ;

ENTRY("AFNI_splashup") ;

   /*--- create splash image ---*/

   if( ! PLUTO_popup_open(handle) ){
      mri_free(imspl) ;
      imspl = SPLASH_decode26( NX_blank, NY_blank, NLINE_blank, BAR_blank ) ;

      if( first ){
         nov  =    (lrand48() >> 8) % NOVER  ;
         dnov = 2*((lrand48() >> 8) % 2) - 1 ;
      }
      nov  = (nov+dnov+NOVER) % NOVER ;
      imov = SPLASH_decode26( xover[nov], yover[nov], lover[nov], bover[nov] ) ;

      mri_overlay_2D( imspl, imov, IXOVER, JYOVER ) ; mri_free(imov) ;

#ifdef NMAIN
      if( !first || AFNI_yesenv("AFNI_SPLASH_OVERRIDE") ){ /* 07 Jun 2000 */
         int good=0 , qq,nq=0 ;
         char * ufname , * qname[10] , str[32] ;

         /* select a user-supplied main image name, if any */

         ufname = getenv("AFNI_IMAGE_PGMFILE") ;
         if( ufname != NULL ) qname[nq++] = ufname ;
         for( qq=1 ; qq < 10 ; qq++ ){
            sprintf(str,"AFNI_IMAGE_PGMFILE_%d",qq) ;
            ufname = getenv(str) ; if( ufname != NULL) qname[nq++] = ufname ;
         }

         switch( nq ){
            case 0:  ufname = NULL                           ; break ;
            case 1:  ufname = qname[0]                       ; break ;
            default: ufname = qname[ (lrand48() >> 8) % nq ] ; break ;
         }

         /* popup user-supplied main image, if any */

         if( ufname != NULL ){         /* 08 & 20 Jun 2000 */
            imov = mri_read(ufname) ;  /* popup user-supplied image */
            if( imov != NULL ){
               if( imov->nx != xmain[0] || imov->ny != ymain[0] ){
                  MRI_IMAGE * imq = mri_resize(imov,xmain[0],ymain[0]) ;
                  mri_free(imov) ; imov = imq ;
               }
               if( imov->kind == MRI_rgb ){             /* color */
                  MRI_IMAGE * imq = mri_to_rgb(imspl) ;
                  mri_free(imspl) ; imspl = imq ;
               } else if( imov->kind != MRI_byte ){     /* gray */
                  MRI_IMAGE * imq = mri_to_byte(imov) ;
                  mri_free(imov) ; imov = imq ;
               }
               mri_overlay_2D( imspl , imov , 0,0 ) ;
               mri_free(imov) ; good = 1 ;
            }
         }
         if( !good ){                  /* no user image ==> use my own */
            nm = (nm+1)%(NMAIN) ;
            imov = SPLASH_decode26( xmain[nm],ymain[nm],lmain[nm],bmain[nm] ) ;
            mri_overlay_2D( imspl , imov , 0,0 ) ;
            mri_free(imov) ;
         }
      }
#endif

      handle = SPLASH_popup_image( handle, imspl ) ;
#ifndef USE_FADING
      mri_free(imspl) ; imspl = NULL ;
#endif

      /* modify image display properties */

      ppp = (PLUGIN_impopper *) handle ;

      if( first ){ dd = MWM_DECOR_BORDER ;
                   ee = 0 ;
      } else     { dd = MWM_DECOR_BORDER | MWM_DECOR_TITLE | MWM_DECOR_MENU ;
                   ee = MWM_FUNC_MOVE | MWM_FUNC_CLOSE ;
      }

      XtVaSetValues( ppp->seq->wtop ,
                       XmNx , (GLOBAL_library.dc->width-NX_blank)/2 ,
                       XmNy , 100 ,
                       XmNmwmDecorations , dd ,
                       XmNmwmFunctions   , ee ,
                     NULL ) ;

      /* actually popup image display */

      drive_MCW_imseq( ppp->seq , isqDR_realize   , NULL                     ) ;
      drive_MCW_imseq( ppp->seq , isqDR_onoffwid  , (XtPointer) isqDR_offwid ) ;
      drive_MCW_imseq( ppp->seq , isqDR_clearstat , NULL                     ) ;
#if 0
      drive_MCW_imseq( ppp->seq , isqDR_reimage   , (XtPointer) 0            ) ;
#endif

      /* some super-frivolities */

      if( !first ){
         drive_MCW_imseq( ppp->seq , isqDR_title , (XtPointer) "AFNI!" ) ;
         drive_MCW_imseq( ppp->seq , isqDR_imhelptext,
                          (XtPointer)
                            "Help me if you can, I'm feeling down,\n"
                            "and I do appreciate you being round.\n"
                            "Help me get my feet back on the ground,\n"
                            "won't you please, please, help me?" ) ;



#if 0
        {                      /* 21 Jun 2000: turn sharpening on */
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

   first = 0 ; EXRETURN ;
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

   mri_free( imp->im ) ;                   /* toss old copy */
   imp->im = mri_to_mri( im->kind , im ) ; /* make new copy */

   /*-- input = inactive popper handle ==> activate it --*/

   if( imp->seq == NULL )
      imp->seq = open_MCW_imseq( GLOBAL_library.dc ,
                                 PLUGIN_imseq_getim , (XtPointer) imp ) ;

   /*-- unlike PLUTO_popup_image, actual popup is left to caller --*/

   RETURN ((void *) imp) ;
}

/*--------------------------------------------------------------------------
  Decode the 26 data into an image
----------------------------------------------------------------------------*/

static MRI_IMAGE * SPLASH_decode26( int nx, int ny , int nl , char ** im26 )
{
   MRI_IMAGE * im ;
   byte * bim ;
   int ii , jj , cc,rr , dd,ee ;
   char bb ;

ENTRY("SPLASH_decode26") ;

   if( nx < 3 || ny < 3 || nl < 3 || im26 == NULL ) RETURN(NULL) ;

   im  = mri_new( nx , ny , MRI_byte ) ;
   bim = MRI_BYTE_PTR(im) ;

   /* decode the RLE image data into a real image array */

   cc = rr = 0 ;
   for( ii=0 ; ii < im->nvox && rr < nl ; ){
      bb = im26[rr][cc++] ; if( bb == '\0' ) break ;
      if( bb >= 'A' && bb <= 'Z' ){
         jj = bb - 'A' ; bim[ii++] = map26[jj] ;
      } else {
         dd = bb - '0' ; bb = im26[rr][cc++] ; if( bb == '\0' ) break ;
         jj = bb - 'A' ;
         for( ee=0 ; ee < dd && ii < im->nvox ; ee++ )
            bim[ii++] = map26[jj] ;
      }
      if( im26[rr][cc] == '\0' ){ cc = 0 ; rr++ ; }
   }

   RETURN(im) ;
}
#endif /* NO_FRIVOLITIES */
