#include "afni.h"

#ifdef NO_FRIVOLITIES

void AFNI_splashdown(void){ return; }  /* for party poopers */
void AFNI_splashup  (void){ return; }

#else  /*================================ for party animals !!!!!!!!!!!!!!!!!!*/

#include "afni_splash.h"     /* contains the RLE image data */

static void * handle = NULL ;
static void * SPLASH_popup_image( void * , MRI_IMAGE * ) ;
static MRI_IMAGE * imspl = NULL ;

#define USE_FADING

/*----------------------------------------------------------------------------*/

void AFNI_splashdown(void)
{
   byte * bspl ; int ii , nv , kk ;
   PLUGIN_impopper * ppp = (PLUGIN_impopper *) handle ;

   if( handle != NULL ){
#ifdef USE_FADING
      float max_splash = 5.0 ;
      char * hh = getenv("AFNI_SPLASHTIME") ;
      if( hh != NULL ) max_splash = strtod(hh,NULL) ;
      if( max_splash > 0.0 ){
         if( imspl != NULL ){  /* fade gently away */
            bspl = MRI_BYTE_PTR(imspl) ; nv = imspl->nvox ;
            for( kk=0 ; kk < 10 ; kk++ ){
               for( ii=0 ; ii < nv ; ii++ ) bspl[ii] *= 0.92 ;
               SPLASH_popup_image(handle,imspl) ;
               drive_MCW_imseq( ppp->seq , isqDR_reimage , (XtPointer) 0 ) ;
            }
         }
         iochan_sleep(100) ; /* 100 msec of solitude */
      }
#endif
      SPLASH_popup_image(handle,NULL); handle = NULL;  /* get rid of window */
   }
   mri_free(imspl) ; imspl = NULL ;
   return ;
}

/*----------------------------------------------------------------------------*/

void AFNI_splashup(void)
{
   PLUGIN_impopper * ppp ;
   int ii , jj , cc,rr , dd,ee ;
   char   bb ;
   byte * bspl ;
   static int first=1 ;

   /*--- create splash image ---*/

   if( ! PLUTO_popup_open(handle) ){
      mri_free(imspl) ;
      imspl = mri_new( NX_SPLASH , NY_SPLASH , MRI_byte ) ;
      bspl  = MRI_BYTE_PTR(imspl) ;

      /* decode the RLE image data into a real image array */

      cc = rr = 0 ;
      for( ii=0 ; ii < imspl->nvox && rr < NLINE_SPLASH ; ){
         bb = im26[rr][cc++] ; if( bb == '\0' ) break ;
         if( bb >= 'A' && bb <= 'Z' ){
            jj = bb - 'A' ; bspl[ii++] = map26[jj] ;
         } else {
            dd = bb - '0' ; bb = im26[rr][cc++] ; if( bb == '\0' ) break ;
            jj = bb - 'A' ;
            for( ee=0 ; ee < dd && ii < imspl->nvox ; ee++ )
               bspl[ii++] = map26[jj] ;
         }
         if( im26[rr][cc] == '\0' ){ cc = 0 ; rr++ ; }
      }

      /* initialize image display */

      handle = SPLASH_popup_image( handle , imspl ) ;
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
                       XmNx , (GLOBAL_library.dc->width-NX_SPLASH)/2 ,
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
         drive_MCW_imseq( ppp->seq , isqDR_title     , (XtPointer) "AFNI!" ) ;
         drive_MCW_imseq( ppp->seq , isqDR_imhelptext,
                          (XtPointer) " \n"
                                      "Help me if you can, I'm feeling down,\n"
                                      "and I do appreciate you being round.\n"
                                      "Help me, get my feet back on the ground,\n"
                                      "won't you please, please, help me?\n"
                        ) ;
      }

   /*--- destroy splash image ---*/

   } else {
      ppp = (PLUGIN_impopper *) handle ;

      /* bring splash window to the top */

      if( ISQ_REALZ(ppp->seq) )
         XMapRaised( XtDisplay(ppp->seq->wtop) , XtWindow(ppp->seq->wtop) ) ;

      AFNI_splashdown() ;  /* off with their heads */
   }

   first = 0 ; return ;
}

/*-----------------------------------------------------------------------
  The following is adapted from PLUTO_popup_image() in afni_plugin.c
-------------------------------------------------------------------------*/

static void * SPLASH_popup_image( void * handle , MRI_IMAGE * im )
{
   PLUGIN_impopper * imp = (PLUGIN_impopper *) handle ;

   /*-- input image is NULL ==> popdown, if applicable --*/

   if( im == NULL ){
      if( imp != NULL )
         drive_MCW_imseq( imp->seq , isqDR_destroy , NULL ) ;

      return ((void *) imp) ;
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

   return ((void *) imp) ;
}
#endif /* NO_FRIVOLITIES */
