#include "afni.h"

#ifdef NO_FRIVOLITIES

void AFNI_splashdown(void){ return; }  /* for party poopers */
void AFNI_splashup  (void){ return; }

#else                                  /* for party animals */

#include "afni_splash.h"

static void * handle = NULL ;
static void * SPLASH_popup_image( void * , MRI_IMAGE * ) ;

void AFNI_splashdown(void)
{
   if( handle != NULL ){ SPLASH_popup_image(handle,NULL); handle = NULL; }
   return ;
}

void AFNI_splashup(void)
{
   PLUGIN_impopper * ppp ;
   MRI_IMAGE * imspl ;
   int ii , jj , cc,rr , dd,ee ;
   char   bb ;
   byte * bspl ;
   static int first=1 ;

   if( ! PLUTO_popup_open(handle) ){
      imspl = mri_new( NX_SPLASH , NY_SPLASH , MRI_byte ) ;
      bspl  = MRI_BYTE_PTR(imspl) ;

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

      handle = SPLASH_popup_image( handle , imspl ) ;
      mri_free(imspl) ;

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

      drive_MCW_imseq( ppp->seq , isqDR_realize   , NULL                     ) ;
      drive_MCW_imseq( ppp->seq , isqDR_onoffwid  , (XtPointer) isqDR_offwid ) ;
      drive_MCW_imseq( ppp->seq , isqDR_clearstat , NULL                     ) ;
#if 0
      drive_MCW_imseq( ppp->seq , isqDR_reimage   , (XtPointer) 0            ) ;
#endif

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
   } else {
      ppp = (PLUGIN_impopper *) handle ;
      if( ISQ_REALZ(ppp->seq) )
         XMapRaised( XtDisplay(ppp->seq->wtop) , XtWindow(ppp->seq->wtop) ) ;
      else
         AFNI_splashdown() ;
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
