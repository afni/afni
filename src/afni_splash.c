#include "afni.h"

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   %%% Some of this file is "frivolities" and could be removed from AFNI %%%
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */

#ifdef NO_FRIVOLITIES

void AFNI_splashdown (void){ return; }  /* for party poopers */
void AFNI_splashup   (void){ return; }
void AFNI_splashraise(void){ return; }

#else  /*=============================== for party animals !!!!!!!!!!!!!!!!!!*/

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
   int   sxx,syy ;
   char * sen ;
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

/*---------------------------------------------------------------------------*/

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   %%%%% The stuff below here is required (i.e., must not be removed)! %%%%%
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */

/*---------------------------------------------------------------------------*/

#define NLBUF 512
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

   for( jj=0 ; str[ii] != '\0' &&
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
   int      npbut              = im3d->vwid->nplugbut ;      /* how many plugins */
   char **  pluglab            = im3d->vwid->pluglab ;       /* their labels     */
   PLUGIN_interface ** plugint = im3d->vwid->plugint ;       /* their interfaces */

ENTRY("AFNI_startup_layout_CB") ;

   if( fname == NULL || fname[0] == '\0' ){ AFNI_splashdown(); EXRETURN; }

   /* read layout file */

   fbuf = AFNI_suck_file(fname); if( fbuf == NULL ){ AFNI_splashdown(); EXRETURN; }
   nbuf = strlen(fbuf) ;         if( nbuf == 0    ){ AFNI_splashdown(); EXRETURN; }

   fptr = fbuf ; linbuf = (char *) malloc(sizeof(char)*NLBUF) ;

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

   if( npbut > 0 ){
      plugin_cont = (int *)   malloc(sizeof(int)   *npbut) ;
      plugin_geom = (char **) malloc(sizeof(char *)*npbut) ;
      for( ipl=0 ; ipl < npbut ; ipl++ ){
         plugin_cont[ipl] = -1 ;      /* controller index to start it with */
         plugin_geom[ipl] = NULL ;    /* geometry string to start it with */
      }
   }

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

         if( graph_pinnum[cc][ww] > 0 ){
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

   /*-- now loop over plugins --*/

   for( ipl=0 ; ipl < npbut ; ipl++ ){

      cc = plugin_cont[ipl] ; if( cc < 0 ) continue ;

      if( GLOBAL_library.controllers[cc] == NULL ) continue ;  /* ERROR */

      PLUG_startup_plugin_CB( GLOBAL_library.controllers[cc]->vwid->plugbut[ipl] ,
                              GLOBAL_library.controllers[cc]->vwid->plugint[ipl] ,
                              NULL ) ;

      if(goslow || PRINT_TRACING) sleep(1);

      if( GLOBAL_library.controllers[cc]->vwid->plugint[ipl]->wid        == NULL ||
          GLOBAL_library.controllers[cc]->vwid->plugint[ipl]->wid->shell == NULL   ){

         fprintf(stderr,"\n** LAYOUT: couldn't start plugin %s\n",pluglab[ipl]) ;
         continue ;
      }

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

   /*--- done ---*/

   if( npbut > 0 ){
      for( ipl=0 ; ipl < npbut ; ipl++ )
         if( plugin_geom[ipl] != NULL ) free(plugin_geom[ipl]) ;
      free(plugin_cont) ; free(plugin_geom) ;
   }

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
                      "Enter layout save filename:" ,
                      NULL , AFNI_finalsave_layout_CB , cd ) ;
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/

void AFNI_finalsave_layout_CB( Widget w , XtPointer cd , MCW_choose_cbs * cbs )
{
   Three_D_View * im3d = (Three_D_View *) cd ;
   int cc,ww , gww,ghh,gxx,gyy ;
   FILE * fp ;
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
   int      npbut              = qm3d->vwid->nplugbut;      /* how many plugins */
   char **  pluglab            = qm3d->vwid->pluglab;       /* their labels     */
   PLUGIN_interface ** plugint = qm3d->vwid->plugint;       /* their interfaces */

ENTRY("AFNI_finalsave_layout_CB") ;

   if( !THD_filename_ok(cbs->cval) ){ BEEPIT; EXRETURN; }

   if( strcmp(cbs->cval,".afnirc") == 0 ){ BEEPIT; EXRETURN; } /* 12 Oct 2000 */

   fp = fopen( cbs->cval , "w" ) ;
   if( fp == NULL ){ BEEPIT; EXRETURN; }

   fprintf(fp,"\n***LAYOUT\n") ;

   /*-- loop over controllers --*/

   for( cc=0 ; cc < MAX_CONTROLLERS ; cc++ ){

      /*-- controller open? */

      if( !IM3D_OPEN(GLOBAL_library.controllers[cc]) ) continue ; /* skip */

      /* print controller info */

      MCW_widget_geom( GLOBAL_library.controllers[cc]->vwid->top_shell ,
                       NULL,NULL , &gxx,&gyy ) ;

      fprintf(fp,"  %c geom=+%d+%d\n" , abet[cc] , gxx,gyy ) ;

      /*-- loop over image viewers --*/

      for( ww=0 ; ww < 3 ; ww++ ){

         isq = (ww == 0) ? GLOBAL_library.controllers[cc]->s123     /* get the image */
              :(ww == 1) ? GLOBAL_library.controllers[cc]->s231     /* viewer struct */
              :            GLOBAL_library.controllers[cc]->s312 ;

         if( isq == NULL ) continue ;   /* skip */

         /* get and print image viewer info */

         MCW_widget_geom( isq->wtop , &gww,&ghh , &gxx,&gyy ) ;

         ifrac = (isq->onoff_state) ? isq->image_frac : 1.0 ;

         if( isq->mont_nx > 1 || isq->mont_ny > 1 ){
            sprintf(mont,"%dx%d:%d:%d:%s" ,
                    isq->mont_nx , isq->mont_ny , isq->mont_skip+1 , isq->mont_gap ,
                    GLOBAL_library.controllers[cc]->dc->ovc->label_ov[isq->mont_gapcolor]);
         } else {
            mont[0] = '\0' ;
         }

         if( mont[0] == '\0' ){
            fprintf(fp, "  %c.%simage geom=%dx%d+%d+%d ifrac=%f\n" ,
                        abet[cc] , wnam[ww] , gww,ghh,gxx,gyy , ifrac ) ;
         } else {
            fprintf(fp, "  %c.%simage geom=%dx%d+%d+%d ifrac=%f mont=%s\n" ,
                        abet[cc] , wnam[ww] , gww,ghh,gxx,gyy , ifrac , mont ) ;
         }
      }

      /*-- loop over graph viewers --*/

      for( ww=0 ; ww < 3 ; ww++ ){

         gra = (ww == 0) ? GLOBAL_library.controllers[cc]->g123    /* get the graph */
              :(ww == 1) ? GLOBAL_library.controllers[cc]->g231    /* viewer struct */
              :            GLOBAL_library.controllers[cc]->g312 ;

         if( gra == NULL ) continue ;   /* ERROR */

         MCW_widget_geom( gra->fdw_graph , &gww,&ghh , &gxx,&gyy ) ;

         pinnum = (gra->pin_num < MIN_PIN) ? 0 : gra->pin_num ;
         matrix = gra->mat ;

         if( pinnum > 0 ){
            fprintf(fp , "  %c.%sgraph geom=%dx%d+%d+%d matrix=%d pinnum=%d\n" ,
                         abet[cc] , wnam[ww] , gww,ghh,gxx,gyy , matrix,pinnum ) ;
         } else {
            fprintf(fp , "  %c.%sgraph geom=%dx%d+%d+%d matrix=%d\n" ,
                         abet[cc] , wnam[ww] , gww,ghh,gxx,gyy , matrix ) ;
         }
      }

   } /* end of loop over controllers */

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

      fprintf(fp , "  %c.plugin.%s geom=+%d+%d\n" ,
                   abet[cc] , plab , gxx,gyy ) ;
   }

   /*-- finito! --*/

   fclose(fp) ; EXRETURN ;
}
