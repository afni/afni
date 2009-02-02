/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#undef MAIN
#include "afni.h"

/*-------------------------------------------------------------------------*/

int label_in_PALTAB( PBAR_palette_table *pt , char *lab )
{
   int ii ;
   if( pt == NULL || PALTAB_NUM(pt) == 0 || lab == NULL || lab[0] == '\0' )
      return -1 ;

   for( ii=0 ; ii < PALTAB_NUM(pt) ; ii++ )
      if( strcmp( PALTAB_ARR_LABEL(pt,ii) , lab ) == 0 ) return ii ;

   return -1 ;
}

/*-----------------------------------------------------------------------
   Process an AFNI setup file.
-------------------------------------------------------------------------*/

#define ISTARRED(s) ( (s)[0]=='*' && (s)[1]=='*' && (s)[2]=='*' )

#define EOLSKIP                                                          \
  do{ for( ; fptr[0] != '\n' && fptr[0] != '\0' ; fptr++ ) ; /* nada */  \
      if( fptr[0] == '\0' ){ free(fbuf) ; EXRETURN ; }                   \
      fptr++ ; } while(0)

#define GETSSS                                                            \
  do{ int nu=0,qq;                                                        \
      if( fptr-fbuf >= nbuf || fptr[0] == '\0' ){ free(fbuf); EXRETURN; } \
      str[0]='\0'; qq=sscanf(fptr,"%127s%n",str,&nu); nused+=nu;fptr+=nu; \
      if( str[0]=='\0' || qq==0 || nu==0 ){ free(fbuf); EXRETURN; }       \
    } while(0)

#define GETSTR                                                            \
  do{ GETSSS ;                                                            \
      while(str[0]=='!' || (str[0]=='/' && str[1]=='/') ||                \
            (str[0]=='#' && str[1]=='\0') ){EOLSKIP; GETSSS;}             \
    } while(0)

#define GETEQN                                                            \
  do{ GETSTR ; if(ISTARRED(str)) goto SkipSection ;                       \
      strcpy(left,str) ;                                                  \
      GETSTR ; if(ISTARRED(str)) goto SkipSection ;                       \
      strcpy(middle,str) ;                                                \
      GETSTR ; if(ISTARRED(str)) goto SkipSection ;                       \
      strcpy(right,str) ; } while(0)

#undef  NSBUF
#define NSBUF 256

void AFNI_process_setup( char *fname , int mode , MCW_DC *dc )
{
   int   nbuf , nused , ii ;
   char *fbuf , *fptr ;
   char str[NSBUF] , left[NSBUF] , middle[NSBUF] , right[NSBUF] ;

ENTRY("AFNI_process_setup") ;

   fbuf = AFNI_suck_file( fname ) ; if( fbuf == NULL ) EXRETURN ;
   nbuf = strlen(fbuf) ;            if( nbuf == 0    ) EXRETURN ;

   fptr = fbuf ; nused = 0 ;

   /** scan for section strings, which start with "***" **/

   str[0] = '\0' ;  /* initialize string */

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"Reading AFNI setup file = %s (%d bytes)",fname,nbuf) ;
  STATUS(str);}

   while( nused < nbuf ){

      /**----------------------------------------**/
      /**-- skip ahead to next section keyword --**/

      SkipSection: while( ! ISTARRED(str) ){ GETSTR; }

      /*- 04 Jun 1999 -*/

      if( mode == SETUP_ENVIRON_MODE && strcmp(str,"***ENVIRONMENT") != 0 ){
         GETSTR ;
         goto SkipSection ;
      }

      /**--------------------**/
      /**-- COLORS section --**/

      if( strcmp(str,"***COLORS") == 0 ){
         char label[NSBUF] , defn[NSBUF] ;

STATUS("enter ***COLORS") ;

         while(1){                          /* loop, looking for 'label = color' */
            GETEQN ;

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"GETEQN: %s %s %s",left,middle,right) ; STATUS(str);}

            /* don't allow 'none' to be redefined! */

            if( strcmp(left,"none")==0 || strcmp(right,"none")==0 ) continue ;
            if( !THD_filename_pure(left) ) continue ;

            if( mode == SETUP_INIT_MODE ){
               if( INIT_ncolovr < MAX_NCOLOVR ){
                  ii = INIT_ncolovr++ ;
                  INIT_labovr[ii] = XtNewString(left) ;
                  INIT_colovr[ii] = XtNewString(right) ;

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"setup into #%d",ii) ; STATUS(str);}

               } else {
                  fprintf(stderr,"\nIn setup file %s, color table overflow!\n",fname);
                  goto SkipSection ;
               }
            } else if( mode == SETUP_LATER_MODE ){
               ii = DC_add_overlay_color( dc , right , left ) ;

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"'new' color index returned as #%d",ii) ; STATUS(str);}

               if( ii < 0 )
                  fprintf(stderr,"\nIn setup file %s, unknown color %s\n",fname,right);
               else
                  OVC_mostest( dc->ovc ) ;
            }
         }
         continue ;  /* skip to end of outer while loop */
      } /* end of COLORS */

      /**----------------------**/
      /**-- PALETTES section --**/

      if( strcmp(str,"***PALETTES") == 0 ){  /* loop, looking for palettes */
         char label[NSBUF] = "NoThing" , ccc , * cpt ;
         PBAR_palette_array * ppar=NULL ;
         PBAR_palette ** ppp ;
         PBAR_palette  * ppnew ;
         int npane , pmode , icol=0 , jj ;
         float val ;

STATUS("enter ***PALETTES") ;

         if( GPT == NULL ){               /* 1st time in --> create palettry */
STATUS("create initial palettes") ;
            INIT_PALTAB(GPT) ;
            INIT_PALARR(ppar,"NoThing") ;
            ADDTO_PALTAB(GPT,ppar) ;
         }

         /* loop, looking for palettes */

         while(1){
            GETSTR ; if( ISTARRED(str) ) goto SkipSection ;
            if( fptr-fbuf >= nbuf ){ free(fbuf) ; EXRETURN ; }

            if( str[0] != '[' ){                     /* found a palette label */
               strcpy(label,str) ;
               if( !THD_filename_ok(label) ){
                  fprintf(stderr,"\nIn setup file %s, bad palette label: %s.\n",
                          fname,label) ;
                  free(fbuf) ; EXRETURN ;
               }

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"found palette label=%s. [len=%d label[0]=%d]",
          label,(int)strlen(label),(int)label[0]); STATUS(str);
  sprintf(str,"nbuf=%d fptr-fbuf=%d",nbuf,(int)(fptr-fbuf)); STATUS(str);}

               ii = label_in_PALTAB( GPT , label ) ; /* an old one? */
               if( ii < 0 ){
STATUS("making a new palette array") ;
                  INIT_PALARR(ppar,label) ;          /* make a new palette array */
                  ADDTO_PALTAB(GPT,ppar) ;
               } else {

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"matches old palette array #%d",ii) ; STATUS(str);}

                  ppar = PALTAB_ARR(GPT,ii) ;        /* retrieve old palette array */
               }
               GETSTR ; if( ISTARRED(str) ) goto SkipSection ;
            }

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"GPT now has %d arrays",PALTAB_NUM(GPT)) ; STATUS(str);}

            if( str[0] != '[' ){                    /* bad news! */
               fprintf(stderr,"\nIn setup file %s, expected palette '[n]' here: %s\n",
                              fname , str ) ;
               break ;
            }

            /* decide how big the new palette is to be, and what mode  */
            ii = sscanf( str , "[%d%c" , &npane , &ccc ) ;
            if( ii < 2 ){
               fprintf(stderr,"\nIn setup file %s, can't interpret palette %s\n",
                              fname , str ) ;
               break ;
            } else if( npane < NPANE_MIN || npane > NPANE_MAX ){
               fprintf(stderr,"\nIn setup file %s, illegal palette count %s\n",
                              fname , str ) ;
               break ;
            }

            pmode = (ccc == '+') ? 1 : 0 ;              /* pbar mode */
            ppp   = (pmode==0) ? (ppar->psgn + npane)   /* pointer to pointer */
                               : (ppar->ppos + npane) ; /* to existing palette */

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"palette definition: npane=%d pmode=%d",npane,pmode) ;
  STATUS(str); }

            ppnew = XtNew(PBAR_palette) ;               /* make a new palette */
            ppnew->npane = npane ;
            ppnew->mode  = pmode ;
            for( ii=0 ; ii < npane ; ii++ ){
               ppnew->val[ii] = PAL_FIGNORE ; ppnew->col[ii] = PAL_IIGNORE ;
            }

            /* at this point, now loop to read parameters for new palette */

            for( ii=0 ; ii < npane ; ii++ ){
               GETEQN ;

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"GETEQN: %s %s %s",left,middle,right) ; STATUS(str);}

               val = strtod(left,&cpt) ;
               if( val == 0.0 && *cpt != '\0' ) val = PAL_FIGNORE ;

               if( mode == SETUP_INIT_MODE ){
                 if( strcmp(right,"none") == 0 ){
                    icol = 0 ;
                 } else {
                    for( jj=0 ; jj < INIT_ncolovr ; jj++ )
                       if( strcmp(right,INIT_labovr[jj]) == 0 ) break ;
                    icol = (jj < INIT_ncolovr) ? jj+1 : PAL_IIGNORE ;
                 }
               } else if( mode == SETUP_LATER_MODE ){
                  icol = DC_find_overlay_color( dc , right ) ;
                  if( icol < 0 ) icol = PAL_IIGNORE ;

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"  DC_find_overlay_color(%s) returns %d\n",right,icol) ;
  STATUS(str) ; }

               }
               ppnew->val[ii] = val ; ppnew->col[ii] = icol ;

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"new palette entry #%d: val=%f col=%d",ii,val,icol) ;
  STATUS(str);}

            }

            ii = check_PBAR_palette( ppnew ) ;
            if( ii < 0 ){
               fprintf(stderr,"\nIn setup file %s, palette '%s [%d%s' is illegal\n",
                       fname,label,npane, (pmode==0)?"]":"+]" ) ;
               myXtFree(ppnew) ;
            } else {
               myXtFree(*ppp) ;
               *ppp = ppnew ;

STATUS("stored new palette") ;
            }
         }

         continue ;  /* to end of outer while */
      } /* end of PALETTES */

      /**---------------------------------------**/
      /**-- ENVIRONMENT section [04 Jun 1999] --**/

      if( strcmp(str,"***ENVIRONMENT") == 0 ){  /* loop, looking for environment settings */
         char *enveqn ; int nl , nr ;

         if( mode != SETUP_ENVIRON_MODE ){ GETSTR ; goto SkipSection ; }

STATUS("enter ***ENVIRONMENT") ;

         while(1){                          /* loop, looking for 'name = value' */
            GETEQN ;

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"GETEQN: %s %s %s",left,middle,right) ; STATUS(str);}

            if( !THD_filename_pure(left) ) continue ;

            nl = strlen(left) ; nr = strlen(right) ;
            enveqn = (char *) malloc(nl+nr+4) ;
            strcpy(enveqn,left) ; strcat(enveqn,"=") ; strcat(enveqn,right) ;
            putenv(enveqn) ;
         }

         continue ;  /* to end of outer while */
      } /* end of ENVIRONMENT */

      /** END **/

      if( strcmp(str,"***END") == 0 ) break ;  /* exit main loop */

      /** unknown section **/

#if 0
      fprintf(stderr,"\nIn setup file %s, unknown section: %s\n",
                     fname , str ) ;
      break ;                                 /* exit main loop */
#else
      GETSTR ; goto SkipSection ;             /* find another section */
#endif

   }  /* end of while loop */

   free(fbuf) ; EXRETURN ;
}

/*-----------------------------------------------------------------*/

int check_PBAR_palette( PBAR_palette *pp )
{
   int ii , nn ;

ENTRY("check_PBAR_palette") ;

   if( pp == NULL ) RETURN(-1) ;
   if( pp->npane < NPANE_MIN || pp->npane >  NPANE_MAX    ) RETURN(-1) ;
   if( pp->mode  < 0         || pp->mode  >= PANE_MAXMODE ) RETURN(-1) ;

   /** val must be all numbers or all ignores -- nothing mixed **/

   nn = 0 ;
   for( ii=0 ; ii < pp->npane ; ii++ )
      if( pp->val[ii] == PAL_FIGNORE ) nn++ ;
   if( nn > 0 && nn != pp->npane ) RETURN(-1) ;

   /** if all numbers, must be ordered **/

   if( nn == 0 ){
      if( pp->val[0] <= 0.0 ) RETURN(-1) ;  /* 1st must be positive */
      for( ii=1 ; ii < pp->npane ; ii++ )
         if( pp->val[ii] >= pp->val[ii-1] ) RETURN(-1) ;  /* disordered? */

      if( pp->mode == 1 && pp->val[pp->npane-1] < 0.0 ) RETURN(-1) ;
   }

   RETURN(1) ;
}

/*------------------------------------------------------------------------------*/

char * dump_PBAR_palette_table( int verb )
{
   int ii , jj , nn , nsss,nuuu , nbuf , kk ;
   char *sss ;
   static char buf[2048] ;
   char s1[32] , s2[32] ;
   PBAR_palette *pp ;
   MCW_DC *dc = GLOBAL_library.dc ;

ENTRY("dump_PBAR_palette_table") ;

   nsss = 256 ; sss = (char *) malloc(sizeof(char) * nsss) ;
   sss[0] = '\0' ; nuuu = 0 ;

   if( verb ){
      sprintf(sss,"Overlay Color Table: \n") ; nuuu = strlen(sss) ;
      for( kk=1 ; kk < dc->ovc->ncol_ov ; kk++ ){
         sprintf(buf,"  %s = %s\n" , dc->ovc->label_ov[kk] , dc->ovc->name_ov[kk] ) ;
         nbuf = strlen(buf) ;
         if( nbuf+nuuu+2 > nsss ){
            nsss = nbuf+nuuu+128 ;
            sss = (char *) realloc( sss , sizeof(char) * nsss ) ;
         }
         strcat(sss,buf) ; nuuu = strlen(sss) ;
      }
   }

   if( GPT == NULL || PALTAB_NUM(GPT) == 0 ){
      sprintf(buf,"\nPalette Table: *** EMPTY ***\n") ;
      nbuf = strlen(buf) ;
      if( nbuf+nuuu+2 > nsss ){
         nsss = nbuf+nuuu+128 ;
         sss = (char *) realloc( sss , sizeof(char) * nsss ) ;
      }
      strcat(sss,buf) ; nuuu = strlen(sss) ;
      RETURN(sss) ;
   }

   sprintf(buf,"\nPalette Table:\n") ;
   nbuf = strlen(buf) ;
   if( nbuf+nuuu+2 > nsss ){
      nsss = nbuf+nuuu+128 ;
      sss = (char *) realloc( sss , sizeof(char) * nsss ) ;
   }
   strcat(sss,buf) ; nuuu = strlen(sss) ;

   for( ii=0 ; ii < PALTAB_NUM(GPT) ; ii++ ){
      if( PALTAB_ARR(GPT,ii) == NULL ){
         sprintf(buf,"#%2d: *** EMPTY PALETTE ***\n",ii) ;
      } else {
         sprintf(buf,"#%2d: %s has" ,
                 ii,PALTAB_ARR_LABEL(GPT,ii) ); nbuf = strlen(buf) ;
         nn = 0 ;
         for( jj=NPANE_MIN ; jj <= NPANE_MAX ; jj++ ){
            if( (pp=PALTAB_ARR_PSGN(GPT,ii,jj)) != NULL ){
               if( verb ){ sprintf(buf+nbuf,"\n") ; nbuf = strlen(buf) ; }
               sprintf(buf+nbuf," [%d]",jj) ; nn++ ; nbuf = strlen(buf) ;
               if( verb ){
                  for( kk=0 ; kk < jj ; kk++ ){
                     sprintf(buf+nbuf,"\n  %s -> %f",
                             dc->ovc->label_ov[pp->col[kk]] , pp->val[kk] ) ;
                     nbuf = strlen(buf) ;
                  }
               } else {
                  if(nn%10 == 0){sprintf(buf+nbuf,"\n       "); nbuf=strlen(buf);}
               }
            }
            if( (pp=PALTAB_ARR_PPOS(GPT,ii,jj)) != NULL ){
               if( verb ){ sprintf(buf+nbuf,"\n") ; nbuf = strlen(buf) ; }
               sprintf(buf+nbuf," [%d+]",jj) ; nn++ ; nbuf = strlen(buf) ;
               if( verb ){
                  for( kk=0 ; kk < jj ; kk++ ){
                     if( pp->col[kk] >= 0 ) strcpy(s1,dc->ovc->label_ov[pp->col[kk]]) ;
                     else                   strcpy(s1,"IGNORE") ;

                     if( pp->val[kk] != PAL_FIGNORE ) sprintf(s2,"%f",pp->val[kk]) ;
                     else                             strcpy(s2,"IGNORE") ;

                     sprintf(buf+nbuf,"\n  %s -> %s", s1,s2 ) ;
                     nbuf = strlen(buf) ;
                  }
               } else {
                  if(nn%10 == 0){sprintf(buf+nbuf,"\n       "); nbuf=strlen(buf);}
               }
            }
         }
              if(         nn    == 0) sprintf(buf+nbuf," nothing \n") ;
         else if( verb || nn%10 != 0) sprintf(buf+nbuf,"\n") ;
      }
      nbuf = strlen(buf) ;
      if( nbuf+nuuu+2 > nsss ){
         nsss = nbuf+nuuu+128 ;
         sss = (char *) realloc( sss , sizeof(char) * nsss ) ;
      }
      strcat(sss,buf) ; nuuu = strlen(sss) ;
   }
   RETURN(sss) ;
}

/*------------------------------------------------------------------------------*/

void load_PBAR_palette_array( MCW_pbar *pbar, PBAR_palette_array *par, int fixim )
{
   int ii , jj , jm , nn ;
   PBAR_palette *pp ;

ENTRY("load_PBAR_palette_array") ;

   if( pbar == NULL || par == NULL ) EXRETURN ;

   nn = 0 ;
   for( jj=NPANE_MIN ; jj <= NPANE_MAX ; jj++ ){
      pp = par->psgn[jj] ; jm = 0 ;
      if( pp != NULL ){
         if( pp->val[0] != PAL_FIGNORE ){
            for( ii=0 ; ii < jj ; ii++ )
               pbar->pval_save[jj][ii][jm] = pp->val[ii] ;
            pbar->pval_save[jj][jj][jm] = - pp->val[0] ;  /* reflection */
         }

         for( ii=0 ; ii < jj ; ii++ )
            if( pp->col[ii] >= 0 && pp->col[ii] < pbar->dc->ovc->ncol_ov )
               pbar->ovin_save[jj][ii][jm] = pp->col[ii] ;

         nn++ ;
      }

      pp = par->ppos[jj] ; jm = 1 ;
      if( pp != NULL ){
         if( pp->val[0] != PAL_FIGNORE ){
            for( ii=0 ; ii < jj ; ii++ )
               pbar->pval_save[jj][ii][jm] = pp->val[ii] ;
            pbar->pval_save[jj][jj][jm] = 0.0 ;  /* zero based */
         }

         for( ii=0 ; ii < jj ; ii++ )
            if( pp->col[ii] >= 0 && pp->col[ii] < pbar->dc->ovc->ncol_ov )
               pbar->ovin_save[jj][ii][jm] = pp->col[ii] ;

         nn++ ;
      }
   }

   if( nn > 0 && !pbar->bigmode ){
      Three_D_View * im3d = (Three_D_View *) pbar->parent ;
      if( fixim ){ HIDE_SCALE(im3d) ; }
      alter_MCW_pbar( pbar , 0 , NULL ) ;
      if( fixim ){ FIX_SCALE_SIZE(im3d) ; }
   }
   EXRETURN ;
}

/*--------------------------------------------------------------*/

char * AFNI_palette_label_CB( MCW_arrowval *av , XtPointer cd )
{
   static char blab[32] ;

   if( av->ival >= 0 && av->ival < PALTAB_NUM(GPT) )
      sprintf(blab,"%-.14s",PALTAB_ARR_LABEL(GPT,av->ival)) ;
   else
      strcpy(blab,"???") ;

   return blab ;
}

/*-----------------------------------------------------------------
  Event handler to find #3 button press for pbar popup
-------------------------------------------------------------------*/

void AFNI_pbar_EV( Widget w , XtPointer cd ,
                   XEvent *ev , Boolean *continue_to_dispatch )
{
   Three_D_View *im3d = (Three_D_View *) cd ;

ENTRY("AFNI_pbar_EV") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

   switch( ev->type ){
      case ButtonPress:{
         XButtonEvent *event = (XButtonEvent *) ev ;
         im3d->vwid->butx = event->x_root ;  /* 17 May 2005 */
         im3d->vwid->buty = event->y_root ;
         event->button    = Button3 ;                            /* fakeout */
         XmMenuPosition( im3d->vwid->func->pbar_menu , event ) ; /* where */
         XtManageChild ( im3d->vwid->func->pbar_menu ) ;         /* popup */
      }
      break ;
   }

   EXRETURN ;
}

/*---------------------------------------------------------------
  Callbacks for all actions in the pbar popup
-----------------------------------------------------------------*/

void AFNI_pbar_CB( Widget w , XtPointer cd , XtPointer cbs )
{
   Three_D_View *im3d = (Three_D_View *) cd ;
   MCW_pbar *pbar ;
   int npane , jm , ii ;
   double pmax , pmin ;
   float pval[NPANE_MAX+1] ;

ENTRY("AFNI_pbar_CB") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;
#if 0
   if( AFNI_splash_isopen() == 1 ){ BEEPIT ; EXRETURN ; }
#endif

   pbar  = im3d->vwid->func->inten_pbar ;
   npane = pbar->num_panes ;
   jm    = pbar->mode ;
   pmax  = pbar->pval_save[npane][0][jm] ;
   pmin  = pbar->pval_save[npane][npane][jm] ;

   /*--- Equalize spacings ---*/

   if( w == im3d->vwid->func->pbar_equalize_pb ){

      if( pbar->bigmode ){ BEEPIT; EXRETURN; } /* 30 Jan 2003 */

      for( ii=0 ; ii <= npane ; ii++ )
         pval[ii] = pmax - ii * (pmax-pmin)/npane ;

      HIDE_SCALE(im3d) ;
      alter_MCW_pbar( pbar , 0 , pval ) ;
      FIX_SCALE_SIZE(im3d) ;
   }

   /*--- Set top value ---*/

   else if( w == im3d->vwid->func->pbar_settop_pb ){
      MCW_choose_integer( im3d->vwid->func->options_label ,
                          "Pbar Top" , 0 , 99999 , 1 ,
                          AFNI_set_pbar_top_CB , cd   ) ;
   }

   /*--- Read in a palette file ---*/

   else if( w == im3d->vwid->func->pbar_readin_pb ){
      XmString xstr ;

      AFNI_make_file_dialog( im3d ) ;
      XtAddCallback( im3d->vwid->file_sbox , XmNokCallback ,
                     AFNI_finalize_read_palette_CB , cd ) ;
      XtAddCallback( im3d->vwid->file_sbox , XmNcancelCallback ,
                     AFNI_finalize_read_palette_CB , cd ) ;
      XtAddCallback( im3d->vwid->file_sbox , XmNhelpCallback ,
                     AFNI_finalize_read_palette_CB , cd ) ;
      im3d->vwid->file_cb = AFNI_finalize_read_palette_CB ;
      im3d->vwid->file_cd = cd ;
      XtVaSetValues( im3d->vwid->file_dialog,
                        XmNtitle,
                        (pbar->bigmode) ? "AFNI: Read colorscale"
                                        : "AFNI: Read Palette" ,
                     NULL ) ;

      xstr = XmStringCreateLtoR( "*.pal"  , XmFONTLIST_DEFAULT_TAG ) ;
      XtVaSetValues( im3d->vwid->file_sbox , XmNpattern , xstr , NULL ) ;
      XmStringFree(xstr) ;

      XtPopup( im3d->vwid->file_dialog , XtGrabNone ) ; RWC_sleep(1);
      RWC_visibilize_widget( im3d->vwid->file_dialog ) ; /* 09 Nov 1999 */
   }

   /*--- Write out a palette file ---*/

   else if( w == im3d->vwid->func->pbar_writeout_pb ){
      MCW_choose_string( im3d->vwid->func->options_label ,
                         (pbar->bigmode) ? "Colorscale Name" : "Palette Name" ,
                         NULL , AFNI_finalize_write_palette_CB , cd ) ;
   }

   /*--- Display the palette table ---*/

   else if( w == im3d->vwid->func->pbar_showtable_pb ){
      char *dum = dump_PBAR_palette_table(1) ;
      new_MCW_textwin( im3d->vwid->func->options_label, dum, TEXT_READONLY ) ;
      free(dum) ;
   }

   /*--- Save pbar into image file ---*/

   else if( w == im3d->vwid->func->pbar_saveim_pb ){
      MCW_choose_string( im3d->vwid->func->options_label ,
                         "PPM file prefix\n"
                         "  * end in .jpg or .png *\n"
                         "  * for those formats   *"
                         , NULL ,
                         AFNI_finalize_saveim_CB , cd ) ;
   }

   /*---- 10 Feb 2004: start the Edit Environment pseudo-plugin ----*/

   else if( w == im3d->vwid->func->pbar_environment_pb &&
            w != NULL                                    ){

     AFNI_misc_CB( im3d->vwid->dmode->misc_environ_pb ,
                   (XtPointer) im3d , (XtPointer) NULL ) ;
   }


   /*** done ***/

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/

void AFNI_palette_av_CB( MCW_arrowval * av , XtPointer cd )
{
   Three_D_View * im3d = (Three_D_View *) cd ;

ENTRY("AFNI_palette_av_CB") ;

   if( ! IM3D_VALID(im3d) || GPT == NULL ) EXRETURN ;
   if( av->ival < 0 || av->ival >= PALTAB_NUM(GPT) ) EXRETURN ;

   load_PBAR_palette_array( im3d->vwid->func->inten_pbar ,
                            PALTAB_ARR(GPT,av->ival) , 1  ) ;

   if( im3d->vinfo->func_visible )
      AFNI_redisplay_func( im3d ) ;

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/

void AFNI_finalize_read_palette_CB( Widget w, XtPointer cd, XtPointer cb )
{
   Three_D_View * im3d = (Three_D_View *) cd ;
   XmFileSelectionBoxCallbackStruct * cbs = (XmFileSelectionBoxCallbackStruct *) cb ;
   char * dum ;

ENTRY("AFNI_finalize_read_palette_CB") ;

   switch( cbs->reason ){

      /** close the file selection dialog **/

      case XmCR_CANCEL:
         RWC_XtPopdown( im3d->vwid->file_dialog ) ;
      break ;

      /** try to read a new palette **/

      case XmCR_OK:{
         char * text = NULL ;
         int ii , npal1 , npal2 ;
         Three_D_View * qq3d ;
         XmStringGetLtoR( cbs->value , XmFONTLIST_DEFAULT_TAG , &text ) ;
         if( text != NULL ){
           if( THD_is_file(text) && !THD_is_directory(text) ){ /* read in file */

             if( im3d->vwid->func->inten_pbar->bigmode ){  /* 22 Oct 2003 */
               char *cmd = AFNI_suck_file( text ) ;
               ii = PBAR_define_bigmap(cmd); free(cmd);
               if( ii == 0 )
                 RWC_XtPopdown( im3d->vwid->file_dialog ) ;
               else
                 (void) MCW_popup_message( w ,
                                           "******************************\n"
                                           "** Can't use the colorscale **\n"
                                           "** file you selected!       **\n"
                                           "******************************"
                                         , MCW_USER_KILL | MCW_TIMER_KILL ) ;
             } else {

               npal1 = (GPT == NULL) ? 0 : PALTAB_NUM(GPT) ;  /* how many before */

               AFNI_process_setup( text , SETUP_LATER_MODE , im3d->dc ) ;

               npal2 = (GPT == NULL) ? 0 : PALTAB_NUM(GPT) ;  /* how many after */

               if( npal2 > npal1 ){                           /* if got some new ones */
                  for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ ){
                     qq3d = GLOBAL_library.controllers[ii] ;
                     if( IM3D_VALID(qq3d) ){
                        refit_MCW_optmenu( qq3d->vwid->func->pbar_palette_av ,
                                           0 ,                     /* new minval */
                                           npal2-1 ,               /* new maxval */
                                           0 ,                     /* new inival */
                                           0 ,                     /* new decim? */
                                           AFNI_palette_label_CB , /* text routine */
                                           NULL                    /* text data */
                                          ) ;

                        /* 14 Jul 1998: whoops */
                        XtManageChild( qq3d->vwid->func->pbar_palette_av->wrowcol ) ;

                        /* 18 Sep 1998: set this palette to be the active one in the caller */

                        if( qq3d == im3d ){
                           AV_assign_ival( qq3d->vwid->func->pbar_palette_av , PALTAB_NUM(GPT)-1 ) ;
                           AFNI_palette_av_CB( qq3d->vwid->func->pbar_palette_av , im3d ) ;
                        }
                     }
                  }
               }

               dum = dump_PBAR_palette_table(0) ;
               (void) MCW_popup_message( im3d->vwid->func->options_label ,
                                         dum , MCW_USER_KILL | MCW_TIMER_KILL ) ;
               free(dum) ;

               RWC_XtPopdown( im3d->vwid->file_dialog ) ;  /* done with dialog */
             }
           } else {                                            /* bad filename */
              (void) MCW_popup_message( w ,
                                          "****************************\n"
                                          "** Can't open the palette **\n"
                                          "** file you selected!     **\n"
                                          "****************************"
                                      , MCW_USER_KILL | MCW_TIMER_KILL ) ;
              BEEPIT ;
           }
           XtFree(text) ;
         }
      }
      break ;

      case XmCR_HELP:
         (void) MCW_popup_message( w ,
                    "To read in a palette file, use the\n"
                    "Directories and Files selectors,\n"
                    "and the Filter entry and button,\n"
                    "to get the 'Selection' box correct;\n"
                    "that is, 'Selection' should be the\n"
                    "the name of the file you want to read.\n"
                    "Then press 'Set'.\n\n"
                    "N.B.: To use the new palette(s), you\n"
                    "      must use the 'Set Pal' chooser."
                 , MCW_USER_KILL ) ;
      break ;
   }
   EXRETURN ;
}

/*----------------------------------------------------------------------------*/

void AFNI_set_pbar_top_CB( Widget wcaller , XtPointer cd , MCW_choose_cbs * cbs )
{
   Three_D_View * im3d = (Three_D_View *) cd ;
   MCW_pbar * pbar ;
   float pval[NPANE_MAX+1] ;
   double pmin,pmax , fac ;
   int ii ;

ENTRY("AFNI_set_pbar_top_CB") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

   pmax  = cbs->fval ; if( pmax <= 0.0 ){ BEEPIT; EXRETURN; }
   pbar  = im3d->vwid->func->inten_pbar ;

   HIDE_SCALE(im3d) ;
   if( pbar->bigmode ){              /* 30 Jan 2003 */
     pbar->bigset = 0 ;
     pmin = (pbar->mode) ? 0.0 : -pmax ;
     PBAR_set_bigmode( pbar , 1 , pmin,pmax ) ;
     AFNI_inten_pbar_CB( pbar , im3d , 0 ) ;
     POPUP_cursorize( pbar->panew ) ;  /* 08 Apr 2005 */
   } else {
     fac = pmax / pbar->pval[0] ;
     for( ii=0 ; ii <= pbar->num_panes ; ii++ )
       pval[ii] = fac * pbar->pval[ii] ;
     alter_MCW_pbar( pbar , 0 , pval ) ;
     NORMAL_cursorize( pbar->panew ) ;  /* 08 Apr 2005 */
   }
   FIX_SCALE_SIZE(im3d) ;

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/

void AFNI_finalize_write_palette_CB( Widget wcaller, XtPointer cd, MCW_choose_cbs *cbs )
{
   Three_D_View *im3d = (Three_D_View *) cd ;
   int ll , ii , jj ;
   char *fname , *ptr ;
   FILE *fp ;
   MCW_pbar *pbar ;
   int jm , npane , novu , ovu[NPANE_MAX] ;
   int *ovin ;
   float *pval ;

ENTRY("AFNI_finalize_write_palette_CB") ;

   if( ! IM3D_OPEN(im3d) || cbs->reason != mcwCR_string ||
       cbs->cval == NULL || (ll=strlen(cbs->cval)) == 0   ){BEEPIT; EXRETURN;}

   fname = (char *) malloc( sizeof(char) * (ll+8) ) ;
   strcpy( fname , cbs->cval ) ;

   if( ll > 240 || ! THD_filename_ok(fname) ){free(fname); BEEPIT; EXRETURN;}

   ptr = strstr(fname,".pal") ;
   if( ptr == NULL || ptr[4] != '0' ){ strcat(fname,".pal"); ll += 4; }
   fp = fopen( fname , "a" ) ;
   if( fp == NULL ){
      char buf[512] ;
      sprintf(buf,"Can't open file\n %s\nfor writing!",fname) ;
      (void) MCW_popup_message( im3d->vwid->func->options_label ,
                                buf , MCW_USER_KILL | MCW_TIMER_KILL ) ;
      BEEPIT ; free(fname) ; EXRETURN ;
   } else {
      char buf[512] ;
      sprintf(buf,"\nWriting current palette to file\n %s\n",fname) ;
      (void) MCW_popup_message( im3d->vwid->func->options_label ,
                                buf , MCW_USER_KILL | MCW_TIMER_KILL ) ;
   }

   pbar  = im3d->vwid->func->inten_pbar ;
   npane = pbar->num_panes ;
   jm    = pbar->mode ;
   ovin  = pbar->ov_index ;
   pval  = pbar->pval ;

   /* 22 Oct 2003: Colorscale? */

   if( pbar->bigmode ){
     fprintf(fp,"%s\n",pbar->bigname) ;
     for( ii=0 ; ii < NPANE_BIG ; ii++ )
       fprintf(fp,"#%02x%02x%02x\n",
               (unsigned int)pbar->bigcolor[ii].r ,
               (unsigned int)pbar->bigcolor[ii].g ,
               (unsigned int)pbar->bigcolor[ii].b  ) ;
   } else {

     /* make list of all discrete colors used, pruning redundancies */

     novu = 1 ; ovu[0] = ovin[0] ;
     for( ii=1 ; ii < npane ; ii++ ){        /* check each pane */
       for( jj=0 ; jj < novu ; jj++ )       /* for match with current list */
         if( ovin[ii] == ovu[jj] ) break ;

       if( jj == novu )                     /* didn't find a match */
         ovu[novu++] = ovin[ii] ;
     }

     /* write colors to file */

     fprintf( fp , "\n***COLORS\n" ) ;
     for( ii=0 ; ii < novu ; ii++ ){
       if( ovu[ii] > 0 )                                   /* don't write 'none' */
         fprintf( fp , "  %s = %s\n" ,
                  im3d->dc->ovc->label_ov[ovu[ii]] ,
                  im3d->dc->ovc->name_ov[ovu[ii]]   ) ;
     }

     fname[ll-4] = '\0' ;
     fprintf( fp , "\n***PALETTES %s [%d%s\n" ,
              fname , npane , (jm==0) ? "]" : "+]" ) ;

     for( ii=0 ; ii < npane ; ii++ )
       fprintf( fp , "  %f -> %s\n" ,
                pval[ii] , im3d->dc->ovc->label_ov[ovin[ii]] ) ;
   }

   POPDOWN_string_chooser; fclose(fp); free(fname); EXRETURN;
}

/*----------------------------------------------------------------------------*/

void AFNI_finalize_saveim_CB( Widget wcaller, XtPointer cd, MCW_choose_cbs * cbs )
{
   Three_D_View * im3d = (Three_D_View *) cd ;
   char * fname , * ptr ;
   int ll , nx=20 , ny=256 ;
   MRI_IMAGE * im ;

ENTRY("AFNI_finalize_saveim_CB") ;

   if( ! IM3D_OPEN(im3d) || cbs->reason != mcwCR_string ||
       cbs->cval == NULL || (ll=strlen(cbs->cval)) == 0   ){BEEPIT; EXRETURN;}

   fname = (char *) malloc( sizeof(char) * (ll+8) ) ;
   strcpy( fname , cbs->cval ) ;

   if( ll > 240 || ! THD_filename_ok(fname) ){free(fname); BEEPIT; EXRETURN;}

   if( !STRING_HAS_SUFFIX_CASE(fname,".ppm") &&
       !STRING_HAS_SUFFIX_CASE(fname,".pnm") &&
       !STRING_HAS_SUFFIX_CASE(fname,".jpg") &&
       !STRING_HAS_SUFFIX_CASE(fname,".png")   ) strcat(fname,".ppm") ;

   INFO_message("Writing palette image to %s",fname) ;

   ptr = getenv( "AFNI_PBAR_IMXY" );
   if( ptr != NULL ){
     ll = sscanf( ptr , "%dx%d" , &nx , &ny ) ;
     if( ll < 2 || nx < 1 || ny < 32 ){ nx=20; ny=256; }
   }

   im = MCW_pbar_to_mri( im3d->vwid->func->inten_pbar , nx,ny ) ;
   mri_write_pnm( fname , im ) ;

   POPDOWN_string_chooser; mri_free(im); free(fname); EXRETURN;
}

/*----------------------------------------------------------------------------*/

void AFNI_palette_tran_CB( MCW_arrowval * av , XtPointer cd )
{
   Three_D_View * im3d = (Three_D_View *) cd ;

ENTRY("AFNI_palette_tran_CB") ;

   if( !IM3D_VALID(im3d) ) EXRETURN ;

   if( av == im3d->vwid->func->pbar_transform0D_av ){         /* 15 Jun 2000 */

      im3d->vwid->func->pbar_transform0D_index = av->ival ;
      if( av->ival == 0 )
         im3d->vwid->func->pbar_transform0D_func = NULL ;
      else {
         im3d->vwid->func->pbar_transform0D_func =
            GLOBAL_library.registered_0D.funcs[av->ival-1];
         if( GLOBAL_library.registered_0D.func_init[av->ival-1] != NULL )
           GLOBAL_library.registered_0D.func_init[av->ival-1]() ;
      }

   } else if( av == im3d->vwid->func->pbar_transform2D_av ){  /* 16 Jun 2000 */

      im3d->vwid->func->pbar_transform2D_index = av->ival ;
      if( av->ival == 0 )
         im3d->vwid->func->pbar_transform2D_func = NULL ;
      else{
         im3d->vwid->func->pbar_transform2D_func =
            GLOBAL_library.registered_2D.funcs[av->ival-1];
         if( GLOBAL_library.registered_2D.func_init[av->ival-1] != NULL )
           GLOBAL_library.registered_2D.func_init[av->ival-1]() ;
      }
   }

   if( im3d->vinfo->func_visible )
      AFNI_redisplay_func( im3d ) ;

   EXRETURN ;
}
