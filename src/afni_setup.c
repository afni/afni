#undef MAIN
#include "afni.h"

#ifdef AFNI_DEBUG
#  define USE_TRACING
#  define PRINT_TRACING
#endif
#include "dbtrace.h"

/*-------------------------------------------------------------------------*/

int label_in_PALTAB( PBAR_palette_table * pt , char * lab )
{
   int ii ;
   if( pt == NULL || PALTAB_NUM(pt) == 0 || lab == NULL || lab[0] == '\0' )
      return -1 ;

   for( ii=0 ; ii < PALTAB_NUM(pt) ; ii++ )
      if( strcmp( PALTAB_ARR_LABEL(pt,ii) , lab ) == 0 ) return ii ;

   return -1 ;
}

/*------------------------------------------------------------------------
   Read an entire file into a character string.  When you are
   done with the returned string, free it.  If the string pointer
   is returned as NULL, something bad happened.
--------------------------------------------------------------------------*/

char * suck_file( char * fname )
{
   int len , fd , ii ;
   char * buf ;

   if( fname == NULL || fname[0] == '\0' ) return NULL ;

   len = THD_filesize( fname ) ;
   if( len <= 0 ) return NULL ;

   fd = open( fname , O_RDONLY ) ;
   if( fd < 0 ) return NULL ;

   buf = (char *) malloc( sizeof(char) * (len+4) ) ;
   ii  = read( fd , buf , len ) ;
   close( fd ) ;

   if( ii <= 0 ){ free(buf) ; return NULL; }
   buf[ii+1] = '\0' ;
   return buf ;
}

/*-----------------------------------------------------------------------
   Process an AFNI setup file.
-------------------------------------------------------------------------*/

#define ISTARRED(s) ( (s)[0]=='*' && (s)[1]=='*' && (s)[2]=='*' )

#define GETSTR                                                              \
  do{ int nu=0,qq ; qq=sscanf(fptr,"%127s%n",str,&nu); nused+=nu;fptr+=nu;  \
      if( qq==0 || nu==0 ){ free(fbuf) ; return ; }                         \
    } while(0)

#define GETEQN                                         \
  do{ GETSTR ; if(ISTARRED(str)) goto SkipSection ;    \
      strcpy(left,str) ;                               \
      GETSTR ; if(ISTARRED(str)) goto SkipSection ;    \
      strcpy(middle,str) ;                             \
      GETSTR ; if(ISTARRED(str)) goto SkipSection ;    \
      strcpy(right,str) ; } while(0)

void AFNI_process_setup( char * fname , int mode , MCW_DC * dc )
{
   int    nbuf , nused , ii ;
   char * fbuf , * fptr ;
   char str[128] , left[128] , middle[128] , right[128] ;

   fbuf = suck_file( fname ) ; if( fbuf == NULL ) return ;
   nbuf = strlen(fbuf) ;       if( nbuf == 0    ) return ;

   fptr = fbuf ; nused = 0 ;

   /** scan for section strings, which start with "***" **/

   str[0] = '\0' ;  /* initialize string */

printf("\nReading AFNI setup file = %s\n",fname) ;

   while( nused < nbuf ){

      /**----------------------------------------**/
      /**-- skip ahead to next section keyword --**/

      SkipSection: while( ! ISTARRED(str) ){ GETSTR; }

      /**--------------------**/
      /**-- COLORS section --**/

      if( strcmp(str,"***COLORS") == 0 ){
         char label[128] , defn[128] ;

printf("  -- enter ***COLORS\n") ;

         while(1){                          /* loop, looking for 'label = color' */
            GETEQN ;
printf("    -- GETEQN: %s %s %s\n",left,middle,right) ;

            if( mode == SETUP_INIT_MODE ){
               if( INIT_ncolovr < MAX_NCOLOVR ){
                  ii = INIT_ncolovr++ ;
                  INIT_labovr[ii] = XtNewString(left) ;
                  INIT_colovr[ii] = XtNewString(right) ;
printf("    -- setup into #%d\n",ii) ;
               } else {
                  fprintf(stderr,"\nIn setup file %s, color table overflow!\n",fname);
                  goto SkipSection ;
               }
            } else if( mode == SETUP_LATER_MODE ){
               ii = DC_add_overlay_color( dc , right , left ) ;
printf("    -- color index returned as #%d\n",ii) ;
               if( ii < 0 ) fprintf(stderr,"\nIn setup file %s, unknown color %s\n",
                                           fname , right ) ;
            }
         }
         continue ;  /* skip to end of outer while loop */
      } /* end of COLORS */

      /**----------------------**/
      /**-- PALETTES section --**/

      if( strcmp(str,"***PALETTES") == 0 ){  /* loop, looking for palettes */
         char label[128] = "NoName" , ccc , * cpt ;
         PBAR_palette_array * ppar ;
         PBAR_palette ** ppp ;
         PBAR_palette  * ppnew ;
         int npane , pmode , icol , jj ;
         float val ;

printf("  -- enter ***PALETTES\n") ;

         if( GPT == NULL ){               /* 1st time in --> create palettry */
printf("    -- create initial palettes\n") ;
            INIT_PALTAB(GPT) ;
            INIT_PALARR(ppar,"NoName") ;
            ADDTO_PALTAB(GPT,ppar) ;
         }

         /* loop, looking for palettes */

         while(1){
            GETSTR ; if( ISTARRED(str) ) goto SkipSection ;

            if( str[0] != '[' ){                     /* found a palette label */
               strcpy(label,str) ;
printf("    -- found palette label = %s\n",label) ;
               ii = label_in_PALTAB( GPT , label ) ; /* an old one? */
               if( ii < 0 ){
printf("    -- making a new palette array\n") ;
                  INIT_PALARR(ppar,label) ;          /* make a new palette array */
                  ADDTO_PALTAB(GPT,ppar) ;
               } else {
printf("    -- matches old palette array #%d\n",ii) ;
                  ppar = PALTAB_ARR(GPT,ii) ;        /* retrieve old palette array */
               }
               GETSTR ; if( ISTARRED(str) ) goto SkipSection ;
            }

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

printf("    -- palette definition: npane=%d pmode=%d\n",npane,pmode) ;

            ppnew = XtNew(PBAR_palette) ;               /* make a new palette */
            ppnew->npane = npane ;
            ppnew->mode  = pmode ;
            for( ii=0 ; ii < npane ; ii++ ){
               ppnew->val[ii] = PAL_FIGNORE ; ppnew->col[ii] = PAL_IIGNORE ;
            }

            /* at this point, now loop to read parameters for new palette */

            for( ii=0 ; ii < npane ; ii++ ){
               GETEQN ;
printf("      -- GETEQN: %s %s %s\n",left,middle,right) ;

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
               }
               ppnew->val[ii] = val ; ppnew->col[ii] = icol ;
printf("      -- palette entry #%d: val=%f col=%d\n",ii,val,icol) ;
            }

            ii = check_PBAR_palette( ppnew ) ;
            if( ii < 0 ){
               fprintf(stderr,"\nIn setup file %s, palette '%s [%d%s' is illegal\n",
                       fname,label,npane, (pmode==0)?"]":"+]" ) ;
               myXtFree(ppnew) ;
            } else {
               myXtFree(*ppp) ;
               *ppp = ppnew ;
printf("    -- stored new palette\n") ;
            }
         }

         continue ;  /* to end of outer while */
      } /* end of PALETTES */

      /** END **/

      if( strcmp(str,"***END") == 0 ) break ;  /* exit main loop */

      /** unknown section **/

      fprintf(stderr,"\nIn setup file %s, unknown section: %s\n",
                     fname , str ) ;
      break ;                                 /* exit main loop */

   }  /* end of while loop */

   free(fbuf) ; return ;
}

/*-----------------------------------------------------------------*/

int check_PBAR_palette( PBAR_palette * pp )
{
   int ii , nn ;

   if( pp == NULL ) return -1 ;
   if( pp->npane < NPANE_MIN || pp->npane >  NPANE_MAX    ) return -1 ;
   if( pp->mode  < 0         || pp->mode  >= PANE_MAXMODE ) return -1 ;

   /** val must be all numbers or all ignores -- nothing mixed **/

   nn = 0 ;
   for( ii=0 ; ii < pp->npane ; ii++ )
      if( pp->val[ii] == PAL_FIGNORE ) nn++ ;
   if( nn > 0 && nn != pp->npane ) return -1 ;

   /** if all numbers, must be ordered **/

   if( nn == 0 ){
      if( pp->val[0] <= 0.0 ) return -1 ;  /* 1st must be positive */
      for( ii=1 ; ii < pp->npane ; ii++ )
         if( pp->val[ii] >= pp->val[ii-1] ) return -1 ;  /* disordered? */

      if( pp->mode == 1 && pp->val[pp->npane-1] < 0.0 ) return -1 ;
   }

   return 1 ;
}

void dump_PBAR_palette_table(void)
{
   int ii , jj , nn ;

   if( GPT == NULL || PALTAB_NUM(GPT) == 0 ){
      printf("Palette table: empty\n") ;
      return ;
   }

   for( ii=0 ; ii < PALTAB_NUM(GPT) ; ii++ ){
      if( PALTAB_ARR(GPT,ii) == NULL ){
         printf("Palette table: array[%d] is empty\n",ii) ;
      } else {
         printf("Palette table: array[%d] label is %s\n",ii,PALTAB_ARR_LABEL(GPT,ii));
         printf("      entries:") ;
         nn = 0 ;
         for( jj=NPANE_MIN ; jj <= NPANE_MAX ; jj++ ){
            if( PALTAB_ARR_PSGN(GPT,ii,jj) != NULL ){
               printf(" [%d]",jj) ; nn++ ;
            }
            if( PALTAB_ARR_PPOS(GPT,ii,jj) != NULL ){
               printf(" [%d+]",jj) ; nn++ ;
            }
         }
         if( nn == 0 ) printf(" NONE\n") ;
         else          printf("\n") ;
      }
   }
   return ;
}

void load_PBAR_palette_array( MCW_pbar * pbar , PBAR_palette_array * par )
{
   int ii , jj , jm , nn ;
   PBAR_palette * pp ;

   if( pbar == NULL || par == NULL ) return ;

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
            if( pp->col[ii] >= 0 && pp->col[ii] < pbar->dc->ncol_ov )
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
            if( pp->col[ii] >= 0 && pp->col[ii] < pbar->dc->ncol_ov )
               pbar->ovin_save[jj][ii][jm] = pp->col[ii] ;

         nn++ ;
      }
   }

   if( nn > 0 ) alter_MCW_pbar( pbar , 0 , NULL ) ;
   return ;
}

/*--------------------------------------------------------------*/

char * AFNI_palette_label_CB( MCW_arrowval * av , XtPointer cd )
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
                   XEvent * ev , Boolean * continue_to_dispatch )
{
   Three_D_View * im3d = (Three_D_View *) cd ;

ENTRY("AFNI_pbar_EV") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

   switch( ev->type ){
      case ButtonPress:{
         XButtonEvent * event = (XButtonEvent *) ev ;
         if( event->button == Button3 ){
            XmMenuPosition( im3d->vwid->func->pbar_menu , event ) ; /* where */
            XtManageChild ( im3d->vwid->func->pbar_menu ) ;         /* popup */
         }
      }
      break ;
   }

   EXRETURN ;
}

/*---------------------------------------------------------------
  Callback for all actions in the pbar popup
-----------------------------------------------------------------*/

void AFNI_pbar_CB( Widget w , XtPointer cd , XtPointer cbs )
{
   Three_D_View * im3d = (Three_D_View *) cd ;
   MCW_pbar * pbar ;
   int npane , jm , ii ;
   double pmax , pmin ;
   float pval[NPANE_MAX+1] ;

ENTRY("AFNI_pbar_CB") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

   pbar  = im3d->vwid->func->inten_pbar ;
   npane = pbar->num_panes ;
   jm    = pbar->mode ;
   pmax  = pbar->pval_save[npane][0][jm] ;
   pmin  = pbar->pval_save[npane][npane][jm] ;

   if( w == im3d->vwid->func->pbar_equalize_pb ){
      for( ii=0 ; ii <= npane ; ii++ )
         pval[ii] = pmax - ii * (pmax-pmin)/npane ;
      alter_MCW_pbar( pbar , 0 , pval ) ;
   }

   else if( w == im3d->vwid->func->pbar_readin_pb ){
      XBell( im3d->dc->display , 100 ) ;
   }

   else if( w == im3d->vwid->func->pbar_writeout_pb ){
      XBell( im3d->dc->display , 100 ) ;
   }

   EXRETURN ;
}

void AFNI_palette_av_CB( MCW_arrowval * av , XtPointer cd )
{
   Three_D_View * im3d = (Three_D_View *) cd ;

   if( ! IM3D_VALID(im3d) || GPT == NULL ) return ;
   if( av->ival < 0 || av->ival >= PALTAB_NUM(GPT) ) return ;

   load_PBAR_palette_array( im3d->vwid->func->inten_pbar ,
                            PALTAB_ARR(GPT,av->ival)      ) ;

   if( im3d->vinfo->func_visible )
      AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_OVERLAY ) ;  /* redraw */

   return ;
}
