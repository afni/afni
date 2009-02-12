/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include <stdio.h>
#include <string.h>
#include <errno.h>       /* 01 May 2003 - rickr */
#include <X11/keysym.h>  /* 24 Jan 2003 */

#undef IMSEQ_DEBUG

#include "mrilib.h"
#include "imseq.h"
#include "xutil.h"
#include "xim.h"

#if 0                   /* used to debug only this file */
# undef  DBG_trace
# define DBG_trace 2
#endif

#define DPRI(st,ijk) \
  if(PRINT_TRACING){ char str[256]; sprintf(str,"%s %d",st,ijk); STATUS(str); }

#define COLSIZE AV_colsize()  /* for optmenus -- 11 Dec 2001 */

#define DONT_ONOFF_ONE        /* 29 Jul 2002 */

#define SEND(sq,cb)                                     \
  AFNI_CALL_VOID_3ARG( (sq)->status->send_CB ,          \
                       MCW_imseq * , sq ,               \
                       XtPointer   , (sq)->getaux ,     \
                       ISQ_cbs *   , &(cb)          )

/************************************************************************
   Define the buttons and boxes that go in the "Disp" dialog
*************************************************************************/

/*-- structures defining action buttons (at bottom of dialog) --*/

#define NACT_DISP 2  /* number of action buttons */
#define DISP_OK   1  /* indices for button labels */
#define DISP_UNDO 0

static MCW_action_item ISQ_disp_act[NACT_DISP] = {
 {"Reset",ISQ_disp_act_CB,NULL,"Sets all options back\nto earlier values","Undo changes", 0 },
 {"Done" ,ISQ_disp_act_CB,NULL,"Closes this window"                      ,"Close window", 1 }
} ;

/*-- structures defining toggle button boxes --*/

                       /* number of buttons in each button box */
#define NBUT_DISP1  4  /* Rotation box */
#define NBUT_DISP2  1  /* Mirror box */
#define NBUT_DISP3  1  /* No overlay box */
#define NBUT_DISP4  3  /* Range scaling box */
#define NBUT_DISP5  2  /* Auto- or Group- scale box */
#define NBUT_DISP6  1  /* Free aspect box */
#define NBUT_DISP7  2  /* Save box */          /* 26 Jul 2001: was 3, now 2 */
#define NBUT_DISP8  3  /* IMPROC buttons */
#define NBUT_DISP9  4  /* CX buttons */

#define NTOG_ROT  0    /* index of which button box control which option(s) */
#define NTOG_MIR  1
#define NTOG_COL  2
#define NTOG_RNG  3
#define NTOG_SCL  4
#define NTOG_ASP  5
#define NTOG_SAV  6
#define NTOG_IMP  7
#define NTOG_CX   8

#define ALLOW_CLIPPING(ss,tt)                                    \
 do{ int za = (int)(tt) ;                                        \
     int zb = ( ISQ_REALZ(ss) && (ss)->dialog != NULL &&         \
                (ss)->dialog_starter == NBUT_DISP     &&         \
                (ss)->bbox[NTOG_RNG] != NULL            ) ;      \
     if(zb) SENSITIZE((ss)->bbox[NTOG_RNG]->wbut[2],za) ;        \
     if( (ss)->opt.scale_range == ISQ_RNG_CLIPPED && !za ){      \
       (ss)->opt.scale_range = ISQ_RNG_02TO98 ;                  \
       if(zb) MCW_set_bbox((ss)->bbox[NTOG_RNG],ISQ_RNG_02TO98); \
     } else if( za && (ss)->redo_clip ){                         \
       (ss)->opt.scale_range = ISQ_RNG_CLIPPED ;                 \
       if(zb) MCW_set_bbox((ss)->bbox[NTOG_RNG],ISQ_RNG_CLIPPED);\
     }                                                           \
 } while(0)

static char * ISQ_dl1[NBUT_DISP1] = {
   "No Rotation" , "CCW 90" , "Rot 180" , "CW 90" } ;
static char * ISQ_dl2[NBUT_DISP2] = { "+ LR Mirror" } ;
static char * ISQ_dl3[NBUT_DISP3] = { "No Overlay" } ;
static char * ISQ_dl4[NBUT_DISP4] = { "Min-to-Max" , "2%-to-98%" , "Clipped" } ;
static char * ISQ_dl5[NBUT_DISP5] = { "Autoscale" , "Groupscale" } ;
static char * ISQ_dl6[NBUT_DISP6] = { "Free Aspect" } ;
static char * ISQ_dl7[NBUT_DISP7] = { "Nsize Save" , "PNM Save" } ;
static char * ISQ_dl8[NBUT_DISP8] = { "Flatten" , "Sharpen" , "Edge Detect" } ;
static char * ISQ_dl9[NBUT_DISP9] = {
   "Complex->Mag" , "Complex->Arg" , "Complex->Real" , "Complex->Imag" } ;

static ISQ_boxdef ISQ_dispbb[] = {
   { NBUT_DISP1 , ISQ_dl1 , MCW_BB_radio_one  , MCW_BB_frame } ,
   { NBUT_DISP2 , ISQ_dl2 , MCW_BB_check      , MCW_BB_frame } ,
   { NBUT_DISP3 , ISQ_dl3 , MCW_BB_check      , MCW_BB_frame } ,
   { NBUT_DISP4 , ISQ_dl4 , MCW_BB_radio_one  , MCW_BB_frame } ,
   { NBUT_DISP5 , ISQ_dl5 , MCW_BB_radio_one  , MCW_BB_frame } ,
   { NBUT_DISP6 , ISQ_dl6 , MCW_BB_check      , MCW_BB_frame } ,
   { NBUT_DISP7 , ISQ_dl7 , MCW_BB_radio_zero , MCW_BB_frame } ,
   { NBUT_DISP8 , ISQ_dl8 , MCW_BB_check      , MCW_BB_frame } ,
   { NBUT_DISP9 , ISQ_dl9 , MCW_BB_radio_one  , MCW_BB_frame } ,
} ;

static char * ISQ_bb1_help[NBUT_DISP1] = {
   "Sets orientation to the\noriginal in the data set" ,
   "Rotate 90 degrees\ncounterclockwise\nfrom original" ,
   "Rotate 180 degrees\nfrom original" ,
   "Rotate 90 degrees\nclockwise\nfrom original"
} ;

static char * ISQ_bb1_hint[NBUT_DISP1] = {
   "No extra rotation of image" ,
   "90 degrees counterclockwise" ,
   "Rotate 180 degrees" ,
   "90 degrees clockwise"
} ;

static char * ISQ_bb2_help[NBUT_DISP2] = {
   "pressed IN means\nleft-right mirror AFTER rotation"
} ;

static char * ISQ_bb2_hint[NBUT_DISP2] = {
   "IN: mirror image AFTER rotation"
} ;

static char * ISQ_bb3_help[NBUT_DISP3] = {
   "pressed IN means\nturn color overlays off"
} ;

static char * ISQ_bb3_hint[NBUT_DISP3] = {
   "IN: turn color overlays off"
} ;

static char * ISQ_bb4_help[NBUT_DISP4] = {
 "Intensities mapped\nover full range of data\n(min->lowest,max->highest)" ,
 "Intensities mapped\nover partial range of data\n(%ages in data histogram)" ,
 "Intensities mapped\nover auto-clipped range\nof data in all images"
} ;

static char * ISQ_bb4_hint[NBUT_DISP4] = {
 "Background intensity = min to max pixel values" ,
 "Background intensity = 2% to 98% pixel values"  ,
 "Background intensity = auto-clipped from volume"
} ;

static char * ISQ_bb5_help[NBUT_DISP5] = {
   "Intensities mapped\nfor each image separately" ,
   "Intensities mapped\nfor all images in common"
} ;

static char * ISQ_bb5_hint[NBUT_DISP5] = {
   "Intensities computed for each slice" ,
   "Intensities computed for all slices at once"
} ;

static char * ISQ_bb6_help[NBUT_DISP6] = {
 "pressed IN means allow arbitrary resizing of window\n"
 "pressed OUT means restrict window aspect ratio"
} ;

static char * ISQ_bb6_hint[NBUT_DISP6] = {
 "IN: Allow arbitrary resizing of window"
} ;

static char * ISQ_bb7_help[NBUT_DISP7] = {
 "Nsize: IN = 'normal' (power of 2)  saved images sizes\n"
 "       OUT= 'natural' (data given) saved images sizes"   ,

 "PNM:   IN = saved images are color (PNM format)\n"
 "       OUT= saved images are background data only\n"
} ;

static char * ISQ_bb7_hint[NBUT_DISP7] = {
 "IN: Save background images in power-of-2 sizes" ,
 "IN: Save background images in PNM format"
} ;

static char * ISQ_bb8_help[NBUT_DISP8] = {
 "Flatten: IN = Flatten histogram of background\n"
 "         OUT= Don't flatten histogram\n"                      ,

 "Sharpen: IN = Apply sharpening filter to background\n"
 "         OUT= Don't apply sharpening filter"                  ,

 "Edge: IN = Use Sobel edge detection filter on background\n"
 "      OUT= Don't use Sobel edge detector"
} ;

static char * ISQ_bb8_hint[NBUT_DISP8] = {
 "Flatten histogram of background" ,
 "Apply sharpening filter to background" ,
 "Apply Sobel edge detector to background"
} ;

#define ISQ_CX_HELP                            \
  "Complex-> options control how complex-\n"   \
  "valued images are displayed:\n"             \
  "  ->Mag  == Display magnitude\n"            \
  "  ->Arg  == Display argument (phase)\n"     \
  "  ->Real == Display real part\n"            \
  "  ->Imag == Display imaginary part"

static char * ISQ_bb9_help[NBUT_DISP9] = {
  ISQ_CX_HELP , ISQ_CX_HELP , ISQ_CX_HELP , ISQ_CX_HELP
} ;

static char * ISQ_bb9_hint[NBUT_DISP9] = {
  "Display magnitude" ,
  "Display argument (phase)"
  "Display real part" ,
  "Display imaginary part"
} ;

static char ** ISQ_bb_allhelp[] = {
  ISQ_bb1_help , ISQ_bb2_help , ISQ_bb3_help ,
  ISQ_bb4_help , ISQ_bb5_help , ISQ_bb6_help ,
  ISQ_bb7_help , ISQ_bb8_help , ISQ_bb9_help
} ;

static char ** ISQ_bb_allhint[] = {
  ISQ_bb1_hint , ISQ_bb2_hint , ISQ_bb3_hint ,
  ISQ_bb4_hint , ISQ_bb5_hint , ISQ_bb6_hint ,
  ISQ_bb7_hint , ISQ_bb8_hint , ISQ_bb9_hint
} ;
/*************************************************************************/

/*------ 27 Jun 2001: external programs that may be of use ------*/

static char ** ppmto_filter  = NULL ;
static char ** ppmto_suffix  = NULL ;
static int   * ppmto_bval    = NULL ;
static int     ppmto_num     = -1 ;

static char *  ppmto_gif_filter  = NULL ;   /* 27 Jul 2001 */
static char *  ppmto_agif_filter = NULL ;

#define USE_GIFF  /* use Fixed colormap GIF for animations */
#ifdef  USE_GIFF
static char *  ppmto_giff_filter = NULL ;   /* 05 Oct 2004 */
#define GIFF_MAPFILE "Qwerty53211.ppm"
#endif

static char *  ppmto_mpeg_filter = NULL ;   /* 02 Aug 2001 */
static char *  ppmto_ppm_filter  = NULL ;

static char *  ppmto_jpg75_filter = NULL ;  /* 27 Mar 2002 */
static char *  ppmto_jpg95_filter = NULL ;  /* 28 Jul 2005 */
static char *  ppmto_png_filter   = NULL ;  /* 07 Dec 2006 */

 /* the first %s will be the list of input gif filenames     */
 /* the second %s is the single output animated gif filename */

#define GIFSICLE_SUFFIX    "-O2 -d %d -k 127 -l %%s > %%s"
#define WHIRLGIF_SUFFIX    "-time %d -loop %%s > %%s"
#define MPEG_ENCODE_SUFFIX "-realquiet %s"

#define DO_AGIF(sq) ((sq)->opt.save_agif)
#define DO_MPEG(sq) ((sq)->opt.save_mpeg)
#define DO_ANIM(sq) (DO_AGIF(sq) || DO_MPEG(sq))

#define ADDTO_PPMTO(pnam,suff,bbb)                                       \
  do{ ppmto_filter = (char **) realloc( ppmto_filter ,                   \
                                        sizeof(char *)*(ppmto_num+1) ) ; \
      ppmto_suffix = (char **) realloc( ppmto_suffix  ,                  \
                                        sizeof(char *)*(ppmto_num+1) ) ; \
      ppmto_bval   = (int *)   realloc( ppmto_bval    ,                  \
                                        sizeof(int)   *(ppmto_num+1) ) ; \
      ppmto_filter[ppmto_num] = (pnam) ;                                 \
      ppmto_suffix[ppmto_num] = (suff) ;                                 \
      ppmto_bval  [ppmto_num] = (bbb)  ; ppmto_num++ ;                   \
      if( dbg ) fprintf(stderr,"IMSAVE: filter '%s' for suffix '%s'\n",  \
                        (pnam) , (suff) ) ;                              \
  } while(0)

  /** 16 Nov 2004: warning messages when can't find Save filters? **/

#define CANT_FIND(nm,fm)                                                 \
 do{ if( !AFNI_noenv("AFNI_IMSAVE_WARNINGS") ){                          \
      if( ncant == 0 )                                                   \
       fprintf(stderr,"\n++++++++ IMAGE SAVE SETUP WARNINGS ++++++++\n");\
      fprintf(stderr,                                                    \
              "++ Can't find program %s for Save to %s\n",(nm),(fm)) ;   \
     } ncant++ ;                                                         \
 } while(0)

/*---- setup programs as filters: ppm stdin to some output file ----*/
void ISQ_setup_ppmto_filters(void)
{
   char *pg , *pg2 , *str , *eee ;
   int bv ;
   int dbg ;
   int ncant=0 , need_netpbm=0 ;  /* 16 Nov 2004 */
   int jpeg_compress;

   ppmto_num = 0 ; bv = ISQ_SAV_PNM ;

   dbg = AFNI_yesenv("AFNI_IMSAVE_DEBUG") ;  /* 03 Sep 2004 */

   /*-- the cheap way to write PPM  --*/
   /*-- [this must always be first] --*/

   pg = THD_find_executable( "cat" ) ;   /* should always find this! */
   if( pg != NULL ){
      str = AFMALL( char, strlen(pg)+32) ;
      sprintf(str,"%s > %%s",pg) ;
      bv <<= 1 ; ADDTO_PPMTO(str,"ppm",bv) ;

      /* 02 Aug 2001: also try for mpeg */

      ppmto_ppm_filter = str ;  /* save this filter string */

      pg = THD_find_executable( "mpeg_encode" ) ;
      if( pg != NULL ){
         str = AFMALL( char, strlen(pg)+64) ;
         sprintf(str,"%s %s",pg,MPEG_ENCODE_SUFFIX) ;
         ppmto_mpeg_filter = str ;
         if( dbg ) fprintf(stderr,"IMSAVE: animation filter '%s' for suffix '%s'\n",
                           str , "mpg" ) ;
      }
      else CANT_FIND("mpeg_encode","MPEG-1") ;
   }
   else CANT_FIND("cat","PPM") ;  /* this is the end of the world! */

   /*-- write JPEG --*/

   pg = THD_find_executable( "cjpeg" ) ;
   if( pg != NULL ){
   /* user environment variable compression quality - mod 5/10/2006 drg */
      eee = my_getenv("AFNI_JPEG_COMPRESS");
      if(eee!=NULL) {
         jpeg_compress = (int) strtod(eee, NULL);
         if((jpeg_compress<=5) || (jpeg_compress>100)) jpeg_compress = 95;
      }
      else jpeg_compress = 95;

#if 0
printf("\njpeg_compress %d\n", jpeg_compress);
#endif
      str = AFMALL( char, strlen(pg)+32) ;
      sprintf(str,"%s -quality %d > %%s",pg,jpeg_compress);
      bv <<= 1 ; ADDTO_PPMTO(str,"jpg",bv) ;
      ppmto_jpg95_filter = strdup(str) ;  /* 28 Jul 2005 */

      /* lower quality JPEGs */

      ppmto_jpg75_filter = AFMALL( char, strlen(pg)+32);
      sprintf(ppmto_jpg75_filter,"%s -quality 80 > %%s",pg) ;
   }
   else CANT_FIND("cjpeg","JPEG") ;

   /*-- write GIF --*/

   pg  = THD_find_executable( "ppmtogif" ) ;
   pg2 = THD_find_executable( "ppmquant" ) ;
   if( pg != NULL && pg2 != NULL ){
      int adel=20 ; char asuff[64] ;               /* 16 Jan 2003 */

      str = AFMALL( char, strlen(pg)+strlen(pg2)+32) ;
      sprintf(str,"%s 255 | %s > %%s",pg2,pg) ;
      bv <<= 1 ; ADDTO_PPMTO(str,"gif",bv) ;

      /*-- 27 Jul 2001: also try for Animated GIF --*/

      ppmto_gif_filter = str ;  /* save this filter string */

#ifdef USE_GIFF                       /* filter for Fixed GIF colormap */
      str = AFMALL( char , strlen(pg)+128 ) ;           /* 05 Oct 2004 */
      sprintf(str,"%s -map %s > %%s",pg,GIFF_MAPFILE) ;
      ppmto_giff_filter = str ;
#endif

      /* 16 Jan 2003: get animated GIF delay (centiseconds) from environment */

      eee = getenv( "AFNI_AGIF_DELAY" ) ;
      if( eee != NULL ){ adel=(int)strtod(eee,NULL); if(adel < 2)adel=20; }

      pg = THD_find_executable( "gifsicle" ) ;    /* preferred */
      if( pg != NULL ){
         sprintf(asuff,GIFSICLE_SUFFIX,adel) ;    /* 16 Jan 2003 */
         str = AFMALL( char, strlen(pg)+64) ;
         sprintf(str,"%s %s",pg,asuff) ;
         ppmto_agif_filter = str ;
         if( dbg ) fprintf(stderr,"IMSAVE: animation filter '%s' for suffix '%s'\n",
                           str , "gif" ) ;
      } else {
         pg = THD_find_executable( "whirlgif" ) ; /* but is OK */
         if( pg != NULL ){
            sprintf(asuff,WHIRLGIF_SUFFIX,adel) ; /* 16 Jan 2003 */
            str = AFMALL( char, strlen(pg)+64) ;
            sprintf(str,"%s %s",pg,asuff) ;
            ppmto_agif_filter = str ;
            if( dbg ) fprintf(stderr,"IMSAVE: animation filter '%s' for suffix '%s'\n",
                              str , "gif" ) ;
         }
      }
      if( ppmto_agif_filter == NULL )
        CANT_FIND("gifsicle OR whirlgif","Animated GIF") ;
   }
   else { CANT_FIND("ppmtogif AND/OR ppmquant","GIF"); need_netpbm++; }

   /*-- write TIFF --*/

   pg = THD_find_executable( "ppm2tiff" ) ;
   if( pg != NULL ){
      str = AFMALL( char, strlen(pg)+32) ;
      sprintf(str,"%s -c none %%s",pg) ;
      bv <<= 1 ; ADDTO_PPMTO(str,"tif",bv) ;
   } else {                                     /* 03 Jul 2001:      */
      pg = THD_find_executable( "pnmtotiff" ) ; /* must use ppm2tiff */
      if( pg != NULL ){                         /* and pnmtotiff     */
         str = AFMALL( char, strlen(pg)+32) ;   /* differently       */
         sprintf(str,"%s > %%s",pg) ;
         bv <<= 1 ; ADDTO_PPMTO(str,"tif",bv) ;
      }
      else { CANT_FIND("ppm2tiff OR pnmtotiff","TIFF"); need_netpbm++; }
   }

   /*-- write Windows BMP --*/

   pg  = THD_find_executable( "ppmtobmp" ) ;

   if( AFNI_yesenv("AFNI_OLD_PPMTOBMP") ){    /* the old way: quantize */
     pg2 = THD_find_executable( "ppmquant" ) ;
     if( pg != NULL && pg2 != NULL ){
        str = AFMALL( char, strlen(pg)+strlen(pg2)+32) ;
        sprintf(str,"%s 255 | %s -windows > %%s",pg2,pg) ;
        bv <<= 1 ; ADDTO_PPMTO(str,"bmp",bv) ;
     }
     else { CANT_FIND("ppmtobmp AND/OR ppmquant","BMP"); need_netpbm++; }
   } else if( pg != NULL ){                   /* 21 Feb 2003: don't quantize */
      str = AFMALL( char, strlen(pg)+32) ;
      sprintf(str,"%s -bpp 24 -windows > %%s",pg) ;
      bv <<= 1 ; ADDTO_PPMTO(str,"bmp",bv) ;
   }
   else { CANT_FIND("ppmtobmp","BMP"); need_netpbm++; }

   /*-- write Encapsulated PostScript --*/

   pg = THD_find_executable( "pnmtops" ) ;
   if( pg != NULL ){
      str = AFMALL( char, strlen(pg)+32) ;
      sprintf(str,"%s -noturn > %%s",pg) ;
      bv <<= 1 ; ADDTO_PPMTO(str,"eps",bv) ;
   }
   else { CANT_FIND("pnmtops","EPS"); need_netpbm++; }

#if 0
   /*-- write a PDF file (God only knows why) --*/

   pg2 = THD_find_executable( "epstopdf" ) ;   /* 19 Oct 2001:  */
   if( pg != NULL && pg2 != NULL ){            /* check pg!=NULL */
      str = AFMALL( char, strlen(pg)+strlen(pg2)+32) ;
      sprintf(str,"%s -noturn | %s --filter > %%s",pg,pg2) ;
      bv <<= 1 ; ADDTO_PPMTO(str,"pdf",bv) ;
   }
   else CANT_FIND("pnmtops AND/OR epstopdf","PDF") ;
#endif

   /*-- Write a PNG file (again, query God) --*/

   pg = THD_find_executable( "pnmtopng" ) ;
   if( pg != NULL ){
      str = AFMALL( char, strlen(pg)+32) ;
      sprintf(str,"%s -compression 9 > %%s",pg) ;
      bv <<= 1 ; ADDTO_PPMTO(str,"png",bv) ;
      ppmto_png_filter = strdup(str) ;  /* 07 Dec 2007 */
   }
   else { CANT_FIND("pnmtopng","PNG"); need_netpbm; }

   /*----- 16 Nov 2004: more warnings? -----*/

   if( !AFNI_noenv("AFNI_IMSAVE_WARNINGS") && ncant > 0 ){
     if( need_netpbm > 0 ){
       fprintf(stderr,
               "++ Some of the missing image Save programs are in\n"
               "++  the netpbm software package, which is freeware.\n" ) ;
#ifdef DARWIN
       fprintf(stderr,
               "++  The 'fink' package at http://fink.sourceforge.net/\n"
               "++  is a way to get the netpbm programs for OS X; *OR*\n");
#endif
       fprintf(stderr,
               "++  Netpbm can be found at http://netpbm.sourceforge.net/\n");
     }

     fprintf(stderr,
               "++ To disable these warnings, set environment\n"
               "++  variable AFNI_IMSAVE_WARNINGS to 'NO'.\n"
               "+++++++++++++++++++++++++++++++++++++++++++\n" ) ;
   }

   return ;
}

/*-------------------------------------------------------------------------
  routine to create a new window for displaying an image sequence:

  dc = pointer to a display context (MCW_DC)
         (stores all the X11 stuff, like the Display *).

  get_image = pointer to a routine that returns the images to be displayed.

              - get_image(n,type,aux) should return a "MRI_IMAGE *" of the
                 n-th image for n=0,1,...,nim-1, where "type" is one
                 of isqCR_getimage (for the underlay image) or
                    isqCR_getoverlay (for the overlay image);

              - get_image(n,isqCR_getstatus,aux) should return a
                   "MCW_imseq_status *" (n is ignored)

              - get_image(n,isqCR_getmemplot,aux) should return a
                   "MEM_plotdata *" (see coxplot.h -- 21 Feb 2001);
                   NULL means no plot

              - get_image(n,isqCR_getlabel,aux) should return a
                   "char *": a NUL-terminated string to be plotted
                   on top of the image; NULL means no label string

            Thus, get_image takes as input 2 "int"s and an "XtPointer",
            and returns a "XtPointer".  Note that the MRI_IMAGEs returned
            will be mri_free-d after being used internally.  Therefore,
            if you want to keep them, you should send a copy, not the
            original.  The same applies to the MCW_imseq_status struct,
            the MEM_plotdata struct, and the char *.

    aux = XtPointer supplied by user, pointing to data to be passed
            get_image for its own internal use (similar in concept to
            "client_data" for callbacks).
-------------------------------------------------------------------------*/

/*-- structure defining buttons in main window --*/

static const ISQ_bdef ISQ_but_bot_def[NBUTTON_BOT] = {  /* label, callback */
     { "Disp"     , ISQ_but_disp_CB } ,
     { "Save:bkg" , ISQ_but_save_CB } ,
     { "Mont"     , ISQ_montage_CB  } ,
     { "Done"     , ISQ_but_done_CB }
} ;

static const Boolean ISQ_but_bot_dial[NBUTTON_BOT] = {  /* use seq->dialog? */
   True , False , True , False
} ;

static char *ISQ_but_done_label1 = "Done" ;
static char *ISQ_but_done_label2 = "DONE" ;
#define NBUT_DONE (NBUTTON_BOT-1)
#define NBUT_SAVE 1
#define NBUT_DISP 0
#define NBUT_MONT 2

static char *ISQ_save_label_bg  = "Save:bkg" ;
static char *ISQ_save_label_all = "Save:pnm" ;

#define SET_SAVE_LABEL(seq)                                               \
  do{ char sl[16] ;                                                       \
      if( (seq)->opt.save_filter < 0 ){                                   \
         strcpy(sl, (seq)->opt.save_pnm ? ISQ_save_label_all              \
                                        : ISQ_save_label_bg );            \
      }else{                                                              \
         sprintf(sl,"Save.%.3s",ppmto_suffix[(seq)->opt.save_filter]) ;   \
      }                                                                   \
           if( (seq)->opt.save_agif ) strcpy(sl,"Sav:aGif") ;             \
      else if( (seq)->opt.save_mpeg ) strcpy(sl,"Sav:mpeg") ;             \
      else if( (seq)->opt.save_one  ) sl[3] = '1' ;                       \
      MCW_set_widget_label( (seq)->wbut_bot[NBUT_SAVE] , sl ) ; } while(0)

static const ISQ_bdef ISQ_but_rig_def[NBUTTON_RIG] = {
     { "Colr" , ISQ_but_color_CB } ,
     { "Swap" , ISQ_but_cswap_CB } ,
     { "Norm" , ISQ_but_cnorm_CB }
} ;

/* popup help for these buttons */

static char * ISQ_but_bot_hint[NBUTTON_BOT] = {
   "Extra image controls" ,
   "Save images controls" ,
   "Image montage controls" ,
   "Close window"
} ;

static char * ISQ_but_bot_help[NBUTTON_BOT] = {
   "Pops up a window with options\n"
   "to control the image display\n"
   "and how the Save button works"   ,

   "Will popup control panels to let you save images from this window.\n"
   "The type of save operation is indicated on the button label, and\n"
   "is selected from the 'Disp' button options window.\n"
   " :bkg = Will save only the background image data values\n"
   "        (in a recorder window, the background image IS in color)\n"
   " :pnm = Will save the actual displayed focus image in color (PNM format)\n"
   "NOTES:\n"
   " * Saved images will NOT be stretched to match window resizing.\n"
   " * The PNM format requires the 'netpbm' package to be useful.\n"
   "    Alternatively, the 'xv' program will read/write PNM images." ,

   "Will popup a control box to let you\n"
   "display a montage of images, instead\n"
   "of just one image at a time.\n\n"
   "WARNING: this can be quite slow!"   ,

   "Closes this\n"
   "viewing window"
} ;

static char * ISQ_but_rig_help[NBUTTON_RIG] = {
   "Switches the colormap\nbetween False Color\nand Grayscale" ,
   "Swaps the colormap\nend for end" ,
   "Restores the colormap\nto its `normal' state"
} ;

static char * ISQ_but_rig_hint[NBUTTON_RIG] = {
   "Switch between color and gray" ,
   "Invert color/gray levels" ,
   "Return color/gray scale to normal"
} ;

static char * ISQ_scale_help =
  "Moves between images:\nDrag bar, or click in trough" ;

static char * ISQ_default_image_help = "This is the image!" ;

static char * ISQ_form_help =
     "************************************************\n"
     "* Image Sequence Display Module                *\n"
     "*                                              *\n"
     "* Copyright 1994, Medical College of Wisconsin *\n"
     "*          -2000  Milwaukee, WI 53226-0509     *\n"
     "* Released under the GPL (v2)                  *\n"
     "*                                              *\n"
     "* Author:  Robert W Cox, PhD                   *\n"
     "************************************************"   ;

/*-- arrow definitions --*/

static char * ISQ_arrow_label[NARROW] = { "c" , "b" , "r" , "g" , "i" } ;

#define NARR_SQUEEZE 0  /* arrow action codes */
#define NARR_BRIGHT  1
#define NARR_ROTATE  2
#define NARR_GAMMA   3
#define NARR_FRAC    4

static char * ISQ_arrow_help[NARROW] = {
   "Change constrast\nin colormap" ,
   "Change brightness\nin colormap" ,
   "Rotate\ncolormap" ,
   "Alter\ndisplay\ngamma" ,
   "Alter\nimage\nfraction\nin window"
} ;

static char * ISQ_arrow_hint[NARROW] = {
   "Contrast" ,
   "Brightness" ,
   "Rotate" ,
   "Gamma" ,
   "Image fraction"
} ;

/*........................................................................*/

static int ISQ_anim_dup = 0 ;
void ISQ_set_anim_dup( int ii ){ ISQ_anim_dup = ii ; }

/*........................................................................*/

#define DEFAULT_MINFRAC 0.02
#define DEFAULT_MAXFRAC 0.90

#define OPACITY_FAC  0.11111  /* 06-07 Mar 2001: overlay opacity stuff */
#define OPACITY_BOT  1
#define OPACITY_TOP  9

#define ZOOM_BOT  1          /* 11 Mar 2002: zoom controls */
#define ZOOM_TOP  4

MCW_imseq * open_MCW_imseq( MCW_DC *dc ,
                            get_ptr get_image , XtPointer aux )
{
   MCW_imseq        *newseq ;
   MCW_imseq_status *imstatus=NULL ;
   int ii , xwide , yhigh , one_image ;
   float fac ;
   MRI_IMAGE *tim=NULL ;
   float minfrac=DEFAULT_MINFRAC ; char *eee ; /* 27 Feb 2001 */
   Widget wtemp ;                              /* 11 Mar 2002 */
   float maxfrac=DEFAULT_MAXFRAC ;             /* 13 Jun 2003 */

ENTRY("open_MCW_imseq") ;

#define ERREX { myXtFree(newseq) ; XBell(dc->display,100) ; RETURN(NULL) ; }

   /*- 27 Jun 2001: setup filters for saving images -*/

   if( ppmto_num < 0 ){
      ISQ_setup_ppmto_filters() ;  /* get filter program names */

      if( ppmto_num > 0 ){         /* modify Save button box setup */

         int nbut_old     = ISQ_dispbb[NTOG_SAV].nbut , qq,pp ;
         char ** lbut_old = ISQ_dispbb[NTOG_SAV].lbut ;
         char ** help_old = ISQ_bb_allhelp[NTOG_SAV] ;
         char ** hint_old = ISQ_bb_allhint[NTOG_SAV] ;

         ISQ_dispbb[NTOG_SAV].nbut += ppmto_num ;
         ISQ_dispbb[NTOG_SAV].lbut  = (char **) malloc(sizeof(char *)
                                                       *ISQ_dispbb[NTOG_SAV].nbut);
         for( qq=0 ; qq < nbut_old ; qq++ )
            ISQ_dispbb[NTOG_SAV].lbut[qq] = lbut_old[qq] ;
         for( pp=0 ; pp < ppmto_num ; pp++,qq++ ){
            ISQ_dispbb[NTOG_SAV].lbut[qq] = AFMALL( char, 32) ;
            sprintf(ISQ_dispbb[NTOG_SAV].lbut[qq] ,
                    "Save to .%.3s(s)" , ppmto_suffix[pp] ) ;
         }

         ISQ_bb_allhelp[NTOG_SAV] = (char **) malloc(sizeof(char *)
                                                     *ISQ_dispbb[NTOG_SAV].nbut);
         ISQ_bb_allhint[NTOG_SAV] = (char **) malloc(sizeof(char *)
                                                     *ISQ_dispbb[NTOG_SAV].nbut);
         for( qq=0 ; qq < nbut_old ; qq++ ){
            ISQ_bb_allhelp[NTOG_SAV][qq] = help_old[qq] ;
            ISQ_bb_allhint[NTOG_SAV][qq] = hint_old[qq] ;
         }
         for( pp=0 ; pp < ppmto_num ; pp++,qq++ )
            ISQ_bb_allhelp[NTOG_SAV][qq] = ISQ_bb_allhint[NTOG_SAV][qq] = NULL ;
      }
   }

   newseq = (MCW_imseq *) XtMalloc( sizeof(MCW_imseq) ) ;  /* new structure */
   memset(newseq, 0, sizeof(MCW_imseq));
   
   newseq->dc     = dc ;               /* copy input pointers */
   newseq->getim  = get_image ;
   newseq->getaux = aux ;

   newseq->never_drawn = 1 ;

#if 0
   imstatus = (MCW_imseq_status *) get_image(0,isqCR_getstatus,aux) ;
#else
   AFNI_CALL_VALU_3ARG( get_image , MCW_imseq_status *,imstatus ,
                        int,0 , int,isqCR_getstatus , XtPointer,aux ) ;
#endif
   if( imstatus->num_total < 1 ){ ERREX ; }
   one_image = (imstatus->num_total == 1) ;

#if 0
   tim = (MRI_IMAGE *) get_image(0,isqCR_getqimage,aux) ;  /* fake image */
#else
   AFNI_CALL_VALU_3ARG( get_image , MRI_IMAGE *,tim ,
                        int,0 , int,isqCR_getqimage , XtPointer,aux ) ;
#endif

   newseq->horig = tim->nx ;  /* save original dimensions */
   newseq->vorig = tim->ny ;

   newseq->cropit       =  0 ; /* 11 Jun 2002 */
   newseq->crop_allowed =  1 ;
   newseq->crop_nxorg   = newseq->crop_nyorg = -1 ;

   newseq->last_width_mm  = IM_WIDTH(tim) ;  /* dimensions in real space */
   newseq->last_height_mm = IM_HEIGHT(tim) ;

   newseq->last_dx = newseq->last_dy = 1.0 ; /* 08 Jun 2004 */
   newseq->rgb_gamma  = 1.0 ;                /* 25 Apr 2005 */
   newseq->rgb_offset = 0.0 ;

   fac = (newseq->last_width_mm  / newseq->horig)    /* width per pixel over */
        /(newseq->last_height_mm / newseq->vorig) ;  /* height per pixel */

   if( fac >= 1.0 ){                                 /* initial display size */
      xwide = newseq->horig * fac + 0.49 ;
      yhigh = newseq->vorig ;
   } else {
      xwide = newseq->horig ;
      yhigh = newseq->vorig / fac + 0.49 ;
   }

if( PRINT_TRACING ){
  char str[256] ;
  sprintf(str,"nx=%d ny=%d dx=%f dy=%f wid=%f hei=%f xwide=%d yhigh=%d",
              tim->nx,tim->ny,tim->dx,tim->dy,newseq->last_width_mm,
              newseq->last_height_mm , xwide,yhigh ) ;
  STATUS(str);
}

   KILL_1MRI(tim) ;  /* don't need tim no more */

   newseq->hbase  = newseq->hactual =
                    newseq->old_hact = xwide ;   /* store display sizes */

   newseq->vbase  = newseq->vactual =
                    newseq->old_vact = yhigh ;

   newseq->status = imstatus ;
   newseq->im_nr  = imstatus->num_total / 2 ;  /* do this image 1st */
   newseq->scl    = 0.0 ;                      /* autoscaling */
   newseq->lev    = dc->ncol_im-1 ;            /* to range 0..ncol_im-1 */
   newseq->bot    = 0 ;
   newseq->top    = dc->ncol_im-1 ;

   newseq->clbot  = newseq->cltop  = 0.0f ;     /* 29 Jul 2001 */
   newseq->barbot = newseq->bartop = 0.0f ;

   strcpy( newseq->im_label , "hi bob" ) ;
   newseq->scl_label[0] = '\0' ;

   /* set display processing options */

   ISQ_DEFAULT_OPT(newseq->opt) ;  /* 09 Oct 1998: macro replaces explicit code */
   if( ppmto_num > 0 ) newseq->opt.save_filter = 0 ;  /* 26 Mar 2002 */
   newseq->opt.parent = (XtPointer) newseq ;
   newseq->old_opt    = newseq->opt ;         /* backup copy */

   newseq->last_image_type = -1 ;     /* not a legal datum type */

   newseq->dialog         = NULL ;               /* no dialog at present */
   newseq->num_bbox       = 0 ;
   newseq->dialog_starter = -1 ;
   newseq->dont_place_dialog = 0 ;         /* 23 Jan 2004 */

   newseq->imim = newseq->ovim = NULL ;    /* NULL out all images */

   newseq->orim      = NULL ;              /* 30 Dec 1998 */
   newseq->set_orim  = 0 ;
   newseq->need_orim = 0 ;

   newseq->given_xim = newseq->sized_xim
                     = newseq->given_xbar
                     = newseq->sized_xbar = NULL ;

   /* Feb 1998: button2 drawing stuff */

   newseq->button2_enabled  = 0 ;
   newseq->button2_active   = 0 ;
   newseq->button2_pixel    = dc->ovc->pixov_greenest ;
   newseq->button2_drawmode = BUTTON2_OPENPOLY ;
   newseq->button2_width    =  0 ;  /* 08 Oct 2002 */
   newseq->wimage_width     = -1 ;
   newseq->wimage_height    = -1 ;

   newseq->cursor_state     = CURSOR_NORMAL ;   /* 10 Mar 2003 */

   /* initialize image statistics */

   newseq->imstat = (ISQ_indiv_statistics *)
                    XtMalloc( sizeof(ISQ_indiv_statistics)
                              * imstatus->num_total ) ;

   newseq->glstat = (ISQ_glob_statistics * )
                    XtMalloc( sizeof(ISQ_glob_statistics) ) ;

   for( ii=0 ; ii < imstatus->num_total ; ii++ ){
     newseq->imstat[ii].one_done = newseq->imstat[ii].glob_done = False ;
     newseq->imstat[ii].parent   = (XtPointer) newseq ;
   }

   newseq->glstat->parent = (XtPointer) newseq ;

   for( ii=0 ; ii < NHISTOG ; ii++ )
      newseq->glstat->hist[ii] = 0 ;  /* initialize */

   newseq->glstat->mm_done =
     newseq->glstat->per_done = (newseq->status->num_series < 2 ) ;

#ifdef AUTOMATE_STATISTICS
   if( newseq->glstat->mm_done ){
      newseq->glstat->worker = 0 ;
   } else {
      newseq->glstat->worker = XtAppAddWorkProc(
                                  newseq->dc->appcontext ,
                                  ISQ_statistics_WP , newseq ) ;
   }
#else
   newseq->glstat->worker = 0 ;
#endif

   /***--------- create widgets ---------- ***/

   newseq->image_frac = IMAGE_FRAC ;  /* 25 Oct 1996 */

   /** 27 Feb 2001: set minimum size for image windows,
                    as a fraction of the overall screen area **/

   eee = my_getenv("AFNI_IMAGE_MINFRAC") ;
   if( eee != NULL ){
      float fff=0.0 ;
      ii = sscanf(eee,"%f",&fff) ;
      if( ii > 0 && fff > 0.0 && fff <= 0.9 ) minfrac = fff ;
      else                                    minfrac = DEFAULT_MINFRAC ;
   }

   eee = my_getenv("AFNI_IMAGE_MAXFRAC") ;
   if( eee != NULL ){
      float fff=0.0 ;
      ii = sscanf(eee,"%f",&fff) ;
      if( ii > 0 && fff > 0.0 && fff <= 1.0 ) maxfrac = fff ;
      else                                    maxfrac = DEFAULT_MAXFRAC ;
   }

   { float xxx = newseq->hactual , yyy = newseq->vactual ;
     float fff = (xxx*yyy)/(dc->width*dc->height) , ggg ;

     /* modify if window too small for display */

     if( fff < minfrac ){
       fff = sqrt(minfrac/fff); xxx *= fff; yyy *= fff;   /* expand area */
     }

     /* modify if window too big for display */

     fff = ggg = 1.0 ;
     if( xxx >= maxfrac*dc->width ) fff = maxfrac*dc->width / xxx; /* don't let  */
     if( yyy >= maxfrac*dc->height) ggg = maxfrac*dc->height/ yyy; /* be too big */
     fff = MIN(fff,ggg) ; xxx *= fff ; yyy *= fff ;
     if( xxx < 1.0 || yyy < 1.0 ){                     /* weird result?? */
       xxx = newseq->hactual ; yyy = newseq->vactual; /* back to old way */
     }
     xwide = (int) ( 0.49 + xxx / IMAGE_FRAC ) ;
     yhigh = (int) ( 0.49 + yyy / IMAGE_FRAC ) ;

     fff = ggg = 1.0 ;
     if( xwide >= maxfrac*dc->width ) fff = maxfrac*dc->width / xwide; /* don't let  */
     if( yhigh >= maxfrac*dc->height) ggg = maxfrac*dc->height/ yhigh; /* be too big */
     fff = MIN(fff,ggg) ; xwide *= fff ; yhigh *= fff ;
   }

   /* toggles for widget controls on or off */

   newseq->onoff_num   = 0 ;
   newseq->onoff_state = 1 ;  /* initially are on */

   /* top level shell to hold all */

   newseq->wtop =
      XtVaAppCreateShell(
           "AFNI" , "AFNI" ,
           topLevelShellWidgetClass , dc->display ,

           XmNminAspectX , xwide ,      /* fix aspect ratio! */
           XmNminAspectY , yhigh ,
           XmNmaxAspectX , xwide ,
           XmNmaxAspectY , yhigh ,

           XmNmaxWidth   , dc->width ,  /* not bigger than the screen! */
           XmNmaxHeight  , dc->height ,

           XmNdeleteResponse , XmDO_NOTHING , /* deletion handled below */

           XmNallowShellResize , False ,       /* let code resize shell */

           XmNinitialResourcesPersistent , False ,
      NULL ) ;

   DC_yokify( newseq->wtop , dc ) ;  /* 14 Sep 1998 */

#if 1
   if( MCW_isitmwm( newseq->wtop ) )
      XtVaSetValues( newseq->wtop ,
                        XmNmwmDecorations , MWM_DECOR_ALL | MWM_DECOR_MAXIMIZE ,
                     NULL ) ;
#endif

   XmAddWMProtocolCallback(           /* make "Close" window menu work */
           newseq->wtop ,
           XmInternAtom( dc->display , "WM_DELETE_WINDOW" , False ) ,
           ISQ_but_done_CB , newseq ) ;

   newseq->done_first = True ;  /* for the first press of "Done" */

   /* form to attach all contents to */

   newseq->wform =
      XtVaCreateWidget(
           "imseq" , xmFormWidgetClass , newseq->wtop ,

            XmNwidth  , xwide ,      /* initial size */
            XmNheight , yhigh ,

            XmNborderWidth , 0 ,

            XmNfractionBase , FORM_FRAC_BASE ,

            XmNhorizontalSpacing , 0 ,  /* 17 Jun 2002 */
            XmNverticalSpacing   , 0 ,

            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
      NULL ) ;

   MCW_register_help( newseq->wform , ISQ_form_help ) ;

   /* drawing area for image space */

   newseq->wimage =
       XtVaCreateManagedWidget(
         "imseq" , xmDrawingAreaWidgetClass , newseq->wform ,

          XmNtopAttachment    , XmATTACH_FORM ,
          XmNleftAttachment   , XmATTACH_FORM ,
          XmNrightAttachment  , XmATTACH_POSITION ,
          XmNrightPosition    , (int)( 0.49 + IMAGE_FRAC * FORM_FRAC_BASE ) ,
          XmNbottomAttachment , XmATTACH_POSITION ,
          XmNbottomPosition   , (int)( 0.49 + IMAGE_FRAC * FORM_FRAC_BASE ) ,

          XmNtraversalOn , False ,
          XmNinitialResourcesPersistent , False ,
       NULL ) ;

   XtInsertEventHandler( newseq->wimage ,      /* handle events in image */

                            0
                          | KeyPressMask        /* get keystrokes */
                          | ButtonPressMask     /* button presses */
                          | ExposureMask        /* exposures */
                          | StructureNotifyMask /* resizes */
                          | Button1MotionMask   /* motion while #1 is down */
                          | ButtonReleaseMask   /* button releases */
                         ,
                         FALSE ,                /* nonmaskable events? */
                         ISQ_drawing_EV ,       /* super-handler! */
                         (XtPointer) newseq ,   /* client data */
                         XtListTail ) ;         /* last in queue */

   strcpy( newseq->im_helptext , ISQ_default_image_help ) ;
   newseq->im_helptext[ISQ_NHELP] = '\0' ;

   MCW_register_help( newseq->wimage , newseq->im_helptext ) ;

   /* all pushbuttons (these are next so they overlay the scale and bar) */

   for( ii=0 ; ii < NBUTTON_BOT ; ii++){

      Arg wa[30] ;
      int na ;

      na = 0 ;

      XtSetArg( wa[na] , XmNmarginWidth   , 1     ) ; na++ ;
      XtSetArg( wa[na] , XmNmarginHeight  , 0     ) ; na++ ;
      XtSetArg( wa[na] , XmNmarginBottom  , 0     ) ; na++ ;
      XtSetArg( wa[na] , XmNmarginTop     , 0     ) ; na++ ;
      XtSetArg( wa[na] , XmNmarginLeft    , 0     ) ; na++ ;
      XtSetArg( wa[na] , XmNmarginRight   , 0     ) ; na++ ;
      XtSetArg( wa[na] , XmNtraversalOn   , False ) ; na++ ;
      XtSetArg( wa[na] , XmNrecomputeSize , False ) ; na++ ;

      XtSetArg( wa[na] , XmNinitialResourcesPersistent , False ) ; na++ ;

      /* attach all buttons to edge of form */

      XtSetArg( wa[na] , EDGING_BOT , XmATTACH_FORM ) ; na++ ;

      if( ii == 0 ){  /* attach 1st button to leading edge of form */

         XtSetArg( wa[na] , LEADING_BOT , XmATTACH_FORM ) ; na++ ;

      } else if( ii == NBUTTON_BOT-1 ){  /* last button */

         XtSetArg(wa[na],LEADING_BOT       ,XmATTACH_WIDGET)        ; na++ ;
         XtSetArg(wa[na],LEADING_WIDGET_BOT,newseq->wbut_bot[ii-1]) ; na++ ;

      } else {  /* other buttons to the widget to their LEADING edge */

         XtSetArg(wa[na],LEADING_BOT       ,XmATTACH_WIDGET )       ; na++ ;
         XtSetArg(wa[na],LEADING_WIDGET_BOT,newseq->wbut_bot[ii-1] ); na++ ;
      }

      newseq->onoff_widgets[(newseq->onoff_num)++] =
      newseq->wbut_bot[ii] =
         XtCreateManagedWidget(
               ISQ_but_bot_def[ii].name ,
               xmPushButtonWidgetClass , newseq->wform ,
               wa , na ) ;

      if( ii == NBUT_DONE )   /* added 3/25/95 */
         MCW_set_widget_bg( newseq->wbut_bot[ii] ,
                            MCW_hotcolor(newseq->wbut_bot[ii]) , 0 ) ;

      XtAddCallback( newseq->wbut_bot[ii] , XmNactivateCallback ,
                     ISQ_but_bot_def[ii].func_CB , newseq ) ;

      MCW_register_help( newseq->wbut_bot[ii] , ISQ_but_bot_help[ii] ) ;
      MCW_register_hint( newseq->wbut_bot[ii] , ISQ_but_bot_hint[ii] ) ;
   }
   SET_SAVE_LABEL(newseq) ;

   /* 27 Jun 2001: popup menu for Save: button */

   if( ppmto_num > 0 )
     XtInsertEventHandler( newseq->wbut_bot[NBUT_SAVE] ,
                           ButtonPressMask ,    /* button presses */
                           FALSE ,              /* nonmaskable events? */
                           ISQ_butsave_EV ,     /* handler */
                           (XtPointer) newseq , /* client data */
                           XtListTail           /* last in queue */
                          ) ;

   /* 24 Apr 2001: initialize recording stuff */

   ISQ_record_button( newseq ) ;

   /* buttons on right */

   STATUS("creating buttons on right") ;
   for( ii=0 ; ii < NBUTTON_RIG ; ii++){

      Arg wa[30] ;
      int na ;

      na = 0 ;

      XtSetArg( wa[na] , XmNmarginWidth   , 1     ) ; na++ ;
      XtSetArg( wa[na] , XmNmarginHeight  , 0     ) ; na++ ;
      XtSetArg( wa[na] , XmNmarginBottom  , 0     ) ; na++ ;
      XtSetArg( wa[na] , XmNmarginTop     , 0     ) ; na++ ;
      XtSetArg( wa[na] , XmNmarginLeft    , 0     ) ; na++ ;
      XtSetArg( wa[na] , XmNmarginRight   , 0     ) ; na++ ;
      XtSetArg( wa[na] , XmNtraversalOn   , False ) ; na++ ;
      XtSetArg( wa[na] , XmNrecomputeSize , False ) ; na++ ;

      XtSetArg( wa[na] , XmNinitialResourcesPersistent , False ) ; na++ ;

      /* attach all buttons to edge of form */

      XtSetArg( wa[na] , EDGING_RIG , XmATTACH_FORM ) ; na++ ;

      if( ii == 0 ){  /* attach 1st button to leading edge of form */

         XtSetArg( wa[na] , LEADING_RIG , XmATTACH_FORM ) ; na++ ;

      } else {  /* other buttons to the widget to their LEADING edge */

         XtSetArg(wa[na],LEADING_RIG       ,XmATTACH_WIDGET        ); na++ ;
         XtSetArg(wa[na],LEADING_WIDGET_RIG,newseq->wbut_rig[ii-1] ); na++ ;
      }

      newseq->onoff_widgets[(newseq->onoff_num)++] =
      newseq->wbut_rig[ii] =
         XtCreateManagedWidget(
               ISQ_but_rig_def[ii].name ,
               xmPushButtonWidgetClass , newseq->wform ,
               wa , na ) ;

      XtAddCallback( newseq->wbut_rig[ii] , XmNactivateCallback ,
                     ISQ_but_rig_def[ii].func_CB , newseq ) ;

      MCW_register_help( newseq->wbut_rig[ii] , ISQ_but_rig_help[ii] ) ;
      MCW_register_hint( newseq->wbut_rig[ii] , ISQ_but_rig_hint[ii] ) ;
   }

   /* arrows on right */

   STATUS("creating arrows on right") ;
   for( ii=0 ; ii < NARROW ; ii++ ){

      newseq->arrow[ii] = new_MCW_arrowval(
                             newseq->wform , ISQ_arrow_label[ii] ,
                             MCW_AV_downup , 0,0,0 ,
                             MCW_AV_notext , 0 ,
                             ISQ_arrow_CB , (XtPointer) newseq ,
                             NULL,NULL ) ;

      newseq->onoff_widgets[(newseq->onoff_num)++] = newseq->arrow[ii]->wrowcol ;

      XtVaSetValues( newseq->arrow[ii]->wrowcol ,
                        EDGING_RIG   , XmATTACH_FORM ,
                        LEADING_RIG  , XmATTACH_WIDGET ,

                        LEADING_WIDGET_RIG ,
                           (ii==0) ? (newseq->wbut_rig[NBUTTON_RIG-1])
                                   : (newseq->arrow[ii-1]->wrowcol) ,
                     NULL ) ;

      if( ii != NARR_FRAC )
         newseq->arrow[ii]->fastdelay = 10 ;                 /* fast */
      newseq->arrow[ii]->parent       = (XtPointer) newseq ; /* set parent */

      MCW_reghelp_children( newseq->arrow[ii]->wrowcol, ISQ_arrow_help[ii] );
      MCW_reghint_children( newseq->arrow[ii]->wrowcol, ISQ_arrow_hint[ii] );
   }

   /** 07 Mar 2001 - add opacity control for overlay, maybe **/

   wtemp = newseq->arrow[NARROW-1]->wrowcol ;  /* 11 Mar 2002 */

   newseq->ov_opacity = 1.0 ;  /* 06 Mar 2001 */

   if( newseq->dc->visual_class == TrueColor ){
     int iov = (int)rint(newseq->ov_opacity/OPACITY_FAC) ;
     char *buf = ISQ_opacity_label(iov) ;

     /** 08 Mar 2001 - put a line between the arrows above and this control **/

     STATUS("creating opacity control") ;
     newseq->ov_opacity_sep = XtVaCreateManagedWidget(
                                "imseq" , xmSeparatorWidgetClass , newseq->wform ,
                                   XmNseparatorType , XmSINGLE_LINE ,
                                   EDGING_RIG   , XmATTACH_FORM ,
                                   LEADING_RIG  , XmATTACH_WIDGET ,
                                   LEADING_WIDGET_RIG , wtemp ,
                                   XmNleftAttachment , XmATTACH_OPPOSITE_WIDGET ,
                                   XmNleftWidget , wtemp ,
                                   XmNleftOffset , 7 ,
                                NULL ) ;
     newseq->onoff_widgets[(newseq->onoff_num)++] = newseq->ov_opacity_sep ;

     newseq->ov_opacity_av = new_MCW_arrowval(
                               newseq->wform ,        /* parent */
                               buf ,                  /* label */
                               MCW_AV_downup ,        /* direction */
                               OPACITY_BOT ,          /* min */
                               OPACITY_TOP ,          /* max */
                               iov ,                  /* init */
                               MCW_AV_notext ,        /* type */
                               0 ,                    /* decim */
                               ISQ_opacity_CB ,       /* action CB */
                               (XtPointer) newseq ,   /* and its data */
                               NULL ,                 /* text maker */
                               NULL                   /* and its data */
                             ) ;

     newseq->ov_opacity_av->parent = (XtPointer) newseq ;
     newseq->onoff_widgets[(newseq->onoff_num)++] = newseq->ov_opacity_av->wrowcol ;

     XtVaSetValues( newseq->ov_opacity_av->wrowcol ,
                      EDGING_RIG   , XmATTACH_FORM ,
                      LEADING_RIG  , XmATTACH_WIDGET ,
                      LEADING_WIDGET_RIG , newseq->ov_opacity_sep ,
                    NULL ) ;

     wtemp = newseq->ov_opacity_av->wrowcol ; /* 11 Mar 2002 */

     MCW_reghelp_children( newseq->ov_opacity_av->wrowcol,
                           "Controls the opacity\n"
                           "of the color overlay:\n"
                           "  1 = barely visible  \n"
                           "  9 = totally opaque"   ) ;
     MCW_reghint_children( newseq->ov_opacity_av->wrowcol, "Color overlay opacity" );

   } else {
     newseq->ov_opacity_av  = NULL ;
     newseq->ov_opacity_sep = NULL ;
   }

   STATUS("creating zoom control") ;
     newseq->zoom_sep = XtVaCreateManagedWidget(
                         "imseq" , xmSeparatorWidgetClass , newseq->wform ,
                            XmNseparatorType , XmSINGLE_LINE ,
                            EDGING_RIG   , XmATTACH_FORM ,
                            LEADING_RIG  , XmATTACH_WIDGET ,
                            LEADING_WIDGET_RIG , wtemp ,
                            XmNleftAttachment , XmATTACH_OPPOSITE_WIDGET ,
                            XmNleftWidget , wtemp ,
                            XmNleftOffset , 7 ,
                         NULL ) ;
     newseq->onoff_widgets[(newseq->onoff_num)++] = newseq->zoom_sep ;

     newseq->zoom_val_av = new_MCW_arrowval(
                               newseq->wform ,        /* parent */
                               "z" ,                  /* label */
                               MCW_AV_downup ,        /* direction */
                               ZOOM_BOT ,             /* min */
                               ZOOM_TOP ,             /* max */
                               ZOOM_BOT ,             /* init */
                               MCW_AV_notext ,        /* type */
                               0 ,                    /* decim */
                               ISQ_zoom_av_CB ,       /* action CB */
                               (XtPointer) newseq ,   /* and its data */
                               NULL ,                 /* text maker */
                               NULL                   /* and its data */
                             ) ;
     newseq->zoom_val_av->parent = (XtPointer) newseq ;
     newseq->onoff_widgets[(newseq->onoff_num)++] = newseq->zoom_val_av->wrowcol ;
     XtVaSetValues( newseq->zoom_val_av->wrowcol ,
                      EDGING_RIG   , XmATTACH_FORM ,
                      LEADING_RIG  , XmATTACH_WIDGET ,
                      LEADING_WIDGET_RIG , newseq->zoom_sep ,
                    NULL ) ;
     MCW_reghelp_children( newseq->zoom_val_av->wrowcol,
                           "- Images can be zoomed in by\n"
                           "   a factor of 2, 3, or 4.\n"
                           "- These 'Z' buttons change\n"
                           "   the zoom factor up and down.\n"
                           "- Panning the zoomed image is\n"
                           "   done by pressing the 'pan'\n"
                           "   button and then clicking and\n"
                           "   dragging with Button #1 down\n\n"
                           "**WARNING: zooming in by 4 can\n"
                           "   consume so much memory that\n"
                           "   AFNI or the X11 server will\n"
                           "   crash.  If this happens, then\n"
                           "   avoid zooming that much (duh).\n" ) ;
     MCW_reghint_children( newseq->zoom_val_av->wrowcol, "Image zoom factor" );
     AV_SENSITIZE_DOWN( newseq->zoom_val_av , False ) ;

     STATUS("creating pan control") ;
     newseq->zoom_drag_pb =
        XtVaCreateManagedWidget(
           "imseq" , xmPushButtonWidgetClass , newseq->wform ,
            LABEL_ARG("pan") ,
            XmNmarginWidth   , 1 ,
            XmNmarginHeight  , 0 ,
            XmNmarginBottom  , 0 ,
            XmNmarginTop     , 0 ,
            XmNmarginLeft    , 0 ,
            XmNmarginRight   , 0 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;
     XtAddCallback( newseq->zoom_drag_pb , XmNactivateCallback ,
                    ISQ_zoom_pb_CB , newseq ) ;

     newseq->onoff_widgets[(newseq->onoff_num)++] = newseq->zoom_drag_pb ;

     XtVaSetValues( newseq->zoom_drag_pb ,
                      EDGING_RIG   , XmATTACH_FORM ,
                      LEADING_RIG  , XmATTACH_WIDGET ,
                      LEADING_WIDGET_RIG , newseq->zoom_val_av->wrowcol ,
                  NULL ) ;

     MCW_register_help( newseq->zoom_drag_pb ,
                           "To pan the zoomed image window:\n"
                           "- Click on this 'pan' button\n"
                           "- Then drag the image with mouse\n"
                           "   Button #1 (the cursor in the\n"
                           "   image window will be hand-shaped)\n"
                           "- When you finish dragging, panning\n"
                           "   mode will be turned off\n"
                           "- If you want panning mode to stay\n"
                           "   turned on until you click 'pan'\n"
                           "   again, set environment variable\n"
                           "   AFNI_KEEP_PANNING to YES"         ) ;
     MCW_register_hint( newseq->zoom_drag_pb ,
                           "Pan zoomed image" );

     SENSITIZE( newseq->zoom_drag_pb , 0 ) ;

     wtemp = newseq->zoom_drag_pb ;

     newseq->zoom_fac     = 1   ;     /* initialize data for zooming */
     newseq->zoom_hor_off = 0.0 ;
     newseq->zoom_ver_off = 0.0 ;
     newseq->zoom_pixmap  = (Pixmap) 0 ;
     newseq->zoom_pw      = 0 ;
     newseq->zoom_ph      = 0 ;
     newseq->zoom_xim     = NULL ;
     newseq->zoom_button1 = 0 ;       /* 15 Mar 2002 */

     /* 17 Jun 2002: crop pushbutton */

     STATUS("creating crop control") ;
     newseq->crop_drag_pb =
        XtVaCreateManagedWidget(
           "imseq" , xmPushButtonWidgetClass , newseq->wform ,
            LABEL_ARG("crop") ,
            XmNmarginWidth   , 1 ,
            XmNmarginHeight  , 0 ,
            XmNmarginBottom  , 0 ,
            XmNmarginTop     , 0 ,
            XmNmarginLeft    , 0 ,
            XmNmarginRight   , 0 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;
     XtAddCallback( newseq->crop_drag_pb , XmNactivateCallback ,
                    ISQ_crop_pb_CB , newseq ) ;
     newseq->crop_drag = 0 ;

     newseq->onoff_widgets[(newseq->onoff_num)++] = newseq->crop_drag_pb ;

     XtVaSetValues( newseq->crop_drag_pb ,
                      EDGING_RIG   , XmATTACH_FORM ,
                      LEADING_RIG  , XmATTACH_WIDGET ,
                      LEADING_WIDGET_RIG , wtemp ,
                  NULL ) ;

     MCW_register_help( newseq->crop_drag_pb ,
                           "To crop the image window:\n"
                           "- Click on this 'crop' button;\n"
                           "- Then click and hold down a\n"
                           "   mouse Button at a corner\n"
                           "   of the rectangle to crop;\n"
                           "- Then drag a rectangle to crop\n"
                           "   and release the mouse button\n"
                           "\n"
                           "To uncrop (back to original size):\n"
                           "- Click on this 'crop' button\n"
                           "- Click on it again without cropping\n"
                           "\n"
                           "Another way to crop without using\n"
                           "this 'crop' button is to drag the\n"
                           "crop rectangle using Shift+Button #2\n"
                           "\n"
                           "Another way to uncrop is to click\n"
                           "Shift+Button #2 in the image without\n"
                           "any dragging\n"
                       ) ;
     MCW_register_hint( newseq->crop_drag_pb ,
                           "Crop image" );

     wtemp = newseq->crop_drag_pb ;

   /* 18 Jul 2003: toggle button for pen (drawing mode) */

   { char *lbl = "pen" ;
     STATUS("creating pen control") ;
     newseq->pen_bbox = new_MCW_bbox( newseq->wform ,
                                      1 , &lbl ,
                                      MCW_BB_check , MCW_BB_noframe ,
                                      ISQ_pen_bbox_CB , (XtPointer)newseq ) ;

     newseq->onoff_widgets[(newseq->onoff_num)++] = newseq->pen_bbox->wrowcol ;

     XtVaSetValues( newseq->pen_bbox->wrowcol ,
                      EDGING_RIG   , XmATTACH_FORM ,
                      LEADING_RIG  , XmATTACH_WIDGET ,
                      LEADING_WIDGET_RIG , wtemp ,
                  NULL ) ;

     MCW_reghelp_children( newseq->pen_bbox->wrowcol ,
                           "In ROI drawing mode, toggles\n"
                           "the cursor to a pen shape,\n"
                           "and turn on drawing with\n"
                           "mouse Button-1."
                         ) ;
     MCW_reghint_children( newseq->pen_bbox->wrowcol ,
                           "Toggle pen drawing" ) ;

     XtUnmanageChild( newseq->pen_bbox->wrowcol ) ;
     wtemp = newseq->pen_bbox->wrowcol ;
   }

   /* scale for image number */

   ii = (one_image) ? 1 : newseq->status->num_total - 1 ;

   STATUS("creating image scale") ;
   newseq->onoff_widgets[(newseq->onoff_num)++] =
   newseq->wscale =
       XtVaCreateManagedWidget(
          "imseq" , xmScaleWidgetClass , newseq->wform ,

          XmNtopAttachment    , XmATTACH_WIDGET ,
          XmNtopWidget        , newseq->wimage ,
          XmNleftAttachment   , XmATTACH_FORM ,
          XmNrightAttachment  , XmATTACH_POSITION ,
          XmNrightPosition    , (int)( 0.49 + IMAGE_FRAC * FORM_FRAC_BASE ),

          XmNminimum       , 0 ,                       /* range of scale */
          XmNmaximum       , ii ,
          XmNvalue         , newseq->im_nr ,           /* initial image */
          XmNshowValue     , True ,                    /* show image num */
          XmNscaleMultiple , 1 ,                       /* single step */
          XmNorientation   , XmHORIZONTAL ,            /* sideways */

          XmNtraversalOn , False ,
          XmNinitialResourcesPersistent , False ,
       NULL ) ;

   XtAddCallback( newseq->wscale , XmNvalueChangedCallback ,
                  ISQ_scale_CB , newseq ) ;

   MCW_reghelp_children( newseq->wscale , ISQ_scale_help ) ;
#if 0
   MCW_register_hint( newseq->wscale , "Moves between images" ) ;
#endif

   /* arrowpad at lower right corner */

   STATUS("creating arrowpad") ;
   newseq->arrowpad = new_MCW_arrowpad(
                           newseq->wform ,
                           ISQ_arrowpad_CB , (XtPointer) newseq ) ;

   newseq->onoff_widgets[(newseq->onoff_num)++] = newseq->arrowpad->wform ;

   XtVaSetValues( newseq->arrowpad->wform ,
                     XmNbottomAttachment , XmATTACH_FORM ,
                     XmNrightAttachment  , XmATTACH_FORM ,
                     XtNmappedWhenManaged , False ,   /* managed later */
                  NULL ) ;

   newseq->arrowpad->parent = (XtPointer) newseq ;

   /* drawing area for color bar */

   STATUS("creating intensity bar") ;
   newseq->onoff_widgets[(newseq->onoff_num)++] =
   newseq->wbar =
       XtVaCreateManagedWidget(
          "imseq" , xmDrawingAreaWidgetClass , newseq->wform ,

           XmNtopAttachment    , XmATTACH_FORM ,
           XmNleftAttachment   , XmATTACH_WIDGET ,
           XmNleftWidget       , newseq->wimage ,
           XmNleftOffset       , COLOR_BAR_SPACE ,
           XmNbottomAttachment , XmATTACH_POSITION ,
           XmNbottomPosition   , (int)( 0.49 + IMAGE_FRAC * FORM_FRAC_BASE ),

           XmNwidth       , COLOR_BAR_WIDTH ,

           XmNtraversalOn , False ,
           XmNinitialResourcesPersistent , False ,
       NULL ) ;

   XtInsertEventHandler( newseq->wbar ,          /* handle events in bar */

                            0
                          | ButtonPressMask      /* button presses */
                          | ExposureMask         /* exposures */
                          | StructureNotifyMask  /* resizes */
                         ,
                         FALSE ,                 /* nonmaskable events? */
                         ISQ_drawing_EV ,        /* super-handler! */
                         (XtPointer) newseq ,    /* client data */
                         XtListTail ) ;          /* last in queue */

   /* popup menu on wbar */

   MCW_register_help( newseq->wbar ,
                      "Use Button 3 to popup\n"
                      "a display control menu\n"
                      "\n"
                      "Use Button 1 to enforce\n"
                      "image aspect ratio"       ) ;

   STATUS("creating intensity bar menu") ;

#ifdef BAD_BUTTON3_POPUPS   /* 21 Jul 2003 */
   newseq->wbar_menu = XmCreatePopupMenu( newseq->wscale, "menu",NULL,0 ) ;
#else
   newseq->wbar_menu = XmCreatePopupMenu( newseq->wbar  , "menu",NULL,0 ) ;
#endif

   SAVEUNDERIZE(XtParent(newseq->wbar_menu)) ;  /* 27 Feb 2001 */

   VISIBILIZE_WHEN_MAPPED(newseq->wbar_menu) ;
   if( !AFNI_yesenv("AFNI_DISABLE_TEAROFF") ) TEAROFFIZE(newseq->wbar_menu) ;

   newseq->wbar_rng_but =
      XtVaCreateManagedWidget(
         "menu" , xmPushButtonWidgetClass , newseq->wbar_menu ,
            LABEL_ARG("Choose Display Range") ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtAddCallback( newseq->wbar_rng_but, XmNactivateCallback, ISQ_wbar_menu_CB, newseq ) ;

   newseq->wbar_zer_but =
      XtVaCreateManagedWidget(
         "menu" , xmPushButtonWidgetClass , newseq->wbar_menu ,
            LABEL_ARG("Choose Zero Color") ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtAddCallback( newseq->wbar_zer_but, XmNactivateCallback, ISQ_wbar_menu_CB, newseq ) ;

   newseq->wbar_flat_but =
      XtVaCreateManagedWidget(
         "menu" , xmPushButtonWidgetClass , newseq->wbar_menu ,
            LABEL_ARG("Choose Flatten Range") ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtAddCallback( newseq->wbar_flat_but, XmNactivateCallback, ISQ_wbar_menu_CB, newseq ) ;

   newseq->wbar_sharp_but =
      XtVaCreateManagedWidget(
         "menu" , xmPushButtonWidgetClass , newseq->wbar_menu ,
            LABEL_ARG("Choose Sharpen factor") ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtAddCallback( newseq->wbar_sharp_but, XmNactivateCallback, ISQ_wbar_menu_CB, newseq ) ;

   newseq->rng_bot   = newseq->rng_top = newseq->rng_ztop = 0 ;
   newseq->flat_bot  = newseq->flat_top = 0.0 ;
   newseq->sharp_fac = 0.60 ; newseq->rng_extern = 0 ;

   newseq->zer_color = 0 ;
   ii = DC_find_overlay_color( newseq->dc , getenv("AFNI_IMAGE_ZEROCOLOR") ) ;
   if( ii > 0 ) newseq->zer_color = ii ;

   /* label for informational display */

   newseq->onoff_widgets[(newseq->onoff_num)++] =
   newseq->winfo = XtVaCreateManagedWidget(
                     "imseq" , xmLabelWidgetClass , newseq->wform ,
                        XmNtopAttachment   , XmATTACH_WIDGET ,
                        XmNtopWidget       , newseq->wscale ,
                        XmNleftAttachment  , XmATTACH_FORM ,
                        XmNrightAttachment , XmATTACH_POSITION ,
                        XmNrightPosition   ,
                            (int)( 0.49 + IMAGE_FRAC * FORM_FRAC_BASE ) ,
                        XmNrecomputeSize   , False ,
                        XmNalignment       , XmALIGNMENT_END ,

                        XmNinitialResourcesPersistent , False ,
                     NULL ) ;
   newseq->winfo_extra[0] = '\0' ;  /* 07 Aug 1999 */

   newseq->winfo_sides[0][0] =
    newseq->winfo_sides[1][0] =
     newseq->winfo_sides[2][0] =
      newseq->winfo_sides[3][0] = '\0' ; /* 01 Dec 1999 */
   newseq->winfo_prefix[0] = '\0' ; /* 10 Dec 2007 */

   /***---------- all widgets now created ------------***/

   newseq->mont_across_av   = NULL ;
   newseq->mont_down_av     = NULL ;
   newseq->mont_skip_av     = NULL ;
   newseq->mont_gap_av      = NULL ;
   newseq->mont_gapcolor_av = NULL ;

   newseq->mont_nx       = newseq->mont_nx_old       = 1 ;
   newseq->mont_ny       = newseq->mont_ny_old       = 1 ;
   newseq->mont_skip     = newseq->mont_skip_old     = 0 ;
   newseq->mont_gap      = newseq->mont_gap_old      = 0 ;
   newseq->mont_gapcolor = newseq->mont_gapcolor_old = 0 ;
   newseq->mont_periodic = 1 ;                             /* default = periodic */

STATUS("creation: widgets created") ;

   XtManageChild( newseq->wform ) ;

#if 0
   XtRealizeWidget( newseq->wtop ) ;
   newseq->valid = 2 ;  /* mark this structure as ready to roll */

   NORMAL_cursorize( newseq->wtop ) ;

#else
   newseq->valid = 1 ;  /* mark this structure as valid but not realized */
#endif

   newseq->ignore_redraws = 0 ;

   /* 30 Oct 1996 -- transformations */

   newseq->transform0D_func  = NULL ;  /* no function to start with */
   newseq->transform0D_av    = NULL ;
   newseq->transform0D_index = 0 ;

   newseq->transform2D_func  = NULL ;  /* no function to start with */
   newseq->transform2D_av    = NULL ;
   newseq->transform2D_index = 0 ;

   newseq->slice_proj_av       = NULL ;  /* 31 Jan 2002 */
   newseq->slice_proj_func     = NULL ;
   newseq->slice_proj_index    = 0    ;
   newseq->slice_proj_range_av = NULL ;
   newseq->slice_proj_range    = 0    ;

   newseq->rowgraph_av  = NULL ;       /* 30 Dec 1998 */
   newseq->rowgraph_num = 0 ;
   newseq->rowgraph_mtd = NULL ;

   newseq->graymap_mtd  = NULL ;       /* 24 Oct 2003 */
   newseq->cmap_changed = 0 ;

#define DEFAULT_THETA  55.0
#define DEFAULT_PHI   285.0

   newseq->surfgraph_av    = NULL ;    /* 21 Jan 1999 */
   newseq->surfgraph_num   = 0    ;
   newseq->surfgraph_mtd   = NULL ;
   newseq->surfgraph_theta = DEFAULT_THETA ;
   newseq->surfgraph_phi   = DEFAULT_PHI   ;
   newseq->surfgraph_arrowpad = NULL ;

   newseq->mplot = NULL ;              /* 19 Sep 2001 */

   /* 20 Sep 2001: add a button box to control plots
                   and arrowvals to control labels   */

   { static char *plabel[1] = { "Plot Overlay Plots" } ;
     static char *alabel[7] = { "Off", "UpperLeft", "UpperRight",
                                       "LowerLeft", "LowerRight",
                                       "UpperMid" , "LowerMid"   } ;
     static char *slabel[5] = { "Small" , "Medium" , "Large" , "Huge" , "Enormous" } ;
     char *eee ; int iii ;

     (void) XtVaCreateManagedWidget( "menu",
                                     xmSeparatorWidgetClass, newseq->wbar_menu,
                                       XmNseparatorType , XmSINGLE_LINE ,
                                     NULL ) ;

     /*-- plots stuff --*/

     newseq->wbar_plots_bbox = new_MCW_bbox( newseq->wbar_menu ,
                                             1 , plabel ,
                                             MCW_BB_check , MCW_BB_noframe ,
                                             ISQ_wbar_plots_CB , (XtPointer)newseq ) ;
     MCW_set_bbox( newseq->wbar_plots_bbox , 1 ) ;

     newseq->wbar_graymap_pb =
        XtVaCreateManagedWidget(
           "menu" , xmPushButtonWidgetClass , newseq->wbar_menu ,
              LABEL_ARG("Display Graymap Plot") ,
              XmNtraversalOn , False ,
              XmNinitialResourcesPersistent , False ,
           NULL ) ;
     XtAddCallback( newseq->wbar_graymap_pb, XmNactivateCallback, ISQ_wbar_menu_CB, newseq ) ;

     (void) XtVaCreateManagedWidget( "menu",
                                     xmSeparatorWidgetClass, newseq->wbar_menu,
                                       XmNseparatorType , XmSINGLE_LINE ,
                                     NULL ) ;

     newseq->timer_id = 0 ;  /* 03 Dec 2003 */

     newseq->render_mode = RENDER_DEFAULT ;  /* 0 */

     /*-- labels stuff --*/

     iii = 0 ;
     eee = getenv("AFNI_IMAGE_LABEL_MODE") ;
     if( eee != NULL ){
       iii = strtol(eee,NULL,10) ; if( iii < 0 || iii > 6 ) iii = 1 ;
     }
     newseq->wbar_label_av =
        new_MCW_arrowval( newseq->wbar_menu ,
                          "Label" ,
                          MCW_AV_optmenu ,      /* option menu style */
                          0 ,                   /* first option */
                          6 ,                   /* last option */
                          iii ,                 /* initial selection */
                          MCW_AV_readtext ,     /* ignored but needed */
                          0 ,                   /* ditto */
                          ISQ_wbar_label_CB ,   /* callback when changed */
                          (XtPointer)newseq ,   /* data for above */
                          MCW_av_substring_CB , /* text creation routine */
                          alabel                /* data for above */
                        ) ;

     iii = 1 ;
     eee = getenv("AFNI_IMAGE_LABEL_SIZE") ;
     if( eee != NULL ){
        iii = strtol(eee,NULL,10) ; if( iii < 0 || iii > 4 ) iii = 2 ;
     }
     newseq->wbar_labsz_av =
        new_MCW_arrowval( newseq->wbar_menu ,
                          "Size " ,
                          MCW_AV_optmenu ,      /* option menu style */
                          0 ,                   /* first option */
                          4 ,                   /* last option */
                          iii ,                 /* initial selection */
                          MCW_AV_readtext ,     /* ignored but needed */
                          0 ,                   /* ditto */
                          ISQ_wbar_label_CB ,   /* callback when changed */
                          (XtPointer)newseq ,   /* data for above */
                          MCW_av_substring_CB , /* text creation routine */
                          slabel                /* data for above */
                        ) ;

   } /* end of plots & labels stuff */

   /** 23 Feb 2003: menu items to control tic marks */

   (void) XtVaCreateManagedWidget( "menu",
                                   xmSeparatorWidgetClass, newseq->wbar_menu,
                                     XmNseparatorType , XmSINGLE_LINE ,
                                   NULL ) ;
   newseq->wbar_ticnum_av =
      new_MCW_arrowval( newseq->wbar_menu ,
                        "Tick Div." ,
                        MCW_AV_optmenu ,      /* option menu style */
                        0 ,                   /* first option */
                        21 ,                  /* last option */
                        0 ,                   /* initial selection */
                        MCW_AV_readtext ,     /* ignored but needed */
                        0 ,                   /* ditto */
                        ISQ_wbar_label_CB ,   /* callback when changed */
                        (XtPointer)newseq ,   /* data for above */
                        NULL                , /* text creation routine */
                        NULL                  /* data for above */
                      ) ;
   AVOPT_columnize(newseq->wbar_ticnum_av,2) ;
   MCW_reghint_children( newseq->wbar_ticnum_av->wrowcol ,
    "Number of tick marks on image edges [cf. AFNI_IMAGE_TICK_DIV_IN_MM]" ) ;
   newseq->wbar_ticsiz_av =
      new_MCW_arrowval( newseq->wbar_menu ,
                        "Tick Size" ,
                        MCW_AV_optmenu ,      /* option menu style */
                        1 ,                   /* first option */
                        10 ,                  /* last option */
                        1 ,                   /* initial selection */
                        MCW_AV_readtext ,     /* ignored but needed */
                        0 ,                   /* ditto */
                        ISQ_wbar_label_CB ,   /* callback when changed */
                        (XtPointer)newseq ,   /* data for above */
                        NULL                , /* text creation routine */
                        NULL                  /* data for above */
                      ) ;
   AVOPT_columnize(newseq->wbar_ticsiz_av,2) ;
   MCW_reghint_children( newseq->wbar_ticsiz_av->wrowcol ,
                         "Size of tick marks around image edges" ) ;

   /** 27 Oct 2008: menu item to control checkerboarding */

   (void) XtVaCreateManagedWidget( "menu",
                                   xmSeparatorWidgetClass, newseq->wbar_menu,
                                     XmNseparatorType , XmSINGLE_LINE ,
                                   NULL ) ;
   newseq->wbar_checkbrd_av =
      new_MCW_arrowval( newseq->wbar_menu ,
                        "CheckBrd#" ,
                        MCW_AV_optmenu ,      /* option menu style */
                        0 ,                   /* first option */
                        43 ,                  /* last option */
                        0 ,                   /* initial selection */
                        MCW_AV_readtext ,     /* ignored but needed */
                        0 ,                   /* ditto */
                        ISQ_wbar_label_CB ,   /* callback when changed */
                        (XtPointer)newseq ,   /* data for above */
                        NULL                , /* text creation routine */
                        NULL                  /* data for above */
                      ) ;
   AVOPT_columnize(newseq->wbar_checkbrd_av,4) ;
   MCW_reghint_children( newseq->wbar_checkbrd_av->wrowcol ,
                         "Size of checks in the checkerboard display [# key]" );

   /** 10 Feb 2009: menu item to control animation duplicates **/

   newseq->wbar_animdup_av =
      new_MCW_arrowval( newseq->wbar_menu ,
                        "Anim_Dup " ,
                        MCW_AV_optmenu ,      /* option menu style */
                        0 ,                   /* first option */
                        19 ,                  /* last option */
                        ISQ_anim_dup ,        /* initial selection */
                        MCW_AV_readtext ,     /* ignored but needed */
                        0 ,                   /* ditto */
                        ISQ_wbar_label_CB ,   /* callback when changed */
                        (XtPointer)newseq ,   /* data for above */
                        NULL                , /* text creation routine */
                        NULL                  /* data for above */
                      ) ;
   AVOPT_columnize(newseq->wbar_animdup_av,2) ;
   MCW_reghint_children( newseq->wbar_animdup_av->wrowcol ,
                         "Duplicate images for Save:aGif and Save:mpeg" ) ;

   newseq->top_clip = 0.0f ; /* 17 Sep 2007 */
   newseq->redo_clip = 0 ;

   /* 23 Jan 2003: set default save? */

   drive_MCW_imseq( newseq , isqDR_setimsave ,
                    (XtPointer)getenv("AFNI_DEFAULT_IMSAVE") ) ;

   /* 23 Jan 2003: set opacity? */

   { char *eee = getenv("AFNI_DEFAULT_OPACITY") ;
     if( eee != NULL ){
       int opval = (int) strtod( eee , NULL ) ;
       if( opval > 0 && opval <= 9 )
         drive_MCW_imseq( newseq , isqDR_setopacity , (XtPointer)opval ) ;
     }
   }

   newseq->parent = NULL ;

   { static int first=1 ;
     if( first ){
       memplot_topshell_setsaver( ".jpg" , memplot_to_jpg ) ; /* 05 Dec 2007 */
       memplot_topshell_setsaver( ".png" , memplot_to_png ) ;
       first = 0 ;
     }
   }

   RETURN(newseq) ;
}

/*----------------------------------------------------------------------
   set the image dimensions in "physical units"
     (based on the dx and dy fields of the input image);
   the goal is to keep the same scaling from pixels -> mm even
   if the image is a different size
------------------------------------------------------------------------*/

void ISQ_reset_dimen( MCW_imseq * seq,  float new_width_mm, float new_height_mm )
{
   int xwide , yhigh , oldx,oldy ;
   float scale_x , scale_y ;
   int wx,hy,xx,yy ;   /* geometry of shell */
   int xp,yp ;
   MCW_DC *dc ;

   float minfrac=DEFAULT_MINFRAC ; char *eee ; /* 12 Jun 2002 */
   float maxfrac=DEFAULT_MAXFRAC ;

ENTRY("ISQ_reset_dimen") ;

   if( ! ISQ_VALID(seq) ) EXRETURN ;

   MCW_widget_geom( seq->wimage , &oldx , &oldy , NULL,NULL ) ;

   scale_x = seq->last_width_mm / oldx ;  /* mm/pixel as displayed now */
   scale_y = seq->last_height_mm/ oldy ;

   if( ! seq->opt.free_aspect ){                      /* fixed aspect */
      scale_x = scale_y = sqrt( scale_x * scale_y ) ; /*  means use   */
   }                                                  /* same scales! */

   xwide = new_width_mm / scale_x + 0.5 ;  /* so scale to new # of pixels */
   yhigh = new_height_mm/ scale_y + 0.5 ;

   /** 12 Jun 2002: set minimum size for image windows,
                    as a fraction of the overall screen area **/

   eee = my_getenv("AFNI_IMAGE_MINFRAC") ;
   if( eee != NULL ){
      float fff=0.0 ; int ii ;
      ii = sscanf(eee,"%f",&fff) ;
      if( ii > 0 && fff > 0.0 && fff <= 1.0 ) minfrac = fff ;
      else                                    minfrac = DEFAULT_MINFRAC ;
   }

   eee = my_getenv("AFNI_IMAGE_MAXFRAC") ;
   if( eee != NULL ){
      float fff=0.0 ; int ii ;
      ii = sscanf(eee,"%f",&fff) ;
      if( ii > 0 && fff > 0.0 && fff <= 1.0 ) maxfrac = fff ;
      else                                    maxfrac = DEFAULT_MAXFRAC ;
   }

   dc = seq->dc ;

   { float xxx = xwide , yyy = yhigh ;
     float fff = (xxx*yyy)/(dc->width*dc->height) , ggg ;

     /* modify if window too small */

     if( fff < minfrac ){
       fff = sqrt(minfrac/fff) ; xxx *= fff ; yyy *= fff ; /* expand area */
     }

     /* modify if window too big */

     fff = ggg = 1.0 ;
     if( xxx >= maxfrac*dc->width ) fff = maxfrac*dc->width / xxx ; /* don't let  */
     if( yyy >= maxfrac*dc->height) ggg = maxfrac*dc->height/ yyy ; /* be too big */
     fff = MIN(fff,ggg) ; xxx *= fff ; yyy *= fff ;
     if( xxx < 1.0 || yyy < 1.0 ){                      /* weird result?? */
        xxx = xwide ; yyy = yhigh ;                    /* back to old way */
     }

     xwide = (int)( 0.49 + xxx ) ;
     yhigh = (int)( 0.49 + yyy ) ;
   }

if( PRINT_TRACING ){
  char str[256] ;
  sprintf(str,"last wid=%f hei=%f  new wid=%f hei=%f",
          seq->last_width_mm,seq->last_height_mm,new_width_mm,new_height_mm ) ;
  STATUS(str) ;
  sprintf(str,"new xwide=%d yhigh=%d  scale_x=%f _y=%f",
          xwide,yhigh,scale_x,scale_y) ;
  STATUS(str) ;
}

   seq->last_width_mm  = new_width_mm ;
   seq->last_height_mm = new_height_mm ;

   /* possibly expand to include control widgets (if they are on) */

   if( seq->onoff_state ){
     float fff,ggg ;
     xwide = (int) ( 0.49 + xwide / seq->image_frac ) ;  /* new size of shell */
     yhigh = (int) ( 0.49 + yhigh / seq->image_frac ) ;

     fff = ggg = 1.0 ;
     if( xwide >= maxfrac*dc->width ) fff = maxfrac*dc->width /xwide; /* 13 Jun 2003  */
     if( yhigh >= maxfrac*dc->height) ggg = maxfrac*dc->height/yhigh; /* Fri the 13th */
     fff = MIN(fff,ggg) ;
     fff = MIN(fff,ggg) ; xwide *= fff ; yhigh *= fff ;
   }

   if( seq->opt.free_aspect ){
      XtVaSetValues( seq->wtop ,
                       XmNminAspectX ,  1 ,   /* free up aspect ratio */
                       XmNminAspectY , 20 ,
                       XmNmaxAspectX , 20 ,
                       XmNmaxAspectY ,  1 ,
                     NULL ) ;
   } else {
      XtVaSetValues( seq->wtop ,
                       XmNminAspectX , xwide ,   /* reset aspect ratio */
                       XmNminAspectY , yhigh ,
                       XmNmaxAspectX , xwide ,
                       XmNmaxAspectY , yhigh ,
                     NULL ) ;
   }

   XtVaSetValues( seq->wtop ,
                     XmNwidth  , xwide ,      /* reset size of form */
                     XmNheight , yhigh ,
                  NULL ) ;

   /* it is possible that the image has flipped off the screen now -- fix that! */

   MCW_widget_geom( seq->wtop , &wx,&hy,&xx,&yy ) ;

   if( xx+wx/2 < 1 ) xp = 10 ; else xp = xx ;
   if( yy+hy/2 < 1 ) yp = 10 ; else yp = yy ;

   if( xp != xx || yp != yy )
     XtVaSetValues( seq->wtop , XmNx , xp , XmNy , yp , NULL ) ;

   /* if there is a dialog, move it too [modified 05 Jan 1999] */

   if( seq->dialog != NULL && XtIsRealized( seq->dialog ) )
     ISQ_place_dialog( seq ) ;

   EXRETURN ;
}

/*-----------------------------------------------------------------------
   copy an imseq status structure
-------------------------------------------------------------------------*/

MCW_imseq_status * ISQ_copy_status( MCW_imseq_status * instat )
{
   MCW_imseq_status * outstat ;

ENTRY("ISQ_copy_status") ;

   outstat = (MCW_imseq_status *) XtMalloc( sizeof(MCW_imseq_status) ) ;

   *outstat = *instat ;   /* shallow copy for now (no pointers) */
   RETURN(outstat) ;
}

/*-----------------------------------------------------------------------*/

char * ISQ_opacity_label( int val ) /* 07 Mar 2001 */
{
   static char dig[] = "0123456789" , buf[3] ;

   buf[0] = dig[val] ; buf[1] = '\0' ; return buf ;
}

/*-----------------------------------------------------------------------*/

void ISQ_opacity_CB( MCW_arrowval *av , XtPointer cd ) /* 07 Mar 2001 */
{
   MCW_imseq *seq = (MCW_imseq *) cd ;
   char *buf = ISQ_opacity_label(av->ival) ;
   XmString xstr = XmStringCreateLtoR( buf , XmFONTLIST_DEFAULT_TAG ) ;

   XtVaSetValues( av->wlabel , XmNlabelString , xstr , NULL ) ;
   XmStringFree( xstr ) ;

   seq->ov_opacity = OPACITY_FAC * av->ival ;
   ISQ_redisplay( seq , -1 , isqDR_display ) ;
   return ;
}

/*-----------------------------------------------------------------------*/

/*! Callback for the zoom arrowval buttons */

void ISQ_zoom_av_CB( MCW_arrowval *apv , XtPointer cd ) /* 11 Mar 2002 */
{
   MCW_imseq    *seq = (MCW_imseq *) cd ;
   MCW_arrowval *av  = seq->zoom_val_av ;
   XmString xstr ;
   int zlev=av->ival , zold=seq->zoom_fac ;

ENTRY("ISQ_zoom_av_CB") ;

   if( !ISQ_REALZ(seq) || av != apv ) EXRETURN ;  /* bad */

   if( seq->mont_nx > 1 || seq->mont_ny > 1 ){   /* 18 Nov 2003 */
#if 0
fprintf(stderr,"zoom: montage nx=%d ny=%d\n",seq->mont_nx,seq->mont_ny) ;
#endif
     AV_assign_ival(av,ZOOM_BOT) ; seq->zoom_fac = 1 ;
     XBell(seq->dc->display,100); EXRETURN;
   }
   if( seq->dialog != NULL && seq->dialog_starter == NBUT_MONT ){
#if 0
fprintf(stderr,"zoom: dialog_starter = %d\n",seq->dialog_starter) ;
#endif
     AV_assign_ival(av,ZOOM_BOT) ; seq->zoom_fac = 1 ;
     XBell(seq->dc->display,100); EXRETURN;
   }

   /*-- change zoom factor --*/

   xstr = XmStringCreateLtoR( (zlev==1)?"z":"Z" , XmFONTLIST_DEFAULT_TAG );
   XtVaSetValues( av->wlabel , XmNlabelString , xstr , NULL ) ;
   XmStringFree( xstr ) ;

   seq->zoom_fac = zlev ;   /* change recorded zoom factor */
   if( zlev == 1 ){
      seq->zoom_hor_off = seq->zoom_ver_off = 0.0 ; /* no offsets */
   } else {
      float mh = (zlev-1.001)/zlev ;        /* max offset allowed */
      float dh = 0.5*(1.0/zold-1.0/zlev) ;  /* change in offset to */
                                            /* keep current center */
      seq->zoom_hor_off += dh ;
      seq->zoom_ver_off += dh ;
           if( seq->zoom_hor_off > mh  ) seq->zoom_hor_off = mh  ;
      else if( seq->zoom_hor_off < 0.0 ) seq->zoom_hor_off = 0.0 ;
           if( seq->zoom_ver_off > mh  ) seq->zoom_ver_off = mh  ;
      else if( seq->zoom_ver_off < 0.0 ) seq->zoom_ver_off = 0.0 ;
   }

   /* change some widgets depending on zoom level */

   SENSITIZE( seq->zoom_drag_pb , (zlev>1) ) ;

   AV_SENSITIZE_DOWN( av , (zlev > 1       ) ) ;
   AV_SENSITIZE_UP  ( av , (zlev < ZOOM_TOP) ) ;

   if( zlev == 1 && seq->zoom_button1 ){       /* can't pan at zlev=1 */
      seq->zoom_button1 = 0 ;
      MCW_invert_widget( seq->zoom_drag_pb ) ;
      POPUP_cursorize( seq->wimage ) ;
   }

   /* free pixmap (need a new one for a new image size) */

   if( seq->zoom_pixmap != (Pixmap) 0 ){
      XFreePixmap( seq->dc->display , seq->zoom_pixmap ) ;
      seq->zoom_pixmap = (Pixmap) 0 ; seq->zoom_pw = seq->zoom_ph = 0 ;
   }

   /* free zoomed image (need a new one for a new image size) */

   MCW_kill_XImage(seq->zoom_xim) ; seq->zoom_xim = NULL ;

   /* must redisplay image totally */

   ISQ_redisplay( seq , -1 , isqDR_display ) ;

   EXRETURN ;
}

/*--------------------------------------------------------------------------*/
/*! Callback for 'pan' button. */

void ISQ_zoom_pb_CB( Widget w , XtPointer client_data , XtPointer call_data )
{
   MCW_imseq * seq = (MCW_imseq *) client_data ;

ENTRY("ISQ_zoom_pb_CB") ;

   if( ! ISQ_REALZ(seq)       ||
       w != seq->zoom_drag_pb ||
       seq->zoom_fac == 1       ) EXRETURN ; /* bad */

   if( seq->cursor_state != CURSOR_NORMAL ){ /* really bad */
     XBell(XtDisplay(w),100); EXRETURN;
   }

   seq->zoom_button1 = !seq->zoom_button1 ; /* toggle dragging */

   if( seq->zoom_button1 ) HAND_cursorize ( seq->wimage ) ;
   else                    POPUP_cursorize( seq->wimage ) ;

   MCW_invert_widget( seq->zoom_drag_pb ) ;

   if( seq->crop_drag ){                       /* turn off crop */
     MCW_invert_widget( seq->crop_drag_pb ) ;  /* button, if on */
     seq->crop_drag = 0 ;
   }

   EXRETURN ;
}

/*-------------------------------------------------------------------------*/
/*! Actually pan the zoomed image, lr steps left/right, ud steps up/down.
---------------------------------------------------------------------------*/

void ISQ_actually_pan( MCW_imseq *seq , int lr , int ud )  /* 24 Jan 2003 */
{
   float hh,vv , mh,dh , hhold,vvold ;

ENTRY("ISQ_actually_pan") ;

   if( !ISQ_REALZ(seq) || seq->zoom_fac == 1 || seq->zoom_xim == NULL ) EXRETURN;

   mh = (seq->zoom_fac-1.001)/seq->zoom_fac ;  /* max offset    */
   dh = 0.020/seq->zoom_fac ;                  /* delta offset   */
   hh=seq->zoom_hor_off ; hhold=hh ;           /* current offsets */
   vv=seq->zoom_ver_off ; vvold=vv ;

   hh += lr*dh ;
        if( hh < 0.0) hh = 0.0 ;
   else if( hh > mh ) hh = mh  ;

   vv += ud*dh ;
        if( vv < 0.0) vv = 0.0 ;
   else if( vv > mh ) vv = mh  ;

   if( vv == vvold && hh == hhold ) EXRETURN ; /* no changes? */

   seq->zoom_hor_off = hh ;                    /* changes! */
   seq->zoom_ver_off = vv ;
   ISQ_show_zoom( seq ) ;                      /* redraw */
   EXRETURN ;
}

/*--------------------------------------------------------------------------*/
/*! Callback for 'crop' button. */

void ISQ_crop_pb_CB( Widget w , XtPointer client_data , XtPointer call_data )
{
   MCW_imseq * seq = (MCW_imseq *) client_data ;

ENTRY("ISQ_crop_pb_CB") ;

   if( !ISQ_REALZ(seq)        ||
       w != seq->crop_drag_pb ||
       ! seq->crop_allowed      ){ XBell(XtDisplay(w),100); EXRETURN; }

   MCW_invert_widget( seq->crop_drag_pb ) ;
   seq->crop_drag = !seq->crop_drag ;

   if( !seq->crop_drag && seq->cropit ){                      /* turn crop off */
     seq->cropit = 0; seq->crop_nxorg = seq->crop_nyorg = -1; /* if double-pressed */
     ISQ_redisplay( seq , -1 , isqDR_display ) ;
   }

   if( seq->zoom_button1 ){                     /* turn pan off if on */
     POPUP_cursorize( seq->wimage ) ;
     MCW_invert_widget( seq->zoom_drag_pb ) ;
     seq->zoom_button1 = 0 ;
   }

   EXRETURN ;
}

/*-----------------------------------------------------------------------*/

MRI_IMAGE * ISQ_index_to_rgb( MCW_DC *dc , int overlay , MRI_IMAGE *im ) /* 07 Mar 2001 */
{
   register int npix,ii,jj ;
   MRI_IMAGE *outim ;
   register byte *our ;
   register short *iar ;

ENTRY("ISQ_short_to_rgb") ;

   if( dc == NULL || im == NULL || im->kind != MRI_short ) RETURN(NULL) ;

   npix  = im->nvox ;
   iar   = MRI_SHORT_PTR(im) ;
   outim = mri_new_conforming( im , MRI_rgb ) ;
   our   = MRI_RGB_PTR(outim) ;

   if( !overlay ){
      for( jj=ii=0 ; ii < npix ; ii++,jj+=3 ){
         if( iar[ii] >= 0 ){                         /* pos => underlay table */
            our[jj  ] = DC_REDBYTE  (dc,iar[ii]) ;
            our[jj+1] = DC_GREENBYTE(dc,iar[ii]) ;
            our[jj+2] = DC_BLUEBYTE (dc,iar[ii]) ;
         } else {                                    /* neg => overlay table */
            our[jj  ] = DCOV_REDBYTE  (dc,-iar[ii]) ;
            our[jj+1] = DCOV_GREENBYTE(dc,-iar[ii]) ;
            our[jj+2] = DCOV_BLUEBYTE (dc,-iar[ii]) ;
         }
      }
   } else {                                      /* use overlay table only */
      for( jj=ii=0 ; ii < npix ; ii++,jj+=3 ){
         if( iar[ii] > 0 ){                         /* valid overlay index */
            our[jj  ] = DCOV_REDBYTE(dc,iar[ii]) ;
            our[jj+1] = DCOV_GREENBYTE(dc,iar[ii]) ;
            our[jj+2] = DCOV_BLUEBYTE(dc,iar[ii]) ;
         } else {                                   /* not valid */
            our[jj] = our[jj+1] = our[jj+2] = 0 ;
         }
      }
   }

   RETURN(outim) ;
}

/*-----------------------------------------------------------------------
   Overlay one image onto another.  Underlay (ulim) and overlay (ovim)
   must be either shorts or rgb.  If they are shorts, they are indices
   into the underlay and overlay color index tables, respectively.
   The overlay opacity is alpha (0 < alpha <= 1).
     If both are shorts and alpha=1, the output is shorts.
     If either is rgb, or alpha < 1, then the output is rgb.
     The output is NULL if the inputs are invalid.
   Pixels from ovim are overlaid only if they are NOT zero.

   06 Mar 2001 -- RWCox
-------------------------------------------------------------------------*/

MRI_IMAGE * ISQ_overlay( MCW_DC *dc, MRI_IMAGE *ulim, MRI_IMAGE *ovim, float alpha )
{
   register int npix,ii,jj ;
   MRI_IMAGE *outim , *orim ;
   register byte *orr, *our ;

ENTRY("ISQ_overlay") ;

   if( dc == NULL || ulim == NULL || ovim == NULL || alpha <= 0.0 ) RETURN(NULL) ;

   npix = ulim->nvox ;

   if( ovim->nvox != npix ) RETURN(NULL) ;

   /*-- Case: both are short indices, no transparency --*/

   if( ulim->kind == MRI_short && ovim->kind == MRI_short && alpha > 0.99 ){
      register short *tar , *oar=MRI_SHORT_PTR(ovim) , *iar=MRI_SHORT_PTR(ulim) ;

      outim = mri_new_conforming( ulim , MRI_short ) ;
      tar = MRI_SHORT_PTR( outim ) ;
      for( ii=0 ; ii < npix ; ii++ )
         tar[ii] = (oar[ii] <= 0) ? iar[ii] : -oar[ii] ;

      RETURN(outim) ;
   }

   /*-- Convert both inputs to RGB, if needed --*/

   switch( ulim->kind ){              /* we always make a new RGB underlay,  */
      case MRI_rgb:                   /* since this will be the output image */
         outim = mri_copy(ulim) ;
         our   = MRI_RGB_PTR(outim) ;
      break ;

      default:
         RETURN(NULL) ; break ;   /* bad bad bad */

      case MRI_short:
         outim = ISQ_index_to_rgb( dc , 0 , ulim ) ;
         our   = MRI_RGB_PTR(outim) ;
      break ;
   }

   switch( ovim->kind ){    /* but we don't make a new overlay unless needed */
      case MRI_rgb:
         orim = ovim ; orr = MRI_RGB_PTR(orim) ; break ;

      default:
         mri_free(outim) ;
         RETURN(NULL) ; break ;              /* bad bad bad */

      case MRI_short:
         orim = ISQ_index_to_rgb( dc , 1 , ovim ) ;
         orr  = MRI_RGB_PTR(orim) ;
      break ;
   }

   /* now overlay */

   if( alpha > 0.99 ){                          /* opaque overlay */
      for( jj=ii=0 ; ii < npix ; ii++,jj+=3 ){
         if( orr[jj] > 0 || orr[jj+1] > 0 || orr[jj+2] > 0 ){
            our[jj  ] = orr[jj  ] ;
            our[jj+1] = orr[jj+1] ;
            our[jj+2] = orr[jj+2] ;
         }
      }
   } else if (!my_getenv("AFNI_MIX_BY_BRIGHT")) {     /* translucent overlay */
      register float aa=alpha , bb=1.0-alpha ;
      for( jj=ii=0 ; ii < npix ; ii++,jj+=3 ){
         if( orr[jj] > 0 || orr[jj+1] > 0 || orr[jj+2] > 0 ){
            our[jj  ] = aa*orr[jj  ] + bb*our[jj  ] ;  /* mix colors */
            our[jj+1] = aa*orr[jj+1] + bb*our[jj+1] ;
            our[jj+2] = aa*orr[jj+2] + bb*our[jj+2] ;
         }
      }
   } else {       /* mix by scaling color with brightness of background */
      register float aa=alpha, bb,
                     mings, maxgs, *gs=NULL,
                     MaxGain = 2.0-alpha*alpha,
                     MinGain = alpha*alpha ;
      gs = (float *)malloc(sizeof(float)*npix);
      /* calculate grey scale, keep track of range */
      mings = 255*3.0; maxgs = 0;
      for( jj=ii=0 ; ii < npix ; ii++,jj+=3 ){
         if( orr[jj] > 0 || orr[jj+1] > 0 || orr[jj+2] > 0 ){
            gs[ii] = (our[jj  ]+our[jj+1]+orr[jj+2]);
            if (gs[ii] < mings) mings = gs[ii];
            else if (gs[ii] > maxgs) maxgs = gs[ii];
         } else {
            gs[ii] = 0.0;
         }
      }
      /* now scale gs values */
      bb = (MaxGain - MinGain)/(maxgs-mings);
      for( ii=0 ; ii < npix ; ii++ ){
         if( gs[ii] ) gs[ii] =  bb * (gs[ii]-mings)+MinGain;
      }
      for( jj=ii=0 ; ii < npix ; ii++,jj+=3 ){
         if( gs[ii] ){ /* Colors will change here, not just a brightness
                          modulation. Need much fancier method to deal
                          with mixing appropriately */
               bb = (gs[ii])*orr[jj  ]  ;  /* mix colors */
               if (bb > 255) our[jj  ] = 255;
               else our[jj  ] = (byte)bb;
               bb = (gs[ii])*orr[jj+1]  ;
               if (bb > 255) our[jj+1] = 255;
               else our[jj+1] = (byte)bb;
               bb = (gs[ii])*orr[jj+2]  ;
               if (bb > 255) our[jj+2] = 255;
               else our[jj+2] = (byte)bb;
         }
      }
      free(gs); gs=NULL;
   }

   if( orim != ovim ) mri_free(orim) ;  /* destroy copy of overlay, if any */

   RETURN(outim) ;
}

/*-----------------------------------------------------------------------
   Make a color bar "given" XImage
-------------------------------------------------------------------------*/

void ISQ_make_bar( MCW_imseq * seq )
{
   MRI_IMAGE * im ;
   int iy , ny ;
   short * ar ;

ENTRY("ISQ_make_bar") ;

   if( ! ISQ_VALID(seq) ) EXRETURN ;

   KILL_2XIM( seq->given_xbar , seq->sized_xbar ) ;

   ny = seq->dc->ncol_im ;
   im = mri_new( 1 , ny , MRI_short ) ;
   ar = mri_data_pointer( im ) ;

   for( iy=0 ; iy < ny ; iy++ ) ar[iy] = ny-1-iy ;

   seq->given_xbar = mri_to_XImage( seq->dc , im ) ;

   KILL_1MRI( im ) ;
   EXRETURN ;
}

/*------------------------------------------------------------------------
   make the MRI_IMAGE and the XImage, given the sequence status:
     - if imim is NULL, get it from the user routine and process it,
       if imim is not NULL, leave it alone
     - convert into an XImage
     - here is where changes to the toggled display options are processed
-------------------------------------------------------------------------*/

void ISQ_make_image( MCW_imseq *seq )
{
   MRI_IMAGE *im , *ovim , *tim ;
   Boolean reset_done = False ;

ENTRY("ISQ_make_image") ;

   if( ! ISQ_VALID(seq) ) EXRETURN ;

   /*-- if doing a montage, make it in a separate function --*/

   if( seq->mont_nx > 1 || seq->mont_ny > 1 ){
     ISQ_make_montage( seq ) ;
     EXRETURN ;
   }

   KILL_2XIM( seq->given_xim , seq->sized_xim ) ;  /* erase the XImages */

   if( seq->mplot != NULL ){                            /* 19 Sep 2001 */
     delete_memplot( seq->mplot ) ; seq->mplot = NULL ;
   }

   /* process toggled options that affect the image that may be stored */

   if( seq->opt.rot         != seq->old_opt.rot         ||
       seq->opt.mirror      != seq->old_opt.mirror      ||
       seq->opt.scale_group != seq->old_opt.scale_group ||
       seq->opt.scale_range != seq->old_opt.scale_range ||
       seq->mont_nx         != seq->mont_nx_old         ||
       seq->mont_ny         != seq->mont_ny_old           ){

      KILL_1MRI( seq->imim ) ;  /* must re-get image for new processing */
      KILL_1MRI( seq->ovim ) ;
   }

   /*--- set the image to process ---*/

   im = seq->imim ;

   if( im == NULL ){
      float new_width_mm , new_height_mm ;

      switch( seq->render_mode ){
        default:
          tim = ISQ_getimage( seq->im_nr , seq ) ;
          if( tim == NULL ) EXRETURN ;
          seq->last_image_type = tim->kind ;
          seq->set_orim = (seq->need_orim != 0) ;  /* 30 Dec 1998 */
          seq->imim = im = ISQ_process_mri( seq->im_nr , seq , tim ) ;
          KILL_1MRI(tim) ;
          seq->set_orim = 0 ;
          seq->barbot = seq->clbot ; /* 29 Jul 2001 */
          seq->bartop = seq->cltop ;
        break ;

        case RENDER_CHECK_UO:
        case RENDER_CHECK_OU:
          seq->set_orim = 0 ;
          seq->imim = im = ISQ_getchecked( seq->im_nr ,seq ) ;
          if( im == NULL ) EXRETURN ;
          seq->last_image_type = im->kind ;
          seq->barbot = seq->bartop = 0.0f ;
        break ;
      }
      ISQ_set_barhint(seq,NULL) ;

      /* fix window dimensions if image size is different from before */

      new_width_mm  = IM_WIDTH(im) ;
      new_height_mm = IM_HEIGHT(im) ;

      seq->horig = im->nx ;  seq->last_dx = fabs(im->dx) ;
      seq->vorig = im->ny ;  seq->last_dy = fabs(im->dy) ;

      if( FLDIF(new_width_mm ,seq->last_width_mm ) ||
          FLDIF(new_height_mm,seq->last_height_mm)   ){

         if( PRINT_TRACING ){
           char str[256] ;
           sprintf(str,"nx=%d ny=%d dx=%f dy=%f wid=%f hei=%f",
                  im->nx,im->ny,im->dx,im->dy,new_width_mm,new_height_mm) ;
           STATUS(str) ;
         }

         ISQ_reset_dimen( seq , new_width_mm , new_height_mm ) ;
         reset_done = True ;
      }
   }

   if( seq->opt.free_aspect != seq->old_opt.free_aspect && !reset_done )
      ISQ_reset_dimen( seq , seq->last_width_mm , seq->last_height_mm ) ;

   /*--- set the overlay to process ---*/

   if( ISQ_SKIP_OVERLAY(seq) ){
     KILL_1MRI( seq->ovim ) ;
     ovim = NULL ;
   } else {
     char *lab ;        /* 20 Sep 2001 */

     ovim = seq->ovim ;
     if( ovim == NULL ){
        tim = ISQ_getoverlay( seq->im_nr , seq ) ;

        if( tim != NULL && !ISQ_GOOD_OVERLAY_TYPE(tim->kind) ){
          fprintf(stderr,"\a\n*** Illegal overlay image kind=%d! ***\n",tim->kind) ;
          KILL_1MRI(tim) ;
        }

        if( tim != NULL )
          ovim = seq->ovim =
            mri_flippo( ISQ_TO_MRI_ROT(seq->opt.rot) , seq->opt.mirror , tim ) ;

        if( tim != ovim ) KILL_1MRI(tim) ;
     }

     /*-- 19 Sep 2001: get an overlay plot, if there is one --*/

     if( MCW_val_bbox(seq->wbar_plots_bbox) != 0 ){
       seq->mplot = ISQ_getmemplot( seq->im_nr , seq ) ;
       if( seq->mplot != NULL )
         flip_memplot( ISQ_TO_MRI_ROT(seq->opt.rot),seq->opt.mirror, seq->mplot );
     }

     /*-- 20 Sep 2001: get a label, if there is one --*/

     if( seq->wbar_label_av->ival != 0 ){
       lab = ISQ_getlabel( seq->im_nr , seq ) ;
       if( lab != NULL ){
         MEM_plotdata *mp = ISQ_plot_label( seq , lab ) ;
         if( mp != NULL ){
           if( seq->mplot != NULL ){
             append_to_memplot( seq->mplot , mp ) ; delete_memplot( mp ) ;
           } else {
             seq->mplot = mp ;
           }
         }
         free(lab) ;
       }
     }

   } /* end of overlay-osity */

   /* set old_opt to current options */

   seq->old_opt = seq->opt ;

   seq->mont_nx_old = seq->mont_ny_old = 1 ;

   STATUS("making given_xim");

   /* overlay, if needed */

   if( ovim == NULL || ISQ_SKIP_OVERLAY(seq) ){          /* nothing to do */

      tim = im ;
#if 1
   } else {                                                /* 06 Mar 2001 */

      tim = ISQ_overlay( seq->dc, im, ovim, seq->ov_opacity ) ;
      if( tim == NULL ) tim = im ;                    /* shouldn't happen */

#else
   } else if( im->kind == MRI_short ){                    /* the old case */
      register short * tar , * oar , * iar ;
      register int ii , npix = im->nx * im->ny ;

      STATUS("overlaying onto 'im'") ;

      tim = mri_new( im->nx , im->ny , MRI_short ) ;
      tar = MRI_SHORT_PTR( tim ) ;                      /* merger   */
      oar = MRI_SHORT_PTR( ovim ) ;                     /* overlay  */
      iar = MRI_SHORT_PTR( im ) ;                       /* underlay */
      for( ii=0 ; ii < npix ; ii++ )
         tar[ii] = (oar[ii] == 0) ? iar[ii] : -oar[ii] ;

   } else if( im->kind == MRI_rgb ){                       /* 11 Feb 1999 */
      register int ii , npix = im->nx * im->ny ;
      register short *oar = MRI_SHORT_PTR(ovim) ;
      register byte *tar , *iar = MRI_RGB_PTR(im) ;
      register Pixel *negpix = seq->dc->ovc->pix_ov ;

      tim = mri_to_rgb( im ) ; tar = MRI_RGB_PTR(tim) ;

      for( ii=0 ; ii < npix ; ii++ )
        if( oar[ii] > 0 )
          DC_pixel_to_rgb( seq->dc, negpix[oar[ii]],
                           tar+(3*ii),tar+(3*ii+1),tar+(3*ii+2) ) ;
#endif
   }

   /* convert result to XImage for display */

   STATUS("converting to XImage") ;
   seq->given_xim = mri_to_XImage( seq->dc , tim ) ;

   if( tim != im ) KILL_1MRI(tim) ;

   EXRETURN ;
}

/*-----------------------------------------------------------------------
  Plot a label into a structure for later display
-------------------------------------------------------------------------*/

MEM_plotdata * ISQ_plot_label( MCW_imseq *seq , char *lab )
{
   MEM_plotdata *mp ; int ww ; float asp , dd ;
   static int sz[5] = { 20 , 28 , 40 , 56 , 80 } ;  /* sz[j] = 20 * pow(2,0.5*j) */
   char *eee ; float rr=1.0,gg=1.0,bb=0.8 , sb=0.003 ;

ENTRY("ISQ_plot_label") ;

   if( !ISQ_REALZ(seq) || lab  == NULL ) RETURN(NULL) ;

   asp = 1.0 ;

   /* set character size (units = 0.001 of plot width) */

   ww = sz[seq->wbar_labsz_av->ival] ;
   if( asp > 1.0 ) ww = (int)(ww/asp+0.5) ;

   dd = 0.0007*ww ;  /* offset from edge */

   create_memplot_surely( "Ilabelplot" , asp ) ;
   set_thick_memplot(0.0) ;

   /* get the color to plot with */

   eee = getenv("AFNI_IMAGE_LABEL_COLOR") ;
   if( eee != NULL )
      DC_parse_color( seq->dc , eee , &rr,&gg,&bb ) ;
   set_color_memplot(rr,gg,bb) ;

   /* get the setback */

   eee = getenv("AFNI_IMAGE_LABEL_SETBACK") ;
   if( eee != NULL ){
      float ss = strtod(eee,NULL) ;
      if( ss >= 0.0 && ss < 0.5 ) sb = ss ;
   }

   /* plot the label */

   switch( seq->wbar_label_av->ival ){
      default:
      case ISQ_LABEL_UPLF:
         plotpak_pwritf( sb,1.0-dd-sb , lab , ww , 0 , -1 ) ; break ;

      case ISQ_LABEL_UPRT:
         plotpak_pwritf( asp-sb,1.0-dd-sb , lab , ww , 0 ,  1 ) ; break ;

      case ISQ_LABEL_DNLF:
         plotpak_pwritf( sb,dd+sb , lab , ww , 0 , -1 ) ; break ;

      case ISQ_LABEL_DNRT:
         plotpak_pwritf( asp-sb,dd+sb , lab , ww , 0 ,  1 ) ; break ;

      case ISQ_LABEL_UPMD:
         plotpak_pwritf( 0.5*asp,1.0-dd-sb , lab , ww , 0 , 0 ) ; break ;

      case ISQ_LABEL_DNMD:
         plotpak_pwritf( 0.5*asp,dd+sb , lab , ww , 0 , 0 ) ; break ;
   }

   mp = get_active_memplot() ; RETURN(mp) ;
}

/*-----------------------------------------------------------------------
   process an MRI_IMAGE from the user into a scaled format for display
   -- the output will be MRI_short (grayscale index) or MRI_rgb
-------------------------------------------------------------------------*/

MRI_IMAGE * ISQ_process_mri( int nn , MCW_imseq *seq , MRI_IMAGE *im )
{
   MRI_IMAGE *newim , *flipim , *lim ;
   int  scl_grp ;
   short clbot=0 , cltop=0 ;
   int must_rescale = 1 ;     /* 31 Jan 2002: always turn this on */
   int have_transform ;

ENTRY("ISQ_process_mri") ;

   seq->clbot = seq->cltop = 0.0 ; /* 29 Jul 2001 */

   if( ! ISQ_VALID(seq) || im == NULL ) RETURN(NULL) ;

   /*** Feb 7, 1996: deal with complex-valued images ***/

   lim = im ;  /* local image = input image, unless complex */

   if( im->kind == MRI_complex ){
      float *lar ; complex *cxar ; int ii , npix ;

      DPRI("complex to real code = ",seq->opt.cx_code) ;

      lim  = mri_new( im->nx , im->ny , MRI_float ) ;
      lar  = MRI_FLOAT_PTR(lim) ;
      cxar = MRI_COMPLEX_PTR(im) ;
      npix = im->nx * im->ny ;
      MRI_COPY_AUX(lim,im) ;
      must_rescale = 1 ;  /** force rescaling of image later **/

      switch( seq->opt.cx_code ){

         default:
         case ISQ_CX_MAG:
            for( ii=0 ; ii < npix ; ii++ ) lar[ii] = CABS(cxar[ii]) ;
         break ;

         case ISQ_CX_PHASE:
            for( ii=0 ; ii < npix ; ii++ ) lar[ii] = CARG(cxar[ii]) ;
         break ;

         case ISQ_CX_REAL:
            for( ii=0 ; ii < npix ; ii++ ) lar[ii] = cxar[ii].r ;
         break ;

         case ISQ_CX_IMAG:
            for( ii=0 ; ii < npix ; ii++ ) lar[ii] = cxar[ii].i ;
         break ;
      }
   }

   have_transform = (seq->transform0D_func != NULL ||
                     seq->transform2D_func != NULL   ) ;

   /****** 11 Feb 1999: if input RGB image, do limited processing *****/

   if( lim->kind == MRI_rgb ){
      MRI_IMAGE *tim , *qim ;

      /** 26 Apr 2005: apply transforms to the intensity channel? **/

      if( have_transform ) qim = mri_copy( lim ) ;
      else                 qim = lim ;

      if( seq->transform0D_func != NULL )
        mri_rgb_transform_nD( qim, 0, seq->transform0D_func ) ;

      if( seq->transform2D_func != NULL )
        mri_rgb_transform_nD( qim, 2, seq->transform2D_func ) ;

      /** histogram flattening (very useless) **/

      if( (seq->opt.improc_code & ISQ_IMPROC_FLAT) != 0 ){
        tim = mri_flatten_rgb( qim ) ;
        if( qim != lim ) mri_free(qim) ;
        qim = tim ;
      }

      /** sharpening (sometimes useful) **/

      if( (seq->opt.improc_code & ISQ_IMPROC_SHARP) != 0 ){
        tim = mri_sharpen_rgb( seq->sharp_fac , qim ) ;
        if( qim != lim ) mri_free(qim) ;
        qim = tim ;
      }

      /** create output:
           copy of input, if input was unmodified above,
           otherwise, the edited/filtered result from above **/

      if( qim == lim )
        newim = mri_copy( lim ) ;   /* just copy it */
      else
        newim = qim ;               /* is already what we want */

      /** 25 Apr 2005: modify image via rgb_gamma exponent? **/

      if( fabs(1.0-seq->rgb_gamma)  > 0.02 || fabs(seq->rgb_offset) > 0.01 ){
        register int npix = newim->nx * newim->ny , ii ;
        register byte *ar = MRI_RGB_PTR(newim) ;
        double gg = seq->rgb_gamma ;
        float  aa = seq->rgb_offset , rv,gv,bv , mx ;

        if( aa > 0.9 ) aa = 0.9; else if( aa < -0.9 ) aa = -0.9;
        for( ii=0 ; ii < npix ; ii++ ){
          if( ar[3*ii] > 0 || ar[3*ii+1] > 0 || ar[3*ii+2] > 0 ){
            if( aa != 0.0 ){
              rv = ar[3*ii]   ; gv = ar[3*ii+1] ; bv = ar[3*ii+2] ;
              mx = MAX(rv,gv) ; mx = (255.0*aa) / MAX(mx,bv) ;
              rv *= mx; gv *= mx; bv *= mx;
            } else {
              rv = gv = bv = 0.0 ;
            }
            rv += (float)(255.0*pow(ar[3*ii  ]/255.0,gg)) ;
            gv += (float)(255.0*pow(ar[3*ii+1]/255.0,gg)) ;
            bv += (float)(255.0*pow(ar[3*ii+2]/255.0,gg)) ;
            mx = MAX(rv,gv) ; mx = MAX(mx,bv) ;
            if( mx > 255.0 ){ mx = 255.0/mx; rv *= mx; gv *= mx; bv *= mx; }
            ar[3*ii  ] = BYTEIZE(rv) ;
            ar[3*ii+1] = BYTEIZE(gv) ;
            ar[3*ii+2] = BYTEIZE(bv) ;
          }
        }
      }

      /** save the 'original' image in float format? **/

      if( seq->set_orim ){                    /* for graphs */
        KILL_1MRI(seq->orim) ;
        seq->orim = mri_to_float(newim) ;    /* intensity image */
      }

      /** 11 May 2004: fill (0,0,0) pixels with zero color? **/

      if( seq->zer_color > 0 ){
        register int npix = newim->nx * newim->ny , ii ;
        register byte rz,gz,bz , *ar = MRI_RGB_PTR(newim) ;
        rz = DCOV_REDBYTE  (seq->dc,seq->zer_color) ;  /* zero  */
        gz = DCOV_GREENBYTE(seq->dc,seq->zer_color) ;  /* color */
        bz = DCOV_BLUEBYTE (seq->dc,seq->zer_color) ;  /* RGBs  */
        for( ii=0 ; ii < npix ; ii++ )
          if( ar[3*ii] == 0 && ar[3*ii+1] == 0 && ar[3*ii+2] == 0 ){
            ar[3*ii] = rz ; ar[3*ii+1] = gz ; ar[3*ii+2] = bz ;
          }
      }

      seq->scl_label[0] = '\0' ;
   }  /** end of RGB processing **/

   /****** Not RGB ==>                                             ******/
   /****** process image in normal fashion if no IMPROC code given ******/

   else if( ! have_transform && seq->opt.improc_code == ISQ_IMPROC_NONE ){

      if( seq->set_orim ){                   /* 30 Dec 1998 */
        KILL_1MRI(seq->orim) ;
        seq->orim = mri_to_float( lim ) ;
      }

      if( !must_rescale && ISQ_DOING_SLICE_PROJ(seq) ) must_rescale = 1 ;

      /*----- first, set scaling based on user desires -----*/

      if( nn < seq->status->num_series ){
        scl_grp = seq->opt.scale_group ; /* in series -> can groupscale */
      } else {
        scl_grp = ISQ_SCL_AUTO ;         /* not in series -> must autoscale */
      }

      if( seq->rng_bot < seq->rng_top ) scl_grp = ISQ_SCL_USER ;

      switch( scl_grp ){

         case ISQ_SCL_USER:{    /* scale from user input ranges */
           ISQ_SCLEV( seq->rng_bot,seq->rng_top ,
                      seq->dc->ncol_im , seq->scl,seq->lev ) ;
           clbot = seq->clbot = seq->rng_bot ;
           cltop = seq->cltop = seq->rng_top ;
           if( seq->rng_extern ) strcpy(seq->scl_label,"[Glob]") ;
           else                  strcpy(seq->scl_label,"[User]") ;
         }
         break ; /* end of user input range scaling */

         default:               /* scale on individual image statistics */
         case ISQ_SCL_AUTO:{
           ISQ_indiv_statistics *st = &( seq->imstat[nn] ) ;
           int scrang = seq->opt.scale_range ;

           if( seq->top_clip <= 0.0f && scrang == ISQ_RNG_CLIPPED ){
             ALLOW_CLIPPING( seq , 0 ) ;
             scrang = seq->opt.scale_range ;
           }

           if( must_rescale ) st->one_done = False ;

           if( ! st->one_done ) ISQ_statify_one( seq , nn , lim ) ;

           /* 09 Jan 2004: adjust scaling method for image entropy */

           if( scrang == ISQ_RNG_02TO98 ){
             double ent_th=AFNI_numenv("AFNI_IMAGE_ENTROPY") ;
             if( ent_th >= 0.0 ){
               if( ent_th == 0.0 ) ent_th = 0.05 ;  /* 10 Jan 2004 */
               if( st->entropy < ent_th ) scrang = ISQ_RNG_MINTOMAX ;
             }
           }

           switch( scrang ){

             default:
             case ISQ_RNG_MINTOMAX:
               seq->scl = st->scl_mm ;
               seq->lev = st->lev_mm ;
               seq->clbot = st->min ;   /* 29 Jul 2001 */
               seq->cltop = st->max ;
               strcpy(seq->scl_label,"[Min2Max]") ;
             break ;

             case ISQ_RNG_02TO98:
               seq->scl = st->scl_per ;
               seq->lev = st->lev_per ;
               clbot = seq->clbot = st->per02 ;
               cltop = seq->cltop = st->per98 ;
               strcpy(seq->scl_label,"[2%-98%]") ;
             break ;

             case ISQ_RNG_CLIPPED:{
               float bf=AFNI_numenv("AFNI_IMAGE_CLIPBOT") , bc ;
               float tf=AFNI_numenv("AFNI_IMAGE_CLIPTOP") , tc ;
               if( bf < 0.0f || bf > 0.5f ) bf = 0.25f ;
               if( tf < 0.6f || bf > 1.9f ) tf = 1.00f ;
               bc = bf * seq->top_clip ;
               tc = tf * seq->top_clip ;
               ISQ_SCLEV( bc,tc ,
                          seq->dc->ncol_im , seq->scl,seq->lev ) ;
               clbot = seq->clbot = bc ;
               cltop = seq->cltop = tc ;
               strcpy(seq->scl_label,"[clipped]") ;
               seq->redo_clip = 1 ;
             }
             break ;
           }
         }
         break ;  /* end of autoscaling */

         case ISQ_SCL_GRP:{         /* scale on group statistics */
            ISQ_glob_statistics *gl = seq->glstat ;

            switch( seq->opt.scale_range ){

               default:
               case ISQ_RNG_MINTOMAX:
                 if( ! gl->mm_done ) ISQ_statify_all( seq , True ) ;
                 seq->scl = gl->scl_mm ;
                 seq->lev = gl->lev_mm ;
                 seq->clbot = gl->min ;   /* 29 Jul 2001 */
                 seq->cltop = gl->max ;
                 strcpy(seq->scl_label,"[Min2Max]") ;
               break ;

               case ISQ_RNG_02TO98:
                 if( ! gl->per_done ) ISQ_statify_all( seq , False ) ;
                 seq->scl = gl->scl_per ;
                 seq->lev = gl->lev_per ;
                 clbot = seq->clbot = gl->per02 ;
                 cltop = seq->cltop = gl->per98 ;
                 strcpy(seq->scl_label,"[2%-98%]") ;
               break ;
            }
         }
         break ;  /* end of groupscaling */
      }  /* end of scaling */

      /* 11/30/94 fix: mri_to_short_sclip has problems with short overflow */

#if 0
      if( lim->kind == MRI_short && clbot < cltop ){

         int npix = lim->nx * lim->ny , ii ;
         short *ar = MRI_SHORT_PTR(lim) ;

         if( seq->rng_ztop == 0 ){
            for( ii=0 ; ii < npix ; ii++ )
                    if( ar[ii] < clbot ) ar[ii] = clbot ;
               else if( ar[ii] > cltop ) ar[ii] = cltop ;
         } else {
            for( ii=0 ; ii < npix ; ii++ )
                    if( ar[ii] < clbot || ar[ii] > cltop ) ar[ii] = clbot ;
         }

      } else if( lim->kind == MRI_byte && clbot < cltop ){

         int npix = lim->nx * lim->ny , ii ;
         byte *ar = MRI_BYTE_PTR(lim) ;

         if( seq->rng_ztop == 0 ){
            for( ii=0 ; ii < npix ; ii++ )
                    if( ar[ii] < clbot ) ar[ii] = clbot ;
               else if( ar[ii] > cltop ) ar[ii] = cltop ;
         } else {
            for( ii=0 ; ii < npix ; ii++ )
                    if( ar[ii] < clbot || ar[ii] > cltop ) ar[ii] = clbot ;
         }
      }
#endif

      /*----- next, scale image as defined above -----*/

STATUS("scaling to shorts") ;

                               /* scaling   to zero   clip bot  clip top */
                               /* --------  --------  --------  -------- */
      newim = mri_to_short_sclip( seq->scl, seq->lev, seq->bot, seq->top, lim );

   /****** end of normal processing; handle special image processing below ******/

   } else {
      MRI_IMAGE *tim , *qim ;
      double scl , lev ;
      float hbot,htop ;

STATUS("begin IMPROCessing") ;

      qim = lim ;  /* at the start of each process stage,
                      qim is the image to process;
                      tim is an intermediate temporary image */

      /***** 30 Oct 1996: transform image *****/

      if( seq->transform0D_func != NULL ){
         tim = mri_to_float(qim) ;
#if 0
         seq->transform0D_func( tim->nvox , MRI_FLOAT_PTR(tim) ) ;
#else
         AFNI_CALL_0D_function( seq->transform0D_func ,
                                tim->nvox , MRI_FLOAT_PTR(tim) ) ;
#endif
         if( qim != lim ) mri_free(qim) ;
         qim = tim ;
      }

      if( seq->transform2D_func != NULL ){
         tim = mri_to_float(qim) ;
#if 0
         seq->transform2D_func( tim->nx , tim->ny ,
                                tim->dx , tim->dy , MRI_FLOAT_PTR(tim) ) ;
#else
         AFNI_CALL_2D_function( seq->transform2D_func ,
                                tim->nx , tim->ny ,
                                tim->dx , tim->dy , MRI_FLOAT_PTR(tim) ) ;
#endif
         if( qim != lim ) mri_free(qim) ;
         qim = tim ;
      }

      /*** flatten ***/

      if( (seq->opt.improc_code & ISQ_IMPROC_FLAT) != 0 ){
STATUS("call mri_flatten") ;
         tim = mri_flatten( qim ) ;
         if( qim != lim ) mri_free(qim) ;
         qim = tim ;

         if( seq->opt.scale_range == ISQ_RNG_02TO98 &&
             seq->flat_top > seq->flat_bot ){

            float *qar = MRI_FLOAT_PTR(qim) ;
            int ii , npix = qim->nx * qim->ny ;

STATUS("clip flattened image") ;

            for( ii=0 ; ii < npix ; ii++ ){
                    if( qar[ii] < seq->flat_bot ) qar[ii] = seq->flat_bot ;
               else if( qar[ii] > seq->flat_top ) qar[ii] = seq->flat_top ;
            }
         }
      }

      /*** sharpen ***/

      if( (seq->opt.improc_code & ISQ_IMPROC_SHARP) != 0 ){
STATUS("call mri_sharpen") ;
         tim = mri_sharpen( seq->sharp_fac , 0 , qim ) ;
         if( qim != lim ) mri_free(qim) ;
         qim = tim ;
      }

      /*** Sobel edge detection ***/

      if( (seq->opt.improc_code & ISQ_IMPROC_SOBEL) != 0 ){
         int ii , npix ;
         float *tar ;

STATUS("call mri_edit_image") ;
         tim = mri_edit_image( 0.10 , 1.0 , qim ) ;   /* soft clip small values */
         if( qim != lim ) mri_free(qim) ;
         qim = tim ;

STATUS("call mri_sobel") ;
         tim  = mri_sobel( 0 , 2 , qim ) ;            /* edge detect */

#if 0
         npix = tim->nx * tim->ny ;                   /* take square root */
         tar  = mri_data_pointer(tim) ;
         for( ii=0 ; ii < npix ; ii++ ) tar[ii] = sqrt(tar[ii]) ;
#endif

         if( qim != lim ) mri_free(qim) ;
         qim = tim ;
      }

      if( seq->set_orim ){                   /* 30 Dec 1998 */
         KILL_1MRI(seq->orim) ;
         seq->orim = mri_to_float( qim ) ;
      }

      /*** scale to shorts (cf. ISQ_statify_one) ***/

      hbot = mri_min(qim) ; htop = mri_max(qim) ;

STATUS("scale to shorts") ;
      switch( seq->opt.scale_range ){
         default:
         case ISQ_RNG_MINTOMAX:
            ISQ_SCLEV( hbot,htop , seq->dc->ncol_im , scl,lev ) ;
            seq->clbot = hbot ;  /* 29 Jul 2001 */
            seq->cltop = htop ;
            strcpy(seq->scl_label,"[Min2Max]") ;
         break ;

         case ISQ_RNG_02TO98:{
            static int hist[NHISTOG] ;
            float h02 , h98 ;

STATUS("call mri_histogram") ;
            mri_histogram( qim , hbot,htop , True , NHISTOG,hist ) ;
STATUS("call ISQ_perpoints") ;
            ISQ_perpoints( hbot,htop , hist , &h02 , &h98 ) ;
            ISQ_SCLEV( h02,h98 , seq->dc->ncol_im , scl,lev ) ;
            seq->clbot = h02 ;  /* 29 Jul 2001 */
            seq->cltop = h98 ;
            strcpy(seq->scl_label,"[2%-98%]") ;
         }
         break ;

         case ISQ_RNG_CLIPPED:{
           float bf=AFNI_numenv("AFNI_IMAGE_CLIPBOT") , bc ;
           float tf=AFNI_numenv("AFNI_IMAGE_CLIPTOP") , tc ;
           if( bf < 0.0f || bf > 0.5f ) bf = 0.25f ;
           if( tf < 0.6f || bf > 1.9f ) tf = 1.00f ;
           bc = bf * seq->top_clip ;
           tc = tf * seq->top_clip ;
           ISQ_SCLEV( bc,tc ,
                      seq->dc->ncol_im , scl,lev ) ;
           seq->clbot = bc ;
           seq->cltop = tc ;
           strcpy(seq->scl_label,"[clipped]") ;
           seq->redo_clip = 1 ;
         }
         break ;
      }

      newim = mri_to_short_sclip( scl , lev , seq->bot, seq->top, qim ) ;
      if( qim != lim ) mri_free(qim) ;
   }

   /**** at this point, the processed image is in "newim" ****/

   /** Aug 31, 1995: put zer_color in at bottom, if nonzero **/

   if( newim->kind == MRI_short && seq->zer_color > 0 ){
     short zz = -seq->zer_color ;
     short *ar = MRI_SHORT_PTR(newim) ;
     int npix = newim->nx * newim->ny , ii ;

     for( ii=0 ; ii < npix ; ii++ )
       if( ar[ii] == seq->bot ) ar[ii] = zz ;
   }

   /** copy pixel sizes, etc. (fixup for mrilib to be happy) **/

   MRI_COPY_AUX( newim , lim ) ;

   /*----- last, rotate/flip image to desired orientation -----*/

STATUS("call mri_flippo") ;
   flipim = mri_flippo( ISQ_TO_MRI_ROT(seq->opt.rot) , seq->opt.mirror , newim ) ;

   if( newim != flipim ) KILL_1MRI(newim) ;  /* discard the trash */
   if( lim   != im     ) KILL_1MRI(lim) ;    /* (if there is any) */

   if( seq->set_orim && seq->orim != NULL ){  /* 30 Dec 1998 */
     MRI_IMAGE *qim ;
     qim = mri_flippo( ISQ_TO_MRI_ROT(seq->opt.rot), seq->opt.mirror, seq->orim ) ;
     if( qim != seq->orim ){ KILL_1MRI(seq->orim) ; seq->orim = qim ; } ;
     MRI_COPY_AUX( seq->orim , flipim ) ;
     seq->set_orim = 0 ;
   }

   RETURN(flipim) ;
}

/*-------------------------------------------------------------------
  Callback handlers for color palette manipulation
---------------------------------------------------------------------*/

void ISQ_but_color_CB( Widget w , XtPointer client_data ,
                                  XtPointer call_data    )
{
   MCW_imseq *seq = (MCW_imseq *) client_data ;

ENTRY("ISQ_but_color_CB") ;

   if( ! ISQ_REALZ(seq) ) EXRETURN ;

   if( seq->dc->use_xcol_im ) DC_palette_setgray( seq->dc ) ;
   else                       DC_palette_setcolor( seq->dc ) ;

   COLORMAP_CHANGE(seq) ;      /* 22 Aug 1998 */
   ISQ_but_done_reset( seq ) ;
   EXRETURN ;
}

/*-------------------------------------------------------------------*/

void ISQ_but_cswap_CB( Widget w , XtPointer client_data ,
                                  XtPointer call_data    )
{
   MCW_imseq *seq = (MCW_imseq *) client_data ;

ENTRY("ISQ_but_cswap_CB") ;

   if( ! ISQ_REALZ(seq) ) EXRETURN ;

   DC_palette_swap( seq->dc ) ;
   COLORMAP_CHANGE(seq) ;      /* 22 Aug 1998 */
   ISQ_but_done_reset( seq ) ;
   EXRETURN ;
}

/*-------------------------------------------------------------------
   image saving options
---------------------------------------------------------------------*/

void ISQ_saver_CB( Widget w , XtPointer cd , MCW_choose_cbs *cbs )
{
   MCW_imseq *seq = (MCW_imseq *) cd ;
   int ii , kf ;
   MRI_IMAGE *tim , *flim ;
   char fname[256] ;
   THD_string_array *agif_list=NULL ; /* 27 Jul 2001 */
   char tsuf[8] ;                     /* 09 Dec 2002 */
   float dx,dy ;                      /* 08 Jun 2004 */
   int dbg ;                          /* 03 Sep 2004 */
   int adup=1 , akk,aa ;              /* 09 Feb 2009 */

#ifndef DONT_USE_METER
#  define METER_MINCOUNT 20
   Widget meter = NULL ;
   int meter_perc , meter_pold=0 , meter_pbase ;
#endif

ENTRY("ISQ_saver_CB") ;

   dbg = AFNI_yesenv("AFNI_IMSAVE_DEBUG") ;  /* 03 Sep 2004 */

   if( ppmto_agif_filter == NULL && DO_AGIF(seq) ){  /* 07 Apr 2005! */
      (void) MCW_popup_message( seq->wtop ,
                                "Animated GIF AFNI logic error!\n"
                                "Report to " COXEMAIL , MCW_USER_KILL ) ;
      seq->opt.save_agif = 0 ;
      EXRETURN ;
   }
   if( ppmto_mpeg_filter == NULL && DO_MPEG(seq) ){
      (void) MCW_popup_message( seq->wtop ,
                                "MPEG-1 AFNI logic error!\n"
                                "Report to " COXEMAIL , MCW_USER_KILL ) ;
      seq->opt.save_mpeg = 0 ;
      EXRETURN ;
   }

   /*---------------*/

   if( seq->saver_prefix == NULL ){  /* just got a string */
      int ll , ii ;

      if( cbs->reason != mcwCR_string ||
          cbs->cval == NULL           || (ll = strlen(cbs->cval)) == 0 ){

         XBell( XtDisplay(w) , 100 ) ; EXRETURN ;
      }

      seq->saver_prefix = (char*)XtMalloc( sizeof(char) * (ll+8) ) ;
      strcpy( seq->saver_prefix , cbs->cval ) ;

      if( seq->saver_prefix[ll-1] != '.' ){  /* add a . at the end */
         seq->saver_prefix[ll++] = '.' ;     /* if one isn't there */
         seq->saver_prefix[ll]   = '\0' ;
      }

      /*-- check that the prefix is acceptable --*/

      if( dbg ) fprintf(stderr,"IMSAVE: got prefix '%s'\n",seq->saver_prefix);

      ll = strlen(seq->saver_prefix) ;

      for( ii=0 ; ii < ll ; ii++ )
         if( iscntrl(seq->saver_prefix[ii]) ||
             isspace(seq->saver_prefix[ii])   ) break ;

      if( ii < ll || ll < 2 || ll > 240 ){
         XBell( XtDisplay(w) , 100 ) ;
         myXtFree( seq->saver_prefix ) ; seq->saver_prefix = NULL ;
         EXRETURN ;
      }

      /*-- April 1996: Save One case here --*/

      if( seq->opt.save_one && !DO_ANIM(seq) ){
         char *ppnm = strstr( seq->saver_prefix , ".pnm." ) ;
         int   sll  = strlen( seq->saver_prefix ) ;

         int    mcod = X2M_USE_CMAP ;        /* 21 Sep 2001: */
         if( seq->opt.save_filter >= 0 ||
             seq->mplot != NULL          )   /* compute mcod rather than */
           mcod |= X2M_FORCE_RGB ;           /* use fixed X2M_USE_CMAP   */

         /* undump XImage to MRI_IMAGE (rgb format) */

         if( dbg ) fprintf(stderr,"IMSAVE: convert XImage to RGB\n") ;

         reload_DC_colordef( seq->dc ) ;  /* 23 Mar 1999 */
         tim = XImage_to_mri( seq->dc , seq->given_xim , mcod ) ; /* 21 Sep 2001: */
                                                                  /* X2M_USE_CMAP -> mcod */

/* INFO_message("AFNI_IMAGE_SAVESQUARE = %s",getenv("AFNI_IMAGE_SAVESQUARE")); */
         if( AFNI_yesenv("AFNI_IMAGE_SAVESQUARE") ){   /* 08 Jun 2004 */
           tim->dx = seq->last_dx ; tim->dy = seq->last_dy ;
           if( dbg ) fprintf(stderr,"  square-ize aspect\n") ;
           flim = mri_squareaspect( tim ) ;
           if( flim != NULL ){ mri_free(tim); tim = flim; }
         }

         /* 23 Mar 2002: zoom out, if ordered */

         if( seq->zoom_fac >  1    &&
             seq->mont_nx  == 1    &&
             seq->mont_ny  == 1    &&
             tim           != NULL && tim->kind == MRI_rgb ){

           MRI_IMAGE *qim ;
           if( dbg ) fprintf(stderr,"  zooming\n") ;
           qim = mri_dup2D(seq->zoom_fac,tim) ;
           mri_free(tim) ; tim = qim ;
         }

         /* 23 Mar 2002: draw overlay lines on top, if any */

         if( tim != NULL && seq->mplot != NULL && tim->kind == MRI_rgb ){
           if( dbg ) fprintf(stderr,"  overlay geometry stuff\n") ;
           memplot_to_RGB_sef( tim, seq->mplot, 0,0,MEMPLOT_FREE_ASPECT ) ;
         }

         /* 25 Mar 2002: perhaps cut up zoomed image
                         (after overlay is drawn on it, that is) */

         if( seq->zoom_fac >  1               &&
             seq->mont_nx  == 1               &&
             seq->mont_ny  == 1               &&
             tim           != NULL            &&
             tim->kind     == MRI_rgb         &&
             AFNI_yesenv("AFNI_CROP_ZOOMSAVE")  ) {

            MRI_IMAGE *qim ;
            int xa,ya , iw=tim->nx/seq->zoom_fac , ih=tim->ny/seq->zoom_fac ;

            if( dbg ) fprintf(stderr,"  crop zoomed image\n") ;
            xa = seq->zoom_hor_off * tim->nx ;
            if( xa+iw > tim->nx ) xa = tim->nx-iw ;
            ya = seq->zoom_ver_off * tim->nx ;
            if( ya+ih > tim->ny ) ya = tim->ny-ih ;
            qim = mri_cut_2D( tim , xa,xa+iw-1 , ya,ya+ih-1 ) ;
            mri_free(tim) ; tim = qim ;
         }

         /* save image to disk */

         if( tim != NULL ){                  /* if we have image, that is */
            static int warned=0 ;

            if( seq->opt.save_filter < 0 ){  /* the old code: dump to PNM file */

               if( ppnm == seq->saver_prefix + (sll-5) )  /* 17 June 1997 */
                  seq->saver_prefix[sll-1] = '\0' ;
               else
                  strcat(seq->saver_prefix,"pnm") ;

               INFO_message("Writing one %dx%d PNM image to file %s",
                            tim->nx,tim->ny,seq->saver_prefix) ;
               mri_write_pnm( seq->saver_prefix , tim ) ;

            } else {  /* 26 Jul 2001: allow Save One in filtered formats */

               char filt[512] ; int ff=seq->opt.save_filter ; FILE *fp ;
               int pc ;

               /* open a pipe to the filter function */

               sprintf(filt,".%s.",ppmto_suffix[ff]) ;
               if( STRING_HAS_SUFFIX_CASE(seq->saver_prefix,filt) ){
                 strcpy(fname,seq->saver_prefix) ;
                 fname[sll-1] = '\0' ;
               } else {
                 sprintf( fname, "%s%s", seq->saver_prefix, ppmto_suffix[ff] ) ;
               }
               sprintf( filt , ppmto_filter[ff] , fname ) ;
               INFO_message("Writing one %dx%d image to file %s",
                            tim->nx,tim->ny,fname) ;
#ifndef CYGWIN
               signal( SIGPIPE , SIG_IGN ) ;
#endif
               errno = 0 ;
               fp = popen( filt , "w" ) ;
               if( fp == NULL ){
                  fprintf(stderr,"** Can't open output filter: %s\a\n",filt) ;
                  if( errno != 0 ) perror("** Unix error message") ;
                  POPDOWN_string_chooser ; mri_free(tim) ; EXRETURN ;
               }

               /* write a PPM file to the filter pipe */

               fprintf(fp,"P6\n%d %d\n255\n" , tim->nx,tim->ny ) ;
               fwrite( MRI_RGB_PTR(tim), sizeof(byte), 3*tim->nvox, fp ) ;
               pc = pclose(fp) ;
               if( pc == -1 ){
                  perror("** Error in image output pipe") ;
                  fprintf(stderr,"** filter command was %s\n",filt) ;
                  POPDOWN_string_chooser ; mri_free(tim) ; EXRETURN ;
               }
            }

            mri_free( tim ) ; tim = NULL ;  /* 17 June 1997 */

            if( seq->dc->visual_class == TrueColor &&
                seq->dc->depth == 16               && !warned ){ /* 30 May 2000 */

               warned = 1 ;
               fprintf(stderr,
                "\n"
                "*** WARNING: Save One with X11 TrueColor depth=16 can ***\n"
                "***          result in gray pixels not having R=G=B.  ***\n");
            }

         } else {
            XBell( XtDisplay(w) , 100 ) ;  /* image creation failed! */
         }
         myXtFree( seq->saver_prefix ) ; seq->saver_prefix = NULL ;
         POPDOWN_string_chooser ;
         EXRETURN ;
      }

      /*-- Not doing Save:One, so    --*/
      /*-- move on to the From value --*/

      POPDOWN_string_chooser ;

      MCW_choose_integer( w , "Image from" ,
                          0 , seq->status->num_total-1 , 0 ,
                          ISQ_saver_CB , (XtPointer) seq ) ;

      seq->saver_from = -1 ;
      EXRETURN ;
   }

   /*--- got 'From' value ---*/

   if( seq->saver_from == -1 ){  /* just got an integer */

      if( cbs->reason != mcwCR_integer ){  /* error */
         XBell( XtDisplay(w) , 100 ) ;
         myXtFree( seq->saver_prefix ) ; seq->saver_prefix = NULL ;
         EXRETURN ;
      }

      if( dbg ) fprintf(stderr,"IMSAVE: got From=%d\n",cbs->ival) ;
      seq->saver_from = cbs->ival ;

      POPDOWN_integer_chooser ;

      MCW_choose_integer(
          w , "Image to" ,
          0 , seq->status->num_total-1 , seq->status->num_total-1 ,
          ISQ_saver_CB , (XtPointer) seq ) ;

      seq->saver_to = -1 ;
      EXRETURN ;
   }

   /*--- go 'To' value ==> last call ---*/

   if( cbs->reason != mcwCR_integer ){  /* error */
      XBell( XtDisplay(w) , 100 ) ;
      myXtFree( seq->saver_prefix ) ; seq->saver_prefix = NULL ;
      EXRETURN ;
   }

   POPDOWN_integer_chooser ;

   if( dbg ) fprintf(stderr,"IMSAVE: got To  =%d\n",cbs->ival) ;

   seq->saver_to = cbs->ival ;

   /* check if all inputs are good */

   if( seq->saver_prefix == NULL ||
       seq->saver_from < 0       ||
       seq->saver_to   < 0       ||
       seq->saver_from > seq->status->num_total-1 ||
       seq->saver_to   > seq->status->num_total-1   ){  /* error */

      XBell( XtDisplay(w) , 100 ) ;
      myXtFree( seq->saver_prefix ) ; seq->saver_prefix = NULL ;
      EXRETURN ;
   }

   if( seq->saver_from > seq->saver_to ){  /* inverted order? */
      ii              = seq->saver_from ;
      seq->saver_from = seq->saver_to ;
      seq->saver_to   = ii ;
   }

#ifndef DONT_USE_METER
   meter_pbase = seq->saver_to - seq->saver_from ;
   if( meter_pbase >= METER_MINCOUNT ){
      meter = MCW_popup_meter( seq->wtop , METER_TOP_WIDE ) ;
      meter_pold = 0 ;
   } else {
      meter = NULL ;
   }
#endif

   if( DO_ANIM(seq) ){                     /* 09 Dec 2002:  */
     tsuf[0] = (lrand48()>>5)%26 + 'A' ;   /* random suffix */
     tsuf[1] = (lrand48()>>5)%26 + 'A' ;   /* for animation */
     tsuf[2] = (lrand48()>>5)%26 + 'A' ;   /* temp files    */
     tsuf[3] = '\0' ;
     adup = (ISQ_anim_dup > 0) ? ISQ_anim_dup : AFNI_numenv("AFNI_ANIM_DUP") ;
     if( adup <= 0 ) adup = 1 ; else if( adup > 99 ) adup = 99 ;
     if( dbg ) fprintf(stderr,"IMSAVE: animation suffix='%s' adup=%d\n",tsuf,adup) ;
   } else {
     tsuf[0] = '\0' ;                      /* not used */
     adup    = 1 ;
   }

#ifdef USE_GIFF          /* create the fixed GIF colormap for animations */
   if( DO_AGIF(seq) ){
     MRI_IMAGE *im = mri_colorsetup( 76 , 6,6,5 ); /* 76 gray levels + */
     remove( GIFF_MAPFILE ) ;                     /* 6*red X 6*green X 5*blue */
     mri_write_pnm( GIFF_MAPFILE , im ) ;
     mri_free( im ) ;
   }
#endif

   /*---- loop thru, get images, save them ----*/

   for( akk=0,kf=seq->saver_from ; kf <= seq->saver_to ; kf++ ){

      /* get the underlay image */

      if( dbg ) fprintf(stderr,"IMSAVE: fetch underlay image #%d\n",kf) ;

      tim = ISQ_getimage( kf , seq ) ;

      /* if we failed to get the image? */

      if( tim == NULL ){
         if( kf == seq->saver_to && agif_list != NULL ){ /* 19 Sep 2001 */
            fprintf(stderr,
                    "** Can't save animation: last image in list is NULL!\n");
            DESTROY_SARR(agif_list) ;
         }
         continue ;  /* skip to next one */
      }

      /* image to save will be in flim */

      flim = tim ;

#ifndef DONT_USE_METER
      if( meter != NULL ){
        meter_perc = (int)(100.9 * (kf - seq->saver_from) / meter_pbase) ;
        if( meter_perc != meter_pold ){
          if( dbg ) fprintf(stderr,"  set meter to %d\n",meter_perc) ;
          MCW_set_meter( meter , meter_perc ) ;
          meter_pold = meter_perc ;
        }
      }
#endif

      /*-- 27 Jun 2001: write image through a filter? --*/

      if( seq->opt.save_filter >= 0 || DO_ANIM(seq) ){
         char filt[512] ; int ff=seq->opt.save_filter ; FILE *fp ;
         MRI_IMAGE *ovim=NULL ;
         int nx , ny , npix , pc ;

         /* process image to make the grayscale index */

         if( dbg ) fprintf(stderr,"  process image\n") ;

         seq->set_orim = 0 ;
         tim  = flim ;
         flim = ISQ_process_mri( kf , seq , tim ) ;
         if( tim != flim ) KILL_1MRI( tim ) ;

         /* get overlay and flip it */

         if( !ISQ_SKIP_OVERLAY(seq) ){
            if( dbg ) fprintf(stderr,"  fetch overlay image\n") ;
            tim = ISQ_getoverlay( kf , seq ) ;
            if( tim != NULL && !ISQ_GOOD_OVERLAY_TYPE(tim->kind) ){
               KILL_1MRI(tim) ;
            }
            if( dbg ) fprintf(stderr,"  flip overlay?\n") ;
            if( tim != NULL )
              ovim = mri_flippo( ISQ_TO_MRI_ROT(seq->opt.rot), seq->opt.mirror, tim );
            if( tim != ovim ) KILL_1MRI(tim) ;
         }

         /* and perform overlay onto flim */

         if( ovim != NULL ){
            tim = flim ;
            if( dbg ) fprintf(stderr,"  merge overlay and underlay images\n") ;
            flim = ISQ_overlay( seq->dc , tim , ovim , seq->ov_opacity ) ;
            if( flim == NULL ){ flim = tim ; }     /* shouldn't happen */
            else              { KILL_1MRI(tim) ; }
            mri_free( ovim ) ;
         }

/* INFO_message("AFNI_IMAGE_SAVESQUARE = %s",getenv("AFNI_IMAGE_SAVESQUARE")); */

         if( AFNI_yesenv("AFNI_IMAGE_SAVESQUARE") ){   /* 08 Jun 2004 */
           flim->dx = seq->last_dx ; flim->dy = seq->last_dy ;
           if( dbg ) fprintf(stderr,"  square-ize aspect ratio\n") ;
           tim = mri_squareaspect( flim ) ;
           if( tim != NULL ){ mri_free(flim); flim = tim; }
         }

         /* if needed, convert from indices to RGB */

         if( flim->kind == MRI_short ){
           if( dbg ) fprintf(stderr,"  convert to RGB\n") ;
           tim = ISQ_index_to_rgb( seq->dc , 0 , flim ) ;
           mri_free(flim) ; flim = tim ;
         }

         /* 26 Mar 2002: zoom out, and geometry overlay, maybe */

         if( seq->zoom_fac > 1 && seq->mont_nx == 1 && seq->mont_ny == 1 ){
           if( dbg ) fprintf(stderr,"  zoom zoom zoom\n") ;
           tim=mri_dup2D(seq->zoom_fac,flim) ;
           mri_free(flim) ; flim = tim ;
         }

         if( MCW_val_bbox(seq->wbar_plots_bbox) != 0 ){  /* draw geometry overlay */
           MEM_plotdata *mp ;
           if( dbg ) fprintf(stderr,"  get geometry overlay?\n") ;
           mp = ISQ_getmemplot( kf , seq ) ;
           if( mp != NULL ){
             if( dbg ) fprintf(stderr,"  perform geometry overlay\n") ;
             flip_memplot( ISQ_TO_MRI_ROT(seq->opt.rot),seq->opt.mirror,mp );
             memplot_to_RGB_sef( flim, mp, 0,0,MEMPLOT_FREE_ASPECT ) ;
             delete_memplot(mp) ;
           }
         }

         if( seq->wbar_label_av->ival != 0 ){  /* 17 Jun 2005 */
           char *lab = ISQ_getlabel( kf , seq ) ;
           if( lab != NULL ){
             MEM_plotdata *mp = ISQ_plot_label( seq , lab ) ;
             if( mp != NULL ){
               memplot_to_RGB_sef( flim, mp, 0,0,MEMPLOT_FREE_ASPECT ) ;
               delete_memplot(mp) ;
             }
             free(lab) ;
           }
         }

         if( seq->zoom_fac > 1 &&                   /* crop zoomed image */
             seq->mont_nx == 1 &&                   /* to displayed part? */
             seq->mont_ny == 1 &&
             AFNI_yesenv("AFNI_CROP_ZOOMSAVE") ) {

           int xa,ya , iw=flim->nx/seq->zoom_fac , ih=flim->ny/seq->zoom_fac ;

           if( dbg ) fprintf(stderr,"  crop zoomed image\n") ;
           xa = seq->zoom_hor_off * flim->nx ;
           if( xa+iw > flim->nx ) xa = flim->nx-iw ;
           ya = seq->zoom_ver_off * flim->nx ;
           if( ya+ih > flim->ny ) ya = flim->ny-ih ;
           tim = mri_cut_2D( flim , xa,xa+iw-1 , ya,ya+ih-1 ) ;
           if( tim != NULL ){ mri_free(flim); flim = tim; }
         }

         /* image dimensions we are saving */

         nx = flim->nx ; ny = flim->ny ; npix = nx*ny ;

         /* write the output file */

         if( !DO_ANIM(seq) ){   /* don't write progress for animation */
           if( kf == seq->saver_from )
              printf("writing %d x %d .%s files",nx,ny,ppmto_suffix[ff]) ;
           else if( kf%10 == 5 )
              printf("." ) ;
           fflush(stdout) ;
         }

         /* create the filter command into string 'filt' */

         for( aa=0 ; aa < adup ; aa++,akk++ ){ /* adup==1 if no animation */
           if( !DO_ANIM(seq) ){                          /* arbitrary filtering */
             sprintf( fname, "%s%04d.%s", seq->saver_prefix, kf, ppmto_suffix[ff] ) ;
             sprintf( filt , ppmto_filter[ff] , fname ) ;
           } else if( DO_AGIF(seq) ){                    /* use the gif filter */
             sprintf( fname, "%s%s.%05d.gif" , seq->saver_prefix,tsuf, akk) ;
#ifndef USE_GIFF
             sprintf( filt , ppmto_gif_filter  , fname ) ;  /* free colormap */
#else
             sprintf( filt , ppmto_giff_filter , fname ) ;  /* fixed colormap */
#endif
             if( agif_list == NULL ) INIT_SARR(agif_list) ;
             ADDTO_SARR(agif_list,fname) ;
           } else if( DO_MPEG(seq) ){                    /* use the ppm filter */
             sprintf( fname, "%s%s.%06d.ppm" , seq->saver_prefix,tsuf, akk) ;
             sprintf( filt , ppmto_ppm_filter , fname ) ;
             if( agif_list == NULL ) INIT_SARR(agif_list) ;
             ADDTO_SARR(agif_list,fname) ;
           }
#ifndef CYGWIN
           signal( SIGPIPE , SIG_IGN ) ;                 /* ignore broken pipe */
#endif
           if( dbg ) fprintf(stderr,"  piping image to '%s'\n",filt) ;
           fp = popen( filt , "w" ) ;                    /* open pipe to filter */
           if( fp == NULL ){                             /* should not happen */
             ERROR_message("Can't open output filter %s",filt) ;
             break ;  /* out of loop over aa */
           }

           /* write RGB image to pipe as a PPM file */

           fprintf(fp,"P6\n%d %d\n255\n" , nx,ny ) ;
           fwrite( MRI_RGB_PTR(flim), sizeof(byte), 3*npix, fp ) ;
           pc = pclose(fp) ;
           if( pc == -1 ) perror("Error in image output pipe") ;
           if( dbg ) fprintf(stderr,"  pipe done\n") ;
         } /* loop over aa = image duplicates for animations */

         /* done with this image */

         mri_free(flim) ; flim = NULL ;

         /* 27 Jul 2001: if doing animation,
                         and if at final image, then create result */

         if( kf == seq->saver_to && agif_list != NULL ){

            int af ;

            if( agif_list->num == 0 ){
               ERROR_message("Can't save animation: no images in list!");
               goto AnimationCleanup ;
            }

            /* animated GIF */

            if( DO_AGIF(seq) ){
               int alen ; char *alc , *alf , *oof ;
#ifdef USE_GIFF
               remove( GIFF_MAPFILE ) ;   /* don't need this any longer */
#endif

               for( alen=af=0 ; af < agif_list->num ; af++ ) /* size of all */
                  alen += strlen( agif_list->ar[af] ) ;      /* filename strings */

               alen += 3*agif_list->num + 32 ;               /* all filenames */
               alc = AFMALL ( char, alen) ; alc[0] = '\0' ;  /* in one string */
               for( alen=af=0 ; af < agif_list->num ; af++ ){
                 strcat(alc," ") ; strcat(alc,agif_list->ar[af]) ;
               }

               oof  = AFMALL( char, strlen(seq->saver_prefix)+32 ) ; /* output fname */
               sprintf(oof,"%sgif",seq->saver_prefix) ;

               alen =  strlen(alc)+strlen(ppmto_agif_filter)+strlen(oof)+32 ;
               alf  = AFMALL( char, alen) ;
               sprintf(alf , ppmto_agif_filter, alc, oof ) ; /* command to run */
               INFO_message("Running '%s'\n",alf) ;
               system(alf) ;                                 /* so run it!    */
               free(alf) ; free(oof) ; free(alc) ;           /* free trash   */
            }

            /* MPEG-1 */

            else if( DO_MPEG(seq) ){ /* 02 Aug 2001 */
               int alen ; char *alf , *oof , *par , *frate ;
               char *qscale , *pattrn ;
               FILE *fpar ;

               /* write mpeg_encode parameter file */

               par = AFMALL( char, strlen(seq->saver_prefix)+32 ) ; /* param fname */
               sprintf(par,"%s%s.PARAM",seq->saver_prefix,tsuf) ;

               if( dbg ) fprintf(stderr,"  creating MPEG parameter file %s\n",par) ;
               fpar = fopen( par , "w" ) ;
               if( fpar == NULL ){ free(par) ; goto AnimationCleanup ; }
               oof = AFMALL( char, strlen(seq->saver_prefix)+32 ) ; /* output fname */
               sprintf(oof,"%smpg",seq->saver_prefix) ;
               qscale=getenv("AFNI_MPEG_QSCALE") ;if(qscale==NULL) qscale="11"   ;
               pattrn=getenv("AFNI_MPEG_PATTERN");if(pattrn==NULL) pattrn="IIIII";
               frate =getenv("AFNI_MPEG_FRAMERATE");if(frate==NULL)frate ="24"   ;
               fprintf(fpar,
                          "OUTPUT %s\n"             /* oof */
                          "GOP_SIZE          5\n"
                          "SLICES_PER_FRAME  1\n"
                          "FRAME_RATE        %s\n"  /* frate */
                          "BASE_FILE_FORMAT  PPM\n"
                          "INPUT_CONVERT     *\n"
                          "INPUT_DIR         .\n"
                          "PATTERN           %s\n"  /* pattrn */
                          "IQSCALE           %s\n"  /* qscale */
                          "PQSCALE           10\n"
                          "BQSCALE           25\n"
                          "PIXEL             HALF\n"
                          "RANGE             10 4\n"
                          "PSEARCH_ALG       LOGARITHMIC\n"
                          "BSEARCH_ALG       SIMPLE\n"
                          "REFERENCE_FRAME   ORIGINAL\n"
                          "INPUT\n"
                          "%s%s.*.ppm [%06d-%06d]\n"  /* prefix, tsuf, from, to */
                          "END_INPUT\n"
                       , oof , frate , pattrn , qscale ,
                         seq->saver_prefix,tsuf,0,akk-1 ) ;
               fclose(fpar) ;

               /* make command to run */

               alen = strlen(par)+strlen(ppmto_mpeg_filter)+32 ;
               alf  = AFMALL( char, alen) ;
               sprintf(alf , ppmto_mpeg_filter, par ) ; /* command to run */
               INFO_message("Running '%s' to produce %s\n",alf,oof) ;
               system(alf) ;                            /* so run it!    */
               remove(par); free(alf); free(oof); free(par); /* free trash   */
            }

            /* animation is done, for good or for ill */

            for( af=0 ; af < agif_list->num ; af++ )  /* erase temp files */
              remove( agif_list->ar[af] ) ;

          AnimationCleanup:
            DESTROY_SARR(agif_list) ;                 /* free more trash */
         }
      }

      /*---------------*/

      else if( flim->kind == MRI_rgb ){ /* 11 Feb 1998: write color image */
                                        /*              directly as PPM   */
         if( kf == seq->saver_from )
            printf("writing %d x %d RGB images",flim->nx,flim->ny) ;
         else if( kf%10 == 5 )
            printf("." ) ;
         fflush(stdout) ;

         seq->set_orim = 0 ;  /* 30 Dec 1998 */
         tim  = flim ;
         flim = ISQ_process_mri( kf , seq , tim ) ;  /* image processing */
         if( tim != flim ) KILL_1MRI( tim ) ;

/* INFO_message("AFNI_IMAGE_SAVESQUARE = %s",getenv("AFNI_IMAGE_SAVESQUARE")); */
         if( AFNI_yesenv("AFNI_IMAGE_SAVESQUARE") ){   /* 08 Jun 2004 */
           flim->dx = seq->last_dx ; flim->dy = seq->last_dy ;
           if( dbg ) fprintf(stderr,"  square-ate aspect ratio\n") ;
           tim = mri_squareaspect( flim ) ;
           if( tim != NULL ){ mri_free(flim); flim = tim; }
         }

         sprintf( fname , "%s%04d.pnm" , seq->saver_prefix , kf ) ;
         mri_write_pnm( fname , flim ) ;

         mri_free(flim) ; flim = NULL ; /* done with this image */

      /*---------------*/

      } else if( ! seq->opt.save_pnm ){ /** write background only **/

         if( seq->opt.save_nsize ){
           tim = mri_nsize( flim ) ;
           if( tim != NULL && tim != flim ){ mri_free(flim) ; flim = tim ; }
         }

         tim  = flim ;
         flim = mri_flippo( ISQ_TO_MRI_ROT(seq->opt.rot) , seq->opt.mirror , tim ) ;
         if( tim != flim ) KILL_1MRI( tim ) ;

         if( kf == seq->saver_from )
            printf("writing %d x %d images",flim->nx,flim->ny) ;
         else if( kf%10 == 5 )
            printf("." ) ;
         fflush(stdout) ;

         if( flim->kind == MRI_byte ){  /* 17 Feb 1999 */
            sprintf( fname , "%s%04d.pnm" , seq->saver_prefix , kf ) ;
            mri_write_pnm( fname , flim ) ; mri_free( flim ) ; flim = NULL ;
         } else {
            sprintf( fname , "%s%04d" , seq->saver_prefix , kf ) ;
            mri_write( fname , flim ) ; mri_free( flim ) ; flim = NULL ;
         }

      /*---------------*/

      } else { /** write color overlay and everything **/

         MRI_IMAGE * ovim=NULL ;
         int ii , nx , ny , npix , bb , allgray , ncode,nout ;
         byte * rgb ;   /* "byte" is defined in mrilib.h */
         short * flar ;
         XColor * ulc , * ovc , * xc ;
         FILE * fd ;
         byte rrr,ggg,bbb ;

         /* process given image to make the grayscale index */

         seq->set_orim = 0 ;  /* 30 Dec 1998 */
         tim  = flim ;
         flim = ISQ_process_mri( kf , seq , tim ) ;  /* will be shorts now */
         if( tim != flim ) KILL_1MRI( tim ) ;

         flar = mri_data_pointer(flim) ;  /* underlay image data */
         nx = flim->nx ;
         ny = flim->ny ; npix = flim->nx * flim->ny ;

         /* get overlay and flip it */

         if( !ISQ_SKIP_OVERLAY(seq) ){
            tim = ISQ_getoverlay( kf , seq ) ;
            if( tim != NULL && !ISQ_GOOD_OVERLAY_TYPE(tim->kind) ){
               KILL_1MRI(tim) ;
            }
            if( tim != NULL )
              ovim = mri_flippo( ISQ_TO_MRI_ROT(seq->opt.rot), seq->opt.mirror, tim ) ;
            if( tim != ovim ) KILL_1MRI(tim) ;
         }

         /* perform overlay onto flim [modified 07 Mar 2001] */

         if( ovim != NULL ){
#if 1
            tim = flim ;
            flim = ISQ_overlay( seq->dc , tim , ovim , seq->ov_opacity ) ;
            if( flim == NULL ){ flim = tim ; }     /* shouldn't happen */
            else              { KILL_1MRI(tim) ; }
#else
            short * ovar ; int jj ;                /* the old way */
            ovar = mri_data_pointer(ovim) ;
            for( jj=0 ; jj < npix ; jj++ )
               if( ovar[jj] != 0 ) flar[jj] = -ovar[jj] ;
#endif
            mri_free( ovim ) ;
         }

/* INFO_message("AFNI_IMAGE_SAVESQUARE = %s",getenv("AFNI_IMAGE_SAVESQUARE")); */
         if( AFNI_yesenv("AFNI_IMAGE_SAVESQUARE") ){   /* 08 Jun 2004 */
           flim->dx = seq->last_dx ; flim->dy = seq->last_dy ;
           tim = mri_squareaspect( flim ) ;
           if( tim != NULL ){ mri_free(flim); flim = tim; }
         }

         /* write the output file */

         if( kf == seq->saver_from )
            printf("writing %d x %d PNM files",nx,ny) ;
         else if( kf%10 == 5 )
            printf("." ) ;
         fflush(stdout) ;

         sprintf( fname , "%s%04d.pnm" , seq->saver_prefix , kf ) ;

         if( flim->kind == MRI_rgb ){                        /* 07 Mar 2001 */
            mri_write_pnm( fname , flim ) ; mri_free(flim) ; flim = NULL ;
         } else {                                            /* the old way */

            /* XColor arrays for underlay and overlay */

            ulc = ( seq->dc->use_xcol_im ) ? seq->dc->xcol_im
                                           : seq->dc->xgry_im ;
            ovc = seq->dc->ovc->xcol_ov ;

            fd = fopen( fname , "r" ) ;
            if( fd != NULL ){
               fclose(fd) ;
               fprintf(stderr,"(FAILED) attempt to overwrite file %s\n",fname) ;
               continue ;
            }
            fd = fopen( fname , "w" ) ;
            if( fd == NULL ){
               fprintf(stderr,"couldn't open output file %s\n",fname) ;
               continue ;
            }

            /* write the XColor intensities into the output */

            rgb = (byte *) XtMalloc( sizeof(byte) * 3 * npix ) ;
            bb  = 0 ;

            allgray = 1 ;  /* June 1995: check if all are gray */

            flar = mri_data_pointer(flim) ;  /* underlay image data */

            for( ii=0 ; ii < npix ; ii++ ){
               xc  = (flar[ii] >= 0) ? (ulc+flar[ii]) : (ovc-flar[ii]) ;
               rrr = rgb[bb++] = INTEN_TO_BYTE( xc->red ) ;
               ggg = rgb[bb++] = INTEN_TO_BYTE( xc->green ) ;
               bbb = rgb[bb++] = INTEN_TO_BYTE( xc->blue ) ;

               if( allgray ) allgray = ((rrr==ggg) && (ggg==bbb)) ;
            }

            /* if all are gray, compress to a PGM, else leave as a PPM */

            if( allgray ){
               bb = 3 ;
               for( ii=1 ; ii < npix ; ii++ ){ rgb[ii] = rgb[bb] ; bb += 3 ; }
               ncode = 5 ;     /* PGM */
               nout  = npix ;
            } else {
               ncode = 6 ;     /* PPM */
               nout  = 3*npix ;
            }

            fprintf(fd,"P%d\n%d %d\n255\n",ncode,nx,ny) ; /* write PNM header */
            fwrite( rgb , sizeof(byte) , nout , fd ) ;         /* write bytes */
            fclose( fd ); mri_free(flim); flim = NULL; myXtFree(rgb); /* DONE */
         }
      }
   } /* end of loop over images */

   printf(". **DONE**\n") ; fflush(stdout) ;

   /*--- go home ---*/

#ifndef DONT_USE_METER
   if( meter != NULL ) MCW_popdown_meter(meter) ;
#endif

   myXtFree( seq->saver_prefix ) ; seq->saver_prefix = NULL ;
   EXRETURN ;
}

/*----------------------------------------------------------------------*/
/*! Called from the 'Save' button; starts the save image dialog. */

void ISQ_but_save_CB( Widget w , XtPointer client_data ,
                                 XtPointer call_data    )
{
   MCW_imseq * seq = (MCW_imseq *) client_data ;

ENTRY("ISQ_but_save_CB") ;

   if( ! ISQ_REALZ(seq) || w == NULL || ! XtIsWidget(w) ) EXRETURN ;

   seq->saver_prefix = NULL ;
   seq->saver_from = seq->saver_to = -1 ;

   MCW_choose_string( w , "Filename prefix:" , NULL ,
                      ISQ_saver_CB , (XtPointer) seq ) ;

   ISQ_but_done_reset( seq ) ;
   EXRETURN ;
}

/*------------------------------------------------------------------------
   Set the "DONE" button back to be the "Done" button
--------------------------------------------------------------------------*/

#ifdef REQUIRE_TWO_DONES
void ISQ_but_done_reset( MCW_imseq * seq )
{
   if( ! ISQ_VALID(seq) || seq->done_first ) return ;

   MCW_set_widget_label( seq->wbut_bot[NBUT_DONE] , ISQ_but_done_label1 ) ;
   seq->done_first = True ;
   return ;
}
#endif

/*-----------------------------------------------------------------------
   Deletion of an imseq
-------------------------------------------------------------------------*/

void ISQ_but_done_CB( Widget w , XtPointer client_data ,
                                 XtPointer call_data    )
{
   MCW_imseq *seq = (MCW_imseq *)client_data ;

ENTRY("ISQ_but_done_CB") ;

   if( ! ISQ_VALID(seq) ) EXRETURN ;

#ifdef REQUIRE_TWO_DONES
   /*-- first call from "Done" button --> change label, return */

   if( w == seq->wbut_bot[NBUT_DONE] && seq->done_first ){
      MCW_set_widget_label( w , ISQ_but_done_label2 ) ;
      seq->done_first = False ;
      EXRETURN ;
   }
#endif

   /*-- second call: kill --*/

   if( seq->glstat->worker != 0 ){  /* remove work process, if started */
     XtRemoveWorkProc( seq->glstat->worker ) ;
     seq->glstat->worker = 0 ;
   }

   ISQ_timer_stop(seq) ;

   if( seq->dialog != NULL ){ /* 13 Aug 2002 */
     XtDestroyWidget( seq->dialog ) ; NI_sleep(1) ;
   }

   ISQ_free_alldata( seq ) ;
   XtDestroyWidget( seq->wtop ) ; NI_sleep(3) ;
   seq->valid = 0 ;     /* WE do not deallocate the data structure! */

   STATUS("IMSEQ: data destroyed!") ;

   if( seq->status->send_CB != NULL ){
      ISQ_cbs cbs ;

      STATUS("IMSEQ: sending destroy message") ;

      cbs.reason = isqCR_destroy ;
#if 0
      seq->status->send_CB( seq , seq->getaux , &cbs ) ;
#else
      SEND(seq,cbs) ;
#endif
   }

   EXRETURN ;
}

/*-----------------------------------------------------------------------
   delete malloc-ed data in an imseq
-------------------------------------------------------------------------*/

void ISQ_free_alldata( MCW_imseq *seq )
{
   int ib ;

ENTRY("ISQ_free_alldata") ;

   if( ! ISQ_VALID(seq) ) EXRETURN ;

   KILL_1MRI( seq->imim ) ;
   KILL_1MRI( seq->ovim ) ;
   KILL_1MRI( seq->orim ) ;  /* 30 Dec 1998 */

   KILL_2XIM( seq->given_xim  , seq->sized_xim  ) ;
   KILL_2XIM( seq->given_xbar , seq->sized_xbar ) ;

   myXtFree( seq->imstat ) ; seq->imstat = NULL ;
   myXtFree( seq->glstat ) ; seq->glstat = NULL ;

   for( ib=0 ; ib < seq->num_bbox ; ib++ )
      myXtFree( seq->bbox[ib] ) ;
   seq->num_bbox = 0 ;

   for( ib=0 ; ib < NARROW ; ib++ ) myXtFree( seq->arrow[ib] ) ;

   myXtFree( seq->arrowpad )           ;
   FREE_AV( seq->mont_across_av )     ;
   FREE_AV( seq->mont_down_av )       ;
   FREE_AV( seq->mont_skip_av )       ;
   FREE_AV( seq->mont_gap_av )        ;
   FREE_AV( seq->mont_gapcolor_av )   ;
   FREE_AV( seq->transform0D_av )     ; /* 30 Oct 1996 */
   FREE_AV( seq->transform2D_av )     ;
   FREE_AV( seq->rowgraph_av )        ; /* 30 Dec 1998 */
   FREE_AV( seq->surfgraph_av )       ; /* 21 Jan 1999 */
   myXtFree( seq->surfgraph_arrowpad );
   FREE_AV( seq->ov_opacity_av )      ; /* 07 Mar 2001 */
   FREE_AV( seq->wbar_label_av )      ; /* 20 Sep 2001 */
   myXtFree( seq->wbar_plots_bbox )   ;

   FREE_AV( seq->wbar_labsz_av )      ; /* 06 Jan 2005: oopsie */
   myXtFree( seq->pen_bbox ) ;          /* 06 Jan 2005: oopsie again */

   FREE_AV( seq->slice_proj_av )      ; /* 31 Jan 2002 */
   FREE_AV( seq->slice_proj_range_av );

   FREE_AV( seq->wbar_ticnum_av )     ; /* 23 Feb 2004 */
   FREE_AV( seq->wbar_ticsiz_av )     ; /* 23 Feb 2004 */

   FREE_AV( seq->zoom_val_av ) ;
   if( seq->zoom_pixmap != (Pixmap) 0 ){
     XFreePixmap( seq->dc->display , seq->zoom_pixmap ) ;
     seq->zoom_pixmap = (Pixmap) 0 ;
   }
   MCW_kill_XImage( seq->zoom_xim ) ; seq->zoom_xim = NULL ;

   if( seq->rowgraph_mtd != NULL ){                /* 30 Dec 1998 */
     seq->rowgraph_mtd->killfunc = NULL ;
     plotkill_topshell( seq->rowgraph_mtd ) ;
   }

   if( seq->surfgraph_mtd != NULL ){               /* 21 Jan 1999 */
     seq->surfgraph_mtd->killfunc = NULL ;
     plotkill_topshell( seq->surfgraph_mtd ) ;
   }

   if( seq->graymap_mtd != NULL ){                 /* 24 Oct 2003 */
     seq->graymap_mtd->killfunc = NULL ;
     plotkill_topshell( seq->graymap_mtd ) ;
   }

#if 0
   myXtFree(seq->status) ;                         /* 05 Feb 2000 */
#endif

   /* 24 Apr 2001: destroy any recorded images */

                   /* 05 Jan 2005: include the memplot recordings */
   if( seq->record_mplot != NULL && seq->record_imarr != NULL ){
     for( ib=0 ; ib < IMARR_COUNT(seq->record_imarr) ; ib++ )
       delete_memplot( seq->record_mplot[ib] ) ;
     free((void *)seq->record_mplot) ; seq->record_mplot = NULL ;
   }
   if( seq->record_imarr != NULL ) DESTROY_IMARR(seq->record_imarr) ;
   if( seq->record_imseq != NULL )
      drive_MCW_imseq( seq->record_imseq , isqDR_destroy , NULL ) ;

   myXtFree( seq->record_status_bbox ) ;
   myXtFree( seq->record_method_bbox ) ;

   if( seq->mplot != NULL ){                       /* 19 Sep 2001 */
      delete_memplot( seq->mplot ); seq->mplot = NULL;
   }

   EXRETURN ;
}

/*----------------------------------------------------------------------
  callback when the scale is moved
------------------------------------------------------------------------*/

void ISQ_scale_CB( Widget w , XtPointer client_data , XtPointer call_data )
{
   MCW_imseq * seq             = (MCW_imseq *)             client_data ;
   XmScaleCallbackStruct * cbs = (XmScaleCallbackStruct *) call_data ;

ENTRY("ISQ_scale_CB") ;

   if( ! ISQ_REALZ(seq) ) EXRETURN ;

   if( seq->status->num_total < 2 ){  /* 29 Jul 2002 */
      XmScaleSetValue( seq->wscale , 0 ) ;
      EXRETURN ;
   }

   ISQ_redisplay( seq , cbs->value , isqDR_display ) ;

   ISQ_but_done_reset( seq ) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------
  Redo the display for a particular image:
     n < 0 , type = isqDR_display ==> redisplay current image and overlay
             type = isqDR_overlay ==> redisplay current overlay only
             type = isqDR_reimage ==> redisplay current image only
             type = isqDR_reshow  ==> just reshow (same as ISQ_show_image)

     n >= 0, type = isqDR_display ==> redisplay image n and overlay n
             type = isqDR_overlay ==> if current image is n, just
                                        redisplay overlay n, otherwise both
             type = isqDR_reimage ==> if current image is n, just
                                        redisplay image n, otherwise both
-------------------------------------------------------------------------*/

/***
  Modified Mar 25 1996:
    If the image number scale is moved, then this routine is called,
    and then ISQ_set_image_number is called, which then calls the
    send_CB callback, which may end up calling this routine again
    (via drive_MCW_imseq -- for example, see AFNI_seq_send_CB).
    This will result in redisplaying the desired image twice, with
    the resulting speed penalty.  To prevent this, ISQ_redisplay
    now checks if the call is recursive.  If it is recursive, and
    it is being called with the same seq and n parameters as before,
    the routine exits.
***/

#define RECUR (recur_flg && seq == recur_seq && n == recur_n)

void ISQ_redisplay( MCW_imseq *seq , int n , int type )
{
   Boolean kill_im , kill_ov ;
   int nrold ;
   static int        recur_flg = FALSE ;
   static int        recur_n   = -1 ;
   static MCW_imseq *recur_seq = NULL ;

   if( seq == NULL || seq->ignore_redraws ) return ;  /* 16 Aug 2002 */
ENTRY("ISQ_redisplay") ;

   if( ! ISQ_VALID(seq) ) EXRETURN ;

   /** check for identical recursive call **/

   if( RECUR ){
     DPRI("ABORTED FOR RECURSION at n =",n) ;
     recur_flg = FALSE ; EXRETURN ;
   }

   /** If no recursion is now occurring, mark for possible recursion later.
       This assumes that each level of recursion does not spawn new levels
       yet again via the send_CB callback.  If this were possible, the
       code for recursion prevention would need to be more complicated! **/

   if( ! recur_flg ){ recur_flg = TRUE ; recur_n = n ; recur_seq = seq ; }

   /** find the image that is being seen right now **/

   nrold = seq->im_nr ;
   seq->im_label[0] = '\0' ;  /* forces redraw of text */

   /** set the image number to be displayed now **/

   if( n >= 0 && !ISQ_set_image_number(seq,n) ){
      if( RECUR ) recur_flg = FALSE ; EXRETURN ;
   }

   MCW_discard_events_all( seq->wimage , ButtonPressMask ) ;  /* 20 Mar 2007 */

   switch( type ){
      default: { if( RECUR ) recur_flg = FALSE ; EXRETURN ; }

      case isqDR_display:
         kill_im = kill_ov = True ;            /* do both images */
      break ;

      case isqDR_overlay:
         kill_im = (n >=0 ) && (n != nrold) ;  /* only do im if new */
         kill_ov = True ;                      /* do overlay */
      break ;

      case isqDR_reimage:
         kill_ov = (n >=0 ) && (n != nrold) ;
         kill_im = True ;
      break ;

      case isqDR_reshow:
         kill_ov = kill_im = (n >=0 ) && (n != nrold) ; /* only if new */
      break ;
   }

   if( kill_im ) KILL_1MRI( seq->imim ) ;
   if( kill_ov ) KILL_1MRI( seq->ovim ) ;

   if( kill_ov || kill_im ) KILL_2XIM( seq->given_xim , seq->sized_xim  ) ;

   if( kill_ov || kill_im ){
      MCW_kill_XImage( seq->zoom_xim ) ; seq->zoom_xim = NULL ;
   }

   seq->scl_label[0] = '\0' ;
   ISQ_show_image( seq ) ;
   ISQ_rowgraph_draw( seq ) ;
   ISQ_surfgraph_draw( seq ) ;  /* 21 Jan 1999 */

   if( seq->graymap_mtd != NULL ) ISQ_graymap_draw( seq ) ; /* 24 Oct 2003 */

   /* 24 Apr 2001: handle image recording */

   if( RECORD_ISON(seq->record_status) && seq->zoom_fac == 1 ){
      int pos , meth ;

      /* compute where to put this sucker */

      switch( seq->record_method ){
         default:
         case RECORD_METHOD_AFTEREND:     pos = 987654321; meth =  1; break;
         case RECORD_METHOD_BEFORESTART:  pos =  0       ; meth = -1; break;
         case RECORD_METHOD_INSERT_MM:    pos = -1       ; meth = -1; break;
         case RECORD_METHOD_INSERT_PP:    pos = -1       ; meth =  1; break;
         case RECORD_METHOD_OVERWRITE:    pos = -1       ; meth =  0; break;
         case RECORD_METHOD_OVERWRITE_MM: pos = -2       ; meth =  0; break;
         case RECORD_METHOD_OVERWRITE_PP: pos = -3       ; meth =  0; break;
      }

      /* put it there */

      ISQ_record_addim( seq , pos , meth ) ;

      /* if recording just one, switch status off */

      if( seq->record_status == RECORD_STATUS_NEXTONE ){
         seq->record_status = RECORD_STATUS_OFF ;
         MCW_set_bbox( seq->record_status_bbox , RECORD_STATUS_OFF ) ;
         MCW_invert_widget( seq->record_cbut ) ;
      }
   }

   /* exit stage left */

   if( RECUR ) recur_flg = FALSE ;
   EXRETURN ;
}

/*------------------------------------------------------------------------
   set image number in an imseq;
   return value is 0 if this can't be done, 1 if things go OK
--------------------------------------------------------------------------*/

int ISQ_set_image_number( MCW_imseq * seq , int n )
{
ENTRY("ISQ_set_image_number") ;

   if( ! ISQ_VALID(seq) ) RETURN(0) ;

   if( n < 0 || n >= seq->status->num_total ){

     if( seq->status->num_total > 1 ){
       XBell( seq->dc->display , 100 ) ;
       fprintf(stderr,"\n*** ILLEGAL IMAGING:\n"
                      " ISQ_set_image_number %d\n",n);

       fprintf(stderr," status: num_total=%d num_series=%d\n",
               seq->status->num_total , seq->status->num_series ) ;
     } else {
       XmScaleSetValue( seq->wscale , 0 ) ;  /* 08 Aug 2001 */
     }

     RETURN(0) ;
   }

   if( seq->im_nr != n ){
     XmScaleSetValue( seq->wscale , n ) ;  /* be sure to change scale */

     if( seq->status->send_CB != NULL ){
       ISQ_cbs cbs ;
       seq->im_nr = n ;
       cbs.reason = isqCR_newimage ;
       cbs.nim    = seq->im_nr ;
#if 0
       seq->status->send_CB( seq , seq->getaux , &cbs ) ;
#else
       SEND(seq,cbs) ;
#endif
     } else {
#if 0
       ISQ_redisplay( seq , n , isqDR_display ) ;  /* 07 Nov 2002 */
#endif
     }
   }
   RETURN(1) ;
}

/*-------------------------------------------------------------------------*/

/* 15 Mar 2002: stuff for processing X11 errors */

static volatile int xwasbad ;
typedef int (*xhandler)(Display *, XErrorEvent *) ;
static int qhandler( Display *dpy , XErrorEvent *xev ){ xwasbad=1; return 0; }

/*-----------------------------------------------------------------------*/

int ISQ_show_zoom( MCW_imseq *seq )   /* 11 Mar 2002 */
{
   int iw,ih , zlev=seq->zoom_fac , pw,ph , xoff,yoff , newim=0 , flash=0 ;
   static int busy=0 ;                /* 23 Jan 2004 */

ENTRY("ISQ_show_zoom") ;

   if( busy ){ STATUS(" recursive entry!"); RETURN(-1); }          /* recursion = bad */
   busy = 1 ;

   /* find the size of the image window */

   MCW_widget_geom( seq->wimage, &iw,&ih , NULL,NULL ) ;

   /* pixmap should be size of image window, scaled up;
      if it isn't that size already, free it right now */

   pw = iw*zlev ; ph = ih*zlev ;

   if( seq->zoom_pixmap != (Pixmap) 0 &&
       (pw != seq->zoom_pw || ph != seq->zoom_ph) ){

STATUS("freeing old pixmap") ;
      XFreePixmap( seq->dc->display , seq->zoom_pixmap ) ;
      seq->zoom_pixmap = (Pixmap) 0 ;
      newim++ ;
   }

   /* (re)make the pixmap, if needed;
      it will be saved in the seq struct for next time */

   if( seq->zoom_pixmap == (Pixmap) 0 ){
      xhandler old_handler = XSetErrorHandler(qhandler); xwasbad = 0;

STATUS("creating new pixmap") ;
      seq->zoom_pixmap = XCreatePixmap( seq->dc->display ,
                                        XtWindow(seq->wimage) ,
                                        pw , ph , seq->dc->depth ) ;

      (void) XSetErrorHandler(old_handler) ;

      /* if allocating pixmap failed, exit now */

      if( xwasbad ){
        fprintf(stderr,"** Can't zoom - out of memory! **\n\a");
        AV_assign_ival( seq->zoom_val_av , 1 ) ;
        ISQ_zoom_av_CB( seq->zoom_val_av , seq ) ;
        busy = 0 ; RETURN(-1) ;
      }

      seq->zoom_pw = pw ; seq->zoom_ph = ph ;
      newim++ ;
   }

   /* if we made a new pixmap, we'll need a new zoomed image for it */

   if( newim && seq->zoom_xim != NULL ){
STATUS("killing old XImage because have new image") ;
     MCW_kill_XImage( seq->zoom_xim ) ; seq->zoom_xim = NULL ;
   }

   /* scale up the given_xim, if needed;
      it will be save in the seq struct for next time,
      unless the image changes, in which case it will have been axed */


   if( seq->zoom_xim == NULL ){
     MRI_IMAGE *im , *tim ;
STATUS("inverting zoom label") ;
     flash = 1 ; MCW_invert_widget( seq->zoom_val_av->wlabel ) ;
STATUS("converting given XImage to MRI_IMAGE") ;
     im  = XImage_to_mri( seq->dc, seq->given_xim, X2M_USE_CMAP|X2M_FORCE_RGB ) ;
STATUS("zooming up MRI_IMAGE") ;
     tim = mri_dup2D(zlev,im) ; mri_free(im) ;
STATUS("converting zoomed MRI_IMAGE back to XImage") ;
     seq->zoom_xim = mri_to_XImage(seq->dc,tim) ; mri_free(tim) ;
     newim++ ;
   }

   /* if zoomed image isn't same size as pixmap, resize it here */

   if( pw != seq->zoom_xim->width || ph != seq->zoom_xim->height ){
     XImage *sxim ;
     sxim = resize_XImage( seq->dc , seq->zoom_xim , pw , ph ) ;
STATUS("killing old XImage because doesn't fit pixmap") ;
     MCW_kill_XImage( seq->zoom_xim ) ;
     seq->zoom_xim = sxim ;
     newim++ ;
   }

   /* if have a new image, put the zoomed XImage into the Pixmap */

   if( newim ){
STATUS("putting new image into pixmap") ;
     XPutImage( seq->dc->display ,
                seq->zoom_pixmap ,
                seq->dc->origGC  , seq->zoom_xim , 0,0,0,0 , pw,ph ) ;

     /* draw the overlay graph into the Pixmap */

     if( !seq->opt.no_overlay && seq->mplot != NULL ){
STATUS("drawing overlay plot into pixmap") ;
        memplot_to_X11_sef( seq->dc->display ,
                            seq->zoom_pixmap , seq->mplot ,
                            0,0,MEMPLOT_FREE_ASPECT        ) ;
     }
   }

   /* now we can copy the relevant area
      from the pixmap into the image window */

   xoff = seq->zoom_hor_off * pw ; if( xoff+iw > pw ) xoff = pw-iw ;
   yoff = seq->zoom_ver_off * ph ; if( yoff+ih > ph ) yoff = ph-ih ;

STATUS("copying from pixmap to image window") ;
   XCopyArea( seq->dc->display ,
              seq->zoom_pixmap ,
              XtWindow(seq->wimage) , seq->dc->origGC ,
              xoff , yoff , iw,ih , 0,0 ) ;

   if( flash ) MCW_invert_widget( seq->zoom_val_av->wlabel ) ;

#ifdef DISCARD_EXCESS_EXPOSES
STATUS("discarding excess Expose events") ;
    MCW_discard_events( seq->wimage , ExposureMask ) ;
#endif

   busy = 0 ; RETURN(1) ;
}

/*-----------------------------------------------------------------------
  actually put the image into window
  23 Apr 2001 - modified to deal with case of NULL image from
                ISQ_make_image() - by drawing a string
-------------------------------------------------------------------------*/

void ISQ_show_image( MCW_imseq *seq )
{
   if( seq == NULL || seq->ignore_redraws ) return ;  /* 16 Aug 2002 */
ENTRY("ISQ_show_image") ;

   if( ! ISQ_REALZ(seq) ) EXRETURN ;

   if( seq->given_xbar == NULL ) ISQ_show_bar( seq ) ;  /* 22 Aug 1998 */

   if( seq->given_xim == NULL ) ISQ_make_image( seq ) ;

   if( seq->given_xim == NULL ) STATUS("bad news: given_xim == NULL!") ;

   if( ! MCW_widget_visible(seq->wimage) ) EXRETURN ;  /* 03 Jan 1999 */

   if( seq->given_xim != NULL &&
       seq->zoom_fac  >  1    &&
       seq->mont_nx   == 1    &&
       seq->mont_ny   == 1      ){    /* show a zoomed image instead */

      int ss = ISQ_show_zoom( seq ) ;
      if( ss > 0 ) EXRETURN ;         /* if it failed, fall through */
   }

   if( seq->given_xim != NULL && seq->sized_xim == NULL ){
      int nx , ny ;

      STATUS("making sized_xim");

      MCW_widget_geom( seq->wimage , &nx , &ny , NULL,NULL ) ;

      seq->sized_xim = resize_XImage( seq->dc , seq->given_xim , nx , ny ) ;
   }

   if( seq->sized_xim != NULL ){
STATUS("putting sized_xim to screen");

#if 0
if( AFNI_yesenv("AFNI_IMSEQ_DEBUG") ){
  fprintf(stderr,"==== imseq->wimage: XPutImage w=%d h=%d\n",
  seq->sized_xim->width , seq->sized_xim->height ) ;
  DBG_traceback() ;
}
#endif

     XPutImage( seq->dc->display , XtWindow(seq->wimage) , seq->dc->origGC ,
                seq->sized_xim , 0,0,0,0,
                seq->sized_xim->width , seq->sized_xim->height ) ;

   } else {  /* 23 Apr 2001 - draw 'EMPTY IMAGE' */

      static MEM_plotdata *empt=NULL ;  /* only create once */

      if( empt == NULL ){
         STATUS("create EMPTY IMAGE plot") ;
         create_memplot_surely("EmptyImagePlot",1.0) ;
         empt = get_active_memplot() ;
         set_color_memplot(1.0,1.0,1.0) ;
         set_thick_memplot(0.009) ;
         plotpak_pwritf( 0.4,0.83 , "EMPTY" , 96 , 0 , 0 ) ;
         plotpak_pwritf( 0.4,0.67 , "IMAGE" , 96 , 0 , 0 ) ;
         set_color_memplot(0.0,0.0,0.0) ;
         plotpak_pwritf( 0.6,0.33 , "EMPTY" , 96 , 0 , 0 ) ;
         plotpak_pwritf( 0.6,0.17 , "IMAGE" , 96 , 0 , 0 ) ;
         set_color_memplot(1.0,1.0,0.0) ;
         set_thick_memplot(0.019) ;
         plotpak_line( 0.01,0.01 , 0.99,0.01 ) ;
         plotpak_line( 0.99,0.01 , 0.99,0.99 ) ;
         plotpak_line( 0.99,0.99 , 0.01,0.99 ) ;
         plotpak_line( 0.01,0.99 , 0.01,0.01 ) ;
         set_thick_memplot(0.0) ;
      }
      STATUS("display EMPTY IMAGE plot") ;
      XClearWindow( seq->dc->display , XtWindow(seq->wimage) ) ;
      memplot_to_X11_sef( seq->dc->display ,
                          XtWindow(seq->wimage) , empt ,
                          0,0,MEMPLOT_FREE_ASPECT     ) ;
   }

   /*-- 26 Feb 2001: draw some line overlay, a la coxplot? --*/
   /*-- 19 Sep 2001: modified to use memplot stored in seq --*/

   if( !seq->opt.no_overlay && seq->mplot != NULL )
      memplot_to_X11_sef( seq->dc->display ,
                          XtWindow(seq->wimage) , seq->mplot ,
                          0,0,MEMPLOT_FREE_ASPECT             ) ;

   seq->never_drawn = 0 ;

   ISQ_draw_winfo( seq ) ;

#ifdef DISCARD_EXCESS_EXPOSES
    MCW_discard_events( seq->wimage , ExposureMask ) ;
#endif

   EXRETURN ;
}

/*-------------------------------------------------------------------
  Draw the message data in the winfo label
---------------------------------------------------------------------*/

void ISQ_draw_winfo( MCW_imseq *seq )
{
   char buf[128] = "\0" ;
   int nn , ibuf ;
   ISQ_indiv_statistics *st ;

ENTRY("ISQ_draw_winfo") ;

   if( ! ISQ_REALZ(seq) ) EXRETURN ;

   if( seq->last_image_type >= 0 ){
     sprintf( buf , "%s" , MRI_TYPE_name[seq->last_image_type] ) ;

     if( seq->last_image_type == MRI_complex ){
       switch( seq->opt.cx_code ){
         case ISQ_CX_MAG:   strcat( buf , "[mag]" ) ; break ;
         case ISQ_CX_PHASE: strcat( buf , "[arg]" ) ; break ;
         case ISQ_CX_REAL:  strcat( buf , "[re]"  ) ; break ;
         case ISQ_CX_IMAG:  strcat( buf , "[im]"  ) ; break ;
       }
     }
   }
   ibuf = strlen(buf) ;

   nn = seq->im_nr ;  if( nn < 0 ) EXRETURN ;

   st = &( seq->imstat[nn] ) ;
#if 0
   if( st->one_done ){
#if 0
      if( seq->opt.scale_group == ISQ_SCL_AUTO   &&
          seq->opt.scale_range == ISQ_RNG_02TO98    )

           sprintf( buf+ibuf , " 2%%=%g 98%%=%g", st->per02 , st->per98 ) ;
      else
#endif

#if 0
           sprintf( buf+ibuf , "=%g..%g ent=%.2f" ,
                    st->min , st->max , st->entropy ) ;
#endif
           sprintf( buf+ibuf , "=%g..%g" , st->min , st->max ) ;
   }
#endif

   if( seq->scl_label[0] != '\0' )
     sprintf(buf+strlen(buf)," %s",seq->scl_label) ;
   if( (seq->opt.improc_code & ISQ_IMPROC_SHARP) )
     sprintf(buf+strlen(buf)," s=%d",(int)(10.0*seq->sharp_fac+.01)) ;
   if( seq->render_mode ) strcat(buf,"#") ;

   if( seq->im_label[0] == '\0' || strcmp(buf,seq->im_label) != 0 ){
     char qbuf[128] ; qbuf[0] = '\0' ;
     if( seq->winfo_prefix[0] != '\0' ){  /* 10 Dec 2007 */
       strcat(qbuf,seq->winfo_prefix) ; strcat(qbuf,": ") ;
     }
     if( seq->winfo_extra[0] == '\0' ){

       int iw=0 ;                                   /* winfo_sides stuff */
       switch( seq->opt.rot ){                      /* from 01 Dec 1999  */
         case ISQ_ROT_0  : iw=0 ; break ;
         case ISQ_ROT_90 : iw=1 ; break ;
         case ISQ_ROT_180: iw=2 ; break ;
         case ISQ_ROT_270: iw=3 ; break ;
       }
       if( seq->opt.mirror ) iw = (iw+2)%4 ;

       if( seq->winfo_sides[iw][0] != '\0' ){
         strcat(qbuf,"left=") ;
         strcat(qbuf,seq->winfo_sides[iw]) ;
         strcat(qbuf," ") ; strcat(qbuf,buf) ;
         MCW_set_widget_label( seq->winfo , qbuf ) ;
       } else if( seq->opt.mirror || seq->opt.rot != ISQ_ROT_0 ){
         switch( seq->opt.rot ){
           case ISQ_ROT_0  : strcat(qbuf,"["   ) ; break ;
           case ISQ_ROT_90 : strcat(qbuf,"[90" ) ; break ;
           case ISQ_ROT_180: strcat(qbuf,"[180") ; break ;
           case ISQ_ROT_270: strcat(qbuf,"[270") ; break ;
         }
         if( seq->opt.mirror ){
           if( seq->opt.rot == ISQ_ROT_0 ) strcat(qbuf,"l] " ) ;
           else                            strcat(qbuf,"+l] ") ;
         } else                            strcat(qbuf,"] "  ) ;
         strcat(qbuf,buf) ;
         MCW_set_widget_label( seq->winfo , qbuf ) ;
       } else {
         MCW_set_widget_label( seq->winfo , buf ) ;   /* default label! */
       }

     } else {                                        /* winfo_extra stuff */
       strcpy(qbuf,seq->winfo_extra) ;              /* from 07 Aug 1999  */
       strcat(qbuf," ") ; strcat(qbuf,buf) ;
       MCW_set_widget_label( seq->winfo , qbuf ) ;
     }
     strcpy(seq->im_label,buf) ;
   }

   EXRETURN ;
}

/*-----------------------------------------------------------------------
  Put a range hint on the color bar, if possible -- 29 Jul 2001
-------------------------------------------------------------------------*/

void ISQ_set_barhint( MCW_imseq *seq , char *lab )
{
   char sbot[16],stop[16] , hint[64] , *sb,*st ;

ENTRY("ISQ_set_barhint") ;

   if( !ISQ_REALZ(seq) ) EXRETURN ;            /* bad news */

   if( seq->barbot < seq->bartop ){            /* can make a hint */
      AV_fval_to_char( seq->barbot , sbot ) ;  /* convert to nice strings */
      AV_fval_to_char( seq->bartop , stop ) ;
      sb = (sbot[0] == ' ') ? sbot+1 : sbot ;  /* skip leading blanks */
      st = (stop[0] == ' ') ? stop+1 : stop ;
      if( lab != NULL && strlen(lab) < 32 )    /* create hint */
         sprintf(hint,"%s: %s .. %s",lab,sb,st) ;
      else
         sprintf(hint,"%s .. %s",sb,st) ;
      MCW_register_hint( seq->wbar , hint ) ;  /* send to hint system */
   } else {
      MCW_unregister_hint( seq->wbar ) ;       /* don't have a hint */
   }

   EXRETURN ;
}

/*-------------------------------------------------------------------*/

void ISQ_set_cursor_state( MCW_imseq *seq , int cstat )  /* 10 Mar 2003 */
{
   if( seq->zoom_button1 || seq->record_mode ){
     XBell(seq->dc->display,100); return;
   }

#if 0
fprintf(stderr,"ISQ_set_cursor_state: old=%d new=%d\n",seq->cursor_state,cstat);
#endif

   switch( cstat ){
     default:
       POPUP_cursorize( seq->wimage ) ;
       seq->cursor_state = CURSOR_NORMAL ;
       MCW_set_bbox( seq->pen_bbox , 0 ) ;
     break ;

     case CURSOR_PENCIL:
       PENCIL_cursorize( seq->wimage ) ;
       seq->cursor_state = CURSOR_PENCIL ;
       MCW_set_bbox( seq->pen_bbox , 1 ) ;
     break ;

     case CURSOR_CROSSHAIR:
       CROSSHAIR_cursorize( seq->wimage ) ;
       seq->cursor_state = CURSOR_CROSSHAIR ;
       MCW_set_bbox( seq->pen_bbox , 0 ) ;
     break ;
   }
   return ;
}

/*-------------------------------------------------------------------
  actually put the color bar into its window
---------------------------------------------------------------------*/

void ISQ_show_bar( MCW_imseq * seq )
{
   if( seq == NULL || seq->ignore_redraws ) return ;  /* 16 Aug 2002 */
ENTRY("ISQ_show_bar") ;

   if( ! ISQ_REALZ(seq) ) EXRETURN ;

   if( ! MCW_widget_visible(seq->wbar) ) EXRETURN ;  /* 03 Jan 1999 */

   if( seq->given_xbar == NULL ) ISQ_make_bar( seq ) ;

   if( seq->sized_xbar == NULL ){
      int nx , ny ;
STATUS("making sized_xbar");

      MCW_widget_geom( seq->wbar , &nx , &ny , NULL,NULL ) ;

      seq->sized_xbar = resize_XImage( seq->dc, seq->given_xbar, nx, ny ) ;
   }


   if( seq->sized_xbar != NULL ){
STATUS("putting sized_xbar to screen");

     XPutImage( seq->dc->display , XtWindow(seq->wbar) , seq->dc->origGC ,
                seq->sized_xbar , 0,0,0,0,
                seq->sized_xbar->width , seq->sized_xbar->height ) ;
   }

#ifdef DISCARD_EXCESS_EXPOSES
    MCW_discard_events( seq->wbar , ExposureMask ) ;
#endif

   EXRETURN ;
}

/*-----------------------------------------------------------------------
   Handle all events in an imseq drawing area widget (image or bar).
   Feb 1998: Button2 events are passed to their own handler.
-------------------------------------------------------------------------*/

void ISQ_drawing_EV( Widget w , XtPointer client_data ,
                     XEvent * ev , Boolean * continue_to_dispatch )
{
   MCW_imseq * seq = (MCW_imseq *) client_data ;
   static ISQ_cbs cbs ;
   static int busy=0 ;   /* 23 Jan 2004: prevent recursion */

ENTRY("ISQ_drawing_EV") ;

   if( busy ){ STATUS("recursive entry!"); EXRETURN; }  /* bad! */
   if( !ISQ_REALZ(seq) ) EXRETURN ;
   busy = 1 ;

   if(PRINT_TRACING){
     char str[256], *wn ;
          if( w == seq->wimage ) wn = "wimage" ;
     else if ( w == seq->wbar  ) wn = "wbar"   ;
     else                        wn = XtName(w) ;
     sprintf(str,"Widget=%s Event type=%d",wn,ev->type);
     STATUS(str) ;
   }

   switch( ev->type ){

      /*----- button release event -----*/

      case ButtonRelease:{
         XButtonEvent * event = (XButtonEvent *) ev ;
         int but = event->button ;

         /** 03 Oct 2002: change Shift+Button1 into Button2, then send to that event handler **/

         if( but == Button1 &&
             ( seq->cursor_state == CURSOR_PENCIL ||
               ((event->state & ShiftMask) && !(event->state & ControlMask)) ) ){
           event->button = but = Button2 ;
           if( seq->button2_enabled && w == seq->wimage )
              ISQ_button2_EV( w , client_data , ev , continue_to_dispatch ) ;
           else
              { XBell(seq->dc->display,100); busy=0;EXRETURN; }
         }

         /* Button1 release: turn off zoom-pan mode, if it was on */

         if( event->button == Button1 && w == seq->wimage ){

           if( seq->zoom_button1 && !AFNI_yesenv("AFNI_KEEP_PANNING") ){
             seq->zoom_button1 = 0 ;
             POPUP_cursorize( seq->wimage ) ;
             MCW_invert_widget( seq->zoom_drag_pb ) ;
           } else if( !seq->zoom_button1 ){           /* 23 Oct 2003 */
             if( seq->cmap_changed ){
               COLORMAP_CHANGE(seq); seq->cmap_changed = 0;
               if( seq->graymap_mtd != NULL && AFNI_yesenv("AFNI_STROKE_AUTOPLOT") ){
                 NI_sleep(456) ;     /* pop down after a short delay */
                 plotkill_topshell( seq->graymap_mtd ) ;
                 seq->graymap_mtd = NULL ;
               }
             } else if( seq->status->send_CB != NULL ){  /* 04 Nov 2003 */
                int imx,imy,nim;
                seq->wimage_width = -1 ;
                ISQ_mapxy( seq , seq->last_bx,seq->last_by , &imx,&imy,&nim ) ;
                cbs.reason = isqCR_buttonpress ;
                cbs.event  = ev ;
                cbs.xim    = imx ;       /* delayed send of Button1 */
                cbs.yim    = imy ;       /* event to AFNI now       */
                cbs.nim    = nim ;
#if 0
                seq->status->send_CB( seq , seq->getaux , &cbs ) ;
#else
                SEND(seq,cbs) ;
#endif
             }
           }
         }
      }
      break ;

      /*----- motion with Button #1 pressed down -----*/

      case MotionNotify:{
        XMotionEvent * event = (XMotionEvent *) ev ;
        int bx,by ;

        /** 03 Oct 2002: change Shift+Button1 into Button2, then send to that event handler **/

        if( (event->state & Button1Mask) &&
             ( seq->cursor_state == CURSOR_PENCIL ||
               ((event->state & ShiftMask) && !(event->state & ControlMask)) ) ){
          event->state |= Button2Mask ;
          if( seq->button2_enabled && w == seq->wimage )
             ISQ_button2_EV( w , client_data , ev , continue_to_dispatch ) ;
          else
             { XBell(seq->dc->display,100); busy=0;EXRETURN; }
          busy=0;EXRETURN ;
        }

        /* Button1 motion: if not panning, changing the color/gray map? */

        if( !seq->zoom_button1 && (event->state & Button1Mask) ){
          int xdif = (event->x - seq->last_bx) ;
          int ydif = (event->y - seq->last_by) ;
          if( !seq->dc->use_xcol_im && (xdif || ydif) ){
            double denom = AFNI_numenv("AFNI_STROKE_THRESHOLD") ;
            if( denom < 1.0l ){
              if( getenv("AFNI_STROKE_THRESHOLD") != NULL ){ busy=0;EXRETURN ;}
              denom = 32.0l ;
            }
            xdif = rint(xdif/denom) ; ydif = rint(ydif/denom) ;
            if( xdif || ydif ){                             /* if big enough change */
              if( seq->imim != NULL && seq->imim->kind == MRI_rgb ){ /* 26 Apr 2005 */

                     if( xdif > 0 ) seq->rgb_gamma  *= 0.95 ;  /* change the RGB */
                else if( xdif < 0 ) seq->rgb_gamma  /= 0.95 ;      /* colorizing */
                     if( ydif < 0 ) seq->rgb_offset += 0.014;
                else if( ydif > 0 ) seq->rgb_offset -= 0.014;
                ISQ_redisplay( seq , -1 , isqDR_reimage ) ;
                seq->cmap_changed = 1 ;
                seq->last_bx=event->x ; seq->last_by=event->y;

              } else {                          /* the old way: change the gray map */

                if( xdif ){ DC_gray_conbrio(seq->dc, xdif); seq->last_bx=event->x;}
                if( ydif ){ DC_gray_change (seq->dc,-ydif); seq->last_by=event->y;}
                seq->cmap_changed = 1 ;
                if( seq->dc->visual_class == TrueColor ){
                  if( seq->graymap_mtd == NULL &&
                      AFNI_yesenv("AFNI_STROKE_AUTOPLOT") ) ISQ_graymap_draw( seq ) ;
                  KILL_2XIM( seq->given_xbar , seq->sized_xbar ) ;
                  ISQ_redisplay( seq , -1 , isqDR_display ) ;
                } else {
                  if( seq->graymap_mtd != NULL ) ISQ_graymap_draw( seq ) ;
                }
              }
            }
          }
          busy=0; EXRETURN ;
        }  /* end of altering colormap */

        /* Button1 motion: check for being in zoom-pan mode */

        if( !seq->zoom_button1              ||
            seq->zoom_fac == 1              ||
            seq->zoom_xim == NULL           ||
            (event->state & Button1Mask)==0   ){ busy=0; EXRETURN; } /* not zoom-pan? */

        /*-- if here, change panning offset --*/

        bx = event->x ; by = event->y ;
        ISQ_actually_pan( seq , (bx>seq->zoom_xp) ? -1
                               :(bx<seq->zoom_xp) ?  1 : 0 ,
                                (by>seq->zoom_yp) ? -1
                               :(by<seq->zoom_yp) ?  1 : 0   ) ;

        seq->zoom_xp = bx ; seq->zoom_yp = by ;

        busy=0; EXRETURN ;
      }
      break ;

      /*----- redraw -----*/

      case Expose:{
         XExposeEvent * event = (XExposeEvent *) ev ;

DPRI(" .. Expose; count=",event->count) ;

         XSync( XtDisplay(w) , False ) ;
         if( event->count == 0 ){      /* don't bother if more Expose to come */
            if( w == seq->wimage ){    /* 25 Sep 2000: check for hidden resizes */
               int nx,ny ;
               MCW_widget_geom( seq->wimage , &nx , &ny , NULL,NULL ) ;

               if( seq->sized_xim != NULL &&
                   ( (nx != seq->sized_xim->width ) ||
                     (ny != seq->sized_xim->height)   ) ){  /* found a hidden resize */
                                                            /* so let's un-hide it! */
                  XConfigureEvent nev ;

STATUS(" .. really a hidden resize") ;

                  nev.type = ConfigureNotify ; nev.width = nx ; nev.height = ny ;
                  ISQ_drawing_EV( w, client_data, (XEvent *) &nev, continue_to_dispatch ) ;

               } else
                  ISQ_show_image( seq ) ;
            }
            else if( w == seq->wbar )
               ISQ_show_bar( seq ) ;

         }
      }
      break ;

      /*----- take key press -----*/

      case KeyPress:{
         XKeyEvent *event = (XKeyEvent *) ev ;
         char       buf[32] ;
         int        nbuf ;
         KeySym     ks ;

STATUS(" .. KeyPress") ;

         ISQ_timer_stop(seq) ;  /* 03 Dec 2003 */

         /* discard if a mouse button is also pressed at this time */

         if( event->state & (Button1Mask|Button2Mask|Button3Mask) ){
           XBell(seq->dc->display,100); busy=0; EXRETURN;
         }

         /* get the string corresponding to the key pressed */

         buf[0] = '\0' ;
         ks     = 0 ;
         nbuf = XLookupString( event , buf , 32 , &ks , NULL ) ;
#if 0
fprintf(stderr,"KeySym=%04x nbuf=%d\n",(unsigned int)ks,nbuf) ;
#endif

         /* 24 Jan 2003: deal with special function keys */

         if( nbuf == 0 || ks > 255 ){
           if( seq->record_mode ){ busy=0; EXRETURN ; }
           nbuf = ISQ_handle_keypress( seq , (unsigned long)ks ) ;
           busy=0; EXRETURN ;
         }

         nbuf = ISQ_handle_keypress( seq , (unsigned long)buf[0] ) ;
         if( nbuf ){ busy=0; EXRETURN; }

         /* in special modes (record, Button2, zoom-pan) mode, this is bad */

         if( seq->record_mode || seq->button2_active || seq->zoom_button1 ){
           XBell(seq->dc->display,100); busy=0; EXRETURN;
         }

         /* otherwise, notify the master, if we have one */

         if( w == seq->wimage && seq->status->send_CB != NULL ){
           cbs.reason = isqCR_keypress ;
           cbs.event  = ev ;
           cbs.key    = buf[0] ;
           cbs.nim    = seq->im_nr ;
#if 0
           seq->status->send_CB( seq , seq->getaux , &cbs ) ;
#else
           SEND(seq,cbs) ;
#endif
         }
      }
      break ;  /* end of KeyPress */

      /*----- take button press -----*/

      case ButtonPress:{
         XButtonEvent *event = (XButtonEvent *) ev ;
         int bx,by , width,height , but ;

STATUS(" .. ButtonPress") ;

         /* don't allow button presses in a recorder window, or in zoom-pan mode */

         if( seq->record_mode || seq->zoom_button1 ){
           if( seq->record_mode || event->button != Button1 ) XBell(seq->dc->display,100);
           busy=0; EXRETURN;
         }

         but = event->button ;

         /* button press in the wbar => popup menu */

         if( w == seq->wbar ){          /* moved here 18 Oct 2001 */
           if( but == Button1 ){ /* 21 Oct 2003 */
             bx = seq->opt.free_aspect ; seq->opt.free_aspect = 0 ;
             ISQ_reset_dimen( seq, seq->last_width_mm, seq->last_height_mm ) ;
             seq->opt.free_aspect = bx ;
           } else if( but == Button3 ){
             XmMenuPosition( seq->wbar_menu , event ) ; /* where */
             XtManageChild ( seq->wbar_menu ) ;         /* popup */
           }
           else if( but == Button4 || but == Button5 ){ /* Scroll Wheel */
              int ddd = (but==Button4) ? -1 : 1 ;
              if( (event->state & (Mod1Mask|Mod2Mask)) )
                DC_palette_bright(  seq->dc , ddd ) ;   /* brightness */
              else
                DC_palette_squeeze( seq->dc , ddd ) ;   /* contrast */
              COLORMAP_CHANGE(seq) ;
           }
           else {
#if 0
             XUngrabPointer( event->display , CurrentTime ) ;
#else
             XBell(seq->dc->display,100) ;
#endif
           }
           MCW_discard_events( w , ButtonPressMask ) ;
           busy=0; EXRETURN ;
         }

         /* below here, button press was in the image */

         seq->last_bx = bx = event->x ;  /* 23 Oct 2003: save last button */
         seq->last_by = by = event->y ;  /*            press (x,y) coords */
         seq->cmap_changed = 0 ;

         /* 26 Feb 2007: Buttons 4 and 5 = Scroll Wheel = change slice */

         if( but == Button4 || but == Button5 ){
           if( seq->button2_enabled ){ busy=0; EXRETURN; }  /* 10 Oct 2007 */
           if( (event->state & (Mod1Mask|Mod2Mask)) ){ /* mod+scroll == '{}' */
             cbs.reason = isqCR_keypress ;
             cbs.event  = ev ;
             cbs.key    = (but==Button4) ? '}' : '{' ; /* == change threshold */
             cbs.nim    = seq->im_nr ;
             SEND(seq,cbs) ;
           } else {                           /* no modifiers == change slice */
             int nold=seq->im_nr , dd=(but==Button4)?-1:+1 , nnew ;
             if( AFNI_yesenv("AFNI_INDEX_SCROLLREV") ) dd = -dd ;
             nnew = nold + dd ;
             ISQ_timer_stop(seq) ;
             if( nnew >= 0 && nnew < seq->status->num_total )
               ISQ_redisplay( seq , nnew , isqDR_display ) ;
           }
           MCW_discard_events( w , ButtonPressMask ) ;
           busy=0; EXRETURN;
         }

         MCW_widget_geom( w , &width , &height , NULL,NULL ) ;
         seq->wimage_width  = width ;
         seq->wimage_height = height ;

         MCW_discard_events( w , ButtonPressMask ) ;

         /* 12-17 Jun 2002: Shift+Button2 for picking crop rectangle */

         if( w == seq->wimage &&
             ( (but==Button2 && (event->state & ShiftMask)) ||
               (seq->crop_drag)                            )  ){

           ISQ_cropper( seq , event ) ;
           busy=0; EXRETURN ;

         } /* end of cropping stuff */

         /** 03 Oct 2002: change Shift+Button1 into Button2 **/

         if( but == Button1 &&
             ( seq->cursor_state == CURSOR_PENCIL ||
               ((event->state & ShiftMask) && !(event->state & ControlMask)) ) )
           event->button = but = Button2 ;

         /*-- default processing --*/

         switch( but ){

            case Button3:
            case Button1:{
              int imx,imy,nim;

              /* while Button2 is active, nothing else is allowed */

              if( seq->button2_active ){
                /*** XBell(seq->dc->display,100) ; ***/
                busy=0; EXRETURN ;
              }

              /* Button3 presses in the image with a modifier
                 key pressed also means to popup some menu    */

              if( w == seq->wimage && but == Button3 &&
                  (event->state & (ShiftMask|ControlMask|Mod1Mask)) ){

                /* 23 Oct 1996: Simulation of bottom buttons */

                if( (event->state & ShiftMask) && !(event->state & ControlMask) )
                  ISQ_but_disp_CB( seq->wbut_bot[NBUT_DISP] , seq , NULL ) ;

                else if( (event->state & ControlMask) ){
                  if( seq->status->num_total > 1 && !(event->state & ShiftMask) ){
                    ISQ_montage_CB( seq->wbut_bot[NBUT_MONT] , seq , NULL ) ;
                  } else {
                    XmMenuPosition( seq->wbar_menu , event ) ;
                    XtManageChild ( seq->wbar_menu ) ;
                  }
                }

                else if( (seq->opt.save_one || seq->status->num_total > 1)
                         && (event->state & Mod1Mask) )
                   ISQ_but_save_CB( seq->wbut_bot[NBUT_SAVE] , seq , NULL ) ;

                else
                   XBell( seq->dc->display , 100 ) ;

              /* compute the location in the image
                 where the button event transpired, and send to AFNI */

              } else if( w == seq->wimage && seq->status->send_CB != NULL ){

                seq->wimage_width = -1 ;
                ISQ_mapxy( seq , bx,by , &imx,&imy,&nim ) ;
                cbs.reason = isqCR_buttonpress ;
                cbs.event  = ev ;
                cbs.xim    = imx ;
                cbs.yim    = imy ;
                cbs.nim    = nim ;

                if( but == Button1 &&
                    (event->state & ControlMask) ){ /* 18 Oct 2001 */
                   event->button = Button3 ;        /* fake Button3 press */
                }

                if( event->button == Button3 )      /* 04 Nov 2003: only for Button3 */
#if 0
                  seq->status->send_CB( seq , seq->getaux , &cbs ) ;
#else
                  SEND(seq,cbs) ;
#endif
              }
            }
            break ;

            /* pass this event to the separate handler, if allowed */

            case Button2:{

              /* drawing mode */

              if( seq->button2_enabled && w == seq->wimage )
                 ISQ_button2_EV( w , client_data , ev , continue_to_dispatch ) ;
              else
                 { XBell(seq->dc->display,100); busy=0; EXRETURN; }
            }
            break ;

            default: break ;
         }
      }
      ISQ_but_done_reset( seq ) ;
      break ;

      /*----- window changed size -----*/

      case ConfigureNotify:{
         XConfigureEvent * event = (XConfigureEvent *) ev ;

         static int am_active = 0  ;  /* 09 Oct 1999 */

#if 0
         /* 04 Nov 2003: don't do anything while mouse is down */
         /* [doesn't work well - usually prevents anything at all] */

         { Window rW,cW ; int rx,ry,x,y ; unsigned int mask ;
           XQueryPointer(XtDisplay(w),XtWindow(w),&rW,&cW,&rx,&ry,&x,&y,&mask) ;
           if( mask & (Button1Mask|Button2Mask|Button3Mask) ) break ;
         }
#endif

         if( am_active ) break ;      /* prevent recursion */
         am_active = 1 ;

 if(PRINT_TRACING){
  char str[256] ;
  sprintf(str," .. ConfigureNotify: width=%d height=%d",
          event->width,event->height);
  STATUS(str) ;
 }

         /* simply delete the XImage sized to the window;
            redisplay will then automatically size it when called */

         if( w == seq->wimage ){

            if( (seq->sized_xim == NULL)                  ||
                (event->width  != seq->sized_xim->width ) ||
                (event->height != seq->sized_xim->height)   ){

               seq->wimage_width = seq->wimage_height = -1 ; /* Feb 1998 */

               KILL_2ndXIM( seq->given_xim , seq->sized_xim ) ;

               /*-- 09 Oct 1999: if ordered, enforce aspect --*/
               /*-- 21 Oct 2003: only if it's been a while  --*/

#if 0
fprintf(stderr,"ConfigureNotify: width=%d height=%d\n",event->width,event->height);
#endif

               if( AFNI_yesenv("AFNI_ENFORCE_ASPECT") && !seq->opt.free_aspect ){
                 static int last_time=0 ; int now_time=NI_clock_time() ;
                 if( now_time == 0 || now_time-last_time > 33 )
                   ISQ_reset_dimen( seq, seq->last_width_mm, seq->last_height_mm ) ;
#if 0
else fprintf(stderr,"  -- too soon to enforce aspect!\n") ;
#endif
                 last_time = now_time ;
               }

               /*-- now show the image in the new window size --*/

               ISQ_show_image( seq ) ;
            }

         } else if( w == seq->wbar ){

             if( (seq->sized_xbar == NULL)                  ||
                 (event->width  != seq->sized_xbar->width ) ||
                 (event->height != seq->sized_xbar->height)   ){

               KILL_2ndXIM( seq->given_xbar , seq->sized_xbar ) ;
               ISQ_show_bar( seq ) ;
            }
         }

         am_active = 0 ;
      }
      break ;

      /*----- ignore all other events -----*/

      default: break ;

   } /* end of switch ev->type */

   busy=0; EXRETURN ;
}

/*-----------------------------------------------------------------------
   Handle Button2 events in the image window -- Feb 1998
-------------------------------------------------------------------------*/

#define NPTS_MAX 4095  /* max # points in a single button2 operation */

void ISQ_button2_EV( Widget w , XtPointer client_data ,
                     XEvent * ev , Boolean * continue_to_dispatch )
{
   MCW_imseq * seq = (MCW_imseq *) client_data ;
   ISQ_cbs cbs ;
   static int nsav ;
   static int * bxsav=NULL , *bysav=NULL , *xyout=NULL ;

ENTRY("ISQ_button2_EV") ;

   /* check for legality */

   if( !ISQ_REALZ(seq) || !seq->button2_enabled || w != seq->wimage ) EXRETURN ;

   ISQ_timer_stop(seq) ;

   switch( ev->type ){

      /*----- take button press -----*/

      case ButtonPress:{
         XButtonEvent *event = (XButtonEvent *) ev ;
         int bx,by , but , xim,yim,zim ;

         but = event->button ; if( but != Button2 ) EXRETURN ;

         seq->button2_active = 1 ;  /* allow other button2 stuff to happen */

         /* 1st time in: allocate space to save points */

         if( bxsav == NULL ){
           bxsav = (int *) malloc( sizeof(int) * (NPTS_MAX+1) ) ;
           bysav = (int *) malloc( sizeof(int) * (NPTS_MAX+1) ) ;
         }

         /* save this point */

         bx = event->x ; by = event->y ;
         bxsav[0] = bx ; bysav[0] = by ; nsav = 1 ;

         /* find where this point is in original images --
            if it is illegal, quit this mockery of a travesty of a sham */

         seq->wimage_width = -1 ;
         ISQ_mapxy( seq , bx,by , &xim,&yim,&zim ) ;
         if( xim < 0 || yim < 0 || zim < 0 || zim >= seq->status->num_total ){
            seq->button2_active = 0 ;         /* disallow button2 stuff */
            XBell( seq->dc->display , 100 ) ; /* express our displeasure */
            EXRETURN ;
         }

         /* draw this point */

         if( seq->button2_drawmode != BUTTON2_NODRAW ){
            DC_fg_colorpix( seq->dc , seq->button2_pixel ) ;
            XDrawPoint( seq->dc->display , XtWindow(seq->wimage) ,
                        seq->dc->myGC , bx,by ) ;
         }
      }
      break ;

      /*----- take button release -----*/

      case ButtonRelease:{
         XButtonEvent * event = (XButtonEvent *) ev ;
         int bx,by ;
         int ii,nout , nim , xim,yim,zim ;

         /* check for legality  */

         if( !seq->button2_active || event->button != Button2 ) EXRETURN ;

         bx = event->x ; by = event->y ;  /* where did it happen? */

         /* if a new point, save it and draw it */

         if( bx != bxsav[nsav-1] || by != bysav[nsav-1] ){

            if( seq->button2_drawmode == BUTTON2_POINTS ){
               XDrawPoint( seq->dc->display , XtWindow(seq->wimage) ,
                           seq->dc->myGC , bx,by ) ;
            } else if( seq->button2_drawmode != BUTTON2_NODRAW ){
               if( seq->button2_width > 0 )                     /* 08 Oct 2002 */
                 DC_linewidth( seq->dc , seq->button2_width ) ;
               XDrawLine( seq->dc->display , XtWindow(seq->wimage) ,
                          seq->dc->myGC , bxsav[nsav-1],bysav[nsav-1],bx,by ) ;
               if( seq->button2_width > 0 ) DC_linewidth( seq->dc , 0 ) ;
            }

            bxsav[nsav] = bx ; bysav[nsav] = by ;
            if( nsav < NPTS_MAX ) nsav++ ;
         }

         /* this is the last point in this sequence --
            if we are drawing closed polygon, then close it now */

         if( seq->button2_drawmode == BUTTON2_CLOSEDPOLY && nsav > 2 ){
            if( seq->button2_width > 0 )                     /* 08 Oct 2002 */
              DC_linewidth( seq->dc , seq->button2_width ) ;
            XDrawLine( seq->dc->display , XtWindow(seq->wimage) ,
                       seq->dc->myGC , bxsav[nsav-1],bysav[nsav-1] ,
                                       bxsav[0]     ,bysav[0]       ) ;
            if( seq->button2_width > 0 ) DC_linewidth( seq->dc , 0 ) ;

            /* and add the 1st point to the list again */

            bxsav[nsav] = bxsav[0] ; bysav[nsav] = bysav[0] ;
            if( nsav < NPTS_MAX ) nsav++ ;
         }

         /* 1st time here: make space for output list */

         if( xyout == NULL )
            xyout = (int *) malloc( sizeof(int) * 2*NPTS_MAX ) ;

         /* now assemble output list of (x,y) pairs,
            in the original image grid --
            but only save points that are in the same image as the 1st point */

         seq->wimage_width = -1 ;
         ISQ_mapxy( seq , bxsav[0] , bysav[0] , &xim,&yim,&zim ) ;
         nim = zim ; xyout[0] = xim ; xyout[1] = yim ; nout = 1 ;
         for( ii=1 ; ii < nsav ; ii++ ){
            ISQ_mapxy( seq , bxsav[ii] , bysav[ii] , &xim,&yim,&zim ) ;
            if( zim == nim && xim >= 0 && yim >= 0 ){
               xyout[2*nout] = xim ; xyout[2*nout+1] = yim ;
               nout++ ;
            }
         }

         /* send to the almighty AFNI */

         cbs.reason   = isqCR_button2_points ;
         cbs.event    = ev ;
         cbs.key      = ii ;                 /* number of points */
         cbs.nim      = nim ;                /* z coord */
         cbs.userdata = (XtPointer) xyout ;  /* x & y coords */
#if 0
         seq->status->send_CB( seq , seq->getaux , &cbs ) ;
#else
         SEND(seq,cbs) ;
#endif

         seq->button2_active = 0 ;  /* disallow button2 stuff */
      }
      break ;

      /*----- take motion events:
              this is minimal so as to keep up with mouse movements -----*/

      case MotionNotify:{
         XMotionEvent * event = (XMotionEvent *) ev ;
         int bx,by ;

         /* check for legality */

         if( !seq->button2_active || (event->state & Button2Mask) == 0 ) EXRETURN ;

         /* if point is redundant with last one, skip it */

         bx = event->x ; by = event->y ;
         if( bx == bxsav[nsav-1] && by == bysav[nsav-1] ) EXRETURN ;

         /* draw point or line to point */

         if( seq->button2_drawmode == BUTTON2_POINTS ){
            XDrawPoint( seq->dc->display , XtWindow(seq->wimage) ,
                        seq->dc->myGC , bx,by ) ;
         } else if( seq->button2_drawmode != BUTTON2_NODRAW ){
            if( seq->button2_width > 0 )                     /* 08 Oct 2002 */
              DC_linewidth( seq->dc , seq->button2_width ) ;
            XDrawLine( seq->dc->display , XtWindow(seq->wimage) ,
                       seq->dc->myGC , bxsav[nsav-1],bysav[nsav-1],bx,by ) ;
            if( seq->button2_width > 0 ) DC_linewidth( seq->dc , 0 ) ;
         }

         /* save it */

         bxsav[nsav] = bx ; bysav[nsav] = by ;
         if( nsav < NPTS_MAX ) nsav++ ;
      }
      break ;

   }
   EXRETURN ;
}

/*---------------------------------------------------------------------
   process Disp button press for an imseq:
     change the way the image is displayed (flip, rotate, ...),
     by popping up a dialog
-----------------------------------------------------------------------*/

void ISQ_but_disp_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   MCW_imseq *seq = (MCW_imseq *) client_data ;
   int ib ;
   Widget rctop , rcboxes , shtop ;
   Widget swtop=NULL ;

ENTRY("ISQ_but_disp_CB") ;

   if( ! ISQ_REALZ(seq) || seq->dialog != NULL ) EXRETURN ;

   for( ib=0 ; ib < NBUTTON_BOT-1 ; ib++ )       /* turn off buttons  */
     if( ISQ_but_bot_dial[ib] == True )          /* that also want to */
       SENSITIZE( seq->wbut_bot[ib] , False ) ;  /* use seq->dialog   */

   seq->dialog = XtVaCreatePopupShell(
                    "menu" , xmDialogShellWidgetClass , seq->wtop ,
                       XmNtitle , "Display Options" ,
                       XmNdeleteResponse , XmDO_NOTHING ,
                       XmNinitialResourcesPersistent , False ,
                    NULL ) ;

   SAVEUNDERIZE(seq->dialog) ; /* 27 Feb 2001 */

   DC_yokify( seq->dialog , seq->dc ) ;  /* 14 Sep 1998 */

   seq->dialog_starter = NBUT_DISP ;

#if 1
   if( MCW_isitmwm(w) )
      XtVaSetValues( seq->dialog ,
                       XmNmwmDecorations , MWM_DECOR_BORDER ,
                       XmNmwmFunctions ,   MWM_FUNC_MOVE
                                         | MWM_FUNC_CLOSE ,
                     NULL ) ;
#endif

   XmAddWMProtocolCallback(           /* make "Close" window menu work */
           seq->dialog ,
           XmInternAtom( seq->dc->display , "WM_DELETE_WINDOW" , False ) ,
           ISQ_disp_act_CB , seq ) ;

   for( ib=0 ; ib < NACT_DISP ; ib++ )
      ISQ_disp_act[ib].data = (XtPointer) seq ;

   if( seq->dc->height < 1024 ||               /* 21 Jun 2005 */
       AFNI_yesenv("AFNI_DISP_SCROLLBARS") ){  /* 31 Jan 2002 */

      shtop = swtop = XtVaCreateManagedWidget(
                 "menu" , xmScrolledWindowWidgetClass , seq->dialog ,
                    XmNscrollingPolicy        , XmAUTOMATIC ,
                    XmNvisualPolicy           , XmVARIABLE ,
                    XmNscrollBarDisplayPolicy , XmAS_NEEDED /* XmSTATIC */ ,
                    XmNinitialResourcesPersistent , False ,
                 NULL ) ;
   } else {
      shtop = seq->dialog ;
   }

   rctop = XtVaCreateWidget(
              "menu" , xmRowColumnWidgetClass , shtop ,
                 XmNpacking    , XmPACK_TIGHT ,
                 XmNnumColumns , 1 ,

                 XmNinitialResourcesPersistent , False ,
              NULL ) ;

   rcboxes = XtVaCreateWidget(
                "menu" , xmRowColumnWidgetClass , rctop ,
                   XmNpacking    , XmPACK_TIGHT ,
                   XmNnumColumns , 2 ,

                   XmNinitialResourcesPersistent , False ,
              NULL ) ;

   for( ib=0 ; ib < NBOX_DISP ; ib++ ){
      int jh ;
      char **bbh = ISQ_bb_allhelp[ib] ;
      char **cch = ISQ_bb_allhint[ib] ;

      /*** 30 Oct 1996: transformations just above the IMPROC buttons ***/

      if( ib == NTOG_IMP ){
         int nav = 0 ;

         /*---- FIRST, add some check boxes for special options ----*/

         char *save_one_label[] = { "Save One" }  ; /* 26 Jul 2001 */
         char *save_agif_label  = "Save Anim GIF" ; /* 27 Jul 2001 */
         char *save_mpeg_label  = "Save Anim MPG" ; /* 02 Aug 2001 */
         char *save_anim_label[2] ;

         seq->save_one_bbox = new_MCW_bbox( rcboxes ,
                                            1 ,
                                            save_one_label ,
                                            MCW_BB_check ,
                                            MCW_BB_frame ,
                                            ISQ_disp_act_CB , (XtPointer) seq ) ;
         MCW_reghelp_children( seq->save_one_bbox->wrowcol ,
                               " \n"
                               "When pressed IN, then the 'Save' button\n"
                               "will only save a snapshot of the current\n"
                               "display.  This is the ONLY way to save\n"
                               "a montage.\n"
                               "\n"
                               "When pressed OUT, then the 'Save' button\n"
                               "asks for the first and last image indexes\n"
                               "to save, and then saves each individual\n"
                               "image (no montage) to a file.\n"
                             ) ;
         MCW_reghint_children( seq->save_one_bbox->wrowcol ,
                               "Save just 1 (including montage)" ) ;

         if( ppmto_agif_filter != NULL || ppmto_mpeg_filter != NULL ){
           int nb = 0 ;
           if( ppmto_agif_filter != NULL ) save_anim_label[nb++]=save_agif_label;
           if( ppmto_mpeg_filter != NULL ) save_anim_label[nb++]=save_mpeg_label;
           seq->save_agif_bbox = new_MCW_bbox( rcboxes ,
                                               nb ,
                                               save_anim_label ,
                                               MCW_BB_radio_zero ,
                                               MCW_BB_frame ,
                                               ISQ_disp_act_CB, (XtPointer)seq );
           MCW_reghelp_children( seq->save_agif_bbox->wrowcol ,
                                 " \n"
                                 "Controls if image sequence is saved to\n"
                                 "an animation file, rather than a bunch\n"
                                 "of separate image files.\n"
                                 "* This takes precedence over 'Save One',\n"
                                 "    if it is also turned on.\n"
                                 "* GIF animations require gifsicle.\n"
                                 "* MPEG-1 animations require mpeg_encode.\n"
                               ) ;
           MCW_reghint_children( seq->save_agif_bbox->wrowcol ,
                                 "Save image sequence to animation" ) ;
         } else {
           seq->save_agif_bbox = NULL ;
         }

         /*---- OK, do the transforms NOW ----*/

         if( seq->status->slice_proj != NULL &&
             seq->status->slice_proj->num > 0  ){  /* 31 Jan 2002 */

             (void) XtVaCreateManagedWidget(
                      "menu" , xmSeparatorWidgetClass , rcboxes ,
                         XmNseparatorType , XmSINGLE_LINE ,
                         XmNinitialResourcesPersistent , False ,
                      NULL ) ;

             seq->slice_proj_av =
                new_MCW_optmenu( rcboxes , "Project" ,
                                 0 , seq->status->slice_proj->num ,
                                 seq->slice_proj_index , 0 ,
                                 ISQ_slice_proj_CB , (XtPointer) seq ,
                                 ISQ_transform_label ,
                                 (XtPointer) seq->status->slice_proj ) ;

             if( seq->status->slice_proj->num >= COLSIZE )
                AVOPT_columnize( seq->slice_proj_av ,
                                 (seq->status->slice_proj->num/COLSIZE)+1 ) ;

             MCW_reghelp_children( seq->slice_proj_av->wrowcol ,
                                   "Choose a projection function\n"
                                   "to apply to plus-or-minus\n"
                                   "'Slab' images from each pixel.\n"
                                   "Built-in projections:\n"
                                   " Minimum = smallest value in slab\n"
                                   " Maximum = largest value in slab\n"
                                   " Mean    = average value in slab\n"
                                   " Median  = median value in slab\n"
                                   " Extreme = value farthest from median" ) ;

             MCW_reghint_children( seq->slice_proj_av->wrowcol ,
                                   "Image projection function"  ) ;

             seq->slice_proj_range_av =
                new_MCW_optmenu( rcboxes , "Slab +-" ,
                                 0 , 19 , seq->slice_proj_range , 0 ,
                                 ISQ_slice_proj_CB , (XtPointer) seq ,
                                 NULL , NULL ) ;
             MCW_reghelp_children( seq->slice_proj_range_av->wrowcol ,
                                   "Choose thickness of Project slice\n"
                                   "package (in each direction from\n"
                                   "central slice).  For example:\n"
                                   " 2 ==> slab is 5 images thick\n"
                                   "       (2 before, 2 after, central)" ) ;
             MCW_reghint_children( seq->slice_proj_range_av->wrowcol ,
                                   "Slab half-thickness"              ) ;
             nav++ ;
         }

         /* 0D transforms */

         if( seq->status->transforms0D != NULL &&
             seq->status->transforms0D->num > 0  ){

             (void) XtVaCreateManagedWidget(
                      "menu" , xmSeparatorWidgetClass , rcboxes ,
                         XmNseparatorType , XmSINGLE_LINE ,
                         XmNinitialResourcesPersistent , False ,
                      NULL ) ;

             seq->transform0D_av =
                new_MCW_optmenu( rcboxes , "Tran 0D" ,
                                 0 , seq->status->transforms0D->num ,
                                 seq->transform0D_index , 0 ,
                                 ISQ_transform_CB , (XtPointer) seq ,
                                 ISQ_transform_label ,
                                 (XtPointer) seq->status->transforms0D ) ;

             if( seq->status->transforms0D->num >= COLSIZE )
                AVOPT_columnize( seq->transform0D_av ,
                                 (seq->status->transforms0D->num/COLSIZE)+1 ) ;

             MCW_reghelp_children( seq->transform0D_av->wrowcol ,
                                   "Choose a function to apply to\n"
                                   "each point in the image." ) ;
             MCW_reghint_children( seq->transform0D_av->wrowcol ,
                                   "Pointwise transformations" ) ;
             nav++ ;
         }

         /* 2D transforms */

         if( seq->status->transforms2D != NULL &&
             seq->status->transforms2D->num > 0  ){

             (void) XtVaCreateManagedWidget(
                      "menu" , xmSeparatorWidgetClass , rcboxes ,
                         XmNseparatorType , XmSINGLE_LINE ,
                         XmNinitialResourcesPersistent , False ,
                      NULL ) ;

             seq->transform2D_av =
                new_MCW_optmenu( rcboxes , "Tran 2D" ,
                                 0 , seq->status->transforms2D->num ,
                                 seq->transform2D_index , 0 ,
                                 ISQ_transform_CB , (XtPointer) seq ,
                                 ISQ_transform_label ,
                                 (XtPointer) seq->status->transforms2D ) ;

             if( seq->status->transforms2D->num >= COLSIZE )
                AVOPT_columnize( seq->transform2D_av ,
                                 (seq->status->transforms2D->num/COLSIZE)+1 ) ;

             MCW_reghelp_children( seq->transform2D_av->wrowcol ,
                                   "Choose a function to apply to\n"
                                   "the underlay image as a whole." ) ;
             MCW_reghint_children( seq->transform2D_av->wrowcol ,
                                   "Global transformations" ) ;
             nav++ ;
         }

         /* 30 Dec 1998: rowgraphs */

         if( nav > 0 && seq->status->send_CB != NULL ){
            (void) XtVaCreateManagedWidget(
                     "menu" , xmSeparatorWidgetClass , rcboxes ,
                        XmNseparatorType , XmSINGLE_LINE ,
                        XmNinitialResourcesPersistent , False ,
                     NULL ) ;

            seq->rowgraph_av =
               new_MCW_optmenu( rcboxes , "RowGraphs" ,
                                0 , ROWGRAPH_MAX , seq->rowgraph_num , 0 ,
                                ISQ_rowgraph_CB , (XtPointer) seq ,
                                ISQ_rowgraph_label , NULL ) ;
            AVOPT_columnize( seq->rowgraph_av , 2 ) ;

            MCW_reghelp_children( seq->rowgraph_av->wrowcol ,
                                  "Rowgraphs are plots of the underlay\n"
                                  "(grayscale) image intensity as\n"
                                  "x vs. y graphs.  Each graph is from\n"
                                  "one displayed horizontal row of the\n"
                                  "image.  The bottom rowgraph is from\n"
                                  "the image row under the crosshairs.\n"
                                  "Upper rowgraphs are from higher image\n"
                                  "rows.  Note that image transformations\n"
                                  "functions and image rotations/flips\n"
                                  "will affect the rowgraphs as well as\n"
                                  "the image display.\n\n"
                                  "N.B.: The color 'UK Flag' marker indicates\n"
                                  "      the crosshair focus point. It can be\n"
                                  "      turned off via the 'No Overlay' button."
                                 ) ;
            MCW_reghint_children( seq->rowgraph_av->wrowcol ,
                                  "Number of image rows to graph" ) ;
            nav++ ;
         }

         /* 21 Jan 1999: surfgraph */

         if( nav > 0 && seq->status->send_CB != NULL ){
            (void) XtVaCreateManagedWidget(
                     "menu" , xmSeparatorWidgetClass , rcboxes ,
                        XmNseparatorType , XmSINGLE_LINE ,
                        XmNinitialResourcesPersistent , False ,
                     NULL ) ;

            seq->surfgraph_av =
               new_MCW_optmenu( rcboxes , "SurfGraph" ,
                                0 , SURFGRAPH_MAX , seq->surfgraph_num , 0 ,
                                ISQ_surfgraph_CB , (XtPointer) seq ,
                                ISQ_surfgraph_label , NULL ) ;

            MCW_reghelp_children( seq->surfgraph_av->wrowcol ,
                                  "The SurfGraph is a wiremesh plot of the\n"
                                  "underlay (grayscale) image intensity vs.\n"
                                  "x and y.  Use the arrows in the SurfGraph\n"
                                  "window to rotate the viewpoint; use the\n"
                                  "middle button between the arrows to reset\n"
                                  "the viewpoint to the default orientation.\n"
                                  "\n"
                                  "N.B.: The plotting routine may produce some\n"
                                  "        erroneous vertical lines on occasion.\n"
                                  "      The color 'UK Flag' marker indicates\n"
                                  "        crosshair focus point.  It is drawn\n"
                                  "        on top of the surface at the end, and\n"
                                  "        so is always visible, even if it should\n"
                                  "        be hidden behind the surface; that is,\n"
                                  "        it shines through, no matter what.\n"
                                  "      The color marker can be turned off with\n"
                                  "        the 'No Overlay' button."
                                 ) ;
            MCW_reghint_children( seq->surfgraph_av->wrowcol ,
                                  "Plot wiremesh surface?" ) ;
            nav++ ;
         }

         /* final separator */

         if( nav ) (void) XtVaCreateManagedWidget(
                            "menu" , xmSeparatorWidgetClass , rcboxes ,
                               XmNseparatorType , XmSINGLE_LINE ,
                               XmNinitialResourcesPersistent , False ,
                            NULL ) ;
      }

      /*** back to the button box stuff ***/

      seq->bbox[ib] = new_MCW_bbox( rcboxes ,
                                     ISQ_dispbb[ib].nbut ,
                                     ISQ_dispbb[ib].lbut ,
                                     ISQ_dispbb[ib].type ,
                                     ISQ_dispbb[ib].frame ,
                                     ISQ_disp_act_CB , (XtPointer) seq ) ;

      seq->bbox[ib]->parent = (XtPointer) seq ;

      seq->num_bbox ++ ;

      for( jh=0 ; jh < seq->bbox[ib]->nbut ; jh++ ){
         MCW_register_help( seq->bbox[ib]->wbut[jh] , bbh[jh] ) ;
         MCW_register_hint( seq->bbox[ib]->wbut[jh] , cch[jh] ) ;
      }

   }

#define NO_GROUP_SCALE
#ifdef  NO_GROUP_SCALE
   XtUnmanageChild( seq->bbox[NTOG_SCL]->wtop ) ;  /* turn this box off! */
#endif

   if( seq->last_image_type != MRI_complex )
     XtUnmanageChild( seq->bbox[NTOG_CX]->wtop ) ;

   XtManageChild( rcboxes ) ;

   (void) MCW_action_area( rctop , ISQ_disp_act , NACT_DISP ) ;

   XtManageChild( rctop ) ;

   if( swtop != NULL ){       /* 31 Jan 2002 */
     int wx,hy , cmax ;
     MCW_widget_geom( rctop  , &wx,&hy,NULL,NULL ) ;
     cmax = seq->dc->height-128 ; if( hy > cmax ) hy = cmax ;
     XtVaSetValues( seq->dialog , XmNwidth,wx+33,XmNheight,hy+19 , NULL ) ;
   }

   ISQ_place_dialog( seq ) ;  /* 05 Jan 1999 */

   XtPopup( seq->dialog , XtGrabNone ) ; NI_sleep(1);

   if( seq->top_clip <= 0.0f ) ALLOW_CLIPPING( seq , 0 ) ;

   ISQ_disp_options( seq , False ) ;  /* set toggles from option list */
   seq->save_opt = seq->opt ;         /* for use with Reset button */

   NORMAL_cursorize( seq->dialog ) ;

   ISQ_but_done_reset( seq ) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------
   05 Jan 1999: place the dialog near the image window
-------------------------------------------------------------------------*/

void ISQ_place_dialog( MCW_imseq *seq )
{
   if( ISQ_REALZ(seq) && !seq->dont_place_dialog )
     ISQ_place_widget( seq->wtop , seq->dialog ) ;

   return ;
}

/*-----------------------------------------------------------------------*/

void ISQ_place_widget( Widget wmain , Widget w )  /* 27 Oct 2003 */
{
   int dw,dh,dx,dy , xp,yp , wx,hy,xx,yy , sh,sw ;

ENTRY("ISQ_place_widget") ;

   if( wmain == (Widget)NULL || w == (Widget)NULL ) EXRETURN ;
   if( !XtIsRealized(wmain)  || !XtIsRealized(w)  ) EXRETURN ;

   MCW_widget_geom( wmain , &wx,&hy,&xx,&yy ) ;  /* geometry of shell */
   MCW_widget_geom( w     , &dw,&dh,&dx,&dy ) ;  /* of dialog */

   sh = HeightOfScreen(XtScreen(wmain)) ;
   sw = WidthOfScreen (XtScreen(wmain)) ;

   xp = xx+wx+8 ;
   if( xp+dw > sw ) xp = xx-dw-8 ;
   if( xp    < 0  ) xp = 0 ;

   yp = yy-4 ;
   if( yp+dh > sh ) yp = sh - dh ;
   if( yp    < 0  ) yp = 0 ;

   RWC_xineramize( XtDisplay(wmain) , xp,yp,dw,dh , &xp,&yp ); /* 27 Sep 2000 */

   XtVaSetValues( w , XmNx , xp , XmNy , yp , NULL ) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------
  Callback for button and toggle actions in the display dialog
-------------------------------------------------------------------------*/

void ISQ_disp_act_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   MCW_imseq *seq           = (MCW_imseq *)client_data ;
   XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *)call_data ;

   int ib , close_window ;
   char *wname ;
   Boolean new_opt = False ;

#ifdef FLASH_TOGGLE
   Boolean flasher ;
#endif

ENTRY("ISQ_disp_act_CB") ;

   if( !ISQ_REALZ(seq) || seq->dialog==NULL || seq->dialog_starter!=NBUT_DISP ) EXRETURN ;

   wname = XtName(w) ;

   for( ib=0 ; ib < NACT_DISP ; ib++ )           /* button index, if any */
     if( strcmp(wname,ISQ_disp_act[ib].label) == 0 ) break ;

   close_window = (ib == DISP_OK)  /* button to exit */
                 ||
                  ( cbs->reason != XmCR_ACTIVATE       &&   /* exit if */
                    cbs->reason != XmCR_DISARM           ); /* not button */

#ifdef FLASH_TOGGLE
   flasher = (cbs->reason == XmCR_DISARM) && (!close_window) ;
   if( flasher ) MCW_invert_widget( w ) ;
#endif

   if( ib == DISP_UNDO ){               /* restore options from entry */
      seq->opt = seq->save_opt ;        /* and then set toggles */
      ISQ_disp_options( seq , False ) ;
      new_opt = True ;
      AV_SENSITIZE(seq->ov_opacity_av,!seq->opt.no_overlay) ; /* 09 Mar 2001 */

   } else {                                     /* any other activation: */
      new_opt = ISQ_disp_options( seq , True ); /* --> set options */
   }

   if( close_window ){                          /* close the window */
      XtDestroyWidget( seq->dialog ) ; NI_sleep(1) ;
      seq->dialog = NULL ;
      for( ib=0 ; ib < NBUTTON_BOT-1 ; ib++ )       /* turn buttons back on */
         if( ISQ_but_bot_dial[ib] == True )         /* that also want to   */
           SENSITIZE( seq->wbut_bot[ib] , True ) ;  /* use seq->dialog    */

      for( ib=0 ; ib < seq->num_bbox ; ib++ ) myXtFree( seq->bbox[ib] ) ;
      seq->num_bbox = 0 ;
      seq->dialog_starter = -1 ;

      FREE_AV( seq->transform0D_av ) ;
      FREE_AV( seq->transform2D_av ) ;
      FREE_AV( seq->rowgraph_av )    ;
      FREE_AV( seq->surfgraph_av )   ;  /* 21 Jan 1999 */
   }

   if( new_opt ){
     ISQ_redisplay( seq , -1 , isqDR_reimage ) ;  /* redo current image */

      /* 01 Dec 1999: perhaps redraw winfo label */

      if( ISQ_USE_SIDES(seq) ){
         seq->im_label[0] = '\0' ;  /* will force redraw */
         ISQ_draw_winfo( seq ) ;
      }
   }

#ifdef FLASH_TOGGLE
   if( flasher ) MCW_invert_widget( w ) ;  /* flash togglebutton */
#endif

   ISQ_but_done_reset( seq ) ;
   EXRETURN ;
}

/*----------------------------------------------------------------------
  map the toggle-button states TO   the options if set == True,
                               FROM the options if set == False

  in the former case, return True if any options changed, False otherwise
  in the latter case, return False always (options ARE unchanged)
------------------------------------------------------------------------*/

Boolean ISQ_disp_options( MCW_imseq *seq , Boolean set )
{
   int bval[NBOX_DISP] ;
   int ib ;

ENTRY("ISQ_disp_options") ;

   if( !ISQ_VALID(seq) || seq->dialog==NULL || seq->dialog_starter!=NBUT_DISP )
     RETURN(False) ;

   if( set ){                         /* set structure from widgets */
      ISQ_options inopt = seq->opt ;
      Boolean changed ;

      for( ib=0 ; ib < NBOX_DISP ; ib++ )
        bval[ib] = MCW_val_bbox( seq->bbox[ib] ) ;

      seq->opt.mirror      = ( bval[NTOG_MIR] & 1 ) != 0 ;

      seq->opt.rot         = bval[NTOG_ROT] ;

      seq->opt.no_overlay  = ( bval[NTOG_COL] & 1 ) != 0 ;

      AV_SENSITIZE(seq->ov_opacity_av,!seq->opt.no_overlay) ; /* 09 Mar 2001 */

      seq->opt.scale_group = bval[NTOG_SCL] ;

      seq->opt.scale_range = bval[NTOG_RNG] ;

      seq->opt.free_aspect = ( bval[NTOG_ASP] & ISQ_ASPECT    ) != 0 ;
      seq->opt.save_nsize  = ( bval[NTOG_SAV] & ISQ_SAV_NSIZE ) != 0 ;
      seq->opt.save_pnm    = ( bval[NTOG_SAV] & ISQ_SAV_PNM   ) != 0 ;

      seq->opt.save_one    = MCW_val_bbox(seq->save_one_bbox)   != 0 ; /* 26 Jul 2001 */

      seq->opt.save_agif = seq->opt.save_mpeg = 0 ;
      if( seq->save_agif_bbox != NULL ){
         int bv = MCW_val_bbox(seq->save_agif_bbox) ;
                                                        /* 07 Apr 2005: oops */
         switch( bv ){        /* need to handle case when agif isn't allowed */
           case 1:
                  if( ppmto_agif_filter != NULL ) seq->opt.save_agif = 1 ;
             else if( ppmto_mpeg_filter != NULL ) seq->opt.save_mpeg = 1 ;
           break ;

           case 2:
                  if( ppmto_mpeg_filter != NULL ) seq->opt.save_mpeg = 1 ;
           break ;
         }
      }

      seq->opt.save_filter = -1 ;
      if( bval[NTOG_SAV] > ISQ_SAV_PNM && ppmto_num > 0 ){  /* 27 Jun 2001 */
         int ii ;
         for( ii=0 ; ii < ppmto_num ; ii++ ){
            if( bval[NTOG_SAV] == ppmto_bval[ii] ){
               seq->opt.save_filter = ii ; break ;
            }
         }
      }

      SET_SAVE_LABEL(seq) ;

      seq->opt.improc_code = bval[NTOG_IMP] ;

      seq->opt.cx_code = bval[NTOG_CX] ;

      /*-- sanity checks --*/

      if( seq->opt.rot != ISQ_ROT_0   &&
          seq->opt.rot != ISQ_ROT_90  &&
          seq->opt.rot != ISQ_ROT_180 &&
          seq->opt.rot != ISQ_ROT_270   ) seq->opt.rot = inopt.rot ;

      if( seq->opt.scale_group != ISQ_SCL_AUTO &&
          seq->opt.scale_group != ISQ_SCL_GRP )
                               seq->opt.scale_group = inopt.scale_group ;

      if( seq->opt.scale_range != ISQ_RNG_CLIPPED ) seq->redo_clip = 0 ;
DPRI("set scale_range =",seq->opt.scale_range) ;

      if( seq->opt.scale_range != ISQ_RNG_MINTOMAX &&
          seq->opt.scale_range != ISQ_RNG_02TO98   &&
          seq->opt.scale_range != ISQ_RNG_CLIPPED    )
                               seq->opt.scale_range = inopt.scale_range ;

      if( seq->opt.scale_range == ISQ_RNG_CLIPPED ){  /* 17 Sep 2007 */
        if( seq->top_clip <= 0.0f ){
          ALLOW_CLIPPING( seq , 0 ) ;
        } else {
          seq->redo_clip = 1 ;
        }
      }

      changed = ! ISQ_OPT_EQUAL( seq->opt , inopt ) ;

      RETURN(changed) ;

   } else {    /* set widgets from structure */

      bval[NTOG_MIR] = (seq->opt.mirror) ? 1 : 0 ;
      bval[NTOG_ROT] = seq->opt.rot ;

      bval[NTOG_COL] = (seq->opt.no_overlay << 0 ) ;

      bval[NTOG_SCL] = seq->opt.scale_group ;
      bval[NTOG_RNG] = seq->opt.scale_range ;

      if( seq->opt.scale_range == ISQ_RNG_CLIPPED &&
          seq->top_clip <= 0.0f                     ){  /* 17 Sep 2007 */

        ALLOW_CLIPPING( seq , 0 ) ;
        bval[NTOG_RNG] = seq->opt.scale_range ;
      }

      bval[NTOG_ASP] = (seq->opt.free_aspect) ? ISQ_ASPECT    : 0 ;

      bval[NTOG_SAV] = ( (seq->opt.save_nsize)? ISQ_SAV_NSIZE : 0 )
                      +( (seq->opt.save_pnm)  ? ISQ_SAV_PNM   : 0 ) ;

      if( seq->opt.save_filter >= 0 && ppmto_num > 0 )       /* 27 Jun 2001 */
         bval[NTOG_SAV] = ppmto_bval[seq->opt.save_filter] ;

      bval[NTOG_IMP] = seq->opt.improc_code ;

      bval[NTOG_CX]  = seq->opt.cx_code ;

      for( ib=0 ; ib < NBOX_DISP ; ib++ )
        MCW_set_bbox( seq->bbox[ib] , bval[ib] ) ;

      MCW_set_bbox( seq->save_one_bbox ,
                    (seq->opt.save_one) ? 1 : 0 ) ; /* 26 Jul 2001 */

      if( seq->save_agif_bbox != NULL ){      /* 07 Apr 2005: oops */
         int bv=0 ; /* need to handle case when agif isn't allowed */
         if( ppmto_agif_filter != NULL )
           bv = (seq->opt.save_agif) + (seq->opt.save_mpeg)*2 ;
         else if( ppmto_mpeg_filter != NULL )
           bv = (seq->opt.save_mpeg) ;
         MCW_set_bbox( seq->save_agif_bbox , bv ) ;
      }

      RETURN(False) ;
   }
}

/*----------------------------------------------------------------------
  routines to collect statistics for scaling these images;
     ISQ_statify_all   -> do all statistics (individual then global)
     ISQ_statistics_WP -> Xt work process to do statistics on one image
     ISQ_statify_one   -> actually do statistics on one image
     ISQ_perpoints     -> get the percentage points for 2%-to-98% scaling
------------------------------------------------------------------------*/

void ISQ_statify_all( MCW_imseq * seq , Boolean stop_on_minmax )
{
   Boolean done ;
   Widget wmsg ;

ENTRY("ISQ_statify_all") ;

   if( ! ISQ_VALID(seq) ) EXRETURN ;

   /* this routine just drives the work process until it is done */

   if( !seq->glstat->mm_done ){
      wmsg = MCW_popup_message( seq->wtop ,
                                "Please Wait.\nComputing Statistics." ,
                                MCW_CALLER_KILL ) ;
   } else {
      wmsg = MCW_popup_message( seq->wtop ,
                                "Please Wait.\nComputing Histogram." ,
                                MCW_CALLER_KILL ) ;
   }

   XBell( seq->dc->display , 100 ) ;

   WATCH_cursorize( seq->wtop ) ;
   WATCH_cursorize( wmsg ) ;
   if( seq->dialog != NULL )
      WATCH_cursorize( seq->dialog ) ;

   XFlush( seq->dc->display ) ;

   if( seq->glstat->worker != 0 ){  /* remove work process, if started */
      XtRemoveWorkProc( seq->glstat->worker ) ;
      seq->glstat->worker = 0 ;
   }

   /**************************************************************/
   do{

      done = ISQ_statistics_WP( (XtPointer) seq ) ;
      done = done || ( stop_on_minmax && seq->glstat->mm_done ) ;

   } while ( ! done ) ;
   /**************************************************************/

   XtDestroyWidget( wmsg ) ; NI_sleep(1) ;

   NORMAL_cursorize( seq->wtop ) ;
   if( seq->dialog != NULL )
      NORMAL_cursorize( seq->dialog ) ;

   EXRETURN;
}

/*-----------------------------------------------------------------------*/

Boolean ISQ_statistics_WP( XtPointer client_data )
{
   MCW_imseq *seq = (MCW_imseq *) client_data ;
   ISQ_glob_statistics * gl ;

   MRI_IMAGE *im=NULL ;
   register int ntot , nser , nn ;

ENTRY("ISQ_statistics_WP") ;

   if( ! ISQ_VALID(seq) ) RETURN( True );

   gl   = seq->glstat ;
   ntot = seq->status->num_total ;  /* image counts */
   nser = seq->status->num_series ;

   /*-- first, check if all individual statistics are done --*/

   if( ! gl->mm_done ){  /* not marked as done:  check them */

      for( nn=0 ; nn < ntot ; nn++ )
         if( ! seq->imstat[nn].one_done ) break ;

      if( nn >= ntot ){ /* all were done, so finish them off */

         gl->min = seq->imstat[0].min ;
         gl->max = seq->imstat[0].max ;
         for( nn=1 ; nn < nser ; nn++ ){ /* global: images in the series */
            gl->min = MIN( gl->min , seq->imstat[nn].min ) ;
            gl->max = MAX( gl->max , seq->imstat[nn].max ) ;
         }
         ISQ_SCLEV(gl->min,gl->max,seq->dc->ncol_im,gl->scl_mm,gl->lev_mm);
         gl->mm_done = True ;

         RETURN( False );  /* continue next time on global histogramming */
      }

      /* if here, image nn has yet to be done for local statistics */

#if 0
      im = (MRI_IMAGE *) seq->getim( nn , isqCR_getimage , seq->getaux ) ;
#else
      AFNI_CALL_VALU_3ARG( seq->getim , MRI_IMAGE *,im ,
                           int,nn , int,isqCR_getimage , XtPointer,seq->getaux ) ;
#endif
      if( im != NULL ){
        ISQ_statify_one( seq , nn , im ) ; KILL_1MRI(im) ;
      }
      RETURN( False );   /* continue next time on next un-statted image */
   }

   /* all individual statistics are done --> global histogramming  */
   /* (note only images in the "series" are used for this purpose) */

   if( ! gl->per_done ){  /* global statistics not marked as done */

      for( nn=0 ; nn < nser ; nn++ )
         if( ! seq->imstat[nn].glob_done ) break ;

      if( nn >= nser ){ /* all were done, so finish them off */

         ISQ_perpoints( gl->min,gl->max,gl->hist ,
                        &(gl->per02) , &(gl->per98) ) ;

         ISQ_SCLEV( gl->per02 , gl->per98 ,
                    seq->dc->ncol_im , gl->scl_per , gl->lev_per ) ;

         gl->per_done = True ;

         RETURN( True );  /* don't need to do any more statistics! */
      }

      /* if here, image nn has yet to be done for global histogram */

#if 0
      im = (MRI_IMAGE *) seq->getim( nn , isqCR_getimage , seq->getaux ) ;
#else
      AFNI_CALL_VALU_3ARG( seq->getim , MRI_IMAGE *,im ,
                           int,nn , int,isqCR_getimage , XtPointer,seq->getaux ) ;
#endif
      if( im != NULL ){
         ISQ_statify_one( seq , nn , im ) ; KILL_1MRI(im) ;
      }
      RETURN( False );   /* continue next time on next un-statted image */
   }

   /* shouldn't get here, but if do, print a message and stop work process */

   fprintf(stderr,"\a\n*** imseq work process error!\n") ;
   RETURN( True );
}

/*-----------------------------------------------------------------------
   collect statistics on an image and put into location n in table
-------------------------------------------------------------------------*/

void ISQ_statify_one( MCW_imseq * seq , int n , MRI_IMAGE * im )
{
   ISQ_indiv_statistics * st ;
   ISQ_glob_statistics *  gl ;
   static int hist[NHISTOG] ; /* static to avoid create/destroy overhead */

ENTRY("ISQ_statify_one") ;

   /* exit if bad data */

   if( ! ISQ_VALID(seq) || n < 0 || n >= seq->status->num_total ) EXRETURN ;

   st = &( seq->imstat[n] ) ;
   gl = seq->glstat ;

   if( im->kind == MRI_rgb ) EXRETURN ;  /* 11 Feb 1999 */

   if( ! st->one_done ){  /* must do individual statistics */

      st->min = mri_min( im ) ;
      st->max = mri_max( im ) ;

      ISQ_SCLEV( st->min , st->max ,
                 seq->dc->ncol_im , st->scl_mm , st->lev_mm ) ;

      mri_histogram( im , st->min , st->max , True , NHISTOG,hist ) ;

      ISQ_perpoints( st->min,st->max,hist , &(st->per02) , &(st->per98) ) ;

      ISQ_SCLEV( st->per02 , st->per98 ,
                 seq->dc->ncol_im , st->scl_per , st->lev_per ) ;

      /* 12 Jan 2004: compute entropy in bits/byte */

      switch( im->kind ){
        default:        st->entropy =        mri_entropy8(im) ; break;
        case MRI_short:
        case MRI_float: st->entropy = 0.5l * mri_entropy16(im); break;
      }

      st->one_done = True ;

   } else if( n < seq->status->num_series &&
              ! st->glob_done               ){  /* do global */

      mri_histogram( im , gl->min , gl->max , False , NHISTOG , gl->hist ) ;
      st->glob_done = True ;
   }

   EXRETURN ;
}

/*-----------------------------------------------------------------------*/

void ISQ_perpoints( float bot , float top ,
                    int hist[] , float * per02 , float * per98 )
{
   register int ih , nsum , ns02 , ns98 ;
   float prev , cur , frac , dbin ;
   static int hcum[NHISTOG] ;  /* static to avoid create-destroy overhead */

ENTRY("ISQ_perpoints") ;

   nsum = 0 ;
   for( ih=0 ; ih < NHISTOG ; ih++ ) hcum[ih] = nsum += hist[ih] ;

   ns02 = 0.02 * nsum ;  /* here is where 2% and 98% are fixed */
   ns98 = 0.98 * nsum ;
   dbin = (top-bot) / NHISTOG ;

   /*-------*/

   for( ih=0 ; ih < NHISTOG ; ih++ ) if( hcum[ih] >= ns02 ) break ;

   if( ih == NHISTOG ) ih-- ;

   prev   = (ih == 0) ? (0.0) : hcum[ih-1] ;
   cur    = hcum[ih] ; if( cur <= prev ) cur = 1.01 * prev + 1.0 ;
   frac   = ih + (ns02-prev)/(cur-prev) ;
   *per02 = bot + dbin * frac ;

   if( *per02 < bot ) *per02 = bot ;

   /*-------*/

   for( ; ih < NHISTOG ; ih++ ) if( hcum[ih] >= ns98 ) break ;

   if( ih == NHISTOG ) ih-- ;

   prev   = (ih == 0) ? (0.0) : hcum[ih-1] ;
   cur    = hcum[ih] ; if( cur <= prev ) cur = 1.01 * prev + 1.0 ;
   frac   = ih + (ns98-prev)/(cur-prev) ;
   *per98 = bot + dbin * frac ;

   if( *per98 > top ) *per98 = top ;

   EXRETURN ;
}

/*------------------------------------------------------------------------
   change the palette based on the arrow actions
--------------------------------------------------------------------------*/

void ISQ_arrow_CB( MCW_arrowval * av , XtPointer client_data )
{
   MCW_imseq * seq = (MCW_imseq *) client_data ;
   int ddd ;

ENTRY("ISQ_arrow_CB") ;

   if( ! ISQ_REALZ(seq) ) EXRETURN ;

   if( av->fval > av->old_fval ) ddd = -1 ;
   else                          ddd =  1 ;

/*
   ddd = (av->fval > av->old_fval) ? (-1) : (1) ;
*/

   if( av == seq->arrow[NARR_SQUEEZE] ){
           DC_palette_squeeze( seq->dc , ddd ) ;
           COLORMAP_CHANGE(seq) ;      /* 22 Aug 1998 */

   } else if( av == seq->arrow[NARR_BRIGHT]  ){
           DC_palette_bright(  seq->dc , ddd ) ;
           COLORMAP_CHANGE(seq) ;      /* 22 Aug 1998 */

   } else if( av == seq->arrow[NARR_ROTATE]  ){
           DC_palette_rotate(  seq->dc ,-ddd ) ;
           COLORMAP_CHANGE(seq) ;      /* 22 Aug 1998 */

   } else if( av == seq->arrow[NARR_GAMMA]   ){
           if( seq->imim == NULL || seq->imim->kind != MRI_rgb ){
             double new_gamma = seq->dc->gamma ;
             if( ddd > 0 ) new_gamma *= 0.95 ;
             else          new_gamma /= 0.95 ;
             DC_palette_restore( seq->dc , new_gamma ) ;
             COLORMAP_CHANGE(seq) ;      /* 22 Aug 1998 */

           } else {   /* 25 Apr 2005: delta gamma on RGB images */
             if( ddd > 0 ) seq->rgb_gamma *= 0.95 ;
             else          seq->rgb_gamma /= 0.95 ;
             ISQ_redisplay( seq , -1 , isqDR_reimage ) ;
           }

   } else if( av == seq->arrow[NARR_FRAC]  ){  /* 25 Oct 1996 */
      float nfrac = seq->image_frac ;

      nfrac += (ddd < 0) ? DFRAC : -DFRAC ;

      if( nfrac >= FRAC_MIN && nfrac <= FRAC_MAX ){
         seq->image_frac = nfrac ;

         XtVaSetValues( seq->wimage ,
                          XmNrightPosition ,(int)(0.49 + nfrac * FORM_FRAC_BASE),
                          XmNbottomPosition,(int)(0.49 + nfrac * FORM_FRAC_BASE),
                        NULL ) ;
         XtVaSetValues( seq->wscale ,
                          XmNrightPosition ,(int)(0.49 + nfrac * FORM_FRAC_BASE),
                        NULL ) ;
         XtVaSetValues( seq->wbar ,
                          XmNbottomPosition,(int)(0.49 + nfrac * FORM_FRAC_BASE),
                        NULL ) ;
         XtVaSetValues( seq->winfo ,
                          XmNrightPosition ,(int)(0.49 + nfrac * FORM_FRAC_BASE),
                        NULL ) ;
      } else {
         XBell( seq->dc->display , 100 ) ;
      }
   }

   ISQ_but_done_reset( seq ) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------
   Norm button callback: normalize the palette
-------------------------------------------------------------------------*/

void ISQ_but_cnorm_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   MCW_imseq *seq = (MCW_imseq *) client_data ;

ENTRY("ISQ_but_cnorm_CB") ;

   if( ! ISQ_REALZ(seq) ) EXRETURN ;

   DC_palette_restore( seq->dc , 0.0 ) ;
   seq->rgb_gamma  = 1.0 ;     /* 25 Apr 2005 */
   seq->rgb_offset = 0.0 ;
   COLORMAP_CHANGE(seq) ;      /* 22 Aug 1998 */
   ISQ_but_done_reset( seq ) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------
   External interface to drive an MCW_imseq:
     seq        = pointer to structure returned by open_MCW_imseq
     drive_code = integer indicating which action to take
     drive_data = data or pointer to data controlling action
                  (you will probably have to cast this to
                   XtPointer to avoid ugly warnings from the compiler)

     drive_code       drive_data should be
     ----------       --------------------
*    isqDR_imhelptext (char *) with new help string for image window
                        N.B.: this string is copied and so may be
                              deleted after the call if you like

*    isqDR_options    (ISQ_options *) with new options for display

*    isqDR_numtotal   (int) with new number of images available;
                        WARNING: you cannot reduce numtotal above
                                 the value num_series in the "status"

*    isqDR_cursor     (int) with new cursor id for the image window;
                       (if negative, means from cursorfont)

*    isqDR_unrealize  (ignored) the viewer is unrealized [hidden], but
                        not destroyed

*    isqDR_realize    (ignored) an unrealized viewer is re-realized

*    isqDR_display    (int) call this int "n":
                         n <  0  ->  re-get current image and overlay
                         n >= 0  ->  move to image # n

*    isqDR_overlay    (int) call this int "n"
                         if n == current image, just re-get overlay
                         otherwise, move to image # n
                         (setting n=-1 is the same as n=current image)

*    isqDR_destroy    (ignored) destroy this MCW_imseq, and delete its
                        own XtMalloc-ed internal data structures;
                        after this call, you must myXtFree(seq) to finish
                        the job.

*    isqDR_arrowpadon (char *) with help string for arrowpad;
                        this call turns the arrowpad on; after this, it
                        will be visible and send callbacks

*    isqDR_arrowpadoff (ignored) turn the arrowpad off

*    isqDR_newseq     (XtPointer) contains new auxiliary data for getim;
                        this call switches the image sequence to a
                        new one entirely;  this call should be followed
                        immediately by one to display the desired
                        image from the new sequence!

*    isqDR_title      (char *) contains new string for window title bar

*    isqDR_clearstat  (ignored) clears the statistics saved for the
                        sequence of images;  usually used for a change
                        in the way the images are rendered

*    isqDR_onoffwid   (int) if 0, turns non-image widgets "off"  (0=isqDR_offwid)
                            if 1, turns non-image widgets "on"   (1=isqDR_onwid)
                            if 2, toggles their current state    (2=isqDR_togwid)

*    isqDR_getimnr    (int *) returns the current image index in the sequence
                        in the location pointed to by this argument

*    isqDR_icon       (Pixmap) sets the icon for this window

*    isqDR_bgicon     (Pixmap) sets the background for this window

*    isqDR_sendmontage (ignored) tells the MCW_imseq to send the
                         montage information back via isqCR_newmontage

*    isqDR_periodicmont (int) tells whether to use periodic montages

*    isqDR_button2_enable  (ignored) tells to enable processing of Button2 events
*    isqDR_button2_disable (ignored) tells to disable such processing
*    isqDR_button2_pixel   (Pixel)   use argument for button2 drawing color
*    isqDR_button2_mode    (int)     tells how to draw; the argument is
                              BUTTON2_OPENPOLY   == open polygon
                              BUTTON2_CLOSEDPOLY == closed polygon
                              BUTTON2_POINTS     == only draw points
                              BUTTON2_NODRAW     == don't draw anything
*    isqDR_button2_width   (int) tells width of lines to draw in button2 mode

*    isqDR_rebar           (ignored) erase the color bar and show it again

*    isqDR_winfotext       (char *) sets the winfo extra text

*    isqDR_winfosides      (char **) sets the winfo_sides text
*    isqDR_winfoprefix     (char *) set the winfo_prefix text

*    isqDR_getoptions      (ISQ_options *) to get the current options

*    isqDR_setmontage      (int *) sets the montage parameters
                            [0] = nx  [1] = ny  [2] = spacing
                            [3] = gap [4] = gap_color (overlay index)

*    isqDR_setifrac        (float *) sets the image fraction
                             between FRAC_MIN and FRAC_MAX

*    isqDR_opacitybut      (int) turns opacity control on/off

*    isqDR_setopacity      (int) sets opacity (value=0..9)
*    isqDR_getopacity      (int *) get opacity setting

*    isqDR_zoombut         (int) turns zoom control on/off
*    isqDR_penbbox         (int) turns pen bbox on/off

*    isqDR_record_mode     (ignored)
                           makes this an image recorder (irreversibly)

*    isqDR_record_disable  (ignored)
                           disables the Rec button (irreversibly)

*    isqDR_plot_label      (int)
                           0..6 for label position; -1=toggle widgets

*    isqDR_plot_plot       (int)
                           1=show overlay plot; 0=don't; -1=toggle widget

*    isqDR_ignore_redraws  (int)
                           1=ignore redraw commands
                           0=don't ignore redraw commands

*    isqDR_setimsave       (char *) suffix of image save mode

*    isqDR_setrange        (float *) points to rng_bot,rng_top
*    isqDR_settopclip      (float *) points to top_clip

*    isqDR_keypress        (unsigned int) character or KeySym to send

*    isqDR_save_jpeg       (char *) save current image to this filename
*    isqDR_save_png        (char *) save current image to this filename
*    isqDR_save_raw        (char *) save current image to this filename
*    isqDR_save_rawmont    (char *) save current montage to this filename
*    isqDR_save_filtered   (char *) save current image to this filter
*    isqDR_save_agif       (char *) save current image series to this filename
*    isqDR_save_mpeg       (char *) save current image series to this filename
*    isqDR_save_jpegall    (char *) save current image series to bunch of files
*    isqDR_save_pngall     (char *) save current image series to bunch of files

*    isqDR_get_crop        (int *) 4 ints that specify current crop status
*    isqDR_set_crop        (int *) 4 ints to change current crop status

The Boolean return value is True for success, False for failure.
-------------------------------------------------------------------------*/

Boolean drive_MCW_imseq( MCW_imseq * seq ,
                         int drive_code , XtPointer drive_data )
{
ENTRY("drive_MCW_imseq") ;
   if( ! ISQ_VALID(seq) ) RETURN( False );

   switch( drive_code ){

      /*------- error! -------*/

      default:{
         fprintf(stderr,"\a\n*** drive_MCW_imseq: code=%d illegal!\n",
                 drive_code) ;
         XBell( seq->dc->display , 100 ) ;
         RETURN( False );
      }
      break ;

      /*--------- set top_clip [14 Sep 2007] ----------*/

      case isqDR_settopclip:{
        float *tc=(float *)drive_data ; int zz=0 ;
        if( tc == NULL ){
          seq->top_clip = 0.0f ; seq->redo_clip = 0 ;
        } else {
          seq->top_clip = *tc ; zz = (seq->top_clip > 0.0f) ;
        }
#if 0
printf("set top_clip=%g  redo_clip=%d zz=%d\n",seq->top_clip,seq->redo_clip,zz);
#endif
        ALLOW_CLIPPING( seq , zz ) ;
        if( tc == NULL ) ISQ_redisplay( seq , -1 , isqDR_display ) ;
        RETURN( True ) ;
      }
      break ;

      /*--------- set display range [04 Nov 2003] ----------*/

      case isqDR_setrange:{
        float *rng = (float *)drive_data ;
        if( rng == NULL ){
          seq->rng_bot = seq->rng_top = seq->rng_ztop = 0.0f ;
        } else {
          seq->rng_bot = rng[0] ; seq->rng_top = rng[1] ; seq->rng_ztop = 0.0 ;
          seq->rng_extern = 1 ;
        }
        if( rng == NULL || rng[2] == 0.0f )
          ISQ_redisplay( seq , -1 , isqDR_display ) ;
        RETURN( True ) ;
      }
      break ;

      /*--------- save image type? [23 Jan 2003] ----------*/

      case isqDR_setimsave:{
        char *suf = (char *)drive_data ;
        int ii ;
        if( suf == NULL || *suf == '\0' || ppmto_num < 1 ) RETURN(False) ;
        for( ii=0 ; ii < ppmto_num ; ii++ ){
          if( strcmp(suf  ,ppmto_suffix[ii]) == 0 ) break ;
          if( strcmp(suf+1,ppmto_suffix[ii]) == 0 ) break ;
        }
        if( ii == ppmto_num ) RETURN(False) ;
        seq->opt.save_filter = ii ;
        SET_SAVE_LABEL(seq) ;
        if( seq->num_bbox > 0 && seq->bbox[NTOG_SAV] != NULL )
          MCW_set_bbox( seq->bbox[NTOG_SAV] , ppmto_bval[ii] ) ;
        RETURN( True ) ;
      }
      break ;

      /*--------- ignore redraws? [16 Aug 2002] -------------*/

      case isqDR_ignore_redraws:{
         int dd = (int)drive_data ;
         seq->ignore_redraws = dd ;
         RETURN( True ) ;
      }
      break ;

      /*--------- overlay plot stuff [20 Sep 2001] ----------*/

      case isqDR_plot_label:{
         int dd = (int)drive_data ;

         if( dd < 0 ){
            INVERT_manage( seq->wbar_label_av->wrowcol ) ;
            INVERT_manage( seq->wbar_labsz_av->wrowcol ) ;
         } else if( dd != seq->wbar_label_av->ival && dd >= 0 && dd <= 4 ){
           AV_assign_ival( seq->wbar_label_av , dd ) ;
           ISQ_redisplay( seq , -1 , isqDR_display ) ;
         }
         RETURN( True ) ;
      }

      /*.....................................................*/

      case isqDR_save_jpeg:{                 /* 28 Jul 2005 */
        char *fname = (char *)drive_data ;
        ISQ_save_jpeg( seq , fname ) ;
        RETURN( True ) ;
      }

      case isqDR_save_png:{                  /* 11 Dec 2006 */
        char *fname = (char *)drive_data ;
        ISQ_save_png( seq , fname ) ;
        RETURN( True ) ;
      }

      case isqDR_save_raw:{                  /* 13 Nov 2007 */
        char *fname = (char *)drive_data ;
        ISQ_save_raw( seq , fname ) ;
        RETURN( True ) ;
      }

      case isqDR_save_rawmont:{              /* 13 Nov 2007 */
        char *fname = (char *)drive_data ;
        ISQ_save_rawmont( seq , fname ) ;
        RETURN( True ) ;
      }

      case isqDR_save_filtered:{             /* 14 Dec 2006 */
        char *fname = (char *)drive_data ;
        ISQ_save_image( seq , NULL , fname , NULL ) ;
        RETURN( True ) ;
      }

      case isqDR_save_agif:{
        char *fname = (char *)drive_data ;
        ISQ_save_anim( seq , fname , 0,0, AGIF_MODE ) ;
        RETURN(True) ;
      }

      case isqDR_save_mpeg:{
        char *fname = (char *)drive_data ;
        ISQ_save_anim( seq , fname , 0,0, MPEG_MODE ) ;
        RETURN(True) ;
      }

      case isqDR_save_jpegall:{
        char *fname = (char *)drive_data ;
        ISQ_save_anim( seq , fname , 0,0, JPEG_MODE ) ;
        RETURN(True) ;
      }

      case isqDR_save_pngall:{
        char *fname = (char *)drive_data ;
        ISQ_save_anim( seq , fname , 0,0, PNG_MODE ) ;
        RETURN(True) ;
      }

      /*.....................................................*/

      case isqDR_plot_plot:{
         int dd = (int)drive_data ;

         if( dd < 0 ){
            INVERT_manage( seq->wbar_plots_bbox->wrowcol ) ;
         } else {
            dd = (dd != 0) ;
            if( dd != MCW_val_bbox(seq->wbar_plots_bbox) ){
               MCW_set_bbox( seq->wbar_plots_bbox , dd ) ;
               ISQ_redisplay( seq , -1 , isqDR_display ) ;
            }
         }
         RETURN( True ) ;
      }

      /*--------- record off forever [24 Apr 2001] ----------*/

      case isqDR_record_disable:{
         ISQ_remove_widget( seq , seq->record_rc ) ;
         seq->record_status = RECORD_STATUS_OFF ;
         RETURN( True ) ;
      }
      break ;

      /*--------- record mode [24 Apr 2001] ----------*/

      case isqDR_record_mode:{
         int ii ;
         static Pixmap record_pixmap = XmUNSPECIFIED_PIXMAP ;
#define record_width 64
#define record_height 32
static unsigned char record_bits[] = {
   0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x01, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x81, 0x0f, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x81, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x81, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x81, 0x40, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x02, 0x81, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02,
   0x81, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02, 0x81, 0x40, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x02, 0x81, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02,
   0x81, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02, 0x81, 0x10, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x02, 0x81, 0x1f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02,
   0x81, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02, 0x81, 0x10, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x02, 0x81, 0x10, 0x70, 0x00, 0xe0, 0xf0, 0x01, 0x02,
   0x81, 0x20, 0x8c, 0xf0, 0x10, 0x21, 0xe2, 0x03, 0x81, 0x20, 0x02, 0x09,
   0x09, 0x22, 0x12, 0x02, 0x81, 0x20, 0x02, 0x09, 0x08, 0x22, 0x10, 0x02,
   0x81, 0x20, 0xfe, 0x09, 0x08, 0x22, 0x10, 0x02, 0x81, 0x40, 0x02, 0x08,
   0x08, 0x22, 0x10, 0x02, 0x81, 0x40, 0x02, 0x08, 0x08, 0x22, 0x10, 0x02,
   0x81, 0x40, 0x02, 0x08, 0x08, 0x22, 0x10, 0x02, 0x81, 0x40, 0x02, 0x08,
   0x08, 0x22, 0x10, 0x02, 0x81, 0x40, 0x04, 0x11, 0x11, 0x21, 0x20, 0x02,
   0x81, 0x40, 0xf8, 0xe0, 0xe0, 0x20, 0xc0, 0x01, 0x01, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00};

         if( seq->record_mode ) RETURN( False ) ;  /* already on */
         seq->record_mode = 1 ;
         seq->cropit = seq->crop_allowed = 0 ;     /* 12 Jun 2002 */

         /* create background pixmap */

         if( record_pixmap == XmUNSPECIFIED_PIXMAP )
            record_pixmap = XCreatePixmapFromBitmapData(
                              seq->dc->display ,
                              RootWindowOfScreen(seq->dc->screen) ,
                              (char *)record_bits, record_width, record_height ,
                              seq->dc->ovc->pixov_brightest ,
                              seq->dc->ovc->pixov_darkest ,
                              DefaultDepthOfScreen(seq->dc->screen) ) ;

         XtVaSetValues( seq->wform, XmNbackgroundPixmap, record_pixmap, NULL ) ;

         /* disable various widgets */

         ISQ_remove_widget( seq , seq->wbut_bot[NBUT_MONT] ) ;
         ISQ_remove_widget( seq , seq->record_rc ) ;
         for( ii=0 ; ii < NBUTTON_RIG ; ii++)
            ISQ_remove_widget( seq , seq->wbut_rig[ii] ) ;
         for( ii=0 ; ii < NARROW-1 ; ii++ ) /* keep "i" arrow */
            ISQ_remove_widget( seq , seq->arrow[ii]->wrowcol ) ;
         if( seq->ov_opacity_av != NULL ){
            ISQ_remove_widget( seq , seq->ov_opacity_sep ) ;
            ISQ_remove_widget( seq , seq->ov_opacity_av->wrowcol ) ;
         }

         ISQ_remove_widget( seq , seq->zoom_sep ) ;
         ISQ_remove_widget( seq , seq->zoom_val_av->wrowcol ) ;
         ISQ_remove_widget( seq , seq->zoom_drag_pb ) ;
         ISQ_remove_widget( seq , seq->crop_drag_pb ) ;
         ISQ_remove_widget( seq , seq->pen_bbox->wrowcol ) ;

         ISQ_remove_widget( seq , seq->arrowpad->wform ) ;
         ISQ_remove_widget( seq , seq->wbar ) ;
         ISQ_remove_widget( seq , seq->winfo ) ;

         /* change to Save:bkg */

         seq->opt.save_one    = 0 ;
         seq->opt.save_agif   = 0 ;   /* 27 Jul 2001 */
         seq->opt.save_mpeg   = 0 ;
         seq->opt.save_pnm    = 0 ;
         seq->opt.save_filter = -1 ;  /* 27 Jun 2001 */
         SET_SAVE_LABEL(seq) ;
         drive_MCW_imseq( seq , isqDR_setimsave ,
                          (XtPointer)getenv("AFNI_DEFAULT_IMSAVE") ) ;

         /* 27 Jun 2001: change help on Save: */

         MCW_unregister_help( seq->wbut_bot[NBUT_DISP] ) ;
         if( ppmto_num > 0 ){
           MCW_register_help( seq->wbut_bot[NBUT_SAVE] ,
                                "Save controls:\n"
                                " Press with Button 1 (left) to save images\n"
                                " Press with Button 3 (right) to change the\n"
                                "   format of the saved images"
                            ) ;
           MCW_register_hint( seq->wbut_bot[NBUT_SAVE] ,
                              "Button 3 => change save format" ) ;
         } else {
           MCW_register_help( seq->wbut_bot[NBUT_SAVE] ,
                                "Save controls:\n"
                                " Press with Button 1 (left) to\n"
                                " in the PNM format save images"  ) ;
           MCW_register_hint( seq->wbut_bot[NBUT_SAVE] ,
                              "Save images as PNM" ) ;
         }

         /* change Disp to Kill */

         XtRemoveCallback( seq->wbut_bot[NBUT_DISP] , XmNactivateCallback ,
                           ISQ_but_bot_def[NBUT_DISP].func_CB , seq        ) ;

         XtAddCallback( seq->wbut_bot[NBUT_DISP] , XmNactivateCallback ,
                        ISQ_record_kill_CB , seq                       ) ;

         MCW_set_widget_label( seq->wbut_bot[NBUT_DISP] , "Kill" ) ;
         MCW_unregister_help( seq->wbut_bot[NBUT_DISP] ) ;
         MCW_register_hint( seq->wbut_bot[NBUT_DISP] , "Erase current image" ) ;
         MCW_register_help( seq->wbut_bot[NBUT_DISP] ,
                            "Erase the current image in the recorded sequence.\n"
                            "If not later overwritten, this image will NOT\n"
                            "be saved when you use Save:bkg to write the image\n"
                            "sequence to disk.\n"
                           ) ;

         /* attach Done to Save (since Mont is now hidden) */

         XtVaSetValues( seq->wbut_bot[NBUT_DONE] ,
                           LEADING_BOT       , XmATTACH_WIDGET          ,
                           LEADING_WIDGET_BOT, seq->wbut_bot[NBUT_SAVE] ,
                        NULL ) ;

         /* Miscellaneous stuff */

         XtVaSetValues( seq->wtop , XmNtitle , "Image Recorder" , NULL ) ;
         if( MCW_isitmwm( seq->wtop ) )
            XtVaSetValues( seq->wtop ,
                            XmNmwmDecorations, MWM_DECOR_ALL | MWM_DECOR_MAXIMIZE,
                           NULL ) ;

         RETURN( True ) ;
      }
      break ;

      /*--------- opacity button [07 Mar 2001] ----------*/

      case isqDR_opacitybut:{
         int val = (int) drive_data ;
         if( seq->ov_opacity_av == NULL ) RETURN( False ) ;
         if( val == 0 ){
            XtUnmanageChild( seq->ov_opacity_sep ) ;
            XtUnmanageChild( seq->ov_opacity_av->wrowcol ) ;
         }
         else {
            XtManageChild( seq->ov_opacity_sep ) ;
            XtManageChild( seq->ov_opacity_av->wrowcol ) ;
         }
         RETURN( True ) ;
      }
      break ;

      /*--------- set opacity value [21 Jan 2003] ----------*/

      case isqDR_setopacity:{
        int val = (int) drive_data ;
        if( seq->ov_opacity_av == NULL ) RETURN( False ) ;
        if( val < OPACITY_BOT || val > OPACITY_TOP ) RETURN( False ) ;
        AV_assign_ival( seq->ov_opacity_av , val ) ;
        ISQ_opacity_CB( seq->ov_opacity_av , seq ) ;
        RETURN( True ) ;
      }
      break ;

      /*--------- get opacity value [21 Jan 2003] ----------*/

      case isqDR_getopacity:{
        int *val = (int *) drive_data ;
        if( seq->ov_opacity_av == NULL || val == NULL ) RETURN( False ) ;
        *val = seq->ov_opacity_av->ival ;
        RETURN( True ) ;
      }
      break ;

      /*--------- get crop data [03 May 2007] -----------*/

      case isqDR_get_crop:{
        int *iar = (int *)drive_data ;
        if( iar != NULL ){
          if( !seq->cropit ){
            iar[0] = iar[1] = iar[2] = iar[3] = -1 ;
          } else {
            iar[0] = seq->crop_xa ; iar[1] = seq->crop_xb ;
            iar[2] = seq->crop_ya ; iar[3] = seq->crop_yb ;
          }
        }
        RETURN(True) ;
      }
      break ;

      /*--------- set crop data [03 May 2007] -----------*/

      case isqDR_set_crop:{
        int *iar = (int *)drive_data ;
        if( iar == NULL              ||
            iar[0] < 0               || iar[2] < 0               ||
            iar[0]+MINCROP >= iar[1] || iar[2]+MINCROP >= iar[3]   ){
          seq->cropit = 0 ;
        } else {
          seq->cropit = 1 ;
          seq->crop_xa = iar[0] ; seq->crop_xb = iar[1] ;
          seq->crop_ya = iar[2] ; seq->crop_yb = iar[3] ;
          if( seq->crop_nxorg > 0 && seq->crop_xb >= seq->crop_nxorg )
            seq->crop_xb = seq->crop_nxorg-1 ;
          if( seq->crop_nyorg > 0 && seq->crop_yb >= seq->crop_nyorg )
            seq->crop_yb = seq->crop_nyorg-1 ;
        }
        ISQ_redisplay( seq , -1 , isqDR_display ) ;
        RETURN(True) ;
      }
      break ;

      /*--------- zoom buttons [11 Mar 2002] ----------*/

      case isqDR_zoombut:{
         int val = (int) drive_data ;
         if( val == 0 ){
            XtUnmanageChild( seq->zoom_sep ) ;
            XtUnmanageChild( seq->zoom_val_av->wrowcol ) ;
            XtUnmanageChild( seq->zoom_drag_pb ) ;
            XtUnmanageChild( seq->crop_drag_pb ) ;
         } else {
            XtManageChild( seq->zoom_sep ) ;
            XtManageChild( seq->zoom_val_av->wrowcol ) ;
            XtManageChild( seq->zoom_drag_pb ) ;
            XtManageChild( seq->crop_drag_pb ) ;
         }
         RETURN( True ) ;
      }
      break ;

      /*--------- pen bbox [18 Jul 2003] ----------*/

      case isqDR_penbbox:{
         int val = (int) drive_data ;
         if( val == 0 )
           XtUnmanageChild( seq->pen_bbox->wrowcol ) ;
         else
           XtManageChild( seq->pen_bbox->wrowcol ) ;
         RETURN( True ) ;
      }
      break ;

      /*--------- set montage [22 Sep 2000] ----------*/
      /* [mostly copied from ISQ_montage_action_CB()] */

      case isqDR_setmontage:{
         int *mm = (int *) drive_data ;

         if( mm == NULL )                     RETURN( False );  /* sanity */
         if( mm[0] < 1 || mm[0] > MONT_NMAX ) RETURN( False );  /* checks */
         if( mm[1] < 1 || mm[1] > MONT_NMAX ) RETURN( False );

         seq->mont_nx_old       = seq->mont_nx       ;  /* save */
         seq->mont_ny_old       = seq->mont_ny       ;
         seq->mont_skip_old     = seq->mont_skip     ;
         seq->mont_gap_old      = seq->mont_gap      ;
         seq->mont_gapcolor_old = seq->mont_gapcolor ;

         /* set new values, if legal */

         seq->mont_nx = mm[0] ;
         seq->mont_ny = mm[1] ;
         if( mm[2] >  0 && mm[2] <= MONT_SMAX ) seq->mont_skip     = mm[2]-1 ;
         if( mm[3] >= 0 && mm[3] <= MONT_GMAX ) seq->mont_gap      = mm[3] ;
         if( mm[4] >= 0 &&
             mm[4] <= seq->dc->ovc->ncol_ov-1 ) seq->mont_gapcolor = mm[4] ;

         /* set Save One */

         if( seq->mont_nx * seq->mont_ny > 1 && !seq->opt.save_one ){
            seq->opt.save_one  = 1 ;
            seq->opt.save_agif = 0 ; /* 27 Jul 2001 */
            seq->opt.save_mpeg = 0 ;
            SET_SAVE_LABEL(seq) ;
         }

         /* now do the redisplay */

         ISQ_redisplay( seq , -1 , isqDR_display ) ;    /* local redraw */

         if( seq->status->send_CB != NULL ){  /* tell AFNI */

            ISQ_cbs cbs ;
            THD_ivec3 minf ;
            int ijcen = (seq->mont_nx)/2 + (seq->mont_ny/2) * seq->mont_nx ,
                nmont = seq->mont_nx * seq->mont_ny ;

            minf.ijk[0]  = ijcen ;            /* number of slices before center */
            minf.ijk[1]  = nmont-ijcen-1 ;    /* number after */
            minf.ijk[2]  = seq->mont_skip ;   /* number between slices */
            cbs.reason   = isqCR_newmontage ;
            cbs.userdata = (XtPointer) &minf ;

            seq->ignore_redraws = 1 ;         /* don't listen to redraws */
#if 0
            seq->status->send_CB( seq , seq->getaux , &cbs ) ;
#else
            SEND(seq,cbs) ;
#endif
            seq->ignore_redraws = 0 ;         /* can listen again */
         }

#if 0
         ISQ_redisplay( seq , -1 , isqDR_display ) ;    /* local redraw */
#endif

         RETURN( True );
      }
      break ;

      /*------- winfo_prefix text [10 Dec 2007] -------*/

      case isqDR_winfoprefix:{
        char *pf=(char *)drive_data , lab[2]=" " ;
        if( pf == NULL || *pf == '\0' ){
          seq->winfo_prefix[0] = '\0' ;
        } else {
          strncpy( seq->winfo_prefix , pf , 15 ) ;
          seq->winfo_prefix[15] = '\0' ;
          if( isgraph(*pf) ) lab[0]=*pf ;
        }
        MCW_set_widget_label( seq->arrowpad->wbut[4] , lab ) ;
        seq->im_label[0] = '\0' ;  /* will force redraw */
        ISQ_draw_winfo( seq ) ;
        RETURN( True );
      }

      /*------- winfo_sides text [01 Dec 1999] -------*/

      case isqDR_winfosides:{
         char **ws = (char **) drive_data ;
         int iw ;

         if( ws == NULL ){                   /* remove the label data */
           seq->winfo_sides[0][0] =
            seq->winfo_sides[1][0] =
             seq->winfo_sides[2][0] =
              seq->winfo_sides[3][0] = '\0' ;

         } else {                           /* change the label data */
           for( iw=0 ; iw < 4 ; iw++ ){
             if( ws[iw] == NULL || ws[iw][0] == '\0' ){
               seq->winfo_sides[iw][0] = '\0' ;
             } else {
               strncpy( seq->winfo_sides[iw] , ws[iw] , 15 ) ;
               seq->winfo_sides[iw][15] = '\0' ;
             }
           }
         }
         seq->im_label[0] = '\0' ;  /* will force redraw */
         ISQ_draw_winfo( seq ) ;

         if( ws == NULL )                       /* 18 May 2005: add a hint */
           MCW_unregister_hint( seq->winfo ) ;  /* for the clueless newbie */
         else
           MCW_register_hint( seq->winfo ,
                    "setenv AFNI_LEFT_IS_LEFT YES disables 'radiology mode'" );

         RETURN( True );
      }

      /*------- setifrac [22 Sep 2000] -------*/
      /* [mostly copied from ISQ_arrow_CB()]  */

      case isqDR_setifrac:{
         float * ff = (float *) drive_data ;

         if( ff == NULL || *ff < FRAC_MIN || *ff > 1.0 ) RETURN( False );

         if( *ff <= FRAC_MAX ){ /* from ISQ_arrow_CB() */
            float nfrac = *ff ;
            seq->image_frac = nfrac ;

            if( !seq->onoff_state )  /* turn widgets on first, recursively */
               drive_MCW_imseq( seq,isqDR_onoffwid,(XtPointer)isqDR_onwid );

            XtVaSetValues( seq->wimage ,
                             XmNrightPosition ,(int)(0.49+nfrac*FORM_FRAC_BASE),
                             XmNbottomPosition,(int)(0.49+nfrac*FORM_FRAC_BASE),
                           NULL ) ;
            XtVaSetValues( seq->wscale ,
                             XmNrightPosition ,(int)(0.49+nfrac*FORM_FRAC_BASE),
                           NULL ) ;
            XtVaSetValues( seq->wbar ,
                             XmNbottomPosition,(int)(0.49+nfrac*FORM_FRAC_BASE),
                           NULL ) ;
            XtVaSetValues( seq->winfo ,
                             XmNrightPosition ,(int)(0.49+nfrac*FORM_FRAC_BASE),
                           NULL ) ;

         } else if( seq->onoff_state ) {  /* turn widgets off */

            drive_MCW_imseq( seq,isqDR_onoffwid,(XtPointer)isqDR_offwid );

         }
         RETURN( True );
      }
      break ;

      /*------- send a simulated key press [18 Feb 2005] -------*/

      case isqDR_keypress:{
        unsigned int key = (unsigned int)drive_data ;
        (void )ISQ_handle_keypress( seq , key ) ;
        RETURN( True );
      }
      break ;

      /*------- winfo extra text [07 Aug 1999] -------*/

      case isqDR_winfotext:{
         char * wt = (char *) drive_data ;

         if( wt == NULL || wt[0] == '\0' ){
            seq->winfo_extra[0] = '\0' ;
         } else {
            strncpy( seq->winfo_extra , wt , 63 ) ;
            seq->winfo_extra[63] = '\0' ;
         }
         seq->im_label[0] = '\0' ;  /* will force redraw */
         ISQ_draw_winfo( seq ) ;
         RETURN( True );
      }

      /*------- button2 stuff -------*/

      case isqDR_button2_pixel:{
         seq->button2_pixel = (Pixel) drive_data ;
         RETURN( True );
      }

      case isqDR_button2_mode:{
         seq->button2_drawmode = (int) drive_data ;
         RETURN( True );
      }

      case isqDR_button2_width:{                  /* 08 Oct 2002 */
         seq->button2_width = (int) drive_data ;
         RETURN( True );
      }

      case isqDR_button2_enable:{
         ISQ_timer_stop(seq) ;
         if( seq->status->send_CB == NULL ) RETURN( False );  /* makes no sense */
         if( seq->button2_enabled )         RETURN( True );   /* already on */

         XtInsertEventHandler(
              seq->wimage ,         /* handle events in image */

               0
               | ButtonReleaseMask  /* button releases (only #2 is used) */
               | Button2MotionMask  /* motion while #2 is down */
              ,
              FALSE ,               /* nonmaskable events? */
              ISQ_button2_EV ,      /* handler routine */
              (XtPointer) seq ,     /* client data */
              XtListTail            /* last in queue */
         ) ;

         seq->button2_enabled = 1 ;
         seq->button2_active  = 0 ;

         XtManageChild( seq->pen_bbox->wrowcol ) ;
         RETURN( True );
      }

      case isqDR_button2_disable:{
         if( seq->status->send_CB == NULL ) RETURN( False );  /* makes no sense */
         if( !seq->button2_enabled )        RETURN( True );   /* already off */

         XtRemoveEventHandler(
              seq->wimage ,         /* unhandle events in image */

               0
               | ButtonReleaseMask  /* button releases (only #2 is used) */
               | Button2MotionMask  /* motion while #2 is down */
              ,
              TRUE ,                /* nonmaskable events? */
              ISQ_button2_EV ,      /* handler routine */
              (XtPointer) seq       /* client data */
         ) ;

         seq->button2_enabled = seq->button2_active = 0 ;
         ISQ_set_cursor_state( seq , CURSOR_NORMAL ) ;
         XtUnmanageChild( seq->pen_bbox->wrowcol ) ;
         RETURN( True );
      }

      /*------- montage stuff -------*/

      case isqDR_periodicmont:{
        int per = ((int) drive_data) != 0 ;

        if( per != seq->mont_periodic ){
           seq->mont_periodic = per ;
           if( ISQ_REALZ(seq) ) ISQ_redisplay( seq , -1 , isqDR_display ) ;
        }
        RETURN( True );
      }

      case isqDR_sendmontage:{
         if( seq->status->send_CB != NULL ){
            ISQ_cbs cbs ;
            THD_ivec3 minf ;
            int ijcen = (seq->mont_nx)/2 + (seq->mont_ny/2) * seq->mont_nx ,
                nmont = seq->mont_nx * seq->mont_ny ;

            minf.ijk[0]  = ijcen ;            /* number of slices before center */
            minf.ijk[1]  = nmont-ijcen-1 ;    /* number after */
            minf.ijk[2]  = seq->mont_skip ;   /* number between slices */
            cbs.reason   = isqCR_newmontage ;
            cbs.userdata = (XtPointer) &minf ;
#if 0
            seq->status->send_CB( seq , seq->getaux , &cbs ) ;
#else
            SEND(seq,cbs) ;
#endif
            RETURN( True );
         } else {
            RETURN( False );
         }
      }
      break ;

      /*------ set icon -----*/

      case isqDR_icon:{
         XtVaSetValues( seq->wtop, XmNiconPixmap,(Pixmap)drive_data , NULL ) ;
         RETURN( True );
      }
      break ;

      /*------ set background icon [28 Jan 2004] -------*/

      case isqDR_bgicon:{
        XtVaSetValues( seq->wform,
                         XmNbackgroundPixmap, (Pixmap)drive_data ,
                       NULL ) ;
        RETURN( True );
      }
      break ;

      /*------ get image number -----*/

      case isqDR_getimnr:{
         int * retval = (int *) drive_data ;

         if( retval != NULL ) *retval = seq->im_nr ;
         RETURN( True );
      }
      break ;

      /*------ widgets on or off -----*/

      case isqDR_onoffwid:{
         int mode = (int) drive_data , turn_on ;
         int ww , hh ;

         switch( mode ){
            default:
            case isqDR_togwid:  turn_on = ! seq->onoff_state ; break ;
            case isqDR_onwid:   turn_on = 1                  ; break ;
            case isqDR_offwid:  turn_on = 0                  ; break ;
         }

         if( turn_on == seq->onoff_state ) RETURN( True );

         MCW_widget_geom( seq->wimage , &ww , &hh , NULL,NULL ) ;

         if( turn_on ){
            MCW_manage_widgets( seq->onoff_widgets , seq->onoff_num ) ;
            XtVaSetValues(
               seq->wimage ,
                  XmNrightPosition ,(int)( 0.49+seq->image_frac*FORM_FRAC_BASE ),
                  XmNbottomPosition,(int)( 0.49+seq->image_frac*FORM_FRAC_BASE ),
               NULL ) ;
            XtVaSetValues( seq->wtop ,
                              XmNwidth  , (int)(0.49+ww/seq->image_frac) ,
                              XmNheight , (int)(0.49+hh/seq->image_frac) ,
                           NULL ) ;
            if( !seq->button2_enabled ) XtUnmanageChild(seq->pen_bbox->wrowcol);
         } else {
            MCW_unmanage_widgets( seq->onoff_widgets , seq->onoff_num ) ;
            XtVaSetValues( seq->wimage ,
                              XmNrightPosition , FORM_FRAC_BASE ,
                              XmNbottomPosition, FORM_FRAC_BASE ,
                           NULL ) ;
            XtVaSetValues( seq->wtop ,
                              XmNwidth  , ww ,
                              XmNheight , hh ,
                           NULL ) ;
         }

         seq->onoff_state = turn_on ;
         RETURN( True );
      }
      break ;

      /*------- title --------*/

      case isqDR_title:{
         char * title = (char *) drive_data ;

         if( title == NULL || strlen(title) == 0 ) title = "AFNI" ;

         XtVaSetValues( seq->wtop , XmNtitle , title , NULL ) ;
#if 1
         if( MCW_isitmwm( seq->wtop ) )
            XtVaSetValues( seq->wtop ,
                            XmNmwmDecorations, MWM_DECOR_ALL | MWM_DECOR_MAXIMIZE,
                           NULL ) ;
#endif
         RETURN( True );
      }
      break ;

      /*------- death! -------*/

      case isqDR_destroy:{
         ISQ_timer_stop(seq) ; NI_sleep(1) ;
         ISQ_but_done_CB( NULL , (XtPointer)seq , NULL ) ; NI_sleep(1) ;
         RETURN( True );
      }
      break ;

      /*------- unrealize! -------*/

      case isqDR_unrealize:{
         ISQ_timer_stop(seq) ;
         if( ISQ_REALZ(seq) ){ XtUnrealizeWidget(seq->wtop); NI_sleep(1); }
         seq->valid = 1 ;
         RETURN( True );
      }
      break ;

      /*------- realize! -------*/

      case isqDR_realize:{
         if( ! ISQ_REALZ(seq) ){
            XtRealizeWidget( seq->wtop )   ; NI_sleep(1) ;
            WAIT_for_window( seq->wtop )   ;
            NORMAL_cursorize( seq->wtop )  ;
            POPUP_cursorize( seq->wimage ) ;
            POPUP_cursorize( seq->wbar )   ;
            POPUP_cursorize( seq->wbut_bot[NBUT_SAVE] ) ;
            XmUpdateDisplay( seq->wtop )   ;
         }
#ifndef DONT_ONOFF_ONE
         if( seq->status->num_total == 1 )  /* 08 Aug 2001 */
           drive_MCW_imseq( seq , isqDR_onoffwid , (XtPointer) isqDR_offwid ) ;
#endif
         ALLOW_CLIPPING( seq , (0.0f < seq->top_clip) ) ;
         seq->valid = 2 ;
         RETURN( True );
      }
      break ;

      /*------- change helptext! -------*/

      case isqDR_imhelptext:{
        char * newtxt = (char *) drive_data ;
        int ii ;

        if( newtxt == NULL ) RETURN( False );
        ii = strlen(newtxt) ;
        if( ii == 0 ) RETURN( False );

        strncpy( seq->im_helptext , newtxt , ISQ_NHELP ) ;
        seq->im_helptext[ISQ_NHELP] = '\0' ;
        RETURN( True );
      }
      break ;

      /*------- display anew! -------*/

      case isqDR_reimage:
      case isqDR_reshow:
      case isqDR_overlay:
      case isqDR_display:{
         int n = (int) drive_data ;

         if( ! seq->ignore_redraws )
            ISQ_redisplay( seq , n , drive_code ) ;
         RETURN( True );
      }
      break ;

      /*------- display bar anew [23 Aug 1998] -------*/

      case isqDR_rebar:{
         KILL_2XIM( seq->given_xbar , seq->sized_xbar ) ; /* destroy old */
         if( seq->onoff_state ) ISQ_show_bar( seq ) ;     /* show new?  */
         RETURN( True );
      }
      break ;

      /*------- new cursor for image -------*/

      case isqDR_cursor:{
         int cur = (int) drive_data ;

         MCW_alter_widget_cursor( seq->wimage , cur , "yellow" , "blue" ) ;
         RETURN( True );
      }
      break ;

      /*------- new options -------*/

      case isqDR_options:{
         ISQ_options * newopt = (ISQ_options *) drive_data ;
         int sf=0 ;

         if( ppmto_num > 0 )           /* 27 Mar 2002: keep the old */
           sf = seq->opt.save_filter ; /*              save filter  */

         if( newopt != NULL ) seq->opt = *newopt ;

         if( ppmto_num > 0 )
           seq->opt.save_filter = sf ;

         seq->opt.parent = (XtPointer) seq ;
         SET_SAVE_LABEL(seq) ;
         AV_SENSITIZE(seq->ov_opacity_av,!seq->opt.no_overlay) ; /* 09 Mar 2001 */


         if( ISQ_REALZ(seq) ) ISQ_redisplay( seq , -1 , isqDR_display ) ;
         RETURN( True );
      }
      break ;

      /*------- get current options [07 Aug 1999] -------*/

      case isqDR_getoptions:{
         ISQ_options * opt = (ISQ_options *) drive_data ;

         if( opt == NULL ) RETURN( False );
         *opt = seq->opt ;
         RETURN( True );
      }

      /*------- turn arrowpad on -------*/

      case isqDR_arrowpadon:{
         char * helptext = (char *) drive_data ;

         XtSetMappedWhenManaged( seq->arrowpad->wform , True ); /* on */

         if( helptext != NULL && strlen(helptext) > 0 ){
            char * str = XtNewString( helptext ) ;
            MCW_reghelp_children( seq->arrowpad->wform , str ) ;
            XtFree(str) ;  /* 28 Sep 1998: via Purify */
         }
         RETURN( True );
      }
      break ;

      case isqDR_arrowpadhint:{
         int ib ;
         char ** hint = (char **) drive_data ;
         if( hint == NULL ) RETURN( False );
         for( ib=0 ; ib < 5 ; ib++ )
            MCW_register_hint( seq->arrowpad->wbut[ib] , hint[ib] ) ;
         RETURN( True );
      }
      break ;

      /*------- turn arrowpad off -------*/

      case isqDR_arrowpadoff:{
         XtSetMappedWhenManaged( seq->arrowpad->wform , False ); /* off */
         RETURN( True );
      }
      break ;

      /*------- new numtotal -------*/

#if 0                                   /* 29 Jul 2002: removed from the canon for unuse */
      case isqDR_numtotal:{
         int newtot = (int) drive_data ,
             oldtot = seq->status->num_total ,
             numser = seq->status->num_series , ii ;
         char * msg =
             "illegal change to image\n"
             "count from driver routine\n"
             "(press here to continue)" ;

         /* check for error conditions */

         if( newtot == oldtot ) RETURN( True );

         if( newtot < 2 || newtot < numser ){
            if( ISQ_REALZ(seq) )
               MCW_popup_message( seq->wimage , msg , MCW_USER_KILL ) ;
            fprintf(stderr,"\n%s\n",msg) ;
            RETURN( False );
         }

         /* stop the automatic statistics calculations, if started */

         if( seq->glstat->worker != 0 ){
            XtRemoveWorkProc( seq->glstat->worker ) ;
            seq->glstat->worker = 0 ;
         }

         /* setup new space for the per image statistics */

         seq->imstat = (ISQ_indiv_statistics *)
                        XtRealloc( (char *) seq->imstat ,
                                   sizeof(ISQ_indiv_statistics) * newtot ) ;

         for( ii=oldtot ; ii < newtot ; ii++ )
             seq->imstat[ii].one_done = seq->imstat[ii].glob_done = False ;

         /* let the imseq know that the number of images is different */

         seq->status->num_total = newtot ;

         XtVaSetValues( seq->wscale ,
                           XmNmaximum , newtot-1 ,
                        NULL ) ;

         if( seq->im_nr >= newtot )
            ISQ_redisplay( seq , newtot-1 , isqDR_display ) ;

         RETURN( True );
      }
      break ;
#endif

      /*------- new image sequence!!! -------*/

      case isqDR_newseq:{
         Boolean good ;
         ISQ_timer_stop(seq) ;
         good = ISQ_setup_new( seq , drive_data ) ;
         RETURN( good );
      }
      break ;

      /*------ re-initialize image statistics -----*/

      case isqDR_clearstat:{
         int ii ;

         seq->opt.scale_group = ISQ_SCL_AUTO ;  /* autoscaling */
         ISQ_disp_options( seq , False ) ;      /* set buttons */

         if( seq->glstat->worker != 0 ){  /* remove work process */
            XtRemoveWorkProc( seq->glstat->worker ) ;
            seq->glstat->worker = 0 ;
         }

         for( ii=0 ; ii < seq->status->num_total ; ii++ )
            seq->imstat[ii].one_done = seq->imstat[ii].glob_done = False ;

         for( ii=0 ; ii < NHISTOG ; ii++ )
            seq->glstat->hist[ii] = 0 ;  /* initialize histogram */

         seq->glstat->mm_done =
           seq->glstat->per_done = (seq->status->num_series < 2 ) ;

#ifdef AUTOMATE_STATISTICS
         if( seq->glstat->mm_done ){
            seq->glstat->worker = 0 ;
         } else {
            seq->glstat->worker = XtAppAddWorkProc(
                                        seq->dc->appcontext ,
                                        ISQ_statistics_WP , seq ) ;
         }
#else
         seq->glstat->worker = 0 ;
#endif
      }
      break ;

   }  /* end of switch on drive_code */

   RETURN( False );  /* should never be reached! */
}

/*---------------------------------------------------------------------*/

#define XYORG 128
#define DXY    64

void ISQ_arrowpad_CB( MCW_arrowpad *apad , XtPointer client_data )
{
   MCW_imseq *seq = (MCW_imseq *) client_data ;

   ISQ_cbs cbs ;
   int xorg,yorg , xwin,ywin , xoff,yoff ;

ENTRY("ISQ_arrowpad_CB") ;

   if( ! ISQ_REALZ(seq) || seq->status->send_CB == NULL ) EXRETURN ;

   cbs.event = &(apad->xev) ;  /* copy event for user's edification */

   if( apad->which_pressed == AP_MID ){
      cbs.reason = isqCR_appress ;
#if 0
      seq->status->send_CB( seq , seq->getaux , &cbs ) ;
#else
      SEND(seq,cbs) ;
#endif
      EXRETURN ;
   }

   /* 24 Jan 2003: pan a zoomed image */

   if( seq->zoom_button1 && seq->zoom_fac > 1 && seq->zoom_xim != NULL ){
     switch( apad->which_pressed ){
       default:
       case AP_DOWN:  xoff =  0 ; yoff = -1 ; break ;
       case AP_UP:    xoff =  0 ; yoff =  1 ; break ;
       case AP_LEFT:  xoff =  1 ; yoff =  0 ; break ;
       case AP_RIGHT: xoff = -1 ; yoff =  0 ; break ;
     }
     ISQ_actually_pan( seq , xoff , yoff ) ;
     EXRETURN ;
   }

   xwin = ywin = XYORG ;

   switch( apad->which_pressed ){
      default:
      case AP_DOWN:  ywin = XYORG + DXY ; break ;
      case AP_UP:    ywin = XYORG - DXY ; break ;
      case AP_LEFT:  xwin = XYORG - DXY ; break ;
      case AP_RIGHT: xwin = XYORG + DXY ; break ;
   }

   xorg = yorg = XYORG ;       ISQ_flipxy( seq , &xorg,&yorg ) ;
   xoff = xwin ; yoff = ywin ; ISQ_flipxy( seq , &xoff,&yoff ) ;

        if( xoff > xorg ) cbs.reason = isqCR_dxplus  ;
   else if( xoff < xorg ) cbs.reason = isqCR_dxminus ;
   else if( yoff > yorg ) cbs.reason = isqCR_dyplus  ;
   else if( yoff < yorg ) cbs.reason = isqCR_dyminus ;
   else                   EXRETURN ;                     /* error! */

#if 0
   seq->status->send_CB( seq , seq->getaux , &cbs ) ;
#else
   SEND(seq,cbs) ;
#endif
   EXRETURN ;
}

/*-------------------------------------------------------------------
   Setup the data structures to handle a new sequence of images;
   this should be immediately followed by a call to set the image
   to the correct number (or things won't look good at all).
---------------------------------------------------------------------*/

Boolean ISQ_setup_new( MCW_imseq *seq , XtPointer newaux )
{
   MCW_imseq_status *imstatus=NULL ;
   int ii ;
   MRI_IMAGE *tim ;

ENTRY("ISQ_setup_new") ;

   if( !ISQ_VALID(seq) ) RETURN( False );

#if 0
   imstatus = (MCW_imseq_status *)seq->getim(0,isqCR_getstatus,newaux);
#else
   AFNI_CALL_VALU_3ARG( seq->getim , MCW_imseq_status *,imstatus ,
                        int,0 , int,isqCR_getstatus , XtPointer,newaux ) ;
#endif
   if( imstatus->num_total < 1 ){ RETURN( False ); }  /* 09 Feb 1999: allow 1 */

#if 0
   tim = (MRI_IMAGE *) seq->getim(0,isqCR_getqimage,newaux) ; /* 1st image */
   KILL_1MRI(tim) ;  /* don't need tim no more */
#endif

#if 1
   if( seq->status != NULL ) myXtFree(seq->status) ;  /* 05 Feb 2000 */
#endif

   seq->status = imstatus ;
   seq->im_nr  = imstatus->num_total / 2 ;  /* do this image 1st */

   KILL_1MRI(seq->imim) ;  /* NULL out all internally stored images */
   KILL_1MRI(seq->ovim) ;
   KILL_1MRI(seq->orim) ;  /* 09 Feb 1999 */

   KILL_2XIM( seq->given_xim  , seq->sized_xim  ) ;
   KILL_2XIM( seq->given_xbar , seq->sized_xbar ) ;

   seq->given_xim = seq->sized_xim
                  = seq->given_xbar
                  = seq->sized_xbar = NULL ;

   seq->imim = seq->ovim = NULL ;

   /* re-initialize image statistics */

   seq->opt.scale_group = ISQ_SCL_AUTO ;
   ISQ_disp_options( seq , False ) ;  /* set toggles from option list */

   seq->imstat = (ISQ_indiv_statistics *)
                 XtRealloc( (char *) seq->imstat ,
                            sizeof(ISQ_indiv_statistics)
                            * imstatus->num_total ) ;

   if( seq->glstat->worker != 0 ){  /* remove work process, if started */
      XtRemoveWorkProc( seq->glstat->worker ) ;
      seq->glstat->worker = 0 ;
   }

   for( ii=0 ; ii < imstatus->num_total ; ii++ ){
      seq->imstat[ii].one_done = seq->imstat[ii].glob_done = False ;
      seq->imstat[ii].parent   = (XtPointer) seq ;
   }
   seq->glstat->parent = (XtPointer) seq ;

   for( ii=0 ; ii < NHISTOG ; ii++ )
      seq->glstat->hist[ii] = 0 ;  /* initialize histogram */

   seq->glstat->mm_done =
     seq->glstat->per_done = (seq->status->num_series < 2 ) ;

#ifdef AUTOMATE_STATISTICS
   if( seq->glstat->mm_done ){
      seq->glstat->worker = 0 ;
   } else {
      seq->glstat->worker = XtAppAddWorkProc(
                                  seq->dc->appcontext ,
                                  ISQ_statistics_WP , seq ) ;
   }
#else
   seq->glstat->worker = 0 ;
#endif

   /* OOPS!  I forgot to reset the scale max value! */

   ii = seq->status->num_total - 1 ; if( ii <= 0 ) ii = 1 ;  /* 09 Feb 1999 */

   XtVaSetValues( seq->wscale ,
                     XmNmaximum , ii ,
                     XmNvalue   , seq->im_nr ,
                  NULL ) ;

#ifndef DONT_ONOFF_ONE
   if( seq->status->num_total == 1 )
      drive_MCW_imseq( seq , isqDR_onoffwid , (XtPointer) isqDR_offwid ) ;
#endif

 if(PRINT_TRACING){
   char str[256] ;
   sprintf(str,"hbase=%d vbase=%d nim=%d lev=%g",
          seq->hbase,seq->vbase, seq->status->num_total,seq->lev ) ;
   STATUS(str) ;
 }

   seq->getaux = newaux ;

   seq->top_clip = 0.0f ;  /* 17 Sep 2007 */
   if( seq->opt.scale_range == ISQ_RNG_CLIPPED ) seq->redo_clip = 1 ;
   ALLOW_CLIPPING(seq,0) ;

   RETURN( True ) ;
}

/*----------------------------------------------------------------------*/
/*----         Stuff for the menu hidden on the color bar           ----*/

void ISQ_wbar_plots_CB( Widget w , XtPointer cld , XtPointer cad ) /* 20 Sep 2001 */
{
   MCW_imseq * seq = (MCW_imseq *) cld ;

ENTRY("ISQ_wbar_plots_CB") ;

   if( !ISQ_REALZ(seq) ) EXRETURN ;
   ISQ_redisplay( seq , -1 , isqDR_display ) ;
   EXRETURN ;
}

/*----------------------------------------------------------------------*/

void ISQ_wbar_label_CB( MCW_arrowval *av , XtPointer cd )
{
   MCW_imseq *seq = (MCW_imseq *)cd ;

ENTRY("ISQ_wbar_label_CB") ;

   if( !ISQ_REALZ(seq) ) EXRETURN ;

   if( av == seq->wbar_animdup_av )
     ISQ_anim_dup = av->ival;  /* 10 Feb 2009 */
   else
     ISQ_redisplay( seq , -1 , isqDR_display ) ;

   EXRETURN ;
}

/*----------------------------------------------------------------------*/

void ISQ_wbar_menu_CB( Widget w , XtPointer client_data ,
                                  XtPointer call_data    )
{
   MCW_imseq * seq = (MCW_imseq *) client_data ;

ENTRY("ISQ_wbar_menu_CB") ;

   if( ! ISQ_REALZ(seq) ) EXRETURN ;

   /*** User range toggle ***/

   if( w == seq->wbar_rng_but ){
      MCW_choose_string( seq->wimage , "Display range: bot top [ztop]" ,
                         NULL , ISQ_set_rng_CB , seq ) ;
   }

   else if( w == seq->wbar_zer_but ){
      MCW_choose_ovcolor( seq->wimage , seq->dc , seq->zer_color ,
                          ISQ_set_zcol_CB , seq ) ;
   }

   else if( w == seq->wbar_flat_but ){
      MCW_choose_string( seq->wimage , "Flatten range: bot top" ,
                         NULL , ISQ_set_flat_CB , seq ) ;
   }

   else if( w == seq->wbar_sharp_but ){
      MCW_choose_integer( seq->wimage , "Sharpen Factor" ,
                          1 , 9 , (int)(10.01*seq->sharp_fac) ,
                          ISQ_set_sharp_CB , seq ) ;
   }

   else if( w == seq->wbar_graymap_pb ){   /* 24 Oct 2003 */
     ISQ_graymap_draw( seq ) ;
   }

   EXRETURN ;
}

/*----------------------------------------------------------------------*/

void ISQ_set_rng_CB( Widget w , XtPointer cd , MCW_choose_cbs * cbs )
{
   MCW_imseq * seq = (MCW_imseq *) cd ;

ENTRY("ISQ_set_rng_CB") ;

   if( ! ISQ_REALZ(seq) || w == NULL || ! XtIsWidget(w) ) EXRETURN ;

   seq->rng_bot = seq->rng_top = seq->rng_ztop = 0.0 ;
   seq->rng_extern = 0 ;
   sscanf( cbs->cval , "%f%f%f" ,
           &(seq->rng_bot) , &(seq->rng_top) , &(seq->rng_ztop) ) ;
   ISQ_redisplay( seq , -1 , isqDR_reimage ) ;  /* redo current image */
   EXRETURN ;
}

/*----------------------------------------------------------------------*/

void ISQ_set_zcol_CB( Widget w , XtPointer cd , MCW_choose_cbs * cbs )
{
   MCW_imseq * seq = (MCW_imseq *) cd ;

ENTRY("ISQ_set_zcol_CB") ;

   if( ! ISQ_REALZ(seq) || w == NULL || ! XtIsWidget(w) ) EXRETURN ;

   seq->zer_color = cbs->ival ;
   ISQ_redisplay( seq , -1 , isqDR_reimage ) ;  /* redo current image */
   EXRETURN ;
}

/*----------------------------------------------------------------------*/

void ISQ_set_flat_CB( Widget w , XtPointer cd , MCW_choose_cbs * cbs )
{
   MCW_imseq * seq = (MCW_imseq *) cd ;

ENTRY("ISQ_set_flat_CB") ;

   if( ! ISQ_REALZ(seq) || w == NULL || ! XtIsWidget(w) ) EXRETURN ;

   seq->flat_bot = seq->flat_top = 0.0 ;
   sscanf( cbs->cval , "%f%f" ,
           &(seq->flat_bot) , &(seq->flat_top) ) ;

   if( seq->flat_bot < 0.0 ) seq->flat_bot = 0.0 ;
   if( seq->flat_bot > 1.0 ) seq->flat_bot*= 0.01 ;
   if( seq->flat_top < 0.0 ) seq->flat_top = 0.0 ;
   if( seq->flat_top > 1.0 ) seq->flat_top*= 0.01 ;

   if( seq->flat_bot >= seq->flat_top || seq->flat_top > 1.0 )
      seq->flat_bot = seq->flat_top = 0.0 ;

   ISQ_redisplay( seq , -1 , isqDR_reimage ) ;  /* redo current image */
   EXRETURN ;
}

/*----------------------------------------------------------------------*/

void ISQ_set_sharp_CB( Widget w , XtPointer cd , MCW_choose_cbs * cbs )
{
   MCW_imseq * seq = (MCW_imseq *) cd ;

ENTRY("ISQ_set_sharp_CB") ;

   if( ! ISQ_REALZ(seq) || w == NULL || ! XtIsWidget(w) ) EXRETURN ;

   seq->sharp_fac = 0.1 * cbs->ival ;

   ISQ_redisplay( seq , -1 , isqDR_reimage ) ;  /* redo current image */
   EXRETURN ;
}

/*----------------------------------------------------------------------------
   April 1996: Routines to process the montage stuff
------------------------------------------------------------------------------*/

#define MONT_quit_label  "Quit"
#define MONT_1x1_label   "1x1"
#define MONT_apply_label "Draw"
#define MONT_done_label  "Set"

#define MONT_quit_help   "Press to close\nthis control box"
#define MONT_1x1_help    "Press to set the controls\nto Across=1 and Down=1"
#define MONT_apply_help  "Press to apply this choice\nand keep this control box"
#define MONT_done_help   "Press to apply this choice\nand close this control box"

#define NUM_MONT_ACT 4

static MCW_action_item MONT_act[NUM_MONT_ACT] = {
 { MONT_quit_label , ISQ_montage_action_CB, NULL, MONT_quit_help ,"Close window"                 ,0 },
 { MONT_1x1_label  , ISQ_montage_action_CB, NULL, MONT_1x1_help  ,"Set Across=Down=1"            ,0 },
 { MONT_apply_label, ISQ_montage_action_CB, NULL, MONT_apply_help,"Apply choice and keep window" ,0 },
 { MONT_done_label , ISQ_montage_action_CB, NULL, MONT_done_help ,"Apply choice and close window",1 },
} ;

#define MONT_QUIT  0
#define MONT_1X1   1
#define MONT_APPLY 2
#define MONT_DONE  3

void ISQ_montage_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   MCW_imseq * seq = (MCW_imseq *) client_data ;
   int ib ;
   Widget wrc ;

ENTRY("ISQ_montage_CB") ;

   if( ! ISQ_REALZ(seq) || seq->dialog != NULL ) EXRETURN ;

   if( seq->zoom_fac != 1 ){
#if 0
fprintf(stderr,"montage: zoom_fac = %d\n",seq->zoom_fac) ;
#endif
     XBell(seq->dc->display,100); EXRETURN; /* 18 Nov 2003 */
   }

   for( ib=0 ; ib < NBUTTON_BOT-1 ; ib++ )       /* turn off buttons  */
     if( ISQ_but_bot_dial[ib] == True )          /* that also want to */
       SENSITIZE( seq->wbut_bot[ib] , False ) ;  /* use seq->dialog   */

   seq->dialog = XtVaCreatePopupShell(
                    "menu" , xmDialogShellWidgetClass , seq->wtop ,
                       XmNtitle , "Montage" ,
                       XmNdeleteResponse , XmDO_NOTHING ,
                       XmNinitialResourcesPersistent , False ,
                    NULL ) ;

   SAVEUNDERIZE(seq->dialog) ; /* 27 Feb 2001 */

   DC_yokify( seq->dialog , seq->dc ) ; /* 14 Sep 1998 */

   seq->dialog_starter = NBUT_MONT ;

#if 1
   if( MCW_isitmwm(w) )
      XtVaSetValues( seq->dialog ,
                       XmNmwmDecorations , MWM_DECOR_BORDER ,
                       XmNmwmFunctions ,   MWM_FUNC_MOVE
                                         | MWM_FUNC_CLOSE ,
                     NULL ) ;
#endif

   XmAddWMProtocolCallback(           /* make "Close" window menu work */
           seq->dialog ,
           XmInternAtom( seq->dc->display , "WM_DELETE_WINDOW" , False ) ,
           ISQ_montage_action_CB , seq ) ;

   wrc  = XtVaCreateWidget(                    /* RowColumn to hold all */
             "menu" , xmRowColumnWidgetClass , seq->dialog ,
                XmNpacking     , XmPACK_TIGHT ,
                XmNorientation , XmVERTICAL ,
                XmNtraversalOn , False ,
                XmNinitialResourcesPersistent , False ,
             NULL ) ;

   (void) XtVaCreateManagedWidget(
            "menu" , xmLabelWidgetClass , wrc ,
               LABEL_ARG("-- Montage Controls --") ,
               XmNalignment  , XmALIGNMENT_CENTER ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;

   (void) XtVaCreateManagedWidget(
            "menu" , xmSeparatorWidgetClass , wrc ,
               XmNseparatorType , XmSHADOW_ETCHED_IN ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;

   seq->mont_across_av = new_MCW_arrowval(
#if 1
                          wrc , "Across:" ,
#else
                          wrc , NULL ,   /* just for testing purposes */
#endif
                          MCW_AV_optmenu ,
                          1 , MONT_NMAX , seq->mont_nx ,
                          MCW_AV_edittext , 0 ,
                          NULL , NULL , NULL , NULL ) ;

   if( MONT_NMAX > COLSIZE )
      AVOPT_columnize(  seq->mont_across_av , 1+(MONT_NMAX-1)/COLSIZE ) ;

   if( seq->mont_across_av->wtext != NULL )
      XtVaSetValues( seq->mont_across_av->wtext , XmNcolumns , 4 , NULL ) ;

   seq->mont_down_av  = new_MCW_arrowval(
                          wrc , "Down:  " ,
                          MCW_AV_optmenu ,
                          1 , MONT_NMAX , seq->mont_ny ,
                          MCW_AV_edittext , 0 ,
                          NULL , NULL , NULL , NULL ) ;

   if( MONT_NMAX > COLSIZE )
      AVOPT_columnize(  seq->mont_down_av , 1+(MONT_NMAX-1)/COLSIZE ) ;

   if( seq->mont_down_av->wtext != NULL )
      XtVaSetValues( seq->mont_down_av->wtext , XmNcolumns , 4 , NULL ) ;

   seq->mont_skip_av  = new_MCW_arrowval(
                          wrc , "Spacing" ,
                          MCW_AV_optmenu ,
                          1 , MONT_SMAX , seq->mont_skip + 1 ,
                          MCW_AV_edittext , 0 ,
                          NULL , NULL , NULL , NULL ) ;

   if( MONT_SMAX > COLSIZE )
      AVOPT_columnize(  seq->mont_skip_av , 1+(MONT_SMAX-1)/COLSIZE ) ;

   if( seq->mont_skip_av->wtext != NULL )
      XtVaSetValues( seq->mont_skip_av->wtext , XmNcolumns , 4 , NULL ) ;

   seq->mont_gap_av  = new_MCW_arrowval(
                          wrc , "Border:" ,
                          MCW_AV_optmenu ,
                          0 , MONT_GMAX , seq->mont_gap,
                          MCW_AV_edittext , 0 ,
                          NULL , NULL , NULL , NULL ) ;

   if( MONT_GMAX > COLSIZE )
      AVOPT_columnize(  seq->mont_gap_av , 1+(MONT_GMAX-1)/COLSIZE ) ;

   if( seq->mont_gap_av->wtext != NULL )
      XtVaSetValues( seq->mont_gap_av->wtext , XmNcolumns , 4 , NULL ) ;

   seq->mont_gapcolor_av = new_MCW_colormenu( wrc ,
                                "Color: " , seq->dc ,
                                0 , seq->dc->ovc->ncol_ov - 1 , seq->mont_gapcolor ,
                                NULL , NULL ) ;

   seq->mont_across_av->allow_wrap   = 1 ;   /* allow wrap at limits of values */
   seq->mont_down_av->allow_wrap     = 1 ;
   seq->mont_skip_av->allow_wrap     = 1 ;
   seq->mont_gap_av->allow_wrap      = 1 ;
   seq->mont_gapcolor_av->allow_wrap = 1 ;

   seq->mont_across_av->fastdelay    = 250 ; /* slow down arrow repeat action */
   seq->mont_down_av->fastdelay      = 250 ;
   seq->mont_skip_av->fastdelay      = 250 ;
   seq->mont_gap_av->fastdelay       = 250 ;
   seq->mont_gapcolor_av->fastdelay  = 250 ;

   seq->mont_nx_old       = seq->mont_nx       ; /* in case something is changed */
   seq->mont_ny_old       = seq->mont_ny       ;
   seq->mont_skip_old     = seq->mont_skip     ;
   seq->mont_gap_old      = seq->mont_gap      ;
   seq->mont_gapcolor_old = seq->mont_gapcolor ;

   MCW_reghelp_children( seq->mont_across_av->wrowcol ,
      "This controls the number\n"
      "of images displayed across\n"
      "(horizontally) the window."
   ) ;
   MCW_reghint_children( seq->mont_across_av->wrowcol ,
                         "Number of images horizontal" ) ;

   MCW_reghelp_children( seq->mont_down_av->wrowcol ,
      "This controls the number\n"
      "of images displayed down\n"
      "(vertically) the window."
   ) ;
   MCW_reghint_children( seq->mont_down_av->wrowcol ,
                         "Number of images vertical" ) ;

   MCW_reghelp_children( seq->mont_skip_av->wrowcol ,
      "This controls the spacing between\n"
      "slice images displayed in the\n"
      "montage.  For example, if Spacing\n"
      "is 4, every fourth slice will be\n"
      "displayed (from left to right, then\n"
      "top to bottom)."
   ) ;
   MCW_reghint_children( seq->mont_skip_av->wrowcol ,
                         "Spacing between images" ) ;

   MCW_reghelp_children( seq->mont_gap_av->wrowcol ,
      "This controls the number\n"
      "of pixels left as borders\n"
      "between the sub-images"
   ) ;
   MCW_reghint_children( seq->mont_gap_av->wrowcol ,
                         "Borders between images" ) ;

   MCW_reghelp_children( seq->mont_gapcolor_av->wrowcol ,
      "This controls the color\n"
      "put in the borders between\n"
      "the sub-images"
   ) ;
   MCW_reghint_children( seq->mont_gapcolor_av->wrowcol ,
                         "Border color" ) ;

   for( ib=0 ; ib < NUM_MONT_ACT ; ib++ )
      MONT_act[ib].data = (XtPointer) seq ;

   (void) MCW_action_area( wrc , MONT_act , NUM_MONT_ACT ) ;

   XtManageChild( wrc ) ;
   ISQ_place_dialog( seq ) ;  /* 05 Jan 1999 */
   XtPopup( seq->dialog , XtGrabNone ) ; NI_sleep(1);
   NORMAL_cursorize( seq->dialog ) ;
   ISQ_but_done_reset( seq ) ;
   EXRETURN ;
}

/*----------------------------------------------------------------------------*/

void ISQ_montage_action_CB( Widget w , XtPointer client_data , XtPointer call_data )
{
   MCW_imseq * seq = (MCW_imseq *) client_data ;
   XmAnyCallbackStruct * cbs = (XmAnyCallbackStruct *) call_data ;
   char * wname ;
   int ib , close_window , new_mont ;

ENTRY("ISQ_montage_action_CB") ;

   if( !ISQ_REALZ(seq) || seq->dialog==NULL || seq->dialog_starter!=NBUT_MONT ) EXRETURN ;

   wname = XtName(w) ;

   for( ib=0 ; ib < NUM_MONT_ACT ; ib++ )           /* button index, if any */
      if( strcmp(wname,MONT_act[ib].label) == 0 ) break ;

   close_window = (ib == MONT_DONE || ib == MONT_QUIT || ib == NUM_MONT_ACT) ;

   if( close_window ){
     RWC_XtPopdown( seq->dialog ) ;
     XSync( XtDisplay(w) , False ) ;
     XmUpdateDisplay( w ) ;
     seq->dont_place_dialog = 1 ;  /* 23 Jan 2004 */
   }

   switch( ib ){

      case MONT_APPLY:
      case MONT_DONE:
         seq->mont_nx       = seq->mont_across_av->ival ;
         seq->mont_ny       = seq->mont_down_av->ival ;
         seq->mont_skip     = seq->mont_skip_av->ival - 1 ;
         seq->mont_gap      = seq->mont_gap_av->ival ;
         seq->mont_gapcolor = seq->mont_gapcolor_av->ival ;

         new_mont = ( seq->mont_nx   != seq->mont_nx_old ||
                      seq->mont_ny   != seq->mont_ny_old ||
                      seq->mont_skip != seq->mont_skip_old ) ;

         if( ib == MONT_APPLY ) MCW_invert_widget(w) ;

         ISQ_redisplay( seq , -1 , isqDR_display ) ;    /* local redraw */

         if( seq->status->send_CB != NULL && new_mont ){

            ISQ_cbs cbs ;
            THD_ivec3 minf ;
            int ijcen = (seq->mont_nx)/2 + (seq->mont_ny/2) * seq->mont_nx ,
                nmont = seq->mont_nx * seq->mont_ny ;

            minf.ijk[0]  = ijcen ;            /* number of slices before center */
            minf.ijk[1]  = nmont-ijcen-1 ;    /* number after */
            minf.ijk[2]  = seq->mont_skip ;   /* number between slices */
            cbs.reason   = isqCR_newmontage ;
            cbs.userdata = (XtPointer) &minf ;

            seq->ignore_redraws = 1 ;         /* don't listen to redraws */
#if 0
            seq->status->send_CB( seq , seq->getaux , &cbs ) ;
#else
            SEND(seq,cbs) ;
#endif
            seq->ignore_redraws = 0 ;         /* can listen again */
         }

#if 0
         ISQ_redisplay( seq , -1 , isqDR_display ) ;    /* local redraw */
#endif

         if( ib == MONT_APPLY ) MCW_invert_widget(w) ;

         seq->mont_nx_old       = seq->mont_nx ;
         seq->mont_ny_old       = seq->mont_ny ;
         seq->mont_skip_old     = seq->mont_skip ;
         seq->mont_gap_old      = seq->mont_gap ;
         seq->mont_gapcolor_old = seq->mont_gapcolor ;

         /* set to "Save One" if have an actual montage going now */

         if( seq->mont_nx * seq->mont_ny > 1 && !seq->opt.save_one ){
            seq->opt.save_one  = 1 ;
            seq->opt.save_agif = 0 ; /* 27 Jul 2001 */
            seq->opt.save_mpeg = 0 ;
            SET_SAVE_LABEL(seq) ;
         }
      break ;

      case MONT_1X1:
         MCW_invert_widget(w) ;
         AV_assign_ival( seq->mont_across_av , 1 ) ;
         AV_assign_ival( seq->mont_down_av   , 1 ) ;
         MCW_invert_widget(w) ;
      break ;
   }

   /*** done -- close the window if ordered ***/

   if( close_window ){                          /* close the window */
      XtDestroyWidget( seq->dialog ) ; NI_sleep(1) ;
      seq->dialog = NULL ;
      for( ib=0 ; ib < NBUTTON_BOT-1 ; ib++ )       /* turn buttons back on */
         if( ISQ_but_bot_dial[ib] == True )         /* that also want to   */
            SENSITIZE( seq->wbut_bot[ib] , True ) ; /* use seq->dialog    */

      FREE_AV( seq->mont_across_av ) ;
      FREE_AV( seq->mont_down_av ) ;
      FREE_AV( seq->mont_skip_av ) ;
      FREE_AV( seq->mont_gap_av ) ;
      FREE_AV( seq->mont_gapcolor_av ) ;

      seq->mont_across_av   = NULL ;
      seq->mont_down_av     = NULL ;
      seq->mont_skip_av     = NULL ;
      seq->mont_gap_av      = NULL ;
      seq->mont_gapcolor_av = NULL ;

      seq->dialog_starter = -1 ;
      seq->dont_place_dialog = 0 ;  /* 23 Jan 2004 */
   }

   EXRETURN ;
}

/*---------------------------------------------------------------------------
   Routine to get one image for the montage display.
   Output image is MRI_short (underlay or overlay index), or MRI_rgb.
-----------------------------------------------------------------------------*/

MRI_IMAGE * ISQ_manufacture_one( int nim , int overlay , MCW_imseq *seq )
{
   MRI_IMAGE *im , *ovim , *tim ;
   int nrold ;

ENTRY("ISQ_manufacture_one") ;

   if( ! ISQ_VALID(seq) ) RETURN( NULL );

   if( seq->mont_periodic ){
     while( nim < 0 )                       nim += seq->status->num_total ;
     while( nim >= seq->status->num_total ) nim -= seq->status->num_total ;
   } else {
     if( nim < 0 || nim >= seq->status->num_total ) RETURN( NULL );
   }

   /** Not an overlay image **/

   if( !overlay ){
     switch( seq->render_mode ){
       default:
         tim = ISQ_getimage( nim , seq ) ;
         if( tim == NULL ) RETURN(NULL) ;
         im = ISQ_process_mri( nim , seq , tim ) ; mri_free(tim) ;
       break ;

       case RENDER_CHECK_UO:
       case RENDER_CHECK_OU:
         im = ISQ_getchecked( nim , seq ) ;
         if( im == NULL ) RETURN(NULL) ;
       break ;
     }
     RETURN(im) ;
   }

   /** Get the overlay image **/

   if( ISQ_SKIP_OVERLAY(seq) ) RETURN( NULL );

   tim = ISQ_getoverlay( nim , seq ) ;

   if( tim == NULL ) RETURN( NULL );

   if( !ISQ_GOOD_OVERLAY_TYPE(tim->kind) ){
     fprintf(stderr,"\a\n*** Illegal overlay image kind=%d! ***\n",tim->kind) ;
     mri_free(tim) ; RETURN( NULL );
   }

   ovim = mri_flippo( ISQ_TO_MRI_ROT(seq->opt.rot),seq->opt.mirror,tim ) ;
   if( tim != ovim ) mri_free(tim) ;
   RETURN( ovim );
}

/*---------------------------------------------------------------------------
   Routine to make a montage of images
   (version of ISQ_make_image when more than one is needed).
-----------------------------------------------------------------------------*/

void ISQ_make_montage( MCW_imseq *seq )
{
   MRI_IMAGE *im , *ovim , *tim ;
   Boolean reset_done = False ;
   float fac , wmm , hmm ;
   short gap_ov ;

   byte  gap_rgb[3] ;  /* 11 Feb 1999 */
   void  *gapval ;
   int   isrgb ;
   int   isrgb_ov ;    /* 07 Mar 2001 */

ENTRY("ISQ_make_montage");

   if( ! ISQ_VALID(seq) ) EXRETURN ;

   KILL_2XIM( seq->given_xim , seq->sized_xim ) ;  /* erase the XImages */

   if( seq->mplot != NULL ){                           /* 19 Sep 2001 */
     delete_memplot( seq->mplot ) ; seq->mplot = NULL ;
   }

   /*-- process toggled options that affect the image that may be stored --*/

   if( seq->opt.rot         != seq->old_opt.rot         ||
       seq->opt.mirror      != seq->old_opt.mirror      ||
       seq->opt.scale_group != seq->old_opt.scale_group ||
       seq->opt.scale_range != seq->old_opt.scale_range ||
       seq->mont_nx         != seq->mont_nx_old         ||
       seq->mont_ny         != seq->mont_ny_old         ||
       seq->mont_skip       != seq->mont_skip_old         ){

      KILL_1MRI( seq->imim ) ;  /* must re-get image for new processing */
      KILL_1MRI( seq->ovim ) ;
   }

   /*--- set the image to process ---*/

   im = seq->imim ;

   if( im == NULL ){
      float new_width_mm = 0.0 , new_height_mm = 0.0 ;
      int   nxim = 0 , nyim = 0 , nxyim = 0 ;
      int ij , nim , nmont = seq->mont_nx * seq->mont_ny , ijcen ;
      MRI_IMARR * mar ;

      INIT_IMARR(mar) ;

      /** Compute ijcen = montage index of subimage that will
                          be the "center" (crosshairs, etc.).
          N.B.: If the algorithm for this is changed here, it
                must be changed in a number of other places,
                including the AFNI multiple crosshairs code! **/

      isrgb = 0 ;
      ijcen = (seq->mont_nx)/2 + (seq->mont_ny/2) * seq->mont_nx ;
      for( ij=0 ; ij < nmont ; ij++ ){
         nim = seq->im_nr + (seq->mont_skip + 1)* (ij - ijcen) ;

DPRI(" Getting montage underlay",nim) ;

         seq->set_orim = (seq->need_orim != 0 && nim == seq->im_nr) ;  /* 30 Dec 1998 */
         tim = ISQ_manufacture_one( nim , 0 , seq ) ;
         seq->set_orim = 0 ;                                           /* 30 Dec 1998 */
         ADDTO_IMARR(mar,tim) ;

         if( nim == seq->im_nr ){
            new_width_mm  = IM_WIDTH(tim)  ; nxim = tim->nx ;
            new_height_mm = IM_HEIGHT(tim) ; nyim = tim->ny ;
            seq->last_image_type = tim->kind ;
            seq->barbot = seq->clbot ; /* 29 Jul 2001 */
            seq->bartop = seq->cltop ;
            ISQ_set_barhint(seq,"Focus") ;
            seq->last_dx = fabs(tim->dx) ; seq->last_dy = fabs(tim->dy) ;
         }

         if( tim != NULL ){
            isrgb = isrgb || (tim != NULL && tim->kind == MRI_rgb) ;
            nxyim++ ;
         }
      }

      if( nxyim == 0 ){                                        /* bad bad bad bad bad */
         fprintf(stderr,"** Montage error: no images found!\n") ;
         DESTROY_IMARR(mar) ; EXRETURN ;
      }

DPRI(" Making underlay cat2D from",nxyim) ;

      if( isrgb ){                       /* 11 Feb 1999 */
         if( seq->mont_gapcolor > 0 )
            DC_pixel_to_rgb( seq->dc , seq->dc->ovc->pix_ov[seq->mont_gapcolor],
                             gap_rgb , gap_rgb+1 , gap_rgb+2 ) ;
         else
            gap_rgb[0] = gap_rgb[1] = gap_rgb[2] = 0 ;

         gapval = (void *) gap_rgb ;
      } else {
         gap_ov = -(seq->mont_gapcolor) ;  /* negative ==> overlay palette */
         gapval = (void *) &gap_ov ;
      }

      /* 17 Feb 1999: if any are rgb, must convert all to that format */

      if( isrgb ){
         for( ij=0 ; ij < nmont ; ij++ ){
            tim = IMARR_SUBIMAGE(mar,ij) ;
            if( tim != NULL && tim->kind != MRI_rgb ){
               MRI_IMAGE * qim ;

               if( tim->kind == MRI_short )
                  qim = ISQ_index_to_rgb( seq->dc , 0 , tim ) ; /* 07 Mar 2001 */
               else
                  qim = mri_to_rgb( tim ) ;                     /* the old way */

               mri_free(tim) ;                   /* replace in image array */
               IMARR_SUBIMAGE(mar,ij) = qim ;
            }
         }
      }

      /* put them all together into one honking image (short or rgb)! */

      seq->imim = im = mri_cat2D( seq->mont_nx , seq->mont_ny ,     /* save this */
                                  seq->mont_gap , gapval , mar ) ;  /* underlay  */

STATUS("Destroying underlay image array") ;

      DESTROY_IMARR(mar) ;

      /* fix window dimensions if individual image size is different */

      seq->horig = nxim ; seq->vorig = nyim ;

      wmm = ( nxim*seq->mont_nx + seq->mont_gap*(seq->mont_nx-1) )
           / (float) nxim ;

      hmm = ( nyim*seq->mont_ny + seq->mont_gap*(seq->mont_ny-1) )
           / (float) nyim ;

      fac = sqrt( wmm / hmm ) ;

      new_width_mm  *= fac ;
      new_height_mm /= fac ;

      if( FLDIF(new_width_mm ,seq->last_width_mm ) ||
          FLDIF(new_height_mm,seq->last_height_mm)   ){

         ISQ_reset_dimen( seq , new_width_mm , new_height_mm ) ;
         reset_done = True ;
      }
   }

   /** at this point, im contains the underlay image, which may be short or rgb **/

   if( seq->opt.free_aspect != seq->old_opt.free_aspect && !reset_done )
      ISQ_reset_dimen( seq , seq->last_width_mm , seq->last_height_mm ) ;

   /*--- set the overlay to process ---*/

   if( ISQ_SKIP_OVERLAY(seq) ){
      KILL_1MRI( seq->ovim ) ; ovim = NULL ;  /* that was easy */
   } else {
      int ij , nim , nmont=seq->mont_nx * seq->mont_ny , nov=0 , ijcen ;

      MEM_plotdata *mp ; /* 19 Sep 2001 */
      int ii,jj ;
      float sx,sy,st , xb,xt,yb,yt , tx,ty ;

      ijcen = (seq->mont_nx)/2 + (seq->mont_ny/2) * seq->mont_nx ;

      /*--- get overlay images and montage them, if needed ---*/

      ovim = seq->ovim ;
      if( ovim == NULL ){
         MRI_IMARR * mar ;

         INIT_IMARR(mar) ;

         isrgb_ov = 0 ;  /* 07 Mar 2001 */

         for( ij=0 ; ij < nmont ; ij++ ){
            nim = seq->im_nr + (seq->mont_skip + 1) * (ij - ijcen) ;

DPRI(" Getting montage overlay",nim) ;

            tim = ISQ_manufacture_one( nim , 1 , seq ) ;
            ADDTO_IMARR(mar,tim) ;
            if( tim != NULL ){
               nov++ ; isrgb_ov = isrgb_ov || tim->kind == MRI_rgb ;
            }
         }

DPRI(" Making overlay cat2D from",nov) ;

         /* 07 Mar 2001: deal with possible RGB overlays */

         if( isrgb_ov ){
            for( ij=0 ; ij < nmont ; ij++ ){
               tim = IMARR_SUBIMAGE(mar,ij) ;
               if( tim != NULL && tim->kind != MRI_rgb ){
                  MRI_IMAGE * qim ;

                  if( tim->kind == MRI_short )
                     qim = ISQ_index_to_rgb( seq->dc , 1 , tim ) ; /* 07 Mar 2001 */
                  else
                     qim = mri_to_rgb( tim ) ;                     /* the old way */

                  mri_free(tim) ;                   /* replace in image array */
                  IMARR_SUBIMAGE(mar,ij) = qim ;
               }
            }
         }

         if( isrgb_ov ){
            gap_rgb[0] = gap_rgb[1] = gap_rgb[2] = 0 ;
            gapval = (void *) gap_rgb ;
         } else {
            gap_ov = 0 ;
            gapval = (void *) &gap_ov ;
         }

         if( nov > 0 ){
            ovim = seq->ovim =                                /* save this */
               mri_cat2D( seq->mont_nx , seq->mont_ny ,       /* overlay   */
                          seq->mont_gap , gapval ,  mar ) ;
         } else
            ovim = seq->ovim = NULL ;                         /* nothing */

STATUS("Destroying overlay image array") ;

         DESTROY_IMARR( mar ) ;
      }

      /*--- 19 Sep 2001: make overlay line plots for image? ---*/

      /*-- get sub-plots for each sub-image,
           merge into a superplot for the montage --*/

      if( MCW_val_bbox(seq->wbar_plots_bbox) != 0 ){
       for( ij=0 ; ij < nmont ; ij++ ){

         nim = seq->im_nr + (seq->mont_skip + 1) * (ij - ijcen) ;
         if( seq->mont_periodic ){
            while( nim < 0 )                       nim += seq->status->num_total ;
            while( nim >= seq->status->num_total ) nim -= seq->status->num_total ;
         } else {
            if( nim < 0 || nim >= seq->status->num_total ) continue ; /* skip */
         }

         mp = ISQ_getmemplot( nim , seq ) ;

         if( mp == NULL ) continue ; /* skip */

         ii = ij % seq->mont_nx ;  /* sub-image x index in montage */
         jj = ij / seq->mont_nx ;  /* sub-image y index in montage */

         tx = im->nx ; ty = im->ny ;  /* size of underlay image */

         /* sub-image is inside (xb..xt) X (yb..yt) in
            plot coordinates -- y is down-to-up,
            whereas image coordinates run y is up-to-down */

         xb = (seq->horig + seq->mont_gap) * ii ;
         xt = xb + seq->horig ;
         yb = (seq->vorig + seq->mont_gap) * (seq->mont_ny - 1 - jj) ;
         yt = yb + seq->vorig ;

         /* scale factors to put this sub-plot
            in the correct place in the montage */

         sx = (xt-xb) / tx ; tx = xb / tx ;
         sy = (yt-yb) / ty ; ty = yb / ty ;  st = sqrt(sx*sy) ;

         /* rotate/flip to same orientation as sub-image */

         flip_memplot( ISQ_TO_MRI_ROT(seq->opt.rot),seq->opt.mirror, mp ) ;

         /* scale to correct location as sub-image */

         scale_memplot( sx,tx , sy,ty , st , mp ) ;

         /* attach to superplot */

         if( seq->mplot == NULL ){  /* make 1st one the superplot */
           seq->mplot = mp ;
         } else {                  /* attach later ones to superplot */
           append_to_memplot( seq->mplot , mp ) ;
           delete_memplot( mp ) ;
         }

       } /* end of loop over sub-images' sub-plots */
      } /* end of if over whether to plot the plot */

      /*--- 20 Sep 2001: plot labels ---*/

      if( seq->wbar_label_av->ival != 0 ){
       char *lab ;

       for( ij=0 ; ij < nmont ; ij++ ){

         nim = seq->im_nr + (seq->mont_skip + 1) * (ij - ijcen) ;
         if( seq->mont_periodic ){
            while( nim < 0 )                       nim += seq->status->num_total ;
            while( nim >= seq->status->num_total ) nim -= seq->status->num_total ;
         } else {
            if( nim < 0 || nim >= seq->status->num_total ) continue ; /* skip */
         }

         /*- get label string -*/

         lab = ISQ_getlabel( nim , seq ) ;
         if( lab != NULL ){
          mp = ISQ_plot_label( seq , lab ) ;  /* plot it */
          if( mp != NULL ){
           ii = ij % seq->mont_nx ;  /* sub-image x index in montage */
           jj = ij / seq->mont_nx ;  /* sub-image y index in montage */
           tx = im->nx ; ty = im->ny ;  /* size of underlay image */
           xb = (seq->horig + seq->mont_gap) * ii ;
           xt = xb + seq->horig ;
           yb = (seq->vorig + seq->mont_gap) * (seq->mont_ny - 1 - jj) ;
           yt = yb + seq->vorig ;
           sx = (xt-xb) / tx ; tx = xb / tx ;
           sy = (yt-yb) / ty ; ty = yb / ty ;  st = sqrt(sx*sy) ;
           scale_memplot( sx,tx , sy,ty , st , mp ) ;
           if( seq->mplot != NULL ){
             append_to_memplot( seq->mplot , mp ) ; delete_memplot( mp ) ;
           } else {
             seq->mplot = mp ;
           }
          }
          free(lab) ;
         }
       } /* end of loop over sub-images */
      } /* end of plot labels */

   } /* end of making overlay stuff */

   /*--- set old_opt to current options ---*/

   seq->old_opt = seq->opt ;

   seq->mont_nx_old        = seq->mont_nx        ;
   seq->mont_ny_old        = seq->mont_ny        ;
   seq->mont_skip_old      = seq->mont_skip      ;
   seq->mont_gap_old       = seq->mont_gap       ;
   seq->mont_gapcolor_old  = seq->mont_gapcolor  ;

   /*--- overlay ovim onto im, producing tim, if needed ---*/

   if( ovim == NULL || ISQ_SKIP_OVERLAY(seq) ){   /* no processing of overlay */
      tim = im ;

#if 1                                  /** 07 Mar 2001 **/
   } else {

      tim = ISQ_overlay( seq->dc, im, ovim, seq->ov_opacity ) ;
      if( tim == NULL ) tim = im ;     /* shouldn't happen */

#else                                  /** the old way **/
   } else if( im->kind == MRI_short ){            /* process overlay onto shorts */

      register short * tar , * oar , * iar ;
      register int ii , npix = im->nx * im->ny ;

      tim = mri_new( im->nx , im->ny , MRI_short ) ;
      tar = MRI_SHORT_PTR( tim ) ;
      oar = MRI_SHORT_PTR( ovim ) ;
      iar = MRI_SHORT_PTR( im ) ;
      (void) memcpy( tar , iar , sizeof(short)*npix ) ; /* this code assumes   */
      for( ii=0 ; ii < npix ; ii++ )                    /* that relatively few */
         if( oar[ii] > 0 ) tar[ii] = -oar[ii] ;         /* pixels are overlaid */

   } else if( im->kind == MRI_rgb ){                       /* 11 Feb 1999 */

      register int ii , npix = im->nx * im->ny ;
      register short * oar = MRI_SHORT_PTR(ovim) ;
      register byte * tar , * iar = MRI_RGB_PTR(im) ;
      register Pixel * negpix = seq->dc->ovc->pix_ov ;

      tim = mri_to_rgb( im ) ; tar = MRI_RGB_PTR(tim) ;

      for( ii=0 ; ii < npix ; ii++ )
         if( oar[ii] > 0 )
            DC_pixel_to_rgb( seq->dc, negpix[oar[ii]], tar+(3*ii),tar+(3*ii+1),tar+(3*ii+2) ) ;
#endif
   }

   /*--- convert result to XImage for display ---*/

   seq->given_xim = mri_to_XImage( seq->dc , tim ) ;

   if( tim != im ) KILL_1MRI(tim) ;
   EXRETURN ;
}

/*----------------------------------------------------------------------
    Given a pair of coordinates in the image window, find the original
    coordinates they come from in the image, allowing for rotations,
    mirroring, and montaging.  This new version (April 1996) also
    returns the image number that the coordinates occurred in, since
    that may vary with montaging.

    12 Mar 2002: modified to allow for possibility of zoom
    12 Jun 2002: allow for cropping
-------------------------------------------------------------------------*/

void ISQ_mapxy( MCW_imseq * seq, int xwin, int ywin,
                int * xim, int * yim, int * nim )
{
   int win_wide,win_high , nxim,nyim ;
   int monx,mony,monsk,mongap , win_wide_orig,win_high_orig ;
   int xorg , yorg , ijcen , xcol,yrow , ij ;
   int zlev = seq->zoom_fac ;

ENTRY("ISQ_mapxy") ;

   if( ! ISQ_REALZ(seq) ) EXRETURN ;

   nxim  = seq->horig     ; nyim   = seq->vorig    ;  /* sizes of original images */
   monx  = seq->mont_nx   ; mony   = seq->mont_ny  ;  /* montage layout parameters */
   monsk = seq->mont_skip ; mongap = seq->mont_gap ;

   win_wide_orig = nxim * monx + mongap * (monx-1) ;  /* un-resized (original) */
   win_high_orig = nyim * mony + mongap * (mony-1) ;  /* displayed image sizes */

   /* get actual (display) image sizes */

   if( seq->wimage_width <= 0 ){
      MCW_widget_geom( seq->wimage , &win_wide , &win_high , NULL,NULL ) ;
      seq->wimage_width  = win_wide ;
      seq->wimage_height = win_high ;
   } else {
      win_wide = seq->wimage_width ;
      win_high = seq->wimage_height ;
   }

   /* convert actual coordinates input to
      equivalent coordinates in the original (montaged) image */

#if 0   /* old code, without zoom */
   xorg = ( (float) xwin / win_wide ) * win_wide_orig /* + 0.49 */ ;
   yorg = ( (float) ywin / win_high ) * win_high_orig /* + 0.49 */ ;
#else

   /* conversion if zoom is not on */

   if( zlev == 1 || monx > 1 || mony > 1 ){

     xorg = ( (float) xwin / win_wide ) * win_wide_orig /* + 0.49 */ ;
     yorg = ( (float) ywin / win_high ) * win_high_orig /* + 0.49 */ ;

   } else {  /* conversion if zoom is on (only in 1x1 montages) */

     int pw=seq->zoom_pw , ph=seq->zoom_ph ;
     float xoff,yoff ;

     xoff = seq->zoom_hor_off*pw; if( xoff+win_wide > pw ) xoff = pw-win_wide;
     yoff = seq->zoom_ver_off*ph; if( yoff+win_high > ph ) yoff = ph-win_high;

     xorg = nxim * (xoff+xwin) / pw ;
     yorg = nyim * (yoff+ywin) / ph ;
   }
#endif

   /* compute the coordinates within the sub-image (*xim and *yim),
      and the grid column and row number of the sub-image (xcol,yrow) */

   *xim = xorg % (nxim+mongap) ; xcol = xorg / (nxim+mongap) ;
   *yim = yorg % (nyim+mongap) ; yrow = yorg / (nyim+mongap) ;

   /* compute the image number in the sequence that (xcol,yrow)
      came from, using the same algorithm as in ISQ_make_montage */

   ij    = xcol   + yrow     * monx ;
   ijcen = monx/2 + (mony/2) * monx ;
   *nim  = seq->im_nr + (monsk+1) * (ij-ijcen) ;

   if( seq->mont_periodic ){
      while( *nim < 0 )                       *nim += seq->status->num_total ;
      while( *nim >= seq->status->num_total ) *nim -= seq->status->num_total ;
   }

   /* flip the (xim,yim) coordinates in case the stupid user used
      one of the rotate or mirror buttons in the "Disp" control box */

   ISQ_flipxy( seq , xim , yim ) ;

   if( seq->cropit ){       /* 12 Jun 2002: allow for cropping */
     *xim += seq->crop_xa ;
     *yim += seq->crop_ya ;
   }

   EXRETURN ;
}

/*---------------------------------------------------------------------
   Inputs: xflip,yflip = pointers to coordinates in the flipped image
   Output: xflip,yflip = values are changed to original image coords

   Note that these coordinates are relative to original (un-resized)
   image dimensions.
-----------------------------------------------------------------------*/

void ISQ_flipxy( MCW_imseq * seq, int * xflip, int * yflip )
{
   int fopt , xim , yim , nx,ny ;

ENTRY("ISQ_flipxy") ;

   fopt = ISQ_TO_MRI_ROT(seq->opt.rot) ;
   if( seq->opt.mirror ) fopt += MRI_FLMADD ;

   nx = seq->horig ; ny = seq->vorig ;

   switch( fopt ){

      default:                                    /* ROT_0, no mirror */
      case (MRI_ROT_0):
         xim = *xflip ; yim = *yflip ; break ;

      case (MRI_ROT_90):                          /* ROT_90, no mirror */
         xim = ny-1-*yflip ; yim = *xflip ; break ;

      case (MRI_ROT_180):                         /* ROT_180, no mirror */
         xim = nx-1-*xflip ; yim = ny-1-*yflip ; break ;

      case (MRI_ROT_270):                         /* ROT_270, no mirror */
         xim = *yflip ; yim = nx-1-*xflip ; break ;

      case (MRI_ROT_0+MRI_FLMADD):                /* ROT_0, mirror */
         xim = nx-1-*xflip ; yim = *yflip ; break ;

      case (MRI_ROT_90+MRI_FLMADD):               /* ROT_90, mirror */
         xim = ny-1-*yflip ; yim = nx-1-*xflip ; break ;

      case (MRI_ROT_180+MRI_FLMADD):              /* ROT_180, mirror */
         xim = *xflip ; yim = ny-1-*yflip ; break ;

      case (MRI_ROT_270+MRI_FLMADD):              /* ROT_270, mirror */
         xim = *yflip ; yim = *xflip ; break ;
   }

   *xflip = xim ; *yflip = yim ; EXRETURN ;
}

/*-------------------------------------------------------------------*/

void ISQ_unflipxy( MCW_imseq * seq, int * xflip, int * yflip )
{
   int fopt , xim , yim , nx,ny ;

ENTRY("ISQ_unflipxy") ;

   fopt = ISQ_TO_MRI_ROT(seq->opt.rot) ;
   if( seq->opt.mirror ) fopt += MRI_FLMADD ;

   nx = seq->horig ; ny = seq->vorig ;

   switch( fopt ){

      default:                                    /* ROT_0, no mirror */
      case (MRI_ROT_0):
         xim = *xflip ; yim = *yflip ; break ;

      case (MRI_ROT_90):                          /* ROT_90, no mirror */
         yim = ny-1-*xflip ; xim = *yflip ; break ;

      case (MRI_ROT_180):                         /* ROT_180, no mirror */
         xim = nx-1-*xflip ; yim = ny-1-*yflip ; break ;

      case (MRI_ROT_270):                         /* ROT_270, no mirror */
         yim = *xflip ; xim = nx-1-*yflip ; break ;

      case (MRI_ROT_0+MRI_FLMADD):                /* ROT_0, mirror */
         xim = nx-1-*xflip ; yim = *yflip ; break ;

      case (MRI_ROT_90+MRI_FLMADD):               /* ROT_90, mirror */
         yim = ny-1-*xflip ; xim = nx-1-*yflip ; break ;

      case (MRI_ROT_180+MRI_FLMADD):              /* ROT_180, mirror */
         xim = *xflip ; yim = ny-1-*yflip ; break ;

      case (MRI_ROT_270+MRI_FLMADD):              /* ROT_270, mirror */
         xim = *yflip ; yim = *xflip ; break ;
   }

   *xflip = xim ; *yflip = yim ; EXRETURN ;
}

/*-----------------------------------------------------------------------------
   Routines to handle transformations of an image.
-------------------------------------------------------------------------------*/

char * ISQ_transform_label( MCW_arrowval * av , XtPointer cd )
{
   MCW_function_list * xforms = (MCW_function_list *) cd ;

   if( av == NULL    || xforms == NULL        ||
       av->ival <= 0 || av->ival > xforms->num  ) return "-none-" ;

   return xforms->labels[av->ival - 1] ;  /* label for each function */
}

/*-----------------------------------------------------------------------------*/

void ISQ_transform_CB( MCW_arrowval * av , XtPointer cd )
{
   MCW_imseq * seq = (MCW_imseq *) cd ;

ENTRY("ISQ_transform_CB") ;

   if( ! ISQ_VALID(seq) ) EXRETURN ;

   /** set the 0D transform function pointer **/

   if( av != NULL && av == seq->transform0D_av ){
      if( seq->status->transforms0D == NULL || av->ival <= 0 ||
          av->ival > seq->status->transforms0D->num            ){

         seq->transform0D_func  = NULL ;  /* no transform */
         seq->transform0D_index = 0 ;
      } else {
         seq->transform0D_func  = seq->status->transforms0D->funcs[av->ival - 1] ;
         seq->transform0D_index = av->ival ;

         /* 21 Jul 2003: do initializing func call, if present */

         if( seq->status->transforms0D->func_init[av->ival-1] != NULL )
          seq->status->transforms0D->func_init[av->ival-1]() ;

      }
   }

   /** set the 2D transform function pointer **/

   if( av != NULL && av == seq->transform2D_av ){
      if( seq->status->transforms2D == NULL || av->ival <= 0 ||
          av->ival > seq->status->transforms2D->num            ){

         seq->transform2D_func  = NULL ;  /* no transform */
         seq->transform2D_index = 0 ;
      } else {
         seq->transform2D_func  = seq->status->transforms2D->funcs[av->ival - 1] ;
         seq->transform2D_index = av->ival ;

         /* 21 Jul 2003: do initializing func call, if present */

         if( seq->status->transforms2D->func_init[av->ival-1] != NULL )
          seq->status->transforms2D->func_init[av->ival-1]() ;
      }
   }

   ISQ_redisplay( seq , -1 , isqDR_reimage ) ;  /* redo current image */
   EXRETURN ;
}

/*-----------------------------------------------------------------------------*/

void ISQ_slice_proj_CB( MCW_arrowval * av , XtPointer cd )
{
   MCW_imseq * seq = (MCW_imseq *) cd ;

ENTRY("ISQ_slice_proj_CB") ;

   if( ! ISQ_VALID(seq) ) EXRETURN ;

   /** set the slice_proj function pointer **/

   if( av != NULL && av == seq->slice_proj_av ){
      if( seq->status->slice_proj == NULL || av->ival <= 0 ||
          av->ival > seq->status->slice_proj->num            ){

         seq->slice_proj_func  = NULL ;  /* no slice_proj */
         seq->slice_proj_index = 0 ;
      } else {
         seq->slice_proj_func  = (float_func *)
                                 seq->status->slice_proj->funcs[av->ival - 1] ;
         seq->slice_proj_index = av->ival ;
      }
   }

   seq->slice_proj_range = seq->slice_proj_range_av->ival ;

   ISQ_redisplay( seq , -1 , isqDR_reimage ) ;  /* redo current image */
   EXRETURN ;
}

/*--------------------------------------------------------------------------
   30 Dec 1998:  Handle the row graphs
----------------------------------------------------------------------------*/

char * ISQ_rowgraph_label( MCW_arrowval * av , XtPointer cd )
{
   static char buf[16] ;
   sprintf(buf,"%2d  ",av->ival) ;
   return buf ;
}

/*--------------------------------------------------------------------------*/

void ISQ_rowgraph_CB( MCW_arrowval * av , XtPointer cd )
{
   MCW_imseq * seq = (MCW_imseq *) cd ;

ENTRY("ISQ_rowgraph_CB") ;

   if( ! ISQ_VALID(seq) ) EXRETURN ;               /* bad input */
   if( av->ival == seq->rowgraph_num ) EXRETURN ;  /* nothing changed */

   seq->rowgraph_num = av->ival ;

   if( seq->rowgraph_num > 0 ) seq->need_orim |=  ROWGRAPH_MASK ;
   else                        seq->need_orim &= ~ROWGRAPH_MASK ;
   if( seq->need_orim == 0 ) KILL_1MRI(seq->orim) ;

   ISQ_redisplay( seq , -1 , isqDR_reimage ) ;  /* redo current image */
   EXRETURN ;
}

/*--------------------------------------------------------------------------*/

void ISQ_rowgraph_draw( MCW_imseq *seq )
{
   MEM_plotdata *mp ;
   ISQ_cbs cbs ;
   int jbot,ix,jy , nrow , jj , nx,ny , ymask ;
   float *yar[ROWGRAPH_MAX] ;

ENTRY("ISQ_rowgraph_draw") ;

   if( ! ISQ_REALZ(seq) ) EXRETURN ;  /* error */

   /* marked for no graphs? */

   if( seq->rowgraph_num == 0 ){
     if( seq->rowgraph_mtd != NULL ){
       plotkill_topshell( seq->rowgraph_mtd ) ;
       seq->rowgraph_mtd = NULL ;
     }
     EXRETURN ;
   }

   if( seq->orim == NULL ) EXRETURN ;

   /* find current location */

   cbs.reason = isqCR_getxynim ;
   cbs.xim = cbs.yim = cbs.nim = -666 ;
   if( seq->status->send_CB != NULL )
#if 0
     seq->status->send_CB( seq , seq->getaux , &cbs ) ;
#else
     SEND(seq,cbs) ;
#endif
   if( cbs.xim < 0 || cbs.yim < 0 ){
     ERROR_message("in ISQ_rowgraph_draw: xim=%d yim=%d",cbs.xim,cbs.yim) ;
     EXRETURN ;  /* bad result */
   }
   ISQ_unflipxy( seq , &(cbs.xim) , &(cbs.yim) ) ;
   jy = jbot = cbs.yim ; ix = cbs.xim ;

   /* get pointers to data rows */

   if( jbot < 0 || jbot >= seq->orim->ny ){
      ERROR_message("in ISQ_rowgraph_draw: jbot=%d",jbot) ;
      EXRETURN ;  /* no data? */
   }

   nrow = MIN( seq->rowgraph_num  , jbot+1 ) ;
   nx   = seq->orim->nx ;
   ny   = seq->orim->ny ;

   for( jj=0 ; jj < nrow ; jj++ )
     yar[jj] = MRI_FLOAT_PTR(seq->orim) + (jbot-jj)*nx ;

   /* make a plot in memory */

   ymask = TSP_SEPARATE_YBOX ;

   mp = plot_ts_mem( nx , NULL , nrow,ymask,yar , "Column (pixels)",NULL,NULL,NULL ) ;
   if( mp == NULL ){
      ERROR_message("in ISQ_rowgraph_draw: can't make plot_ts_mem") ;
      EXRETURN ;  /* error */
   }

   /*-- plot a * at the selected point (if it is in range) --*/

   if( !ISQ_SKIP_OVERLAY(seq) && ix >= 0 && ix < nx && jy >= 0 && jy < ny ){
      float xx , yy , dx , dy , xbot,xtop, ybot,ytop ;

      xx = ix ; dx = 0.016 * nx ; yy = yar[0][ix] ;
#if 0
      ybot = ytop = yar[0][0] ;
      for( jj=1 ; jj < nx ; jj++ )
              if( yar[0][jj] < ybot ) ybot = yar[0][jj] ;
         else if( yar[0][jj] > ytop ) ytop = yar[0][jj] ;
      dy = 0.016 * nrow * (ytop-ybot) ;
#else
      plotpak_getset( NULL,NULL,NULL,NULL , &xbot,&xtop , &ybot,&ytop ) ;
      dx = 0.016 * fabs(xtop-xbot) ;
      dy = 0.016 * fabs(ytop-ybot) * nrow ;
#endif

#undef  THIK
#define THIK 0.003

      set_color_memplot( 0.8 , 0.0 , 0.2 ) ;
      set_thick_memplot( THIK ) ;
      plotpak_line( xx-dx , yy    , xx+dx , yy    ) ; /* - stroke */
      plotpak_line( xx    , yy-dy , xx    , yy+dy ) ; /* | stroke */
      plotpak_line( xx-dx , yy-dy , xx+dx , yy+dy ) ; /* / stroke */
      plotpak_line( xx+dx , yy-dy , xx-dx , yy+dy ) ; /* \ stroke */
      set_color_memplot( 0.2 , 0.0 , 0.8 ) ;
      plotpak_line( xx+dx , yy-dy , xx+dx , yy+dy ) ; /* box around outside */
      plotpak_line( xx+dx , yy+dy , xx-dx , yy+dy ) ;
      plotpak_line( xx-dx , yy+dy , xx-dx , yy-dy ) ;
      plotpak_line( xx-dx , yy-dy , xx+dx , yy-dy ) ;
      set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
      set_thick_memplot( 0.0 ) ;
   }

   /* if there is a plot window open, plot into it, otherwise open a new window */

   if( seq->rowgraph_mtd != NULL ){

      MTD_replace_plotdata( seq->rowgraph_mtd , mp ) ;
      redraw_topshell( seq->rowgraph_mtd ) ;

   } else {  /* make a new plot window */

      seq->rowgraph_mtd = memplot_to_topshell( seq->dc->display, mp,
                                               ISQ_rowgraph_mtdkill ) ;

      if( seq->rowgraph_mtd == NULL ){ delete_memplot( mp ); EXRETURN; }

      seq->rowgraph_mtd->userdata = (void *) seq ;
   }

   EXRETURN ;
}

/*-----------------------------------------------------------------------*/
/*! This function is called when then rowgraph_mtd is killed.            */

void ISQ_rowgraph_mtdkill( MEM_topshell_data * mp )
{
   MCW_imseq * seq ;

ENTRY("ISQ_rowgraph_mtdkill") ;

   if( mp == NULL ) EXRETURN ;
   seq = (MCW_imseq *) mp->userdata ; if( ! ISQ_VALID(seq) ) EXRETURN ;

   seq->rowgraph_mtd = NULL ;

   AV_assign_ival( seq->rowgraph_av , 0 ) ;
   seq->rowgraph_num = 0 ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------*/
/*! This function is called when the graymap_mtd is killed.              */

void ISQ_graymap_mtdkill( MEM_topshell_data *mp )  /* 24 Oct 2003 */
{
   MCW_imseq *seq ;

ENTRY("ISQ_graymap_mtdkill") ;

   if( mp == NULL ) EXRETURN ;
   seq = (MCW_imseq *) mp->userdata ;
   if( ISQ_VALID(seq) ){
     seq->graymap_mtd = NULL ;
     seq->need_orim &= ~GRAYMAP_MASK ;  /* turn off need for orim for graymap */
   }

   EXRETURN ;
}

/*-----------------------------------------------------------------------*/

void ISQ_graymap_draw( MCW_imseq *seq )  /* 24 Oct 2003 */
{
   MEM_plotdata *mp ;
   int ix , nx , ny , nxx ;
   float *yar[2] , *xar , dx , *ar ;

ENTRY("ISQ_graymap_draw") ;

   if( !ISQ_REALZ(seq) || seq->dc->use_xcol_im ) EXRETURN ;  /* error */

   seq->need_orim |= GRAYMAP_MASK ;

   /* make float arrays with grayscales and data range */
   /* Modifed 12 Jan 2004 to plot in histogram style. */

   nx     = seq->dc->ncol_im ;
   nxx    = 2*nx+2 ;
   ny     = 1 ;
   dx     = (seq->bartop - seq->barbot) / nx ; if( dx == 0.0 ) EXRETURN ;
   yar[0] = (float *) malloc( sizeof(float)*nxx ) ;
   xar    = (float *) malloc( sizeof(float)*nxx ) ;
   xar[0] = seq->barbot ;
   for( ix=0 ; ix < nx ; ix++ ){
     xar[2*ix+1]     = seq->barbot + ix*dx ;
     xar[2*ix+2]     = seq->barbot + (ix+1)*dx ;
     yar[0][2*ix+1]  = seq->dc->xint_im[ix] ;
     if( yar[0][2*ix+1] < 0.0 ){
       yar[0][2*ix+1] = 0.0 ;
     } else {
       yar[0][2*ix+1] *= (255.0/65280.0);
       if( yar[0][2*ix+1] > 255.0 ) yar[0][2*ix+1] = 255.0;
     }
     yar[0][2*ix+2] = yar[0][2*ix+1] ;
   }
   xar[2*nx+1]    = seq->bartop ;
   yar[0][0]      = yar[0][1] ;
   yar[0][2*nx+1] = yar[0][2*nx] ;

   /* histogram the image? */

   if( seq->orim != NULL ){
     float *iar=MRI_FLOAT_PTR(seq->orim) , *har , val ;
     float scl=nx/(seq->bartop-seq->barbot) ; int ii,jj ;
     har = (float *) calloc( sizeof(float),nx  ) ;
     for( ii=0 ; ii < seq->orim->nvox ; ii++ ){
       jj = (int)( scl*(iar[ii]-seq->barbot) ) ;
       if( jj < 0 ) jj = 0 ; else if( jj > nx-1 ) jj = nx-1 ;
       har[jj] += 1.0 ;
     }
     for( scl=0.0,ii=1 ; ii < nx ; ii++ )
       if( har[ii] > scl ) scl = har[ii] ;
     if( scl > 0.0 ){
       ny = 2 ;
       yar[1] = (float *) malloc( sizeof(float)*nxx ) ;
       scl = 255.0/sqrt(scl) ;
       yar[1][0] = yar[1][2*nx+1] = 0.0 ;
       for( ii=0 ; ii < nx ; ii++ ){
         val = scl*sqrt(har[ii]) ; if( val > 255.0 ) val = 255.0 ;
         yar[1][2*ii+1] = yar[1][2*ii+2] = val ;
       }
     }
     free( (void *)har ) ;
   }

   /* make a plot in memory */

   mp = plot_ts_mem( nxx,xar, ny,0,yar, "Data Value",
                     (ny == 1) ? "GrayLevel"
                               : "GrayLevel\\red/Histogram\\black" ,
                     NULL,NULL ) ;
   free(xar); free(yar[0]); if( ny == 2 ) free(yar[1]) ;
   if( mp == NULL ){
     fprintf(stderr,"*** error in ISQ_graymap_draw: can't make plot_ts_mem\n") ;
     EXRETURN ;  /* error */
   }

   /* if there is a plot window open, plot into it, otherwise open a new window */

   if( seq->graymap_mtd != NULL ){

      MTD_replace_plotdata( seq->graymap_mtd , mp ) ;
      redraw_topshell( seq->graymap_mtd ) ;

   } else {  /* make a new plot window */

      seq->graymap_mtd = memplot_to_topshell( seq->dc->display, mp, ISQ_graymap_mtdkill ) ;
      if( seq->graymap_mtd == NULL ){ delete_memplot(mp); EXRETURN; }
      seq->graymap_mtd->userdata = (void *) seq ;
      ISQ_place_widget( seq->wtop , seq->graymap_mtd->top ) ;
   }

   EXRETURN ;
}

/*-----------------------------------------------------------------------
   21 Jan 1999: Handle the surface graph stuff
-------------------------------------------------------------------------*/

char * ISQ_surfgraph_label( MCW_arrowval * av , XtPointer cd )
{
   switch( av->ival ){
      case 0:  return "No"  ;
      case 1:  return "Yes" ;
      case 2:  return "Inv" ;
   }
   return "?*?" ;
}

/*-------- called when the user changes the SurfGraph menu button --------*/

void ISQ_surfgraph_CB( MCW_arrowval * av , XtPointer cd )
{
   MCW_imseq * seq = (MCW_imseq *) cd ;

ENTRY("ISQ_surfgraph_CB") ;

   if( ! ISQ_VALID(seq) ) EXRETURN ;                /* bad input */
   if( av->ival == seq->surfgraph_num ) EXRETURN ;  /* nothing changed */

   seq->surfgraph_num = av->ival ;

   if( seq->surfgraph_num > 0 ) seq->need_orim |=  SURFGRAPH_MASK ;
   else                         seq->need_orim &= ~SURFGRAPH_MASK ;
   if( seq->need_orim == 0 ) KILL_1MRI(seq->orim) ;

   ISQ_redisplay( seq , -1 , isqDR_reimage ) ;  /* redo current image */
   EXRETURN ;
}

/*---------------- called to redraw the surface graph -------------------*/

void ISQ_surfgraph_draw( MCW_imseq * seq )
{
   MEM_plotdata * mp ;
   ISQ_cbs cbs ;
   int ix , jy ;

ENTRY("ISQ_surfgraph_draw") ;

   if( ! ISQ_REALZ(seq) ) EXRETURN ;  /* error */

   /* marked for no graph? */

   if( seq->surfgraph_num == 0 ){
     if( seq->surfgraph_mtd != NULL ){
       plotkill_topshell( seq->surfgraph_mtd ) ;
       seq->surfgraph_mtd = NULL ;
     }
     EXRETURN ;
   }

   if( seq->orim == NULL ) EXRETURN ;

   /* find current location */

   if( ISQ_SKIP_OVERLAY(seq) ){
      ix = jy = -1 ;
   } else {
      cbs.reason = isqCR_getxynim ;
      cbs.xim = cbs.yim = cbs.nim = -666 ;
      if( seq->status->send_CB != NULL )
#if 0
         seq->status->send_CB( seq , seq->getaux , &cbs ) ;
#else
         SEND(seq,cbs) ;
#endif
      if( cbs.xim < 0 || cbs.yim < 0 ){
         ix = jy = -1 ;
      } else {
         ISQ_unflipxy( seq , &(cbs.xim) , &(cbs.yim) ) ;
         ix = cbs.xim ; jy = cbs.yim ;
      }
   }

   /* plot the data */

   mp = plot_image_surface( seq->orim , (seq->surfgraph_num == 2) ? -1.0 : 1.0 ,
                            seq->surfgraph_theta , seq->surfgraph_phi ,
                            ix , jy ) ;
   if( mp == NULL ) EXRETURN ;

   /* if there is a plot window open, plot into it, otherwise open a new window */

   if( seq->surfgraph_mtd != NULL ){

      MTD_replace_plotdata( seq->surfgraph_mtd , mp ) ;
      redraw_topshell( seq->surfgraph_mtd ) ;

   } else {  /* make a new plot window */

      seq->surfgraph_mtd = memplot_to_topshell( seq->dc->display, mp, ISQ_surfgraph_mtdkill ) ;

      if( seq->surfgraph_mtd == NULL ){ delete_memplot( mp ); EXRETURN; }

      seq->surfgraph_mtd->userdata = (void *) seq ;

      /* add an arrowpad to it (lower right corner) */

      seq->surfgraph_arrowpad = new_MCW_arrowpad( seq->surfgraph_mtd->form ,
                                                  ISQ_surfgraph_arrowpad_CB ,
                                                  (XtPointer) seq ) ;

      XtUnmanageChild( seq->surfgraph_arrowpad->wform ) ;

      XtVaSetValues( seq->surfgraph_arrowpad->wform ,
                        XmNbottomAttachment , XmATTACH_FORM ,
                        XmNrightAttachment  , XmATTACH_FORM ,
                        XmNleftAttachment   , XmATTACH_NONE ,
                        XmNtopAttachment    , XmATTACH_NONE ,
                        XmNwidth            , 60 ,
                        XmNheight           , 60 ,
                     NULL ) ;

      MCW_set_widget_bg( seq->surfgraph_arrowpad->wform , "white" , 0 ) ;

      XtManageChild( seq->surfgraph_arrowpad->wform ) ;

      seq->surfgraph_arrowpad->parent = (XtPointer) seq ;
      seq->surfgraph_arrowpad->fastdelay = MCW_AV_longdelay ;
   }

   EXRETURN ;
}

/*-----------------------------------------------------------*/
/*--- Called when the user kills the surface graph window ---*/

void ISQ_surfgraph_mtdkill( MEM_topshell_data * mp )
{
   MCW_imseq * seq ;

ENTRY("ISQ_surfgraph_mtdkill") ;

   if( mp == NULL ) EXRETURN ;
   seq = (MCW_imseq *) mp->userdata ; if( ! ISQ_VALID(seq) ) EXRETURN ;

   seq->surfgraph_mtd   = NULL ;
   seq->surfgraph_theta = DEFAULT_THETA  ;
   seq->surfgraph_phi   = DEFAULT_PHI ;
   myXtFree( seq->surfgraph_arrowpad ) ;

   seq->surfgraph_num = 0 ;
   AV_assign_ival( seq->surfgraph_av , 0 ) ;
   EXRETURN ;
}

/*--- actually draws an image to a wiremesh, in memory ---*/

MEM_plotdata * plot_image_surface( MRI_IMAGE * im , float fac ,
                                   float theta , float phi , int ix , int jy )
{
   MRI_IMAGE * fim , * qim ;
   MEM_plotdata * mp ;
   float * x , * y , * z ;
   float  dx ,  dy , zbot,ztop ;
   int ii , nx , ny , nxy ;
   char str[128] ;

ENTRY("plot_image_surface") ;

   if( im == NULL ) RETURN( NULL );

   /*-- setup to plot --*/

   nx = im->nx ; ny = im->ny ;
   if( nx < 3 || ny < 3 ) RETURN( NULL );

   create_memplot_surely( "imsurf" , 1.1 ) ;

   dx = im->dx ; if( dx <= 0.0 ) dx = 1.0 ;
   dy = im->dy ; if( dy <= 0.0 ) dy = 1.0 ;

   x = (float *) malloc( sizeof(float) * nx ) ;
   for( ii=0 ; ii < nx ; ii++ ) x[ii] = ii * dx ;

   y = (float *) malloc( sizeof(float) * ny ) ;
   for( ii=0 ; ii < ny ; ii++ ) y[ii] = ii * dy ;

   /*-- scale image data --*/

   qim = mri_flippo( MRI_ROT_180 , 1 , im ) ;
   if( fac == 1.0 || fac == 0.0 ) fim = mri_to_float(qim) ;
   else                           fim = mri_scale_to_float(fac,qim) ;
   z = MRI_FLOAT_PTR(fim) ; mri_free(qim) ;
   nxy = nx * ny ; zbot = ztop = z[0] ;
   for( ii=1 ; ii < nxy ; ii++ ){
           if( z[ii] < zbot ) zbot = z[ii] ;
      else if( z[ii] > ztop ) ztop = z[ii] ;
   }
   ztop = ztop - zbot ;
   if( ztop > 0.0 ){
      ztop = 0.85 * sqrt( x[nx-1] * y[ny-1] ) / ztop ;
      for( ii=0 ; ii < nxy ; ii++ ) z[ii] = (z[ii]-zbot) * ztop ;
   }

   /*-- plot surface --*/

   set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
   set_thick_memplot( 0.0 ) ;
   plotpak_srface( x , y , z , nx , ny , theta, phi ) ;

   /*-- plot a * at the selected point (if it is in range) --*/

   if( ix >= 0 && ix < nx && jy >= 0 && jy < ny ){
      real xi,yi,zi ; float xt,yt,zt , xtp,ytp,ztp ;
      ii = 1 ;
      xi = x[ix] ; yi = y[ny-1-jy] ; zi = z[ix+(ny-1-jy)*nx] ;
      (void) trn32s_( &xi , &yi , &zi ,
                      (real *)(&xt) , (real *)(&yt) , (real *)(&zt) ,
                      (integer *)(&ii) ) ;

#undef  THIK
#define THIK 0.003

      dx = 0.016 * x[nx-1] ; dy = 0.016 * y[ny-1] ; dx = MAX(dx,dy) ;
      xi = x[ix]+dx ; yi = y[ny-1-jy]+dx ; zi = z[ix+(ny-1-jy)*nx] ;
      (void) trn32s_( &xi , &yi , &zi ,
                      (real *)(&xtp) , (real *)(&ytp) , (real *)(&ztp) ,
                      (integer *)(&ii) ) ;
      dx = fabs(xtp-xt) ; dy = fabs(ytp-yt) ; dx = MAX(dx,dy) ;

      set_color_memplot( 0.8 , 0.0 , 0.2 ) ;
      set_thick_memplot( THIK ) ;
      plotpak_line( xt-dx , yt    , xt+dx , yt    ) ; /* - stroke */
      plotpak_line( xt    , yt-dx , xt    , yt+dx ) ; /* | stroke */
      plotpak_line( xt-dx , yt-dx , xt+dx , yt+dx ) ; /* / stroke */
      plotpak_line( xt+dx , yt-dx , xt-dx , yt+dx ) ; /* \ stroke */
      set_color_memplot( 0.2 , 0.0 , 0.8 ) ;
      plotpak_line( xt+dx , yt-dx , xt+dx , yt+dx ) ; /* box around outside */
      plotpak_line( xt+dx , yt+dx , xt-dx , yt+dx ) ;
      plotpak_line( xt-dx , yt+dx , xt-dx , yt-dx ) ;
      plotpak_line( xt-dx , yt-dx , xt+dx , yt-dx ) ;
      set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
      set_thick_memplot( 0.0 ) ;
   }

   free(x); free(y) ; mri_free(fim);

   plotpak_set( 0.0,1.0 , 0.0,1.0 , 0.0,1.0 , 0.0,1.0 , 1 ) ;
   sprintf(str,"\\theta=%.0f\\degree \\phi=%.0f\\degree",theta,phi) ;
   plotpak_pwritf( 1.099 , 0.97 , str, 19 , 0 , 1 ) ;

   mp = get_active_memplot() ; RETURN( mp );
}

/*--- called when the user presses a surface graph arrowpad button ---*/

void ISQ_surfgraph_arrowpad_CB( MCW_arrowpad * apad , XtPointer client_data )
{
   MCW_imseq * seq = (MCW_imseq *) client_data ;
   XButtonEvent * xev = (XButtonEvent *) &(apad->xev) ;
   float step = 10.0 ;

ENTRY("ISQ_surfgraph_arrowpad_CB") ;

   if( ! ISQ_REALZ(seq) ) EXRETURN ;  /* error */

   if( ( xev->type == ButtonPress || xev->type == ButtonRelease ) ){
      if( xev->state & (ShiftMask|ControlMask) ) step = 90.0 ; /* big step   */
      if( xev->state & Mod1Mask                ) step =  2.0 ; /* small step */
   }

   switch( apad->which_pressed ){
      case AP_MID:   seq->surfgraph_theta = DEFAULT_THETA ;
                     seq->surfgraph_phi   = DEFAULT_PHI   ; break ;

      case AP_DOWN:  seq->surfgraph_theta += step ; break ;
      case AP_UP:    seq->surfgraph_theta -= step ; break ;
      case AP_LEFT:  seq->surfgraph_phi   += step ; break ;
      case AP_RIGHT: seq->surfgraph_phi   -= step ; break ;

      default:                                   EXRETURN ; /* error */
   }

   while( seq->surfgraph_theta < 0.0    ) seq->surfgraph_theta += 360.0 ;
   while( seq->surfgraph_theta >= 360.0 ) seq->surfgraph_theta -= 360.0 ;

   while( seq->surfgraph_phi < 0.0    ) seq->surfgraph_phi += 360.0 ;
   while( seq->surfgraph_phi >= 360.0 ) seq->surfgraph_phi -= 360.0 ;

   ISQ_surfgraph_draw( seq ) ; EXRETURN ;
}

/*-----------------------------------------------------------------------
  24 Apr 2001: remove a widget from the onoff list,
               and permanently unmanage it (for the recorder)
-------------------------------------------------------------------------*/

void ISQ_remove_widget( MCW_imseq * seq , Widget w )
{
   int ii ;
ENTRY("ISQ_remove_onoff") ;

   if( !ISQ_VALID(seq) || w == NULL ) EXRETURN ;

   XtUnmanageChild( w ) ;  /* turn it off */

   for( ii=0 ; ii < seq->onoff_num ; ii++ ){     /* find in list */
     if( w == seq->onoff_widgets[ii] ){
       seq->onoff_widgets[ii] = NULL ;
       break ;
     }
   }

   for( ii=seq->onoff_num-1 ; ii > 0 ; ii-- ){   /* truncate list */
     if( seq->onoff_widgets[ii] == NULL )
       seq->onoff_num = ii ;
     else
       break ;
   }

   EXRETURN ;
}

/*-----------------------------------------------------------------------
  24 Apr 2001: recording button and accoutrements
-------------------------------------------------------------------------*/

void ISQ_record_button( MCW_imseq * seq )
{
   Widget rc , mbar , menu , cbut , wpar ;
   XmString xstr ;

ENTRY("ISQ_record_button") ;

   /*--- make the widgets ---*/

   /* rowcol to hold the menubar */

   seq->onoff_widgets[(seq->onoff_num)++] = seq->record_rc = rc =
     XtVaCreateWidget(
           "imseq" , xmRowColumnWidgetClass , seq->wform ,
              XmNorientation    , XmHORIZONTAL ,
              XmNpacking        , XmPACK_TIGHT ,

              LEADING_BOT       , XmATTACH_WIDGET              ,
              LEADING_WIDGET_BOT, seq->wbut_bot[NBUTTON_BOT-1] ,
              EDGING_BOT        , XmATTACH_FORM                ,

              XmNmarginWidth  , 1 ,
              XmNmarginHeight , 0 ,
              XmNmarginBottom , 0 ,
              XmNmarginTop    , 0 ,
              XmNmarginLeft   , 0 ,
              XmNmarginRight  , 0 ,
              XmNspacing      , 0 ,
              XmNborderWidth  , 0 ,
              XmNborderColor  , 0 ,

              XmNrecomputeSize , False ,
              XmNtraversalOn , False ,
              XmNinitialResourcesPersistent , False ,
           NULL ) ;

   /* menubar to hold the cascade button */

   mbar = XmCreateMenuBar( rc , "imseq" , NULL,0 ) ;
   XtVaSetValues( mbar ,
                     XmNmarginWidth  , 1 ,
                     XmNmarginHeight , 0 ,
                     XmNmarginBottom , 0 ,
                     XmNmarginTop    , 0 ,
                     XmNmarginLeft   , 0 ,
                     XmNmarginRight  , 0 ,
                     XmNspacing      , 0 ,
                     XmNborderWidth  , 0 ,
                     XmNborderColor  , 0 ,
                     XmNtraversalOn  , False ,
                     XmNbackground   , seq->dc->ovc->pixov_brightest ,
                  NULL ) ;

   /* the menu pane */

   menu = XmCreatePulldownMenu( mbar , "menu" , NULL,0 ) ;
   VISIBILIZE_WHEN_MAPPED(menu) ;
#if 0  /* doesn't work well */
   if( !AFNI_yesenv("AFNI_DISABLE_TEAROFF") ) TEAROFFIZE(menu) ;
#endif

   /* the cascade button (what the user sees) */

   xstr = XmStringCreateLtoR( "Rec" , XmFONTLIST_DEFAULT_TAG ) ;
   seq->record_cbut = cbut =
     XtVaCreateManagedWidget(
            "imseq" , xmCascadeButtonWidgetClass , mbar ,
               XmNlabelString , xstr ,
               XmNsubMenuId   , menu ,
               XmNmarginWidth , 1 ,
               XmNmarginHeight, 0 ,
               XmNmarginBottom, 0 ,
               XmNmarginTop   , 0 ,
               XmNmarginRight , 0 ,
               XmNmarginLeft  , 0 ,
               XmNtraversalOn , False ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;
   XmStringFree( xstr ) ;
   XtManageChild( mbar ) ;
   MCW_register_hint( cbut , "Turn image recording on/off" ) ;
   MCW_register_help( cbut ,
                      " \n"
                      "This menu controls image recording. Whenever the image\n"
                      "displayed is altered, an RGB copy of it can be saved\n"
                      "into a separate image buffer.  In this way, you can\n"
                      "build a sequence of images that can later be written\n"
                      "to disk for further processing (e.g., animation).\n"
                      "\n"
                      "---- These options control WHEN images  ----\n"
                      "---- will be recorded into the sequence ----\n"
                      "\n"
                      " Off      = don't record\n"
                      " Next One = record next image, then turn Off\n"
                      " Stay On  = record all images\n"
                      "\n"
                      "---- These options control WHERE new images ----\n"
                      "---- are to be stored into the sequence     ----\n"
                      "\n"
                      " After End    = at tail of sequence\n"
                      " Before Start = at head of sequence\n"
                      " Insert --    = insert before current sequence position\n"
                      " Insert ++    = insert after current sequence position\n"
                      " OverWrite    = replace current sequence position\n"
                      " -- OverWrite = replace image before current position\n"
                      " ++ OverWrite = replace image after current position\n"
                      "\n"
                      "---- HINTS and NOTES ----\n"
                      "\n"
                      "* You may want to set Xhairs to 'Off' on the AFNI\n"
                      "   control panel before recording images.\n"
                      "* The recording window is like a dataset image\n"
                      "   viewing window with most controls removed.\n"
                      "   The slider moves between recorded images, rather\n"
                      "   than between slices.\n"
                      "* The new 'Kill' button in the recording window lets\n"
                      "   you erase one image from the recorded sequence.\n"
                      "   Erased images, if not overwritten, will NOT be\n"
                      "   saved to disk.\n"
                      "* Use 'Save:bkg' in the recording window to save the\n"
                      "   sequence of recorded images to disk in PPM format.\n"
                      "   The recorded images are in color, and will be saved\n"
                      "   in color (despite the :bkg label on the Save button).\n"
                      "* You may want to use set 'Warp Anat on Demand' on\n"
                      "   the Datamode control panel to force the display\n"
                      "   voxels to be cubical.  Otherwise, the saved image\n"
                      "   pixels will have the same aspect ratio as the voxels\n"
                      "   in the dataset, which may not be square!\n"
                     ) ;

   /*-- top of menu = a label to click on that does nothing at all --*/

   /* This --- Cancel --- label does not cause the hangup, so it is
   left alone. See related comments in afni_graph.c LessTif patrol, Jan 07 09 */

   xstr = XmStringCreateLtoR( "-- Cancel --" , XmFONTLIST_DEFAULT_TAG ) ;
   (void) XtVaCreateManagedWidget(
            "menu" , xmLabelWidgetClass , menu ,
               XmNlabelString , xstr ,
               XmNrecomputeSize , False ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;
   XmStringFree(xstr) ;

   (void) XtVaCreateManagedWidget(
            "menu" , xmSeparatorWidgetClass , menu ,
               XmNseparatorType , XmSINGLE_LINE ,
            NULL ) ;

   /*-- menu toggles switches --*/

   {  static char * status_label[3] = { "Off" , "Next One" , "Stay On" } ;
      static char * method_label[7] = { "After End"    ,
                                        "Before Start" ,
                                        "Insert --"    ,
                                        "Insert ++"    ,
                                        "OverWrite"    ,
                                        "-- OverWrite" ,
                                        "++ OverWrite"   } ;

      seq->record_status_bbox =
         new_MCW_bbox( menu , 3,status_label ,
                       MCW_BB_radio_one , MCW_BB_noframe ,
                       ISQ_record_CB , (XtPointer) seq ) ;
      seq->record_status = RECORD_STATUS_OFF ;

      (void) XtVaCreateManagedWidget(
               "menu" , xmSeparatorWidgetClass , menu ,
                  XmNseparatorType , XmSINGLE_LINE ,
               NULL ) ;

      seq->record_method_bbox =
         new_MCW_bbox( menu , 7,method_label ,
                       MCW_BB_radio_one , MCW_BB_noframe ,
                       ISQ_record_CB , (XtPointer) seq ) ;
      seq->record_method = RECORD_METHOD_AFTEREND ;
   }

   /*-- done with Widgets --*/

   XtManageChild( rc ) ;

   /*-- setup other variables --*/

   seq->record_mode  = 0 ;    /* not a recorder itself (yet) */
   seq->record_imseq = NULL ; /* doesn't have a recorder */
   seq->record_imarr = NULL ; /* doesn't have a recorded sequence */
   seq->record_mplot = NULL ; /* 05 Jan 2005 */

   EXRETURN ;
}

/*-----------------------------------------------------------------------
  Callback for toggle actions in the record menu
-------------------------------------------------------------------------*/

void ISQ_record_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   MCW_imseq * seq = (MCW_imseq *) client_data ;
   int ib ;

ENTRY("ISQ_record_CB") ;

   if( !ISQ_REALZ(seq) ) EXRETURN ;

   ib = MCW_val_bbox( seq->record_status_bbox ) ;
   if( ib != seq->record_status ){
      if( RECORD_ISON(ib) != RECORD_ISON(seq->record_status) )
         MCW_invert_widget( seq->record_cbut ) ;
      seq->record_status = ib ;
   }

   ib = MCW_val_bbox( seq->record_method_bbox ) ;
   if( ib != seq->record_method ){
      seq->record_method = ib ;
   }

   EXRETURN ;
}

/*----------------------------------------------------------------------
  Insert the current image from seq into seq's recorder, at index pos,
  with method meth: -1 => insert before pos
                    +1 => insert after pos
                     0 => overwrite pos
  If pos < 0, then it means
                    -1 => current position
                    -2 => before current position
                    -3 => after current position
  If pos >= 0, then it is an index into the recorded image sequence.
  If it is past the end of the sequence, then it means the end.
  The recorder is left positioned at the new image.
------------------------------------------------------------------------*/

void ISQ_record_addim( MCW_imseq * seq , int pos , int meth )
{
   MRI_IMAGE * tim ;
   int opos , ii,bot,top ;

ENTRY("ISQ_record_addim") ;

   /* sanity checks */

   if( !ISQ_REALZ(seq)        ||
       seq->record_mode       ||
       seq->given_xim == NULL   ) EXRETURN; /* bad */

   /* if recorded image sequence doesn't exist, create it */

   if( seq->record_imarr == NULL ){
     INIT_IMARR(seq->record_imarr) ;
     meth = 1 ;  /* change meth for this special case */

     seq->record_mplot = NULL ;  /* 05 Jan 2005 */
   }

   /* convert current XImage to RGB format */

   tim = XImage_to_mri( seq->dc, seq->given_xim, X2M_USE_CMAP|X2M_FORCE_RGB );

   if( tim == NULL ) EXRETURN ; /* bad */

   /* figure out where to put this image in the list */

   opos = pos ;
   if( opos < 0 ){  /* need current position of recorder */

      if( seq->record_imseq != NULL ){
         drive_MCW_imseq( seq->record_imseq, isqDR_getimnr, (XtPointer)&opos );
              if( pos == -2 && opos > 0                                ) opos--;
         else if( pos == -3 && opos < IMARR_COUNT(seq->record_imarr)-1 ) opos++;
      }
      else
         opos = -1 ; /* special case */

   } else if( opos >= IMARR_COUNT(seq->record_imarr)-1 ) {

      opos = IMARR_COUNT(seq->record_imarr)-1 ;
   }

   if( opos < 0 ) meth = 1 ; /* special case: sequence is empty now */

   /* if we are inserting, we need to add an image */

   if( meth != 0 ){

      ADDTO_IMARR( seq->record_imarr , NULL ) ;  /* add at end */
      seq->record_mplot =(MEM_plotdata **)       /* 05 Jan 2005 */
                         realloc( (void *)seq->record_mplot ,
                                  sizeof(MEM_plotdata *)
                                 *IMARR_COUNT(seq->record_imarr) ) ;
      bot = (meth < 0) ? opos : opos+1 ;         /* move images up */
      top = IMARR_COUNT(seq->record_imarr)-2 ;
      for( ii=top ; ii >= bot ; ii-- ){
        IMARR_SUBIM(seq->record_imarr,ii+1) = IMARR_SUBIM(seq->record_imarr,ii);
        seq->record_mplot[ii+1] = seq->record_mplot[ii] ;  /* 05 Jan 2005 */
      }

      IMARR_SUBIM(seq->record_imarr,bot) = tim ; /* insert */
      seq->record_mplot[bot]             = copy_memplot( seq->mplot ) ;

   } else {  /* overwrite image */

      bot = opos ;
      mri_free( IMARR_SUBIM(seq->record_imarr,bot) ) ; /* out with the old */
      IMARR_SUBIM(seq->record_imarr,bot) = tim ;       /* in with the new */

      delete_memplot( seq->record_mplot[bot] ) ;       /* 05 Jan 2005 */
      seq->record_mplot[bot] = copy_memplot( seq->mplot ) ;
   }

   /* at this point, we have put the new image into location bot in the array */

   /* if the recorder isn't open now, open it, otherwise update it */

   if( seq->record_imseq == NULL )
      ISQ_record_open( seq ) ;
   else
      ISQ_record_update( seq , bot ) ;

   EXRETURN ;
}

/*-----------------------------------------------------------------------*/

void ISQ_record_open( MCW_imseq * seq )
{
   int ntot ;

ENTRY("ISQ_record_open") ;

   if( !ISQ_REALZ(seq)                     ||
       seq->record_imarr == NULL           ||
       IMARR_COUNT(seq->record_imarr) == 0   ) EXRETURN ;

   ntot = IMARR_COUNT(seq->record_imarr) ;

   seq->record_imseq = open_MCW_imseq( seq->dc , ISQ_record_getim , seq ) ;
   seq->record_imseq->parent = seq ;

   drive_MCW_imseq( seq->record_imseq , isqDR_record_mode , NULL ) ;

   drive_MCW_imseq( seq->record_imseq , isqDR_realize, NULL ) ;

#ifndef DONT_ONOFF_ONE
   if( ntot == 1 )
      drive_MCW_imseq( seq->record_imseq,isqDR_onoffwid,(XtPointer)isqDR_offwid);
   else
      drive_MCW_imseq( seq->record_imseq,isqDR_onoffwid,(XtPointer)isqDR_onwid );
#endif

   drive_MCW_imseq( seq->record_imseq , isqDR_reimage , (XtPointer) (ntot-1) ) ;

   ISQ_set_cursor_state( seq , -1 ) ;  /* 10 Mar 2003 */
   NORMAL_cursorize( seq->wbar ) ;

   EXRETURN ;
}

/*-----------------------------------------------------------------------*/

void ISQ_record_update( MCW_imseq * seq , int npos )
{
   int ntot , ii ;

ENTRY("ISQ_record_update") ;

   if( !ISQ_REALZ(seq)                     ||
       seq->record_imseq == NULL           ||
       seq->record_imarr == NULL           ||
       IMARR_COUNT(seq->record_imarr) == 0   ) EXRETURN ;

   ntot = IMARR_COUNT(seq->record_imarr) ;

        if( npos <  0    ) npos = 0 ;
   else if( npos >= ntot ) npos = ntot-1 ;

   drive_MCW_imseq( seq->record_imseq , isqDR_newseq , seq ) ;

#ifndef DONT_ONOFF_ONE
   if( ntot == 1 )
      drive_MCW_imseq( seq->record_imseq,isqDR_onoffwid,(XtPointer)isqDR_offwid);
   else
      drive_MCW_imseq( seq->record_imseq,isqDR_onoffwid,(XtPointer)isqDR_onwid );
#endif

   drive_MCW_imseq( seq->record_imseq , isqDR_reimage , (XtPointer)npos ) ;

   EXRETURN ;
}

/*------------------------------------------------------------------
   Routine to provide data to the recording imseq.
   Just returns the control information, or the selected image.
--------------------------------------------------------------------*/

XtPointer ISQ_record_getim( int n , int type , XtPointer handle )
{
   int ntot = 0 ;
   MCW_imseq * seq = (MCW_imseq *) handle ;  /* parent of recorder */

ENTRY("ISQ_record_getim") ;

   if( seq->record_imarr != NULL ) ntot = IMARR_COUNT(seq->record_imarr) ;
   if( ntot < 1 ) ntot = 1 ;

   /*--- send control info ---*/

   if( type == isqCR_getstatus ){
      MCW_imseq_status * stat = myXtNew( MCW_imseq_status ); /* will be free-d */
                                                             /* when imseq is */
                                                             /* destroyed    */
      stat->num_total  = ntot ;
      stat->num_series = stat->num_total ;
      stat->send_CB    = ISQ_record_send_CB ;
      stat->parent     = NULL ;
      stat->aux        = NULL ;

      stat->transforms0D = NULL ;
      stat->transforms2D = NULL ;

      RETURN( (XtPointer)stat ) ;
   }

   /*--- overlay [05 Jan 2005] ---*/

   if( type == isqCR_getoverlay ) RETURN(NULL) ;  /* no image overlay */

   if( type == isqCR_getmemplot ){                /* graphics overlay */
     MEM_plotdata *mp ;
     if( seq->record_mplot == NULL ) RETURN(NULL) ;
     if( n < 0 ) n = 0 ; else if( n >= ntot ) n = ntot-1 ;
     mp = copy_memplot( seq->record_mplot[n] ) ;
     RETURN( (XtPointer)mp ) ;   /* may be NULL */
   }

   /*--- return a copy of a recorded image
         (since the imseq will delete it when it is done) ---*/

   if( type == isqCR_getimage || type == isqCR_getqimage ){
      MRI_IMAGE *im = NULL , *rim ;

      if( seq->record_imarr != NULL ){
         if( n < 0 ) n = 0 ; else if( n >= ntot ) n = ntot-1 ;
         rim = IMARR_SUBIMAGE(seq->record_imarr,n) ;
         if( rim != NULL ) im = mri_to_rgb( rim ) ;
      }
      RETURN( (XtPointer)im ) ;
   }

   RETURN( NULL ) ; /* should not occur, but who knows? */
}

/*---------------------------------------------------------------------------
   Routine called when the recording imseq wants to send a message.
   In this case, all we need to handle is the destroy message,
   so that we can free some memory.
-----------------------------------------------------------------------------*/

void ISQ_record_send_CB( MCW_imseq * seq , XtPointer handle , ISQ_cbs * cbs )
{
ENTRY("ISQ_record_send_CB") ;

   switch( cbs->reason ){

      case isqCR_destroy:{
         MCW_imseq *pseq = (MCW_imseq *) seq->parent ;

         /* turn off recording in the parent */

         pseq->record_imseq = NULL ;
         if( pseq->record_mplot != NULL && pseq->record_imarr != NULL ){
           int ib ;
           for( ib=0 ; ib < IMARR_COUNT(pseq->record_imarr) ; ib++ )
             delete_memplot( pseq->record_mplot[ib] ) ;
           free((void *)pseq->record_mplot) ; pseq->record_mplot = NULL ;
         }
         if( pseq->record_imarr != NULL ) DESTROY_IMARR(pseq->record_imarr) ;
         if( RECORD_ISON(pseq->record_status) ){
            pseq->record_status = RECORD_STATUS_OFF ;
            MCW_set_bbox( pseq->record_status_bbox , RECORD_STATUS_OFF ) ;
            MCW_invert_widget( pseq->record_cbut ) ;
         }

         /* can now clean out the recording imseq */

         myXtFree(seq->status) ; myXtFree(seq) ;
      }
      break ;

   }

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/

void ISQ_record_kill_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   MCW_imseq * seq = (MCW_imseq *) client_data ;
   MCW_imseq * pseq ;
   int pos=-1 ;

ENTRY("ISQ_record_kill_CB") ;

   if( !ISQ_REALZ(seq) || !seq->record_mode ) EXRETURN ; /* bad */

   pseq = (MCW_imseq *) seq->parent ;  /* the one driving this recorder */

   if( pseq->record_imarr == NULL ) EXRETURN ; /* bad */

   drive_MCW_imseq( seq , isqDR_getimnr, (XtPointer)&pos ) ; /* where am us? */

   if( pos < 0 || pos >= IMARR_COUNT(pseq->record_imarr) ) EXRETURN ;

   /* empty out the image in the recorded sequence */

   mri_free( IMARR_SUBIM(pseq->record_imarr,pos) ) ;
   IMARR_SUBIM(pseq->record_imarr,pos) = NULL ;
   delete_memplot( pseq->record_mplot[pos] ) ;  /* 05 Jan 2005 */
   pseq->record_mplot[pos] = NULL ;

   ISQ_redisplay( seq , -1 , isqDR_display ) ;  /* show the empty image */

   EXRETURN ;
}

/*---------------------------------------------------------------------
   Handle the user's action on the Button 3 popup on the Save: button
-----------------------------------------------------------------------*/

void ISQ_butsave_choice_CB( Widget w , XtPointer client_data ,
                                       MCW_choose_cbs * cbs   )
{
   MCW_imseq * seq = (MCW_imseq *) client_data ;
   int pp , agif_ind=0 , mpeg_ind=0 , nstr ;

   if( !ISQ_REALZ(seq)               ||
       cbs->reason != mcwCR_integer  ||
       seq->dialog_starter==NBUT_DISP  ){  /* bad things */

      XBell(XtDisplay(w),100); POPDOWN_strlist_chooser ; return ;
   }

   nstr = ppmto_num+1 ;
   if( ppmto_agif_filter != NULL ) agif_ind = nstr++ ;
   if( ppmto_mpeg_filter != NULL ) mpeg_ind = nstr++ ;

   seq->opt.save_nsize = seq->opt.save_pnm
                       = seq->opt.save_agif = seq->opt.save_mpeg = 0 ;

   pp = cbs->ival ;
        if( pp == 0         ) seq->opt.save_filter=-1  ; /* Save:bkg */
   else if( pp <= ppmto_num ) seq->opt.save_filter=pp-1; /* Save.typ */
   else if( pp == agif_ind  ) seq->opt.save_agif  = 1  ; /* Sav:aGif */
   else if( pp == mpeg_ind  ) seq->opt.save_mpeg  = 1  ; /* Sav:mpeg */

   if( ppmto_agif_filter == NULL ) seq->opt.save_agif = 0 ;  /* 07 Apr 2005 */
   if( ppmto_mpeg_filter == NULL ) seq->opt.save_mpeg = 0 ;

   SET_SAVE_LABEL(seq) ; return ;
}

/*--------------------------------------------------------------------
  make Button 3 popup for Save button
  -- 27 Jul 2001: add stuff for animated GIF (ppmto_num+1 index)
----------------------------------------------------------------------*/

void ISQ_butsave_EV( Widget w , XtPointer client_data ,
                     XEvent * ev , Boolean * continue_to_dispatch )
{
   MCW_imseq * seq = (MCW_imseq *) client_data ;

   if( !ISQ_REALZ(seq) ) return ;

   ISQ_timer_stop(seq) ;

   switch( ev->type ){
      case ButtonPress:{
         XButtonEvent * event = (XButtonEvent *) ev ;
         if( event->button == Button3 ){
            char **strlist ; int pp , nstr , agif_ind=0 , mpeg_ind=0 ;
            if( seq->dialog_starter==NBUT_DISP ){XBell(XtDisplay(w),100); return; }
            strlist = (char **) malloc(sizeof(char *)*(ppmto_num+3)) ;
            strlist[0] = strdup("Save:bkg") ;             /* special case */
            for( pp=0 ; pp < ppmto_num ; pp++ ){          /* filters */
               strlist[pp+1] = AFMALL( char, 16) ;
               sprintf(strlist[pp+1],"Save.%.3s",ppmto_suffix[pp]) ;
            }
            nstr = ppmto_num+1 ;
            if( ppmto_agif_filter != NULL ){
               agif_ind = nstr ;
               strlist[nstr++] = strdup("Sav:aGif") ;     /* special case */
            }
            if( ppmto_mpeg_filter != NULL ){
               mpeg_ind = nstr ;
               strlist[nstr++] = strdup("Sav:mpeg") ;     /* special case */
            }
                 if(seq->opt.save_agif && agif_ind > 0 ) pp=agif_ind ;
            else if(seq->opt.save_mpeg && mpeg_ind > 0 ) pp=mpeg_ind ;
            else if(seq->opt.save_filter < 0)            pp=0        ;
            else                                     pp=seq->opt.save_filter+1 ;
            MCW_choose_strlist( w , "Image Save format" ,
                                nstr , pp , strlist ,
                                ISQ_butsave_choice_CB , (XtPointer) seq ) ;
            for( pp=0 ; pp < nstr ; pp++ ) free(strlist[pp]) ;
            free(strlist) ;
         } else if( event->button == Button2 ){
            XBell(XtDisplay(w),100) ;
            MCW_popup_message( w, " \n Ouch! \n ", MCW_USER_KILL );
            /** AFNI_speak( "Ouch!" , 0 ) ; **/
         }
      }
      break ;
   }
   return ;
}

/*--------------------------------------------------------------------------*/
/*! Get the label for overlay. [11 Jun 2002]
----------------------------------------------------------------------------*/

char * ISQ_getlabel( int nn , MCW_imseq *seq )
{
   char *lab=NULL ;

ENTRY("ISQ_getlabel") ;

#if 0
   lab = (char *) seq->getim( nn,isqCR_getlabel,seq->getaux );
#else
   AFNI_CALL_VALU_3ARG( seq->getim , char *,lab ,
                        int,nn , int,isqCR_getlabel , XtPointer,seq->getaux ) ;
#endif
   RETURN(lab) ;
}

/*--------------------------------------------------------------------------*/
/*! Get the memplot for overlay. [11 Jun 2002]
----------------------------------------------------------------------------*/

MEM_plotdata * ISQ_getmemplot( int nn , MCW_imseq *seq )
{
   MEM_plotdata *mp=NULL ;
   int           ntic ;

ENTRY("ISQ_getmemplot") ;

#if 0
   mp = (MEM_plotdata *) seq->getim( nn,isqCR_getmemplot,seq->getaux );
#else
   AFNI_CALL_VALU_3ARG( seq->getim , MEM_plotdata *,mp ,
                        int,nn , int,isqCR_getmemplot , XtPointer,seq->getaux ) ;
#endif

   if( mp != NULL && seq->cropit ){  /* scale memplot for cropping region */
     float sx,sy,tx,ty ;
     float xa=seq->crop_xa, xb=seq->crop_xb, ya=seq->crop_ya, yb=seq->crop_yb ;
     float nxorg=seq->crop_nxorg , nyorg=seq->crop_nyorg ;
     MEM_plotdata *np ;

     if( xb >= nxorg ) xb = nxorg-1 ;
     if( yb >= nyorg ) yb = nyorg-1 ;

     /**
      Original plot has [0..1]x[0..1] mapped to [0..nxorg]x[nyorg..0].
      Now, image will be cropped to [xa..xb]x[ya..yb], which will be
      mapped from plot coords [0..1]x[1..0].  So we need to transform
      plot coords so that the new
           x_plot=0 is at x_image=xa
           x_plot=1 is at x_image=xb
           y_plot=0 is at y_image=yb
           y_plot=1 is at y_image=ya

      Input:   x_plot  = x_image / nxorg
               y_plot  = 1 - y_image / nyorg

      Output:  x_plot' = sx * x_plot + tx   > This is done in
               y_plot' = sy * y_plot + ty   > scale_memplot function

      Find sx,tx so that x_plot'[x_image=xa  ]=0 and x_plot'[x_image=xb+1]=1.
      Find sy,ty so that y_plot'[y_image=yb+1]=0 and y_plot'[y_image=ya  ]=1.
     **/

     sx = nxorg / (xb+1-xa) ;
     tx = -sx * xa / nxorg ;

     sy = nyorg / (yb+1-ya) ;
     ty = -sy * (1.0 - (yb+1) / nyorg) ;

     scale_memplot( sx,tx , sy,ty , 1.0 , mp ) ;    /* expand scale  */
     np = clip_memplot( 0.0,0.0 , 1.0,1.0 , mp ) ;  /* clip to window */
     DESTROY_MEMPLOT(mp) ; mp = np ;
   }

   /*** 23 Feb 2004: tick marks around the edge of the image? ***/

   ntic = seq->wbar_ticnum_av->ival ;

   if( ntic > 0 ){
     MEM_plotdata *tp ;
     char *eee ;
     /* float tic, fac=1.0/ntic ; */
     float rr=0.8,gg=1.0,bb=0.6 , tic, xfac, yfac;
     float xlen = 0.0, ylen = 0.0;  /* when ntics is in mm */
     int it, nticx, nticy;

     /* plot ntic as separation distance for J Binder  23 Feb 2006 [rickr] */

     if( seq->imim && AFNI_yesenv("AFNI_IMAGE_TICK_DIV_IN_MM") ){
         /* get image size */
         if( mp != NULL && seq->cropit ){  /* cropped size */
           xlen = abs(seq->crop_xb - seq->crop_xa);
           ylen = abs(seq->crop_yb - seq->crop_ya);
         } else {                          /* full size */
           xlen = seq->imim->nx * seq->imim->dx;
           ylen = seq->imim->ny * seq->imim->dy;
         }

         nticx = xlen/ntic;
         nticy = ylen/ntic;
         xfac=ntic/xlen;
         yfac=ntic/ylen;
     } else {
         nticx = nticy = ntic;
         xfac=1.0/ntic;
         yfac=1.0/ntic;
     }

     create_memplot_surely( "Iticplot" , 1.0 ) ;
     set_thick_memplot(0.0) ;
     eee = getenv("AFNI_IMAGE_LABEL_COLOR") ;
     if( eee != NULL )
       DC_parse_color( seq->dc , eee , &rr,&gg,&bb ) ;
     set_color_memplot(rr,gg,bb) ;

     tic = 0.01 * seq->wbar_ticsiz_av->ival ;  /* percent of image size */

     /* x and y are separate, in case ntic is in mm */
     for( it=0 ; it <= nticy ; it++ ){
       plotpak_line( 0.0,it*yfac , tic    ,it*yfac ) ;
       plotpak_line( 1.0,it*yfac , 1.0-tic,it*yfac ) ;
     }
     for( it=0 ; it <= nticx ; it++ ){
       plotpak_line( it*xfac,0.0 , it*xfac ,tic    ) ;
       plotpak_line( it*xfac,1.0 , it*xfac ,1.0-tic) ;
     }

     /* append tick plot to existing plot, if any */

     tp = get_active_memplot() ;
     if( mp != NULL ){ append_to_memplot(mp,tp); delete_memplot(tp); }
     else              mp = tp ;
   }

   RETURN(mp) ;
}

/*--------------------------------------------------------------------------*/
/*! Get the image for overlay. [11 Jun 2002]
----------------------------------------------------------------------------*/

MRI_IMAGE * ISQ_getoverlay( int nn , MCW_imseq *seq )
{
   MRI_IMAGE *tim=NULL ;

ENTRY("ISQ_getoverlay") ;

#if 0
   tim = (MRI_IMAGE *) seq->getim( nn , isqCR_getoverlay , seq->getaux ) ;
#else
   AFNI_CALL_VALU_3ARG( seq->getim , MRI_IMAGE *,tim ,
                        int,nn , int,isqCR_getoverlay , XtPointer,seq->getaux ) ;
#endif

   if( tim == NULL ) RETURN(NULL) ;

   /*--- cut out cropped region, if any ---*/

   if( seq->cropit ){
     MRI_IMAGE *qim = mri_cut_2D( tim, seq->crop_xa,seq->crop_xb,
                                       seq->crop_ya,seq->crop_yb ) ;
     if( qim != NULL ){ mri_free(tim); tim = qim; }
   }

   RETURN(tim) ;
}

/*--------------------------------------------------------------------------*/
/*! Get the image for display.  Maybe use projections. [31 Jan 2002] */

MRI_IMAGE * ISQ_getimage( int nn , MCW_imseq *seq )
{
   int ii , rr , jj , ns , npix , ktim ;
   MRI_IMAGE *tim=NULL , *qim=NULL , *fim=NULL ;
   MRI_IMARR *imar ;
   float *far , val=0.0f , *qar , **iar ;

ENTRY("ISQ_getimage") ;

   /* get the commanded slice */

#if 0
   tim = (MRI_IMAGE *) seq->getim( nn, isqCR_getimage, seq->getaux ) ;
#else
   AFNI_CALL_VALU_3ARG( seq->getim , MRI_IMAGE *,tim ,
                        int,nn , int,isqCR_getimage , XtPointer,seq->getaux ) ;
#endif

   if( tim == NULL ) RETURN(NULL) ;

   if( seq->cropit ){

     if( seq->crop_nxorg < 0 || seq->crop_nyorg < 0 ){ /* orig image size not set yet */
       seq->crop_nxorg = tim->nx ;
       seq->crop_nyorg = tim->ny ;
     }

     if( tim->nx != seq->crop_nxorg ||    /* image changed size? */
         tim->ny != seq->crop_nyorg   ){  /* => turn cropping off */

       seq->cropit = 0 ; seq->crop_nxorg = seq->crop_nyorg = -1 ;

       if( seq->crop_drag ){              /* should not happen */
         MCW_invert_widget( seq->crop_drag_pb ) ;
         seq->crop_drag = 0 ;
       }

     } else {
       MRI_IMAGE *cim ;
       if( seq->crop_xb >= seq->crop_nxorg ) seq->crop_xb = seq->crop_nxorg - 1 ;
       if( seq->crop_yb >= seq->crop_nyorg ) seq->crop_yb = seq->crop_nyorg - 1 ;
       cim = mri_cut_2D( tim, seq->crop_xa,seq->crop_xb,
                              seq->crop_ya,seq->crop_yb ) ;
       if( cim != NULL ){ mri_free(tim); tim = cim; }
     }
   }

   /* the old way - return this slice */

   if( !ISQ_DOING_SLICE_PROJ(seq) ) RETURN(tim) ;

   ns = seq->status->num_series ;
   rr = seq->slice_proj_range   ; if( rr > ns/2 ) rr = ns/2 ;

   if( rr                    == 0           ||
       seq->slice_proj_index == 0           ||
       seq->slice_proj_func  == NULL        ||
       tim                   == NULL        ||
       tim->kind             == MRI_rgb     ||
       tim->kind             == MRI_complex   ){

      RETURN(tim) ;
   }

   /* the new way - return the projection of a bunch of images */

   INIT_IMARR(imar) ;

   ktim = tim->kind ;  /* save for later use */

   /* get the images into imar */

   for( ii=-rr ; ii <= rr ; ii++ ){

      if( ii == 0 ){                /* at the middle, just put a   */
         fim = mri_to_float(tim) ;  /* copy of the commanded slice */
         ADDTO_IMARR(imar,fim) ;
         continue ;
      }

      jj = nn+ii ;                    /* offset slice */
           if( jj < 0   ) jj = 0    ; /* but not past the edges */
      else if( jj >= ns ) jj = ns-1 ;

#if 0
      qim = (MRI_IMAGE *) seq->getim( jj, isqCR_getimage, seq->getaux ) ;
#else
      AFNI_CALL_VALU_3ARG( seq->getim , MRI_IMAGE *,qim ,
                           int,jj , int,isqCR_getimage , XtPointer,seq->getaux ) ;
#endif

      if( qim == NULL )
         fim = mri_to_float(tim) ;                 /* need something */
      else if( qim->kind != MRI_float ){
         fim = mri_to_float(qim) ; mri_free(qim) ; /* convert it */
      } else
         fim = qim ;                               /* just put it here */

      if( seq->cropit ){
        MRI_IMAGE *cim = mri_cut_2D( fim , seq->crop_xa,seq->crop_xb,
                                           seq->crop_ya,seq->crop_yb ) ;
        if( cim != NULL ){ mri_free(fim); fim = cim; }
      }

      ADDTO_IMARR(imar,fim) ;
   }

   /* project images, put results into qim */

   qim = mri_new_conforming( tim , MRI_float ) ;
   qar = MRI_FLOAT_PTR(qim) ; MRI_COPY_AUX(qim,tim) ;
   mri_free(tim) ;

   npix = qim->nvox ;
   rr   = 2*rr+1 ;
   far  = (float * ) malloc( sizeof(float  ) * rr ) ;
   iar  = (float **) malloc( sizeof(float *) * rr ) ;

   for( ii=0 ; ii < rr ; ii++ )
     iar[ii] = MRI_FLOAT_PTR(IMARR_SUBIM(imar,ii)) ;

   for( jj=0 ; jj < npix ; jj++ ){

     for( ii=0 ; ii < rr ; ii++ ) far[ii] = iar[ii][jj] ;

#if 0
     val = seq->slice_proj_func( rr , far ) ;
#else
     AFNI_CALL_proj_function( seq->slice_proj_func , rr,far , val ) ;
#endif

     qar[jj] = val ;
   }

   free(iar) ; free(far) ; DESTROY_IMARR(imar) ;

   if( ktim != MRI_float ){
     tim = mri_to_mri(ktim,qim); mri_free(qim); qim = tim;
   }

   RETURN(qim) ;
}

/*---------------------------------------------------------------------*/

MRI_IMAGE * ISQ_cropim( MRI_IMAGE *tim , MCW_imseq *seq )
{
   if( tim == NULL || !seq->cropit ) return NULL ;

   if( seq->crop_nxorg < 0 || seq->crop_nyorg < 0 ){ /* orig image size not set yet */
     seq->crop_nxorg = tim->nx ;
     seq->crop_nyorg = tim->ny ;
   }

   if( tim->nx != seq->crop_nxorg ||    /* image changed size? */
       tim->ny != seq->crop_nyorg   ){  /* => turn cropping off */

     seq->cropit = 0 ; seq->crop_nxorg = seq->crop_nyorg = -1 ;

     if( seq->crop_drag ){              /* should not happen */
       MCW_invert_widget( seq->crop_drag_pb ) ;
       seq->crop_drag = 0 ;
     }

   } else {
     MRI_IMAGE *cim ;
     if( seq->crop_xb >= seq->crop_nxorg ) seq->crop_xb = seq->crop_nxorg - 1 ;
     if( seq->crop_yb >= seq->crop_nyorg ) seq->crop_yb = seq->crop_nyorg - 1 ;
     cim = mri_cut_2D( tim, seq->crop_xa,seq->crop_xb,
                            seq->crop_ya,seq->crop_yb ) ;
     if( cim != NULL ){ MRI_COPY_AUX(cim,tim); return cim; }
   }

   return NULL ;
}

/*---------------------------------------------------------------------*/

MRI_IMAGE * ISQ_getulay( int nn , MCW_imseq *seq )
{
   MRI_IMAGE *tim=NULL , *cim ;

   AFNI_CALL_VALU_3ARG( seq->getim , MRI_IMAGE *,tim ,
                        int,nn , int,isqCR_getulayim , XtPointer,seq->getaux ) ;

   cim = ISQ_cropim( tim , seq ) ;
   if( cim != NULL ){ mri_free(tim) ; tim = cim ; }
   return tim ;
}

/*---------------------------------------------------------------------*/

MRI_IMAGE * ISQ_getolay( int nn , MCW_imseq *seq )
{
   MRI_IMAGE *tim=NULL , *cim ;

   AFNI_CALL_VALU_3ARG( seq->getim , MRI_IMAGE *,tim ,
                        int,nn , int,isqCR_getolayim , XtPointer,seq->getaux ) ;

   cim = ISQ_cropim( tim , seq ) ;
   if( cim != NULL ){ mri_free(tim) ; tim = cim ; }
   return tim ;
}

/*---------------------------------------------------------------------*/

MRI_IMAGE *ISQ_getchecked( int nn , MCW_imseq *seq )
{
   MRI_IMAGE *qim=NULL , *uim , *oim ; float dx,dy ;

ENTRY("ISQ_getchecked") ;

   qim = ISQ_getimage(nn,seq) ; if( qim == NULL ) RETURN(NULL) ;
   dx  = qim->dx ; dy = qim->dy ;
   uim = ISQ_process_mri(nn,seq,qim) ; mri_free(qim) ;

   qim = ISQ_getolay (nn,seq) ; if( qim == NULL ) RETURN(uim) ;
   oim = ISQ_process_mri(nn,seq,qim) ; mri_free(qim) ;

   if( uim->kind == MRI_rgb && oim->kind == MRI_short ){
     qim = ISQ_index_to_rgb( seq->dc , 0 , oim ) ;
     mri_free(oim) ; oim = qim ;
   } else if( uim->kind == MRI_short && oim->kind == MRI_rgb ){
     qim = ISQ_index_to_rgb( seq->dc , 0 , uim ) ;
     mri_free(uim) ; uim = qim ;
   }

   if( seq->render_mode == RENDER_CHECK_OU )
     qim = mri_check_2D( seq->wbar_checkbrd_av->ival , oim , uim ) ;
   else
     qim = mri_check_2D( seq->wbar_checkbrd_av->ival , uim , oim ) ;

   mri_free(oim) ;
   if( qim == NULL ){ uim->dx = dx ; uim->dy = dy ; RETURN(uim) ; }

   mri_free(uim) ;    qim->dx = dx ; qim->dy = dy ; RETURN(qim) ;
}

/*---------------------------------------------------------------------*/
/*! Deal with dragging a crop window after a button has been pressed.
-----------------------------------------------------------------------*/

void ISQ_cropper( MCW_imseq *seq , XButtonEvent *event )
{
   int x1=event->x,y1=event->y , x2,y2 ;
   int imx1,imy1,nim1 , imx2,imy2,nim2 , tt ;
   int zlev = seq->zoom_fac ;

ENTRY("ISQ_cropper") ;

   if( !seq->crop_allowed ){
     XBell(seq->dc->display,100); EXRETURN;
   }

   /*** make the user drag a rectangle while button is pressed:
        (x1,y1) = window coords of rectangle start
        (x2,y2) = window coords of rectangle finish         ***/

#if 1
   RWC_drag_rectangle( seq->wimage , x1,y1,&x2,&y2 ) ;
#else
   { int rad ;
     RWC_drag_circle( seq->wimage , x1,y1 , &rad ) ;  /** just a test **/
     fprintf(stderr,"rad=%d\n",rad) ; EXRETURN ;
   }
#endif

   /*** find corners of rectangle in original image pixels ***/

   ISQ_mapxy( seq , x1,y1 , &imx1,&imy1,&nim1 ) ;
   ISQ_mapxy( seq , x2,y2 , &imx2,&imy2,&nim2 ) ;

   /*** ensure coords of rectangle run upwards (upperleft to lowerright) ***/

   if( imx1 > imx2 ){ tt = imx1; imx1 = imx2; imx2 = tt; }
   if( imy1 > imy2 ){ tt = imy1; imy1 = imy2; imy2 = tt; }

   /*** if dragging occured across sub-images in a montage,
        or if rectangle edge is in a Montage's inter-image border */

   if( nim1 != nim2 || imx1 < 0 || imy1 < 0 ){
     static int npop=0 ;
     char str[64] ;
     if( npop < 5 ){
       sprintf(str,
               " \n  %s \n  crop\n  rectangle! \n\n[Crosses montage border]\n",
               Random_Insult()) ;
       MCW_popup_message( seq->wimage,str, MCW_USER_KILL|MCW_TIMER_KILL ) ;
       npop++ ;
     }
     XBell(seq->dc->display,100); goto CropDone;
   }

   /*** if crop window is too small, then deal with that ***/

   if( imx2-imx1 < MINCROP || imy2-imy1 < MINCROP ){ /* too small */
     if( imx2-imx1 < 2 || imy2-imy1 < 2 ){
       seq->cropit = 0 ; seq->crop_nxorg = seq->crop_nyorg = -1 ;  /* turn crop off */
     } else {
       XBell(seq->dc->display,100);                 /* do nothing */
     }

   /*** otherwise (not too small), set the crop region ***/

   } else {

     /* 14 Jun 2002: if we are also zoomed, things are more complex */

     if( zlev > 1 ){

       /* xmid = middle of crop region */
       /* xh   = half-width of crop region, as drawn */
       /* xhw  = half-width enlarged by zoom factor */

       int xmid=(imx2+imx1)/2, xh=(imx2-imx1)/2, xhw=zlev*xh ;
       int ymid=(imy2+imy1)/2, yh=(imy2-imy1)/2, yhw=zlev*yh ;
       int nx,ny ;
       float mh = (zlev-1.001)/zlev ;  /* max offset allowed */

       /* set size of original image from which cropping will be done */

       nx = (seq->crop_nxorg > 0) ? seq->crop_nxorg : seq->horig ;
       ny = (seq->crop_nyorg > 0) ? seq->crop_nyorg : seq->vorig ;
#if 0
fprintf(stderr,"Crop: imx1=%d imx2=%d xmid=%d xh=%d xhw=%d nx=%d\n",imx1,imx2,xmid,xh,xhw,nx);
fprintf(stderr,"      imy1=%d imy2=%d ymid=%d yh=%d yhw=%d ny=%d\n",imy1,imy2,ymid,yh,yhw,ny);
#endif

       /* cropping should run from imx1-xhw to imx2+xhw
          (since we want the image window to show what the user
           drew, so we have to crop a larger rectangle and then
           zoom in on THAT),
          but we can't go outside the original image boundaries,
          so we recompute imx1..imx2 here                       */

       imx1 = xmid-xhw ; imx2 = xmid+xhw ;
            if( imx1 <  0    ){ imx1 = 0   ; imx2 = imx1+2*xhw; }
       else if( imx2 >= nx-1 ){ imx2 = nx-1; imx1 = imx2-2*xhw; }
       imy1 = ymid-yhw ; imy2 = ymid+yhw ;
            if( imy1 <  0    ){ imy1 = 0   ; imy2 = imy1+2*yhw; }
       else if( imy2 >= ny-1 ){ imy2 = ny-1; imy1 = imy2-2*yhw; }

       /* set the offset for the zoom window so that we'll show
          the crop region just computed in the image display    */

       if( seq->opt.mirror )
         seq->zoom_hor_off = ((float)(imx2-xmid-xh))
                            /((float)(imx2-imx1)) ;
       else
         seq->zoom_hor_off = ((float)(xmid-xh-imx1))
                            /((float)(imx2-imx1)) ;

       seq->zoom_ver_off = ((float)(ymid-yh-imy1))
                          /((float)(imy2-imy1)) ;
#if 0
fprintf(stderr,"      imx1=%d imx2=%d hor_off=%f\n",imx1,imx2,seq->zoom_hor_off);
fprintf(stderr,"      imy1=%d imy2=%d ver_off=%f\n",imy1,imy2,seq->zoom_ver_off);
#endif

       /* safeguard: don't let the zoom window offset be out of range! */

            if( seq->zoom_hor_off > mh  ) seq->zoom_hor_off = mh  ;
       else if( seq->zoom_hor_off < 0.0 ) seq->zoom_hor_off = 0.0 ;
            if( seq->zoom_ver_off > mh  ) seq->zoom_ver_off = mh  ;
       else if( seq->zoom_ver_off < 0.0 ) seq->zoom_ver_off = 0.0 ;

     } /* end of mangling crop+zoom interaction */

     /* now set crop parameters */

     seq->crop_xa = imx1 ; seq->crop_xb = imx2 ;
     seq->crop_ya = imy1 ; seq->crop_yb = imy2 ;
     seq->cropit = 1 ; seq->crop_nxorg = seq->crop_nyorg = -1 ;
   }

   /*** force image redisplay ***/

CropDone:
   if( seq->crop_drag ){                       /* turn off crop */
     MCW_invert_widget( seq->crop_drag_pb ) ;  /* button, if on */
     seq->crop_drag = 0 ;
   }

   ISQ_redisplay( seq , -1 , isqDR_display ) ;
   EXRETURN ;
}

/**************************************************************************/
/*** 20 Jun 2003: snapshot stuff for recording the contents of a widget ***/

/*! Xt warning handler (to avoid messages to screen). */

static void SNAP_warnhandler(char * msg){ return ; }

/*----------------------------------------------------------------------*/

static MCW_imseq *snap_isq  = NULL ;
static MCW_DC    *snap_dc   = NULL ;  /* cf. SNAP_make_dc() */
static MRI_IMARR *snap_imar = NULL ;

static void SNAP_imseq_send_CB( MCW_imseq *, XtPointer, ISQ_cbs * ) ;

/*------------------------------------------------------------------*/

void ISQ_snap_agif( char *prefix )
{
   ISQ_save_anim( snap_isq , prefix , 0,0 , AGIF_MODE ) ;
}
void ISQ_snap_agif_rng( char *prefix , int a, int b )
{
   ISQ_save_anim( snap_isq , prefix , a,b , AGIF_MODE ) ;
}

/*------------------------------------------------------------------*/

void ISQ_snap_mpeg( char *prefix )
{
   ISQ_save_anim( snap_isq , prefix , 0,0 , MPEG_MODE ) ;
}
void ISQ_snap_mpeg_rng( char *prefix , int a, int b )
{
   ISQ_save_anim( snap_isq , prefix , a,b , MPEG_MODE ) ;
}

/*------------------------------------------------------------------*/

void ISQ_snap_jpeg( char *prefix )
{
   ISQ_save_anim( snap_isq , prefix , 0,0 , JPEG_MODE ) ;
}
void ISQ_snap_jpeg_rng( char *prefix , int a, int b )
{
   ISQ_save_anim( snap_isq , prefix , a,b , JPEG_MODE ) ;
}

/*------------------------------------------------------------------*/

void ISQ_snap_png( char *prefix )
{
   ISQ_save_anim( snap_isq , prefix , 0,0 , PNG_MODE ) ;
}
void ISQ_snap_png_rng( char *prefix , int a, int b )
{
   ISQ_save_anim( snap_isq , prefix , a,b , PNG_MODE ) ;
}

/*------------------------------------------------------------------
   Routine to provide data to the imseq.
   Just returns the control information, or the selected image.
--------------------------------------------------------------------*/

static XtPointer SNAP_imseq_getim( int n, int type, XtPointer handle )
{
   int ntot = 0 ;

ENTRY("SNAP_imseq_getim") ;

   if( snap_imar != NULL ) ntot = IMARR_COUNT(snap_imar) ;
   if( ntot < 1 ) ntot = 1 ;

   /*--- send control info ---*/

   if( type == isqCR_getstatus ){
     MCW_imseq_status *stat = myXtNew( MCW_imseq_status ) ; /* will be freed */
                                                            /* when imseq is */
                                                            /* destroyed    */
     stat->num_total  = ntot ;
     stat->num_series = ntot ;
     stat->send_CB    = SNAP_imseq_send_CB ;
     stat->parent     = NULL ;
     stat->aux        = NULL ;

     stat->transforms0D = NULL ;
     stat->transforms2D = NULL ;
     stat->slice_proj   = NULL ;

     RETURN( (XtPointer)stat ) ;
   }

   /*--- return a copy of an image
         (since the imseq will delete it when it is done) ---*/

   if( type == isqCR_getimage || type == isqCR_getqimage ){
     MRI_IMAGE *im = NULL , *rim ;

     if( snap_imar != NULL ){
       if( n < 0 ) n = 0 ; else if( n >= ntot ) n = ntot-1 ;
       rim = IMARR_SUBIMAGE(snap_imar,n) ;
       im  = mri_copy( rim ) ;
     }
     RETURN( (XtPointer)im );
   }

   RETURN( NULL ) ; /* all other cases */
}

/*---------------------------------------------------------------------------
   Routine called when the imseq wants to send a message.
   In this case, all we need to handle is the destroy message,
   so that we can free some memory.
-----------------------------------------------------------------------------*/

static void SNAP_imseq_send_CB( MCW_imseq *seq, XtPointer handle, ISQ_cbs *cbs )
{
ENTRY("SNAP_imseq_send_CB") ;
   switch( cbs->reason ){
     case isqCR_destroy:{
       myXtFree(snap_isq) ;         snap_isq  = NULL ;
       DESTROY_IMARR( snap_imar ) ; snap_imar = NULL ;
     }
     break ;
   }
   EXRETURN ;
}
/*------------------------------------------------------------------------*/
/*! Create display context if we don't have one.  [03 Jul 2003] */

static void SNAP_make_dc( Widget w )
{
ENTRY("SNAP_make_dc") ;
   if( snap_dc == NULL ){
     if( first_dc != NULL ) snap_dc = first_dc ;
     else{
       if( w == (Widget) NULL ){
         fprintf(stderr,"** Can't snapshot/save with NULL widget!\n") ;
         EXRETURN ;
       }
       (void ) XtAppSetWarningHandler( XtWidgetToApplicationContext(w),
                                       SNAP_warnhandler ) ;
       snap_dc = MCW_new_DC( w, 4,0, NULL,NULL, 1.0,0 ) ;
     }
   }
   EXRETURN ;
}

static int NoDuplicates = 1;
void SNAP_NoDuplicates (void) { NoDuplicates = 1; return; }
void SNAP_OkDuplicates (void) { NoDuplicates = 0; return; }

/*-------------------------------------------------------------------------*/
/*! Save image into a viewer, which should be opened near the widget w. */

static void SNAP_store_image( MRI_IMAGE *tim , Widget w )
{
ENTRY("SNAP_store_image") ;

   if( tim == NULL ) EXRETURN ;

   if( snap_imar == NULL ) INIT_IMARR(snap_imar) ;

   if( NoDuplicates && IMARR_COUNT(snap_imar) > 0 ){
     MRI_IMAGE *qim = IMARR_LASTIM( snap_imar ) ;
     if( mri_equal(qim,tim) ){
       fprintf(stderr,"++ Image recorder: reject duplicate image at #%d\n",
               IMARR_COUNT(snap_imar)-1 ) ;
       mri_free(tim); EXRETURN;
     }
   }

   ADDTO_IMARR(snap_imar,tim) ;

   /* create viewer, if not present already */

   if( snap_isq == NULL ){
     int xr,yr , wx,hy , xx,yy ;
     Position xroot,yroot ;
     Widget wpar ;

     SNAP_make_dc( w ) ; if( snap_dc == NULL ) EXRETURN ;

     snap_isq = open_MCW_imseq( snap_dc, SNAP_imseq_getim, NULL ) ;

     drive_MCW_imseq( snap_isq, isqDR_periodicmont, (XtPointer) 0 ) ;
     drive_MCW_imseq( snap_isq, isqDR_realize     , NULL          ) ;
     drive_MCW_imseq( snap_isq, isqDR_title       , "Snapshots"   ) ;

     /* put next to top shell of widget we are snapshotting */

     if( w != (Widget) NULL ){
       wpar = w ;
       while( XtParent(wpar) != NULL ) wpar = XtParent(wpar) ;  /* find top */
       XtTranslateCoords( wpar , 0,0 , &xroot,&yroot ) ;
       xr = (int) xroot ; yr = (int) yroot ;
       MCW_widget_geom( wpar , &wx,NULL , NULL,NULL ) ;
       xx = 1+wx+xr ; yy = 1+yr ;
       if( xx >= snap_dc->width-wx/3 ){
         XLowerWindow( snap_dc->display , XtWindow(wpar) ) ; xx = yy = 2 ;
       }
       XtVaSetValues( snap_isq->wtop , XmNx,xx , XmNy,yy , NULL ) ;
     }
   }

   /* tell the image viewer about the new image */

   if( IMARR_COUNT(snap_imar) > 1 ){
     int ii ;
     drive_MCW_imseq( snap_isq, isqDR_newseq      , NULL ) ;
     drive_MCW_imseq( snap_isq, isqDR_onoffwid    , (XtPointer)isqDR_onwid  );

     /* turn off some controls that don't make sense here */

     XtUnmanageChild( snap_isq->wbar ) ;
     XtUnmanageChild( snap_isq->arrowpad->wform ) ;
     for( ii=0 ; ii < NBUTTON_RIG ; ii++)
       XtUnmanageChild( snap_isq->wbut_rig[ii] ) ;
     for( ii=0 ; ii < NARROW-1 ; ii++ ) /* keep "i" arrow */
       XtUnmanageChild( snap_isq->arrow[ii]->wrowcol ) ;
     XtUnmanageChild( snap_isq->ov_opacity_sep ) ;
     XtUnmanageChild( snap_isq->ov_opacity_av->wrowcol ) ;
     XtUnmanageChild( snap_isq->winfo ) ;
     XtUnmanageChild( snap_isq->pen_bbox->wrowcol ) ;

   } else {
     drive_MCW_imseq( snap_isq, isqDR_onoffwid    , (XtPointer)isqDR_offwid );
   }

   /* force display of the new image */

   ISQ_redisplay( snap_isq , IMARR_COUNT(snap_imar)-1 , isqDR_display ) ;

   EXRETURN ;
}

/*----------------------------------------------------------------------*/
/*! Call this function to get a snapshot of a widget and save
    it into an image viewer.  Also see ISQ_snapsave().
------------------------------------------------------------------------*/

void ISQ_snapshot( Widget w )
{
   MRI_IMAGE *tim ;
   Window win ;

ENTRY("ISQ_snapshot") ;

   if( w == NULL || !XtIsWidget(w) )         EXRETURN ;
   if( !XtIsRealized(w) || !XtIsManaged(w) ) EXRETURN ;
   win = XtWindow(w); if( win == (Window)0 ) EXRETURN ;

   /* try to get image */

   SNAP_make_dc( w ) ; if( snap_dc == NULL ) EXRETURN ;

   tim = SNAP_grab_image( w , snap_dc ) ;
   if( tim == NULL )                         EXRETURN ;

   /* got image; save it and display it */

   SNAP_store_image( tim , w ) ;
   EXRETURN ;
}

/*----------------------------------------------------------------------------*/
/*! Called to add an image directly to the snapshot save sequence.
     - ww, hh = width and height of image
     - if(hh < 0) ==> flip image vertically (e.g., from glReadPixels)
     - pix = pointer to 3*ww*hh bytes of RGB data
     - w = Widget that the view should popup next to (can't be NULL)
     - RWCox - 03 Jul 2003
------------------------------------------------------------------------------*/

void ISQ_snapsave( int ww , int hh , byte *pix , Widget w )
{
   MRI_IMAGE *tim ;
   byte *qix ;
   int ii , jj , flip=0 ;

ENTRY("ISQ_snapsave") ;

   if( ww < 2 || pix == NULL ) EXRETURN ;
   if( hh < 0 ){ hh = -hh ; flip = 1 ; }
   if( hh < 2 ) EXRETURN ;

   SNAP_make_dc( w ) ; if( snap_dc == NULL ) EXRETURN ;

   tim = mri_new( ww,hh, MRI_rgb ) ; qix = MRI_RGB_PTR(tim) ;

   if( flip ){                    /* flipper, flipper, faster than lightning */
     for( jj=0 ; jj < hh ; jj++ )
       memcpy( qix+3*ww*(hh-jj-1) , pix+3*ww*jj , 3*ww ) ;
   } else {                                                   /* simple copy */
     memcpy( qix , pix , 3*ww*hh ) ;
   }

   SNAP_store_image( tim , w ) ;
   EXRETURN ;
}

/*----------------------------------------------------------------------------*/

void ISQ_pen_bbox_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   MCW_imseq *seq = (MCW_imseq *)client_data ;
   int val ;

ENTRY("ISQ_pen_bbox_CB") ;
   if( !ISQ_REALZ(seq) ) EXRETURN ;                 /* bad, but impossible */

   if( !seq->button2_enabled ){                     /* shouldn't happen */
     MCW_set_bbox( seq->pen_bbox , 0 ) ;
     ISQ_set_cursor_state( seq, CURSOR_NORMAL ) ;
     XtUnmanageChild( seq->pen_bbox->wrowcol ) ;
     EXRETURN ;
   }

   val = MCW_val_bbox( seq->pen_bbox ) ;
   ISQ_set_cursor_state( seq, (val==0) ? CURSOR_NORMAL : CURSOR_PENCIL ) ;
   EXRETURN ;
}

/*----------------------------------------------------------------------------*/
/*! Do something every so often. */

void ISQ_timer_CB( XtPointer cd , XtIntervalId *id ) /* 03 Dec 2003 */
{
   MCW_imseq *seq = (MCW_imseq *)cd ;
   int redo = 0 ;

ENTRY("ISQ_timer_CB") ;

   if( !ISQ_REALZ(seq) || seq->timer_id == 0 ) EXRETURN ;

   switch( seq->timer_func ){

     case ISQ_TIMERFUNC_INDEX:{
       int nn=seq->im_nr , nt=seq->status->num_total ;
       if( nt > 1 && seq->timer_param != 0 ){
         nn = (nn+seq->timer_param+nt) % nt ;
         ISQ_redisplay( seq , nn , isqDR_display ) ;
         redo = 1 ;
       }
     }
     break ;

     case ISQ_TIMERFUNC_BOUNCE:{
       int nn=seq->im_nr , nt=seq->status->num_total ;
       if( nt > 1 && seq->timer_param != 0 ){
         nn = nn + seq->timer_param ;
         if( nn <  0  ){
           nn = -nn; seq->timer_param = -seq->timer_param;
         } else if( nn >= nt ){
           nn = 2*(nt-1)-nn; seq->timer_param = -seq->timer_param;
         }
         ISQ_redisplay( seq , nn , isqDR_display ) ;
         redo = 1 ;
       }
     }
     break ;

   }

   if( redo ) seq->timer_id = XtAppAddTimeOut(
                               XtWidgetToApplicationContext(seq->wform) ,
                               seq->timer_delay , ISQ_timer_CB , seq ) ;
   else       seq->timer_id = 0 ;

   EXRETURN ;
}

void ISQ_timer_stop( MCW_imseq *seq )
{
ENTRY("ISQ_timer_stop") ;
   if( seq != NULL && seq->timer_id > 0 ){
     XtRemoveTimeOut(seq->timer_id); seq->timer_id = 0;
   }
   EXRETURN ;
}

/*--------------------------------------------------------------------*/
/*! Deal with a single keypress in an image viewer window.
    Return value is 1 if processed OK, 0 if not.
----------------------------------------------------------------------*/

int ISQ_handle_keypress( MCW_imseq *seq , unsigned long key )
{
   static int busy=0 ;   /* prevent recursion */

ENTRY("ISQ_handle_keypress") ;

   ISQ_timer_stop(seq) ;  /* 03 Dec 2003 */

   if( busy || key == 0 ) RETURN(1) ;
   busy = 1 ;

   /* 24 Jan 2003: deal with special function keys */

   if( key > 255 ){
     KeySym ks = (KeySym)key ;
     switch( ks ){

       case XK_Left:
       case XK_KP_Left:
         seq->arrowpad->which_pressed = AP_LEFT ;
         seq->arrowpad->xev.type = 0 ;
         ISQ_arrowpad_CB( seq->arrowpad , (XtPointer)seq ) ;
       break ;

       case XK_Right:
       case XK_KP_Right:
         seq->arrowpad->which_pressed = AP_RIGHT ;
         seq->arrowpad->xev.type = 0 ;
         ISQ_arrowpad_CB( seq->arrowpad , (XtPointer)seq ) ;
       break ;

       case XK_Down:
       case XK_KP_Down:
         seq->arrowpad->which_pressed = AP_DOWN ;
         seq->arrowpad->xev.type = 0 ;
         ISQ_arrowpad_CB( seq->arrowpad , (XtPointer)seq ) ;
       break ;

       case XK_Up:
       case XK_KP_Up:
         seq->arrowpad->which_pressed = AP_UP ;
         seq->arrowpad->xev.type = 0 ;
         ISQ_arrowpad_CB( seq->arrowpad , (XtPointer)seq ) ;
       break ;

       case XK_Page_Up:
       case XK_KP_Page_Up:
       case XK_Page_Down:
       case XK_KP_Page_Down:{
         int nn=seq->im_nr , nt=seq->status->num_total ;
         if( nt > 1 ){
           if( ks==XK_Page_Down || ks==XK_KP_Page_Down ){ nn--; if(nn< 0 ) nn=nt-1; }
           else                                         { nn++; if(nn>=nt) nn=0   ; }
#if 1
           ISQ_redisplay( seq , nn , isqDR_display ) ;
#else
           ISQ_set_image_number( seq , nn ) ;
#endif
         }
       }
       break ;

       case XK_Delete:              /* 20 Feb 2003: drawing undo */
       case XK_KP_Delete:
         if( seq->button2_enabled && seq->status->send_CB != NULL ){
           ISQ_cbs cbs ;
           cbs.reason   = isqCR_button2_key ;
           cbs.key      = (int) XK_Delete ;
#if 0
           seq->status->send_CB( seq , seq->getaux , &cbs ) ;
#else
           SEND(seq,cbs) ;
#endif
         }
       break ;

       /* 10 Mar 2003: change cursor state to drawing pencil */

       case XK_F2:{
         if( !seq->button2_enabled ){
           MCW_popup_message( seq->wimage,
                              " \n Only when \n"
                              " Drawing!! \n ", MCW_USER_KILL );
           XBell(seq->dc->display,100); busy=0; RETURN(0);
         }
         ISQ_set_cursor_state( seq ,
                               (seq->cursor_state == CURSOR_PENCIL)
                               ? CURSOR_NORMAL : CURSOR_PENCIL ) ;
       }
       break ;

       case XK_F4:
       case XK_F3:{                     /* 13 Sep 2008 */
         ISQ_cbs cbs ;
         if( !seq->button2_enabled ){
           MCW_popup_message( seq->wimage,
                              " \n Only when \n"
                              " Drawing!! \n ", MCW_USER_KILL );
           XBell(seq->dc->display,100); busy=0; RETURN(0);
         }
         cbs.reason = isqCR_button2_key ;
         cbs.key    = (int)ks ;
         SEND(seq,cbs) ;
       }
       break ;

       default:
       case XK_Home:
       case XK_F5:
       case XK_F6:
       case XK_F7:
       case XK_F8:
       case XK_F9:
       case XK_F10:
       case XK_F11:
       case XK_F12:
#if 0
         XBell(seq->dc->display,100) ;
         MCW_popup_message( seq->wimage, " \n Ouch! \n ", MCW_USER_KILL );
         AFNI_speak( "Ouch!" , 0 ) ;
#endif
       break ;
     }
     busy=0; RETURN(1) ;
   }

         /* 07 Dec 2002: modified ad hoc series of if-s into a switch */

   switch( key ){

     /* 10 Mar 2002: quit if 'q' or 'Q' is pressed */

     case 'q':
     case 'Q':{
       ISQ_but_done_CB( NULL, (XtPointer)seq, NULL ) ; NI_sleep(1) ;
       busy=0; RETURN(1) ;
     }
     break ;

     /* 03 Dec 2003: advance picture continuously? */

     case 'v':
     case 'V':{
       if( seq->button2_enabled ){
         MCW_popup_message( seq->wimage,
                               " \n Not when \n"
                               " Drawing! \n ", MCW_USER_KILL );
         XBell(seq->dc->display,100) ;
       } else if( seq->status->num_total > 1 ){      /* bring it on */
         seq->timer_func  = ISQ_TIMERFUNC_INDEX ;
         seq->timer_delay = (int) AFNI_numenv("AFNI_VIDEO_DELAY") ;
         if( seq->timer_delay <= 0 ) seq->timer_delay = 1 ;
         seq->timer_param = (key == 'v') ? 1 : -1 ;
         seq->timer_id    =
           XtAppAddTimeOut( XtWidgetToApplicationContext(seq->wform) ,
                            seq->timer_delay , ISQ_timer_CB , seq ) ;
       }
       busy=0; RETURN(1) ;
     }
     break ;

     case 'r':
     case 'R':{
       if( seq->button2_enabled ){
         MCW_popup_message( seq->wimage,
                              " \n Not when \n"
                              " Drawing! \n ", MCW_USER_KILL );
         XBell(seq->dc->display,100) ;
       } else if( seq->status->num_total > 1 ){      /* bring it on */
         seq->timer_func  = ISQ_TIMERFUNC_BOUNCE ;
         seq->timer_delay = (int) AFNI_numenv("AFNI_VIDEO_DELAY") ;
         if( seq->timer_delay <= 0 ) seq->timer_delay = 1 ;
         seq->timer_param = (key == 'r') ? 1 : -1 ;
         seq->timer_id    =
           XtAppAddTimeOut( XtWidgetToApplicationContext(seq->wform) ,
                            seq->timer_delay , ISQ_timer_CB , seq ) ;
       }
       busy=0; EXRETURN ;
     }
     break ;

     /* 07 Dec 2002: scroll forward or backward
                     using '<' or '>' keys (like graphs) */

     case '>':
     case '<':
     case ',':
     case '.':{
       int nn=seq->im_nr , nt=seq->status->num_total ;
       if( nt > 1 ){
         if( key == '<' || key == ',' ){ nn--; if( nn <  0 ) nn = nt-1; }
         else                          { nn++; if( nn >= nt) nn = 0   ; }
#if 1
         ISQ_redisplay( seq , nn , isqDR_display ) ;
#else
         ISQ_set_image_number( seq , nn ) ;
#endif
       }
       busy=0; RETURN(1) ;
     }
     break ;

     /* 05 Apr 2002: zoom out/in for 'z' or 'Z' */

     case 'z':
     case 'Z':{
       int call=0 , zlev=seq->zoom_fac ;
       if( key == 'z' && zlev > ZOOM_BOT ){
         AV_assign_ival( seq->zoom_val_av , zlev-1 ) ; call = 1 ;
       } else if( key == 'Z' && zlev < ZOOM_TOP ){
         AV_assign_ival( seq->zoom_val_av , zlev+1 ) ; call = 1 ;
       }
       if( call )
         ISQ_zoom_av_CB( seq->zoom_val_av , (XtPointer)seq ) ;
       else
         XBell(seq->dc->display,100) ;
       busy=0; RETURN(1) ;
     }
     break ;

     /* and toggle panning with 'p' or 'P' */

     case 'P':
     case 'p':{
       if( seq->zoom_fac > 1 )
         ISQ_zoom_pb_CB( seq->zoom_drag_pb , (XtPointer)seq , NULL ) ;
       else
         XBell(seq->dc->display,100) ;
       busy=0; RETURN(1) ;
     }
     break ;

     /* 17 Jun 2002: toggle cropping with 'c' or 'C' */

     case 'c':
     case 'C':{
       ISQ_crop_pb_CB( seq->crop_drag_pb , (XtPointer)seq , NULL ) ;
       busy=0; RETURN(1) ;
     }
     break ;

     /* 17 May 2002: do image fraction up or down */

     case 'i':
     case 'I':{
       int iv = seq->arrow[NARR_FRAC]->ival ;
       if( key == 'i' )
         AV_assign_ival( seq->arrow[NARR_FRAC] , iv-1 ) ;
       else if( key == 'I' )
         AV_assign_ival( seq->arrow[NARR_FRAC] , iv+1 ) ;
       ISQ_arrow_CB( seq->arrow[NARR_FRAC] , seq ) ;
       busy=0; RETURN(1) ;
     }
     break ;

     /* 22 Aug 2005: 'm' == Min-to-Max toggle */

     case 'm':{
       if( seq->dialog_starter==NBUT_DISP ){XBell(seq->dc->display,100); break;}
       switch( seq->opt.scale_range ){
         default:
         case ISQ_RNG_MINTOMAX: seq->opt.scale_range = ISQ_RNG_02TO98;  break;
         case ISQ_RNG_CLIPPED:  seq->opt.scale_range = ISQ_RNG_MINTOMAX;break;
         case ISQ_RNG_02TO98:   seq->opt.scale_range = ISQ_RNG_MINTOMAX;break;
       }

       ISQ_redisplay( seq , -1 , isqDR_display ) ;
       busy=0 ; RETURN(1) ;
     }
     break ;

     /* 22 Aug 2005: 'l' == LR mirror toggle */

     case 'l':{
       if( seq->dialog_starter==NBUT_DISP ){XBell(seq->dc->display,100); break;}
       seq->opt.mirror = ! seq->opt.mirror ;
       ISQ_redisplay( seq , -1 , isqDR_display ) ;
       busy=0 ; RETURN(1) ;
     }
     break ;

     /* 22 Aug 2005: 'a' = fix aspect ratio */

     case 'a':{
       int bx = seq->opt.free_aspect ; seq->opt.free_aspect = 0 ;
       ISQ_reset_dimen( seq, seq->last_width_mm, seq->last_height_mm ) ;
       seq->opt.free_aspect = bx ;
       busy=0 ; RETURN(1) ;
     }
     break ;

     /* 23 Aug 2005: 's' = sharpen */

     case 's':{
       if( seq->dialog_starter==NBUT_DISP ){XBell(seq->dc->display,100); break;}
       if( !(seq->opt.improc_code & ISQ_IMPROC_SHARP) ){
         seq->opt.improc_code |= ISQ_IMPROC_SHARP ;
       } else {
         int ss = (int)(10.01*seq->sharp_fac)+1 ;
         if( ss > 9 ) ss = 1 ;
         seq->sharp_fac = 0.1 * ss ;
       }
       ISQ_redisplay( seq , -1 , isqDR_display ) ;
       busy=0 ; RETURN(1) ;
     }
     break ;

     /* 26 Apr 2007: time indexing */

     case '[':
     case ']':{
       ISQ_cbs cbs ;
       cbs.reason = isqCR_setindex ;
       cbs.key    = (key == '[') ? -1 : +1 ;
       SEND(seq,cbs) ;
     }
     break ;  /** N.B.: '{' and '}' are reserved for threshold changing! **/

     /* 23 Aug 2005: open some windows */

     case 'D':
       ISQ_but_disp_CB( seq->wbut_bot[NBUT_DISP] , seq , NULL ) ;
       busy=0 ; RETURN(1) ;
     break ;

     case 'M':
       if( seq->status->num_total > 1 )
         ISQ_montage_CB( seq->wbut_bot[NBUT_MONT] , seq , NULL ) ;
       busy=0 ; RETURN(1) ;
     break ;

     case 'S':
       if( (seq->opt.save_one || seq->status->num_total > 1) )
         ISQ_but_save_CB( seq->wbut_bot[NBUT_SAVE] , seq , NULL ) ;
       busy=0 ; RETURN(1) ;
     break ;

     case '3':
     case '#':{
       int rr = (key == '#') ? RENDER_CHECK_UO : RENDER_CHECK_OU ;
       if( seq->render_mode == rr ) rr = 0 ;
       seq->render_mode = rr ;
       ISQ_redisplay( seq , -1 , isqDR_display ) ;
       ISQ_draw_winfo( seq ) ;
       busy=0 ; RETURN(1) ;
     }
     break ;

#if 0
     case 'G':
     case 'H':
     case 'J':
     case 'K':{
       int mode = (key=='G') ? AGIF_MODE
                 :(key=='H') ? MPEG_MODE
                 :(key=='J') ? JPEG_MODE
                 :             PNG_MODE  ;
       ISQ_save_anim( seq , NULL , 0,0 , mode ) ;
       busy=0 ; RETURN(1) ;
     }
     break ;
#endif

   } /* end of switch on character typed */

   busy=0; RETURN(0);
}

/*----------------------------------------------------------------------------*/
/*! Carry out a scalar filter on the intensity component of an RGB image.
    Afterwards, the color for each pixel is rescaled from the old to the
    new by the filtered intensity.  This transform is done in-place.
------------------------------------------------------------------------------*/

void mri_rgb_transform_nD( MRI_IMAGE *im, int ndim, generic_func *tfunc )
{
   MRI_IMAGE *flim , *shim ;
   byte  *iar ;
   float *sar , *far ;
   int ii , nvox , rr,gg,bb ;
   float fac , smax,fmax,fsrat ;

ENTRY("mri_rgb_transform_nD") ;

   if( im    == NULL || im->kind != MRI_rgb     ) EXRETURN ;  /* bad image? */
   if( tfunc == NULL || (ndim !=0 && ndim != 2) ) EXRETURN ;  /* bad tfunc? */

   flim = mri_to_float( im ) ;              /* input intensity image */
   fmax = mri_max( flim ) ;
   if( fmax == 0.0 ){ mri_free(flim); EXRETURN; }  /* nothing to do? */

   shim = mri_copy( flim ) ;        /* will be transformed intensity */

   switch( ndim ){               /* call in-place transform function */
     case 0:
       AFNI_CALL_0D_function( tfunc , shim->nvox , MRI_FLOAT_PTR(shim) ) ;
     break ;

     case 2:
       AFNI_CALL_2D_function( tfunc ,
                              shim->nx , shim->ny ,
                              shim->dx , shim->dy , MRI_FLOAT_PTR(shim) ) ;
     break ;
   }

   /* get scale factor to adjust for changes in overall amplitude */

   smax = mri_max(shim) ;
   if( smax == 0.0 ){ mri_free(flim); mri_free(shim); EXRETURN; }
   fsrat = fmax / smax ;

   iar = MRI_BYTE_PTR(im) ;
   far = MRI_FLOAT_PTR(flim) ; sar = MRI_FLOAT_PTR(shim) ;

   /* loop over pixels,
      adjusting the color of each one by the transformed intensity */

   nvox = im->nvox ;
   for( ii=0 ; ii < nvox ; ii++ ){
     if( far[ii] <= 0.0 || sar[ii] <= 0.0 ){       /* inten <= 0? */
       iar[3*ii] = iar[3*ii+1] = iar[3*ii+2] = 0 ;
     } else {
       fac = fsrat * sar[ii] / far[ii] ; /* will be positive */
       rr  = fac * iar[3*ii]   ; iar[3*ii  ] = (rr > 255) ? 255 : rr ;
       gg  = fac * iar[3*ii+1] ; iar[3*ii+1] = (gg > 255) ? 255 : gg ;
       bb  = fac * iar[3*ii+2] ; iar[3*ii+2] = (bb > 255) ? 255 : bb ;
     }
   }

   mri_free(flim) ; mri_free(shim) ;  /* toss the trash */
   EXRETURN ;
}

/*--------------------------------------------------------------------------*/
/*! Save the current image to a file thru a filter.
    - Refactored from the former ISQ_save_jpeg() - 11 Dec 2006
    - Modified for use as a filter (no fname or suffix) - 14 Dec 2006 */

void ISQ_save_image( MCW_imseq *seq  , char *fname ,
                     char *filtername, char *suffix )
{
   MRI_IMAGE *tim , *flim ;
   char fn[299], filt[512] ;
   FILE *fp ;
   int sll ;

ENTRY("ISQ_save_image") ;

   if( !ISQ_REALZ(seq) || filtername == NULL ) EXRETURN;

   if( fname != NULL ){
     sll = strlen(fname) ; if( sll < 1 || sll > 255 ) EXRETURN ;
   }
   if( filtername == NULL ){
     if( fname == NULL ){ filtername = "cat > AFNI.ppm" ;             }
     else               { filtername = "cat > %s" ; suffix = ".ppm" ; }
   }

   /*-- get image that's stored for display, then process it --*/

   reload_DC_colordef( seq->dc ) ;
   tim = XImage_to_mri( seq->dc, seq->given_xim, X2M_USE_CMAP | X2M_FORCE_RGB );
   if( tim == NULL ) EXRETURN ;

   /** make the image square? **/

/* INFO_message("AFNI_IMAGE_SAVESQUARE = %s",getenv("AFNI_IMAGE_SAVESQUARE")); */
   if( AFNI_yesenv("AFNI_IMAGE_SAVESQUARE") ){
     tim->dx = seq->last_dx ; tim->dy = seq->last_dy ;
     flim = mri_squareaspect( tim ) ;
     if( flim != NULL ){ mri_free(tim); tim = flim; }
   }

   /** zoom? **/

   if( seq->zoom_fac > 1 && seq->mont_nx == 1 && seq->mont_ny == 1 ){
     flim = mri_dup2D(seq->zoom_fac,tim) ;
     if( flim != NULL ){ mri_free(tim); tim = flim; }
   }

   /** line drawing overlay? **/

   if( seq->mplot != NULL )
     memplot_to_RGB_sef( tim, seq->mplot, 0,0,MEMPLOT_FREE_ASPECT ) ;

   /** cut up zoomed image? **/

   if( seq->zoom_fac >  1               &&
       seq->mont_nx  == 1               &&
       seq->mont_ny  == 1               &&
       AFNI_yesenv("AFNI_CROP_ZOOMSAVE")  ) {

      int xa,ya , iw=tim->nx/seq->zoom_fac , ih=tim->ny/seq->zoom_fac ;

      xa = seq->zoom_hor_off * tim->nx ;
      if( xa+iw > tim->nx ) xa = tim->nx-iw ;
      ya = seq->zoom_ver_off * tim->nx ;
      if( ya+ih > tim->ny ) ya = tim->ny-ih ;
      flim = mri_cut_2D( tim , xa,xa+iw-1 , ya,ya+ih-1 ) ;
      if( flim != NULL ){ mri_free(tim); tim = flim; }
   }

   /** open a pipe to the filter function **/

   if( fname != NULL ){
     strcpy(fn,fname) ;
     if( suffix != NULL && *suffix != '\0' &&
         !STRING_HAS_SUFFIX_CASE(fname,suffix) ){
       if( *suffix != '.' ) strcat(fn,".") ;
       strcat(fn,suffix) ;
     }
     sprintf( filt , filtername , fn ) ;
   } else {
     strcpy( filt , filtername ) ;
   }
   INFO_message("Writing one %dx%d image to filter '%s'",tim->nx,tim->ny,filt) ;

#ifndef CYGWIN
   signal( SIGPIPE , SIG_IGN ) ;
#endif
   errno = 0 ; fp = popen( filt , "w" ) ;
   if( fp == NULL ){
     ERROR_message("Can't open output filter: %s",filt) ;
     if( errno != 0 ) perror("** Unix error message") ;
     mri_free(tim) ; EXRETURN ;
   }

   /** write a PPM file to the filter pipe **/

   fprintf(fp,"P6\n%d %d\n255\n" , tim->nx,tim->ny ) ;
   fwrite( MRI_RGB_PTR(tim), sizeof(byte), 3*tim->nvox, fp ) ; fflush(fp) ;
   errno = 0 ; sll = pclose(fp) ;
   if( sll == -1 ){
     ERROR_message("Image save filter command was %s\n",filt) ;
     if( errno != 0 ) perror("** Unix error in image output pipe") ;
   }

   mri_free(tim) ; EXRETURN ;
}

/*--------------------------------------------------------------------------*/

void ISQ_save_jpeg( MCW_imseq *seq , char *fname )
{
   ISQ_save_image( seq , fname , ppmto_jpg95_filter , ".jpg" ) ;
   return ;
}

void ISQ_save_png( MCW_imseq *seq , char *fname )  /* 11 Dec 2006 */
{
   ISQ_save_image( seq , fname , ppmto_png_filter , ".png" ) ;
   return ;
}

/*--------------------------------------------------------------------------*/

void ISQ_save_raw( MCW_imseq *seq , char *fname )  /* 13 Nov 2007 */
{
   MRI_IMAGE *im ;

ENTRY("ISQ_save_raw") ;
   if( !ISQ_REALZ(seq) ) EXRETURN ;
   if( fname == NULL || *fname == '\0' ) fname = "image.raw" ;

   im = ISQ_getimage( seq->im_nr , seq ) ;

   if( im != NULL ){
     INFO_message("Writing one %dx%d raw image (type=%s bytes=%d) to file '%s'",
                  im->nx,im->ny,MRI_TYPE_name[im->kind],im->nvox*im->pixel_size,fname ) ;
     mri_write_raw(fname,im); mri_free(im);
   }
   EXRETURN ;
}

/*--------------------------------------------------------------------------*/

void ISQ_save_rawmont( MCW_imseq *seq , char *fname ) /* 13 Nov 2007 */
{
   MRI_IMAGE *im ;
   MRI_IMARR *mar ;
   int nmont=seq->mont_nx * seq->mont_ny ,ij,nim,ijcen,nxyim ;

ENTRY("ISQ_save_raw_montage") ;
   if( !ISQ_REALZ(seq) ) EXRETURN ;

   if( nmont < 2 ){
     INFO_message("save_rawmont: montage not turned on") ;
     ISQ_save_raw(seq,fname); EXRETURN;
   }

   if( fname == NULL || *fname == '\0' ) fname = "image_montage.raw" ;

   /* the following code is mostly from ISQ_make_montage() */

   INIT_IMARR(mar) ;

   ijcen = (seq->mont_nx)/2 + (seq->mont_ny/2) * seq->mont_nx ;
   for( nxyim=ij=0 ; ij < nmont ; ij++ ){
      nim = seq->im_nr + (seq->mont_skip + 1) * (ij - ijcen) ;
      im  = ISQ_getimage( nim , seq ) ; if( im != NULL ) nxyim++ ;
      ADDTO_IMARR(mar,im) ;
   }
   if( nxyim == 0 ){
     ERROR_message("Raw montage error: no images found!") ;
     DESTROY_IMARR(mar) ; EXRETURN ;
   }

   im = mri_cat2D( seq->mont_nx , seq->mont_ny , 0 , NULL , mar ) ;
   DESTROY_IMARR(mar) ;

   if( im != NULL ){
     INFO_message("Writing one %dx%d raw image (type=%s bytes=%d) to file '%s'",
                  im->nx,im->ny,MRI_TYPE_name[im->kind],im->nvox*im->pixel_size,fname ) ;
     mri_write_raw(fname,im); mri_free(im);
   } else {
     ERROR_message("Can't make raw montage for some reason!") ;
   }

   EXRETURN ;
}

/*--------------------------------------------------------------------------*/
/*! Save the current images to an MPEG or AGIF file.  [06 Dec 2006]
      Say the recorder has N images       ZSS Jan 07
   if top < 0 then top = N +top
      top ==0 then top = N -1
   else top = min(top, N)
   if bot < 0 then bot = N +bot
   else bot = max(bot, 0)

   use bot = 0 and top = 0 to save everything.
*/

void ISQ_save_anim( MCW_imseq *seq, char *prefin, int bot, int top, int mode )
{
   int ii , kf , ll ;
   MRI_IMAGE *tim , *flim ;
   char fname[256] , *prefix ;
   THD_string_array *agif_list=NULL ;
   char tsuf[8] ;
   float dx,dy ;
#ifndef USE_GIFF
   char *togif = ppmto_gif_filter ;
#else
   char *togif = ppmto_giff_filter ;
#endif
   int doanim=0 ;
   char filt[512], *ppo = NULL; FILE *fp ; MRI_IMAGE *ovim ;
   int nx , ny , npix , pc ;
   int adup=1 , akk,aa ;        /* 10 Feb 2009 */

ENTRY("ISQ_save_anim") ;

   if( !ISQ_REALZ(seq) ) EXRETURN ;  /* bad input */
   switch( mode ){
     default: EXRETURN ;             /* bad input */

     case AGIF_MODE:
       if( ppmto_agif_filter == NULL || togif == NULL ){
         ERROR_message("Can't save AGIF - missing filter!\a") ; EXRETURN ;
       }
       doanim = 1 ;
     break ;

     case MPEG_MODE:
       if( ppmto_mpeg_filter == NULL || ppmto_ppm_filter == NULL ){
         ERROR_message("Can't save MPEG - missing filter!\a") ; EXRETURN ;
       }
       doanim = 1 ;
     break ;

     case JPEG_MODE:
       if( ppmto_jpg95_filter == NULL ){
         ERROR_message("Can't save JPEG - missing filter!\a") ; EXRETURN ;
       }
       doanim = 0 ;
     break ;

     case PNG_MODE:
       if( ppmto_png_filter == NULL ){
         ERROR_message("Can't save PNG - missing filter!\a") ; EXRETURN ;
       }
       doanim = 0 ;
     break ;
   }

  if (bot < 0) { /* special case */
      bot = seq->status->num_total+bot ;
      if (bot < 0) bot = 0;
   } else {
      bot = MAX(bot,0) ;
   }
   if( top < 0 ) {
      top = seq->status->num_total+top ;
      if (top > seq->status->num_total-1) top = seq->status->num_total-1;
   } else if (top == 0) {
      top = seq->status->num_total -1;
   }  else  {
      top = MIN(top,seq->status->num_total-1) ;
   }
   if( bot > top || (bot==top && doanim) ){
     ERROR_message("Can't save image range %d..%d!\a",bot,top) ; EXRETURN ;
   }

   /*
      fprintf(stderr,
         "+++ Will save from %d to %d with %d images total in recorder.\n",
               bot, top, seq->status->num_total);
   */
   /*--- setup prefix for animation filename to save ---*/

   if( prefin == NULL || *prefin == '\0' ) prefin = "Anim" ;
   if( !THD_filename_ok(prefin) ){
     ERROR_message("Bad image save filename '%s'\a",prefin) ; EXRETURN ;
   }
   ll = strlen(prefin) ;
   prefix = (char*)malloc( sizeof(char) * (ll+8) ) ;
   strcpy( prefix , prefin ) ;

   ppo = THD_trailname(prefix,0) ;               /* strip directory */

   if( prefix[ll-1] != '.' ){  /* add a . at the end */
     prefix[ll++] = '.' ;      /* if one isn't there */
     prefix[ll]   = '\0' ;
   }

   tsuf[0] = (lrand48()>>5)%26 + 'A' ;   /* random suffix */
   tsuf[1] = (lrand48()>>5)%26 + 'A' ;   /* for animation */
   tsuf[2] = (lrand48()>>5)%26 + 'A' ;   /* temp files    */
   tsuf[3] = '\0' ;

#ifdef USE_GIFF          /* create the fixed GIF colormap for animations */
   if( mode == AGIF_MODE ){
     MRI_IMAGE *im = mri_colorsetup( 76 , 6,6,5 ); /* 76 grays + */
     remove( GIFF_MAPFILE ) ;                     /* 6*red X 6*green X 5*blue */
     mri_write_pnm( GIFF_MAPFILE , im ) ;
     mri_free( im ) ;
   }
#endif

   if( mode == AGIF_MODE || mode == MPEG_MODE ){
     adup = (ISQ_anim_dup > 0) ? ISQ_anim_dup : AFNI_numenv("AFNI_ANIM_DUP") ;
     if( adup <= 0 ) adup = 1 ; else if( adup > 99 ) adup = 99 ;
   }


   /*---- loop thru, get images, save them ----*/

   if( doanim )
     INFO_message("Starting to save images to temp files") ;
   else
     INFO_message("Starting to save images") ;

   for( akk=0,kf=bot ; kf <= top ; kf++ ){

      /* get the underlay image */

      tim = ISQ_getimage( kf , seq ) ;

      /* if we failed to get the image? */

      if( tim == NULL ) continue ;  /* skip to next one? */

      /* image to save will be in flim */

      flim = tim ;

      /* process image to make the grayscale index */

      seq->set_orim = 0 ;
      tim  = flim ;
      flim = ISQ_process_mri( kf , seq , tim ) ;
      if( tim != flim ) KILL_1MRI( tim ) ;

      /* get overlay and flip it */

      ovim = NULL ;
      if( !ISQ_SKIP_OVERLAY(seq) ){
        tim = ISQ_getoverlay( kf , seq ) ;
        if( tim != NULL && !ISQ_GOOD_OVERLAY_TYPE(tim->kind) ){
          KILL_1MRI(tim) ;
        }
        if( tim != NULL )
         ovim = mri_flippo( ISQ_TO_MRI_ROT(seq->opt.rot), seq->opt.mirror, tim );
        if( tim != ovim ) KILL_1MRI(tim) ;
      }

      /* and perform overlay onto flim */

      if( ovim != NULL ){
        tim = flim ;
        flim = ISQ_overlay( seq->dc , tim , ovim , seq->ov_opacity ) ;
        if( flim == NULL ){ flim = tim ; }     /* shouldn't happen */
        else              { KILL_1MRI(tim) ; }
        mri_free( ovim ) ;
      }

/* INFO_message("AFNI_IMAGE_SAVESQUARE = %s",getenv("AFNI_IMAGE_SAVESQUARE")); */
      if( AFNI_yesenv("AFNI_IMAGE_SAVESQUARE") ){   /* 08 Jun 2004 */
        flim->dx = seq->last_dx ; flim->dy = seq->last_dy ;
        tim = mri_squareaspect( flim ) ;
        if( tim != NULL ){ mri_free(flim); flim = tim; }
      }

      /* if needed, convert from indices to RGB */

      if( flim->kind == MRI_short ){
        tim = ISQ_index_to_rgb( seq->dc , 0 , flim ) ;
        mri_free(flim) ; flim = tim ;
      }

      /* 26 Mar 2002: zoom out, and geometry overlay, maybe */

      if( seq->zoom_fac > 1 && seq->mont_nx == 1 && seq->mont_ny == 1 ){
        tim=mri_dup2D(seq->zoom_fac,flim) ;
        mri_free(flim) ; flim = tim ;
      }

      if( MCW_val_bbox(seq->wbar_plots_bbox) != 0 ){  /* draw geometry overlay */
        MEM_plotdata *mp ;
        mp = ISQ_getmemplot( kf , seq ) ;
        if( mp != NULL ){
          flip_memplot( ISQ_TO_MRI_ROT(seq->opt.rot),seq->opt.mirror,mp );
          memplot_to_RGB_sef( flim, mp, 0,0,MEMPLOT_FREE_ASPECT ) ;
          delete_memplot(mp) ;
        }
      }

      if( seq->wbar_label_av->ival != 0 ){  /* 17 Jun 2005 */
        char *lab = ISQ_getlabel( kf , seq ) ;
        if( lab != NULL ){
          MEM_plotdata *mp = ISQ_plot_label( seq , lab ) ;
          if( mp != NULL ){
            memplot_to_RGB_sef( flim, mp, 0,0,MEMPLOT_FREE_ASPECT ) ;
            delete_memplot(mp) ;
          }
          free(lab) ;
        }
      }

      if( seq->zoom_fac > 1 &&                   /* crop zoomed image */
          seq->mont_nx == 1 &&                   /* to displayed part? */
          seq->mont_ny == 1 &&
          AFNI_yesenv("AFNI_CROP_ZOOMSAVE") ) {

        int xa,ya , iw=flim->nx/seq->zoom_fac , ih=flim->ny/seq->zoom_fac ;

        xa = seq->zoom_hor_off * flim->nx ;
        if( xa+iw > flim->nx ) xa = flim->nx-iw ;
        ya = seq->zoom_ver_off * flim->nx ;
        if( ya+ih > flim->ny ) ya = flim->ny-ih ;
        tim = mri_cut_2D( flim , xa,xa+iw-1 , ya,ya+ih-1 ) ;
        if( tim != NULL ){ mri_free(flim); flim = tim; }
      }

      /* image dimensions we are saving */

      nx = flim->nx ; ny = flim->ny ; npix = nx*ny ;

      /* create the filter command into string 'filt' */

      for( aa=0 ; aa < adup ; aa++,akk++ ){ /* adup==1 if no animation */

        switch( mode ){
          case AGIF_MODE:
            sprintf( fname, "%s%s.%05d.gif" , prefix,tsuf, akk) ;
            sprintf( filt , togif  , fname ) ;  /* free colormap */
            if( agif_list == NULL ) INIT_SARR(agif_list) ;
            ADDTO_SARR(agif_list,fname) ;
          break ;

          case MPEG_MODE:
            sprintf( fname, "%s%s.%06d.ppm" , ppo,tsuf, akk) ;
            sprintf( filt , ppmto_ppm_filter , fname ) ;
            if( agif_list == NULL ) INIT_SARR(agif_list) ;
            ADDTO_SARR(agif_list,fname) ;
          break ;

          case JPEG_MODE:
            sprintf( fname, "%s%05d.jpg" , prefix, kf) ;
            sprintf( filt , ppmto_jpg95_filter , fname ) ;
            if( agif_list == NULL ) INIT_SARR(agif_list) ;
            ADDTO_SARR(agif_list,fname) ;
          break ;

          case PNG_MODE:
            sprintf( fname, "%s%05d.png" , prefix, kf) ;
            sprintf( filt , ppmto_png_filter , fname ) ;
            if( agif_list == NULL ) INIT_SARR(agif_list) ;
            ADDTO_SARR(agif_list,fname) ;
          break ;
        }
#ifndef CYGWIN
        signal( SIGPIPE , SIG_IGN ) ;                 /* ignore broken pipe */
#endif
        fp = popen( filt , "w" ) ;                    /* open pipe to filter */
        if( fp == NULL ){
          ERROR_message("Can't open output filter %s\a",filt) ;
          break ;  /* out of loop over aa */
        }

        /* write RGB image to pipe as a PPM file */

        fprintf(fp,"P6\n%d %d\n255\n" , nx,ny ) ;
        fwrite( MRI_RGB_PTR(flim), sizeof(byte), 3*npix, fp ) ;
        pc = pclose(fp) ;
        if( pc == -1 ) perror("Error in image output pipe") ;

      } /* end of loop over aa = image duplicates for animation */

      /* done with this image */

      mri_free(flim) ; flim = NULL ;

   } /* end of loop over image sequence to save */

   /** post-process saved images into animation? **/

   if( agif_list != NULL && agif_list->num > 0 && doanim ){

     int af ;

     switch( mode ){
      /* animated GIF */

      case AGIF_MODE:{
        int alen ; char *alc , *alf , *oof ;
#ifdef USE_GIFF
        remove( GIFF_MAPFILE ) ;   /* don't need this any longer */
#endif
        for( alen=af=0 ; af < agif_list->num ; af++ ) /* size of all */
          alen += strlen( agif_list->ar[af] ) ;       /* filenames  */

        alen += 3*agif_list->num + 32 ;               /* all filenames */
        alc = AFMALL ( char, alen) ; alc[0] = '\0' ;  /* in one string */
        for( alen=af=0 ; af < agif_list->num ; af++ ){
          strcat(alc," ") ; strcat(alc,agif_list->ar[af]) ;
        }

        oof  = AFMALL( char, strlen(prefix)+32 ) ; /* output fname */
        sprintf(oof,"%sgif",prefix) ;

        alen =  strlen(alc)+strlen(ppmto_agif_filter)+strlen(oof)+32 ;
        alf  = AFMALL( char, alen) ;
        sprintf(alf , ppmto_agif_filter, alc, oof ) ; /* command to run */
        INFO_message("Running '%s'",alf) ;
        if( THD_is_ondisk(oof) ) WARNING_message("Over-writing '%s'",oof);
        system(alf) ;                                 /* so run it!    */
        free(alf) ; free(oof) ; free(alc) ;           /* free trash   */
      }
      break ;

      /* MPEG-1 */

      case MPEG_MODE:{
        int alen ; char *alf , *oof , *par , *frate ;
        char *qscale , *pattrn ;
        FILE *fpar ;

        /* write mpeg_encode parameter file */

        par = AFMALL( char, strlen(ppo)+32 ) ; /* param fname */
        sprintf(par,"%s%s.PARAM",ppo,tsuf) ;

        fpar = fopen( par , "w" ) ;
        if( fpar == NULL ){ free(par) ; break ; }
        oof = AFMALL( char, strlen(prefix)+32 ) ; /* output fname */
        sprintf(oof,"%smpg",prefix) ;
        qscale=getenv("AFNI_MPEG_QSCALE") ;if(qscale==NULL) qscale="11"   ;
        pattrn=getenv("AFNI_MPEG_PATTERN");if(pattrn==NULL) pattrn="IIIII";
        frate =getenv("AFNI_MPEG_FRAMERATE");if(frate==NULL)frate ="24"   ;
        fprintf(fpar,
                  "OUTPUT %s\n"             /* oof */
                  "GOP_SIZE          5\n"
                  "SLICES_PER_FRAME  1\n"
                  "FRAME_RATE        %s\n"  /* frate */
                  "BASE_FILE_FORMAT  PPM\n"
                  "INPUT_CONVERT     *\n"
                  "INPUT_DIR         .\n"
                  "PATTERN           %s\n"  /* pattrn */
                  "IQSCALE           %s\n"  /* qscale */
                  "PQSCALE           10\n"
                  "BQSCALE           25\n"
                  "PIXEL             HALF\n"
                  "RANGE             10 4\n"
                  "PSEARCH_ALG       LOGARITHMIC\n"
                  "BSEARCH_ALG       SIMPLE\n"
                  "REFERENCE_FRAME   ORIGINAL\n"
                  "INPUT\n"
                  "%s%s.*.ppm [%06d-%06d]\n"  /* prefix, tsuf, from, to */
                  "END_INPUT\n"
               , oof , frate , pattrn , qscale ,
                 ppo,tsuf,0,akk) ;
        fclose(fpar) ;

        /* make command to run */

        alen = strlen(par)+strlen(ppmto_mpeg_filter)+32 ;
        alf  = AFMALL( char, alen) ;
        sprintf(alf , ppmto_mpeg_filter, par ) ; /* command to run */
        INFO_message("Running '%s' to produce %s",alf,oof) ;
        if( THD_is_ondisk(oof) ) WARNING_message("Over-writing '%s'",oof);
        system(alf) ;                            /* so run it!    */
        remove(par); free(alf); free(oof); free(par); /* free trash   */
      }
      break ;
     }

     /* animation is done, for good or for ill */

     for( af=0 ; af < agif_list->num ; af++ )  /* erase temp files */
       remove( agif_list->ar[af] ) ;
     INFO_message("Done saving images") ;

   } else if( agif_list != NULL && agif_list->num > 0 ){
     if( agif_list->num > 1 )
       INFO_message("%d images saved in files %s .. %s",
                    agif_list->num ,
                    agif_list->ar[0] , agif_list->ar[agif_list->num-1] ) ;
     else
       INFO_message("1 image saved in file %s",agif_list->ar[0]) ;
   }

   /*--- go home ---*/

   DESTROY_SARR(agif_list) ; free(prefix) ; EXRETURN ;
}
