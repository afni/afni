#include <stdio.h>
#include <string.h>

#include "mrilib.h"

#include "imseq.h"
#include "xutil.h"
#include "xim.h"

#define PR(str) (printf("imseq: %s\n",str),fflush(stdout))

#ifdef IMSEQ_DEBUG
#  define DPR(str)      (printf("imseq: %s\n",str)       ,fflush(stdout))
#  define DPRI(str,ijk) (printf("imseq: %s %d\n",str,ijk),fflush(stdout))
#else
#  define DPR(str)
#  define DPRI(str,ijk)
#endif

#define COLSIZE 20  /* for optmenus */

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
#define NBUT_DISP4  2  /* Range scaling box */
#define NBUT_DISP5  2  /* Auto- or Group- scale box */
#define NBUT_DISP6  1  /* Free aspect box */
#define NBUT_DISP7  3  /* Save box */
#define NBUT_DISP8  3  /* IMPROC buttons */
#define NBUT_DISP9  4  /* CX buttons */

#define NTOG_ROT  0  /* index of which button box control which option(s) */
#define NTOG_MIR  1
#define NTOG_COL  2
#define NTOG_RNG  3
#define NTOG_SCL  4
#define NTOG_ASP  5
#define NTOG_SAV  6
#define NTOG_IMP  7
#define NTOG_CX   8

static char * ISQ_dl1[NBUT_DISP1] = {
   "No Rotation" , "CCW 90" , "Rot 180" , "CW 90" } ;
static char * ISQ_dl2[NBUT_DISP2] = { "+ LR Mirror" } ;
static char * ISQ_dl3[NBUT_DISP3] = { "No Overlay" } ;
static char * ISQ_dl4[NBUT_DISP4] = { "Min-to-Max" , "2%-to-98%" } ;
static char * ISQ_dl5[NBUT_DISP5] = { "Autoscale" , "Groupscale" } ;
static char * ISQ_dl6[NBUT_DISP6] = { "Free Aspect" } ;
static char * ISQ_dl7[NBUT_DISP7] = { "Nsize Save" , "PNM Save" , "Save One" } ;
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
 "Intensities mapped\nover partial range of data\n(%ages in data histogram)"
} ;

static char * ISQ_bb4_hint[NBUT_DISP4] = {
 "Background intensity = min to max pixel values" ,
 "Background intensity = 2% to 98% pixel values"
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
 "       OUT= saved images are background data only\n" ,

 "One:   IN = only the current image will be saved (PNM format)\n"
 "       OUT= a range of slices can be saved\n"

} ;

static char * ISQ_bb7_hint[NBUT_DISP7] = {
 "IN: Save background images in power-of-2 sizes" ,
 "IN: Save background images in PNM format" ,
 "IN: Save one image in PNM format"
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

            Thus, get_image takes as input 2 "int"s and an "XtPointer",
            and returns a "XtPointer".  Note that the MRI_IMAGEs returned
            will be mri_free-d after being used internally.

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

static char * ISQ_but_done_label1 = "Done" ;
static char * ISQ_but_done_label2 = "DONE" ;
#define NBUT_DONE (NBUTTON_BOT-1)
#define NBUT_SAVE 1
#define NBUT_DISP 0
#define NBUT_MONT 2

static char * ISQ_save_label_bg  = "Save:bkg" ;
static char * ISQ_save_label_all = "Save:pnm" ;
static char * ISQ_save_label_one = "Save:one" ;

#define SET_SAVE_LABEL(seq) \
   MCW_set_widget_label( (seq)->wbut_bot[NBUT_SAVE] ,             \
                         (seq)->opt.save_pnm ? ISQ_save_label_all \
                        :(seq)->opt.save_one ? ISQ_save_label_one \
                                             : ISQ_save_label_bg )

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

   "Will popup control panels to let you save slices from this window.\n"
   "The type of save operation is indicated on the button label, and\n"
   "is selected from the 'Disp' button options window.\n"
   " :bkg = Will save only the background image data values\n"
   " :pnm = Will save the actual displayed image in color (PNM format)\n"
   " :one = Will save just the single frame displayed on the screen\n"
   "        using the PNM format\n"
   "NOTES:\n"
   " * Saved images will NOT be stretched to match window resizing.\n"
   " * Only the Save:one mode will reflect the use of Montages.\n"
   "    Because of this, use of the Mont button to create anything\n"
   "    but a 1x1 layout will switch this Save button to Save:one.\n"
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
     "*          -1996  Milwaukee, WI 53226-0509     *\n"
     "*                                              *\n"
     "* Author:  Robert W. Cox, Ph.D                 *\n"
     "* E-mail:  rwcox@mcw.edu                       *\n"
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

MCW_imseq * open_MCW_imseq( MCW_DC * dc ,
                            get_ptr get_image , XtPointer aux )
{
   MCW_imseq        * newseq ;
   MCW_imseq_status * imstatus ;
   int ii , xwide , yhigh , one_image ;
   float fac ;
   MRI_IMAGE * tim ;
   static char * redcolor = NULL ;

#define ERREX { free(newseq) ; XBell(dc->display,100) ; return NULL ; }

   newseq = (MCW_imseq *) XtMalloc( sizeof(MCW_imseq) ) ;  /* new structure */

   newseq->dc     = dc ;               /* copy input pointers */
   newseq->getim  = get_image ;
   newseq->getaux = aux ;

   imstatus = (MCW_imseq_status *) get_image(0,isqCR_getstatus,aux) ;
   if( imstatus->num_total < 1 ) ERREX ;
   one_image = (imstatus->num_total == 1) ;

   tim = (MRI_IMAGE *) get_image(0,isqCR_getqimage,aux) ;  /* fake image */

   newseq->horig = tim->nx ;  /* save original dimensions */
   newseq->vorig = tim->ny ;

   newseq->last_width_mm  = IM_WIDTH(tim) ;  /* dimensions in real space */
   newseq->last_height_mm = IM_HEIGHT(tim) ;

   fac = (newseq->last_width_mm  / newseq->horig)    /* width per pixel over */
        /(newseq->last_height_mm / newseq->vorig) ;  /* height per pixel */

   if( fac >= 1.0 ){                                 /* initial display size */
      xwide = newseq->horig * fac + 0.49 ;
      yhigh = newseq->vorig ;
   } else {
      xwide = newseq->horig ;
      yhigh = newseq->vorig / fac + 0.49 ;
   }

#ifdef IMSEQ_DEBUG
printf("new imseq: nx=%d ny=%d dx=%f dy=%f wid=%f hei=%f xwide=%d yhigh=%d\n",
       tim->nx,tim->ny,tim->dx,tim->dy,newseq->last_width_mm,
       newseq->last_height_mm , xwide,yhigh ) ;
fflush(stdout) ;
#endif

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

   strcpy( newseq->im_label , "hi bob" ) ;

   newseq->opt.mirror      = FALSE ;       /* initialize display options */
   newseq->opt.rot         = ISQ_ROT_0 ;
   newseq->opt.no_overlay  = False ;
   newseq->opt.scale_group = ISQ_SCL_AUTO ;
   newseq->opt.scale_range = ISQ_RNG_02TO98 ;
   newseq->opt.free_aspect = False ;
   newseq->opt.save_nsize  = False ;
   newseq->opt.save_pnm    = False ;
   newseq->opt.save_one    = True ;
   newseq->opt.improc_code = ISQ_IMPROC_NONE ;
   newseq->opt.cx_code     = ISQ_CX_MAG ;

   newseq->last_image_type = -1 ;     /* not a legal datum type */

   newseq->opt.parent      = (XtPointer) newseq ;
   newseq->opt.aux         = NULL ;

   newseq->old_opt = newseq->opt ;         /* backup copy */

   newseq->dialog         = NULL ;               /* no dialog at present */
   newseq->num_bbox       = 0 ;
   newseq->dialog_starter = -1 ;

   newseq->imim = newseq->ovim = NULL ;    /* NULL out all images */

   newseq->given_xim = newseq->sized_xim
                     = newseq->given_xbar
                     = newseq->sized_xbar = NULL ;

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

#ifdef IMSEQ_DEBUG
printf("creation: hbase=%d vbase=%d nim=%d lev=%g\n",
          newseq->hbase,newseq->vbase,
          newseq->status->num_total,newseq->lev ) ;
fflush(stdout) ;
#endif

   /***--------- create widgets ---------- ***/

   newseq->image_frac = IMAGE_FRAC ;  /* 25 Oct 1996 */

   xwide = (int) ( 0.49 + newseq->hactual / IMAGE_FRAC ) ;  /* size of wform */
   yhigh = (int) ( 0.49 + newseq->vactual / IMAGE_FRAC ) ;

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
           "dialog" , xmFormWidgetClass , newseq->wtop ,

            XmNwidth  , xwide ,      /* initial size */
            XmNheight , yhigh ,

            XmNborderWidth , 0 ,

            XmNfractionBase , FORM_FRAC_BASE ,

            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
      NULL ) ;

   MCW_register_help( newseq->wform , ISQ_form_help ) ;

   /* drawing area for image space */

   newseq->wimage =
       XtVaCreateManagedWidget(
         "dialog" , xmDrawingAreaWidgetClass , newseq->wform ,

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
                         ,
                         FALSE ,                /* nonmaskable events? */
                         ISQ_drawing_EV ,       /* super-handler! */
                         (XtPointer) newseq ,   /* client data */
                         XtListTail ) ;         /* last in queue */

   strcpy( newseq->im_helptext , ISQ_default_image_help ) ;
   newseq->im_helptext[ISQ_NHELP] = '\0' ;

   MCW_register_help( newseq->wimage , newseq->im_helptext ) ;

   /* all pushbuttons (these are next so they overlay the scale and bar) */

   if( redcolor == NULL ){ HOTCOLOR(newseq->wform,redcolor) ; }

   for( ii=0 ; ii < NBUTTON_BOT ; ii++){

      Arg wa[30] ;
      int na ;

      na = 0 ;

      XtSetArg( wa[na] , XmNmarginWidth   , 2     ) ; na++ ;
      XtSetArg( wa[na] , XmNmarginHeight  , 1     ) ; na++ ;
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
         MCW_set_widget_bg( newseq->wbut_bot[ii] , redcolor , 0 ) ;

      XtAddCallback( newseq->wbut_bot[ii] , XmNactivateCallback ,
                     ISQ_but_bot_def[ii].func_CB , newseq ) ;

      MCW_register_help( newseq->wbut_bot[ii] , ISQ_but_bot_help[ii] ) ;
      MCW_register_hint( newseq->wbut_bot[ii] , ISQ_but_bot_hint[ii] ) ;
   }
   SET_SAVE_LABEL(newseq) ;

   /* buttons on right */

   for( ii=0 ; ii < NBUTTON_RIG ; ii++){

      Arg wa[30] ;
      int na ;

      na = 0 ;

      XtSetArg( wa[na] , XmNmarginWidth   , 1     ) ; na++ ;
      XtSetArg( wa[na] , XmNmarginHeight  , 2     ) ; na++ ;
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

   /* scale for image number */

   ii = (one_image) ? 1 : newseq->status->num_total - 1 ;

   newseq->onoff_widgets[(newseq->onoff_num)++] =
   newseq->wscale =
       XtVaCreateManagedWidget(
          "dialog" , xmScaleWidgetClass , newseq->wform ,

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
   MCW_register_hint( newseq->wscale , "Moves between images" ) ;

   /* arrowpad at lower right corner */

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

   newseq->onoff_widgets[(newseq->onoff_num)++] =
   newseq->wbar =
       XtVaCreateManagedWidget(
          "dialog" , xmDrawingAreaWidgetClass , newseq->wform ,

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
                      "a display control menu"  ) ;

   newseq->wbar_menu = XmCreatePopupMenu( newseq->wbar , "menu" , NULL , 0 ) ;

   newseq->wbar_rng_but =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , newseq->wbar_menu ,
            LABEL_ARG("Choose Display Range") ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtAddCallback( newseq->wbar_rng_but, XmNactivateCallback, ISQ_wbar_menu_CB, newseq ) ;

   newseq->wbar_zer_but =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , newseq->wbar_menu ,
            LABEL_ARG("Choose Zero Color") ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtAddCallback( newseq->wbar_zer_but, XmNactivateCallback, ISQ_wbar_menu_CB, newseq ) ;

   newseq->wbar_flat_but =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , newseq->wbar_menu ,
            LABEL_ARG("Choose Flatten Range") ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtAddCallback( newseq->wbar_flat_but, XmNactivateCallback, ISQ_wbar_menu_CB, newseq ) ;

   newseq->wbar_sharp_but =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , newseq->wbar_menu ,
            LABEL_ARG("Choose Sharpen factor") ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtAddCallback( newseq->wbar_sharp_but, XmNactivateCallback, ISQ_wbar_menu_CB, newseq ) ;

   newseq->rng_bot   = newseq->rng_top = newseq->rng_ztop = 0 ;
   newseq->zer_color = 0 ;
   newseq->flat_bot  = newseq->flat_top = 0.0 ;
   newseq->sharp_fac = 0.60 ;

   /* label for informational display */

   newseq->onoff_widgets[(newseq->onoff_num)++] =
   newseq->winfo = XtVaCreateManagedWidget(
                     "dialog" , xmLabelWidgetClass , newseq->wform ,
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

#ifdef IMSEQ_DEBUG
printf("creation: widgets created\n") ;
fflush(stdout) ;
#endif

   XtManageChild( newseq->wform ) ;

#if 0
   XtRealizeWidget( newseq->wtop ) ;
   newseq->valid = 2 ;  /* mark this structure as ready to roll */

   MCW_alter_widget_cursor( newseq->wtop , -XC_left_ptr ,"yellow","blue" ) ;

#else
   newseq->valid = 1 ;  /* mark this structure as valid */
#endif

   newseq->ignore_redraws = 0 ;

   /* 30 Oct 1996 -- transformations */

   newseq->transform0D_func  = NULL ;  /* no function to start with */
   newseq->transform0D_av    = NULL ;
   newseq->transform0D_index = 0 ;

   newseq->transform2D_func  = NULL ;  /* no function to start with */
   newseq->transform2D_av    = NULL ;
   newseq->transform2D_index = 0 ;

   newseq->parent = NULL ;
   return newseq ;
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

   if( ! ISQ_VALID(seq) ) return ;

DPR("ISQ_reset_dimen") ;

   MCW_widget_geom( seq->wimage , &oldx , &oldy , NULL,NULL ) ;

   scale_x = seq->last_width_mm / oldx ;  /* mm/pixel as displayed now */
   scale_y = seq->last_height_mm/ oldy ;

   if( ! seq->opt.free_aspect ){                      /* fixed aspect */
      scale_x = scale_y = sqrt( scale_x * scale_y ) ; /*  means use   */
   }                                                  /* same scales! */

   xwide = new_width_mm / scale_x + 0.5 ;  /* so scale to new # of pixels */
   yhigh = new_height_mm/ scale_y + 0.5 ;

#ifdef IMSEQ_DEBUG
printf("ISQ_reset_dimen: last wid=%f hei=%f  new wid=%f hei=%f\n",
       seq->last_width_mm,seq->last_height_mm,new_width_mm,new_height_mm ) ;
printf("  new xwide=%d yhigh=%d  scale_x=%f _y=%f\n",
       xwide,yhigh,scale_x,scale_y) ;
fflush(stdout) ;
#endif

   seq->last_width_mm  = new_width_mm ;
   seq->last_height_mm = new_height_mm ;

   if( seq->onoff_state ){
      xwide = (int) ( 0.49 + xwide / seq->image_frac ) ;  /* new size of shell */
      yhigh = (int) ( 0.49 + yhigh / seq->image_frac ) ;
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

   /* if there is a dialog, move it too */

   if( seq->dialog != NULL && XtIsRealized( seq->dialog ) ){

      int dw,dh,dx,dy ;  /* geometry of dialog */

      MCW_widget_geom( seq->dialog , &dw,&dh,&dx,&dy ) ;
      MCW_widget_geom( seq->wtop   , &wx,&hy,&xx,&yy ) ;

      xp = xx+wx+8 ;
      if( xp+dw > seq->dc->width ) xp = seq->dc->width - dw ;
      if( xp    < 0 )              xp = 0 ;

      yp = yy+8 ;
      if( yp+dh > seq->dc->height ) yp = seq->dc->height - dh ;
      if( yp    < 0 )               yp = 0 ;

      XtVaSetValues( seq->dialog , XmNx , xp , XmNy , yp , NULL ) ;
   }

   return ;
}

/*-----------------------------------------------------------------------
   copy an imseq status structure
-------------------------------------------------------------------------*/

MCW_imseq_status * ISQ_copy_status( MCW_imseq_status * instat )
{
   MCW_imseq_status * outstat ;

   outstat = (MCW_imseq_status *) XtMalloc( sizeof(MCW_imseq_status) ) ;

   *outstat = *instat ;   /* shallow copy for now (no pointers) */
   return outstat ;
}

/*-----------------------------------------------------------------------
   Make a color bar "given" XImage
-------------------------------------------------------------------------*/

void ISQ_make_bar( MCW_imseq * seq )
{
   MRI_IMAGE * im ;
   int iy , ny ;
   short * ar ;

   if( ! ISQ_VALID(seq) ) return ;

DPR("ISQ_make_bar");

   KILL_2XIM( seq->given_xbar , seq->sized_xbar ) ;

   ny = seq->dc->ncol_im ;
   im = mri_new( 1 , ny , MRI_short ) ;
   ar = mri_data_pointer( im ) ;

   for( iy=0 ; iy < ny ; iy++ ) ar[iy] = ny-1-iy ;

   seq->given_xbar = mri_to_XImage( seq->dc , im ) ;

   KILL_1MRI( im ) ;
   return ;
}

/*------------------------------------------------------------------------
   make the MRI_IMAGE and the XImage, given the sequence status:
     - if imim is NULL, get it from the user routine and process it,
       if imim is not NULL, leave it alone
     - convert into an XImage
     - here is where changes to the toggled display options are processed
-------------------------------------------------------------------------*/

void ISQ_make_image( MCW_imseq * seq )
{
   MRI_IMAGE * im , * ovim , * tim ;
   Boolean reset_done = False ;

   if( ! ISQ_VALID(seq) ) return ;

   if( seq->mont_nx > 1 || seq->mont_ny > 1 ){
      ISQ_make_montage( seq ) ;
      return ;
   }

DPR("ISQ_make_image");

   KILL_2XIM( seq->given_xim , seq->sized_xim ) ;  /* erase the XImages */

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

      tim = (MRI_IMAGE *) seq->getim( seq->im_nr , isqCR_getimage , seq->getaux ) ;

      if( tim == NULL ){
         fprintf(stderr,
                 "\n*** ISQ_make_image: NULL image returned for display! ***\n") ;
         return ;
      }

      seq->last_image_type = tim->kind ;

      seq->imim = im = ISQ_process_mri( seq->im_nr , seq , tim ) ;
      KILL_1MRI(tim) ;

      /* fix window dimensions if image size is different from before */

      new_width_mm  = IM_WIDTH(im) ;
      new_height_mm = IM_HEIGHT(im) ;

      seq->horig = im->nx ;
      seq->vorig = im->ny ;

      if( FLDIF(new_width_mm ,seq->last_width_mm ) ||
          FLDIF(new_height_mm,seq->last_height_mm)   ){

#ifdef IMSEQ_DEBUG
printf("\nnew image: nx=%d ny=%d dx=%f dy=%f wid=%f hei=%f\n",
       im->nx,im->ny,im->dx,im->dy,new_width_mm,new_height_mm) ;
fflush(stdout) ;
#endif

         ISQ_reset_dimen( seq , new_width_mm , new_height_mm ) ;
         reset_done = True ;
      }
   }

   if( seq->opt.free_aspect != seq->old_opt.free_aspect && !reset_done )
      ISQ_reset_dimen( seq , seq->last_width_mm , seq->last_height_mm ) ;

   /*--- set the overlay to process ---*/

   if( seq->opt.no_overlay ){
      KILL_1MRI( seq->ovim ) ;
      ovim = NULL ;
   } else {
      ovim = seq->ovim ;
      if( ovim == NULL ){
         tim = (MRI_IMAGE *) seq->getim( seq->im_nr ,
                                         isqCR_getoverlay , seq->getaux ) ;

         if( tim != NULL && tim->kind != MRI_short ){
            fprintf(stderr,"\a\n*** Illegal non-short overlay image! ***\n") ;
            KILL_1MRI(tim) ;
         }

         if( tim != NULL )
            ovim = seq->ovim =
               mri_flippo( ISQ_TO_MRI_ROT(seq->opt.rot) , seq->opt.mirror , tim ) ;

         if( tim != ovim ) KILL_1MRI(tim) ;
      }
   }

   /* set old_opt to current options */

   seq->old_opt = seq->opt ;

   seq->mont_nx_old = seq->mont_ny_old = 1 ;

DPR("  ISQ_make_image -- making given_xim");

   /* overlay, if needed */

   if( ovim == NULL || seq->opt.no_overlay ){
      tim = im ;
   } else {
      register short * tar , * oar , * iar ;
      register int ii , npix = im->nx * im->ny ;

DPR("  ISQ_make_image -- overlaying onto 'im'") ;
      tim = mri_new( im->nx , im->ny , MRI_short ) ;
      tar = mri_data_pointer( tim ) ;
      oar = mri_data_pointer( ovim ) ;
      iar = mri_data_pointer( im ) ;
      for( ii=0 ; ii < npix ; ii++ )
         tar[ii] = (oar[ii] == 0) ? iar[ii] : -oar[ii] ;
   }

   /* convert result to XImage for display */

DPR("  ISQ_make_image -- converting to XImage") ;
   seq->given_xim = mri_to_XImage( seq->dc , tim ) ;

   if( tim != im ) KILL_1MRI(tim) ;

DPR("  ISQ_make_image -- exit") ;
   return ;
}

/*-----------------------------------------------------------------------
   process an MRI_IMAGE from the user into a scaled format for display
-------------------------------------------------------------------------*/

MRI_IMAGE * ISQ_process_mri( int nn , MCW_imseq * seq , MRI_IMAGE * im )
{
   MRI_IMAGE * newim , * flipim , * lim ;
   int  scl_grp ;
   short clbot=0 , cltop=0 ;
   int must_rescale = 0 ;
   int have_transform ;

   if( ! ISQ_VALID(seq) || im == NULL ) return NULL ;

DPRI("ISQ_process_mri",nn) ;

   /*** Feb 7, 1996: deal with complex-valued images ***/

   lim = im ;  /* local image = input image, unless complex */

   if( im->kind == MRI_complex ){
      float * lar ; complex * cxar ; int ii , npix ;

DPRI("  -- complex to real code = ",seq->opt.cx_code) ;

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

   /****** process image in normal fashion if no IMPROC code given ******/

   have_transform = (seq->transform0D_func != NULL ||
                     seq->transform2D_func != NULL   ) ;

   if( ! have_transform && seq->opt.improc_code == ISQ_IMPROC_NONE ){

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
            clbot = seq->rng_bot ;
            cltop = seq->rng_top ;
         }
         break ; /* end of user input range scaling */

         default:               /* scale on individual image statistics */
         case ISQ_SCL_AUTO:{
            ISQ_indiv_statistics * st = &( seq->imstat[nn] ) ;

            if( must_rescale ) st->one_done = False ;

            if( ! st->one_done ) ISQ_statify_one( seq , nn , lim ) ;

            switch( seq->opt.scale_range ){

               default:
               case ISQ_RNG_MINTOMAX:
                  seq->scl = st->scl_mm ;
                  seq->lev = st->lev_mm ;
               break ;

               case ISQ_RNG_02TO98:
                  seq->scl = st->scl_per ;
                  seq->lev = st->lev_per ;

                  clbot = st->per02 ;
                  cltop = st->per98 ;
               break ;
            }
         }
         break ;  /* end of autoscaling */

         case ISQ_SCL_GRP:{         /* scale on group statistics */
            ISQ_glob_statistics * gl = seq->glstat ;

            switch( seq->opt.scale_range ){

               default:
               case ISQ_RNG_MINTOMAX:
                  if( ! gl->mm_done ) ISQ_statify_all( seq , True ) ;
                  seq->scl = gl->scl_mm ;
                  seq->lev = gl->lev_mm ;
               break ;

               case ISQ_RNG_02TO98:
                  if( ! gl->per_done ) ISQ_statify_all( seq , False ) ;
                  seq->scl = gl->scl_per ;
                  seq->lev = gl->lev_per ;

                  clbot = gl->per02 ;
                  cltop = gl->per98 ;
               break ;
            }
         }
         break ;  /* end of groupscaling */
      }  /* end of scaling */

      /* 11/30/94 fix: mri_to_short_sclip has problems with short overflow */

      if( lim->kind == MRI_short && clbot < cltop ){

         int npix = lim->nx * lim->ny , ii ;
         short * ar = lim->im.short_data ;

DPRI("  -- clipping # pixels = ",npix) ;

         if( seq->rng_ztop == 0 ){
            for( ii=0 ; ii < npix ; ii++ )
                    if( ar[ii] < clbot ) ar[ii] = clbot ;
               else if( ar[ii] > cltop ) ar[ii] = cltop ;
         } else {
            for( ii=0 ; ii < npix ; ii++ )
                    if( ar[ii] < clbot || ar[ii] > cltop ) ar[ii] = clbot ;
         }
      }

      /*----- next, scale image as defined above -----*/

DPR("  -- scaling to shorts") ;

                               /* scaling   to zero   clip bot  clip top */
                               /* --------  --------  --------  -------- */
      newim = mri_to_short_sclip( seq->scl, seq->lev, seq->bot, seq->top, lim );

   /****** end of normal processing; handle special image processing below ******/

   } else {
      MRI_IMAGE * tim , * qim ;
      double scl , lev ;
      float hbot,htop ;

DPR("  -- begin IMPROCessing") ;

      qim = lim ;  /* at the start of each process stage,
                      qim is the image to process;
                      tim is an intermediate temporary image */

      /***** 30 Oct 1996: transform image *****/

      if( seq->transform0D_func != NULL ){
         tim = mri_to_float(qim) ;
         seq->transform0D_func( tim->nvox , MRI_FLOAT_PTR(tim) ) ;
         if( qim != lim ) mri_free(qim) ;
         qim = tim ;
      }

      if( seq->transform2D_func != NULL ){
         tim = mri_to_float(qim) ;
         seq->transform2D_func( tim->nx , tim->ny ,
                                tim->dx , tim->dy , MRI_FLOAT_PTR(tim) ) ;
         if( qim != lim ) mri_free(qim) ;
         qim = tim ;
      }

      /*** flatten ***/

      if( (seq->opt.improc_code & ISQ_IMPROC_FLAT) != 0 ){
DPR("  -- mri_flatten:") ;
         tim = mri_flatten( qim ) ;
         if( qim != lim ) mri_free(qim) ;
         qim = tim ;

         if( seq->opt.scale_range == ISQ_RNG_02TO98 &&
             seq->flat_top > seq->flat_bot ){

            float * qar = MRI_FLOAT_PTR(qim) ;
            int ii , npix = qim->nx * qim->ny ;

DPR("  -- clip flattened image") ;

            for( ii=0 ; ii < npix ; ii++ ){
                    if( qar[ii] < seq->flat_bot ) qar[ii] = seq->flat_bot ;
               else if( qar[ii] > seq->flat_top ) qar[ii] = seq->flat_top ;
            }
         }
      }

      /*** sharpen ***/

      if( (seq->opt.improc_code & ISQ_IMPROC_SHARP) != 0 ){
DPR("  -- mri_sharpen:") ;
         tim = mri_sharpen( seq->sharp_fac , 0 , qim ) ;
         if( qim != lim ) mri_free(qim) ;
         qim = tim ;
      }

      /*** Sobel edge detection ***/

      if( (seq->opt.improc_code & ISQ_IMPROC_SOBEL) != 0 ){
         int ii , npix ;
         float * tar ;

DPR("  -- mri_edit_image:") ;
         tim = mri_edit_image( 0.10 , 1.0 , qim ) ;   /* soft clip small values */
         if( qim != lim ) mri_free(qim) ;
         qim = tim ;

DPR("  -- mri_sobel:") ;
         tim  = mri_sobel( 0 , 2 , qim ) ;            /* edge detect */

         npix = tim->nx * tim->ny ;                   /* take square root */
         tar  = mri_data_pointer(tim) ;
         for( ii=0 ; ii < npix ; ii++ ) tar[ii] = sqrt(tar[ii]) ;

         if( qim != lim ) mri_free(qim) ;
         qim = tim ;
      }

      /*** scale to shorts (cf. ISQ_statify_one) ***/

      hbot = mri_min(qim) ; htop = mri_max(qim) ;

DPR("  -- scale to shorts") ;
      switch( seq->opt.scale_range ){
         default:
         case ISQ_RNG_MINTOMAX:
            ISQ_SCLEV( hbot,htop , seq->dc->ncol_im , scl,lev ) ;
         break ;

         case ISQ_RNG_02TO98:{
            static int hist[NHISTOG] ;
            float h02 , h98 ;

DPR("  -- mri_histogram:") ;
            mri_histogram( qim , hbot,htop , True , NHISTOG,hist ) ;
DPR("  -- ISQ_perpoints:") ;
            ISQ_perpoints( hbot,htop , hist , &h02 , &h98 ) ;
            ISQ_SCLEV( h02,h98 , seq->dc->ncol_im , scl,lev ) ;
         }
         break ;
      }

      newim = mri_to_short_sclip( scl , lev , seq->bot, seq->top, qim ) ;
      if( qim != lim ) mri_free(qim) ;
   }

   /** Aug 31, 1995: put zer_color in at bottom, if nonzero **/

   if( seq->zer_color > 0 ){
      short zz = -seq->zer_color ;
      short * ar = MRI_SHORT_PTR(newim) ;
      int npix = newim->nx * newim->ny , ii ;

DPRI("  -- loading zero color index = ",zz) ;
      for( ii=0 ; ii < npix ; ii++ )
         if( ar[ii] == seq->bot ) ar[ii] = zz ;
   }

   /* copy sizes (fixup for mrilib) */

   MRI_COPY_AUX( newim , lim ) ;

   /*----- last, rotate/flip image to desired orientation -----*/

DPR("  -- mri_flippo:") ;
   flipim = mri_flippo( ISQ_TO_MRI_ROT(seq->opt.rot) , seq->opt.mirror , newim ) ;

   if( newim != flipim ) KILL_1MRI(newim) ;  /* discard the trash */
   if( lim   != im     ) KILL_1MRI(lim) ;    /* (if there is any) */

   return flipim ;
}

/*-------------------------------------------------------------------
  Callback handlers for color palette manipulation
---------------------------------------------------------------------*/

void ISQ_but_color_CB( Widget w , XtPointer client_data ,
                                  XtPointer call_data    )
{
   MCW_imseq * seq = (MCW_imseq *) client_data ;

   if( ! ISQ_REALZ(seq) ) return ;

   if( seq->dc->use_xcol_im ) DC_palette_setgray( seq->dc ) ;
   else                       DC_palette_setcolor( seq->dc ) ;

   ISQ_but_done_reset( seq ) ;
   return ;
}

void ISQ_but_cswap_CB( Widget w , XtPointer client_data ,
                                  XtPointer call_data    )
{
   MCW_imseq * seq = (MCW_imseq *) client_data ;

   if( ! ISQ_REALZ(seq) ) return ;

   DC_palette_swap( seq->dc ) ;

   ISQ_but_done_reset( seq ) ;
   return ;
}

/*-------------------------------------------------------------------
   image saving options
---------------------------------------------------------------------*/

void ISQ_saver_CB( Widget w , XtPointer cd , MCW_choose_cbs * cbs )
{
   MCW_imseq * seq = (MCW_imseq *) cd ;
   int ii , kf , nppm=0 , npgm=0 ;
   MRI_IMAGE * tim , * flim ;
   char fname[256] ;

#ifndef DONT_USE_METER
#  define METER_MINCOUNT 20
   Widget meter = NULL ;
   int meter_perc , meter_pold , meter_pbase ;
#endif

   if( seq->saver_prefix == NULL ){  /* just got a string */
      int ll , ii ;

      if( cbs->reason != mcwCR_string ||
          cbs->cval == NULL           || (ll = strlen(cbs->cval)) == 0 ){

         XBell( XtDisplay(w) , 100 ) ; return ;
      }

      seq->saver_prefix = XtMalloc( sizeof(char) * (ll+8) ) ;
      strcpy( seq->saver_prefix , cbs->cval ) ;

      if( seq->saver_prefix[ll-1] != '.' ){  /* add a . at the end? */
         seq->saver_prefix[ll++] = '.' ;
         seq->saver_prefix[ll]   = '\0' ;
      }

      /*-- check that the prefix is acceptable --*/

      ll = strlen(seq->saver_prefix) ;

      for( ii=0 ; ii < ll ; ii++ )
         if( iscntrl(seq->saver_prefix[ii]) ||
             isspace(seq->saver_prefix[ii])   ) break ;

      if( ii < ll || ll < 2 || ll > 240 ){
         XBell( XtDisplay(w) , 100 ) ;
         myXtFree( seq->saver_prefix ) ; seq->saver_prefix = NULL ;
         return ;
      }

      /*-- April 1996: Save:one case here --*/

      if( seq->opt.save_one ){
         char * ppnm = strstr( seq->saver_prefix , ".pnm." ) ;
         int    sll  = strlen( seq->saver_prefix ) ;

         if( ppnm == seq->saver_prefix + (sll-5) )  /* 17 June 1997 */
            seq->saver_prefix[sll-1] = '\0' ;
         else
            strcat(seq->saver_prefix,"pnm") ;

         tim = XImage_to_mri( seq->dc , seq->given_xim ) ;
         if( tim != NULL ){
            printf("Writing one PNM image to file %s\n",seq->saver_prefix) ;
            mri_write_pnm( seq->saver_prefix , tim ) ;
            mri_free( tim ) ; tim = NULL ;  /* 17 June 1997 */
         } else {
            XBell( XtDisplay(w) , 100 ) ;
         }
         myXtFree( seq->saver_prefix ) ; seq->saver_prefix = NULL ;
         POPDOWN_string_chooser ;
         return ;
      }

      /*-- move on to the From value --*/

      POPDOWN_string_chooser ;

      MCW_choose_integer( w , "Slice from" ,
                          0 , seq->status->num_total-1 , 0 ,
                          ISQ_saver_CB , (XtPointer) seq ) ;

      seq->saver_from = -1 ;
      return ;
   }

   /*--- got from ---*/

   if( seq->saver_from == -1 ){  /* just got an integer */

      if( cbs->reason != mcwCR_integer ){  /* error */
         XBell( XtDisplay(w) , 100 ) ;
         myXtFree( seq->saver_prefix ) ; seq->saver_prefix = NULL ;
         return ;
      }

      seq->saver_from = cbs->ival ;

      POPDOWN_integer_chooser ;

      MCW_choose_integer(
          w , "Slice to" ,
          0 , seq->status->num_total-1 , seq->status->num_total-1 ,
          ISQ_saver_CB , (XtPointer) seq ) ;

      seq->saver_to = -1 ;
      return ;
   }

   /*--- go to: last call ---*/

   if( cbs->reason != mcwCR_integer ){  /* error */
      XBell( XtDisplay(w) , 100 ) ;
      myXtFree( seq->saver_prefix ) ; seq->saver_prefix = NULL ;
      return ;
   }

   POPDOWN_integer_chooser ;

   seq->saver_to = cbs->ival ;

   if( seq->saver_prefix == NULL ||
       seq->saver_from < 0       ||
       seq->saver_to   < 0       ||
       seq->saver_from > seq->status->num_total-1 ||
       seq->saver_to   > seq->status->num_total-1   ){  /* error */

      XBell( XtDisplay(w) , 100 ) ;
      myXtFree( seq->saver_prefix ) ; seq->saver_prefix = NULL ;
      return ;
   }

   if( seq->saver_from > seq->saver_to ){
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


   /*---- loop thru, get images, save them ----*/

   for( kf=seq->saver_from ; kf <= seq->saver_to ; kf++ ){

      tim = (MRI_IMAGE *) seq->getim( kf , isqCR_getimage , seq->getaux ) ;
      if( tim == NULL ){
         fprintf(stderr,"\n*** ISQ_saver_CB: NULL image %d returned! ***\n",kf) ;
         continue ;  /* skip to next one */
      }
      flim = tim ;

#ifndef DONT_USE_METER
      if( meter != NULL ){
         meter_perc = (int)(100.9 * (kf - seq->saver_from) / meter_pbase) ;
         if( meter_perc != meter_pold ){
            MCW_set_meter( meter , meter_perc ) ;
            meter_pold = meter_perc ;
         }
      }
#endif

      if( ! seq->opt.save_pnm ){ /** write background only **/

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

         sprintf( fname , "%s%04d" , seq->saver_prefix , kf ) ;
         mri_write( fname , flim ) ; mri_free( flim ) ;

      } else { /** write color overlay and everything **/

         MRI_IMAGE * ovim=NULL ;
         int ii , nx , ny , npix , bb , allgray , ncode,nout ;
         byte * rgb ;   /* "byte" is defined in mrilib.h */
         short * flar ;
         XColor * ulc , * ovc , * xc ;
         FILE * fd ;
         byte rrr,ggg,bbb ;

         /* process given image to make the grayscale index */

         tim  = flim ;
         flim = ISQ_process_mri( kf , seq , tim ) ;  /* will be shorts now */
         if( tim != flim ) KILL_1MRI( tim ) ;

         flar = mri_data_pointer(flim) ;  /* underlay image data */
         nx = flim->nx ;
         ny = flim->ny ;
         npix = flim->nx * flim->ny ;

         /* get overlay and flip it */

         if( !seq->opt.no_overlay ){
            tim = (MRI_IMAGE *) seq->getim( kf,isqCR_getoverlay,seq->getaux) ;
            if( tim != NULL && tim->kind != MRI_short ){
               KILL_1MRI(tim) ;
            }
            if( tim != NULL )
               ovim = mri_flippo( ISQ_TO_MRI_ROT(seq->opt.rot) , seq->opt.mirror , tim ) ;
            if( tim != ovim ) KILL_1MRI(tim) ;
         }

         /* perform overlay onto flim */

         if( ovim != NULL ){
            short * ovar ; int jj ;
            ovar = mri_data_pointer(ovim) ;
            for( jj=0 ; jj < npix ; jj++ )
               if( ovar[jj] != 0 ) flar[jj] = -ovar[jj] ;
            mri_free( ovim ) ;
         }

         /* XColor arrays for underlay and overlay */

         ulc = ( seq->dc->use_xcol_im ) ? seq->dc->xcol_im
                                        : seq->dc->xgry_im ;
         ovc = seq->dc->xcol_ov ;

         /* write the output file */

         if( kf == seq->saver_from )
            printf("writing %d x %d PNM files",nx,ny) ;
         else if( kf%10 == 5 )
            printf("." ) ;
         fflush(stdout) ;

         sprintf( fname , "%s%04d.pnm" , seq->saver_prefix , kf ) ;

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
            npgm++ ;
         } else {
            ncode = 6 ;     /* PPM */
            nout  = 3*npix ;
            nppm++ ;
         }

         fprintf(fd,"P%d\n%d %d\n255\n",ncode,nx,ny) ; /* write PNM header */
         fwrite( rgb , sizeof(byte) , nout , fd ) ;    /* write bytes */
         fclose( fd ) ; mri_free( flim ) ; myXtFree(rgb) ; /* DONE */
      }
   }

   if( nppm+npgm > 0 ) printf(" #PPM=%d #PGM=%d\n",nppm,npgm) ;
   else                printf(". done\n") ;

   /*--- go home ---*/

#ifndef DONT_USE_METER
   if( meter != NULL ) MCW_popdown_meter(meter) ;
#endif

   myXtFree( seq->saver_prefix ) ; seq->saver_prefix = NULL ;
   return ;
}

/*----------------------------------------------------------------------*/

void ISQ_but_save_CB( Widget w , XtPointer client_data ,
                                 XtPointer call_data    )
{
   MCW_imseq * seq = (MCW_imseq *) client_data ;

   if( ! ISQ_REALZ(seq) || w == NULL || ! XtIsWidget(w) ) return ;

   seq->saver_prefix = NULL ;
   seq->saver_from = seq->saver_to = -1 ;

   MCW_choose_string( w , "Filename prefix:" , NULL ,
                      ISQ_saver_CB , (XtPointer) seq ) ;

   ISQ_but_done_reset( seq ) ;
   return ;
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
   MCW_imseq * seq = (MCW_imseq *) client_data ;

   if( ! ISQ_VALID(seq) ) return ;

DPR("ISQ_but_done_CB");

#ifdef REQUIRE_TWO_DONES
   /*-- first call from "Done" button --> change label, return */

   if( w == seq->wbut_bot[NBUT_DONE] && seq->done_first ){
      MCW_set_widget_label( w , ISQ_but_done_label2 ) ;
      seq->done_first = False ;
      return ;
   }
#endif

   /*-- second call: kill --*/

   if( seq->glstat->worker != 0 ){  /* remove work process, if started */
      XtRemoveWorkProc( seq->glstat->worker ) ;
      seq->glstat->worker = 0 ;
   }

   ISQ_free_alldata( seq ) ;
   XtDestroyWidget( seq->wtop ) ;
   seq->valid = 0 ;     /* WE do not deallocate the data structure! */

#ifdef IMSEQ_DEBUG
printf("IMSEQ: data destroyed!") ;
fflush(stdout) ;
#endif

   if( seq->status->send_CB != NULL ){
      ISQ_cbs cbs ;

#ifdef IMSEQ_DEBUG
printf("IMSEQ: sending destroy message\n") ;
fflush(stdout) ;
#endif

      cbs.reason = isqCR_destroy ;
      seq->status->send_CB( seq , seq->getaux , &cbs ) ;
   }

   return ;
}

/*-----------------------------------------------------------------------
   delete malloc-ed data in an imseq
-------------------------------------------------------------------------*/

void ISQ_free_alldata( MCW_imseq * seq )
{
   int ib ;

   if( ! ISQ_VALID(seq) ) return ;

DPR("ISQ_free_alldata");

   KILL_1MRI( seq->imim ) ;
   KILL_1MRI( seq->ovim ) ;

   KILL_2XIM( seq->given_xim  , seq->sized_xim  ) ;
   KILL_2XIM( seq->given_xbar , seq->sized_xbar ) ;

   myXtFree( seq->imstat ) ; seq->imstat = NULL ;
   myXtFree( seq->glstat ) ; seq->glstat = NULL ;

   for( ib=0 ; ib < seq->num_bbox ; ib++ )
      myXtFree( seq->bbox[ib] ) ;
   seq->num_bbox = 0 ;

   for( ib=0 ; ib < NARROW ; ib++ ) myXtFree( seq->arrow[ib] ) ;

   myXtFree( seq->arrowpad ) ;

   myXtFree( seq->mont_across_av )   ;
   myXtFree( seq->mont_down_av )     ;
   myXtFree( seq->mont_skip_av )     ;
   myXtFree( seq->mont_gap_av )      ;
   myXtFree( seq->mont_gapcolor_av ) ;
   myXtFree( seq->transform0D_av )   ; /* 30 Oct 1996 */
   myXtFree( seq->transform2D_av )   ;

   return ;
}

/*----------------------------------------------------------------------
  callback when the scale is moved
------------------------------------------------------------------------*/

void ISQ_scale_CB( Widget w , XtPointer client_data , XtPointer call_data )
{
   MCW_imseq * seq             = (MCW_imseq *)             client_data ;
   XmScaleCallbackStruct * cbs = (XmScaleCallbackStruct *) call_data ;

   if( ! ISQ_REALZ(seq) ) return ;

   ISQ_redisplay( seq , cbs->value , isqDR_display ) ;

   ISQ_but_done_reset( seq ) ;
   return ;
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

void ISQ_redisplay( MCW_imseq * seq , int n , int type )
{
   Boolean kill_im , kill_ov ;
   int nrold ;
   static int         recur_flg = FALSE ;
   static int         recur_n   = -1 ;
   static MCW_imseq * recur_seq = NULL ;

   if( ! ISQ_VALID(seq) ) return ;

DPR("ISQ_redisplay entry") ;

   /** check for identical recursive call **/

   if( RECUR ){
#ifdef IMSEQ_DEBUG
DPRI("ISQ_redisplay ABORTED FOR RECURSION at n =",n) ;
#endif
      recur_flg = FALSE ; return ;
   }

   /** If no recursion is now occurring, mark for possible recursion later.
       This assumes that each level of recursion does not spawn new levels
       yet again via the send_CB callback.  If this were possible, the
       code for recursion prevention would need to be more complicated! **/

   if( ! recur_flg ){ recur_flg = TRUE ; recur_n = n ; recur_seq = seq ; }

   /** find the image that is being seen right now **/

   nrold = seq->im_nr ;

#ifdef IMSEQ_DEBUG
printf("imseq: ISQ_redisplay n=%d type=%d nrold=%d recur_flg=%d\n",n,type,nrold,recur_flg);
#endif

   /** set the image number to be displayed now **/

   if( n >= 0 && !ISQ_set_image_number(seq,n) ){
      if( RECUR ) recur_flg = FALSE ; return ;
   }

   switch( type ){
      default: { if( RECUR ) recur_flg = FALSE ; return ; }

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

   if( kill_ov || kill_im ) KILL_2XIM( seq->given_xim  , seq->sized_xim  ) ;

   ISQ_show_image( seq ) ;

   if( RECUR ) recur_flg = FALSE ;

DPR("ISQ_redisplay: exit") ;
   return ;
}

/*------------------------------------------------------------------------
   set image number in an imseq
--------------------------------------------------------------------------*/

int ISQ_set_image_number( MCW_imseq * seq , int n )
{
   if( ! ISQ_VALID(seq) ) return 0 ;

#ifdef IMSEQ_DEBUG
printf("imseq: ISQ_set_image_number %d\n",n);
fflush(stdout) ;
#endif

   if( n < 0 || n >= seq->status->num_total ){
      XBell( seq->dc->display , 100 ) ;
      printf("\n*** ILLEGAL IMAGING:\n"
             " ISQ_set_image_number %d\n",n);

      printf(" status: num_total=%d num_series=%d\n",
              seq->status->num_total , seq->status->num_series ) ;

      return 0 ;
   }

   if( seq->im_nr != n ){
      seq->im_nr = n ;
      XmScaleSetValue( seq->wscale , n ) ;  /* be sure to change scale */

      if( seq->status->send_CB != NULL ){
         ISQ_cbs cbs ;

         cbs.reason = isqCR_newimage ;
         cbs.nim    = seq->im_nr ;
         seq->status->send_CB( seq , seq->getaux , &cbs ) ;
      }
   }
   return 1 ;
}

/*-----------------------------------------------------------------------
  actually put the image into window
-------------------------------------------------------------------------*/

void ISQ_show_image( MCW_imseq * seq )
{
   if( ! ISQ_REALZ(seq) ) return ;

DPR("ISQ_show_image");

   if( seq->given_xim == NULL ) ISQ_make_image( seq ) ;

   if( seq->given_xim == NULL ){
      fprintf(stderr,"\n***seq->given_xim == NULL -- cannot display image\n") ;
      return ;
   }

   if( seq->sized_xim == NULL ){
      int nx , ny ;

DPR("  -- making sized_xim");

      MCW_widget_geom( seq->wimage , &nx , &ny , NULL,NULL ) ;

#ifdef IMSEQ_DEBUG
printf("imseq:   -- dimensions %d %d\n",nx,ny);
fflush(stdout) ;
#endif

      seq->sized_xim = resize_XImage( seq->dc , seq->given_xim , nx , ny ) ;
   }

DPR("  -- putting sized_xim to screen");

   XPutImage( seq->dc->display , XtWindow(seq->wimage) , seq->dc->origGC ,
              seq->sized_xim , 0,0,0,0,
              seq->sized_xim->width , seq->sized_xim->height ) ;

   ISQ_draw_winfo( seq ) ;
   return ;
}

/*-------------------------------------------------------------------
  Draw the message data in the winfo label
---------------------------------------------------------------------*/

void ISQ_draw_winfo( MCW_imseq * seq )
{
   char buf[64] = "\0" ;
   int nn , ibuf ;
   ISQ_indiv_statistics * st ;

   if( ! ISQ_REALZ(seq) ) return ;

   if( seq->last_image_type >= 0 ){
      sprintf( buf , "%s" , MRI_TYPE_name[seq->last_image_type] ) ;

      if( seq->last_image_type == MRI_complex ){
         switch( seq->opt.cx_code ){
            case ISQ_CX_MAG:   strcat( buf , "[mag]"  ) ; break ;
            case ISQ_CX_PHASE: strcat( buf , "[arg]"  ) ; break ;
            case ISQ_CX_REAL:  strcat( buf , "[real]" ) ; break ;
            case ISQ_CX_IMAG:  strcat( buf , "[imag]" ) ; break ;
         }
      }
   }
   ibuf = strlen(buf) ;

   nn = seq->im_nr ;  if( nn < 0 ) return ;
   st = &( seq->imstat[nn] ) ;
   if( st->one_done ){
#if 0
      if( seq->opt.scale_group == ISQ_SCL_AUTO   &&
          seq->opt.scale_range == ISQ_RNG_02TO98    )

           sprintf( buf+ibuf , " 2%%=%g 98%%=%g", st->per02 , st->per98 ) ;
      else
#endif
           sprintf( buf+ibuf , " min=%g max=%g" , st->min   , st->max   ) ;
   }

   if( strcmp(buf,seq->im_label) != 0 ){
      MCW_set_widget_label( seq->winfo , buf ) ;
      strcpy(seq->im_label,buf) ;
   }

   return ;
}

/*-------------------------------------------------------------------
  actually put the color bar into its window
---------------------------------------------------------------------*/

void ISQ_show_bar( MCW_imseq * seq )
{

   if( ! ISQ_REALZ(seq) ) return ;

DPR("ISQ_show_bar");

   if( seq->given_xbar == NULL ) ISQ_make_bar( seq ) ;

   if( seq->sized_xbar == NULL ){
      int nx , ny ;

DPR("  -- making sized_xbar");

      MCW_widget_geom( seq->wbar , &nx , &ny , NULL,NULL ) ;

#ifdef IMSEQ_DEBUG
printf("imseq:   -- dimensions %d %d\n",nx,ny);
fflush(stdout) ;
#endif

      seq->sized_xbar = resize_XImage( seq->dc, seq->given_xbar, nx, ny ) ;
   }

DPR("  -- putting sized_xbar to screen");

   XPutImage( seq->dc->display , XtWindow(seq->wbar) , seq->dc->origGC ,
              seq->sized_xbar , 0,0,0,0,
              seq->sized_xbar->width , seq->sized_xbar->height ) ;

   return ;
}

/*-----------------------------------------------------------------------
   Handle all events in an imseq drawing area widget (image or bar)
-------------------------------------------------------------------------*/

void ISQ_drawing_EV( Widget w , XtPointer client_data ,
                     XEvent * ev , Boolean * continue_to_dispatch )
{
   MCW_imseq * seq = (MCW_imseq *) client_data ;
   static ISQ_cbs cbs ;

   if( ! ISQ_REALZ(seq) ) return ;

#ifdef IMSEQ_DEBUG
printf("imseq: ISQ_drawing_EV %s %d\n",XtName(w),ev->type);
fflush(stdout) ;
#endif

   switch( ev->type ){

      /*----- redraw -----*/

      case Expose:{
         XExposeEvent * event = (XExposeEvent *) ev ;

DPRI(" .. Expose; count=",event->count) ;

         XSync( XtDisplay(w) , False ) ;
#if 0
         MCW_discard_events( w , ExposureMask );
#endif
         if( event->count == 0 ){
                 if( w == seq->wimage ) ISQ_show_image( seq ) ;
            else if( w == seq->wbar   ) ISQ_show_bar( seq ) ;
         }
      }
      break ;

      /*----- take key press -----*/

      case KeyPress:{
         XKeyEvent * event = (XKeyEvent *) ev ;
         char           buf[32] ;
         KeySym         ks ;
         XComposeStatus status ;

DPR(" .. KeyPress") ;

         buf[0] = '\0' ;
         XLookupString( event , buf , 32 , &ks , &status ) ;

         if( w == seq->wimage && buf[0] != '\0' &&
             seq->status->send_CB != NULL         ){

            cbs.reason = isqCR_keypress ;
            cbs.event  = ev ;
            cbs.key    = buf[0] ;
            cbs.nim    = seq->im_nr ;

            seq->status->send_CB( seq , seq->getaux , &cbs ) ;
         }
      }
      ISQ_but_done_reset( seq ) ;
      break ;

      /*----- take button press -----*/

      case ButtonPress:{
         XButtonEvent * event = (XButtonEvent *) ev ;
         int bx,by , width,height , but ;

DPR(" .. ButtonPress") ;

         bx  = event->x ;
         by  = event->y ;
         but = event->button ;
         MCW_widget_geom( w , &width , &height , NULL,NULL ) ;

         MCW_discard_events( w , ButtonPressMask ) ;

         switch( but ){

            case Button2:
            case Button3:
            case Button1:{
               int imx,imy,nim;

               if( w == seq->wimage && but == Button3 &&
                   (event->state & (ShiftMask|ControlMask|Mod1Mask)) ){

                  /* 23 Oct 1996: Simulation of bottom buttons */

                  if( (event->state & ShiftMask) )
                     ISQ_but_disp_CB( seq->wbut_bot[NBUT_DISP] , seq , NULL ) ;

                  else if( seq->status->num_total > 1 && (event->state & ControlMask) )
                     ISQ_montage_CB( seq->wbut_bot[NBUT_MONT] , seq , NULL ) ;

                  else if( seq->status->num_total > 1 && (event->state & Mod1Mask) )
                     ISQ_but_save_CB( seq->wbut_bot[NBUT_SAVE] , seq , NULL ) ;

                  else
                     XBell( seq->dc->display , 100 ) ;

               } else if( w == seq->wimage && seq->status->send_CB != NULL ){

                  ISQ_mapxy( seq , bx,by , &imx,&imy,&nim ) ;
                  cbs.reason = isqCR_buttonpress ;
                  cbs.event  = ev ;
                  cbs.xim    = imx ;
                  cbs.yim    = imy ;
                  cbs.nim    = nim ;

                  seq->status->send_CB( seq , seq->getaux , &cbs ) ;
               }
               else if( w == seq->wbar && but == Button3 ){
                  XmMenuPosition( seq->wbar_menu , event ) ; /* where */
                  XtManageChild ( seq->wbar_menu ) ;         /* popup */
               }
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

#ifdef IMSEQ_DEBUG
printf("imseq:  .. ConfigureNotify: width=%d height=%d\n",
       event->width,event->height);
fflush(stdout) ;
#endif

         /* simply delete the XImage sized to the window;
            redisplay will then automatically size it when called */

#if 0
         XSync( XtDisplay(w) , False ) ;
         MCW_discard_events( w , ExposureMask ) ;
#endif

         if( w == seq->wimage ){

            if( (seq->sized_xim == NULL)                  ||
                (event->width  != seq->sized_xim->width ) ||
                (event->height != seq->sized_xim->height)   ){

               KILL_2ndXIM( seq->given_xim , seq->sized_xim ) ;
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
      }
      break ;

      /*----- ignore all other events -----*/

      default: break ;

   } /* end of switch ev->type */

   return ;
}

/*---------------------------------------------------------------------
   process Disp button press for an imseq:
     change the way the image is displayed (flip, rotate, ...),
     by popping up a dialog
-----------------------------------------------------------------------*/

void ISQ_but_disp_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   MCW_imseq * seq = (MCW_imseq *) client_data ;
   int ib , wx,hy,xx,yy ;
   Widget rctop , rcboxes ;

   if( ! ISQ_REALZ(seq) || seq->dialog != NULL ) return ;

DPR("ISQ_but_disp_CB");

   for( ib=0 ; ib < NBUTTON_BOT-1 ; ib++ )        /* turn off buttons  */
      if( ISQ_but_bot_dial[ib] == True )          /* that also want to */
        SENSITIZE( seq->wbut_bot[ib] , False ) ;  /* use seq->dialog   */

   MCW_widget_geom( seq->wtop , &wx,&hy,&xx,&yy ) ;  /* geometry of shell */

   seq->dialog = XtVaCreatePopupShell(
                    "menu" , xmDialogShellWidgetClass , seq->wtop ,
                       XmNtitle , "Display Options" ,
                       XmNdeleteResponse , XmDO_NOTHING ,

                       XmNx , xx+wx+8 ,    /* put off right edge */
                       XmNy , yy+1    ,

                       XmNinitialResourcesPersistent , False ,
                    NULL ) ;

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

   rctop = XtVaCreateWidget(
              "menu" , xmRowColumnWidgetClass , seq->dialog ,
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
      char ** bbh = ISQ_bb_allhelp[ib] ;
      char ** cch = ISQ_bb_allhint[ib] ;

      /*** 30 Oct 1996: transformations just above the IMPROC buttons ***/

      if( ib == NTOG_IMP ){
         int nav = 0 ;

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

   XtPopup( seq->dialog , XtGrabNone ) ;

   ISQ_disp_options( seq , False ) ;  /* set toggles from option list */
   seq->save_opt = seq->opt ;         /* for use with Reset button */

   MCW_alter_widget_cursor( seq->dialog , -XC_left_ptr ,"yellow","blue" ) ;

   ISQ_but_done_reset( seq ) ;
   return ;
}

/*-----------------------------------------------------------------------
  Callback for button and toggle actions in the display dialog
-------------------------------------------------------------------------*/

void ISQ_disp_act_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   MCW_imseq * seq           = (MCW_imseq *) client_data ;
   XmAnyCallbackStruct * cbs = (XmAnyCallbackStruct *) call_data ;

   int ib , close_window ;
   char * wname ;
   Boolean new_opt = False ;

#ifdef FLASH_TOGGLE
   Boolean flasher ;
#endif

   if( !ISQ_REALZ(seq) || seq->dialog==NULL || seq->dialog_starter!=NBUT_DISP ) return ;

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

   } else {                                     /* any other activation: */
      new_opt = ISQ_disp_options( seq , True ); /* --> set options */
   }

   if( close_window ){                          /* close the window */
      XtDestroyWidget( seq->dialog ) ;
      seq->dialog = NULL ;
      for( ib=0 ; ib < NBUTTON_BOT-1 ; ib++ )       /* turn buttons back on */
         if( ISQ_but_bot_dial[ib] == True )         /* that also want to   */
            SENSITIZE( seq->wbut_bot[ib] , True ) ; /* use seq->dialog    */

      for( ib=0 ; ib < seq->num_bbox ; ib++ ) myXtFree( seq->bbox[ib] ) ;
      seq->num_bbox = 0 ;
      seq->dialog_starter = -1 ;

      myXtFree( seq->transform0D_av ) ;
      myXtFree( seq->transform2D_av ) ;
   }

   if( new_opt )
      ISQ_redisplay( seq , -1 , isqDR_reimage ) ;  /* redo current image */

#ifdef FLASH_TOGGLE
   if( flasher ) MCW_invert_widget( w ) ;  /* flash togglebutton */
#endif

   ISQ_but_done_reset( seq ) ;
   return ;
}

/*----------------------------------------------------------------------
  map the toggle-button states TO   the options if set == True,
                               FROM the options if set == False

  in the former case, return True if any options changed, False otherwise
  in the latter case, return False always (options ARE unchanged)
------------------------------------------------------------------------*/

Boolean ISQ_disp_options( MCW_imseq * seq , Boolean set )
{
   int bval[NBOX_DISP] ;
   int ib ;

   if( !ISQ_VALID(seq) || seq->dialog== NULL || seq->dialog_starter!=NBUT_DISP )
      return False ;

   if( set ){
      ISQ_options inopt = seq->opt ;
      Boolean changed ;

      for( ib=0 ; ib < NBOX_DISP ; ib++ )
         bval[ib] = MCW_val_bbox( seq->bbox[ib] ) ;

      seq->opt.mirror      = ( bval[NTOG_MIR] & 1 ) != 0 ;

      seq->opt.rot         = bval[NTOG_ROT] ;

      seq->opt.no_overlay  = ( bval[NTOG_COL] & 1 ) != 0 ;

      seq->opt.scale_group = bval[NTOG_SCL] ;

      seq->opt.scale_range = bval[NTOG_RNG] ;

      seq->opt.free_aspect = ( bval[NTOG_ASP] & ISQ_ASPECT    ) != 0 ;
      seq->opt.save_nsize  = ( bval[NTOG_SAV] & ISQ_SAV_NSIZE ) != 0 ;
      seq->opt.save_pnm    = ( bval[NTOG_SAV] & ISQ_SAV_PNM   ) != 0 ;
      seq->opt.save_one    = ( bval[NTOG_SAV] & ISQ_SAV_ONE   ) != 0 ;
      SET_SAVE_LABEL(seq) ;

      seq->opt.improc_code = bval[NTOG_IMP] ;

      seq->opt.cx_code = bval[NTOG_CX] ;

      /* sanity checks */

      if( seq->opt.rot != ISQ_ROT_0   &&
          seq->opt.rot != ISQ_ROT_90  &&
          seq->opt.rot != ISQ_ROT_180 &&
          seq->opt.rot != ISQ_ROT_270   ) seq->opt.rot = inopt.rot ;

      if( seq->opt.scale_group != ISQ_SCL_AUTO &&
          seq->opt.scale_group != ISQ_SCL_GRP )
                               seq->opt.scale_group = inopt.scale_group ;

      if( seq->opt.scale_range != ISQ_RNG_MINTOMAX &&
          seq->opt.scale_range != ISQ_RNG_02TO98 )
                               seq->opt.scale_range = inopt.scale_range ;

      changed = ! ISQ_OPT_EQUAL( seq->opt , inopt ) ;

#ifdef IMSEQ_DEBUG
printf("ISQ_disp_options: SET VALUES FROM BUTTONS\n"
       "  mirror=%d rot=%d no_overlay=%d\n"
       "  scale_group=%d scale_range=%d free_aspect=%d\n"
       "  save_nsize=%d save_pnm=%d improc_code=%d cx_code=%d\n",
       seq->opt.mirror,seq->opt.rot,seq->opt.no_overlay ,
          seq->opt.scale_group,seq->opt.scale_range,seq->opt.free_aspect ,
          seq->opt.save_nsize,seq->opt.save_pnm,seq->opt.improc_code,seq->opt.cx_code ) ;
#endif

      return changed ;

   } else {

      bval[NTOG_MIR] = (seq->opt.mirror) ? 1 : 0 ;
      bval[NTOG_ROT] = seq->opt.rot ;

      bval[NTOG_COL] = (seq->opt.no_overlay << 0 ) ;

      bval[NTOG_SCL] = seq->opt.scale_group ;
      bval[NTOG_RNG] = seq->opt.scale_range ;

      bval[NTOG_ASP] = (seq->opt.free_aspect) ? ISQ_ASPECT    : 0 ;

      bval[NTOG_SAV] = ( (seq->opt.save_nsize)? ISQ_SAV_NSIZE : 0 )
                      +( (seq->opt.save_pnm)  ? ISQ_SAV_PNM   : 0 )
                      +( (seq->opt.save_one)  ? ISQ_SAV_ONE   : 0 ) ;

      bval[NTOG_IMP] = seq->opt.improc_code ;

      bval[NTOG_CX]  = seq->opt.cx_code ;

      for( ib=0 ; ib < NBOX_DISP ; ib++ )
         MCW_set_bbox( seq->bbox[ib] , bval[ib] ) ;

#ifdef IMSEQ_DEBUG
printf("ISQ_disp_options: SET BUTTONS FROM VALUES\n"
       "  mirror=%d rot=%d no_overlay=%d\n"
       "  scale_group=%d scale_range=%d free_aspect=%d\n"
       "  save_nsize=%d save_pnm=%d improc_code=%d cx_code=%d\n",
       seq->opt.mirror,seq->opt.rot,seq->opt.no_overlay ,
          seq->opt.scale_group,seq->opt.scale_range,seq->opt.free_aspect ,
          seq->opt.save_nsize,seq->opt.save_pnm,seq->opt.improc_code,seq->opt.cx_code ) ;
#endif

      return False ;
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

   if( ! ISQ_VALID(seq) ) return ;

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

   MCW_set_widget_cursor( seq->wtop , -XC_watch ) ;
   MCW_set_widget_cursor( wmsg      , -XC_watch ) ;
   if( seq->dialog != NULL )
      MCW_set_widget_cursor( seq->dialog , -XC_watch ) ;

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

   XtDestroyWidget( wmsg ) ;

   MCW_alter_widget_cursor( seq->wtop , -XC_left_ptr ,"yellow","blue" ) ;
   if( seq->dialog != NULL )
      MCW_alter_widget_cursor( seq->dialog , -XC_left_ptr ,"yellow","blue" ) ;

   return;
}

/*-----------------------------------------------------------------------*/

Boolean ISQ_statistics_WP( XtPointer client_data )
{
   MCW_imseq * seq = (MCW_imseq *) client_data ;
   ISQ_glob_statistics * gl ;

   MRI_IMAGE * im ;
   register int ntot , nser , nn ;

   if( ! ISQ_VALID(seq) ) return True ;

   gl   = seq->glstat ;
   ntot = seq->status->num_total ;  /* image counts */
   nser = seq->status->num_series ;

   /*-- first, check if all individual statistics are done --*/

   if( ! gl->mm_done ){  /* not marked as done:  check them */

      for( nn=0 ; nn < ntot ; nn++ )
         if( ! seq->imstat[nn].one_done ) break ;

#ifdef IMSEQ_DEBUG_STAT
printf("WP: indiv @ nn=%d\n",nn);
fflush(stdout) ;
#endif

      if( nn >= ntot ){ /* all were done, so finish them off */

         gl->min = seq->imstat[0].min ;
         gl->max = seq->imstat[0].max ;
         for( nn=1 ; nn < nser ; nn++ ){ /* global: images in the series */
            gl->min = MIN( gl->min , seq->imstat[nn].min ) ;
            gl->max = MAX( gl->max , seq->imstat[nn].max ) ;
         }
         ISQ_SCLEV(gl->min,gl->max,seq->dc->ncol_im,gl->scl_mm,gl->lev_mm);
         gl->mm_done = True ;

#ifdef IMSEQ_DEBUG_STAT
printf("WP:  gl min=%g max=%g scl_mm=%g lev_mm=%g\n",
       gl->min,gl->max,gl->scl_mm,gl->lev_mm ) ;
fflush(stdout) ;
#endif

         return False ;  /* continue next time on global histogramming */
      }

      /* if here, image nn has yet to be done for local statistics */

      im = (MRI_IMAGE *) seq->getim( nn , isqCR_getimage , seq->getaux ) ;
      if( im != NULL ){
         ISQ_statify_one( seq , nn , im ) ; KILL_1MRI(im) ;
      }
      return False ;   /* continue next time on next un-statted image */
   }

   /* all individual statistics are done --> global histogramming  */
   /* (note only images in the "series" are used for this purpose) */

   if( ! gl->per_done ){  /* global statistics not marked as done */

      for( nn=0 ; nn < nser ; nn++ )
         if( ! seq->imstat[nn].glob_done ) break ;

#ifdef IMSEQ_DEBUG_STAT
printf("WP: glob @ nn=%d\n",nn) ;
fflush(stdout) ;
#endif

      if( nn >= nser ){ /* all were done, so finish them off */

         ISQ_perpoints( gl->min,gl->max,gl->hist ,
                        &(gl->per02) , &(gl->per98) ) ;

         ISQ_SCLEV( gl->per02 , gl->per98 ,
                    seq->dc->ncol_im , gl->scl_per , gl->lev_per ) ;

         gl->per_done = True ;

#ifdef IMSEQ_DEBUG_STAT
printf("WP:  gl per02=%g per98=%g scl_per=%g lev_per=%g\n",
       gl->per02,gl->per98,gl->scl_per,gl->lev_per ) ;
fflush(stdout) ;
#endif

         return True ;  /* don't need to do any more statistics! */
      }

      /* if here, image nn has yet to be done for global histogram */

      im = (MRI_IMAGE *) seq->getim( nn , isqCR_getimage , seq->getaux ) ;
      if( im != NULL ){
         ISQ_statify_one( seq , nn , im ) ; KILL_1MRI(im) ;
      }
      return False ;   /* continue next time on next un-statted image */
   }

   /* shouldn't get here, but if do, print a message and stop work process */

   fprintf(stderr,"\a\n*** imseq work process error!\n") ;
   return True ;
}

/*-----------------------------------------------------------------------
   collect statistics on an image and put into location n in table
-------------------------------------------------------------------------*/

void ISQ_statify_one( MCW_imseq * seq , int n , MRI_IMAGE * im )
{
   ISQ_indiv_statistics * st ;
   ISQ_glob_statistics *  gl ;
   static int hist[NHISTOG] ; /* static to avoid create/destroy overhead */

   /* exit if bad data */

   if( ! ISQ_VALID(seq) || n < 0 || n >= seq->status->num_total ) return ;

   st = &( seq->imstat[n] ) ;
   gl = seq->glstat ;

   if( ! st->one_done ){  /* must do individual statistics */

      st->min = mri_min( im ) ;
      st->max = mri_max( im ) ;

      ISQ_SCLEV( st->min , st->max ,
                 seq->dc->ncol_im , st->scl_mm , st->lev_mm ) ;

      mri_histogram( im , st->min , st->max , True , NHISTOG,hist ) ;

      ISQ_perpoints( st->min,st->max,hist , &(st->per02) , &(st->per98) ) ;

      ISQ_SCLEV( st->per02 , st->per98 ,
                 seq->dc->ncol_im , st->scl_per , st->lev_per ) ;

      st->one_done = True ;

#ifdef IMSEQ_DEBUG_STAT
printf("SO1:  n=%d min=%g max=%g scl_mm=%g lev_mm=%g\n",
       n,st->min,st->max,st->scl_mm,st->lev_mm) ;
printf("SO1:    per02=%g per98=%g scl_per=%g lev_per=%g\n",
       st->per02,st->per98,st->scl_per,st->lev_per) ;
fflush(stdout) ;
#endif

   } else if( n < seq->status->num_series &&
              ! st->glob_done               ){  /* do global */

      mri_histogram( im , gl->min , gl->max , False , NHISTOG , gl->hist ) ;
      st->glob_done = True ;
   }

   return ;
}

/*-----------------------------------------------------------------------*/

void ISQ_perpoints( float bot , float top ,
                    int hist[] , float * per02 , float * per98 )
{
   register int ih , nsum , ns02 , ns98 ;
   float prev , cur , frac , dbin ;
   static int hcum[NHISTOG] ;  /* static to avoid create-destroy overhead */

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

#ifdef IMSEQ_DEBUG_STAT
printf("PP: ih=%d nsum=%d ns02=%d prev=%g cur=%g frac=%g per02=%g\n",
       ih,nsum,ns02,prev,cur,frac,*per02 ) ;
fflush(stdout) ;
#endif

   /*-------*/

   for( ; ih < NHISTOG ; ih++ ) if( hcum[ih] >= ns98 ) break ;

   if( ih == NHISTOG ) ih-- ;

   prev   = (ih == 0) ? (0.0) : hcum[ih-1] ;
   cur    = hcum[ih] ; if( cur <= prev ) cur = 1.01 * prev + 1.0 ;
   frac   = ih + (ns98-prev)/(cur-prev) ;
   *per98 = bot + dbin * frac ;

   if( *per98 > top ) *per98 = top ;

#ifdef IMSEQ_DEBUG_STAT
printf("PP: ih=%d nsum=%d ns98=%d prev=%g cur=%g frac=%g per98=%g\n",
       ih,nsum,ns98,prev,cur,frac,*per98 ) ;
fflush(stdout) ;
#endif

   return ;
}

/*------------------------------------------------------------------------
   change the palette based on the arrow actions
--------------------------------------------------------------------------*/

void ISQ_arrow_CB( MCW_arrowval * av , XtPointer client_data )
{
   MCW_imseq * seq = (MCW_imseq *) client_data ;
   int ddd ;

   if( ! ISQ_REALZ(seq) ) return ;

   if( av->fval > av->old_fval ) ddd = -1 ;
   else                          ddd =  1 ;

/*
   ddd = (av->fval > av->old_fval) ? (-1) : (1) ;
*/

#ifdef IMSEQ_DEBUG
printf("ISQ_arrow_CB: fval=%f old=%f\n" , av->fval , av->old_fval ) ;
fflush(stdout) ;
#endif

        if( av == seq->arrow[NARR_SQUEEZE] )
           DC_palette_squeeze( seq->dc , ddd ) ;

   else if( av == seq->arrow[NARR_BRIGHT]  )
           DC_palette_bright(  seq->dc , ddd ) ;

   else if( av == seq->arrow[NARR_ROTATE]  )
           DC_palette_rotate(  seq->dc ,-ddd ) ;

   else if( av == seq->arrow[NARR_GAMMA]   ){
           double new_gamma = seq->dc->gamma ;
           if( ddd > 0 ) new_gamma *= 0.98 ;
           else          new_gamma /= 0.98 ;

           DC_palette_restore( seq->dc , new_gamma ) ;

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
}

/*-----------------------------------------------------------------------
   Norm button callback: normalize the palette
-------------------------------------------------------------------------*/

void ISQ_but_cnorm_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   MCW_imseq * seq = (MCW_imseq *) client_data ;

   if( ! ISQ_REALZ(seq) ) return ;

   DC_palette_restore( seq->dc , 0.0 ) ;

   ISQ_but_done_reset( seq ) ;
   return ;
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

*    isqDR_sendmontage (ignored) tells the MCW_imseq to send the
                         montage information back via isqCR_newmontage

*    isqDR_periodicmont (int) tells whether to use periodic montages

The Boolean return value is True for success, False for failure.
-------------------------------------------------------------------------*/

Boolean drive_MCW_imseq( MCW_imseq * seq ,
                         int drive_code , XtPointer drive_data )
{
   if( ! ISQ_VALID(seq) ) return False ;

   switch( drive_code ){

      /*------- error! -------*/

      default:{
         fprintf(stderr,"\a\n*** drive_MCW_imseq: code=%d illegal!\n",
                 drive_code) ;
         XBell( seq->dc->display , 100 ) ;
         return False ;
      }
      break ;

      case isqDR_periodicmont:{
        int per = ((int) drive_data) != 0 ;

        if( per != seq->mont_periodic ){
           seq->mont_periodic = per ;
           if( ISQ_REALZ(seq) ) ISQ_redisplay( seq , -1 , isqDR_display ) ;
        }
        return True ;
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
            seq->status->send_CB( seq , seq->getaux , &cbs ) ;
            return True ;
         } else {
            return False ;
         }
      }
      break ;

      /*------ set icon -----*/

      case isqDR_icon:{
         XtVaSetValues( seq->wtop , XmNiconPixmap , (Pixmap) drive_data , NULL ) ;
         return True ;
      }
      break ;

      /*------ get image number -----*/

      case isqDR_getimnr:{
         int * retval = (int *) drive_data ;

         if( retval != NULL ) *retval = seq->im_nr ;
         return True ;
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

         if( turn_on == seq->onoff_state ) return True ;

         MCW_widget_geom( seq->wimage , &ww , &hh , NULL,NULL ) ;

         if( turn_on ){
            XtManageChildren( seq->onoff_widgets , seq->onoff_num ) ;
            XtVaSetValues(
               seq->wimage ,
                  XmNrightPosition ,(int)( 0.49 + seq->image_frac * FORM_FRAC_BASE ),
                  XmNbottomPosition,(int)( 0.49 + seq->image_frac * FORM_FRAC_BASE ),
               NULL ) ;
            XtVaSetValues( seq->wtop ,
                              XmNwidth  , (int)(0.49+ww/seq->image_frac) ,
                              XmNheight , (int)(0.49+hh/seq->image_frac) ,
                           NULL ) ;
         } else {
            XtUnmanageChildren( seq->onoff_widgets , seq->onoff_num ) ;
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
         return True ;
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
         return True ;
      }
      break ;

      /*------- death! -------*/

      case isqDR_destroy:{
         ISQ_but_done_CB( NULL , (XtPointer) seq , NULL ) ;
         return True ;
      }
      break ;

      /*------- unrealize! -------*/

      case isqDR_unrealize:{
         if( ISQ_REALZ(seq) ) XtUnrealizeWidget( seq->wtop ) ;
         seq->valid = 1 ;
         return True ;
      }
      break ;

      /*------- realize! -------*/

      case isqDR_realize:{
         if( ! ISQ_REALZ(seq) ){
            XtRealizeWidget( seq->wtop ) ;
            MCW_alter_widget_cursor( seq->wtop , -XC_left_ptr ,"yellow","blue") ;
         }
         seq->valid = 2 ;
         return True ;
      }
      break ;

      /*------- change helptext! -------*/

      case isqDR_imhelptext:{
        char * newtxt = (char *) drive_data ;
        int ii ;

        if( newtxt == NULL ) return False ;
        ii = strlen(newtxt) ;
        if( ii == 0 ) return False ;

        strncpy( seq->im_helptext , newtxt , ISQ_NHELP ) ;
        seq->im_helptext[ISQ_NHELP] = '\0' ;
        return True ;
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
         return True ;
      }
      break ;

      /*------- new cursor for image -------*/

      case isqDR_cursor:{
         int cur = (int) drive_data ;

         MCW_alter_widget_cursor( seq->wimage , cur , "yellow" , "blue" ) ;
         return True ;
      }
      break ;

      /*------- new options -------*/

      case isqDR_options:{
         ISQ_options * newopt = (ISQ_options *) drive_data ;

         if( newopt != NULL ) seq->opt = * newopt ;
         seq->opt.parent = (XtPointer) seq ;

         ISQ_redisplay( seq , -1 , isqDR_display ) ;
         return True ;
      }
      break ;

      /*------- turn arrowpad on -------*/

      case isqDR_arrowpadon:{
         char * helptext = (char *) drive_data ;

         XtSetMappedWhenManaged( seq->arrowpad->wform , True ); /* on */

         if( helptext != NULL && strlen(helptext) > 0 ){
            char * str = XtNewString( helptext ) ;
            MCW_reghelp_children( seq->arrowpad->wform , str ) ;
         }
         return True ;
      }
      break ;

      case isqDR_arrowpadhint:{
         int ib ;
         char ** hint = (char **) drive_data ;
         if( hint == NULL ) return False ;
         for( ib=0 ; ib < 5 ; ib++ )
            MCW_register_hint( seq->arrowpad->wbut[ib] , hint[ib] ) ;
         return True ;
      }
      break ;

      /*------- turn arrowpad off -------*/

      case isqDR_arrowpadoff:{
         XtSetMappedWhenManaged( seq->arrowpad->wform , False ); /* off */
         return True ;
      }
      break ;

      /*------- new numtotal -------*/

      case isqDR_numtotal:{
         int newtot = (int) drive_data ,
             oldtot = seq->status->num_total ,
             numser = seq->status->num_series , ii ;
         char * msg =
             "illegal change to image\n"
             "count from driver routine\n"
             "(press here to continue)" ;

         /* check for error conditions */

         if( newtot == oldtot ) return True ;

         if( newtot < 2 || newtot < numser ){
            if( ISQ_REALZ(seq) )
               MCW_popup_message( seq->wimage , msg , MCW_USER_KILL ) ;
            fprintf(stderr,"\n%s\n",msg) ;
            return False ;
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

         return True ;
      }
      break ;

      /*------- new image sequence!!! -------*/

      case isqDR_newseq:{
         Boolean good ;
         good = ISQ_setup_new( seq , drive_data ) ;
         return good ;
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

   return False ;  /* should never be reached! */
}

/*---------------------------------------------------------------------*/

#define XYORG 128
#define DXY    64

void ISQ_arrowpad_CB( MCW_arrowpad * apad , XtPointer client_data )
{
   MCW_imseq * seq = (MCW_imseq *) client_data ;

   ISQ_cbs cbs ;
   int xorg,yorg , xwin,ywin , xoff,yoff ;

   if( ! ISQ_REALZ(seq) || seq->status->send_CB == NULL ) return ;

   cbs.event = &(apad->xev) ;  /* copy event for user's edification */

   if( apad->which_pressed == AP_MID ){
      cbs.reason = isqCR_appress ;
      seq->status->send_CB( seq , seq->getaux , &cbs ) ;
      return ;
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
   else                   return ;                     /* error! */

   seq->status->send_CB( seq , seq->getaux , &cbs ) ;
   return ;
}

/*-------------------------------------------------------------------
   Setup the data structures to handle a new sequence of images;
   this should be immediately followed by a call to set the image
   to the correct number (or things won't look good at all).
---------------------------------------------------------------------*/

Boolean ISQ_setup_new( MCW_imseq * seq , XtPointer newaux )
{
   MCW_imseq_status * imstatus ;
   int ii ;
   MRI_IMAGE * tim ;

   if( !ISQ_VALID(seq) ) return False ;

   imstatus = (MCW_imseq_status *) seq->getim(0,isqCR_getstatus,newaux);
   if( imstatus->num_total < 2 ) return False ;

#if 0
   tim = (MRI_IMAGE *) seq->getim(0,isqCR_getqimage,newaux) ; /* 1st image */
   KILL_1MRI(tim) ;  /* don't need tim no more */
#endif

   seq->status = imstatus ;
   seq->im_nr  = imstatus->num_total / 2 ;  /* do this image 1st */

   KILL_1MRI(seq->imim) ;  /* NULL out all internally stored images */
   KILL_1MRI(seq->ovim) ;

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

   XtVaSetValues( seq->wscale ,
                     XmNmaximum , seq->status->num_total - 1 ,
                     XmNvalue   , seq->im_nr ,
                  NULL ) ;


#ifdef IMSEQ_DEBUG
printf("ISQ_setup_new: hbase=%d vbase=%d nim=%d lev=%g\n",
          seq->hbase,seq->vbase,
          seq->status->num_total,seq->lev ) ;
fflush(stdout) ;
#endif

   seq->getaux = newaux ;

   return True ;
}

/*----------------------------------------------------------------------*/
/*----         Stuff for the menu hidden on the color bar           ----*/

void ISQ_wbar_menu_CB( Widget w , XtPointer client_data ,
                                  XtPointer call_data    )
{
   MCW_imseq * seq = (MCW_imseq *) client_data ;

   if( ! ISQ_REALZ(seq) || w == NULL || ! XtIsWidget(w) ) return ;

   /*** User range toggle ***/

   if( w == seq->wbar_rng_but ){
      MCW_choose_string( seq->wbar , "Display range: bot top [ztop]" ,
                         NULL , ISQ_set_rng_CB , seq ) ;
   }

   if( w == seq->wbar_zer_but ){
      MCW_choose_ovcolor( seq->wbar , seq->dc , seq->zer_color ,
                          ISQ_set_zcol_CB , seq ) ;
   }

   if( w == seq->wbar_flat_but ){
      MCW_choose_string( seq->wbar , "Flatten range: bot top" ,
                         NULL , ISQ_set_flat_CB , seq ) ;
   }

   if( w == seq->wbar_sharp_but ){
      MCW_choose_integer( seq->wbar , "Sharpen Percentage" ,
                          1 , 99 , (int)(100*seq->sharp_fac) ,
                          ISQ_set_sharp_CB , seq ) ;
   }

   return ;
}

void ISQ_set_rng_CB( Widget w , XtPointer cd , MCW_choose_cbs * cbs )
{
   MCW_imseq * seq = (MCW_imseq *) cd ;

   if( ! ISQ_REALZ(seq) || w == NULL || ! XtIsWidget(w) ) return ;

   seq->rng_bot = seq->rng_top = seq->rng_ztop = 0 ;
   sscanf( cbs->cval , "%f%f%f" ,
           &(seq->rng_bot) , &(seq->rng_top) , &(seq->rng_ztop) ) ;
   ISQ_redisplay( seq , -1 , isqDR_reimage ) ;  /* redo current image */
   return ;
}

void ISQ_set_zcol_CB( Widget w , XtPointer cd , MCW_choose_cbs * cbs )
{
   MCW_imseq * seq = (MCW_imseq *) cd ;

   if( ! ISQ_REALZ(seq) || w == NULL || ! XtIsWidget(w) ) return ;

   seq->zer_color = cbs->ival ;
   ISQ_redisplay( seq , -1 , isqDR_reimage ) ;  /* redo current image */
   return ;
}

void ISQ_set_flat_CB( Widget w , XtPointer cd , MCW_choose_cbs * cbs )
{
   MCW_imseq * seq = (MCW_imseq *) cd ;

   if( ! ISQ_REALZ(seq) || w == NULL || ! XtIsWidget(w) ) return ;

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
   return ;
}

void ISQ_set_sharp_CB( Widget w , XtPointer cd , MCW_choose_cbs * cbs )
{
   MCW_imseq * seq = (MCW_imseq *) cd ;

   if( ! ISQ_REALZ(seq) || w == NULL || ! XtIsWidget(w) ) return ;

   seq->sharp_fac = 0.01 * cbs->ival ;

   ISQ_redisplay( seq , -1 , isqDR_reimage ) ;  /* redo current image */
   return ;
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
   int ib , wx,hy,xx,yy ;
   Widget wrc ;

   if( ! ISQ_REALZ(seq) || seq->dialog != NULL ) return ;

   for( ib=0 ; ib < NBUTTON_BOT-1 ; ib++ )        /* turn off buttons  */
      if( ISQ_but_bot_dial[ib] == True )          /* that also want to */
        SENSITIZE( seq->wbut_bot[ib] , False ) ;  /* use seq->dialog   */

   MCW_widget_geom( seq->wtop , &wx,&hy,&xx,&yy ) ;  /* geometry of shell */

   seq->dialog = XtVaCreatePopupShell(
                    "menu" , xmDialogShellWidgetClass , seq->wtop ,
                       XmNtitle , "Montage" ,
                       XmNdeleteResponse , XmDO_NOTHING ,

                       XmNx , xx+wx+8 ,    /* put off right edge */
                       XmNy , yy+1    ,

                       XmNinitialResourcesPersistent , False ,
                    NULL ) ;

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
                          wrc , "Across:" ,
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
                                0 , seq->dc->ncol_ov - 1 , seq->mont_gapcolor ,
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
                         "Spacing between slices" ) ;

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

   for( ib=0 ; ib < NUM_MONT_ACT ; ib++ ) MONT_act[ib].data = (XtPointer) seq ;

   (void) MCW_action_area( wrc , MONT_act , NUM_MONT_ACT ) ;

   XtManageChild( wrc ) ;
   XtPopup( seq->dialog , XtGrabNone ) ;
   MCW_alter_widget_cursor( seq->dialog , -XC_left_ptr ,"yellow","blue" ) ;
   ISQ_but_done_reset( seq ) ;
   return ;
}

void ISQ_montage_action_CB( Widget w , XtPointer client_data , XtPointer call_data )
{
   MCW_imseq * seq = (MCW_imseq *) client_data ;
   XmAnyCallbackStruct * cbs = (XmAnyCallbackStruct *) call_data ;
   char * wname ;
   int ib , close_window , new_mont ;

   if( !ISQ_REALZ(seq) || seq->dialog==NULL || seq->dialog_starter!=NBUT_MONT ) return ;

   wname = XtName(w) ;

   for( ib=0 ; ib < NUM_MONT_ACT ; ib++ )           /* button index, if any */
      if( strcmp(wname,MONT_act[ib].label) == 0 ) break ;

   close_window = (ib == MONT_DONE || ib == MONT_QUIT || ib == NUM_MONT_ACT) ;

   if( close_window ){
      XtPopdown( seq->dialog ) ;
      XSync( XtDisplay(w) , False ) ;
      XmUpdateDisplay( w ) ;
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
            seq->status->send_CB( seq , seq->getaux , &cbs ) ;
            seq->ignore_redraws = 0 ;         /* can listen again */
         }

         ISQ_redisplay( seq , -1 , isqDR_display ) ;    /* local redraw */

         if( ib == MONT_APPLY ) MCW_invert_widget(w) ;

         seq->mont_nx_old       = seq->mont_nx ;
         seq->mont_ny_old       = seq->mont_ny ;
         seq->mont_skip_old     = seq->mont_skip ;
         seq->mont_gap_old      = seq->mont_gap ;
         seq->mont_gapcolor_old = seq->mont_gapcolor ;

         /* set to "Save:one" if have an actual montage going now */

         if( seq->mont_nx * seq->mont_ny > 1 && !seq->opt.save_one ){
            seq->opt.save_nsize  = 0 ;
            seq->opt.save_pnm    = 0 ;
            seq->opt.save_one    = 1 ;
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
      XtDestroyWidget( seq->dialog ) ;
      seq->dialog = NULL ;
      for( ib=0 ; ib < NBUTTON_BOT-1 ; ib++ )       /* turn buttons back on */
         if( ISQ_but_bot_dial[ib] == True )         /* that also want to   */
            SENSITIZE( seq->wbut_bot[ib] , True ) ; /* use seq->dialog    */

      myXtFree( seq->mont_across_av ) ;
      myXtFree( seq->mont_down_av ) ;
      myXtFree( seq->mont_skip_av ) ;
      myXtFree( seq->mont_gap_av ) ;
      myXtFree( seq->mont_gapcolor_av ) ;

      seq->mont_across_av   = NULL ;
      seq->mont_down_av     = NULL ;
      seq->mont_skip_av     = NULL ;
      seq->mont_gap_av      = NULL ;
      seq->mont_gapcolor_av = NULL ;

      seq->dialog_starter = -1 ;
   }

   return ;
}

/*---------------------------------------------------------------------------
   Routine to get one image for the montage display.
-----------------------------------------------------------------------------*/

MRI_IMAGE * ISQ_manufacture_one( int nim , int overlay , MCW_imseq * seq )
{
   MRI_IMAGE * im , * ovim , * tim ;
   int nrold , nwrap ;

   if( ! ISQ_VALID(seq) ) return NULL ;

DPR("ISQ_manufacture_image");

   if( seq->mont_periodic ){
      nwrap = (nim < 0) || (nim >= seq->status->num_total) ;
      while( nim < 0 )                       nim += seq->status->num_total ;
      while( nim >= seq->status->num_total ) nim -= seq->status->num_total ;
   } else {
      if( nim < 0 || nim >= seq->status->num_total ) return NULL ;
      nwrap = 0 ;
   }

   /** Not an overlay image **/

   if( ! overlay ){
      tim = (MRI_IMAGE *) seq->getim( nim , isqCR_getimage , seq->getaux ) ;
      if( tim == NULL ) return NULL ;
      im = ISQ_process_mri( nim , seq , tim ) ; mri_free(tim) ;

#if 0                                      /* puts a marker on the image */
      if( nwrap ){
         short * shar = MRI_SHORT_PTR(im) ;
         shar[0] = shar[1] = shar[im->nx] = seq->top ;
      }
#endif

      return im ;
   }

   /** Get the overlay image **/

   if( seq->opt.no_overlay ) return NULL ;

   tim = (MRI_IMAGE *) seq->getim( nim , isqCR_getoverlay , seq->getaux ) ;

   if( tim == NULL ) return NULL ;

   if( tim->kind != MRI_short ){
      fprintf(stderr,"\a\n*** Illegal non-short overlay image! ***\n") ;
      mri_free(tim) ; return NULL ;
   }

   ovim = mri_flippo( ISQ_TO_MRI_ROT(seq->opt.rot),seq->opt.mirror,tim ) ;
   if( tim != ovim ) mri_free(tim) ;
   return ovim ;
}

/*---------------------------------------------------------------------------
   Routine to make a montage of images
   (version of ISQ_make_image when more than one is needed).
-----------------------------------------------------------------------------*/

void ISQ_make_montage( MCW_imseq * seq )
{
   MRI_IMAGE * im , * ovim , * tim ;
   Boolean reset_done = False ;
   float fac , wmm , hmm ;
   short gap_ov ;

   if( ! ISQ_VALID(seq) ) return ;

DPR("ISQ_make_montage");

   KILL_2XIM( seq->given_xim , seq->sized_xim ) ;  /* erase the XImages */

   /* process toggled options that affect the image that may be stored */

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

      ijcen = (seq->mont_nx)/2 + (seq->mont_ny/2) * seq->mont_nx ;
      for( ij=0 ; ij < nmont ; ij++ ){
         nim = seq->im_nr + (seq->mont_skip + 1)* (ij - ijcen) ;

DPRI(" Getting montage underlay",nim) ;

         tim = ISQ_manufacture_one( nim , 0 , seq ) ;
         ADDTO_IMARR(mar,tim) ;

         if( tim != NULL ) nxyim++ ;

         if( nim == seq->im_nr ){
            new_width_mm  = IM_WIDTH(tim)  ; nxim = tim->nx ;
            new_height_mm = IM_HEIGHT(tim) ; nyim = tim->ny ;
            seq->last_image_type = tim->kind ;
         }
      }

DPRI(" Making underlay cat2D from",nxyim) ;

      gap_ov = -(seq->mont_gapcolor) ;  /* negative ==> overlay */

      seq->imim = im = mri_cat2D( seq->mont_nx , seq->mont_ny ,      /* save this */
                                  seq->mont_gap , &gap_ov , mar ) ;  /* underlay  */

DPR("Destroying underlay image array") ;

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

   if( seq->opt.free_aspect != seq->old_opt.free_aspect && !reset_done )
      ISQ_reset_dimen( seq , seq->last_width_mm , seq->last_height_mm ) ;

   /*--- set the overlay to process ---*/

   if( seq->opt.no_overlay ){
      KILL_1MRI( seq->ovim ) ; ovim = NULL ;
   } else {
      ovim = seq->ovim ;
      if( ovim == NULL ){
         int ij , nim , nmont = seq->mont_nx * seq->mont_ny , nov = 0 , ijcen ;
         MRI_IMARR * mar ;

         INIT_IMARR(mar) ;

         ijcen = (seq->mont_nx)/2 + (seq->mont_ny/2) * seq->mont_nx ;
         for( ij=0 ; ij < nmont ; ij++ ){
            nim = seq->im_nr + (seq->mont_skip + 1) * (ij - ijcen) ;

DPRI(" Getting montage overlay",nim) ;

            tim = ISQ_manufacture_one( nim , 1 , seq ) ;
            ADDTO_IMARR(mar,tim) ;
            if( tim != NULL ) nov++ ;
         }

DPRI(" Making overlay cat2D from",nov) ;

         if( nov > 0 ){
            gap_ov = 0 ;
            ovim = seq->ovim =                                /* save this */
               mri_cat2D( seq->mont_nx , seq->mont_ny ,       /* overlay   */
                          seq->mont_gap , &gap_ov ,  mar ) ;
         } else ovim = seq->ovim = NULL ;                     /* nothing */

DPR("Destroying overlay image array") ;

         DESTROY_IMARR( mar ) ;
      }
   }

   /* set old_opt to current options */

   seq->old_opt = seq->opt ;

   seq->mont_nx_old        = seq->mont_nx        ;
   seq->mont_ny_old        = seq->mont_ny        ;
   seq->mont_skip_old      = seq->mont_skip      ;
   seq->mont_gap_old       = seq->mont_gap       ;
   seq->mont_gapcolor_old  = seq->mont_gapcolor  ;

   /* overlay, if needed */

   if( ovim == NULL || seq->opt.no_overlay ){     /* no processing of overlay */
      tim = im ;
   } else {                                       /* process overlay */
      register short * tar , * oar , * iar ;
      register int ii , npix = im->nx * im->ny ;

      tim = mri_new( im->nx , im->ny , MRI_short ) ;
      tar = MRI_SHORT_PTR( tim ) ;
      oar = MRI_SHORT_PTR( ovim ) ;
      iar = MRI_SHORT_PTR( im ) ;
#ifdef DONT_USE_MEMCPY
      for( ii=0 ; ii < npix ; ii++ )
         tar[ii] = (oar[ii] == 0) ? iar[ii] : -oar[ii] ;
#else
      (void) memcpy( tar , iar , sizeof(short)*npix ) ; /* this code assumes   */
      for( ii=0 ; ii < npix ; ii++ )                    /* that relatively few */
         if( oar[ii] > 0 ) tar[ii] = -oar[ii] ;         /* pixels are overlaid */
#endif
   }

   /* convert result to XImage for display */

   seq->given_xim = mri_to_XImage( seq->dc , tim ) ;

   if( tim != im ) KILL_1MRI(tim) ;
   return ;
}

/*----------------------------------------------------------------------
    Given a pair of coordinates in the image window, find the original
    coordinates they come from in the image, allowing for rotations,
    mirroring, and montaging.  This new version (April 1996) also
    returns the image number that the coordinates occurred in, since
    that may vary with montaging.
-------------------------------------------------------------------------*/

void ISQ_mapxy( MCW_imseq * seq, int xwin, int ywin,
                int * xim, int * yim, int * nim )
{
   int win_wide,win_high , nxim,nyim ;
   int monx,mony,monsk,mongap , win_wide_orig,win_high_orig ;
   int xorg , yorg , ijcen , xcol,yrow , ij ;

   if( ! ISQ_REALZ(seq) ) return ;

DPR("ISQ_mapxy") ;

   nxim  = seq->horig     ; nyim   = seq->vorig    ;  /* sizes of original images */
   monx  = seq->mont_nx   ; mony   = seq->mont_ny  ;  /* montage layout parameters */
   monsk = seq->mont_skip ; mongap = seq->mont_gap ;

   win_wide_orig = nxim * monx + mongap * (monx-1) ;  /* un-resized (original) */
   win_high_orig = nyim * mony + mongap * (mony-1) ;  /* displayed image sizes */

   /* get actual (display) image sizes */

   MCW_widget_geom( seq->wimage , &win_wide , &win_high , NULL,NULL ) ;

   /* convert actual coordinates input to
      equivalent coordinates in the original (montaged) image */

   xorg = ( (float) xwin / win_wide ) * win_wide_orig /* + 0.49 */ ;
   yorg = ( (float) ywin / win_high ) * win_high_orig /* + 0.49 */ ;

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

   return ;
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

   *xflip = xim ; *yflip = yim ; return ;
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

void ISQ_transform_CB( MCW_arrowval * av , XtPointer cd )
{
   MCW_imseq * seq = (MCW_imseq *) cd ;

   if( ! ISQ_VALID(seq) ) return ;

   /** set the 0D transform function pointer **/

   if( av != NULL && av == seq->transform0D_av ){
      if( seq->status->transforms0D == NULL || av->ival <= 0 ||
          av->ival > seq->status->transforms0D->num            ){

         seq->transform0D_func  = NULL ;  /* no transform */
         seq->transform0D_index = 0 ;
      } else {
         seq->transform0D_func  = seq->status->transforms0D->funcs[av->ival - 1] ;
         seq->transform0D_index = av->ival ;
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
      }
   }

   ISQ_redisplay( seq , -1 , isqDR_reimage ) ;  /* redo current image */
   return ;
}

/*----------------------- Sample 2D transformations --------------------*/

static float * atemp = NULL ;
static int    natemp = -666 ;

#define MAKE_ATEMP(nvox)                     \
  do{ if( natemp < (nvox) ){                 \
         if( atemp != NULL ) free(atemp) ;   \
         natemp = (nvox) ;                   \
         atemp  = (float *) malloc( sizeof(float) * natemp ) ; } } while(0)

void median9_box_func( int nx , int ny , double dx, double dy, float * ar )
{
   int ii , jj , nxy , joff ;
   float aa[9] ;
   float * ajj , * ajm , * ajp ;

   if( nx < 3 || ny < 3 ) return ;

   /** make space and copy input into it **/

   nxy = nx * ny ;
   MAKE_ATEMP(nxy) ; if( atemp == NULL ) return ;
   for( ii=0 ; ii < nxy ; ii++ ) atemp[ii] = ar[ii] ;

   /** process copy of input back into the input array **/

   for( jj=0 ; jj < ny ; jj++ ){

      joff = jj * nx ;      /* offset into this row */
      ajj  = atemp + joff ; /* pointer to this row */

      ajm  = (jj==0   ) ? ajj : ajj-nx ;  /* pointer to last row */
      ajp  = (jj==ny-1) ? ajj : ajj+nx ;  /* pointer to next row */

      /* do interior points of this row */

      for( ii=1 ; ii < nx-1 ; ii++ ){
         aa[0] = ajm[ii-1] ; aa[1] = ajm[ii] ; aa[2] = ajm[ii+1] ;
         aa[3] = ajj[ii-1] ; aa[4] = ajj[ii] ; aa[5] = ajj[ii+1] ;
         aa[6] = ajp[ii-1] ; aa[7] = ajp[ii] ; aa[8] = ajp[ii+1] ;
         isort_float( 9 , aa ) ; ar[ii+joff] = aa[4] ;
      }

      /* do leading edge point (ii=0) */

      aa[0] = ajm[0] ; aa[1] = ajm[0] ; aa[2] = ajm[1] ;
      aa[3] = ajj[0] ; aa[4] = ajj[0] ; aa[5] = ajj[1] ;
      aa[6] = ajp[0] ; aa[7] = ajp[0] ; aa[8] = ajp[1] ;
      isort_float( 9 , aa ) ; ar[joff] = aa[4] ;

      /* do trailing edge point (ii=nx-1) */

      aa[0] = ajm[nx-2] ; aa[1] = ajm[nx-1] ; aa[2] = ajm[nx-1] ;
      aa[3] = ajj[nx-2] ; aa[4] = ajj[nx-1] ; aa[5] = ajj[nx-1] ;
      aa[6] = ajp[nx-2] ; aa[7] = ajp[nx-1] ; aa[8] = ajp[nx-1] ;
      isort_float( 9 , aa ) ; ar[nx-1+joff] = aa[4] ;
   }
   return ;
}
