#ifndef _MCW_PBAR_H_
#define _MCW_PBAR_H_

#include <Xm/Label.h>
#include <Xm/PanedW.h>
#include <Xm/DrawnB.h>
#include <Xm/BulletinB.h>

#include <stdio.h>
#include <string.h>
#include <math.h>

#include "display.h"
#include "bbox.h"
#include "xutil.h"

void PBAR_click_CB( Widget , XtPointer , XtPointer ) ;
void PBAR_set_CB( Widget , XtPointer , MCW_choose_cbs * ) ;
void PBAR_resize_CB( Widget , XtPointer , XtPointer ) ;
void PBAR_labelize( float , char * ) ;

#define check_width 8
#define check_height 8
static char check_bits[] = {
   0x11, 0xaa, 0x44, 0xaa, 0x11, 0xaa, 0x44, 0xaa};

static Pixmap check_pixmap = XmUNSPECIFIED_PIXMAP ;

#define NPANE_MIN       2
#define NPANE_MAX       20
#define PANE_WIDTH      15
#define PANE_MIN_HEIGHT  5
#define PANE_LOFF        6
#define PANE_SPACING     2

#define PANE_MAXMODE     2

#define NPANE_NOSASH    21
#define SASH_HYES        5
#define SASH_HNO         1

#define KEEP_LABEL(ip,np) ( ((ip) <= (np) && (np) < NPANE_NOSASH) || ((ip) == 0 || (ip) == (np)) )

#define pbCR_COLOR       (1<<0)
#define pbCR_VALUE       (1<<1)

typedef struct {
  Widget top , panew , panes[NPANE_MAX]   , labels[NPANE_MAX+1] ;
  int num_panes , panes_sum , panew_height , ov_index[NPANE_MAX] , renew_all ;
  float pval[NPANE_MAX+1] ;
  MCW_DC * dc ;

  float pval_save[NPANE_MAX+1][NPANE_MAX+1][PANE_MAXMODE] ;  /* saved values */
  int   ovin_save[NPANE_MAX+1][NPANE_MAX+1][PANE_MAXMODE] ;
  int   npan_save[PANE_MAXMODE] ;

  int   update_me , mode , hide_changes ;

  gen_func * pb_CB ;
  XtPointer  pb_data ;

  XtPointer parent ;
} MCW_pbar ;

MCW_pbar * new_MCW_pbar( Widget , MCW_DC * ,
                         int,int , float,float , gen_func * , XtPointer ) ;

void alter_MCW_pbar( MCW_pbar * , int , float * ) ;
void update_MCW_pbar( MCW_pbar * ) ;

#endif
