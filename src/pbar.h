/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#ifndef _MCW_PBAR_H_
#define _MCW_PBAR_H_

#include <Xm/Label.h>
#include <Xm/PanedW.h>
#include <Xm/DrawnB.h>
#include <Xm/BulletinB.h>

#include <stdio.h>
#include <string.h>
#include <math.h>

#include "mrilib.h"      /* 15 Jun 2000 */
#include "mcw_malloc.h"

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

#define NPANE_MIN        2
#define NPANE_MAX       20
#define PANE_WIDTH      15
#define PANE_MIN_HEIGHT  5
#define PANE_LOFF        6
#define PANE_SPACING     2

#define PANE_MAXMODE     2

#define NPANE_NOSASH    21  /* doesn't work well */
#define SASH_HYES        5
#define SASH_HNO         1

#define KEEP_LABEL(ip,np) ( ((ip)<=(np) && (np)<NPANE_NOSASH) || ((ip)==0 || (ip)==(np)) )

#define pbCR_COLOR       (1<<0)
#define pbCR_VALUE       (1<<1)

#define NPANE_BIG      128    /* 30 Jan 2003: # colors in "big" mode */

typedef struct {
  Widget top , panew , panes[NPANE_MAX]   , labels[NPANE_MAX+1] ;
  int num_panes , panes_sum , panew_height , ov_index[NPANE_MAX] , renew_all ;
  float pval[NPANE_MAX+1] ;
  MCW_DC * dc ;

  float pval_save[NPANE_MAX+1][NPANE_MAX+1][PANE_MAXMODE] ;  /* saved values */
  int   ovin_save[NPANE_MAX+1][NPANE_MAX+1][PANE_MAXMODE] ;
  int   npan_save[PANE_MAXMODE] ;

  int   pane_hsum[NPANE_MAX+1] ;  /* Dec 1997 */

  int   update_me , mode , hide_changes , keep_pval ;

  gen_func * pb_CB ;
  XtPointer  pb_data ;

  XtPointer parent ;

  int    bigmode , bigset ;     /* 30 Jan 2003 */
  float  bigtop , bigbot ;
  rgbyte bigcolor[NPANE_BIG] ;
  XImage * bigxim ;
  int    bigmap_index ;         /* 31 Jan 2003 */
} MCW_pbar ;

MCW_pbar * new_MCW_pbar( Widget , MCW_DC * ,
                         int,int , float,float , gen_func * , XtPointer ) ;

void alter_MCW_pbar( MCW_pbar * , int , float * ) ;
void update_MCW_pbar( MCW_pbar * ) ;

MRI_IMAGE * MCW_pbar_to_mri( MCW_pbar *,int,int ) ; /* 15 Jun 2000 */

void rotate_MCW_pbar( MCW_pbar * , int ) ; /* 30 Mar 2001 */

void PBAR_set_panecolor( MCW_pbar *, int , int ) ;  /* 17 Jan 2003 */

void PBAR_set_bigmode( MCW_pbar *, int, float,float ) ;     /* 30 Jan 2003 */
void PBAR_bigexpose_CB( Widget , XtPointer , XtPointer ) ;  /* 30 Jan 2003 */
void PBAR_add_bigmap( char * , rgbyte * ) ;                 /* 31 Jan 2003 */
void PBAR_read_bigmap( char *, MCW_DC * ) ;                 /* 01 Feb 2003 */

#endif
