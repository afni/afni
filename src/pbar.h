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

#include "pbardefs.h"

#ifdef  __cplusplus
extern "C" {                    /* care of Greg Balls    7 Aug 2006 [rickr] */
#endif

void PBAR_click_CB   ( Widget , XtPointer , XtPointer ) ;
void PBAR_setcolor_CB( Widget , XtPointer , MCW_choose_cbs * ) ;
void PBAR_setonoff_CB( Widget , XtPointer , MCW_choose_cbs * ) ;  /* 10 Feb 2012 */
void PBAR_resize_CB  ( Widget , XtPointer , XtPointer ) ;
void PBAR_labelize   ( float , char * ) ;

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

#define NPANE_NOSASH    21  /* doesn't work well, so this disables the no-sash feature */
#define SASH_HYES        5
#define SASH_HNO         1

#define KEEP_LABEL(ip,np) ( ((ip)<=(np) && (np)<NPANE_NOSASH) || ((ip)==0 || (ip)==(np)) )

#define pbCR_COLOR       (1<<0)
#define pbCR_VALUE       (1<<1)

#define PBAR_BIGTHREE_MASK  1


typedef struct {
  Widget top , panew , panes[NPANE_MAX]   , labels[NPANE_MAX+1] ;
  int num_panes , panes_sum , panew_height , ov_index[NPANE_MAX] , renew_all ;
  float pval[NPANE_MAX+1] ;
  MCW_DC *dc ;

  float pval_save[NPANE_MAX+1][NPANE_MAX+1][PANE_MAXMODE] ;  /* saved values */
  int   ovin_save[NPANE_MAX+1][NPANE_MAX+1][PANE_MAXMODE] ;
  int   npan_save[PANE_MAXMODE] ;

  int   pane_hsum[NPANE_MAX+1] ;  /* Dec 1997 */

  int   update_me , mode , hide_changes , keep_pval ;

  gen_func *pb_CB ;
  XtPointer pb_data ;

  XtPointer parent ;

  int    bigmode , bigset ;     /* 30 Jan 2003 */
  float  bigtop , bigbot , bigmax ;
  rgbyte bigcolor[NPANE_BIGGEST] ;
  char  *bigname ;              /* 22 Oct 2003 */
  XImage *bigxim ;
  int    bigmap_index ;         /* 31 Jan 2003 */
  float  bigfac ;               /* 11 Feb 2003 */
  int    bigflip ;              /* 07 Feb 2004 */
  int    bigrota ;              /* 07 Feb 2004 */
  int    big30, big32 , big31 ; /* Feb 2012 */
  int    bigh0, bigh1 , bigh2 ; /* Feb 2012 */
  int    ignore_resize ;        /* Feb 2012 */
  int    dont_alter_bigmax ;    /* Feb 2012 */

  Widget big_menu, big_label, big_choose_pb, big_scaleup_pb, big_scaledn_pb ;
  Widget big_picktopbot_pb ;
} MCW_pbar ;

MCW_pbar * new_MCW_pbar( Widget , MCW_DC * ,
                         int,int, float,float, gen_func *, XtPointer, int ) ;

void alter_MCW_pbar( MCW_pbar * , int , float * ) ;
void update_MCW_pbar( MCW_pbar * ) ;

int MCW_pbars_equivalent( MCW_pbar *, MCW_pbar * ) ; /* 07 Jul 2014 */

MRI_IMAGE * MCW_pbar_to_mri( MCW_pbar *,int,int ) ; /* 15 Jun 2000 */

void rotate_MCW_pbar( MCW_pbar * , int ) ; /* 30 Mar 2001 */

void PBAR_set_panecolor( MCW_pbar *, int , int ) ;  /* 17 Jan 2003 */

void PBAR_set_bigmode( MCW_pbar *, int, float,float ) ;     /* 30 Jan 2003 */
void PBAR_bigexpose_CB( Widget , XtPointer , XtPointer ) ;  /* 30 Jan 2003 */
void PBAR_add_bigmap( char * , rgbyte * ) ;                 /* 31 Jan 2003 */
void PBAR_read_bigmap( char *, MCW_DC * ) ;                 /* 01 Feb 2003 */
void PBAR_make_bigmap( char *,
                       int, float *, rgbyte *, MCW_DC * );  /* 02 Feb 2003 */

void PBAR_set_bigmap( MCW_pbar * , char * ) ;               /* 03 Feb 2003 */
void PBAR_set_bigmap_index( MCW_pbar * , int ) ;            /* 11 Mar 2011 */
char * PBAR_get_bigmap( MCW_pbar * ) ;                      /* 03 Feb 2003 */
int PBAR_define_bigmap( char *cmd ) ;                       /* 07 Feb 2003 */
void PBAR_flip( MCW_pbar * ) ;                              /* 07 Feb 2004 */
int PBAR_get_bigmap_index ( char *bnam );                   /* 26 Feb 2010 ZSS */

extern int AFNI_set_func_range_nval( XtPointer *vp_im3d, float val);
                                                            /* 15 Feb 2010 */

extern int AFNI_set_dset_pbar(XtPointer *vp_im3d);          /* 26 Feb 2010 ZSS */

#define PBAR_force_bigexpose(pb) \
  do{ MCW_kill_XImage(pb->bigxim); pb->bigxim = NULL; PBAR_bigexpose_CB(NULL,pb,NULL); } while(0)

#ifdef  __cplusplus
}
#endif

#endif
