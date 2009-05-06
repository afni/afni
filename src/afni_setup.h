/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#ifndef _AFNI_SETUP_HEADER_
#define _AFNI_SETUP_HEADER

#include "mcw_malloc.h"

#define PAL_FIGNORE -9753.0
#define PAL_IIGNORE -1
#define MAX_PALABEL 16

#ifndef MCW_strncpy
#define MCW_strncpy(dest,src,n) \
   ( (void) strncpy( (dest) , (src) , (n)-1 ) , (dest)[(n)-1] = '\0' )
#endif

#define SETUP_INIT_MODE    701
#define SETUP_LATER_MODE   703
#define SETUP_ENVIRON_MODE 707

/*-------------------------------------------------------------------------*/

typedef struct {
   int npane , mode ;
   float val[NPANE_MAX+1] ;
   int   col[NPANE_MAX] ;
} PBAR_palette ;

typedef struct {
   char label[32] ;
   PBAR_palette * psgn[NPANE_MAX+1] ;
   PBAR_palette * ppos[NPANE_MAX+1] ;
} PBAR_palette_array ;

#define INIT_PALARR(name,lab)                      \
 do{ int qp ; (name) = XtNew(PBAR_palette_array) ; \
     MCW_strncpy((name)->label,(lab),32) ;         \
     for( qp=0 ; qp <= NPANE_MAX ; qp++ )          \
        (name)->psgn[qp] = (name)->ppos[qp] = NULL ; } while(0)

typedef struct {
   int num , nall ;
   PBAR_palette_array ** par ;
} PBAR_palette_table ;

#define INC_PALTAB 8

#define INIT_PALTAB(name)                 \
   ( (name) = XtNew(PBAR_palette_table) , \
     (name)->num = (name)->nall = 0 ,     \
     (name)->par = NULL )

#define ADDTO_PALTAB(name,pp)                                       \
 do { if( (name)->num == (name)->nall ){                             \
       (name)->nall += INC_PALTAB ;                                   \
       (name)->par   = (PBAR_palette_array **)                         \
                        XtRealloc( (char *) (name)->par ,               \
                         sizeof(PBAR_palette_array *) * (name)->nall ) ; \
      }                                                                 \
      if( (pp) != NULL ){                                              \
       (name)->par[(name)->num] = (pp) ; ((name)->num)++ ;            \
      } } while(0)

#define PALTAB_NUM(name)            ( (name)->num )
#define PALTAB_ARR(name,qq)         ( (name)->par[(qq)] )
#define PALTAB_ARR_LABEL(name,qq)   ( (name)->par[(qq)]->label )
#define PALTAB_ARR_PSGN(name,qq,ww) ( (name)->par[(qq)]->psgn[(ww)] )
#define PALTAB_ARR_PPOS(name,qq,ww) ( (name)->par[(qq)]->ppos[(ww)] )

#ifdef  __cplusplus
extern "C" {
#endif

extern int label_in_PALTAB( PBAR_palette_table * , char * ) ;
extern void AFNI_process_setup( char * , int , MCW_DC * ) ;
extern int check_PBAR_palette( PBAR_palette * ) ;
extern char * dump_PBAR_palette_table(int) ;
extern void AFNI_pbar_CB( Widget , XtPointer , XtPointer ) ;
extern void AFNI_pbar_EV( Widget , XtPointer , XEvent * , Boolean * ) ;
extern char * AFNI_palette_label_CB( MCW_arrowval * , XtPointer ) ;
extern void AFNI_palette_av_CB( MCW_arrowval * , XtPointer ) ;
extern void load_PBAR_palette_array( MCW_pbar * , PBAR_palette_array * , int ) ;
extern void AFNI_finalize_read_palette_CB( Widget , XtPointer , XtPointer ) ;
extern void AFNI_set_pbar_top_CB( Widget, XtPointer , MCW_choose_cbs * ) ;
extern void AFNI_finalize_write_palette_CB( Widget, XtPointer, MCW_choose_cbs * );
extern void AFNI_finalize_saveim_CB( Widget, XtPointer, MCW_choose_cbs * );
extern void AFNI_palette_tran_CB( MCW_arrowval * , XtPointer ) ;

extern void AFNI_thr_EV( Widget , XtPointer , XEvent * , Boolean * ) ;
extern void AFNI_clu_CB( Widget , XtPointer , XtPointer ) ;

extern void AFNI_thronoff_change_CB( Widget, XtPointer, XtPointer ) ;

extern void set_vedit_cluster_label( Three_D_View *, int ) ; /* 26 Mar 2007 */

extern void AFNI_cluster_dispkill( Three_D_View *im3d ) ;
extern void AFNI_cluster_dispize( Three_D_View *im3d , int force ) ;

#define VEDIT_unhelpize(iq)                                                   \
 do{ MCW_unregister_help((iq)->vwid->func->options_label);                     \
     if( (iq)->vedlabel != NULL ){ free((iq)->vedlabel); (iq)->vedlabel=NULL; } \
 } while(0)

#define VEDIT_clear_label(iq)  \
 do{ set_vedit_cluster_label(iq,0); VEDIT_unhelpize(iq); } while(0)

#define VEDIT_clust_label(iq) set_vedit_cluster_label(iq,1)

#define VEDIT_cluster_helpize(iq)                                         \
 do{ char *hc = mri_clusterize_report(); VEDIT_unhelpize(iq) ;            \
     if( hc != NULL ){                                                    \
       (iq)->vedlabel = strdup(hc);                                       \
       MCW_register_help((iq)->vwid->func->options_label,(iq)->vedlabel); \
     }                                                                    \
 } while(0)

#ifdef  __cplusplus
}
#endif

#endif
