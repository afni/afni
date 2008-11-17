/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#ifndef _AFNI_PLUGIN_HEADER_
#define _AFNI_PLUGIN_HEADER_

/*-----------------------------------------------------------------
  Most of this file is included only if ALLOW_PLUGINS is defined,
  but a little at the end is always used.
-------------------------------------------------------------------*/

#define PLUTO_X11_display   (GLOBAL_library.dc->display)
#define PLUTO_Xt_appcontext (GLOBAL_library.dc->appcontext)

#ifdef ALLOW_PLUGINS

#if defined(__cplusplus) || defined(c_plusplus)
# define DEFINE_PLUGIN_PROTOTYPE \
  extern "C" { PLUGIN_interface * PLUGIN_init( int ncall ) ; }
#else
# define DEFINE_PLUGIN_PROTOTYPE
#endif


#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <sys/time.h>
#include <sys/errno.h>
#include <sys/times.h>
#include <limits.h>

struct AFNI_plugin_array ; /* incomplete definition */

#include "afni.h"

#include <Xm/XmAll.h>

/*******************************************************************
   Define macros and typedefs for opening, closing, and finding
   symbols from dynamic libraries.  This is not done the same
   way on all Unixoid systems, unfortunately (that is to say,
   HP-UX is different).
*******************************************************************/

#ifdef __cplusplus
extern "C" {
#endif

typedef int int_func() ;     /* generic function returning integer */
typedef void * vptr_func() ; /* generic function returning void *  */
typedef char * cptr_func() ; /* generic function returning char *  */

#ifdef __cplusplus
}
#endif

/***************** The dlfcn.h and dl library ****************/

#ifdef DYNAMIC_LOADING_VIA_DL

#ifndef DARWIN
#  include <dlfcn.h>
#else
#  include "dlcompat/dlfcn.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

   typedef void * DYNAMIC_handle ;

#  define ISVALID_DYNAMIC_handle(handle) ((handle) != (DYNAMIC_handle) 0)

#  define DYNAMIC_OPEN(libname,handle) \
      (handle) = dlopen( (libname) , RTLD_LAZY )

#  define DYNAMIC_ERROR_STRING  dlerror()  /* 18 May 2001 */

#  define DYNAMIC_CLOSE(handle) \
      (void) dlclose( (handle) )

#  define DYNAMIC_SYMBOL(handle,symbol,address) \
      (address) = dlsym( (handle) , (symbol) )

#  define DYNAMIC_suffix ".so"
#ifdef __cplusplus
}
#endif

#endif

/****************** The dl.h and dld library ******************/

#ifdef DYNAMIC_LOADING_VIA_SHL
#  include <dl.h>
#  include <errno.h>  /* 18 May 2001 */

#ifdef __cplusplus
extern "C" {
#endif
   typedef shl_t DYNAMIC_handle ;

#  define ISVALID_DYNAMIC_handle(handle) ((handle) != (DYNAMIC_handle) 0)

#  define DYNAMIC_OPEN(libname,handle) \
      (handle) = shl_load( (libname) , BIND_DEFERRED , 0L )

#  define DYNAMIC_ERROR_STRING strerror(errno) /* 18 May 2001 */

#  define DYNAMIC_CLOSE(handle) \
      (void) shl_unload( (handle) )

#  define DYNAMIC_SYMBOL(handle,symbol,address)      \
      do{ (address) = NULL ;                         \
          (void) shl_findsym( &(handle) , (symbol) , \
                              TYPE_UNDEFINED , &(address) ) ; } while(0)

#  define DYNAMIC_suffix ".sl"
#ifdef __cplusplus
}
#endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

#ifdef NO_DYNAMIC_LOADING             /* this stuff is not actually used,  */
#  define DYNAMIC_suffix ".fixed"     /* but is needed to make things cool */
   typedef int DYNAMIC_handle ;       /* with the C compiler               */
#endif

#ifndef DYNAMIC_suffix
#  error "Plugins not properly set up -- see machdep.h"
#endif

/*****************************************************************
   Data to define the interface between a plugin and AFNI
******************************************************************/

/* dimensions of various arrays */

#define PLUGIN_LABEL_SIZE           32  /* for buttons and menus */
#define PLUGIN_STRING_SIZE          64  /* longer things */

#if 1
#define PLUGIN_MAX_STRING_RANGE     99  /* isn't this enough? */
#else
#define PLUGIN_MAX_STRING_RANGE     34  /* isn't this enough? */
#endif

#define PLUGIN_MAX_SUBVALUES         7  /* isn't this enough? */
                                        /*  -- Nope --  03 May 2002 [BDWard] */

/* data type codes (not all are implemented yet!) */

#define PLUGIN_NOTHING_TYPE          0
#define PLUGIN_NUMBER_TYPE           1  /* implemented */
#define PLUGIN_STRING_TYPE           2  /* implemented */
#define PLUGIN_DATASET_TYPE          3  /* implemented */
#define PLUGIN_DATASET_LIST_TYPE     4
#define PLUGIN_TIMESERIES_TYPE       5  /* implemented */
#define PLUGIN_TIMESERIES_LIST_TYPE  6
#define PLUGIN_2DIMAGE_TYPE          7
#define PLUGIN_2DIMAGE_LIST_TYPE     8
#define PLUGIN_3DIMAGE_TYPE          9
#define PLUGIN_3DIMAGE_LIST_TYPE    10
#define PLUGIN_4DIMAGE_TYPE         11
#define PLUGIN_4DIMAGE_LIST_TYPE    12
#define PLUGIN_OVERLAY_COLOR_TYPE   13

/** macro to copy string into plugin label array,
    filling with blanks or truncating length, as needed **/

#define PLUGIN_LABEL_strcpy(plab,str)                                \
   do{ int ll=strlen((str)) , ii ;                                   \
       if( ll >= PLUGIN_LABEL_SIZE ) ll = PLUGIN_LABEL_SIZE - 1 ;    \
       for( ii=0 ; ii < ll ; ii++ ) (plab)[ii] = (str)[ii] ;         \
       for( ; ii < PLUGIN_LABEL_SIZE - 1 ; ii++ ) (plab)[ii] = ' ' ; \
       plab[PLUGIN_LABEL_SIZE - 1] = '\0' ; } while(0)

/** prototype of routine to compute length
    of string, not counting blanks at the end **/

extern int PLUG_nonblank_len(char *) ;

/******* typedef to hold data describing
         each subvalue that an option requires *******/

typedef struct {
   int data_type ;  /* one of the PLUGIN_*_TYPE codes above */

   char label[PLUGIN_LABEL_SIZE] ;  /* label for AFNI to display */
   char *hint ;

   /** values describing the range of acceptable inputs **/

   int int_range_bot , int_range_top , int_range_decim ; /* for NUMBER */

   int   string_range_count ;                           /* for STRING */
   char *string_range[PLUGIN_MAX_STRING_RANGE] ;

   int dset_anat_mask , dset_func_mask , dset_ctrl_mask ; /* for DATASET */

   /** the default value to set up the interface with **/

   int value_default ;  /* for NUMBER or STRING */

   /** values describing other properties of the input **/

   int editable ;  /* can the user type into this? */

} PLUGIN_subvalue ;

/****** typedef to hold data describing
        each option input line to a plugin ******/

typedef struct {
   char label[PLUGIN_LABEL_SIZE] ;  /* label for AFNI to display */
   char tag[PLUGIN_STRING_SIZE] ;   /* passed to plugin */
   char * hint ;

   int subvalue_count ;                              /* number of subvalues */
   PLUGIN_subvalue subvalue[PLUGIN_MAX_SUBVALUES] ;  /* subvalue descriptors */

   int mandatory ;  /* required by law? */

   int    chosen ;
   void * callvalue[PLUGIN_MAX_SUBVALUES] ; /* actual values given to plugin */
} PLUGIN_option ;

/******* typedef to describe the option-level
         widgets for the interface to a plugin *******/

#define OP_CHOOSER_NONE       0   /* nothing                      */
#define OP_CHOOSER_DSET       1   /* PushButton                   */
#define OP_CHOOSER_OPTMENU    2   /* optmenu type of MCW_arrowval */
#define OP_CHOOSER_STRING     3   /* string type of MCW_arrowval  */
#define OP_CHOOSER_NUMBER     4   /* number type of MCW_arrowval  */
#define OP_CHOOSER_TEXTFIELD  5   /* TextField                    */
#define OP_CHOOSER_TIMESERIES 6   /* PushButton                   */
#define OP_CHOOSER_COLORMENU  7   /* optmenu for overlay colors   */

#define OP_OPTMENU_LIMIT     99
#define OP_OPTMENU_COLSIZE   20

typedef struct {
   Widget rowcol , label , textf ;
} PLUGIN_strval ;

typedef struct {
   char       title[THD_MAX_NAME] ;
   MCW_idcode idcode ;
} PLUGIN_dataset_link ;

extern void make_PLUGIN_dataset_link( THD_3dim_dataset *, PLUGIN_dataset_link * ) ;
extern void patch_PLUGIN_dataset_links( int , PLUGIN_dataset_link * ) ;

typedef struct {
   Widget rowcol , label , pb ;

   int                   dset_count ;  /* number of datasets to display */
   PLUGIN_dataset_link * dset_link ;   /* info about them */
   PLUGIN_subvalue     * sv ;          /* my good friend */

   int dset_choice ;                   /* the chosen index */

   int multi , nchosen , * chosen ;    /* 24 Nov 1996: for dataset lists */
   int current ;
   MCW_idcode * idclist ;
} PLUGIN_dsetval ;

typedef PLUGIN_dsetval MCW_idclist ;

typedef struct {
   Widget rowcol , label , pb ;

   MRI_IMARR       * tsimar ;    /* array of time series to choose from */
   PLUGIN_subvalue * sv ;        /* my good friend */

   MRI_IMAGE * tsim ;            /* the chosen timeseries */
   int         ts_choice ;       /* the chosen index */
} PLUGIN_tsval ;

typedef struct {
   Widget toggle , label ;
   void * chooser[PLUGIN_MAX_SUBVALUES] ;
   Widget chtop[PLUGIN_MAX_SUBVALUES] ;
   int    chooser_type[PLUGIN_MAX_SUBVALUES] ;
} PLUGIN_option_widgets ;

/******* typedef to describe the top-level
         widgets for the interface to a plugin *******/

typedef struct {
   Widget shell , form , label , scrollw , workwin ;
   PLUGIN_option_widgets ** opwid ;
   Widget meter ;
} PLUGIN_widgets ;

/******* typedef to describe
         the interface to a plugin *******/

#define PLUGIN_CALL_IMMEDIATELY  77
#define PLUGIN_CALL_VIA_MENU     88
#define PLUGIN_CALL_VIA_CUSTOM   99

typedef struct PLUGIN_interface {
   char label[PLUGIN_LABEL_SIZE] ;         /* for a button */
   char description[PLUGIN_STRING_SIZE] ;  /* for the interface panel */
   char * helpstring ;                     /* from the user? */
   char * hint ;

   int         call_method ;  /* one of the PLUGIN_CALL_* codes above */
   cptr_func * call_func ;    /* function to call */

   int option_count ;         /* number of option lines */
   PLUGIN_option ** option ;  /* array of option line descriptors */
   PLUGIN_widgets * wid ;     /* toplevel widgets for this interface */

   Three_D_View * im3d ;      /* who called me up? */

   int opnum , svnum ;        /* used during get_*_from readout */

   char seqcode[PLUGIN_STRING_SIZE] ;  /* 06 Aug 1999 */
   char butcolor[PLUGIN_STRING_SIZE] ; /* 01 Nov 1999 */

   int  flags ;                        /* 29 Mar 2002 */

   char run_label [PLUGIN_LABEL_SIZE] ; /* 04 Nov 2003 */
   char doit_label[PLUGIN_LABEL_SIZE] ;
} PLUGIN_interface ;

#define SHORT_CHOOSE_FLAG 1
#define SHORT_NUMBER_FLAG 2

#define PLUTO_short_choose(pl) (pl->flags |= SHORT_CHOOSE_FLAG)
#define PLUTO_short_number(pl) (pl->flags |= SHORT_NUMBER_FLAG)

/*************** Prototypes for creation of the above structures ***************/

#define SESSION_ALL_MASK      (1<<0)

#define ANAT_NONE_MASK        0
#define FUNC_NONE_MASK        0

#define DIMEN_3D_MASK         (1<<1)
#define DIMEN_4D_MASK         (1<<2)
#define DIMEN_ALL_MASK        (DIMEN_3D_MASK | DIMEN_4D_MASK)

#define WARP_ON_DEMAND_MASK   (1<<3)

#define BRICK_BYTE_MASK       (1<<8)
#define BRICK_SHORT_MASK      (1<<9)
#define BRICK_FLOAT_MASK      (1<<10)
#define BRICK_COMPLEX_MASK    (1<<11)
#define BRICK_RGB_MASK        (1<<12)
#define BRICK_ALLTYPE_MASK    ( BRICK_BYTE_MASK  | BRICK_SHORT_MASK   |  \
                                BRICK_FLOAT_MASK | BRICK_COMPLEX_MASK |  \
                                BRICK_RGB_MASK                         )

#define BRICK_ALLREAL_MASK    ( BRICK_BYTE_MASK  | BRICK_SHORT_MASK |    \
                                BRICK_FLOAT_MASK )

extern int PLUGIN_dset_check( int,int    , THD_3dim_dataset * ) ;
extern int PLUTO_dset_check ( int,int,int, THD_3dim_dataset * ) ;

#define PLUTO_add_option         add_option_to_PLUGIN_interface
#define PLUTO_add_number         add_number_to_PLUGIN_interface
#define PLUTO_add_string         add_string_to_PLUGIN_interface
#define PLUTO_add_dataset        add_dataset_to_PLUGIN_interface
#define PLUTO_add_timeseries     add_timeseries_to_PLUGIN_interface
#define PLUTO_add_dataset_list   add_dataset_list_to_PLUGIN_interface
#define PLUTO_add_overlaycolor   add_overlaycolor_to_PLUGIN_interface

#define PLUTO_register_environment_numeric ENV_add_numeric     /* 20 Jun 2000 */
#define PLUTO_register_environment_string  ENV_add_string
#define PLUTO_register_environment_yesno   ENV_add_yesno       /* 08 Aug 2001 */

extern void PLUTO_add_hint( PLUGIN_interface * , char * ) ;

extern void PLUTO_set_sequence( PLUGIN_interface *, char * ) ; /* 06 Aug 1999 */
extern void PLUTO_set_butcolor( PLUGIN_interface *, char * ) ; /* 01 Nov 1999 */

/* 15 Jun 1999: redo PLUTO_new_interface */

#define PLUTO_new_interface(a,b,c,d,e) new_PLUGIN_interface_1999(a,b,c,d,e,__DATE__)

extern PLUGIN_interface * new_PLUGIN_interface( char *, char *, char *,
                                                int, cptr_func * ) ;

extern PLUGIN_interface * new_PLUGIN_interface_1999( char *, char *, char *,
                                                     int, cptr_func * , char * ) ;

void PLUTO_set_runlabels( PLUGIN_interface *, char *, char * ) ; /* 04 Nov 2003 */

extern void add_option_to_PLUGIN_interface( PLUGIN_interface *,
                                            char *, char *, int ) ;

extern void add_number_to_PLUGIN_interface( PLUGIN_interface *, char *,
                                            int, int, int, int, int ) ;

extern void add_string_to_PLUGIN_interface( PLUGIN_interface *,
                                            char *, int, char **, int) ;

extern void add_dataset_to_PLUGIN_interface( PLUGIN_interface *,
                                             char *, int,int,int ) ;

extern void add_dataset_list_to_PLUGIN_interface( PLUGIN_interface *,
                                                  char *, int,int,int ) ;

extern void add_timeseries_to_PLUGIN_interface( PLUGIN_interface *, char * ) ;

extern void add_overlaycolor_to_PLUGIN_interface( PLUGIN_interface *, char * );

extern void PLUTO_set_initcolorindex(int) ; /* 10 Oct 2007 */

extern void PLUG_fillin_values( PLUGIN_interface * plint ) ;
extern void PLUG_freeup_values( PLUGIN_interface * plint ) ;

extern char * PLUTO_commandstring( PLUGIN_interface * plint ) ;

#define PLUTO_get_label        get_label_from_PLUGIN_interface
#define PLUTO_get_descripton   get_description_from_PLUGIN_interface
#define PLUTO_get_optiontag    get_optiontag_from_PLUGIN_interface
#define PLUTO_get_callvalue    get_callvalue_from_PLUGIN_interface
#define PLUTO_get_number       get_number_from_PLUGIN_interface
#define PLUTO_get_string       get_string_from_PLUGIN_interface
#define PLUTO_get_idcode       get_idcode_from_PLUGIN_interface
#define PLUTO_get_timeseries   get_timeseries_from_PLUGIN_interface
#define PLUTO_peek_callvalue   peek_callvalue_type_from_PLUGIN_interface
#define PLUTO_peek_optiontag   peek_optiontag_from_PLUGIN_interface
#define PLUTO_get_idclist      get_idclist_from_PLUGIN_interface
#define PLUTO_get_overlaycolor get_overlaycolor_from_PLUGIN_interface

#define PLUTO_idclist_count(ll) ( ((ll) != NULL) ? (ll)->nchosen : 0 )
#define PLUTO_idclist_reset(ll) ((ll)->current = 0)
#define PLUTO_idclist_next(ll)  (((ll)->current < (ll)->nchosen)     \
                                 ? ((ll)->idclist+((ll)->current)++) : NULL)

extern char * get_label_from_PLUGIN_interface       ( PLUGIN_interface * ) ;
extern char * get_description_from_PLUGIN_interface ( PLUGIN_interface * ) ;
extern char * get_optiontag_from_PLUGIN_interface   ( PLUGIN_interface * ) ;
extern void * get_callvalue_from_PLUGIN_interface   ( PLUGIN_interface * , int ) ;
extern float  get_number_from_PLUGIN_interface      ( PLUGIN_interface * ) ;
extern char * get_string_from_PLUGIN_interface      ( PLUGIN_interface * ) ;
extern int    get_overlaycolor_from_PLUGIN_interface( PLUGIN_interface * ) ;

extern MCW_idcode * get_idcode_from_PLUGIN_interface( PLUGIN_interface * ) ;
extern MRI_IMAGE * get_timeseries_from_PLUGIN_interface( PLUGIN_interface * ) ;
extern MCW_idclist * get_idclist_from_PLUGIN_interface( PLUGIN_interface * ) ;

extern int    peek_callvalue_type_from_PLUGIN_interface( PLUGIN_interface * ) ;
extern char * peek_optiontag_from_PLUGIN_interface     ( PLUGIN_interface * ) ;

#define NEXT_PLUGIN_OPTION(pl) (void)get_optiontag_from_PLUGIN_interface((pl))
#define NEXT_OPTION            NEXT_PLUGIN_OPTION
#define PLUTO_next_option      NEXT_PLUGIN_OPTION

#define BAD_NUMBER        (-31416.666)
#define PLUTO_BAD_NUMBER  BAD_NUMBER

/**************************************************************************/
/***** Define data structures to hold control information for plugins *****/

#define AFNI_PLUGIN_TYPE        9754
#define ISVALID_AFNI_PLUGIN(pl) ((pl)!=NULL && (pl)->type==AFNI_PLUGIN_TYPE)

#define MAX_PLUGIN_NAME 128

/*** one plugin ***/

typedef struct {
   int type ;     /* identifier */

   char           libname[MAX_PLUGIN_NAME] ;
   DYNAMIC_handle libhandle ;
   vptr_func    * libinit_func ;

   int                 interface_count ;
   PLUGIN_interface ** interface ;

   char seqcode[PLUGIN_STRING_SIZE] ;  /* 06 Aug 1999 */
} AFNI_plugin ;

/*** dynamic array of many plugins ***/

typedef struct AFNI_plugin_array {
   int num , nall ;
   AFNI_plugin ** plar ;
} AFNI_plugin_array ;

/*** macros to create, add to, destroy, and free an array of plugins ***/

#define INC_PLUGIN_ARRAY 8

/** "name" is a variable of type (AFNI_plugin_array *) **/

#define INIT_PLUGIN_ARRAY(name)                                                     \
   do{ int iq ;                                                                     \
       (name)       = (AFNI_plugin_array *) malloc(sizeof(AFNI_plugin_array)) ;     \
       (name)->num  = 0 ;                                                           \
       (name)->nall = INC_PLUGIN_ARRAY ;                                            \
       (name)->plar = (AFNI_plugin **)malloc(sizeof(AFNI_plugin*)*(name)->nall) ;   \
       for( iq=(name)->num ; iq < (name)->nall ; iq++ ) (name)->plar[iq] = NULL ;   \
     } while(0)

/** "plug" is a variable of type (AFNI_plugin *) **/

#define ADDTO_PLUGIN_ARRAY(name,plug)                                                 \
   do{ int nn , iq ;                                                                  \
       if( (name)->num == (name)->nall ){                                             \
          nn = (name)->nall = 1.1*(name)->nall + INC_PLUGIN_ARRAY ;                   \
          (name)->plar      = (AFNI_plugin **)                                        \
                               realloc( (name)->plar,sizeof(AFNI_plugin *)*nn ) ;     \
          for( iq=(name)->num ; iq < (name)->nall ; iq++ ) (name)->plar[iq] = NULL ;} \
       nn = (name)->num ; ((name)->num)++ ;                                           \
       (name)->plar[nn] = (plug) ;                                                    \
     } while(0)

/** this frees all the memory associated with this array **/

#define DESTROY_PLUGIN_ARRAY(name)                                      \
   do{ int nn ;                                                         \
       if( (name) != NULL ){                                            \
          for( nn=0 ; nn < (name)->num ; nn++ )                         \
             if( (name)->plar[nn] != NULL ) free( (name)->plar[nn] ) ;  \
          free( (name)->plar ) ; free((name)) ; (name) = NULL ;         \
       } } while(0)

/** this just frees the control data associated
    with this array -- the actual plugins are not freed. **/

#define FREE_PLUGIN_ARRAY(name)                                         \
   do{ int nn ;                                                         \
       if( (name) != NULL ){                                            \
          free( (name)->plar ) ; free((name)) ; (name) = NULL ;         \
       } } while(0)

/*********************************************************************************/

/***** Other prototypes *****/

extern AFNI_plugin_array * PLUG_get_all_plugins( char * dname ) ;
extern AFNI_plugin *       PLUG_read_plugin( char * fname ) ;
extern AFNI_plugin_array * PLUG_get_many_plugins(char *) ;

extern void PLUG_setup_widgets( PLUGIN_interface *, MCW_DC * ) ;

extern void PLUG_action_CB           ( Widget , XtPointer , XtPointer ) ;
extern void PLUG_delete_window_CB    ( Widget , XtPointer , XtPointer ) ;
extern void PLUG_optional_toggle_CB  ( Widget , XtPointer , XtPointer ) ;
extern void PLUG_choose_dataset_CB   ( Widget , XtPointer , XtPointer ) ;
extern void PLUG_startup_plugin_CB   ( Widget , XtPointer , XtPointer ) ;
extern void PLUG_choose_timeseries_CB( Widget , XtPointer , XtPointer ) ;

extern void PLUTO_turnoff_options( PLUGIN_interface * ) ; /* 21 Feb 2001 */

extern void PLUG_finalize_dataset_CB   (Widget, XtPointer, MCW_choose_cbs *);
extern void PLUG_finalize_timeseries_CB(Widget, XtPointer, MCW_choose_cbs *);

extern void PLUTO_popup_dset_chooser( Widget, int, int,
                                      int_func *, void_func *, void * ) ;

extern void PLUG_finalize_user_dset_CB( Widget, XtPointer, MCW_choose_cbs * ) ;

extern void AFNI_plugin_button( Three_D_View * ) ;

#define DSET_ACTION_NONE           0
#define DSET_ACTION_MAKE_CURRENT   1

extern int PLUTO_add_dset( PLUGIN_interface *, THD_3dim_dataset *, int ) ;

extern THD_3dim_dataset * PLUTO_copy_dset( THD_3dim_dataset *, char * ) ;

extern void PLUTO_dset_redisplay_mode( THD_3dim_dataset * , int ) ;
extern void PLUTO_dset_redisplay( THD_3dim_dataset * ) ;

extern int PLUTO_prefix_ok( char * ) ;
extern int PLUTO_string_index( char * , int , char ** ) ;

#define PLUTO_popup_message(pl,ch)   \
   PLUTO_popup_worker((pl),(ch),MCW_USER_KILL)

#define PLUTO_popup_transient(pl,ch) \
   PLUTO_popup_worker((pl),(ch),MCW_USER_KILL|MCW_TIMER_KILL);

#define PLUTO_popup_textwin(pl,ch)   \
   PLUTO_popup_worker((pl),(ch),-1)

extern void PLUTO_fixup_names(void) ;
extern void PLUTO_popup_worker( PLUGIN_interface * , char * , int ) ;
extern void PLUTO_beep(void) ;

extern void PLUTO_popup_meter( PLUGIN_interface * ) ;
extern void PLUTO_popdown_meter( PLUGIN_interface * ) ;
extern void PLUTO_set_meter( PLUGIN_interface * , int ) ;

extern void PLUTO_set_topshell( PLUGIN_interface *, Widget ) ; /* 22 Sep 2000 */

/*------------------------------------------------------------------------*/

typedef struct {
   MCW_imseq * seq ;
   MRI_IMAGE * im ;
} PLUGIN_impopper ;

extern void *    PLUTO_popup_image( void * , MRI_IMAGE * ) ;
extern XtPointer PLUGIN_imseq_getim( int , int , XtPointer ) ;
extern void      PLUGIN_seq_send_CB( MCW_imseq * , XtPointer , ISQ_cbs * ) ;

#define PLUTO_popdown_image(hh) (void)PLUTO_popup_image((hh),NULL)

#define PLUTO_popup_open(hh) \
   ( (hh) != NULL && ISQ_REALZ(((PLUGIN_impopper *)(hh))->seq) )

#define PLUTO_popkill_image(hh) \
   ( PLUTO_popdown_image(hh) , XtFree((char *)(hh)) , (hh)=NULL )

/*------------------------------------------------------------------------*/

typedef struct {
   MCW_imseq * seq ;
   MRI_IMARR * imar ;
   int rgb_count ;
   generic_func * kill_func ;
   void * kill_data ;
} PLUGIN_imseq ;

extern void * PLUTO_imseq_popup( MRI_IMARR *, generic_func *, void * ) ;
extern void * PLUTO_imseq_popim( MRI_IMAGE *, generic_func *, void * ) ;

extern void   PLUTO_imseq_addto( void * , MRI_IMAGE * ) ;
extern void   PLUTO_imseq_destroy( void * ) ;
extern void   PLUTO_imseq_retitle( void * , char * ) ;
extern void   PLUTO_imseq_rekill( void *, generic_func *, void * ) ;
extern void   PLUTO_imseq_setim( void * , int ) ;  /* 17 Dec 2004 */

extern XtPointer PLUTO_imseq_getim( int , int , XtPointer ) ;
extern void PLUTO_imseq_send_CB( MCW_imseq * , XtPointer , ISQ_cbs * ) ;

/*------------------------------------------------------------------------*/

extern THD_3dim_dataset * PLUTO_4D_to_typed_fim( THD_3dim_dataset * old_dset ,
                                                 char * new_prefix , int new_datum ,
                                                 int ignore , int detrend ,
                                                 generic_func * user_func ,
                                                 void * user_data ) ;

#define PLUTO_4D_to_fim(ds,np,ig,dtr,uf,ud) \
  PLUTO_4D_to_typed_fim( (ds),(np), MRI_short , (ig),(dtr),(uf),(ud) )

/* BDW, 24 Feb 1997 */
extern THD_3dim_dataset * PLUTO_4D_to_typed_fith( THD_3dim_dataset * old_dset ,
						  char * new_prefix , int new_datum ,
						  int ignore , int detrend ,
						  generic_func * user_func ,
						  void * user_data ) ;

#define PLUTO_4D_to_fith(ds,np,ig,dtr,uf,ud) \
  PLUTO_4D_to_typed_fith( (ds),(np), MRI_short , (ig),(dtr),(uf),(ud) )

/* RWC, 13 Dec 1997 */

extern THD_3dim_dataset * PLUTO_4D_to_typed_fbuc( THD_3dim_dataset * old_dset ,
                                                  char * new_prefix , int new_datum ,
                                                  int ignore , int detrend ,
                                                  int nbrik ,
                                                  generic_func * user_func ,
                                                  void * user_data ) ;

extern void PLUTO_report( PLUGIN_interface * , char * ) ;

#define PLUTO_output_header(ds) THD_write_3dim_dataset(NULL,NULL,(ds),False)

extern PLUGIN_strval * new_PLUGIN_strval( Widget , char * ) ;
extern void destroy_PLUGIN_strval( PLUGIN_strval * ) ;
extern void alter_PLUGIN_strval_width( PLUGIN_strval * , int ) ;
extern void set_PLUGIN_strval( PLUGIN_strval * , char * ) ;
extern char * get_PLUGIN_strval( PLUGIN_strval * ) ;

/* for vol2surf plugin                                09 Sep 2004 [rickr] */
extern int PLUTO_set_v2s_addrs(void ** vopt, char *** maps, char ** hist);


#endif /* ALLOW_PLUGINS */

/*--------------------------------------------------------------------
  Stuff that is always defined
----------------------------------------------------------------------*/

#define PLUTO_extract_series(ijk,ds)     THD_extract_series((ijk),(ds),0)
#define PLUTO_extract_series_raw(ijk,ds) THD_extract_series((ijk),(ds),1)

#define PLUTO_register_0D_function  AFNI_register_0D_function
#define PLUTO_register_1D_function  AFNI_register_1D_function
#define PLUTO_register_2D_function  AFNI_register_2D_function

#define PLUTO_register_1D_funcstr   AFNI_register_1D_funcstr

#define PLUTO_cursorize(w)  NORMAL_cursorize(w)

extern void PLUTO_register_timeseries( char * , MRI_IMAGE * ) ;

extern THD_3dim_dataset * PLUTO_find_dset( MCW_idcode * ) ;
extern THD_3dim_dataset * PLUTO_find_dset_idc( char * ) ;
extern THD_slist_find     PLUTO_dset_finder( char * ) ;

extern void PLUTO_histoplot( int, float, float, int *,
                             char *, char *, char * , int,int ** ) ;
extern void PLUTO_histoplot_f( int, float, float, float *,
                             char *, char *, char * , int,float ** ) ;

extern void PLUTO_scatterplot( int , float *, float *,
                               char *, char *, char * , float,float ) ;

extern void PLUTO_force_redisplay( void ) ;
extern void PLUTO_force_rebar( void ) ;

extern void PLUTO_register_workproc( XtWorkProc , XtPointer ) ;
extern void PLUTO_remove_workproc  ( XtWorkProc ) ;
extern Boolean PLUG_workprocess( XtPointer ) ;
extern void PLUTO_register_timeout( int, generic_func *, XtPointer ) ;
extern double PLUTO_cpu_time(void) ;
extern double PLUTO_elapsed_time(void) ;

#ifdef __cplusplus
}
#endif

#endif /* _AFNI_PLUGIN_HEADER_ */
