#ifndef _MRILIB_DICOM_STUFF_
#define _MRILIB_DICOM_STUFF_

#include "vecmat.h"

/*-- manufacturer codes --*/

#undef  AFD_MAN_OFFSET
#define AFD_MAN_OFFSET     7532000

#define AFD_MAN_SIEMENS    (AFD_MAN_OFFSET + 1)
#define AFD_MAN_GE         (AFD_MAN_OFFSET + 2)
#define AFD_MAN_PHILIPS    (AFD_MAN_OFFSET + 3)
#define AFD_MAN_TOSHIBA    (AFD_MAN_OFFSET + 4)
#define AFD_MAN_FONAR      (AFD_MAN_OFFSET + 5)
#define AFD_MAN_HITACHI    (AFD_MAN_OFFSET + 6)
#define AFD_MAN_MAGNASERV  (AFD_MAN_OFFSET + 7)
#define AFD_MAN_ODIN       (AFD_MAN_OFFSET + 8)
#define AFD_MAN_ONI        (AFD_MAN_OFFSET + 9)
#define AFD_MAN_BRUKER     (AFD_MAN_OFFSET +10)
#define AFD_MAN_VARIAN     (AFD_MAN_OFFSET +11)

/*-- struct to hold header info from one file --*/

typedef struct {
  int   manufacturer_code ;                 /* from ID  group */
  float tr , slice_spacing , slice_thick ;  /* from ACQ group */
  float pos_xx , pos_yy , pos_zz ,          /* from REL group */
        ori_ix , ori_iy , ori_iz ,
        ori_jx , ori_jy , ori_jz ,
        slice_loc ;
  float di , dj ;                           /* from IMG group */
  int   ni , nj , nk ;
  unsigned data_offset, data_length ;       /* from PXL group */
  int      nbits ;

  float rescale_intercept, rescale_slope,
        window_center    , window_width  ;

  char *filename ;                          /* where 'tis */
  void *extra_info ;                        /* whatever   */
  char  manufacturer_string[128] ;
} AFD_dicom_header ;

typedef struct {
   THD_fvec3 xvec, yvec;            /* Image Orientation fields */
   THD_fvec3 dfpos1;                /* image origin for first two slices*/
   THD_fvec3 dfpos2;
   THD_fvec3 del;                   /* voxel dimensions */
   int mosaic;                      /* data is mosaic */
   int mos_ix, mos_nx, mos_ny, mos_nslice; /* mosaic properties */
   int nx, ny;                      /* overall mosaic dimensions */
   float Tr_dicom[4][4];            /* transformation matrix */
   float slice_xyz[2][3];           /* coordinates for 1st and last slices */
   int mos_sliceinfo;               /* flag for existence of coordinate info */
   int flip_slices;
} oblique_info;

extern oblique_info obl_info;

/*-- global processing structs (image fields and env vars) --*/
typedef struct {
   int     study, series, image, image_index; /* DICOM image values  */
   float   acq_time;                    /* acquisition time, if set  */
   float   slice_loc;                   /* 0020 1041 REL Slice Loc   */
   int     is_obl, is_mosaic;           /* oblique flag, mosaic flag */
   int     mos_nslice, mos_nx, mos_ny;  /* mosaic dimensions         */
} dicom_image_globals_t;
extern dicom_image_globals_t g_image_info;

/* persistent globals (not per image) to control processing */
typedef struct {
   int     init;                /* have the vars been initialized?       */
   int     read_data;           /* flag: do we read data at all?      */
   int     verb;                /* AFNI_DICOM_VERBOSE               */
   int     rescale;             /* AFNI_DICOM_RESCALE              */
   int     window;              /* AFNI_DICOM_WINDOW              */
   int     use_last_elem;       /* AFNI_DICOM_USE_LAST_ELEMENT   */
} dicom_globals_t;
extern dicom_globals_t g_dicom_ctrl;

/*-- extra_info from Siemens --*/

#define AFD_EIT_SIEMENS AFD_MAN_SIEMENS

typedef struct {
  int eitype ;          /* type code for this extra info */
  int mosaic_num    ,   /* number of sub-images actually stored in mosaic */
      mos_ix,mos_iy ,   /* number of sub-images along each edge of mosaic */
      mos_nx,mos_ny ,   /* dimensions of mosaic sub-images                */
      mos_nz         ;  /* overall size of mosaic = mos_ix * mos_iy       */

  float auto_align[16]; /* auto align matrix, if [15] == 1 */

  int    nslice       ; /* number of slices we have info about these arrays */
  float *position_sag ;
  float *position_cor ;
  float *position_tra ;
  float *normal_sag   ;
  float *normal_cor   ;
  float *normal_tra   ;
  float *inplane_rot  ;
} AFD_siemens_info ;

/*------ stuff for multi-frame collections of 2D images [05 May 2008] ------*/

typedef struct {
  int nframe ;                      /* number of frames */
  int *time_index , *stack_index ;  /* nframe of each array */
  float *xpos , *ypos , *zpos ;     /* might be NULL */
} MultiFrame_info ;

#define INIT_MultiFrame(mf,n)                                    \
 do{ (mf) = (MultiFrame_info *)malloc(sizeof(MultiFrame_info));  \
     (mf)->nframe      = (n) ;                                   \
     (mf)->time_index  = (int *)  calloc(sizeof(int)  ,(n)) ;    \
     (mf)->stack_index = (int *)  calloc(sizeof(int)  ,(n)) ;    \
     (mf)->xpos        = (float *)calloc(sizeof(float),(n)) ;    \
     (mf)->ypos        = (float *)calloc(sizeof(float),(n)) ;    \
     (mf)->zpos        = (float *)calloc(sizeof(float),(n)) ;    \
 } while(0)

#define KILL_MultiFrame(mf)                                      \
 do{ if( (mf) != NULL ){                                         \
       if( (mf)->time_index  ) free((void *)(mf)->time_index) ;  \
       if( (mf)->stack_index ) free((void *)(mf)->stack_index) ; \
       if( (mf)->xpos        ) free((void *)(mf)->xpos) ;        \
       if( (mf)->ypos        ) free((void *)(mf)->ypos) ;        \
       if( (mf)->zpos        ) free((void *)(mf)->zpos) ;        \
       free((void *)(mf)) ; (mf) = NULL ;                        \
     }                                                           \
 } while(0)

#define DELPOS_MultiFrame(mf)                                           \
 do{ if( (mf) != NULL ){                                                \
       if( (mf)->xpos ){ free((void *)(mf)->xpos); (mf)->xpos = NULL; } \
       if( (mf)->ypos ){ free((void *)(mf)->ypos); (mf)->ypos = NULL; } \
       if( (mf)->zpos ){ free((void *)(mf)->zpos); (mf)->xpos = NULL; } \
     }                                                                  \
 } while(0)

/*-- prototypes --*/

extern char *AFD_manufacturer_code_to_string( int code ) ;
extern void AFD_siemens_info_free( void *aei ) ;
extern void AFD_dicom_header_free( AFD_dicom_header *adh ) ;
extern int AFD_manufacturer_string_to_code( char * ) ;
extern AFD_dicom_header * AFD_scanfor_header( char * ) ;
extern char * AFD_format_header( AFD_dicom_header *adh ) ;
extern int disp_dicom_globals(char * mesg);

extern MultiFrame_info * AFD_scanfor_MultiFrame( char *ppp ) ;

#endif /* _MRILIB_DICOM_STUFF_ */
