/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#ifndef _AFNI_WARP_HEADER_
#define _AFNI_WARP_HEADER_

#include "mrilib.h"

/** external routine for freeing memory in THD_load_datablock **/

extern MRI_IMAGE * AFNI_dataset_slice( THD_3dim_dataset * , int,int,int,int ) ;
extern MRI_IMAGE * FD_warp_to_mri( int,int , FD_brick * ) ;

extern MRI_IMAGE * AFNI_slice_flip(int,int,int,int,int,int, THD_3dim_dataset *);

/***********************************************************************/
/*  Prototypes for the template routines in afni_slice.c               */

/* macro to test data type for OK-ness with slicing and dicing routines */

#define AFNI_GOOD_DTYPE(dt) ((dt)==MRI_short || (dt)==MRI_float   || \
                             (dt)==MRI_byte  || (dt)==MRI_complex || \
                             (dt)==MRI_rgb                            )

/* macro to test data type for OK-ness with functional processing routines */

#define AFNI_GOOD_FUNC_DTYPE(dt) ((dt)==MRI_short || (dt)==MRI_rgb || \
                                  (dt)==MRI_float || (dt)==MRI_byte    )

/*---------------------------------------------------------------------*/
/* There should be one set of routines for each legal datum type above */
/*---------------------------------------------------------------------*/
#undef  WTYPE
#undef  LMAP_XNAME
#undef  LMAP_YNAME
#undef  LMAP_ZNAME
#undef  B2SL_NAME
#define WTYPE short
#define LMAP_XNAME TWO_TWO(AFNI_lmap_to_xslice_,WTYPE)
#define LMAP_YNAME TWO_TWO(AFNI_lmap_to_yslice_,WTYPE)
#define LMAP_ZNAME TWO_TWO(AFNI_lmap_to_zslice_,WTYPE)
#define B2SL_NAME  TWO_TWO(AFNI_br2sl_,WTYPE)

extern void LMAP_XNAME( THD_linear_mapping * , int ,
                        THD_dataxes * , WTYPE * ,
                        THD_dataxes * new_daxes , int , WTYPE * ) ;

extern void LMAP_YNAME( THD_linear_mapping * , int ,
                        THD_dataxes * , WTYPE * ,
                        THD_dataxes * new_daxes , int , WTYPE * ) ;

extern void LMAP_ZNAME( THD_linear_mapping * , int ,
                        THD_dataxes * , WTYPE * ,
                        THD_dataxes * new_daxes , int , WTYPE * ) ;

extern void B2SL_NAME( int nxx, int nyy, int nzz ,
                       int fixed_axis , int fixed_index ,
                       WTYPE * bold , WTYPE * bslice ) ;
/*------------------------------------------------------------------*/
#undef  WTYPE
#undef  LMAP_XNAME
#undef  LMAP_YNAME
#undef  LMAP_ZNAME
#undef  B2SL_NAME
#define WTYPE float
#define LMAP_XNAME TWO_TWO(AFNI_lmap_to_xslice_,WTYPE)
#define LMAP_YNAME TWO_TWO(AFNI_lmap_to_yslice_,WTYPE)
#define LMAP_ZNAME TWO_TWO(AFNI_lmap_to_zslice_,WTYPE)
#define B2SL_NAME  TWO_TWO(AFNI_br2sl_,WTYPE)

extern void LMAP_XNAME( THD_linear_mapping * , int ,
                        THD_dataxes * , WTYPE * ,
                        THD_dataxes * new_daxes , int , WTYPE * ) ;

extern void LMAP_YNAME( THD_linear_mapping * , int ,
                        THD_dataxes * , WTYPE * ,
                        THD_dataxes * new_daxes , int , WTYPE * ) ;

extern void LMAP_ZNAME( THD_linear_mapping * , int ,
                        THD_dataxes * , WTYPE * ,
                        THD_dataxes * new_daxes , int , WTYPE * ) ;

extern void B2SL_NAME( int nxx, int nyy, int nzz ,
                       int fixed_axis , int fixed_index ,
                       WTYPE * bold , WTYPE * bslice ) ;
/*---------------------------------------------------------------------*/
#undef  WTYPE
#undef  LMAP_XNAME
#undef  LMAP_YNAME
#undef  LMAP_ZNAME
#undef  B2SL_NAME
#define WTYPE byte
#define LMAP_XNAME TWO_TWO(AFNI_lmap_to_xslice_,WTYPE)
#define LMAP_YNAME TWO_TWO(AFNI_lmap_to_yslice_,WTYPE)
#define LMAP_ZNAME TWO_TWO(AFNI_lmap_to_zslice_,WTYPE)
#define B2SL_NAME  TWO_TWO(AFNI_br2sl_,WTYPE)

extern void LMAP_XNAME( THD_linear_mapping * , int ,
                        THD_dataxes * , WTYPE * ,
                        THD_dataxes * new_daxes , int , WTYPE * ) ;

extern void LMAP_YNAME( THD_linear_mapping * , int ,
                        THD_dataxes * , WTYPE * ,
                        THD_dataxes * new_daxes , int , WTYPE * ) ;

extern void LMAP_ZNAME( THD_linear_mapping * , int ,
                        THD_dataxes * , WTYPE * ,
                        THD_dataxes * new_daxes , int , WTYPE * ) ;

extern void B2SL_NAME( int nxx, int nyy, int nzz ,
                       int fixed_axis , int fixed_index ,
                       WTYPE * bold , WTYPE * bslice ) ;
/*---------------------------------------------------------------------*/
#undef  WTYPE
#undef  LMAP_XNAME
#undef  LMAP_YNAME
#undef  LMAP_ZNAME
#undef  B2SL_NAME
#define WTYPE complex
#define LMAP_XNAME TWO_TWO(AFNI_lmap_to_xslice_,WTYPE)
#define LMAP_YNAME TWO_TWO(AFNI_lmap_to_yslice_,WTYPE)
#define LMAP_ZNAME TWO_TWO(AFNI_lmap_to_zslice_,WTYPE)
#define B2SL_NAME  TWO_TWO(AFNI_br2sl_,WTYPE)

extern void LMAP_XNAME( THD_linear_mapping * , int ,
                        THD_dataxes * , WTYPE * ,
                        THD_dataxes * new_daxes , int , WTYPE * ) ;

extern void LMAP_YNAME( THD_linear_mapping * , int ,
                        THD_dataxes * , WTYPE * ,
                        THD_dataxes * new_daxes , int , WTYPE * ) ;

extern void LMAP_ZNAME( THD_linear_mapping * , int ,
                        THD_dataxes * , WTYPE * ,
                        THD_dataxes * new_daxes , int , WTYPE * ) ;

extern void B2SL_NAME( int nxx, int nyy, int nzz ,
                       int fixed_axis , int fixed_index ,
                       WTYPE * bold , WTYPE * bslice ) ;
/*---------------------------------------------------------------------*/
#undef  WTYPE
#undef  LMAP_XNAME
#undef  LMAP_YNAME
#undef  LMAP_ZNAME
#undef  B2SL_NAME
#define WTYPE rgbyte
#define LMAP_XNAME TWO_TWO(AFNI_lmap_to_xslice_,WTYPE)
#define LMAP_YNAME TWO_TWO(AFNI_lmap_to_yslice_,WTYPE)
#define LMAP_ZNAME TWO_TWO(AFNI_lmap_to_zslice_,WTYPE)
#define B2SL_NAME  TWO_TWO(AFNI_br2sl_,WTYPE)

extern void LMAP_XNAME( THD_linear_mapping * , int ,
                        THD_dataxes * , WTYPE * ,
                        THD_dataxes * new_daxes , int , WTYPE * ) ;

extern void LMAP_YNAME( THD_linear_mapping * , int ,
                        THD_dataxes * , WTYPE * ,
                        THD_dataxes * new_daxes , int , WTYPE * ) ;

extern void LMAP_ZNAME( THD_linear_mapping * , int ,
                        THD_dataxes * , WTYPE * ,
                        THD_dataxes * new_daxes , int , WTYPE * ) ;

extern void B2SL_NAME( int nxx, int nyy, int nzz ,
                       int fixed_axis , int fixed_index ,
                       WTYPE * bold , WTYPE * bslice ) ;
/*------------------------------------------------------------------*/
#undef  WTYPE
#undef  LMAP_XNAME
#undef  LMAP_YNAME
#undef  LMAP_ZNAME
#undef  B2SL_NAME
/********************************************************************/

#endif /* _AFNI_WARP_HEADER_ */
