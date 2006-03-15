#ifndef _MRILIB_DICOM_STUFF_
#define _MRILIB_DICOM_STUFF_

/*-- manufacturer codes --*/

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
#define AFD_MAN_BRUKER     (AFD_MAN_OFFSET + 9)
#define AFD_MAN_VARIAN     (AFD_MAN_OFFSET +10)

#define AFD_MAN_UNKNOWN    (AFD_MAN_OFFSET+666)

#undef  AFD_MAN_GOOD
#define AFD_MAN_GOOD(mm)   ((mm) > AFD_MAN_OFFSET && (mm) <= AFD_MAN_UNKNOWN)

/*-- struct to hold header info from one file --*/

typedef struct {
  int   manufacturer ;                      /* from ID  group */
  float tr , slice_spacing , slice_thick ;  /* from ACQ group */
  float pos_xx , pos_yy , pos_zz ,          /* from REL group */
        ori_ix , ori_iy , ori_iz ,
        ori_jx , ori_jy , ori_jz ,
        slice_loc ;
  float di , dj ;                           /* from IMG group */
  int   ni , nj ;
  unsigned data_offset, data_length ;       /* from PXL group */
  int      nbits ;

  char *filename ;                          /* where 'tis */
  void *extra_info ;                        /* whatever   */
} AFD_dicom_header ;

typedef struct {
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

#endif /* _MRILIB_DICOM_STUFF_ */
