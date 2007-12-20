#define THD_BN_CMTOP

typedef enum { SPECIE_NOT_SET=-1, HUMAN = 0, MONKEY, MARMOSET, RAT, N_SPECIES} Species; 

#define WRITE_MRI_IMAGE_3D_RAI(im, pref){\
   char *m_pout=NULL, *m_en=NULL, m_nen[300];  \
   THD_ivec3 m_orixyz , m_nxyz ;   \
   THD_fvec3 m_dxyz , m_orgxyz;  \
   THD_3dim_dataset *m_oset = NULL;   \
   \
   if (MRI_IS_3D(im)) { \
      m_oset = EDIT_empty_copy( NULL ) ;\
      LOAD_IVEC3( m_nxyz   , im->nx    , im->ny    , im->nz    );  \
      LOAD_FVEC3( m_dxyz   , im->dx    , im->dy    , im->dz    );  \
      LOAD_FVEC3( m_orgxyz , im->xo    , im->yo    , im->zo    );  \
      LOAD_IVEC3( m_orixyz , ORI_R2L_TYPE , ORI_A2P_TYPE , ORI_I2S_TYPE );  \
      if (pref) m_pout = pref; else m_pout = "imout";  \
      EDIT_dset_items( m_oset ,  \
                      ADN_prefix      , m_pout ,  \
                      ADN_datum_all   , im->kind , \
                      ADN_nxyz        , m_nxyz ,  \
                      ADN_xyzdel      , m_dxyz ,  \
                      ADN_xyzorg      , m_orgxyz ,   \
                      ADN_xyzorient   , m_orixyz ,   \
                      ADN_malloc_type , DATABLOCK_MEM_MALLOC , \
                      ADN_view_type   , VIEW_ORIGINAL_TYPE ,   \
                      ADN_type        , HEAD_ANAT_TYPE , \
                      ADN_func_type   , ANAT_BUCK_TYPE ,    \
                    ADN_none ) ; \
      EDIT_substitute_brick(  m_oset , 0 , im->kind ,  \
                              mri_data_pointer(im) ) ;  \
      m_en = my_getenv("AFNI_DECONFLICT");   \
      putenv("AFNI_DECONFLICT=OVERWRITE") ;  \
      fprintf(stderr,"Notice WRITE_MRI_IMAGE_3D_RAI: Wrote %s\n", pref);   \
      DSET_write(m_oset) ; \
      if (m_en) {    \
         sprintf(m_nen,"AFNI_DECONFLICT %s", m_en);  \
         AFNI_setenv(m_nen);  \
      } else { \
         putenv("AFNI_DECONFLICT=NO") ;  /* set to default */ \
      }  \
      /* Now delete m_oset but preserve pointer */  \
      mri_fix_data_pointer(NULL, DSET_BRICK(m_oset,0));\
      DSET_delete(m_oset); m_oset = NULL; \
   } else { \
      fprintf(stderr,"-- Error WRITE_MRI_IMAGE_3D_RAI: image not 3D\n");   \
   }  \
} 
