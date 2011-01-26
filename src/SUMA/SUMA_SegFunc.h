#ifndef SUMA_SEGFUNC_INCLUDED
#define SUMA_SEGFUNC_INCLUDED

#define IN_MASK(mm,k) ( (!(mm) || (mm)[k]) )

/*!
   Swap a column between two dsets.
   This macro will check for nothing. 
   You must ensure that the two dsets have the same 
   dimensions, brick types, etc.
*/
#define SWAP_COL(p1,p2,i) {  \
   void *m_ap1 = DSET_ARRAY(p1,i); \
   void *m_ap2 = DSET_ARRAY(p2,i); \
   float m_f1 = DSET_BRICK_FACTOR(p1,i); \
   float m_f2 = DSET_BRICK_FACTOR(p2,i); \
   mri_fix_data_pointer(m_ap1,DSET_BRICK(p2,i));  \
   mri_fix_data_pointer(m_ap2,DSET_BRICK(p1,i));  \
   EDIT_BRICK_FACTOR(p2,i,m_f1);   \
   EDIT_BRICK_FACTOR(p1,i,m_f2);   \
}

/*!
   Get a sub-brick index from label
*/
#define SB_LABEL(p,lbl, ia) { \
   for (ia = 0; ia <DSET_NVALS(p); ++ia) {   \
      if (!strcmp(DSET_BRICK_LABEL(p,ia),lbl)) break; \
   }  \
   if (ia==DSET_NVALS(p)) ia = -1;  /* failed */\
}

#define GET_BFs(pout, bf) { \
   int m_i; \
   for (m_i=0; m_i<DSET_NVALS(pout); ++m_i) {   \
      bf[m_i] = DSET_BRICK_FACTOR(pout,m_i); \
   }\
}
#define PUT_BFs(pout, bf) { \
   int m_i; \
   for (m_i=0; m_i<DSET_NVALS(pout); ++m_i) {   \
      EDIT_BRICK_FACTOR(pout,m_i, bf[m_i]); \
   }  \
}

#define GET_VEC_AT_VOX(pout, j, dv, bf) {   \
   int m_i;   \
   short *m_p;   \
   for (m_i=0; m_i<DSET_NVALS(pout); ++m_i) {   \
      m_p = (short *)DSET_ARRAY(pout, m_i);   \
      if (bf[m_i]!=0.0f) { \
         dv[m_i] = m_p[j]*bf[m_i];  \
      } else { dv[m_i] = m_p[j]; }  \
   }  \
}

#define MAX_AT_VOX(pout, j, imax, max) {   \
   int m_i;   \
   short *m_p=(short *)DSET_ARRAY(pout, 0);   \
   max = m_p[j]; imax = 0;   \
   for (m_i=1; m_i<DSET_NVALS(pout); ++m_i) {   \
      m_p = (short *)DSET_ARRAY(pout, m_i);   \
      if (m_p[j] > max) { imax=m_i; max = m_p[j]; }  \
   }  \
}

#define PUT_VEC_AT_VOX(pout, j, dv, bf) {   \
   int m_i;   \
   short *m_p;   \
   for (m_i=0; m_i<DSET_NVALS(pout); ++m_i) {   \
      m_p = (short *)DSET_ARRAY(pout, m_i);   \
      if (bf[m_i]!=0.0f) { \
         m_p[j] = (short) (dv[m_i] / bf[m_i]);  \
      } else { \
         m_p[j] = (short) (dv[m_i]); \
      } \
   }  \
}

#define NEW_SHORTY(par,nsb,nm,pb){  \
   int m_i;   \
   pb = EDIT_empty_copy(par); \
   EDIT_dset_items( pb ,   \
                    ADN_prefix , nm ,  \
                    ADN_nvals, nsb, \
                    ADN_ntt, nsb, \
                    ADN_malloc_type , DATABLOCK_MEM_MALLOC ,   \
                    ADN_view_type   , VIEW_ORIGINAL_TYPE ,  \
                    ADN_type        , HEAD_ANAT_TYPE ,   \
                    ADN_func_type   , ANAT_BUCK_TYPE ,   \
                    ADN_none ) ; \
   for(m_i=0;m_i<nsb;++m_i) EDIT_substitute_brick( pb, m_i, MRI_short, NULL ) ; \
}

#define NEW_FLOATY(par,nsb,nm,pb){  \
   int m_i;   \
   pb = EDIT_empty_copy(par); \
   EDIT_dset_items( pb ,   \
                    ADN_prefix , nm ,  \
                    ADN_nvals, nsb, \
                    ADN_ntt, nsb, \
                    ADN_malloc_type , DATABLOCK_MEM_MALLOC ,   \
                    ADN_view_type   , VIEW_ORIGINAL_TYPE ,  \
                    ADN_type        , HEAD_ANAT_TYPE ,   \
                    ADN_func_type   , ANAT_BUCK_TYPE ,   \
                    ADN_none ) ; \
   for(m_i=0;m_i<nsb;++m_i) EDIT_substitute_brick( pb, m_i, MRI_float, NULL ) ; \
}

#define EPS 0.000001
#define MINP 0.01   /* I tried 0.05, 0.01, and 0.001: See
                        results of @Run.PROB.12class.1 (at 0.01)
                        @Run.PROB.12class.1b (at 0.001), and 
                        @Run.PROB.12class.1c (at 0.05) .
                     0.05 is too blunt a cut off, seems to cause too much
                     loss of information. 0.01 is OK, 0.001 seemed to
                     give slightly more stability for CSF for subject 1,
                     But further testing and changes to p_a_GIV_cvfu don't
                     show that, so 0.01 it remains*/ 
#define SQ2PI 2.506628

#define IND_1D_2_NORM_3D_index(ijk, IJK, ni, nij, halfN){  \
   int m_i=0, m_j=0, m_k=0;   \
   m_k = ((ijk) / (nij)); \
   m_j = ((ijk) % (nij));   \
   m_i = ((m_j) % (ni));  \
   m_j = ((m_j) / (ni)); \
   /* and normalize from -1 to 1 */ \
   IJK[0] = (m_i - halfN[0])/halfN[0];   \
   IJK[1] = (m_j - halfN[1])/halfN[1];   \
   IJK[2] = (m_k - halfN[2])/halfN[2];   \
   /* fprintf(stderr,"ijk %d = [%d, %d, %d] = [ %f %f %f]\n",  \
                     ijk, m_i, m_j, m_k, IJK[0], IJK[1], IJK[2]);  \ */ \
}

#define V_NEIGHB_0(IJK,ni, nij, cmask, ijkn) { /* at i-1 */\
   static int m_i ;\
   m_i = IJK[0]-1; \
   ijkn=-1; \
   if (m_i >= 0) {  \
      ijkn = m_i+IJK[1]*ni+IJK[2]*nij; \
      if (!IN_MASK(cmask,ijkn)) ijkn = -1;   \
   } \
}

#define V_NEIGHB_1(IJK, ni, nij, cmask, nj, ijkn) { /* at j-1 */\
   static int m_j ;\
   m_j = IJK[1]-1; \
   ijkn=-1; \
   if (m_j>=0) { \
      ijkn = IJK[0]+m_j*ni+IJK[2]*nij; \
      if (!IN_MASK(cmask,ijkn))  ijkn = -1;   \
   } \
}

#define V_NEIGHB_2(IJK, ni, nij, cmask, ijkn) { /* at i+1 */\
   static int m_i;   \
   m_i = IJK[0]+1; \
   ijkn=-1; \
   if (m_i<ni) { \
      ijkn=m_i+IJK[1]*ni+IJK[2]*nij; \
      if (!IN_MASK(cmask,ijkn)) ijkn = -1;   \
   } \
}

#define V_NEIGHB_3(IJK, ni, nij, cmask, nj, ijkn) { /* at j+1 */\
   static int m_j;   \
   m_j = IJK[1]+1; \
   ijkn=-1; \
   if (m_j<nj) { \
      ijkn = IJK[0]+m_j*ni+IJK[2]*nij; \
      if (!IN_MASK(cmask,ijkn)) ijkn = -1;   \
   } \
}

#define V_NEIGHB_4(IJK, ni, nij, cmask, nk, ijkn) { /* at k-1 */\
   static int m_k;   \
   m_k = IJK[2]-1; \
   ijkn=-1; \
   if (m_k >= 0) { \
      ijkn = IJK[0]+IJK[1]*ni+m_k*nij; \
      if (!IN_MASK(cmask,ijkn)) ijkn = -1;   \
   } \
}

#define V_NEIGHB_5(IJK, ni, nij, cmask, nk, ijkn) { /* at k+1 */\
   static int m_k;   \
   m_k = IJK[2]+1; \
   ijkn=-1; \
   if (m_k<nk) { \
      ijkn = IJK[0]+IJK[1]*ni+m_k*nij; \
      if (!IN_MASK(cmask,ijkn)) ijkn = -1;   \
   } \
}

#define Vox1D2Vox3D(i1d, ni, nij, IJK) {  \
   IJK[2] = ((i1d) / (nij)); \
   IJK[1] = ((i1d) % (nij));   \
   IJK[0] = ((IJK[1]) % (ni));  \
   IJK[1] = ((IJK[1]) / (ni)); \
}

#define GET_NEIGHBS_IN_MASK(cmask, ijk, ni, nj, nk, nij, ijkn_vec){  \
   static int m_IJK[3];   \
   m_IJK[2] = ((ijk) / (nij)); \
   m_IJK[1] = ((ijk) % (nij));   \
   m_IJK[0] = ((m_IJK[1]) % (ni));  \
   m_IJK[1] = ((m_IJK[1]) / (ni)); \
   V_NEIGHB_0(m_IJK, ni, nij, cmask,     ijkn_vec[0]);/* i - 1 */ \
   V_NEIGHB_1(m_IJK, ni, nij, cmask, nj, ijkn_vec[1]);/* j - 1 */ \
   V_NEIGHB_2(m_IJK, ni, nij, cmask,     ijkn_vec[2]);/* i + 1 */ \
   V_NEIGHB_3(m_IJK, ni, nij, cmask, nj, ijkn_vec[3]);/* j + 1 */ \
   V_NEIGHB_4(m_IJK, ni, nij, cmask, nk, ijkn_vec[4]);/* k - 1 */ \
   V_NEIGHB_5(m_IJK, ni, nij, cmask, nk, ijkn_vec[5]);/* k + 1 */ \
}

/* THESE TWO ARE IS NOT VALID*/
#define V_ADD_CONTRIB_NOTGOOD(c,ijkn,cijk,P,NP) {\
   if (ijkn>=0) { \
      NP += 2;  \
      if (c[ijkn] == cijk) { P += 2; }   \
      /* No match, no contribution */  \
   } else { NP += 1; P += 1; } /* No neighbor, half +ve contribution */   \
}

#define P_l_GIV_NEIGHBS_NOTGOOD(c, ijkn_vec, cijk, Nvicinity, Pf) {\
   static int m_i, m_P, m_NP;\
   m_P = 0; m_NP = 0;  \
   for (m_i=0; m_i<Nvicinity; ++m_i) { \
      V_ADD_CONTRIB_NOTGOOD(c,ijkn_vec[m_i],cijk,m_P,m_NP);   \
   }  \
   if (m_NP) { Pf= (float)m_P/m_NP; }  \
   else {Pf = 0.0; } \
}

#define E_ADD_CONTRIB(c,ijkn,cijk,E,NE) {\
   if (ijkn>=0) { \
      NE += 1;  \
      if (c[ijkn] == cijk) { E -= 1; } /* match, least energy */\
      else E += 1; /* no match, higher energy */   \
   } else {  } /* No neighbor, neutral contribution */   \
}

#define E_l_GIV_NEIGHBS(c, ijkn_vec, cijk, Nvicinity, E, BoT) {\
   static int m_i, m_E, m_NE;\
   m_E = 0; m_NE = 0;  \
   for (m_i=0; m_i<Nvicinity; ++m_i) { \
      E_ADD_CONTRIB(c,ijkn_vec[m_i],cijk,m_E,m_NE);   \
   }  \
   if (m_NE) { E = BoT*m_E/(double)m_NE; } else {E=0.0;}\
}

#define AFNI_FEED(cs, sstr, iter, dset) {\
   if (cs->talk_suma) { \
      SUMA_S_Notev("Sending %s volume to AFNI, iter %d\n", \
                   sstr, iter); \
      if (!SUMA_SendToAfni(cs, dset, 1)) { \
         SUMA_SL_Err("Failed to send volume to AFNI");   \
         cs->afni_Send = NOPE;   \
      }  \
   }  \
}      
      
#define SUMA_SEG_WRITE_DSET(pref, dset, iter) {\
   char m_pref[512];   \
   char *opref = SUMA_copy_string(DSET_PREFIX(dset)); \
   if (iter >=0) sprintf(m_pref, "%s.%d", pref, iter); \
   else sprintf(m_pref, "%s", pref); \
   SUMA_S_Notev("Writing %s\n", m_pref);   \
   EDIT_dset_items(  dset , ADN_prefix  , m_pref, ADN_none);  \
   UNIQ_idcode_fill(DSET_IDCODE_STR(dset));/* new id */   \
   DSET_overwrite(dset);   \
   EDIT_dset_items(  dset , ADN_prefix  , opref, ADN_none);  \
   SUMA_free(opref); \
}

int SUMA_KeyofLabel_Dtable(Dtable *vl_dtable, char *label);
void SUMA_ShowClssKeys(NI_str_array *clss, int *keys);
char *SUMA_LabelsKeys2labeltable_str(NI_str_array *clss, int *keys);
Dtable *SUMA_LabelsKeys2Dtable (NI_str_array *clss, int *keys);

int get_train_pdist(SEG_OPTS *Opt, char *feat, char *cls, 
                     double *par, double *scpar) ;
double pdfgam(double x,double ash, double brt);
int p_a_GIV_cvfu(SEG_OPTS *Opt, char *feat, char *cls, 
                  THD_3dim_dataset *pout);
int p_cv_GIV_afu (SEG_OPTS *Opt, char *feat, 
                  char *cls, double *d);
int p_cv_GIV_A (SEG_OPTS *Opt, char *cls, double *dr);
int normalize_p(SEG_OPTS *Opt, THD_3dim_dataset *pout);
THD_3dim_dataset *p_C_GIV_A (SEG_OPTS *Opt);
int LabelToGroupedIndex(char *cls_str, NI_str_array *group_clss);
int LabelToGroupedKey(char *cls_str, NI_str_array *group_clss, 
                      int *group_keys);
int GroupLabelMapping (NI_str_array *clss , NI_str_array *grpclss, 
                        int *map, int verb); 
int Regroup_classes (SEG_OPTS *Opt, 
                     NI_str_array *group_classes,
                     int  * ugroup_keys,
                     THD_3dim_dataset *pset, 
                     THD_3dim_dataset *cset,
                     THD_3dim_dataset **gpset, 
                     THD_3dim_dataset **gcset); 
THD_3dim_dataset *assign_classes (SEG_OPTS *Opt, THD_3dim_dataset *pset);
int  group_mean (SEG_OPTS *Opt, THD_3dim_dataset *aset,
                 byte *mm, THD_3dim_dataset *pset, int N_cl,
                 double *M_v, int scl);          
THD_3dim_dataset *estimate_bias_field_NoCross (SEG_OPTS *Opt, 
                                       THD_3dim_dataset *aset,
                                       THD_3dim_dataset *cset,
                                       THD_3dim_dataset *pset) ;
THD_3dim_dataset *SUMA_estimate_bias_field (SEG_OPTS *Opt,
                                       int polorder, 
                                       THD_3dim_dataset *aset,
                                       THD_3dim_dataset *cset,
                                       THD_3dim_dataset *pset,
                                       THD_3dim_dataset *pout);
THD_3dim_dataset *SUMA_apply_bias_field (SEG_OPTS *Opt, 
                                       THD_3dim_dataset *aset,
                                       THD_3dim_dataset *fset,
                                       THD_3dim_dataset *pout);
THD_3dim_dataset *SUMA_SegEnhanceInitCset(THD_3dim_dataset *aseti, 
                                          THD_3dim_dataset *cset, 
                                 byte *cmask, int cmask_count, 
                                 SUMA_CLASS_STAT *cs,
                                 SEG_OPTS *Opt);
int bias_stats (SEG_OPTS *Opt, 
                THD_3dim_dataset *aset, THD_3dim_dataset *gset, 
                THD_3dim_dataset *xset, int N_cl);
int SUMA_show_Class_Stat(SUMA_CLASS_STAT *cs, char *h);
int SUMA_set_Stat(SUMA_CLASS_STAT *cs, char *label, char *pname, double val);
double SUMA_get_Stat(SUMA_CLASS_STAT *cs, char *label, char *pname);
int SUMA_Stat_position (SUMA_CLASS_STAT *cs, char *label, char *pname, int pp[]);
SUMA_CLASS_STAT *SUMA_Free_Class_Stat(SUMA_CLASS_STAT *cs);
SUMA_CLASS_STAT *SUMA_New_Class_Stat(NI_str_array *clss, int *keys, 
                                    int nP, NI_str_array *pnames);
int SUMA_Class_stats(THD_3dim_dataset *aset, 
                     THD_3dim_dataset *cset, 
                     byte *cmask, 
                     THD_3dim_dataset *wset, 
                     SUMA_CLASS_STAT *cs);
double pdfnorm(double x, double mean, double stdv);
THD_3dim_dataset *SUMA_p_Y_GIV_C_B_O(
                           THD_3dim_dataset *aset, THD_3dim_dataset *cset,
                                 byte *cmask, SUMA_CLASS_STAT *cs, 
                                 THD_3dim_dataset *pygc); 
THD_3dim_dataset *SUMA_MAP_labels(THD_3dim_dataset *aset, 
                        byte *cmask, THD_3dim_dataset *pygcbo, 
                        SUMA_CLASS_STAT *cs, int neighopt, 
                        THD_3dim_dataset *cset, SEG_OPTS *Opt);
THD_3dim_dataset *SUMA_p_c_GIV_y(THD_3dim_dataset *aset, THD_3dim_dataset *cset, 
                                 byte *cmask, SUMA_CLASS_STAT *cs, int neighopt,
                                 THD_3dim_dataset *pcgy, SEG_OPTS *Opt);

#endif
