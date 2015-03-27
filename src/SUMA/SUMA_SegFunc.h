#ifndef SUMA_SEGFUNC_INCLUDED
#define SUMA_SEGFUNC_INCLUDED


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

/*! Macros to get (and put) value from sub-brick k, vox ijk
   
   DSET_ARRAY calls mri_data_pointer, 
   this can perhaps be sped up with direct access
   to im with:
   *(dset->dblk->brick->imarr[k]->im+ijk) 
*/
#define GVAL(dset,k,ijk,V) {\
   short *cc = (short *)DSET_ARRAY(dset,k);   \
   V = cc[ijk];   \
}
#define GSCVAL(dset,k,ijk,sf,V) {\
   short *cc = (short *)DSET_ARRAY(dset,k);   \
   V = cc[ijk]*sf;   \
}
#define PVAL(dset,k,ijk,V) {\
   short *cc = (short *)DSET_ARRAY(dset,k);   \
   cc[ijk] = (short)V;   \
}
#define PSCVAL(dset,k,ijk,sf,V) {\
   short *cc = (short *)DSET_ARRAY(dset,k);   \
   cc[ijk] = (short)(V/sf);   \
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

#define MAX_SC_AT_VOX(pout, j, imax, max) {   \
   int m_i;   \
   short *m_p=(short *)DSET_ARRAY(pout, 0);   \
   float m_f = DSET_BRICK_FACTOR(pout,0); if (m_f == 0.0f) m_f = 1.0; \
   max = m_p[j]*m_f; imax = 0;   \
   for (m_i=1; m_i<DSET_NVALS(pout); ++m_i) {   \
      m_p = (short *)DSET_ARRAY(pout, m_i);   \
      m_f = DSET_BRICK_FACTOR(pout,m_i); if (m_f == 0.0f) m_f = 1.0;   \
      if (m_p[j]*m_f > max) { imax=m_i; max = m_p[j]*m_f; }  \
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

#define NEW_SHORTYV(par,nsb,nm,pb,view){  \
   NEW_SHORTY(par,nsb,nm,pb); \
   if (view) {\
            if (!strstr(view,"orig")) \
         EDIT_dset_items( pb ,ADN_view_type , VIEW_ORIGINAL_TYPE ,ADN_none ) ; \
      else  if (!strstr(view,"acpc")) \
         EDIT_dset_items( pb ,ADN_view_type, VIEW_ACPCALIGNED_TYPE ,ADN_none ); \
      else  if (!strstr(view,"tlrc")) \
         EDIT_dset_items( pb ,ADN_view_type, VIEW_TALAIRACH_TYPE ,ADN_none ) ; \
      else SUMA_S_Errv("In NEW_SHORTYV; View of %s is rubbish", view);   \
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
                    ADN_type        , HEAD_ANAT_TYPE ,   \
                    ADN_func_type   , ANAT_BUCK_TYPE ,   \
                    ADN_none ) ; \
               /* ADN_view_type   , VIEW_ORIGINAL_TYPE ,  ZSS Sep 28 2012 */   \
   for(m_i=0;m_i<nsb;++m_i) EDIT_substitute_brick( pb, m_i, MRI_short, NULL ) ; \
   tross_Copy_History( par , pb ) ; \
}

#define SET_ALL_BRICK_FACTORS(pb, fac) {\
   int m_i; \
   for(m_i=0;m_i<DSET_NVALS(pb);++m_i) EDIT_BRICK_FACTOR(pb, m_i, (fac)); \
}

#define NEW_FLOATY(par,nsb,nm,pb){  \
   int m_i;   \
   pb = EDIT_empty_copy(par); \
   EDIT_dset_items( pb ,   \
                    ADN_prefix , nm ,  \
                    ADN_nvals, nsb, \
                    ADN_ntt, nsb, \
                    ADN_malloc_type , DATABLOCK_MEM_MALLOC ,   \
                    ADN_type        , HEAD_ANAT_TYPE ,   \
                    ADN_func_type   , ANAT_BUCK_TYPE ,   \
                    ADN_none ) ; \
   for(m_i=0;m_i<nsb;++m_i) EDIT_substitute_brick( pb, m_i, MRI_float, NULL ) ; \
   tross_Copy_History( par , pb ) ; \
}

#define NEW_FLOATYV(par,nsb,nm,pb,fv){  \
   int m_i; \
   pb = EDIT_empty_copy(par); \
   EDIT_dset_items( pb ,   \
                    ADN_prefix , nm ,  \
                    ADN_nvals, nsb, \
                    ADN_ntt, nsb, \
                    ADN_malloc_type , DATABLOCK_MEM_MALLOC ,   \
                    ADN_type        , HEAD_ANAT_TYPE ,   \
                    ADN_func_type   , ANAT_BUCK_TYPE ,   \
                    ADN_none ) ; \
   for(m_i=0;m_i<nsb;++m_i) \
      EDIT_substitute_brick( pb, m_i, MRI_float, (m_i==0 && (fv))?(fv):NULL); \
   tross_Copy_History( par , pb ) ; \
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

/*Macros to go from 1D to 4D ubyte indexing. 
  For SUMA_COLID_N2RGBA, you will need to pass ints for r g b a
  even though ubyte is needed for final storage */
#define SUMA_COLID_N2RGBA(n,r,g,b,a) { \
   (b) = (n) & (16777215); /* x % 2^24 = x & (2^n-1) */ \
   (a) = (n) >> 24; /* divide by 2^24 (256x256x256) */  \
   (g) = (b) & (65535); \
   (b) = (b) >> 16; \
   (r) = (g) & (255); \
   (g) = (g) >> 8;   \
}
#define SUMA_COLID_N2RGBA_slow(n,r,g,b,a) { \
   (b) = (n) % (16777216); /* x % 2^24 = x & (2^n-1) */ \
   (a) = (n) /  16777216; /* divide by 2^24 (256x256x256) */  \
   (g) = (b) % (65536); \
   (b) = (b) / 65536; \
   (r) = (g) % (256); \
   (g) = (g) / 256;  \
}
#define SUMA_COLID_RGBA2N(r,g,b,a,n) { \
   (n) = ((a) << 24) + ((b) << 16) + ((g) << 8) + (r);  \
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

#define E_l_GIV_NEIGHBS(c, ijkn_vec, cijk, Nvicinity, E) {\
   static int m_i, m_E, m_NE;\
   m_E = 0; m_NE = 0;  \
   for (m_i=0; m_i<Nvicinity; ++m_i) { \
      E_ADD_CONTRIB(c,ijkn_vec[m_i],cijk,m_E,m_NE);   \
   }  \
   if (m_NE) { E = m_E/(double)m_NE; } else {E=0.0;}\
}

#define AFNI_FEED(cs, sstr, iter, mdset) {\
   if (cs->talk_suma) { \
      SUMA_SEND_2AFNI SS2A;   \
      char *opref = SUMA_copy_string(DSET_PREFIX(mdset)); \
      char *m_pref = SUMA_append_string("t.",DSET_PREFIX(mdset)); \
      char *oid = SUMA_copy_string(DSET_IDCODE_STR(mdset));   \
      char lbuf[strlen(sstr)+10];   \
      if (Opt->debug > 1) SUMA_S_Notev("Sending %s volume to AFNI, iter %d\n", \
                   sstr, iter); \
      if (iter >=0) sprintf(lbuf, "%s.%d", sstr, iter); \
      else sprintf(lbuf, "%s", sstr); \
      EDIT_BRICK_LABEL( mdset, 0, lbuf);      \
      EDIT_dset_items(  mdset , ADN_prefix  , m_pref, ADN_none);  \
      strcpy(DSET_IDCODE_STR(mdset), UNIQ_hashcode(m_pref)); \
      SS2A.dset = mdset; SS2A.at_sb = iter;   \
      if (!SUMA_SendToAfni(cs, &SS2A, 1)) { \
         SUMA_SL_Err("Failed to send volume to AFNI");   \
         cs->afni_Send = NOPE;   \
      }  \
      EDIT_dset_items(  mdset , ADN_prefix  , opref, ADN_none);  \
      strcpy(DSET_IDCODE_STR(mdset), oid); \
      SUMA_free(opref); SUMA_free(oid);\
   }  \
}      

#define GRID_MISMATCH(a,b) (   (DSET_NX(a) != DSET_NX(b)) \
                            || (DSET_NY(a) != DSET_NY(b)) \
                            || (DSET_NZ(a) != DSET_NZ(b)) )

#define SUMA_IposBOUND  1
#define SUMA_InegBOUND  2
#define SUMA_I_HOLE     3
#define SUMA_JposBOUND  4
#define SUMA_JnegBOUND  8
#define SUMA_J_HOLE     12
#define SUMA_kposBOUND  16
#define SUMA_knegBOUND  32
#define SUMA_K_HOLE     48

/* A macro for SUMA_hist_freq(), requires 
int m_i0;
float m_a;
If you change this macro, be sure to reflect
the changes in function SUMA_hist_freq()
*/
#define SUMA_HIST_FREQ(hh, val, fr) {  \
   m_a = ((val-hh->b[0])/hh->W);   \
   m_i0 = (int)m_a;   \
   if (m_i0<0) { fr = hh->cn[0]; } \
   else if (m_i0>=hh->K) { fr = hh->cn[hh->K-1]; } \
   else { m_a = m_a-m_i0; fr = m_a*hh->cn[m_i0+1]+(1.0-m_a)*hh->cn[m_i0]; }  \
}


SUMA_HIST *SUMA_hist(float *v, int n, int Ku, float Wu, 
                     float *range, char *label, int ignoreout);
SUMA_HIST *SUMA_hist_opt(float *v, int n, int Ku, float Wu, float *range, 
                     char *label, int ignoreout, 
                     float oscfrqthr, char *methods);
SUMA_HIST *SUMA_dset_hist(THD_3dim_dataset *dset, int ia, 
                          byte *cmask, char *label, SUMA_HIST *href,
                          int ignoreout, float oscifreq, char *methods);
int SUMA_hist_smooth( SUMA_HIST *hh, int N_iter ); 
float SUMA_hist_oscillation( SUMA_HIST *hh, 
                             float minmaxfrac, float oscfracthr, int *N_osci);
SUMA_HIST *SUMA_Free_hist(SUMA_HIST *hh);
void SUMA_Show_hist(SUMA_HIST *hh, int norm, FILE *out);
void SUMA_Show_dist(SUMA_FEAT_DIST *FD, FILE *out);
void SUMA_Show_dists(SUMA_FEAT_DISTS *FDV, FILE *out, int level);
char *SUMA_dist_info(SUMA_FEAT_DIST *FD, int level);
char *SUMA_dists_info(SUMA_FEAT_DISTS *FDV, int level);
float SUMA_hist_freq(SUMA_HIST *hh, float vv);
double SUMA_hist_value(SUMA_HIST *hh, double vv, char *what);
float SUMA_hist_perc_freq(SUMA_HIST *nn, float perc, int norm, int *iperc, 
                          float minfreq);
double SUMA_val_at_count(SUMA_HIST *hh, double count, int norm, int from_top);
char *SUMA_hist_variable(SUMA_HIST *hh);
char *SUMA_hist_conditional(SUMA_HIST *hh);
char *SUMA_dist_variable(SUMA_FEAT_DIST *hh);
char *SUMA_dist_conditional(SUMA_FEAT_DIST *hh);
char *SUMA_label_variable(char *label, char c);
char *SUMA_label_conditional(char *label, char c);
NI_str_array * SUMA_dists_featureset(SUMA_FEAT_DISTS *FDV);
NI_str_array * SUMA_dists_classset(SUMA_FEAT_DISTS *FDV);
char *SUMA_hist_fname(char *proot, char *variable, char *conditional, 
                      int withext);
char *SUMA_corrmat_fname(char *proot, char *conditional, int withext);
SUMA_FEAT_DIST *SUMA_find_feature_dist(SUMA_FEAT_DISTS *FDV, 
                                       char *label, char *feature, char *class,
                                       int *ifind);
SUMA_FEAT_DISTS *SUMA_grow_feature_dists(SUMA_FEAT_DISTS *FDV);
SUMA_FEAT_DIST *SUMA_free_dist(SUMA_FEAT_DIST *FD);
SUMA_FEAT_DISTS *SUMA_free_dists(SUMA_FEAT_DISTS *FDV);
SUMA_FEAT_DISTS *SUMA_add_feature_dist(SUMA_FEAT_DISTS *FDV, 
                                       SUMA_FEAT_DIST **FDp,
                                       int append);
SUMA_FEAT_DIST *SUMA_hist_To_dist(SUMA_HIST **hhp, char *thislabel);
SUMA_FEAT_DISTS *SUMA_TRAIN_DISTS_To_dists(SUMA_FEAT_DISTS *FDV, 
                                           NI_element *ndist);
SUMA_FEAT_DISTS *SUMA_get_all_dists(char *where);
NI_group *SUMA_hist_To_NIhist(SUMA_HIST *hh);
SUMA_HIST *SUMA_NIhist_To_hist(NI_group *ngr);
int SUMA_write_hist(SUMA_HIST *hh, char *name);
SUMA_HIST *SUMA_read_hist(char *name);

int SUMA_Seg_Write_Dset(char *proot, char *prefi, THD_3dim_dataset *dset, 
                        int iter, char *hh);
int SUMA_KeyofLabel_Dtable(Dtable *vl_dtable, char *label);
void SUMA_ShowClssKeys(char **label, int N_label, int *keys);
char *SUMA_LabelsKeys2labeltable_str(char **label, int N_label, int *keys);
Dtable *SUMA_LabelsKeys2Dtable (char **label, int N_label, int *keys);

int get_train_pdist(SEG_OPTS *Opt, char *feat, char *cls, 
                     double *par, double *scpar) ;
double pdfgam(double x,double ash, double brt);
int p_a_GIV_cvfu(SEG_OPTS *Opt, char *feat, char *cls, 
                  THD_3dim_dataset *pout);
int p_cv_GIV_afu (SEG_OPTS *Opt, char *feat, 
                  char *cls, double *d);
int p_cv_GIV_A (SEG_OPTS *Opt, char *cls, double *dr);
int normalize_p(SEG_OPTS *Opt, THD_3dim_dataset *pout);
int is_shorty(THD_3dim_dataset *pset);
int set_p_floor(THD_3dim_dataset *pset, float pfl, byte *cmask);
SUMA_Boolean SUMA_set_Stat_mix_floor(SUMA_CLASS_STAT *cs, float floor);
THD_3dim_dataset *p_C_GIV_A (SEG_OPTS *Opt);
THD_3dim_dataset *p_C_GIV_A_omp (SEG_OPTS *Opt);
int SUMA_LabelToGroupedIndex(char *cls_str, char **group_clss_lbls, int N_lbls);
int SUMA_LabelToGroupedKey(char *cls_str, char **group_clss_lbls, int N_lbls, 
                           int *group_keys);
int SUMA_GroupLabelMapping (char **clss , int N_clss, 
                            char **grpclss, int N_grpclss, 
                            int *map, int verb); 
int SUMA_Regroup_classes (SEG_OPTS *Opt, 
                     char **clss_lbls,
                     int N_clss_lbls,
                     int *keys,                      
                     char **group_classes,
                     int N_group_classes,
                     int  * ugroup_keys,
                     byte *cmask,
                     THD_3dim_dataset *pset, 
                     THD_3dim_dataset *cset,
                     THD_3dim_dataset **gpset, 
                     THD_3dim_dataset **gcset); 
int SUMA_assign_classes (THD_3dim_dataset *pset, 
                         SUMA_CLASS_STAT *cs,
                         byte *cmask,
                         THD_3dim_dataset **csetp);
int SUMA_assign_classes_eng(THD_3dim_dataset *pset, 
                         char **label, int N_label, int *keys,
                         byte *cmask,
                         THD_3dim_dataset **csetp);
int  group_mean (SEG_OPTS *Opt, THD_3dim_dataset *aset,
                 byte *mm, THD_3dim_dataset *pset, int N_cl,
                 double *M_v, int scl);          
THD_3dim_dataset *SUMA_estimate_bias_field (SEG_OPTS *Opt,
                                       int polorder, 
                                       THD_3dim_dataset *aset,
                                       THD_3dim_dataset *cset,
                                       THD_3dim_dataset *pset,
                                       THD_3dim_dataset *pout);
int SUMA_estimate_bias_field_Wells (SEG_OPTS *Opta, 
                                       byte *cmask, SUMA_CLASS_STAT *cs,
                                       float fwhm, char *bias_classes,
                                       THD_3dim_dataset *Aset,
                                       THD_3dim_dataset *pstCgALL,
                                       THD_3dim_dataset **Bsetp);
int SUMA_apply_bias_field (SEG_OPTS *Opt, 
                           THD_3dim_dataset *aset,
                           THD_3dim_dataset *fset,
                           THD_3dim_dataset **pout);
THD_3dim_dataset *SUMA_SegEnhanceInitCset(THD_3dim_dataset *aseti, 
                                          THD_3dim_dataset *cset, 
                                 byte *cmask, int cmask_count, 
                                 SUMA_CLASS_STAT *cs,
                                 SEG_OPTS *Opt);
int bias_stats (SEG_OPTS *Opt, 
                THD_3dim_dataset *aset, THD_3dim_dataset *gset, 
                THD_3dim_dataset *xset, int N_cl);
double SUMA_CompareBiasDsets(THD_3dim_dataset *gold_bias, THD_3dim_dataset *bias,
                         byte *cmask, int cmask_count, 
                         float thresh, THD_3dim_dataset *prat );
int SUMA_show_Class_Stat(SUMA_CLASS_STAT *cs, char *h, char *fname);
int SUMA_dump_Class_Stat(SUMA_CLASS_STAT *cs, char *head, FILE *Out);
char *SUMA_Class_Stat_Info(SUMA_CLASS_STAT *cs, char *head);
int SUMA_set_Stat(SUMA_CLASS_STAT *cs, char *label, char *pname, double val);
double SUMA_get_Stat(SUMA_CLASS_STAT *cs, char *label, char *pname);
double *SUMA_get_Stats(SUMA_CLASS_STAT *cs,  char *pname);
int SUMA_MixFrac_from_ClassStat(SUMA_CLASS_STAT *cs, float *mf);
int SUMA_ZeroSamp_from_ClassStat(SUMA_CLASS_STAT *cs);
int SUMA_Stat_position (SUMA_CLASS_STAT *cs, char *label, char *pname, int pp[]);
SUMA_CLASS_STAT *SUMA_Free_Class_Stat(SUMA_CLASS_STAT *cs);
SUMA_CLASS_STAT *SUMA_New_Class_Stat(char **clsl, int N_clsl, int *keys, 
                                    int nP, NI_str_array *pnames);
int SUMA_Class_stats(THD_3dim_dataset *aset, 
                     THD_3dim_dataset *cset, 
                     byte *cmask, int cmask_count,
                     THD_3dim_dataset *wset,
                     THD_3dim_dataset *pC, 
                     THD_3dim_dataset *gold,
                     SUMA_CLASS_STAT *cs, float mix_frac_floor);
double SUMA_mixopt_2_mixfrac(char *mixopt, char *label, int key, int N_clss,
                             byte *cmask, THD_3dim_dataset *cset);
double pdfnorm(double x, double mean, double stdv);
THD_3dim_dataset *SUMA_p_Y_GIV_C_B_O(
                           THD_3dim_dataset *aset, THD_3dim_dataset *cset,
                                 byte *cmask, SUMA_CLASS_STAT *cs, 
                                 THD_3dim_dataset *pygc); 
int SUMA_MAP_labels(THD_3dim_dataset *aset, 
                        byte *cmask, 
                        SUMA_CLASS_STAT *cs, int neighopt, 
                        THD_3dim_dataset *pC,
                        THD_3dim_dataset **csetp, 
                        THD_3dim_dataset **pCgN, 
                        SEG_OPTS *Opt);
int SUMA_pst_C_giv_ALL(THD_3dim_dataset *aset,  
                                 byte *cmask, int cmask_count,
                                 SUMA_CLASS_STAT *cs, 
                                 THD_3dim_dataset *pC, THD_3dim_dataset *pCgN, 
                                 float mrfB, float Temp, byte mix,
                                 THD_3dim_dataset **pstCgALLp);
int SUMA_CompareSegDsets(THD_3dim_dataset *base, THD_3dim_dataset *seg,
                         byte *cmask, byte mask_by_base,
                         SUMA_CLASS_STAT *cs );
int SUMA_ray_k(int n1D, int ni, int nij, int nk, float *av, byte *ba, 
               float ta[], int da[]);
int SUMA_ray_j(int n1D, int ni, int nij, int nj, float *av, byte *ba, 
               float ta[], int da[]);
int SUMA_ray_i(int n1D, int ni, int nij, float *av, byte *ba, 
               float ta[], int da[]);
int SUMA_ray_unplug_k(int n1D, int ni, int nij, int nk, 
                      float *av, byte *ba, int side);
int SUMA_ray_unplug_j(int n1D, int ni, int nij, int nj, 
                      float *av, byte *ba, int side);
int SUMA_ray_unplug_i(int n1D, int ni, int nij, 
                      float *av, byte *ba, int side);
int SUMA_VolumeInFill(THD_3dim_dataset *aset,
                      THD_3dim_dataset **filledp,
                      int method, int integ, int MxIter,
                      int minhits, int erode, int dilate, float val,
                      byte *mask);
int SUMA_Volume_RadFill(THD_3dim_dataset *aset, float *ufv, byte *ucmask,
                      float *ucm, THD_3dim_dataset **filledp,
                      int nplug, int nlin, int fitord, float smooth, int N_off);
int SUMA_mri_volume_infill(MRI_IMAGE *imin);
int SUMA_mri_volume_infill_zoom(MRI_IMAGE *imin, byte thorough, 
                                 int integ, int mxiter);
int SUMA_mri_volume_infill_solid(MRI_IMAGE *imin, int minhits, 
                                 int mxiter, int unholize,
                                 byte *mask);
int SUMA_VolumeBlurInMask(THD_3dim_dataset *aset,
                                     byte *cmask,
                                     THD_3dim_dataset **blrdp,
                                     float FWHM, float unifac,
                                     int speed);
int SUMA_VolumeBlur(THD_3dim_dataset *aset,
                   byte *cmask,
                   THD_3dim_dataset **blurredp,
                   float FWHM);
int SUMA_VolumeLSBlurInMask(THD_3dim_dataset *aset ,
                                     byte *cmask,
                                     THD_3dim_dataset **blurredp,
                                     float FWHM, float mxvx);
double SUMA_EdgeEnergy(short *a, float af, short *b, float bf,
                      int Ni, int Nj, int Nk,
                      short *c, short c1, short c2, 
                      byte *mask, SUMA_CLASS_STAT *cs,
                      int method, short *skel, 
                      int *n_en);
double SUMA_DsetEdgeEnergy(THD_3dim_dataset *aset,
                      THD_3dim_dataset *cset,
                      byte *mask, 
                      THD_3dim_dataset *fset, THD_3dim_dataset *skelset,
                      SUMA_CLASS_STAT *cs, int method,
                      int *UseK, int N_kok);
double SUMA_MAP_EdgeEnergy(THD_3dim_dataset *aset, byte *cmask, int cmask_count,
                        THD_3dim_dataset *Bset, SUMA_CLASS_STAT *cs, 
                        THD_3dim_dataset *cset, int method, 
                        THD_3dim_dataset *priCgAll, THD_3dim_dataset *pCgN, 
                        float mrfB, float Temp, float deltamean, float deltastd,
                        SEG_OPTS * Opt);
int SUMA_ShortizeProbDset(THD_3dim_dataset **csetp, 
                        SUMA_CLASS_STAT *cs, 
                        byte *cmask, int cmask_count, 
                        SEG_OPTS *Opt, 
                        THD_3dim_dataset **psetp);
int SUMA_OtherizeProbDset(THD_3dim_dataset *pC, 
                          byte *cmask, int cmask_count);
int SUMA_FlattenProb(THD_3dim_dataset *pC, 
                     byte *cmask, int cmask_count, 
                     int mode);
int SUMA_AddOther(  NI_str_array *clss, int **keys, 
                    byte *cmask, int cmask_count,
                    THD_3dim_dataset *cset, THD_3dim_dataset *pstCgALL,
                    THD_3dim_dataset *pCgA, THD_3dim_dataset *pCgL,
                    SUMA_CLASS_STAT *cs);
int SUMA_Class_k_Selector(
   SUMA_CLASS_STAT *cs, char *action, char *value, int *UseK);
int SUMA_Class_k_Label_Locator(SUMA_CLASS_STAT *cs, char *label); 
int SUMA_Class_k_Key_Locator(SUMA_CLASS_STAT *cs, int key);
int SUMA_InitDset(THD_3dim_dataset  *aset, float *val, int nval,
                  byte *cmask, byte setsf);
int SUMA_MergeCpriors(SUMA_CLASS_STAT *cs, byte *cmask,
                                      THD_3dim_dataset  *Aset,
                                      THD_3dim_dataset  *priCgA, float wA,
                                      THD_3dim_dataset  *priCgL, float wL,
                                      THD_3dim_dataset  **priCgALLp,
                                      SEG_OPTS *Opt);
int SUMA_SegInitCset(THD_3dim_dataset *aseti, 
                     THD_3dim_dataset **csetp, 
                     byte *cmask, int cmask_count,
                     char *mixopt, 
                     SUMA_CLASS_STAT *cs,
                     SEG_OPTS *Opt);
void SUMA_set_SegFunc_debug(int dbg, int vdbg, int *vdbg3, FILE *out);
int SUMA_Split_Classes(char **Glbls, int N_Glbls, int *Gkeys, int *Split,
                       THD_3dim_dataset *aset, THD_3dim_dataset *Gcset,
                       byte *cmask,
                       THD_3dim_dataset **Scsetp, SUMA_CLASS_STAT **Scs,
                       SEG_OPTS *Opt);
int SUMA_SetDsetLabeltable(THD_3dim_dataset *dset, char **labels, 
                           int N_labels, int *keys);

SUMA_SurfaceObject *SUMA_Mask_Skin(THD_3dim_dataset *iset, int ld,
                       int smooth_final, int shrink_mode, SUMA_COMM_STRUCT *cs);
SUMA_SurfaceObject *SUMA_Dset_ConvexHull(THD_3dim_dataset *dset, int isb,
                                        float th, byte *umask);
SUMA_SurfaceObject *SUMA_ExtractHead_hull(THD_3dim_dataset *iset,
                                     float hullvolthr, SUMA_COMM_STRUCT *cs);
SUMA_SurfaceObject *SUMA_ExtractHead(THD_3dim_dataset *iset,
                                     float hullvolthr, SUMA_COMM_STRUCT *cs);
SUMA_SurfaceObject *SUMA_ExtractHead_RS(THD_3dim_dataset *iset,
                               THD_3dim_dataset **urset, SUMA_COMM_STRUCT *cs);
SUMA_Boolean SUMA_ShrinkSkullHull2Mask(SUMA_SurfaceObject *SO, 
                             THD_3dim_dataset *iset, float thr,
                             int smooth_final, float *under_over_mm,
                             int zero_attractor, 
                             SUMA_COMM_STRUCT *cs);
SUMA_Boolean SUMA_ShrinkSkullHull(SUMA_SurfaceObject *SO, 
                             THD_3dim_dataset *iset, float thr,
                             int use_rs, SUMA_COMM_STRUCT *cs);                  
SUMA_Boolean SUMA_ShrinkHeadSurf(SUMA_SurfaceObject *SO, 
                             THD_3dim_dataset *aset, 
                             THD_3dim_dataset *arset,
                             float *ucm,
                             SUMA_COMM_STRUCT *cs);
SUMA_Boolean SUMA_ShrinkSkullHull_RS(SUMA_SurfaceObject *SO, 
                             THD_3dim_dataset *aset, 
                             THD_3dim_dataset *arset, float thr,
                             SUMA_COMM_STRUCT *cs) ;
SUMA_Boolean SUMA_ShrinkHeadSurf_RS(SUMA_SurfaceObject *SO, 
                             THD_3dim_dataset *aset, 
                             THD_3dim_dataset *arset,
                             float *ucm,
                             SUMA_COMM_STRUCT *cs);
THD_3dim_dataset *SUMA_Dset_FindVoxelsInSurface(
                     SUMA_SurfaceObject *SO, THD_3dim_dataset *iset, 
                     SUMA_VOLPAR *vp, char *vpname,
                     char *prefix, int meth, int maskonly);          
THD_3dim_dataset *SUMA_dset_hist_equalize(THD_3dim_dataset *din, 
                                          int sb, byte *cmask, SUMA_HIST *hh);

#endif
