#ifndef SUMA_BRAINWRAP_INCLUDED
#define SUMA_BRAINWRAP_INCLUDED

typedef enum { SUMA_3dSS_NO_PAUSE = 0, SUMA_3dSS_DEMO_PAUSE, SUMA_3dSS_INTERACTIVE } SUMA_3DSS_MODES;

float SUMA_LoadPrepInVol (SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt, SUMA_SurfaceObject **SOhull);
int SUMA_Find_IminImax (float *xyz, float *dir, SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt, int ni, 
                        float *MinMax, float *MinMax_dist , float *MinMax_over, float *MinMax_over_dist,
                        float *Means, float *undershish, float *overshish, int *fvecind_under, int *fvecind_over, 
                        float d1, float d4, int ShishMax);
int SUMA_SkullMask (SUMA_SurfaceObject *SO, SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt, SUMA_COMM_STRUCT *cs);
int SUMA_StretchToFitLeCerveau (SUMA_SurfaceObject *SO, SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt, SUMA_COMM_STRUCT *cs);
byte *SUMA_FindVoxelsInSurface_SLOW (SUMA_SurfaceObject *SO, SUMA_VOLPAR *VolPar, int *N_inp, int fillhole) ;
short *SUMA_SurfGridIntersect (SUMA_SurfaceObject *SO, float *NodeIJKlist, SUMA_VOLPAR *VolPar, int *N_inp, int fillhole, THD_3dim_dataset *fillholeset);
short *SUMA_FindVoxelsInSurface (SUMA_SurfaceObject *SO, SUMA_VOLPAR *VolPar, int *N_inpnt, int  fillhole, THD_3dim_dataset *fillholeset) ;
int SUMA_PushToEdge(SUMA_SurfaceObject *SO, SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt, float limtouch, SUMA_COMM_STRUCT *cs, int agressive) ;
int SUMA_PushToOuterSkull(SUMA_SurfaceObject *SO, SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt, float limtouch, SUMA_COMM_STRUCT *cs, int N_itermax) ;
int SUMA_PushToInnerSkull(SUMA_SurfaceObject *SO, SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt, float limtouch, SUMA_COMM_STRUCT *cs) ;
int SUMA_Reposition_Touchup(SUMA_SurfaceObject *SO, SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt, float limtouch, SUMA_COMM_STRUCT *cs) ;
float *SUMA_Suggest_Touchup(SUMA_SurfaceObject *SO, SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt, float limtouch, SUMA_COMM_STRUCT *cs, int *N_touch);
float *SUMA_Suggest_Touchup_Grad(SUMA_SurfaceObject *SO, SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt, float limtouch, SUMA_COMM_STRUCT *cs, int *N_touch);
float *SUMA_Suggest_Touchup_PushEdge(SUMA_SurfaceObject *SO, 
                                    SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt, 
                                    float limtouch, SUMA_COMM_STRUCT *cs, int *N_touch);
float *SUMA_Suggest_Touchup_PushOuterSkull(SUMA_SurfaceObject *SO, 
                                    SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt, 
                                    float limtouch, SUMA_COMM_STRUCT *cs, int *N_touch, int priorityatnode);
float *SUMA_Suggest_Touchup_PushInnerSkull(SUMA_SurfaceObject *SO, 
                                    SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt, 
                                    float limtouch, SUMA_COMM_STRUCT *cs, int *N_touch);
SUMA_Boolean SUMA_LimitCoordToVolume(float *NewCoord,          
                                     THD_3dim_dataset *in_volp,
                                     int units,
                                     int *limited);
int SUMA_DidUserQuit(void);
EDIT_options *SUMA_BlankAfniEditOptions(void);
void *SUMA_Push_Nodes_To_Hull(SUMA_SurfaceObject *SO, SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt, SUMA_COMM_STRUCT *cs, int N_itermax);
SUMA_Boolean SUMA_3dedge3(THD_3dim_dataset *inset, float *emask, THD_3dim_dataset **poutsetp);

/*!
   SUMA_WRAP_BRAIN_SMOOTH(niter, bufp1, bufp2);
   \brief a chunk used in two places in SUMA_BrainWrap.
   Requires two float pointers than can be null on the first call
   but must be freed at the end 
*/
#define SUMA_WRAP_BRAIN_SMOOTH_NN(niter, dsmooth, refNodeList, nmask){ \
   SUMA_SurfaceObject m_SOref;   \
   int m_in;   \
   float *m_a, m_P2[2][3], m_U[3], m_Un, m_Rref, m_R, m_Dr, m_Dn;  \
   if (!refNodeList) {  \
      refNodeList = (float *)SUMA_malloc(sizeof(float)*SO->N_Node*3);   \
      if (!refNodeList) { \
         SUMA_SL_Crit("Failed to allocate for refNodeList!"); \
         SUMA_RETURN(NOPE); }   \
   }  \
   SUMA_SO_RADIUS(SO, m_Rref);   /* get the radius before shrinking */\
   memcpy(  (void*)refNodeList, (void *)SO->NodeList, \
            SO->N_Node * 3 * sizeof(float)); /* copy original coords */ \
   dsmooth = SUMA_NN_GeomSmooth( SO, niter, SO->NodeList, 3, \
                                 SUMA_ROW_MAJOR, dsmooth, cs, nmask, 1);    \
   memcpy((void*)SO->NodeList, (void *)dsmooth, SO->N_Node * 3 * sizeof(float));    /* copy smoothed surface coords */  \
   /* matching area fails for some reason. */\
   if (0) { \
      /* just grow surface by 3 mm outward, fails for meshes of different size and different degrees of smoothing*/  \
      for (m_in=0; m_in < SO->N_Node; ++m_in) { \
         m_a = &(SO->NodeList[3*m_in]); \
         SUMA_UNIT_VEC(SO->Center, m_a, m_U, m_Un);\
         SUMA_POINT_AT_DISTANCE_NORM(m_U, SO->Center, (m_Un+2), m_P2);   \
         SO->NodeList[3*m_in] = m_P2[0][0]; SO->NodeList[3*m_in+1] = m_P2[0][1]; SO->NodeList[3*m_in+2] = m_P2[0][2];   \
      }  \
   }  else {   /* an approximate resizing based on radius */\
      SUMA_SO_RADIUS(SO, m_R);   /* get the radius after shrinking */\
      m_Dr = ( m_Rref - m_R ) / m_Rref;   \
      for (m_in=0; m_in < SO->N_Node; ++m_in) { \
         if (!nmask || nmask[m_in]) {  \
            m_a = &(SO->NodeList[3*m_in]); \
            SUMA_UNIT_VEC(SO->Center, m_a, m_U, m_Un);\
            m_Dn = m_Dr*m_Un + m_Un;   \
            if (m_Un) { \
               SUMA_POINT_AT_DISTANCE_NORM(m_U, SO->Center, m_Dn, m_P2);  \
               SO->NodeList[3*m_in] = m_P2[0][0]; SO->NodeList[3*m_in+1] = m_P2[0][1]; SO->NodeList[3*m_in+2] = m_P2[0][2];   \
            }  \
         }\
      }  \
   }  \
   SUMA_RECOMPUTE_NORMALS(SO);   \
}

/* Area matching is failing for some reason (negative area patches ?) */
#define SUMA_WRAP_BRAIN_SMOOTH_MATCHAREA(niter, dsmooth, refNodeList, nmask){ \
   SUMA_SurfaceObject m_SOref;   \
   if (!refNodeList) {  \
      refNodeList = (float *)SUMA_malloc(sizeof(float)*SO->N_Node*3);   \
      if (!refNodeList) {  \
         SUMA_SL_Crit("Failed to allocate for refNodeList!"); \
         SUMA_RETURN(NOPE); \
      }   \
   }  \
   memcpy(  (void*)refNodeList, (void *)SO->NodeList,    \
            SO->N_Node * 3 * sizeof(float)); /* copy original surface coords */\
   dsmooth = SUMA_NN_GeomSmooth( SO, niter, SO->NodeList, \
                                 3, SUMA_ROW_MAJOR, dsmooth, cs, nmask,1);    \
   memcpy((void*)SO->NodeList, (void *)dsmooth, \
            SO->N_Node * 3 * sizeof(float));\
   /* copy smoothed surface coords */  \
   m_SOref.PolyArea = NULL; m_SOref.NodeDim = 3; m_SOref.FaceSetDim = 3;   \
   m_SOref.NodeList = refNodeList; m_SOref.N_Node = SO->N_Node;   \
   m_SOref.FaceSetList = SO->FaceSetList; m_SOref.N_FaceSet = SO->N_FaceSet;  \
   m_SOref.idcode_str = SO->idcode_str; m_SOref.Label = SO->Label;\
   SUMA_EquateSurfaceAreas(SO, &m_SOref, 0.1, cs);  \
   SUMA_RECOMPUTE_NORMALS(SO);   \
}

/*!
   this one is too wimpy to get rid of folding 
*/
#define SUMA_WRAP_BRAIN_SMOOTH_TAUB(niter, dsmooth, refNodeList){ \
      dsmooth = SUMA_Taubin_Smooth( SO, NULL,   \
                                    0.6307, -.6732, SO->NodeList, \
                                    niter, 3, SUMA_ROW_MAJOR, dsmooth, cs);   \
      memcpy((void*)SO->NodeList, (void *)dsmooth, SO->N_Node * 3 * sizeof(float)); \
}



/*!
   Stop using this monster, use SUMA_Reposition_Touchup
*/
#define SUMA_REPOSITION_TOUCHUP(limtouch){  \
      int m_in, m_N_troub = 0, m_cond1=0, m_cond2=0, m_cond3 = 0;   \
      float m_MinMax_over[2], m_MinMax[2], m_MinMax_dist[2], m_MinMax_over_dist[2], m_Means[3], m_tb; \
      float m_U[3], m_Un, *m_a, m_P2[2][3], *m_norm, m_shft; \
      float *m_touchup=NULL, **m_wgt=NULL, *m_dsmooth=NULL; \
      m_touchup = (float *)SUMA_calloc(SO->N_Node, sizeof(float));   \
      if (!m_touchup) { SUMA_SL_Crit("Failed to allocate"); exit(1); }  \
      for (m_in=0; m_in<SO->N_Node; ++m_in) {   \
         SUMA_Find_IminImax(&(SO->NodeList[3*m_in]), &(SO->NodeNormList[3*m_in]), Opt, m_in,  m_MinMax, m_MinMax_dist, m_MinMax_over, m_MinMax_over_dist, m_Means, NULL, NULL, NULL, NULL, 0); \
         /* Shift the node outwards:   \
            0- the minimum location of the minimum over the node is not 0
            AND
            1- if the minimum over the node is closer than the minimum below the node  or the internal minimum is more than 1mm away \
               and the internal min is masked to 0 or the minimum below the node just sucks, > tb and the minimum over the node is < tb\
               The second part of the condition is meant to better model a finger of cortex that has enough csf on the inner surface \
               to have it be set to 0 by SpatNorm this dark csf would keep the brain surface from reaching the outer surface \
            AND   \
            2- if the maximum over the node is much higher than the maximum below the node (fat on skull), \
               it must be beyond the location of the minimum over the node   \
            AND   \
            3- The new position of the node is not much larger than the current value
         */ \
         m_tb = (m_MinMax[1] - Opt->t2) * 0.5 + Opt->t2; \
         m_cond1 = (    (m_MinMax_over_dist[0] < m_MinMax_dist[0]) || (m_MinMax_dist[0] > 1 && m_MinMax[0] >= m_MinMax_over[0] ) \
                     || (m_MinMax[0] > m_tb && m_MinMax_over[0] < m_MinMax[0]) ); \
         if (m_MinMax_over[1] > m_MinMax[1] && m_MinMax_over[1] > 0.9 * Opt->t98 && (m_MinMax_over_dist[1] < m_MinMax_over_dist[0]) ) m_cond2 = 0;  \
         else m_cond2 = 1; \
         if (m_MinMax_over[0] > 1.2 * m_Means[0]) m_cond3 = 0;\
         else m_cond3 = 1; \
         if (Opt->NodeDbg == m_in) {   \
               m_a = &(SO->NodeList[3*m_in]);   \
               fprintf(SUMA_STDERR, "%s: Debug during touchup for node %d:\n"   \
                                    " MinMax     =[%.1f,%.1f] MinMax_dist     = [%.2f,%.2f] \n"   \
                                    " MinMax_over=[%.1f,%.1f] MinMax_over_dist= [%.2f,%.2f] \n"   \
                                    " Val at node %.1f, mean below %.1f, mean above %.1f\n"  \
                                    " zalt= %f, r/2 = %f\n"    \
                                    " Conditions: %f %d %d %d\n",    \
                       FuncName, m_in, \
                       m_MinMax[0], m_MinMax[1], m_MinMax_dist[0], m_MinMax_dist[1], \
                       m_MinMax_over[0], m_MinMax_over[1], m_MinMax_over_dist[0], m_MinMax_over_dist[1], \
                       m_Means[0], m_Means[1], m_Means[2],   \
                       m_a[2] - SO->Center[2], Opt->r/2, \
                       m_MinMax_over_dist[0] , m_cond1 , m_cond2, m_cond3); \
         }  \
         if (m_MinMax_over_dist[0] && m_cond1 && m_cond2 && m_cond3) {   \
               m_a = &(SO->NodeList[3*m_in]);   \
               /* reposition nodes */  \
               if (0 && LocalHead) { fprintf(SUMA_STDERR, "%s: Suggest repair for node %d (better min above):\n"   \
                                    " MinMax     =[%.1f,%.1f] MinMax_dist     = [%.2f,%.2f] \n"   \
                                    " MinMax_over=[%.1f,%.1f] MinMax_over_dist= [%.2f,%.2f] \n"   \
                                    " Val at node %.1f, mean below %.1f, mean above %.1f\n"  \
                                    " zalt= %f, r/2 = %f\n",    \
                       FuncName, m_in, \
                       m_MinMax[0], m_MinMax[1], m_MinMax_dist[0], m_MinMax_dist[1], \
                       m_MinMax_over[0], m_MinMax_over[1], m_MinMax_over_dist[0], m_MinMax_over_dist[1], \
                       m_Means[0], m_Means[1], m_Means[2],   \
                       m_a[2] - SO->Center[2], Opt->r/2); } \
               {  /* Used to insist on value and top half if (m_MinMax_over_dist[0] && (m_a[2] - SO->Center[2] > 0))  */\
                                             /* to avoid going into eye balls */\
                                             /* Then added !SUMA_IS_EYE_ZONE(m_a,SO->Center) to avoid eyes */  \
                                             /* but that is no longer necessary with fancier expansion conditions. */\
                  m_touchup[m_in] = m_MinMax_over_dist[0]; \
               }  \
            ++m_N_troub;   \
         }  \
         /* nodes in the front of the brain that are sitting in the midst of a very bright area \
         should be moved further down until they hit the brain vs non brain area*/ \
      }  \
      if (LocalHead) fprintf (SUMA_STDERR,"%s: ********************* %d troubled nodes found\n", FuncName, m_N_troub); \
      if (1){ \
         /* fix the big bumps: Bad for brain surface that has lots of bump on top, like 201, 
         that kind of smoothing is better performed on an inner model of the skull .... Someday ....*/\
         if (LocalHead) fprintf (SUMA_STDERR,"%s: ********************* Now filtering...\n", FuncName); \
         m_wgt = SUMA_Chung_Smooth_Weights(SO);   \
         if (!m_wgt) { \
            SUMA_SL_Err("Failed to compute weights.\n"); \
            exit(1); \
         }  \
         m_dsmooth = SUMA_Chung_Smooth (SO, m_wgt, 200, 20, m_touchup, 1, SUMA_COLUMN_MAJOR, NULL, ps->cs, NULL);   \
         if (LocalHead) fprintf (SUMA_STDERR,"%s: ********************* filtering done.\n", FuncName);   \
      } else { \
         m_dsmooth = m_touchup; m_touchup = NULL;  \
      }  \
      /* add the changes */   \
      for (m_in=0; m_in<SO->N_Node; ++m_in) {   \
         m_a = &(SO->NodeList[3*m_in]);   \
         if (1 || !SUMA_IS_EYE_ZONE(m_a,SO->Center)) { \
            if (m_a[2] - SO->Center[2] > 10 )  m_shft = m_touchup[m_in]; /* no smooth baby for top where bumpy sulci can occur */ \
            else m_shft = m_dsmooth[m_in];   \
            if (m_shft) { \
               m_a = &(SO->NodeList[3*m_in]);   \
               m_norm = &(SO->NodeNormList[3*m_in]);  \
               SUMA_POINT_AT_DISTANCE(m_norm, m_a, SUMA_MIN_PAIR(m_shft, limtouch), m_P2);   \
               SO->NodeList[3*m_in] = m_P2[0][0]; SO->NodeList[3*m_in+1] = m_P2[0][1]; SO->NodeList[3*m_in+2] = m_P2[0][2];   \
               if (0 && LocalHead) fprintf(SUMA_STDERR,"%s: Acting on node %d because it is in top half, boosting by %f\n", \
                        FuncName, m_in, SUMA_MIN_PAIR(m_shft, limtouch));   \
            }\
         }  \
      }  \
      if (m_touchup) SUMA_free(m_touchup); m_touchup = NULL;   \
      if (m_dsmooth) SUMA_free(m_dsmooth); m_dsmooth = NULL;   \
      if (m_wgt) SUMA_free2D ((char **)m_wgt, SO->N_Node); m_wgt = NULL;   \
}

#define SUMA_IS_EYE_ZONE(n,c) ( ( ( (n)[1] - (c)[1] ) < -10 && ( (n)[2] - (c)[2] ) < 0 ) ? 1 : 0 ) 
#define SUMA_IS_LOWER_ZONE(n,c) ( ( ( (n)[2] - (c)[2] ) < 10 ) ? 1 : 0 ) 

/*!
   finds the mean coordinate of the neighboring nodes (not including node i)
*/
#define SUMA_MEAN_NEIGHB_COORD(SO, i, mnc) { \
   int m_j, m_in; \
   mnc[0] = 0; mnc[1] = 0; mnc[2] = 0; \
   for (m_j = 0; m_j < SO->FN->N_Neighb[i]; ++m_j) {  \
      m_in = 3*SO->FN->FirstNeighb[i][m_j]; \
      mnc[0] += SO->NodeList[m_in]; \
      mnc[1] += SO->NodeList[m_in+1]; \
      mnc[2] += SO->NodeList[m_in+2]; \
   }  \
   mnc[0] = mnc[0] / (SO->FN->N_Neighb[i]); mnc[1] = mnc[1] / (SO->FN->N_Neighb[i]);  mnc[2] = mnc[2] / (SO->FN->N_Neighb[i]); \
}
/*!
   find the mean segment length 
*/
#define SUMA_MEAN_SEGMENT_LENGTH(SO, l) { \
   int m_cnt=0, m_j;   \
   float *m_p1, *m_p2, m_d=0.0, m_v[3];  \
   l = 0.0; \
   for (m_j=0; m_j < SO->EL->N_EL; ++m_j) {  \
      if (SO->EL->ELps[m_j][2] >= 0) { \
         m_p1 = &(SO->NodeList[3*SO->EL->EL[m_j][0]]); m_p2 = &(SO->NodeList[3*SO->EL->EL[m_j][1]]); \
         SUMA_UNIT_VEC(m_p1, m_p2, m_v, m_d);   \
         l += m_d;   \
         ++m_cnt; \
      }  \
   }  \
   if (m_cnt) l /= m_cnt; \
}

/*!
   find the earliest max step, quit looking beyond a threshold
*/
#define SUMA_MAX_SHISH_JUMP(vec, diffmax, i_diffmax, thresh_clip){  \
   int m_kk = 1, m_thresh = 0;   \
   float m_diff;  \
   m_diff = 0; diffmax = 0;  i_diffmax = 0;\
   while (vec[m_kk] >=0 && m_kk < ShishMax && !m_thresh) {   \
      m_diff = vec[m_kk] - vec[m_kk - 1]; \
      if (m_diff > diffmax) {diffmax = m_diff; i_diffmax = m_kk-1; }  \
      if (diffmax > thresh_clip) m_thresh = 1;   \
      ++ m_kk; \
   }  \
}

/*!
   find the earliest max negative step
*/
#define SUMA_MAX_NEG_SHISH_JUMP(vec, diffmax, i_diffmax){  \
   int m_kk = 1, m_thresh = 0;   \
   float m_diff;  \
   m_diff = 0; diffmax = 0;  i_diffmax = 0;\
   while (vec[m_kk] >=0 && m_kk < ShishMax && !m_thresh) {   \
      m_diff = vec[m_kk] - vec[m_kk - 1]; \
      if (m_diff < diffmax) {\
         diffmax = m_diff; i_diffmax = m_kk-1; \
      }  \
      ++ m_kk; \
   }  \
}
/*!
   find the number of negative jumps above a certain threshold 
*/
#define SUMA_MIN_SHISH_JUMP(vec, diffmin, i_diffmin, thresh_clip, N_neg){  \
   int m_kk = 1;   \
   float m_diff;  \
   m_diff = 0; diffmin = 0;  i_diffmin = 0; N_neg=0;\
   while (vec[m_kk] >=0 && m_kk < ShishMax ) {   \
      m_diff = vec[m_kk] - vec[m_kk - 1]; \
      if (m_diff < diffmin) { diffmin = m_diff; i_diffmax = m_kk-1; }  \
      if (SUMA_ABS(diffmin) > thresh_clip) ++N_neg;   \
      ++ m_kk; \
   }  \
}

/*! \brief fill a shish d units lond starting at xyz and moving along direction U
   
      \param XYZ coordinates of starting point
      \param U the famed direction vector
      \param in_vol AFNI volume structure
      \param fvec data vector 
      \param stp size of the step to take 
      \param n_stp number of steps to take
      \param nxx, nxy number of voxels in the x and x*y directions
      \param shish a vector to store fvec's values as it crosses them 
      \param N_shishmax (int) maximum number of values allowed in shish
*/

#define SUMA_ONE_SHISH_PLEASE(nl, U, in_vol, fvec, stp, n_stp, nxx, nxy, shish, N_shishmax){\
   int m_istep, m_nind;   \
   static THD_fvec3 m_ncoord, m_ndicom; \
   static THD_ivec3 m_nind3;  \
   static float m_jmp, m_td[3];  \
   m_istep = 0; \
   m_jmp = 0.0;  \
   while (m_istep <= n_stp) {   \
      m_td[0] = m_jmp * U[0]; m_td[1] = m_jmp * U[1]; m_td[2] = m_jmp * U[2]; \
      /* get 1d coord of point */   \
      m_ndicom.xyz[0] = nl[0] + m_td[0] ; m_ndicom.xyz[1] = nl[1]+ m_td[1]; m_ndicom.xyz[2] = nl[2]+ m_td[2];  \
      m_ncoord = THD_dicomm_to_3dmm(in_vol, m_ndicom);   \
      m_nind3 = THD_3dmm_to_3dind(in_vol, m_ncoord);  \
      m_nind = m_nind3.ijk[0] + m_nind3.ijk[1] * nxx + m_nind3.ijk[2] * nxy;  \
      if (m_istep < N_shishmax) shish[m_istep] = fvec[m_nind];  \
      else break; \
      m_jmp += stp;  \
      ++m_istep;  \
   }  \
   if (m_istep < N_shishmax) shish[m_istep] = -1;  \
}


#endif
