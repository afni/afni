#include "SUMA_suma.h"
#include "../thd_brainormalize.h"

#undef STAND_ALONE

#if defined SUMA_BrainWrap_STANDALONE
#define STAND_ALONE 
#endif

#ifdef STAND_ALONE
/* these global variables must be declared even if they will not be used by this main */
SUMA_SurfaceViewer *SUMAg_cSV = NULL; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
SUMA_DO *SUMAg_DOv = NULL;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF = NULL; /*!< Global pointer to structure containing info common to all viewers */
#else
extern SUMA_CommonFields *SUMAg_CF;
extern SUMA_DO *SUMAg_DOv;
extern SUMA_SurfaceViewer *SUMAg_SVv;
extern int SUMAg_N_SVv; 
extern int SUMAg_N_DOv;  
#endif

/* 
   volume returned is negative if failed to read volume. 
   Volume units are in mm3
   \brief Volume preparations for later stripping:
   -1 Run SpatNorm (Not done here at the moment)
   -2 Get percentile ranges and sphere center a la BET 
      (Fast robust automated brain extraction, Stephen M. Smith, HBM 2002 v 17:3 pp 143-155)
   -3 Get convex hull of outer surface and shrink to set limit on expansion
*/         
float SUMA_LoadPrepInVol (SUMA_ISOSURFACE_OPTIONS *Opt, SUMA_SurfaceObject **SOhull)
{
   static char FuncName[]={"SUMA_LoadPrepInVol"};
   int orient, i, nxx, nxy, cnt, *isort = NULL,iPercRangeVal[2], tind, znxy, *ijk=NULL, nf = 0, trouble= 0, Force = 0;
   THD_fvec3 ncoord, ndicom;
   THD_ivec3 nind3;
   float vol=-1, mass=-1, volmass = -1, *CoordList = NULL;
   double PercRange[2], PercRangeVal[2], *dvec_sort=NULL, *dvec=NULL;
   SUMA_ISINSPHERE IsIn;
   SUMA_SurfaceObject *SOhullp = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (Opt->debug > 2) LocalHead = YUP;
   
   Opt->in_vol = THD_open_dataset( Opt->in_name );
   if (!ISVALID_DSET(Opt->in_vol)) {
      if (!Opt->in_name) {
         SUMA_SL_Err("NULL input volume.");
         SUMA_RETURN(vol);
      } else {
         SUMA_SL_Err("invalid volume.");
         SUMA_RETURN(vol);
      }
   } else if ( DSET_BRICK_TYPE(Opt->in_vol, 0) == MRI_complex) {
      SUMA_SL_Err("Can't do complex data.");
      SUMA_RETURN(vol);
   }
   
   Opt->nvox = DSET_NVOX( Opt->in_vol );
   if (DSET_NVALS( Opt->in_vol) != 1) {
      SUMA_SL_Err("Input volume can only have one sub-brick in it.\nUse [.] selectors to choose sub-brick needed.");
      SUMA_RETURN(vol);
   }
   
   Opt->VolCM[0] = XCM; Opt->VolCM[1] = YCM; Opt->VolCM[2] = ZCM;
   
   /* load the dset */
   DSET_load(Opt->in_vol);
   Opt->dvec = (double *)SUMA_malloc(sizeof(double) * Opt->nvox);
   if (!Opt->dvec) {
      SUMA_SL_Crit("Faile to allocate for dvec.\nOh misery.");
      SUMA_RETURN(NOPE);
   }
   EDIT_coerce_scale_type( Opt->nvox , DSET_BRICK_FACTOR(Opt->in_vol,0) ,
                           DSET_BRICK_TYPE(Opt->in_vol,0), DSET_ARRAY(Opt->in_vol, 0) ,      /* input  */
                           MRI_double               , Opt->dvec  ) ;   /* output */
   
   /* estimate the t2, t98 and tm parameters, should do them regionally by Bob's octant method */
   if (Opt->t2 < 0) {
      PercRange[0] = 2; PercRange[1] = 98;
      dvec_sort = SUMA_dPercRange (Opt->dvec, NULL, Opt->nvox, PercRange, PercRangeVal, iPercRangeVal);
      if (!dvec_sort) {
         SUMA_SL_Err("Failed to find range");
         SUMA_RETURN(NOPE);
      }
      Opt->t2 = PercRangeVal[0]; Opt->t98 = PercRangeVal[1];
      if (!Opt->t98) {
         SUMA_SL_Err("98 percentile is 0!\nWhat kind of dataset is this?");
         SUMA_RETURN(NOPE);
      }
      
      Opt->t = Opt->t2 + 0.1 * (Opt->t98 - Opt->t2);
   
      Opt->tm = -1;
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Wild shot tm = %f\n", FuncName, dvec_sort[(iPercRangeVal[0]+iPercRangeVal[1])/2]);
      if (dvec_sort) SUMA_free(dvec_sort); dvec_sort = NULL;
   } else {
      /* user specified */
   }

   /* estimate the volume */
   CoordList = (float *)SUMA_calloc(3 * Opt->nvox, sizeof(float) );
   if (!CoordList) {
      SUMA_SL_Err("Failed to allocate CoordList");
      SUMA_RETURN(vol);
   }
   Opt->cog[0] = Opt->cog[1] = Opt->cog[2] = 0;
   nxx = DSET_NX(Opt->in_vol);
   nxy = DSET_NX(Opt->in_vol) * DSET_NY(Opt->in_vol);
   vol = 0.0; volmass = 0;
   for (i=0; i<Opt->nvox; ++i) { 
      /* coord of point */
         nind3.ijk[2] = (int)(i/nxy); znxy = nind3.ijk[2] * nxy;
         nind3.ijk[1] = ( i - znxy ) / nxx;
         nind3.ijk[0] = i - nind3.ijk[1] * nxx - znxy;
         ncoord = THD_3dind_to_3dmm(Opt->in_vol, nind3);
         ndicom = THD_3dmm_to_dicomm(Opt->in_vol,ncoord);
         CoordList[3*i] = ndicom.xyz[0];
         CoordList[3*i+1] = ndicom.xyz[1];
         CoordList[3*i+2] = ndicom.xyz[2];
      if (Opt->dvec[i] > Opt->t) {
         mass = SUMA_MIN_PAIR(Opt->t98, Opt->dvec[i]);
         Opt->cog[0] += mass * ndicom.xyz[0];
         Opt->cog[1] += mass * ndicom.xyz[1];
         Opt->cog[2] += mass * ndicom.xyz[2];
         volmass += mass;
         ++vol;
      } 
   }
   Opt->cog[0] = Opt->cog[0]/volmass; Opt->cog[1] = Opt->cog[1]/volmass; Opt->cog[2] = Opt->cog[2]/volmass; 
   if (LocalHead) {
      fprintf (SUMA_STDERR,"%s: COG %f %f %f\n",FuncName, Opt->cog[0], Opt->cog[1], Opt->cog[2]); 
   }
   vol *= fabs(DSET_DX(Opt->in_vol) * DSET_DY(Opt->in_vol) * DSET_DZ(Opt->in_vol) );
   
   /* find the radius */
   Opt->r = pow(vol*3.0/(3.14159*4.0), 1/3.0);
   
   /* form a vector of values inside the sphere */
   if (Opt->tm < 0) {
      /* create a mask of the voxels inside the sphere of radius r */
      IsIn =  SUMA_isinsphere (CoordList, Opt->nvox, Opt->cog , Opt->r , 1 );
      dvec = (double *)SUMA_malloc(Opt->nvox * sizeof(double));
      if (!dvec) { SUMA_SL_Crit("Failed to allocate for dvec"); SUMA_RETURN(-1); }
      cnt = 0;
      for (i=0; i<IsIn.nIsIn; ++i) { 
         dvec[cnt] = Opt->dvec[IsIn.IsIn[i]]; ++cnt;
      }
      /* now sort dvec */
      isort = SUMA_z_doubqsort(dvec, cnt);
      Opt->tm = dvec[cnt/2];
      if (LocalHead) {
         fprintf (SUMA_STDERR,"%s: Real tm %f\nt2 = %f, t = %f, t98 = %f\n", FuncName, Opt->tm, Opt->t2, Opt->t, Opt->t98);
      }
      if (dvec) SUMA_free(dvec); dvec = NULL;
      if (isort) SUMA_free(isort); isort = NULL;
      SUMA_Free_IsInSphere(&IsIn);
      
   }
   
   /* kill values over 98 % to help not catch the eyes*/
   if (Opt->Kill98) {
      int max_mask, cnt;
      if (LocalHead) SUMA_SL_Note("Killing values > 98");
      max_mask = (int)(Opt->nvox*0.1);
      Opt->k98mask = (int *)SUMA_calloc(max_mask, sizeof(int));
      if (!Opt->k98mask) {
         SUMA_SL_Crit("Failed to allocate");
         SUMA_RETURN(-1);
      }
      cnt = 0;
      for (i=0; i<Opt->nvox; ++i) {
         if (Opt->dvec[i] >= Opt->t98) {
            Opt->dvec[i] = 0;
            if (cnt == max_mask) {
               SUMA_SL_Warn("Too many high values in dset!");
               max_mask *= 2;
               Opt->k98mask = (int *)SUMA_realloc(Opt->k98mask, max_mask * sizeof(int));
               if (!Opt->k98mask) { SUMA_SL_Crit("Failed to allocate"); SUMA_RETURN(-1); }
            } 
            Opt->k98mask[cnt] = i; ++cnt;
         }  
      }
      Opt->k98maskcnt = cnt;
   }
   
   /* use CoordList to build a convex hull */
   if (*SOhull) {
      byte * isskin = NULL;
      int N_skin;
      if (LocalHead) SUMA_SL_Note("Computing Convex Hull ..");
      isskin = SUMA_isSkin(Opt->in_vol, Opt->dvec, Opt->t2, &N_skin);
      if (!N_skin || !isskin) {
         SUMA_SL_Err("Failed to find skin!\nIgnoring skull limit.");
      } else {
         cnt = 0;
         for (i=0; i<Opt->nvox; ++i) { 
            if (isskin[i] > Opt->t2) {
               CoordList[3*cnt] = CoordList[3*i] ;
               CoordList[3*cnt+1] = CoordList[3*i+1] ;
               CoordList[3*cnt+2] = CoordList[3*i+2] ;
               ++cnt;
            }
         }
         if (! (nf = SUMA_qhull_wrap(cnt, CoordList, &ijk, 1)) ) {
            fprintf(SUMA_STDERR,"Error %s:\nFailed in SUMA_qhull_wrap\n", FuncName);
            *SOhull = NULL;
         } else {
            fprintf(SUMA_STDERR,"%s:\nForming hull.\n", FuncName);
            *SOhull = SUMA_Patch2Surf(CoordList, cnt, ijk, nf, 3);
            SOhullp = *SOhull;
            #if 0
            if (LocalHead) fprintf(SUMA_STDERR,"%s: Making hull consistently orientated\n", FuncName);
            SOhullp->EL = SUMA_Make_Edge_List_eng (SOhullp->FaceSetList, SOhullp->N_FaceSet, SOhullp->N_Node, SOhullp->NodeList, 1, NULL);
            if (!SUMA_MakeConsistent (SOhullp->FaceSetList, SOhullp->N_FaceSet, SOhullp->EL, 0, &trouble)) {
               SUMA_SL_Err("Failed in SUMA_MakeConsistent");
            }
            if (LocalHead) fprintf(SUMA_STDERR,"%s: Checking orientation.\n", FuncName);
            SUMA_SL_Warn("Stuff shaky here, Attend to it.");
            Force = 1; 
            orient = SUMA_OrientTriangles (SOhullp->NodeList, SOhullp->N_Node, SOhullp->FaceSetList, SOhullp->N_FaceSet, 1, Force);
            if (orient < 0 || Force) { /* flipping was done, dump the edge list */ if (SOhullp->EL) SUMA_free_Edge_List(SOhullp->EL); SOhullp->EL = NULL; }
            if (!orient) { fprintf(SUMA_STDERR,"Error %s:\nFailed in SUMA_OrientTriangles\n", FuncName); }
            if (LocalHead) {
               if (orient < 0) { SUMA_SL_Note("Hull was reoriented"); }
               else { SUMA_SL_Note("Hull was properly oriented"); }
            }
            #endif 
               
            if (LocalHead) fprintf(SUMA_STDERR,"%s: Refining hull surface.\n", FuncName);
            if (LocalHead) fprintf(SUMA_STDERR,"%s: %d nodes, %d triangles.\n", FuncName, SOhullp->N_Node, SOhullp->N_FaceSet);
            /* SUMA_Print_Surface_Object(SOhullp, stderr); */
            /* if (!SUMA_Subdivide_Mesh(&(SOhullp->NodeList), &(SOhullp->N_Node), &(SOhullp->FaceSetList), &(SOhullp->N_FaceSet), 50)) { */
            if (0) {
               SUMA_SL_Err("Failed to subdivide mesh");
               SUMA_Free_Surface_Object (SOhullp); 
               *SOhull = NULL;
            } else {
               /* SUMA_Print_Surface_Object(SOhullp, stderr); */
               if (SOhullp->NodeNormList) SUMA_free(SOhullp->NodeNormList); SOhullp->NodeNormList = NULL;
               if (SOhullp->FaceNormList) SUMA_free(SOhullp->FaceNormList); SOhullp->FaceNormList = NULL;
               if (SOhullp->glar_NodeList) SUMA_free(SOhullp->glar_NodeList); SOhullp->glar_NodeList = NULL;
               if (SOhullp->glar_FaceSetList) SUMA_free(SOhullp->glar_FaceSetList); SOhullp->glar_FaceSetList = NULL;
               if (SOhullp->glar_FaceNormList) SUMA_free(SOhullp->glar_FaceNormList); SOhullp->glar_FaceNormList = NULL;
               if (SOhullp->glar_NodeNormList) SUMA_free(SOhullp->glar_NodeNormList); SOhullp->glar_NodeNormList = NULL;
               if (LocalHead) fprintf(SUMA_STDERR,"%s: %d nodes, %d triangles.\n", FuncName, SOhullp->N_Node, SOhullp->N_FaceSet);
               SUMA_RECOMPUTE_NORMALS((SOhullp));
               if (!SUMA_SurfaceMetrics(SOhullp, "EdgeList,MemberFace,PolyArea", NULL)) {
                  fprintf(SUMA_STDERR,"Error %s: Error in SUMA_SurfaceMetrics\n", FuncName);
                  SUMA_Free_Surface_Object (SOhullp); /* that takes care of freeing leftovers in MF */
                  *SOhull = NULL;
               }
            }

         }
      
         if (ijk) free(ijk); ijk=NULL;
         if (isskin) SUMA_free(isskin); isskin = NULL;
      }
   }
   
   if (CoordList) SUMA_free(CoordList); CoordList = NULL;
   SUMA_RETURN(vol);
}

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
      \param dvec data vector 
      \param stp size of the step to take 
      \param n_stp number of steps to take
      \param nxx, nxy number of voxels in the x and x*y directions
      \param shish a vector to store dvec's values as it crosses them 
      \param N_shishmax (int) maximum number of values allowed in shish
*/

#define SUMA_ONE_SHISH_PLEASE(nl, U, in_vol, dvec, stp, n_stp, nxx, nxy, shish, N_shishmax){\
   int m_istep, m_nind;   \
   static THD_fvec3 m_ncoord, m_ndicom; \
   static THD_ivec3 m_nind3;  \
   static double m_jmp, m_td[3];  \
   m_istep = 0; \
   m_jmp = 0.0;  \
   while (m_istep <= n_stp) {   \
      m_td[0] = m_jmp * U[0]; m_td[1] = m_jmp * U[1]; m_td[2] = m_jmp * U[2]; \
      /* get 1d coord of point */   \
      m_ndicom.xyz[0] = nl[0] + m_td[0] ; m_ndicom.xyz[1] = nl[1]+ m_td[1]; m_ndicom.xyz[2] = nl[2]+ m_td[2];  \
      m_ncoord = THD_dicomm_to_3dmm(in_vol, m_ndicom);   \
      m_nind3 = THD_3dmm_to_3dind(in_vol, m_ncoord);  \
      m_nind = m_nind3.ijk[0] + m_nind3.ijk[1] * nxx + m_nind3.ijk[2] * nxy;  \
      if (m_istep < N_shishmax) shish[m_istep] = dvec[m_nind];  \
      else break; \
      m_jmp += stp;  \
      ++m_istep;  \
   }  \
   if (m_istep < N_shishmax) shish[m_istep] = -1;  \
}

/*!
   \param MinMax (float) 2x1: MinMax[0] minimum under node
                              MinMax_dist[1] maximum under node
   \param MinMax_dist (float) 2x1: MinMax_dist[0] distance of minimum under node
                                   MinMax_dist[1] distance of maximum under node
   \param MinMax_over (float) 2x1: MinMax_over[0] minimum over node
                                   MinMax_over[1] maximum over node
                                   (set this one to NULL if you do not want to search over the node.
                                   In that case all the _over (or above like Means[2]) values will
                                   be undetermined.
   \param MinMax_over_dist (float) 2x1: MinMax_over_dist[0] distance of minimum over node
                                        MinMax_over_dist[1] distance of maximum over node
   \param Means (float *) 3x1 :  Means[0] value at node, 
                                 Means[1] mean value below node,
                                 Means[2] mean value above node  
*/
int SUMA_Find_IminImax (SUMA_SurfaceObject *SO, SUMA_ISOSURFACE_OPTIONS *Opt, int ni, 
                        float *MinMax, float *MinMax_dist , float *MinMax_over, float *MinMax_over_dist,
                        float *Means, float *undershish, float *overshish, int ShishMax)
{
   static char FuncName[]={"SUMA_Find_IminImax"};
   float d1, d2, travdir[3], d1t, d2t, lmin, lmax, lmin_dist, lmax_dist;
   float t2 = Opt->t2, tm = Opt->tm, t = Opt->t; 
   THD_fvec3 ncoord, ndicom;
   THD_ivec3 nind3;
   int nind, istep, istepmax, istep2max, nxx, nxy;
   SUMA_ENTRY;
   
   d1 = Opt->d1; d2 = d1/2.0; 
   
   Means[0] = Means[1] = Means[2] = 0.0;
   lmin = Opt->tm;
   lmin_dist = 0.0;
   lmax_dist = 0.0;
   lmax = Opt->t;
   nxx = DSET_NX(Opt->in_vol);
   nxy = DSET_NX(Opt->in_vol) * DSET_NY(Opt->in_vol);
   istep = 0; istepmax = (int)(ceil((double)d1/Opt->travstp)); istep2max = (int)(ceil((double)d2/Opt->travstp));
   
   while (istep <= istepmax) {
      /* calculate offset */
      travdir[0] = - istep * Opt->travstp * SO->NodeNormList[3*ni]; travdir[1] = -istep * Opt->travstp * SO->NodeNormList[3*ni+1]; travdir[2] = -istep * Opt->travstp * SO->NodeNormList[3*ni+2]; 
      
      /* get 1d coord of point */
      ndicom.xyz[0] = SO->NodeList[3*ni] + travdir[0] ; ndicom.xyz[1] = SO->NodeList[3*ni+1]+ travdir[1]; ndicom.xyz[2] = SO->NodeList[3*ni+2]+ travdir[2]; 
      ncoord = THD_dicomm_to_3dmm(Opt->in_vol, ndicom);
      nind3 = THD_3dmm_to_3dind(Opt->in_vol, ncoord);
      nind = nind3.ijk[0] + nind3.ijk[1] * nxx + nind3.ijk[2] * nxy;
      #if 0
      if (ni == Opt->NodeDbg) {
         fprintf(SUMA_STDERR, "%s: Node %d\n"
                              " nind3 = [%d %d %d] voxVal = %.3f\n", 
                              FuncName, Opt->NodeDbg, nind3.ijk[0], nind3.ijk[1], nind3.ijk[2],
                              Opt->dvec[nind]);
      }
      #endif
      if (undershish && istep < ShishMax) undershish[istep] = Opt->dvec[nind];   /* store values under node */
      
      /* track the mean */
      Means[1] += Opt->dvec[nind];
       
      /* find local min */ 
      /* lmin = SUMA_MIN_PAIR(lmin, Opt->dvec[nind]); */
      if (lmin > Opt->dvec[nind]) { lmin = Opt->dvec[nind]; SUMA_NORM_VEC(travdir, 3, lmin_dist); }
      
      if (istep <= istep2max) {
         if (lmax < Opt->dvec[nind]) { lmax = Opt->dvec[nind]; SUMA_NORM_VEC(travdir, 3, lmax_dist); }  
      }
      
      ++istep;
   }
   
   if (istep < ShishMax) undershish[istep] = -1; /* crown the shish */
   
   Means[1] /= istep;
   MinMax[0] = SUMA_MAX_PAIR(t2, lmin);
   MinMax_dist[0] = lmin_dist;  
   MinMax[1] = SUMA_MIN_PAIR(tm, lmax); 
   MinMax_dist[1] = lmax_dist;
   
   /* search for a minimum overhead */
   if (MinMax_over) {
      lmin = Opt->tm;
      lmin_dist = 0.0;
      lmax_dist = 0.0;
      lmax = Opt->t;
      istep = 0; istepmax = (int)(ceil((double)Opt->d4/Opt->travstp));
      while (istep <= istepmax) {
         /* calculate offset */
         travdir[0] =  istep * Opt->travstp * SO->NodeNormList[3*ni]; travdir[1] = istep * Opt->travstp * SO->NodeNormList[3*ni+1]; travdir[2] = istep * Opt->travstp * SO->NodeNormList[3*ni+2]; 

         /* get 1d coord of point */
         ndicom.xyz[0] = SO->NodeList[3*ni] + travdir[0] ; ndicom.xyz[1] = SO->NodeList[3*ni+1]+ travdir[1]; ndicom.xyz[2] = SO->NodeList[3*ni+2]+ travdir[2]; 
         ncoord = THD_dicomm_to_3dmm(Opt->in_vol, ndicom);
         nind3 = THD_3dmm_to_3dind(Opt->in_vol, ncoord);
         nind = nind3.ijk[0] + nind3.ijk[1] * nxx + nind3.ijk[2] * nxy;
         #if 0
         if (ni == Opt->NodeDbg) {
            fprintf(SUMA_STDERR, "%s: Node %d\n"
                                 " nind3 = [%d %d %d] voxVal = %.3f\n", 
                                 FuncName, Opt->NodeDbg, nind3.ijk[0], nind3.ijk[1], nind3.ijk[2],
                                 Opt->dvec[nind]);
         }
         #endif
         if (!istep) { /* get the value at the node */
            Means[0] = Opt->dvec[nind];
         }
         
         if (overshish && istep < ShishMax) overshish[istep] = Opt->dvec[nind];   /* store values under node */
         
         Means[2] += Opt->dvec[nind]; 
         
         /* find local min and max*/ 
         if (lmin > Opt->dvec[nind]) { lmin = Opt->dvec[nind]; SUMA_NORM_VEC(travdir, 3, lmin_dist); }
         if (lmax < Opt->dvec[nind]) { lmax = Opt->dvec[nind]; SUMA_NORM_VEC(travdir, 3, lmax_dist); }  
         
         ++istep;
      }
      
      if (istep < ShishMax) overshish[istep] = -1; /* crown the shish */

      Means[2] /= istep;
      MinMax_over[0] = lmin; 
      MinMax_over_dist[0] = lmin_dist;  
      MinMax_over[1] = lmax;
      MinMax_over_dist[1] = lmax_dist;
   }
   
   SUMA_RETURN(YUP);
}

/*!
   SUMA_WRAP_BRAIN_SMOOTH(niter, bufp1, bufp2);
   \brief a chunk used in two places in SUMA_BrainWrap.
   Requires two float pointers than can be null on the first call
   but must be freed at the end 
*/
#define SUMA_WRAP_BRAIN_SMOOTH_NN(niter, dsmooth, refNodeList){ \
   SUMA_SurfaceObject m_SOref;   \
   int m_in;   \
   float *m_a, m_P2[2][3], m_U[3], m_Un, m_Rref, m_R, m_Dr, m_Dn;  \
   if (!refNodeList) {  \
      refNodeList = (float *)SUMA_malloc(sizeof(float)*SO->N_Node*3);   \
      if (!refNodeList) { SUMA_SL_Crit("Failed to allocate for refNodeList!"); SUMA_RETURN(NOPE); }   \
   }  \
   SUMA_SO_RADIUS(SO, m_Rref);   /* get the radius before shrinking */\
   memcpy((void*)refNodeList, (void *)SO->NodeList, SO->N_Node * 3 * sizeof(float)); /* copy original surface coords */ \
   dsmooth = SUMA_NN_GeomSmooth( SO, niter, SO->NodeList, 3, SUMA_ROW_MAJOR, dsmooth, cs);    \
   memcpy((void*)SO->NodeList, (void *)dsmooth, SO->N_Node * 3 * sizeof(float)); /* copy smoothed surface coords */  \
   SUMA_RECOMPUTE_NORMALS(SO);   \
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
         m_a = &(SO->NodeList[3*m_in]); \
         SUMA_UNIT_VEC(SO->Center, m_a, m_U, m_Un);\
         m_Dn = m_Dr*m_Un + m_Un;   \
         if (m_Un) { \
            SUMA_POINT_AT_DISTANCE_NORM(m_U, SO->Center, m_Dn, m_P2);  \
            SO->NodeList[3*m_in] = m_P2[0][0]; SO->NodeList[3*m_in+1] = m_P2[0][1]; SO->NodeList[3*m_in+2] = m_P2[0][2];   \
         }  \
      }  \
   }  \
}

/* Area matching is failing for some reason (negative area patches ?) */
#define SUMA_WRAP_BRAIN_SMOOTH_MATCHAREA(niter, dsmooth, refNodeList){ \
   SUMA_SurfaceObject m_SOref;   \
   if (!refNodeList) {  \
      refNodeList = (float *)SUMA_malloc(sizeof(float)*SO->N_Node*3);   \
      if (!refNodeList) { SUMA_SL_Crit("Failed to allocate for refNodeList!"); SUMA_RETURN(NOPE); }   \
   }  \
   memcpy((void*)refNodeList, (void *)SO->NodeList, SO->N_Node * 3 * sizeof(float)); /* copy original surface coords */ \
   dsmooth = SUMA_NN_GeomSmooth( SO, niter, SO->NodeList, 3, SUMA_ROW_MAJOR, dsmooth, cs);    \
   memcpy((void*)SO->NodeList, (void *)dsmooth, SO->N_Node * 3 * sizeof(float)); /* copy smoothed surface coords */  \
   m_SOref.PolyArea = NULL; m_SOref.NodeDim = 3; m_SOref.FaceSetDim = 3;   \
   m_SOref.NodeList = refNodeList; m_SOref.N_Node = SO->N_Node;   \
   m_SOref.FaceSetList = SO->FaceSetList; m_SOref.N_FaceSet = SO->N_FaceSet;  \
   SUMA_EquateSurfaceAreas(SO, &m_SOref, 0.1, cs);  \
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


#define SUMA_REPOSITION_TOUCHUP(limtouch){  \
      int m_in, m_N_troub = 0, m_cond1=0, m_cond2=0, m_cond3 = 0;   \
      float m_MinMax_over[2], m_MinMax[2], m_MinMax_dist[2], m_MinMax_over_dist[2], m_Means[3], m_tb; \
      float m_U[3], m_Un, *m_a, m_P2[2][3], *m_norm, m_shft; \
      float *m_touchup=NULL, **m_wgt=NULL, *m_dsmooth=NULL; \
      m_touchup = (float *)SUMA_calloc(SO->N_Node, sizeof(float));   \
      if (!m_touchup) { SUMA_SL_Crit("Failed to allocate"); exit(1); }  \
      for (m_in=0; m_in<SO->N_Node; ++m_in) {   \
         SUMA_Find_IminImax(SO, Opt, m_in,  m_MinMax, m_MinMax_dist, m_MinMax_over, m_MinMax_over_dist, m_Means, NULL, NULL, 0); \
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
         m_dsmooth = SUMA_Chung_Smooth (SO, m_wgt, 200, 20, m_touchup, 1, SUMA_COLUMN_MAJOR, NULL, ps->cs);   \
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

/*! 
   \brief creates a crude mask of the brain's limit based on the surface of the skull
   mask is crude, for purposes of keeping BrainWrap from leaking out when there are 
   very strong shading artifacts
*/
int SUMA_SkullMask (SUMA_SurfaceObject *SO, SUMA_ISOSURFACE_OPTIONS *Opt, SUMA_COMM_STRUCT *cs)
{
   static char FuncName[]={"SUMA_SkullMask"};
   int it=0, in=0, Npass, Done, ShishMax, i_diffmax, kth_buf, N_it;
   float *n=NULL, *undershish = NULL, diffmax;
   float *tmpptr, *NewCoord = NULL, f3, f4, *dsmooth = NULL;
   float *refNodeList = NULL, sksearch = 15;
   float U[3], Un;
   int  nx, nxy, n_stp;
   SUMA_Boolean DoDbg=NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (Opt->debug > 2) LocalHead = YUP;

   nx = DSET_NX(Opt->in_vol);
   nxy = nx * DSET_NY(Opt->in_vol);   
   
   kth_buf = cs->kth;
      
   
   NewCoord = (float *)SUMA_malloc(3*SO->N_Node*sizeof(float));
   if (!NewCoord) {
      SUMA_SL_Crit("Could not allocate for temp vector.");
      SUMA_RETURN(NOPE);
   }
   
   ShishMax = (int)(SUMA_MAX_PAIR(Opt->d1, Opt->d4) / Opt->travstp * 1.2);
   undershish = (float *)SUMA_malloc(sizeof(float)*ShishMax);
   if (!undershish )  {
      SUMA_SL_Crit("Failed to allocate");
      SUMA_RETURN(NOPE);
   }
   
   
   /* search for a distance for the biggest negative gradient over a distance of 2 cm */
   sksearch = 15; /* maximum search into skull */
   n_stp = (int)(ceil((double)sksearch/Opt->travstp));
   Npass = 0;  Done = 0; N_it = 1;
   do {
      if (Opt->debug > 1) fprintf(SUMA_STDERR,"%s: Gradient Pass #%d\n", FuncName, Npass);
      for (it=0; it < N_it; ++it) {
         for (in=0; in<SO->N_Node; ++in) {
            n = &(SO->NodeList[3*in]);

            /* using normals for direction, NORMALS SHOULD POINT OUTWARDS... 
            You should check for that someday ...*/
            if (n[2] > -45) { /* Do not touch nodes on bottom */
               U[0] = -SO->NodeNormList[3*in]; U[1] = -SO->NodeNormList[3*in+1]; U[2] = -SO->NodeNormList[3*in+2]; 
               SUMA_ONE_SHISH_PLEASE(n, U, Opt->in_vol, Opt->dvec, Opt->travstp, n_stp, nx, nxy, undershish, ShishMax);
               SUMA_MAX_NEG_SHISH_JUMP(undershish, diffmax, i_diffmax);
            } else {
               i_diffmax = 0;
            }
            NewCoord[3*in+0] = n[0] + i_diffmax * Opt->travstp * U[0]; 
            NewCoord[3*in+1] = n[1] + i_diffmax * Opt->travstp * U[1]; 
            NewCoord[3*in+2] = n[2] + i_diffmax * Opt->travstp * U[2]; 
            
            DoDbg = NOPE;
         } /* loop over number of nodes */
   
            
         /* swap vectors */
         tmpptr = SO->NodeList;
         SO->NodeList = NewCoord; 
         NewCoord = tmpptr;
         tmpptr = NULL;

         cs->kth = 1;  /*make sure all gets sent at this stage */
         dsmooth = SUMA_Taubin_Smooth( SO, NULL,
                                    0.6307, -.6732, SO->NodeList,
                                    8, 3, SUMA_ROW_MAJOR, dsmooth, cs);    
         memcpy((void*)SO->NodeList, (void *)dsmooth, SO->N_Node * 3 * sizeof(float));
         cs->kth = kth_buf; 
         
         /* recalculate surface normals */
         SUMA_RECOMPUTE_NORMALS(SO); 
         if (cs->Send) {
            if (!SUMA_SendToSuma (SO, cs, (void *)SO->NodeList, SUMA_NODE_XYZ, 1)) {
               SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
            }
         }

      } /* loop over number of iterations */
      Done = 1;
   } while (!Done);         
   

   if (undershish) SUMA_free(undershish); undershish = NULL;
   if (refNodeList) SUMA_free(refNodeList); refNodeList = NULL;
   if (dsmooth) SUMA_free(dsmooth); dsmooth = NULL;
   if (NewCoord) SUMA_free(NewCoord); NewCoord = NULL;
   SUMA_RETURN(YUP);
}

/*! 
   Main function to stretch a sphere to fit the brain. 
   A modification of BET's algorithm.
*/
int SUMA_StretchToFitLeCerveau (SUMA_SurfaceObject *SO, SUMA_ISOSURFACE_OPTIONS *Opt, SUMA_COMM_STRUCT *cs)
{
   static char FuncName[]={"SUMA_StretchToFitLeCerveau"};
   int it=0, in=0, ii, Npass, Done, ShishMax, i_diffmax, kth_buf;
   float mnc[3], s[3], sn[3], st[3], dp, threshclip, lztfac,
         nsn, rmin, rmax, E, F, su1, su2, su3, su4,  
         r, u1[3], u2[3], u3[3], u[3],
         tm=0.0, t2 = 0.0, t98 = 0.0, Imin, Imax, l=0.0, MinMax[2], 
         MinMax_dist[2],MinMax_over[2], MinMax_over_dist[2], Means[3],
         *n=NULL, tb = 0.0, tbe = 0.0, t = 0.0, Imin_over=0.0, Imin_dist_over=0.0,
         *overshish = NULL, *undershish = NULL, diffmax;
   float *tmpptr, *NewCoord = NULL, f3, f4, lZt, *dsmooth = NULL;
   float *refNodeList = NULL, *OrigNodeList=NULL;
   FILE *OutNodeFile = NULL;
   SUMA_Boolean DoDbg=NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (Opt->debug > 2) LocalHead = YUP;

   kth_buf = cs->kth;
   
   t2 = Opt->t2; t98 = Opt->t98; tm = Opt->tm; t = Opt->t;
   threshclip = ((Opt->t98 - Opt->t2)/5.0);
   
   if (LocalHead) fprintf(SUMA_STDERR,"%s: t2 = %f, t = %f, tm = %f, t98 = %f\n", FuncName, t2, t, tm, t98);
   
   
   NewCoord = (float *)SUMA_malloc(3*SO->N_Node*sizeof(float));
   if (!NewCoord) {
      SUMA_SL_Crit("Could not allocate for temp vector.");
      SUMA_RETURN(NOPE);
   }
   rmin = 3.33; rmax = 10.0;  E = (1/rmin + 1/rmax)/2; F = 6/(1/rmin - 1/rmax); 
   su1 = Opt->su1; 
   
   if (Opt->NodeDbg >= 0) {
      char fname[100];
      sprintf(fname,"dbg_%d.1D", Opt->NodeDbg);
      OutNodeFile = fopen(fname,"w");
      if (!OutNodeFile) {
         SUMA_S_Err("Failed to open debug file");
         SUMA_RETURN(NOPE);
      }
      fprintf(OutNodeFile, "#0 Node id (in)\n"
                           "#1 Iteration (it)\n"
                           "#2 Seg length (l)\n"
                           "#3-5 Imin, Imax, tb\n"
                           "     MinMaxdist, 2 col\n"
                           "     MinMaxover, 2 col\n"
                           "     MinMaxover_dist, 2 col\n"
                           "     Means (at, under, over), 3 col\n"
                           "#6-8 Node coord (n)\n"
                           "#9 Norm . Disp (dp)\n"
                           "#10-12 Mean Neighb Coord (mnc)\n"
                           "#13-15 Normal direction (  )\n"
                           "#16-18 Disp (s)\n"
                           "#19-21 DispT (st)\n"
                           "#22-24 DispN (sn)\n"
                           "#25-27 constants (su1 su2 su3)\n"
                           "#28-30 update vector (u)\n"
                           "#31 t2\n"
                           "#32 t\n"
                           "#33 tm\n"
                           "#34 t98\n"
                           "#\n"
                           "#t2 = %f, t = %f, tm = %f, t98 = %f\n", t2, t, tm, t98);
   }
   
   ShishMax = (int)(SUMA_MAX_PAIR(Opt->d1, Opt->d4) / Opt->travstp * 1.2);
   overshish = (float *)SUMA_malloc(sizeof(float)*ShishMax);
   undershish = (float *)SUMA_malloc(sizeof(float)*ShishMax); 
   OrigNodeList = (float*)SUMA_malloc(sizeof(float)*3*SO->N_Node);
   if (!OrigNodeList || !overshish || !undershish) {
      SUMA_SL_Crit("Failed to allocate");
      SUMA_RETURN(NOPE);
   }
   memcpy((void*)OrigNodeList, (void *)SO->NodeList, SO->N_Node * 3 * sizeof(float));
   
   Npass = 0;   Done = 0;
   do {
      if (Opt->debug > 1) fprintf(SUMA_STDERR,"%s: Pass #%d\n", FuncName, Npass);
      for (it=0; it < Opt->N_it; ++it) {
         SUMA_MEAN_SEGMENT_LENGTH(SO, l);
         if (LocalHead && !(it % 50)) fprintf (SUMA_STDERR,"%s: Iteration %d, l = %f . SO->Center = %f, %f, %f...\n", FuncName, it, l, SO->Center[0], SO->Center[1], SO->Center[2]);
         if (Opt->var_lzt) {
            lztfac = SUMA_MAX_PAIR(0.2, (1.2* (float)it / (float)Opt->N_it));   /* make things expand quickly in the beginning, to help escape large sections of csf close to outer surface, then tighten grip towards the end*/
         } else lztfac = 1.0;
         for (in=0; in<SO->N_Node; ++in) {
            SUMA_MEAN_NEIGHB_COORD(SO, in, mnc);
            n = &(SO->NodeList[3*in]);
            s[0] = mnc[0] - n[0]; s[1] = mnc[1] - n[1]; s[2] = mnc[2] - n[2];
            SUMA_DOTP_VEC(s, &(SO->NodeNormList[3*in]), dp, 3, float, float);  /* dp is the dot product of s with the normal at in */ 
            SUMA_SCALE_VEC(&(SO->NodeNormList[3*in]), sn, dp, 3, float, float); /* sn is the component of s along normal */
            SUMA_NORM_VEC(sn, 3, nsn); /* norm of sn */
            SUMA_SUB_VEC(s, sn, st, 3, float, float, float);  /* st is the tangential component of s along normal */
            SUMA_SCALE_VEC(st, u1, su1, 3, float, float); /* u1 = st * su1 */

            r = ( l * l ) / (2.0 * nsn);
            su2 = ( 1 + tanh(F*(1/r - E)))/2.0;
            SUMA_SCALE_VEC(sn, u2, su2, 3, float, float); /* u2 = sn * su2 */

            SUMA_Find_IminImax(SO, Opt, in, MinMax, MinMax_dist, MinMax_over, MinMax_over_dist, Means, undershish, overshish, ShishMax);  
            Imin = MinMax[0]; Imax = MinMax[1];
            
            if (n[2] - SO->Center[2] < -25) { lZt = SUMA_MAX_PAIR (Opt->bot_lztclip, (Opt->ztv[in] * lztfac)); } /* clip lZt at no less than 0.5 for lowest sections, 
                                                                                                         otherwise you might go down to the dumps */
            else { lZt = Opt->ztv[in] * lztfac; if (lZt > 1) lZt = 0.999; }

            tb = (Imax - t2) * lZt + t2; 
            f3 = 2.0 * (Imin - tb) / (Imax - t2 );

            su3 = Opt->ExpFrac * l * f3 ;
            
            if (Opt->UseExpansion) {
               tbe = (MinMax_over[1] - t2) * lZt + t2; 
               f4 = 2.0 * (MinMax_over[0] - tbe) / (MinMax_over[1] - t2 );
               su4 = Opt->ExpFrac * l * f4 ; if (su4 < 0) su4 = 0; /* too tight expanding otherwise */
            } else su4 = 0;

            /* work the eyes */
            if (Opt->NoEyes) {
               if (it > 0.1 * Opt->N_it) { 
                  if (SUMA_IS_EYE_ZONE(n,SO->Center)) { /* (n[1] - SO->Center[1]) < -10 && (n[2] - SO->Center[2]) < 0 area with the eyes */
                     /* if the mean over is larger than the mean below, go easy, likely to hit the eyes */
                     if (!Opt->Stop[in]) {
                        if (Opt->dbg_eyenodes) { 
                           int kk;
                           fprintf (Opt->dbg_eyenodes,"%d\t %f %.3f ", in, Means[2], Means[2]/Means[1]); 
                           for (kk=0; kk<ShishMax; ++kk) if (overshish[kk] >= 0) fprintf (Opt->dbg_eyenodes,"%.3f ", overshish[kk]);
                           fprintf (Opt->dbg_eyenodes,"\n");
                        }
                        if (Means[2] > 1.2*Means[1]) {
                           SUMA_MAX_SHISH_JUMP(overshish, diffmax, i_diffmax, threshclip);
                           Opt->Stop[in] = overshish[i_diffmax];
                           if (0 && LocalHead) fprintf (SUMA_STDERR, "%s: Stopping node %d, at values > %f\n", FuncName, in, Opt->Stop[in]);
                        } else if (Means[0] >= Opt->t98) {
                           Opt->Stop[in] = Opt->t98;
                           if (0 && LocalHead) fprintf (SUMA_STDERR, "%s: Stopping node %d, at values > %f\n", FuncName, in, Opt->Stop[in]);
                        }
                     }
                  }
               }
               if (Opt->Stop[in]) {
                  if (Means[0] >= Opt->Stop[in]) { su3 = 0; su4 = 0; }
               }
            }
            
            u[0] = su1 * st[0] + su2 * sn[0] + ( su3 + su4 ) * SO->NodeNormList[3*in]   ;
            u[1] = su1 * st[1] + su2 * sn[1] + ( su3 + su4 ) * SO->NodeNormList[3*in+1] ;
            u[2] = su1 * st[2] + su2 * sn[2] + ( su3 + su4 ) * SO->NodeNormList[3*in+2] ; 
            if ((in == Opt->NodeDbg)) { 
               fprintf(SUMA_STDERR, "%s: Node %d, iter %d, l %.6f\n", FuncName, in, it, l);
               fprintf(SUMA_STDERR, " MinMaxTb = [%.6f %.6f %.6f]\n", Imin, Imax, tb);
               fprintf(SUMA_STDERR, " MinMaxdist=[%.6f %.6f ]\n", MinMax_dist[0], MinMax_dist[1]);
               fprintf(SUMA_STDERR, " MinMaxover     =[%.6f %.6f ]\n", MinMax_over[0], MinMax_over[1]);
               fprintf(SUMA_STDERR, " MinMaxover_dist=[%.6f %.6f ]\n", MinMax_over_dist[0], MinMax_over_dist[1]); 
               fprintf(SUMA_STDERR, " Means (at, under, over) = [%.6f %.6f %.6f]\n", Means[0], Means[1], Means[2]); 
               fprintf(SUMA_STDERR, " Coord(n )= [%.6f %.6f %.6f], dp = %f\n",   n[0],  n[1],  n[2], dp); 
               fprintf(SUMA_STDERR, " Neigh(mnc)=[%.6f %.6f %.6f]\n",   mnc[0],  mnc[1],  mnc[2]); 
               fprintf(SUMA_STDERR, " Norm (  )= [%.6f %.6f %.6f]\n",   SO->NodeNormList[3*in], SO->NodeNormList[3*in+1],SO->NodeNormList[3*in+2]); 
               fprintf(SUMA_STDERR, " Disp (s )= [%.6f %.6f %.6f]\n",   s[0],  s[1],  s[2]); 
               fprintf(SUMA_STDERR, "      (st)= [%.6f %.6f %.6f]\n",  st[0], st[1], st[2]); 
               fprintf(SUMA_STDERR, "      (sn)= [%.6f %.6f %.6f]\n",  sn[0], sn[1], sn[2]); 
               fprintf(SUMA_STDERR, "       su = [%.6f %.6f %.6f]\n",  su1, su2, su3); 
               fprintf(SUMA_STDERR, "        u = [%.6f %.6f %.6f]\n",  u[0], u[1], u[2]); 
               if (OutNodeFile) {
                  fprintf(OutNodeFile, " %d %d  %.6f"
                                       " %.6f %.6f %.6f"
                                       " %.6f %.6f"
                                       " %.6f %.6f"
                                       " %.6f %.6f"
                                       " %.6f %.6f %.6f"
                                       " %.6f %.6f %.6f %.6f"
                                       " %.6f %.6f %.6f"
                                       " %.6f %.6f %.6f"
                                       " %.6f %.6f %.6f"
                                       " %.6f %.6f %.6f"
                                       " %.6f %.6f %.6f"
                                       " %.6f %.6f %.6f"
                                       " %.6f %.6f %.6f"
                                       " %.6f %.6f %.6f %.6f"
                                       "\n"
                                       , in, it, l
                                       , Imin, Imax, tb
                                       , MinMax_dist[0], MinMax_dist[1]
                                       , MinMax_over[0], MinMax_over[1]
                                       , MinMax_over_dist[0], MinMax_over_dist[1]
                                       , Means[0], Means[1], Means[2]
                                       ,   n[0],  n[1],  n[2], dp
                                       ,   mnc[0],  mnc[1],  mnc[2]
                                       ,SO->NodeNormList[3*in], SO->NodeNormList[3*in+1],SO->NodeNormList[3*in+2]
                                       ,s[0],  s[1],  s[2]
                                       ,  st[0], st[1], st[2]
                                       ,  sn[0], sn[1], sn[2]
                                       ,  su1, su2, su3
                                       ,  u[0], u[1], u[2]
                                       , t2, t, tm, t98);
               }

            }
            NewCoord[3*in] = n[0] + u[0]; NewCoord[3*in+1] = n[1] + u[1]; NewCoord[3*in+2] = n[2] + u[2];       
            DoDbg = NOPE;
         } /* loop over number of nodes */
         /* swap vectors */
            tmpptr = SO->NodeList;
            SO->NodeList = NewCoord; 
            NewCoord = tmpptr;
            tmpptr = NULL;

         if (Opt->NNsmooth) {
            if (!(it % Opt->smootheach) && it > 0 && it < 0.75*Opt->N_it) {  /* cannot filter too close to convergence */
               SUMA_WRAP_BRAIN_SMOOTH_NN(Opt->NNsmooth, dsmooth, refNodeList);
            }
         }

         /* recalculate surface normals */
         SUMA_RECOMPUTE_NORMALS(SO); 
         if (cs->Send) {
            if (!SUMA_SendToSuma (SO, cs, (void *)SO->NodeList, SUMA_NODE_XYZ, 1)) {
               SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
            }
         }

      } /* loop over number of iterations */
      Done = 1;
   } while (!Done);
      
   
   if (undershish) SUMA_free(undershish); undershish = NULL;
   if (overshish) SUMA_free(overshish); overshish = NULL;
   if (OrigNodeList) SUMA_free(OrigNodeList); OrigNodeList = NULL;
   if (refNodeList) SUMA_free(refNodeList); refNodeList = NULL;
   if (dsmooth) SUMA_free(dsmooth); dsmooth = NULL;
   if (OutNodeFile) fclose(OutNodeFile); OutNodeFile = NULL;
   if (NewCoord) SUMA_free(NewCoord); NewCoord = NULL;
   SUMA_RETURN(YUP);
}

#define MSK_DBG 1
#define Meth1   /* SLOW METHOD, undefine it to have an even slower method! */
/*!
   \brief First pass at finding voxels inside a surface. Slow baby, slow.
   The first approach traces a ray parallel to the X axis from each voxel 
   and counts the number of intersections, in the direction of the ray, with
   the surface. Odd = in, Even = out. That is Meth1.
   The other method, projects all triangles to the YZ plane and then finds out 
   if a voxel's projection lies inside the projected triangles. You still have to
   do a test to count only the triangles that are intersected in the direction of the ray
   In the end, it turns out to be slower than Meth1. Bad
   
   \sa SUMA_FindVoxelsInSurface
*/
byte *SUMA_FindVoxelsInSurface_SLOW (SUMA_SurfaceObject *SO, SUMA_VOLPAR *VolPar, int *N_inp) 
{
   static char FuncName[]={"SUMA_FindVoxelsInSurface_SLOW"};
   byte *isin = NULL, *tmpin = NULL;
   int i, N_in, j, k , n, khits, dims[2], N_hits, iii, jjj, niii, ncul;
   float *tmpXYZ=NULL, Center[3], MaxDims[3], MinDims[3], aMaxDims, aMinDims, delta_t;
   float hdim[3], t0, t1, t2, SOCenter[0], p0[3], p1[3];
   SUMA_MT_INTERSECT_TRIANGLE *mti = NULL; 
   struct  timeval tti;
   int nfound = 0, N_poshits = 0;
   int cnt = 0, sgn, sgnref, n1, n2, n3;
   float ivec[3] = { 1, 0, 0}, dp = 0.0, cx = 0.0;
   SUMA_Boolean LocalHead = NOPE;
   #if MSK_DBG
      FILE *fdb=fopen("SUMA_FindVoxelsInSurface.1D","w");
   #endif
   
   SUMA_ENTRY;
   
   SUMA_etime (&tti, 0);
   if (LocalHead) {
      fprintf(SUMA_STDERR,"%s: Patience...\n", FuncName);
   }
   N_in = *N_inp = 0;
   
   /* copy surface coordinates, we're going to ijk land */
   tmpXYZ = (float *)SUMA_malloc(SO->N_Node * 3 * sizeof(float));
   isin = (byte *)SUMA_malloc(VolPar->nx*VolPar->ny*VolPar->nz * sizeof(byte));
   if (!tmpXYZ || !isin) {
      SUMA_SL_Crit("Faile to allocate");
      SUMA_RETURN(NULL);
   }
   
   memcpy ((void*)tmpXYZ, (void *)SO->NodeList, SO->N_Node * 3 * sizeof(float));
   
   /* transform the surface's coordinates from RAI to 3dfind */
   SUMA_vec_dicomm_to_3dfind (tmpXYZ, SO->N_Node, SO->VolPar);
   
   /* find the new center and bounding box for the surface in the new coordinate system*/
   SUMA_MIN_MAX_SUM_VECMAT_COL (tmpXYZ, SO->N_Node, 3, MinDims, MaxDims, SOCenter); 
   SOCenter[0] /= SO->N_Node;  
   SOCenter[1] /= SO->N_Node;  
   SOCenter[2] /= SO->N_Node;  
   SUMA_MIN_VEC (MinDims, 3, aMinDims );   
   SUMA_MAX_VEC (MaxDims, 3, aMaxDims);    
   /* Box half Dim */
   for (i=0; i<3; ++i) { hdim[i] = ( MaxDims[i] - MinDims[i]) / 2.0; }
   /* Box Center */
   for (i=0; i<3; ++i) { Center[i] = MinDims[i] + hdim[i]; }
   if (LocalHead) fprintf (SUMA_STDERR,"Center in IJK is: %f %f %f\n"
                                       "HlfDim in IJK is: %f %f %f\n"
                                       , Center[0], Center[1], Center[2],
                                         hdim[0], hdim[1], hdim[2]);
   
   n = 0; N_in = 0;
   for (k=0; k < VolPar->nz; ++k) {
      for (j=0; j < VolPar->ny; ++j) {
         for (i=0; i < VolPar->nx; ++i) {
            isin[n] = 0; /* initialize to out */
            /* is voxel center inside bounding box ? */
            t0 = hdim[0] - SUMA_ABS(i - Center[0]);   
            if (t0 >= 0) {
               t1 = hdim[1] - SUMA_ABS(j - Center[1]);
               if (t1 >= 0) {
                  t2 = hdim[2] - SUMA_ABS(k - Center[2]); 
                  if (t2 >= 0)  {
                     isin[n] = 1; ++N_in;
                  }
               }   
            }
            if (isin[n]) { /* inside bounding box, but is it inside the surface ? */
               p0[0] = i; p1[0] = i+1000; p0[1] = p1[1] = j; p0[2] = p1[2] = k; 
               #ifdef Meth1
                  /* works, but slow as a turtle */
                  mti = SUMA_MT_intersect_triangle(p0, p1, tmpXYZ, SO->N_Node, SO->FaceSetList, SO->N_FaceSet, mti);
                  if (!(mti->N_poshits % 2)) { /* number of positive direction hits is a multiple of 2 */
                     isin[n] = 0; --N_in;
                  } 
               #else 
                  dims[0] = 1; dims[1] = 2; /* rays are along x vector, we will look for intersections with projections on plane y z */
                  tmpin = SUMA_isinpoly(p0, tmpXYZ, SO->FaceSetList, SO->N_FaceSet, SO->FaceSetDim, dims, &N_hits, tmpin, NULL);
                  N_poshits = 0; sgnref = 0;
                  nfound = 0; cnt = 0;
                  while (nfound < N_hits) {
                     if (tmpin[cnt]) {
                        ++nfound; 
                        /* face centroid x coordinate, this contraption works as long as dims is [1 2] and as long as
                        the triangles making up the mesh are comparable in size to voxel resolution. Otherwise, it is 
                        not a particularly pleasing approximation. The proper way would be to do a ray/triangle intersection
                        for those hit triangles (use SUMA_MT_isIntersect_Triangle and check for differences in signed distance). 
                        But that ends up like the method above with no speed up to write home about NOT WORTH IT*/
                        n1 =  SO->FaceSetList[3*cnt]; n2 = SO->FaceSetList[3*cnt+1]; n3 = SO->FaceSetList[3*cnt+2];   \
                        cx = (tmpXYZ[3*n1]   + tmpXYZ[3*n2]   + tmpXYZ[3*n3]  )/3; \
                        sgn = SUMA_SIGN(cx - i);
                        if (nfound == 1) {
                           sgnref = sgn;
                        }
                        /* fprintf(SUMA_STDERR, "%s: nfound %d: dp %f: Ref %d, sgn %d\n", FuncName, nfound, dp, sgnref, sgn); */
                        if (sgnref == sgn) ++N_poshits;
                     }
                     ++cnt;      
                  }
                  if (N_poshits % 2 == 0) { /* dude's outside */ 
                     isin[n] = 0; --N_in;
                     /* if (LocalHead) fprintf (SUMA_STDERR,"%d %d %d %d hits\n",i, j, k, N_hits);  */
                  }
               #endif
            }
            #if MSK_DBG
               if (isin[n]) fprintf(fdb, "%d %d %d %d\n", i, j, k, isin[n]);
            #endif
            ++n;
         }
      }               
   }         

   *N_inp = N_in;
   #if MSK_DBG
      fclose(fdb); fdb = NULL;
   #endif
   
   delta_t = SUMA_etime (&tti, 1); 
   if (LocalHead) {
      fprintf(SUMA_STDERR,"%s: Execution time %f seconds.\n", FuncName, delta_t);
   }
   
   SUMA_free(tmpXYZ); tmpXYZ = NULL;
   if (mti) mti = SUMA_Free_MT_intersect_triangle(mti);
   if (tmpin) SUMA_free(tmpin); tmpin = NULL;
   SUMA_RETURN(isin);
}



/*!
   \param NodeIJKlist (float *) the equivalent of SO->NodeList only in i,j,k indices into VolPar's grid.
                           In the future, might want to allow for this param to be NULL and have it
                           be generated internally from SO->NodeList and VolPar.
   \return isin (short *)  1: voxel contains node
                           2: voxel is in box containing triangle
                           3: voxel is in box containing triangle and is on the side opposite to the normal 
                           See SUMA_SURF_GRID_INTERSECT_OPTIONS for the various possible values
*/
short *SUMA_SurfGridIntersect (SUMA_SurfaceObject *SO, float *NodeIJKlist, SUMA_VOLPAR *VolPar, int *N_inp)
{
   static char FuncName[]={"SUMA_SurfGridIntersect"};
   short *isin=NULL;
   byte *ijkmask=NULL, *inmask = NULL;
   float *p1, *p2, *p3, min_v[3], max_v[3], p[3], dist;
   float MaxDims[3], MinDims[3], SOCenter[3];
   int nn, nijk, nx, ny, nz, nxy, nxyz, nf, n1, n2, n3, nn3, *voxelsijk=NULL, N_alloc, en;
   int N_inbox, nt, nt3, ijkseed, N_in;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (SO->FaceSetDim != 3 || SO->NodeDim != 3) {
      SUMA_SL_Err("SO->FaceSetDim != 3 || SO->NodeDim != 3"); 
      SUMA_RETURN(NULL);
   }
   
   nx = SO->VolPar->nx; ny = SO->VolPar->ny; nz = SO->VolPar->nz; nxy = nx * ny; nxyz = nx * ny * nz;
   

   isin = (short *)SUMA_calloc(nxyz, sizeof(short));
   if (!isin) {
      SUMA_SL_Crit("Faile to allocate");
      SUMA_RETURN(NULL);
   }
   
   /* mark the node voxels */
   for (nn=0; nn<SO->N_Node; ++nn) {
      /* find the ijk of each node: */
      nn3 = 3*nn;
      nijk = SUMA_3D_2_1D_index((int)NodeIJKlist[nn3], (int)NodeIJKlist[nn3+1] , (int)NodeIJKlist[nn3+2], nx , nxy); 
      if (nijk < nxyz) { if (!isin[nijk]) { isin[nijk] = SUMA_ON_NODE; ++(*N_inp); } }
   }
   
   
   /* cycle through all triangles and find voxels that intersect them */
   N_alloc = 600; /* expected maximum number of voxels in triangle's bounding box */
   voxelsijk = (int *)SUMA_malloc(sizeof(int)*N_alloc*3);
   if (!voxelsijk) { SUMA_SL_Crit("Failed to Allocate!"); SUMA_RETURN(NULL);  }   
   for (nf=0; nf<SO->N_FaceSet; ++nf) {
      n1 = SO->FaceSetList[SO->FaceSetDim*nf]; n2 = SO->FaceSetList[SO->FaceSetDim*nf+1]; n3 = SO->FaceSetList[SO->FaceSetDim*nf+2];
      /* find the bounding box of the triangle */
      p1 = &(NodeIJKlist[3*n1]); p2 = &(NodeIJKlist[3*n2]); p3 = &(NodeIJKlist[3*n3]); 
      SUMA_TRIANGLE_BOUNDING_BOX(p1, p2, p3, min_v, max_v)
      
      /* quick check of preallocate size of voxelsijk */
      en =((int)(max_v[0] - min_v[0] + 2) * (int)(max_v[1] - min_v[1] + 2) * (int)(max_v[2] - min_v[2] + 2)); 
      if ( en > N_alloc) {
         SUMA_SL_Warn("Reallocating, increase limit to improve speed.");
         N_alloc = 2*en;
         voxelsijk = (int *)SUMA_realloc(voxelsijk, 3*N_alloc*sizeof(int));
         if (!voxelsijk) { SUMA_SL_Crit("Failed to Allocate!"); SUMA_RETURN(NULL); }
      } 
      /* find the list of voxels inhabiting this box */
      N_inbox = 0;
      if (!SUMA_VoxelsInBox(voxelsijk, &N_inbox, min_v, max_v)) {
         SUMA_SL_Err("Unexpected error!"); SUMA_RETURN(NULL); 
      }
      if (!N_inbox) { SUMA_SL_Err("Unexpected error, no voxels in box!"); SUMA_RETURN(NULL);  }
      
      /* mark hese voxels as inside the business */
      for (nt=0; nt < N_inbox; ++nt) {
         nt3 = 3*nt;
         if (voxelsijk[nt3] < nx &&  voxelsijk[nt3+1] < ny &&  voxelsijk[nt3+2] < nz) {
            nijk = SUMA_3D_2_1D_index(voxelsijk[nt3], voxelsijk[nt3+1], voxelsijk[nt3+2], nx , nxy);  
            if (!isin[nijk]) { 
               /* what side of the plane is this voxel on ? */
               p[0] = (float)voxelsijk[nt3]; p[1] = (float)voxelsijk[nt3+1]; p[2] = (float)voxelsijk[nt3+2]; 
               SUMA_DIST_FROM_PLANE(p1, p2, p3, p, dist);
               if (dist) {
                  if (SUMA_IS_NEG(dist)) isin[nijk] = SUMA_IN_TRIBOX_INSIDE; 
                  else isin[nijk] = SUMA_IN_TRIBOX_OUTSIDE; 
               }
               ++(*N_inp);    
            }
         }
      }
   }
   
   /* Now fill in inside of the sphere */
      /* create a mask vector*/
      ijkmask = (byte *)SUMA_calloc(nxyz, sizeof(byte));
      if (!ijkmask) {
         SUMA_SL_Crit("Failed to allocate");
         SUMA_RETURN(NULL);
      }
      for (nt=0; nt<nxyz; ++nt) if (isin[nt]) ijkmask[nt] = 1;
      
      /* seed: find the center the surface in the index coordinate system*/
      SUMA_MIN_MAX_SUM_VECMAT_COL (NodeIJKlist, SO->N_Node, 3, MinDims, MaxDims, SOCenter); 
      SOCenter[0] /= SO->N_Node;  SOCenter[1] /= SO->N_Node;   SOCenter[2] /= SO->N_Node;
      if (LocalHead) {
         fprintf(SUMA_STDERR,"%s:\nSeed is %d %d %d\n", FuncName, (int)SOCenter[0], (int)SOCenter[1], (int)SOCenter[2]); 
      }
      ijkseed = SUMA_3D_2_1D_index(SOCenter[0], SOCenter[1], SOCenter[2], nx , nxy);  
      /* get the mask */
      inmask = SUMA_FillToVoxelMask(ijkmask, ijkseed, nx, ny, nz, &N_in, NULL); 
      if (!inmask) {
         SUMA_SL_Err("Failed to FillToVoxelMask!");
      } else {
         /* flag it */
         for (nt=0; nt<nxyz; ++nt) if (inmask[nt] && !isin[nt]) isin[nt] = SUMA_INSIDE_SURFACE;
      }
      
   if (ijkmask) SUMA_free(ijkmask); ijkmask = NULL;
   if (inmask) SUMA_free(inmask); inmask = NULL;
   if (voxelsijk) SUMA_free(voxelsijk); voxelsijk = NULL;
   SUMA_RETURN(isin);
   
}               

/*!
   \brief finds voxels inside a closed surface. Surface's normals are to point outwards.
   \sa   SUMA_SurfGridIntersect   
   \sa   SUMA_SURF_GRID_INTERSECT_OPTIONS for the various possible values
 
*/   
short *SUMA_FindVoxelsInSurface (SUMA_SurfaceObject *SO, SUMA_VOLPAR *VolPar, int *N_inp) 
{
   static char FuncName[]={"SUMA_FindVoxelsInSurface"};
   short *isin = NULL;
   byte *tmpin = NULL;
   int i, N_in, j, k , n, khits, dims[2], N_hits, iii, jjj, niii, ncul;
   float *tmpXYZ=NULL, Center[3], MaxDims[3], MinDims[3], aMaxDims, aMinDims, delta_t;
   float hdim[3], t0, t1, t2, SOCenter[0], p0[3], p1[3];
   SUMA_MT_INTERSECT_TRIANGLE *mti = NULL; 
   struct  timeval tti;
   int nfound = 0, N_poshits = 0;
   int cnt = 0, sgn, sgnref, n1, n2, n3;
   float ivec[3] = { 1, 0, 0}, dp = 0.0, cx = 0.0;
   SUMA_Boolean LocalHead = NOPE;
   

   #if MSK_DBG
      FILE *fdb=fopen("SUMA_FindVoxelsInSurface.1D","w");
   #endif
   
   SUMA_ENTRY;
   
   SUMA_etime (&tti, 0);
   if (LocalHead) {
      fprintf(SUMA_STDERR,"%s: Patience...\n", FuncName);
   }
   N_in = *N_inp = 0;
   
   /* copy surface coordinates, we're going to ijk land */
   tmpXYZ = (float *)SUMA_malloc(SO->N_Node * 3 * sizeof(float));
   if (!tmpXYZ) {
      SUMA_SL_Crit("Faile to allocate");
      SUMA_RETURN(NULL);
   }
   
   memcpy ((void*)tmpXYZ, (void *)SO->NodeList, SO->N_Node * 3 * sizeof(float));
   
   /* transform the surface's coordinates from RAI to 3dfind */
   SUMA_vec_dicomm_to_3dfind (tmpXYZ, SO->N_Node, SO->VolPar);
   
   /* find the new center and bounding box for the surface in the new coordinate system*/
   SUMA_MIN_MAX_SUM_VECMAT_COL (tmpXYZ, SO->N_Node, 3, MinDims, MaxDims, SOCenter); 
   SOCenter[0] /= SO->N_Node;  
   SOCenter[1] /= SO->N_Node;  
   SOCenter[2] /= SO->N_Node;  
   SUMA_MIN_VEC (MinDims, 3, aMinDims );   
   SUMA_MAX_VEC (MaxDims, 3, aMaxDims);    
   /* Box half Dim */
   for (i=0; i<3; ++i) { hdim[i] = ( MaxDims[i] - MinDims[i]) / 2.0; }
   /* Box Center */
   for (i=0; i<3; ++i) { Center[i] = MinDims[i] + hdim[i]; }
   if (LocalHead) fprintf (SUMA_STDERR,"Center in IJK is: %f %f %f\n"
                                       "HlfDim in IJK is: %f %f %f\n"
                                       , Center[0], Center[1], Center[2],
                                         hdim[0], hdim[1], hdim[2]);
   
   /* Find the voxels that intersect the surface */
   isin = SUMA_SurfGridIntersect (SO, tmpXYZ, VolPar, &N_in);               

   *N_inp = N_in;
   #if MSK_DBG
      fclose(fdb); fdb = NULL;
   #endif
   
   delta_t = SUMA_etime (&tti, 1); 
   if (LocalHead) {
      fprintf(SUMA_STDERR,"%s: Execution time %f seconds.\n", FuncName, delta_t);
   }
   
   SUMA_free(tmpXYZ); tmpXYZ = NULL;
   if (mti) mti = SUMA_Free_MT_intersect_triangle(mti);
   if (tmpin) SUMA_free(tmpin); tmpin = NULL;
   
   SUMA_RETURN(isin);
}

#ifdef SUMA_BrainWrap_STANDALONE
void usage_SUMA_BrainWrap (SUMA_GENERIC_ARGV_PARSE *ps)
   {
      static char FuncName[]={"usage_SUMA_BrainWrap"};
      char * s = NULL, *sio=NULL, *st = NULL, *sts = NULL;
      int i;
      s = SUMA_help_basics();
      sio  = SUMA_help_IO_Args(ps);
      sts = SUMA_help_talk(ps);
      printf ( "\n"
               "Usage: A program to extract the brain from surrounding.\n"
               "  tissue from MRI T1-weighted images. The largely automated\n"
               "  process consists of three steps:\n"
               "  1- Preprocessing of volume to remove gross spatial image \n"
               "  non-uniformity artifacts and reposition the brain in\n"
               "  a reasonable manner for convenience.\n"
               "  2- Expand a spherical surface iteratively until it envelopes\n"
               "  the brain. This is a modified version of the BET algorithm:\n"
               "     Fast robust automated brain extraction, \n"
               "      by Stephen M. Smith, HBM 2002 v 17:3 pp 143-155\n"
               "    Modifications include the use of:\n"
               "     . outer brain surface\n"
               "     . expansion driven by data inside and outside the surface\n"
               "     . avoidance of eyes and ventricles\n"
               "     . a set of operations to avoid the clipping of certain brain\n"
               "       areas and reduce leakage into the skull in heavily shaded\n"
               "       data\n"
               "  3- The creation of various masks and surfaces modeling brain\n"
               "     and portions of the skull\n"  
               "\n"
               "  3dSkullStrip  < -input VOL >\n"
               "             [< -o_TYPE PREFIX >] [< -prefix Vol_Prefix >] \n"
               "             [< -niter N_ITER >]\n"
               "             [< -ld LD >] [< -shrink_fac SF >]\n"
               "             [< -touchup >] [< -perc_int PERC_INT >]\n"
               "             [< -max_inter_iter MII >] [< -use_skull >]\n"
               "             [< -debug DBG >]\n"  
               "\n"
               "  Mandatory parameters:\n"
               "     -input VOL: Input AFNI (or AFNI readable) volume.\n"
               "  ****** AT THE MOMENT, VOL is the volume output from 3dSpatNorm.\n"
               "\n"
               "  Optional Parameters:\n"
               "     -ld LD: Parameter to control the density of the surface.\n"
               "             Default is 20. See CreateIcosahedron -help\n"
               "             for details on this option.\n"
               "     -niter N_ITER: Number of iterations. Default is 250\n"
               "        For denser meshes, you need more iterations\n"
               "        N_ITER of 750 works for LD of 50.\n"
               "     -shrink_fac SF: Parameter controlling the brain vs non-brain\n"
               "             intensity threshold (tb). Default is 0.6.\n"
               "              tb = (Imax - t2) SF + t2 \n"
               "             where t2 is the 2 percentile value and Imax is the local\n"
               "             maximum, limited to the median intensity value.\n"
               "             For more information on tb, t2, etc. read the BET paper\n"
               "             mentioned above. Note that in 3dSkullStrip, SF can vary across \n"
               "             iterations and might be automatically clipped in certain areas.\n"
               "             SF can vary between 0 and 1.\n"
               "             0: Intensities < median inensity are considered non-brain\n"
               "             1: Intensities < t2 are considered non-brain\n" 
               "     -var_shrink_fac: Vary the shrink factor with the number of\n"
               "             iterations. This reduces the likelihood of a surface\n"
               "             getting stuck on large pools of CSF before reaching\n"
               "             the outer surface of the brain. (Default)\n"
               "     -no_var_shrink_fac: Do not use var_shrink_fac.\n"
               "     -shrink_fac_bot_lim SFBL: Do not allow the varying SF to go\n"
               "             below SFBL . Default 0.65. \n"
               "             This option helps reduce potential for leakage below \n"
               "             the cerebellum.\n"
               "     -pushout: Consider values above each node in addition to values\n"
               "               below the node when deciding on expansion. (Default)\n"
               "     -no_pushout: Do not use -pushout.\n"
               "     -exp_frac: Speed of expansion (see BET paper). Default is 0.1.\n"
               "     -touchup: Perform touchup operations at end to include\n"
               "               areas not covered by surface expansion. (Default)\n"
               "     -no_touchup: Do not use -touchup\n"
               "     -NN_smooth NN_SM: Perform Nearest Neighbor coordinate interpolation\n"
               "                       every few iterations. Default is 72\n"
               "     -smooth_final SM: Perform final surface smoothing after all iterations.\n"
               "                       Default is 20 smoothing iterations.\n"
               "                       Smoothing is done using Taubin's method, \n"
               "                       see SurfSmooth -help for detail.\n"
               "     -avoid_vent: avoid ventricles. Default.\n"
               "     -no_avoid_vent: Do not use -avoid_vent.\n"
               "     -avoid_eyes: avoid eyes. Default\n"
               "     -no_avoid_eyes: Do not use -avoid_eyes.\n"
               "     -use_skull: Use outer skull to limit expansion of surface into\n"
               "                 the skull due to very strong shading artifacts.\n"
               "                 Default.\n"
               "                 Turn this option off (-no_use_skull) if you do not\n"
               "                 have skull imaged at the top or the sides of the brain.\n"
               "     -no_use_skull: Do not use -use_skull\n"
               "     -perc_int PERC_INT: Percentage of segments allowed to intersect\n"
               "                         surface. Ideally this should be 0 (Default). \n"
               "                         However, few surfaces might have small stubborn\n"
               "                         intersections that produce a few holes.\n"
               "                         PERC_INT should be a small number, typically\n"
               "                         between 0 and 0.1\n"
               "     -max_inter_iter N_II: Number of iteration to remove intersection\n"
               "                           problems. With each iteration, the program\n"
               "                           automatically increases the amount of smoothing\n"
               "                           to get rid of intersections. Default is 4\n"
               "     -demo_pause: Pause at various step in the process to facilitate\n"
               "                  interactive demo while 3dSkullStrip is communicating\n"
               "                  with AFNI and SUMA. See 'Eye Candy' mode below and\n"
               "                  -talk_suma option. \n"
               "     -o_TYPE PREFIX: prefix of output surface.\n"
               "        where TYPE specifies the format of the surface\n"
               "        and PREFIX is, well, the prefix.\n"
               "        TYPE is one of: fs, 1d (or vec), sf, ply.\n"
               "        Default is: -o_ply brainwrap_out\n"
               "     -prefix VOL_PREFIX: prefix of output volumes.\n"
               "        If not specified, the prefix is the same\n"
               "        as the one used with -o_TYPE.\n"
               "\n"
               "%s"
               "\n"
               "%s"
               "\n"
               "     -debug DBG: debug levels of 0 (default), 1, 2, 3.\n"
               "        This is no Rick Reynolds debug, which is oft nicer\n"
               "        than the results, but it will do.\n"
               "     -node_dbg NODE_DBG: Output lots of parameters for node\n"
               "                         NODE_DBG for each iteration.\n"
               "\n"
               " Eye Candy Mode:\n"
               " You can run BrainWarp and have it send successive iterations\n"
               " to SUMA and AFNI. This is very helpful in following the\n"
               " progression of the algorithm and determining the source\n"
               " of trouble, if any.\n"
               " Example:\n"
               "     afni -niml -yesplugouts &\n"
               "     suma -niml &\n"
               "     3dSkullStrip -input Anat+orig -o_ply anat_brain -talk_suma -feed_afni -send_kth 5\n"
               "\n"
               " Tips:\n"
               " I ran the program with the default parameters on 200+ datasets.\n"
               " The results were quite good in all but a couple of instances:\n"
               "\n"  
               /*
               "     -sm_fac SMFAC: Smoothing factor (Default is 1)\n"
               "     -d1 D1: Distance to search inward (Default 20 mm)\n"
               "     -t2 T2: Force t2 to be T2 (Default automated)\n"
               "     -t T: Force t to be T (Default automated)\n"
               "     -tm TM: Force tm to be TM (Default automated)\n"
               "     -t98 T98: Force t98 to be T98 (Default automated)\n"
               */
               "%s"
               "\n", sio, sts, s);
       SUMA_free(s); s = NULL; SUMA_free(st); st = NULL; SUMA_free(sio); sio = NULL; SUMA_free(sts); sts = NULL;         
       s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
       printf("       Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov     \n");
       exit (0);
   }
/*!
   \brief parse the arguments for SurfSmooth program
   
   \param argv (char *)
   \param argc (int)
   \return Opt (SUMA_ISOSURFACE_OPTIONS *) options structure.
               To free it, use 
               SUMA_free(Opt->out_name); 
               SUMA_free(Opt);
*/
SUMA_ISOSURFACE_OPTIONS *SUMA_BrainWrap_ParseInput (char *argv[], int argc, SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_BrainWrap_ParseInput"}; 
   SUMA_ISOSURFACE_OPTIONS *Opt=NULL;
   int kar, i, ind;
   char *outname;
   SUMA_Boolean brk = NOPE;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   Opt = (SUMA_ISOSURFACE_OPTIONS *)SUMA_malloc(sizeof(SUMA_ISOSURFACE_OPTIONS));
   
   kar = 1;
   Opt->spec_file = NULL;
   Opt->out_vol_prefix = NULL;
   Opt->out_prefix = NULL;
   Opt->sv_name = NULL;
   Opt->N_surf = -1;
   Opt->in_name = NULL;
   Opt->cmask = NULL;
   Opt->MaskMode = SUMA_ISO_UNDEFINED;
   for (i=0; i<ISOSURFACE_MAX_SURF; ++i) { Opt->surf_names[i] = NULL; }
   outname = NULL;
   Opt->in_vol = NULL;
   Opt->nvox = -1;
   Opt->ninmask = -1;
   Opt->mcdatav = NULL;
   Opt->debug = 0;
   Opt->v0 = 0.0;
   Opt->v1 = -1.0;
   Opt->dvec = NULL;
   Opt->SurfFileType = SUMA_PLY;
   Opt->SurfFileFormat = SUMA_ASCII;
   Opt->xform = SUMA_ISO_XFORM_MASK;
   Opt->obj_type = -1;
   Opt->obj_type_res = -1;
   Opt->XYZ = NULL;
   Opt->in_1D = NULL;
   Opt->N_XYZ = 0;
   Opt->Zt = 0.6;
   Opt->ExpFrac = 0.1;
   Opt->N_it = 250;
   Opt->Icold = 20;
   Opt->NodeDbg = -1;
   Opt->t2 = Opt->t98 = Opt->t = Opt->tm = -1;
   Opt->r = 0;
   Opt->d1 = 20;
   Opt->su1 = 1;
   Opt->UseNew = 1.0;
   Opt->d4 = 15;
   Opt->ztv = NULL;
   Opt->Kill98 = 0;
   Opt->NoEyes = 1;
   Opt->NNsmooth = 72;
   Opt->smootheach = 50;
   Opt->avoid_vent = 1;
   Opt->smooth_end = 20;
   Opt->k98mask = NULL;
   Opt->k98maskcnt = 0;
   Opt->dbg_eyenodes = NULL;
   Opt->travstp = 1.0;
   Opt->Stop = NULL;
   Opt->MaxIntIter = 4;
   Opt->UseExpansion = 1;
   Opt->PercInt = 0;
   Opt->UseSkull = 1;
   Opt->send_hull = 1;
   Opt->bot_lztclip = 0.65; /* 0.5 is OK but causes too much leakage below cerebellum in most dsets, 0.65 seems better. 0 if you do not want to use it*/
	Opt->var_lzt = 1.0; /* a flag at the moment, set it to 1 to cause shirnk fac to vary during iterations. Helps escape certain large 
                           chunks of CSF just below the brain */
   Opt->DemoPause = 0;
   brk = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_SUMA_BrainWrap(ps);
          exit (0);
		}
		
		SUMA_SKIP_COMMON_OPTIONS(brk, kar);
            
      if (!brk && (strcmp(argv[kar], "-pushout") == 0)) {
         Opt->UseExpansion = 1;
         brk = YUP;
      }
      if (!brk && (strcmp(argv[kar], "-no_pushout") == 0)) {
         Opt->UseExpansion = 0;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-use_skull") == 0)) {
         Opt->UseSkull = 1;
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-no_use_skull") == 0)) {
         Opt->UseSkull = 0;
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-send_no_skull") == 0)) {
         Opt->send_hull = 0;
         brk = YUP;
      }
      
      
      if (!brk && (strcmp(argv[kar], "-var_shrink_fac") == 0)) {
         Opt->var_lzt = 1;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-no_var_shrink_fac") == 0)) {
         Opt->var_lzt = 0;
         brk = YUP;
      }
      
      
      if (!brk && (strcmp(argv[kar], "-perc_int") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -perc_int \n");
				exit (1);
			}
			Opt->PercInt = atof(argv[kar]);
         if ( (Opt->PercInt < 0 && Opt->PercInt != -1) || Opt->PercInt > 10) {
            fprintf (SUMA_STDERR, "parameter after -perc_int should be -1 or between 0 and 10 (have %f) \n", Opt->PercInt);
				exit (1);
         }
         brk = YUP;
		}
      
      
      if (!brk && (strcmp(argv[kar], "-input_1D") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -input_1D \n");
				exit (1);
			}
			Opt->in_1D = argv[kar];
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-avoid_vent") == 0)) {
			Opt->avoid_vent = 1;
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-demo_pause") == 0)) {
			Opt->DemoPause = 1;
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-d1") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -d1 \n");
				exit (1);
			}
			Opt->d1 = atof(argv[kar]);
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-max_inter_iter") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -max_inter_iter \n");
				exit (1);
			}
			Opt->MaxIntIter = atoi(argv[kar]);
         if (Opt->MaxIntIter < 0 || Opt->MaxIntIter > 10) {
            fprintf (SUMA_STDERR, "-max_inter_iter parameter should be between 0 and 10 \n");
				exit (1);
         }  
         brk = YUP;
		}
      if (!brk && (strcmp(argv[kar], "-smooth_final") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -smooth_final \n");
				exit (1);
			}
			Opt->smooth_end = atoi(argv[kar]);
         if (Opt->smooth_end < 0 || Opt->smooth_end > 100 || Opt->smooth_end % 2) {
            fprintf (SUMA_STDERR, "irrational or exhuberant values for smooth_final\n Must use even number between 0 to 100 (%d entered)\n", Opt->smooth_end);
				exit (1);
         }
         brk = YUP;
		}
      if (!brk && (strcmp(argv[kar], "-NNsmooth") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -NNsmooth \n");
				exit (1);
			}
			Opt->NNsmooth = atoi(argv[kar]);
         if (Opt->NNsmooth % 2) {
            fprintf (SUMA_STDERR, "Number of smoothing steps must be an even positive number (not %d) \n", Opt->NNsmooth);
				exit (1);
         }
         brk = YUP;
		}
      
      if (!brk && ((strcmp(argv[kar], "-shrink_fac") == 0) || (strcmp(argv[kar], "-bf") == 0))) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -shrink_fac || -bf \n");
				exit (1);
			}
			Opt->Zt = atof(argv[kar]);
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-shrink_fac_bot_lim") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -shrink_fac_bot_lim \n");
				exit (1);
			}
			Opt->bot_lztclip = atof(argv[kar]);
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-t2") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -t2 \n");
				exit (1);
			}
			Opt->t2 = atof(argv[kar]);
         brk = YUP;
		}
      if (!brk && (strcmp(argv[kar], "-t98") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -t98 \n");
				exit (1);
			}
			Opt->t98 = atof(argv[kar]);
         brk = YUP;
		}
      if (!brk && (strcmp(argv[kar], "-t") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -t \n");
				exit (1);
			}
			Opt->t = atof(argv[kar]);
         brk = YUP;
		}
      if (!brk && (strcmp(argv[kar], "-tm") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -tm \n");
				exit (1);
			}
			Opt->tm = atof(argv[kar]);
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-niter") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -niter \n");
				exit (1);
			}
			Opt->N_it = atoi(argv[kar]);
         if (Opt->N_it < 0 || Opt->N_it > 100000) {
            fprintf (SUMA_STDERR, "%d is a bad iteration number.\n", Opt->N_it);
				exit (1);
         }
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-exp_frac") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -exp_frac \n");
				exit (1);
			}
			Opt->ExpFrac = atof(argv[kar]);
         if (Opt->ExpFrac < -1 || Opt->ExpFrac > 1) { /* accroding to algo, should be between 0 and 1 */
            fprintf (SUMA_STDERR, "%f is a bad expantion fraction.\n", Opt->ExpFrac);
				exit (1);
         }
         brk = YUP;
		}
      if (!brk && (strcmp(argv[kar], "-node_dbg") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -node_dbg \n");
				exit (1);
			}
			Opt->NodeDbg = atoi(argv[kar]);
         if (Opt->NodeDbg < 0) {
            fprintf (SUMA_STDERR, "%d is a bad node number.\n", Opt->NodeDbg);
				exit (1);
         }
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-sm_fac") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -sm_fac \n");
				exit (1);
			}
			Opt->su1 = atof(argv[kar]);
         if (Opt->su1 < 0 || Opt->su1 > 1.5) {
            fprintf (SUMA_STDERR, "%f is a bad smoothness factor (acceptable range is 0 to 1\nbut values up to 1.5 are allowed for amusement).\n", Opt->su1);
				exit (1);
         }
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-ld") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -ld \n");
				exit (1);
			}
			Opt->Icold = atoi(argv[kar]);
         if (Opt->Icold < 0 || Opt->Icold > 300) {
            fprintf (SUMA_STDERR, "%d is a bad iteration number.\n", Opt->Icold);
				exit (1);
         }
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-debug") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -debug \n");
				exit (1);
			}
			Opt->debug = atoi(argv[kar]);
         if (Opt->debug > 2) { LocalHead = YUP; }
         brk = YUP;
		}      
      
     
      if (!brk && (strcmp(argv[kar], "-input") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -input \n");
				exit (1);
			}
			Opt->in_name = SUMA_copy_string(argv[kar]);
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-prefix") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -prefix \n");
				exit (1);
			}
			Opt->out_vol_prefix = SUMA_copy_string(argv[kar]);
         brk = YUP;
		}
      
      
      if (!brk && ( (strcmp(argv[kar], "-touchup") == 0) ) ) {
         Opt->UseNew = 1.0;
         brk = YUP;
      }

      if (!brk && ( (strcmp(argv[kar], "-no_touchup") == 0) ) ) {
         Opt->UseNew = 0.0;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-k98") == 0)) {
         SUMA_SL_Warn("Bad option, causes trouble (big clipping) in other parts of brain.");
         Opt->Kill98 = 1;
         brk = YUP;
      }
      
      if (!brk && ( (strcmp(argv[kar], "-noeyes") == 0) || (strcmp(argv[kar], "-no_eyes") == 0) || (strcmp(argv[kar], "-avoid_eyes") == 0)) ) {
         Opt->NoEyes = 1;
         brk = YUP;
      }
      
      if (!brk && ( (strcmp(argv[kar], "-no_noeyes") == 0) || (strcmp(argv[kar], "-no_no_eyes") == 0) || (strcmp(argv[kar], "-no_avoid_eyes") == 0)) ) {
         Opt->NoEyes = 0;
         brk = YUP;
      }
      
      if (!brk && !ps->arg_checked[kar]) {
			fprintf (SUMA_STDERR,"Error %s:\nOption %s not understood. Try -help for usage\n", FuncName, argv[kar]);
			exit (1);
		} else {	
			brk = NOPE;
			kar ++;
		}
   }
   
   /* transfer some options to Opt from ps. Clunky because this is retrofitting */
   if (ps->o_N_surfnames) {
      Opt->out_prefix = SUMA_copy_string(ps->o_surfnames[0]);
      Opt->SurfFileType = ps->o_FT[0];
      Opt->SurfFileFormat = ps->o_FF[0];
   }
   
   if (!Opt->in_name) {
      fprintf (SUMA_STDERR,"Error %s:\n-input  must be used.\n", FuncName);
      exit(1);
   }
   
   if (!Opt->out_prefix) Opt->out_prefix = SUMA_copy_string("brainwrap_out");
   if (!Opt->out_vol_prefix) {
      if (!Opt->out_prefix) Opt->out_vol_prefix = SUMA_copy_string("brainwrap_out");
      else Opt->out_vol_prefix = SUMA_copy_string(Opt->out_prefix);
   }
   
   if (Opt->t2 >= 0 || Opt->t98 >= 0 || Opt->tm >= 0  || Opt->t >= 0 ) {
      if (!(Opt->t2 >= 0 && Opt->t98 > 0 && Opt->tm > 0 && Opt->t > 0)){
         fprintf (SUMA_STDERR,"Error %s: \nAll or none of the t parameters are to be specified on command line:\nt2 %f t %f tm %f t98 %f\n", 
            FuncName, Opt->t2, Opt->t, Opt->tm, Opt->t98);
         exit(1);
      }
   }
   SUMA_RETURN(Opt);
}




int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"3dSkullStrip"}; 
	int i, N_in = 0, i3, kth_buf, hull_ld;
   int ii,jj,kk,ll,ijk , nx,ny,nz , nxyz , nn, nint = 0 , nseg;
   void *SO_name=NULL, *SO_name_hull=NULL;
   float vol, *isin_float=NULL, pint, *dsmooth = NULL;
   SUMA_SurfaceObject *SO = NULL, *SOhull=NULL;
   SUMA_ISOSURFACE_OPTIONS *Opt;  
   char  stmp[200], stmphull[200], *hullprefix=NULL;
   SUMA_Boolean exists = NOPE;
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   short *isin = NULL;
   SUMA_FORM_AFNI_DSET_STRUCT *OptDs = NULL;
   THD_3dim_dataset *dset = NULL;
   
   SUMA_Boolean LocalHead = NOPE;

	SUMA_mainENTRY;
   SUMA_STANDALONE_INIT;
   

   /* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "-o;-i;-sv;-talk;");
   
   if (argc < 2) {
      usage_SUMA_BrainWrap(ps);
      exit (1);
   }
   
   Opt = SUMA_BrainWrap_ParseInput (argv, argc, ps);

   if (Opt->debug > 2) LocalHead = YUP;

   SO_name = SUMA_Prefix2SurfaceName(Opt->out_prefix, NULL, NULL, Opt->SurfFileType, &exists);
   if (exists) {
      fprintf(SUMA_STDERR,"Error %s:\nOutput file(s) %s* on disk.\nWill not overwrite.\n", FuncName, Opt->out_prefix);
      exit(1);
   }
   hullprefix = SUMA_append_string(Opt->out_prefix,"_hull");
   SO_name_hull = SUMA_Prefix2SurfaceName(hullprefix, NULL, NULL, Opt->SurfFileType, &exists);
   if (exists) {
      fprintf(SUMA_STDERR,"Error %s:\nOutput file(s) %s* on disk.\nWill not overwrite.\n", FuncName, hullprefix);
      exit(1);
   }   
   kth_buf = ps->cs->kth; /* keep track of required value */
   
   /* turn on debugging for inodes */
   #if 0
   SUMA_SL_Note("Debugging for eye nodes");
   Opt->dbg_eyenodes = fopen("eyenodes.1D.dset", "w");
   #endif
   
   /* Load the AFNI volume 
   This volume must have already been cleaned up by SpatNorm 
   Also create a convex hull*/
   if (Opt->UseSkull) { SOhull = SUMA_Alloc_SurfObject_Struct(1); }
   else SOhull = NULL;
   sprintf(stmphull,"OuterHull");  

   vol = SUMA_LoadPrepInVol (Opt, &SOhull);
   if ( vol <= 0 ) {
      SUMA_S_Err("Failed to load/prep volume");
      exit(1);
   } else {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Got me a volume of %f mm3\n", FuncName, vol);
   }
   
   
   
  do {   
      /* Now create that little sphere that is about to expand */
      sprintf(stmp,"icobaby_ld%d", Opt->Icold);  
      SO = SUMA_CreateIcosahedron (Opt->r/2.0, Opt->Icold, Opt->cog, "n", 1);
      if (!SO) {
         SUMA_S_Err("Failed to create Icosahedron");
         exit(1);
      }
      if (Opt->NoEyes) {
         if (Opt->Stop) SUMA_free(Opt->Stop); Opt->Stop = NULL;
         if (!Opt->Stop) {
            Opt->Stop = (float *)SUMA_calloc(SO->N_Node, sizeof(float));
            if (!Opt->Stop) {
               SUMA_SL_Crit("Failed to allocate");
               exit(1);
            }
         }
      }
      if (Opt->avoid_vent) {
         float U[3], Un, *a, P2[2][3];
         for (i=0; i<SO->N_Node; ++i) {
            /* stretch the top coordinates by d1 and the back too*/
            a = &(SO->NodeList[3*i]); 
            if (a[2] - SO->Center[2] > Opt->r/3 || a[1] - SO->Center[1] > Opt->r/2) {
               SUMA_UNIT_VEC(SO->Center, a, U, Un);
               SUMA_POINT_AT_DISTANCE_NORM(U, SO->Center, (Un+1.1*Opt->d1), P2);
               SO->NodeList[3*i] = P2[0][0]; SO->NodeList[3*i+1] = P2[0][1]; SO->NodeList[3*i+2] = P2[0][2];
            }
         }   
      }
      /* allocate and fix zt */
      Opt->ztv = (float *)SUMA_malloc(sizeof(float)*SO->N_Node);
      if (!Opt->ztv) {
         SUMA_SL_Crit("Failed to allocate");
         exit(1);
      } 
      for (i=0; i<SO->N_Node; ++i) Opt->ztv[i] = Opt->Zt;

      /* need sv for communication to AFNI */
      SO->VolPar = SUMA_VolPar_Attr (Opt->in_name);
      SO->SUMA_VolPar_Aligned = YUP; /* Surface is in alignment with volume, should not call SUMA_Align_to_VolPar ... */
   
      if (!SO->State) {SO->State = SUMA_copy_string("3dSkullStrip"); }
      if (!SO->Group) {SO->Group = SUMA_copy_string("3dSkullStrip"); }
      if (!SO->Label) {SO->Label = SUMA_copy_string(stmp); }
      /* make the idcode_str depend on the Label, it is convenient to
      send the same surface all the time to SUMA */
      if (SO->Label) { if (SO->idcode_str) SUMA_free(SO->idcode_str); SO->idcode_str = UNIQ_hashcode(SO->Label); }
      
      if (!nint && SOhull) {
         SOhull->VolPar = SUMA_VolPar_Attr (Opt->in_name);
         SOhull->SUMA_VolPar_Aligned = YUP; /* Surface is in alignment with volume, should not call SUMA_Align_to_VolPar ... */
   
         if (!SOhull->State) {SOhull->State = SUMA_copy_string("3dSkullStrip"); }
         if (!SOhull->Group) {SOhull->Group = SUMA_copy_string("3dSkullStrip"); }
         if (!SOhull->Label) {SOhull->Label = SUMA_copy_string(stmphull); }
         if (SOhull->Label) { if (SOhull->idcode_str) SUMA_free(SOhull->idcode_str); SOhull->idcode_str = UNIQ_hashcode(SOhull->Label); }
      }

      /* see if SUMA talk is turned on */
      if (ps->cs->talk_suma) {
         ps->cs->istream = SUMA_BRAINWRAP_LINE;
         ps->cs->kth = 1; /* make sure all surfaces get sent */
         if (!SUMA_SendToSuma (NULL, ps->cs, NULL, SUMA_NO_DSET_TYPE, 0)) {
            SUMA_SL_Err("Failed to initialize SUMA_SendToSuma");
            ps->cs->Send = NOPE;
            ps->cs->talk_suma = NOPE;
         } else {
            if (!nint && SOhull) {
               SUMA_LH("Sending Hull");
               if (Opt->DemoPause) { SUMA_PAUSE_PROMPT("Sending HULL next"); }
               SUMA_SendSumaNewSurface(SOhull, ps->cs);
            }
            SUMA_LH("Sending Ico");
            if (Opt->DemoPause) { SUMA_PAUSE_PROMPT("Sending Ico next"); }
            SUMA_SendSumaNewSurface(SO, ps->cs);

         }
         ps->cs->kth = kth_buf;
      }

      if (!nint && Opt->UseSkull) {
         /* get a crude mask of inner skull */
         if (Opt->DemoPause) { SUMA_PAUSE_PROMPT("Shrinking skull hull next"); }
         SUMA_SkullMask (SOhull, Opt, ps->cs);
         /* Now take mask and turn it into a volume */
         fprintf (SUMA_STDERR,"%s: Locating voxels on skull boundary  ...\n", FuncName);
         isin = SUMA_FindVoxelsInSurface (SOhull, SO->VolPar, &N_in);
         isin_float = (float *)SUMA_malloc(sizeof(float) * SO->VolPar->nx*SO->VolPar->ny*SO->VolPar->nz);
         if (!isin_float) {
            SUMA_SL_Crit("Failed to allocate");
            exit(1);
         }
         for (i=0; i<SO->VolPar->nx*SO->VolPar->ny*SO->VolPar->nz; ++i) { if (isin[i] <= SUMA_ON_NODE) Opt->dvec[i] = 0; }
         #if 0
            SUMA_SL_Note("Writing hull mask");
            {
               FILE *fout=fopen("hullmask.1D","w");
               int ii, jj, kk; 
               for (i=0; i<SO->VolPar->nx*SO->VolPar->ny*SO->VolPar->nz; ++i) { 
                  SUMA_1D_2_3D_index(i, ii, jj, kk, SO->VolPar->nx, SO->VolPar->nx*SO->VolPar->ny); 
                  fprintf(fout,"%d %d %d %d\n",ii, jj, kk, isin[i]); 
               }
               fclose(fout);
            }
         #endif
         if (isin) SUMA_free(isin); isin = NULL;
      }
            
      /* This is it baby, start walking */
      if (Opt->DemoPause) { SUMA_PAUSE_PROMPT("Brain expansion next"); }
      SUMA_StretchToFitLeCerveau (SO, Opt, ps->cs);

      /* check for intersections */
      if (Opt->PercInt >= 0) {
         fprintf(SUMA_STDERR,"%s: Checking for self intersection...\n", FuncName);
         nseg = 30 * Opt->Icold * Opt->Icold; /* number of segments in Ico */
         nint = SUMA_isSelfIntersect(SO, (int)(Opt->PercInt * nseg / 100.0));
         if (nint < 0) {
            SUMA_SL_Err("Error in SUMA_isSelfIntersect. Ignoring self intersection test.");
            nint = 0;
         }
         /* percentage of guilty segments */
         pint = (float)nint / (float)nseg * 100;
         if (LocalHead) fprintf (SUMA_STDERR,"%s: at least %d (%f%%) of segments intersect the surface.\nStopping criterion is %f%%\n", FuncName, nint, pint, Opt->PercInt);
         if (pint > Opt->PercInt) {
            static int SelfIntRep = 0;
            int smstep;
            if (SelfIntRep < Opt->MaxIntIter) {
               /* SUMA_PAUSE_PROMPT("SelfIntersection Baby"); */
               if (!Opt->avoid_vent && !SelfIntRep) {
                  Opt->avoid_vent = 1;
               } else {
                  smstep = 12 * ( Opt->Icold / 25 ) * ( Opt->Icold / 25 ); /* 12 is OK for ld = 25 */ 
                  if ( smstep < 12) smstep = 12;
                  else if ( smstep % 2 ) ++smstep;
                  #if 0
                  if (!(SelfIntRep % 2)) { 
                     Opt->avoid_vent = 0;
                     Opt->NNsmooth += smstep; 
                  } else {
                     Opt->avoid_vent = 1;
                  }
                  #else
                     /* don't mess with avoid_vent option, it works well all the time */
                     Opt->NNsmooth += smstep; 
                  #endif
                  ++SelfIntRep;
               }
               fprintf(SUMA_STDERR,"Warning %s:****************\n Surface self intersecting! trying again:\n smoothing of %d, avoid_vent %d\n", FuncName, Opt->NNsmooth, (int)Opt->avoid_vent);
               SUMA_Free_Surface_Object(SO); SO = NULL;
            } else {
               fprintf(SUMA_STDERR,"Warning %s: Stubborn intersection remaining at smoothing of %d. Ignoring it.", FuncName, Opt->NNsmooth);
               nint = 0;
            }
         } else  {
            if (nint) fprintf(SUMA_STDERR,"%s: Number of intersection below criterion.\n", FuncName);
            else fprintf(SUMA_STDERR,"%s: No intersections found.\n", FuncName);
            nint = 0;
         }
      } else {
         fprintf(SUMA_STDERR,"%s: Self intersection check turned off.\n", FuncName);
         nint = 0;   
      }
   } while (nint != 0);
   fprintf(SUMA_STDERR,"%s: Final smoothing of %d\n", FuncName, Opt->NNsmooth);
   
   /* touch up, these might cause some surface intersection, but their effects should be small */
   if (Opt->UseNew) {
         double mval = 255;
         fprintf (SUMA_STDERR,"%s: Touchup correction, pass 1 ...\n", FuncName);
         if (Opt->DemoPause) { SUMA_PAUSE_PROMPT("touchup correction next"); }
         /* recover the eye balls please */
         if (mval < Opt->t98) {
            SUMA_SL_Warn("Looks like some values in dset might be larger than 255 !");
            mval = Opt->t98+10;
         }
         if (Opt->k98maskcnt && Opt->k98mask) { for (ii=0; ii<Opt->k98maskcnt; ++ii) Opt->dvec[Opt->k98mask[ii]] = mval; }
         SUMA_REPOSITION_TOUCHUP(6);
         #if 0
         /* smooth the surface a bit */
         ps->cs->kth = 1;
         dsmooth = SUMA_Taubin_Smooth( SO, NULL,
                                    0.6307, -.6732, SO->NodeList,
                                    8, 3, SUMA_ROW_MAJOR, dsmooth, ps->cs);    
         memcpy((void*)SO->NodeList, (void *)dsmooth, SO->N_Node * 3 * sizeof(float));
         SUMA_RECOMPUTE_NORMALS(SO);
         if (ps->cs->Send) {
            if (!SUMA_SendToSuma (SO, ps->cs, (void *)SO->NodeList, SUMA_NODE_XYZ, 1)) {
               SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
            }
         }
         ps->cs->kth = kth_buf;
         SUMA_REPOSITION_TOUCHUP(3); 
         #endif
         if (LocalHead) fprintf (SUMA_STDERR,"%s: Touchup correction  Done.\n", FuncName);
   }
   
   /* smooth the surface a bit */
   if (Opt->smooth_end) {
      ps->cs->kth = 1;  /*make sure all gets sent at this stage */
      if (Opt->DemoPause) { SUMA_PAUSE_PROMPT("beauty treatment smoothing next"); }
      fprintf (SUMA_STDERR,"%s: The beauty treatment smoothing.\n", FuncName);
      dsmooth = SUMA_Taubin_Smooth( SO, NULL,
                                    0.6307, -.6732, SO->NodeList,
                                    Opt->smooth_end, 3, SUMA_ROW_MAJOR, dsmooth, ps->cs);    
      memcpy((void*)SO->NodeList, (void *)dsmooth, SO->N_Node * 3 * sizeof(float));
      SUMA_RECOMPUTE_NORMALS(SO);
      if (ps->cs->Send) {
         if (!SUMA_SendToSuma (SO, ps->cs, (void *)SO->NodeList, SUMA_NODE_XYZ, 1)) {
            SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
         }
      }
      ps->cs->kth = kth_buf; 
   }
   
   /* one more correction pass */
   if (Opt->UseNew) {
      ps->cs->kth = 1; /*make sure all gets sent at this stage */
      if (Opt->DemoPause) { SUMA_PAUSE_PROMPT("touchup correction 2 next"); }
      fprintf (SUMA_STDERR,"%s: Final touchup correction ...\n", FuncName);
      SUMA_REPOSITION_TOUCHUP(2);
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Final touchup correction  Done.\n", FuncName);
      ps->cs->kth = kth_buf; 
   }

   /* write the surfaces to disk */
   fprintf (SUMA_STDERR,"%s: Writing surface  ...\n", FuncName);
   if (!SUMA_Save_Surface_Object (SO_name, SO, Opt->SurfFileType, Opt->SurfFileFormat, NULL)) {
      fprintf (SUMA_STDERR,"Error %s: Failed to write surface object.\n", FuncName);
      exit (1);
   }
   if (Opt->UseSkull && SOhull) {
      fprintf (SUMA_STDERR,"%s: Writing skull surface  ...\n", FuncName);
      if (!SUMA_Save_Surface_Object (SO_name_hull, SOhull, Opt->SurfFileType, Opt->SurfFileFormat, NULL)) {
         fprintf (SUMA_STDERR,"Error %s: Failed to write surface object.\n", FuncName);
         exit (1);
      }
   }
   
   /* what voxels are inside the surface ? */
   fprintf (SUMA_STDERR,"%s: Locating voxels inside surface  ...\n", FuncName);
   isin = SUMA_FindVoxelsInSurface (SO, SO->VolPar, &N_in);
   isin_float = (float *)SUMA_malloc(sizeof(float) * SO->VolPar->nx*SO->VolPar->ny*SO->VolPar->nz);
   if (!isin_float) {
      SUMA_SL_Crit("Failed to allocate");
      exit(1);
   }
   for (i=0; i<SO->VolPar->nx*SO->VolPar->ny*SO->VolPar->nz; ++i) isin_float[i] = (float)isin[i];
   if (isin) SUMA_free(isin); isin = NULL;
      
   fprintf (SUMA_STDERR,"%s: Writing mask volume  ...\n", FuncName);
   OptDs = SUMA_New_FormAfniDset_Opt();
   if (Opt->out_vol_prefix) {
      SUMA_FileName NewName = SUMA_StripPath(Opt->out_vol_prefix);
      OptDs->prefix = NewName.FileName; NewName.FileName = NULL;
      OptDs->prefix_path = NewName.Path; NewName.Path = NULL;
   }  else {
      OptDs->prefix = SUMA_copy_string("3dSkullStrip");
      OptDs->prefix_path = SUMA_copy_string("./");
   }
   OptDs->master = SUMA_copy_string(Opt->in_name);
   OptDs->full_list = 1;
   OptDs->dval = 1;
   dset = SUMA_FormAfnidset (NULL, isin_float, SO->VolPar->nx*SO->VolPar->ny*SO->VolPar->nz, OptDs);
   if (!dset) {
      SUMA_SL_Err("Failed to create output dataset!");
   } else {
      tross_Make_History( FuncName , argc,argv , dset ) ;
      DSET_write(dset) ;
   }
   
   /* you don't want to exit rapidly because the SUMA might not be done processing the last elements*/
   if (ps->cs->Send && !ps->cs->GoneBad) {
      /* cleanup and close connections */
      if (!SUMA_SendToSuma (SO, ps->cs, NULL, SUMA_NODE_XYZ, 2)) {
         SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCleanup failed");
      }
   }   
   
   if (dsmooth) SUMA_free(dsmooth); dsmooth = NULL;
   if (OptDs) { OptDs = SUMA_Free_FormAfniDset_Opt(OptDs);  }
   if (dset) { DSET_delete(dset); dset = NULL; }
   if (isin) { SUMA_free(isin); isin = NULL; }
   if (isin_float) { SUMA_free(isin_float); isin_float = NULL; }
   if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   if (Opt->dbg_eyenodes) fclose(Opt->dbg_eyenodes); Opt->dbg_eyenodes = NULL;
   if (Opt->k98mask) SUMA_free(Opt->k98mask); Opt->k98mask = NULL;
   if (Opt->Stop) SUMA_free(Opt->Stop); Opt->Stop = NULL;
   if (Opt->dvec) SUMA_free(Opt->dvec); Opt->dvec = NULL;
   if (Opt->mcdatav) {SUMA_free(Opt->mcdatav); Opt->mcdatav = NULL;} 
   if (Opt->in_vol) { DSET_delete( Opt->in_vol); Opt->in_vol = NULL;} 
   if (Opt->out_prefix) SUMA_free(Opt->out_prefix); Opt->out_prefix = NULL;
   if (Opt->out_vol_prefix) SUMA_free(Opt->out_vol_prefix); Opt->out_vol_prefix = NULL;
   if (Opt->XYZ) SUMA_free(Opt->XYZ); Opt->XYZ = NULL;
   if (Opt->ztv) SUMA_free(Opt->ztv); Opt->ztv = NULL;
   if (Opt) SUMA_free(Opt);
   if (hullprefix) SUMA_free(hullprefix); hullprefix = NULL;
   if (SO_name_hull) SUMA_free(SO_name_hull); SO_name_hull = NULL;
   if (SO_name) SUMA_free(SO_name); SO_name = NULL;
   if (SO) SUMA_Free_Surface_Object(SO); SO = NULL;
   if (SOhull) SUMA_Free_Surface_Object(SOhull); SOhull = NULL;
   if (!SUMA_Free_Displayable_Object_Vect (SUMAg_DOv, SUMAg_N_DOv)) {
      SUMA_SL_Err("DO Cleanup Failed!");
   }
   if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   exit(0);
}   
#endif
