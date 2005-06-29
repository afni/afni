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

static int InteractiveQuit;

int SUMA_DidUserQuit(void)
{
   return(InteractiveQuit);
}

/* 
   volume returned is negative if failed to read volume. 
   Volume units are in mm3
   \brief Volume preparations for later stripping:
   -1 Run SpatNorm (Not done here at the moment)
   -2 Get percentile ranges and sphere center a la BET 
      (Fast robust automated brain extraction, Stephen M. Smith, HBM 2002 v 17:3 pp 143-155)
   -3 Get convex hull of outer surface and shrink to set limit on expansion
*/         
float SUMA_LoadPrepInVol (SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt, SUMA_SurfaceObject **SOhull)
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
   
   
   Opt->nvox = DSET_NVOX( Opt->in_vol );
   if (DSET_NVALS( Opt->in_vol) != 1) {
      SUMA_SL_Err("Input volume can only have one sub-brick in it.\nUse [.] selectors to choose sub-brick needed.");
      SUMA_RETURN(vol);
   }
   
   Opt->VolCM[0] = THD_BN_XCM; Opt->VolCM[1] = THD_BN_YCM; Opt->VolCM[2] = THD_BN_ZCM;
   
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
   if (LocalHead) {
         fprintf (SUMA_STDERR,"%s: Volume %f, radius %f\n", FuncName, vol, Opt->r);
   }
   
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
                                 Means[1] mean value below node (only for vals > Opt->t),
                                 Means[2] mean value above node  (only for vals > Opt->t)
*/
int SUMA_Find_IminImax (SUMA_SurfaceObject *SO, SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt, int ni, 
                        float *MinMax, float *MinMax_dist , float *MinMax_over, float *MinMax_over_dist,
                        float *Means, float *undershish, float *overshish, int *dvecind_under, int *dvecind_over, int ShishMax)
{
   static char FuncName[]={"SUMA_Find_IminImax"};
   float d1, d2, travdir[3], d1t, d2t, lmin, lmax, lmin_dist, lmax_dist;
   float t2 = Opt->t2, tm = Opt->tm, t = Opt->t; 
   THD_fvec3 ncoord, ndicom;
   THD_ivec3 nind3;
   int nind, istep, istepmax, istep2max, nxx, nxy, nMeans[3], stopint;
   SUMA_ENTRY;
   
   d1 = Opt->d1; d2 = d1/2.0; 
   
   Means[0] = Means[1] = Means[2] = 0.0;
   nMeans[0] = nMeans[1] = nMeans[2] = 0;
   lmin = Opt->tm;
   lmin_dist = 0.0;
   lmax_dist = 0.0;
   lmax = Opt->t;
   nxx = DSET_NX(Opt->in_vol);
   nxy = DSET_NX(Opt->in_vol) * DSET_NY(Opt->in_vol);
   istep = 0; istepmax = (int)(ceil((double)d1/Opt->travstp)); istep2max = (int)(ceil((double)d2/Opt->travstp));
   stopint = 0;
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
      
      /* track the brain mean, only if the value is above brain non-brain threshold
      stop inegrating as soon as you hit nonbrain */
      if (Opt->dvec[nind] > Opt->t && !stopint) { Means[1] += Opt->dvec[nind]; ++ nMeans[1]; }
      else stopint = 1;
      if (dvecind_under) dvecind_under[istep] = nind;
        
      /* find local min */ 
      /* lmin = SUMA_MIN_PAIR(lmin, Opt->dvec[nind]); */
      if (lmin > Opt->dvec[nind]) { lmin = Opt->dvec[nind]; SUMA_NORM_VEC(travdir, 3, lmin_dist); }
      
      if (istep <= istep2max) {
         if (lmax < Opt->dvec[nind]) { lmax = Opt->dvec[nind]; SUMA_NORM_VEC(travdir, 3, lmax_dist); }  
      }
      
      ++istep;
   }
   
   if (istep < ShishMax) { undershish[istep] = -1; if (dvecind_under) dvecind_under[istep] = -1; }/* crown the shish */
   
   Means[1] /= (float)nMeans[1];
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
      stopint = 0;
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
         
         if (Opt->dvec[nind] > Opt->t && !stopint) { Means[2] += Opt->dvec[nind]; ++ nMeans[2]; } 
         else stopint = 1;
         if (dvecind_over) dvecind_over[istep] = nind;
         
         /* find local min and max*/ 
         if (lmin > Opt->dvec[nind]) { lmin = Opt->dvec[nind]; SUMA_NORM_VEC(travdir, 3, lmin_dist); }
         if (lmax < Opt->dvec[nind]) { lmax = Opt->dvec[nind]; SUMA_NORM_VEC(travdir, 3, lmax_dist); }  
         
         ++istep;
      }
      
      if (istep < ShishMax) { overshish[istep] = -1; if (dvecind_over) dvecind_over[istep] = -1; }/* crown the shish */

      Means[2] /= (float)nMeans[2];
      MinMax_over[0] = lmin; 
      MinMax_over_dist[0] = lmin_dist;  
      MinMax_over[1] = lmax;
      MinMax_over_dist[1] = lmax_dist;
   }
   
   SUMA_RETURN(YUP);
}

/*! 
   \brief creates a crude mask of the brain's limit based on the surface of the skull
   mask is crude, for purposes of keeping BrainWrap from leaking out when there are 
   very strong shading artifacts
*/
int SUMA_SkullMask (SUMA_SurfaceObject *SO, SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt, SUMA_COMM_STRUCT *cs)
{
   static char FuncName[]={"SUMA_SkullMask"};
   int it=0, in=0, Npass, Done, ShishMax, i_diffmax, kth_buf, N_it;
   float *n=NULL, *undershish = NULL, diffmax;
   float *tmpptr, *NewCoord = NULL, f3, f4, *dsmooth = NULL;
   float *refNodeList = NULL, sksearch = 15;
   float U[3], Un;
   int  nx, nxy, n_stp;
   SUMA_Boolean DoDbg=NOPE, Send_buf;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (Opt->debug > 2) LocalHead = YUP;

   nx = DSET_NX(Opt->in_vol);
   nxy = nx * DSET_NY(Opt->in_vol);   
   
   kth_buf = cs->kth;
   Send_buf = cs->Send;
   if (!Opt->send_hull) {
      cs->Send = NOPE;
   }   

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

            /* using normals for direction, NORMALS SHOULD POINT OUTWARDS...*/
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
                                    8, 3, SUMA_ROW_MAJOR, dsmooth, cs, NULL);    
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
   
   cs->Send = Send_buf;

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
int SUMA_StretchToFitLeCerveau (SUMA_SurfaceObject *SO, SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt, SUMA_COMM_STRUCT *cs)
{
   static char FuncName[]={"SUMA_StretchToFitLeCerveau"};
   int it=0,it0 = 0, in=0, ii, Npass, Done, ShishMax, i_diffmax, kth_buf, nit, Stage;
   float mnc[3], s[3], sn[3], st[3], dp, threshclip, lztfac,
         nsn, rmin, rmax, E, F, su1, su2, su3, su4,  
         r, u1[3], u2[3], u3[3], u[3],
         tm=0.0, t2 = 0.0, t98 = 0.0, Imin, Imax, l=0.0, MinMax[2], 
         MinMax_dist[2],MinMax_over[2], MinMax_over_dist[2], Means[3],
         *n=NULL, tb = 0.0, tbe = 0.0, t = 0.0, Imin_over=0.0, Imin_dist_over=0.0,
         *overshish = NULL, *undershish = NULL, diffmax, *touchup=NULL, *a, P2[2][3], *norm;
   float *tmpptr, *NewCoord = NULL, f3, f4, lZt, *dsmooth = NULL;
   float *refNodeList = NULL, *OrigNodeList=NULL;
   double MaxExp;
   int keepgoing=0, N_troub, past_N_troub;
   double pastarea, curarea, darea;
   char cbuf;
   FILE *OutNodeFile = NULL;
   SUMA_Boolean DoDbg=NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   InteractiveQuit = 0;
   
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
   
   pastarea = 0.0; curarea = 0.0; darea = 0.0, past_N_troub = 0;
   Npass = 0;   Done = 0; nit = Opt->N_it; it0 = 0;
   do {
      Stage = 0; /* reset stage */
      if (Opt->debug > 1) fprintf(SUMA_STDERR,"%s: Pass #%d\n", FuncName, Npass);
      MaxExp = 0.0; /* maximum expansion of any node */
      for (it=it0; it < nit; ++it) {
         SUMA_MEAN_SEGMENT_LENGTH(SO, l);
         if (LocalHead && !(it % 50)) fprintf (SUMA_STDERR,"%s: Iteration %d, l = %f . SO->Center = %f, %f, %f...\n", 
            FuncName, it, l, SO->Center[0], SO->Center[1], SO->Center[2]);
         if (Opt->var_lzt) {
            if (it <= Opt->N_it) lztfac = SUMA_MAX_PAIR(0.2, (1.2* (float)it / (float)Opt->N_it));  /* make things expand quickly in the beginning, to help escape large sections of csf close to outer surface, then tighten grip towards the end*/
            else lztfac = 1.2;
         } else lztfac = 1.0;
         MaxExp = 0.0; /* maximum expansion of any node */
         for (in=0; in<SO->N_Node; ++in) {
            n = &(SO->NodeList[3*in]);
               SUMA_MEAN_NEIGHB_COORD(SO, in, mnc);
               s[0] = mnc[0] - n[0]; s[1] = mnc[1] - n[1]; s[2] = mnc[2] - n[2];
               SUMA_DOTP_VEC(s, &(SO->NodeNormList[3*in]), dp, 3, float, float);  /* dp is the dot product of s with the normal at in */ 
               SUMA_SCALE_VEC(&(SO->NodeNormList[3*in]), sn, dp, 3, float, float); /* sn is the component of s along normal */
               SUMA_NORM_VEC(sn, 3, nsn); /* norm of sn */
               SUMA_SUB_VEC(s, sn, st, 3, float, float, float);  /* st is the tangential component of s along normal */
               SUMA_SCALE_VEC(st, u1, su1, 3, float, float); /* u1 = st * su1 */

               r = ( l * l ) / (2.0 * nsn);
               su2 = ( 1 + tanh(F*(1/r - E)))/2.0;
               SUMA_SCALE_VEC(sn, u2, su2, 3, float, float); /* u2 = sn * su2 */

               SUMA_Find_IminImax(SO, Opt, in, MinMax, MinMax_dist, MinMax_over, MinMax_over_dist, Means, undershish, overshish, NULL, NULL, ShishMax);  
               Imin = MinMax[0]; Imax = MinMax[1];

               if (n[2] - SO->Center[2] < -25) { lZt = SUMA_MAX_PAIR (Opt->bot_lztclip, (Opt->ztv[in] * lztfac)); } /* clip lZt at no less than 0.5 for lowest sections, 
                                                                                                            otherwise you might go down to the dumps */
               else { lZt = Opt->ztv[in] * lztfac;  }

               if (lZt > 1) lZt = 0.999;

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
               /* freezing: a node that has been frozen, freezing may be used during final touchup*/
               if (Opt->Stop[in] < 0) { 
                  su3 = 0; su4 = 0;   
                  if (in == Opt->NodeDbg) fprintf(SUMA_STDERR,"%s:\nNode %d has been frozen\n", FuncName, in);
               }
               
               u[0] = su1 * st[0] + su2 * sn[0] + ( su3 + su4 ) * SO->NodeNormList[3*in]   ;
               u[1] = su1 * st[1] + su2 * sn[1] + ( su3 + su4 ) * SO->NodeNormList[3*in+1] ;
               u[2] = su1 * st[2] + su2 * sn[2] + ( su3 + su4 ) * SO->NodeNormList[3*in+2] ; 
               if (0 && (in == Opt->NodeDbg)) { 
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
               MaxExp = SUMA_MAX_PAIR (MaxExp, ( sqrt(u[0]*u[0]+u[1]*u[1]+u[2]*u[2]) ) );
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
      ++Stage;
      if (Opt->DemoPause == SUMA_3dSS_INTERACTIVE) {
         fprintf (SUMA_STDERR,"3dSkullStrip Interactive: \n"
                              "Increasing number of iterations to reach minimal expansion.\n"
                              "Do you want to (C)ontinue, (P)ass or (S)ave this? [C] ");
         cbuf = SUMA_ReadCharStdin ('c', 0,"csp");
         fprintf (SUMA_STDERR,"%c\n", cbuf);
         switch (cbuf) {
            case 's':   
               fprintf (SUMA_STDERR,"Will check surface, save mask and exit.\n");
               InteractiveQuit = 1;
               goto CLEAN_RETURN;
               break;
            case 'p':
               fprintf (SUMA_STDERR,"Passing this stage \n");
               goto CLEAN_RETURN;
               break;
            case 'c':
               fprintf (SUMA_STDERR,"Continuing with stage.\n");
               break;
         }                 
      }
      if (Stage == 1) { /* if the surface is still growing, keep going */
         if (Opt->DemoPause == SUMA_3dSS_DEMO_PAUSE) { SUMA_PAUSE_PROMPT("About to be in stage 1"); }
         if (LocalHead) fprintf (SUMA_STDERR,"%s: \n In stage 1\n", FuncName);
         if (MaxExp > 0.5) {
            /* Now, if you still have expansion, continue */
            if (!pastarea) { /* first time around, calculate area */
               pastarea = SUMA_Mesh_Area(SO, NULL, -1);
               if (LocalHead) fprintf (SUMA_STDERR,"%s: \n Stage1: pastarea = %f\n", FuncName, pastarea);
               keepgoing = 1;
            }else {
               curarea = SUMA_Mesh_Area(SO, NULL, -1);
               darea = ( curarea - pastarea ) / pastarea; 
               if (SUMA_ABS(darea) > 0.1) {
                  keepgoing = 1;
               } else { 
                  keepgoing = 0;
               }
               pastarea = curarea;
            }
            if (keepgoing) {
               it0 = nit; nit = nit + (int)(Opt->N_it/2.5);
               if (LocalHead) fprintf (SUMA_STDERR,"%s: \n Stage1: MaxExp = %f, darea = %f, going for more...\n", FuncName, MaxExp, darea);
               Done = 0;
            } else {
               if (LocalHead) fprintf (SUMA_STDERR,"%s: \n Stage1: satiated, small area differential.\n", FuncName);
               ++Stage;
            }
         } else {
            if (LocalHead) fprintf (SUMA_STDERR,"%s: \n Stage1: satiated, low MaxExp\n", FuncName);
            ++Stage;
         }
      }
      if (Stage == 2) {
         if (0) {
            if (Opt->DemoPause == SUMA_3dSS_DEMO_PAUSE) { SUMA_PAUSE_PROMPT("About to be in stage 2"); }
            touchup = SUMA_Suggest_Touchup_Grad(SO, Opt, 4.0, cs, &N_troub);
            if (!touchup) SUMA_SL_Warn("Failed in SUMA_Suggest_Touchup");
         } else {
            SUMA_LH( "Bypasing, SUMA_Suggest_Touchup_Grad is bad,"
                     " need serious 3D edge detection. "
                     " And cone shaped search zone for the gradient crossing  ... ");
            N_troub = 0;
         }
         
         if (!N_troub) { 
            /* No touchup, done */
            ++Stage;
         } else {
            if (touchup) SUMA_free(touchup); touchup = NULL;
            ++Stage;  
         }
      }
      if (Stage == 3) {
         if (Opt->DemoPause == SUMA_3dSS_DEMO_PAUSE) { SUMA_PAUSE_PROMPT("About to be in stage 3"); }
         /* see if there is room for extension */
         touchup = SUMA_Suggest_Touchup(SO, Opt, 4.0, cs, &N_troub);
         if (!touchup) SUMA_SL_Warn("Failed in SUMA_Suggest_Touchup");
         if (!N_troub) { 
            /* No touchup, done */
            Done = 1;
         } else {
            /* reduce tightness where expansion is needed */
            if (LocalHead) {
               fprintf(SUMA_STDERR,"%s:\n reducing tightness, applying touchup\n", FuncName);
            }
            for (in=0; in<SO->N_Node; ++in) {
               if (touchup[in]) {
                  if (Opt->NodeDbg == in) fprintf(SUMA_STDERR,"%s: Acting on node %d, touchup[in] %f, past shrink_fac = %f\n", 
                                                         FuncName, in, touchup[in], Opt->ztv[in]);                  
                  if (touchup[in] < 1) Opt->ztv[in] *= 0.8;
                  else if (touchup[in] < 2) Opt->ztv[in] *= 0.7;
                  else if (touchup[in] < 3) Opt->ztv[in] *= 0.6;
                  else if (touchup[in] < 4) Opt->ztv[in] *= 0.5;
                  else Opt->ztv[in] *= 0.4;
               }
            }
            it0 = nit; nit = nit + (int)(Opt->N_it/2.5);
            if (LocalHead) fprintf (SUMA_STDERR,"%s: \n Stage3: %d troubled nodes, going for more...\n", FuncName, N_troub);
            Done = 0;
            
            if (!past_N_troub) { past_N_troub = 0; }
            else {
               float dtroub;
               dtroub = (float)(past_N_troub - N_troub)/(float)past_N_troub;
               if (SUMA_ABS(dtroub) > 0.2) {
                  Done = 0; /* continue */
               } else {
                  Done = 1; /* done deal */
               }
               past_N_troub = N_troub;
            }
         }
         
         if (touchup) SUMA_free(touchup); touchup = NULL;
      }
      if (Stage > 3) {
         SUMA_SL_Err("Stage number invalid");
         Done = 1;
      }
      /* put a limit */
      if ( (nit > 3 * Opt->N_it) && !Done) {
         SUMA_LH("Funding limit reached. Abandoning improvements");
         Done = 1;
      }
   } while (!Done);
      
   CLEAN_RETURN:
   if (undershish) SUMA_free(undershish); undershish = NULL;
   if (overshish) SUMA_free(overshish); overshish = NULL;
   if (OrigNodeList) SUMA_free(OrigNodeList); OrigNodeList = NULL;
   if (refNodeList) SUMA_free(refNodeList); refNodeList = NULL;
   if (dsmooth) SUMA_free(dsmooth); dsmooth = NULL;
   if (OutNodeFile) fclose(OutNodeFile); OutNodeFile = NULL;
   if (NewCoord) SUMA_free(NewCoord); NewCoord = NULL;
   SUMA_RETURN(YUP);
}

#define MSK_DBG 0
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
byte *SUMA_FindVoxelsInSurface_SLOW (SUMA_SurfaceObject *SO, SUMA_VOLPAR *VolPar, int *N_inp, int fillhole) 
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
   SUMA_vec_dicomm_to_3dfind (tmpXYZ, SO->N_Node, VolPar);
   
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
short *SUMA_SurfGridIntersect (SUMA_SurfaceObject *SO, float *NodeIJKlist, SUMA_VOLPAR *VolPar, int *N_inp, int fillhole, THD_3dim_dataset *fillmaskset)
{
   static char FuncName[]={"SUMA_SurfGridIntersect"};
   short *isin=NULL;
   byte *ijkmask=NULL, *inmask = NULL, *ijkout = NULL;
   float *p1, *p2, *p3, min_v[3], max_v[3], p[3], dist;
   float MaxDims[3], MinDims[3], SOCenter[3], dxyz[3];
   int nn, nijk, nx, ny, nz, nxy, nxyz, nf, n1, n2, n3, nn3, *voxelsijk=NULL, N_alloc, en;
   int N_inbox, nt, nt3, ijkseed = -1, N_in, N_realloc;
   byte *fillmaskvec=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (SO->FaceSetDim != 3 || SO->NodeDim != 3) {
      SUMA_SL_Err("SO->FaceSetDim != 3 || SO->NodeDim != 3"); 
      SUMA_RETURN(NULL);
   }
   
   nx = VolPar->nx; ny = VolPar->ny; nz = VolPar->nz; nxy = nx * ny; nxyz = nx * ny * nz;
   

   isin = (short *)SUMA_calloc(nxyz, sizeof(short));
   if (!isin) {
      SUMA_SL_Crit("Failed to allocate");
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
   N_alloc = 2000; /* expected maximum number of voxels in triangle's bounding box */
   N_realloc = 0;
   voxelsijk = (int *)SUMA_malloc(sizeof(int)*N_alloc*3);
   if (!voxelsijk) { SUMA_SL_Crit("Failed to Allocate!"); SUMA_RETURN(NULL);  }   
   dxyz[0] = VolPar->dx; dxyz[1] = VolPar->dy; dxyz[2] = VolPar->dz;
   for (nf=0; nf<SO->N_FaceSet; ++nf) {
      n1 = SO->FaceSetList[SO->FaceSetDim*nf]; n2 = SO->FaceSetList[SO->FaceSetDim*nf+1]; n3 = SO->FaceSetList[SO->FaceSetDim*nf+2];
      /* find the bounding box of the triangle */
      p1 = &(NodeIJKlist[3*n1]); p2 = &(NodeIJKlist[3*n2]); p3 = &(NodeIJKlist[3*n3]); 
      SUMA_TRIANGLE_BOUNDING_BOX(p1, p2, p3, min_v, max_v)
      
      /* quick check of preallocate size of voxelsijk */
      en =((int)(max_v[0] - min_v[0] + 2) * (int)(max_v[1] - min_v[1] + 2) * (int)(max_v[2] - min_v[2] + 2)); 
      if ( en > N_alloc) {
         ++N_realloc; if (N_realloc > 5) { SUMA_SL_Warn("Reallocating, increase limit to improve speed.\nEither triangles too large or grid too small"); }
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
      
      /* mark these voxels as inside the business */
      for (nt=0; nt < N_inbox; ++nt) {
         nt3 = 3*nt;
         if (voxelsijk[nt3] < nx &&  voxelsijk[nt3+1] < ny &&  voxelsijk[nt3+2] < nz) {
            nijk = SUMA_3D_2_1D_index(voxelsijk[nt3], voxelsijk[nt3+1], voxelsijk[nt3+2], nx , nxy);  
            if (!isin[nijk]) { 
               /* what side of the plane is this voxel on ? */
               p[0] = (float)voxelsijk[nt3]; p[1] = (float)voxelsijk[nt3+1]; p[2] = (float)voxelsijk[nt3+2]; 
               SUMA_DIST_FROM_PLANE(p1, p2, p3, p, dist);
               
               if (dist) {
                  if (SUMA_IS_NEG(VolPar->Hand * dist)) isin[nijk] = SUMA_IN_TRIBOX_INSIDE;  /* ZSS Added handedness factor. who would have thought? Be damned 3D coord systems! */
                  else isin[nijk] = SUMA_IN_TRIBOX_OUTSIDE; 
               }
               /* does this triangle actually intersect this voxel ? */
               if (0) {
                  if (SUMA_isVoxelIntersect_Triangle (p, dxyz, p1, p2, p3)) {
                     isin[nijk] = SUMA_INTERSECTS_TRIANGLE;
                  }
               }
               ++(*N_inp);    
            }
         }
      }
   }
   
   /* Now fill in inside of the sphere */
      /* create a mask vector*/
      ijkmask = (byte *)SUMA_calloc(nxyz, sizeof(byte));
      ijkout = (byte *)SUMA_calloc(nxyz, sizeof(byte));
      if (!ijkmask) {
         SUMA_SL_Crit("Failed to allocate");
         SUMA_RETURN(NULL);
      }
      for (nt=0; nt<nxyz; ++nt) {
         if (isin[nt]) {
            ijkmask[nt] = 1;
            if (isin[nt] == SUMA_IN_TRIBOX_OUTSIDE) ijkout[nt] = 1;
         }
      }
      
      /* seed: find the center the surface in the index coordinate system*/
      SUMA_MIN_MAX_SUM_VECMAT_COL (NodeIJKlist, SO->N_Node, 3, MinDims, MaxDims, SOCenter); 
      SOCenter[0] /= SO->N_Node;  SOCenter[1] /= SO->N_Node;   SOCenter[2] /= SO->N_Node;
      {
         float u[3], un, p0[3], p1[3];
         int Found = 0, cnt;
         SUMA_MT_INTERSECT_TRIANGLE *mti = NULL; 

         /* Ray from a node on the surface to the center */
         p0[0] = NodeIJKlist[0]; p1[0] = SOCenter[0]; 
         p0[1] = NodeIJKlist[1]; p1[1] = SOCenter[1]; 
         p0[2] = NodeIJKlist[2]; p1[2] = SOCenter[2]; 
         SUMA_UNIT_VEC(p0, p1, u, un);
         /* travel along that ray until you find a point inside the surface AND not on the mask */
         Found = 0; cnt = 1;
         while (!Found && cnt <= un) {
            p1[0] = p0[0] + cnt * u[0];
            p1[1] = p0[1] + cnt * u[1];
            p1[2] = p0[2] + cnt * u[2];
            if (LocalHead) {
               fprintf(SUMA_STDERR,"%s:\nTrying seed ijk is %d %d %d\n", FuncName, (int)p1[0], (int)p1[1], (int)p1[2]); 
            }
            ijkseed = SUMA_3D_2_1D_index(p1[0], p1[1], p1[2], nx , nxy);
            mti = SUMA_MT_intersect_triangle(p1, SOCenter, NodeIJKlist, SO->N_Node, SO->FaceSetList, SO->N_FaceSet, mti);
            if (!(mti->N_poshits % 2)) { /* number of positive direction hits is a multiple of 2 */
               /* seed is outside */
               SUMA_LH("Seed outside");
            } else {
               SUMA_LH("Seed inside");
               /* seed is inside, is it on the mask ? */
               if (!ijkmask[ijkseed]) { SUMA_LH("Seed Accepted");Found = YUP; }
               else SUMA_LH("Seed on mask");
            }
            ++cnt;   
         }
         if (!Found) {
            SUMA_SL_Err("Failed to find seed!");
            if (isin) SUMA_free(isin); isin = NULL;
            goto CLEAN_EXIT;
         }
         if (mti) mti = SUMA_Free_MT_intersect_triangle(mti);
      }
      
      /* hide the mask values that are outside the surface */
      for (nt=0; nt<nxyz; ++nt) { if (ijkout[nt]) isin[nt] = 0; }
      
      if (fillmaskset) {
                  fillmaskvec = (byte *)SUMA_malloc(nx*ny*nz*sizeof(byte));
                  if (!fillmaskvec) { SUMA_SL_Crit("Failed to allocate fillmaskvec"); }
                  /* is this value 0 in the fillmaskset */
                  EDIT_coerce_scale_type( nx*ny*nz , DSET_BRICK_FACTOR(fillmaskset,0) ,
                           DSET_BRICK_TYPE(fillmaskset,0), DSET_ARRAY(fillmaskset, 0) ,      /* input  */
                           MRI_byte               , fillmaskvec  ) ;   /* output */
      }
      
      /* get the mask */
      if (ijkseed < 0) {
         SUMA_SL_Err("Should not be here!");
         if (isin) SUMA_free(isin); isin = NULL;
         goto CLEAN_EXIT;
      }
      inmask = SUMA_FillToVoxelMask(ijkmask, ijkseed, nx, ny, nz, &N_in, NULL); 
      if (!inmask) {
         SUMA_SL_Err("Failed to FillToVoxelMask!");
      } else {
         if (fillhole) { /* not a good idea to keep in SUMA_FillToVoxelMask */
            byte *inmask_prefill=NULL;
            
            /* keep track of original inmask */
            inmask_prefill = (byte *)SUMA_calloc(nx * ny * nz , sizeof(byte));
            memcpy ((void*)inmask_prefill, (void *)inmask, nx * ny * nz * sizeof(byte));

            if (LocalHead) fprintf(SUMA_STDERR,"%s:\n filling small holes %d...\n", FuncName, fillhole);
            /* fill in any isolated holes in mask */
            (void) THD_mask_fillin_once( nx,ny,nz , inmask , fillhole ) ;  /* thd_automask.c */
            /* remove filled holes that overlap with 0 values in mask */
            if (fillmaskvec) {
               for (nt=0; nt<nxyz; ++nt) {
                  if (inmask_prefill[nt] == 0 && inmask[nt] == 1 && fillmaskvec[nt] == 0) { /* if a voxel was not in mask before filling, then was filled as a hole but it is zero in the original dset, then fill it not */
                     inmask[nt] = 0;
                     if (LocalHead) fprintf(SUMA_STDERR,"%s: Shutting fill at %d\n", FuncName, nt);
                  }
               }
            }
            SUMA_free(inmask_prefill); inmask_prefill = NULL;
         }
         /* flag it */
         for (nt=0; nt<nxyz; ++nt) {
            if (inmask[nt] && !isin[nt]) {
               /* it would be nice to fill this value only if the intensity in the spat normed volume is not 0 
               The problem is that you have to transform coordinates back from this dataset back to the spatnormed set*/
                  isin[nt] = SUMA_INSIDE_SURFACE;
            }
         }
      }
      /* now put back the values outside the surface but protect values in mask */
      for (nt=0; nt<nxyz; ++nt) { if (ijkout[nt] && !inmask[nt]) isin[nt] = SUMA_IN_TRIBOX_OUTSIDE; }
   
   CLEAN_EXIT:   
   if (ijkout) SUMA_free(ijkout); ijkout = NULL;
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
short *SUMA_FindVoxelsInSurface (SUMA_SurfaceObject *SO, SUMA_VOLPAR *VolPar, int *N_inp, int fillhole, THD_3dim_dataset *fillmaskset) 
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
   SUMA_vec_dicomm_to_3dfind (tmpXYZ, SO->N_Node, VolPar);
   
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
   isin = SUMA_SurfGridIntersect (SO, tmpXYZ, VolPar, &N_in, fillhole, fillmaskset);               

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
   \brief A function to recommend node movement towards a better future.
   FAILS, mesh density too low. Need 3d edge mask....
   \sa SUMA_Reposition_Touchup
*/
float *SUMA_Suggest_Touchup_Grad(SUMA_SurfaceObject *SO, SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt, float limtouch, SUMA_COMM_STRUCT *cs, int *N_touch)
{
   static char FuncName[]={"SUMA_Suggest_Touchup_Grad"};
   int in, N_troub = 0, cond1=0, cond2=0, cond3 = 0, cond4 = 0, nn, ShishMax, *dvecind_under, *dvecind_over, ni, nj, nk, nx, ny, nxy;   
   float MinMax_over[2], MinMax[2], MinMax_dist[2], MinMax_over_dist[2], Means[3], tb; 
   float U[3], Un, *a, P2[2][3], *norm, shft, *b, gradient_promise; 
   float *touchup=NULL, targ[3]; 
   float *overshish=NULL, *undershish=NULL, *gradovershish=NULL;
   static FILE *GradOut = NULL;
   SUMA_Boolean LocalHead = YUP;
   
   SUMA_ENTRY;
      
   SUMA_SL_Warn("Don't call me, good for nothing, mesh density too low to work well ...\n");
   SUMA_RETURN(NULL);
      
      if (!GradOut) GradOut = fopen("GradOut.1D", "wa");

      if (Opt->debug > 2) LocalHead = YUP;
      *N_touch = 0;
      nx = SO->VolPar->nx; ny = SO->VolPar->ny; nxy = nx * ny;
      
      ShishMax = (int)(SUMA_MAX_PAIR(Opt->d1, Opt->d4) / Opt->travstp * 1.2);
      overshish = (float *)SUMA_malloc(sizeof(float)*ShishMax);
      undershish = (float *)SUMA_malloc(sizeof(float)*ShishMax);
      gradovershish = (float *)SUMA_malloc(sizeof(float)*ShishMax);
      dvecind_under = (int *)SUMA_malloc(sizeof(int)*ShishMax);
      dvecind_over = (int *)SUMA_malloc(sizeof(int)*ShishMax);
      touchup = (float *)SUMA_calloc(SO->N_Node, sizeof(float));   
      if (!touchup) { SUMA_SL_Crit("Failed to allocate"); SUMA_RETURN(NULL); }  
      for (in=0; in<SO->N_Node; ++in) {   
         SUMA_Find_IminImax(SO, Opt, in,  MinMax, MinMax_dist, MinMax_over, MinMax_over_dist, 
                           Means, undershish, overshish, dvecind_under, dvecind_over, ShishMax); 
         /* Shift the node outwards:   
            0- the minimum location of the minimum over the node is not 0
            AND
            1- if the minimum over the node is closer than the minimum below the node  
               or the internal minimum is more than 1mm away  and the internal min is masked to 0 
               or the minimum below the node just sucks, > tb and the minimum over the node is < tb
               The second part of the condition is meant to better model a finger of cortex that has enough csf on the inner surface 
               to have it be set to 0 by SpatNorm this dark csf would keep the brain surface from reaching the outer surface 
            AND   
            2- if the maximum over the node is much higher than the maximum below the node (fat on skull), 
               it must be beyond the location of the minimum over the node   
            AND   
            3- The new position of the node is not much larger than the current value
            AND
            4- There are no large negative gradients being crossed on the way out.  
         */ 
         tb = (MinMax[1] - Opt->t2) * 0.5 + Opt->t2; 
         cond1 = (    (MinMax_over_dist[0] < MinMax_dist[0]) 
                     || (MinMax_dist[0] > 1 && MinMax[0] >= MinMax_over[0] ) 
                     || (MinMax[0] > tb && MinMax_over[0] < MinMax[0]) ); 
         if (MinMax_over[1] > MinMax[1] && MinMax_over[1] > 0.9 * Opt->t98 && (MinMax_over_dist[1] < MinMax_over_dist[0]) ) cond2 = 0;  
         else cond2 = 1; 
         if (MinMax_over[0] > 1.2 * Means[0]) cond3 = 0;
         else cond3 = 1; 
         if (Opt->NodeDbg == in) {   
               a = &(SO->NodeList[3*in]);   
               fprintf(SUMA_STDERR, "%s: Debug during touchup for node %d:\n"   
                                    " MinMax     =[%.1f,%.1f] MinMax_dist     = [%.2f,%.2f] \n"   
                                    " MinMax_over=[%.1f,%.1f] MinMax_over_dist= [%.2f,%.2f] \n"   
                                    " Val at node %.1f, mean below %.1f, mean above %.1f\n"  
                                    " zalt= %f, r/2 = %f\n"    
                                    " Conditions: %f %d %d %d\n",    
                       FuncName, in, 
                       MinMax[0], MinMax[1], MinMax_dist[0], MinMax_dist[1], 
                       MinMax_over[0], MinMax_over[1], MinMax_over_dist[0], MinMax_over_dist[1], 
                       Means[0], Means[1], Means[2],   
                       a[2] - SO->Center[2], Opt->r/2, 
                       MinMax_over_dist[0] , cond1 , cond2, cond3); 
         }  
         /* Now search for the first location that has a negative gradient of 1/3 tm*/
         nn = 1;
         while(nn<ShishMax) {
            if (overshish[nn] >= 0) {
               gradovershish[nn-1] = overshish[nn] - overshish[nn-1];
               if (gradovershish[nn-1] < - Opt->tm/3.0) {
                  targ[0] = (nn - 1) * Opt->travstp * SO->NodeNormList[3*in]; 
                  targ[1] = (nn - 1) * Opt->travstp * SO->NodeNormList[3*in+1];
                  targ[2] = (nn - 1) * Opt->travstp * SO->NodeNormList[3*in+2];
                  if (dvecind_over[nn] < 0 || dvecind_over[nn] > Opt->nvox) {
                     fprintf(SUMA_STDERR,"Error %s: Bad value for  dvecind_over[%d]=%d\n", FuncName, nn, dvecind_over[in]);
                  } else {
                     if (MinMax_over_dist[0] && cond1 && cond2 && cond3 && Opt->dvec[dvecind_over[nn]]) { 
                        while (nn < ShishMax && dvecind_over[nn] >=0) {
                           Opt->dvec[dvecind_over[nn]] = 0;
                           if (LocalHead) { 
                              SUMA_1D_2_3D_index(dvecind_over[nn], ni, nj, nk, nx, nxy);
                              fprintf(SUMA_STDERR,"%s: Zeroing voxel %d %d %d, grad %f, from node %d\n", FuncName, ni, nj, nk, gradovershish[nn-1], in);
                              fprintf(GradOut,"%d %d %d\n",ni, nj, nk); 
                           }
                           ++nn;
                        }
                     }
                  }
               }
               ++nn;
            }else {
               gradovershish[nn-1] = 0;
               nn = ShishMax;
            }
         }
         /* STUFF BELOW IS USELESS */
         
         SUMA_NORM_VEC(targ, 3, gradient_promise);
         
         if (MinMax_over_dist[0] && cond1 && cond2 && cond3) {   
               a = &(SO->NodeList[3*in]);   
               /* reposition nodes */  
               if (0 && LocalHead) { fprintf(SUMA_STDERR, "%s: Suggest repair for node %d (better min above):\n"   
                                    " MinMax     =[%.1f,%.1f] MinMax_dist     = [%.2f,%.2f] \n"   
                                    " MinMax_over=[%.1f,%.1f] MinMax_over_dist= [%.2f,%.2f] \n"   
                                    " Val at node %.1f, mean below %.1f, mean above %.1f\n"  
                                    " zalt= %f, r/2 = %f\n",    
                       FuncName, in, 
                       MinMax[0], MinMax[1], MinMax_dist[0], MinMax_dist[1], 
                       MinMax_over[0], MinMax_over[1], MinMax_over_dist[0], MinMax_over_dist[1], 
                       Means[0], Means[1], Means[2],   
                       a[2] - SO->Center[2], Opt->r/2); } 
               {  /* Used to insist on value and top half if (MinMax_over_dist[0] && (a[2] - SO->Center[2] > 0))  */
                                             /* to avoid going into eye balls */
                                             /* Then added !SUMA_IS_EYE_ZONE(a,SO->Center) to avoid eyes */  
                                             /* but that is no longer necessary with fancier expansion conditions. */
                  if (gradient_promise < MinMax_over_dist[0]) {
                     touchup[in] = gradient_promise;
                  } else touchup[in] = MinMax_over_dist[0]; 
               }  
            ++N_troub;   
         }  
         /* nodes in the front of the brain that are sitting in the midst of a very bright area 
         should be moved further down until they hit the brain vs non brain area*/ 
      }  
      
      if (dvecind_under) SUMA_free(dvecind_under); dvecind_under= NULL;
      if (dvecind_over) SUMA_free(dvecind_over); dvecind_over= NULL;
      if (overshish) SUMA_free(overshish); overshish = NULL; 
      if (gradovershish) SUMA_free(gradovershish); gradovershish = NULL; 
      if (undershish) SUMA_free(undershish); undershish = NULL; 
   
   *N_touch = N_troub;
   if (GradOut) fclose(GradOut); GradOut = NULL;
   
   SUMA_RETURN(touchup);
}
   
/*!
   \brief A function to recommend node movement towards a better future.
   \sa SUMA_Reposition_Touchup
*/
float *SUMA_Suggest_Touchup(SUMA_SurfaceObject *SO, SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt, float limtouch, SUMA_COMM_STRUCT *cs, int *N_touch)
{
   static char FuncName[]={"SUMA_Suggest_Touchup"};
   int in, N_troub = 0, cond1=0, cond2=0, cond3 = 0, cond4 = 0, nn, ShishMax;   
   float MinMax_over[2], MinMax[2], MinMax_dist[2], MinMax_over_dist[2], Means[3], tb; 
   float U[3], Un, *a, P2[2][3], *norm, shft, *b; 
   float *touchup=NULL; 
   float *overshish=NULL, *undershish=NULL, *gradovershish=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
      if (Opt->debug > 2) LocalHead = YUP;
      *N_touch = 0;
      
      ShishMax = (int)(SUMA_MAX_PAIR(Opt->d1, Opt->d4) / Opt->travstp * 1.2);
      overshish = (float *)SUMA_malloc(sizeof(float)*ShishMax);
      undershish = (float *)SUMA_malloc(sizeof(float)*ShishMax);
      gradovershish = (float *)SUMA_malloc(sizeof(float)*ShishMax);
      touchup = (float *)SUMA_calloc(SO->N_Node, sizeof(float));   
      if (!touchup) { SUMA_SL_Crit("Failed to allocate"); SUMA_RETURN(NULL); }  
      for (in=0; in<SO->N_Node; ++in) {   
         SUMA_Find_IminImax(SO, Opt, in,  MinMax, MinMax_dist, MinMax_over, MinMax_over_dist, Means, undershish, overshish, NULL, NULL, ShishMax); 
         /* Shift the node outwards:   
            0- the minimum location of the minimum over the node is not 0
            AND
            1- if the minimum over the node is closer than the minimum below the node  or the internal minimum is more than 1mm away 
               and the internal min is masked to 0 or the minimum below the node just sucks, > tb and the minimum over the node is < tb
               The second part of the condition is meant to better model a finger of cortex that has enough csf on the inner surface 
               to have it be set to 0 by SpatNorm this dark csf would keep the brain surface from reaching the outer surface 
            AND   
            2- if the maximum over the node is much higher than the maximum below the node (fat on skull), 
               it must be beyond the location of the minimum over the node   
            AND   
            3- The new position of the node is not much larger than the current value
            AND
            4- The mean above the node is not more than 1.2 * the mean below the node.  
         */ 
         tb = (MinMax[1] - Opt->t2) * 0.5 + Opt->t2; 
         cond1 = (    (MinMax_over_dist[0] < MinMax_dist[0]) || (MinMax_dist[0] > 1 && MinMax[0] >= MinMax_over[0] ) 
                     || (MinMax[0] > tb && MinMax_over[0] < MinMax[0]) ); 
         /* statement below used to be: if (MinMax_over[1] > MinMax[1] && MinMax_over[1] > 0.9 * Opt->t98 && (MinMax_over_dist[1] < MinMax_over_dist[0]) ) cond2 = 0;  */
         if (MinMax_over[1] > 1.2 * MinMax[1] && (MinMax_over_dist[1] < MinMax_over_dist[0]) ) cond2 = 0;  
         else cond2 = 1; 
         if (MinMax_over[0] > 1.2 * Means[0]) cond3 = 0;
         else cond3 = 1;
         if (Means[2] > Means[1]) cond4 = 0;  /* March 21 */
         else cond4 = 1; 
         if (Opt->NodeDbg == in) {   
               a = &(SO->NodeList[3*in]);   
               fprintf(SUMA_STDERR, "%s: Debug during touchup for node %d:\n"   
                                    " MinMax     =[%.1f,%.1f] MinMax_dist     = [%.2f,%.2f] \n"   
                                    " MinMax_over=[%.1f,%.1f] MinMax_over_dist= [%.2f,%.2f] \n"   
                                    " Val at node %.1f, mean below %.1f, mean above %.1f\n"  
                                    " zalt= %f, r/2 = %f\n"    
                                    " Conditions: %f %d %d %d %d\n",    
                       FuncName, in, 
                       MinMax[0], MinMax[1], MinMax_dist[0], MinMax_dist[1], 
                       MinMax_over[0], MinMax_over[1], MinMax_over_dist[0], MinMax_over_dist[1], 
                       Means[0], Means[1], Means[2],   
                       a[2] - SO->Center[2], Opt->r/2, 
                       MinMax_over_dist[0] , cond1 , cond2, cond3, cond4); 
         }  
         /* what is happening with the gradients NOT USED YET*/
         for (nn=1; nn<ShishMax; ++nn) {
            if (overshish[nn] >= 0) {
               gradovershish[nn-1] = overshish[nn] - overshish[nn-1];
                  
            }else {
               gradovershish[nn-1] = 0;
            }
         }
         
         if (MinMax_over_dist[0] && cond1 && cond2 && cond3) {   
               a = &(SO->NodeList[3*in]);   
               if (cond4) {
                  /* reposition nodes */  
                  if (Opt->NodeDbg == in) { fprintf(SUMA_STDERR, "%s: Suggest repair for node %d (better min above):\n"   
                                       " MinMax     =[%.1f,%.1f] MinMax_dist     = [%.2f,%.2f] \n"   
                                       " MinMax_over=[%.1f,%.1f] MinMax_over_dist= [%.2f,%.2f] \n"   
                                       " Val at node %.1f, mean below %.1f, mean above %.1f\n"  
                                       " zalt= %f, r/2 = %f\n",    
                          FuncName, in, 
                          MinMax[0], MinMax[1], MinMax_dist[0], MinMax_dist[1], 
                          MinMax_over[0], MinMax_over[1], MinMax_over_dist[0], MinMax_over_dist[1], 
                          Means[0], Means[1], Means[2],   
                          a[2] - SO->Center[2], Opt->r/2); } 
                  {  /* Used to insist on value and top half if (MinMax_over_dist[0] && (a[2] - SO->Center[2] > 0))  */
                                                /* to avoid going into eye balls */
                                                /* Then added !SUMA_IS_EYE_ZONE(a,SO->Center) to avoid eyes */  
                                                /* but that is no longer necessary with fancier expansion conditions. */
                     touchup[in] = MinMax_over_dist[0]; 
                  }  
               ++N_troub; 
            } else { /* freeze that node, an attempt to control leak into lardo */
               Opt->Stop[in] = -1.0;
               if (Opt->NodeDbg == in) { fprintf(SUMA_STDERR, "%s: Freezing node %d\n", FuncName, in); }
            }  
         }  
         /* nodes in the front of the brain that are sitting in the midst of a very bright area 
         should be moved further down until they hit the brain vs non brain area*/ 
      }  
      
      
      if (overshish) SUMA_free(overshish); overshish = NULL; 
      if (gradovershish) SUMA_free(gradovershish); gradovershish = NULL; 
      if (undershish) SUMA_free(undershish); undershish = NULL; 
   
   *N_touch = N_troub;
   
   SUMA_RETURN(touchup);
}
/*!
   \brief A function to move node surfaces towards a better future.
   
   This function used to be a macro gone too large. 
*/
int SUMA_Reposition_Touchup(SUMA_SurfaceObject *SO, SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt, float limtouch, SUMA_COMM_STRUCT *cs) 
{
   static char FuncName[]={"SUMA_Reposition_Touchup"};
   byte *fmask=NULL;
   int in, N_troub = 0, cond1=0, cond2=0, cond3 = 0, cond4 = 0, nn;   
   float MinMax_over[2], MinMax[2], MinMax_dist[2], MinMax_over_dist[2], Means[3], tb; 
   float U[3], Un, *a, P2[2][3], *norm, shft, *b; 
   float *touchup=NULL, **wgt=NULL, *dsmooth=NULL; 
   int nstp, stillmoving, kth_buf, N_Touch;
   float stp, *csmooth=NULL, *shftvec=NULL, *targetloc=NULL;
   SUMA_Boolean Send_buf;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
      if (Opt->debug > 2) LocalHead = YUP;
      
      touchup = SUMA_Suggest_Touchup(SO, Opt, limtouch, cs, &N_troub);
      if (!touchup) {
         SUMA_SL_Err("Failed in SUMA_Suggest_Touchup");
         SUMA_RETURN(NOPE);
      }
      if (!N_troub) {
         SUMA_LH("Nothing to do, no trouble nodes.");
         SUMA_RETURN(YUP);
      }
      
      if (LocalHead) fprintf (SUMA_STDERR,"%s: ********************* %d troubled nodes found\n", FuncName, N_troub); 
      if (0){ /* March 22: Smoothing is not such a hot idea because it might cause certain nodes to depart into void and 
         then beyond, if you do a 2 pass touchup. Do not do it anymore */
          
         /* fix the big bumps: Bad for brain surface that has lots of bump on top, like 201, 
         that kind of smoothing is better performed on an inner model of the skull .... Someday ....*/
         if (LocalHead) fprintf (SUMA_STDERR,"%s: ********************* Now filtering...\n", FuncName); 
         wgt = SUMA_Chung_Smooth_Weights(SO);   
         if (!wgt) { 
            SUMA_SL_Err("Failed to compute weights.\n"); 
            exit(1); 
         }  
         dsmooth = SUMA_Chung_Smooth (SO, wgt, 200, 20, touchup, 1, SUMA_COLUMN_MAJOR, NULL, cs);   
         if (LocalHead) fprintf (SUMA_STDERR,"%s: ********************* filtering done.\n", FuncName);   
      } else { 
         dsmooth = touchup; /* carefull at the free business */
      }  
      /* add the changes */   
      #if 1 /* sudden jump, Fast and furious but that causes intersections */ 
      for (in=0; in<SO->N_Node; ++in) {   
         a = &(SO->NodeList[3*in]);   
         if (Opt->Stop[in] >= 0) {  
            if (1 || !SUMA_IS_EYE_ZONE(a,SO->Center)) { 
               if (a[2] - SO->Center[2] > 10 )  shft = touchup[in]; /* no smooth baby for top where bumpy sulci can occur */ 
               else shft = dsmooth[in];   
               if (shft) { 
                  a = &(SO->NodeList[3*in]);   
                  norm = &(SO->NodeNormList[3*in]);  
                  SUMA_POINT_AT_DISTANCE(norm, a, SUMA_MIN_PAIR(shft, limtouch), P2);   
                  SO->NodeList[3*in] = P2[0][0]; SO->NodeList[3*in+1] = P2[0][1]; SO->NodeList[3*in+2] = P2[0][2];   
                  if (0 && LocalHead) fprintf(SUMA_STDERR,"%s: Acting on node %d because it is in top half, boosting by %f\n", 
                           FuncName, in, SUMA_MIN_PAIR(shft, limtouch));   
               }
            }
         } else {
            if (in == Opt->NodeDbg) fprintf(SUMA_STDERR,"%s:\nNode %d has been frozen before, no cigar.\n", FuncName, in);
         }
      }  
      #else /* smooth operator, slow as hell, does not reach far enough */
      /* first figure out where each node is to go: */
      shftvec = (float *)SUMA_calloc( SO->N_Node, sizeof(float));
      targetloc = (float *)SUMA_malloc(3 * SO->N_Node * sizeof(float));
      for (in=0; in<SO->N_Node; ++in) {  
         shftvec[in] = 0.0;
         a = &(SO->NodeList[3*in]);   
         if (1 || !SUMA_IS_EYE_ZONE(a,SO->Center)) { 
            if (a[2] - SO->Center[2] > 10 )  shft = touchup[in]; /* no smooth baby for top where bumpy sulci can occur */ 
            else shft = dsmooth[in];   
            shft = SUMA_MIN_PAIR(shft, limtouch);
            if (shft) { 
               shftvec[in] = shft;
               norm = &(SO->NodeNormList[3*in]); 
               targetloc[3*in  ] = shft*norm[0]+a[0];
               targetloc[3*in+1] = shft*norm[1]+a[1];
               targetloc[3*in+2] = shft*norm[2]+a[2]; 
            }
         }
      }
      /* now move slowly towards target, only move those nodes that are to be shifted */
      fmask = (byte *)SUMA_calloc(SO->N_Node , sizeof(byte));
      stp = 1; /* incremental step in mm */
      nstp = 0;
      do {
         stillmoving = 0;
         for (in=0; in<SO->N_Node; ++in) {  
            fmask[in] = 0;
            a = &(SO->NodeList[3*in]); 
            b = &(targetloc[3*in]);
            if (shftvec[in]) {
               SUMA_UNIT_VEC( a, b, U, Un);
               if (Un < stp)  {
                  shft = Un;
                  shftvec[in] = 0.0; /* no more shifts */
               } else {
                  shft = stp;
                  stillmoving = 1;
               }
               /* fprintf(SUMA_STDERR,"%s:\n node %d shft=%f\nU=[%f %f %f]\n", FuncName, in, shft, U[0], U[1], U[2]); */
               if (shft) { 
                  SUMA_POINT_AT_DISTANCE(U, a, shft, P2);   
                  SO->NodeList[3*in] = P2[0][0]; SO->NodeList[3*in+1] = P2[0][1]; SO->NodeList[3*in+2] = P2[0][2];
                  fmask[in] = 1; /* filter the one that moves */
               }         
            }
         }
            Send_buf = cs->Send;
            cs->Send = NOPE;
            kth_buf = cs->kth;
            cs->kth = 1;  /*make sure all gets sent at this stage */
         if (1) {/* filter the surface */
            fprintf (SUMA_STDERR,"%s: Touchup smoothing.\n", FuncName);
            csmooth = SUMA_Taubin_Smooth( SO, NULL,
                                          0.6307, -.6732, SO->NodeList,
                                          20, 3, SUMA_ROW_MAJOR, csmooth, cs, fmask);    
            memcpy((void*)SO->NodeList, (void *)csmooth, SO->N_Node * 3 * sizeof(float));
         }      
            cs->Send = Send_buf;
            SUMA_RECOMPUTE_NORMALS(SO);
            if (cs->Send) {
               if (!SUMA_SendToSuma (SO, cs, (void *)SO->NodeList, SUMA_NODE_XYZ, 1)) {
                  SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
               }
            }
            cs->kth = kth_buf;
         if (LocalHead) fprintf (SUMA_STDERR,"%s: Touchup pass %d complete.\n", FuncName, nstp);  
         ++nstp;
      } while (stillmoving && nstp < 60);
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Stopped growth stillmoving = %d, nstp = %d\n", FuncName, stillmoving, nstp);
      #endif 
      if (fmask) SUMA_free(fmask); fmask = NULL; 
      if (shftvec) SUMA_free(shftvec); shftvec = NULL;
      if (targetloc) SUMA_free(targetloc); targetloc = NULL;
      if (touchup == dsmooth) dsmooth = NULL;
      if (touchup) SUMA_free(touchup); touchup = NULL;   
      if (dsmooth) SUMA_free(dsmooth); dsmooth = NULL;   
      if (csmooth) SUMA_free(csmooth); csmooth = NULL;   
      if (wgt) SUMA_free2D ((char **)wgt, SO->N_Node); wgt = NULL;   
   
   SUMA_RETURN(YUP);
}
