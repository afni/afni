#include "SUMA_suma.h"
#include "../thd_brainormalize.h"
#include "extrema.h"

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
   float PercRange[2], PercRangeVal[2], *fvec_sort=NULL, *fvec=NULL, cog[3];
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
   
   Opt->VolCM[0] = THD_BN_xcm(); Opt->VolCM[1] = THD_BN_ycm(); Opt->VolCM[2] = THD_BN_zcm();
   
   Opt->fvec = (float *)SUMA_malloc(sizeof(float) * Opt->nvox);
   if (!Opt->fvec) {
      SUMA_SL_Crit("Faile to allocate for fvec.\nOh misery.");
      SUMA_RETURN(NOPE);
   }
   EDIT_coerce_scale_type( Opt->nvox , DSET_BRICK_FACTOR(Opt->in_vol,0) ,
                           DSET_BRICK_TYPE(Opt->in_vol,0), DSET_ARRAY(Opt->in_vol, 0) ,      /* input  */
                           MRI_float               , Opt->fvec  ) ;   /* output */
   
   /* estimate the t2, t98 and tm parameters, should do them regionally by Bob's octant method */
   if (Opt->t2 < 0) {
      PercRange[0] = 2; PercRange[1] = 98;
      fvec_sort = SUMA_PercRange (Opt->fvec, NULL, Opt->nvox, PercRange, PercRangeVal, iPercRangeVal);
      if (!fvec_sort) {
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
      if (LocalHead) 
         fprintf (SUMA_STDERR,
            "%s: Wild shot tm = %f\n", 
            FuncName, 
            fvec_sort[(iPercRangeVal[0]+iPercRangeVal[1])/2]);
      if (fvec_sort) SUMA_free(fvec_sort); fvec_sort = NULL;
   } else {
      /* user specified */
   }

   /* estimate the volume */
   CoordList = (float *)SUMA_calloc(3 * Opt->nvox, sizeof(float) );
   if (!CoordList) {
      SUMA_SL_Err("Failed to allocate CoordList");
      SUMA_RETURN(vol);
   }
   cog[0] = cog[1] = cog[2] = 0;
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
      if (Opt->fvec[i] > Opt->t) {
         if (!Opt->SurfaceCoil) {
            mass = SUMA_MIN_PAIR(Opt->t98, Opt->fvec[i]);
         } else mass = 1.0;
         cog[0] += mass * ndicom.xyz[0];
         cog[1] += mass * ndicom.xyz[1];
         cog[2] += mass * ndicom.xyz[2];
         volmass += mass;
         ++vol;
      } 
   }
   cog[0] = cog[0]/volmass; 
   cog[1] = cog[1]/volmass; 
   cog[2] = cog[2]/volmass; 
   
   if (Opt->cog[0]< -8999.0f || Opt->cog[1]< -8999.0f || Opt->cog[2]< -8999.0f) {
      Opt->cog[0] = cog[0];
      Opt->cog[1] = cog[1];
      Opt->cog[2] = cog[2];
      if (LocalHead) {
         fprintf (SUMA_STDERR,
            "%s: COG %f %f %f\n",
            FuncName, Opt->cog[0], Opt->cog[1], Opt->cog[2]); 
      }
   } else {
      if (LocalHead) {
         fprintf (SUMA_STDERR,
            "%s: COG preset to %f %f %f\n",
            FuncName, Opt->cog[0], Opt->cog[1], Opt->cog[2]); 
      }
   }
   vol *= fabs(DSET_DX(Opt->in_vol) * 
               DSET_DY(Opt->in_vol) * 
               DSET_DZ(Opt->in_vol) );
   
   /* find the radius */
   if (Opt->r < 0.0f) {
      Opt->r = pow(vol*3.0/(3.14159*4.0), 1/3.0);
      if (Opt->debug) {
            fprintf (SUMA_STDERR,
               "%s: Volume %f, radius %f\n", FuncName, vol, Opt->r);
      }
      if (Opt->specie == MONKEY) {
         Opt->r  = Opt->r/THD_BN_rat(); /* used to reduce radius by 
                                       because of large amount of
                                       muscle around brain to Dec. 08 */
         if (Opt->debug) {
            fprintf (SUMA_STDERR,
               "%s: Radius kept at %f, less brain, more muscle.\n",
               FuncName, Opt->r);
         }
      } else if (Opt->specie == MARMOSET) {
         Opt->r  = Opt->r/THD_BN_rat(); /* used to reduce radius by 
                                       because of large amount of
                                       muscle around brain to Dec. 08 */
         if (Opt->debug) {
            fprintf (SUMA_STDERR,
               "%s: Radius kept at %f, less brain, more muscle.\n",
               FuncName, Opt->r);
         }
      } else if (Opt->specie == RAT) {
         Opt->r  = SUMA_MAX_PAIR(Opt->r/THD_BN_rat(), 6.0);       
         if (Opt->debug) {
            fprintf (SUMA_STDERR,
               "%s: Radius at %f. RATS!.\n",  FuncName, Opt->r);
         }
      } else if (Opt->specie == HUMAN) {
         if (Opt->r > 100) {
            SUMA_S_Notev("Radius estimated at %f is large.\n"
                         "Setting back to 100.0\n", Opt->r);
            Opt->r = 100;
         }
      }
   } else {
      if (Opt->debug) {
            fprintf (SUMA_STDERR,
               "%s: User set radius at %f.\n",  FuncName, Opt->r);
      }
   }
   
   /* form a vector of values inside the sphere */
   if (Opt->tm < 0) {
      /* create a mask of the voxels inside the sphere of radius r */
      IsIn =  SUMA_isinsphere (CoordList, Opt->nvox, Opt->cog , Opt->r , 1 );
      fvec = (float *)SUMA_malloc(Opt->nvox * sizeof(float));
      if (!fvec) { 
         SUMA_SL_Crit("Failed to allocate for fvec"); 
         SUMA_RETURN(-1); 
      }
      cnt = 0;
      for (i=0; i<IsIn.nIsIn; ++i) { 
         fvec[cnt] = Opt->fvec[IsIn.IsIn[i]]; ++cnt;
      }
      /* now sort fvec */
      isort = SUMA_z_qsort(fvec, cnt);
      Opt->tm = fvec[cnt/2];
      if (LocalHead) {
         fprintf (SUMA_STDERR,
            "%s: Real tm %f\nt2 = %f, t = %f, t98 = %f\n", 
            FuncName, Opt->tm, Opt->t2, Opt->t, Opt->t98);
      }
      if (fvec) SUMA_free(fvec); fvec = NULL;
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
         if (Opt->fvec[i] >= Opt->t98) {
            Opt->fvec[i] = 0;
            if (cnt == max_mask) {
               SUMA_SL_Warn("Too many high values in dset!");
               max_mask *= 2;
               Opt->k98mask = (int *)SUMA_realloc( Opt->k98mask, 
                                                   max_mask * sizeof(int));
               if (!Opt->k98mask) { 
                  SUMA_SL_Crit("Failed to allocate"); 
                  SUMA_RETURN(-1); 
               }
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
      isskin = SUMA_isSkin(Opt->in_vol, Opt->fvec, Opt->t2, &N_skin);
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
            fprintf( SUMA_STDERR,
                     "Error %s:\nFailed in SUMA_qhull_wrap\n", FuncName);
            *SOhull = NULL;
         } else {
            fprintf(SUMA_STDERR,"%s:\nForming hull.\n", FuncName);
            *SOhull = SUMA_Patch2Surf(CoordList, cnt, ijk, nf, 3);
            SOhullp = *SOhull;
            #if 0
            if (LocalHead) 
               fprintf( SUMA_STDERR,
                        "%s: Making hull consistently orientated\n", FuncName);
            SOhullp->EL = 
               SUMA_Make_Edge_List_eng (SOhullp->FaceSetList, SOhullp->N_FaceSet,                                         SOhullp->N_Node, SOhullp->NodeList, 
                                        1, NULL);
            if (!SUMA_MakeConsistent ( SOhullp->FaceSetList, SOhullp->N_FaceSet, 
                                       SOhullp->EL, 0, &trouble)) {
               SUMA_SL_Err("Failed in SUMA_MakeConsistent");
            }
            if (LocalHead) 
               fprintf(SUMA_STDERR,"%s: Checking orientation.\n", FuncName);
            SUMA_SL_Warn("Stuff shaky here, Attend to it.");
            Force = 1; 
            orient = SUMA_OrientTriangles (  SOhullp->NodeList, SOhullp->N_Node, 
                                             SOhullp->FaceSetList, 
                                             SOhullp->N_FaceSet, 1, Force);
            if (orient < 0 || Force) { 
               /* flipping was done, dump the edge list */ 
               if (SOhullp->EL) SUMA_free_Edge_List(SOhullp->EL); 
               SOhullp->EL = NULL; 
            }
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
int SUMA_Find_IminImax (float *xyz, float *dir, SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt, 
                        int ni, 
                        float *MinMax, float *MinMax_dist , 
                        float *MinMax_over, float *MinMax_over_dist,
                        float *Means, 
                        float *undershish, float *overshish, 
                        int *fvecind_under, int *fvecind_over,
                        float d1, float d4, int ShishMax)
{
   static char FuncName[]={"SUMA_Find_IminImax"};
   float travdir[3], d1t, d2t, lmin, lmax, lmin_dist, lmax_dist;
   float d2, t2 = Opt->t2, tm = Opt->tm, t = Opt->t; 
   THD_fvec3 ncoord, ndicom;
   THD_ivec3 nind3;
   int nind, istep, istepmax, istep2max, nxx, nxy, nMeans[3], stopint, out;
   SUMA_ENTRY;
   
   d2 = d1/2.0;
   
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
   out = 0;
   while (istep <= istepmax && !out) {
      /* calculate offset */
      travdir[0] = - istep * Opt->travstp * dir[0]; travdir[1] = -istep * Opt->travstp * dir[1]; travdir[2] = -istep * Opt->travstp * dir[2]; 
      
      /* get 1d coord of point */
      ndicom.xyz[0] = xyz[0] + travdir[0] ; ndicom.xyz[1] = xyz[1]+ travdir[1]; ndicom.xyz[2] = xyz[2]+ travdir[2]; 
      ncoord = THD_dicomm_to_3dmm(Opt->in_vol, ndicom);
      nind3 = THD_3dmm_to_3dind_warn(Opt->in_vol, ncoord, &out);
      nind = nind3.ijk[0] + nind3.ijk[1] * nxx + nind3.ijk[2] * nxy;
      
      #if 0
      if (ni == Opt->NodeDbg) {
         fprintf(SUMA_STDERR, "%s: Node %d\n"
                              " nind3 = [%d %d %d] voxVal = %.3f\n", 
                              FuncName, Opt->NodeDbg, nind3.ijk[0], nind3.ijk[1], nind3.ijk[2],
                              Opt->fvec[nind]);
      }
      #endif
      if (undershish && istep < ShishMax) undershish[istep] = Opt->fvec[nind];   /* store values under node */
      
      /* track the brain mean, only if the value is above brain non-brain threshold
      stop inegrating as soon as you hit nonbrain */
      if (Opt->fvec[nind] > Opt->t && !stopint) { Means[1] += Opt->fvec[nind]; ++ nMeans[1]; }
      else stopint = 1;
      if (fvecind_under && istep < ShishMax) fvecind_under[istep] = nind;
        
      /* find local min */ 
      /* lmin = SUMA_MIN_PAIR(lmin, Opt->fvec[nind]); */
      if (lmin > Opt->fvec[nind]) { lmin = Opt->fvec[nind]; SUMA_NORM_VEC(travdir, 3, lmin_dist); }
      
      if (istep <= istep2max) {
         if (lmax < Opt->fvec[nind]) { lmax = Opt->fvec[nind]; SUMA_NORM_VEC(travdir, 3, lmax_dist); }  
      }
      
      if (!out) ++istep;
   }
   
   if (istep < ShishMax) { undershish[istep] = -1; if (fvecind_under) fvecind_under[istep] = -1; }/* crown the shish */
   
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
      out = 0;
      while (istep <= istepmax && !out) {
         /* calculate offset */
         travdir[0] =  istep * Opt->travstp * dir[0]; travdir[1] = istep * Opt->travstp * dir[1]; travdir[2] = istep * Opt->travstp * dir[2]; 

         /* get 1d coord of point */
         ndicom.xyz[0] = xyz[0] + travdir[0] ; ndicom.xyz[1] = xyz[1]+ travdir[1]; ndicom.xyz[2] = xyz[2]+ travdir[2]; 
         ncoord = THD_dicomm_to_3dmm(Opt->in_vol, ndicom);
         nind3 = THD_3dmm_to_3dind_warn(Opt->in_vol, ncoord, &out);
         nind = nind3.ijk[0] + nind3.ijk[1] * nxx + nind3.ijk[2] * nxy;
         #if 0
         if (ni == Opt->NodeDbg) {
            fprintf(SUMA_STDERR, "%s: Node %d\n"
                                 " nind3 = [%d %d %d] voxVal = %.3f\n", 
                                 FuncName, Opt->NodeDbg, nind3.ijk[0], nind3.ijk[1], nind3.ijk[2],
                                 Opt->fvec[nind]);
         }
         #endif
         if (!istep) { /* get the value at the node */
            Means[0] = Opt->fvec[nind];
         }
         
        /* store values under node */
         if (overshish && istep < ShishMax) overshish[istep] = Opt->fvec[nind]; 
         
         if (Opt->fvec[nind] > Opt->t && !stopint) { 
            Means[2] += Opt->fvec[nind]; ++ nMeans[2]; } 
         else stopint = 1;
         if (fvecind_over && istep < ShishMax) fvecind_over[istep] = nind;
         
         /* find local min and max*/ 
         if (lmin > Opt->fvec[nind]) { 
            lmin = Opt->fvec[nind]; SUMA_NORM_VEC(travdir, 3, lmin_dist); }
         if (lmax < Opt->fvec[nind]) { 
            lmax = Opt->fvec[nind]; SUMA_NORM_VEC(travdir, 3, lmax_dist); }  
         
         if (!out) ++istep;
      }
      
      if (istep < ShishMax) { overshish[istep] = -1; if (fvecind_over) fvecind_over[istep] = -1; }/* crown the shish */

      Means[2] /= (float)nMeans[2];
      MinMax_over[0] = lmin; 
      MinMax_over_dist[0] = lmin_dist;  
      MinMax_over[1] = lmax;
      MinMax_over_dist[1] = lmax_dist;
   }
   
   SUMA_RETURN(YUP);
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
int SUMA_Find_IminImax_Avg (SUMA_SurfaceObject *SO, SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt, 
                        int ni, 
                        float *MinMax, float *MinMax_dist , 
                        float *MinMax_over, float *MinMax_over_dist,
                        float *Means, 
                        float *undershish, float *overshish, 
                        int *fvecind_under, int *fvecind_over, int ShishMax)
{
   static char FuncName[]={"SUMA_Find_IminImax_Avg"};
   float d1, d2, travdir[3], d1t, d2t, lmin, lmax, lmin_dist, lmax_dist;
   float t2 = Opt->t2, tm = Opt->tm, t = Opt->t; 
   THD_fvec3 ncoord, ndicom;
   THD_ivec3 nind3;
   int nind, istep, istepmax, istep2max, nxx, nxy, nMeans[3], stopint;
   int N_vtnmax = 500, N_vtn, vtn[N_vtnmax]; /* vector of triangles incident triangles to node ni */
   
   SUMA_ENTRY;
   
   d1 = Opt->d1; d2 = d1/2.0; 
   
   vtn[N_vtnmax-1] = -1; /* flag to check for overruns */
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
      
      /* find the set of triangles incident to node ni */
      N_vtn = N_vtnmax; /* pass limit to SUMA_Get_NodeIncident */
      if (!SUMA_Get_NodeIncident(ni, SO, vtn, &N_vtn)) {
          SUMA_SL_Err("Failed to find incident triangles.\nDecidement, ca va tres mal.\n");
          SUMA_RETURN(NOPE);
      }
      if (vtn[N_vtnmax-1] != -1) {
         SUMA_SL_Err("Way too many incident triangles. Memory corruption likely.");
         SUMA_RETURN(NOPE);
      }
      /* for each triangle, find the voxels that it intersects */
      
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
                              Opt->fvec[nind]);
      }
      #endif
      if (undershish && istep < ShishMax) undershish[istep] = Opt->fvec[nind];   /* store values under node */
      
      /* track the brain mean, only if the value is above brain non-brain threshold
      stop inegrating as soon as you hit nonbrain */
      if (Opt->fvec[nind] > Opt->t && !stopint) { Means[1] += Opt->fvec[nind]; ++ nMeans[1]; }
      else stopint = 1;
      if (fvecind_under && istep < ShishMax) fvecind_under[istep] = nind;
        
      /* find local min */ 
      /* lmin = SUMA_MIN_PAIR(lmin, Opt->fvec[nind]); */
      if (lmin > Opt->fvec[nind]) { lmin = Opt->fvec[nind]; SUMA_NORM_VEC(travdir, 3, lmin_dist); }
      
      if (istep <= istep2max) {
         if (lmax < Opt->fvec[nind]) { lmax = Opt->fvec[nind]; SUMA_NORM_VEC(travdir, 3, lmax_dist); }  
      }
      
      ++istep;
   }
   
   if (istep < ShishMax) { undershish[istep] = -1; if (fvecind_under) fvecind_under[istep] = -1; }/* crown the shish */
   
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
                                 Opt->fvec[nind]);
         }
         #endif
         if (!istep) { /* get the value at the node */
            Means[0] = Opt->fvec[nind];
         }
         
         if (overshish && istep < ShishMax) overshish[istep] = Opt->fvec[nind];   /* store values under node */
         
         if (Opt->fvec[nind] > Opt->t && !stopint) { Means[2] += Opt->fvec[nind]; ++ nMeans[2]; } 
         else stopint = 1;
         if (fvecind_over) fvecind_over[istep] = nind;
         
         /* find local min and max*/ 
         if (lmin > Opt->fvec[nind]) { lmin = Opt->fvec[nind]; SUMA_NORM_VEC(travdir, 3, lmin_dist); }
         if (lmax < Opt->fvec[nind]) { lmax = Opt->fvec[nind]; SUMA_NORM_VEC(travdir, 3, lmax_dist); }  
         
         ++istep;
      }
      
      if (istep < ShishMax) { overshish[istep] = -1; if (fvecind_over) fvecind_over[istep] = -1; }/* crown the shish */

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
   float U[3]={0.0, 0.0, 0.0}, Un;
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
               SUMA_ONE_SHISH_PLEASE(n, U, Opt->in_vol, Opt->fvec, Opt->travstp, n_stp, nx, nxy, undershish, ShishMax);
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
                                    8, 3, SUMA_ROW_MAJOR, dsmooth, cs, NULL, 0);    
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
   int it=0,it0 = 0, in=0, ii, Npass, Done, ShishMax, i_diffmax, kth_buf, nit, Stage, *fvecind_over, Stage2Type;
   float mnc[3], s[3], sn[3], st[3], dp, threshclip, lztfac,
         nsn, rmin, rmax, E, F, su1, su2, su3, su4,  
         r, u1[3], u2[3], u3[3], u[3],
         tm=0.0, t2 = 0.0, t98 = 0.0, Imin, Imax, l=0.0, MinMax[2], 
         MinMax_dist[2],MinMax_over[2], MinMax_over_dist[2], Means[3],
         *n=NULL, tb = 0.0, tbe = 0.0, t = 0.0, Imin_over=0.0, Imin_dist_over=0.0,
         *overshish = NULL, *undershish = NULL, diffmax, *touchup=NULL, *a, P2[2][3], *norm;
   float *tmpptr, *NewCoord = NULL, f3, f4, lZt, *dsmooth = NULL;
   float *refNodeList = NULL, *OrigNodeList=NULL, MaxNormalExp=-1.0;
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
   fvecind_over = (int *)SUMA_malloc(sizeof(int)*ShishMax);
   if (!OrigNodeList || !overshish || !undershish || !fvecind_over) {
      SUMA_SL_Crit("Failed to allocate");
      SUMA_RETURN(NOPE);
   }
   memcpy((void*)OrigNodeList, (void *)SO->NodeList, SO->N_Node * 3 * sizeof(float));
   
   Stage2Type = 1; 
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
            MaxNormalExp = -1.0;
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

               SUMA_Find_IminImax(&(SO->NodeList[3*in]), &(SO->NodeNormList[3*in]), Opt, in, MinMax, MinMax_dist, MinMax_over, MinMax_over_dist, Means, 
                                       undershish, overshish, NULL, fvecind_over,  Opt->d1, Opt->d4, ShishMax);  
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
               if (Opt->NoEyes && !Opt->emask) { /* The no edge way */
                  #if 0
                     if (it > 0.1 * Opt->N_it) { /* The olde, stupid way */
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
                                 if (LocalHead) fprintf (SUMA_STDERR, "%s: Stopping node %d, at values > %f\n", FuncName, in, Opt->Stop[in]);
                              } else if (Means[0] >= Opt->t98) {
                                 Opt->Stop[in] = Opt->t98;
                                 if (LocalHead) fprintf (SUMA_STDERR, "%s: Stopping node %d, at values > %f\n", FuncName, in, Opt->Stop[in]);
                              }
                           }
                        }
                     }
                     if (Opt->Stop[in]) {
                        if (Means[0] >= Opt->Stop[in]) { su3 = 0; su4 = 0; }
                     }
                  #else 
                     if (it > 0.1 * Opt->N_it) { /* The newer, better way */
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
                                 MaxNormalExp = Opt->travstp*i_diffmax;
                                 if (LocalHead) fprintf (SUMA_STDERR, "%s: Stopping node %d, from expanding beyond %fmm \n", FuncName, in, MaxNormalExp );
                              } else if (Means[0] >= Opt->t98) {
                                 MaxNormalExp = 0.0; /* ????? */
                                 if (LocalHead) fprintf (SUMA_STDERR, "%s: Dunno what to do here : Stopping node %d, from expanding beyond %fmm\n", 
                                       FuncName, in, MaxNormalExp );
                              }
                           }
                        }
                     }
                     
                  #endif
               }
                
               if (Opt->NoEyes && Opt->emask) {
                  /* Another attempt at controlling the eyes */
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
                              /* do we have edges ahead? */
                              /* don't move if a node is too close to an edge */
                              int ie = 0;
                              MaxNormalExp = -1.0;
                              while (MaxNormalExp < 0 && fvecind_over[ie] >= 0 && Opt->travstp*ie < 2.0) {
                                 if (Opt->emask[fvecind_over[ie]]) {
                                    if (Opt->NodeDbg == in) { 
                                       fprintf(SUMA_STDERR, "%s: Eye Node %d will not be allowed to grow, too close to an edge!\n", FuncName, in);
                                    }
                                    MaxNormalExp = Opt->travstp*ie;    
                                    if (LocalHead) fprintf (SUMA_STDERR, "%s: Edge-Stopping node %d, from expanding beyond %fmm \n", FuncName, in, MaxNormalExp );
                                 }
                                 ++ie;
                              }
                           } else if (Means[0] >= Opt->t98) {
                              MaxNormalExp = 0.0; /* ????? */
                              if (LocalHead) fprintf (SUMA_STDERR, "%s: Edge-Dunno what to do here : Stopping node %d, from expanding beyond %fmm\n", 
                                       FuncName, in, MaxNormalExp );
                           }
                        }
                     }
                  }
               }
               /* freezing: a node that has been frozen, freezing may be used during final touchup*/
               if (Opt->Stop[in] < 0) { 
                  su3 = 0; su4 = 0;   
                  if (in == Opt->NodeDbg) fprintf(SUMA_STDERR,"%s:\nNode %d has been frozen\n", FuncName, in);
               }
               
               /* slow down growth if it is to go beyond Maximal Expansion along the normal */
               if (MaxNormalExp >= 0.0) {
                  if (su3+su4 > MaxNormalExp) {
                     if (LocalHead) fprintf (SUMA_STDERR, "%s: Expansion reduced from %fmm to %fmm\n", FuncName, su3+su4, MaxNormalExp);
                     su3 = MaxNormalExp; su4 = 0.0;  
                  }
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
               NewCoord[3*in] = n[0] + u[0]; 
               NewCoord[3*in+1] = n[1] + u[1]; 
               NewCoord[3*in+2] = n[2] + u[2];       
               MaxExp = SUMA_MAX_PAIR (
                  MaxExp, ( sqrt(u[0]*u[0]+u[1]*u[1]+u[2]*u[2]) ) );
               DoDbg = NOPE;
            
         } /* loop over number of nodes */
         /* swap vectors */
            tmpptr = SO->NodeList;
            SO->NodeList = NewCoord; 
            NewCoord = tmpptr;
            tmpptr = NULL;

         if (Opt->NNsmooth) {
            byte *msk=NULL; /* use variable to avoid macro warning */
            if (!(it % Opt->smootheach) && it > 0 && it < 0.75*Opt->N_it) {  
               /* cannot filter too close to convergence */
               if (!Opt->match_area) {
                  SUMA_WRAP_BRAIN_SMOOTH_NN( 
                     Opt->NNsmooth, dsmooth, refNodeList, msk);
               } else {SUMA_WRAP_BRAIN_SMOOTH_MATCHAREA( 
                     Opt->NNsmooth, dsmooth, refNodeList, msk);
               }
            }
         }

         /* recalculate surface normals */
         SUMA_RECOMPUTE_NORMALS(SO); 
         if (cs->Send) {
            if (!SUMA_SendToSuma (SO, cs, 
                                 (void *)SO->NodeList, SUMA_NODE_XYZ, 1)) {
               SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
            }
         }

      } /* loop over number of iterations */
      ++Stage;
      if (Opt->DemoPause == SUMA_3dSS_INTERACTIVE) {
         fprintf (SUMA_STDERR,
            "3dSkullStrip Interactive: \n"
            "Increasing number of iterations to reach minimal expansion.\n"
            "Do you want to (C)ontinue, (P)ass or (S)ave this? ");
         cbuf = SUMA_ReadCharStdin ('c', 0,"csp");
         fprintf (SUMA_STDERR,"%c\n", cbuf);
         switch (cbuf) {
            case 's':   
               fprintf (SUMA_STDERR,
                  "Will check surface, save mask and exit.\n");
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
         if (Opt->debug) fprintf (SUMA_STDERR,"%s: \n In stage 1\n", FuncName);
         if (MaxExp > 0.5) {
            /* Now, if you still have expansion, continue */
            if (!pastarea) { /* first time around, calculate area */
               pastarea = SUMA_Mesh_Area(SO, NULL, -1);
               if (Opt->debug) fprintf (SUMA_STDERR,"%s: \n Stage1: pastarea = %f\n", FuncName, pastarea);
               keepgoing = 1;
            }else {
               curarea = SUMA_Mesh_Area(SO, NULL, -1);
               darea = ( curarea - pastarea ) / pastarea; 
               if (SUMA_ABS(darea) > 0.05) {
                  keepgoing = 1;
               } else { 
                  keepgoing = 0;
               }
               pastarea = curarea;
            }
            if (keepgoing) {
               it0 = nit; nit = nit + (int)(Opt->N_it/2.5);
               if (Opt->debug) fprintf (SUMA_STDERR,"%s: \n Stage1: MaxExp = %f, darea = %f, going for more...\n", FuncName, MaxExp, darea);
               Done = 0;
            } else {
               if (Opt->debug) fprintf (SUMA_STDERR,"%s: \n Stage1: satiated, small area differential.\n", FuncName);
               ++Stage;
            }
         } else {
            if (Opt->debug) fprintf (SUMA_STDERR,"%s: \n Stage1: satiated, low MaxExp\n", FuncName);
            ++Stage;
         }
      }
      if (Stage == 2) {
         if (Stage2Type == 1) { 
            if (Opt->DemoPause == SUMA_3dSS_DEMO_PAUSE) { SUMA_PAUSE_PROMPT("About to be in stage 2, type 1"); }
            /* see if there is room for extension */
            touchup = SUMA_Suggest_Touchup(SO, Opt, 4.0, cs, &N_troub);
            if (!touchup) SUMA_SL_Warn("Failed in SUMA_Suggest_Touchup");
            if (!N_troub) { 
               /* No touchup, done */
               ++Stage;
            } else {
                  /* reduce tightness where expansion is needed */
                  if (Opt->debug) {
                     fprintf(SUMA_STDERR,"%s:\n reducing tightness, applying touchup with Stage2Type = %d\n", FuncName, Stage2Type);
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
            }

            if (touchup) SUMA_free(touchup); touchup = NULL;
         
         } else if (Stage2Type == 2) {
            if (Opt->DemoPause == SUMA_3dSS_DEMO_PAUSE) { SUMA_PAUSE_PROMPT("About to be in stage 2, type 2"); }
            if (Opt->PushToEdge) {
               /* Make a bloody jump */
               N_troub = SUMA_PushToEdge(SO, Opt, 4, cs, 1); SUMA_RECOMPUTE_NORMALS(SO);
            } else {
               N_troub = 0;
            }
            if (!N_troub) { 
               Done = 0;
               /* No touchup, done */
               ++Stage;
            }
         } else {
            SUMA_S_Err("Logic Error!!!");
         }
 
         it0 = nit; nit = nit + (int)(Opt->N_it/2.5);
         Done = 0;

         if (!past_N_troub) { 
            past_N_troub = N_troub; 
            if (Opt->debug) fprintf (SUMA_STDERR,"%s: \n Stage %d, type %d: %d troubled nodes, going for more...\n", FuncName, Stage, Stage2Type, N_troub);
         } else {
            float dtroub;
            dtroub = (float)(past_N_troub - N_troub)/(float)past_N_troub;
            if (Opt->debug) fprintf (SUMA_STDERR,"%s: \n Stage %d, type %d: %f change in troubled nodes.\n", FuncName, Stage, Stage2Type, dtroub);
            if (dtroub > 0.01) {
               if (Opt->debug) fprintf (SUMA_STDERR,"%s: \n Continuing with Stage.\n", FuncName);
               Done = 0; /* continue */
            } else {
               if (Opt->debug) fprintf (SUMA_STDERR,"%s: \n Stage converged. Moving to new Stage or Type.\n", FuncName);
               ++Stage; /* go to next stage */
            }
            past_N_troub = N_troub;
         }
         
      }
      if (0 && Stage == 3) {
         if (Opt->emask) {
            SUMA_LH("Pushing to edge");
            if (Opt->DemoPause == SUMA_3dSS_DEMO_PAUSE) { SUMA_PAUSE_PROMPT("About to be in stage 3"); }
            /* see if there is room for extension */
            touchup = SUMA_Suggest_Touchup_PushEdge(SO, Opt, 4.0, cs, &N_troub);
            if (!touchup) SUMA_SL_Warn("Failed in SUMA_Suggest_Touchup_PushEdge");
            if (!N_troub) { 
               /* No touchup, done */
               Done = 1;
            } else {
               /* reduce tightness where expansion is needed */
               if (LocalHead) {
                  fprintf(SUMA_STDERR,"%s:\n Edging, reducing tightness, applying touchup\n", FuncName);
               }
               for (in=0; in<SO->N_Node; ++in) {
                  if (touchup[in]) {
                     if (Opt->NodeDbg == in) fprintf(SUMA_STDERR,"%s: Edge Acting on node %d, touchup[in] %f, past shrink_fac = %f\n", 
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
         } else {
            SUMA_LH("No edge mask, stage useless");
         }
      } 
      
      if (Stage > 2) {
         if (Stage2Type < 2) {
            SUMA_LH("Going for push-edge mode stage");
            Stage2Type = 2;
         } else {
            SUMA_LH("Mutli stage process Done");
            Done = 1;
         }
      }
      /* put a limit, in case convergence is not reached*/
      if ( (nit > 8 * (Stage2Type) * Opt->N_it) && !Done) {
         SUMA_LH("Funding limit reached. Abandoning improvements");
         Done = 1;
      }
   } while (!Done);
      
   CLEAN_RETURN:
   if (undershish) SUMA_free(undershish); undershish = NULL;
   if (overshish) SUMA_free(overshish); overshish = NULL;
   if (fvecind_over) SUMA_free(fvecind_over); fvecind_over = NULL;
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
   float hdim[3], t0, t1, t2, SOCenter[3], p0[3], p1[3];
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

   - The first part of this function is the source for function SUMA_GetVoxelsIntersectingTriangle
   If you find bugs here, fix them there too. 

*/
short *SUMA_SurfGridIntersect (SUMA_SurfaceObject *SO, float *NodeIJKlist, SUMA_VOLPAR *VolPar, int *N_inp, int fillhole, THD_3dim_dataset *fillmaskset)
{
   static char FuncName[]={"SUMA_SurfGridIntersect"};
   short *isin=NULL;
   byte *ijkmask=NULL, *inmask = NULL, *ijkout = NULL;
   float *p1, *p2, *p3, min_v[3], max_v[3], p[3], dist;
   float MaxDims[3], MinDims[3], SOCenter[3], dxyz[3];
   int nn, nijk, nx, ny, nz, nxy, nxyz, nf, n1, n2, n3, nn3, *voxelsijk=NULL, N_alloc, en;
   int N_inbox, nt, nt3, ijkseed = -1, N_in, N_realloc, isincand;
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
      SUMA_TRIANGLE_BOUNDING_BOX(p1, p2, p3, min_v, max_v);
      #if 0
         if (LocalHead && nf == 5 ) {
            FILE *fout = NULL;
            int i;
            fout = fopen("SUMA_SurfGridIntersect_BB.1D","w");
            if (fout) {
               SUMA_LH("Writing BB for debug...");
               for (i=0; i<3; ++i) {
                  fprintf(fout,"%d po p1 p2: %f %f %f\n", nf, p1[i], p2[i], p3[i]);
                  fprintf(fout,"%d min max: %f %f\n", nf, min_v[i], max_v[i]);
               }
               fclose(fout);
            } else {
               SUMA_LH("Failed to write BB for debug...");
            }   
         }
      #endif
      /* quick check of preallocate size of voxelsijk */
      en =((int)(max_v[0] - min_v[0] + 3) * (int)(max_v[1] - min_v[1] + 3) * (int)(max_v[2] - min_v[2] + 3)); 
      if ( en > N_alloc) {
         ++N_realloc; if (N_realloc > 5) { SUMA_SL_Warn("Reallocating, increase limit to improve speed.\nEither triangles too large or grid too small"); }
         N_alloc = 2*en;
         voxelsijk = (int *)SUMA_realloc(voxelsijk, 3*N_alloc*sizeof(int));
         if (!voxelsijk) { SUMA_SL_Crit("Failed to Allocate!"); SUMA_RETURN(NULL); }
      } 
      /* find the list of voxels inhabiting this box */
      N_inbox = N_alloc; /* that's used to tell SUMA_VoxelsInBox how much is allocated. 
                            N_inbox will reflect the number of voxels in the box on the way back 
                            from SUMA_VoxelsInBox */
      if (!SUMA_VoxelsInBox(voxelsijk, &N_inbox, min_v, max_v)) {
         SUMA_SL_Err("Unexpected error!"); SUMA_RETURN(NULL); 
      }
      if (!N_inbox) { SUMA_SL_Err("Unexpected error, no voxels in box!"); SUMA_RETURN(NULL);  }
      
      /* mark these voxels as inside the business */
      for (nt=0; nt < N_inbox; ++nt) {
         nt3 = 3*nt;
         if (voxelsijk[nt3] < nx &&  voxelsijk[nt3+1] < ny &&  voxelsijk[nt3+2] < nz) {
            isincand = 0;
            nijk = SUMA_3D_2_1D_index(voxelsijk[nt3], voxelsijk[nt3+1], voxelsijk[nt3+2], nx , nxy);  
            #if 0
               if (nijk == 6567) {
                  fprintf(SUMA_STDERR,"%s: %d examined, cand %d initial isin[%d] = %d\n", FuncName, nijk,  isincand, nijk, isin[nijk]);
               }
            #endif
            if (1 || !isin[nijk]) { /* Used to be that if a voxel was tagged (isin[nijk]), do not tag it again, 
                           But that is not good if a voxel has been tagged outside for one
                           node but should be tagged inside the surface for that voxel's intersection
                           with a triangle or perhaps for containing another node. */ 
               /* what side of the plane is this voxel on ? */
               p[0] = (float)voxelsijk[nt3]; p[1] = (float)voxelsijk[nt3+1]; p[2] = (float)voxelsijk[nt3+2]; 
               SUMA_DIST_FROM_PLANE(p1, p2, p3, p, dist);
               
               if (dist) {
                  if (SUMA_IS_NEG(VolPar->Hand * dist)) isincand = SUMA_IN_TRIBOX_INSIDE;  /* ZSS Added handedness factor. who would have thought? Be damned 3D coord systems! */
                  else isincand = SUMA_IN_TRIBOX_OUTSIDE; 
               }
               #if 0               
                  if (nijk == 6567) {
                     fprintf(SUMA_STDERR,"    after dist check cand = %d\n", isincand);
                     SUMA_Set_VoxIntersDbg(1);
                  }
               #endif              
               if (1) { /* does this triangle actually intersect this voxel ?*/
                  if (SUMA_isVoxelIntersect_Triangle (p, dxyz, p1, p2, p3)) {
                     if (isincand == SUMA_IN_TRIBOX_INSIDE) isincand = SUMA_INTERSECTS_TRIANGLE_INSIDE;
                     else isincand = SUMA_INTERSECTS_TRIANGLE_OUTSIDE;
                  } 
               }
               #if 0               
                  if (nijk == 6567) {
                     SUMA_Set_VoxIntersDbg(0);
                     fprintf(SUMA_STDERR,"    after intersection cand = %d\n", isincand);
                  }              
               #endif              
               if (isincand > isin[nijk]) { /* voxel has graduated further inwards */
                  if (!isin[nijk]) { ++(*N_inp);   } /* a new baby */
                  isin[nijk] = isincand;
               }
               #if 0               
                  if (nijk == 6567) {
                     fprintf(SUMA_STDERR,"    final isin[%d] = %d\n", nijk, isin[nijk]);
                  } 
               #endif              
                   
            }
         }
      }
   }
   
   if (LocalHead) {
      FILE *fout = NULL;
      int i;
      fout = fopen("SUMA_SurfGridIntersect_isin1.1D","w");
      if (fout) {
         SUMA_LH("Writing isin for debug...");
         for (i=0; i<nxyz; ++i) {
            if (isin[i]) fprintf(fout,"%d   %d\n", i, isin[i]);
         }
         fclose(fout);
      } else {
         SUMA_LH("Failed to write isin for debug...");
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
         int Found = 0, cnt, itry = 0;
         SUMA_MT_INTERSECT_TRIANGLE *mti = NULL; 

         /* Ray from a node on the surface to the center */
         p1[0] = SOCenter[0];    
         p1[1] = SOCenter[1];    
         p1[2] = SOCenter[2];    
         while (!Found && itry < SO->N_Node/100) {
            p0[0] = NodeIJKlist[3*itry+0]; 
            p0[1] = NodeIJKlist[3*itry+1]; 
            p0[2] = NodeIJKlist[3*itry+2]; 

            SUMA_UNIT_VEC(p0, p1, u, un);
            SUMA_LHv("Try %d, un=%f...\nP0[%f %f %f] P1[%f %f %f]\n", 
                  itry, un, p0[0], p0[1], p0[2], p1[0], p1[1], p1[2]);
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
            ++itry;
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
      if (LocalHead) {
         FILE *fout = NULL;
         int i;
         fout = fopen("SUMA_SurfGridIntersect_inmask.1D","w");
         if (fout) {
            SUMA_LH("Writing inmask for debug...");
            for (i=0; i<nxyz; ++i) {
               if (inmask[i]) fprintf(fout,"%d   %d\n", i, inmask[i]);
            }
            fclose(fout);
         } else {
            SUMA_LH("Failed to write inmask for debug...");
         }
      }

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
short *SUMA_FindVoxelsInSurface (
   SUMA_SurfaceObject *SO, SUMA_VOLPAR *VolPar, 
   int *N_inp, int fillhole, THD_3dim_dataset *fillmaskset) 
{
   static char FuncName[]={"SUMA_FindVoxelsInSurface"};
   short *isin = NULL;
   byte *tmpin = NULL;
   int i, N_in, j, k , n, khits, dims[2], N_hits, iii, jjj, niii, ncul;
   float *tmpXYZ=NULL, Center[3], 
         MaxDims[3], MinDims[3], aMaxDims, aMinDims, delta_t;
   float hdim[3], t0, t1, t2, SOCenter[3], p0[3], p1[3];
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
   #if MSK_DBG
      if (fdb) {
         fprintf(fdb, "Begin NodeList and copy\n");
         for (i=0; i<SO->N_Node; ++i) {
            fprintf(fdb, "%f %f %f\n%f %f %f\n\n",
                     SO->NodeList[3*i  ], SO->NodeList[3*i+1],
                     SO->NodeList[3*i+2], 
                     tmpXYZ[3*i  ], tmpXYZ[3*i+1], tmpXYZ[3*i+2]);
         }
         fprintf(fdb, "Begin VolPar action\n"); 
         SUMA_Show_VolPar(VolPar, fdb);
      }
      
   #endif
   /* transform the surface's coordinates from RAI to 3dfind */
   if (!SUMA_vec_dicomm_to_3dfind (tmpXYZ, SO->N_Node, VolPar)) {
      SUMA_SL_Err("Failed to effectuate coordinate transform.");
      SUMA_free(tmpXYZ); tmpXYZ = NULL;
      SUMA_RETURN(NULL);
   }
   #if MSK_DBG
      if (fdb) {
         fprintf(fdb, "Begin SUMA_vec_dicomm_to_3dfind\n");
         for (i=0; i<SO->N_Node; ++i) {
            fprintf(fdb, "%f %f %f\n%f %f %f\n\n",
                     SO->NodeList[3*i  ], SO->NodeList[3*i+1],
                     SO->NodeList[3*i+2], 
                     tmpXYZ[3*i  ], tmpXYZ[3*i+1], tmpXYZ[3*i+2]);
         }
      }
   #endif
   
   /* find the new center and bounding box for 
   the surface in the new coordinate system*/
   SUMA_MIN_MAX_SUM_VECMAT_COL ( tmpXYZ, 
                                 SO->N_Node, 3, 
                                 MinDims, MaxDims, SOCenter); 
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
   isin = SUMA_SurfGridIntersect (SO, tmpXYZ, VolPar, 
                                 &N_in, fillhole, fillmaskset);               

   *N_inp = N_in;
   #if MSK_DBG
      fclose(fdb); fdb = NULL;
   #endif
   
   delta_t = SUMA_etime (&tti, 1); 
   if (LocalHead) {
      fprintf(SUMA_STDERR,
         "%s: Execution time %f seconds.\n", FuncName, delta_t);
   }
   
   SUMA_free(tmpXYZ); tmpXYZ = NULL;
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
   int in, N_troub = 0, cond1=0, cond2=0, cond3 = 0, cond4 = 0, nn, ShishMax, *fvecind_under, *fvecind_over, ni, nj, nk, nx, ny, nxy;   
   float MinMax_over[2], MinMax[2], MinMax_dist[2], MinMax_over_dist[2], Means[3], tb; 
   float U[3], Un, *a, P2[2][3], *norm, shft, *b, gradient_promise; 
   float *touchup=NULL, targ[3]; 
   float *overshish=NULL, *undershish=NULL, *gradovershish=NULL;
   static FILE *GradOut = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
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
      fvecind_under = (int *)SUMA_malloc(sizeof(int)*ShishMax);
      fvecind_over = (int *)SUMA_malloc(sizeof(int)*ShishMax);
      touchup = (float *)SUMA_calloc(SO->N_Node, sizeof(float));   
      if (!touchup) { SUMA_SL_Crit("Failed to allocate"); SUMA_RETURN(NULL); }  
      for (in=0; in<SO->N_Node; ++in) {   
         SUMA_Find_IminImax(&(SO->NodeList[3*in]), &(SO->NodeNormList[3*in]), Opt, in,  MinMax, MinMax_dist, MinMax_over, MinMax_over_dist, 
                           Means, undershish, overshish, fvecind_under, fvecind_over,  Opt->d1, Opt->d4, ShishMax); 
         /* Shift the node outwards:   
            0- the minimum location of the minimum over the node is not 0
            AND
            1- if the minimum over the node is closer than the minimum below the node  
               or the internal minimum is more than 1mm away  and the internal min is masked to 0 
               or the minimum below the node just sucks, > tb and the minimum over the node is < tb
               The second part of the condition is meant to better model a finger of cortex that has enough csf on the outer surface 
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
                  if (fvecind_over[nn] < 0 || fvecind_over[nn] > Opt->nvox) {
                     fprintf(SUMA_STDERR,"Error %s: Bad value for  fvecind_over[%d]=%d\n", FuncName, nn, fvecind_over[in]);
                  } else {
                     if (MinMax_over_dist[0] && cond1 && cond2 && cond3 && Opt->fvec[fvecind_over[nn]]) { 
                        while (nn < ShishMax && fvecind_over[nn] >=0) {
                           Opt->fvec[fvecind_over[nn]] = 0;
                           if (LocalHead) { 
                              SUMA_1D_2_3D_index(fvecind_over[nn], ni, nj, nk, nx, nxy);
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
      
      if (fvecind_under) SUMA_free(fvecind_under); fvecind_under= NULL;
      if (fvecind_over) SUMA_free(fvecind_over); fvecind_over= NULL;
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
float *SUMA_Suggest_Touchup(
   SUMA_SurfaceObject *SO, 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt, 
   float limtouch, SUMA_COMM_STRUCT *cs, 
   int *N_touch)
{
   static char FuncName[]={"SUMA_Suggest_Touchup"};
   int in, N_troub = 0, cond1=0, cond2=0, cond3 = 0, 
       cond4 = 0, cond5 = 0, nn, ShishMax, Down, Cross;   
   float MinMax_over[2], MinMax[2], MinMax_dist[2],
         MinMax_over_dist[2], Means[3], tb, GradThick; 
   float U[3], Un, *a, P2[2][3], *norm, shft, *b; 
   float *touchup=NULL; 
   float *overshish=NULL, *undershish=NULL, *gradovershish=NULL;
   int *fvecind_under=NULL, *fvecind_over=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
      if (Opt->debug > 2) LocalHead = YUP;
      *N_touch = 0;
      
      
      ShishMax = (int)(SUMA_MAX_PAIR(Opt->d1, Opt->d4) / Opt->travstp * 1.2);
      if (LocalHead && Opt->emask) {
         SUMA_S_Notev("************************* Using emask!!!\n"
                     "ShishMax = %d\n", ShishMax);
      }
      overshish = (float *)SUMA_malloc(sizeof(float)*ShishMax);
      undershish = (float *)SUMA_malloc(sizeof(float)*ShishMax);
      gradovershish = (float *)SUMA_malloc(sizeof(float)*ShishMax);
      fvecind_under = (int *)SUMA_malloc(sizeof(int)*ShishMax);
      fvecind_over = (int *)SUMA_malloc(sizeof(int)*ShishMax);
      touchup = (float *)SUMA_calloc(SO->N_Node, sizeof(float));   
      if (!touchup) { SUMA_SL_Crit("Failed to allocate"); SUMA_RETURN(NULL); }  
      for (in=0; in<SO->N_Node; ++in) {   
         SUMA_Find_IminImax(  &(SO->NodeList[3*in]), 
                              &(SO->NodeNormList[3*in]), 
                              Opt, in,  MinMax, 
                              MinMax_dist, MinMax_over, 
                              MinMax_over_dist, Means, 
                              undershish, overshish, 
                              fvecind_under, fvecind_over,  
                              Opt->d1, Opt->d4, ShishMax); 
         /* Shift the node outwards:   
            0- the minimum location of the minimum over the node is not 0
            AND
            1- if the minimum over the node is closer than the minimum 
               below the node  or the internal minimum is more than 1mm away 
               and the internal min is masked to 0 or the minimum below the node
               just sucks, > tb and the minimum over the node is < tb
               The second part of the condition is meant to better model a
               finger of cortex that has enough csf on the outer surface 
               to have it be set to 0 by SpatNorm this dark csf would keep the
               brain surface from reaching the outer surface 
            AND   
            2- (Applied only if not using emask) if the maximum over the node is
               much higher than the maximum below the node (fat on skull), 
               it must be beyond the location of the minimum over the node   
            AND   
            3- The new position of the node is not much larger than the current
               value
            AND
            4- The mean above the node is not more than 1.2 * the mean below the
               node.
            AND 
            5- If an edge mask is provided, do not move if you are within 1.5mm
               of an edge value   
         */ 
         tb = (MinMax[1] - Opt->t2) * 0.5 + Opt->t2; 
         cond1 = (      (MinMax_over_dist[0] < MinMax_dist[0]) 
                     || (MinMax_dist[0] > 1 && MinMax[0] >= MinMax_over[0] ) 
                     || (MinMax[0] > tb && MinMax_over[0] < MinMax[0]) ); 
         /* statement below used to be: 
            if (  MinMax_over[1] > MinMax[1] && 
                  MinMax_over[1] > 0.9 * Opt->t98 && 
                  (MinMax_over_dist[1] < MinMax_over_dist[0]) ) cond2 = 0;  */
         if (  !Opt->emask && 
               (   MinMax_over[1] > 1.2 * MinMax[1] && 
                  (MinMax_over_dist[1] < MinMax_over_dist[0]) ) ) cond2 = 0;  
         else cond2 = 1; 
         if (MinMax_over[0] > 1.2 * Means[0]) cond3 = 0;
         else cond3 = 1;
         if (Means[2] > Means[1]) cond4 = 0;  /* March 21 */
         else cond4 = 1; 
         if (Opt->NodeDbg == in) {   
               a = &(SO->NodeList[3*in]);   
               fprintf(SUMA_STDERR, 
                  "%s: Debug during touchup for node %d:\n"
                  " emask pointer: %p\n"   
                  " MinMax     =[%.1f,%.1f] MinMax_dist     = [%.2f,%.2f] \n"   
                  " MinMax_over=[%.1f,%.1f] MinMax_over_dist= [%.2f,%.2f] \n"   
                  " Val at node %.1f, mean below %.1f, mean above %.1f\n"  
                  " zalt= %f, r/2 = %f\n"    
                  " Conditions: %f %d %d %d %d\n",
                  FuncName, in, Opt->emask,
                  MinMax[0], MinMax[1], MinMax_dist[0], MinMax_dist[1], 
                  MinMax_over[0], MinMax_over[1], 
                  MinMax_over_dist[0], MinMax_over_dist[1], 
                  Means[0], Means[1], Means[2],   
                  a[2] - SO->Center[2], Opt->r/2, 
                  MinMax_over_dist[0] , cond1 , cond2, cond3, cond4); 
         }  
         if (Opt->emask) {
            /* don't move if a node is too close to an edge */
            int ie = 0;
            if (Opt->NodeDbg == in) { 
               fprintf(SUMA_STDERR, 
                  "%s: Node %d's gradient below.\n", FuncName, in);
               while (fvecind_under[ie] >= 0) {
                  fprintf(SUMA_STDERR, "\t%f",Opt->emask[fvecind_under[ie]]);
                  ++ie;   
               }
               fprintf(SUMA_STDERR, "\n");
            }
            ie = 0;
            cond5 = 1;
            while (  cond5 && 
                     fvecind_under[ie] >= 0 && 
                     Opt->travstp*ie < 3.5/THD_BN_rat()) {
               if (Opt->emask[fvecind_under[ie]]) {
                  if (Opt->NodeDbg == in) { 
                     fprintf(SUMA_STDERR, 
                        "%s: Node %d will not be allowed to grow, "
                        "too close to an edge!\n", FuncName, in);
                  }
                  cond5 = 0;    
               }
               ++ie;
            }
         } else {
            cond5 = 1;
         }
         
         if (MinMax_over_dist[0] && cond1 && cond2 && cond3 && cond5) {   
               a = &(SO->NodeList[3*in]);   
               if (cond4 ) {
                  /* reposition nodes */  
                  if (Opt->NodeDbg == in) { fprintf(SUMA_STDERR, "%s: Suggest repair for node %d (better min above):\n"   
                                       " MinMax     =[%.1f,%.1f] MinMax_dist     = [%.2f,%.2f] \n"   
                                       " MinMax_over=[%.1f,%.1f] MinMax_over_dist= [%.2f,%.2f] \n"   
                                       " Val at node %.1f, mean below %.1f, mean above %.1f\n"  
                                       " zalt= %f, r/2 = %f, Opt->shrink_bias = %f\n",    
                          FuncName, in, 
                          MinMax[0], MinMax[1], MinMax_dist[0], MinMax_dist[1], 
                          MinMax_over[0], MinMax_over[1], MinMax_over_dist[0], MinMax_over_dist[1], 
                          Means[0], Means[1], Means[2],   
                          a[2] - SO->Center[2], Opt->r/2, Opt->shrink_bias[in]); } 
         /* what is happening with the gradients NOT USED YET*/ 
         Down = 0;
         Cross = 0;
         GradThick = 0.0;
         for (nn=1; nn<ShishMax; ++nn) {
            if (overshish[nn] >= 0) {
               gradovershish[nn-1] = overshish[nn] - overshish[nn-1];
            }else {
               gradovershish[nn-1] = 0;
            }
            if ((gradovershish[nn-1] < - Opt->tm/3.0 || (overshish[nn] < Opt->tm/2.0 && gradovershish[nn-1] < 0)) && !Cross) {  
               /* if you cross a large negative gradient, OR a smaller negative one from an already low intensity */
               Down = nn;
               if (Opt->NodeDbg == in) {
                  fprintf(SUMA_STDERR, "%s: Crossing down limit (%f) gradient at node %d (%f)\n", FuncName, Opt->tm/3.0, in, gradovershish[nn-1]);
               }
            }
            if (Down && gradovershish[nn-1] > Opt->tm/3.0 && !Cross) {
               Cross = nn;
               GradThick = (Cross-Down)*Opt->travstp;
               if (Opt->NodeDbg == in) {
                   fprintf(SUMA_STDERR, "%s: Crossing up limit (%f) gradient at node %d (%f): Thick = %f mm\n", 
                  FuncName, Opt->tm/3.0, in, gradovershish[nn-1], GradThick);
               }
            }
         }
         if (Opt->NodeDbg == in) {
            fprintf(SUMA_STDERR, "%s: Value over node %d:\n", FuncName, in);
            for (nn=0; nn<ShishMax; ++nn) { fprintf(SUMA_STDERR, "   %f", overshish[nn] ); }fprintf(SUMA_STDERR, "\n");
            fprintf(SUMA_STDERR, "%s: Gradient over node %d:\n", FuncName, in);
            for (nn=1; nn<ShishMax; ++nn) { fprintf(SUMA_STDERR, "   %f", gradovershish[nn-1] ); }fprintf(SUMA_STDERR, "\n");
         }
             if ((GradThick > 0.0)) { 
               if (Opt->NodeDbg == in) { fprintf(SUMA_STDERR, "%s: Node %d (%f, %f, %f) is on a gradient of %f\n",
                                       FuncName, in, a[0], a[1], a[2], GradThick); }
               if (SUMA_IS_LOWER_ZONE(a, SO->Center)) { /* lower part, slow down growth no matter how thick gradient is */
                  touchup[in] = SUMA_MIN_PAIR(((Down)*Opt->travstp), (SUMA_MIN_PAIR(MinMax_over_dist[0], 4)));  
                  if (Opt->NodeDbg == in) fprintf(SUMA_STDERR, "%s: Node is in lower areas, growth slowed to gradient edge (or hard limit) at %f mm\n", FuncName, touchup[in]);
                  ++N_troub; 
               } else { /* upper part, allow for growth unless gradient is thin */
                  if (GradThick < 3.0) {
                     touchup[in] = SUMA_MIN_PAIR(((Down)*Opt->travstp), MinMax_over_dist[0]);  
                     if (Opt->NodeDbg == in) fprintf(SUMA_STDERR, "%s: Node is in upper area, gradient is thin, slow growth to %fmm\n", FuncName, touchup[in]);
                     ++N_troub; 
                  }else {
                     /* thick gradient, perhaps presence of finger-like lobes with lots of csf */
                     touchup[in] = MinMax_over_dist[0]; 
                     if (Opt->NodeDbg == in) fprintf(SUMA_STDERR, "%s: Node is in upper area, gradient is thick, allow growth of %fmm\n", FuncName, touchup[in]);
                     ++N_troub; 
                  }
               }
             } else {  /* Used to insist on value and top half if (MinMax_over_dist[0] && (a[2] - SO->Center[2] > 0))  */
                                                /* to avoid going into eye balls */
                                                /* Then added !SUMA_IS_EYE_ZONE(a,SO->Center) to avoid eyes */  
                                                /* but that is no longer necessary with fancier expansion conditions. */
                     if (SUMA_IS_LOWER_ZONE(a, SO->Center)) {
                        touchup[in] = SUMA_MIN_PAIR((MinMax_over_dist[0]), 4);   /* No big jumps below the belt */
                     } else {
                        touchup[in] = MinMax_over_dist[0]; /* towards the top, go nuts */
                     } 
                     ++N_troub; 
                  }  
            } else { /* freeze that node, an attempt to control leak into lardo */
               Opt->Stop[in] = -1.0;
               if (Opt->NodeDbg == in) { fprintf(SUMA_STDERR, "%s: Freezing node %d, cond4 = %d \n", 
                  FuncName, in, cond4); }
            }
            /* make sure that touchup does not go beyond the closest edge above the node */
            if (Opt->emask && touchup[in] > 0.0) {
               int ie = 0;
               float edged = -1.0;
               while (edged < 0 && fvecind_over[ie] >= 0 ) {
                  if (Opt->emask[fvecind_over[ie]]) {
                     edged = Opt->travstp*(ie+1); /* stop at next voxel, not before the edge */
                     if (edged < touchup[in]) {
                        if (Opt->debug > 2 || Opt->NodeDbg == in) { 
                           fprintf(SUMA_STDERR, "%s: Node %d will not be allowed to grow beyond edge on top of it at %fmm!\n", FuncName, in, edged);
                        }
                        touchup[in] = edged;
                        if (!touchup[in]) --N_troub;
                     } else {
                        /* try again */
                        edged = -1.0;
                     }
                  }
                  ++ie;
               }
            }
            #if 0  /* turns out not to do much ... */
            /* if a node is not to move outwards any more and is sitting on a point much brighter than below it, 
            then move it to the next edge behind it*/
            if (0 && !touchup[in]) {
               if (Means[0] > 1.2* Means[1] && Means[0] > Opt->tm) { /* if value at node is bright and quite higher than mean below it */
                  if (Opt->emask) {
                     int ie = 0;
                     float edged = -1.0;
                     while (edged < 0 && fvecind_under[ie] >= 0 ) {
                        if (Opt->emask[fvecind_under[ie]]) {
                           edged = SUMA_MIN_PAIR(Opt->travstp*(ie), 2.0); /* don't allow big moves */
                           if (1 || Opt->NodeDbg == in || Opt->debug > 2) { 
                              fprintf(SUMA_STDERR, "%s: Node %d: %f\t%f\t%f\n   will be pushed back to edge at -%fmm!\n", 
                                             FuncName, in, Means[0], Means[1], Opt->tm, edged);
                           }
                           
                           touchup[in] = -edged;
                           ++N_troub;
                        }
                        ++ie;
                     }
                  }
               }
            }
            #endif  
         }  
      }  
      
      
      if (overshish) SUMA_free(overshish); overshish = NULL; 
      if (gradovershish) SUMA_free(gradovershish); gradovershish = NULL; 
      if (undershish) SUMA_free(undershish); undershish = NULL; 
      if (fvecind_under) SUMA_free(fvecind_under); fvecind_under=NULL;
      if (fvecind_over) SUMA_free(fvecind_over); fvecind_over = NULL;

   *N_touch = N_troub;
   
   SUMA_RETURN(touchup);
}            

/*!
   int SUMA_GetNodeHood(SUMA_SurfaceObject *SO, int in, float frac, float *xyz_list)
   \param SO (SUMA_SurfaceObject *) with structure FN present
   \param in (int) node index
   \param frac (float) frac of segment to move from central node
   \param xyz_list (float *) vector to containing xyz triplets of node in and surrounding
                             locations at frac * segment length from in for each segment
                             formed by in and one of its neighbors. Note that xyz_list should
                             be big enough to hold 3*(SO->FN->N_Neighb_max+1)*sizeof(float)
   \return (int) (-1) in case of trouble.
*/ 
            
int SUMA_GetNodeHood(SUMA_SurfaceObject *SO, int in, float frac, float *xyz_list)
{
   static char FuncName[]={"SUMA_GetNodeHood"};
   int i, i3, ine, ine3;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!SO || !SO->FN) {
      SUMA_SL_Err("No SO or SO->FN!!!\n");
      SUMA_RETURN(-1);
   }

   if (frac < 0.0 || frac > 1.0) {
      SUMA_SL_Err("frac must be between 0 and 1.0!!!\n");
      SUMA_RETURN(-1);
   }
   /* fill first locations with node coords */
   xyz_list[0] = SO->NodeList[3*in  ];
   xyz_list[1] = SO->NodeList[3*in+1];
   xyz_list[2] = SO->NodeList[3*in+2];
   
   for (i=0; i<SO->FN->N_Neighb[in]; ++i) {
      i3 = 3*i;
      ine = SO->FN->FirstNeighb[in][i]; ine3 = 3*ine;
      xyz_list[3+i3] = xyz_list[0] + frac * (SO->NodeList[ine3  ] - xyz_list[0]);
      xyz_list[4+i3] = xyz_list[1] + frac * (SO->NodeList[ine3+1] - xyz_list[1]);
      xyz_list[5+i3] = xyz_list[2] + frac * (SO->NodeList[ine3+2] - xyz_list[2]);
   }
   
   SUMA_RETURN(0);
}

#define SUMA_IS_ON_FLESH(vect, ie) ( ( vect[ie] > Opt->t2 || (ie > 0 && vect[ie-1] > Opt->t2) || (vect[ie+1] > Opt->t2) ) ? 1 : 0 )

float *SUMA_Suggest_Touchup_PushOuterSkull(SUMA_SurfaceObject *SO, 
                                    SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt, 
                                    float limtouch, SUMA_COMM_STRUCT *cs, int *N_touch,
                                    int PriorityAtNode)
{
   static char FuncName[]={"SUMA_Suggest_Touchup_PushOuterSkull"};
   int in, N_troub = 0, cond1=0, cond2=0, cond3 = 0, cond4 = 0, cond5 = 0, nn, ShishMax, Down, Cross, inh;   
   float MinMax_over[2], MinMax[2], MinMax_dist[2], MinMax_over_dist[2], Means[3], tb, GradThick; 
   float U[3], Un, *a, P2[2][3], *norm, shft, *b; 
   float *touchup=NULL, EdgeBelow=-1.0; 
   float *overshish=NULL, *undershish=NULL, *gradovershish=NULL, *xyz_list=NULL, *shft_list=NULL;
   int *fvecind_under=NULL, *fvecind_over=NULL, ie;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!Opt->emask) {
      SUMA_SL_Err("No emask!!!\n");
      SUMA_RETURN(NOPE);
   }
      
      Opt->d4 = limtouch+1.0;
      
      ShishMax = (int)(SUMA_MAX_PAIR(Opt->d1, Opt->d4) / Opt->travstp * 1.2); 
      overshish = (float *)SUMA_malloc(sizeof(float)*ShishMax);
      undershish = (float *)SUMA_malloc(sizeof(float)*ShishMax);
      gradovershish = (float *)SUMA_malloc(sizeof(float)*ShishMax);
      fvecind_under = (int *)SUMA_malloc(sizeof(int)*ShishMax);
      fvecind_over = (int *)SUMA_malloc(sizeof(int)*ShishMax);
      touchup = (float *)SUMA_calloc(SO->N_Node, sizeof(float));   
      xyz_list = (float *) SUMA_calloc(3*(SO->FN->N_Neighb_max+1), sizeof(float));
      shft_list = (float *) SUMA_calloc(SO->FN->N_Neighb_max+1, sizeof(float));
      if (!touchup) { SUMA_SL_Crit("Failed to allocate"); SUMA_RETURN(NOPE); }  
      for (in=0; in<SO->N_Node; ++in) {
         /* get a bunch of locations around the node */
         SUMA_GetNodeHood(SO, in, 0.5, xyz_list);
         a = &(xyz_list[0]); /* direction U will be same for all neighbors */
         SUMA_UNIT_VEC(SO->Center, a , U, Un);
         for (inh=0; inh <= SO->FN->N_Neighb[in]; ++inh) {
               if (Opt->debug > 2 && Opt->NodeDbg == in ) { 
                     fprintf(SUMA_STDERR, "%s: Searching node %d's %dth neighbor\n %f %f %f\n", 
                                    FuncName, in, inh, xyz_list[3*inh], xyz_list[3*inh+1],xyz_list[3*inh+2]);
               }
               
              SUMA_Find_IminImax(&(xyz_list[3*inh]), U, Opt, in,  MinMax, MinMax_dist, MinMax_over, MinMax_over_dist, Means,
                                  undershish, overshish, fvecind_under, fvecind_over,  Opt->d1, Opt->d4, ShishMax); 
               /* Shift the node outwards:   
                  0- if node is not on an edge and there is an edge within Max(Opt->d1 and Opt->d4) and it is not air in between
               */ 


               { /* find next location based on edges */
                  /* find the farthest edge */
                  
                  if (Opt->debug > 2 && Opt->NodeDbg == in) { 
                     fprintf(SUMA_STDERR, "%s: emask over node %d, neighb %d\n", FuncName, in, inh);
                     ie = 0;
                     while (fvecind_over[ie] >= 0) {
                        fprintf(SUMA_STDERR,"%d\t %f\t\t", ie, Opt->emask[fvecind_over[ie]]);
                        ++ie;
                     }
                     fprintf(SUMA_STDERR,"\n");
                  }
                  ie = 0;
                  while (fvecind_over[ie] >= 0) {
                     if (Opt->emask[fvecind_over[ie]] && SUMA_IS_ON_FLESH(overshish, ie)) {
                        touchup[in] = Opt->travstp*(ie+1);
                        if (Opt->debug > 2 && Opt->NodeDbg == in) { 
                              fprintf(SUMA_STDERR, "%s: Edge candidate for %d, neighb %d at %f with edge %f (vox index %d)\n",
                                                      FuncName, in, inh, touchup[in], 
                                                      Opt->emask[fvecind_over[ie]], fvecind_over[ie]);
                        }                             
                     }
                     ++ie;
                  }
                  
               }
               
               shft_list[inh] = touchup[in]; /* store touchup result */
         }
         /* Now sort the shft_list */
         if (Opt->debug > 2 && Opt->NodeDbg == in) {   
            fprintf(SUMA_STDERR, "%s: Shifts in neighborhood of node %d\n", FuncName, in);
            for (ie=0; ie<SO->FN->N_Neighb[in]+1; ++ie) {
               fprintf(SUMA_STDERR, "%f \t", shft_list[ie]);
            }
            fprintf(SUMA_STDERR, "\n");
         }
         if (PriorityAtNode && shft_list[0] > 0.001) {
            /* OK, accept */
            touchup[in] = shft_list[0];
            if (Opt->debug > 2 && Opt->NodeDbg == in) {   
               fprintf(SUMA_STDERR, "%s: Touchup value for node %d is %fmm\n", FuncName, in, touchup[in]);
            }
         } else {
            qsort(shft_list, SO->FN->N_Neighb[in], sizeof(float), (int(*) (const void *, const void *)) SUMA_compare_float);
            /* get the median */
            touchup[in] = shft_list[(int)(SO->FN->N_Neighb[in]/2)];
         
            if (Opt->debug > 2 && Opt->NodeDbg == in) {   
               fprintf(SUMA_STDERR, "%s: Median touchup value for node %d is %fmm\n", FuncName, in, touchup[in]);
            }
         }
         if (touchup[in]) ++N_troub;
      }
              
      
      if (overshish) SUMA_free(overshish); overshish = NULL; 
      if (gradovershish) SUMA_free(gradovershish); gradovershish = NULL; 
      if (undershish) SUMA_free(undershish); undershish = NULL; 
      if (fvecind_under) SUMA_free(fvecind_under); fvecind_under=NULL;
      if (fvecind_over) SUMA_free(fvecind_over); fvecind_over = NULL;
      if (shft_list) SUMA_free(shft_list); shft_list = NULL;
      if (xyz_list) SUMA_free(xyz_list); xyz_list = NULL;
      
   *N_touch = N_troub;
   
   SUMA_RETURN(touchup);

}

/*
   average intensity above and below a location 
*/
#define SUMA_AVERAGE_BELOW_ABOVE(sunder, sover, ie, depth, undervect, overvect) {\
   /* sum over ie should be larger than sum below ie */  \
      cnt = 1; \
      sover = 0.0;   \
      while (overvect[ie+cnt] >= 0  && cnt <= depth) {  \
         sover += overvect[ie+cnt];   \
         ++cnt;   \
      }  \
      if (cnt > 1) sover /= (float)(cnt-1);  \
      /* now back off from ie */ \
      cnt = 1; \
      sunder = 0.0;  \
      while (ie - cnt >= 0 && cnt <= depth) {   \
         sunder += overvect[ie-cnt];  \
         ++cnt;   \
      }  \
      /* continue below node */  \
      while (undervect[ie+depth-cnt] >= 0  && cnt <= depth) { \
         sunder += undervect[ie+depth-cnt]; \
         ++cnt;   \
      }  \
      if (cnt > 1) sunder /= (float)(cnt-1);    \
} 


float *SUMA_Suggest_Touchup_PushInnerSkull(SUMA_SurfaceObject *SO, 
                                    SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt, 
                                    float limtouch, SUMA_COMM_STRUCT *cs, int *N_touch)
{
   static char FuncName[]={"SUMA_Suggest_Touchup_PushInnerSkull"};
   int in, N_troub = 0, cond1=0, cond2=0, cond3 = 0, cond4 = 0, cond5 = 0, nn, ShishMax, Down, Cross, inh;   
   float MinMax_over[2], MinMax[2], MinMax_dist[2], MinMax_over_dist[2], Means[3], tb, GradThick; 
   float U[3], Dw[3], Up[3], dwn, dup, Un, *a, P2[2][3]={ {0.0, 0.0, 0.0}, {0.0, 0.0, 0.0} }, *norm, shft, *b; 
   float *touchup=NULL, EdgeBelow=-1.0; 
   float *overshish=NULL, *undershish=NULL, *gradovershish=NULL, 
         *xyz_list=NULL, *shft_list=NULL, *shft_list_under=NULL,
         sup, sdwn, sover, sunder;
   int *fvecind_under=NULL, *fvecind_over=NULL, ie, cnt;
   int nx, ny, nxy, i, j, k, depth, new_stop = 0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   nx = SO->VolPar->nx; ny = SO->VolPar->ny; nxy = nx * ny;

   if (!Opt->emask) {
      SUMA_SL_Err("No emask!!!\n");
      SUMA_RETURN(NOPE);
   }
      
      Opt->d4 = limtouch+1.0;
      
      ShishMax = (int)(SUMA_MAX_PAIR(Opt->d1, Opt->d4) / Opt->travstp * 1.2); 
      overshish = (float *)SUMA_malloc(sizeof(float)*ShishMax);
      undershish = (float *)SUMA_malloc(sizeof(float)*ShishMax);
      gradovershish = (float *)SUMA_malloc(sizeof(float)*ShishMax);
      fvecind_under = (int *)SUMA_malloc(sizeof(int)*ShishMax);
      fvecind_over = (int *)SUMA_malloc(sizeof(int)*ShishMax);
      touchup = (float *)SUMA_calloc(SO->N_Node, sizeof(float));   
      xyz_list = (float *) SUMA_calloc(3*(SO->FN->N_Neighb_max+1), sizeof(float));
      shft_list = (float *) SUMA_calloc(SO->FN->N_Neighb_max+1, sizeof(float));
      shft_list_under = (float *) SUMA_calloc(SO->FN->N_Neighb_max+1, sizeof(float));
      if (!touchup) { SUMA_SL_Crit("Failed to allocate"); SUMA_RETURN(NOPE); }  
      for (in=0; in<SO->N_Node; ++in) {
         if (1) {
            /* get a bunch of locations around the node */
            SUMA_GetNodeHood(SO, in, 0.5, xyz_list);
            a = &(xyz_list[0]); /* direction U will be same for all neighbors */
            b = &(Opt->Skull_Outer[3*in]);
            SUMA_UNIT_VEC(a, b , U, Un);  /* direction is along the line between the current location and the outer skull */
            for (inh=0; inh <= SO->FN->N_Neighb[in]; ++inh) {
                  if (Opt->debug > 2 && Opt->NodeDbg == in) { 
                        fprintf(SUMA_STDERR, "%s: Searching node %d's %dth neighbor\n %f %f %f\n", 
                                       FuncName, in, inh, xyz_list[3*inh], xyz_list[3*inh+1],xyz_list[3*inh+2]);
                  }

                 SUMA_Find_IminImax(&(xyz_list[3*inh]), U, Opt, in,  MinMax, MinMax_dist, MinMax_over, MinMax_over_dist, Means,
                                     undershish, overshish, fvecind_under, fvecind_over,  Opt->d1, Opt->d4, ShishMax); 

                  { /* find next location based on edges */
                     /* find the closest upper edge */
                     if (Opt->NodeDbg == in) { 
                        fprintf(SUMA_STDERR, "%s: emask over node %d, neighb %d\n", FuncName, in, inh);
                        ie = 0;
                        while (fvecind_over[ie] >= 0) {
                           fprintf(SUMA_STDERR,"%d\t %f\t\t", ie, Opt->emask[fvecind_over[ie]]);
                           ++ie;
                        }
                        fprintf(SUMA_STDERR,"\n");
                        fprintf(SUMA_STDERR, "%s: fvec over node %d, neighb %d\n", FuncName, in, inh);
                        ie = 0;
                        while (fvecind_over[ie] >= 0) {
                           SUMA_1D_2_3D_index(fvecind_over[ie], i, j, k, nx, nxy);
                           fprintf(SUMA_STDERR,"%d\t [%d %d %d] %f\t\t", ie, i, j, k, Opt->fvec[fvecind_over[ie]]);
                           ++ie;
                        }
                        fprintf(SUMA_STDERR,"\n");
                     }
                     ie = 0;
                     touchup[in] = 0.0;
                     depth = 3; /* how far to go over node and below it */
                     while (fvecind_over[ie] >= 0 && touchup[in] < 0.001 && Opt->travstp*(ie+1) < limtouch) {
                        if (Opt->emask[fvecind_over[ie]] 
                             &&  SUMA_IS_ON_FLESH(overshish, ie) /* we're on flesh either at location or 1 step around it */
                           ) {
                           SUMA_AVERAGE_BELOW_ABOVE(sunder, sover, ie, depth, undershish, overshish);
                           if (sover < sunder) {
                              if (Opt->NodeDbg == in) { 
                                 fprintf(SUMA_STDERR, "%s: Node %d's edge at %f is no good, sover = %f sunder = %f\n", FuncName, in, touchup[in], sover, sunder);
                              }
                              touchup[in] = 0.0;
                           } else {
                              touchup[in] = Opt->travstp*(ie);
                           }
                           if (Opt->Stop[in] < 0 && touchup[in] > 0.001) { /* special case, 1st estimate */
                              if (touchup[in] <= 1.0) {
                                 if (Opt->NodeDbg == in) { 
                                    fprintf(SUMA_STDERR, "%s: Node %d's edge at %f is too close\n", FuncName, in, touchup[in]);
                                 }
                                 /* too close to an edge, need to initialize Opt->Stop, ignore */
                                 touchup[in] = 0.0;
                              } 
                           }
                           if (Opt->debug > 2 && Opt->NodeDbg == in) { 
                                 fprintf(SUMA_STDERR, "%s: Edge candidate for %d, neighb %d at %f with edge %f (vox index %d)\n",
                                                         FuncName, in, inh, touchup[in], 
                                                         Opt->emask[fvecind_over[ie]], fvecind_over[ie]);
                           }                             
                        }
                        ++ie;
                     }

                  }
                  shft_list[inh] = touchup[in]; /* store upper touchup result */

                  { /* Then find closest lower edge */
                     if (Opt->debug > 2 && Opt->NodeDbg == in) { 
                        fprintf(SUMA_STDERR, "%s: emask under node %d, neighb %d\n", FuncName, in, inh);
                        ie = 0;
                        while (fvecind_under[ie] >= 0) {
                           fprintf(SUMA_STDERR,"%d\t %f\t\t", ie, Opt->emask[fvecind_under[ie]]);
                           ++ie;
                        }
                        fprintf(SUMA_STDERR,"\n");
                     }
                     ie = 0;
                     touchup[in] = 0.0;
                     while (  fvecind_under[ie] >= 0  /* valid values */
                              && touchup[in] < 0.001 
                              && Opt->travstp*(ie+1) < limtouch /* reach is within limit */
                              ) {
                        if (Opt->emask[fvecind_under[ie]] 
                              && SUMA_IS_ON_FLESH(undershish, ie)
                           ) {
                           touchup[in] = Opt->travstp*(ie);
                           if (Opt->debug > 2 && Opt->NodeDbg == in) { 
                                 fprintf(SUMA_STDERR, "%s: Edge candidate for %d, neighb %d at %f with edge %f (vox index %d)\n",
                                                         FuncName, in, inh, touchup[in], 
                                                         Opt->emask[fvecind_under[ie]], fvecind_under[ie]);
                           }                             
                        }
                        ++ie;
                     }

                  }
                  shft_list_under[inh] = touchup[in]; /* store under touchup result */           
            }
            /* Now sort the shft_list */
            if (Opt->NodeDbg == in) {   
               fprintf(SUMA_STDERR, "%s: Upper Shifts in neighborhood of node %d\n", FuncName, in);
               for (ie=0; ie<SO->FN->N_Neighb[in]+1; ++ie) {
                  fprintf(SUMA_STDERR, "%f \t", shft_list[ie]);
               }
               fprintf(SUMA_STDERR, "\n");
            }
            qsort(shft_list, SO->FN->N_Neighb[in], sizeof(float), (int(*) (const void *, const void *)) SUMA_compare_float);
            /* get smallest non zero jump */
            sup = 0.0;
            cnt = 0;
            while (sup < 0.001 && cnt < SO->FN->N_Neighb[in]) { sup = shft_list[cnt]; ++cnt; }
            if (Opt->NodeDbg == in) {   
               fprintf(SUMA_STDERR, "%s: Lower Shifts in neighborhood of node %d\n", FuncName, in);
               for (ie=0; ie<SO->FN->N_Neighb[in]+1; ++ie) {
                  fprintf(SUMA_STDERR, "%f \t", shft_list_under[ie]);
               }
               fprintf(SUMA_STDERR, "\n");
            }
            qsort(shft_list_under, SO->FN->N_Neighb[in], sizeof(float), (int(*) (const void *, const void *)) SUMA_compare_float);
            sdwn = shft_list_under[(int)(SO->FN->N_Neighb[in]/2)];
            
         } else {
            /* No fancy search, just along segment */
            a = &(SO->NodeList[3*in]); /* direction U will be same for all neighbors */
            b = &(Opt->Skull_Outer[3*in]);
            SUMA_UNIT_VEC(a, b , U, Un);  /* direction is along the line between the current location and the outer skull */
            SUMA_Find_IminImax( a, U, Opt, in,  MinMax, MinMax_dist, MinMax_over, MinMax_over_dist, Means,
                                undershish, overshish, fvecind_under, fvecind_over,  Opt->d1, Opt->d4, ShishMax); 

            { /* find next location based on edges */
               if (Opt->NodeDbg == in) { 
                        fprintf(SUMA_STDERR, "%s: emask over node %d, neighb %d\n", FuncName, in, inh);
                        ie = 0;
                        while (fvecind_over[ie] >= 0) {
                           fprintf(SUMA_STDERR,"%d\t %f\t\t", ie, Opt->emask[fvecind_over[ie]]);
                           ++ie;
                        }
                        fprintf(SUMA_STDERR,"\n");
               }
               ie = 0;
               touchup[in] = 0.0;
               while (fvecind_over[ie] >= 0 && touchup[in] < 0.001 && Opt->travstp*(ie+1) < limtouch) {
                  if (Opt->emask[fvecind_over[ie]] && overshish[ie]) {
                     touchup[in] = Opt->travstp*(ie);
                     if (Opt->Stop[in] < 0) { /* special case, 1st estimate */
                        if (touchup[in] <= 1.0) {
                           if (Opt->NodeDbg == in) { 
                              fprintf(SUMA_STDERR, "%s: Node %d's edge at %f is too close\n", FuncName, in, touchup[in]);
                           }
                           /* too close to an edge, need to initialize Opt->Stop, ignore */
                           touchup[in] = 0.0;
                        } else {
                           /* sum over ie should be larger than sum below ie */
                           cnt = 1;
                           sover = 0.0;
                           while (overshish[ie+cnt] >= 0  && cnt <= 3) {
                              sover += overshish[ie+cnt];
                              ++cnt;
                           }
                           if (cnt > 1) sover /= (float)(cnt-1);
                           cnt = 1;
                           sunder = 0.0;
                           while (undershish[ie+cnt] >= 0  && cnt <= 3) {
                              sunder += undershish[ie+cnt];
                              ++cnt;
                           }
                           if (cnt > 1) sunder /= (float)(cnt-1);
                           if (sover < sunder) {
                              if (Opt->NodeDbg == in) { 
                                 fprintf(SUMA_STDERR, "%s: Node %d's edge at %f is no good, sover = %f sunder = %f\n", FuncName, in, touchup[in], sover, sunder);
                              }
                              touchup[in] = 0.0;
                           }
                        }
                     }
                     if ( Opt->NodeDbg == in) { 
                           fprintf(SUMA_STDERR, "%s: Edge candidate for %d, at %f with edge %f (vox index %d)\n",
                                                   FuncName, in, touchup[in], 
                                                   Opt->emask[fvecind_over[ie]], fvecind_over[ie]);
                     }                             
                  }
                  ++ie;
               }

            }

            sup = touchup[in]; /* store upper touchup result */

            { /* Then find closest lower edge */
               ie = 0;
               touchup[in] = 0.0;
               while (  fvecind_under[ie] >= 0  /* valid values */
                        && touchup[in] < 0.001 
                        && Opt->travstp*(ie+1) < limtouch /* reach is within limit */
                        ) {
                  if (Opt->emask[fvecind_under[ie]] && undershish[ie]) {
                     touchup[in] = Opt->travstp*(ie);
                     if (Opt->debug > 2 && Opt->NodeDbg == in) { 
                           fprintf(SUMA_STDERR, "%s: Edge candidate for %d, at %f with edge %f (vox index %d)\n",
                                                   FuncName, in, touchup[in], 
                                                   Opt->emask[fvecind_under[ie]], fvecind_under[ie]);
                     }                             
                  }
                  ++ie;
               }

            }
            sdwn = touchup[in]; /* store under touchup result */           
         }
         
         if (Opt->Stop[in] >= 0.0) {
            /* Where would we be if we went up? */
            SUMA_POINT_AT_DISTANCE(U, a, sup, P2);
            Up[0] = P2[0][0]; Up[1] = P2[0][1]; Up[2] = P2[0][2];   
            /* Where would we be if we went down? */
            SUMA_POINT_AT_DISTANCE(U, a, -sdwn, P2);
            Dw[0] = P2[0][0]; Dw[1] = P2[0][1]; Dw[2] = P2[0][2];   
            /* which one gets us close to Opt->Stop ? */
            a = &(Opt->Brain_Hull[3*in]);
            SUMA_UNIT_VEC(a, Up, U, dup);
            SUMA_UNIT_VEC(a, Dw, U, dwn);
            if (Opt->NodeDbg == in) {   
               fprintf(SUMA_STDERR, "%s: Shift up takes us to %f mm from hull at %f %f %f\n", FuncName, dup, a[0], a[1], a[2]);
               fprintf(SUMA_STDERR, "%s: Shift down takes us to %f mm from hull\n", FuncName, dwn);
            }
            if (SUMA_ABS((Opt->Stop[in] - dup)) < SUMA_ABS((Opt->Stop[in] - dwn))) {
               /* better go up */
               touchup[in] = sup;   
            }  else {
               /* better go down */
               touchup[in] = -sdwn;   
            } 
         } else {
            /* should happen on first pass */
            new_stop = 1;
            /* get the median up*/
            touchup[in] = sup;
            Opt->Stop[in] = touchup[in];
            /* how far is the outer skull from the hull? */
            a = &(Opt->Brain_Hull[3*in]);
            b = &(Opt->Skull_Outer[3*in]);
            SUMA_UNIT_VEC(a, b, U, Un);
            if (Opt->NodeDbg == in) {   
               fprintf(SUMA_STDERR, "%s: Brain Hull to outerskull distance at node %d is %fmm\n", FuncName, in, Un);
            }
         }
         if (Opt->NodeDbg == in) {   
            fprintf(SUMA_STDERR, "%s: Median touchup value for node %d is %fmm, Opt->Stop = %f\n", FuncName, in, touchup[in], Opt->Stop[in]);
         }
         if (touchup[in]) ++N_troub;
      }
             
      /* if have just created touchup, blur it with a median filter */
      if (new_stop) {
         float *touchv = SUMA_malloc(SO->FN->N_Neighb_max * sizeof(float));
         float *far = SUMA_malloc(SO->N_Node * sizeof(float)); 
         for (in = 0;  in < SO->N_Node; ++in) {
            for (nn = 0; nn < SO->FN->N_Neighb[in]; ++nn) {
               touchv[nn] = Opt->Stop[SO->FN->FirstNeighb[in][nn]];
            }
            qsort(touchv, SO->FN->N_Neighb[in], sizeof(float), (int(*) (const void *, const void *)) SUMA_compare_float);
            far[in] = touchv[(int)(SO->FN->N_Neighb[in]/2)];
            if (Opt->NodeDbg == in) {   
               fprintf(SUMA_STDERR, "%s: Median interpolated Stop for node %d is %fmm\n", FuncName, in, far[in]);
            }
         }
         memcpy((void*)Opt->Stop, (void *)far, SO->N_Node*sizeof(float));
         SUMA_free(far); far = NULL;
         SUMA_free(touchv); touchv = NULL;
      }       
             
              
      
      if (overshish) SUMA_free(overshish); overshish = NULL; 
      if (gradovershish) SUMA_free(gradovershish); gradovershish = NULL; 
      if (undershish) SUMA_free(undershish); undershish = NULL; 
      if (fvecind_under) SUMA_free(fvecind_under); fvecind_under=NULL;
      if (fvecind_over) SUMA_free(fvecind_over); fvecind_over = NULL;
      if (shft_list) SUMA_free(shft_list); shft_list = NULL;
      if (shft_list_under) SUMA_free(shft_list_under); shft_list_under = NULL;
      if (xyz_list) SUMA_free(xyz_list); xyz_list = NULL;
      
   *N_touch = N_troub;
   
   SUMA_RETURN(touchup);

}

float *SUMA_Suggest_Touchup_PushEdge(SUMA_SurfaceObject *SO, 
                                    SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt, 
                                    float limtouch, SUMA_COMM_STRUCT *cs, int *N_touch)
{
   static char FuncName[]={"SUMA_Suggest_Touchup_PushEdge"};
   int in, N_troub = 0, cond1=0, cond2=0, cond3 = 0, cond4 = 0, cond5 = 0, nn, ShishMax, Down, Cross;   
   float MinMax_over[2], MinMax[2], MinMax_dist[2], MinMax_over_dist[2], Means[3], tb, GradThick; 
   float U[3], Un, *a, P2[2][3], *norm, shft, *b; 
   float *touchup=NULL, EdgeBelow=-1.0; 
   float *overshish=NULL, *undershish=NULL, *gradovershish=NULL;
   int *fvecind_under=NULL, *fvecind_over=NULL, ie;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!Opt->emask) {
      SUMA_SL_Err("No emask!!!\n");
      SUMA_RETURN(NOPE);
   }
      
      ShishMax = (int)(SUMA_MAX_PAIR(Opt->d1, Opt->d4) / Opt->travstp * 1.2);
      overshish = (float *)SUMA_malloc(sizeof(float)*ShishMax);
      undershish = (float *)SUMA_malloc(sizeof(float)*ShishMax);
      gradovershish = (float *)SUMA_malloc(sizeof(float)*ShishMax);
      fvecind_under = (int *)SUMA_malloc(sizeof(int)*ShishMax);
      fvecind_over = (int *)SUMA_malloc(sizeof(int)*ShishMax);
      touchup = (float *)SUMA_calloc(SO->N_Node, sizeof(float));   
      if (!touchup) { SUMA_SL_Crit("Failed to allocate"); SUMA_RETURN(NOPE); }  
      for (in=0; in<SO->N_Node; ++in) {   
         SUMA_Find_IminImax(&(SO->NodeList[3*in]), &(SO->NodeNormList[3*in]), Opt, in,  MinMax, MinMax_dist, MinMax_over, MinMax_over_dist, Means,
                            undershish, overshish, fvecind_under, fvecind_over,  Opt->d1, Opt->d4, ShishMax); 
         /* Shift the node outwards:   
            0- if node is not on an edge and there is an edge within Max(Opt->d1 and Opt->d4) and it is not air in between
         */ 
         
         cond1 = 0;
         /* how far down in the edge */
         ie = 0;
         EdgeBelow = -1.0;
         while (fvecind_under[ie] >= 0 && EdgeBelow < 0) {
            if (Opt->NodeDbg == in) { 
                     fprintf(SUMA_STDERR, "%s: Node %d \nUnder:Opt->emask[%d]=%f\nOver:Opt->emask[%d]=%f\n", 
                           FuncName, in, fvecind_under[ie], Opt->emask[fvecind_under[ie]], 
                           fvecind_over[ie], Opt->emask[fvecind_over[ie]]);
            }
            if (Opt->emask[fvecind_under[ie]]) {
               EdgeBelow = Opt->travstp*ie;
            }
            ++ie;
         }
         if (LocalHead || Opt->NodeDbg == in) { 
            fprintf(SUMA_STDERR, "%s: Node %d has an edge below it at %fmm (-1.0 means no edge in sight)!\n", FuncName, in, EdgeBelow);
         }
         if (  EdgeBelow > 1.0/THD_BN_rat() || 
               EdgeBelow  < 0) { /* node not on edge, proceed */
            /* find next edge above */
            ie = 0;
            while (!cond1 &&  fvecind_over[ie] >= 0 /* meaningful voxel index */
                          &&  SUMA_IS_ON_FLESH(overshish, ie) /* value on top not extremely small */
                              && overshish[ie] < 0.9*Opt->t98 /* value on top not too high */
                              && overshish[ie] <= 1.1/3.0*(undershish[0]+undershish[1]+undershish[3]) /* value on top not much higher than value at node */) {
               if (Opt->emask[fvecind_over[ie]]) {
                  touchup[in] = Opt->travstp*(ie+1);
                  if (Opt->NodeDbg == in) { 
                     fprintf(SUMA_STDERR, "%s: Node %d will be pushed to edge by a proposed %fmm!\n", FuncName, in, touchup[in]);
                  }
                  ++N_troub;
                  cond1 = 1;    
               }
               ++ie;
            }
         }
         if (Opt->NodeDbg == in) {   
               a = &(SO->NodeList[3*in]);   
               fprintf(SUMA_STDERR, "%s: Debug during touchup for node %d:\n"
                                    " emask pointer: %p\n"   
                                    " MinMax     =[%.1f,%.1f] MinMax_dist     = [%.2f,%.2f] \n"   
                                    " MinMax_over=[%.1f,%.1f] MinMax_over_dist= [%.2f,%.2f] \n"   
                                    " Val at node %.1f, mean below %.1f, mean above %.1f\n"  
                                    " zalt= %f, r/2 = %f\n"    
                                    " Conditions: %d \n",    
                       FuncName, in, Opt->emask,
                       MinMax[0], MinMax[1], MinMax_dist[0], MinMax_dist[1], 
                       MinMax_over[0], MinMax_over[1], MinMax_over_dist[0], MinMax_over_dist[1], 
                       Means[0], Means[1], Means[2],   
                       a[2] - SO->Center[2], Opt->r/2, 
                       cond1 ); 
         }  
         
         if (cond1) {   
               a = &(SO->NodeList[3*in]);   
                  /* reposition nodes */  
                  if (Opt->NodeDbg == in) { fprintf(SUMA_STDERR, "%s: Suggest repair for node %d (to edge above):\n"   
                                       " MinMax     =[%.1f,%.1f] MinMax_dist     = [%.2f,%.2f] \n"   
                                       " MinMax_over=[%.1f,%.1f] MinMax_over_dist= [%.2f,%.2f] \n"   
                                       " Val at node %.1f, mean below %.1f, mean above %.1f\n"  
                                       " zalt= %f, r/2 = %f, Opt->shrink_bias = %f\n",    
                          FuncName, in, 
                          MinMax[0], MinMax[1], MinMax_dist[0], MinMax_dist[1], 
                          MinMax_over[0], MinMax_over[1], MinMax_over_dist[0], MinMax_over_dist[1], 
                          Means[0], Means[1], Means[2],   
                          a[2] - SO->Center[2], Opt->r/2, Opt->shrink_bias[in]); } 
          
         }  
      }  
      
      if (overshish) SUMA_free(overshish); overshish = NULL; 
      if (gradovershish) SUMA_free(gradovershish); gradovershish = NULL; 
      if (undershish) SUMA_free(undershish); undershish = NULL; 
      if (fvecind_under) SUMA_free(fvecind_under); fvecind_under=NULL;
      if (fvecind_over) SUMA_free(fvecind_over); fvecind_over = NULL;

   *N_touch = N_troub;
   
   SUMA_RETURN(touchup);

}

/*!
   Push nodes of surface to the surface's convex hull
*/
void *SUMA_Push_Nodes_To_Hull(SUMA_SurfaceObject *SO, SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt, SUMA_COMM_STRUCT *cs, int N_itermax)
{
   static char FuncName[]={"SUMA_Push_Nodes_To_Hull"};
   float U[3], Un, *a, P2[2][3]={ {0.0, 0.0, 0.0}, {0.0, 0.0, 0.0} }, *norm, shft, *b; 
   int *ijk=NULL, nf = 0, N_iter = 0, N_troub, in, N_smooth, inh;
   float *dsmooth=NULL, *refNodeList=NULL, *dirvec = NULL, *diropt=NULL, iscale;
   byte *nmask = NULL;
   SUMA_Boolean exists; 
   void *SO_name_hull;
   
   SUMA_M2M_STRUCT *M2M = NULL;
   SUMA_SurfaceObject *SOhull = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (! (nf = SUMA_qhull_wrap(SO->N_Node, SO->NodeList, &ijk, 1)) ) {
         fprintf(SUMA_STDERR,"Error %s:\nFailed in SUMA_qhull_wrap\n", FuncName);
         SOhull = NULL;
         SUMA_RETURN(NULL);
   } else {
         if (LocalHead) fprintf(SUMA_STDERR,"%s:\nForming hull.\n", FuncName);
         if (!(SOhull = SUMA_Patch2Surf(SO->NodeList, SO->N_Node, ijk, nf, 3))) {
            SUMA_SL_Err("Failed to create hull");
            SUMA_RETURN(NULL);
         }
         SUMA_RECOMPUTE_NORMALS((SOhull));
         if (!SUMA_SurfaceMetrics_eng(SOhull, "EdgeList,MemberFace,PolyArea", NULL, SUMA_MAX_PAIR(0, Opt->debug -1), SUMAg_CF->DsetList)) {
            fprintf(SUMA_STDERR,"Error %s: Error in SUMA_SurfaceMetrics\n", FuncName);
            SUMA_Free_Surface_Object (SOhull); /* that takes care of freeing leftovers in MF */
            SOhull = NULL;
            SUMA_RETURN(NULL);
         }
      #if 0 
      fprintf (SUMA_STDERR,"%s: Writing hull surface  ...\n", FuncName);
      SO_name_hull = SUMA_Prefix2SurfaceName(
                     "hull", NULL, NULL, SUMA_PLY, &exists);
      if (!SUMA_Save_Surface_Object (SO_name_hull, SOhull, 
                                     Opt->SurfFileType, Opt->SurfFileFormat,
                                     NULL)) {
         fprintf (SUMA_STDERR,"Error %s: Failed to write surface object.\n", FuncName);
      }
      #endif
   }

   /* need idcodes for SO and SOhull */
   if (!SO->idcode_str) SUMA_NEW_ID(SO->idcode_str, "Bonjour");
   if (!SOhull->idcode_str) SUMA_NEW_ID(SOhull->idcode_str, "Edward");
   
   dirvec = (float *) SUMA_malloc(sizeof(float)*3*SO->N_Node);
   if (!dirvec) {
      SUMA_SL_Crit("Failed to allocate");
      SUMA_RETURN(NULL);
   }
   
   {
      N_iter = 0;
      N_smooth = 20;
      do {
         N_troub = 0;
         iscale = (float)(N_itermax - N_iter)/(float)N_itermax;
         #if 0
         /* filter */
         N_smooth = (int)( (10 + 20 * iscale ) / 2.0 ) * 2;
         fprintf(SUMA_STDERR,
                  "%s:  Now smoothing with %d iterations \n", 
                  FuncName, N_smooth);
         if (N_smooth > 0) {
            dsmooth = SUMA_NN_GeomSmooth( SO, N_smooth , SO->NodeList, 
                                          3, SUMA_ROW_MAJOR, dsmooth, 
                                          cs, nmask);    \
         }
         SUMA_RECOMPUTE_NORMALS(SO);
         #else
         if (N_iter < N_itermax / 2) {
            if (!Opt->match_area) { 
               SUMA_WRAP_BRAIN_SMOOTH_NN(6, dsmooth, refNodeList, nmask);  
            } else { 
               SUMA_WRAP_BRAIN_SMOOTH_MATCHAREA( 6, dsmooth, 
                                                refNodeList, nmask); 
            }
         } else if (N_iter < N_itermax) {
            /* civilized smoothing */
            dsmooth = SUMA_Taubin_Smooth( SO, NULL,
                                    0.6307, -.6732, SO->NodeList,
                                    Opt->smooth_end, 3, SUMA_ROW_MAJOR, 
                                    dsmooth, cs, NULL, 0);    
            memcpy(  (void*)SO->NodeList, (void *)dsmooth, 
                     SO->N_Node * 3 * sizeof(float));
            SUMA_RECOMPUTE_NORMALS(SO);
         } 
         #endif
         if (0 && N_iter > N_itermax / 2) {
            /* find distance of each node to hull, 
               go along direction from center instead of normals*/
            for (in=0; in < SO->N_Node; ++in) {
               a = &(SO->NodeList[3*in]);   
               b = &(dirvec[3*in  ]);
               SUMA_UNIT_VEC(SO->Center, a, b, Un);
            }
            diropt = dirvec;
         } else {
            diropt = SO->NodeNormList; /* initially, use normals, 
                                          because some nodes may be behind
                                          the center, ie. normal dot
                                          (center-->node) is negative */
         }
         M2M = SUMA_GetM2M_NN(SO, SOhull, NULL, 
                              SO->N_Node, diropt, 100.0, Opt->NodeDbg);
         /* move each node by fraction of distance along dirvec */
         for (in=0; in < SO->N_Node; ++in) {
            shft = M2M->PD[in];
            if (SUMA_ABS(shft) > 0.1){
               a = &(SO->NodeList[3*in]);   
               norm = &(diropt[3*in]);  
               SUMA_POINT_AT_DISTANCE(norm, a, SUMA_MIN_PAIR(shft, 4), P2);
               SO->NodeList[3*in] = P2[0][0]; 
               SO->NodeList[3*in+1] = P2[0][1]; 
               SO->NodeList[3*in+2] = P2[0][2];   
               ++N_troub;  
               if (LocalHead || Opt->NodeDbg == in) 
                  fprintf(SUMA_STDERR,
                     "%s: Acting on node %d, boosting by %f, "
                     "original request of %fmm\n", 
                        FuncName, in, shft, shft);  
            }
         }
         /*free M2M */
         M2M = SUMA_FreeM2M(M2M);
         ++N_iter;
         if (cs->Send) {
            if (!SUMA_SendToSuma (  SO, cs, (void *)SO->NodeList, 
                                    SUMA_NODE_XYZ, 1)) {
               SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
            }
         } 
         if (LocalHead) { 
            fprintf(SUMA_STDERR,
            "%s: Iteration %d, N_troub = %d                              \n", 
                  FuncName, N_iter, N_troub);
         }  
      } while (N_troub && N_iter < N_itermax);
   }
   
   
   if (SOhull) SUMA_Free_Surface_Object (SOhull); SOhull = NULL;
   if (dsmooth) SUMA_free(dsmooth); dsmooth = NULL;
   if (refNodeList) SUMA_free(refNodeList); refNodeList = NULL;
   if (nmask) SUMA_free(nmask); nmask = NULL;
   if (dirvec) SUMA_free(dirvec); dirvec = NULL;
   SUMA_RETURN(NULL);
}

/*!
   \brief A function to move node surfaces outwards to the edge.
   Be conservative, DO NO HARM!
   
*/
int SUMA_PushToEdge(SUMA_SurfaceObject *SO, SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt, float limtouch, SUMA_COMM_STRUCT *cs, int agressive) 
{
   static char FuncName[]={"SUMA_PushToEdge"};
   byte *fmask=NULL;
   int in, N_troub = 0, cond1=0, cond2=0, cond3 = 0, cond4 = 0, nn, nin;   
   float MinMax_over[2], MinMax[2], MinMax_dist[2], MinMax_over_dist[2], Means[3], tb; 
   float U[3], Un, *a, P2[2][3]={ {0.0, 0.0, 0.0}, {0.0, 0.0, 0.0} }, *norm, shft, *b; 
   float *touchup=NULL, **wgt=NULL, *dsmooth=NULL, *overshish=NULL, 
         *undershish=NULL, *gradovershish=NULL; 
   int   nstp, stillmoving, kth_buf, N_Touch, ie, ShishMax = 0,
         *fvecind_under=NULL, *fvecind_over=NULL;
   float stp, *csmooth=NULL, *shftvec=NULL, *targetloc=NULL, EdgeBelow;
   SUMA_Boolean Send_buf;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY; 
   
   if (Opt->debug > 2) LocalHead = YUP;
   
      touchup = SUMA_Suggest_Touchup_PushEdge(SO, Opt, limtouch, cs, &N_troub);

      if (!N_troub) {
         SUMA_LH("Nothing to do, no trouble nodes.");
         SUMA_RETURN(N_troub);
      }
      
      if (LocalHead) fprintf (SUMA_STDERR,"%s: ********************* %d troubled nodes found\n", FuncName, N_troub); 
      /* add the changes */   
      for (in=0; in<SO->N_Node; ++in) {   
         a = &(SO->NodeList[3*in]);   
         if (Opt->NodeDbg == in || LocalHead) { 
               fprintf(SUMA_STDERR,"%s: Opt->Stop[in] = %f, Touchup for node %d is %f\n", 
                  FuncName, Opt->Stop[in], in, touchup[in]); 
         } 
         if (1 || Opt->Stop[in] >= 0) { /* no need to heed the freezd */
            if (1 || !SUMA_IS_EYE_ZONE(a,SO->Center)) { 
               if (agressive)  { shft = touchup[in]; }/* if not aggressive, force smoothing */ 
               else { /* use the mean of the immediate neighbors, no high freq. shifts expected down low */
                  shft = touchup[in];
                  if (Opt->NodeDbg == in) {
                     fprintf(SUMA_STDERR,"%s: Node %d is in lower zone, shift = %f. Neighbors: [ ", FuncName, in, shft);
                  }
                  for (nin=0; nin<SO->FN->N_Neighb[in]; ++nin) {
                     shft += touchup[SO->FN->FirstNeighb[in][nin]];
                     if (Opt->NodeDbg == in) { fprintf(SUMA_STDERR,"   (%f)", touchup[SO->FN->FirstNeighb[in][nin]]); }
                  }
                  if (shft < 1.3*SO->FN->N_Neighb[in]*touchup[in]) {
                     if (Opt->NodeDbg == in) { fprintf(SUMA_STDERR,"\nonly few nodes wants to move in this hood. No sir, Shft = 0\n");}
                     shft = 0;
                  } else {
                     shft /= (SO->FN->N_Neighb[in]+1);  
                     if (Opt->NodeDbg == in) {
                        fprintf(SUMA_STDERR,"]. Smoothed to = %f\n", shft);
                     } 
                  }
               }
               if (shft) { 
                  a = &(SO->NodeList[3*in]);   
                  norm = &(SO->NodeNormList[3*in]);  
                  SUMA_POINT_AT_DISTANCE(norm, a, SUMA_MIN_PAIR(shft, limtouch), P2);   
                  SO->NodeList[3*in] = P2[0][0]; SO->NodeList[3*in+1] = P2[0][1]; SO->NodeList[3*in+2] = P2[0][2];   
                  if (LocalHead) fprintf(SUMA_STDERR,"%s: Acting on node %d, boosting by %f, original request of %fmm\n", 
                           FuncName, in, SUMA_MIN_PAIR(shft, limtouch), shft);   
               }
            }
         } else {
            if (in == Opt->NodeDbg || LocalHead) fprintf(SUMA_STDERR,"%s:\nNode %d has been frozen before, no cigar.\n", FuncName, in);
         }
      }  

      if (fmask) SUMA_free(fmask); fmask = NULL; 
      if (shftvec) SUMA_free(shftvec); shftvec = NULL;
      if (targetloc) SUMA_free(targetloc); targetloc = NULL;
      if (touchup == dsmooth) dsmooth = NULL;
      if (touchup) SUMA_free(touchup); touchup = NULL;   
      if (dsmooth) SUMA_free(dsmooth); dsmooth = NULL;   
      if (csmooth) SUMA_free(csmooth); csmooth = NULL;   
      if (wgt) SUMA_free2D ((char **)wgt, SO->N_Node); wgt = NULL;   
   
   SUMA_RETURN(N_troub);
   

}
/*!
   \brief A function to move node surfaces outwards to the edge.
   Be conservative, DO NO HARM!
   
*/
int SUMA_PushToOuterSkull( SUMA_SurfaceObject *SO,
                           SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt, 
                           float limtouch, SUMA_COMM_STRUCT *cs, 
                           int N_itermax) 
{
   static char FuncName[]={"SUMA_PushToOuterSkull"};
   byte *fmask=NULL;
   static int niter = 0;
   int in, N_troub = 0, cond1=0, cond2=0, cond3 = 0, cond4 = 0, nn, nin;   
   float MinMax_over[2], MinMax[2], MinMax_dist[2], 
         MinMax_over_dist[2], Means[3], tb; 
   float U[3], Un, *a, P2[2][3]={ {0.0, 0.0, 0.0}, {0.0, 0.0, 0.0} }, 
         shft, *b, dot; 
   float *touchup=NULL, **wgt=NULL, *dsmooth=NULL, *refNodeList=NULL,
         *overshish=NULL, 
         *undershish=NULL, *gradovershish=NULL; 
   int   nstp, stillmoving, kth_buf, N_Touch, ie, ShishMax = 0, ine,
         *fvecind_under=NULL, *fvecind_over=NULL;
   float stp, *csmooth=NULL, *shftvec=NULL, *targetloc=NULL, EdgeBelow,
         move_frac;
   SUMA_Boolean Send_buf;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY; 
   
   if (niter < N_itermax/2)
      move_frac = 0.3;
   else if (niter < N_itermax - 5)
      move_frac = 0.8;
   else move_frac = 1.0;
   
   if (Opt->debug > 2) LocalHead = YUP;
      
   {   
      /* make sure no nodes are 'behind the center' 
         a more elegant way would be to start with a surface
         that encloses the convex hull of the outer brain*/
      for (in=0; in<SO->N_Node; ++in) {   
         a = &(SO->NodeList[3*in]);
         /* original distance from center */
         SUMA_UNIT_VEC(SO->Center, a , U, Opt->Stop[in]); 
         b = &(SO->NodeNormList[3*in]);
         /* dot product with normal at node*/
         SUMA_DOTP_VEC(U, b, dot, 3, float, float); 
         if (dot < 0) {
            if (Opt->NodeDbg == in || LocalHead) { 
               fprintf(SUMA_STDERR,"%s: negative U for node %d\n", 
                  FuncName, in); 
            } 
            /* push node out along negative U direction by half of limtouch */
            U[0] = -U[0]; U[1] = -U[1]; U[2] = -U[2];
            SUMA_POINT_AT_DISTANCE(U, a, move_frac*limtouch, P2);
            SO->NodeList[3*in] = P2[0][0]; 
            SO->NodeList[3*in+1] = P2[0][1]; 
            SO->NodeList[3*in+2] = P2[0][2];    
         }
      }
   }
      
      
      if (niter < N_itermax - 5) {
         /* smooth the surface a little, then apply shift */
         if (!Opt->match_area) {
            SUMA_WRAP_BRAIN_SMOOTH_NN(4, dsmooth, 
                                          refNodeList, Opt->nmask);
         } else {
            SUMA_WRAP_BRAIN_SMOOTH_MATCHAREA( 4, dsmooth, 
                                                refNodeList, Opt->nmask);
         }
         #if 0
         SUMA_S_Note("Not used? ");
         /* How much did this node loose in filtering ?*/
         for (in=0; in<SO->N_Node; ++in) {   
            a = &(refNodeList[3*in]);
            SUMA_UNIT_VEC(SO->Center, a , 
                        U, Opt->Stop[in]); /* original distance from center */
         }
         #endif
      }
      
      touchup = SUMA_Suggest_Touchup_PushOuterSkull(SO, Opt, 
                                                   limtouch, cs, &N_troub, 1);

      if (!N_troub) {
         SUMA_LH("Nothing to do, no trouble nodes.");
         SUMA_RETURN(N_troub);
      }
      if (LocalHead) fprintf (SUMA_STDERR,
                     "%s: ********************* %d candidate nodes found\n",
                      FuncName, N_troub); 
      /* add the changes */   
      for (in=0; in<SO->N_Node; ++in) {   
         if (Opt->NodeDbg == in || LocalHead) { 
               fprintf(SUMA_STDERR,"%s:  Touchup for node %d is %f\n", 
                  FuncName, in, touchup[in]); 
         } 
         { 
            { 
               shft = touchup[in]; /* move all the nodes that need touchup */
               if (shft) { 
                  a = &(SO->NodeList[3*in]);   
                  SUMA_UNIT_VEC(SO->Center, a , U, Un);/*Un is the current
                                                         distance from center*/
                  /* if the direction is opposite the normal, flip it */
                  b = &(SO->NodeNormList[3*in]);
                  SUMA_DOTP_VEC(U, b, dot, 3, float, float);
                  if (dot < 0) {
                     U[0] = -U[0]; U[1] = -U[1]; U[2] = -U[2];
                  }
                  SUMA_POINT_AT_DISTANCE(
                        U, a, move_frac*(SUMA_MIN_PAIR(shft, limtouch)), P2);   
                  SO->NodeList[3*in] = P2[0][0]; 
                  SO->NodeList[3*in+1] = P2[0][1]; 
                  SO->NodeList[3*in+2] = P2[0][2];   
                  if (LocalHead) 
                     fprintf(SUMA_STDERR,
                              "%s: Acting on node %d, boosting by %f,"
                              " original request of %fmm\n", 
                              FuncName, in, 
                              SUMA_MIN_PAIR(shft, limtouch), shft);   
               } else { /* node is to be stopped, if we have iterated enough */
                  if (niter > 1) Opt->nmask[in] = 0; 
               }
            }
         }
      }  
      
      ++niter;
      
      if (dsmooth) SUMA_free(dsmooth); dsmooth = NULL;
      if (refNodeList) SUMA_free(refNodeList); refNodeList = NULL;
      if (fmask) SUMA_free(fmask); fmask = NULL; 
      if (shftvec) SUMA_free(shftvec); shftvec = NULL;
      if (targetloc) SUMA_free(targetloc); targetloc = NULL;
      if (touchup == dsmooth) dsmooth = NULL;
      if (touchup) SUMA_free(touchup); touchup = NULL;   
      if (dsmooth) SUMA_free(dsmooth); dsmooth = NULL;   
      if (csmooth) SUMA_free(csmooth); csmooth = NULL;   
      if (wgt) SUMA_free2D ((char **)wgt, SO->N_Node); wgt = NULL;   
   
   SUMA_RETURN(N_troub);
   

}

/*!
   \brief A function to move node surfaces outwards to the edge.
   Be conservative, DO NO HARM!
   
*/
int SUMA_PushToInnerSkull(SUMA_SurfaceObject *SO, SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt, float limtouch, SUMA_COMM_STRUCT *cs) 
{
   static char FuncName[]={"SUMA_PushToInnerSkull"};
   byte *fmask=NULL;
   static int niter = 0;
   int in, N_troub = 0, cond1=0, cond2=0, cond3 = 0, cond4 = 0, nn, nin;   
   float MinMax_over[2], MinMax[2], MinMax_dist[2], MinMax_over_dist[2], Means[3], tb; 
   float U[3], Un, *a, P2[2][3]={ {0.0, 0.0, 0.0}, {0.0, 0.0, 0.0} }, shft, *b, dot; 
   float *touchup=NULL, **wgt=NULL, *dsmooth=NULL, *refNodeList=NULL, *overshish=NULL, 
         *undershish=NULL, *gradovershish=NULL, min_shft; 
   int   nstp, stillmoving, kth_buf, N_Touch, ie, ShishMax = 0, ine,
         *fvecind_under=NULL, *fvecind_over=NULL;
   float stp, *csmooth=NULL, *shftvec=NULL, *targetloc=NULL, EdgeBelow, move_frac;
   SUMA_Boolean Send_buf;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY; 
   
   if (!Opt->Brain_Hull || !Opt->Skull_Outer) {
      SUMA_S_Err("Need Brain Hull and Skull_Outer");
      SUMA_RETURN(-1);
   }
   
   move_frac = 1.0;
   if (Opt->debug > 2) LocalHead = YUP;
      
   if (!niter) {
      SUMA_LH("Starting from Brain Hull");
      memcpy((void *)SO->NodeList, (void*)Opt->Brain_Hull,  SO->N_Node * 3 * sizeof(float)); 
      /* Resetta the new Opt->Stop values */
      for (in=0; in<SO->N_Node; ++in) {
         Opt->Stop[in] = -1.0;
      }     
   } else {
     limtouch = 5.0; /* don't go far after first pass */
   }
            
   touchup = SUMA_Suggest_Touchup_PushInnerSkull(SO, Opt, limtouch, cs, &N_troub);

      if (!N_troub) {
         SUMA_LH("Nothing to do, no trouble nodes.");
         SUMA_RETURN(N_troub);
      }
      if (LocalHead) fprintf (SUMA_STDERR,"%s: ********************* %d candidate nodes found\n", FuncName, N_troub); 
      /* add the changes */   
      for (in=0; in<SO->N_Node; ++in) {   
         a = &(SO->NodeList[3*in]); 
         b = &(Opt->Skull_Outer[3*in]);  
         SUMA_UNIT_VEC(a, b , U, min_shft); /* Direction from hull to outer skull */
         if (Opt->NodeDbg == in || LocalHead) { 
               fprintf(SUMA_STDERR,"%s: min_shft = %f, Touchup for node %d is %f\n", 
                  FuncName, min_shft, in, touchup[in]); 
         } 
         { 
            /* smooth the shifts */
            shft = touchup[in];
            { 
               if (shft) { 
                  a = &(SO->NodeList[3*in]);
                  b = &(Opt->Skull_Outer[3*in]);     
                  SUMA_UNIT_VEC(a , b, U, Un); /* Un is the current distance from the hull to the outer skull */
                  if (0 && Un < min_shft) min_shft = min_shft - Un; /* This now contains a minimum boost */
                  else min_shft = 0.0;
                  
                  SUMA_POINT_AT_DISTANCE(U, a, min_shft + move_frac*(SUMA_MIN_PAIR(shft, limtouch)), P2);   
                  SO->NodeList[3*in] = P2[0][0]; SO->NodeList[3*in+1] = P2[0][1]; SO->NodeList[3*in+2] = P2[0][2];   
                  if (LocalHead) fprintf(SUMA_STDERR,"%s: Acting on node %d, boosting by %f, original request of %fmm\n", 
                           FuncName, in, SUMA_MIN_PAIR(shft, limtouch), shft);   
               } else { /* node is to be stopped, if we have iterated enough */
                  if (niter > 1) Opt->nmask[in] = 0; 
               }
            }
         }
      }  
      /* push the surface to its hull */
      if (!niter) {
         if (Opt->debug) SUMA_S_Note("Pushing inner skull surface to its hull");
         SUMA_Push_Nodes_To_Hull(SO, Opt, cs, 8);
      }
      
      ++niter;
      
      if (dsmooth) SUMA_free(dsmooth); dsmooth = NULL;
      if (refNodeList) SUMA_free(refNodeList); refNodeList = NULL;
      if (fmask) SUMA_free(fmask); fmask = NULL; 
      if (shftvec) SUMA_free(shftvec); shftvec = NULL;
      if (targetloc) SUMA_free(targetloc); targetloc = NULL;
      if (touchup == dsmooth) dsmooth = NULL;
      if (touchup) SUMA_free(touchup); touchup = NULL;   
      if (dsmooth) SUMA_free(dsmooth); dsmooth = NULL;   
      if (csmooth) SUMA_free(csmooth); csmooth = NULL;   
      if (wgt) SUMA_free2D ((char **)wgt, SO->N_Node); wgt = NULL;   
   
   SUMA_RETURN(0);
   

}

/*!
   \brief A function to move node surfaces towards a better future.
   
   This function used to be a macro gone too large. 
*/
int SUMA_Reposition_Touchup(SUMA_SurfaceObject *SO, SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt, float limtouch, SUMA_COMM_STRUCT *cs) 
{
   static char FuncName[]={"SUMA_Reposition_Touchup"};
   byte *fmask=NULL;
   int in, N_troub = 0, cond1=0, cond2=0, cond3 = 0, cond4 = 0, nn, nin;   
   float MinMax_over[2], MinMax[2], MinMax_dist[2], MinMax_over_dist[2], Means[3], tb; 
   float U[3], Un, *a, P2[2][3]={ {0.0, 0.0, 0.0}, {0.0, 0.0, 0.0} }, *norm, shft, *b; 
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
         that kind of smoothing is better performed on an outer model of the skull .... Someday ....*/
         if (LocalHead) fprintf (SUMA_STDERR,"%s: ********************* Now filtering...\n", FuncName); 
         wgt = SUMA_Chung_Smooth_Weights(SO);   
         if (!wgt) { 
            SUMA_SL_Err("Failed to compute weights.\n"); 
            exit(1); 
         }  
         dsmooth = SUMA_Chung_Smooth (SO, wgt, 200, 20, touchup, 1, SUMA_COLUMN_MAJOR, NULL, cs, NULL, 1);   
         if (LocalHead) fprintf (SUMA_STDERR,"%s: ********************* filtering done.\n", FuncName);   
      } else { 
         dsmooth = touchup; /* carefull at the free business */
      }  
      /* add the changes */   
      #if 1 /* sudden jump, Fast and furious but that causes intersections */ 
      for (in=0; in<SO->N_Node; ++in) {   
         a = &(SO->NodeList[3*in]);   
         if (Opt->NodeDbg == in) { 
               fprintf(SUMA_STDERR,"%s: Opt->Stop[in] = %f, Touchup for node %d is %f\n", 
                  FuncName, Opt->Stop[in], in, touchup[in]); 
         } 
         if (Opt->Stop[in] >= 0) {
            if (1 || !SUMA_IS_EYE_ZONE(a,SO->Center)) { 
               if (!(SUMA_IS_LOWER_ZONE(a, SO->Center )))  { shft = touchup[in]; }/* no smooth baby for top where bumpy sulci can occur */ 
               else { /* use the mean of the immediate neighbors, no high freq. shifts expected down low */
                  shft = touchup[in];
                  if (Opt->NodeDbg == in) {
                     fprintf(SUMA_STDERR,"%s: Node %d is in lower zone, shift = %f. Neighbors: [ ", FuncName, in, shft);
                  }
                  for (nin=0; nin<SO->FN->N_Neighb[in]; ++nin) {
                     shft += touchup[SO->FN->FirstNeighb[in][nin]];
                     if (Opt->NodeDbg == in) { fprintf(SUMA_STDERR,"   (%f)", touchup[SO->FN->FirstNeighb[in][nin]]); }
                  }
                  if (shft == touchup[in]) {
                     if (Opt->NodeDbg == in) { fprintf(SUMA_STDERR,"\nonly one node wants to move in this hood. No sir, Shft = 0\n");}
                     shft = 0;
                  } else {
                     shft /= (SO->FN->N_Neighb[in]+1);  
                     if (Opt->NodeDbg == in) {
                        fprintf(SUMA_STDERR,"]. Smoothed to = %f\n", shft);
                     } 
                  }
               }
               if (shft) { 
                  a = &(SO->NodeList[3*in]);   
                  norm = &(SO->NodeNormList[3*in]);  
                  SUMA_POINT_AT_DISTANCE(norm, a, SUMA_MIN_PAIR(shft, limtouch), P2);   
                  SO->NodeList[3*in] = P2[0][0]; SO->NodeList[3*in+1] = P2[0][1]; SO->NodeList[3*in+2] = P2[0][2];   
                  if (LocalHead) fprintf(SUMA_STDERR,"%s: Acting on node %d, boosting by %f, original request of %fmm\n", 
                           FuncName, in, SUMA_MIN_PAIR(shft, limtouch), shft);   
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
                                          20, 3, SUMA_ROW_MAJOR, csmooth, cs, fmask, 0);    
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

/*!
   \brief Creates an empty AFNI EDIT_options structure.
    All fields are initialized to do nothing.
    The fields initialized are based on those used in EDIT_one_dataset in edt_onedset.c
   Free returned pointer with SUMA_free
*/
EDIT_options *SUMA_BlankAfniEditOptions(void)
{
   static char FuncName[]={"SUMA_BlankAfniEditOptions"};
   EDIT_options *edopt = NULL;

   SUMA_ENTRY;

   edopt = (EDIT_options *)SUMA_calloc(1, sizeof(EDIT_options));
   /* set all fields to do nothing */
   edopt->thtoin = 0; 
   edopt->noneg = 0; 
   edopt->abss = 0; 
   edopt->clip_bot = 0; 
   edopt->clip_top = 0; 
   edopt->thresh = 0.0; 
   edopt->edit_clust = ECFLAG_SAME - 1;
   edopt->clust_rmm = 0.0;
	edopt->clust_vmul = 0.0;
   edopt->erode_pv  = 0.0;
   edopt->dilate = 0;
   edopt->filter_opt = FCFLAG_NONE;
   edopt->filter_rmm = 0.0;
   edopt->thrfilter_opt = FCFLAG_NONE;
   edopt->thrfilter_rmm = 0.0;
   edopt->blur = 0.0; 
   edopt->thrblur = 0.0; 
   edopt->scale = 0.0; 
   edopt->mult = 0.0; 
   edopt->do_zvol = 0; 
   edopt->iv_fim = -1;
   edopt->iv_thr = -1;
   edopt->verbose = 0;
   edopt->fake_dxyz = 0;
   edopt->clip_unscaled = 0; 

   SUMA_RETURN(edopt);
}  

/*!
   \brief Create an edge dataset
   \param inset (THD_3dim_dataset *) input dataset
   \param emask (float *) a preallocated vector as big as DSET_NX(inset)*DSET_NY(inset)*DSET_NZ(inset). 
                           This vector will contain the edge values
   \param poutsetp (THD_3dim_dataset **)If not null, it will point to a dataset that contains
                                        the edges.
*/
SUMA_Boolean SUMA_3dedge3(THD_3dim_dataset *inset, float *emask, THD_3dim_dataset **poutsetp)
{
   static char FuncName[]={"SUMA_3dedge3"};
   THD_3dim_dataset *outset;
   int verb = 1, datum = -1;
   int border[3]={0,0,0};
   int indims[3]={0,0,0};
   float filterCoefs[3] = {1.0, 1.0, 1.0}, **sum = NULL;
   int ii, nx, ny, nz, nval, kk;
   int nxyz = DSET_NX(inset)*DSET_NY(inset)*DSET_NZ(inset);
   int fscale=0 , gscale=0 , nscale=0 ;
   
   recursiveFilterType filterType = ALPHA_DERICHE;  /* BAD BOY ZIAD */
   
   SUMA_ENTRY;

   /*-- Edge detect  --*/
   indims[0] = DSET_NX(inset);
   indims[1] = DSET_NY(inset);
   indims[2] = DSET_NZ(inset);
   border[0] = 50;
   border[1] = 50;
   border[2] = 50;

   switch( DSET_BRICK_TYPE(inset,0) ){
     default:
        fprintf(stderr,"ERROR: illegal input sub-brick datum\n") ;
        SUMA_RETURN(0) ;

     case MRI_float:{
       float *pp = (float *) DSET_ARRAY(inset,0) ;
       float fac = DSET_BRICK_FACTOR(inset,0)  ;

       if( fac ) {
         for( ii=0 ; ii < nxyz ; ii++ ) { pp[ii] *= fac; }
       }
       if ( Extract_Gradient_Maxima_3D( (void *)pp, FLOAT,
				         emask, FLOAT,
				         indims,
				         border,
				         filterCoefs,
				         filterType ) == 0 ) {
          fprintf( stderr, "ERROR: gradient extraction failed.\n" );
          SUMA_RETURN(0);
        }

     }
     break ;

      case MRI_short:{
         short *pp = (short *) DSET_ARRAY(inset,0) ;
         float fac = DSET_BRICK_FACTOR(inset,0)  ;

         if( fac ) {
            for( ii=0 ; ii < nxyz ; ii++ ) { pp[ii] *= fac; }
         }
         if ( Extract_Gradient_Maxima_3D( (void *)pp, USHORT,
				         emask, FLOAT,
				         indims,
				         border,
				         filterCoefs,
				         filterType ) == 0 ) {
          fprintf( SUMA_STDERR, "ERROR: gradient extraction failed.\n" );
          SUMA_RETURN(0);
         }
      }
      break ;

      case MRI_byte:{
         byte *pp = (byte *) DSET_ARRAY(inset,0) ;
         float fac = DSET_BRICK_FACTOR(inset,0)  ;

         if( fac ) {
            for( ii=0 ; ii < nxyz ; ii++ ) { pp[ii] *= fac; }
         }
         if ( Extract_Gradient_Maxima_3D( (void *)pp, UCHAR,
				         emask, FLOAT,
				         indims,
				         border,
				         filterCoefs,
				         filterType ) == 0 ) {
          fprintf(SUMA_STDERR , "ERROR: gradient extraction failed.\n" );
          SUMA_RETURN(0);
         }
      }
      break ;
   }      

   if (poutsetp) {
      /* user wants an output dset */
      nx   = DSET_NX(inset) ;
      ny   = DSET_NY(inset) ;
      nz   = DSET_NZ(inset) ; nxyz= nx*ny*nz;
      nval = DSET_NVALS(inset) ; /* emask has enough room for one sub-brik only */ 

      sum = (float **) malloc( sizeof(float *)*nval ) ;    /* array of sub-bricks */
      for( kk=0 ; kk < nval ; kk++ ){
        sum[kk] = (float *) malloc(sizeof(float)*nxyz) ;  /* kk-th sub-brick */ 
      }
      
      /* fill sum */
      {
         float *a  = sum[0];
         for (kk=0; kk < DSET_NVOX(inset); ++kk) {
            a[kk] = emask[kk];
         }
      }
      
      outset = EDIT_empty_copy( inset ) ;
   
      if( datum < 0 ) datum = DSET_BRICK_TYPE(inset,0) ;

      /*
      tross_Copy_History( inset , outset ) ;
      tross_Make_History( "3dedge3 function call" , 0 , NULL , outset ) ;
      */
      EDIT_dset_items( outset ,
                          ADN_prefix    , "LeBigMac" ,
                          ADN_datum_all , datum ,
                       ADN_none ) ;

      /*
      if( THD_is_file(outset->dblk->diskptr->header_name) ){
         fprintf(stderr,
                 "*** Output file %s already exists -- cannot continue!\n",
                 outset->dblk->diskptr->header_name ) ;
         SUMA_RETURN(0) ;
      }

      */
         
      switch( datum ){

         default:
            fprintf(stderr,
                    "*** Fatal Error ***\n"
                    "*** Somehow ended up with datum = %d\n",datum) ;
            SUMA_RETURN(0) ;

         case MRI_float:{
            for( kk=0 ; kk < nval ; kk++ ){
                EDIT_substitute_brick(outset, kk, MRI_float, sum[kk]);
                DSET_BRICK_FACTOR(outset, kk) = 0.0;
            }
         }
         break ;

         case MRI_byte:
         case MRI_short:{
            void ** dfim ;
            float gtop , fimfac , gtemp ;

            if( verb )
               fprintf(stderr,"  ++ Scaling output to type %s brick(s)\n",
                       MRI_TYPE_name[datum] ) ;

            dfim = (void **) malloc(sizeof(void *)*nval) ;

            if( gscale ){   /* allow global scaling */
               gtop = 0.0 ;
               for( kk=0 ; kk < nval ; kk++ ){
                  gtemp = MCW_vol_amax( nxyz , 1 , 1 , MRI_float, sum[kk] ) ;
                  gtop  = MAX( gtop , gtemp ) ;
                  if( gtemp == 0.0 )
                     fprintf(stderr,"  -- Warning: output sub-brick %d is all zeros!\n",kk) ;
               }
            }

            for (kk = 0 ; kk < nval ; kk ++ ) {

               if( ! gscale ){
                  gtop = MCW_vol_amax( nxyz , 1 , 1 , MRI_float, sum[kk] ) ;
                  if( gtop == 0.0 )
                     fprintf(stderr,"  -- Warning: output sub-brick %d is all zeros!\n",kk) ;
               }

               if( fscale ){
                  fimfac = (gtop > 0.0) ? MRI_TYPE_maxval[datum] / gtop : 0.0 ;
               } else if( !nscale ){
                  fimfac = (gtop > MRI_TYPE_maxval[datum] || (gtop > 0.0 && gtop <= 1.0) )
                           ? MRI_TYPE_maxval[datum]/ gtop : 0.0 ;
               } else {
                  fimfac = 0.0 ;
               }

               if( verb ){
                  if( fimfac != 0.0 )
                     fprintf(stderr,"  ++ Sub-brick %d scale factor = %f\n",kk,fimfac) ;
                  else
                     fprintf(stderr,"  ++ Sub-brick %d: no scale factor\n" ,kk) ;
               }

               dfim[kk] = (void *) malloc( mri_datum_size(datum) * nxyz ) ;
               if( dfim[kk] == NULL ){ fprintf(stderr,"*** malloc fails at output\n");SUMA_RETURN(0); }

               EDIT_coerce_scale_type( nxyz , fimfac ,
                                       MRI_float, sum[kk] , datum,dfim[kk] ) ;
               free( sum[kk] ) ;
               EDIT_substitute_brick(outset, kk, datum, dfim[kk] );

               DSET_BRICK_FACTOR(outset,kk) = (fimfac != 0.0) ? 1.0/fimfac : 0.0 ;
             }
         }
         break ;
      }

      if( verb ) fprintf(stderr,"  ++ Computing output statistics\n") ;
      THD_load_statistics( outset ) ;

      if (verb) {
         THD_write_3dim_dataset( NULL,NULL , outset , True ) ;
         fprintf(stderr,"  ++ Wrote output: %s\n",DSET_BRIKNAME(outset)) ;
      }
      
      *poutsetp = outset;
      
   }   
      
   SUMA_RETURN(YUP);
}
