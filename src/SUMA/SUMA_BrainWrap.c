#include "SUMA_suma.h"
#include "../thd_brainormalize.h"
#include "TrackIO.h"
#include "extrema.h"

static int InteractiveQuit;
static int NodeDbg;

void SUMA_setBrainWrap_NodeDbg(int n) { NodeDbg = n; }
int  SUMA_getBrainWrap_NodeDbg(void) { return(NodeDbg); }

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
float SUMA_LoadPrepInVol (SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt, 
                           SUMA_SurfaceObject **SOhull)
{
   static char FuncName[]={"SUMA_LoadPrepInVol"};
   int orient, i, nxx, nxy, cnt, *isort = NULL,iPercRangeVal[2], 
       tind, znxy, *ijk=NULL, nf = 0, trouble= 0, Force = 0;
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
      SUMA_SL_Err("Input volume can only have one sub-brick in it.\n"
                  "Use [.] selectors to choose sub-brick needed.");
      SUMA_RETURN(vol);
   }
   
   Opt->VolCM[0] = THD_BN_xcm(); 
   Opt->VolCM[1] = THD_BN_ycm(); 
   Opt->VolCM[2] = THD_BN_zcm();
   
   Opt->fvec = (float *)SUMA_malloc(sizeof(float) * Opt->nvox);
   if (!Opt->fvec) {
      SUMA_SL_Crit("Faile to allocate for fvec.\nOh misery.");
      SUMA_RETURN(NOPE);
   }
   EDIT_coerce_scale_type( Opt->nvox , DSET_BRICK_FACTOR(Opt->in_vol,0) ,
                           DSET_BRICK_TYPE(Opt->in_vol,0), 
                           DSET_ARRAY(Opt->in_vol, 0) ,      /* input  */
                           MRI_float, Opt->fvec  ) ;   /* output */
   
   /* estimate the t2, t98 and tm parameters, 
      should do them regionally by Bob's octant method */
   if (Opt->t2 < 0) {
      PercRange[0] = 2; PercRange[1] = 98;
      fvec_sort = SUMA_PercRange (Opt->fvec, NULL, Opt->nvox, PercRange, 
                                  PercRangeVal, iPercRangeVal);
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
         if (! (nf = SUMA_qhull_wrap(cnt, CoordList, &ijk, 1, NULL)) ) {
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
            if (!(orient = SUMA_OrientSOTriangles(SOhullp, 1, Force, NULL))) { 
               fprintf(SUMA_STDERR,"Error %s:\nFailed in SUMA_OrientTriangles\n",
                        FuncName); 
            }
            #endif 
               
            if (LocalHead) 
               fprintf(SUMA_STDERR,"%s: Refining hull surface.\n", FuncName);
            if (LocalHead) 
               fprintf(SUMA_STDERR,"%s: %d nodes, %d triangles.\n", 
                        FuncName, SOhullp->N_Node, SOhullp->N_FaceSet);
            /* SUMA_Print_Surface_Object(SOhullp, stderr); */
            /* if (!SUMA_Subdivide_Mesh(&(SOhullp->NodeList), 
                                 &(SOhullp->N_Node), &(SOhullp->FaceSetList), 
                                 &(SOhullp->N_FaceSet), 50)) { */
            if (0) {
               SUMA_SL_Err("Failed to subdivide mesh");
               SUMA_Free_Surface_Object (SOhullp); 
               *SOhull = NULL;
            } else {
               /* SUMA_Print_Surface_Object(SOhullp, stderr); */
               if (SOhullp->NodeNormList) 
                  SUMA_free(SOhullp->NodeNormList); SOhullp->NodeNormList = NULL;
               if (SOhullp->FaceNormList) 
                  SUMA_free(SOhullp->FaceNormList); SOhullp->FaceNormList = NULL;
               if (SOhullp->glar_NodeList) 
                  SUMA_free(SOhullp->glar_NodeList);SOhullp->glar_NodeList= NULL;
               if (SOhullp->glar_FaceSetList) SUMA_free(SOhullp->glar_FaceSetList); 
                  SOhullp->glar_FaceSetList = NULL;
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
   \param xyz (float *) node xyz triplet in RAI coords
   \param dir (float *) direction of propagation, unit length
                        +ve direction point out
   \param in_vol (THD_3dim.. *) the dataset 
   \param fvecp (float **) pointer to vector of all voxel values in in_vol
                           if *fvecp = NULL, it is created from in_vol,
                           Either free *fevcp on return or recycle it.
   \param travstep (float) travel step in mm, usually = smallest voxel dimension
   \param under_dist (float) maximum travel in mm below node
   \param over_dist (float) maximum travel in mm above node 
   \param stop_avg_thr (float) When computing means, stop adding when you reach 
                               a value < stop_avg_thr
   \param NodeDbg (int) index of debug node
   
   Variables filled upon returning. 
   
   \param MinMax (float) 2x1: MinMax[0] minimum under node
                              MinMax_dist[1] maximum under node
   \param MinMax_dist (float) 2x1: MinMax_dist[0] distance of minimum under node
                                   MinMax_dist[1] distance of maximum under node
   \param MinMax_over (float) 2x1: MinMax_over[0] minimum over node
                                   MinMax_over[1] maximum over node
               (set this one to NULL if you do not want to search over the node.
                In that case all the _over (or above like Means[2]) values will
                be undetermined.)
   \param MinMax_over_dist (float) 2x1: MinMax_over_dist[0] distance of 
                                        minimum over node
                                        MinMax_over_dist[1] distance of 
                                        maximum over node
   \param Means (float *) 3x1 :  Means[0] value at node, 
                 Means[1] mean value below node (only up to vals > stop_avg_thr),
                 Means[2] mean value above node  (only up to vals > stop_avg_thr)
   
   \params undershish (float *) Vector containing values under node
   \params overshsis (float *) Vector containing values over node
   \param fvecind_under (int *) indices into volume of voxels encountered
                                underneath the node
   \param fvecind_over (int *) indices into volume of voxels encountered
                               over the node
      MAKE SURE YOU ALLOCATE enough for the last four vectors.
      Their lengths should be: 
               (int)(ceil((double)under_dist/travstp))+2, to be safe
*/
int SUMA_Find_IminImax_2 (float *xyz, float *dir, 
                        THD_3dim_dataset *in_vol, float **fvecp,
                        float travstp, 
                        float under_dist, float over_dist, 
                        float stop_avg_thr, 
                        int verb, 
                        float *MinMax, float *MinMax_dist , 
                        float *MinMax_over, float *MinMax_over_dist,
                        float *Means, 
                        float *undershish, float *overshish, 
                        int *fvecind_under, int *fvecind_over
                        )
{
   static char FuncName[]={"SUMA_Find_IminImax_2"};
   float travdir[3], lmin, lmax, lmin_dist, lmax_dist, *fvec;
   THD_fvec3 ncoord, ndicom;
   THD_ivec3 nind3;
   int nind, istep, istepmax, nxx, nxy, nMeans[3], stopint, out;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   #if 0
   if (LocalHead) {
      SUMA_LH("On dset %s, travstp %f, under_dist %f, over_dist %f, "
              "stop_avg_thr %f, fvecp=%p",
              DSET_PREFIX(in_vol),
              travstp, under_dist, over_dist, stop_avg_thr, fvecp);
      SUMA_DUMP_TRACE("Who wants IminImax?");
   }
   #endif
   if (!fvecp) SUMA_RETURN(NOPE);
   else {
      if (*fvecp == NULL) {
         if (!(fvec = THD_extract_to_float(0, in_vol))) {
            SUMA_S_Err("Failed to extract brick");
            SUMA_RETURN(NOPE);
         }
         *fvecp = fvec;
      }
   }
   fvec = *fvecp;
   
   if (undershish) undershish[0] = -1;
   if (overshish) overshish[0] = -1;
   Means[0] = Means[1] = Means[2] = 0.0;
   nMeans[0] = nMeans[1] = nMeans[2] = 0;
   lmin = 1000000.0;
   lmin_dist = 0.0;
   lmax_dist = 0.0;
   lmax = 0; 
   nxx = DSET_NX(in_vol);
   nxy = DSET_NX(in_vol) * DSET_NY(in_vol);
   istep = 0; 
   istepmax = (int)(ceil((double)under_dist/travstp)); 
   stopint = 0;
   out = 0;
   while (istep <= istepmax && !out) {
      /* calculate offset */
      travdir[0] = -istep * travstp * dir[0]; 
      travdir[1] = -istep * travstp * dir[1]; 
      travdir[2] = -istep * travstp * dir[2]; 
      
      /* get 1d coord of point */
      ndicom.xyz[0] = xyz[0] + travdir[0] ; 
      ndicom.xyz[1] = xyz[1]+ travdir[1]; 
      ndicom.xyz[2] = xyz[2]+ travdir[2];
      ncoord = THD_dicomm_to_3dmm(in_vol, ndicom);
      nind3 = THD_3dmm_to_3dind_warn(in_vol, ncoord, &out);
      if (out) continue;
      nind = nind3.ijk[0] + nind3.ijk[1] * nxx + nind3.ijk[2] * nxy;

      #if 0
      if (verb) {
         fprintf( SUMA_STDERR, 
                  "%s: at node %d undershish[%d/%d] \n"
                  " nind3 = [%d %d %d] voxVal = %.3f\n", 
                  FuncName, SUMA_getBrainWrap_NodeDbg(), istep, istepmax,
                  nind3.ijk[0], nind3.ijk[1], nind3.ijk[2],
                  fvec[nind]);
      }
      #endif
      if (undershish) 
         undershish[istep] = fvec[nind];   /* store values under node */
      
      /* track the brain mean, 
      only if the value is above brain non-brain threshold
      stop inegrating as soon as you hit nonbrain */
      if (fvec[nind] > stop_avg_thr && !stopint) { 
         Means[1] += fvec[nind]; ++ nMeans[1]; }
      else stopint = 1;
      if (fvecind_under) fvecind_under[istep] = nind;
        
      /* find local min */ 
      /* lmin = SUMA_MIN_PAIR(lmin, fvec[nind]); */
      if (lmin > fvec[nind]) { 
         lmin = fvec[nind]; SUMA_NORM_VEC(travdir, 3, lmin_dist); }
      
      if (istep <= istepmax) {
         if (lmax < fvec[nind]) { 
            lmax = fvec[nind]; SUMA_NORM_VEC(travdir, 3, lmax_dist); }  
      }
      
      if (!out) ++istep;
   }
   #if 0
   if (verb) {
         fprintf( SUMA_STDERR, 
                  "Crowning undershish[%d/%d]\n", istep, istepmax);
   }
   #endif
   if (1) { 
      undershish[istep] = -1; 
      if (fvecind_under) fvecind_under[istep] = -1; 
   }/* crown the shish */
   
   
   Means[1] /= (float)nMeans[1];
   MinMax[0] = lmin;
   MinMax_dist[0] = lmin_dist;  
   MinMax[1] = lmax; 
   MinMax_dist[1] = lmax_dist;
   
   /* search for a minimum overhead */
   if (MinMax_over) {
      lmin = 10000000.0;
      lmin_dist = 0.0;
      lmax_dist = 0.0;
      lmax = 0.0;
      istep = 0; istepmax = (int)(ceil((double)over_dist/travstp));
      stopint = 0;
      out = 0;
      while (istep <= istepmax && !out) {
         /* calculate offset */
         travdir[0] =  istep * travstp * dir[0]; 
         travdir[1] = istep * travstp * dir[1]; 
         travdir[2] = istep * travstp * dir[2]; 

         /* get 1d coord of point */
         ndicom.xyz[0] = xyz[0] + travdir[0] ; 
         ndicom.xyz[1] = xyz[1]+ travdir[1]; 
         ndicom.xyz[2] = xyz[2]+ travdir[2]; 
         ncoord = THD_dicomm_to_3dmm(in_vol, ndicom);
         nind3 = THD_3dmm_to_3dind_warn(in_vol, ncoord, &out);
         if (out) continue;
         nind = nind3.ijk[0] + nind3.ijk[1] * nxx + nind3.ijk[2] * nxy;
         #if 0
         if (verb) {
            fprintf(SUMA_STDERR, 
                     "%s: Node %d\n"
                     " nind3 = [%d %d %d] voxVal = %.3f\n", 
                     FuncName, NodeDbg, 
                     nind3.ijk[0], nind3.ijk[1], nind3.ijk[2],
                     fvec[nind]);
         }
         #endif
         if (!istep) { /* get the value at the node */
            Means[0] = fvec[nind];
         }
         
        /* store values over node */
         if (overshish) overshish[istep] = fvec[nind]; 
         
         if (fvec[nind] > stop_avg_thr && !stopint) { 
            Means[2] += fvec[nind]; ++ nMeans[2]; } 
         else stopint = 1;
         if (fvecind_over) fvecind_over[istep] = nind;
         
         /* find local min and max*/ 
         if (lmin > fvec[nind]) { 
            lmin = fvec[nind]; SUMA_NORM_VEC(travdir, 3, lmin_dist); }
         if (lmax < fvec[nind]) { 
            lmax = fvec[nind]; SUMA_NORM_VEC(travdir, 3, lmax_dist); }  
         
         if (!out) ++istep;
      }
      #if 0
      if (verb) {
         fprintf( SUMA_STDERR, 
                  "Crowning overshish[%d/%d]\n", istep, istepmax);
      }
      #endif
      if (1) { 
         overshish[istep] = -1; 
         if (fvecind_over) fvecind_over[istep] = -1; 
      }/* crown the shish */

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
      
     This function is left for 3dSkullStrip, see newer saner SUMA_Find_IminImax_2
*/
int SUMA_Find_IminImax (float *xyz, float *dir, 
                        SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt, 
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
   
   if (undershish) undershish[0] = -1;
   if (overshish) overshish[0] = -1;
   Means[0] = Means[1] = Means[2] = 0.0;
   nMeans[0] = nMeans[1] = nMeans[2] = 0;
   lmin = Opt->tm;
   lmin_dist = 0.0;
   lmax_dist = 0.0;
   lmax = Opt->t;
   nxx = DSET_NX(Opt->in_vol);
   nxy = DSET_NX(Opt->in_vol) * DSET_NY(Opt->in_vol);
   istep = 0; 
   istepmax = (int)(ceil((double)d1/Opt->travstp)); 
   istep2max = (int)(ceil((double)d2/Opt->travstp));
   stopint = 0;
   out = 0;
   while (istep <= istepmax && !out) {
      /* calculate offset */
      travdir[0] = - istep * Opt->travstp * dir[0]; 
      travdir[1] = -istep * Opt->travstp * dir[1]; 
      travdir[2] = -istep * Opt->travstp * dir[2]; 
      
      /* get 1d coord of point */
      ndicom.xyz[0] = xyz[0] + travdir[0] ; 
      ndicom.xyz[1] = xyz[1]+ travdir[1]; 
      ndicom.xyz[2] = xyz[2]+ travdir[2]; 
      ncoord = THD_dicomm_to_3dmm(Opt->in_vol, ndicom);
      nind3 = THD_3dmm_to_3dind_warn(Opt->in_vol, ncoord, &out);
      nind = nind3.ijk[0] + nind3.ijk[1] * nxx + nind3.ijk[2] * nxy;
      
      #if 0
      if (ni == Opt->NodeDbg) {
         fprintf( SUMA_STDERR, 
                  "%s: Node %d\n"
                  " nind3 = [%d %d %d] voxVal = %.3f\n", 
                  FuncName, Opt->NodeDbg, 
                  nind3.ijk[0], nind3.ijk[1], nind3.ijk[2],
                  Opt->fvec[nind]);
      }
      #endif
      if (undershish && istep < ShishMax) 
         undershish[istep] = Opt->fvec[nind];   /* store values under node */
      
      /* track the brain mean, 
      only if the value is above brain non-brain threshold
      stop inegrating as soon as you hit nonbrain */
      if (Opt->fvec[nind] > Opt->t && !stopint) { 
         Means[1] += Opt->fvec[nind]; ++ nMeans[1]; }
      else stopint = 1;
      if (fvecind_under && istep < ShishMax) fvecind_under[istep] = nind;
        
      /* find local min */ 
      /* lmin = SUMA_MIN_PAIR(lmin, Opt->fvec[nind]); */
      if (lmin > Opt->fvec[nind]) { 
         lmin = Opt->fvec[nind]; SUMA_NORM_VEC(travdir, 3, lmin_dist); }
      
      if (istep <= istep2max) {
         if (lmax < Opt->fvec[nind]) { 
            lmax = Opt->fvec[nind]; SUMA_NORM_VEC(travdir, 3, lmax_dist); }  
      }
      
      if (!out) ++istep;
   }
   
   if (istep < ShishMax) { 
      undershish[istep] = -1; 
      if (fvecind_under) fvecind_under[istep] = -1; 
   }/* crown the shish */
   
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
         travdir[0] =  istep * Opt->travstp * dir[0]; 
         travdir[1] = istep * Opt->travstp * dir[1]; 
         travdir[2] = istep * Opt->travstp * dir[2]; 

         /* get 1d coord of point */
         ndicom.xyz[0] = xyz[0] + travdir[0] ; 
         ndicom.xyz[1] = xyz[1]+ travdir[1]; 
         ndicom.xyz[2] = xyz[2]+ travdir[2]; 
         ncoord = THD_dicomm_to_3dmm(Opt->in_vol, ndicom);
         nind3 = THD_3dmm_to_3dind_warn(Opt->in_vol, ncoord, &out);
         nind = nind3.ijk[0] + nind3.ijk[1] * nxx + nind3.ijk[2] * nxy;
         #if 0
         if (ni == Opt->NodeDbg) {
            fprintf(SUMA_STDERR, 
                     "%s: Node %d\n"
                     " nind3 = [%d %d %d] voxVal = %.3f\n", 
                     FuncName, Opt->NodeDbg, 
                     nind3.ijk[0], nind3.ijk[1], nind3.ijk[2],
                     Opt->fvec[nind]);
         }
         #endif
         if (!istep) { /* get the value at the node */
            Means[0] = Opt->fvec[nind];
         }
         
        /* store values over node */
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
      
      if (istep < ShishMax) { 
         overshish[istep] = -1; 
         if (fvecind_over) fvecind_over[istep] = -1; 
      }/* crown the shish */

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
                 be undetermined.)
   \param MinMax_over_dist (float) 2x1: 
                        MinMax_over_dist[0] distance of minimum over node
                        MinMax_over_dist[1] distance of maximum over node
   \param Means (float *) 3x1 :  Means[0] value at node, 
                     Means[1] mean value below node (only for vals > Opt->t),
                     Means[2] mean value above node  (only for vals > Opt->t)
*/
int SUMA_Find_IminImax_Avg (SUMA_SurfaceObject *SO, 
                        SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt, 
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
   int N_vtnmax = 500, N_vtn, 
       vtn[N_vtnmax]; /* vector of triangles incident triangles to node ni */
   
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
   istep = 0; 
   istepmax = (int)(ceil((double)d1/Opt->travstp)); 
   istep2max = (int)(ceil((double)d2/Opt->travstp));
   stopint = 0;
   while (istep <= istepmax) {
      /* calculate offset */
      travdir[0] = - istep * Opt->travstp * SO->NodeNormList[3*ni  ]; 
      travdir[1] = - istep * Opt->travstp * SO->NodeNormList[3*ni+1]; 
      travdir[2] = - istep * Opt->travstp * SO->NodeNormList[3*ni+2]; 
      
      /* find the set of triangles incident to node ni */
      N_vtn = N_vtnmax; /* pass limit to SUMA_Get_NodeIncident */
      if (!SUMA_Get_NodeIncident(ni, SO, vtn, &N_vtn)) {
          SUMA_SL_Err("Failed to find incident triangles.\n"
                      "Decidement, ca va tres mal.\n");
          SUMA_RETURN(NOPE);
      }
      if (vtn[N_vtnmax-1] != -1) {
         SUMA_SL_Err("Way too many incident triangles. "
                     "Memory corruption likely.");
         SUMA_RETURN(NOPE);
      }
      /* for each triangle, find the voxels that it intersects */
      
      /* get 1d coord of point */
      ndicom.xyz[0] = SO->NodeList[3*ni] + travdir[0] ; 
      ndicom.xyz[1] = SO->NodeList[3*ni+1]+ travdir[1]; 
      ndicom.xyz[2] = SO->NodeList[3*ni+2]+ travdir[2]; 
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
      
      if (undershish && istep < ShishMax) undershish[istep] = Opt->fvec[nind];
      
      /* track the brain mean, only if the value is above 
      brain non-brain threshold
      stop inegrating as soon as you hit nonbrain */
      if (Opt->fvec[nind] > Opt->t && !stopint) { 
         Means[1] += Opt->fvec[nind]; ++ nMeans[1]; 
      }
      else stopint = 1;
      if (fvecind_under && istep < ShishMax) fvecind_under[istep] = nind;
        
      /* find local min */ 
      /* lmin = SUMA_MIN_PAIR(lmin, Opt->fvec[nind]); */
      if (lmin > Opt->fvec[nind]) { 
         lmin = Opt->fvec[nind]; SUMA_NORM_VEC(travdir, 3, lmin_dist); 
      }
      
      if (istep <= istep2max) {
         if (lmax < Opt->fvec[nind]) { 
            lmax = Opt->fvec[nind]; SUMA_NORM_VEC(travdir, 3, lmax_dist); 
         }  
      }
      
      ++istep;
   }
   
   if (istep < ShishMax) { /* crown the shish */
      if (undershish) undershish[istep] = -1; 
      if (fvecind_under) fvecind_under[istep] = -1; 
   }
   
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
         travdir[0] =  istep * Opt->travstp * SO->NodeNormList[3*ni  ]; 
         travdir[1] =  istep * Opt->travstp * SO->NodeNormList[3*ni+1];
         travdir[2] = istep * Opt->travstp * SO->NodeNormList[3*ni+2]; 

         /* get 1d coord of point */
         ndicom.xyz[0] = SO->NodeList[3*ni  ] + travdir[0]; 
         ndicom.xyz[1] = SO->NodeList[3*ni+1] + travdir[1]; 
         ndicom.xyz[2] = SO->NodeList[3*ni+2] + travdir[2]; 
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
         
         if (overshish && istep < ShishMax) 
            overshish[istep] = Opt->fvec[nind];   /* store values under node */
         
         if (Opt->fvec[nind] > Opt->t && !stopint) { 
            Means[2] += Opt->fvec[nind]; ++ nMeans[2]; 
         } else stopint = 1;
         if (fvecind_over) fvecind_over[istep] = nind;
         
         /* find local min and max*/ 
         if (lmin > Opt->fvec[nind]) { 
            lmin = Opt->fvec[nind]; SUMA_NORM_VEC(travdir, 3, lmin_dist); 
         }
         if (lmax < Opt->fvec[nind]) { 
            lmax = Opt->fvec[nind]; SUMA_NORM_VEC(travdir, 3, lmax_dist); 
         }  
         
         ++istep;
      }
      
      if (istep < ShishMax) { /* crown the shish */
         if (overshish) overshish[istep] = -1; 
         if (fvecind_over) fvecind_over[istep] = -1; 
      }

      Means[2] /= (float)nMeans[2];
      MinMax_over[0] = lmin; 
      MinMax_over_dist[0] = lmin_dist;  
      MinMax_over[1] = lmax;
      MinMax_over_dist[1] = lmax_dist;
   }
   
   SUMA_RETURN(YUP);
}

int SUMA_THD_Radial_Avgs( THD_3dim_dataset  *dset, float *ufin,
                           byte *cmask, float *ucm,
                           byte zeropad, 
                           float under, float over, int trvoff[2],
                           float **umu, float **umo)
{
   static char FuncName[]={"SUMA_THD_Radial_Avgs"};
   int ii, jj, kk, vv, trv[2];
   THD_fvec3 ccc, ncoord;
   float cm[3], *mo=NULL, *mu=NULL, *fin=NULL,
         xyz_ijk[3], means[3], mvoxd;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   

   if (!dset || (!umu && !umo)) {
      SUMA_S_Err("Nothing to do");
      SUMA_RETURN(NOPE);
   }
   if (umu && !*umu) {
      *umu = (float *)SUMA_calloc(DSET_NVOX(dset), sizeof(float));
   }
   if (umu) mu = *umu;
   if (umo && !*umo) {
      *umo = (float *)SUMA_calloc(DSET_NVOX(dset), sizeof(float));
   }
   if (umo) mo = *umo;
   
   if (ufin) fin = ufin;
   else fin = THD_extract_to_float(0, dset);

   if ((!mo && !mu) || !fin) {
      SUMA_S_Err("Stranger things never happen");
      SUMA_RETURN(NOPE);
   }
   
   if (!ucm) {
      ccc = THD_cmass(dset, 0, cmask,0);
      cm[0] = ccc.xyz[0];
      cm[1] = ccc.xyz[1];
      cm[2] = ccc.xyz[2];
   } else {
      cm[0] = ucm[0];
      cm[1] = ucm[1];
      cm[2] = ucm[2];
   }
   /* change cm to mm coords */
   ccc.xyz[0]=cm[0]; ccc.xyz[1]=cm[1]; ccc.xyz[2]=cm[2]; 
   ncoord = THD_dicomm_to_3dmm(dset, ccc);
   ccc = THD_3dmm_to_3dfind(dset, ncoord);
   cm[0] = ccc.xyz[0];
   cm[1] = ccc.xyz[1];
   cm[2] = ccc.xyz[2];
   
   
   /* min voxel dim */
   mvoxd = SUMA_MIN_PAIR(SUMA_ABS(DSET_DX(dset)), SUMA_ABS(DSET_DY(dset)));
   mvoxd = SUMA_MIN_PAIR(mvoxd, SUMA_ABS(DSET_DZ(dset)));
   
   trv[0] = trv[1] = 0;
   if (mu) trv[0] = under / mvoxd;
   if (mo) trv[1] = over / mvoxd; 
   
   /* get voxel values */
   vv=0;
   for (kk=0; kk<DSET_NZ(dset); ++kk) {
   SUMA_LHv("kk=%d/%d, cmijk=[%f %f %f]\n", 
            kk, DSET_NZ(dset), cm[0], cm[1], cm[2]);
   for (jj=0; jj<DSET_NY(dset); ++jj) {
   for (ii=0; ii<DSET_NX(dset); ++ii) {
      if (!cmask || cmask[vv]) {
         xyz_ijk[0]= ii; xyz_ijk[1]= jj; xyz_ijk[2]= kk;
         if (!SUMA_Vox_Radial_Stats(fin,
                  DSET_NX(dset), DSET_NY(dset), DSET_NZ(dset),
                  xyz_ijk, cm, trv, trvoff,
                  means, 
                  NULL, NULL, NULL, NULL, zeropad)) {
            SUMA_S_Errv("Failed at voxel %d %d %d\n",
                        ii, jj, kk);
            SUMA_RETURN(NOPE);
         }
         if (mu) mu[vv] = means[1];
         if (mo) mo[vv] = means[2];
      } else {
         if (mu) mu[vv] = 0.0;
         if (mo) mo[vv] = 0.0;
      }   
      if (vv == NodeDbg) {
        SUMA_S_Notev("At cm have means=[%f %f %f]\n", 
                     means[0], means[1], means[2]);
      }
      ++vv;
   }}}
   
   
   if (!ufin) free(fin);
   fin = NULL;

   SUMA_RETURN(YUP);
}

int SUMA_THD_Radial_Stats( THD_3dim_dataset  *dset,
                           byte *cmask, float *ucm,
                           THD_3dim_dataset **osetp,
                           byte zeropad, 
                           float under, float over, int avgwin )
{
   static char FuncName[]={"SUMA_THD_Radial_Stats"};
   THD_3dim_dataset *oset=NULL, *shoset=NULL;
   int ii, jj, kk, vv, trv[2], sb, 
       *ioversh=NULL, *iundersh=NULL, si, ssi, trvoff[2];
   THD_fvec3 ccc, ncoord;
   float cm[3], *mo=NULL, *mu=NULL, *fin=NULL, *mr=NULL,
         xyz_ijk[3], means[3], factor, mvoxd,
         *shs = NULL, *oversh = NULL, *undersh = NULL;
   char stmp[36];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (avgwin < 1) avgwin =1;
   if (!osetp || !dset) SUMA_RETURN(NOPE);
   if (!ucm) {
      ccc = THD_cmass(dset, 0, cmask,0);
      cm[0] = ccc.xyz[0];
      cm[1] = ccc.xyz[1];
      cm[2] = ccc.xyz[2];
   } else {
      cm[0] = ucm[0];
      cm[1] = ucm[1];
      cm[2] = ucm[2];
   }
   /* change cm to mm coords */
   ccc.xyz[0]=cm[0]; ccc.xyz[1]=cm[1]; ccc.xyz[2]=cm[2]; 
   ncoord = THD_dicomm_to_3dmm(dset, ccc);
   ccc = THD_3dmm_to_3dfind(dset, ncoord);
   cm[0] = ccc.xyz[0];
   cm[1] = ccc.xyz[1];
   cm[2] = ccc.xyz[2];
   
   
   /* min voxel dim */
   mvoxd = SUMA_MIN_PAIR(SUMA_ABS(DSET_DX(dset)), SUMA_ABS(DSET_DY(dset)));
   mvoxd = SUMA_MIN_PAIR(mvoxd, SUMA_ABS(DSET_DZ(dset)));
   
   trv[0] = under / mvoxd; trv[1] = over / mvoxd; 
   trvoff[0] = 0; trvoff[1] = 1;
   if (avgwin > trv[0] || avgwin > trv[1]) {
      SUMA_S_Err("Averaging window longer than shish");
      SUMA_RETURN(NOPE);
   }
   
   /* get voxel values */
   fin = THD_extract_to_float(0, dset);
   mu = (float*)SUMA_calloc(DSET_NVOX(dset), sizeof(float));
   mo = (float*)SUMA_calloc(DSET_NVOX(dset), sizeof(float));
   mr = (float*)SUMA_calloc(DSET_NVOX(dset), sizeof(float));
   undersh = (float*)SUMA_calloc(trv[0]+2, sizeof(float));
   oversh  = (float*)SUMA_calloc(trv[1]+2, sizeof(float));
   iundersh = (int *)SUMA_calloc(trv[0]+2, sizeof(int));
   ioversh  = (int*)SUMA_calloc(trv[1]+2, sizeof(int));
   shs = (float *)SUMA_calloc(trv[0]+trv[1]+2, sizeof(float));
   
   /* output shishes */
   NEW_SHORTY(dset, trv[0]+trv[1]-2*(avgwin-1), FuncName, shoset);
   SET_ALL_BRICK_FACTORS(shoset, DSET_BRICK_FACTOR(dset,0));
   for (ssi=0, si=trv[0]-1-(avgwin-1); si>=0; --si) {
      if (avgwin == 1) sprintf(stmp,"%d below",si+trvoff[0]); 
      else sprintf(stmp,"avg(%d) %d below",avgwin, si+trvoff[0]); 
      EDIT_BRICK_LABEL (shoset, ssi, stmp);  
      ++ssi;
   }
   for (si=0; si<trv[1]-(avgwin-1); ++si) {
      if (avgwin == 1) sprintf(stmp,"%d above",si+trvoff[1]);
      else sprintf(stmp,"avg(%d) %d above",avgwin, si+trvoff[1]);
      EDIT_BRICK_LABEL (shoset, ssi, stmp);  
      ++ssi;
   } 

   vv=0;
   for (kk=0; kk<DSET_NZ(dset); ++kk) {
   SUMA_LHv("kk=%d/%d, cmijk=[%f %f %f]\n", 
            kk, DSET_NZ(dset), cm[0], cm[1], cm[2]);
   for (jj=0; jj<DSET_NY(dset); ++jj) {
   for (ii=0; ii<DSET_NX(dset); ++ii) {
      if (ii==(int)cm[0] && jj==(int)cm[1] && kk==(int)cm[2]) {
         SUMA_setBrainWrap_NodeDbg(vv);
      }
      if (!cmask || cmask[vv]) {
         xyz_ijk[0]= ii; xyz_ijk[1]= jj; xyz_ijk[2]= kk;
         if (!SUMA_Vox_Radial_Stats(fin,
                  DSET_NX(dset), DSET_NY(dset), DSET_NZ(dset),
                  xyz_ijk, cm, trv, trvoff,
                  means, 
                  undersh, oversh, iundersh, ioversh, zeropad)) {
            SUMA_S_Errv("Failed at voxel %d %d %d\n",
                        ii, jj, kk);
            SUMA_RETURN(NOPE);
         }
         if (vv == NodeDbg) {
            int ddd=0;
            SUMA_S_Notev("Values at voxel %d\n", vv);
            fprintf(SUMA_STDERR,"Under=[");
            for (ddd=0; ddd<trv[0]; ++ddd) {
               fprintf(SUMA_STDERR,"%f\t", undersh[ddd]);
            }
            fprintf(SUMA_STDERR,"]\nOver=[");
            for (ddd=0; ddd<trv[0]; ++ddd) {
               fprintf(SUMA_STDERR,"%f\t", oversh[ddd]);
            }
            fprintf(SUMA_STDERR,"]\n");
         }
         {/* assemble all shish, start from bottom and go to top */
            if (avgwin > 1) {
               SUMA_MOVING_SUM(undersh, trv[0], avgwin, 1,1);
               SUMA_MOVING_SUM(oversh, trv[1], avgwin, 1,1);
            }
            if (vv == NodeDbg) {
               int ddd=0;
               SUMA_S_Notev("%d smoothed Values at voxel %d\n", avgwin, vv);
               fprintf(SUMA_STDERR,"Under%d=[", avgwin);
               for (ddd=0; ddd<trv[0]; ++ddd) {
                  fprintf(SUMA_STDERR,"%f\t", undersh[ddd]);
               }
               fprintf(SUMA_STDERR,"]\nOver%d=[",avgwin);
               for (ddd=0; ddd<trv[0]; ++ddd) {
                  fprintf(SUMA_STDERR,"%f\t", oversh[ddd]);
               }
               fprintf(SUMA_STDERR,"]\n");
            }
            ssi = 0;
            for (si=trv[0]-1-(avgwin-1); si>=0; --si) {
               shs[ssi++] = undersh[si]; 
            }
            for (si=0; si<trv[1]-(avgwin-1); ++si) {
               shs[ssi++] = oversh[si];
            }
            /* put in output volume */
            THD_insert_series(vv, shoset, trv[0]+trv[1]-2*(avgwin-1), 
                              MRI_float, shs, 1);
         }
         mu[vv] = means[1];
         mo[vv] = means[2];
         if (mu[vv]+mo[vv] != 0.0f) mr[vv] = (mu[vv]-mo[vv])/(mu[vv]+mo[vv]);
         else mr[vv] = 0.0;
      } else {
         mu[vv] = 0.0;
         mo[vv] = 0.0;
         mr[vv] = 0.0;
      }   
      if (vv == NodeDbg) {
        SUMA_S_Notev("At cm have means=[%f %f %f], rat %f\n", 
                     means[0], means[1], means[2], mr[vv]);
      }
      ++vv;
   }}}
   
   /* stick the results in the output */
   if (!*osetp) {
      NEW_SHORTY(dset, 3, FuncName, oset);
      *osetp = oset;
   } else {
      oset = *osetp;
   }
   
   sb = 0;
   factor = EDIT_coerce_autoscale_new(DSET_NVOX(oset), MRI_float, mu, 
                          DSET_BRICK_TYPE(oset,sb), DSET_BRICK_ARRAY(oset,sb));
   if (factor > 0.0f) {
      factor = 1.0 / factor;
   } else factor = 0.0;
   EDIT_BRICK_FACTOR (oset, sb, factor);
   EDIT_BRICK_LABEL (oset, sb, "UnderMean");
   
   sb = 1;
   factor = EDIT_coerce_autoscale_new(DSET_NVOX(oset), MRI_float, mo, 
                          DSET_BRICK_TYPE(oset,sb), DSET_BRICK_ARRAY(oset,sb));
   if (factor > 0.0f) {
      factor = 1.0 / factor;
   } else factor = 0.0;
   EDIT_BRICK_FACTOR (oset, sb, factor);
   EDIT_BRICK_LABEL (oset, sb, "OverMean");
   
   sb = 2;
   factor = EDIT_coerce_autoscale_new(DSET_NVOX(oset), MRI_float, mr, 
                          DSET_BRICK_TYPE(oset,sb), DSET_BRICK_ARRAY(oset,sb));
   if (factor > 0.0f) {
      factor = 1.0 / factor;
   } else factor = 0.0;
   EDIT_BRICK_FACTOR (oset, sb, factor);
   EDIT_BRICK_LABEL (oset, sb, "(U-O)/(U+O)");
   
   /* write out the shish set */
   DSET_write(shoset) ; DSET_delete(shoset);
   
   if (fin) free(fin);
   if (undersh) SUMA_free(undersh); undersh=NULL;
   if (iundersh) SUMA_free(iundersh); iundersh=NULL;
   if (oversh) SUMA_free(oversh); oversh=NULL;
   if (ioversh) SUMA_free(ioversh); ioversh=NULL;
   if (shs) SUMA_free(shs); shs=NULL;
   if (mu) SUMA_free(mu); 
   if (mo) SUMA_free(mo);
   if (mr) SUMA_free(mr); 
   SUMA_RETURN(YUP);
}
                     
/*
   Create a mask on non brain area by
   finding voxels outside of ellipse centered
   on center of mass. The ellipse is 200 wide (LR) 
   and 230 long (AP). Its side does not change
   with height.
   
   if mean and stdv are not null, median and MAD
   are used to estimate the mean and stdv in
   the voxels outside of the brain.
*/
int SUMA_CrudeNonHeadMask(THD_3dim_dataset *dset,
                            float *cm_dicom, byte **cmaskp,
                            float *mean, float *stdv) 
{
   static char FuncName[]={"SUMA_CrudeNonHeadMask"};
   
   float cm[3], dLR, dAP, fAP, fAP2, jjfAP, rad, radi, radj, sqdist, radsq;
   byte location, *smask=NULL, *cmask=NULL, *raimask=NULL;
   float *vmask=NULL, *vvals=NULL, omed, omad;
   int ii, jj, kk, vv, pp, n_vmask, n_smask;
   THD_3dim_dataset *raiset=NULL, *dbuf=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!dset || !cm_dicom) {
      SUMA_S_Err("Need dset and its com");
      SUMA_RETURN(NOPE);
   }
   if ((mean && !stdv) || (!mean && stdv)) {
      SUMA_S_Err("Need both mean and stdv or neither");
      SUMA_RETURN(NOPE);
   }

   /* turn dset to RAI */
   raiset =  r_new_resam_dset(dset, NULL, 0.0, 0.0, 0.0, 
                              "RAI", RESAM_NN_TYPE, NULL, 1, 0);  
   raimask = THD_makemask( raiset , 0 , 1.0, -1.0 );
   
   SUMA_THD_dicomm_to_3dfind(raiset,cm_dicom[0],cm_dicom[1],cm_dicom[2],cm);         
   dLR=200; /* 200 mm from left to right */
   dAP=230; /* 230 mm from Anterior to posterior*/
   fAP = dLR/dAP; /* compression factor for AP direction */ 
   fAP2 = fAP*fAP;
   /* We don't mess with the IS direction because cm is bad along that
      axis, depending on the coverage, all slices will have the big
      ellipse */
   rad = dLR/2.0; radsq = rad*rad;
   radi = rad/SUMA_ABS(DSET_DX(raiset));
   radj = rad/SUMA_ABS(DSET_DY(raiset));
   
   /* make a mask for one slice */
   SUMA_LHv("Slice Mask, radi=%f vox., radj=%f vox, fAP=%f, cm[%f,%f]vox.\n", 
               radi, radj, fAP, cm[0], cm[1]);
   smask = (byte *)SUMA_calloc(DSET_NX(raiset)*DSET_NY(raiset),sizeof(byte));
   pp=0;
   for (jj=0; jj<DSET_NY(raiset); ++jj) { /* Anterior to Posterior */
   jjfAP = jj*fAP;
   for (ii=0; ii<DSET_NX(raiset); ++ii) { /* Right to Left */
      location=0; /* outside */
             if ( (ii > cm[0]-radi)    &&
                  (ii < cm[0]+radi)    &&
                  (jjfAP > cm[1]-radj) &&
                  (jjfAP < cm[1]+radj) ) location = 1;
      if (location) { /* now check the distance */
         sqdist = (ii-cm[0])*(ii-cm[0])+(jj-cm[1])*(jj-cm[1])*fAP2;
         if (sqdist < radsq) location = 2; /* inside the ellipse */
      }
      if (location == 0) smask[pp]=2;
      else if (location == 1) smask[pp]=1;
      else smask[pp]=0;
      ++pp;
   }}
   n_smask = pp;
   
   /* Now spread mask for whole brain */
   SUMA_LH("Vol Mask");
   cmask = (byte *)SUMA_calloc(DSET_NVOX(raiset),sizeof(byte));
   if (mean) {
      vvals = THD_extract_to_float(0, raiset);
      vmask = (float *)SUMA_calloc(n_smask*DSET_NZ(raiset), sizeof(float));
   }
   
   vv=0;n_vmask=0;
   for (kk=0; kk<DSET_NZ(raiset); ++kk) {
   pp=0;
   for (jj=0; jj<DSET_NY(raiset); ++jj) {
   for (ii=0; ii<DSET_NX(raiset); ++ii) {
      if (smask[pp] && raimask[vv]) {
         cmask[vv]=smask[pp];
         if (mean) vmask[n_vmask++]=vvals[vv];
      }
      ++pp; ++vv;
   }}}
   SUMA_free(smask); smask=NULL;
   SUMA_free(raimask); raimask=NULL;

   /* some stats */
   if (mean) {
      qmedmad_float( n_vmask, vmask , &omed , &omad ) ;
      SUMA_LHv("Median MAD outside: %f %f (~%f stdv)\n", 
               omed, omad, omad*1.4826);
      *mean = omed; *stdv=omad*1.4826;
      free(vvals); vvals=NULL;
      SUMA_free(vmask); vmask=NULL;
   }
   
   /* put mask in dset */
   SUMA_LH("Coercing");
   EDIT_coerce_scale_type(DSET_NVOX(raiset), 0.0, MRI_byte, cmask, 
                    DSET_BRICK_TYPE(raiset,0), DSET_BRICK_ARRAY(raiset,0));          if (DSET_BRICK_TYPE(raiset,0) != MRI_byte) SUMA_free(cmask); cmask=NULL;
   
   
   /* reorient */
   SUMA_LH("Reorient");
   dbuf = r_new_resam_dset(raiset, dset, 0.0, 0.0, 0.0, 
                           NULL, RESAM_NN_TYPE, NULL, 1, 0);        
   cmask = (byte *)SUMA_calloc(DSET_NVOX(dset),sizeof(byte));
   memcpy(cmask, DSET_BRICK_ARRAY(dbuf,0), sizeof(byte)*DSET_NVOX(dset));
   if (LocalHead) {
      SUMA_LH("Writing reoriented mask");
      DSET_write(dbuf);
   }
   DSET_delete(dbuf); dbuf=NULL;
   DSET_delete(raiset); raiset=NULL;
   
   if (cmaskp) *cmaskp=cmask;
   else SUMA_free(cmask); 
   cmask=NULL;
   
   SUMA_RETURN(YUP);
}



#define   within_Nvox_From_Edge(dset,ii,jj,kk,w) (\
   (ii < w-1 || ii > DSET_NX(dset)-w || \
    jj < w-1 || jj > DSET_NY(dset)-w || \
    kk < w-1 || kk > DSET_NZ(dset)-w ) ? 1 : 0)

/*
   Transform the anatomical dataset as to enhnace voxels at 
   the perimeter of the head */
int SUMA_THD_Radial_HeadBoundary( THD_3dim_dataset  *dset, float uthr,
                           byte *cmask, float *ucm,
                           THD_3dim_dataset **osetp,
                           byte zeropad, 
                           float under, float over, 
                           float avgwinthick, int arcfill,
                           float *Rcmstats, float *Rnzstats)
{
   static char FuncName[]={"SUMA_THD_Radial_HeadBoundary"};
   THD_3dim_dataset *oset=NULL;
   int ii, jj, kk, vv, trv[2], sb, cmijk, doubavgwin, tripavgwin,
       *ioversh=NULL, *iundersh=NULL, si, trvoff[2],
       avgwin, IJK[3];
   THD_fvec3 ccc, ncoord;
   float cm[3], cmdic[3], *mo=NULL, *mu=NULL, *fin=NULL, *mr=NULL, *fedges=NULL,
         xyz_ijk[3], factor, mvoxd, means[3], cmstats[5], P[3], 
         *vxZ=NULL, *vxNZ=NULL,
         *oversh = NULL, *undersh = NULL, voxZ, voxNZ, nzmean, nzstdv;
   byte *ok=NULL;
   char stmp[36];
   MRI_IMAGE *dsim=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!osetp || !dset) SUMA_RETURN(NOPE);
   if (!ucm) {
      ccc = THD_cmass(dset, 0, cmask,0);
      cm[0] = ccc.xyz[0];
      cm[1] = ccc.xyz[1];
      cm[2] = ccc.xyz[2];
   } else {
      cm[0] = ucm[0];
      cm[1] = ucm[1];
      cm[2] = ucm[2];
   }

   
   /* change cm to mm coords */
   cmdic[0]=cm[0]; cmdic[1]=cm[1]; cmdic[2]=cm[2];
   SUMA_THD_dicomm_to_3dfind(dset,cm[0],cm[1],cm[2],cm);
   cmijk = (int)cm[0]+(int)cm[1]*DSET_NX(dset)+
                      (int)cm[2]*DSET_NX(dset)*DSET_NY(dset);
   
   /* Get some stats on voxels outside the brain */
   SUMA_CrudeNonHeadMask(dset, cmdic, NULL, &nzmean, &nzstdv);
   SUMA_S_Notev("Outside head: mean %f, stdv %f\n", nzmean, nzstdv);
   if (Rnzstats) { Rnzstats[0]=nzmean; Rnzstats[1]=nzstdv; }
   
   /* min voxel dim */
   mvoxd = SUMA_MIN_PAIR(SUMA_ABS(DSET_DX(dset)), SUMA_ABS(DSET_DY(dset)));
   mvoxd = SUMA_MIN_PAIR(mvoxd, SUMA_ABS(DSET_DZ(dset)));
   
   if (avgwinthick < 1) avgwinthick = 5.0;  /* skull thickness about 5mm */
   avgwin = avgwinthick/mvoxd; 
   if (avgwin < 2) { /* low res voxels, for fast debugging only */
      SUMA_S_Warn("Low res? Forcing min win of 2");
      avgwinthick = 2*mvoxd;
      avgwin = 2;
   }
   doubavgwin = 2*avgwin; tripavgwin = 3.0*avgwin;
   if (under < 1) under = 3.0*avgwinthick;
   if (over < 1) over = 3.1*avgwinthick;
   if (under < 3.0*avgwinthick || over < 2.0*avgwinthick) {
      SUMA_S_Err("Under and Over distances should be > (3.0,2.0)*avgwinthick");
      SUMA_RETURN(NOPE);
   }
   trv[0] = under / mvoxd; trv[1] = over / mvoxd; 
   trvoff[0] = 0; trvoff[1] = 0;
   
   if (avgwin > trv[0] || avgwin > trv[1]) {
      SUMA_S_Err("Averaging window longer than shish");
      SUMA_RETURN(NOPE);
   }
  
   /* get voxel values */
   dsim = THD_extract_float_brick(0, dset);
   fin = MRI_FLOAT_PTR(dsim);
   mu = (float*)SUMA_calloc(DSET_NVOX(dset), sizeof(float));
   mo = (float*)SUMA_calloc(DSET_NVOX(dset), sizeof(float));
   mr = (float*)SUMA_calloc(DSET_NVOX(dset), sizeof(float));
   ok = (byte*)SUMA_calloc(DSET_NVOX(dset), sizeof(byte));
    vxZ = (float*)SUMA_calloc(DSET_NVOX(dset), sizeof(float));
   vxNZ = (float*)SUMA_calloc(DSET_NVOX(dset), sizeof(float));
   undersh = (float*)SUMA_calloc(trv[0]+2, sizeof(float));
   oversh  = (float*)SUMA_calloc(trv[1]+2, sizeof(float));
   iundersh = (int *)SUMA_calloc(trv[0]+2, sizeof(int));
   ioversh  = (int*)SUMA_calloc(trv[1]+2, sizeof(int));


   /* compute some stats around the center of mass */
   {
      float *nbar=NULL;
      int nbar_num;
      MCW_cluster *nbhd=NULL;
      nbhd = MCW_rectmask( SUMA_ABS(DSET_DX(dset)), 
                           SUMA_ABS(DSET_DY(dset)),
                           SUMA_ABS(DSET_DZ(dset)), 
                           40, 40, 40 ) ;
      nbar = (float*)SUMA_calloc(nbhd->num_pt, sizeof(float));
      nbar_num = mri_get_nbhd_array( dsim , cmask, 
                        (int)cm[0], (int)cm[1], (int)cm[2] , nbhd , nbar ) ;
      mri_nstat_mMP2S( nbar_num , nbar, fin[cmijk], cmstats ) ;
      SUMA_free(nbar); nbar = NULL;
      KILL_CLUSTER(nbhd); nbhd = NULL;
      
      cmstats[3] = 1.4826*cmstats[3];     /* turn MAD to stdv */
      SUMA_S_Notev("cmijk=[%f %f %f], median %f, stdv from MAD %f\n", 
               cm[0], cm[1], cm[2], cmstats[1], cmstats[3]);
      if (Rcmstats) { Rcmstats[0]=cmstats[1]; Rcmstats[1]=cmstats[3]; }
   }
    
   vv=0;
   for (kk=0; kk<DSET_NZ(dset); ++kk) {
   SUMA_LHv("kk=%d/%d, cmijk=[%f %f %f]\n", 
            kk, DSET_NZ(dset), cm[0], cm[1], cm[2]);
   for (jj=0; jj<DSET_NY(dset); ++jj) {
   for (ii=0; ii<DSET_NX(dset); ++ii) {
      ok[vv] = 0; voxZ=0; voxNZ = 0;
      if (IN_MASK(cmask,vv)) {
         xyz_ijk[0]= ii; xyz_ijk[1]= jj; xyz_ijk[2]= kk;
         if (!SUMA_Vox_Radial_Stats(fin,
                  DSET_NX(dset), DSET_NY(dset), DSET_NZ(dset),
                  xyz_ijk, cm, trv, trvoff,
                  means, 
                  undersh, oversh, iundersh, ioversh, zeropad)) {
            SUMA_S_Errv("Failed at voxel %d %d %d\n",
                        ii, jj, kk);
            SUMA_RETURN(NOPE);
         }
          voxZ = (means[0]-cmstats[1])/cmstats[3];
         voxNZ = (means[0]-nzmean)/nzstdv;
         vxZ[vv] = voxZ; vxNZ[vv]=voxNZ;
         SUMA_MOVING_SUM(undersh, trv[0], avgwin, 1,1);
         SUMA_MOVING_SUM(oversh, trv[1], avgwin, 1,1);
            /* User threshold, non-zero average and mask belonging 
               are preconditions */ 
            /* A: obviously bright enough*/
            /* B: Steady brain underneath, skipping over space between
                  skull and brain */
            /* C: Average should not be too small compared to signal average
                  around COM */
         if ( oversh[0] != 0.0f && means[0] >= uthr ) {
            if ( (means[0] > cmstats[1]  || (voxZ > -1 && voxNZ > 3)) &&
                 (undersh[doubavgwin]/cmstats[1] > 0.5)  )
            { /* quite bright, and not too far from chunky stuff, accept */
               ok[vv] = 1;   
            } else {
               if ( within_Nvox_From_Edge(dset, ii, jj, kk, doubavgwin)) {
                  /* Right along the boundary of the volume where there 
                    is no information and oversh[avgwin] could be all 0 */
                  if ((oversh[0]/undersh[avgwin]>0.1 &&                      
                         undersh[avgwin]/undersh[doubavgwin] > 0.5 &&
                         undersh[doubavgwin]/cmstats[1] > 0.5)    /* B */ &&
                        (voxZ > -2 && voxNZ > 2)     /* C */ 
                     ) {
                     ok[vv] = 2;
                  } 
               } else {/* far from volume edge be more liberal with intensity */
                  if ((oversh[0]/undersh[avgwin]>0.1 &&                      
                         undersh[avgwin]/undersh[doubavgwin] > 0.5 &&
                         undersh[doubavgwin]/cmstats[1] > 0.5)    /* B */ &&
                        (voxZ > -2.5 && voxNZ > 2.5)     /* C */ 
                     ) {
                     ok[vv] = 3;
                  } else { /* accept voxels not too bright, 
                              but brighter than noise for sure */ 
                     if ( (voxZ > -2.5 && voxNZ > 5) && 
                           (oversh[0]-cmstats[1])/cmstats[3] > -2.5 ) {
                        ok[vv] = 4;      
                     }
                  } 
               } 
            }
            
         }
           
         mu[vv] = oversh[0];
         mo[vv] = oversh[avgwin];
         if (mu[vv] != 0.0f) mr[vv] = (mu[vv]-mo[vv])/(mu[vv]);
         else mr[vv] = 0.0;
      } else {
         mu[vv] = oversh[0];
         mo[vv] = oversh[avgwin];
         mr[vv] = 0.0;
      }
      if (vv == NodeDbg) {
        SUMA_S_Notev("At vox %d have val %f means=[%f %f], rat %f\n"
                     "O(0)=%f, O(w)=%f, U(0)=%f, U(w)=%f, U(2w)=%f\n"
                     "O(0)/U(w)=%f, U(w)/U(2w)=%f,U(2w)/CMavg=%f\n"
                     "voxZ=%f, voxNZ=%f, ok %d, CMstats %f %f\n"
                     "avgwin=%d\n", 
                     vv, means[0], mu[vv], mo[vv], mr[vv], 
                     oversh[0], oversh[avgwin], 
                     undersh[0], undersh[avgwin], undersh[2*avgwin],
                     oversh[0]/undersh[avgwin], 
                        undersh[avgwin]/undersh[doubavgwin],
                           undersh[doubavgwin]/cmstats[1],
                     voxZ, voxNZ, ok[vv], 
                     cmstats[1], cmstats[3], avgwin);
      }
      ++vv;
   }}}
   
   /* get rid of sporadic ok voxels */
   if (1){ /* Get rid of voxels with more than
              mxblnk blanks in their 18 vox. neighborhood 
              Dilate does the opposite */
      byte *okb=NULL;
      int nlp=0, mxblnk=10;
      okb = (byte *)SUMA_calloc(DSET_NVOX(dset), sizeof(byte));
      for (vv=0; vv<DSET_NVOX(dset); ++vv) if (ok[vv]) okb[vv]=1;
      for (nlp=0; nlp<30; ++nlp) {
         THD_mask_erode_sym( DSET_NX(dset), DSET_NY(dset), DSET_NZ(dset), 
                             okb, mxblnk, 2 );
         THD_mask_dilate(DSET_NX(dset), DSET_NY(dset), DSET_NZ(dset), 
                             okb, mxblnk, 2);
      }
      for (vv=0; vv<DSET_NVOX(dset); ++vv) if (okb[vv]) {
                                              if (!ok[vv])  ok[vv] = 5;
                                           } else ok[vv] = 0;
      free(okb); okb=NULL;
   }
   
   /* Now we would like to capture those voxels with high ratio, 
      and a decent intensity that were ignored in the first pass, 
      but you only want to do this in the vicinity of voxels that have 
      already been marked as spectacular */
   if (arcfill) { /* works, but slow */
      DList *hlst=NULL;
      DListElmt *CurElmt=NULL;
      byte *examined = NULL, *umask=NULL;
      float locmean;
      int N_locint, N_hood, nfix, iter=0, N_u=0, N_in=0, N_n=0,
          vvn[6], NX, 
          NXY, nn;
      WegdeNeighbData *wd=NULL;
      
      NX=DSET_NX(dset); NXY=DSET_NY(dset)*NX;
      umask = (byte *)SUMA_calloc(DSET_NVOX(dset), sizeof(byte));
      iter=0; nfix = 1;
      while (iter < arcfill && nfix > 0) { 
         /* Trim which voxels to consider */
         vv=0; N_u=0;
         for (kk=0; kk<DSET_NZ(dset); ++kk) {
         for (jj=0; jj<DSET_NY(dset); ++jj) {
         for (ii=0; ii<DSET_NX(dset); ++ii) {
            SUMA_THD_3dfind_to_dicomm(dset,ii, jj, kk, P);
            if (  ii && jj && kk && 
                ii<DSET_NX(dset)-1 && 
                jj<DSET_NY(dset)-1 && 
                kk<DSET_NZ(dset)-1 &&
                P[2] > cmdic[2]  /* Don't waste effort on low voxels */) {
            if (IN_MASK(cmask,vv) && mr[vv] > 0.5 && ok[vv] > 0) {
               ++N_in;
               vvn[0]=vv-1  ; vvn[1]=vv+1  ; 
               vvn[2]=vv-NX ; vvn[3]=vv+NX ;
               vvn[4]=vv-NXY; vvn[5]=vv+NXY; 
               N_n=0;
               for (nn=0; nn<6; ++nn) {
                  if ( IN_MASK(cmask,vvn[nn]) ) {
                     voxZ  = (fin[vvn[nn]]-cmstats[1])/cmstats[3];
                     voxNZ = (fin[vvn[nn]]-nzmean)/nzstdv;
                     if ( !ok[vvn[nn]] && mr[vvn[nn]] > 0.5 &&
                          voxNZ > 5) ++N_n; 
                  }
               } 
               if (N_n > 1){ /*  possible neighbors missing */
                  umask[vv]=1; ++N_u;
                  #if 0
                     fprintf(stdout,"%d %d %d\n", ii, jj, kk);
                  #endif
               }
            }
            }
            ++vv;
         }}}

         if (NodeDbg >= 0) umask[NodeDbg] = 1;

         SUMA_S_Notev("Will be considering %d/%d voxel neighborhoods\n", 
                       N_u, N_in);
         nfix = 0; 
         vv=0;
         for (kk=0; kk<DSET_NZ(dset); ++kk) {
         SUMA_LHv("kk=%d/%d, cmijk=[%f %f %f]\n", 
                  kk, DSET_NZ(dset), cm[0], cm[1], cm[2]);
         for (jj=0; jj<DSET_NY(dset); ++jj) {
         for (ii=0; ii<DSET_NX(dset); ++ii) {
            if (IN_MASK(umask,vv) && mr[vv] > 0.5 && ok[vv] > 0 && ok[vv]<10) {
               if (vv == NodeDbg) {
                  SUMA_LHv("Considering hood of voxel %d for upgrades\n",vv);
               }
               /* get some candidate voxels around vv */
               SUMA_THD_3dfind_to_dicomm(dset,ii, jj, kk, P);
               if ((N_hood= SUMA_THD_WedgeNeighborhood( dset, cmask, &examined,
                                            P, cmdic, 5.0, 25.0, &hlst)) > 30) {
                  /* Collect the intensity profile of high edge voxels */
                  N_locint = 0; locmean = 0.0;
                  if ((CurElmt = dlist_head(hlst))) {
                     do {
                        wd = (WegdeNeighbData *)CurElmt->data;
                        voxNZ = (fin[wd->id]-nzmean)/nzstdv;
                        if (mr[wd->id]>0.5 && ok[wd->id] && voxNZ > 5) {
                           locmean += fin[wd->id]; ++N_locint; 
                        }
                     } while ((CurElmt = dlist_next(CurElmt)));

                     /* what is the mean intensity in this hood ? */
                     if (N_locint > 15 && (float)N_locint/(float)N_hood > 0.5) {
                        locmean /= N_locint;
                        /* Now loop over candidates and flag them */
                        CurElmt = dlist_head(hlst);
                        do {
                           /* is it a candidate for an upgrade? */
                           wd = (WegdeNeighbData *)CurElmt->data;
                           if (mr[wd->id]>0.5 && !ok[wd->id]) {
                              voxZ  = (fin[wd->id]-cmstats[1])/cmstats[3];
                              voxNZ = (fin[wd->id]-nzmean)/nzstdv;
                              if ( voxNZ > 5 && 
                                  fin[wd->id]/locmean > 0.5) {
                                 if (vv == NodeDbg) {
                                    SUMA_LHv("Voxel %d upgraded.\n",vv);
                                 }
                                 ok[wd->id] = 10+arcfill+iter;
                                 ++nfix;   
                              }
                           } 
                        } while ((CurElmt = dlist_next(CurElmt)));
                     }
                  }
               }
               dlist_destroy(hlst); SUMA_free(hlst); hlst=NULL;
            }
            ++vv;
         }}}
         SUMA_S_Notev("Brought in %d voxels, iteration %d\n", nfix, iter);

         for (vv=0; vv<DSET_NVOX(dset); ++vv) /* relabel touched up voxels */
            if (ok[vv]>9+arcfill) ok[vv]=5+ok[vv]-(9+arcfill); 
         
         memset (umask, 0, DSET_NVOX(dset)*sizeof(byte));
         ++iter;
      }  
      SUMA_free(umask); umask=NULL;
      SUMA_free(examined); examined = NULL;
      SUMA_free(hlst); hlst=NULL;
   }
   
   /* stick the results in the output */
   if (!*osetp) {
      NEW_SHORTY(dset, 9, FuncName, oset);
      *osetp = oset;
   } else {
      oset = *osetp;
   }
   
   sb = 0;
   factor = EDIT_coerce_autoscale_new(DSET_NVOX(oset), MRI_float, mu, 
                          DSET_BRICK_TYPE(oset,sb), DSET_BRICK_ARRAY(oset,sb));
   if (factor > 0.0f) {
      factor = 1.0 / factor;
   } else factor = 0.0;
   EDIT_BRICK_FACTOR (oset, sb, factor);
   EDIT_BRICK_LABEL (oset, sb, "UnderMean");
   
   sb = 1;
   factor = EDIT_coerce_autoscale_new(DSET_NVOX(oset), MRI_float, mo, 
                          DSET_BRICK_TYPE(oset,sb), DSET_BRICK_ARRAY(oset,sb));
   if (factor > 0.0f) {
      factor = 1.0 / factor;
   } else factor = 0.0;
   EDIT_BRICK_FACTOR (oset, sb, factor);
   EDIT_BRICK_LABEL (oset, sb, "OverMean");
   
   sb = 2;
   for (vv=0; vv<DSET_NVOX(dset); ++vv){
      if (mr[vv]>10.0) mr[vv]=10.0;
      else if (mr[vv]<-10.0) mr[vv]=-10.0;
   }
   factor = 100.0;
   EDIT_coerce_scale_type(DSET_NVOX(oset), 100.0, MRI_float, mr, 
                          DSET_BRICK_TYPE(oset,sb), DSET_BRICK_ARRAY(oset,sb));
   EDIT_BRICK_FACTOR (oset, sb, 1/factor);
   EDIT_BRICK_LABEL (oset, sb, "(U-O)/U");
   
   sb = 3;
   EDIT_coerce_scale_type(DSET_NVOX(oset), 0.0, MRI_byte, ok, 
                          DSET_BRICK_TYPE(oset,sb), DSET_BRICK_ARRAY(oset,sb));
   
   factor = 0.0;
   EDIT_BRICK_FACTOR (oset, sb, factor);
   EDIT_BRICK_LABEL (oset, sb, "ok");
   
   /* add the edges for convenience */
   fedges = (float *)SUMA_calloc(DSET_NVOX(dset),  sizeof(float));
   if (!SUMA_3dedge3(dset, fedges, NULL)){
      SUMA_S_Err("Failed to get edges");
      SUMA_free(fedges); fedges = NULL;
   }
   sb = 4;
   factor = EDIT_coerce_autoscale_new(DSET_NVOX(oset), MRI_float, fedges, 
                          DSET_BRICK_TYPE(oset,sb), DSET_BRICK_ARRAY(oset,sb));
   if (factor > 0.0f) {
      factor = 1.0 / factor;
   } else factor = 0.0;
   EDIT_BRICK_FACTOR (oset, sb, factor);
   EDIT_BRICK_LABEL (oset, sb, "Edges");
   SUMA_free(fedges); fedges=NULL;
   
   sb = 5;
   factor = EDIT_coerce_autoscale_new(DSET_NVOX(oset), MRI_float, vxZ, 
                          DSET_BRICK_TYPE(oset,sb), DSET_BRICK_ARRAY(oset,sb));
   if (factor > 0.0f) {
      factor = 1.0 / factor;
   } else factor = 0.0;
   EDIT_BRICK_FACTOR (oset, sb, factor);
   EDIT_BRICK_LABEL (oset, sb, "BrainZ");

   sb = 6;
   factor = EDIT_coerce_autoscale_new(DSET_NVOX(oset), MRI_float, vxNZ, 
                          DSET_BRICK_TYPE(oset,sb), DSET_BRICK_ARRAY(oset,sb));
   if (factor > 0.0f) {
      factor = 1.0 / factor;
   } else factor = 0.0;
   EDIT_BRICK_FACTOR (oset, sb, factor);
   EDIT_BRICK_LABEL (oset, sb, "NoizeZ");

   trvoff[0]=0; trvoff[1]=0;
   SUMA_THD_Radial_Avgs( oset, mr, cmask, cmdic, zeropad, 
                         8.0, 8.0, trvoff,
                         &mu, &mo);
   for (vv=0; vv<DSET_NVOX(oset); ++vv) {
      vxZ[vv] = mu[vv] - mo[vv];
      vxNZ[vv] = -(mu[vv]*mo[vv]);
      if (vxNZ[vv]>100) vxNZ[vv]=100;
      else if (vxNZ[vv]<-100) vxNZ[vv]=-100;
   }
   sb = 7;
   factor = EDIT_coerce_autoscale_new(DSET_NVOX(oset), MRI_float, vxZ, 
                          DSET_BRICK_TYPE(oset,sb), DSET_BRICK_ARRAY(oset,sb));
   if (factor > 0.0f) {
      factor = 1.0 / factor;
   } else factor = 0.0;
   EDIT_BRICK_FACTOR (oset, sb, factor);
   EDIT_BRICK_LABEL (oset, sb, "DeltaRat");
   
   #if 0
   trvoff[0]=0; trvoff[1]=0;
   SUMA_THD_Radial_Avgs( oset, mu, cmask, cmdic, zeropad, 
                         5.0, 5.0, trvoff,
                         &mr, &mo);
   for (vv=0; vv<DSET_NVOX(oset); ++vv) {
      mr[vv] = -(mr[vv] * mo[vv]);
      if (mr[vv]>100) mr[vv]=100;
      else if (mr[vv]<-100) mr[vv]=-100;
   }
   sb = 8;
   factor = EDIT_coerce_autoscale_new(DSET_NVOX(oset), MRI_float, mr, 
                          DSET_BRICK_TYPE(oset,sb), DSET_BRICK_ARRAY(oset,sb));
   if (factor > 0.0f) {
      factor = 1.0 / factor;
   } else factor = 0.0;
   EDIT_BRICK_FACTOR (oset, sb, factor);
   EDIT_BRICK_LABEL (oset, sb, "-ProdDeltaRat");
   #else
   sb = 8;
   factor = EDIT_coerce_autoscale_new(DSET_NVOX(oset), MRI_float, vxNZ, 
                          DSET_BRICK_TYPE(oset,sb), DSET_BRICK_ARRAY(oset,sb));
   if (factor > 0.0f) {
      factor = 1.0 / factor;
   } else factor = 0.0;
   EDIT_BRICK_FACTOR (oset, sb, factor);
   EDIT_BRICK_LABEL (oset, sb, "-ProdRat");
   #endif
   
   mri_free(dsim); dsim = NULL; fin = NULL;
   if (undersh) SUMA_free(undersh); undersh=NULL;
   if (iundersh) SUMA_free(iundersh); iundersh=NULL;
   if (oversh) SUMA_free(oversh); oversh=NULL;
   if (ioversh) SUMA_free(ioversh); ioversh=NULL;
   if (vxZ) SUMA_free(vxZ); vxZ=NULL;
   if (vxNZ) SUMA_free(vxNZ); vxNZ=NULL;
   if (mu) SUMA_free(mu); 
   if (mo) SUMA_free(mo);
   if (mr) SUMA_free(mr); 
   SUMA_RETURN(YUP);
}

int SUMA_Vox_Radial_Stats (float *fvec, int nxx, int nyy, int nzz, 
                        float *xyz_ijk, float *cen_ijk, 
                        int *voxtrav, int *travoff, 
                        float *Means, 
                        float *undershish, float *overshish, 
                        int *fvecind_under, int *fvecind_over, 
                        byte zeropad)
{
   static char FuncName[]={"SUMA_Vox_Radial_Stats"};
   float travdir[3];
   float U[3], Un, X[3], vval; 
   THD_ivec3 nind3;
   int nind, istep, nxy, nMeans[3], nindin, offu=1, offo=1;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!travoff) { offu = 1; offo=1; }
   else { offu=travoff[0]; offo = travoff[1]; }
   
   Means[0] = Means[1] = Means[2] = 0.0;
   nMeans[0] = nMeans[1] = nMeans[2] = 0;
   nxy = nxx*nyy;
   istep = 0; 
   SUMA_UNIT_VEC(xyz_ijk, cen_ijk, U, Un);
   nindin = (int)xyz_ijk[0]+((int)xyz_ijk[1])*nxx+((int)xyz_ijk[2])*nxy;
   Means[0]=fvec[nindin];
   while (istep < voxtrav[0]) {
      /* calculate offset */
      travdir[0] = (istep+offu) * U[0]; 
      travdir[1] = (istep+offu) * U[1]; 
      travdir[2] = (istep+offu) * U[2]; 
            
      /* get index coord of point */
      X[0] = (int)(xyz_ijk[0]+travdir[0]); 
      X[1] = (int)(xyz_ijk[1]+travdir[1]); 
      X[2] = (int)(xyz_ijk[2]+travdir[2]);
      
      vval = 0.0; nind = -1;
      if (X[0] < 0 || X[0] > nxx-1 ||
          X[1] < 0 || X[1] > nyy-1 ||
          X[2] < 0 || X[2] > nzz-1 ) {
         if (!zeropad) break;
      } else {
         nind = (int)X[0] + ((int)X[1]) * nxx + ((int)X[2]) * nxy;
         vval = fvec[nind];
      }
      if (nindin == NodeDbg) {
         SUMA_S_Notev("Down from Voxel %d [%d %d %d], step %d\n"
                              " Xind = [%d %d %d] voxVal = %.3f\n", 
               NodeDbg, 
               (int)xyz_ijk[0], (int)xyz_ijk[1], (int)xyz_ijk[2],
               istep,
               (int)X[0], (int)X[1], (int)X[2],
               vval);
      }
      
      if (undershish) undershish[istep] = vval;
      
      /* track the brain mean */
      Means[1] += vval; ++ nMeans[1]; 
      if (fvecind_under) fvecind_under[istep] = nind;
      
      ++istep;
   }
   
   if (istep < voxtrav[0]) { /* crown the shish */
      if (undershish) undershish[istep] = -1; 
      if (fvecind_under) fvecind_under[istep] = -1; 
   }
   
   if (nMeans[1] > 0) Means[1] /= (float)nMeans[1];
   else Means[1] = 0.0;
   
   /* scan overhead */
   istep = 0; 
   while (istep < voxtrav[1]) {
      /* calculate offset */
      travdir[0] =  -(istep+offu) * U[0];
      travdir[1] =  -(istep+offu) * U[1];
      travdir[2] =  -(istep+offu) * U[2];

      /* get index coord of point */
      X[0] = (int)(xyz_ijk[0]+travdir[0]); 
      X[1] = (int)(xyz_ijk[1]+travdir[1]); 
      X[2] = (int)(xyz_ijk[2]+travdir[2]);
      vval = 0.0; nind = -1;
      if (X[0] < 0 || X[0] > nxx-1 ||
          X[1] < 0 || X[1] > nyy-1 ||
          X[2] < 0 || X[2] > nzz-1 ) {
         if (!zeropad) break;
      } else {
         nind = (int)X[0] + ((int)X[1]) * nxx + ((int)X[2]) * nxy;
         vval = fvec[nind];
      }
      if (nindin == NodeDbg) {
         SUMA_S_Notev("Up from Voxel %d [%d %d %d], step %d\n"
                              " Xind = [%d %d %d] voxVal = %.3f\n", 
               NodeDbg, 
               (int)xyz_ijk[0], (int)xyz_ijk[1], (int)xyz_ijk[2],
               istep,
               (int)X[0], (int)X[1], (int)X[2],
               vval);
      }
      if (overshish) overshish[istep] = vval;   

      Means[2] += vval; ++ nMeans[2]; 
      if (fvecind_over) fvecind_over[istep] = nind;

      ++istep;
   }

   if (istep < voxtrav[1]) { /* crown the shish */
      if (overshish) overshish[istep] = -1; 
      if (fvecind_over) fvecind_over[istep] = -1; 
   }

   if (nMeans[2] > 0) Means[2] /= (float)nMeans[2];
   else Means[2] = 0.0;
   
   SUMA_RETURN(YUP);
}

#define SUMA_INDEX_IN_VOL(X, nxx, nyy, nzz) ( \
        ( X[0] < 0 || X[0] > (nxx-1) ||  \
          X[1] < 0 || X[1] > (nyy-1) ||  \
          X[2] < 0 || X[2] > (nzz-1)  ) ? 0:1   )
/* Get all the values in a dataset along the segment from the 
center of mass to the edge of the volume and passing through 
xyz_ijk. The first value is at the center of mass, the last
is at edge of the dataset 
Unless you send in a point outside of the dataset, you'll
always get one voxel back.
Note that you should allocate enough space in undershish and 
fvecind_under to hold enough voxels covering the longest half
diagonal + 1 
 */
int SUMA_Vox_Radial_Samples (float *fvec, int nxx, int nyy, int nzz, 
                        float *xyz_ijk, float *cen_ijk, 
                        float *shish, 
                        int *fvecind)
{
   static char FuncName[]={"SUMA_Vox_Radial_Samples"};
   float travdir[3];
   float U[3], Un, X[3], vval; 
   THD_ivec3 nind3;
   int nind, istep, igood, nxy, nMeans[3], nindin;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   nxy = nxx*nyy;
   istep = 0; 
   SUMA_UNIT_VEC(cen_ijk, xyz_ijk, U, Un);
   
   if (!SUMA_INDEX_IN_VOL(xyz_ijk, nxx, nyy, nzz)) {
      SUMA_S_Err("Starting point outside volume!");
      SUMA_RETURN(NOPE);
   }
   
   /* Start from the center and go to the edge */
   X[0] = (int)(cen_ijk[0]);
   X[1] = (int)(cen_ijk[1]);
   X[2] = (int)(cen_ijk[2]);
   istep = 0; igood = 0;
   while (SUMA_INDEX_IN_VOL(X, nxx, nyy, nzz)) {
      nind = (int)X[0] + ((int)X[1]) * nxx + ((int)X[2]) * nxy;
      if (igood == 0 || nind != fvecind[igood-1]) {
         if (shish) shish[igood] = fvec[nind];
         if (fvecind) fvecind[igood] = nind;
         ++igood;
      }      
      /* calculate offset to next one*/
      travdir[0] = istep * U[0]; 
      travdir[1] = istep * U[1]; 
      travdir[2] = istep * U[2]; 
            
      /* get index coord of point */
      X[0] = (int)(cen_ijk[0]+travdir[0]); 
      X[1] = (int)(cen_ijk[1]+travdir[1]); 
      X[2] = (int)(cen_ijk[2]+travdir[2]);
      
      
      ++istep;
   }
   
   if (shish) shish[igood] = -1; 
   if (fvecind) fvecind[igood] = -1; 
   
   SUMA_RETURN(igood);
}

void SUMA_FreeWNDatum(void *wnd) { if (wnd) SUMA_free(wnd); return; }

/* 
   Find the voxels in a frustrum centered on P (dicom coords), with
   concentric spheres centered at C.
   Frustrum height (thickness) is t and it spans adeg total.
   
   dset (THD_3dim_dataset *) the dset grid
   mask (byte *) Mask to restrict candidates
   examinedp (byte **) Pointer to reusable mask vector. 
                       if *examinedp==NULL, the function allocates
                       the needed space. Then *examinedp is set to 
                       all zeros and is used to flag voxels that have
                       been examined in this round. Under certain
                       conditions, *examinedp might become the mask
                       for those voxels forming the neighborhood.
   P (float 3) The center of the frustrum
   C (float 3) The center of the two spheres bounding the frustrum
   t (float) the height of the frustrum (difference of spheres' radii)
   adeg (angle) width of frustrum
   Hoodu (DList **) if Hoodu, then *Hoodu is set to contain the linked
                  list of the voxel neighborhood.
                  Otherwise, *examinedp is reinitialized then voxels
                  in the neighborhood are set to 1, so that becomes
                  the output.
   The function returns the number of voxels in the neighborhood.
*/
int SUMA_THD_WedgeNeighborhood(THD_3dim_dataset *dset, byte *mask,
                               byte **examinedp,
                               float *P, float *C,
                               float t, float adeg,
                               DList **Hoodu)
{
   static char FuncName[]={"SUMA_THD_WedgeNeighborhood"};
   int Nhood = -1, iP, iii, nii, njj, nkk, iiC, NX, NY, NXY, NZ, iP3[3];
   DList *HoodList=NULL;
   DListElmt *CurElmt=NULL;
   float uCP[3], rCP, Q[3], rr1, rr2, coshal,
         rrQ, cosaQ;
   byte *examined=NULL;
   WegdeNeighbData *wd=NULL, *cd=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!dset || !C || !P || !examinedp) SUMA_RETURN(Nhood);
   
   if (!*examinedp) {
      SUMA_LH("Allocating for examined");
      *examinedp = (byte*)SUMA_malloc(DSET_NVOX(dset)*sizeof(byte));
   }
   if (!(examined = *examinedp)) {
      SUMA_S_Err("Something bad is afoot");
      SUMA_RETURN(Nhood);
   }
   if (Hoodu && *Hoodu) {
      SUMA_S_Err("Initialize *Hoodu to NULL if you want list back");
      SUMA_RETURN(Nhood);
   }
   NX = DSET_NX(dset); NY = DSET_NY(dset); NZ = DSET_NZ(dset); NXY = NX*NY;
   memset(examined,0,sizeof(byte)*DSET_NVOX(dset));
   HoodList = (DList *)SUMA_malloc(sizeof(DList));
   dlist_init(HoodList, SUMA_FreeWNDatum); 
   
   Nhood = 1;
   SUMA_UNIT_VEC(C,P,uCP,rCP);
   iP = SUMA_THD_dicomm_to_1dind(dset, P[0], P[1], P[2], iP3);
   wd = (WegdeNeighbData *)SUMA_malloc(sizeof(WegdeNeighbData));
   wd->id = iP; wd->ii=iP3[0]; wd->jj=iP3[1]; wd->kk=iP3[2];
   wd->d2 = rCP*rCP; wd->ca = 1.0;
   rr1 = (rCP-t/2.0)*(rCP-t/2.0); rr2 = (rCP+t/2.0)*(rCP+t/2.0);
   coshal = cos(0.5*adeg*SUMA_PI/180.0);
   dlist_ins_next(HoodList, dlist_head(HoodList), wd); examined[iP]=1; 
   CurElmt = dlist_head(HoodList);
   do {
      wd = (WegdeNeighbData *)CurElmt->data;
      for (iii=0; iii<6; ++iii) {
         switch (iii) {
            case 0:
               if ( (nii = wd->ii+1) >= NX ) continue;
               njj = wd->jj; nkk = wd->kk;
               iiC = wd->id+1; /* 1D index of candidate Neighbor*/
               break;
            case 1:
               if ( (nii = wd->ii-1) < 0 ) continue;
               njj = wd->jj; nkk = wd->kk;
               iiC = wd->id-1;
               break;
            case 2:
               if ( (njj = wd->jj+1) >= NY ) continue;
               nii = wd->ii; nkk = wd->kk;
               iiC = wd->id+NX; /* 1D index of candidate Neighbor*/
               break;
            case 3:
               if ( (njj = wd->jj-1) < 0 ) continue;
               nii = wd->ii; nkk = wd->kk;
               iiC = wd->id-NX; 
               break;
            case 4:
               if ( (nkk = wd->kk+1) >= NZ ) continue;
               nii = wd->ii; njj = wd->jj;
               iiC = wd->id+NXY; /* 1D index of candidate Neighbor*/
               break;
            case 5:
               if ( (nkk = wd->kk-1) < 0 ) continue;
               nii = wd->ii; njj = wd->jj;
               iiC = wd->id-NXY; 
               break;
            default:
               SUMA_S_Err("No such neighb");
               SUMA_RETURN(-1);   
         }
         if (!IN_MASK(mask, iiC)) continue;
         if (!examined[iiC]) { /* never been examined */
            SUMA_THD_3dfind_to_dicomm(dset, nii, njj, nkk, Q);
            if (is_in_wedge(P, C, rr1, rr2, coshal, Q, 
                            uCP, &rrQ, &cosaQ)) {
               /* add it to the candidate list */
               cd = (WegdeNeighbData *)SUMA_malloc(sizeof(WegdeNeighbData));
               cd->id = iiC; cd->ii=nii; cd->jj=njj; cd->kk=nkk;
               cd->d2 = rrQ; cd->ca = cosaQ;
               /* and insert it at the bottom of the list */
               dlist_ins_next(HoodList, dlist_tail(HoodList),cd);
               #if 0
               fprintf(stderr,"%d %d %d %d %f %f\n",
                              iiC, nii, njj, nkk, sqrtf(rrQ), cosaQ);
               #endif
               ++Nhood;
            }
            examined[iiC]=1;
         }
      }
      
   } while ((CurElmt = dlist_next(CurElmt)));
   
   if (!Hoodu) {
      /* by default, return results in examined */
      memset(examined,0,sizeof(byte)*DSET_NVOX(dset));
      if ((CurElmt = dlist_head(HoodList))) {
         do {
            wd = (WegdeNeighbData *)CurElmt->data;
            examined[wd->id]=1;
         } while ((CurElmt = dlist_next(CurElmt)));
      }

      /* destroy list*/
      dlist_destroy(HoodList); SUMA_free(HoodList); HoodList=NULL;
   } else {
      *Hoodu = HoodList; HoodList = NULL; 
   }
   SUMA_RETURN(Nhood);
}

/*! 
   \brief creates a crude mask of the brain's limit based on the surface of the skull
   mask is crude, for purposes of keeping BrainWrap from leaking out when there are 
   very strong shading artifacts
*/
int SUMA_SkullMask (SUMA_SurfaceObject *SO, 
                    SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt, 
                    SUMA_COMM_STRUCT *cs)
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
   
   
   /* search for a distance for the biggest negative 
      gradient over a distance of 2 cm */
   sksearch = 15; /* maximum search into skull */
   n_stp = (int)(ceil((double)sksearch/Opt->travstp));
   Npass = 0;  Done = 0; N_it = 1;
   do {
      if (Opt->debug > 1) 
         fprintf(SUMA_STDERR,"%s: Gradient Pass #%d\n", FuncName, Npass);
      for (it=0; it < N_it; ++it) {
         for (in=0; in<SO->N_Node; ++in) {
            n = &(SO->NodeList[3*in]);

            /* using normals for direction, NORMALS SHOULD POINT OUTWARDS...*/
            if (n[2] > -45) { /* Do not touch nodes on bottom */
               U[0] = -SO->NodeNormList[3*in]; 
               U[1] = -SO->NodeNormList[3*in+1]; 
               U[2] = -SO->NodeNormList[3*in+2]; 
               SUMA_ONE_SHISH_PLEASE(n, U, Opt->in_vol, Opt->fvec, Opt->travstp, 
                                     n_stp, nx, nxy, undershish, ShishMax);
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
         memcpy((void*)SO->NodeList, (void *)dsmooth, 
                SO->N_Node * 3 * sizeof(float));
         cs->kth = kth_buf; 
         
         /* recalculate surface normals */
         SUMA_RECOMPUTE_NORMALS(SO); 
         if (cs->Send) {
            if (!SUMA_SendToSuma (SO, cs, (void *)SO->NodeList, 
                                  SUMA_NODE_XYZ, 1)) {
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

SUMA_Boolean SUMA_LimitCoordToVolume(float *NewCoord,          
                                     THD_3dim_dataset *in_volp,
                                     int units,
                                     int *limited)
{
   static char FuncName[]={"SUMA_LimitCoordToVolume"};
   static THD_3dim_dataset *in_vol=NULL;
   static THD_fvec3 bb3dmm0, bb3dmm1;
   static THD_ivec3 bb3dind0, bb3dind1;
   static THD_fvec3 bbRAI0, bbRAI1;  
   float ocoord[3];
   int i;
   byte c;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!in_volp || !NewCoord || !limited) 
      SUMA_RETURN(NOPE);
   
   if (in_vol != in_volp) {
      in_vol = in_volp;
      /* recreate bounding box in dicom */
      bb3dind0.ijk[0] = 0; bb3dind0.ijk[1] = 0; bb3dind0.ijk[2] = 0;
      bb3dind1.ijk[0] = DSET_NX(in_vol)-1; 
      bb3dind1.ijk[1] = DSET_NY(in_vol)-1; 
      bb3dind1.ijk[2] = DSET_NZ(in_vol)-1;
        
      bb3dmm0 = THD_3dind_to_3dmm( in_vol ,  bb3dind0);
      bb3dmm1 = THD_3dind_to_3dmm( in_vol ,  bb3dind1);
      
      bbRAI0 = THD_3dmm_to_dicomm( in_vol, bb3dmm0);
      bbRAI1 = THD_3dmm_to_dicomm( in_vol, bb3dmm1);
      
      if (LocalHead) {
         fprintf(SUMA_STDERR,
                "Bounding Box:\n"
                "%d   %d   %d  -->   %d   %d   %d  units 1 (3dind)\n"
                "%f   %f   %f  -->   %f   %f   %f  units 2 (3dmm)\n"
                "%f   %f   %f  -->   %f   %f   %f  units 3 (RAI)\n",
                bb3dind0.ijk[0], bb3dind0.ijk[1], bb3dind0.ijk[2],
                bb3dind1.ijk[0], bb3dind1.ijk[1], bb3dind1.ijk[2],
                bb3dmm0.xyz[0], bb3dmm0.xyz[1], bb3dmm0.xyz[2],
                bb3dmm1.xyz[0], bb3dmm1.xyz[1], bb3dmm1.xyz[2],
                bbRAI0.xyz[0], bbRAI0.xyz[1], bbRAI0.xyz[2], 
                bbRAI1.xyz[0], bbRAI1.xyz[1], bbRAI1.xyz[2]); 
      }
   }
   
   if (LocalHead) {
      ocoord[0] = NewCoord[0]; 
      ocoord[1] = NewCoord[1]; 
      ocoord[2] = NewCoord[2]; 
   }
   *limited = 0;
   switch (units) {
      case 1:  /* 3dind */
         for (i=0; i<3; ++i) {
            if (NewCoord[i] < bb3dind0.ijk[i]) { 
               *limited = 1; 
               NewCoord[i]= bb3dind0.ijk[i];
            } else if (NewCoord[i] > bb3dind1.ijk[i]) {
               *limited = 1;
               NewCoord[i]= bb3dind1.ijk[i];
            }
         }
         break;
      case 2:  /* 3dmm */
         for (i=0; i<3; ++i) {
            if (NewCoord[i] < bb3dmm0.xyz[i]) {
               *limited = 1;
               NewCoord[i]= bb3dmm0.xyz[i];
            } else if (NewCoord[i] > bb3dmm1.xyz[i]) {
               *limited = 1;
               NewCoord[i]= bb3dmm1.xyz[i];
            }
         }
         break;  
      case 3:  /* RAI */
         for (i=0; i<3; ++i) {
            if (NewCoord[i] < bbRAI0.xyz[i]) {
               *limited = 1;
               NewCoord[i]= bbRAI0.xyz[i];
            } else if (NewCoord[i] > bbRAI1.xyz[i]) {
               *limited = 1;
               NewCoord[i]= bbRAI1.xyz[i];
            }
         }
         break;
      default: /* bad news */
         SUMA_S_Err("Bad units");
         SUMA_RETURN(NOPE);  
         
   }
   if (LocalHead && *limited) {
      fprintf(SUMA_STDERR,"In: %f   %f   %f   units %d\n",
                           ocoord[0], ocoord[1], ocoord[2], units );
      fprintf(SUMA_STDERR,"Out: %f   %f   %f   units %d\n",
                           NewCoord[0], NewCoord[1], NewCoord[2], units );
   }
   
   SUMA_RETURN(YUP); 
}

/*! 
   Main function to stretch a sphere to fit the brain. 
   A modification of BET's algorithm.
*/
int SUMA_StretchToFitLeCerveau (
      SUMA_SurfaceObject *SO, 
      SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt, 
      SUMA_COMM_STRUCT *cs)
{
   static char FuncName[]={"SUMA_StretchToFitLeCerveau"};
   int it=0,it0 = 0, in=0, ii, Npass, Done, ShishMax, 
       i_diffmax, kth_buf, nit, Stage, *fvecind_over, Stage2Type, out;
   float mnc[3], s[3], sn[3], st[3], dp, threshclip, lztfac,
         nsn, rmin, rmax, E, F, su1, su2, su3, su4,  
         r, u1[3], u2[3], u3[3], u[3],
         tm=0.0, t2 = 0.0, t98 = 0.0, Imin, Imax, l=0.0, MinMax[2], 
         MinMax_dist[2],MinMax_over[2], MinMax_over_dist[2], Means[3],
         *n=NULL, tb = 0.0, tbe = 0.0, t = 0.0, Imin_over=0.0, 
         Imin_dist_over=0.0,
         *overshish = NULL, *undershish = NULL, diffmax, *touchup=NULL, *a, 
         P2[2][3], *norm;
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
   
   if (LocalHead) 
      fprintf( SUMA_STDERR,
               "%s: t2 = %f, t = %f, tm = %f, t98 = %f\n", 
               FuncName, t2, t, tm, t98);
   
   
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
                           "#t2 = %f, t = %f, tm = %f, t98 = %f\n", 
            t2, t, tm, t98);
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
   memcpy(  (void*)OrigNodeList, (void *)SO->NodeList, 
            SO->N_Node * 3 * sizeof(float));
   
   Stage2Type = 1; 
   pastarea = 0.0; curarea = 0.0; darea = 0.0, past_N_troub = 0;
   Npass = 0;   Done = 0; nit = Opt->N_it; it0 = 0;
   do {
      Stage = 0; /* reset stage */
      if (Opt->debug > 1) fprintf(SUMA_STDERR,"%s: Pass #%d\n", FuncName, Npass);
      MaxExp = 0.0; /* maximum expansion of any node */
      for (it=it0; it < nit; ++it) {
         SUMA_MEAN_SEGMENT_LENGTH(SO, l);
         if (LocalHead && !(it % 50)) {
            fprintf (SUMA_STDERR,
                     "%s: Iteration %d, l = %f . SO->Center = %f, %f, %f...\n", 
                     FuncName, it, l, 
                     SO->Center[0], SO->Center[1], SO->Center[2]);
            if (Opt->DemoPause == SUMA_3dSS_DEMO_PAUSE) { 
               SUMA_PAUSE_PROMPT("About to start iterating, stage 0"); 
            }
         }         
         if (Opt->var_lzt) {
            if (it <= Opt->N_it) 
               lztfac = SUMA_MAX_PAIR(0.2, (1.2* (float)it / (float)Opt->N_it)); 
                           /* make things expand quickly in the beginning, 
                              to help escape large sections of csf close to outer
                              surface, then tighten grip towards the end */
            else lztfac = 1.2;
         } else lztfac = 1.0;
         MaxExp = 0.0; /* maximum expansion of any node */
         for (in=0; in<SO->N_Node; ++in) {
            MaxNormalExp = -1.0;
            n = &(SO->NodeList[3*in]);
               SUMA_MEAN_NEIGHB_COORD(SO, in, mnc);
               s[0] = mnc[0] - n[0]; s[1] = mnc[1] - n[1]; s[2] = mnc[2] - n[2];
               SUMA_DOTP_VEC(s, &(SO->NodeNormList[3*in]), dp, 3, float, float);
                           /* dp is the dot product of s with the normal at in */
               SUMA_SCALE_VEC(&(SO->NodeNormList[3*in]), sn, 
                              dp, 3, float, float); 
                                    /* sn is the component of s along normal */
               SUMA_NORM_VEC(sn, 3, nsn); /* norm of sn */
               SUMA_SUB_VEC(  s, sn, 
                              st, 3, float, float, float);  
                           /* st is the tangential component of s  */
               SUMA_SCALE_VEC(st, u1, su1, 3, float, float); /* u1 = st * su1 */

               r = ( l * l ) / (2.0 * nsn);
               su2 = ( 1 + tanh(F*(1/r - E)))/2.0;
               SUMA_SCALE_VEC(sn, u2, su2, 3, float, float); /* u2 = sn * su2 */

               SUMA_Find_IminImax(&(SO->NodeList[3*in]), 
                                  &(SO->NodeNormList[3*in]), 
                                  Opt, in, MinMax, MinMax_dist, 
                                  MinMax_over, MinMax_over_dist, Means, 
                                  undershish, overshish, NULL, fvecind_over,  
                                  Opt->d1, Opt->d4, ShishMax);  
               Imin = MinMax[0]; Imax = MinMax[1];

               if (n[2] - SO->Center[2] < -25) { 
                  lZt = SUMA_MAX_PAIR ( Opt->bot_lztclip, 
                                       (Opt->ztv[in] * lztfac)); 
               } /* clip lZt at no less than 0.5 for lowest sections,                                 otherwise you might go down to the dumps */
               else { lZt = Opt->ztv[in] * lztfac;  }

               if (lZt > 1) lZt = 0.999;

               tb = (Imax - t2) * lZt + t2; 
               f3 = 2.0 * (Imin - tb) / (Imax - t2 );

               su3 = Opt->ExpFrac * l * f3 ;

               if (Opt->UseExpansion) {
                  tbe = (MinMax_over[1] - t2) * lZt + t2; 
                  f4 = 2.0 * (MinMax_over[0] - tbe) / (MinMax_over[1] - t2 );
                  su4 = Opt->ExpFrac * l * f4 ; 
                  if (su4 < 0) su4 = 0; /* too tight expanding otherwise */
               } else su4 = 0;

               /* work the eyes */
               if (Opt->NoEyes && !Opt->emask) { /* The no edge way */
                  #if 0
                     if (it > 0.1 * Opt->N_it) { /* The olde, stupid way */
                        if (SUMA_IS_EYE_ZONE(n,SO->Center)) { 
                              /* (n[1] - SO->Center[1]) < -10 && 
                                 (n[2] - SO->Center[2]) < 0 area with the eyes */
                              /* if the mean over is larger than the mean below, 
                                 go easy, likely to hit the eyes */
                           if (!Opt->Stop[in]) {
                              if (Opt->dbg_eyenodes) { 
                                 int kk;
                                 fprintf (Opt->dbg_eyenodes,
                                          "%d\t %f %.3f ", 
                                          in, Means[2], Means[2]/Means[1]); 
                                 for (kk=0; kk<ShishMax; ++kk) 
                                    if (overshish[kk] >= 0) 
                                       fprintf (Opt->dbg_eyenodes,
                                                "%.3f ", overshish[kk]);
                                 fprintf (Opt->dbg_eyenodes,"\n");
                              }
                              if (Means[2] > 1.2*Means[1]) {
                                 SUMA_MAX_SHISH_JUMP( overshish, diffmax, 
                                                      i_diffmax, threshclip);
                                 Opt->Stop[in] = overshish[i_diffmax];
                                 if (LocalHead) 
                                    fprintf (SUMA_STDERR, 
                                             "%s: Stopping node %d, "
                                             "at values > %f\n", 
                                             FuncName, in, Opt->Stop[in]);
                              } else if (Means[0] >= Opt->t98) {
                                 Opt->Stop[in] = Opt->t98;
                                 if (LocalHead) 
                                    fprintf (SUMA_STDERR, 
                                             "%s: Stopping node %d, "
                                             "at values > %f\n", 
                                             FuncName, in, Opt->Stop[in]);
                              }
                           }
                        }
                     }
                     if (Opt->Stop[in]) {
                        if (Means[0] >= Opt->Stop[in]) { su3 = 0; su4 = 0; }
                     }
                  #else 
                     if (it > 0.1 * Opt->N_it) { /* The newer, better way */
                        if (SUMA_IS_EYE_ZONE(n,SO->Center)) { 
                              /* (n[1] - SO->Center[1]) < -10 && 
                                 (n[2] - SO->Center[2]) < 0 area with the eyes */
                           /* if the mean over is larger than the mean below, go 
                              easy, likely to hit the eyes */
                           if (!Opt->Stop[in]) {
                              if (Opt->dbg_eyenodes) { 
                                 int kk;
                                 fprintf (Opt->dbg_eyenodes,
                                          "%d\t %f %.3f ", 
                                          in, Means[2], Means[2]/Means[1]); 
                                 for (kk=0; kk<ShishMax; ++kk) 
                                    if (overshish[kk] >= 0) 
                                       fprintf (Opt->dbg_eyenodes,
                                                "%.3f ", overshish[kk]);
                                 fprintf (Opt->dbg_eyenodes,"\n");
                              }
                              if (Means[2] > 1.2*Means[1]) {
                                 SUMA_MAX_SHISH_JUMP(overshish, diffmax, 
                                                     i_diffmax, threshclip);
                                 MaxNormalExp = Opt->travstp*i_diffmax;
                                 if (LocalHead) 
                                    fprintf (SUMA_STDERR, 
                                             "%s: Stopping node %d, from"  
                                             "expanding beyond %fmm \n", 
                                             FuncName, in, MaxNormalExp );
                              } else if (Means[0] >= Opt->t98) {
                                 MaxNormalExp = 0.0; /* ????? */
                                 if (LocalHead) 
                                    fprintf (SUMA_STDERR, 
                                             "%s: Dunno what to do here :" 
                                             "Stopping node %d, from expanding" 
                                             "beyond %fmm\n", 
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
                     if (SUMA_IS_EYE_ZONE(n,SO->Center)) { 
                           /* (n[1] - SO->Center[1]) < -10 && 
                              (n[2] - SO->Center[2]) < 0 area with the eyes */
                        /* if the mean over is larger than the mean below, go 
                           easy, likely to hit the eyes */
                        if (!Opt->Stop[in]) {
                           if (Opt->dbg_eyenodes) { 
                              int kk;
                              fprintf (Opt->dbg_eyenodes,
                                       "%d\t %f %.3f ", 
                                       in, Means[2], Means[2]/Means[1]); 
                              for (kk=0; kk<ShishMax; ++kk) 
                                 if (overshish[kk] >= 0) 
                                    fprintf (Opt->dbg_eyenodes,
                                             "%.3f ", overshish[kk]);
                              fprintf (Opt->dbg_eyenodes,"\n");
                           }
                           if (Means[2] > 1.2*Means[1]) {
                              /* do we have edges ahead? */
                              /* don't move if a node is too close to an edge */
                              int ie = 0;
                              MaxNormalExp = -1.0;
                              while (MaxNormalExp < 0 && 
                                     fvecind_over[ie] >= 0 && 
                                     Opt->travstp*ie < 2.0) {
                                 if (Opt->emask[fvecind_over[ie]]) {
                                    if (Opt->NodeDbg == in) { 
                                       fprintf( SUMA_STDERR, 
                                                "%s: Eye Node %d will not be"
                                                " allowed to grow, too close to"
                                                " an edge!\n", FuncName, in);
                                    }
                                    MaxNormalExp = Opt->travstp*ie;    
                                    if (LocalHead) 
                                       fprintf (SUMA_STDERR, 
                                                "%s: Edge-Stopping node %d, from"
                                                " expanding beyond %fmm \n", 
                                                FuncName, in, MaxNormalExp );
                                 }
                                 ++ie;
                              }
                           } else if (Means[0] >= Opt->t98) {
                              MaxNormalExp = 0.0; /* ????? */
                              if (LocalHead) 
                                 fprintf (SUMA_STDERR, 
                                          "%s: Edge-Dunno what to do here :" 
                                          "Stopping node %d, from expanding" 
                                          "beyond %fmm\n", 
                                          FuncName, in, MaxNormalExp );
                           }
                        }
                     }
                  }
               }
               /* freezing: a node that has been frozen, 
                  freezing may be used during final touchup*/
               if (Opt->Stop[in] < 0) { 
                  su3 = 0; su4 = 0;   
                  if (in == Opt->NodeDbg) 
                     fprintf( SUMA_STDERR,
                              "%s:\nNode %d has been frozen\n", FuncName, in);
               }
               
               /* slow down growth if it is to go beyond 
                  Maximal Expansion along the normal */
               if (MaxNormalExp >= 0.0) {
                  if (su3+su4 > MaxNormalExp) {
                     if (LocalHead) 
                        fprintf (SUMA_STDERR, 
                                 "%s: Expansion reduced from %fmm to %fmm\n", 
                                 FuncName, su3+su4, MaxNormalExp);
                     su3 = MaxNormalExp; su4 = 0.0;  
                  }
               }
               
               u[0] = su1 * st[0] + su2 * sn[0] + 
                     ( su3 + su4 ) * SO->NodeNormList[3*in]   ;
               u[1] = su1 * st[1] + su2 * sn[1] + 
                     ( su3 + su4 ) * SO->NodeNormList[3*in+1] ;
               u[2] = su1 * st[2] + su2 * sn[2] + 
                     ( su3 + su4 ) * SO->NodeNormList[3*in+2] ; 
               if (LocalHead && (in == Opt->NodeDbg)) { 
                  fprintf(SUMA_STDERR, 
                          "%s: Node %d, iter %d, l %.6f\n", 
                          FuncName, in, it, l);
                  fprintf(SUMA_STDERR, 
                          " MinMaxTb = [%.6f %.6f %.6f]\n", 
                          Imin, Imax, tb);
                  fprintf(SUMA_STDERR, 
                          " MinMaxdist=[%.6f %.6f ]\n", 
                          MinMax_dist[0], MinMax_dist[1]);
                  fprintf(SUMA_STDERR, 
                          " MinMaxover     =[%.6f %.6f ]\n", 
                          MinMax_over[0], MinMax_over[1]);
                  fprintf(SUMA_STDERR, 
                          " MinMaxover_dist=[%.6f %.6f ]\n", 
                          MinMax_over_dist[0], MinMax_over_dist[1]); 
                  fprintf(SUMA_STDERR, 
                          " Means (at, under, over) = [%.6f %.6f %.6f]\n", 
                          Means[0], Means[1], Means[2]); 
                  fprintf(SUMA_STDERR, 
                          " Coord(n )= [%.6f %.6f %.6f], dp = %f\n",   
                          n[0],  n[1],  n[2], dp); 
                  fprintf(SUMA_STDERR, 
                          " Neigh(mnc)=[%.6f %.6f %.6f]\n",   
                          mnc[0],  mnc[1],  mnc[2]); 
                  fprintf(SUMA_STDERR, 
                          " Norm (  )= [%.6f %.6f %.6f]\n",   
                          SO->NodeNormList[3*in], 
                          SO->NodeNormList[3*in+1],
                          SO->NodeNormList[3*in+2]); 
                  fprintf(SUMA_STDERR, 
                          " Disp (s )= [%.6f %.6f %.6f]\n",   
                          s[0],  s[1],  s[2]); 
                  fprintf(SUMA_STDERR, 
                          "      (st)= [%.6f %.6f %.6f]\n",  
                          st[0], st[1], st[2]); 
                  fprintf(SUMA_STDERR, 
                          "      (sn)= [%.6f %.6f %.6f]\n",  
                          sn[0], sn[1], sn[2]); 
                  fprintf(SUMA_STDERR, 
                          "       su = [%.6f %.6f %.6f]\n",  
                          su1, su2, su3); 
                  fprintf(SUMA_STDERR, 
                          "        u = [%.6f %.6f %.6f]\n",  
                          u[0], u[1], u[2]); 
                  if (OutNodeFile) {
                     fprintf(OutNodeFile, 
                              " %d %d  %.6f"
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
                              ,SO->NodeNormList[3*in] 
                              ,SO->NodeNormList[3*in+1]
                              ,SO->NodeNormList[3*in+2]
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
               /* limit to be sure you do not go beyond volume,
               This happened on some Rat data. */
               SUMA_LimitCoordToVolume(&(NewCoord[3*in]), Opt->in_vol, 3, &out);
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
         if (Opt->DemoPause == SUMA_3dSS_DEMO_PAUSE) { 
            SUMA_PAUSE_PROMPT("About to be in stage 1"); 
         }
         if (Opt->debug) fprintf (SUMA_STDERR,"%s: \n In stage 1\n", FuncName);
         if (MaxExp > 0.5) {
            /* Now, if you still have expansion, continue */
            if (!pastarea) { /* first time around, calculate area */
               pastarea = SUMA_Mesh_Area(SO, NULL, -1);
               if (Opt->debug) 
                  fprintf (SUMA_STDERR,
                           "%s: \n Stage1: pastarea = %f\n", FuncName, pastarea);
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
               if (Opt->debug) 
                  fprintf (SUMA_STDERR,
                           "%s: \n Stage1: MaxExp = %f, darea = %f, "
                           "going for more...\n", FuncName, MaxExp, darea);
               Done = 0;
            } else {
               if (Opt->debug) 
                  fprintf (SUMA_STDERR,
                           "%s: \n Stage1: satiated, small area differential.\n",                            FuncName);
               ++Stage;
            }
         } else {
            if (Opt->debug) 
               fprintf (SUMA_STDERR,
                        "%s: \n Stage1: satiated, low MaxExp\n", FuncName);
            ++Stage;
         }
      }
      if (Stage == 2) {
         if (Stage2Type == 1) { 
            if (Opt->DemoPause == SUMA_3dSS_DEMO_PAUSE) { 
               SUMA_PAUSE_PROMPT("About to be in stage 2, type 1"); 
            }
            /* see if there is room for extension */
            touchup = SUMA_Suggest_Touchup(SO, Opt, 4.0, cs, &N_troub);
            if (!touchup) SUMA_SL_Warn("Failed in SUMA_Suggest_Touchup");
            if (!N_troub) { 
               /* No touchup, done */
               ++Stage;
            } else {
                  /* reduce tightness where expansion is needed */
                  if (Opt->debug) {
                     fprintf( SUMA_STDERR,
                              "%s:\n reducing tightness, applying touchup "
                              "with Stage2Type = %d\n", FuncName, Stage2Type);
                  }
                  for (in=0; in<SO->N_Node; ++in) {
                     if (touchup[in]) {
                        if (Opt->NodeDbg == in) 
                           fprintf(SUMA_STDERR,
                                   "%s: Acting on node %d, touchup[in] %f, "
                                   "past shrink_fac = %f\n", 
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
            if (Opt->DemoPause == SUMA_3dSS_DEMO_PAUSE) {   
               SUMA_PAUSE_PROMPT("About to be in stage 2, type 2"); 
            }
            if (Opt->PushToEdge) {
               /* Make a bloody jump */
               N_troub = SUMA_PushToEdge(SO, Opt, 4, cs, 1); 
               SUMA_RECOMPUTE_NORMALS(SO);
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
#define VOX_DBG 0
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
   
   If you just want a mask of the bounding box, it is quite fast
   \sa SUMA_FindVoxelsInSurface
*/
short *SUMA_FindVoxelsInSurface_SLOW (SUMA_SurfaceObject *SO, 
                                     SUMA_VOLPAR *VolPar,
                                     int *N_inp, int boxonly) 
{
   static char FuncName[]={"SUMA_FindVoxelsInSurface_SLOW"};
   short *isin = NULL, *tmpin = NULL;
   int i, N_in, j, k , n, khits, dims[2], N_hits, iii, jjj, niii, ncul;
   float *tmpXYZ=NULL, Center[3], MaxDims[3], MinDims[3], 
         aMaxDims, aMinDims, delta_t;
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
   isin = (short *)SUMA_malloc(VolPar->nx*VolPar->ny*VolPar->nz * sizeof(short));
   if (!tmpXYZ || !isin) {
      SUMA_SL_Crit("Faile to allocate");
      SUMA_RETURN(NULL);
   }
   
   memcpy ((void*)tmpXYZ, (void *)SO->NodeList, SO->N_Node * 3 * sizeof(float));
   
   /* transform the surface's coordinates from RAI to 3dfind */
   SUMA_vec_dicomm_to_3dfind (tmpXYZ, SO->N_Node, VolPar);
   
   /* find the new center and bounding box for the surface in the new 
      coordinate system*/
   SUMA_MIN_MAX_SUM_VECMAT_COL ( tmpXYZ, SO->N_Node, 3, 
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
            if (isin[n] && !boxonly) { 
                        /* inside bounding box, but is it inside  surface ? */
               p0[0] = i; p1[0] = i+1000; p0[1] = p1[1] = j; p0[2] = p1[2] = k; 
               #ifdef Meth1
                  /* works, but slow as a turtle */
                  mti = SUMA_MT_intersect_triangle(p0, p1, tmpXYZ, SO->N_Node, 
                                          SO->FaceSetList, SO->N_FaceSet, mti,0);
                  if (!(mti->N_poshits % 2)) { 
                     /* number of positive direction hits is a multiple of 2 */
                     isin[n] = 1; --N_in; /* 1 marks outside surface but in box*/
                  } else {
                     isin[n] = 2; /* very much inside */
                  }
               #else 
                  dims[0] = 1; dims[1] = 2; /* rays are along x vector, we will 
                        look for intersections with projections on plane y z */
                  tmpin = SUMA_isinpoly(p0, tmpXYZ, SO->FaceSetList, 
                                        SO->N_FaceSet, SO->FaceSetDim, dims, 
                                        &N_hits, tmpin, NULL);
                  N_poshits = 0; sgnref = 0;
                  nfound = 0; cnt = 0;
                  while (nfound < N_hits) {
                     if (tmpin[cnt]) {
                        ++nfound; 
               /* face centroid x coordinate, this contraption works as long as 
               dims is [1 2] and as long as the triangles making up the mesh are 
               comparable in size to voxel resolution. Otherwise, it is 
               not a particularly pleasing approximation. The proper way would be                to do a ray/triangle intersection
               for those hit triangles (use SUMA_MT_isIntersect_Triangle and 
               check for differences in signed distance). 
               But that ends up like the method above with no speed up to write 
               home about NOT WORTH IT*/
                        n1 =  SO->FaceSetList[3*cnt]; 
                        n2 = SO->FaceSetList[3*cnt+1]; 
                        n3 = SO->FaceSetList[3*cnt+2];
                        cx = (tmpXYZ[3*n1] + tmpXYZ[3*n2] + tmpXYZ[3*n3]  )/3; 
                        sgn = SUMA_SIGN(cx - i);
                        if (nfound == 1) {
                           sgnref = sgn;
                        }
                        /* fprintf(SUMA_STDERR, 
                                  "%s: nfound %d: dp %f: Ref %d, sgn %d\n", 
                                  FuncName, nfound, dp, sgnref, sgn); */
                        if (sgnref == sgn) ++N_poshits;
                     }
                     ++cnt;      
                  }
                  if (N_poshits % 2 == 0) { /* dude's outside */ 
                     isin[n] = 0; --N_in;
                     /* if (LocalHead) 
                        fprintf (SUMA_STDERR,"%d %d %d %d hits\n",
                                    i, j, k, N_hits);  */
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
   This function is useful when trying to find all voxels inside a surface
   
   \param NodeIJKlist (float *) the equivalent of SO->NodeList 
                           only in i,j,k indices into VolPar's grid.
                           If NULL, it gets generated internally from 
                           SO->NodeList and VolPar.
   \return isin (short *)  1: voxel contains node
                           2: voxel is in box containing triangle
                           3: voxel is in box containing triangle and is on 
                              the side opposite to the normal 
           See SUMA_SURF_GRID_INTERSECT_OPTIONS for the various possible values

   - The first part of this function is the source for function 
      SUMA_GetVoxelsIntersectingTriangle
   If you find bugs here, fix them there too. 
   
   \sa SUMA_SurfaceIntersectionVolume
   
   Significant bug fixes Oct 2012
*/
short *SUMA_SurfGridIntersect (SUMA_SurfaceObject *SO, float *NodeIJKlistU, 
                               SUMA_VOLPAR *VolPar, int *N_inp, 
                               int fillhole, THD_3dim_dataset *fillmaskset)
{
   static char FuncName[]={"SUMA_SurfGridIntersect"};
   short *isin=NULL;
   byte *ijkmask=NULL, *inmask = NULL, *ijkout = NULL;
   float *p1, *p2, *p3, min_v[3], max_v[3], p[3], dist;
   float MaxDims[3], MinDims[3], SOCenter[3], dxyz[3], dot;
   int nn, nijk, nx, ny, nz, nxy, nxyz, nf, n1, n2, n3, nn3, 
       *voxelsijk=NULL, N_alloc, en;
   int N_inbox, nt, nt3, ijkseed = -1, N_in, N_realloc, isincand;
   byte *fillmaskvec=NULL;
   float *NodeIJKlist=NULL;
   int voxdbg = 101444, dbgnode[]={106, 500, 247}, InAirSpace=0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (SO->FaceSetDim != 3 || SO->NodeDim != 3) {
      SUMA_SL_Err("SO->FaceSetDim != 3 || SO->NodeDim != 3"); 
      SUMA_RETURN(NULL);
   }
   
   nx = VolPar->nx; ny = VolPar->ny; nz = VolPar->nz; 
   nxy = nx * ny; nxyz = nx * ny * nz;
   
   if (!NodeIJKlistU) {
      NodeIJKlist = (float *)SUMA_malloc(SO->N_Node * 3 * sizeof(float));
      memcpy ((void*)NodeIJKlist, (void *)SO->NodeList, 
                  SO->N_Node * 3 * sizeof(float));
      /* transform the surface's coordinates from RAI to 3dfind */
      if (!SUMA_vec_dicomm_to_3dfind (NodeIJKlist, SO->N_Node, VolPar)) {
         SUMA_SL_Err("Failed to effectuate coordinate transform.");
         SUMA_free(NodeIJKlist); NodeIJKlist = NULL;
         SUMA_RETURN(NULL);
      }
   } else {
      NodeIJKlist = NodeIJKlistU;
   #if VOX_DBG
      for (nijk=0; nijk<3; ++nijk) {
         nn = dbgnode[nijk];
         SUMA_S_Notev("User Node %d:\n"
                   "   XYZ: %f %f %f\n",
                   nn, 
                        NodeIJKlist[3*nn  ], 
                        NodeIJKlist[3*nn+1], 
                        NodeIJKlist[3*nn+2]);
      }             
   #endif
      for (nn=0; nn<SO->N_Node ; ++nn) { /* check */
         if (NodeIJKlist[3*nn  ] < 0 || NodeIJKlist[3*nn  ]>= nx ||
             NodeIJKlist[3*nn+1] < 0 || NodeIJKlist[3*nn+1]>= ny || 
             NodeIJKlist[3*nn+2] < 0 || NodeIJKlist[3*nn+2]>= nz ) {
            SUMA_S_Errv("Looks like NodeIJKlist is not in index units.\n"
                        "At node %d, have %f %f %f\n"
                        , nn, 
                        NodeIJKlist[3*nn  ], 
                        NodeIJKlist[3*nn+1], 
                        NodeIJKlist[3*nn+2]);
            SUMA_RETURN(NULL);
         } 
      }
   }

   isin = (short *)SUMA_calloc(nxyz, sizeof(short));
   if (!isin) {
      SUMA_SL_Crit("Failed to allocate");
      SUMA_RETURN(NULL);
   }
   
   #if VOX_DBG
      for (nijk=0; nijk<3; ++nijk) {
         nn = dbgnode[nn];
         SUMA_S_Notev("Node %d:\n"
                   "   XYZ: %f %f %f\n",
                   nn, 
                        NodeIJKlist[3*nn  ], 
                        NodeIJKlist[3*nn+1], 
                        NodeIJKlist[3*nn+2]);
      }             
   #endif
   /* mark the node voxels */
   for (nn=0; nn<SO->N_Node; ++nn) {
      /* find the ijk of each node: */
      nn3 = 3*nn;
      nijk = SUMA_3D_2_1D_index( (int)NodeIJKlist[nn3], 
                                 (int)NodeIJKlist[nn3+1] , 
                                 (int)NodeIJKlist[nn3+2], nx , nxy); 
      if (nijk < nxyz) { 
         if (!isin[nijk]) { isin[nijk] = SUMA_ON_NODE; ++(*N_inp); } 
      }
   }
   
   
   /* cycle through all triangles and find voxels that intersect them */
   N_alloc = 2000; /* expected max. numb. of voxels in triangle's bounding box */
   N_realloc = 0;
   voxelsijk = (int *)SUMA_malloc(sizeof(int)*N_alloc*3);
   if (!voxelsijk) { SUMA_SL_Crit("Failed to Allocate!"); SUMA_RETURN(NULL);  }   
   /* ZSS Pre Oct 2012:
      The dxyz were not being modified to reflect the changing of coordinates
      from XYZ mm to ijk !
      dxyz[0] = VolPar->dx; dxyz[1] = VolPar->dy; dxyz[2] = VolPar->dz;
   */ 
   dxyz[0] = 1.0; dxyz[1] = 1.0; dxyz[2] = 1.0;
   for (nf=0; nf<SO->N_FaceSet; ++nf) {
      n1 = SO->FaceSetList[SO->FaceSetDim*nf]; 
      n2 = SO->FaceSetList[SO->FaceSetDim*nf+1]; 
      n3 = SO->FaceSetList[SO->FaceSetDim*nf+2];
      /* find the bounding box of the triangle */
      p1 = &(NodeIJKlist[3*n1]); 
      p2 = &(NodeIJKlist[3*n2]); 
      p3 = &(NodeIJKlist[3*n3]); 
      SUMA_TRIANGLE_BOUNDING_BOX(p1, p2, p3, min_v, max_v);
      #if VOX_DBG
         if (LocalHead && nf == 5 ) {
            FILE *fout = NULL;
            int i;
            fout = fopen("SUMA_SurfGridIntersect_BB.1D","w");
            if (fout) {
               SUMA_LH("Writing BB for debug...");
               for (i=0; i<3; ++i) {
                  fprintf(fout,"%d po p1 p2: %f %f %f\n", 
                                 nf, p1[i], p2[i], p3[i]);
                  fprintf(fout,"%d min max: %f %f\n", nf, min_v[i], max_v[i]);
               }
               fclose(fout);
            } else {
               SUMA_LH("Failed to write BB for debug...");
            }   
         }
      #endif
      /* quick check of preallocate size of voxelsijk */
      en =( (int)(max_v[0] - min_v[0] + 3) * 
            (int)(max_v[1] - min_v[1] + 3) * 
            (int)(max_v[2] - min_v[2] + 3)); 
      if ( en > N_alloc) {
         ++N_realloc; 
         if (N_realloc > 5) { 
            SUMA_SL_Warn("Reallocating, increase limit to improve speed.\n"
                         "Either triangles too large or grid too small"); 
         }
         N_alloc = 2*en;
         voxelsijk = (int *)SUMA_realloc(voxelsijk, 3*N_alloc*sizeof(int));
         if (!voxelsijk) { 
            SUMA_SL_Crit("Failed to Allocate!"); SUMA_RETURN(NULL); 
         }
      } 
      /* find the list of voxels inhabiting this box */
      N_inbox = N_alloc; /* that's used to tell SUMA_VoxelsInBox how much 
                            is allocated. N_inbox will reflect the number 
                            of voxels in the box on the way back 
                            from SUMA_VoxelsInBox */
      if (!SUMA_VoxelsInBox(voxelsijk, &N_inbox, min_v, max_v)) {
         SUMA_SL_Err("Unexpected error!"); SUMA_RETURN(NULL); 
      }
      if (!N_inbox) { 
         SUMA_SL_Err("Unexpected error, no voxels in box!"); 
         SUMA_RETURN(NULL);  
      }
      
      /* mark these voxels as inside the business */
      for (nt=0; nt < N_inbox; ++nt) {
         nt3 = 3*nt;
         if (voxelsijk[nt3  ] >= 0 && voxelsijk[nt3  ] < nx &&  
             voxelsijk[nt3+1] >= 0 && voxelsijk[nt3+1] < ny &&  
             voxelsijk[nt3+2] >= 0 && voxelsijk[nt3+2] < nz) {
            isincand = 0;
            nijk = SUMA_3D_2_1D_index(voxelsijk[nt3], voxelsijk[nt3+1], 
                                       voxelsijk[nt3+2], nx , nxy);  
            #if VOX_DBG
               if (nijk == voxdbg) {
                  fprintf(SUMA_STDERR,
                          "%s: %d examined, cand %d initial isin[%d] = %d\n", 
                          FuncName, nijk,  isincand, nijk, isin[nijk]);
               }
            #endif
            if (1 || !isin[nijk]) { /* Used to be that if a voxel was tagged 
                           (isin[nijk]), do not tag it again. 
                           But that is not good if a voxel has been tagged 
                           outside for one node but should be tagged inside the 
                           surface for that voxel's intersection with a 
                           triangle or perhaps for containing another node. */ 
               /* what side of the plane is this voxel on ? */
               p[0] = (float)voxelsijk[nt3]; 
               p[1] = (float)voxelsijk[nt3+1]; 
               p[2] = (float)voxelsijk[nt3+2]; 
               
               /* See also SUMA_DIST_FROM_PLANE2 */
               SUMA_DIST_FROM_PLANE(p1, p2, p3, p, dist);
               

               if (dist) { 
                  if ( SUMA_IS_NEG(VolPar->Hand * dist) ) {
                     /* don't label it, just box the outside,
                      SUMA_isVoxelIntersect_Triangle should catch the good ones*/
                  } else {
                     isincand = SUMA_IN_TRIBOX_OUTSIDE; 
                  }
               } 
               
               #if VOX_DBG               
                  if (nijk == voxdbg) {
                     fprintf( SUMA_STDERR,
                              "    after dist (%f to tri #%d nodes %d %d %d) \n"
                              "    hand (%d) inairspace=%d, dot=%f \n"
                              "    check cand = %d\n", 
                                 dist, nf, n1, n2, n3, 
                                 VolPar->Hand, InAirSpace, dot, 
                                 isincand);
                     SUMA_Set_VoxIntersDbg(1);
                  }
               #endif              
               if (1) { /* does this triangle actually intersect this voxel ?*/
                  if (SUMA_isVoxelIntersect_Triangle (p, dxyz, p1, p2, p3)) {
                     if (dist < 0) 
                          isincand = SUMA_INTERSECTS_TRIANGLE_INSIDE;
                     else isincand = SUMA_INTERSECTS_TRIANGLE_OUTSIDE;
                  } 
               }
               #if VOX_DBG               
                  if (nijk == voxdbg) {
                     SUMA_Set_VoxIntersDbg(0);
                     fprintf(SUMA_STDERR,
                             "    after intersection cand = %d\n", isincand);
                  }              
               #endif              
               if (isincand > isin[nijk]) { 
                                 /* voxel has graduated further inwards */
                  if (!isin[nijk]) { ++(*N_inp);   } /* a new baby */
                  isin[nijk] = isincand;
               }
               #if VOX_DBG               
                  if (nijk == voxdbg) {
                     fprintf(SUMA_STDERR,
                             "    final isin[%d] = %d\n", nijk, isin[nijk]);
                  } 
               #endif              
                   
            }
         }
      }
   }
   
   if (LocalHead) {
      FILE *fout = NULL;
      int i, IJK[3];
      fout = fopen("SUMA_SurfGridIntersect_isin1.1D","w");
      if (fout) {
         SUMA_LH("Writing isin for debug...");
         for (i=0; i<nxyz; ++i) {
            if (isin[i]) {
               Vox1D2Vox3D(i, VolPar->nx, VolPar->ny*VolPar->nx, IJK);
               fprintf(fout,"%d %d %d   %d\n", 
                              IJK[0], IJK[1], IJK[2], isin[i]);
            }
         }
         fclose(fout);
      } else {
         SUMA_LH("Failed to write isin for debug...");
      }
   }
   
   if (fillhole >= 0) {
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
      SUMA_MIN_MAX_SUM_VECMAT_COL (NodeIJKlist, SO->N_Node, 3, 
                                    MinDims, MaxDims, SOCenter); 
      SOCenter[0] /= SO->N_Node;  
      SOCenter[1] /= SO->N_Node;   
      SOCenter[2] /= SO->N_Node;
      {
         float u[3], un, p0[3], p1[3];
         int Found = 0, cnt, itry = 0, ijkseed_unmask = -1;
         SUMA_MT_INTERSECT_TRIANGLE *mti = NULL; 

         /* Ray from a node on the surface to the center */
         while (!Found && itry < SO->N_Node) {
            p1[0] = SOCenter[0];    
            p1[1] = SOCenter[1];    
            p1[2] = SOCenter[2];    
            p0[0] = NodeIJKlist[3*itry+0]; 
            p0[1] = NodeIJKlist[3*itry+1]; 
            p0[2] = NodeIJKlist[3*itry+2]; 

            SUMA_UNIT_VEC(p0, p1, u, un);
            SUMA_LHv("Try %d, un=%f...\nP0[%f %f %f] P1[%f %f %f]\n", 
                  itry, un, p0[0], p0[1], p0[2], p1[0], p1[1], p1[2]);
            /* travel along that ray until you find a point inside the 
               surface AND not on the mask */
            Found = 0; cnt = 1;
            while (!Found && cnt <= un) {
               p1[0] = p0[0] + cnt * u[0];
               p1[1] = p0[1] + cnt * u[1];
               p1[2] = p0[2] + cnt * u[2];
               ijkseed = SUMA_3D_2_1D_index(SUMA_ROUND(p1[0]), 
                                            SUMA_ROUND(p1[1]), 
                                            SUMA_ROUND(p1[2]), nx , nxy);
               if (LocalHead) {
                  fprintf(SUMA_STDERR,"%s:\nTrying seed ijk is %f %f %f [%d]\n", 
                                       FuncName, 
                                       p1[0], p1[1], p1[2], ijkseed); 
               }
               mti = SUMA_MT_intersect_triangle(p1, SOCenter, NodeIJKlist, 
                              SO->N_Node, SO->FaceSetList, SO->N_FaceSet, mti,0);
               if (!(mti->N_poshits % 2)) { 
                  /* number of positive direction hits is a multiple of 2 */
                  /* seed is outside */
                  SUMA_LHv("Seed outside N_poshits=%d\n", mti->N_poshits);
               } else {
                  /* seed is inside, is it on the mask ? */
                  if (!ijkmask[ijkseed]) { 
                     SUMA_LH("Seed inside Accepted");
                     Found = YUP; 
                  } else {
                     SUMA_LH("Seed inside but on mask, unmask if needed later");
                     if (ijkseed_unmask < 0) {
                        ijkseed_unmask = ijkseed;
                     }
                  }
               }
               ++cnt;   
            }
            ++itry;
         }
         if (!Found) {
            if (ijkseed_unmask < 0) {
               SUMA_S_Note("Failed to find internal seed.");
               if (isin) SUMA_free(isin); isin = NULL;
               goto CLEAN_EXIT;
            } else {
               ijkseed = ijkseed_unmask;
               ijkmask[ijkseed] = 0;
               Found = YUP;
            }
         }
         if (mti) mti = SUMA_Free_MT_intersect_triangle(mti);
      }
      
      /* hide the mask values that are outside the surface */
      for (nt=0; nt<nxyz; ++nt) { if (ijkout[nt]) isin[nt] = 0; }
      
      if (fillmaskset) {
         fillmaskvec = (byte *)SUMA_malloc(nx*ny*nz*sizeof(byte));
         if (!fillmaskvec) { 
            SUMA_SL_Crit("Failed to allocate fillmaskvec"); 
         }
         /* is this value 0 in the fillmaskset */
         EDIT_coerce_scale_type( nx*ny*nz , DSET_BRICK_FACTOR(fillmaskset,0) ,
                           DSET_BRICK_TYPE(fillmaskset,0), 
                           DSET_ARRAY(fillmaskset, 0) ,      /* input  */
                           MRI_byte , fillmaskvec  ) ;   /* output */
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
         if (fillhole > 0 ){/* not a good idea to keep in SUMA_FillToVoxelMask */
            byte *inmask_prefill=NULL;
            
            /* keep track of original inmask */
            inmask_prefill = (byte *)SUMA_calloc(nx * ny * nz , sizeof(byte));
            memcpy ((void*)inmask_prefill, (void *)inmask, 
                     nx * ny * nz * sizeof(byte));

            if (LocalHead) 
               fprintf(SUMA_STDERR,"%s:\n filling small holes %d...\n", 
                                 FuncName, fillhole);
            /* fill in any isolated holes in mask */
            (void) THD_mask_fillin_once( nx,ny,nz , inmask , fillhole ) ;     
                     /* thd_automask.c */
            /* remove filled holes that overlap with 0 values in mask */
            if (fillmaskvec) {
               for (nt=0; nt<nxyz; ++nt) {
                  if (  inmask_prefill[nt] == 0 && 
                        inmask[nt] == 1 && fillmaskvec[nt] == 0) { 
                           /* if a voxel was not in mask before filling, 
                           then was filled as a hole but it is zero in the 
                           original dset, then fill it not */
                     inmask[nt] = 0;
                     if (LocalHead) 
                        fprintf(SUMA_STDERR,
                              "%s: Shutting fill at %d\n", FuncName, nt);
                  }
               }
            }
            SUMA_free(inmask_prefill); inmask_prefill = NULL;
         }
         /* flag it */
         for (nt=0; nt<nxyz; ++nt) {
            if (inmask[nt] && !isin[nt]) {
               /* it would be nice to fill this value only if 
               the intensity in the spat normed volume is not 0 
               The problem is that you have to transform coordinates back from 
               this dataset back to the spatnormed set*/
                  isin[nt] = SUMA_INSIDE_SURFACE;
            }
         }
      }
      /* now put back values outside the surface but protect values in mask */
      for (nt=0; nt<nxyz; ++nt) { 
         if (ijkout[nt] && !inmask[nt]) isin[nt] = SUMA_IN_TRIBOX_OUTSIDE; 
      }
   }
      
   CLEAN_EXIT:   
   if (ijkout) SUMA_free(ijkout); ijkout = NULL;
   if (ijkmask) SUMA_free(ijkmask); ijkmask = NULL;
   if (inmask) SUMA_free(inmask); inmask = NULL;
   if (voxelsijk) SUMA_free(voxelsijk); voxelsijk = NULL;
   if (!NodeIJKlistU) SUMA_free(NodeIJKlist);
   
   SUMA_RETURN(isin);
   
}               
 
/*!
   \brief finds voxels inside a closed surface. 
         Surface's normals are to point outwards.
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
            fprintf(fdb, "Node %d pre: %f %f %f\n%f %f %f\n\n",
                     i, SO->NodeList[3*i  ], SO->NodeList[3*i+1],
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
            fprintf(fdb, "Node %d post: %f %f %f\n%f %f %f\n\n",
                     i, SO->NodeList[3*i  ], SO->NodeList[3*i+1],
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
         memset(overshish, 0, sizeof(float)*ShishMax);   
            /* a brute force way to quiet a valgrind warning
            about unintialization */
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
                  if (Opt->NodeDbg == in) { 
                     fprintf(SUMA_STDERR, 
                  "%s: Suggest repair for node %d (better min above):\n"   
                  " MinMax     =[%.1f,%.1f] MinMax_dist     = [%.2f,%.2f] \n"   
                  " MinMax_over=[%.1f,%.1f] MinMax_over_dist= [%.2f,%.2f] \n"   
                  " Val at node %.1f, mean below %.1f, mean above %.1f\n"  
                  " zalt= %f, r/2 = %f, Opt->shrink_bias = %f\n",    
                          FuncName, in, 
                          MinMax[0], MinMax[1], MinMax_dist[0], MinMax_dist[1], 
                          MinMax_over[0], MinMax_over[1], 
                          MinMax_over_dist[0], MinMax_over_dist[1], 
                          Means[0], Means[1], Means[2],   
                          a[2] - SO->Center[2], Opt->r/2, Opt->shrink_bias[in]); 
                   } 
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
            if (  (gradovershish[nn-1] < - Opt->tm/3.0 || 
                  (overshish[nn] < Opt->tm/2.0 && gradovershish[nn-1] < 0)) && 
                  !Cross) {  
               /* if you cross a large negative gradient, OR a smaller negative 
                  one from an already low intensity */
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
            qsort(shft_list, SO->FN->N_Neighb[in], sizeof(float), 
                  (int(*) (const void *, const void *)) SUMA_compare_float);
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
void *SUMA_Push_Nodes_To_Hull(SUMA_SurfaceObject *SO, 
                              SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt, 
                              SUMA_COMM_STRUCT *cs, int N_itermax)
{
   static char FuncName[]={"SUMA_Push_Nodes_To_Hull"};
   float U[3], Un, *a, P2[2][3]={ {0.0, 0.0, 0.0}, {0.0, 0.0, 0.0} }, 
         *norm, shft, *b; 
   int *ijk=NULL, nf = 0, N_iter = 0, N_troub, in, N_smooth, inh;
   float *dsmooth=NULL, *refNodeList=NULL, *dirvec = NULL, *diropt=NULL, iscale;
   byte *nmask = NULL;
   SUMA_Boolean exists; 
   void *SO_name_hull;
   
   SUMA_M2M_STRUCT *M2M = NULL;
   SUMA_SurfaceObject *SOhull = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (! (nf = SUMA_qhull_wrap(SO->N_Node, SO->NodeList, &ijk, 1, NULL)) ) {
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
         if (!SUMA_SurfaceMetrics_eng(SOhull, "EdgeList,MemberFace,PolyArea", 
                  NULL, SUMA_MAX_PAIR(0, Opt->debug -1), SUMAg_CF->DsetList)) {
            fprintf(SUMA_STDERR,
                     "Error %s: Error in SUMA_SurfaceMetrics\n", FuncName);
            SUMA_Free_Surface_Object (SOhull); /* freeing leftovers in MF */
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
         SUMA_S_Err("Failed to write surface object.\n");
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
                              SO->N_Node, diropt, 100.0, Opt->NodeDbg, 0);
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
   edopt->clust_rmm = -1.0;
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
   \param emask (float *) a preallocated vector as big as 
                  DSET_NX(inset)*DSET_NY(inset)*DSET_NZ(inset). 
                           This vector will contain the edge values
   \param poutsetp (THD_3dim_dataset **)If not null, it will point to a 
                  dataset that contains the edges.
*/
SUMA_Boolean SUMA_3dedge3(THD_3dim_dataset *inset, 
                         float *emask, THD_3dim_dataset **poutsetp)
{
   static char FuncName[]={"SUMA_3dedge3"};
   THD_3dim_dataset *outset;
   int verb = 1, datum = -1;
   int border[3]={0,0,0};
   int indims[3]={0,0,0};
   float filterCoefs[3] = {1.0, 1.0, 1.0}, **sum = NULL;
   int ii, nx, ny, nz, nval, kk, nwarn=0;
   int nxyz = DSET_NX(inset)*DSET_NY(inset)*DSET_NZ(inset);
   int fscale=0 , gscale=0 , nscale=0 ;
   SUMA_Boolean LocalHead = NOPE;
   recursiveFilterType filterType = ALPHA_DERICHE;  /* BAD BOY ZIAD */
   
   SUMA_ENTRY;
   if (!inset) {
      SUMA_S_Err("NULL dset");
      SUMA_RETURN(0);
   }  
   if (!DSET_ARRAY(inset,0)) {
      SUMA_S_Err("NULL array");
      SUMA_RETURN(0);
   }
   
   SUMA_LH("Load/Call Extract Gradient");
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
       SUMA_LH("Float");
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
         SUMA_LHv("Short %p\n", pp);
         if( fac && verb) {
            INFO_message(
               "Ignoring brick factor of %f for Gradient Extraction", 
                           fac);
         }
         SUMA_LH("Short Extracting");
         if ( Extract_Gradient_Maxima_3D( (void *)pp, SSHORT,
				         emask, FLOAT,
				         indims,
				         border,
				         filterCoefs,
				         filterType ) == 0 ) {
          fprintf( SUMA_STDERR, "ERROR: gradient extraction failed.\n" );
          SUMA_RETURN(0);
         }
         SUMA_LH("Short Extracted");
      }
      break ;

      case MRI_byte:{
         byte *pp = (byte *) DSET_ARRAY(inset,0) ;
         float fac = DSET_BRICK_FACTOR(inset,0)  ;
         SUMA_LH("Byte");
         if( fac && verb) {
            INFO_message(
               "Ignoring brick factor of %f for Gradient Extraction", 
                           fac);
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
      SUMA_LH("Forming output");
      /* user wants an output dset */
      nx   = DSET_NX(inset) ;
      ny   = DSET_NY(inset) ;
      nz   = DSET_NZ(inset) ; nxyz= nx*ny*nz;
      nval = DSET_NVALS(inset) ; /* emask has enough room for one 
                                   sub-brik only */ 

      sum = (float **) malloc( sizeof(float *)*nval ) ;/* array of sub-bricks */
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
