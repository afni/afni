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
*/         
float SUMA_LoadPrepInVol (SUMA_ISOSURFACE_OPTIONS *Opt)
{
   static char FuncName[]={"SUMA_LoadPrepInVol"};
   int i, nxx, nxy, cnt, *isort = NULL,iPercRangeVal[2], tind, znxy;
   THD_fvec3 ncoord, ndicom;
   THD_ivec3 nind3;
   float vol=-1, mass=-1, volmass = -1, *CoordList = NULL;
   double PercRange[2], PercRangeVal[2], *dvec_sort=NULL, *dvec=NULL;
   SUMA_ISINSPHERE IsIn;
   SUMA_Boolean LocalHead = YUP;
   
   SUMA_ENTRY;
   
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
      if (0 && !Opt->t2) { /* No need*/
         fprintf (SUMA_STDERR,"%s: t2 is zero, going for next non zero val\n", FuncName);
         /* search for first non zero entry in sorted vector */
         while (!(dvec_sort[iPercRangeVal[0]])) ++iPercRangeVal[0];
         Opt->t2 = PercRangeVal[0] = dvec_sort[iPercRangeVal[0]];
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
         fprintf (SUMA_STDERR,"%s: Real tm %f\n", FuncName, Opt->tm);
      }
      if (dvec) SUMA_free(dvec); dvec = NULL;
      if (isort) SUMA_free(isort); isort = NULL;
      SUMA_Free_IsInSphere(&IsIn);
      
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
   l /= m_cnt; \
}

/*!
   Based on BET's recipe 
*/
int SUMA_Find_IminImax (SUMA_SurfaceObject *SO, SUMA_ISOSURFACE_OPTIONS *Opt, int ni, float *Iminp, float *Imaxp )
{
   static char FuncName[]={"SUMA_Find_IminImax"};
   float d1, d2, travdir[3], travstp, d1t, d2t, lmin, lmax;
   float t2 = Opt->t2, tm = Opt->tm, t = Opt->t; 
   THD_fvec3 ncoord, ndicom;
   THD_ivec3 nind3;
   int nind, istep, istepmax, istep2max, nxx, nxy;
   SUMA_ENTRY;
   
   d1 = Opt->d1; d2 = d1/2.0; 
   travstp = 1.0;
   
   lmin = Opt->tm;
   lmax = Opt->t;
   nxx = DSET_NX(Opt->in_vol);
   nxy = DSET_NX(Opt->in_vol) * DSET_NY(Opt->in_vol);
   istep = 0; istepmax = (int)(ceil((double)d1/travstp)); istep2max = (int)(ceil((double)d2/travstp));
   while (istep <= istepmax) {
      /* calculate offset */
      travdir[0] = - istep * travstp * SO->NodeNormList[3*ni]; travdir[1] = -istep * travstp * SO->NodeNormList[3*ni+1]; travdir[2] = -istep * travstp * SO->NodeNormList[3*ni+2]; 
      
      /* get 1d coord of point */
      ndicom.xyz[0] = SO->NodeList[3*ni] + travdir[0] ; ndicom.xyz[1] = SO->NodeList[3*ni+1]+ travdir[1]; ndicom.xyz[2] = SO->NodeList[3*ni+2]+ travdir[2]; 
      ncoord = THD_dicomm_to_3dmm(Opt->in_vol, ndicom);
      nind3 = THD_3dmm_to_3dind(Opt->in_vol, ncoord);
      nind = nind3.ijk[0] + nind3.ijk[1] * nxx + nind3.ijk[2] * nxy;
      if (ni == Opt->NodeDbg) {
         fprintf(SUMA_STDERR, "%s: Node %d\n"
                              " nind3 = [%d %d %d] voxVal = %.3f\n", 
                              FuncName, Opt->NodeDbg, nind3.ijk[0], nind3.ijk[1], nind3.ijk[2],
                              Opt->dvec[nind]);
      }
      /* find local min */ 
      lmin = SUMA_MIN_PAIR(lmin, Opt->dvec[nind]);
      if (istep <= istep2max) {
         lmax = SUMA_MAX_PAIR(lmax, Opt->dvec[nind]);  
      }
      
      ++istep;
   }
   
   
   *Iminp = SUMA_MAX_PAIR(t2, lmin);
   *Imaxp = SUMA_MIN_PAIR(tm, lmax); 
   
   SUMA_RETURN(YUP);
}

/*!
   Based on BET's recipe 
*/
int SUMA_StretchToFitLeCerveau (SUMA_SurfaceObject *SO, SUMA_ISOSURFACE_OPTIONS *Opt, SUMA_COMM_STRUCT *cs)
{
   static char FuncName[]={"SUMA_StretchToFitLeCerveau"};
   int it=0, in=0, ii;
   float mnc[3], s[3], sn[3], st[3], dp, 
         nsn, rmin, rmax, E, F, su1, su2, su3,  
         r, u1[3], u2[3], u3[3], u[3],
         tm=0.0, t2 = 0.0, t98 = 0.0, Imin, Imax, l,
         *n=NULL, tb = 0.0, t = 0.0;
   float *tmpptr, *NewCoord = NULL, f3;
   FILE *OutNodeFile = NULL;
   SUMA_Boolean DoDbg=NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   
   t2 = Opt->t2; t98 = Opt->t98; tm = Opt->tm; t = Opt->t;
   
   if (LocalHead) fprintf(SUMA_STDERR,"%s: t2 = %f, t = %f, tm = %f, t98 = %f\n", FuncName, t2, t, tm, t98);
   
   NewCoord = (float *)SUMA_malloc(3*SO->N_Node*sizeof(float));
   if (!NewCoord) {
      SUMA_SL_Crit("Could not allocate for temp vector.");
      SUMA_RETURN(NOPE);
   }
   rmin = 3.33; rmax = 10.0;  E = (1/rmin + 1/rmax)/2; F = 6/(1/rmin - 1/rmax); 
   su1 = 0.5; 
   
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
   for (it=0; it < Opt->N_it; ++it) {
      SUMA_MEAN_SEGMENT_LENGTH(SO, l);
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Iteration %d, l = %f...\n", FuncName, it, l);
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
         
         SUMA_Find_IminImax(SO, Opt, in, &Imin, &Imax);  

         tb = (Imax - t2) * Opt->Zt + t2; 
         f3 = 2.0 * (Imin - tb) / (Imax - t2 );
         
         
         su3 = Opt->ExpFrac * l * f3 ;
         
         u[0] = su1 * st[0] + su2 * sn[0] + su3 * SO->NodeNormList[3*in] ;
         u[1] = su1 * st[1] + su2 * sn[1] + su3 * SO->NodeNormList[3*in+1] ;
         u[2] = su1 * st[2] + su2 * sn[2] + su3 * SO->NodeNormList[3*in+2] ; 
         if ((in == Opt->NodeDbg)) { 
            fprintf(SUMA_STDERR, "%s: Node %d, iter %d, l %.6f\n", FuncName, in, it, l);
            fprintf(SUMA_STDERR, " MinMaxTb = [%.6f %.6f %.6f]\n", Imin, Imax, tb);  
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
                                    ,Imin, Imax, tb
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
      /* recalculate surface normals */
      SUMA_RECOMPUTE_NORMALS(SO);
      if (cs->Send) {
         if (!SUMA_SendToSuma (SO, cs, (void *)SO->NodeList, SUMA_NODE_XYZ, 1)) {
            SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
         }
      }
   } /* loop over number of iterations */
   if (OutNodeFile) fclose(OutNodeFile); OutNodeFile = NULL;
   if (NewCoord) SUMA_free(NewCoord); NewCoord = NULL;
   SUMA_RETURN(YUP);
}

#ifdef SUMA_BrainWrap_STANDALONE
void usage_SUMA_BrainWrap (SUMA_GENERIC_ARGV_PARSE *ps)
   {
      static char FuncName[]={"usage_SUMA_BrainWrap"};
      char * s = NULL, *sio=NULL, *st = NULL;
      int i;
      s = SUMA_help_basics();
      sio  = SUMA_help_IO_Args(ps);
      printf ( "\n"
               "Usage: A program to find the convex hull of a set of points.\n"
               "  This program is a wrapper for the Qhull program.\n"
               "  see copyright notice by running suma -sources.\n"
               "\n"
               "  BrainWrap  \n"
               "     usage 1: < -input VOL >\n"
               "              < -isoval V | -isorange V0 V1 | -isocmask MASK_COM >\n"
               "              [<-xform XFORM>]\n"
               "     usage 2: < i_TYPE input surface >\n"
               "              [<-sv SURF_VOL>]\n"
               "     usage 3: < -input_1D XYZ >\n"      
               "     common optional:\n"
               "              [< -o_TYPE PREFIX>]\n"
               "              [< -debug DBG >]\n"  
               "\n"
               "  Mandatory parameters, choose one of three usage modes:\n"
               "  Usage 1:\n"
               "     You must use one of the following two options:\n"
               "     -input VOL: Input AFNI (or AFNI readable) volume.\n"
               "     You must use one of the following iso* options:\n"
               "     -isoval V: Create isosurface where volume = V\n"
               "     -isorange V0 V1: Create isosurface where V0 <= volume < V1\n"
               "     -isocmask MASK_COM: Create isosurface where MASK_COM != 0\n"
               "        For example: -isocmask '-a VOL+orig -expr (1-bool(a-V))' \n"
               "        is equivalent to using -isoval V. \n"
               "     NOTE: -isorange and -isocmask are only allowed with -xform mask\n"
               "            See -xform below for details.\n"
               "\n"
               "  Usage 2:\n"
               "     -i_TYPE SURF:  Use the nodes of a surface model\n"
               "                    for input. See help for i_TYPE usage\n"
               "                    below.\n"
               "\n"
               "  Usage 3:\n"
               "     -input_1D XYZ: Construct the convex hull of the points\n"
               "                    contained in 1D file XYZ. If the file has\n"
               "                    more than 3 columns, use AFNI's [] selectors\n"
               "                    to specify the XYZ columns.\n"
               "\n" 
               "  Optional Parameters:\n"
               "     Usage 1 only:\n"
               "     -xform XFORM:  Transform to apply to volume values\n"
               "                    before searching for sign change\n"
               "                    boundary. XFORM can be one of:\n"
               "            mask: values that meet the iso* conditions\n"
               "                  are set to 1. All other values are set\n"
               "                  to -1. This is the default XFORM.\n"
               "            shift: subtract V from the dataset and then \n"
               "                   search for 0 isosurface. This has the\n"
               "                   effect of constructing the V isosurface\n"
               "                   if your dataset has a continuum of values.\n"
               "                   This option can only be used with -isoval V.\n"
               "            none: apply no transforms. This assumes that\n"
               "                  your volume has a continuum of values \n"
               "                  from negative to positive and that you\n"
               "                  are seeking to 0 isosurface.\n"
               "                  This option can only be used with -isoval 0.\n"
               "     Usage 2 only:\n"
               "     -sv SURF_VOL: Specify a surface volume which contains\n"
               "                   a transform to apply to the surface node\n"
               "                   coordinates prior to constructing the \n"
               "                   convex hull.\n"
               "     All Usage:\n"
               "     -o_TYPE PREFIX: prefix of output surface.\n"
               "        where TYPE specifies the format of the surface\n"
               "        and PREFIX is, well, the prefix.\n"
               "        TYPE is one of: fs, 1d (or vec), sf, ply.\n"
               "        Default is: -o_ply \n"
               "\n"
               "%s"
               "\n"
               /*"     -debug DBG: debug levels of 0 (default), 1, 2, 3.\n"
               "        This is no Rick Reynolds debug, which is oft nicer\n"
               "        than the results, but it will do.\n"
               "\n" */
               "%s"
               "\n", sio, s);
       SUMA_free(s); s = NULL; SUMA_free(st); st = NULL; SUMA_free(sio); sio = NULL;          
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
   Opt->Zt = 0.5;
   Opt->ExpFrac = 0.01;
   Opt->N_it = 200;
   Opt->Icold = 50;
   Opt->NodeDbg = -1;
   Opt->t2 = Opt->t98 = Opt->t = Opt->tm = -1;
   Opt->r = 0;
   Opt->d1 = 20;
	brk = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_SUMA_BrainWrap(ps);
          exit (0);
		}
		
		SUMA_SKIP_COMMON_OPTIONS(brk, kar);
      
      if (!brk && (strcmp(argv[kar], "-xform") == 0)) {
         kar ++;
         if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -xform \n");
				exit (1);
			}
         if (!strcmp(argv[kar], "mask")) {
            Opt->xform = SUMA_ISO_XFORM_MASK;
         } else if (!strcmp(argv[kar], "none")) {
            Opt->xform = SUMA_ISO_XFORM_NONE;
         } else if (!strcmp(argv[kar], "shift")) {
            Opt->xform = SUMA_ISO_XFORM_SHIFT;
         }else {
            fprintf (SUMA_STDERR, "%s is a bad parameter for -xform option. \n", argv[kar]);
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
      
      if (!brk && (strcmp(argv[kar], "-d1") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -d1 \n");
				exit (1);
			}
			Opt->d1 = atof(argv[kar]);
         brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-bf") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -bf \n");
				exit (1);
			}
			Opt->Zt = atof(argv[kar]);
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
      
      if (!brk && (strcmp(argv[kar], "-isocmask") == 0)) {
         if (Opt->MaskMode != SUMA_ISO_UNDEFINED) {
            fprintf (SUMA_STDERR, "only one masking mode (-iso*) allowed.\n");
         }
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -isocmask \n");
				exit (1);
			}
			Opt->cmask = argv[kar];
         Opt->MaskMode = SUMA_ISO_CMASK;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-isoval") == 0)) {
         if (Opt->MaskMode != SUMA_ISO_UNDEFINED) {
            fprintf (SUMA_STDERR, "only one masking mode (-iso*) allowed.\n");
         }
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -isoval \n");
				exit (1);
			}
			Opt->v0 = atof(argv[kar]);
         Opt->MaskMode = SUMA_ISO_VAL;
			brk = YUP;
		}
      
      if (!brk && (strcmp(argv[kar], "-isorange") == 0)) {
         if (Opt->MaskMode != SUMA_ISO_UNDEFINED) {
            fprintf (SUMA_STDERR, "only one masking mode (-iso*) allowed.\n");
         }
         kar ++;
			if (kar+1 >= argc)  {
		  		fprintf (SUMA_STDERR, "need 2 arguments after -isorange \n");
				exit (1);
			}
			Opt->v0 = atof(argv[kar]);kar ++;
         Opt->v1 = atof(argv[kar]);
         Opt->MaskMode = SUMA_ISO_RANGE;
         if (Opt->v1 < Opt->v0) {
            fprintf (SUMA_STDERR, "range values wrong. Must have %f <= %f \n", Opt->v0, Opt->v1);
				exit (1);
         }
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
   
   if (ps->i_N_surfnames) {
      if (Opt->in_name || Opt->in_1D) {
         fprintf (SUMA_STDERR,"Error %s:\nOptions -i_TYPE, -input and -input_1D are mutually exclusive.\n", FuncName);
         exit(1);
      }
   }
   
   if (Opt->in_name) {
      if (ps->i_N_surfnames !=0 || Opt->in_1D) {
         fprintf (SUMA_STDERR,"Error %s:\nOptions -i_TYPE, -input and -input_1D are mutually exclusive.\n", FuncName);
         exit(1);
      }
   }
   
   if (Opt->in_1D) {
      if (ps->i_N_surfnames !=0 || Opt->in_name) {
         fprintf (SUMA_STDERR,"Error %s:\nOptions -i_TYPE, -input and -input_1D are mutually exclusive.\n", FuncName);
         exit(1);
      }
   }
   
   if (Opt->in_name && Opt->obj_type >= 0) {
      fprintf (SUMA_STDERR,"Error %s:\nOptions -input and -shape are mutually exclusive.\n", FuncName);
      exit(1);
   }
   if ((!Opt->in_name && Opt->obj_type < 0) && (!Opt->in_1D && !ps->i_N_surfnames)) {
      fprintf (SUMA_STDERR,"Error %s:\nEither -input or -input_1D or -i_TYPE options must be used.\n", FuncName);
      exit(1);
   }
   
   if (Opt->in_1D || ps->i_N_surfnames) {
      if (Opt->MaskMode != SUMA_ISO_UNDEFINED) {
         fprintf (SUMA_STDERR,"Error %s:\nCannot use -iso* options with either -input_1D or -i_TYPE options.\n", FuncName);
         exit(1);
      }
      if (Opt->xform != SUMA_ISO_XFORM_MASK) {
         fprintf (SUMA_STDERR,"Error %s:\nCannot use -xform option with either -input_1D or -i_TYPE options.\n", FuncName);
         exit(1);
      }
   }
   
   if (!Opt->out_prefix) Opt->out_prefix = SUMA_copy_string("brainwrap_out");
   
   if (Opt->xform == SUMA_ISO_XFORM_NONE) {
      if (Opt->v0 != 0) {
         fprintf (SUMA_STDERR,"Error %s: Bad %f isovalue\nWith -xform none you can only extract the 0 isosurface.\n(i.e. -isoval 0)\n", FuncName, Opt->v0);
         exit(1);
      }
      if (Opt->MaskMode != SUMA_ISO_VAL) {
         fprintf (SUMA_STDERR,"Error %s: \nWith -xform none you can only use -isoval 0\n", FuncName);
         exit(1);
      }
   }
   
   if (Opt->xform == SUMA_ISO_XFORM_SHIFT) {
      if (Opt->MaskMode != SUMA_ISO_VAL) {
         fprintf (SUMA_STDERR,"Error %s: \nWith -xform shift you can only use -isoval val\n", FuncName);
         exit(1);
      }
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
   static char FuncName[]={"BrainWrap"}; 
	int i, i3;
   void *SO_name=NULL;
   float vol;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_ISOSURFACE_OPTIONS *Opt;  
   char  stmp[200];
   SUMA_Boolean exists = NOPE;
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   SUMA_Boolean LocalHead = YUP;

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
   SO_name = SUMA_Prefix2SurfaceName(Opt->out_prefix, NULL, NULL, Opt->SurfFileType, &exists);
   if (exists) {
      fprintf(SUMA_STDERR,"Error %s:\nOutput file(s) %s* on disk.\nWill not overwrite.\n", FuncName, Opt->out_prefix);
      exit(1);
   }
   
   /* Load the AFNI volume 
   This volume must have already been cleaned up by SpatNorm */
   vol = SUMA_LoadPrepInVol (Opt);
   if ( vol <= 0 ) {
      SUMA_S_Err("Failed to load/prep volume");
      exit(1);
   } else {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Got me a volume of %f mm3\n", FuncName, vol);
   }
   
   
   /* Now create that little sphere that is about to expand */
   SO = SUMA_CreateIcosahedron (Opt->r/2.0, Opt->Icold, Opt->cog, "n", 1);
   if (!SO) {
      SUMA_S_Err("Failed to create Icosahedron");
      exit(1);
   }
   
   /* need sv for communication to AFNI */
   SO->VolPar = SUMA_VolPar_Attr (Opt->in_name);
   SO->SUMA_VolPar_Aligned = YUP; /* Surface is in alignment with volume, should not call SUMA_Align_to_VolPar ... */

   if (!SO->State) {SO->State = SUMA_copy_string("BrainWrap"); }
   if (!SO->Group) {SO->Group = SUMA_copy_string("BrainWrap"); }
   if (!SO->Label) {SO->Label = SUMA_copy_string("icobaby"); }
   
   /* make the idcode_str depend on the Label, it is convenient to
   send the same surface all the time to SUMA */
   if (SO->Label) { if (SO->idcode_str) SUMA_free(SO->idcode_str); SO->idcode_str = UNIQ_hashcode(SO->Label); }
   if (0 && LocalHead) SUMA_Print_Surface_Object (SO, NULL);      
   
   /* see if SUMA talk is turned on */
   if (ps->cs->talk_suma) {
      ps->cs->istream = SUMA_BRAINWRAP_LINE;
      if (!SUMA_SendToSuma (SO, ps->cs, NULL, SUMA_NO_DSET_TYPE, 0)) {
         SUMA_SL_Err("Failed to initialize SUMA_SendToSuma");
         ps->cs->Send = NOPE;
         ps->cs->talk_suma = NOPE;
      } else {
         /* send the mesh since this is a new surface */
         if (!SUMA_SendToSuma (SO, ps->cs, (void *)SO->FaceSetList, SUMA_NEW_MESH_IJK, 1)) {
            SUMA_SL_Err("Failed to initialize SUMA_SendToSuma");
            ps->cs->Send = NOPE;
            ps->cs->talk_suma = NOPE;
         }
         /* now send the coordinates of the new surface */
         if (!SUMA_SendToSuma (SO, ps->cs, (void *)SO->NodeList, SUMA_NEW_NODE_XYZ, 1)) {
            SUMA_SL_Err("Failed to initialize SUMA_SendToSuma");
            ps->cs->Send = NOPE;
            ps->cs->talk_suma = NOPE;
         }
         /* now send the command to register the new surface with viewers*/
         if (!SUMA_SendToSuma (SO, ps->cs, NULL, SUMA_PREP_NEW_SURFACE, 1)) {
            SUMA_SL_Err("Failed to initialize SUMA_SendToSuma");
            ps->cs->Send = NOPE;
            ps->cs->talk_suma = NOPE;
         }
         /* now manually clean up the function that created the mesh.
         last SUMA_SendToSuma call will clean up the  SUMA_NODE_XYZ related
         functions. SUMA_SendToSuma can only clean when the same dtype is being sent */
         SUMA_Mesh_IJK2Mesh_IJK_nel (SO, NULL, YUP, SUMA_NEW_MESH_IJK);
      }
   }

   /* This is it baby, start walking */
   SUMA_StretchToFitLeCerveau (SO, Opt, ps->cs);
   
   /* write the surface to disk */
   if (!SUMA_Save_Surface_Object (SO_name, SO, Opt->SurfFileType, Opt->SurfFileFormat, NULL)) {
      fprintf (SUMA_STDERR,"Error %s: Failed to write surface object.\n", FuncName);
      exit (1);
   }
      
   /* you don't want to exit rapidly because the SUMA might not be done processing the last elements*/
   if (ps->cs->Send && !ps->cs->GoneBad) {
      /* cleanup and close connections */
      if (!SUMA_SendToSuma (SO, ps->cs, NULL, SUMA_NODE_XYZ, 2)) {
         SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCleanup failed");
      }
   }   

   if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   if (Opt->dvec) SUMA_free(Opt->dvec); Opt->dvec = NULL;
   if (Opt->mcdatav) {SUMA_free(Opt->mcdatav); Opt->mcdatav = NULL;} 
   if (Opt->in_vol) { DSET_delete( Opt->in_vol); Opt->in_vol = NULL;} 
   if (Opt->out_prefix) SUMA_free(Opt->out_prefix); Opt->out_prefix = NULL;
   if (Opt->XYZ) SUMA_free(Opt->XYZ); Opt->XYZ = NULL;
   if (Opt) SUMA_free(Opt);
   if (!SUMA_Free_Displayable_Object_Vect (SUMAg_DOv, SUMAg_N_DOv)) {
      SUMA_SL_Err("DO Cleanup Failed!");
   }
   if (SO_name) SUMA_free(SO_name); SO_name = NULL;
   if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   exit(0);
}   
#endif
