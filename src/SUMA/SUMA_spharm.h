#ifndef SUMA_SPHARM_INCLUDED
#define SUMA_SPHARM_INCLUDED

typedef struct {
   SUMA_SurfaceObject *SOu;   /* DO NOT FREE */
   char *BasesFileRoot;     /* DO NOT FREE */
   char *SaveBases;        /* DO NOT FREE */
   int debug;
} SUMA_OPT_SPHERICAL_BASES;


#define SUMA_SPHARM_SMOOTH_EST(x, xe, betax, cc) { \
   dif = SUMA_MxVecAdd(x, xe, -1, dif);   /* x_j */   \
   if (Opt->debug > 1) { \
      sprintf(stmp,"\ndif_vec_%s vector\n", cc);   \
      SUMA_ShowMxVec(dif, 1, NULL, stmp);   \
   }  \
   betal=SUMA_MxVecMult(Ycommon, dif, NULL, 0); \
   if (Opt->debug > 1) { \
      sprintf(stmp,"\nbetal_%s vector\n", cc);   \
      SUMA_ShowMxVec(betal, 1, NULL, stmp);  \
   }  \
   for (j=0;j<2*l+1;++j) { \
      mxvd2(betax,l, j) = mxvd1(betal,j); \
   }  \
   if (Opt->debug > 1) { \
      sprintf(stmp,"\nbeta_%s  vector\n", cc);   \
      SUMA_ShowMxVec(betax, 1, NULL, stmp);  \
   }  \
   sm = SUMA_MxVecMult(y_l_t, betal, sm, 0); \
   if (Opt->debug > 1) { \
      sprintf(stmp,"\nsm_%s  vector\n", cc);   \
      SUMA_ShowMxVec(sm , 1, NULL, stmp); \
   }  \
   for (i=0;i<x->N_vals;++i) {   \
      mxvd1(xe,i) += fac * mxvd1(sm,i);   \
   }  \
   if (Opt->debug > 1) { \
      sprintf(stmp,"\n%s_estimate  vector\n", cc);   \
      SUMA_ShowMxVec(xe, 1, NULL, stmp);    \
      sprintf(stmp,"%s_estimate_l%d.1D.dset", cc, l);   \
      if (Opt->debug > 2) SUMA_WriteMxVec(xe, stmp, "#estimate at last l\n"); \
   }  \
}
            
double SUMA_pLegendre(int l, int m, double t);
SUMA_MX_VEC *SUMA_Y_l(int *lp, SUMA_MX_VEC *theta, SUMA_MX_VEC *phi, int debug);
int SUMA_SphericalCoordsUnitSphere(SUMA_SurfaceObject *SO, SUMA_MX_VEC **phip, SUMA_MX_VEC **thetap, double **sph_coordp);
SUMA_MX_VEC *SUMA_Spherical_Bases(int *lp, SUMA_OPT_SPHERICAL_BASES  *opt);
SUMA_MX_VEC *SUMA_YLcomp_to_YLdoub( SUMA_MX_VEC **y_lp, int debug); 

#endif
