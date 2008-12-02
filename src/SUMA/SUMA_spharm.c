#include "SUMA_suma.h"
#include "SUMA_spharm.h"


/*
  Function below taken from code written by Dr. Hidenori Ogata 
  http://comp.cs.ehime-u.ac.jp/~ogata/
  http://comp.cs.ehime-u.ac.jp/~ogata/nac/index.html
  
  associated Legendre polynomial
  P_l^m(cos(t)) 
  = (-1)^{l+m} / (2^l l!) sin^m(t) (d/d cos(t))^{l+m} sin^{2l}(t)
  
  This function returns P_l^m(cos(t)). If you want P_l^m(t) then 
  pass acos(t) instead of t
*/
double SUMA_pLegendre(int l, int m, double t)
{
  /*
    compute the associated Legendre polynomials P_l^m(cos(t)) 
    of degree l and order m
    in double precision by the recurrence relation

    (input) 
    l (int) degree s.t. l>=0
    m (int) order s.t. -l<=m <=l
    t (double) variable
  */
  double cs, sn, y, y_1, y_2, yy, c, d;
  int mm, i, k;

  if (l<0) {
    printf("l=%d. l must be non-negative.\n", l);
    return 0;
  }
  if (m<-l || m>l) {
    printf("m=%d. m must be -l <= m <= l.\n", m);
    return 0;
  }
  /*
    compute P_l^m(x) by the recurrence relation
    (l-m)P_l^m(x) = x(2l-1)P_{l-1}^m(x) - (l+m-1)P_{l-2}^m(x)
    with 
    P_m^m(x) = (-1)^m (2m-1)!! (1-x)^{m/2}, 
    P_{m+1}^m(x) = x(2m+1) P_m^m(x).
  */
  cs = cos(t);
  sn = sin(t);
  mm = m;                   /*   mm = |m|   */
  if (m<0) mm = - mm;
  y_1 = 1.0;
  for (i=1; i<=mm; ++i)
    y_1 *= - 1.0 * (2.0*i-1) * sn;
  if (l==mm) 
    {
      yy = y_1;
    } 
  else 
    {
      y = (2.0*mm + 1.0) * cs * y_1;
      if (l==mm+1) 
	{
	  yy = y;
	} 
      else 
	{
	  c = 2.0 * mm - 1.0;
	  for (k=mm+2; k<=l; ++k)
	    {
	      y_2 = y_1;
	      y_1 = y;
	      d = 1.0 / (k - mm);
	      y = (2.0 + c * d) * cs * y_1 - (1.0 + c * d) * y_2;
	    }
	  yy = y;
	}
    }
  /*
    In the case that m<0, 
    compute P_n^{-|m|}(x) by the formula 
    P_l^{-|m|}(x) = (-1)^{|m|}((l-|m|)!/(l+|m|)!)^{1/2} P_l^{|m|}(x). 
  */
  if (m<0) 
    {
      for (i=l-mm+1; i<=l+mm; ++i)
	yy *= 1.0 / i;
      if (mm%2==1) yy = - yy;
    }
  
  return yy;
}

/*!
   Calculate spherical harmonic of degree l at locations (theta,phi) on the sphere
   Y_l is a l+1 x theta->N_vals complex matrix
*/
   
SUMA_MX_VEC *SUMA_Y_l(int *lp, SUMA_MX_VEC *theta, SUMA_MX_VEC *phi, int debug)
{
   static char FuncName[]={"SUMA_Y_l"};
   int  i = -1,  i3, OK, dims[2], l, k, j, o;
   double *fact = NULL, dd= 0.0, scl=0.0;
   SUMA_MX_VEC *Y_l=NULL, *ones=NULL, *CLM=NULL, *exp_i_m=NULL, *sign_m=NULL;
   SUMA_MX_VEC *clm = NULL, *SGNM=NULL, *Pn=NULL;
   struct  timeval tt; 
   
   SUMA_ENTRY;
   
   if (debug ) SUMA_etime2(FuncName, NULL, NULL);
   
   if (!theta || !phi) {
      SUMA_S_Err("NULL input");
      SUMA_RETURN(Y_l);
   }
   l = *lp;
      
   /** Initialize the factorials **/
   fact = SUMA_factorial_array(2*l); /* careful, indexing shift here relative to Y_l */
   if (!fact) {
      SUMA_S_Err("Failed to calculate factorials");
      SUMA_RETURN(Y_l);
   }
   if (debug > 2) {
      fprintf(SUMA_STDERR,"Factorials:\n");
      for (i=0; i<2*l+1; ++i) {
         fprintf(SUMA_STDERR,"%d %g\n", i, (double)fact[i]);
      }
   }
   /* scan for overflow */
   i=0;
   OK = 1;
   do {
      if (!SUMA_IS_GOOD_FLOAT(fact[i])) OK = 0;
      ++i;
   } while (i<=2*l && OK);
   if (!OK) {
      SUMA_S_Warnv("Degree of %d causes overflow. Limiting to %d\n", 
                     l, (i+1)/2-1);
      l = (i+1)/2-1;
   }
   /** Create clm vector */
   dims[0] = l+1; dims[1] = 1;
   clm = SUMA_NewMxVec(SUMA_double, 2, dims, 1);
   if (!clm) {
      SUMA_S_Crit("Failed to allocate Y_l");
      SUMA_RETURN(Y_l);
   }
   for (i=0; i<=l; ++i) {
      mxvd2(clm,i,0) = sqrt(((2.0*l+1.0)/(2.0*SUMA_PI))*(fact[l-i]/fact[l+i]));   
   }
   if (debug > 1) {
      fprintf(SUMA_STDERR,"clm:\n");
      for (i=0; i<=l; ++i) {
         fprintf(SUMA_STDERR,"%d %g\n", i, mxvd2(clm,i,0));     
      }
   }

   /** Create CLM matrix */
   dims[0] = 1; dims[1] = theta->N_vals;
   ones = SUMA_NewMxVec(SUMA_double, 2, dims, 1);
   dd = 1.0;
   if (!ones || !SUMA_MxVecInit(ones, (void *)&dd)) {
      SUMA_S_Err("Failed to initialize...");
      goto CLEAN_OUT;
   }
   if (debug > 2) {
      SUMA_ShowMxVec(ones, 1, NULL, "\nones matrix\n");
   }
   
   CLM = SUMA_KronProd(ones, clm);
   if (!CLM) {
      SUMA_S_Crit("Failed to allocate CLM");
      SUMA_RETURN(Y_l);
   }
   if (debug > 2) {
      SUMA_ShowMxVec(CLM, 1, NULL, "\nCLM matrix\n");
   }
   
   dims[0] = l+1; dims[1] = theta->N_vals;
   exp_i_m = SUMA_NewMxVec(SUMA_complex, 2, dims, 1);
   dims[0] = l+1; dims[1] = 1;
   sign_m = SUMA_NewMxVec(SUMA_double, 2, dims, 1);
   if (!exp_i_m || !sign_m) {
      SUMA_S_Crit("Failed to allocate exp_i_m or sign_m");
      SUMA_RETURN(Y_l);   
   }
   for (j=0; j<theta->N_vals; ++j) {
      for (k=0; k<=l; ++k) {
         mxvc2(exp_i_m,k,j).r = cos((double)k*mxvd1(phi,j));
         mxvc2(exp_i_m,k,j).i = sin((double)k*mxvd1(phi,j));
      }
   }
   for (k=0; k<=l; ++k) {
      if (k % 2) { /* odd */ mxvd2(sign_m,k,0) = -1.0f; } 
      else { mxvd2(sign_m,k,0) = 1.0f; }      
   }
   scl = sqrt(2.0);
   for (j=0;j<exp_i_m->dims[1]; ++j) {
      mxvc2(exp_i_m,0,j).r /= scl; 
      mxvc2(exp_i_m,0,j).i /= scl; 
   }
   if (debug > 1) {
      if (debug > 2) SUMA_WriteMxVec(phi, "phi.1D.dset", "#Aloha\n");
      SUMA_ShowMxVec(sign_m, 1, NULL, "\nsign_m matrix\n");
      SUMA_ShowMxVec(exp_i_m, 1, NULL, "\nexp_i_m matrix\n");
      if (debug > 2) SUMA_WriteMxVec(exp_i_m, "exp_i_m.1D.dset", "#Aloha\n");
   }
   
   SGNM = SUMA_KronProd(ones, sign_m);
   if (!SGNM) {
      SUMA_S_Crit("Failed to calculate Kronecker product.");
      SUMA_RETURN(Y_l);
   }
   if (debug > 2) {
      SUMA_ShowMxVec(SGNM, 1, NULL, "\nSGNM matrix\n");
   }
   /* get the Legendre coefficients */
   dims[0] = l+1; dims[1] = theta->N_vals;
   Pn = SUMA_NewMxVec(SUMA_double, 2, dims, 1);
   if (!Pn) {
      SUMA_S_Crit("Failed to allocate Pn");
      SUMA_RETURN(Y_l);   
   }
   for (i=0; i<theta->N_vals;++i) {
      for (o=0; o<=l;++o) { /* order */
         /* Don't pass cos(mxvd1(theta,i)) 'cause pLegendre returns P_l_m(cost(t)), not P_l_m(t) */
         mxvd2(Pn,o, i) = SUMA_pLegendre( l, o, mxvd1(theta,i) ); 
         if (debug > 2 && i<5 && o < 4) {
            SUMA_S_Notev("theta %f: legendre(%d, %d, %f) = %g\n", mxvd1(theta,i), l, o,  cos(mxvd1(theta,i)),  mxvd2(Pn,o, i));
         }
      }
   }
   if (debug > 1) {
      SUMA_ShowMxVec(Pn, 1, NULL, "\nPn matrix\n");
      if (debug > 2) SUMA_WriteMxVec(Pn, "Pn.1D.dset", "#Aloha\n");
   }

   dims[0] = l+1; dims[1] = theta->N_vals;
   Y_l = SUMA_NewMxVec(SUMA_complex, 2, dims, 1);
   if (!Y_l) {
      SUMA_S_Crit("Failed to allocate Y_l");
      SUMA_RETURN(Y_l);
   }
   for (i=0; i<Y_l->N_vals; ++i) {
      dd = CLM->dv[i]*SGNM->dv[i]*Pn->dv[i];
      Y_l->cv[i].r = dd*exp_i_m->cv[i].r;	
      Y_l->cv[i].i = dd*exp_i_m->cv[i].i;
   }
   if (debug > 1) {
      SUMA_ShowMxVec(Y_l, 1, NULL, "\nY_l matrix\n");
      if (debug > 2) SUMA_WriteMxVec(Y_l, "Y_l.1D.dset", "#Aloha\n");
   }
   CLEAN_OUT:
   if (fact) SUMA_free(fact); fact = NULL;
   if (clm) SUMA_FreeMxVec(clm); clm = NULL;
   if (CLM) SUMA_FreeMxVec(CLM); clm = NULL;
   if (ones) SUMA_FreeMxVec(ones); ones = NULL;
   if (sign_m) SUMA_FreeMxVec(sign_m); sign_m = NULL;
   if (exp_i_m) SUMA_FreeMxVec(exp_i_m);  exp_i_m= NULL;
   if (SGNM) SUMA_FreeMxVec(SGNM);  SGNM = NULL;
   if (Pn) SUMA_FreeMxVec(Pn);  Pn = NULL;
  
   *lp = l;

   if (debug ) {
      fprintf(SUMA_STDERR,"%s: Order %d.\n", FuncName, *lp);
      SUMA_etime2(FuncName, "---", FuncName);
   }

   SUMA_RETURN(Y_l);
}

/*!
   if sph_coordp is not null then *sph_coordp is a pointer to the spherical coordinates (Rho, phi, theta) as calculated
   if phip is not null then then *phip will contain the phi angle, shifted to match Moo Chung's convention
   if thetap is not null then then *thetap will contain the theta angle, shifted to match Moo Chung's convention
*/
int SUMA_SphericalCoordsUnitSphere(SUMA_SurfaceObject *SO, SUMA_MX_VEC **phip, SUMA_MX_VEC **thetap, double **sph_coordp)
{
   static char FuncName[]={"SUMA_SphericalCoordsUnitSphere"};
   int i, i3;
   FILE *sph_out=NULL;
   double *sph_coord = NULL;
   double d_phi, d_theta, two_pi;
   SUMA_MX_VEC *theta=NULL, *phi=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* calculate the spherical coordinates */
   sph_coord = SUMA_Cart2Sph (SO->NodeList, SO->N_Node, NULL );
   
   /* do the shifts to match Moo's origin */
   if (1) {
      d_phi = 1.5*SUMA_PI;
      d_theta = 0.5*SUMA_PI;
      two_pi = 2.0*SUMA_PI;
      if (thetap) {  
         theta = SUMA_NewMxVec(SUMA_double, 1, &(SO->N_Node), 1); 
         if (!theta) {
            SUMA_S_Err("Failed to allocate");
            SUMA_RETURN(0);
         }
      }
      if (phip) {
         phi = SUMA_NewMxVec(SUMA_double, 1, &(SO->N_Node), 1);
         if (!phi) {
            SUMA_S_Err("Failed to allocate");
            SUMA_RETURN(0);
         }
      }
      if (phip) {
         for (i=0; i<SO->N_Node; ++i) {
            i3 = 3*i;
            mxvd1(phi, i) = sph_coord[i3+1] + d_phi; 
            if (mxvd1(phi, i) > two_pi) mxvd1(phi, i) -= two_pi;
         }
         *phip = phi;
      }
      if (thetap) {
         for (i=0; i<SO->N_Node; ++i) {
            i3 = 3*i;
            mxvd1(theta, i) = d_theta - sph_coord[i3+2];
         }
         *thetap = theta;
      }
      
   }
   
   if (LocalHead) {
      sph_out=fopen("sphout.1D.dset","w");
      SUMA_S_Notev("Writing spherical coords to %s...\n", "sphout.1D.dset");
      if (sph_out) {
         for (i=0; i<SO->N_Node; ++i) {
            fprintf(sph_out, "%d %f %f %f\n", 
                        i, sph_coord[3*i+0], sph_coord[3*i+1], sph_coord[3*i+2]);
         }
         fclose(sph_out); sph_out = NULL;
      } else {
         SUMA_S_Err("Failed to open sphout.1D.dset for writing");
      }
      if (theta) {
         sph_out=fopen("theta_suma.1D.dset","w");
         SUMA_S_Notev(  "Writing spherical coords to %s...\n", 
                        "theta_suma.1D.dset");
         if (sph_out) {
            for (i=0; i<SO->N_Node; ++i) {
               fprintf(sph_out, "%d %f \n", 
                           i, mxvd1(theta, i));
            }
            fclose(sph_out); sph_out = NULL;
         } else {
            SUMA_S_Err("Failed to open theta_suma.1D.dset for writing");
         }
      }
      if (phi) {
         sph_out=fopen("phi_suma.1D.dset","w");
         SUMA_S_Notev("Writing spherical coords to %s...\n", "phi_suma.1D.dset");
         if (sph_out) {
            for (i=0; i<SO->N_Node; ++i) {
               fprintf(sph_out, "%d %f \n", 
                           i, mxvd1(phi, i));
            }
            fclose(sph_out); sph_out = NULL;
         } else {
            SUMA_S_Err("Failed to open phi_suma.1D.dset for writing");
         }
      }
   }
   
   if (sph_coordp) *sph_coordp = sph_coord;
   else if (sph_coord) SUMA_free(sph_coord); 
   
   if (LocalHead) {
      SUMA_ShowMxVec(*thetap, 1, NULL, 
                     "\nTheta in SUMA_SphericalCoordsUnitSphere\n");
      SUMA_ShowMxVec(*phip, 1, NULL, 
                     "\nPhi in SUMA_SphericalCoordsUnitSphere\n");
   }
   SUMA_RETURN(1);
}

SUMA_MX_VEC *SUMA_Spherical_Bases(int *lp, SUMA_OPT_SPHERICAL_BASES  *opt)
{
   static char FuncName[]={"SUMA_Spherical_Bases"};
   char *oname=NULL, stmp[100];
   static double *sph_coord=NULL;
   complex *cv=NULL;
   int lc=0, l = *lp, dims[2], N_dims=-1, ncol=-1, nrow=-1;
   SUMA_MX_VEC *y_l=NULL;
   static SUMA_MX_VEC *theta=NULL, *phi=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!opt) {
      SUMA_LH("Init/Cleanup mode.\n");
      if (sph_coord) SUMA_free(sph_coord); sph_coord = NULL;
      if (theta) theta = SUMA_FreeMxVec(theta);
      if (phi) phi = SUMA_FreeMxVec(phi);
      SUMA_RETURN(y_l);
   }
   
   if (opt->SOu) {
      SUMA_LHv("Calculating bases of order %d using unit sphere %s\n", 
               l, opt->SOu->Label);
      if (!theta) {
         if (!(SUMA_SphericalCoordsUnitSphere(opt->SOu, &phi, &theta, NULL))) {
            SUMA_S_Err("Failed to calculate spherical coords.");
            goto CLEANUP;
         }
      }
      { /* calculate anew */
         /* Now create thems bases a la Y_l.m */
         lc = l;
         if (!(y_l = SUMA_Y_l(&lc, theta, phi, opt->debug))) {
            SUMA_S_Err("Failed to caluclate y_l!");
            goto CLEANUP;
         }
         if (lc < l) {
            SUMA_S_Notev("Can't go to l > %d\n", lc);
            *lp = lc;
         }
         if (opt->SaveBases) {
            sprintf(stmp, ".sph%02d.1D", l);
            oname = SUMA_append_string(opt->SaveBases, stmp);
            if (l==0) {
               SUMA_S_Notev(  "Saving bases of order %d to %s\n"
                              "Message muted for higher l.\n", l, oname); 
            }
            sprintf(stmp,"#Spherical Harmonic of order %d\n"
                         "#Domain has %d nodes.", l, opt->SOu->N_Node);
            SUMA_WriteMxVec(y_l, oname, stmp );
         }
      } 
   } else {
      if (!opt->BasesFileRoot) {
         SUMA_S_Err("NULL BasesFileRoot");
         goto CLEANUP;
      }
         sprintf(stmp, ".sph%02d.1D", l);
         oname = SUMA_append_string(opt->BasesFileRoot, stmp);
         if (l==0) {
            SUMA_S_Notev("Loading bases of order %d from file %s ...\n"
                         "Message muted for higher l.\n", l, oname);
         }
         cv = SUMA_LoadComplex1D_eng (oname, &ncol, &nrow, 0, 0);
         SUMA_LH("   Done Reloading!");
         if (!cv) {
            SUMA_S_Errv("Failed to find  y_l[%d] from file %s\n", l, oname);
            goto  CLEANUP;
         } else {
            dims[0] = nrow; dims[1] = ncol;
            y_l = SUMA_VecToMxVec(SUMA_complex, 2, dims, 1, (void *)cv); 
            cv = NULL; /* cv should be nulled, pointer copied into output*/
         }
   }
   
   CLEANUP:
   if (oname) SUMA_free(oname); oname = NULL; 
   
   SUMA_RETURN(y_l);
}

SUMA_MX_VEC *SUMA_YLcomp_to_YLdoub( SUMA_MX_VEC **y_lp, int debug) 
{
   static char FuncName[]={"SUMA_YLcomp_to_YLdoub"};
   SUMA_MX_VEC *yc = NULL;
   SUMA_MX_VEC *y_l = *y_lp;
   int dims[2];
   int i, j, j_l;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   if (debug > 1)  {
      SUMA_ShowMxVec(y_l, 1, NULL, "\noriginal y_l matrix\n");
      if (debug > 2) {
         SUMA_WriteMxVec(y_l, "y_l_o.1D.dset", "#original y_l matrix\n");
      }
   }
   dims[0] = 2*y_l->dims[0]-1; dims[1] = y_l->dims[1];
   if (!(yc = SUMA_NewMxVec(SUMA_double, 2, dims, 1))) {
      SUMA_S_Err("Failed to create Yc");
      SUMA_RETURN(NULL);
   }
   for (j=0; j<yc->dims[0]; ++j) {
      if (j<y_l->dims[0]) {
         j_l = j;
         for (i=0; i<yc->dims[1]; ++i) {
            mxvd2(yc, j, i) = mxvc2(y_l, j_l, i).r;
         }
      } else {
         j_l = j-y_l->dims[0];
         for (i=0; i<yc->dims[1]; ++i) {
            mxvd2(yc, j, i) = -mxvc2(y_l, j_l+1, i).i;   /* the minus is here because one is storing the transpose of the imaginary component of y_l ! */
         }
      }
   }
   y_l = SUMA_FreeMxVec(y_l); *y_lp = NULL;   /* Now y_l is cleared and inaccessible */

   SUMA_RETURN(yc);
}
