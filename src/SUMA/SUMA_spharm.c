/*USE This sample to start writing standalone programs.
Change spharm_test to the program name of your choosing.
*/
#include "SUMA_suma.h"


SUMA_SurfaceViewer *SUMAg_cSV = NULL; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
SUMA_DO *SUMAg_DOv = NULL;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF = NULL; /*!< Global pointer to structure containing info common to all viewers */

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

void usage_spharm_test (SUMA_GENERIC_ARGV_PARSE *ps)
{
      static char FuncName[]={"usage_spharm_test"};
      char * s = NULL, *sio=NULL, *st = NULL, *sts = NULL;
      int i;
      s = SUMA_help_basics();
      sio  = SUMA_help_IO_Args(ps);
      printf ( "\n"
               "Usage: A template code for writing SUMA programs.\n"
               " \n"
               "%s"
               "  -unit_sph UNIT_SPH\n"
               "  -l ORDER\n"
               "  -debug BDG\n"
               "  -bases_prefix \n"
               "  -bases\n"
               "  -sigma SIGMA\n"
               " example: \n"
               " spharm_test -i_iv unit_sph.iv -i_iv original.iv -l 86 \\\n"
               "             -unit_sph unit_sph -bases_prefix bbb -prefix sample -debug 1\n"
               " example: \n"
               " spharm_test -i_iv unit_sph.iv -i_iv outersurface.iv -l 86 \\\n"
               "             -unit_sph unit_sph -o_fs yoyo -debug 1 -talk_suma | & tee log\n"
               "%s"
               "\n", sio,  s);
      SUMA_free(s); s = NULL; SUMA_free(st); st = NULL; SUMA_free(sio); sio = NULL;       
      s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
      printf("       Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov     \n");
      exit(0);
}

SUMA_GENERIC_PROG_OPTIONS_STRUCT *SUMA_spharm_test_ParseInput(char *argv[], int argc, SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_spharm_test_ParseInput"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt=NULL;
   int kar;
   SUMA_Boolean brk;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   Opt = SUMA_Alloc_Generic_Prog_Options_Struct();
   Opt->iopt = 86;
   Opt->out_prefix = NULL;
   Opt->bases_prefix = NULL;
   Opt->unit_sphere_name = NULL;
   Opt->v0 = 0.0001;
   kar = 1;
   brk = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_spharm_test(ps);
          exit (0);
		}
		
		SUMA_SKIP_COMMON_OPTIONS(brk, kar);
      
      if (!brk && (strcmp(argv[kar], "-l") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need an integer after -l \n");
            exit (1);
         }
         
         Opt->iopt = atoi(argv[++kar]);
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-prefix") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a name after -prefix\n");
            exit (1);
         }
         
         Opt->out_prefix = SUMA_copy_string(argv[++kar]);
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-bases_prefix") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a name after -bases_prefix\n");
            exit (1);
         }
         
         Opt->bases_prefix = argv[++kar];
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-bases") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a name after -bases\n");
            exit (1);
         }
         
         Opt->bases_prefix = argv[++kar];
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-unit_sph") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a name after -unit_sph\n");
            exit (1);
         }
         
         Opt->unit_sphere_name = argv[++kar];
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-debug") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a number after -debug \n");
            exit (1);
         }
         
         Opt->debug = atoi(argv[++kar]);
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-sigma") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a number after -sigma \n");
            exit (1);
         }
         
         Opt->v0 = atof(argv[++kar]);
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
   
   SUMA_RETURN(Opt);
}

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
      SUMA_S_Warnv("Degree of %d causes overflow. Limiting to %d\n", l, (i+1)/2-1);
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

typedef struct {
   SUMA_SurfaceObject *SOu;   /* DO NOT FREE */
   char *BasesFileRoot;     /* DO NOT FREE */
   char *SaveBases;        /* DO NOT FREE */
   int debug;
} SUMA_OPT_SPHERICAL_BASES;

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
            mxvd1(phi, i) = sph_coord[i3+1] + d_phi; if (mxvd1(phi, i) > two_pi) mxvd1(phi, i) -= two_pi;
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
         SUMA_S_Notev("Writing spherical coords to %s...\n", "theta_suma.1D.dset");
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
      SUMA_ShowMxVec(*thetap, 1, NULL, "\nTheta in SUMA_SphericalCoordsUnitSphere\n");
      SUMA_ShowMxVec(*phip, 1, NULL, "\nPhi in SUMA_SphericalCoordsUnitSphere\n");
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
      SUMA_S_Note("Init/Cleanup mode.\n");
      if (sph_coord) SUMA_free(sph_coord); sph_coord = NULL;
      if (theta) theta = SUMA_FreeMxVec(theta);
      if (phi) phi = SUMA_FreeMxVec(phi);
      SUMA_RETURN(y_l);
   }
   
   if (opt->SOu) {
      SUMA_LHv("Calculating bases of order %d using unit sphere %s\n", l, opt->SOu->Label);
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
            sprintf(stmp, "%d.1D", l);
            oname = SUMA_append_string(opt->SaveBases, stmp);
            SUMA_S_Notev("Saving bases of order %d to %s\n", l, oname); 
            SUMA_WriteMxVec(y_l, oname, "#Aloha\n");
         }
      } 
   } else {
      if (!opt->BasesFileRoot) {
         SUMA_S_Err("NULL BasesFileRoot");
         goto CLEANUP;
      }
         sprintf(stmp, "%d.1D", l);
         oname = SUMA_append_string(opt->BasesFileRoot, stmp);
         SUMA_S_Notev("Loading bases of order %d from file %s ...\n", l, oname);
         cv = SUMA_LoadComplex1D_eng (oname, &ncol, &nrow, 0, 0);
         SUMA_LH("   Done Reloading!");
         if (!cv) {
            SUMA_S_Errv("Failed to find  y_l[%d] from file %s\n", l, oname);
            goto  CLEANUP;
         } else {
            dims[0] = nrow; dims[1] = ncol;
            y_l = SUMA_VecToMxVec(SUMA_complex, 2, dims, 1, (void *)cv); cv = NULL; /* cv should be nulled, pointer copied into output*/
         }
   }
   
   CLEANUP:
   if (oname) SUMA_free(oname); oname = NULL; 
   
   SUMA_RETURN(y_l);
}

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
            

int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"spharm_test"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt;  
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   SUMA_SurfSpecFile *Spec = NULL;
   int *isin=NULL;
   int  i = -1, ii, jj, kk, il, N_Spec=0, i3, OK, l, lc, ncol=0, nrow=0, dims[20], N_dims=0, N_surfs=0, j, j_l;
   int n_subject=1,  iso=0;
   SUMA_Boolean exists=NOPE;
   float *far=NULL;
   double  *dv=NULL, fac = 0.0;
   complex *cv=NULL;
   SUMA_FORM_AFNI_DSET_STRUCT *OptDs = NULL;
   SUMA_SurfaceObject *SO = NULL, *SOu=NULL, *SOn=NULL, *SOv[500], *SOvn[500];
   SUMA_VOLPAR *vp = NULL;
   SUMA_MX_VEC *y_l=NULL, *y_l_t=NULL , *x=NULL, *y=NULL, *z=NULL, *y_c=NULL;
   SUMA_MX_VEC *betal=NULL, *betax=NULL, *betay=NULL, *betaz=NULL, *xsmooth = NULL, *ysmooth=NULL, *zsmooth=NULL;
   SUMA_MX_VEC *zestimate=NULL, *xe=NULL, *ye=NULL, *ze=NULL, *dif = NULL, *sm=NULL;
   char *oname=NULL, stmp[100], *pref=NULL;
   SUMA_SO_File_Format form=SUMA_FF_NOT_SPECIFIED;
   SUMA_SO_File_Type tp=SUMA_FT_NOT_SPECIFIED;
   SUMA_OPT_SPHERICAL_BASES optb;
   void *SO_name=NULL;
   struct  timeval tt; 
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_STANDALONE_INIT;
	SUMA_mainENTRY;

   /* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "-i;-spec;-talk;-o;");
 
   #if 0
   SUMA_TestMxVecMatOps();
   exit(1);
   #endif
      
   if (argc < 2) {
      usage_spharm_test(ps);
      exit (1);
   }
   
   #if 0 /* a couple of tests of SUMA_pLegendre function */
   SUMA_S_Notev("legendre(1, 0, cos(0.4375))=%g\n", SUMA_pLegendre(1,0,0.4375));
   SUMA_S_Notev("legendre(1, 1, cos(0.4375))=%g\n", SUMA_pLegendre(1,1,0.4375));
   SUMA_S_Notev("legendre(1, 0, 0.4375)=%g\n", SUMA_pLegendre(1,0,acos(0.4375)));
   SUMA_S_Notev("legendre(1, 1, 0.4375)=%g\n", SUMA_pLegendre(1,1,acos(0.4375)));
   exit(1);
   #endif
   Opt = SUMA_spharm_test_ParseInput (argv, argc, ps);
   
   if (Opt->debug) LocalHead = YUP;
   
   if (ps->o_N_surfnames && Opt->out_prefix) {
      SUMA_S_Err("Cannot use -o_ options along with -prefix options");
      exit(1);
   }
   if (!ps->o_N_surfnames && !Opt->out_prefix) {
      SUMA_S_Notev("Using default prefix of %s\n", "spharm_sm");
      Opt->out_prefix = SUMA_copy_string("spharm_sm");
   }
   

   if (Opt->debug > 2) LocalHead = YUP;
   
   /* check on inputs */
   optb.SOu=NULL;
   optb.BasesFileRoot=Opt->bases_prefix;
   optb.SaveBases=Opt->bases_prefix;
   optb.debug = Opt->debug;
   
   N_surfs = ps->s_N_surfnames + ps->i_N_surfnames + ps->t_N_surfnames;
   if (N_surfs < 1 || N_surfs > 2) {
      SUMA_S_Errv("One or 2 surface(s) are needed.\nHave %d on command line.\n", N_surfs);
      exit(1);
   }

   Spec = SUMA_IO_args_2_spec(ps, &N_Spec);
   if (N_Spec == 0) {
      SUMA_S_Err("No surfaces found.");
      exit(1);
   }
   if (N_Spec != 1) {
      SUMA_S_Err("Multiple spec at input.");
      exit(1);
   }

   n_subject = 0;
   SOv[n_subject] = NULL;
   SOvn[n_subject] = NULL;
   for (i=0; i<N_surfs; ++i) {
      SO = SUMA_Load_Spec_Surf(Spec, i, ps->sv[i], 0);
      if (!SO) {
            fprintf (SUMA_STDERR,"Error %s:\n"
                                 "Failed to find surface\n"
                                 "in spec file. \n",
                                 FuncName );
            exit(1);

      }   
      if (Opt->debug > 2) SUMA_Print_Surface_Object(SO, SUMA_STDERR); 
      if (Opt->unit_sphere_name) {
         if (     (SO->Name.FileName && SUMA_iswordin(SO->Name.FileName, Opt->unit_sphere_name))
               || (SO->Name_coord.FileName && SUMA_iswordin(SO->Name_coord.FileName, Opt->unit_sphere_name))
               || (SO->Name_topo.FileName && SUMA_iswordin(SO->Name_topo.FileName, Opt->unit_sphere_name)) ) {
            if (!optb.SOu) {
               optb.SOu = SO; SO = NULL;
               SUMA_LHv("Set %s as unit sphere.\n", Opt->unit_sphere_name);
            } else {
               SUMA_S_Err("Have conflict in determining unit sphere surface.\nPlease post command line on AFNI'smessage board.\n");
               exit(1);
            }  
         } else {
            if (!SO->normdir) SO->normdir = 1;  /* set it to something */
            SOv[n_subject] = SO;
            SOvn[n_subject] = SUMA_CreateChildSO( SO, NULL, -1, NULL, -1, 0);
            ++n_subject; SOv[n_subject] = NULL; SOvn[n_subject] = NULL; 
         }
      }
   }
   /* see if SUMA talk is turned on */
   if (ps->cs->talk_suma && n_subject != 1) {
      SUMA_S_Warn("Can't talk with more than one surface processed at the same time.\n");
      ps->cs->talk_suma = NOPE;
   }
   if (ps->cs->talk_suma) {
      ps->cs->istream = SUMA_GEOMCOMP_LINE;
      if (!SUMA_SendToSuma (SO, ps->cs, NULL, SUMA_NO_DSET_TYPE, 0)) {
         SUMA_SL_Err("Failed to initialize SUMA_SendToSuma");
         ps->cs->Send = NOPE;
         ps->cs->talk_suma = NOPE;
      }
      SUMA_SendSumaNewSurface(SO, ps->cs);
   }
   
   if (N_surfs == 2 && !optb.SOu) {
      SUMA_S_Err("Specified two surfaces, neither set as unit sphere!");
      exit(1);
   }
   
   SUMA_LHv("Have %d surfaces to process\n", n_subject);
   if (optb.SOu) {
      SUMA_LHv("Have surface %s as unit sphere.\n", optb.SOu->Label);
   }
   
   /* initialize output variables  */
   dims[0] = SO->N_Node; dims[1] = 1; 
   x = SUMA_NewMxVec(SUMA_double, 2, dims, 1);
   y = SUMA_NewMxVec(SUMA_double, 2, dims, 1);
   z = SUMA_NewMxVec(SUMA_double, 2, dims, 1);
   dims[0] = SO->N_Node; dims[1] = 1; 
   xe = SUMA_NewMxVec(SUMA_double, 2, dims, 1);
   ye = SUMA_NewMxVec(SUMA_double, 2, dims, 1);
   ze = SUMA_NewMxVec(SUMA_double, 2, dims, 1);
   dims[0] = Opt->iopt+1; dims[1] = 2*Opt->iopt+1; /* (allow for 2d, easy to extend to 3d) */
   betax = SUMA_NewMxVec(SUMA_double, 2, dims, 1);
   betay = SUMA_NewMxVec(SUMA_double, 2, dims, 1);
   betaz = SUMA_NewMxVec(SUMA_double, 2, dims, 1);
   
   if (!x || !y || !z || !xe || !ye || !ze) {
      SUMA_S_Err("Failed to allocate");
      exit(1);
   }

            
  
   SUMA_Spherical_Bases(&l, NULL);  /* init SUMA_Spherical_Bases */
   /* start timer*/
   SUMA_etime2(FuncName, NULL, NULL);
   
   l=0;
   do {
      SUMA_MX_VEC *Ycommon=NULL, *vt=NULL, *vt2=NULL, *yc = NULL;
      
      lc = l;
      y_l = SUMA_Spherical_Bases(&lc, &optb); /*  y_l is equal to Y' in Moo's SPHARsmooth.m function*/
      if (lc < l) {
         Opt->iopt = lc;
         SUMA_S_Notev("Cannot go for order higher than %d\n.", lc);
         goto NEXT_L;
      }
      
      iso = 0;
      do {
         SO = SOv[iso];
         if (SO) {
            if ((LocalHead && l == 0) || Opt->debug > 1) SUMA_LHv("Working with %s as surface to process (%dth out of %d, bases order %d).\n", SO->Label, iso, n_subject, l);
            if (Opt->debug > 2) SUMA_Print_Surface_Object (SO, SUMA_STDERR);
         } else {
            SUMA_S_Err("NULL SO!!!");
            exit(1);
         }
         /* load up */
         for (i=0; i<SO->N_Node; ++i) {
            i3 = 3*i;
            mxvd1(x, i) = SO->NodeList[i3  ];
            mxvd1(y, i) = SO->NodeList[i3+1];
            mxvd1(z, i) = SO->NodeList[i3+2];
         }
         if (Opt->debug > 1) SUMA_ShowMxVec(x, 1, NULL, "\nx vector\n");
         
         
         if (LocalHead || Opt->debug)  {
                  fprintf(SUMA_STDERR,"%s: Doing l = %d\n", FuncName, l);
                  SUMA_etime2(FuncName, "Entering loop", FuncName);
         }
         if (l==0) {
            yc = SUMA_CoerceMxVec(y_l, SUMA_double, 0, NULL);  /* NEED TO COERCE y_l to be double, for order 0 */
            y_l = SUMA_FreeMxVec(y_l); y_l = yc; yc = NULL;    /* it is all real. No need to carry 0i all over here ...  */
            y_l_t = SUMA_MxVecTranspose(y_l, NULL);/*  y_l is equal to Y' in Moo's SPHARsmooth.m function*/
            if (Opt->debug > 1)  SUMA_ShowMxVec(y_l_t, 1, NULL, "\ny_l_t matrix\n");
            vt = SUMA_MxVecMult(y_l,y_l_t, NULL, MATRIX_B_IS_AT);
            if (Opt->debug > 1) SUMA_ShowMxVec(vt, 1, NULL, "\nvt matrix\n");
            vt2 = SUMA_MxVecInverse(vt, NULL);
            vt = SUMA_FreeMxVec(vt);
            if (Opt->debug > 1) SUMA_ShowMxVec(vt2, 1, NULL, "\nvt2 matrix\n");
            Ycommon = SUMA_MxVecMult(vt2,y_l, NULL, 0);
            if (Opt->debug > 1) SUMA_ShowMxVec(Ycommon, 1, NULL, "\nYcommon matrix\n");
            vt2 = SUMA_FreeMxVec(vt2);
            
            betal = SUMA_MxVecMult(Ycommon, x, NULL, 0);
            if (Opt->debug > 1) SUMA_ShowMxVec(betal, 1, NULL, "\nbetalx matrix\n");
            mxvd2(betax,0,0) =  mxvd2(betal, 0, 0);
            if (Opt->debug > 1) SUMA_ShowMxVec(betax, 1, NULL, "\nbetax matrix\n");
            xe = SUMA_MxVecMult(y_l_t, betal, NULL, 0);
            betal = SUMA_FreeMxVec(betal);
            if (Opt->debug > 1) {
               SUMA_ShowMxVec(xe, 1, NULL, "\nxe vector\n");
               if (Opt->debug > 2) SUMA_WriteMxVec(xe, "xe_l0.1D", "#xe vector");
            }
            betal = SUMA_MxVecMult(Ycommon, y, NULL, 0);
            if (Opt->debug > 1) SUMA_ShowMxVec(betal, 1, NULL, "\nbetaly matrix\n");
            mxvd2(betay,0,0) =  mxvd2(betal, 0, 0);
            ye = SUMA_MxVecMult(y_l_t, betal, NULL, 0);
            betal = SUMA_FreeMxVec(betal);
            if (Opt->debug > 1) SUMA_ShowMxVec(ye, 1, NULL, "\nye vector\n");
            
            betal = SUMA_MxVecMult(Ycommon, z, NULL, 0);
            if (Opt->debug > 1) SUMA_ShowMxVec(betal, 1, NULL, "\nbetalz matrix\n");
            mxvd2(betaz,0,0) =  mxvd2(betal, 0, 0);
            ze = SUMA_MxVecMult(y_l_t, betal, NULL, 0);
            betal = SUMA_FreeMxVec(betal);
            if (Opt->debug > 1) SUMA_ShowMxVec(ze, 1, NULL, "\nze vector\n");
         } else { /* higher order, need to deal with real and imaginary parts*/
            /* Catenate the columns of y_l with the real columns first (negative harmonics), 
            followed by imaginary ones (positive harmonics)*/ 
            if (Opt->debug > 1)  {
               SUMA_ShowMxVec(y_l, 1, NULL, "\noriginal y_l matrix\n");
               if (Opt->debug > 2) {
                  SUMA_WriteMxVec(y_l, "y_l_o.1D.dset", "#original y_l matrix\n");
               }
            }
            sprintf(stmp, "Starting with order l=%d", l);
            if (Opt->debug) SUMA_etime2(FuncName, stmp, FuncName);
            dims[0] = 2*y_l->dims[0]-1; dims[1] = y_l->dims[1];
            if (!(yc = SUMA_NewMxVec(SUMA_double, 2, dims, 1))) {
               SUMA_S_Err("Failed to create Yc");
               exit(1);
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
            y_l = SUMA_FreeMxVec(y_l); y_l = yc; y_c = NULL;   /* Now y_l is all real */
            if (Opt->debug ) SUMA_etime2(FuncName, "Created y_l", FuncName);
            if (Opt->debug > 2) {
               SUMA_WriteMxVec(y_l, "y_l.1D.dset", "#y_l real matrix\n");
            }
            y_l_t = SUMA_MxVecTranspose(y_l, NULL);   /*  y_l is equal to Y' in Moo's SPHARsmooth.m function*/
            if (Opt->debug > 1)  SUMA_ShowMxVec(y_l_t, 1, NULL, "\ny_l_t matrix\n");
            if (Opt->debug ) SUMA_etime2(FuncName, "Trasnposed y_l", FuncName);
            #if 0
            vt = SUMA_MxVecMult(y_l,y_l_t, NULL, 0);
            #else
            vt = SUMA_MxVecMult(y_l, y_l_t, NULL, MATRIX_B_IS_AT);   
            #endif
            if (Opt->debug > 1) SUMA_ShowMxVec(vt, 1, NULL, "\nvt matrix\n");
            if (Opt->debug) {
               sprintf(stmp,"Multiplied yl (%dx%d) by ylt(%dx%d)", 
                           y_l->dims[0], y_l->dims[1],  
                           y_l_t->dims[0], y_l_t->dims[1]);  
               SUMA_etime2(FuncName, stmp, FuncName);
            }
            vt2 = SUMA_MxVecInverse(vt, NULL);
            vt = SUMA_FreeMxVec(vt);
            if (Opt->debug ) SUMA_etime2(FuncName, "Inverted vt", FuncName);
            if (Opt->debug > 1) SUMA_ShowMxVec(vt2, 1, NULL, "\nvt2 matrix\n");
            Ycommon = SUMA_MxVecMult(vt2,y_l, NULL, 0);
            if (Opt->debug ) {
               sprintf(stmp, "Created Ycommon (%dx%d) Mult (%dx%d)",  
                                          vt2->dims[0],  vt2->dims[1],
                                          y_l->dims[0], y_l->dims[1]);
               SUMA_etime2(FuncName, stmp,  FuncName);
            }
            if (Opt->debug > 1) {
               SUMA_ShowMxVec(Ycommon, 1, NULL, "\nYcommon matrix\n");
               if (Opt->debug > 2) {
                  SUMA_WriteMxVec(Ycommon, "Ycommon.1D.dset", "#Ycommon matrix\n");
               }
            }
            vt2 = SUMA_FreeMxVec(vt2);
            
            fac = exp((double)(-l*(l+1))*Opt->v0);
            
            /* Steps 4 and 5 here, refitting the residual */
            SUMA_SPHARM_SMOOTH_EST(x, xe, betax, "x");
            SUMA_SPHARM_SMOOTH_EST(y, ye, betay, "y");
            SUMA_SPHARM_SMOOTH_EST(z, ze, betaz, "z");
            if (Opt->debug) SUMA_etime2(FuncName, "Refit residual", FuncName);
         }
         
         /* store new coordinates, in a temp surface, fun to update as we're progressing in case want to feed suma at some point*/
         SOn = SOvn[iso];
         for (i=0; i<SO->N_Node; ++i) {
            i3 = 3*i;
            SOn->NodeList[i3  ] = mxvd1(xe, i);
            SOn->NodeList[i3+1] = mxvd1(ye, i);
            SOn->NodeList[i3+2] = mxvd1(ze, i);
         }
         if (ps->cs->Send) { /* send the smoothed coords  */
            if (l > 0) {
               if (!SUMA_SendToSuma (SO, ps->cs, (void *)SOn->NodeList, SUMA_NODE_XYZ, 1)) {
                  SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
               }
            }
         }

         /* Done with order l */
         if (Ycommon) Ycommon = SUMA_FreeMxVec(Ycommon);
         if (y_l) y_l = SUMA_FreeMxVec(y_l);
         if (y_l_t) y_l_t = SUMA_FreeMxVec(y_l_t);
         if (betal) betal = SUMA_FreeMxVec(betal);
         
         if (Opt->debug ) {
            SUMA_S_Note("MemCheck:");
            MCHECK;
         }
         
         ++iso;
      } while (iso < n_subject);
      NEXT_L:
      ++l; 
   } while (l <= Opt->iopt);
   
   /* done with coordinate containers */
   if (x) x = SUMA_FreeMxVec(x);
   if (y) y = SUMA_FreeMxVec(y);
   if (z) z = SUMA_FreeMxVec(z);
   if (xe) xe = SUMA_FreeMxVec(xe);
   if (ye) ye = SUMA_FreeMxVec(ye);
   if (ze) ze = SUMA_FreeMxVec(ze);
   /* done with Fourier Coefficients */
   if (betax) betax = SUMA_FreeMxVec(betax);
   if (betay) betay = SUMA_FreeMxVec(betay);
   if (betaz) betaz = SUMA_FreeMxVec(betaz);
   
   if (dif) dif = SUMA_FreeMxVec(dif);
   if (sm) sm = SUMA_FreeMxVec(sm);
   
   SUMA_Spherical_Bases(&l, NULL);  /* clean SUMA_Spherical_Bases */

   /* you don't want to exit rapidly because the SUMA might not be done processing the last elements*/
   if (ps->cs->Send && !ps->cs->GoneBad) {
      /* cleanup and close connections */
      if (!SUMA_SendToSuma (SOv[0], ps->cs, NULL, SUMA_NODE_XYZ, 2)) {
         SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCleanup failed");
      }
   }   

   if (SOu) SUMA_Free_Surface_Object(SOu); SOu = NULL;
   for (i=0; i<n_subject; ++i) { 
      if (SOvn[i]) {
         sprintf(stmp, "%d", l);
         if (Opt->out_prefix) { /* user specified prefix, keep formats */
            pref = SUMA_append_string(Opt->out_prefix, stmp);
            form = SOv[i]->FileFormat;
            tp = SOv[i]->FileType;
         } else {
            pref = SUMA_append_string(ps->o_surfnames[0], stmp);
            tp = ps->o_FT[0];
            form = ps->o_FF[0];
         }
         SO_name = SUMA_Prefix2SurfaceName(pref, NULL, NULL, tp, &exists);
         if (exists) {
            fprintf(SUMA_STDERR,"Warning %s:\nOutput file(s) %s* on disk.\nWill overwrite.\n", FuncName, pref);
            exit(1);
         }
         if (Opt->debug) {
            SUMA_S_Notev("Saving surface under prefix %s\n", pref);
         }
         if (!SUMA_Save_Surface_Object (SO_name, SOvn[i], tp, form, NULL)) {
            SUMA_S_Err("Failed to write smoothed surface!");
            exit(1);   
         }
         SUMA_Free_Surface_Object(SOv[i]); SOv[i] = NULL; 
         SUMA_Free_Surface_Object(SOvn[i]); SOvn[i] = NULL; 
         SUMA_free(pref); pref = NULL;
      }
   }
   if (N_Spec) {
      int k=0; 
      for (k=0; k<N_Spec; ++k) {
         if (!SUMA_FreeSpecFields(&(Spec[k]))) { SUMA_S_Err("Failed to free spec fields"); } 
      }
      SUMA_free(Spec); Spec = NULL; N_Spec = 0;
   }
   if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   if (Opt) Opt = SUMA_Free_Generic_Prog_Options_Struct(Opt);
   if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   
   exit(0);
   
} 
