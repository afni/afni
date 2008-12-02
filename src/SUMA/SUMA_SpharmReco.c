#include "SUMA_suma.h"
#include "SUMA_spharm.h"

SUMA_SurfaceViewer *SUMAg_cSV = NULL; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
SUMA_DO *SUMAg_DOv = NULL;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF = NULL; /*!< Global pointer to structure containing info common to all viewers */


void usage_SpharmReco (SUMA_GENERIC_ARGV_PARSE *ps)
{
      static char FuncName[]={"usage_SpharmReco"};
      char * s = NULL, *sio=NULL, *st = NULL, *sts = NULL;
      int i;
      s = SUMA_help_basics();
      sio  = SUMA_help_IO_Args(ps);
      printf ( 
"\n"
"Spherical Harmonics Reconstruction from a set of harmonics \n"
"and their corresponding coefficients.\n"
"\n"
"Usage: \n"
"  SpharmReco <-i_TYPE S> <-l L>\n"
"             <-bases_prefix BASES>\n"
"             <-coef BETA.0> <-coef BETA.1> ...\n"
"             [<-prefix PREFIX>] [<-o_TYPE SDR> ...]\n"
"             [-debug DBG]  [-sigma s]\n" 
"Input:\n"
"  -i_TYPE SURF: SURF is a surface that is only used to provide\n"
"                the topology of the mesh (the nodes' connections)\n"
"  -l L: Decomposition order\n"
"  -bases_prefix BASES_PREFIX: Files containing the bases functions (spherical\n"
"                              harmonics). See SpharmDeco for generating these\n"
"                              files.\n"
"  -coef COEF.n: BETA.n is the coefficients file that is used to recompose \n"
"                the nth data column. These files are created with SpharmDeco.\n"
"                You can specify N coefficient files by repeating the \n"
"                option on command line. If N is a multiple \n"
"                of three AND you use -o_TYPE option, then each three \n"
"                consecutive files are considered to form the XYZ coordinates\n"
"                of a surface. See sample commands in @Spharm.examples \n"
"  -prefix PREFIX: Write out the reconstructed data into dataset PREFIX. \n"
"                  the output dataset contains N columns; one for each of the\n"
"                  COEF.n files.\n"
"  -o_TYPE SDR: Write out a new surface with reconstructed coordinates.\n"
"               This requires N to be a multiple of 3, so 6 -coef options\n"
"               will result in 2 surfaces written to disk. The naming of the\n"
"               surfaces depends on the number of -o_TYPE options used, much \n"
"               like in SpharmDeco\n"
"  -debug DBG: Debug levels (1-3)\n"
"  -sigma s: Smoothing parameter (0 .. 0.001) which weighs down the \n"
"            contribution of higher order harmonics.\n"
"\n"
"-----------------------------------------------------------------------\n"
" For more detail, references, and examples, see script @Spharm.examples  \n"
"-----------------------------------------------------------------------\n"
"\n"
"%s"
"%s"
               "\n", sio,  s);
      SUMA_free(s); s = NULL; SUMA_free(st); 
      st = NULL; SUMA_free(sio); sio = NULL;       
      s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
      printf("       Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov     \n");
      exit(0);
}

SUMA_GENERIC_PROG_OPTIONS_STRUCT *
   SUMA_SpharmReco_ParseInput(char *argv[], int argc, 
                              SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_SpharmReco_ParseInput"}; 
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
   Opt->n_in_namev = 0;
   Opt->v0 = 0.0001;
   kar = 1;
   brk = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_SpharmReco(ps);
          exit (0);
		}
		
		SUMA_SKIP_COMMON_OPTIONS(brk, kar);
      
      if (!brk && (strcmp(argv[kar], "-coef") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need an prefix after -coef \n");
            exit (1);
         }
         
         Opt->in_namev[Opt->n_in_namev++] = argv[++kar];
         brk = YUP;
      }
      
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
			fprintf (SUMA_STDERR,
                  "Error %s:\n"
                  "Option %s not understood. Try -help for usage\n", 
                  FuncName, argv[kar]);
			exit (1);
		} else {	
			brk = NOPE;
			kar ++;
		}
   }
   
   SUMA_RETURN(Opt);
}            

int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"SpharmReco"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt;  
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   SUMA_SurfSpecFile *Spec = NULL;
   int *isin=NULL;
   int   i = -1, ii, jj, kk, il, N_Spec=0, i3, OK, 
         l, lc, ncol=0, nrow=0, dims[20], N_dims=0, N_surfs=0, j, j_l;
   SUMA_Boolean exists=NOPE;
   float *far=NULL, *xbuf=NULL, *ybuf=NULL, *zbuf=NULL;
   double  *dv=NULL, fac = 0.0;
   complex *cv=NULL;
   SUMA_FORM_AFNI_DSET_STRUCT *OptDs = NULL;
   SUMA_SurfaceObject *SO = NULL, *SOt=NULL;
   SUMA_VOLPAR *vp = NULL;
   SUMA_MX_VEC *y_l=NULL, *y_l_t=NULL;
   SUMA_MX_VEC *betal=NULL;
   SUMA_MX_VEC *xe=NULL, *sm=NULL, *yc=NULL;
   SUMA_MX_VEC **axe=NULL, **abeta=NULL;
   char *oname=NULL, stmp[100], *pref=NULL;
   SUMA_SO_File_Format form=SUMA_FF_NOT_SPECIFIED;
   SUMA_SO_File_Type tp=SUMA_FT_NOT_SPECIFIED;
   SUMA_OPT_SPHERICAL_BASES optb;
   void *SO_name=NULL;
   struct  timeval tt; 
   int oform= SUMA_NO_DSET_FORMAT;
   char *ooo=NULL;
   float *fbuf=NULL;
   SUMA_DSET *out_dset = NULL;
   SUMA_Boolean do_surf_xyz = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_STANDALONE_INIT;
	SUMA_mainENTRY;

   /* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "-i;-spec;-talk;-o;");
 
   if (argc < 2) {
      usage_SpharmReco(ps);
      exit (1);
   }
   
   Opt = SUMA_SpharmReco_ParseInput (argv, argc, ps);
   
   if (Opt->debug) LocalHead = YUP;
   if (Opt->n_in_namev < 1) {
      SUMA_S_Err("No Coef!");
      exit (1);
   }
   if (ps->o_N_surfnames) do_surf_xyz = YUP;
   
   if (!ps->o_N_surfnames && !Opt->out_prefix) {
      SUMA_S_Notev("Using default prefix of %s\n", "spharm_sm");
      Opt->out_prefix = SUMA_copy_string("spharm_sm");
   }

   if ((Opt->n_in_namev % 3) && do_surf_xyz) {
      SUMA_S_Errv("Number of coefficient options (%d) must be\n"
                  "a multiple of three if output is \n"
                  "to be treated as x, y, z coordinates\n", Opt->n_in_namev);
      exit(1);            
   }
   /* decide on output form */
   if (Opt->out_prefix) {
      oform = SUMA_GuessFormatFromExtension(Opt->out_prefix,"something.1D.dset");
   }
   if (Opt->debug > 2) LocalHead = YUP;
   
   /* check on inputs */
   optb.SOu=NULL;
   optb.BasesFileRoot=Opt->bases_prefix;
   optb.SaveBases=Opt->bases_prefix;
   optb.debug = Opt->debug;
   
   N_surfs = ps->s_N_surfnames + ps->i_N_surfnames + ps->t_N_surfnames;
   if (( N_surfs != 1)) {
      SUMA_S_Errv("You must provide only one surface.\n"
                  "Have %d on command line.\n", N_surfs);
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
      if (!SO->normdir) SO->normdir = 1;  /* set it to something */
      SOt = SUMA_CreateChildSO( SO, NULL, -1, NULL, -1, 0);
      if (!SOt->State) {SOt->State = SUMA_copy_string("spharm_domain"); }
      if (!SOt->Group) {SOt->Group = SUMA_copy_string("spharm_domain"); }
   }
   
   /* see if SUMA talk is turned on */
   if (ps->cs->talk_suma) {
      ps->cs->istream = SUMA_GEOMCOMP_LINE;
      if (!SUMA_SendToSuma (SO, ps->cs, NULL, SUMA_NO_DSET_TYPE, 0)) {
         SUMA_SL_Err("Failed to initialize SUMA_SendToSuma");
         ps->cs->Send = NOPE;
         ps->cs->talk_suma = NOPE;
      }
      SUMA_SendSumaNewSurface(SO, ps->cs);
   }
   
   /* Number of coefficients */
   axe   = (SUMA_MX_VEC **)SUMA_calloc(Opt->n_in_namev, sizeof(SUMA_MX_VEC*));
   abeta = (SUMA_MX_VEC **)SUMA_calloc(Opt->n_in_namev, sizeof(SUMA_MX_VEC*));
   
   /* initialize output variables  */
   for (i=0; i<Opt->n_in_namev; ++i) {
      dims[0] = SO->N_Node; dims[1] = 1;  N_dims = 2;
      axe[i]   = SUMA_NewMxVec(SUMA_double, 2, dims, 1);
      if (Opt->debug > 1)  SUMA_ShowMxVec(axe[i], 1, NULL, "\naxe[i]\n");
      N_dims = -1;            /* take whatever is in file, otherwise, 
                                 should setup dims[0] and dims[1]    */
      abeta[i] = SUMA_Read1DMxVec(SUMA_double, Opt->in_namev[i], dims, &N_dims);
      if (Opt->debug > 1)  SUMA_ShowMxVec(abeta[i], 1, NULL, "\nabeta[i]\n");
   }

   SUMA_Spherical_Bases(&l, NULL);  /* init SUMA_Spherical_Bases */
  
   /* start timer*/
   SUMA_etime2(FuncName, NULL, NULL);
   
   /* A record of matrices and processes to follow for spharm reconstruction
      Here y_l, for each l, and beta are loaded from files created by SpharmDeco  
      N_Node   : Number of points in parametrization of sphere
      sigma    : the smoothing kernel 0 == No smoothing, 0.01 
               (quite a bit of smoothing).
                 the higher the sigma the more the attenuation of higher 
                 degree harmonics. 
      for l = 0;
         y_l      : l th degree harmonic             complex l+1      x  N_Node
                    turned to real since 
                    imaginary is all 0
         y_l_t    : y_l'                              double  N_Node   x  l+1
         XX vt       : y_l * y_l_t                    double  l+1      x  l+1
         XX vt2      : inv(vt)                        double  l+1      x  l+1  
         XX Ycommon  : vt2 * y_l                      double  l+1      x  N_Node
         XX betal    : Ycommon * x                    double  l+1      x  1
         betal    : filled from beta(l,0:2l)          double  l+1      x  1
                    (beta(0,0), really)  
         xe       : y_l_t * betal                     double  N_Node   x  1
      
      for l > 0;
         y_l      : l th degree harmonic              complex l+1      x  N_Node
                    turned to real of dimensions:     double  2*l+1    x  N_Node
                    where the imaginary part of y_l 
                    is appended below the real part
         y_l_t    : y_l'                               double  N_Node   x  2*l+1
         XX vt       : y_l * y_l_t                     double  2*l+1    x  2*l+1
         XX vt2      : inv(vt)                         double  2*l+1    x  2*l+1
         XX Ycommon  : vt2 * y_l                       double  2*l+1    x  N_Node
         XX dif_vec_x: x - xe                          double  N_Node   x  1
         betal    : filled from beta(l,0:2l)           double  2*l+1    x  1
         sm       : y_l_t * betal                      double  N_Node   x  1
         fac      : exp((-l*(l+1))*sigma)              double  1        x  1          
         xe       : xe + fac * sm                      double  N_Node   x  1
                    xe is the estimate of x up to order l harmonics
      
      beta     : lower triangular matrix where betal   double  l+1      x 2l+1
                 are  stored for all l degrees
   */
   l=0;
   do {
      
      lc = l;
      y_l = SUMA_Spherical_Bases(&lc, &optb);   
         /*  y_l is equal to Y' in Moo's SPHARsmooth.m function   */
      if (lc < l) {                             
         /*  The function will read in the bases from disk        */
         Opt->iopt = lc;   /*  y_l is a (l+1 x N_Node) complex matrix.    */
         SUMA_S_Notev("Cannot go for order higher than %d\n.", lc);
         goto NEXT_L;
      }
      
      do {
         if (SO) {
            if ((LocalHead && l == 0) || Opt->debug > 1) 
               SUMA_S_Notev("Using the mesh from %s for \n"
                            "a reconstruction of degree %d.\n", SO->Label, l);
            if (Opt->debug > 2) SUMA_Print_Surface_Object (SO, SUMA_STDERR);
         } else {
            SUMA_S_Err("NULL SO!!!");
            exit(1);
         }
         if (LocalHead || Opt->debug > 1)  {
                  fprintf(SUMA_STDERR,"%s: Doing l = %d\n", FuncName, l);
                  SUMA_etime2(FuncName, "Entering loop", FuncName);
         }
         if (l==0) {
            yc = SUMA_CoerceMxVec(y_l, SUMA_double, 0, NULL); 
               /* for l == 0, all imaginary comp. are 0 */
            y_l = SUMA_FreeMxVec(y_l); y_l = yc; yc = NULL; 
            y_l_t = SUMA_MxVecTranspose(y_l, NULL);
               /*  y_l is equal to Y' in Moo's SPHARsmooth.m function*/
            if (Opt->debug > 1)  
               SUMA_ShowMxVec(y_l_t, 1, NULL, "\ny_l_t matrix\n");
            dims[0] = 2*l+1; dims[1] = 1;
            betal = SUMA_NewMxVec(SUMA_double, 2, dims, 1);
            for (jj=0; jj<Opt->n_in_namev; ++jj) {
               for (i=0;i<=2*l;++i) { mxvd2(betal,i,0) = mxvd2(abeta[jj],l, i); }
               if (Opt->debug > 1) 
                  SUMA_ShowMxVec(betal, 1, NULL, "\nbetal matrix\n");
               xe = SUMA_MxVecMult(y_l_t, betal, NULL, 0);
               if (Opt->debug > 1) {
                  SUMA_ShowMxVec(xe, 1, NULL, "\nxe vector\n");
                  if (Opt->debug > 2) 
                     SUMA_WriteMxVec(xe, "xe_l0.1D", "#xe vector");
               }
               axe[jj] = xe; xe = NULL;
            }
         } else { /* higher order, need to deal with real and imaginary parts*/
            /* Catenate the columns of y_l with the real columns first 
            (negative harmonics), followed by imaginary ones (positive harms.)*/ 
            
            sprintf(stmp, "Starting with order l=%d", l);
            if (Opt->debug) SUMA_etime2(FuncName, stmp, FuncName);
            y_l = SUMA_YLcomp_to_YLdoub(&y_l, Opt->debug); /* Now y_l is real */
            if (Opt->debug ) SUMA_etime2(FuncName, "Created y_l", FuncName);
            
            if (Opt->debug > 2) {
               SUMA_WriteMxVec(y_l, "y_l.1D.dset", "#y_l real matrix\n");
            }
            y_l_t = SUMA_MxVecTranspose(y_l, NULL);   
               /*  y_l is equal to Y' in Moo's SPHARsmooth.m function*/
            if (Opt->debug > 1)  
               SUMA_ShowMxVec(y_l_t, 1, NULL, "\ny_l_t matrix\n");
            if (Opt->debug ) SUMA_etime2(FuncName, "Trasnposed y_l", FuncName);
            
            fac = exp((double)(-l*(l+1))*Opt->v0);
            dims[0] = 2*l+1; dims[1] = 1;
            betal = SUMA_NewMxVec(SUMA_double, 2, dims, 1);
            for (jj=0; jj<Opt->n_in_namev; ++jj) {
               xe = axe[jj];
               for (i=0;i<=2*l;++i) { mxvd2(betal,i,0) = mxvd2(abeta[jj],l, i); }
               if (Opt->debug > 1) 
                  SUMA_ShowMxVec(betal, 1, NULL, "\nbetal matrix\n");
               sm = SUMA_MxVecMult(y_l_t, betal, sm, 0); 
               if (Opt->debug > 1) { 
                  sprintf(stmp,"\nsm_%d  vector\n", jj);   
                  SUMA_ShowMxVec(sm , 1, NULL, stmp); 
               }  
               for (i=0;i<xe->N_vals;++i) {   
                  mxvd1(xe,i) += fac * mxvd1(sm,i);   
               }  
               if (Opt->debug > 1) { 
                  sprintf(stmp,"\n%d_estimate  vector\n", jj);   
                  SUMA_ShowMxVec(xe, 1, NULL, stmp);    
                  sprintf(stmp,"%d_estimate_l%d.1D.dset", jj, l);   
                  if (Opt->debug > 2) 
                     SUMA_WriteMxVec(xe, stmp, "#estimate at last l\n"); 
               }
            }  
            if (Opt->debug) SUMA_etime2(FuncName, "Refit residual", FuncName);
         }
         
         /* store new coordinates, in a temp surface, fun to update as we're
            progressing in case want to feed suma at some point*/
         if (do_surf_xyz) {
            if (l==0 && ps->cs->Send && Opt->debug) {
               SUMA_S_Warn("Assuming coefficients are XYZ of surfaces !\n"
                           "Only the 1st three data columns will be used\n"
                           "in talk_suma mode.\n"
                           "Warnings muted for l > 0\n");
            }
            for (i=0; i<SO->N_Node; ++i) {
               i3 = 3*i;
               SOt->NodeList[i3  ] = mxvd1(axe[0], i);
               SOt->NodeList[i3+1] = mxvd1(axe[1], i);
               SOt->NodeList[i3+2] = mxvd1(axe[2], i);
            }
         }
         
         if (ps->cs->Send) { /* send the smoothed coords  */
            if (do_surf_xyz) { /* surface coordinates */
               if (l > 0) {
                  if (Opt->debug) {
                     SUMA_S_Notev("[%f %f %f]\n", 
                              SOt->NodeList[0], 
                              SOt->NodeList[1], SOt->NodeList[2]);
                  }
                  if (!SUMA_SendToSuma (  SO, ps->cs, (void *)SOt->NodeList, 
                                          SUMA_NODE_XYZ, 1)) {
                     SUMA_SL_Warn(  "Failed in SUMA_SendToSuma\n"
                                    "Communication halted.");
                  }
               }
            } else {
               if (l > 0) {
                  if (!fbuf) fbuf = (float *)SUMA_malloc(SO->N_Node*
                                                         sizeof(float));
                  for (i=0; i<SO->N_Node; ++i) fbuf[i] = mxvd1(axe[0], i);
                  if (!SUMA_SendToSuma (  SO, ps->cs, 
                                          (void *)fbuf, 
                                          SUMA_NODE_RGBAb, 1)) {
                     SUMA_SL_Warn(  "Failed in SUMA_SendToSuma\n"
                                    "Communication halted.");
                  }
               }
            }
         }

         /* Done with order l */
         if (y_l) y_l = SUMA_FreeMxVec(y_l);
         if (y_l_t) y_l_t = SUMA_FreeMxVec(y_l_t);
         if (betal) betal = SUMA_FreeMxVec(betal);
         
         if (Opt->debug ) {
            SUMA_S_Note("MemCheck:");
            MCHECK;
         }
         
      } while (0);
      NEXT_L:
      ++l; 
   } while (l <= Opt->iopt);
   --l; /* back to where you stopped, kid */
   
   
   /* Now create an output data set for the reconstructed data */
   out_dset = SUMA_CreateDsetPointer(
         Opt->out_prefix ? Opt->out_prefix:"spharm_reco",  
         SUMA_NODE_BUCKET,                /* mix and match */
         NULL,    /* no idcode, let the function create one from the filename*/
         NULL,       /* no domain str specified */
         SO->N_Node    /* Number of nodes allocated for */
         );
   /* add results */
   for (jj=0; jj<Opt->n_in_namev; ++jj) {
      if (!fbuf) fbuf = (float *)SUMA_malloc(SO->N_Node*sizeof(float));
      for (i=0; i<SO->N_Node; ++i) fbuf[i] = mxvd1(axe[jj], i);
      if (!SUMA_AddDsetNelCol(  out_dset, "sp_reco", SUMA_NODE_FLOAT, 
                                fbuf, NULL, 1)) {
            SUMA_S_Err("Failed to update output.");
            exit(1);
      }
      axe[jj] = SUMA_FreeMxVec(axe[jj]); 
   }
   
   if (Opt->out_prefix) {
      /* write out the data */
      ooo = SUMA_WriteDset_s( Opt->out_prefix, out_dset, 
                              oform, THD_ok_overwrite(), 0);
      if (Opt->debug) SUMA_S_Notev("Wrote %s\n", ooo);
      SUMA_free(ooo); ooo=NULL;   
   }
   
   /* do we need to write out surfaces? */
   if (do_surf_xyz && ps->o_N_surfnames) {
      for (i=0; i<Opt->n_in_namev/3; ++i) {   
         if (ps->o_N_surfnames == Opt->n_in_namev/3) {
            pref = SUMA_copy_string(ps->o_surfnames[i]);
            tp = ps->o_FT[i];
            form = ps->o_FF[i];
         } else {
            sprintf(stmp, "s%02d", i);
            pref = SUMA_append_string(ps->o_surfnames[0], stmp);
            tp = ps->o_FT[0];
            form = ps->o_FF[0];
         }
         if (Opt->debug) {
            SUMA_S_Notev("Prepping to write surface %s\n"
                         "with X Y Z from cols. %d %d %d\n"
                         , pref, i*3, 1+i*3, 2+i*3);
         }
         xbuf = (float*)out_dset->dnel->vec[0+i*3];
         ybuf = (float*)out_dset->dnel->vec[1+i*3];
         zbuf = (float*)out_dset->dnel->vec[2+i*3];
         for (ii=0; ii<SO->N_Node; ++ii) {
            i3 = 3*ii;
            SO->NodeList[i3  ] = xbuf[ii];
            SO->NodeList[i3+1] = ybuf[ii];
            SO->NodeList[i3+2] = zbuf[ii];
         }
         /* write the surface */
         SO_name = SUMA_Prefix2SurfaceName(pref, NULL, NULL, tp, &exists);
         if (!THD_ok_overwrite() && exists) {
            fprintf(SUMA_STDERR,"Warning %s:\nOutput surface %s* on disk.\n"
                                "Will not overwrite.\n", FuncName, pref);
            exit(1);
         }
         if (Opt->debug) {
            SUMA_S_Notev("Saving surface under prefix %s\n", pref);
         }
         if (!SUMA_Save_Surface_Object (SO_name, SO, tp, form, NULL)) {
            SUMA_S_Err("Failed to write reconstructed surface!");
            exit(1);   
         }
      }
   }
   
   /* clean and quit */
   if (fbuf) SUMA_free(fbuf);
   
   for (i=0; i<Opt->n_in_namev; ++i) {
      SUMA_FreeMxVec(abeta[i]);
   }
   SUMA_free(axe); axe = NULL;
   SUMA_free(abeta); abeta = NULL;
   
   if (sm) sm = SUMA_FreeMxVec(sm);
   
   SUMA_Spherical_Bases(&l, NULL);  /* clean SUMA_Spherical_Bases */

   /* you don't want to exit rapidly because the 
      SUMA might not be done processing the last elements*/
   if (ps->cs->Send && !ps->cs->GoneBad) {
      /* cleanup and close connections */
      if (!SUMA_SendToSuma (SO, ps->cs, NULL, SUMA_NODE_XYZ, 2)) {
         SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCleanup failed");
      }
   }   
   
   if (N_Spec) {
      int k=0; 
      for (k=0; k<N_Spec; ++k) {
         if (!SUMA_FreeSpecFields(&(Spec[k]))) { 
            SUMA_S_Err("Failed to free spec fields"); } 
      }
      SUMA_free(Spec); Spec = NULL; N_Spec = 0;
   }
   if (out_dset) SUMA_FreeDset(out_dset); out_dset = NULL;
   if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   if (Opt) Opt = SUMA_Free_Generic_Prog_Options_Struct(Opt);
   if (!SUMA_Free_CommonFields(SUMAg_CF)) 
      SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   
   exit(0);
   
} 
