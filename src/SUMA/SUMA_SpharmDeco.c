#include "SUMA_suma.h"
#include "SUMA_spharm.h"

void usage_SpharmDeco (SUMA_GENERIC_ARGV_PARSE *ps)
{
      static char FuncName[]={"usage_SpharmDeco"};
      char * s = NULL, *sio=NULL, *st = NULL, *sts = NULL;
      int i;
      s = SUMA_help_basics();
      sio  = SUMA_help_IO_Args(ps);
      printf ( 
"\n"
"Spherical Harmonics Decomposition of a surface's coordinates or data\n"
"Model:\n"                  
"%s"
"\n"
"Usage:\n"
"       SpharmDeco  <-i_TYPE S> -unit_sph UNIT_SPH_LABEL> <-l L>\n"
"                   [<-i_TYPE SD> ... | <-data D>] \n"
"                   [-bases_prefix BASES] \n"
"                   [<-prefix PREFIX>] [<-o_TYPE SDR> ...]\n"
"                   [-debug DBG]  [-sigma s]\n"
"  \n"
"Input: \n"
"  -i_TYPE S: Unit sphere, isotopic to the surface domain over which the \n"
"                    data to be decomposed is defined.\n"
"                    This surface is used to calculate the basis functions \n"
"                    up to order L.\n"
"                    These basis functions are saved under \n"
"                    the prefix BASES_PREFIX.\n"
"                    Note that this surface does not need to be of \n"
"                    radius 1. \n"
"  -unit_sph UNIT_SPH_LABEL: Provide the label of the unit sphere. \n"
"                   If you do not do that, the program won't know \n"
"                   which of the two -i_TYPE options specifies the \n"
"                   unit sphere.\n"
"  -l L: Decomposition order\n"
"  One of:\n"
"     -i_TYPE SD: A surface that is isotopic to S and whose node coordinates \n"
"                 provide three data vectors (X, Y, Z) to be decomposed\n"
"                 See help section on surface input to understand the\n"
"                 syntax of -i_TYPE\n"
"                 You can specify multiple surfaces to be processed by \n"
"                 using repeated instances of -i_TYPE SD option. This is more\n"
"                 computationally efficient than doing each surface separately."
"    or \n"
"     -data D: A dataset whose K columns are to be individually decomposed. \n"
"\n"
"  -bases_prefix BASES_PREFIX: If -unit_sph is used, this option save the\n"
"                              bases functions under the prefix BASES_PREFIX\n"
"                              Otherwise, if BASES_PREFIX exists on disk, the\n"
"                              program will reload them. This is intended to\n"
"                              speed up the program, however, in practice, \n"
"                              this may not be the case.\n"
"                           Note that the bases are not reusable with a\n"
"                              different unit sphere. \n" 
"  -debug DBG: Debug levels (1-3)\n"
"  -sigma s: Smoothing parameter (0 .. 0.001) which weighs down the \n"
"            contribution of higher order harmonics.\n"
"  -prefix PREFIX: Write out the reconstructed data into dataset PREFIX\n"
"                  and write the beta coefficients for each processed \n"
"                  data column. Note that when you are using node \n"
"                  coordinates form J surfaces, the output will be for \n"
"                  3*J columns with the 1st triplet of columns for the first \n"
"                  surface's X Y Z coordinates and the 2nd triplet for the\n"
"                  second surface's coordinates, etc.\n"
"  -o_TYPE SDR: Write out a new surface with reconstructed coordinates.\n"
"               This option is only valid if -i_TYPE SD is used.\n"
"               See help section on surface output to understand the\n"
"               syntax of -o_TYPE.\n"
"               If you specify multiple (M) SD surfaces, you will get M\n"
"               reconstructed surfaces out. They can be named in one of\n"
"               two ways depending on how many -o_TYPE options you use.\n"
"               If only one -o_TYPE is used, then M names are automatically\n"
"               generated by appending .sXX to SDR. Alternately, you can \n"
"               name all the output surfaces by using M -o_TYPE options.\n" 
"\n"
"Output files:\n"
"  Harmonics of each order l are stored in a separate\n"
"     file with the order l in its name. For example for l = 3, the harmonics\n"
"     are stored in a file called  BASES_PREFIX.sph03.1D.\n"
"     In the simplest form, this file is in .1D format and contains an\n" 
"     (l+1 x N) complex matrix. The real part constitutes the negative degree\n" "     harmonics and the positive part contains the positive degree ones.\n"
"     (Internally, the complex matrix is turned into a real matrix of size \n"
"      2l+1 x N )\n" 
"  Beta coefficients are stored in one for each of the input K data columns.\n"
"     For example the beta coefficients for the data column 2 is called: \n"
"     PREFIX.beta.col002.1D.dset. \n"
"     The (l+1 x 2l+1) matrix in each file in real valued with each row \n"
"     containing coefficients that for order l.\n"
"  Surface or data reconstruction files are named based on PREFIX. \n"
"\n"
"This program is based on Moo Chung's matlab implementation of spherical\n"
"  harmonics decomposition which is presented in: \n"
"  Chung, M.K., Dalton, K.M., Shen, L., L., Evans, A.C., Davidson, R.J. 2006. \n"
"  Unified cortical surface morphometry and its application to quantifying\n"
"  amount of gray matter. \n" 
"  Technical Report 1122. \n"
"  Department of Statistics, University of Wisconsin-Madison.\n"
"  http://www.stat.wisc.edu/~mchung/papers/TR1122.2006.pdf \n" 
"\n"
"-------------------------------------------\n"
" For examples, see script @Spharm.examples  \n"
"-------------------------------------------\n"
"\n"
"%s"
"\n"
"%s"
"\n", SpharmEquation, sio,  s);
      SUMA_free(s); s = NULL; SUMA_free(st); st = NULL; 
      SUMA_free(sio); sio = NULL;       
      s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
      printf("       Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov     \n");
      exit(0);
}

SUMA_GENERIC_PROG_OPTIONS_STRUCT *
   SUMA_SpharmDeco_ParseInput(char *argv[], int argc, 
                              SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_SpharmDeco_ParseInput"}; 
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
   Opt->in_name = NULL;
   kar = 1;
   brk = NOPE;
	while (kar < argc) { /* loop across command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_SpharmDeco(ps);
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
      
      if (!brk && (strcmp(argv[kar], "-data") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a string after -data \n");
            exit (1);
         }
         Opt->in_name = argv[++kar];
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
   
   Opt->ps = ps;
   
   SUMA_RETURN(Opt);
}            

int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"SpharmDeco"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt;  
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   SUMA_SurfSpecFile *Spec = NULL;
   int *isin=NULL;
   int   i = -1, ii, jj, kk, il, N_Spec=0, 
         i3, OK, l, lc, ncol=0, nrow=0, dims[20], 
         N_dims=0, N_surfs=0, j, j_l;
   int n_subject=1,  iso=0;
   SUMA_Boolean exists=NOPE;
   float *far=NULL;
   double  *dv=NULL, fac = 0.0, *dbuf=NULL;
   byte *nmask=NULL;
   int N_nmask;
   complex *cv=NULL;
   SUMA_FORM_AFNI_DSET_STRUCT *OptDs = NULL;
   SUMA_SurfaceObject   *SO = NULL, *SOu=NULL, *SOt=NULL,
                        *SOn=NULL, *SOv[500], *SOvn[500];
   SUMA_VOLPAR *vp = NULL;
   SUMA_MX_VEC *y_l=NULL, *y_l_t=NULL , *x=NULL;
   SUMA_MX_VEC *betal=NULL, **betax=NULL,  
               *xsmooth = NULL ;
   SUMA_MX_VEC *zestimate=NULL, **xe=NULL,  
               *dif = NULL, *sm=NULL;
   char *oname=NULL, stmp[100], *pref=NULL;
   SUMA_SO_File_Format form=SUMA_FF_NOT_SPECIFIED;
   SUMA_SO_File_Type tp=SUMA_FT_NOT_SPECIFIED;
   SUMA_OPT_SPHERICAL_BASES optb;
   void *SO_name=NULL;
   struct  timeval tt;
   SUMA_DSET *data_dset=NULL, *out_dset=NULL; 
   int oform= SUMA_NO_DSET_FORMAT, icol;
   char *ooo=NULL;
   SUMA_Boolean do_surf_xyz = NOPE;
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
      usage_SpharmDeco(ps);
      exit (1);
   }
   
   #if 0 /* a couple of tests of SUMA_pLegendre function */
   SUMA_S_Notev("legendre(1, 0, cos(0.4375))=%g\n", SUMA_pLegendre(1,0,0.4375));
   SUMA_S_Notev("legendre(1, 1, cos(0.4375))=%g\n", SUMA_pLegendre(1,1,0.4375));
   SUMA_S_Notev("legendre(1, 0, 0.4375)=%g\n", SUMA_pLegendre(1,0,acos(0.4375)));
   SUMA_S_Notev("legendre(1, 1, 0.4375)=%g\n", SUMA_pLegendre(1,1,acos(0.4375)));
   exit(1);
   #endif
   
   Opt = SUMA_SpharmDeco_ParseInput (argv, argc, ps);
   
   if (Opt->debug) LocalHead = YUP;
   
   if (!Opt->in_name || !strcmp(Opt->in_name,"surf_xyz")) {
      do_surf_xyz = YUP;
   } else {
      do_surf_xyz = NOPE;
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
   SUMA_LHv("Have %d surfaces on command line\n", N_surfs);
   if (N_surfs < 1) {
      SUMA_S_Errv("At least one surface is needed.\n"
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
         if (     (  SO->Name.FileName && 
                     SUMA_iswordin(SO->Name.FileName, Opt->unit_sphere_name))
               || (  SO->Name_coord.FileName && 
                     SUMA_iswordin( SO->Name_coord.FileName, 
                                    Opt->unit_sphere_name))
               || (  SO->Name_topo.FileName && 
                     SUMA_iswordin( SO->Name_topo.FileName, 
                                    Opt->unit_sphere_name)) ) {
            if (!optb.SOu) {
               optb.SOu = SO; SO = NULL;
               SUMA_LHv("Set %s as unit sphere.\n", Opt->unit_sphere_name);
            } else {
               SUMA_S_Err( "Have conflict in determining unit sphere surface.\n"
                           "Please post command line on AFNI'smessage board.\n");
               exit(1);
            }  
         } else {
            if (!SO->normdir) SO->normdir = 1;  /* set it to something */
            SOv[n_subject] = SO;
            SOvn[n_subject] = SUMA_CreateChildSO( SO, NULL, -1, NULL, -1, 0);
            ++n_subject; SOv[n_subject] = NULL; SOvn[n_subject] = NULL; 
         }
      } else {
         if (n_subject) {
            if (SO->N_Node != SOv[0]->N_Node) {
               SUMA_S_Errv("%d%s surface %s is not isotopic with %s\n",
                           n_subject+1, SUMA_COUNTER_SUFFIX(n_subject+1),
                           SO->Label, SOv[0]->Label);
               exit(1); 
            }
         }
         if (!SO->normdir) SO->normdir = 1;  /* set it to something */
            SOv[n_subject] = SO;
            SOvn[n_subject] = SUMA_CreateChildSO( SO, NULL, -1, NULL, -1, 0);
            ++n_subject; SOv[n_subject] = NULL; SOvn[n_subject] = NULL; 
      }  
   }
   if (optb.SOu && n_subject) {
      if (optb.SOu->N_Node != SOv[0]->N_Node) {
         SUMA_S_Err("Unit sphere and other surface(s) not isotopic");
         exit(1);
      }
   }
   if (!optb.SOu && !Opt->bases_prefix) {
      SUMA_S_Err("Have no unit sphere or bases_prefix specified");
      exit(1);
   }
   
   /* see if SUMA talk is turned on */
   if (ps->cs->talk_suma && n_subject > 1) {
      if (n_subject > 1) {
         SUMA_S_Warn(  "Can't talk with more than one surface, "
                        "only 1st will be sent.\n");
      } else if (!n_subject && !optb.SOu) {
         SUMA_S_Warn(  "No surfaces. Cannot talk to suma. ");
         ps->cs->talk_suma = NOPE;
      }
   }
   if (ps->cs->talk_suma) {
      ps->cs->istream = SUMA_GEOMCOMP_LINE;
      if (SOv[0]) {
         SOt = SOv[0];
      } else if (optb.SOu) {
         SOt = optb.SOu;
      } else {
         SOt = NULL;
      }
      if (!SUMA_SendToSuma (SOt, ps->cs, NULL, SUMA_NO_DSET_TYPE, 0)) {
         SUMA_SL_Err("Failed to initialize SUMA_SendToSuma");
         ps->cs->Send = NOPE;
         ps->cs->talk_suma = NOPE;
      }
      SUMA_SendSumaNewSurface(SOt, ps->cs);
   }
   
   SUMA_LHv("Have %d surfaces to process\n", n_subject);
   if (optb.SOu) {
      SUMA_LHv("Have surface %s as unit sphere.\n", optb.SOu->Label);
   }
   
   SUMA_LH("Initializing data dset");
   /* Now deal with data */
   if (do_surf_xyz ) { 
      if (Opt->debug > 2) SUMA_Show_IO_args(Opt->ps);
      if (!(nmask = 
            SUMA_load_all_command_masks(  Opt->ps->bmaskname, 
                                          Opt->ps->nmaskname, 
                                          Opt->ps->cmask, 
                                          SOv[0]->N_Node, &N_nmask)) 
         && N_nmask < 0) {
         SUMA_S_Err("Failed loading mask");
         exit(1);
      }  
      /* form a dataset */
      if (!(data_dset = SUMA_CreateDsetPointer("NodeXYZ", SUMA_NODE_BUCKET,NULL,
                                               NULL, SOv[0]->N_Node ))) {
         SUMA_S_Err("Failed to form dset");
         exit(1);
      }
      SUMA_PopulateDsetNodeIndexNel(data_dset, 0);
      for (i=0; i<n_subject; ++i) {
         SO = SOv[i];
         sprintf(stmp, "s%02d.Ze_X", i);
         if (!SUMA_AddDsetNelCol (data_dset, stmp, SUMA_NODE_FLOAT, 
                                 (void *)(SO->NodeList), NULL ,3)) {
            SUMA_S_Err("Failed to add X col.");
            exit(1);
         }
         sprintf(stmp, "s%02d.Ze_Y", i);
         if (!SUMA_AddDsetNelCol (data_dset, stmp, SUMA_NODE_FLOAT, 
                                 (void *)(SO->NodeList+1), NULL ,3)) {
            SUMA_S_Err("Failed to add Y col.");
            exit(1);
         }
         sprintf(stmp, "s%02d.Ze_Z", i);
         if (!SUMA_AddDsetNelCol (data_dset, stmp, SUMA_NODE_FLOAT, 
                                 (void *)(SO->NodeList+2), NULL ,3)) {
            SUMA_S_Err("Failed to add Z col.");
            exit(1);
         }
      }
      oform = SUMA_1D;
   } else {
      /* load dataset */
      data_dset = SUMA_LoadDset_s (Opt->in_name, &oform, 0);
      if (!data_dset) {
         SUMA_S_Errv("Failed to read %s as a surface dset\n", Opt->in_name);
         exit(1);
      }
      /* Need to deal with prefix for the output ... */
      oform = SUMA_GuessFormatFromExtension("dset_prefix.1D", Opt->in_name);
   }
   
   if (Opt->debug) {
      SUMA_S_Notev(  "Have %d columns to process.\n"
                     "But first, initializing output dset...\n", 
                     SDSET_VECNUM(data_dset));
   }
   /* initialize output dataset  */
   if (!(out_dset = SUMA_MaskedCopyofDset(data_dset, NULL, NULL, 0, 1))) { 
      SUMA_S_Err("Failed to initialize output dset");
      exit(1);                                   
   }
   
   

   /* A record of matrices and processes to follow for spharm decomposition 
      N_Node   : Number of points in parametrization of sphere
      x        : data to model ( x coord for example)             
                                          double     N_Node   x  1  
      sigma    : the smoothing kernel 0 == No smoothing, 0.01 
                  (quite a bit of smoothing).
                 the higher the sigma the more the attenuation 
                 of higher degree harmonics. 
      for l = 0;
         y_l      : l th degree harmonic            complex l+1      x  N_Node
                    turned to real since imaginary is all 0
         y_l_t    : y_l'                            double  N_Node   x  l+1
         vt       : y_l * y_l_t                     double  l+1      x  l+1
         vt2      : inv(vt)                         double  l+1      x  l+1  
         Ycommon  : vt2 * y_l                       double  l+1      x  N_Node
         betal    : Ycommon * x                     double  l+1      x  1
         xe       : y_l_t * betal                   double  N_Node   x  1
      
      for l > 0;
         y_l      : l th degree harmonic          complex l+1      x  N_Node
                    turned to real of dimensions: double  2*l+1    x  N_Node
                    where the imaginary part of y_l is appended below the real
                    part
         y_l_t    : y_l'                          double  N_Node   x  2*l+1
         vt       : y_l * y_l_t                   double  2*l+1    x  2*l+1
         vt2      : inv(vt)                       double  2*l+1    x  2*l+1
         Ycommon  : vt2 * y_l                     double  2*l+1    x  N_Node
         dif_vec_x: x - xe                        double  N_Node   x  1
         betal    : Ycommon * dif_vec_x           double  2*l+1    x  1
         sm       : y_l_t * betal                 double  N_Node   x  1
         fac      : exp((-l*(l+1))*sigma)         double  1        x  1          
         xe       : xe + fac * sm                 double  N_Node   x  1
                    xe is the estimate of x up to order l harmonics
      
      beta     : lower triangular matrix where   double  l+1      x  2l+1
                 betal coefficients are stored for all l degrees
   */
   
   SUMA_Spherical_Bases(&l, NULL); /* init SUMA_Spherical_Bases */
   
   /* start timer*/
   SUMA_etime2(FuncName, NULL, NULL);
   
   l=0;
   do { /* for each order l */
      SUMA_MX_VEC *Ycommon=NULL, *vt=NULL, *vt2=NULL, *yc = NULL;
      
      lc = l;
      if (!(y_l = SUMA_Spherical_Bases(&lc, &optb))) {
         SUMA_S_Err("Failed to get basis function");
         exit(1);
      }
         /*  y_l is equal to Y' in Moo's SPHARsmooth.m function*/
      if (lc < l) {
         Opt->iopt = lc;
         SUMA_S_Notev("Cannot go for order higher than %d\n.", lc);
         goto NEXT_L;
      }
      
      /* prepare matrices common to all columns */
      if (l==0) {
         yc = SUMA_CoerceMxVec(y_l, SUMA_double, 0, NULL);  
            /* NEED TO COERCE y_l to be double, for order 0 */
         y_l = SUMA_FreeMxVec(y_l); y_l = yc; yc = NULL;    
            /* it is all real. No need to carry 0i all over here ...  */
         y_l_t = SUMA_MxVecTranspose(y_l, NULL);
            /*  y_l is equal to Y' in Moo's SPHARsmooth.m function*/
         if (Opt->debug > 1)  
            SUMA_ShowMxVec(y_l_t, 1, NULL, "\ny_l_t matrix\n");
            vt = SUMA_MxVecMult(y_l,y_l_t, NULL, MATRIX_B_IS_AT);
         if (Opt->debug > 1) SUMA_ShowMxVec(vt, 1, NULL, "\nvt matrix\n");
         vt2 = SUMA_MxVecInverse(vt, NULL);
         vt = SUMA_FreeMxVec(vt);
         if (Opt->debug > 1) 
            SUMA_ShowMxVec(vt2, 1, NULL, "\nvt2 matrix\n");
         Ycommon = SUMA_MxVecMult(vt2,y_l, NULL, 0);
         if (Opt->debug > 1) 
            SUMA_ShowMxVec(Ycommon, 1, NULL, "\nYcommon matrix\n");
         vt2 = SUMA_FreeMxVec(vt2);
      } else {/* higher order, need to deal with real and imaginary parts*/
         /* Catenate the columns of y_l with the real columns first 
            (negative harmonics), 
            followed by imaginary ones (positive harmonics)*/ 
         if (Opt->debug > 1)  {
            SUMA_ShowMxVec(y_l, 1, NULL, "\noriginal y_l matrix\n");
            if (Opt->debug > 2) {
               SUMA_WriteMxVec(y_l, 
                              "y_l_o.1D.dset", "#original y_l matrix\n");
            }
         }
         sprintf(stmp, "Starting with order l=%d", l);
         if (Opt->debug > 2) SUMA_etime2(FuncName, stmp, FuncName);
         y_l = SUMA_YLcomp_to_YLdoub( &y_l, Opt->debug);  
         /* Now y_l is all real */
         if (Opt->debug > 2) SUMA_etime2(FuncName, "Created y_l", FuncName);
         if (Opt->debug > 2) {
            SUMA_WriteMxVec(y_l, "y_l.1D.dset", "#y_l real matrix\n");
         }
         y_l_t = SUMA_MxVecTranspose(y_l, NULL);   
            /*  y_l is equal to Y' in Moo's SPHARsmooth.m function*/
         if (Opt->debug > 1)  
            SUMA_ShowMxVec(y_l_t, 1, NULL, "\ny_l_t matrix\n");
         if (Opt->debug > 2) 
            SUMA_etime2(FuncName, "Trasnposed y_l", FuncName);
         #if 0
         vt = SUMA_MxVecMult(y_l,y_l_t, NULL, 0);
         #else
         vt = SUMA_MxVecMult(y_l, y_l_t, NULL, MATRIX_B_IS_AT);   
         #endif
         if (Opt->debug > 1) SUMA_ShowMxVec(vt, 1, NULL, "\nvt matrix\n");
         if (Opt->debug > 1) {
            sprintf(stmp,"Multiplied yl (%dx%d) by ylt(%dx%d)", 
                        y_l->dims[0], y_l->dims[1],  
                        y_l_t->dims[0], y_l_t->dims[1]);  
            SUMA_etime2(FuncName, stmp, FuncName);
         }
         vt2 = SUMA_MxVecInverse(vt, NULL);
         vt = SUMA_FreeMxVec(vt);
         if (Opt->debug > 2) SUMA_etime2(FuncName, "Inverted vt", FuncName);
         if (Opt->debug > 1) 
            SUMA_ShowMxVec(vt2, 1, NULL, "\nvt2 matrix\n");
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
               SUMA_WriteMxVec(Ycommon, 
                              "Ycommon.1D.dset", "#Ycommon matrix\n");
            }
         }
         vt2 = SUMA_FreeMxVec(vt2);

         fac = exp((double)(-l*(l+1))*Opt->v0);
      }

      iso = 0;
      SO = NULL; /* safety measure, no SO to be used in this block */

      icol = 0;
      while (icol < SDSET_VECNUM(data_dset)) { /* load up */
         if (Opt->debug) { 
            SUMA_S_Notev("Processing column %d/%d\n", 
                           icol+1, SDSET_VECNUM(data_dset));
         }
         if (icol == 0) {
            dbuf = SUMA_DsetCol2DoubleFullSortedColumn (
               data_dset, icol, &nmask, 0.0, SDSET_VECLEN(out_dset),
               &N_nmask, YUP);
         } else {
            dbuf = SUMA_DsetCol2DoubleFullSortedColumn (
               data_dset, icol, NULL, 0.0, SDSET_VECLEN(out_dset),
               &N_nmask, NOPE);
         }
         dims[0] = SDSET_VECLEN(data_dset); dims[1] = 1;

         if (x) {
            SUMA_S_Err( "x should be null here!\n" 
                        "Might have forgotten to free it.\n");
            exit(1);
         } else {
            if (!(x = SUMA_VecToMxVec(SUMA_double, 2, dims, 
                                       1, (void *)dbuf))) {
               SUMA_S_Err("Failed to allocate for x");
               exit(1);
            }
         }
         dbuf = NULL;   /* hide it, should free x later */
         if (!xe || !betax) { /* these are reusable, allocate 1st time only */                dims[0] = SDSET_VECLEN(data_dset); dims[1] = 1; 
            xe = (SUMA_MX_VEC **)SUMA_calloc(SDSET_VECNUM(data_dset),
                                                sizeof(SUMA_MX_VEC*));
            for (i=0; i<SDSET_VECNUM(data_dset); ++i) {
               xe[i] = SUMA_NewMxVec(SUMA_double, 2, dims, 1);
               if (!xe[i]) {
                  SUMA_S_Err("Failed to allocate");
                  exit(1);
               }
            }
            dims[0] = Opt->iopt+1; dims[1] = 2*Opt->iopt+1; 
                  /* (allow for 2d, easy to extend to 3d) */
            betax = (SUMA_MX_VEC **)SUMA_calloc(SDSET_VECNUM(data_dset),
                                                sizeof(SUMA_MX_VEC*));
            for (i=0; i<SDSET_VECNUM(data_dset); ++i) {
               betax[i] = SUMA_NewMxVec(SUMA_double, 2, dims, 1);
               if (!betax[i]) {
                  SUMA_S_Err("Failed to allocate betax[i]");
                  exit(1);
               }
            }
         }

         if (Opt->debug > 1) SUMA_ShowMxVec(x, 1, NULL, "\nx vector\n");

         if (LocalHead || Opt->debug)  {
               fprintf(SUMA_STDERR,"%s: Doing l = %d, col %d\n", 
                                    FuncName, l, icol);
               SUMA_etime2(FuncName, "Entering loop", FuncName);
         }

         if (l==0) {   
            betal = SUMA_MxVecMult(Ycommon, x, NULL, 0);
            if (Opt->debug > 1) 
               SUMA_ShowMxVec(betal, 1, NULL, "\nbetalx matrix\n");
            mxvd2(betax[icol],0,0) =  mxvd2(betal, 0, 0);
            if (Opt->debug > 1) 
               SUMA_ShowMxVec(betax[icol], 1, NULL, "\nbetax matrix\n");
            xe[icol] = SUMA_MxVecMult(y_l_t, betal, NULL, 0);
            betal = SUMA_FreeMxVec(betal);
            if (Opt->debug > 1) {
               SUMA_ShowMxVec(xe[icol], 1, NULL, "\nxe vector\n");
               if (Opt->debug > 2) 
                  SUMA_WriteMxVec(xe[icol], "xe_l0.1D", "#xe vector");
            }
         } else { 
            /* Steps 4 and 5 here, refitting the residual */
            SUMA_SPHARM_SMOOTH_EST(x, xe[icol], betax[icol], "x");
            if (Opt->debug) SUMA_etime2(FuncName, "Refit residual", FuncName);
         }
         
         if (Opt->debug) { SUMA_S_Notev("Storing col %d\n", icol); }
         /* store new data in output dset (A copy of the first) */
         if (!SUMA_Vec2DsetCol(  out_dset, icol, xe[icol]->v, 
                                 SUMA_double, 1, nmask)) {
            SUMA_S_Err("Failed to update output.");
            exit(1);
         }
         
         if (do_surf_xyz) {
            if (Opt->debug) {
               SUMA_S_Notev("Adding column to xyz[:,%d] for surface %s\n", 
                              icol%3, SOvn[icol/3]->Label);
            }
            /* special case of a surface's XYZ decomposition,
            store coords for eye candy */
            SOn = SOvn[icol/3];
            for (i=0; i<SOn->N_Node; ++i) {
               i3 = 3*i;
               SOn->NodeList[i3+icol%3] = mxvd1(xe[icol], i);
            }
         }  
         /* free x */
         x = SUMA_FreeMxVec(x);
         ++icol;
      } /* for each of the columns */  

      if (ps->cs->Send) { /* send the smoothed coords  */
         if ( do_surf_xyz) { /* doing surface coordinates */
            if (l > 0) {
               if (!SUMA_SendToSuma (  SOt, ps->cs, 
                                       (void *)SOn->NodeList, 
                                       SUMA_NODE_XYZ, 1)) {
                  SUMA_SL_Warn(  "Failed in SUMA_SendToSuma\n"
                                 "Communication halted.");
               }
            }
         } else {
            if (l > 0) {
               if (!SUMA_SendToSuma (  SOt, ps->cs, 
                                       (void *)out_dset->dnel->vec[0], 
                                       SUMA_NODE_RGBAb, 1)) {
                  SUMA_SL_Warn(  "Failed in SUMA_SendToSuma\n"
                                 "Communication halted.");
               }
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

      NEXT_L:
      ++l; 
   } while (l <= Opt->iopt);
   --l; 
   
   /* write out the coefficients */
   if (Opt->out_prefix) {
      char *fname = NULL;
      for (icol=0; icol<SDSET_VECNUM(out_dset); ++icol) {
         sprintf(stmp, ".beta.col%03d.1D", icol);
         fname = SUMA_append_string(Opt->out_prefix,stmp);
         sprintf(stmp, "#beta coef for spharm orders 0 to %d", l);
         SUMA_WriteMxVec(betax[icol], fname, stmp);
         SUMA_free(fname);
      }
   }
   
   /* done with coordinate containers */
   if (x) x = SUMA_FreeMxVec(x);
   if (xe) {
      for (icol=0; icol<SDSET_VECNUM(out_dset); ++icol) 
         xe[icol] = SUMA_FreeMxVec(xe[icol]);
      SUMA_free(xe); xe = NULL;
   }
   /* done with Fourier Coefficients */
   if (betax) {
      for (icol=0; icol<SDSET_VECNUM(out_dset); ++icol) 
         betax[icol] = SUMA_FreeMxVec(betax[icol]);
      SUMA_free(betax); betax = NULL;
   }
   if (dif) dif = SUMA_FreeMxVec(dif);
   if (sm) sm = SUMA_FreeMxVec(sm);
   
   SUMA_Spherical_Bases(&l, NULL);   /* clean SUMA_Spherical_Bases */

   /* you don't want to exit rapidly because 
      the SUMA might not be done processing the last elements*/
   if (ps->cs->Send && !ps->cs->GoneBad) {
      /* cleanup and close connections */
      if (!SUMA_SendToSuma (SOv[0], ps->cs, NULL, SUMA_NODE_XYZ, 2)) {
         SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCleanup failed");
      }
   }   
   if (SOu) SUMA_Free_Surface_Object(SOu); SOu = NULL;
   if (do_surf_xyz && ps->o_N_surfnames) {
      for (i=0; i<n_subject; ++i) { 
         if (SOvn[i]) {
            if (ps->o_N_surfnames == n_subject) {
               pref = SUMA_copy_string(ps->o_surfnames[i]);
               tp = ps->o_FT[i];
               form = ps->o_FF[i];
            } else {
               sprintf(stmp, "s%02d", i);
               pref = SUMA_append_string(ps->o_surfnames[0], stmp);
               tp = ps->o_FT[0];
               form = ps->o_FF[0];
            }
         }
         SO_name = SUMA_Prefix2SurfaceName(pref, NULL, NULL, tp, &exists);
         if (!THD_ok_overwrite() && exists) {
            fprintf(SUMA_STDERR,"Warning %s:\nOutput surface %s* on disk.\n"
                                "Will not overwrite.\n", FuncName, pref);
            exit(1);
         }
         if (Opt->debug) {
            SUMA_S_Notev("Saving surface under prefix %s\n", pref);
         }
         if (!SUMA_Save_Surface_Object (SO_name, SOvn[i], tp, form, NULL)) {
            SUMA_S_Err("Failed to write reconstructed surface!");
            exit(1);   
         }
         SUMA_Free_Surface_Object(SOv[i]); SOv[i] = NULL; 
         SUMA_Free_Surface_Object(SOvn[i]); SOvn[i] = NULL; 
         SUMA_free(pref); pref = NULL;
      }
   }
   
   /* save output if needed */
   if (Opt->debug) {
      SUMA_S_Note("Writing output dset");
   }
   if (Opt->out_prefix) {
      ooo = SUMA_WriteDset_s( Opt->out_prefix, out_dset, 
                              oform, THD_ok_overwrite(), 0);
      SUMA_free(ooo); ooo=NULL;
   }
   
   /* free dataset structures */
   if (data_dset) { SUMA_FreeDset(data_dset); data_dset = NULL; }
   if (out_dset) { SUMA_FreeDset(out_dset); out_dset = NULL; } 
   
   if (N_Spec) {
      int k=0; 
      for (k=0; k<N_Spec; ++k) {
         if (!SUMA_FreeSpecFields(&(Spec[k]))) { 
            SUMA_S_Err("Failed to free spec fields"); } 
      }
      SUMA_free(Spec); Spec = NULL; N_Spec = 0;
   }
   if (Opt) Opt = SUMA_Free_Generic_Prog_Options_Struct(Opt);
   if (!SUMA_Free_CommonFields(SUMAg_CF)) 
      SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   
   exit(0);
   
} 
