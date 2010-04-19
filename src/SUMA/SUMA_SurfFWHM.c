#include "SUMA_suma.h"

SUMA_SurfaceViewer *SUMAg_cSV = NULL; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
SUMA_DO *SUMAg_DOv = NULL;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF = NULL; /*!< Global pointer to structure containing info common to all viewers */

static char uSFWHM_Example1[]={
   "1- Estimating the FWHM of smoothed noise:\n"   
   "     echo Create a simple surface, a sphere and feed it to SUMA.\n"
   "\n"
   "     suma -niml &\n"
   "     set Niso = `CreateIcosahedron -rad 100 -ld 80 -nums_quiet`; \\\n"
   "           set Niso = $Niso[1]\n"
   "     CreateIcosahedron -tosphere   -rad 100 -ld 80 \\\n"
   "                       -prefix sphere_iso_$Niso\n"
   "     DriveSuma  -com show_surf -label sphere_iso_$Niso \\\n"
   "                -i_fs sphere_iso_${Niso}.asc\n"
   "\n"
   "     echo Create some noise on the sphere.\n"
   "     1deval -num $Niso -del 1 \\\n"
   "            -expr 'gran(0,1)*10000' > ${Niso}_rand.1D.dset\n"
   "     DriveSuma  -com surf_cont -label sphere_iso_$Niso \\\n"
   "                -load_dset ${Niso}_rand.1D.dset\\\n"
   "                -switch_dset ${Niso}_rand.1D.dset -T_sb -1\n"
   "\n"
   "     echo What is the global FWHM of the noise? -a sanity check-\n"
   "     set randFWHM = `SurfFWHM -i_fs sphere_iso_${Niso}.asc \\\n"
   "                              -input ${Niso}_rand.1D.dset` ; \\\n"
   "                              echo $randFWHM \n"
   "\n"
   "     echo Now smooth the noise\n"
   "     set opref_rand = ${Niso}_rand_sm10 && rm -f ${opref_rand}.1D.dset \n"
   "     SurfSmooth -spec sphere_iso_$Niso.spec -surf_A sphere_iso_$Niso \\\n"
   "                -met HEAT_07  \\\n"
   "                -input ${Niso}_rand.1D.dset -fwhm 10 \\\n"
   "                -output ${opref_rand}.1D.dset\n" 
   "     DriveSuma  -com surf_cont -label sphere_iso_$Niso \\\n"
   "                -load_dset ${opref_rand}.1D.dset \\\n"
   "                -switch_dset ${opref_rand}.1D.dset -T_sb -1\n"
   "\n"
   "     echo Let us find the FWHM both globally and locally\n"
   "     echo Note:"
   "     echo Because the surface where the data are defined is itself\n"
   "     echo a sphere, we need not specify it spherical version.\n"
   "     echo If this were not the case, we would need to specify\n"
   "     echo the spherical surface in the SurfFWHM command. This would be\n"
   "     echo via an additional -i_fs spherical_version.asc . \n"
   "     set fwhmpref = FWHM_${opref_rand} && rm -f ${fwhmpref}.1D.dset\n"
   "     set gFWHM = `SurfFWHM  -i_fs sphere_iso_${Niso}.asc \\\n"
   "                            -input ${opref_rand}.1D.dset \\\n"
   "                            -hood -1 -prefix ${fwhmpref}` \n"
   "     echo The global FWHM is $gFWHM\n"
   "     echo The local FWHM are sent to SUMA next:"
   "     DriveSuma   -com surf_cont -label sphere_iso_$Niso \\\n"
   "                 -load_dset ${fwhmpref}.1D.dset \\\n"
   "                 -switch_dset ${fwhmpref}.1D.dset -T_sb -1\n"
   "\n"
   "     echo Produce a histogram showing the distribution of local FWHM.\n"
   "     3dhistog ${fwhmpref}.1D.dset > ${fwhmpref}_histog.1D\n"
   "     set mFWHM = `3dBrickStat -slow -mean ${fwhmpref}.1D.dset`\n"
   "     1dplot -ylabel 'number of nodes' \\\n"
   "            -x ${fwhmpref}_histog.1D'[0]' -xlabel 'Local FWHM'\\\n"
   "            -plabel \"(Mean,Global) =($mFWHM, $gFWHM)\" \\\n"
   "            ${fwhmpref}_histog.1D'[1]' & \n"
   "\n"
   "     echo Notice that these tests are for sanity checks. The smoothing \n"
   "     echo operation relies itself on smoothness estimates. You could  \n"
   "     echo change the example to add a preset number of smoothing   \n"
   "     echo iterations with a kernel width of your choosing.\n"
   "\n"
   
}; 

void examples_SurfFWHM ()
{
   printf ( "\n"
            "Examples for SurfFWHM\n"
            "\n"
            "%s\n"
            "\n", uSFWHM_Example1);
   return;
}   
void usage_SurfFWHM (SUMA_GENERIC_ARGV_PARSE *ps)
{
      static char FuncName[]={"usage_SurfFWHM"};
      char * s = NULL, *sio=NULL, *st = NULL, *sts = NULL;
      int i;
      s = SUMA_help_basics();
      sio  = SUMA_help_IO_Args(ps);
      printf ( 
"\n"
"Usage: A program for calculating local and global FWHM.\n"
"------\n"
" -input DSET = DSET is the dataset for which the FWHM is \n"
"               to be calculated. \n"
" (-SURF_1): An option for specifying the surface over which\n"
"            DSET is defined. (For option's syntax, see \n"
"            'Specifying input surfaces' section below).\n"
" (-MASK)  : An option to specify a node mask so that only\n"
"            nodes in the mask are used to obtain estimates.\n"
"            See section 'SUMA mask options' for details on\n"
"            the masking options.\n"
" Clean output:\n"
" -------------\n"
"  The results are written to stdout and the warnings or\n"
"  notices to stderr. You can capture the output to a file\n"
"  with the output redirection '>'. The output can be \n"
"  further simplified for ease of parsing with -clean.\n"
"  -clean: Strip text from output to simplify parsing.\n"
"\n" 
" For Datasets With Multiple Sub-Bricks (a time axis):\n"
" ----------------------------------------------------\n"
"  For FWHM estimates, one is typically not interested\n"
"    in intrinsic spatial structure of the data but in \n"
"    the smoothness of the noise. Usually, the residuals\n"
"    from linear regression are used for estimating FWHM.\n"
"  A lesser alternate would be to use a detrended version\n"
"    of the FMRI time series. \n"
"  N.B.: Do not use catenated time series. Process one\n"
"        continuous run at a time.\n"
"\n"
"  See note under 'INPUT FILE RECOMMENDATIONS' in 3dFWHMx -help : \n"
"\n"
/*    "  -dmed      = If the input dataset has more than one sub-brick\n"
"                (e.g., has a time axis), then subtract the median\n"
"                of each voxel's time series before processing FWHM.\n"
"                This will tend to remove intrinsic spatial structure\n"
"                and leave behind the noise.\n"
"                [Default = don't do this]\n"
"  -unif       = If the input dataset has more than one sub-brick,\n"
"                then normalize each voxel's time series to have\n"
"                the same MAD before processing FWHM.  Implies -dmed.\n"
"                [Default = don't do this]\n" */
"  -detrend [q]= Detrend to order 'q'.  If q is not given, \n"
"                the program picks q=NT/30.\n"
"        **N.B.: This is the same detrending as done in 3dDespike;\n"
"                using 2*q+3 basis functions for q > 0.\n"
"     or \n"
"  -detpoly p = Detrend with polynomials of order p.\n"
/*    "                -detrend disables -dmed, and includes -unif.\n"
"        **N.B.: I recommend this option, and it is not the default\n"
"                only for historical compatibility reasons.  It may\n"
"                become the default someday. Depending on my mood.\n"
"                It is already the default in program 3dBlurToFWHM.\n" */
"  -detprefix d= Save the detrended file into a dataset with prefix 'd'.\n"
"                Used mostly to figure out what the hell is going on,\n"
"                when funky results transpire.\n"
"\n"
" Options for Local FWHM estimates:\n"
" ---------------------------------\n"
" (-SURF_SPHERE): The spherical version of SURF_1. This is \n"
"                 necessary for Local FWHM estimates as the\n"
"                 neighborhoods are rapidly estimated via the\n"
"                 spherical surface.\n"
"                 (-SURF_SPHERE) is the second surface specified\n"
"                 on the command line. The syntax for specifying\n"
"                 it is the same as for -SURF_1.\n"
"                 If -SURF_1 happens to be a sphere, then there\n"
"                 is no need to specify -SURF_SPHERE\n"
" -hood R     = Using this option indicates that you want local\n"
" -nbhd_rad R = as well as global measures of FWHM. Local measurements\n"
"               at node n are obtained using a neighborhood that \n"
"               consists of nodes within R distance from n \n"
"               as measured by an approximation of the shortest \n"
"               distance along the mesh.\n"
"               The choice of R is important. R should be at least\n"
"               twice as large as the FWHM. Otherwise you will be\n"
"               underestimating the Local FWHM at most of the nodes.\n"
"               The more FWHM/R exceeds 0.5, the more you will under-\n"
"               estimate FWHM. Going for an excessive R however is not\n"
"               very advantagious either. Large R is computationaly \n"
"               expensive and if it is much larger than FWHM estimates,\n" 
"               it will lead to a blurring of the local FWHM estimates.\n"
"               Set R to -1 to allow the program\n"
"               to set it automatically.\n"
" -prefix PREFIX = Prefix of output data set. \n"
" -vox_size D = Specify the nominal voxel size in mm. This helps\n"
"               in the selection of neighborhood size for local smoothness\n"
"               estimation.\n"
"\n"
" -ok_warn\n"
" -examples  = Show command line examples and quit.\n"
" Options for no one to use:\n"
" -slice : Use the contours from planar intersections to estimated\n"
"          gradients. This is for testing and development purposes\n"
"          only. Leave it alone.\n"
" \n"
" The program is rather slow when estimating Local FWHM. The speed gets\n"
" worse with larger hoods. But I can do little to speed it up without\n"
" making serious shortcuts on the estimates. It is possible however to make\n"
" it faster when estimating the FWHM over multiple sub-bricks. If you find \n"
" yourself doing this often, let me know. I hestitate to implement the faster \n"
" method now because it is more complicated to program.\n"
"\n"  
" Examples:\n"
"%s"
"%s"
"%s"
               "\n", uSFWHM_Example1, sio,  s);
      SUMA_free(s); s = NULL; SUMA_free(st); st = NULL; SUMA_free(sio); sio = NULL;       
      s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
      printf("       Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov     \n");
      exit(0);
}
static int ncode=-1 , code[MAX_NCODE];

SUMA_GENERIC_PROG_OPTIONS_STRUCT *SUMA_SurfFWHM_ParseInput(char *argv[], int argc, SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_SurfFWHM_ParseInput"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt=NULL;
   int kar;
   SUMA_Boolean brk;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
      
   Opt = SUMA_Alloc_Generic_Prog_Options_Struct();
   Opt->ps = ps; /* for convenience */
   Opt->NodeDbg = -1;
   Opt->out_prefix = NULL;
   Opt->r = -999.0;
   Opt->iopt = 0;
   Opt->d1 = -1.0;
   Opt->b1 = 0;
   Opt->b2 = 0;
   Opt->unif = Opt->dmed = 0;
   Opt->geom = -1;
   Opt->poly = -1;
   Opt->corder = -2; /* -2 do nothing, -1 autodetrend, 0 mean, 1 linear trend, etc ..*/
   Opt->Use_emask = 0;
   ncode = 0;
   kar = 1;
   brk = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_SurfFWHM(ps);
          exit (0);
		}
		
		SUMA_SKIP_COMMON_OPTIONS(brk, kar);
      
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
      
      if (!brk && (strcmp(argv[kar], "-node_debug") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a node index after -node_debug \n");
            exit (1);
         }
         
         Opt->NodeDbg = atoi(argv[++kar]);
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-slice") == 0))
      {
         Opt->Use_emask = 1;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-clean") == 0))
      {
         Opt->iopt = 1;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-fix_NN") == 0))
      {
         Opt->b2 = 1;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-prefix") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a dset prefix after -prefix \n");
            exit (1);
         }
         
         Opt->out_prefix = SUMA_copy_string(argv[++kar]);
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-hood") == 0 || strcmp(argv[kar], "-nbhd_rad") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a value after -nbhd_rad \n");
            exit (1);
         }
         
         Opt->r = atof(argv[++kar]);
         if (Opt->r <= 0.0 && Opt->r != -1.0f) {
            fprintf (SUMA_STDERR,"Error %s:\nneighborhood radius is not valid (have %f from %s).\n", FuncName, Opt->r, argv[kar]);
		      exit (1);
         }
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-vox_size") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a value after -vox_size \n");
            exit (1);
         }
         
         Opt->d1 = atof(argv[++kar]);
         if (Opt->d1 <= 0.0) {
            fprintf (SUMA_STDERR,"Error %s:\nvoxel dimension is not valid (have %f from %s).\n", FuncName, Opt->d1, argv[kar]);
		      exit (1);
         }
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-ok_warn") == 0))
      {
         Opt->b1 = 1;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-examples") == 0))
      {
         examples_SurfFWHM();
         exit(0);
      }
      
      if (!brk && (strcmp(argv[kar],"-detrend") == 0 )){         
         Opt->corder = -1 ;
         if( kar < argc-1 && isdigit(argv[kar+1][0]) ){
            Opt->corder = (int)strtod(argv[++kar],NULL) ;
            if( Opt->corder == 0 ){ /* Use poly of order 0 (mean) */
              Opt->poly = 0 ; fprintf(SUMA_STDOUT,"-detrend 0 replaced by -detpoly 0") ;
              Opt->corder = -2;
            }
         }
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar],"-detpoly") == 0 )){         
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a value after -detpoly \n");
            exit (1);
         }
         Opt->poly = (int)strtod(argv[++kar],NULL) ;
         brk = YUP;
      }
      
      if( strncmp(argv[kar],"-geom",4) == 0 ){          
         Opt->geom = 1 ; brk = YUP;
      }
      if( strncmp(argv[kar],"-arith",5) == 0 ){        
         Opt->geom = 0 ; brk = YUP;
      }
      if( strncmp(argv[kar],"-dmed",5) == 0 ){         
         SUMA_S_Errv("Option %s not supported.\nUse -detrend instead.\n", argv[kar]);
         exit(1);
         Opt->dmed = 1 ; brk = YUP ;
      }
      if( strncmp(argv[kar],"-unif",5) == 0 ){          
         SUMA_S_Errv("Option %s not supported.\nUse -detrend instead.\n", argv[kar]);
         exit(1);
         Opt->unif = Opt->dmed = 1 ; brk = YUP ;
      }
      if( strcmp(argv[kar],"-detprefix") == 0 ){
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a value after -detprefix \n");
            exit (1);
         }
         Opt->out_vol_prefix = SUMA_copy_string(argv[++kar]);
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

   if (!Opt->out_prefix) {
      Opt->out_prefix = SUMA_copy_string("SurfLocalstat");
   }

   if (Opt->r > 0.0 && Opt->d1 > 0.0) {
      if (Opt->r / Opt->d1 < 2.99) {   /* no magic reason for 3 other than it results in approx. pi*(3*d)2 mm2 area, which would be approx. pi*3*3 voxels. */
         SUMA_S_Warnv(  "\n"
                        "**********************************************\n"
                        "The neighborhood radius of %.3fmm is likely too\n"
                        " small, relative to the voxel size of %.3fmm, \n"
                        " to yield an appropriate estimate for FWHM. \n"
                        " A radius of at least %.3f would be more \n"
                        " appropriate. Use -ok_warn to proceed despite\n"
                        " warning.\n"
                        " ZSS. DC CC and its vanilla suburbs.\n"
                        "***********************************************\n"
                        "\n", 
                        Opt->r, Opt->d1, Opt->d1*3.0 ); 
         if (!Opt->b1) exit(1);
      }
   }
   
   SUMA_RETURN(Opt);
}

/* See labbook NIH-4, pp 104 ...> */
int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"SurfFWHM"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt;  
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   SUMA_SurfaceObject *SO=NULL, *SOf=NULL;
   int *icols=NULL, N_icols = -1;
   SUMA_DSET_FORMAT iform = SUMA_NO_DSET_FORMAT;
   SUMA_DSET *din=NULL, *dout=NULL;
   SUMA_SurfSpecFile *Spec = NULL;
   int i, N_Spec, N_inmask = -1;
   float *fwhmv=NULL, fwhmg_max;
   double MinArea = -1.0;
   char *ooo=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_STANDALONE_INIT;
	SUMA_mainENTRY;

   /* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "-i;-t;-spec;-m;-dset;-talk;");
   
   if (argc < 2) {
      usage_SurfFWHM(ps);
      exit (1);
   }
   
   Opt = SUMA_SurfFWHM_ParseInput (argv, argc, ps);

   if (Opt->debug > 2) LocalHead = YUP;
   if (Opt->ps->N_dsetname != 1) {
      SUMA_S_Errv("Need one and only one dset please. Have %d on command line.\n", Opt->ps->N_dsetname);
      exit(1);
   }
   if (!(din = SUMA_LoadDset_s (Opt->ps->dsetname[0], &iform, 0))) {
      SUMA_S_Errv("Failed to load dset named %s\n", Opt->ps->dsetname[0]);
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

   if (Opt->Use_emask) SUMA_Set_UseSliceFWHM(1);
   else SUMA_Set_UseSliceFWHM(0);
   
   SUMA_LH("Loading surface...");
   SO = SUMA_Load_Spec_Surf(Spec, 0, ps->sv[0], Opt->debug);
   if (!SO) {
         fprintf (SUMA_STDERR,"Error %s:\n"
                              "Failed to find surface\n"
                              "in spec file. \n",
                              FuncName );
         exit(1);
      
   }   
   if (Spec->N_Surfs == 2) { 
      SOf = SUMA_Load_Spec_Surf(Spec, 1, ps->sv[0], Opt->debug); 
      if (!SOf) {
         fprintf (SUMA_STDERR,"Error %s:\n"
                              "Failed to find surface\n"
                              "in spec file. \n",
                              FuncName );
         exit(1);
      }   
   } else { SOf = NULL; }
   
   if (!(Opt->nmask = SUMA_load_all_command_masks(Opt->ps->bmaskname, Opt->ps->nmaskname, Opt->ps->cmask, SO->N_Node, &N_inmask)) && N_inmask < 0) {
         SUMA_S_Err("Failed loading mask");
         exit(1);
   }
   
   if( (Opt->dmed || Opt->unif || Opt->corder > -2) && SDSET_VECNUM(din) < 4 ){
     SUMA_S_Warnv(
       "-dmed and/or -corder and/or -unif ignored: only %d input sub-bricks\n",
       SDSET_VECNUM(din) ) ;
     Opt->dmed = Opt->unif = 0 ;Opt->corder = -2;
   }

   if( Opt->dmed && Opt->corder > -2){
     Opt->dmed = 0 ; SUMA_S_Warn("-dmed is overriden by -corder") ;
   }

   if( Opt->corder == -1 ) Opt->corder = SDSET_VECNUM(din) / 30 ;
   if( Opt->corder >= 0 && 2*Opt->corder+3 >= SDSET_VECNUM(din) ){
      SUMA_S_Errv("-corder %d is too big for this dataset",Opt->corder) ;
   }
   if (Opt->corder > 0 && Opt->poly >= 0) {
      SUMA_S_Errv("Cannot specify both -detrend and -detpoly\n Have %d and %d, respectively.\n", 
                  Opt->corder, Opt->poly);
      exit(1);
   }
   if (Opt->r == -1.0f || Opt->r > 0.0) {
      if (!SOf && !SO->isSphere) {
         SUMA_S_Err("Need a spherical surface to accompany the non-spherical surface on input.\n");
         exit(1);
      }
   }

   /*-- if detrending, do that now --*/

   if( Opt->corder > 0 || Opt->poly >= 0){
      int nref=-1 , jj,iv,kk ;
      float **ref , tm,fac,fq ;
      THD_3dim_dataset *newset=NULL, *inset=NULL ;

      /* need to take dataset to AFNI dset */
      if (!(inset = SUMA_sumadset2afnidset(&din, 1, 1))) {
         SUMA_S_Err("Failed to transform surface dset to afni dset");
         exit(1);
      }

      if (Opt->corder > 0) {
         nref = 2*Opt->corder+3;
         if (Opt->debug) { 
            SUMA_S_Notev(  "trig. detrending start: "
                           "%d baseline funcs, %d time points\n"
                           ,nref, DSET_NVALS(inset)) ; 
         }
         ref = THD_build_trigref( Opt->corder , DSET_NVALS(inset) ) ;
         if( ref == NULL ) ERROR_exit("THD_build_trigref failed!") ;
      } else {
         nref = Opt->poly+1;
         if (Opt->debug) {
            SUMA_S_Notev(  "poly. detrending start: "
                           "%d baseline funcs, %d time points\n"
                           ,nref, DSET_NVALS(inset)) ;
         }
         ref = THD_build_polyref( nref , DSET_NVALS(inset) ) ;
         if( ref == NULL ) ERROR_exit("THD_build_trigref failed!") ;

      }
      
      if (!(newset = THD_detrend_dataset( inset , nref , 
                                          ref , 2 , 1 , Opt->nmask , NULL ))) { 
         SUMA_S_Err("detrending failed!") ;
         exit(1);
      }
      
      SUMA_LH("detrending done") ;

      for(jj=0;jj<nref;jj++) free(ref[jj]) ;
      free(ref); DSET_delete(inset); inset=newset; newset = NULL;
      Opt->dmed = Opt->unif = 0 ;
      
      SUMA_LH("detrending cleanup done") ;

      /* Now back to SUMA_DSET,  don't need afni volume anymore*/
      din = SUMA_afnidset2sumadset(&inset, 1, 1); 

   }
   if( Opt->out_vol_prefix != NULL ){     /** for debugging,  keep it outside**/
      char *ooo=NULL;                     /**  of detrending condition **/
      if (Opt->debug) {
         SUMA_S_Notev("Writing detrended volume to %s\n",
                        Opt->out_vol_prefix);
      }
      ooo = SUMA_WriteDset_s(Opt->out_vol_prefix, din, SUMA_ASCII_NIML, 1, 0);
      SUMA_free(ooo); ooo=NULL;
   }

   /* check for and fix nearest neighbor sampling which results in identical
   values on neighboring nodes */
   if (Opt->b2) {
      if (Opt->debug) {
         SUMA_S_Note(  "Fixing NN resampling problem "
                        "(should pass perhaps a data column "
                        "that is surely floaty...");
      }
      if (!SUMA_FixNN_Oversampling(SO, din, Opt->nmask, 0, YUP)) {
         SUMA_S_Err("Failed in SUMA_FixNN_Oversampling");
         exit(1);
      }
   }

   if (Opt->debug) {
      SUMA_S_Note("Doing Global FWHM...");
   }
   /* what columns can we process ?*/
   icols = SUMA_FindNumericDataDsetCols(din, &N_icols);
         
   if (N_icols <= 0) {
      SUMA_SL_Err("No approriate data columns in dset");
      exit(1);   
   }
   
   /* SUMA_SetDbgFWHM(1); */
   if (LocalHead && Opt->nmask) {
      SUMA_S_Note("Have mask, will travel");
      SUMA_WRITE_ARRAY_1D(Opt->nmask,SO->N_Node,1,"mask.1D");
   }
   if (!(fwhmv = SUMA_estimate_dset_FWHM_1dif(  SO, din, 
                                          icols, N_icols, Opt->nmask, 
                                          1, NULL))) {
      SUMA_S_Err("Rien ne va plus");
      exit(1);                                         
   }
  
   fprintf(stderr,"Global FWHM estimates for each column:\n");
   fwhmg_max = fwhmv[0];
   for (i=0; i<N_icols; ++i) {
      if (!Opt->iopt) fprintf(stdout,"FWHM[ %4d ]= %.4f\n", icols[i], fwhmv[i]);
      else fprintf(stdout,"%.4f\n",  fwhmv[i]);
      if (fwhmv[i] > fwhmg_max) fwhmg_max = fwhmv[i];
   }
   if (N_icols > 0) {
      int N=0;
      float fwhmg;
      if (Opt->geom < 0 || Opt->geom == 0) {
         SUMA_FWHM_MEAN(fwhmv, N_icols, fwhmg, "arit", N);
         if (!Opt->iopt) fprintf(stdout,"Arithmetic Mean (non-zero fwhm only)= %.4f\n", fwhmg);
         else fprintf(stdout,"#Arit_mean   %.4f\n", fwhmg);
      }
      if (Opt->geom < 0 || Opt->geom == 1) {
         SUMA_FWHM_MEAN(fwhmv, N_icols, fwhmg, "geom", N);
         if (!Opt->iopt) fprintf(stdout,"Geometric  Mean (non-zero fwhm only)= %.4f\n", fwhmg);
         else fprintf(stdout,"#Geo_mean   %.4f\n", fwhmg);
      }
   }
   if (Opt->r == -1.0f) {
      /* let's come up with a decent number */
      /* first go with about 2.5 times the largest FWHM, 
      and not smaller than 5.5*edge length and
      not smaller than about 3 voxels */
      Opt->r = SUMA_MAX_PAIR(2.5 * fwhmg_max, 5.5*SO->EL->AvgLe);
      /* Now make sure this covers more than about three voxels */
      if (Opt->d1 > 0.0) {
         SUMA_LHv("Have Opt->r %f and Opt->d1 %f. Making sure hood >= 3*voxels (%f)\n", Opt->r, Opt->d1, Opt->d1*3.0);
         if (Opt->r < Opt->d1*3.0) Opt->r = Opt->d1*3.0;
      }
      SUMA_S_Notev("Neighborhood radius set to %f\n", Opt->r);
   }
   
   if (Opt->r > 0.0) { /* wants to do localized FWHM */
      if (Opt->debug) {
         SUMA_S_Note("Doing local FWHM...");
      }
      if (!SOf) {
         if (!SO->isSphere) {
            SUMA_S_Err( "Need a spherical surface to accompany the "
                        "non-spherical surface on input.\n");
            exit(1);
         } else {
            /* Keep SOf NULL, that is acceptable, 'cause SO is spherical */ 
         }
      } else {
         if (!SOf->isSphere) {
            SUMA_S_Err( "The spherical surface does not have the spherical flag"
                        " set.\n");
            exit(1);
         }
      }
      code[0] = NSTAT_FWHMx;
      ncode = 1;
      if (Opt->d1 > 0.0) {
         /* have a way of suggesting minimum number of nodes to 
            enter in FWHM calculations 
            need at least area covering a radius of 3 voxels */
         MinArea = SUMA_PI * SUMA_POW2((3.0 * Opt->d1)); 
         SUMA_SetFWHM_MinArea(MinArea);
      }
      if (Opt->Use_emask) SUMA_Set_UseSliceFWHM(1);
      else SUMA_Set_UseSliceFWHM(0);
      if (!(dout = SUMA_CalculateLocalStats(SO, din, 
                                       Opt->nmask, 1,
                                       Opt->r, NULL,
                                       ncode, code, 
                                       NULL, Opt->NodeDbg,
                                       SOf))) {
         SUMA_S_Err("Failed in SUMA_CalculateLocalStats");
         exit(1);
      }
   
      /* write it out */
      if (Opt->debug) {
         SUMA_S_Notev("Writing output to %s\n", Opt->out_prefix);
      }
      ooo = SUMA_WriteDset_s(Opt->out_prefix, dout, iform, 
                              THD_ok_overwrite(), 0);
      if (dout) SUMA_FreeDset(dout); dout = NULL; SUMA_free(ooo); ooo=NULL;
   }
   
   
   if (din) SUMA_FreeDset(din); din = NULL;
   if (dout) SUMA_FreeDset(dout); dout = NULL;
   if (fwhmv) SUMA_free(fwhmv); fwhmv=NULL;
   if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   if (Opt) Opt = SUMA_Free_Generic_Prog_Options_Struct(Opt);
   if (!SUMA_Free_CommonFields(SUMAg_CF)) 
      SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   
   exit(0);
   
} 
