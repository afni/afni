#define MAIN

#ifdef USE_OMP
#include <omp.h>
#endif

#include "SUMA_suma.h"
#include "thd_segtools_fNM.h"
#include "SUMA_SegOpts.h"
#include "SUMA_SegFunc.h"
#include "matrix.h"

#ifdef USE_OMP
#include "mri_blur3d_variable.c"
#include "SUMA_SegFunc.c"
#endif


static int vn=0 ;


SEG_OPTS *Infill_ParseInput (SEG_OPTS *Opt, char *argv[], int argc)
{
   static char FuncName[]={"Infill_ParseInput"}; 
   int kar, i, ind, exists;
   char *outname, cview[10];
   int brk = 0;
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;

   ENTRY("Seg_ParseInput");
   
   brk = 0;
   kar = 1;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 Opt->helpfunc(strlen(argv[kar])>3 ? 2:1);
          exit (0);
		}
      
 		SUMA_SKIP_COMMON_OPTIONS(brk, kar);
     
      #ifdef USE_TRACING
            if( strncmp(argv[kar],"-trace",5) == 0 ){
               DBG_trace = 1 ;
               brk = 1 ;
            }
            if( strncmp(argv[kar],"-TRACE",5) == 0 ){  
               DBG_trace = 2 ;
               brk = 1 ;
            }
      #endif
      
      if (!brk && (strcmp(argv[kar], "-debug") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -debug \n");
				exit (1);
			}
			Opt->debug = atoi(argv[kar]);
         brk = 1;
		}      
      
      if (!brk && (strcmp(argv[kar], "-vox_debug") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need 1D vox index after -vox_debug \n");
				exit (1);
			}
         if (kar+2<argc) { /* see if we have ijk */
            int iii, jjj, kkk;
           if (argv[kar][0]!='-' && argv[kar+1][0]!='-' && argv[kar+2][0]!='-' &&
                (iii = atoi(argv[kar  ])) >= 0 &&
                (jjj = atoi(argv[kar+1])) >= 0 && 
                (kkk = atoi(argv[kar+2])) >= 0 ) {
               Opt->VoxDbg3[0]=iii;
               Opt->VoxDbg3[1]=jjj;
               Opt->VoxDbg3[2]=kkk;    
               ++kar; ++kar;
            } 
         }
			if (Opt->VoxDbg3[0] < 0) {
            Opt->VoxDbg = atoi(argv[kar]);
         }
         brk = 1;
		}      

      if (!brk && (strcmp(argv[kar], "-vox_debug_file") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need filename after -vox_debug_file \n");
				exit (1);
			}
			if (!strcmp(argv[kar],"-")) {
            Opt->VoxDbgOut = stdout;
         } else if (!strcmp(argv[kar],"+")) {
            Opt->VoxDbgOut = stderr;
         } else {
            Opt->VoxDbgOut = fopen(argv[kar],"w");
         }
         brk = 1;
		}      
      
     
      if (!brk && (strcmp(argv[kar], "-cmask") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		ERROR_exit("-cmask option requires a following argument!\n");
			}
			Opt->cmask = EDT_calcmask( argv[kar] , &(Opt->dimcmask), 0 ) ;
         if( Opt->cmask == NULL ) ERROR_exit("Can't compute -cmask!\n");
         brk = 1;
		}
      
      if (!brk && (strcmp(argv[kar], "-mask") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -mask \n");
				exit (1);
			}
			Opt->mset_name = argv[kar];
         brk = 1;
      }
      
      if( !brk && (strncmp(argv[kar],"-mrange",5) == 0 || \
                   strcmp(argv[kar],"-mask_range") == 0) ) {
         if( kar+2 >= argc )
           ERROR_exit("-mrange (or -mask_range) requires 2  arguments!\n");
         Opt->mask_bot = strtod( argv[++kar] , NULL ) ;
         Opt->mask_top = strtod( argv[++kar] , NULL ) ;
         if( Opt->mask_top < Opt->mask_bot )
           ERROR_exit("-mrange (or -mask_range) inputs are illegal!\n") ;
         brk = 1;
      }
      
      if (!brk && (strcmp(argv[kar], "-input") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -input \n");
				exit (1);
			}
			Opt->aset_name = argv[kar];
         brk = 1;
		}
            
      if (!brk && (strcmp(argv[kar], "-Niter") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need an integer after -Niter \n");
				exit (1);
			}
			Opt->N_main = (int)strtod(argv[kar],NULL);
         brk = 1;
		}

      if (!brk && (strcmp(argv[kar], "-radial_start") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need a positive integer after -radial_start \n");
				exit (1);
			}
			Opt->fmode = (int)strtod(argv[kar],NULL);
         brk = 1;
		}

      if (!brk && (strcmp(argv[kar], "-radial_fit_max") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need a positive integer after -radial_fit_max \n");
				exit (1);
			}
			Opt->i1 = (int)strtod(argv[kar],NULL);
         brk = 1;
		}

      if (!brk && (strcmp(argv[kar], "-radial_pass") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need a positive integer after -radial_pass \n");
				exit (1);
			}
			Opt->i2 = (int)strtod(argv[kar],NULL);
         switch (Opt->i2) {
            case 1:
               Opt->i2=1;
               break;
            case 2: 
               Opt->i2=5;
               break;
            case 3:
               Opt->i2=9;
               break;
            default:
               SUMA_S_Errv("Bad value (%d) for -radial_pass 1,2, or 3 allowed\n",
                  Opt->i2);
               exit(1);
         }
         brk = 1;
		}

      if (!brk && (strcmp(argv[kar], "-radial_fit_order") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need a positive integer after -radial_fit_order \n");
				exit (1);
			}
			Opt->i3 = (int)strtod(argv[kar],NULL);
         if (Opt->i3 < 0 || Opt->i3 > 6) {
            SUMA_S_Errv("-radial_fit_order must be between 0 and 6, have %d\n",
                        Opt->i3);
            exit (1);
         }
         brk = 1;
		}

      if (!brk && (strcmp(argv[kar], "-radial_smooth") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need FWHM after -radial_smooth \n");
				exit (1);
			}
			Opt->f1 = (float)strtod(argv[kar],NULL);
         brk = 1;
		}

      if (!brk && (strcmp(argv[kar], "-minhits") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need value between 1 and 3 after -minhits \n");
				exit (1);
			}
			Opt->i4 = (int)strtod(argv[kar],NULL);
         if (Opt->i4 < 1 || Opt->i4 > 3) {
            fprintf (stderr, "need value between 1 and 3 after -minhits \n");
				exit (1);
         }
         brk = 1;
		}

      if (!brk && (strcmp(argv[kar], "-blend") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need a string after -blend \n");
				exit (1);
			}
			if (!strcmp(argv[kar], "MODE")) Opt->Other = 1;
         else if (!strcmp(argv[kar], "AVG")) 
            Opt->Other = 0;
         else if (!strcmp(argv[kar], "AUTO")) 
            Opt->Other = -1;
         else if (!strcmp(argv[kar], "SOLID")) 
            Opt->Other = 2;
         else if (!strcmp(argv[kar], "SOLID_CLEAN")) 
            Opt->Other = 3;
         else {
            ERROR_exit("Bad value (%s) for -blend", argv[kar]);
         }
         brk = 1;
		}
      
      if (!brk && (strcmp(argv[kar], "-ed") == 0)) {
         kar ++;
			if (kar+1 >= argc)  {
		  		fprintf (stderr, "need two values after -ed \n");
				exit (1);
			}
			Opt->erode = Opt->dilate = atoi(argv[kar]); ++kar;
			Opt->f2 = atof(argv[kar]); 
         brk = 1;
		}
      
      if (!brk && (strcmp(argv[kar], "-prefix") == 0)) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (stderr, "need argument after -prefix \n");
				exit (1);
			}
			Opt->smode = storage_mode_from_filename(argv[kar]);
         Opt->prefix = (char*)calloc(strlen(argv[kar])+20, sizeof(char));
         sprintf(Opt->prefix,"%s", argv[kar]);
         brk = 1;
		}
      
      
      if (!brk) {
			fprintf (stderr,"Option %s not understood. \n"
                         "Try -help for usage\n", argv[kar]);
			suggest_best_prog_option(argv[0], argv[kar]);
         exit (1);
		} else {	
			brk = 0;
			kar ++;
		}

   }
   
   if (!Opt->prefix) Opt->prefix = strdup("./infill");
   if (Opt->uid[0]=='\0') UNIQ_idcode_fill(Opt->uid);
   if (Opt->VoxDbg > -1 && !Opt->VoxDbgOut) {
      char stmp[256];
      sprintf(stmp,"%d.GFD.dbg", Opt->VoxDbg);
      Opt->VoxDbgOut = fopen(stmp,"w");
   }

   RETURN(Opt);
}


void Infill_usage() 
{
   int i = 0;
   
   ENTRY("Infill_usage");
   
   
   printf( 
"A program to fill holes in a volumes.\n"
"\n"
"     3dinfill    <-input DSET> \n"
"\n"
"Options:\n"
"   -input  DSET: Fill volume DSET\n"
"   -prefix PREF: Use PREF for output prefix.\n"
"   -Niter NITER: Do not allow the fill function to do more than NITER\n"
"                 passes. A -1 (default) lets the function go to a maximum\n"
"                 of 500 iterations. You will be warned if you run our of \n"
"                 iterations and holes persist.\n"
"   -blend METH: Sets method for assigning a value to a hole.\n"
"                MODE: Fill with most frequent neighbor value. Use MODE when\n"
"                      filling integral valued data such as ROIs or atlases.\n"
"                AVG: Fill with average of neighboring values.\n"
"                AUTO: Use MODE if DSET is integral, AVG otherwise.\n"
"                SOLID: No blending, brutish fill. See also -minhits\n"
"                SOLID_CLEAN: SOLID, followed by removal of dangling chunks\n"
"                             Dangling chunks are defined as non-zero regions\n"
"                             that surround lesser holes, i.e. holes that have\n"
"                             less than MH. The cleanup step is not iterative\n"
"                             though, and you are most likely better off using\n"
"                             option -ed to do the cleanup.\n" 
"   -minhits MH: Crietrion for considering a zero voxel to be a hole\n"
"                MH refers to the total number of directions alogn which a\n"
"                zero voxel is considered surrounded by non zero values.\n"
"                a value of 1 is the least strict criterion, and a value of 3\n"
"                is the strictest. \n"
"                This parameter can only be used with -blend SOLID\n"
"   -ed N V: Erode N times then dialate N times to get rid of hanging chunks.\n"
"            Values filled in by this process get value V.\n"
"   -mask MSET: Provide mask dataset to select subset of input.\n"
"   -mask_range BOT TOP: Specify the range of values to consider from MSET.\n"
"                        Default is anything non-zero.\n"
"   -mrange BOT TOP: Same as option -mask_range\n" 
"   -cmask CMASK: Provide cmask expression. Voxels where expression is 0\n"
"                 are excluded from computations. For example:\n"
"            -cmask '-a T1.div.r+orig -b T1.uni.r+orig -expr step(a/b-10)'\n"
"   NOTE: For the moment, masking is only implemented for the SOLID* fill\n"
"         method.\n"
/*
" The following set of options are used to fill the outside of a volume \n"
" using radial linear interpolation:\n"
"        For each voxel on the perimeter of the grid, the program \n"
"        samples the voxels until along the trace to the volume's \n"
"        center of mass. Once MINVOX consecutive non-zero (in mask)\n"
"        voxels are encountered, sampling continues until a total\n"
"        MAXFIT non-zero voxels are found. A linear model is fit to\n"
"        the MAXFIT voxels and used to extrapolate to voxels \n"
"        outside the mask.\n"
"   -radial_start MINVOX: Minimum number of consecutive non-zero voxels \n"
"                        required to begin sampling.\n"
"   -radial_fit_max MAXFIT: Maximum number of non-zero voxels used to fit\n"
"                        the model. Those will include the voxels needed\n"
"                        to reach MINVOX threshold.\n"
"   -radial_fit_order ORD:  Set the fit order, 0 = constant, 1 = linear, ...\n"
"                       Default is 1\n"
"   -radial_smooth FWHM: Smooth resultant volume by FWHM mm\n"  
"   -radial_pass NPASS: NPASS controls the number of center offsets\n"
"                       to ensure fuller coverage of the area outside\n"
"                       the mask. By default NPASS is 3, but for speed\n"
"                       you can choose 2, or 1. With 1 you are almost\n"
"                       assured a few voxels will remain unfilled unless\n"
"                       you smooth the result. With 9, you'll get the vast\n"
"                       majority of voxels outside the mask. Voxels inside\n"
"                       the mask are not filled\n"  
*/
"\n"
"Example 1:\n"
"Starting from a whole head mask that has some big holes in it where CSF and \n"
"cavities are. Fill the inside of the mask and remove dangling chunks in the\n"
"end with -ed \n"
"        3dinfill -blend SOLID -ed 3 1 -prefix filledmask \\\n"
"                 -minhits 2 -input holymask+orig.  \n"
"\n"
"This program will be slow for high res datasets with large holes.\n"
"If you are trying to fill holes in masks, consider also:\n"
"  3dmask_tool -fill_holes \n"
"\n"
        );
   
   
   EXRETURN;
}

SEG_OPTS *Infill_Default(char *argv[], int argc) 
{
   SEG_OPTS *Opt=NULL;
   
   ENTRY("Infill_Default");
   
   Opt = SegOpt_Struct();
   Opt->helpfunc = &Infill_usage;
   Opt->ps = SUMA_Parse_IO_Args(argc, argv, "-talk;");
   Opt->aset_name = NULL;
   Opt->mset_name = NULL;
   Opt->uid[0] = '\0';
   Opt->prefix = NULL;
   Opt->aset = NULL;
   Opt->mset = NULL;
   Opt->debug = 0;
   Opt->VoxDbg = -1;
   Opt->VoxDbg3[0] = Opt->VoxDbg3[1] = Opt->VoxDbg3[2] = -1;
   Opt->VoxDbgOut = stderr;
   Opt->Other = -1;
   Opt->fmode = 0;
   Opt->i1 = -1;
   Opt->i2 = 9;
   Opt->i3 = -1;
   Opt->i4 = -1;
   Opt->f1 = 0.0;
   Opt->erode = Opt->dilate = 0;
   Opt->f2 = 0.0;
   Opt->VoxDbg = -1;
   Opt->VoxDbg3[0] = Opt->VoxDbg3[1] = Opt->VoxDbg3[2] = -1;
   Opt->VoxDbgOut = NULL;
   Opt->erode = 0;
   Opt->dilate = 0;
   Opt->openmp = 0;
   Opt->N_main = -1;
   Opt->Bset=NULL;
   Opt->proot = NULL;
   Opt->cmask = NULL;
   Opt->smode = STORAGE_BY_BRICK;   
   RETURN(Opt);
}

int main(int argc, char **argv)
{
   static char FuncName[]={"3dinfill"};
   SEG_OPTS *Opt=NULL;
   char *atr=NULL;
   float *mixfrac= NULL;
   int i=0;
   double ff;
   SUMA_SEND_2AFNI SS2A;
   SUMA_Boolean LocalHead = NOPE;

   
   SUMA_STANDALONE_INIT;
	SUMA_mainENTRY;
   
   SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   
   Opt = Infill_Default(argv, argc);
   Opt = Infill_ParseInput (Opt,argv,  argc);
   Opt->hist = tross_commandline( FuncName , argc , argv ) ;
   

   /* load the input data */
   if (!(Opt->aset = Seg_load_dset( Opt->aset_name ))) {      
      SUMA_RETURN(1);
   }
   
   if (Opt->mset_name) {
      if (!(Opt->mset = Seg_load_dset( Opt->mset_name ))) {      
         SUMA_RETURN(1);
      }
   }
   
   Opt->cmask = MaskSetup(Opt, Opt->aset, 0,
                &(Opt->mset), &(Opt->cmask), Opt->dimcmask, 
                Opt->mask_bot, Opt->mask_top, &(Opt->cmask_count));
                
   /* Fix VoxDbg */
   if (Opt->VoxDbg >= 0) {
      Vox1D2Vox3D(Opt->VoxDbg, 
                  DSET_NX(Opt->aset), DSET_NX(Opt->aset)*DSET_NY(Opt->aset),
                  Opt->VoxDbg3);
   } else if (Opt->VoxDbg3[0]>=0) {
      Opt->VoxDbg = Opt->VoxDbg3[0] + Opt->VoxDbg3[1]*DSET_NX(Opt->aset) +
                        Opt->VoxDbg3[2]*DSET_NX(Opt->aset)*DSET_NY(Opt->aset);
   }
   SUMA_set_SegFunc_debug( Opt->debug, Opt->VoxDbg, Opt->VoxDbg3, 
                           Opt->VoxDbgOut);
   
   if (Opt->fmode > 0) {
      if (Opt->f1 == -1.0f) Opt->f1 = Opt->fmode;
      if (Opt->i1 == -1) Opt->i1 = 6*Opt->fmode;
      if (Opt->i1 < Opt->fmode) {
         SUMA_S_Errv("-radial_fit_max (%d) parameter < -radial_start (%d)\n",
                     Opt->i1, Opt->fmode);
         exit(1); 
      }
      if (Opt->i3 == -1) Opt->i3 = 1;
      if (!SUMA_Volume_RadFill(Opt->aset, NULL, NULL, NULL, 
                            &Opt->Bset, Opt->fmode, Opt->i1, Opt->i3,
                            Opt->f1, Opt->i2)) {
         SUMA_S_Err("Failed to rad fill volume");
         SUMA_RETURN(1);
      }
   } else {
      switch (Opt->Other) {
         default:
            /* using method 1: SUMA_mri_volume_infill_zoom */
            if (!SUMA_VolumeInFill(Opt->aset, &Opt->Bset, Opt->Other, -1,
                          Opt->N_main, -1, Opt->erode, Opt->dilate, Opt->f2,
                          Opt->cmask)) {
               SUMA_S_Err("Failed to fill volume");
               SUMA_RETURN(1);
            }
            break;
         case 2:
         case 3:
            /* using method 2: SUMA_mri_volume_infill_solid */
            if (Opt->i4 < 0) Opt->i4 = 3;
            if (!SUMA_VolumeInFill(Opt->aset, &Opt->Bset, Opt->Other,
                    -1, Opt->N_main, Opt->i4, Opt->erode, Opt->dilate, Opt->f2,
                    Opt->cmask)){
               SUMA_S_Err("Failed to fill volume with solid method");
               SUMA_RETURN(1);
            }
            break;
      }
   }
      
   /* write output */
   if (Opt->Bset) {
      tross_Append_History(Opt->Bset, Opt->hist);
      SUMA_Seg_Write_Dset(Opt->proot, Opt->prefix, /* DSET_PREFIX(Opt->Bset) */
                          Opt->Bset, -1, Opt->hist);
   }
                       
   /* all done, free */
   Opt = free_SegOpts(Opt);
  
   PRINT_COMPILE_DATE ; 
   SUMA_RETURN(0);
}
