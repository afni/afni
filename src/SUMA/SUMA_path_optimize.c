#include "SUMA_suma.h"
#include "matrix.h"
#include "SUMA_SurfWarp.h"

SUMA_SurfaceViewer *SUMAg_cSV = NULL; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
SUMA_DO *SUMAg_DOv = NULL;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF = NULL; /*!< Global pointer to structure containing info common to all viewers */



void usage_path_optimize (SUMA_GENERIC_ARGV_PARSE *ps)
{
      static char FuncName[]={"usage_path_optimize"};
      char * s = NULL, *sio=NULL, *st = NULL, *sts = NULL;
      int i;
      s = SUMA_help_basics();
      sio  = SUMA_help_IO_Args(ps);
      printf ( "\n"
               "usage:\n"
               "  path_optimize [-ctrl CTRL_FILE] [-dom_dim DOM_DIM] [-dim DIM] [-N_sub N_SUB] [-N_step N_STEP]   \n"
               "             [-renew_weights] [-adjust] [-dot] [-neighb_adjust] [-neighb_check] [-sigma SIGMA] \n"
               "             [-talk_pause] [-debug DEBUG] [-ouput OUTPUT] [-M_time M_TIME]                     \n"
               "                                                                                               \n"
               "  -ctrl CTRLFILE:   Control nodes 1D file.                                                     \n"
               "                    Each row is for one node's intial and final XYZ.                           \n" 
               "  -dom_dim DOM_DIM: Set domain dimension. Default is 2, a circle.                              \n"
               "                    Must choose dom_dim = 3 for a sphere.                                      \n" 
               "  -dim DIM:         Set the dimension.  Default is 2D. Needed because of -dot option.          \n"
               "  -N_sub N_SUB:     Set the number of subdivisions on the circle.                              \n"
               "                    Approximate the number of subdivisions on the icosahedron.                 \n"
               "                    Default N_sub is 100.                                                      \n"
               "  -N_step N_STEP:   Set the number of steps (inverse dt).                                      \n"
               "  -renew_weights:   Choose to recalculate spline weights after every step.                     \n"
               "                    Default does not renew the weights.                                        \n"
               "  -adjust:          Choose to scale displacement.                                              \n"
               "                    Default is no adjustment.                                                  \n"  
               "  -dot:             Choose to use a dot product requirment when calculated the weights         \n"
               "                    to ensure tangency.                                                        \n"
               "  -neighb_check:    Will check at each iteration if the distance to each node's nearest        \n"
               "                    neighbor is smaller than the proposed step size.  Will print results       \n"
               "                    to file, but will not make any step size adjustment.                       \n"
               "  -neighb_adjust:   Checks neighbor distances and adjusts step size accordingly.               \n"
               "  -sigma SIGMA:     Include sigma constant when calculating weights. Set value of sigma.       \n"
               "                    Default does not use sigma.                                                \n"
               "  -talk_pause:      For use with SUMA and -talk_suma option.  Will pause program after         \n"
               "                    intial conditions are setting, allowing user to adjust viewing window      \n"
               "                    before completing the rest of the iterations.                              \n"
               "  -debug DEBUG:     Choose to turn debugging on.  Default is no debugging.                     \n"                        
               "  -output OUTPUT:   Name output file. Default OUTPUT is test_move.                             \n"
               "  -M_time M_TIME:   Choose number of time steps used in optimization.                          \n"
               "                    If set M_time = 1, then no optimization performed.                         \n"
               "                                                                                               \n" 
               "%s"
               "%s"
               "\n", sio,  s);
      SUMA_free(s); s = NULL; SUMA_free(st); st = NULL; SUMA_free(sio); sio = NULL;       
      s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
      printf(  "       Julia T. Molony SSCC/NIMH/NIH molonyj@nih.gov \n"
               "       Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov       \n");
      exit(0);
}

SUMA_GENERIC_PROG_OPTIONS_STRUCT *SUMA_path_optimize_ParseInput(char *argv[], int argc, SUMA_GENERIC_ARGV_PARSE *ps, MyCircleOpt *popt)
{
   static char FuncName[]={"SUMA_path_optimize_ParseInput"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt=NULL;
   int kar, shft;
   SUMA_Boolean brk;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   Opt = SUMA_Alloc_Generic_Prog_Options_Struct();
   
   popt->dbg_flag = 0;
   popt->N_sub = 100;
   popt->N_step = 100;
   popt->dt = 0.001; 
   popt->ctrl = NULL;
   popt->N_ctrl_points = 1;
   popt->renew_weights = 0;
   popt->CtrlPts_iim = NULL;
   popt->CtrlPts = NULL;
   popt->CtrlPts_i = NULL;
   popt->CtrlPts_f = NULL;
   popt->Dtheta = NULL;
   popt->Nrm = NULL;    /* Access of rotation used in the spline weights function. */
   popt->dim = 2;
   popt->adjust = 0;
   popt->dom_dim = 2;
   popt->dot = 0;
   popt->neighb_adjust = 0;
   popt->neighb_check = 0;
   popt->pause = 0;
   popt->sigma = 0.0;
   popt->M_time_steps = 10;
   snprintf(popt->outfile, 499*sizeof(char),"test_move");
   kar = 1;
   brk = NOPE;
	while (kar < argc) { /* Loop across command line options. */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_path_optimize(ps);
          exit (0);
		}
		
		SUMA_SKIP_COMMON_OPTIONS(brk, kar);
      
      if (!brk && (strcmp(argv[kar], "-debug") == 0))
      {
         popt->dbg_flag = 1; 
         brk = 1;
      }
      if (!brk && (strcmp(argv[kar], "-talk_pause") == 0))
      {
         popt->pause = 1; 
         brk = 1;
      }
      if (!brk && (strcmp(argv[kar], "-dt") == 0)) {
         fprintf(stderr, "No MORE dt, fool!\n");
         exit (1);
         kar ++;
         if (kar >= argc)  
         {
            fprintf (stderr, "need argument after -dt \n");
            exit (1);
         }
         popt->dt = (double)atof(argv[kar]);               
         brk = 1;
      }
      
      if (!brk && (strcmp(argv[kar], "-sigma") == 0)) {
         kar ++;
         if (kar >= argc)  
         {
            fprintf (stderr, "need argument after -sigma \n");
            exit (1);
         }
         popt->sigma = (double)atof(argv[kar]);               
         brk = 1;
      }
      
      if (!brk && (strcmp(argv[kar], "-dom_dim") == 0)) {
         kar ++;
         if (kar >= argc)  
         {
            fprintf (stderr, "need argument after -dom_dim \n");
            exit (1);
         }
         popt->dom_dim = atoi(argv[kar]);               
         if (popt->dom_dim != 2 && popt->dom_dim != 3) {
            fprintf (stderr, "Easy there Einstein \n");
            exit (1);
         } 
         brk = 1;
      }
      if (!brk && (strcmp(argv[kar], "-N_sub") == 0)) {
         kar ++;
         if (kar >= argc)  
         {
            fprintf (stderr, "need argument after -N_sub \n");
            exit (1);
         }
         popt->N_sub = atoi(argv[kar]);               
         if (popt->N_sub <= 0 || popt->N_sub > 10000000) {
            fprintf (stderr, "You are mad \n");
            exit (1);
         } 
         brk = 1;
      }
      if (!brk && (strcmp(argv[kar], "-N_step") == 0)) {
         kar ++;
         if (kar >= argc)  
         {
            fprintf (stderr, "need argument after -N_step \n");
            exit (1);
         }
         popt->N_step = atoi(argv[kar]);               
         if (popt->N_step <= 0 || popt->N_step > 100000000) {
            fprintf (stderr, "You are mad \n");
            exit (1);
         } 
         brk = 1;
      }
      
      if (!brk && (strcmp(argv[kar], "-M_time") == 0)) {
         kar ++;
         if (kar >= argc)  
         {
            fprintf (stderr, "need argument after -M_time \n");
            exit (1);
         }
         popt->M_time_steps = atoi(argv[kar]);               
         if (popt->M_time_steps <= 0 || popt->M_time_steps > 100) {
            fprintf (stderr, "You are mad \n");
            exit (1);
         } 
         brk = 1;
      }
  
      if (!brk && (strcmp(argv[kar], "-ctrl") == 0)) {
         kar ++;
         if (kar >= argc)  
         {
           fprintf (stderr, "need filename after -ctrl \n");
           exit (1);
         }
         popt->ctrl = argv[kar];
         if (!SUMA_filexists(popt->ctrl)) {
            SUMA_S_Err("ctrl file not found");
            exit(1);
         }
         brk = 1;
      }
      
      if (!brk && (strcmp(argv[kar], "-dim") == 0)) {
         kar ++;
         if (kar >= argc)  
         {
           fprintf (stderr, "need argument after -dim \n");
           exit (1);
         }
         popt->dim = atoi(argv[kar]);
         brk = 1;
      }
      
      if (!brk && (strcmp(argv[kar], "-renew_weights") == 0)) {
         popt->renew_weights = 1;
         brk = 1;
      }      
      
      if (!brk && (strcmp(argv[kar], "-adjust") == 0)) {
         popt->adjust = 1;
         brk = 1;
      }  
      
      if (!brk && (strcmp(argv[kar], "-dot") == 0)) {
         popt->dot = 1;
         brk = 1;
      }  
      
      if (!brk && (strcmp(argv[kar], "-neighb_adjust") == 0)) {
         popt->neighb_adjust = 1;
         brk = 1;
      } 
      
      if (!brk && (strcmp(argv[kar], "-neighb_check") == 0)) {
         popt->neighb_check = 1;
         brk = 1;
      } 

      if (!brk && (strcmp(argv[kar], "-output") == 0)) {
         kar ++;
         if (kar >= argc)  
         {
           fprintf (stderr, "need argument after -output \n");
           exit (1);
         }
         snprintf(popt->outfile, 400*sizeof(char),"%s", argv[kar]);
         brk = 1;
      }

      if (!brk && !ps->arg_checked[kar]) {
			fprintf (SUMA_STDERR,"Error %s:\n Option %s not understood. Try -help for usage\n", FuncName, argv[kar]);
			exit (1);
		} else {	
			brk = NOPE;
			kar ++;
		}
   }
 
   if (!popt->ctrl) {
      SUMA_S_Err("No control file specified");
      exit(1);
   }
   
   /* Read the control point file. */
   {
      MRI_IMAGE *im = NULL;
      float *far=NULL;
      int i, i3, i6, i7, co[6];
      
      im = mri_read_1D (popt->ctrl);

      if (!im) {
         SUMA_S_Err("Failed to read 1D file");
         exit(1);
      }
      
      if (LocalHead) {
         fprintf(SUMA_STDERR,"%s: Have %d col, %d rows in 1D file %s\n",
               FuncName, im->ny, im->nx, popt->ctrl);
      }
      
      far = MRI_FLOAT_PTR(im);
      if (!im->nx) {
         SUMA_S_Err("Empty ctrl points file");
         exit(1);
      }
      popt->N_ctrl_points = im->nx;
      if (im->ny != 6 && im->ny != 7) {
         SUMA_S_Err("Expecting 6 or 7 columns in ctrl points file");
         exit(1);
      }
      if (im->ny == 7) { 
         shft = 1;
      } else { 
         shft = 0; 
      }
      /* allocate for CtrlPoints and initialize them */
      popt->CtrlPts_iim = (int *)SUMA_malloc(popt->N_ctrl_points * sizeof (int));
      popt->CtrlPts = (double *)SUMA_calloc(popt->N_ctrl_points * 3, sizeof (double));    /* renewed initial (t=0) XYZ location of each node */
      popt->CtrlPts_f = (double *)SUMA_calloc(popt->N_ctrl_points * 3, sizeof (double));  /* final   (t=1) XYZ location of each node */
      popt->CtrlPts_i = (double *)SUMA_calloc(popt->N_ctrl_points * 3, sizeof (double));  /* initial XYZ location of each node, never renewed */
                                                                                          /* Cp_i used for error calculations at end. */
      /* column offsets */
      for (i=0; i<6; ++i) { co[i] = (i+shft)*popt->N_ctrl_points; }
      
      for (i=0; i < popt->N_ctrl_points; ++i) {
         i3 = 3*i;
         if (shft) popt->CtrlPts_iim[i] = (int)far[i];
         else popt->CtrlPts_iim[i] = -1;
         popt->CtrlPts_i[i3  ] = far[i+co[0]];
         popt->CtrlPts_i[i3+1] = far[i+co[1]];        
         popt->CtrlPts_i[i3+2] = far[i+co[2]];
         popt->CtrlPts_f[i3  ] = far[i+co[3]];
         popt->CtrlPts_f[i3+1] = far[i+co[4]];
         popt->CtrlPts_f[i3+2] = far[i+co[5]];
         ++shft;
      }
      
      if (LocalHead) {
         fprintf(SUMA_STDERR, "%s: Contents of ctrl file:\n", FuncName);
         for (i=0; i < popt->N_ctrl_points; ++i) {
            i3 = 3*i;
            fprintf(SUMA_STDERR, "%d   %.5f   %.5f   %.5f   %.5f   %.5f   %.5f   \n",
                                 popt->CtrlPts_iim[i],
                                 popt->CtrlPts_i[i3  ], popt->CtrlPts_i[i3+1], popt->CtrlPts_i[i3+2],
                                 popt->CtrlPts_f[i3  ], popt->CtrlPts_f[i3+1], popt->CtrlPts_f[i3+2] );
         }
      }
      mri_free(im); im = NULL; 
   }
   
   Opt->popt = (void*)popt;
   SUMA_RETURN(Opt);
}

/* Julia:
This main started as a copy of SUMA_toy_circle and I added to it an example to show you how to use 
the array functions.
To comile this program, use:
 make path_optimize && mv path_optimize ~/abin       

The functions should be put in SUMA_SurfWarp.c and function prototypes should be in SUMA_SurfWarp.h. Don't forget
to close each prototype with the ';'

The main programs (toy_circle and path_optimize) depend on SUMA_SurfWarp.o so if SUMA_SurfWarp.c was modified,
it will automatically get recompiled before the main. Note that compiling errors can occur in SUMA_SurfWarp.c as 
well as in SUMA_toy_circle.c or SUMA_path_optimize.c but the compiler's complaint should easily point you to the 
proper file.

I also left all the help and parsing that was used in toy_circle. I am sure you'll need many of the same options and
it is easier to trim and add rather than start anew.

The array manipulation code is inside a block tagged with:
   ZSS Array demo 
*/

int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"MAIN_path_optimize"}; 
   char outfile_SphereQuality[50], outfile_PlotPath[50], outfile_PlotPathSeg[50], outfile_PathConnected[50];
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt;  
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   MyCircleOpt myopt, *opt = NULL;
   int i, i3, idm, j, j3, k, adj_factor = 10, s4, p, m, opt_count = 0;
   double dt;    
   double oxyz[3]={0.0, 0.0, 0.0};
   double oda, faa, error, dtheta=0.0, nrmi[3]={0.0, 0.0, 0.0}, nrmf[3]={0.0, 0.0, 0.0};
   int niter=0, a_niter, first_bad_niter = 0;
   SUMA_SurfaceObject *SO = NULL;
   void *SO_name;
   char *shist=NULL;
   int nbad;
   int too_close = 0, need_neighb_adjust = 0, need_more_adjustment = 0;
   double energy, energy_sum, time_elapsed;    /* DO THESE NEED TO BE STATIC VARIABLES?  
                                                         DO I NEED THEM WHEN I RETURN TO THE FUNCTION?? */
   double *dp = NULL, *dp_mi = NULL, *dp_mf = NULL, *dp_new = NULL; 
   double Lda = 0.0;
   
    
   SUMA_Boolean exists;
      
   MyCircle *Ci = NULL;
   vector Wv;
   
   SUMA_Boolean LocalHead = NOPE;

	SUMA_mainENTRY;
   SUMA_STANDALONE_INIT;

#if 0
   /* ZSS Array demo starts here ... */
   /* Form a test array M of size 2x3x4x2 like so (created in matlab, indices start at 1 in matlab!) 
   M(:,:,1,1) = [ 1 2 3; 4 5 6]
      M(:,:,1) =

           1     2     3
           4     5     6
   M(:,:,2,1) = 2*[ 1 2 3; 4 5 6]
   M(:,:,3,1) = 3*[ 1 2 3; 4 5 6]
   M(:,:,4,1) = 4*[ 1 2 3; 4 5 6]
   M(:,:,:,2) = 0.5 + M(:,:,:,1)
   a one dimentional representation of M with first dimension first would be : M(:)'
       fprintf(1,'%.1f, ', M(:)')
       1.0, 4.0, 2.0, 5.0, 3.0, 6.0, 2.0, 8.0, 4.0, 10.0, 6.0, 12.0, 3.0, 12.0, 
       6.0, 15.0, 9.0, 18.0, 4.0, 16.0, 8.0, 20.0, 12.0, 24.0, 1.5, 4.5, 2.5, 5.5, 
       3.5, 6.5, 2.5, 8.5, 4.5, 10.5, 6.5, 12.5, 3.5, 12.5, 6.5, 15.5, 9.5, 18.5, 
       4.5, 16.5, 8.5, 20.5, 12.5, 24.5
   */
   /* Now I will initialize a one dimensional vector with the results above so that you
   can test the various functions and get comfortable using them. You could also use
   the commands above in matlab, which provides for convenient indexing and compare results*/
   {
      double vr[]= { 1.0, 4.0, 2.0, 5.0, 3.0, 6.0, 2.0, 8.0, 4.0, 10.0, 6.0, 12.0, 3.0, 12.0, 
       6.0, 15.0, 9.0, 18.0, 4.0, 16.0, 8.0, 20.0, 12.0, 24.0, 1.5, 4.5, 2.5, 5.5, 
       3.5, 6.5, 2.5, 8.5, 4.5, 10.5, 6.5, 12.5, 3.5, 12.5, 6.5, 15.5, 9.5, 18.5, 
       4.5, 16.5, 8.5, 20.5, 12.5, 24.5}; /*    Note, that you need not specify a size for vr (vr[]) 
                                                when you initialize it with its elements.
                                                These are the values, stored in multiplexed vector
                                                format, to populate the 
                                                4 dimensional array
                                                */
      int dims[10] = { 2, 3, 4, 2 }; /*   but you can always specify size if you want to, 
                                          here the size is bigger than the number of elements initialized.
                                          These are the dimensions of that 4 dimensional array */
      int N_dims = 4; /* Number of dimensions for the array */
      SUMA_MX_VEC *mxv = NULL; /* The structure that will carry the array and some auxilliary info. */
      int i,j,k,l,n; /* stupid indexing stuff */
      double d1, *dp; /* some dummy variables for illustrating matters */
      
      mxv = SUMA_NewMxVec(SUMA_double, 4, dims, 1); /* allocate for multiplexed vector structure, see function
                                                      help and code in SUMA_DataSets.c. The macros used below
                                                      are defined in SUMA_DataSets.h */                                             
      SUMA_ShowMxVec (mxv, 1, NULL); /* show me the contents of mxv struct */                                                
      /* Since I have vr created from matlab, let us insert it into mxv and then access its content with 4 indices */
      for (n=0; n<mxv->N_vals; ++n) mxv->dv[n] = vr[n];  
      SUMA_ShowMxVec (mxv, 1, NULL); /* Now show me the contents of mxv struct */                                                
      fprintf (stderr,"DIMS AS STORED IN MXV:%d %d %d %d\n"
                      "NUMBER OF DIMS AS STORED IN MXV: %d\n", mxv->dims[0], mxv->dims[1], mxv->dims[2], mxv->dims[3], mxv->N_dims);
      
      /* Now, for some access tricks */
         /* Each time you use mutliple indices to access an element, the indices have to be combined to get at that element.
            For example element (i,j,k,l) is stored at index i + j*dims[0] + k*dims[0]*dims[1] + l*dims[0]*dims[1]*dims[2]
            So try to minimize multi indexing access, to the extent possible. Don't beat yourself silly over this though. */
         /* method 1, get some value in array*/
            i = 0; j = 2; k = 2; l = 0; /* in matlab example, this would be M(1,3,3,1) */
            fprintf (stderr,"Value at (%d, %d, %d, %d) is %f\n", i,j,k,l, mxvd4(mxv, i,j,k,l));
         /* method 2, get the POINTER to some value in array */
            dp = mxvdp4(mxv, i,j,k,l); /* note the slight change in the macro's name */
            /* Since this array is store first dim first, dp[0] and dp[1] would give me
               values at (i,j,k,l) and (i+1, j, k, l) (assuming i+1 is <= dims[0]). So if
               your first dimension refers to xyz coordinates then the coordinates of a point
               at j,k,l would be dp[0], dp[1] and dp[2] . In this toy example, the first dim is of size 2
               so think xy instead of xyz (life is not fair Julia!). */
            fprintf (stderr,"Values at (:, %d, %d, %d): %f %f\n", j,k,l, dp[0], dp[1]); /* in matlab the two values would be M(1,3,3,1) and M(2,3,3,1) */
            /* Note the avantage of having retrieved a bunch of values via the pointer route without having
               to call  mxvd4 repeatedly. But again, this advantage is only for stepping along the first 
               dimension of the array */
      /* Now how can we put some values into the array */         
         /* method 1, directly, one value at a time */
            mxvd4(mxv, i,j,k,l) = -mxvd4(mxv, i,j,k,l); /* just flipping signs */
            mxvd4(mxv, i,j-1,k,l) = -999.09; /* something */
            fprintf (stderr,"New Value at (%d, %d, %d, %d) is %f\n", i,j,k,l, mxvd4(mxv, i,j,k,l));  
            fprintf (stderr,"New Value at (%d, %d, %d, %d) is %f\n", i,j-1,k,l, mxvd4(mxv, i,j-1,k,l));  
            /* note that the macros do not check that i,j,k,l and >=0 and < dims[*] otherwise they would be more inefficient. 
                                                                                    you should be careful with your indices */
         /* method 2, via the pointer advantageous along the 1st dim */
            dp = mxvdp4(mxv, i,j,k,l);
            dp[0] = -1.0; dp[1] = -2.0;
            fprintf (stderr,"Newer Values at (:, %d, %d, %d): %f %f\n", 
                              j,k,l, mxvd4(mxv, i,j,k,l), mxvd4(mxv, i+1,j,k,l)); /* Note that I used mxvd4 in two separate calls 
                                                                                    instead of dp[0] and dp[1]. But I wanted to 
                                                                                    highlight that the values were indeed changed
                                                                                    inside of mxv (as they should) */
      /* Show me All the contents of mxv, don't do this for very large arrays!*/
         SUMA_ShowMxVec (mxv, -1, NULL); 
            
      /* all done, cleanup and exit */
      
      mxv = SUMA_FreeMxVec(mxv);
      exit(0);  /*Used to stop the program here. */
   }   
   /* ZSS Array demo stops here ... */
#endif  
 
   /* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "-o;-talk;");
   
   if (argc < 2) {
      usage_path_optimize(ps);
      exit (1);
   }
   
   Opt = SUMA_path_optimize_ParseInput (argv, argc, ps, &myopt);
   opt = (MyCircleOpt *)Opt->popt;
   
/*******Begin Optimization Code************************/   
      
      int N_dims = 3;
      int dims[10] = {3, opt->N_ctrl_points, (opt->M_time_steps + 1) };
      SUMA_MX_VEC *ControlCurve = NULL;
      ControlCurve = SUMA_NewMxVec( SUMA_double, N_dims, dims, 1 );
   
      /* Store max allowabe movement of the ith point at the mth time step. Need this later for limiting lamda. */
      int M_N_dims = 2;
      int M_dims[10] = {opt->N_ctrl_points, (opt->M_time_steps + 1) };
      SUMA_MX_VEC *MaxStep = NULL;
      MaxStep = SUMA_NewMxVec( SUMA_double, M_N_dims, M_dims, 1 );

      /* Stores perturbation vectors with theta included. */
      /* Need to store a zero perturbation vector for the end points.  
            Include this vector for ease of loop calculations later. */
      int Perturb_N_dims = 4; 
      int Perturb_dims[12] = {3, opt->N_ctrl_points, (opt->M_time_steps+1), 2 };
      SUMA_MX_VEC *Perturb_Vec = NULL;
      Perturb_Vec = SUMA_NewMxVec( SUMA_double, Perturb_N_dims, Perturb_dims, 1 );
      
      int S_N_dims = 4;
      int S_dims[20] = {opt->N_ctrl_points, opt->M_time_steps, 2, 2}; /* control point, time step, q, p */   
                     /* q for which point perturbed, p for type of perturbation. */
      SUMA_MX_VEC *Del_S = NULL;
      Del_S = SUMA_NewMxVec( SUMA_double, S_N_dims, S_dims, 1 );
      
      /* Stores new path. */
      int XL_N_dims = 3;
      int XL_dims[10] = {3, opt->N_ctrl_points, (opt->M_time_steps+1) };
      SUMA_MX_VEC *X_Lamda = NULL;
      X_Lamda = SUMA_NewMxVec( SUMA_double, XL_N_dims, XL_dims, 1 );
      
      /* Find the initial control curve.  Calculated uing desired arc of travel and number of steps taken. */
      Set_up_Control_Curve(opt, ControlCurve);
   
   do{
      /* To see the values stored at ControlCurve. */ 
      /* SUMA_ShowMxVec(ControlCurve, -1, NULL); */   
      fprintf(SUMA_STDERR, "Control Curve:\n");
      for(i=0; i<opt->N_ctrl_points; ++i) {
         for (m=0; m<opt->M_time_steps+1; ++m) {
            dp = mxvdp3(ControlCurve, 0, i, m);
            fprintf(SUMA_STDERR, "%f %f %f\n", dp[0], dp[1], dp[2]);
         }
         fprintf(SUMA_STDERR, "\n");
      }
    
      /* Store path as a sphere or segment file to be viewed in SUMA. */
      FILE *PlotPath = NULL;
      sprintf(outfile_PlotPath, "SUMA_path%d.1D.dset", opt_count); 
      PlotPath = fopen (outfile_PlotPath, "w"); 
      fprintf (PlotPath, "#spheres\n");
      dp = NULL;
      for(i=0; i<opt->N_ctrl_points; ++i) {
         for (m=0; m<opt->M_time_steps+1; ++m) {
            dp = mxvdp3(ControlCurve, 0, i, m);
            if(i==0) fprintf (PlotPath, "%f   %f    %f   1.0  0.0  0.0  1.0  0.01  1\n", dp[0], dp[1], dp[2]);
            if(i==1) fprintf (PlotPath, "%f   %f    %f   0.0  1.0  0.0  1.0  0.01  1\n", dp[0], dp[1], dp[2]);
            if(i==2) fprintf (PlotPath, "%f   %f    %f   0.0  0.0  1.0  1.0  0.01  1\n", dp[0], dp[1], dp[2]);
            if(i==3) fprintf (PlotPath, "%f   %f    %f   0.63 0.0 0.67  1.0  0.01  1\n", dp[0], dp[1], dp[2]);
            dp = NULL;
         }
         fprintf (PlotPath,"\n");
      }   

      Perturbations( opt, ControlCurve, MaxStep, Perturb_Vec );

      fprintf(SUMA_STDERR, "Max Allowable Movement:\n");
      SUMA_ShowMxVec(MaxStep, -1, NULL);
      /*SUMA_ShowMxVec(Perturb_Vec, -1, NULL);*/

      fprintf(SUMA_STDERR, "%s: Perturbation Vectors:\n", FuncName);
      for(p=0; p<2; ++p) {
         for(k=0; k<(opt->M_time_steps+1); ++k) {
            for(i=0; i<opt->N_ctrl_points; ++i) {
               dp = NULL;
               dp = mxvdp4(Perturb_Vec, 0, i, k, p);
               fprintf(SUMA_STDERR, "%f   %f   %f\n", dp[0], dp[1], dp[2]);
            }
         }
      }

      fprintf( SUMA_STDERR, "%s: Called Change_in_Energy.\n", FuncName); 
      Change_in_Energy(opt, ControlCurve, Perturb_Vec, Del_S);

      /* fprintf(SUMA_STDERR, "Show Del_S:\n");
         SUMA_ShowMxVec(Del_S, -1, NULL); */

      /* THIS IS IT! This list, X_Lamda, contains the adjusted path, the new control point locations. */
      Lda = Find_Lamda(opt, ControlCurve, MaxStep, Perturb_Vec, Del_S, X_Lamda);
      fprintf(SUMA_STDERR, "Lamda = %f\n", Lda);

      fprintf(SUMA_STDERR, "Show X_Lamda:\n");
      SUMA_ShowMxVec(X_Lamda, -1, NULL); 
      fprintf(SUMA_STDERR, "\n%s: Finished Creating Control Curve.\n", FuncName);

      /* Add new path to sphere or segment file to be viewed in SUMA. */
      dp = NULL;
      for(i=0; i<opt->N_ctrl_points; ++i) {
         for (m=0; m<opt->M_time_steps+1; ++m) {
            dp = mxvdp3(X_Lamda, 0, i, m);
            if(i==0) fprintf (PlotPath, "%f   %f    %f     0.95   0.231  0.607  1.0  0.01  2\n", dp[0], dp[1], dp[2]);
            if(i==1) fprintf (PlotPath, "%f   %f    %f     0.0285 0.633  0.331  1.0  0.01  2\n", dp[0], dp[1], dp[2]);
            if(i==2) fprintf (PlotPath, "%f   %f    %f     0.363  0.998  0.999  1.0  0.01  2\n", dp[0], dp[1], dp[2]);
            if(i==3) fprintf (PlotPath, "%f   %f    %f     0.923  0.405  0.263  1.0  0.01  2\n", dp[0], dp[1], dp[2]);
            dp = NULL;
         }
         fprintf (PlotPath,"\n");
      }   
      fclose (PlotPath); PlotPath = NULL; 
      
      FILE *PlotPathSeg = NULL;
      sprintf(outfile_PlotPathSeg, "SUMA_PathSeg%d.1D.dset", opt_count); 
      PlotPathSeg = fopen (outfile_PlotPathSeg, "w"); 
      fprintf (PlotPathSeg, "#oriented_segments\n");
      dp = NULL;
      for(i=0; i<opt->N_ctrl_points; ++i) {
         for (m=0; m<opt->M_time_steps+1; ++m) {
            dp = mxvdp3(ControlCurve, 0, i, m);
            dp_new = mxvdp3(X_Lamda, 0, i, m);
            if(i==0) fprintf (PlotPathSeg, "%f   %f    %f    %f   %f    %f  1.0  0.0  0.0  1.0  2 \n", 
                                       dp[0], dp[1], dp[2], dp_new[0], dp_new[1], dp_new[2]);
            if(i==1) fprintf (PlotPathSeg, "%f   %f    %f   %f   %f    %f  0.0  1.0  0.0  1.0  2 \n", 
                                       dp[0], dp[1], dp[2], dp_new[0], dp_new[1], dp_new[2]);
            if(i==2) fprintf (PlotPathSeg, "%f   %f    %f   %f   %f    %f  0.0  0.0  1.0  1.0  2 \n", 
                                       dp[0], dp[1], dp[2], dp_new[0], dp_new[1], dp_new[2]);
            if(i==3) fprintf (PlotPathSeg, "%f   %f    %f   %f   %f    %f  0.63 0.0 0.67  1.0  2 \n", 
                                       dp[0], dp[1], dp[2], dp_new[0], dp_new[1], dp_new[2]);
            dp_new = NULL;
         }
         fprintf (PlotPathSeg,"\n");
      }   
      
      fclose (PlotPathSeg); PlotPathSeg = NULL; 
      
      /* Send original control curve to connected path file to be viewed in SUMA. */
      if(opt_count == 0) {
         FILE *PathConnected = NULL;
         PathConnected = fopen ("SUMA_PathConnected_Control.1D.dset", "w"); 
         fprintf(PathConnected, "#oriented_segments\n");
         for(i=0; i<opt->N_ctrl_points; ++i) {
            for (m=0; m<opt->M_time_steps+1; ++m) {
               if(m<opt->M_time_steps) {
                  dp = mxvdp3(ControlCurve, 0, i, m);
                  dp_new = mxvdp3(ControlCurve, 0, i, m+1);
                  if(i==0) fprintf (PathConnected, "%f   %f    %f    %f   %f    %f  0.95   0.231  0.607  1.0  2 \n", 
                                                dp[0], dp[1], dp[2], dp_new[0], dp_new[1], dp_new[2]);
                  if(i==1) fprintf (PathConnected, "%f   %f    %f   %f   %f    %f  0.0285 0.633  0.331  1.0  2 \n", 
                                                dp[0], dp[1], dp[2], dp_new[0], dp_new[1], dp_new[2]);
                  if(i==2) fprintf (PathConnected, "%f   %f    %f   %f   %f    %f  0.363  0.998  0.999  1.0  2 \n", 
                                                dp[0], dp[1], dp[2], dp_new[0], dp_new[1], dp_new[2]);
                  if(i==3) fprintf (PathConnected, "%f   %f    %f   %f   %f    %f 0.923  0.405  0.263 1.0  2 \n", 
                                                dp[0], dp[1], dp[2], dp_new[0], dp_new[1], dp_new[2]);
               } else {
                  dp = mxvdp3(ControlCurve, 0, i, m);
                  if(i==0) fprintf (PathConnected, "%f   %f    %f    %f   %f    %f  0.95   0.231  0.607  1.0  2 \n", 
                                                dp[0], dp[1], dp[2], dp[0], dp[1], dp[2]);
                  if(i==1) fprintf (PathConnected, "%f   %f    %f   %f   %f    %f  0.0285 0.633  0.331  1.0  2 \n", 
                                                dp[0], dp[1], dp[2], dp[0], dp[1], dp[2]);
                  if(i==2) fprintf (PathConnected, "%f   %f    %f   %f   %f    %f  0.363  0.998  0.999  1.0  2 \n", 
                                                dp[0], dp[1], dp[2], dp[0], dp[1], dp[2]);
                  if(i==3) fprintf (PathConnected, "%f   %f    %f   %f   %f    %f  0.923  0.405  0.263  1.0  2 \n", 
                                                dp[0], dp[1], dp[2], dp[0], dp[1], dp[2]);
               }                            
               dp = NULL; dp_new = NULL;          
            }
         }
         fclose (PathConnected); PathConnected = NULL;
      } 
      
      /* Set X_Lamda as new ControlCurve. When Changing Lda no longer helps, most recent ControlCurve is best path. */
      /* Now repeat optimization.  Keep repeating until path no longer changes.  Stop when best Lda is 0. */
      if( Lda>0.0) {    /* When Lda=0, no longer adjusting path.  Then want Control Curve to stay the same. */
         for(i=0; i<opt->N_ctrl_points; ++i) {
            for (m=0; m<opt->M_time_steps+1; ++m) {
               dp = mxvdp3(ControlCurve, 0, i, m);

               dp[0] = mxvd3(X_Lamda, 0, i, m);
               dp[1] = mxvd3(X_Lamda, 1, i, m);
               dp[2] = mxvd3(X_Lamda, 2, i, m);
               dp = NULL;
            }
         }
      }
     
      /* Send connected control curve to file to be viewed in SUMA.  Draws line between dots to connect the path. */
      FILE *PathConnected = NULL;
      sprintf(outfile_PathConnected, "SUMA_PathConnected_%d.1D.dset", opt_count); 
      PathConnected = fopen (outfile_PathConnected, "w"); 
      fprintf (PathConnected, "#oriented_segments\n");
      dp = NULL;

      for(i=0; i<opt->N_ctrl_points; ++i) {
         for (m=0; m<opt->M_time_steps+1; ++m) {
            if(m<opt->M_time_steps) {
               dp = mxvdp3(ControlCurve, 0, i, m);
               dp_new = mxvdp3(ControlCurve, 0, i, m+1);
               if(i==0) fprintf (PathConnected, "%f   %f    %f    %f   %f    %f  1.0  0.0  0.0  1.0  2 \n", 
                                             dp[0], dp[1], dp[2], dp_new[0], dp_new[1], dp_new[2]);
               if(i==1) fprintf (PathConnected, "%f   %f    %f   %f   %f    %f  0.0  1.0  0.0  1.0  2 \n", 
                                             dp[0], dp[1], dp[2], dp_new[0], dp_new[1], dp_new[2]);
               if(i==2) fprintf (PathConnected, "%f   %f    %f   %f   %f    %f  0.0  0.0  1.0  1.0  2 \n", 
                                             dp[0], dp[1], dp[2], dp_new[0], dp_new[1], dp_new[2]);
               if(i==3) fprintf (PathConnected, "%f   %f    %f   %f   %f    %f  0.63 0.0  0.67  1.0  2 \n", 
                                             dp[0], dp[1], dp[2], dp_new[0], dp_new[1], dp_new[2]);
               
            } else {
               dp = mxvdp3(ControlCurve, 0, i, m);
               if(i==0) fprintf (PathConnected, "%f   %f    %f    %f   %f    %f  1.0  0.0  0.0  1.0  2 \n", 
                                             dp[0], dp[1], dp[2], dp[0], dp[1], dp[2]);
               if(i==1) fprintf (PathConnected, "%f   %f    %f   %f   %f    %f  0.0  1.0  0.0  1.0  2 \n", 
                                             dp[0], dp[1], dp[2], dp[0], dp[1], dp[2]);
               if(i==2) fprintf (PathConnected, "%f   %f    %f   %f   %f    %f  0.0  0.0  1.0  1.0  2 \n", 
                                             dp[0], dp[1], dp[2], dp[0], dp[1], dp[2]);
               if(i==3) fprintf (PathConnected, "%f   %f    %f   %f   %f    %f  0.63 0.0  0.67  1.0  2 \n", 
                                             dp[0], dp[1], dp[2], dp[0], dp[1], dp[2]);
            }                            
            dp = NULL; dp_new = NULL;                          
         }
      }
      fclose (PathConnected); PathConnected = NULL;

      ++opt_count;
   } while ( Lda > 0.0);
   
   fprintf(SUMA_STDERR, "Final Control Curve:\n");
   for(i=0; i<opt->N_ctrl_points; ++i) {
      for (m=0; m<opt->M_time_steps+1; ++m) {
         dp = mxvdp3(ControlCurve, 0, i, m);
         fprintf(SUMA_STDERR, "%f %f %f\n", dp[0], dp[1], dp[2]);
      }
      fprintf(SUMA_STDERR, "\n");
   }
  
/********************************************************/    
   
   if (opt->dbg_flag) { fprintf(stderr,"%s: About to allocate (N_sub = %d).\n", FuncName, opt->N_sub); }
   Ci = (MyCircle *)SUMA_malloc(sizeof (MyCircle)); 
   
   /* based on dim, fix N_sub */
   if (opt->dom_dim == 2) {
      Ci->N_Node = opt->N_sub;
   } else {
      opt->N_sub = SUMA_ROUND((sqrt((float)( opt->N_sub - 2 ) / 10.0)));
      Ci->N_Node =  2 + 10 * SUMA_POW2(opt->N_sub);
      fprintf (SUMA_STDERR,"Note %s: Closest number of nodes is %d, Number of Subdivisions is %d\n", 
         FuncName, Ci->N_Node, opt->N_sub);
   }
 
   if (opt->dbg_flag) { fprintf(stderr,"%s: Object contains %d nodes.\n", FuncName, Ci->N_Node); }
   Ci->NodeList = Ci->VelocityField = Ci->Vf_Step = Ci->NewNodeList = Ci->VelocityMagnitude = NULL;
   Ci->Theta = NULL;
   Ci->NodeList = (double *)SUMA_malloc(Ci->N_Node * 3 * sizeof (double));
   Ci->VelocityField = (double *)SUMA_malloc(Ci->N_Node * 3 * sizeof (double));
   Ci->Vf_Step = (double *)SUMA_malloc(Ci->N_Node * 4 * sizeof (double));
   Ci->VelocityMagnitude = (double *)SUMA_malloc(Ci->N_Node * sizeof (double));
   Ci->NewNodeList = (double *)SUMA_malloc(Ci->N_Node * 3 * sizeof (double));
   Ci->Theta = (double *)SUMA_malloc(Ci->N_Node * 3 * sizeof (double));
 
   if(opt->dot == 1) { fprintf( stderr, "USING DOT PRODUCT RESTRICTION.\n"); } 
   
   if (opt->dom_dim == 2) {
      if (opt->dbg_flag) { fprintf(stderr,"%s: Creating circle.\n", FuncName); }

      for (i = 0; i < Ci->N_Node; ++i)
      {  
         i3 = i*3;
         /* circle, sphere should be of unit radius and centered on 0 */
         opt->Radius= 1.0;
         opt->Center[0] = opt->Center[1] = opt->Center[2] = 0.0;

         Ci->Theta[i3  ] = (2.0* SUMA_PI / Ci->N_Node)*i;
         Ci->Theta[i3+1] = SUMA_PI/2;
         Ci->Theta[i3+2] = 0.0;

         Ci->NodeList [i3  ] = opt->Radius*cos(Ci->Theta[i3  ])*sin(Ci->Theta[i3+1]);
         Ci->NodeList [i3+1] = opt->Radius*sin(Ci->Theta[i3  ])*sin(Ci->Theta[i3+1]);
         Ci->NodeList [i3+2] = opt->Radius*cos(Ci->Theta[i3+1]);
      }
   } else {
      float ctr[3];
      
      if (opt->dbg_flag) { fprintf(stderr,"%s: Creating icosahedron with %d subdivisions.\n", FuncName, opt->N_sub); }
      opt->Radius= 1.0;
      opt->Center[0] = opt->Center[1] = opt->Center[2] = 0.0;
      ctr[0] = (float)opt->Center[0];
      ctr[1] = (float)opt->Center[1];
      ctr[2] = (float)opt->Center[2];
      
      SO = SUMA_CreateIcosahedron(opt->Radius, opt->N_sub, ctr, "n", 1);
      for (i = 0; i < 3*Ci->N_Node; ++i) {
         Ci->Theta[i] = 0.0;
         Ci->NodeList[i] = SO->NodeList[i];
      }
      SO_name = SUMA_Prefix2SurfaceName ("toy_ico", NULL, NULL, SUMA_VEC, &exists);
      if (!SO->State) {SO->State = SUMA_copy_string("Julia"); }
      if (!SO->Group) {SO->Group = SUMA_copy_string("Julia"); }
      if (!SO->Label) {SO->Label = SUMA_copy_string("toy_ico"); }
      /* make the idcode_str depend on the Label, it is convenient to send the same surface all the time to SUMA */
      if (SO->Label) { if (SO->idcode_str) SUMA_free(SO->idcode_str); SO->idcode_str = NULL; SUMA_NEW_ID(SO->idcode_str, SO->Label); }
   
      SUMA_Save_Surface_Object(SO_name, SO, SUMA_VEC, SUMA_ASCII, NULL);   /*Creates .coord and .topo files for SUMA */
      
      /* see if SUMA talk is turned on */
      if (ps->cs->talk_suma) {
         ps->cs->istream = SUMA_BRAINWRAP_LINE;
         ps->cs->afni_istream = SUMA_AFNI_STREAM_INDEX2;
         ps->cs->kth = 1; /* make sure all surfaces get sent */
         if (!SUMA_SendToSuma (NULL, ps->cs, NULL, SUMA_NO_DSET_TYPE, 0) || !SO) {
            SUMA_SL_Err("Failed to initialize SUMA_SendToSuma");
            ps->cs->Send = NOPE;
            ps->cs->afni_Send = NOPE;
            ps->cs->talk_suma = NOPE;
         } else {
            SUMA_LH("Sending Ico");
            SUMA_SendSumaNewSurface(SO, ps ->cs);
         }
         if (opt->pause) {
            SUMA_PAUSE_PROMPT("Initial surface in your face\nDo something to proceed.\n");
         }
      }
   }
   
   if (opt->dbg_flag || LocalHead) {  
      FILE *cout=fopen("circleiXYZ.1D","w");
      if (cout) {
         for (i = 0; i < Ci->N_Node; ++i) {
            i3 = i*3;
            fprintf(cout,"%d   %.5f   %.5f   %.5f\n", i, Ci->NodeList [i3], Ci->NodeList [i3+1], Ci->NodeList [i3+2]);
         }
         fclose (cout);
      } else {
         SUMA_S_Err("Failed to open file for debug writing");
      }
   }

   /* To start, Nodelist comes from Icosahedron or circle creation above. */
   /* Initialize NewNodelist and Velocity Field. */
   for (i = 0; i < 3*Ci->N_Node; ++i) {
      Ci->NewNodeList[i] = 0.0;
      Ci->NewNodeList[i] = Ci->NodeList[i]; 
   }

   /* For plotting energy over time.  Energy over the whole move, initial to final. */
   FILE *energy_graph = NULL;
   energy_graph = fopen("plot_energy.1D", "w");

   for(m=0; m<opt->M_time_steps; ++m) {
      first_bad_niter = 0; 
      fprintf(SUMA_STDERR, "\n%s:Setting up for move. m = %d\n", FuncName, m);

      for (i = 0; i < 3*Ci->N_Node; ++i) Ci->VelocityField[i] = 0.0;  
      
      if(Lda > 0.0 && opt->M_time_steps > 1) {
         for(i=0; i<opt->N_ctrl_points; ++i) {
            i3 = 3*i;
            dp_mi = mxvdp3(X_Lamda, 0, i, m);    /* initial */
            dp_mf = mxvdp3(X_Lamda, 0, i, m+1);  /* final */

            opt->CtrlPts_i[i3  ] = dp_mi[0];
            opt->CtrlPts_i[i3+1] = dp_mi[1];
            opt->CtrlPts_i[i3+2] = dp_mi[2];

            opt->CtrlPts_f[i3  ] = dp_mf[0];
            opt->CtrlPts_f[i3+1] = dp_mf[1];
            opt->CtrlPts_f[i3+2] = dp_mf[2];

            dp_mi = NULL; dp_mf = NULL;
         }        
      } 
      if(Lda == 0.0 || opt->M_time_steps < 2) {  /* When Lda = 0, use original path. Use ControlCurve. */
         for(i=0; i<opt->N_ctrl_points; ++i) {
            i3 = 3*i;
            dp_mi = mxvdp3(ControlCurve, 0, i, m);    /* initial */
            dp_mf = mxvdp3(ControlCurve, 0, i, m+1);  /* final */

            opt->CtrlPts_i[i3  ] = dp_mi[0];
            opt->CtrlPts_i[i3+1] = dp_mi[1];
            opt->CtrlPts_i[i3+2] = dp_mi[2];

            opt->CtrlPts_f[i3  ] = dp_mf[0];
            opt->CtrlPts_f[i3+1] = dp_mf[1];
            opt->CtrlPts_f[i3+2] = dp_mf[2];

            dp_mi = NULL; dp_mf = NULL;
         }        
      }
      
      fprintf(SUMA_STDERR, "Control Points! m=%d to m=%d\n", m, m+1);
      for(i=0; i<opt->N_ctrl_points; ++i) {
         i3 = 3*i;
         fprintf(SUMA_STDERR, "[%f   %f    %f]  [%f   %f    %f] \n", 
                              opt->CtrlPts_i[i3  ], opt->CtrlPts_i[i3+1], opt->CtrlPts_i[i3+2], 
                              opt->CtrlPts_f[i3  ], opt->CtrlPts_f[i3+1], opt->CtrlPts_f[i3+2]);  
      }  
     
      /* Initialize renewable initial control points (for error computations). */  
      for (i = 0; i < 3*opt->N_ctrl_points; ++i) opt->CtrlPts[i] = opt->CtrlPts_i[i]; 

      /* Calculate spline weights to fit velocity field. */   
      if (!FindSplineWeights (Ci, opt)) {
         SUMA_S_Err("Failed in FindSplineWeights");
         exit(1);
      }

      /* Calculate initial velocity field.*/
      if (LocalHead) { fprintf(stderr,"%s: Calculating initial velocity field.\n", FuncName); }
      if (!Velocity(Ci, opt)) {
         SUMA_S_Err("Failed while calculating velocity field");
         exit(1);
      }

      if( LocalHead ) {
         fprintf(SUMA_STDERR, "%s: Calculated Initial Velocity Field at Control Points:\n"
                              "V = [ \n", FuncName); 
         for(i=0; i < opt->N_ctrl_points; ++i){ 
            fprintf(SUMA_STDERR, "  %.20f;   %.20f;    %.20f;\n",
                                 Ci->VelocityField[3*opt->CtrlPts_iim[i]  ],
                                 Ci->VelocityField[3*opt->CtrlPts_iim[i]+1],
                                 Ci->VelocityField[3*opt->CtrlPts_iim[i]+2]);}  
         fprintf(SUMA_STDERR, "];\n"); 
      } 

      /* loop until time is 1 */
      if (LocalHead) {
         fprintf(SUMA_STDERR,"%s: About to enter main loop:\n"
                             "adjust shorty     = %d\n", 
                             FuncName, 
                             opt->adjust );
      } 

      if (opt->dbg_flag) {
         if (opt->adjust == 0) fprintf(stderr,"%s: Moving the points, no adjustment. (N_Node = %d)\n", FuncName, Ci->N_Node); 
         else fprintf(stderr,"%s: Moving the points, adjusted. (N_Node = %d)\n", FuncName, Ci->N_Node); 
      }


      niter = 0;
      a_niter = 0; 
      do { 

         if(opt->neighb_adjust) {
            if(a_niter) { dt = 1.0/(adj_factor*(opt->N_step - niter) - a_niter); }
            else { dt = 1.0/(opt->N_step - niter); } 
         }else { dt = 1.0/(opt->N_step - niter); }

         if(LocalHead) { Debug_Move (Ci, opt, SO, dt, niter, a_niter, first_bad_niter);}

         /* To create graphs like those at the end of the paper of energy of velocity over time, need to 
               calculate the sum of the dot product of the velocity and the weights at each control point
               and store this value to file along side a column with the time. */
         /* WILL NEED TO ADD TO THIS SO WORKS WITH NEIGHBOR ADJUSTMENT.  NEEDS TO ALLOW FOR A_NITER.*/
         energy_sum = 0.0;
         for(i=0; i < opt->N_ctrl_points; ++i) {
            j = opt->CtrlPts_iim[i];
            j3 = 3*j;
            i3 = 3*i;
            energy = 0.0;
            energy = SUMA_MT_DOT( (&(Ci->VelocityField[j3])), (&(Ci->Wv.elts[i3])) );
            energy_sum += energy;  
         }
         time_elapsed = (niter); 
         time_elapsed = time_elapsed/opt->N_step;
         fprintf(energy_graph, "%11.8f  %11.8f\n", time_elapsed, energy_sum);

         /* Calculate step size and make adjustment. */
         Calculate_Step (Ci, opt, dt);

         /* Check to see if Neighbors are too close. */
         if(opt->neighb_check || opt->neighb_adjust) {
            if(!a_niter){
               too_close = 0.0;
               too_close = Neighbor(Ci, opt, SO, niter, a_niter);
               if(!need_neighb_adjust) need_neighb_adjust = too_close;
            }
         }  

         /* Adjust neighbors if they are too close. */
         if(opt->neighb_adjust && !a_niter){ 
            if(too_close){    /* Since nearest neighbor is too close for this step size, 
                                 adjust dt and recalculate stepping velocity. */
                              /* Only allowing for dt to be shrunk once. If using a_niter to index, 
                                 don't let dt get any smaller. */
               dt = 1.0/( adj_factor*(opt->N_step - niter) - a_niter );
               fprintf(SUMA_STDERR, "%s: dt_adjusted = %f; niter = %d, a_niter = %d\n", FuncName, dt, niter, a_niter);

               /* Calculate step size and make adjustment. */
               Calculate_Step (Ci, opt, dt);

               /* Recheck neighbor distances to see if adjustment was enough.  
                  Find one time when adjustment not enough, then stop checking. */
               if(!need_more_adjustment) need_more_adjustment = Neighbor(Ci, opt, SO, niter, a_niter);

               if(LocalHead) {   
                  i = opt->CtrlPts_iim[0]; 
                  s4 = i*4;
                  fprintf(SUMA_STDERR, "%s:First control point:\n"
                                       "  New Vf*dt = [%f; %f; %f], stepsize = %f\n", 
                                       FuncName, Ci->Vf_Step[s4  ], Ci->Vf_Step[s4+1], Ci->Vf_Step[s4+2], Ci->Vf_Step[s4+3]);
               }
            }
         }

         /* ONCE STEP SIZE IS SMALLER THAN DISTANCE TO NEAREST NODE, CAN CONTINUE WITH MOVE. */
         if (LocalHead) {
            i = opt->CtrlPts_iim[0];  /* Debug when node is 1st control point. */
            i3 = i*3;
            s4 = i*4;
            fprintf(SUMA_STDERR, "********************************************\n");
            if(opt->neighb_adjust){ fprintf(SUMA_STDERR,"Iter %d: (a_niter %d) debug for node %d\n", niter, a_niter, i); }
            else{ fprintf(SUMA_STDERR,"Iter %d: debug for node %d\n", niter, i); }
            if(too_close){ fprintf(SUMA_STDERR, "Consider adjusting dt because step size larger than distance to neighbor.\n"); }
            fprintf(SUMA_STDERR, "dt = %f\n"
                                 "u        = [%.8f %.8f %.8f], um = %.8f\n"
                                 "udotrad  = [%.18f]\n" 
                                 "old[XYZ] = [%.8f %.8f %.8f]\n", 
                                 dt, 
                                 Ci->Vf_Step[s4  ], Ci->Vf_Step[s4+1], Ci->Vf_Step[s4+2], Ci->Vf_Step[s4+3],
                                 SUMA_MT_DOT( (&(Ci->Vf_Step[s4  ])),(&(Ci->NewNodeList[i3  ]))),
                                 Ci->NewNodeList[i3  ], Ci->NewNodeList[i3+1], Ci->NewNodeList[i3+2]);
            oxyz[0] = Ci->NewNodeList[i3  ];
            oxyz[1] = Ci->NewNodeList[i3+1];
            oxyz[2] = Ci->NewNodeList[i3+2];
         }

         /* Move the points! */
         Move_Points(Ci, opt);

         /* Recalculate surface normals. */
         if (ps->cs->talk_suma) {
            for (i3=0; i3<3*SO->N_Node; ++i3) SO->NodeList[i3] = Ci->NewNodeList[i3];
            SUMA_RECOMPUTE_NORMALS(SO); 
            if (ps->cs->Send) {
               if (!SUMA_SendToSuma (SO, ps->cs, (void *)SO->NodeList, SUMA_NODE_XYZ, 1)) {
                  SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
               }
            }
         }

#if 0 
         /* Check Sphere Quality.  Since want to check at the end of each iteration, need to start at niter = 1. */
         if(!first_bad_niter || niter < first_bad_niter+5){ 
            
            fprintf(SUMA_STDERR, "\n%s: NITER = %d\n", FuncName, niter);
            sprintf(outfile_SphereQuality, "SphereQuality_m%d_n%d", m, niter);
             
            SO_name = SUMA_Prefix2SurfaceName (outfile_SphereQuality, NULL, NULL, SUMA_VEC, &exists);
            for (i3=0; i3<3*SO->N_Node; ++i3) SO->NodeList[i3] = Ci->NewNodeList[i3];
            SUMA_RECOMPUTE_NORMALS(SO);
            
            shist = SUMA_HistString (FuncName, argc, argv, NULL);
            nbad = SUMA_SphereQuality(SO, outfile_SphereQuality , shist);
            if(nbad && !first_bad_niter){ first_bad_niter = niter; }
            if (shist) SUMA_free(shist); shist = NULL;
            SUMA_Save_Surface_Object(SO_name, SO, SUMA_VEC, SUMA_ASCII, NULL);
         }
#endif
 
         
         /* Call spline weight function to recalculate spline weights for renew_weights option. */
         if (opt->renew_weights) { 
            if(LocalHead) {
               fprintf( SUMA_STDERR, "%s: RENEW_WEIGHTS OPTION -- Check renewed control points: \n", FuncName ); }
            for(i=0; i < opt->N_ctrl_points; ++i) {
               i3 = 3*i;
               opt->CtrlPts[i3  ] = Ci->NewNodeList[3*(opt->CtrlPts_iim[i])  ];
               opt->CtrlPts[i3+1] = Ci->NewNodeList[3*(opt->CtrlPts_iim[i])+1];
               opt->CtrlPts[i3+2] = Ci->NewNodeList[3*(opt->CtrlPts_iim[i])+2];

               Ci->Wv.elts[i3  ] = 0.0; 
               Ci->Wv.elts[i3+1] = 0.0; 
               Ci->Wv.elts[i3+2] = 0.0; 
            if(opt->dbg_flag) {
               fprintf( SUMA_STDERR, "%s: Control Point(%d) = [%11.8f;  %11.8f;   %11.8f] \n", FuncName, i, 
                                       opt->CtrlPts[i3  ], opt->CtrlPts[i3+1], opt->CtrlPts[i3+2] ); }
            }
            FindSplineWeights(Ci, opt); 
         } 

         /* Reset Velocity Field. */
         for (i = 0; i < 3*Ci->N_Node; ++i) Ci->VelocityField[i] = 0.0;  
         
         /* Recalculating velocity field for next step.  Same m here. */
         Velocity(Ci, opt); 

         if(LocalHead) {
            fprintf(SUMA_STDERR, "%s: VelocityField at Control Points(m=%d): \n", FuncName, m);
            for(j=0; j < opt->N_ctrl_points; ++j){ 
               i = opt->CtrlPts_iim[j];
               i3 = i*3;
               fprintf(SUMA_STDERR, "Vf(%d) = [%f   %f    %f]\n"
                                    "  Magnitude = %f \n"
                                    "  Dot_v = %.12f \n",
                                    i, Ci->VelocityField[i3  ], Ci->VelocityField[i3+1], Ci->VelocityField[i3+2], 
                                    sqrt( Ci->VelocityField[i3  ]*Ci->VelocityField[i3  ] + 
                                          Ci->VelocityField[i3+1]*Ci->VelocityField[i3+1] +
                                          Ci->VelocityField[i3+2]*Ci->VelocityField[i3+2] ), 
                                    SUMA_MT_DOT( (&(Ci->VelocityField[i3])), (&(Ci->NewNodeList[i3])) ) ); }  
         }

         if(opt->neighb_adjust && too_close && a_niter < adj_factor-1) ++a_niter; 
         else { ++niter; a_niter = 0; }
      }  while( niter < opt->N_step ); 
      
      
      fprintf(SUMA_STDERR, "******End Do-Loop here.\n");
      
      /* Check Sphere Quality after each m time interval. */    
      fprintf(SUMA_STDERR, "\n%s: Check Sphere Quality: m=%d to m=%d\n", FuncName, m, m+1);
      sprintf(outfile_SphereQuality, "SphereQuality_m%d", m);
        
      SO_name = SUMA_Prefix2SurfaceName (outfile_SphereQuality, NULL, NULL, SUMA_VEC, &exists);
      for (i3=0; i3<3*SO->N_Node; ++i3) SO->NodeList[i3] = Ci->NewNodeList[i3];
      SUMA_RECOMPUTE_NORMALS(SO);
      shist = SUMA_HistString (FuncName, argc, argv, NULL);
      nbad = SUMA_SphereQuality(SO, outfile_SphereQuality , shist);
      if(nbad && !first_bad_niter){ first_bad_niter = niter; }
      if (shist) SUMA_free(shist); shist = NULL;
      SUMA_Save_Surface_Object(SO_name, SO, SUMA_VEC, SUMA_ASCII, NULL);
   }

 

   fclose(energy_graph); energy_graph = NULL;
    
   for (i=0; i<opt->N_ctrl_points; ++i) {
      i3 = 3 * i;
      fprintf(SUMA_STDERR,"%s: Angular error reports where control points coincide with nodes:\n"
                          "Niter = %d\n"
                          "#Col. 0: Ctrl_Node\n"
                          "#Col. 1: Original Desired Angle \n"
                          "#Col. 2: Final Achieved Angle  \n"
                          "#Col. 3: Error (ODA-FAA) in rad.\n"
                          "#Col. 4: Error (ODA-FAA) in deg.\n", FuncName, niter);
      if (opt->CtrlPts_iim[i] >= 0) {
         SUMA_ANGLE_DIST_NC( (&(opt->CtrlPts_f[i3])), (&(opt->CtrlPts_i[i3])), oda, nrmi); /* original desired angle */ 
         SUMA_ANGLE_DIST_NC( (&(Ci->NewNodeList[3*opt->CtrlPts_iim[i]])), (&(opt->CtrlPts_i[i3])), faa, nrmf); /* final achieved angle */ 
         error = abs(oda - faa);
         fprintf(SUMA_STDERR,"%d   %.5f   %.5f   %.15f   %.15f\n", opt->CtrlPts_iim[i], oda, faa, error, SUMA_R2D(error));
         if(need_neighb_adjust && opt->neighb_check){ 
            fprintf(SUMA_STDERR,"TOO CLOSE!! Consider adjusting dt because step size larger than distance to neighbor.\n"
                                "First time neighbors too close, Niter = %d.\n", need_neighb_adjust); }
         if(need_more_adjustment && opt->neighb_adjust){
            fprintf(SUMA_STDERR,"TOO CLOSE!! Need greater dt adjustment. Distance to neighbor still smaller than stepsize.\n"
                                 "First time neighbors too close, Niter = %d.\n", need_more_adjustment); }
         /*fprintf(SUMA_STDERR,  "  ERROR REPORTS:\n"
                                 "     original desired axis of rotation = %f    %f    %f \n"
                                 "     final achieved axis of rotation =   %f    %f    %f \n", 
                                 nrmi[0], nrmi[1], nrmi[2], nrmf[0], nrmf[1], nrmf[2]); */
         fprintf(SUMA_STDERR, "Number of times optimization run = %d\n", opt_count);
      }
   }
   
   /* Compute the distance between the nodes on the surface and the center of the sphere. */
   if(LocalHead) {
      FILE *sphere_radius = NULL;
      sphere_radius = fopen ("sphere_radius.1D", "w");
      for(i = 0; i < Ci->N_Node; ++i) {
         i3 = 3*i;
         fprintf (sphere_radius, "%d   %11.8f\n", i, sqrt( SUMA_POW2 (Ci->NewNodeList[i3  ]) +
                                                           SUMA_POW2 (Ci->NewNodeList[i3+1]) + 
                                                           SUMA_POW2 (Ci->NewNodeList[i3+2]) ) );
      }  
      fclose (sphere_radius); sphere_radius = NULL;
   }
 
   /* you don't want to exit rapidly because the SUMA might not be done processing the last elements*/
   if (ps->cs->Send && !ps->cs->GoneBad) {
      /* cleanup and close connections */
      if (!SUMA_SendToSuma (SO, ps->cs, NULL, SUMA_NODE_XYZ, 2)) {
         SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCleanup failed");
      }
   }   

   /* Write final surface to file, so can view in SUMA without taking the time of the -talk option. */
   SO_name = SUMA_Prefix2SurfaceName (opt->outfile, NULL, NULL, SUMA_VEC, &exists); 
   for (i3=0; i3<3*SO->N_Node; ++i3) SO->NodeList[i3] = Ci->NewNodeList[i3];
   SUMA_RECOMPUTE_NORMALS(SO);
   shist = SUMA_HistString (NULL, argc, argv, NULL);
   fprintf( SUMA_STDERR, "\nNITER = %d", niter );
   nbad = SUMA_SphereQuality(SO, opt->outfile , shist);   
   if (nbad) {
      fprintf(SUMA_STDERR,"Shist %s!:\n you have %d bad points!\n", FuncName, nbad);
   } else {
      fprintf(SUMA_STDERR,"%s: Happy pretend Valentine!\n"
                          "   You have no errors when checking node normals!\n" , FuncName);
   }
   if (shist) SUMA_free(shist); shist = NULL;
   
   if(first_bad_niter) { fprintf(SUMA_STDERR, "First iteration with facet or node flip = %d\n", first_bad_niter); }
   
   SUMA_Save_Surface_Object(SO_name, SO, SUMA_VEC, SUMA_ASCII, NULL);

   /*Cleanup*/
   
   ControlCurve = SUMA_FreeMxVec( ControlCurve );
   MaxStep = SUMA_FreeMxVec( MaxStep );
   Perturb_Vec = SUMA_FreeMxVec( Perturb_Vec );
   Del_S = SUMA_FreeMxVec( Del_S );
   X_Lamda = SUMA_FreeMxVec( X_Lamda );
   vector_destroy(&(Ci->Wv));
   
   if (SO) SUMA_Free_Surface_Object(SO); SO = NULL;
   if (Ci->NodeList) SUMA_free(Ci->NodeList); Ci ->NodeList = NULL;
   if (Ci->VelocityField) SUMA_free(Ci->VelocityField); Ci ->VelocityField = NULL;
   if (Ci->VelocityMagnitude) SUMA_free(Ci->VelocityMagnitude); Ci ->VelocityMagnitude = NULL;
   if (Ci->NewNodeList) SUMA_free(Ci->NewNodeList); Ci ->NewNodeList = NULL; 
   if (Ci->Theta) SUMA_free(Ci->Theta); Ci ->Theta = NULL; 
   if (Ci) SUMA_free(Ci); Ci = NULL;
   if (Opt->debug > 2) LocalHead = YUP;   /* What is this? */
   if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   if (Opt) Opt = SUMA_Free_Generic_Prog_Options_Struct(Opt);
   if (opt->CtrlPts_iim) SUMA_free(opt->CtrlPts_iim); opt->CtrlPts_iim = NULL;
   if (opt->CtrlPts) SUMA_free(opt->CtrlPts); opt->CtrlPts = NULL;
   if (opt->CtrlPts_i) SUMA_free(opt->CtrlPts_i); opt->CtrlPts_i = NULL;
   if (opt->CtrlPts_f) SUMA_free(opt->CtrlPts_f); opt->CtrlPts_f = NULL;
   if (opt->Dtheta) SUMA_free(opt->Dtheta); opt->Dtheta = NULL;
   if (opt->Nrm) SUMA_free(opt->Nrm); opt->Nrm = NULL;
   if (opt->ctrl) opt->ctrl = NULL; /* pointer from argv, do not free */
   if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   exit(0);
}
