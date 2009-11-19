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
               "  path_optimize [-ctrl CTRL_FILE] [-dom_dim DOM_DIM] [-dim DIM] [-N_sub N_SUB] [-N_step N_STEP]\n"
               "             [-renew_weights] [-adjust] [-dot] [-neighb_adjust] [-neighb_check]                \n"
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
               "  -sin_kern         Choose to use the expansion factor that includes the sine term.            \n"
               "                    Default is without the sine component of the kernel,  but the default      \n"
               "                       doesn't work. This option is manditory for successful deformations.     \n"
               "  -dot:             Choose to use a dot product requirment when calculated the weights         \n"
               "                    to ensure tangency.                                                        \n"
               "  -neighb_check:    Will check at each iteration if the distance to each node's nearest        \n"
               "                    neighbor is smaller than the proposed step size.  Will print results       \n"
               "                    to file, but will not make any step size adjustment.                       \n"
               "  -neighb_adjust:   Checks neighbor distances and adjusts step size accordingly.               \n"
               "  -talk_pause:      For use with SUMA and -talk_suma option.  Will pause program after         \n"
               "                    intial conditions are setting, allowing user to adjust viewing window      \n"
               "                    before completing the rest of the iterations.                              \n"
               "                    Use -talk_pause -talk_pause (yes, twice) to a pause at each iteration      \n"
               "  -debug DEBUG:     Choose to turn debugging on.  Default is no debugging.                     \n"                        
               "  -output OUTPUT:   Name output file. Default OUTPUT is test_move.                             \n"
               "  -M_time M_TIME:   Choose number of time steps used in optimization.                          \n"
               "                    If set M_time = 1, then no optimization performed.                         \n"
               "                                                                                               \n"
               "  -read_path PATH:  Read precomputed path results from files                                   \n"
               "                    PATH_X_Lamda.1D,  PATH_ControlCurve.1D, and PATH_Misc.1D                   \n"
               "                    These files would have been created on a previous run and written out      \n"
               "                    for debugging purposes. This option is for developers. Use carefully.      \n"
               "\n" 
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
   popt->M_time_steps = 10;
   popt->sin_kern = 0;
   popt->Zero = 0.0000000000001;     /* Need global variable for zero. */
   popt->read_path[0] = '\0';
   popt->SO = NULL;
   popt->ControlCurve = NULL;
   popt->X_Lamda = NULL;
   popt->Lda = -1.0;
   popt->iter_count = -1;
   popt->psepsilon = 1e-18;
   
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
         kar ++;
         if (kar >= argc)  
         {
            fprintf (stderr, "need argument after -debug \n");
            exit (1);
         }
         popt->dbg_flag = atoi(argv[kar]); 
         brk = 1;
      }
      if (!brk && (strcmp(argv[kar], "-talk_pause") == 0))
      {
         ++popt->pause; 
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
      if (!brk && (strcmp(argv[kar], "-pseps") == 0)) {
         kar ++;
         if (kar >= argc)  
         {
            fprintf (stderr, "need argument after -pseps \n");
            exit (1);
         }
         popt->psepsilon = (double)atof(argv[kar]);               
         brk = 1;
      }
      if (!brk && (strcmp(argv[kar], "-sin_kern") == 0))
      {
         popt->sin_kern = 1; 
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

      if (!brk && (strcmp(argv[kar], "-read_path") == 0)) {
         kar ++;
         if (kar >= argc)  
         {
           fprintf (stderr, "need argument after -read_path \n");
           exit (1);
         }
         snprintf(popt->read_path, 400*sizeof(char),"%s", argv[kar]);
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

SUMA_SegmentDO *SUMA_ControlCurve2SDO(SUMA_MX_VEC *ControlCurve, char *Label)
{
   static char FuncName[]={"SUMA_ControlCurve2SDO"};
   SUMA_SegmentDO *SDO=NULL;
   int N_n,  oriented,  NodeBased,  Stipple, i, m, d, kcc;
   char *idcode_str=NULL,  *Parent_idcode_str=NULL;
   float LineWidth;
   double *dp=NULL, *dp_new=NULL;
   int *NodeId=NULL;
   float *n0=NULL,  *n1=NULL;
   float *colv=NULL, *thickv=NULL;
   float acol[4], LineCol[4] = { 1.000000, 0.300000, 1.000000, 1.000000 };

   SUMA_ENTRY;

   oriented = 1;
   Stipple = 0;
   NodeBased = 0;
   LineWidth = 4;
   SUMA_NEW_ID(idcode_str, Label);

   N_n = ControlCurve->dims[1] * (ControlCurve->dims[2]);
   n0 = (float *) SUMA_malloc(sizeof(float)*N_n*3);
   n1 = (float *) SUMA_malloc(sizeof(float)*N_n*3);
   colv = (float *) SUMA_malloc(sizeof(float)*N_n*4);
   thickv = (float *) SUMA_malloc(sizeof(float)*N_n);

   kcc = 0;
   for(i=0; i<ControlCurve->dims[1]; ++i) {
      SUMA_a_good_col("ROI_256", i+2, acol);/* get a decent colormap */  
                  /* Add 2 to the color map index to avoid getting purple. */
      for (m=0; m<ControlCurve->dims[2]; ++m) {
         if(m<ControlCurve->dims[2]-1) {
            dp = mxvdp3(ControlCurve, 0, i, m);
            dp_new = mxvdp3(ControlCurve, 0, i, m+1);
            for (d=0;d<3;++d) {
               n0[3*kcc+d] = dp[d];
               n1[3*kcc+d] = dp_new[d];
            }
         } else {
            dp = mxvdp3(ControlCurve, 0, i, m);
            for (d=0;d<3;++d) {
               n0[3*kcc+d] = dp[d];
               n1[3*kcc+d] = dp[d];
            }                                     
         }                            
         for (d=0;d<4;++d)   colv[4*kcc+d] = acol[d];
         thickv[kcc] = 3;   
         ++kcc;
         dp = NULL; dp_new = NULL;          
      }
   }

   SDO = SUMA_CreateSegmentDO( N_n, oriented, NodeBased, Stipple,
                               Label, idcode_str, Parent_idcode_str,
                               LineWidth, LineCol,
                               NodeId, NULL, n0, n1,
                               colv, thickv 
                              );

   if (idcode_str) SUMA_free(idcode_str); idcode_str = NULL;
   if (n0) SUMA_free(n0); n0 = NULL;
   if (n1) SUMA_free(n1); n1 = NULL;
   if (colv) SUMA_free(colv); colv = NULL;
   if (thickv) SUMA_free(thickv); thickv = NULL;

   SUMA_RETURN(SDO);
}

int SUMA_Optimize_Path(MyCircleOpt *opt, SUMA_GENERIC_ARGV_PARSE *ps)
{ 
   static char FuncName[]={"SUMA_Optimize_Path"};    
   int N_dims, dims[10], M_N_dims, M_dims[10], Perturb_N_dims, Perturb_dims[12], S_N_dims, S_dims[20], XL_N_dims, XL_dims[10]; 
   double *dp = NULL, *dp_new = NULL;
   int i, m, p, k;
   char stmp[600], outfile_PlotPathSeg[50], outfile_PathConnected[50], outfile_TempCC[50];
   char outfile_Condition[50], outfile_Condition_Only[50];
   FILE *PathConnected = NULL;
   FILE *PlotPathSeg = NULL;
   FILE *TempCC = NULL;  /* Store most recent control curve locations for debugging purposes. */
   SUMA_MX_VEC *Del_S = NULL, *mo = NULL;
   SUMA_MX_VEC *MaxStep = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (opt->dbg_flag > 2) LocalHead = YUP;
     
   opt->iter_count = -1;
   
   /* Set up file for storing condition numbers, matrices, and eigen values. */
   FILE *condition_num = NULL;
   sprintf(outfile_Condition, "Kern_ConditionEigen.1D");
   condition_num = fopen (outfile_Condition, "w"); 
   
    /* Set up file for storing condition numbers. */
   FILE *condition_num_only = NULL;
   sprintf(outfile_Condition_Only, "Kern_Condition_Num.1D");
   condition_num_only = fopen (outfile_Condition_Only, "w"); 
   
   
   N_dims = 3;
   dims[0] = 3;
   dims[1] = opt->N_ctrl_points;
   dims[2] = opt->M_time_steps + 1;
   
   opt->ControlCurve = SUMA_NewMxVec( SUMA_double, N_dims, dims, 1 );

   /* Store max allowabe movement of the ith point at the mth time step. Need this later for limiting lamda. */
   M_N_dims = 2;
   M_dims[0] = opt->N_ctrl_points;
   M_dims[1] = (opt->M_time_steps + 1);
   MaxStep = SUMA_NewMxVec( SUMA_double, M_N_dims, M_dims, 1 );

   /* Stores perturbation vectors with theta included. */
   /* Need to store a zero perturbation vector for the end points.  
         Include this vector for ease of loop calculations later. */
   Perturb_N_dims = 4; 
   Perturb_dims[0] = 3;
   Perturb_dims[1] = opt->N_ctrl_points; 
   Perturb_dims[2] = (opt->M_time_steps+1);
   Perturb_dims[3] = 2;
   SUMA_MX_VEC *Perturb_Vec = NULL;
   Perturb_Vec = SUMA_NewMxVec( SUMA_double, Perturb_N_dims, Perturb_dims, 1 );

   S_N_dims = 4;
   S_dims[0] = opt->N_ctrl_points; /* control point */
   S_dims[1] = opt->M_time_steps;   /* time step */
   S_dims[2] = 2; /* q */
   S_dims[3] = 2; /* p */   
       /* q for which point perturbed, p for type of perturbation. */
   Del_S = SUMA_NewMxVec( SUMA_double, S_N_dims, S_dims, 1 );

   /* Stores new path. */
   XL_N_dims = 3;
   XL_dims[0] = 3;
   XL_dims[1] = opt->N_ctrl_points;
   XL_dims[2] = (opt->M_time_steps+1);
   opt->X_Lamda = SUMA_NewMxVec( SUMA_double, XL_N_dims, XL_dims, 1 );
   
   TempCC = fopen ("SUMA_TempControlCurve.1D.dset", "w"); 
   
   /* Find the initial control curve.  Calculated using desired arc of travel and number of steps taken. */
   Set_up_Control_Curve(opt, opt->ControlCurve);
   
   SUMA_etime2(FuncName, NULL, NULL);
   opt->iter_count = 0;
   do{
      if (opt->dbg_flag > 2) {
         /* To see the values stored at opt->ControlCurve. */ 
         /* SUMA_ShowMxVec(opt->ControlCurve, -1, NULL, "\ntthhh\n"); */   
         fprintf(SUMA_STDERR, "Control Curve:\n");
         for(i=0; i<opt->N_ctrl_points; ++i) {
            for (m=0; m<opt->M_time_steps+1; ++m) {
               dp = mxvdp3(opt->ControlCurve, 0, i, m);
               fprintf(SUMA_STDERR, "%f %f %f\n", dp[0], dp[1], dp[2]);
            }
            fprintf(SUMA_STDERR, "\n");
         }
      }
   
      /* Store path as a sphere or segment file to be viewed in SUMA. */
      /* See snip2.c */
      if (opt->dbg_flag) {
         sprintf(stmp, "\n\n*** Beginning iteration %d", opt->iter_count);
         SUMA_etime2(FuncName, stmp, "Pre-prerturbations");
      }
      
      Perturbations( opt, opt->ControlCurve, MaxStep, Perturb_Vec, ps );
      if (opt->dbg_flag) {
         sprintf(stmp, "Done with Perturbations");
         SUMA_etime2(FuncName, stmp, "Pre-Change_in_Energy");
      }
      
      if (opt->dbg_flag > 2) {
         fprintf(SUMA_STDERR, "Max Allowable Movement:\n");
         SUMA_ShowMxVec(MaxStep, MaxStep->N_vals, NULL, "\nMaxStep\n");
         fprintf(SUMA_STDERR, "%s: Perturbation Vectors:\n", FuncName);
         SUMA_ShowMxVec(Perturb_Vec, Perturb_Vec->N_vals, NULL, "\nPerturb_vec\n");

         fprintf(SUMA_STDERR, "%s: Perturbation Vectors In Other View:\n", FuncName);
         for(p=0; p<2; ++p) {
            for(k=0; k<(opt->M_time_steps+1); ++k) {
               for(i=0; i<opt->N_ctrl_points; ++i) {
                  dp = NULL;
                  dp = mxvdp4(Perturb_Vec, 0, i, k, p);
                  fprintf(SUMA_STDERR, "%f   %f   %f\n", dp[0], dp[1], dp[2]);
               }
            }
         }
      }

      Change_in_Energy(opt, opt->ControlCurve, Perturb_Vec, Del_S, condition_num, condition_num_only);
      if (opt->dbg_flag) {
         sprintf(stmp, "Done with Change in Energy");
         SUMA_etime2(FuncName, stmp, "Pre-Find_Lamda");
      }

      /* fprintf(SUMA_STDERR, "Show Del_S:\n");
         SUMA_ShowMxVec(Del_S, -1, NULL, "\nDel_S\n"); */

      /* THIS IS IT! This list, opt->X_Lamda, contains the adjusted path, the new control point locations. */
      opt->Lda = Find_Lamda(opt, opt->ControlCurve, MaxStep, Perturb_Vec, Del_S, opt->X_Lamda, ps);
      
      if (opt->dbg_flag) {
         sprintf(stmp, "Finished Creating Control Curve.\nLambda now %lf", opt->Lda);
         SUMA_etime2(FuncName, stmp, "Pre-Path update");
      }
      
      if (opt->dbg_flag > 2) {
         fprintf(SUMA_STDERR, "Show X_Lamda:\n"); 
         SUMA_ShowMxVec(opt->X_Lamda, -1, NULL, "\nX_Lamda\n"); 
      }
      
      /* Add new path to sphere or segment file to be viewed in SUMA. */
      /* see snip3.c */
      if(LocalHead){
         if (opt->dbg_flag) {
            fprintf(SUMA_STDERR, "\n%s: Writing path segment...\n", FuncName);
         }
         sprintf(outfile_PlotPathSeg, "SUMA_PathSeg%d.1D.dset", opt->iter_count); 
         PlotPathSeg = fopen (outfile_PlotPathSeg, "w"); 
         fprintf (PlotPathSeg, "#oriented_segments\n");
         dp = NULL;
         for(i=0; i<opt->N_ctrl_points; ++i) {
            for (m=0; m<opt->M_time_steps+1; ++m) {
               dp = mxvdp3(opt->ControlCurve, 0, i, m);
               dp_new = mxvdp3(opt->X_Lamda, 0, i, m);
               if(i==0) fprintf (PlotPathSeg, "%f   %f    %f    %f   %f    %f  1.0  0.0  0.0  1.0  3 \n", 
                                          dp[0], dp[1], dp[2], dp_new[0], dp_new[1], dp_new[2]);
               if(i==1) fprintf (PlotPathSeg, "%f   %f    %f   %f   %f    %f  0.0  1.0  0.0  1.0  3 \n", 
                                          dp[0], dp[1], dp[2], dp_new[0], dp_new[1], dp_new[2]);
               if(i==2) fprintf (PlotPathSeg, "%f   %f    %f   %f   %f    %f  0.0  0.0  1.0  1.0  2 \n", 
                                          dp[0], dp[1], dp[2], dp_new[0], dp_new[1], dp_new[2]);
               if(i==3) fprintf (PlotPathSeg, "%f   %f    %f   %f   %f    %f  0.63 0.0 0.67  1.0  2 \n", 
                                          dp[0], dp[1], dp[2], dp_new[0], dp_new[1], dp_new[2]);
               if(i==4) fprintf (PlotPathSeg, "%f   %f    %f   %f   %f    %f  0.92 0.74 0.18 1.0  2 \n", 
                                          dp[0], dp[1], dp[2], dp_new[0], dp_new[1], dp_new[2]);
               dp_new = NULL;
            }
            fprintf (PlotPathSeg,"\n");
         }   
         fclose (PlotPathSeg); PlotPathSeg = NULL; 
      }
      
      if (ps->cs->talk_suma) {
         SUMA_SegmentDO *sdo=NULL;
         NI_group *ngr = NULL;
         int suc;
         /* Send  control curve to SUMA */
         sdo = SUMA_ControlCurve2SDO(opt->ControlCurve, "CC");
         /* change that thing to NIML */
         ngr = SUMA_SDO2niSDO(sdo);
         #if 0
         /* write it to disk, for kicks */
         sprintf(stmp, "file:CC_%d.niml.SDO", opt->iter_count);
         NEL_WRITE_TX(ngr, stmp, suc);
         #endif
         /* send it to suma */
         if (!SUMA_SendToSuma (opt->SO, ps->cs, (void *)ngr, SUMA_SEGMENT_OBJECT, 1)) {
            SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
            ps->cs->talk_suma = NOPE;
         }
         SUMA_free_SegmentDO(sdo); sdo = NULL;
         NI_free(ngr); ngr = NULL; 
         if ((!opt->iter_count && opt->pause) || opt->pause > 1) {
            SUMA_PAUSE_PROMPT("Initial surface in your face\nDo something to proceed.\n");
         }

      }
      
      /* Send original control curve to connected path file to be viewed in SUMA. */
      if(opt->iter_count == 0) {
         PathConnected = fopen ("SUMA_PathConnected_Control.1D.dset", "w"); 
         fprintf(PathConnected, "#oriented_segments\n");
         for(i=0; i<opt->N_ctrl_points; ++i) {
            for (m=0; m<opt->M_time_steps+1; ++m) {
               if(m<opt->M_time_steps) {
                  dp = mxvdp3(opt->ControlCurve, 0, i, m);
                  dp_new = mxvdp3(opt->ControlCurve, 0, i, m+1);
                  if(i==0) fprintf (PathConnected, "%f   %f    %f    %f   %f    %f  1.0   0.0   0.0  1.0  3 \n", 
                                                dp[0], dp[1], dp[2], dp_new[0], dp_new[1], dp_new[2]);
                  if(i==1) fprintf (PathConnected, "%f   %f    %f   %f   %f    %f  0.0 1.0   0.0  1.0  3 \n", 
                                                dp[0], dp[1], dp[2], dp_new[0], dp_new[1], dp_new[2]);
                  if(i==2) fprintf (PathConnected, "%f   %f    %f   %f   %f    %f  0.363  0.998  0.999  1.0  2 \n", 
                                                dp[0], dp[1], dp[2], dp_new[0], dp_new[1], dp_new[2]);
                  if(i==3) fprintf (PathConnected, "%f   %f    %f   %f   %f    %f 0.923  0.405  0.263 1.0  2 \n", 
                                                dp[0], dp[1], dp[2], dp_new[0], dp_new[1], dp_new[2]);
                  if(i==4) fprintf (PathConnected, "%f   %f    %f   %f   %f    %f 0.92    0.74  0.18 1.0  2 \n", 
                                                dp[0], dp[1], dp[2], dp_new[0], dp_new[1], dp_new[2]);                              
               } else {
                  dp = mxvdp3(opt->ControlCurve, 0, i, m);
                  if(i==0) fprintf (PathConnected, "%f   %f    %f    %f   %f    %f  1.0   0.0   0.0  1.0  3 \n", 
                                                dp[0], dp[1], dp[2], dp[0], dp[1], dp[2]);
                  if(i==1) fprintf (PathConnected, "%f   %f    %f   %f   %f    %f  0.0 1.0   0.0  1.0  3 \n", 
                                                dp[0], dp[1], dp[2], dp[0], dp[1], dp[2]);
                  if(i==2) fprintf (PathConnected, "%f   %f    %f   %f   %f    %f  0.363  0.998  0.999  1.0  2 \n", 
                                                dp[0], dp[1], dp[2], dp[0], dp[1], dp[2]);
                  if(i==3) fprintf (PathConnected, "%f   %f    %f   %f   %f    %f  0.923  0.405  0.263  1.0  2 \n", 
                                                dp[0], dp[1], dp[2], dp[0], dp[1], dp[2]);
                  if(i==4) fprintf (PathConnected, "%f   %f    %f   %f   %f    %f 0.92    0.74  0.18 1.0  2 \n", 
                                                dp[0], dp[1], dp[2], dp[0], dp[1], dp[2]);                                        
               }                            
               dp = NULL; dp_new = NULL;          
            }
         }
         fclose (PathConnected); PathConnected = NULL;
      } 
      
      /* Set opt->X_Lamda as new opt->ControlCurve. When Changing opt->Lda no longer helps, most recent opt->ControlCurve is best path. */
      /* Now repeat optimization.  Keep repeating until path no longer changes.  Stop when best opt->Lda is 0. */
      SUMA_S_Warn("Warning Warning Will Robinson");
      if (opt->Lda>0.0) {    /* When opt->Lda=0, no longer adjusting path.  Then want Control Curve to stay the same. */
         fprintf(TempCC, "Number of times through optimization process = %d\n", opt->iter_count);
         for(i=0; i<opt->N_ctrl_points; ++i) {
            fprintf(TempCC, "\n i = %d\n", i);
            for (m=0; m<opt->M_time_steps+1; ++m) {
               dp = mxvdp3(opt->ControlCurve, 0, i, m);

               dp[0] = mxvd3(opt->X_Lamda, 0, i, m);
               dp[1] = mxvd3(opt->X_Lamda, 1, i, m);
               dp[2] = mxvd3(opt->X_Lamda, 2, i, m);
               
               /* Store newest control curve locations.  This is for debugging only! */
               fprintf(TempCC, "%f; \n", dp[0]);
               fprintf(TempCC, "%f; \n", dp[1]);
               fprintf(TempCC, "%f; \n", dp[2]);
               
               dp = NULL;
            }
         }
      }
      
      if (opt->dbg_flag > 1) {
         sprintf(stmp, "Done with iteration %d", opt->iter_count);
         SUMA_etime2(FuncName, stmp, FuncName);
      }

      ++opt->iter_count;
   } while ( opt->Lda > 0.0);
   
   fclose(TempCC); TempCC = NULL;
   
   /* Done, write out output */
   
   /* Send connected control curve to file to be viewed in SUMA.  Draws line between dots to connect the path. */
   sprintf(outfile_PathConnected, "SUMA_PathConnected_final.1D.dset"); 
   PathConnected = fopen (outfile_PathConnected, "w"); 
   fprintf (PathConnected, "#oriented_segments\n");
   dp = NULL;

   for(i=0; i<opt->N_ctrl_points; ++i) {
      for (m=0; m<opt->M_time_steps+1; ++m) {
         if(m<opt->M_time_steps) {
            dp = mxvdp3(opt->ControlCurve, 0, i, m);
            dp_new = mxvdp3(opt->ControlCurve, 0, i, m+1);
            if(i==0) fprintf (PathConnected, "%f   %f    %f    %f   %f    %f  1.0  0.0  0.0  1.0  3 \n", 
                                          dp[0], dp[1], dp[2], dp_new[0], dp_new[1], dp_new[2]);
            if(i==1) fprintf (PathConnected, "%f   %f    %f   %f   %f    %f  0.0  1.0  0.0  1.0  3 \n", 
                                          dp[0], dp[1], dp[2], dp_new[0], dp_new[1], dp_new[2]);
            if(i==2) fprintf (PathConnected, "%f   %f    %f   %f   %f    %f  0.0  0.0  1.0  1.0  2 \n", 
                                          dp[0], dp[1], dp[2], dp_new[0], dp_new[1], dp_new[2]);
            if(i==3) fprintf (PathConnected, "%f   %f    %f   %f   %f    %f  0.63 0.0  0.67  1.0  2 \n", 
                                          dp[0], dp[1], dp[2], dp_new[0], dp_new[1], dp_new[2]);
            if(i==4) fprintf (PathConnected, "%f   %f    %f   %f   %f    %f 0.92    0.74  0.18 1.0  2 \n", 
                                             dp[0], dp[1], dp[2], dp_new[0], dp_new[1], dp_new[2]);                                        

         } else {
            dp = mxvdp3(opt->ControlCurve, 0, i, m);
            if(i==0) fprintf (PathConnected, "%f   %f    %f    %f   %f    %f  1.0  0.0  0.0  1.0  3 \n", 
                                          dp[0], dp[1], dp[2], dp[0], dp[1], dp[2]);
            if(i==1) fprintf (PathConnected, "%f   %f    %f   %f   %f    %f  0.0  1.0  0.0  1.0  3 \n", 
                                          dp[0], dp[1], dp[2], dp[0], dp[1], dp[2]);
            if(i==2) fprintf (PathConnected, "%f   %f    %f   %f   %f    %f  0.0  0.0  1.0  1.0  2 \n", 
                                          dp[0], dp[1], dp[2], dp[0], dp[1], dp[2]);
            if(i==3) fprintf (PathConnected, "%f   %f    %f   %f   %f    %f  0.63 0.0  0.67  1.0  2 \n", 
                                          dp[0], dp[1], dp[2], dp[0], dp[1], dp[2]);
            if(i==4) fprintf (PathConnected, "%f   %f    %f   %f   %f    %f 0.92    0.74  0.18 1.0  2 \n", 
                                             dp[0], dp[1], dp[2], dp[0], dp[1], dp[2]);           
         }                            
         dp = NULL; dp_new = NULL;                          
      }
   }
   fclose (PathConnected); PathConnected = NULL;

   fprintf(SUMA_STDERR, "Final Control Curve:\n");
   for(i=0; i<opt->N_ctrl_points; ++i) {
      for (m=0; m<opt->M_time_steps+1; ++m) {
         dp = mxvdp3(opt->ControlCurve, 0, i, m);
         fprintf(SUMA_STDERR, "%f %f %f\n", dp[0], dp[1], dp[2]);
      }
      fprintf(SUMA_STDERR, "\n");
   }

   
   /* write results */
   sprintf(stmp,"%s_X_Lamda.1D", opt->outfile);
   if (!SUMA_WriteMxVec(opt->X_Lamda, stmp, "Das X Lamda")) {
      SUMA_S_Errv("Failed in writing %s, continuing ...\n", stmp);
   }
   sprintf(stmp,"%s_ControlCurve.1D", opt->outfile);
   if (!SUMA_WriteMxVec(opt->ControlCurve, stmp, "Das Control Curve")) {
      SUMA_S_Errv("Failed in writing %s, continuing ...\n", stmp);
   }
   i = 2;
   mo = SUMA_NewMxVec(SUMA_double, 1, &i,  1);
   mxvd1(mo, 0) = opt->Lda;
   mxvd1(mo, 1) = opt->iter_count;
   sprintf(stmp,"%s_Misc.1D", opt->outfile);
   if (!SUMA_WriteMxVec(mo, stmp, "Das Misc values (Lda, iter_count)")) {
      SUMA_S_Errv("Failed in writing %s, continuing ...\n", stmp);
   }
   
   /* cleanup */
   fclose(condition_num); condition_num = NULL;
   fclose(condition_num_only); condition_num_only = NULL;
   mo = SUMA_FreeMxVec( mo );
   MaxStep = SUMA_FreeMxVec( MaxStep );
   Perturb_Vec = SUMA_FreeMxVec( Perturb_Vec );
   Del_S = SUMA_FreeMxVec( Del_S );
   
      
   SUMA_RETURN(opt->iter_count);
}

int SUMA_Apply_Deformation(MyCircleOpt *opt, SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_Apply_Deformation"};
   char outfile_segments[50], outfile_Vmag[50], outfile_dt[50];
   double oda, faa, error, dtheta=0.0, nrmi[3]={0.0, 0.0, 0.0}, nrmf[3]={0.0, 0.0, 0.0}, dtorig=0.0;
   MyCircle *Ci = NULL;
   int i, i3, idm, j, j3, adj_factor = 20, s4;
   void *SO_name;
   double *dp = NULL, *dp_mi = NULL, *dp_mf = NULL; 
   int niter=0, a_niter, first_bad_niter = 0, second_bad_niter = 0;
   double  Vf_segment[3], dist_to_target = 0.0, dist = 0.0, dist_nrm[3], orig_dist = 0.0;
   int n0, n1, n2, Halving=0, Force_dt=0, m=0, k=0;
   double dt = 0.0;    
   double energy, energy_sum, time_elapsed = 0;    
   double v0, v1, v2, delta_dist;
   char outfile_SphereQuality[50], outfile_PlotPath[50], outfile_Condition[50], outfile_Condition_Only[50];
   int too_close = 0, need_neighb_adjust = 0, need_more_adjustment = 0;
   double oxyz[3]={0.0, 0.0, 0.0}, distance;
   char *shist=NULL, stmp[600];
   int nbad, close_neighb = 0, nbad_face, nbad_node;
   byte *RiskyTrigs=NULL;
   SUMA_Boolean exists;
   SUMA_SPHERE_QUALITY SSQ;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* Set up file for storing condition numbers for matrix used in calculating spline weights during the deformation. */
   FILE *condition_num = NULL;
   sprintf(outfile_Condition, "Matrix_ConditionEigen.1D");
   condition_num = fopen (outfile_Condition, "w"); 
   
   /* Set up file for storing condition numbers for matrix used in calculating spline weights during the deformation. */
   FILE *condition_num_only = NULL;
   sprintf(outfile_Condition_Only, "Matrix_Condition_Num.1D");
   condition_num_only = fopen (outfile_Condition_Only, "w"); 
   
   if (opt->dbg_flag) { fprintf(stderr,"%s: About to allocate (N_sub-corrected-= %d).\n", FuncName, opt->N_sub); }
   Ci = (MyCircle *)SUMA_malloc(sizeof (MyCircle)); 
   
   /* based on dim, fix N_sub */
   if (opt->dom_dim < 3) {
      Ci->N_Node = opt->N_sub;
   } else {
      /* opt->N_sub has been taken care of */
      if(!opt->SO){
         Ci->N_Node =  2 + 10 * SUMA_POW2(opt->N_sub);
         fprintf (SUMA_STDERR,"Note %s: Closest number of nodes is %d, Number of Subdivisions is %d\n", 
            FuncName, Ci->N_Node, opt->N_sub);
      } else{
         Ci->N_Node = opt->SO->N_Node;
      }
   }
   
   if (opt->dbg_flag) { fprintf(stderr,"%s: Object contains %d nodes.\n", FuncName, Ci->N_Node); }
   Ci->NodeList = Ci->VelocityField = Ci->Vf_Step = Ci->NewNodeList = Ci->VelocityMagnitude = Ci->NewNodeList_temp = NULL;
   Ci->Theta = NULL;
   Ci->NodeList = (double *)SUMA_malloc(Ci->N_Node * 3 * sizeof (double));
   Ci->VelocityField = (double *)SUMA_malloc(Ci->N_Node * 3 * sizeof (double));
   Ci->Vf_Step = (double *)SUMA_malloc(Ci->N_Node * 4 * sizeof (double));
   Ci->VelocityMagnitude = (double *)SUMA_malloc(Ci->N_Node * sizeof (double));
   Ci->NewNodeList = (double *)SUMA_malloc(Ci->N_Node * 3 * sizeof (double));
   Ci->NewNodeList_temp = (double *)SUMA_malloc(Ci->N_Node * 3 * sizeof (double));
   Ci->Theta = (double *)SUMA_malloc(Ci->N_Node * 3 * sizeof (double));
 
   if(opt->dot == 1) { fprintf( stderr, "USING DOT PRODUCT RESTRICTION.\n"); } 
   
   if (opt->dom_dim < 3) {
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
      if (!opt->SO) {
         SUMA_S_Err("NULL SO, bad for the stomach.\n");
         SUMA_RETURN(0);
      }
      for (i = 0; i < 3*Ci->N_Node; ++i) {
         Ci->Theta[i] = 0.0;
         Ci->NodeList[i] = opt->SO->NodeList[i];
      }

      /* see if SUMA talk is turned on */
      if (ps->cs->talk_suma) {
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
      second_bad_niter = 0; 
      fprintf(SUMA_STDERR, "\n%s:Setting up for move. m = %d\n", FuncName, m);

      for (i = 0; i < 3*Ci->N_Node; ++i) Ci->VelocityField[i] = 0.0;  
      
      if(opt->Lda > 0.0 && opt->M_time_steps > 1) {
         for(i=0; i<opt->N_ctrl_points; ++i) {
            i3 = 3*i;
            dp_mi = mxvdp3(opt->X_Lamda, 0, i, m);    /* initial */
            dp_mf = mxvdp3(opt->X_Lamda, 0, i, m+1);  /* final */

            opt->CtrlPts_i[i3  ] = dp_mi[0];
            opt->CtrlPts_i[i3+1] = dp_mi[1];
            opt->CtrlPts_i[i3+2] = dp_mi[2];

            opt->CtrlPts_f[i3  ] = dp_mf[0];
            opt->CtrlPts_f[i3+1] = dp_mf[1];
            opt->CtrlPts_f[i3+2] = dp_mf[2];

            dp_mi = NULL; dp_mf = NULL;
         }        
      } 
      if(opt->Lda == 0.0 || opt->M_time_steps < 2) {  /* When opt->Lda = 0, use original path. Use opt->ControlCurve. */
         for(i=0; i<opt->N_ctrl_points; ++i) {
            i3 = 3*i;
            dp_mi = mxvdp3(opt->ControlCurve, 0, i, m);    /* initial */
            dp_mf = mxvdp3(opt->ControlCurve, 0, i, m+1);  /* final */

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
      
      /* Calculate total desired distance of move.  Used to adjust dt at end of interval, 
            so can get to final destination more efficiently. */
      orig_dist = 0.0;
      for( i=0; i<opt->N_ctrl_points; ++i) {
            i3 = 3*i;
            dist = 0.0;
            dist_nrm[0] = 0.0; dist_nrm[1] = 0.0; dist_nrm[2] = 0.0; 
            SUMA_ANGLE_DIST_NC( (&(opt->CtrlPts_f[i3])), (&(opt->CtrlPts_i[i3])) , dist, dist_nrm); 
            orig_dist = orig_dist + dist;
      }
      fprintf(SUMA_STDERR, "ORIGINAL DISTANCE : %g\n", orig_dist);
      
      /* Initialize renewable initial control points (for error computations). */  
      for (i = 0; i < 3*opt->N_ctrl_points; ++i) opt->CtrlPts[i] = opt->CtrlPts_i[i]; 

      /* Calculate spline weights to fit velocity field. */   
      if (!FindSplineWeights (Ci, opt, condition_num, condition_num_only)) {
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
     
/******************Begin New Counting Method. */ 
      niter = 0;
      a_niter = 0;
      dt = 1.0/(opt->N_step);    /* No longer want dt dependent on number of steps.  */
      fprintf(SUMA_STDERR, "     ORIGINAL DISTANCE = %g, DISTANCE TO TARGET = %g\n", orig_dist, dist_to_target);
      dist_to_target = orig_dist;
      dtorig = dt;
      Force_dt = 0;

      /* Send dt values to file for plotting in matlab. */
      FILE *plot_dt = NULL;
      sprintf(outfile_dt, "plot_dt_m%d.1D.dset", m); 
      plot_dt = fopen (outfile_dt, "w");  

      do {
         delta_dist = 0.0;
         delta_dist = (dist_to_target - orig_dist);
         if(!Force_dt) {
            /* fprintf(SUMA_STDERR, "Niter = %d; dtorig = %g; orig_dist = %g; dist_to_target  = %g; dt = %g;\n", 
                                    niter, dtorig, orig_dist, dist_to_target, dt); */
            /* fprintf(SUMA_STDERR, "     1.0-dtorig = %g\n", (1.0-dtorig));
            fprintf(SUMA_STDERR, "     dist_to_target - orig_dist = %g\n", (dist_to_target - orig_dist)); */
            
            dt = (1.0-dtorig)*( ((dist_to_target - orig_dist )/(-orig_dist) ) + dtorig);
            /* fprintf(SUMA_STDERR, "DT = %g (before any adjustment) \n", dt);
            dt = pow(dt, 0.25);*/
            fprintf(SUMA_STDERR, "Niter = %d; dtorig = %g; orig_dist = %g; dist_to_target  = %g; dt = %g;\n", 
                                 niter, dtorig, orig_dist, dist_to_target, dt);
            fprintf(condition_num, "Niter = %d; dist_to_target  = %g; dt = %g;\n", 
                                 niter, dist_to_target, dt);
         }else {
            fprintf(SUMA_STDERR, "           Using Adjusted dt.  Will continue from this dt. \n");
            /* fprintf(SUMA_STDERR, "Niter = %d; orig_dist = %g; dist_to_target  = %g; dt = %g;\n", 
                                    niter, orig_dist, dist_to_target, dt); */
         }
         fprintf(plot_dt, "%g    %g\n", dist_to_target, dt);
         if(!niter) Debug_Move (Ci, opt, opt->SO, dt, niter, m, a_niter, first_bad_niter); 
         
         /* For plotting energy graphs like in the paper. */
         /* The measure of the velocity is the dot product of the velocity vector and the weight, summed for all the control points. */
         /* Need to collect energy data for whole move.  From m=0 to m=1. */
         energy_sum = 0.0;
         for(i=0; i < opt->N_ctrl_points; ++i) {
            j = opt->CtrlPts_iim[i];
            j3 = 3*j;
            i3 = 3*i;
            energy = 0.0;
            energy = SUMA_MT_DOT( (&(Ci->VelocityField[j3])), (&(Ci->Wv.elts[i3])) );
            energy_sum += energy;  
         }
         fprintf(energy_graph, "%11.8f  %11.8f\n", time_elapsed, energy_sum);
         ++time_elapsed;

         do{  /* Move the points and make sure dt is small enough, if not small enough, undo move and try again with smaller dt. */
            Calculate_Step (Ci, opt, dt);

            /* Check to see if Neighbors are too close. */
            if(opt->neighb_check) {
               too_close = 0.0;
               for(i = 0; i < Ci->N_Node; ++i) {  
                  s4 = 4*i;
                  for(k=0; k < opt->SO->FN->N_Neighb[i]; ++k) {   
                     j = opt->SO->FN->FirstNeighb[i][k];  /*Index of node neighbor.*/
                     i3 = 3*i;
                     j3 = 3*j;
                     distance = sqrt(  SUMA_POW2(Ci->NewNodeList[j3  ] - Ci->NewNodeList[i3  ]) + 
                                       SUMA_POW2(Ci->NewNodeList[j3+1] - Ci->NewNodeList[i3+1]) + 
                                       SUMA_POW2(Ci->NewNodeList[j3+2] - Ci->NewNodeList[i3+2]) ); 
                     /* Consider keeping steps smaller than one half of the distance. */
                     distance = 0.3*distance; 

                     if(Ci->Vf_Step[s4+3] > distance) {
                        close_neighb = i;
                        fprintf(SUMA_STDERR, "%s:  Stepsize too big! Node = %d, Neighbor = %d\n"
                                             "     distance to nearest node = %f\n"
                                             "     step size = %f\n",
                                             FuncName, i, j, distance, Ci->Vf_Step[s4+3] ); 
                        break;
                     }
                     if(close_neighb) break;
                  }
                  if(close_neighb) break;
               }
               if(LocalHead) fprintf(SUMA_STDERR,"%s: End loop for checking neighbors.\n", FuncName);
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

            Move_Points(Ci, opt);   
            
            /* Recalculate surface normals. */
            if (ps->cs->talk_suma) {
               /* for (i3=0; i3<3*opt->SO->N_Node; ++i3) opt->SO->NodeList[i3] = Ci->NewNodeList_temp[i3]; */
               for (i=0; i<3*opt->SO->N_Node; ++i) opt->SO->NodeList[i] = Ci->NewNodeList_temp[i];
               SUMA_RECOMPUTE_NORMALS(opt->SO); 
               
               if (opt->pause > 1) {
                  SUMA_PAUSE_PROMPT("Go ye Huskies");
               }
               if (ps->cs->Send) {
                  if (!SUMA_SendToSuma (opt->SO, ps->cs, (void *)opt->SO->NodeList, SUMA_NODE_XYZ, 1)) {
                     SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
                  }
               }
            }
           
            /* Check Sphere Quality.  Since want to check at the end of each iteration, need to start at niter = 1. */
               nbad = 0;
               for (i3=0; i3<3*opt->SO->N_Node; ++i3) opt->SO->NodeList[i3] = Ci->NewNodeList_temp[i3];
               nbad = SUMA_Bad_FacesetNorm_Dot_Radius(opt->SO, RiskyTrigs, 0.00001, NULL, NULL, 1);
            if (LocalHead || !(niter % 100)) fprintf(SUMA_STDERR, "\n%s: NITER = %d (dt = %g)\n", FuncName, niter, dt);
            
            if (nbad) { /* Trouble, do the whole quality check */
               fprintf(SUMA_STDERR,"Trouble at iteration %d, %d bad facets (dt .= %g).\n", niter, nbad, dt);
               /*sprintf(outfile_SphereQuality, "SphereQuality_m%d_n%d", m, niter);
               SO_name = SUMA_Prefix2SurfaceName (outfile_SphereQuality, NULL, NULL, SUMA_VEC, &exists);
               SUMA_RECOMPUTE_NORMALS(opt->SO);

               shist = SUMA_HistString (FuncName, argc, argv, NULL);
               SSQ = SUMA_SphereQuality(opt->SO, outfile_SphereQuality, shist);
               nbad = SSQ.N_bad_facesets;
               SUMA_Save_Surface_Object(SO_name, opt->SO, SUMA_VEC, SUMA_ASCII, NULL);   */
            } else {
               if (LocalHead) fprintf(SUMA_STDERR,"iteration %d, %d bad facests: Nema Problema!\n", niter, nbad);
            }
            #if 0
            if(nbad && !first_bad_niter){ first_bad_niter = niter; }
            if(nbad && first_bad_niter && !second_bad_niter) { second_bad_niter = niter; }
            if(first_bad_niter && !second_bad_niter) fprintf(SUMA_STDERR, "FLIPPED FACET! Niter = %d\n", niter);
            if(second_bad_niter) fprintf(SUMA_STDERR, "SECOND FLIPPED FACET! Niter = %d\n", niter);
            #endif
            if (shist) SUMA_free(shist); shist = NULL;

            /* Adjust dt if had flipped facet.  This will repeat the move with a smaller dt.  
                  Keep shrinking dt until no more flipped facets. Once happy with NewNodeList_temp, 
                  store these values in NewNodeList and exit this do-loop. */
            if( nbad ) {
               SUMA_S_Notev("Halving dt from to %g to %g\n", dt, dt/2.0);
               Halving = 1;
               Force_dt = 1;
               dt = dt/2.0;
            }
            if( !nbad ) {
               if (Halving) {
                  SUMA_S_Notev("Halving at iteration %d worked. No more flipping. dt now %g\n", niter, dt);
                  /* proceed with dt untouched for at least one more iteration */
               }else {
                  /* looks clean , no folding, can we recover? */
                  if (dt < dtorig) {
                     dt = dt * 2.0;
                     SUMA_S_Notev("             Doubling at iteration %d . dt now %g\n", niter, dt);
                  }
               }
               Halving = 0;
               for (i = 0; i < 3*Ci->N_Node; ++i) Ci->NewNodeList[i] = Ci->NewNodeList_temp[i]; 
            }
         
         } while( nbad > 0 );  /* Once there are no more bad facets, dt is small enough and can continue to next iteration. */
                  /* What will dt be now?  Back to original or stick with small dt?  Will need to see how 
                        this loop affects dt for rest of move. */
                  /* For now, have dt changed permantly by the loop above.  Changed because facets were flipping. */

         if (opt->dbg_flag > 1) {
            /* Print Velocity Magnitudes to File. */   
            FILE *plot_Vmag = NULL;
            sprintf(outfile_Vmag, "DBG_out/SUMA_Vmag_m%d_n%d.1D.dset", m, niter); 
            plot_Vmag = fopen (outfile_Vmag, "w"); 
            for (i = 0; i < Ci->N_Node; ++i) {
               i3 = 3*i;
               fprintf (plot_Vmag, "%11.8f\n", sqrt(  SUMA_POW2(Ci->VelocityField[i3  ]) + 
                                                      SUMA_POW2(Ci->VelocityField[i3+1]) + 
                                                      SUMA_POW2(Ci->VelocityField[i3+2]) ));
            }
            fclose (plot_Vmag); plot_Vmag = NULL;


            /* TO PLOT VELOCITY FIELD AT ANY ITERATION. */
            /* Write oriented segment file for plotting in SUMA. */
            FILE *plot_segments = NULL;
            sprintf(outfile_segments, "DBG_out/SUMA_segments_m%d_n%d.1D.dset", m, niter); 
            plot_segments = fopen (outfile_segments, "w"); 
            fprintf (plot_segments, "#oriented_segments\n");
            for (i = 0; i < Ci->N_Node; ++i) {
               i3 = 3*i;
               /* To plot end of segment, must calculate the location of the point of the velocity vector. 
                  This is done by adding the position vector of the node to the velocity vector at that node. */
               Vf_segment[0] = Ci->VelocityField[i3  ];
               Vf_segment[1] = Ci->VelocityField[i3+1];
               Vf_segment[2] = Ci->VelocityField[i3+2];

               Vf_segment[0] = Ci->NewNodeList[i3  ] + Vf_segment[0];
               Vf_segment[1] = Ci->NewNodeList[i3+1] + Vf_segment[1];
               Vf_segment[2] = Ci->NewNodeList[i3+2] + Vf_segment[2];

               fprintf (plot_segments, "%11.8f  %11.8f  %11.8f  %11.8f  %11.8f  %11.8f 0.0  0.0  1.0  1.0 1.5\n",
                                       Ci->NewNodeList[i3], Ci->NewNodeList[i3+1], Ci->NewNodeList[i3+2], 
                                       Vf_segment[0], Vf_segment[1], Vf_segment[2] );
            }
            fclose (plot_segments); plot_segments = NULL; 
         }
         
         /* Call spline weight function to recalculate spline weights for renew_weights option. */
         if (opt->renew_weights) { 
            
            if(LocalHead) fprintf( SUMA_STDERR, "%s: RENEW_WEIGHTS OPTION -- Check renewed control points: \n", FuncName ); 
            for(i=0; i < opt->N_ctrl_points; ++i) {
               i3 = 3*i;
               opt->CtrlPts[i3  ] = Ci->NewNodeList[3*(opt->CtrlPts_iim[i])  ];
               opt->CtrlPts[i3+1] = Ci->NewNodeList[3*(opt->CtrlPts_iim[i])+1];
               opt->CtrlPts[i3+2] = Ci->NewNodeList[3*(opt->CtrlPts_iim[i])+2];

               Ci->Wv.elts[i3  ] = 0.0; 
               Ci->Wv.elts[i3+1] = 0.0; 
               Ci->Wv.elts[i3+2] = 0.0; 
              
               if(LocalHead) fprintf( SUMA_STDERR, "%s: Control Point(%d) = [%11.8f;  %11.8f;   %11.8f] \n", FuncName, i, 
                                       opt->CtrlPts[i3  ], opt->CtrlPts[i3+1], opt->CtrlPts[i3+2] ); 
            }
            FindSplineWeights(Ci, opt, condition_num, condition_num_only); 
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
         
         /* Need to calculate how close New_Nodes are to target, so know when to end do-loop. */
         dist_to_target = 0.0;
         for( i=0; i<opt->N_ctrl_points; ++i) {
            i3 = 3*i;
            dist = 0.0;
            dist_nrm[0] = 0.0; dist_nrm[1] = 0.0; dist_nrm[2] = 0.0; 
            SUMA_ANGLE_DIST_NC( (&(opt->CtrlPts_f[i3])), (&(opt->CtrlPts[i3])) , dist, dist_nrm); 
            dist_to_target = dist_to_target + dist;
         }
         if( niter % 100) fprintf( SUMA_STDERR, "DISTANCE: dist_to_target = %g, dt = %g, niter = %d \n", dist_to_target, dt, niter);
         
         ++niter;
      } while( dist_to_target > 0.000000001 ); 
         /* while(niter < opt->N_step); */
      fclose (plot_dt); plot_dt = NULL;

/************************************/
    
/** Snip_0.c here **/

      if(close_neighb) fprintf(SUMA_STDERR, "Too Close! m=%d\n", m);
      fprintf(SUMA_STDERR, "******End Do-Loop here.\n");
      close_neighb = 0;
      first_bad_niter = 0;
      
      /* Check Sphere Quality after each m time interval. */    
      fprintf(SUMA_STDERR, "\n%s: End of move. Checking Sphere Quality: m=%d to m=%d\n", FuncName, m, m+1);
      for (i3=0; i3<3*opt->SO->N_Node; ++i3) opt->SO->NodeList[i3] = Ci->NewNodeList[i3];
      nbad = SUMA_Bad_FacesetNorm_Dot_Radius(opt->SO, RiskyTrigs, 0.00001, NULL, NULL, 1);
   
      if (nbad) { /* Trouble, do the whole quality check */
         fprintf(SUMA_STDERR,"Trouble at knot %d, %d bad facests.\n", m, nbad);
         sprintf(outfile_SphereQuality, "SphereQuality_m%d", m);

         SO_name = SUMA_Prefix2SurfaceName (outfile_SphereQuality, NULL, NULL, SUMA_VEC, &exists);
         SUMA_RECOMPUTE_NORMALS(opt->SO);

         SSQ = SUMA_SphereQuality(opt->SO, outfile_SphereQuality , NULL);
         nbad = SSQ.N_bad_facesets;
         SUMA_Save_Surface_Object(SO_name, opt->SO, SUMA_VEC, SUMA_ASCII, NULL);
      }
        
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
         fprintf(SUMA_STDERR, "Number of times optimization run = %d\n", opt->iter_count);
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
      if (!SUMA_SendToSuma (opt->SO, ps->cs, NULL, SUMA_NODE_XYZ, 2)) {
         SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCleanup failed");
      }
   }   

   /* copy last coordinates to opt->SO */
   for (i3=0; i3<3*opt->SO->N_Node; ++i3) opt->SO->NodeList[i3] = Ci->NewNodeList[i3];
   SUMA_RECOMPUTE_NORMALS(opt->SO);


   /* cleanup */
   fclose(condition_num); condition_num = NULL;
   fclose(condition_num_only); condition_num_only = NULL;
   vector_destroy(&(Ci->Wv));
   if (Ci->NodeList) SUMA_free(Ci->NodeList); Ci ->NodeList = NULL;
   if (Ci->VelocityField) SUMA_free(Ci->VelocityField); Ci ->VelocityField = NULL;
   if (Ci->VelocityMagnitude) SUMA_free(Ci->VelocityMagnitude); Ci ->VelocityMagnitude = NULL;
   if (Ci->NewNodeList) SUMA_free(Ci->NewNodeList); Ci ->NewNodeList = NULL; 
   if (Ci->NewNodeList_temp) SUMA_free(Ci->NewNodeList_temp); Ci ->NewNodeList_temp = NULL; 
   if (Ci->Theta) SUMA_free(Ci->Theta); Ci ->Theta = NULL; 
   if (Ci) SUMA_free(Ci); Ci = NULL;

   SUMA_RETURN(1);
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
   char outfile_Spheres[50];
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt;  
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   MyCircleOpt myopt, *opt = NULL;
   SUMA_SPHERE_QUALITY SSQ;
   vector Wv;
   int i, niter = 0, nbad_face, nbad_node, dims[50], N_dims, i3;
   FILE *SpheresAtNodes = NULL;
   void *SO_name;
   SUMA_Boolean exists;
   SUMA_MX_VEC *mo=NULL;
   float ctr[3];
   double Rref = 0.0, mesh_area = 0.0, mag_ctrlpt, mag_nodelist;
   char *shist = NULL, stmp[600];
   SUMA_SurfSpecFile *Spec = NULL;
   int N_Spec;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
    
   SUMA_STANDALONE_INIT;
	SUMA_mainENTRY;


   /** See Snip_1.c for examples on how to use the multiplexed vectors*/
   
   /* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "-o;-talk;-i;-t;-spec;-s;");
   
   if (argc < 2) {
      usage_path_optimize(ps);
      exit (1);
   }
   
   Opt = SUMA_path_optimize_ParseInput (argv, argc, ps, &myopt);
   opt = (MyCircleOpt *)Opt->popt;
   
   if (opt->dbg_flag > 1) {
      SUMA_S_Note("Writing debug files in DBG_out");
      system("mkdir DBG_out");
   }

   if (ps->s_N_surfnames + ps->i_N_surfnames + ps->t_N_surfnames > 1) {
      SUMA_S_Err("Multiple surface specifications used. Only one surface allowed.");
      exit(1);
   }

   Spec = SUMA_IO_args_2_spec(ps, &N_Spec);
   if (N_Spec == 0) {
      SUMA_S_Note("No surfaces found, using internal ico");
   } else {
      if (N_Spec != 1) {
         SUMA_S_Err("Multiple spec at input.");
         exit(1);
      }
      SUMA_LH("Loading surface...");
      SO = SUMA_Load_Spec_Surf(Spec, 0, ps->sv[0], Opt->debug);
      if (!SO) {
            fprintf (SUMA_STDERR,"Error %s:\n"
                                 "Failed to find surface\n"
                                 "in spec file. \n",
                                 FuncName );
            exit(1);

      }
      opt->SO = SO;
      opt->N_sub = -1; /*meaningless here */    
   }
/****** Adjust surface to be a unit sphere. ****************/   

     /* When loading a spec file and not creating my own icosahedron, must adjust the sphere. */
                 /* Need to shrink sphere to be unit sphere so assumptions still apply. */
  if(opt->SO){    
      fprintf(SUMA_STDERR, "VIEW CONTENTS OF SURFACE OBJECT.  BEFORE ADJUSTMENT.  RADIUS MAYBE BE >1.\n");
      SUMA_Print_Surface_Object(opt->SO, NULL);

      /* Set center to zero. */
      SUMA_S_Warnv("Surface center set to 0.0, 0.0, 0.0\n Actual Center estimated at %f, %f, %f\n",
                    opt->SO->Center[0],opt->SO->Center[1],opt->SO->Center[2]);
      opt->SO->Center[0] = 0.0;
      opt->SO->Center[1] = 0.0;
      opt->SO->Center[2] = 0.0;
      
      fprintf(SUMA_STDERR,"****************\n \n \n %f %f %f\n",opt->SO->NodeList[0],opt->SO->NodeList[1],opt->SO->NodeList[2]);
      
      opt->Radius = 1.0;   /* radius has to be 1.0 for the optimization yall */
      if (!SUMA_NewSurfaceRadius(opt->SO, opt->Radius, NULL)) {
         SUMA_S_Err("Misere!");
         exit(1);
      }

      /* Check Work. */
      fprintf(SUMA_STDERR, "VIEW CONTENTS OF SURFACE OBJECT.  AFTER ADJUSTMENT.  RADIUS MUST BE 1.0.\n");
      SUMA_Print_Surface_Object(opt->SO, NULL);
      
   }
   
/********************************************************/
   
   /* Create file for plotting spheres at the nodes.  These spheres will follow the landmark movement.
      Useful for making movies. */
   sprintf(outfile_Spheres, "SUMA_Spheres.1D.dset"); 
   SpheresAtNodes = fopen (outfile_Spheres, "w"); 
   fprintf (SpheresAtNodes, "#node-based_spheres\n");
   for(i=0; i<opt->N_ctrl_points; ++i) {
      fprintf (SpheresAtNodes, "%d  1.0  1.0  1.0  1.0  0.015  2  \n", opt->CtrlPts_iim[i]);
   }   
   fclose (SpheresAtNodes); SpheresAtNodes = NULL;

/******* Surface Creation section ********************/
   if (opt->dom_dim >= 3) opt->N_sub = SUMA_ROUND((sqrt((float)( opt->N_sub - 2 ) / 10.0)));   
   if (opt->dbg_flag) { fprintf(stderr,"%s: Creating icosahedron with %d subdivisions.\n", FuncName, opt->N_sub); }
   if(!opt->SO) opt->Radius= 1.0;
   opt->Center[0] = opt->Center[1] = opt->Center[2] = 0.0;
   ctr[0] = (float)opt->Center[0];
   ctr[1] = (float)opt->Center[1];
   ctr[2] = (float)opt->Center[2];
      
   if (!opt->SO) {
      opt->SO = SUMA_CreateIcosahedron(opt->Radius, opt->N_sub, ctr, "n", 1);
      SO_name = SUMA_Prefix2SurfaceName ("toy_ico", NULL, NULL, SUMA_VEC, &exists);
      if (!opt->SO->State) {opt->SO->State = SUMA_copy_string("Julia"); }
      if (!opt->SO->Group) {opt->SO->Group = SUMA_copy_string("Julia"); }
      if (!opt->SO->Label) {opt->SO->Label = SUMA_copy_string("toy_ico"); }
      /* make the idcode_str depend on the Label, it is convenient to send the same surface all the time to SUMA */
      if (opt->SO->Label) {   if (opt->SO->idcode_str) SUMA_free(opt->SO->idcode_str); 
                              opt->SO->idcode_str = NULL; 
                              SUMA_NEW_ID(opt->SO->idcode_str, opt->SO->Label); }

      SUMA_Save_Surface_Object(SO_name, opt->SO, SUMA_VEC, SUMA_ASCII, NULL);   /*Creates .coord and .topo files for SUMA */
   } else {
      if (!opt->SO->State) {opt->SO->State = SUMA_copy_string("Julia"); }
      if (!opt->SO->Group) {opt->SO->Group = SUMA_copy_string("Julia"); }
      if (!opt->SO->Label) {opt->SO->Label = SUMA_copy_string("toy_ico"); }
      /* make the idcode_str depend on the Label, it is convenient to send the same surface all the time to SUMA */
      if (opt->SO->Label) {   if (opt->SO->idcode_str) SUMA_free(opt->SO->idcode_str); 
                              opt->SO->idcode_str = NULL; 
                              SUMA_NEW_ID(opt->SO->idcode_str, opt->SO->Label); }
   }   
   /* see if SUMA talk is turned on */
   if (ps->cs->talk_suma) {
      ps->cs->istream = SUMA_BRAINWRAP_LINE;
      ps->cs->afni_istream = SUMA_AFNI_STREAM_INDEX2;
      ps->cs->kth = 1; /* make sure all surfaces get sent */
      if (!SUMA_SendToSuma (NULL, ps->cs, NULL, SUMA_NO_DSET_TYPE, 0) || !opt->SO) {
         SUMA_SL_Err("Failed to initialize SUMA_SendToSuma");
         ps->cs->Send = NOPE;
         ps->cs->afni_Send = NOPE;
         ps->cs->talk_suma = NOPE;
      } else {
         SUMA_LH("Sending Ico");
         SUMA_SendSumaNewSurface(opt->SO, ps ->cs);
      }
   }
  
/******* Optimization section ************************/   
   matrix_psinv_seteps(opt->psepsilon);
   
   if (opt->read_path[0]) {/* read results */
      SUMA_S_Warn("Warning, reading optimal path from files.");
      N_dims = 3; dims[0]= 3; dims[1] = opt->N_ctrl_points; dims[2] = (opt->M_time_steps+1);
      sprintf(stmp,"%s_X_Lamda.1D", opt->read_path);
      opt->X_Lamda = SUMA_Read1DMxVec(SUMA_double, stmp, dims, &N_dims);
      if (opt->X_Lamda == NULL) {
         SUMA_S_Errv("Failed in reading %s\n", stmp);
         exit(1);
      }
      N_dims = 3; dims[0] = 3; dims[1] = opt->N_ctrl_points; dims[2] = opt->M_time_steps + 1;
      sprintf(stmp,"%s_ControlCurve.1D", opt->read_path);
      opt->ControlCurve = SUMA_Read1DMxVec(SUMA_double, stmp, dims, &N_dims);
      if (opt->ControlCurve == NULL) {
         SUMA_S_Errv("Failed in reading %s\n", stmp);
         exit(1);
      }
      N_dims = 1; dims[0] = 2;
      sprintf(stmp,"%s_Misc.1D", opt->read_path);
      mo = SUMA_Read1DMxVec(SUMA_double, stmp, dims, &N_dims);
      if (!mo) { SUMA_S_Errv("Failed in reading %s\n", stmp); exit(1); }
      opt->Lda = mxvd1(mo, 0);
      opt->iter_count = mxvd1(mo, 1);
      mo = SUMA_FreeMxVec(mo);
   } else { /* Do the real thing */
      if ( !(SUMA_Optimize_Path(opt, ps))) {
         SUMA_S_Err("Failed to optimize path");
         exit(1);
      }
   }
   
/****** Deformation section ************************/    
   if ( !(SUMA_Apply_Deformation(opt, ps))) {
      SUMA_S_Err("Failed to apply deformation");
      exit(1);
   }   

/****** Output results ****************************/
   /* Write final surface to file, so can view in SUMA without taking the time of the -talk option. */
   SO_name = SUMA_Prefix2SurfaceName (opt->outfile, NULL, NULL, SUMA_VEC, &exists); 
   shist = SUMA_HistString (NULL, argc, argv, NULL);
   fprintf( SUMA_STDERR, "\nNITER = %d", niter );
   SSQ = SUMA_SphereQuality(opt->SO, opt->outfile , shist);   
   nbad_face = SSQ.N_bad_facesets;
   nbad_node = SSQ.N_bad_nodes;
   
   if (nbad_face) {
      fprintf(SUMA_STDERR,"Shist %s!:\n you have %d bad facets!\n", FuncName, nbad_face);
   } else if (nbad_node) {
      fprintf(SUMA_STDERR,"Shist %s!:\n you have %d bad nodes!\n", FuncName, nbad_node);
   } else {
      fprintf(SUMA_STDERR,"%s: Happy pretend Valentine!\n"
                          "   You have no errors when checking node normals!\n" , FuncName);
   }
   if (shist) SUMA_free(shist); shist = NULL;
   
   SUMA_Save_Surface_Object(SO_name, opt->SO, SUMA_VEC, SUMA_ASCII, NULL);


   /*Cleanup*/
   if (opt->SO) SUMA_Free_Surface_Object(opt->SO); opt->SO = NULL;
   if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   if (Opt) Opt = SUMA_Free_Generic_Prog_Options_Struct(Opt);
   if (opt->CtrlPts_iim) SUMA_free(opt->CtrlPts_iim); opt->CtrlPts_iim = NULL;
   if (opt->CtrlPts) SUMA_free(opt->CtrlPts); opt->CtrlPts = NULL;
   if (opt->CtrlPts_i) SUMA_free(opt->CtrlPts_i); opt->CtrlPts_i = NULL;
   if (opt->CtrlPts_f) SUMA_free(opt->CtrlPts_f); opt->CtrlPts_f = NULL;
   if (opt->Dtheta) SUMA_free(opt->Dtheta); opt->Dtheta = NULL;
   if (opt->X_Lamda) opt->X_Lamda = SUMA_FreeMxVec( opt->X_Lamda );
   if (opt->ControlCurve) opt->ControlCurve = SUMA_FreeMxVec( opt->ControlCurve );
   if (opt->Nrm) SUMA_free(opt->Nrm); opt->Nrm = NULL;
   if (opt->ctrl) opt->ctrl = NULL; /* pointer from argv, do not free */
   if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   exit(0);
}
