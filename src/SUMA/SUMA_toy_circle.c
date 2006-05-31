#include "SUMA_suma.h"

SUMA_SurfaceViewer *SUMAg_cSV = NULL; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
SUMA_DO *SUMAg_DOv = NULL;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF = NULL; /*!< Global pointer to structure containing info common to all viewers */

#include "matrix.h"
#include "matrix.c"

typedef struct
   {
      int dbg_flag;
      int N_sub;           /* Number of subdivisions. (For 2D objects, it is the number of nodes.) */
      int N_step;          /* Number of steps.  Related to dt.  This is the inverse of dt. */
      int dom_dim;
      double dt;           /* Used to store step size which is calculated using N_step. */
      char *ctrl;          /* A pointer copy of argv containing filename. Do not free. */
      int N_ctrl_points;
      int *CtrlPts_iim;    /* Index of a particular ctrl point in the mesh. It's not necessary
                              for calculations but useful for debugging. An index of -1 is used
                              if ctrl point does not overlap with a node */
      double *CtrlPts_i;
      double *CtrlPts_I;   /* Specified by user. Will not change. */
      double *CtrlPts_f;
      double *Dtheta;
      double *Nrm;         /* Axis of rotation used for calculated velocity at the control points. */
      double Center[3];
      double Radius;
      int renew_weights;
      int adjust; 
      int dim;
      int dot;
      int neighb_adjust;   /* Check distance to nearest node and adjust step size accordingly. */
      int neighb_check;    /* Check nearest neighbor distances, but don't make a dt adjustment. */
      char outfile[500];
      int pause;
      double sigma;        /* If using sigma option to calculate weights, need to find new way to
                              determine velocity field for the entire surface.  Old way won't work. */
   } MyCircleOpt;

typedef struct
   {
      int N_Node;
      double *NodeList;
      double *VelocityField;
      double *Vf_Step;
      double *VelocityMagnitude;
      double *NewNodeList;
      double *Theta;
      vector Wv;
   } MyCircle;


void usage_toy_circle (SUMA_GENERIC_ARGV_PARSE *ps)
{
      static char FuncName[]={"usage_toy_circle"};
      char * s = NULL, *sio=NULL, *st = NULL, *sts = NULL;
      int i;
      s = SUMA_help_basics();
      sio  = SUMA_help_IO_Args(ps);
      printf ( "\n"
               "usage:\n"
               "  toy_circle [-ctrl CTRL_FILE] [-dom_dim DOM_DIM] [-dim DIM] [-N_sub N_SUB] [-N_step N_STEP]   \n"
               "             [-renew_weights] [-adjust] [-dot] [-neighb_adjust] [-neighb_check] [-sigma SIGMA] \n"
               "             [-talk_pause] [-debug DEBUG] [-ouput OUTPUT]                                      \n"
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

SUMA_GENERIC_PROG_OPTIONS_STRUCT *SUMA_toy_circle_ParseInput(char *argv[], int argc, SUMA_GENERIC_ARGV_PARSE *ps, MyCircleOpt *popt)
{
   static char FuncName[]={"SUMA_toy_circle_ParseInput"}; 
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
   popt->CtrlPts_i = NULL;
   popt->CtrlPts_I = NULL;
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
   snprintf(popt->outfile, 499*sizeof(char),"test_move");
   kar = 1;
   brk = NOPE;
	while (kar < argc) { /* Loop across command line options. */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_toy_circle(ps);
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
      popt->CtrlPts_i = (double *)SUMA_calloc(popt->N_ctrl_points * 3, sizeof (double)); /* initial (t=0) XYZ location of each node */
      popt->CtrlPts_f = (double *)SUMA_calloc(popt->N_ctrl_points * 3, sizeof (double)); /* final   (t=1) XYZ location of each node */
      popt->CtrlPts_I = (double *)SUMA_calloc(popt->N_ctrl_points * 3, sizeof (double)); /* renewed initial XYZ location of each node */
     
      /* column offsets */
      for (i=0; i<6; ++i) { co[i] = (i+shft)*popt->N_ctrl_points; }
      
      for (i=0; i < popt->N_ctrl_points; ++i) {
         i3 = 3*i;
         if (shft) popt->CtrlPts_iim[i] = (int)far[i];
         else popt->CtrlPts_iim[i] = -1;
         popt->CtrlPts_I[i3  ] = far[i+co[0]];
         popt->CtrlPts_I[i3+1] = far[i+co[1]];        
         popt->CtrlPts_I[i3+2] = far[i+co[2]];
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
                                 popt->CtrlPts_I[i3  ], popt->CtrlPts_I[i3+1], popt->CtrlPts_I[i3+2],
                                 popt->CtrlPts_f[i3  ], popt->CtrlPts_f[i3+1], popt->CtrlPts_f[i3+2] );
         }
      }
      mri_free(im); im = NULL; 
   }
   
   Opt->popt = (void*)popt;
   SUMA_RETURN(Opt);
}

SUMA_Boolean Debug_Weights( MyCircle *C, MyCircleOpt *opt, matrix M, matrix Mi, vector Vv) 
{
   static char FuncName[]={"FindSplineWeights"};
   static int matrix_ncall;
   int i, i3, j, j3, r, k, c, idm;
 
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   if(matrix_ncall == 3) matrix_ncall = 0;

   FILE *output_matrix = NULL;  /*for sending the matrix to a file to be read by matlab. */
   output_matrix = fopen( "output_matrix.m", "w" );
   /* Print Matrix */
   if(matrix_ncall == 0) {
      if (opt->dot) {
         fprintf(SUMA_STDERR, "%s:\n"
                              "M = [\n", FuncName);
         for (r=0; r<opt->N_ctrl_points; ++r) {
            for (k=0; k<(3+opt->N_ctrl_points); ++k) {
               for(c=0; c<opt->N_ctrl_points; ++c) { 
                  fprintf (SUMA_STDERR,"%.8f   %.8f   %.8f   ", 
                                          M.elts [ ((3+opt->N_ctrl_points) * r+k) ][ (3*c  ) ],
                                          M.elts [ ((3+opt->N_ctrl_points) * r+k) ][ (3*c+1) ],
                                          M.elts [ ((3+opt->N_ctrl_points) * r+k) ][ (3*c+2) ]);
               }
               fprintf (SUMA_STDERR,"\n");
            }
         }
         fprintf(SUMA_STDERR, "];\n"); 
      } else { 
         fprintf(SUMA_STDERR, "%s:\n"
                              "M = [\n", FuncName);
         for (r=0; r<opt->N_ctrl_points; ++r) {
            for (k=0; k<3; ++k) {
               for(c=0; c<opt->N_ctrl_points; ++c) { 
                  fprintf (SUMA_STDERR,"%.5f   %.5f   %.5f   ", 
                                          M.elts [ (3*r+k) ][ (3*c  ) ],
                                          M.elts [ (3*r+k) ][ (3*c+1) ],
                                          M.elts [ (3*r+k) ][ (3*c+2) ]);
               }
               fprintf (SUMA_STDERR,"\n");
            }
         }
         fprintf(SUMA_STDERR, "];\n");
      }
 
      /* Write matrix to a file so can be read by matlab. */
      #if 0
      if (opt->dot) {
         fprintf(output_matrix, "M = [\n");
            for (r=0; r<opt->N_ctrl_points; ++r) {
               for (k=0; k<(3+opt->N_ctrl_points); ++k) {
                  for(c=0; c<opt->N_ctrl_points; ++c) { 
                     fprintf (output_matrix,"%11.8f   %11.8f   %11.8f   ", 
                                             M.elts [ ((3+opt->N_ctrl_points) * r+k) ][ (3*c  ) ],
                                             M.elts [ ((3+opt->N_ctrl_points) * r+k) ][ (3*c+1) ],
                                             M.elts [ ((3+opt->N_ctrl_points) * r+k) ][ (3*c+2) ]);
                  }
                  fprintf (output_matrix,"\n");
               } }
         fprintf(output_matrix, "]; \n \n"); 
      } else { 
         fprintf(output_matrix, "M = [\n");
            for (r=0; r<opt->N_ctrl_points; ++r) {
               for (k=0; k<3; ++k) {
                  for(c=0; c<opt->N_ctrl_points; ++c) { 
                     fprintf (output_matrix,"%11.8f   %11.8f   %11.8f   ", 
                                             M.elts [ (3*r+k) ][ (3*c  ) ],
                                             M.elts [ (3*r+k) ][ (3*c+1) ],
                                             M.elts [ (3*r+k) ][ (3*c+2) ]);
                  }
                  fprintf (output_matrix,"\n");
               } }
         fprintf(output_matrix, "]; \n \n");  } 
      #endif
   }
   
   /*Print the Inverse Coefficient Matrix.*/
   if(matrix_ncall == 1) {
      if(opt->dot) {   
         if (opt->dim == 3) {
            fprintf(SUMA_STDERR, "%s:\n"
                                 "Mi = [\n", FuncName);
            for (r=0; r<opt->N_ctrl_points; ++r) {
               for (k=0; k<3; ++k) {
                  for(c=0; c<opt->N_ctrl_points; ++c) { 
                     fprintf (SUMA_STDERR,"%.5f   %.5f   %.5f   %.5f ", 
                                             Mi.elts [ (3*r+k) ][ (4*c  ) ],
                                             Mi.elts [ (3*r+k) ][ (4*c+1) ],
                                             Mi.elts [ (3*r+k) ][ (4*c+2) ],
                                             Mi.elts [ (3*r+k) ][ (4*c+3) ]);
                  }
                  fprintf (SUMA_STDERR,"\n");
               }
            }
            fprintf(SUMA_STDERR, "];\n");
         } else {
            fprintf(SUMA_STDERR, "%s:\n"
                                 "Mi = [\n", FuncName);
            for (r=0; r<opt->N_ctrl_points; ++r) {
               for (k=0; k<3; ++k) {
                  for(c=0; c<opt->N_ctrl_points; ++c) { 
                     fprintf (SUMA_STDERR,"%.5f   %.5f  ", 
                                             Mi.elts [ (3*r+k) ][ (2*c  ) ],
                                             Mi.elts [ (3*r+k) ][ (2*c+1) ]);
                  }
                  fprintf (SUMA_STDERR,"\n");
               }
            }
            fprintf(SUMA_STDERR, "];\n");
         }
      } else {   
         if (opt->dim == 3) {
            fprintf(SUMA_STDERR, "%s:\n"
                                 "Mi = [\n", FuncName);
            for (r=0; r<opt->N_ctrl_points; ++r) {
               for (k=0; k<3; ++k) {
                  for(c=0; c<opt->N_ctrl_points; ++c) { 
                     fprintf (SUMA_STDERR,"%.5f   %.5f   %.5f   ", 
                                             Mi.elts [ (3*r+k) ][ (3*c  ) ],
                                             Mi.elts [ (3*r+k) ][ (3*c+1) ],
                                             Mi.elts [ (3*r+k) ][ (3*c+2) ]);
                  }
                  fprintf (SUMA_STDERR,"\n");
               }
            }
            fprintf(SUMA_STDERR, "];\n");
         } else {
            fprintf(SUMA_STDERR, "%s:\n"
                                 "Mi = [\n", FuncName);
            for (r=0; r<opt->N_ctrl_points; ++r) {
               for (k=0; k<2; ++k) {
                  for(c=0; c<opt->N_ctrl_points; ++c) { 
                     fprintf (SUMA_STDERR,"%.5f   %.5f  ", 
                                             Mi.elts [ (2*r+k) ][ (2*c  ) ],
                                             Mi.elts [ (2*r+k) ][ (2*c+1) ]);
                  }
                  fprintf (SUMA_STDERR,"\n");
               }
            }
            fprintf(SUMA_STDERR, "];\n");
         }
      }
   }
   
   /* Print velocity vectors. */
   if(matrix_ncall == 2) {
      fprintf(SUMA_STDERR, "%s:\n"
                           "V = [ \n", FuncName );
      for (i=0; i < opt->N_ctrl_points; ++i) {
         if(opt->dot) { 
            idm = (opt->dim+opt->N_ctrl_points)*i; 
            fprintf(SUMA_STDERR, "  %10.8f;  %10.8f;  %10.8f; ",
                                 Vv.elts[idm  ], Vv.elts[idm+1], Vv.elts[idm+2] ); 
            for(k=0; k < opt->N_ctrl_points; ++k) { fprintf(SUMA_STDERR, "  0.0000;" ); } 
            fprintf(SUMA_STDERR, "\n" );  
         } else { 
            idm = opt->dim*i; 
            fprintf(SUMA_STDERR, "  %10.8f;  %10.8f;  %10.8f \n",
                                 Vv.elts[idm  ], Vv.elts[idm+1], Vv.elts[idm+2]); 
         }
      }
      fprintf(SUMA_STDERR, "];\n \n" );

      /* Send velocity vector to output file for checking math in matlab. */
      fprintf(output_matrix, "V = [ \n" );
      for (i=0; i < opt->N_ctrl_points; ++i) {
         if(opt->dot) { 
            idm = (opt->dim+opt->N_ctrl_points)*i; 
            fprintf(output_matrix, "  %11.20f;  %11.20f;  %11.20f; ",
                                 Vv.elts[idm  ], Vv.elts[idm+1], Vv.elts[idm+2] ); 
            for(k=0; k < opt->N_ctrl_points; ++k) { fprintf(output_matrix, "  0.0000;" ); } 
            fprintf(output_matrix, "\n" );  
         } else { 
            idm = opt->dim*i; 
            fprintf(output_matrix, "  %11.20f;  %11.20f;  %11.20f \n",
                                 Vv.elts[idm  ], Vv.elts[idm+1], Vv.elts[idm+2]); 
         }
      }
      fprintf(output_matrix, "];\n \n" );
 
      /* Print Weights. */
      fprintf(SUMA_STDERR, "%s:\n"
                           "Wv = [\n", FuncName);
      for (r=0; r<opt->N_ctrl_points; ++r) {
         if (opt->dim == 3) {
            fprintf (SUMA_STDERR,"%.20f;   %.20f;   %.20f;  \n",
                                  C->Wv.elts[3*r], C->Wv.elts[3*r+1], C->Wv.elts[3*r+2]);   
         }else {
             fprintf (SUMA_STDERR,"%.20f;   %.20f;     \n", C->Wv.elts[2*r], C->Wv.elts[2*r+1]);   
         }
      }
      fprintf (SUMA_STDERR,"];\n");
      for (i=0; i<opt->N_ctrl_points; ++i) {
         fprintf (SUMA_STDERR,"Dot_Wv (%d) = %.24f \n"
                              "  W(%d) =  [%f;   %f;    %f]\n"
                              "  Control Point(%d) = [%f;   %f;    %f]\n", 
                                 i, SUMA_MT_DOT( (&(opt->CtrlPts_i[3*i])), (&(C->Wv.elts[3*i])) ),
                                 i, C->Wv.elts[3*i], C->Wv.elts[3*i+1], C->Wv.elts[3*i+2],
                                 i, opt->CtrlPts_i[3*i], opt->CtrlPts_i[3*i+1],  opt->CtrlPts_i[3*i+2]); 
      }

      /* Also send weights to output file for use in matlab. */
      fprintf(output_matrix, "Wv = [\n" );
      for (r=0; r<opt->N_ctrl_points; ++r) {
         if (opt->dim == 3) {
            fprintf (output_matrix,"  %11.20f;   %11.20f;   %11.20f;  \n",
                                  C->Wv.elts[3*r], C->Wv.elts[3*r+1], C->Wv.elts[3*r+2]);   
         }else {
             fprintf (output_matrix,"  %11.20f;   %11.20f;  \n", C->Wv.elts[2*r], C->Wv.elts[2*r+1]);   
         }
      }
      fprintf (output_matrix,"];\n \n");
   } 
   
   fclose (output_matrix); output_matrix = NULL;
  
   ++matrix_ncall;
   SUMA_RETURN(YUP);
}



SUMA_Boolean FindSplineWeights (MyCircle *C, MyCircleOpt *opt)
{
   static char FuncName[]={"FindSplineWeights"};
   vector Vv;
   static matrix M, Mi;
   double Mcr[3][3], nrm_cr[3] = {0.0, 0.0, 0.0};
   double check_a, check_cr[3] = {0.0, 0.0, 0.0}, tan_v_mag;
   int i, idm=0.0, k, r, c, nr, nc, i3, c3, r3, r4, f, row;
   double V_row1[3], V_row2[3], V_row3[3], nrm[3]={0.0,0.0,0.0};
   double  *t=NULL;
   double t_rc = 0.0, nrm_rc[3]={0.0,0.0,0.0};
   double tan_v[3]={0.0,0.0,0.0};  /* Meaning tangent velocity vector. Stores cross product when calculating given velocity. */
   double Vv_mag = 0.0, t_cr, expand_cr = 0.0; 
   static matrix *nullptr = NULL;
   double I[3][3], sI[3][3];
   
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (LocalHead) {
      fprintf(SUMA_STDERR, "%s:\n"
                           "Number of control points: %d\n", 
                     	    FuncName, opt->N_ctrl_points);
   }
   
   if (opt->dim !=2 && opt->dim != 3) {
      SUMA_S_Err("Stupid");
      SUMA_RETURN(NOPE);
   }
   
   t = (double *)SUMA_calloc(opt->N_ctrl_points, sizeof(double)); 
   
   if (opt->Dtheta) {
      SUMA_S_Err("Expecting no Dtheta yet!");
      SUMA_RETURN(NOPE);
   } else {
      opt->Dtheta = (double *)SUMA_calloc(opt->N_ctrl_points , sizeof (double));  /* angular displacement between t = 0 and t = 1 for each node.
                                                                                     One does not need to store for the algorithm but it makes
                                                                                     debugging easier. */
   }
   
   opt->Nrm = (double *)SUMA_calloc(opt->N_ctrl_points * 3 , sizeof (double));

   /* CALCULATE GIVEN VELOCITY AT CONTROL POINTS USING DESIRED ANGLE AND AXIS OF ROTATION. */
   vector_initialize (&Vv);
   if (opt->dot) { vector_create ((opt->dim*opt->N_ctrl_points + opt->N_ctrl_points*opt->N_ctrl_points), &Vv); }
      else { vector_create (opt->dim*opt->N_ctrl_points, &Vv); }
   
   for (i=0; i < opt->N_ctrl_points; ++i) {
      if(opt->dot) { idm = (opt->dim + opt->N_ctrl_points)*i; }
         else { idm = opt->dim*i; }
      i3 = 3*i;
      /* Distance between control point and desired destination of control point */ 
      /* nrm is the axis of rotation from intial control point location to final. */       
      /* BEWARE: USING _NC HERE, MEANING NO CENTER.  WILL THE CENTER ALWAYS BE THE ORIGIN? */
      SUMA_ANGLE_DIST_NC((&(opt->CtrlPts_f[i3])), (&(opt->CtrlPts_i[i3])), opt->Dtheta[i], (&(opt->Nrm[i3])) );

      if (LocalHead) {
         fprintf(SUMA_STDERR, "%s: Point %d, Dtheta = %.12f rad (%.12f deg)\n",
                                 FuncName, i, opt->Dtheta[i], SUMA_R2D(opt->Dtheta[i]));                            
      }

      SUMA_MT_CROSS(tan_v, (&(opt->Nrm[i3])), (&(opt->CtrlPts_i[i3])) );
      
      /* Need to normalize because using the direction (unit vector) of this tangent vector in the velocity calculation. */
      tan_v_mag = sqrt( tan_v[0]*tan_v[0] + tan_v[1]*tan_v[1] + tan_v[2]*tan_v[2] );
      if ( tan_v_mag > 0.000000001) {
         tan_v[0] = tan_v[0]/tan_v_mag;
         tan_v[1] = tan_v[1]/tan_v_mag;
         tan_v[2] = tan_v[2]/tan_v_mag; }
         
      /*if (LocalHead) {
         fprintf(SUMA_STDERR, "  TAN_V normalized = [ %.12f;    %.12f;    %.12f ]\n"
                              "  Dot product( tan_v, control point) = %.12f\n"
                              "  Dtheta = %f\n", 
                              tan_v[0], tan_v[1], tan_v[2],
                              SUMA_MT_DOT(tan_v, (&(opt->CtrlPts_i[i3])) ),
                              opt->Dtheta[i] ); } */
      
      Vv.elts[idm  ] = opt->Dtheta[i]*tan_v[0];
      Vv.elts[idm+1] = opt->Dtheta[i]*tan_v[1];
         if (opt->dim > 2) { Vv.elts[idm+2] = opt->Dtheta[i]*tan_v[2]; }
      
      /* Dot Product Restriction.  Since want tangent velocity contributions, need dot(radius, weight) = 0. */
      /* Need row with a zero for each control point. */
      if( opt->dot ) { 
         for(k=3; k < (opt->dim + opt->N_ctrl_points); ++k ) { Vv.elts[idm+k] = 0.00000; } }  
   }
   
   if( LocalHead ) {
      fprintf(SUMA_STDERR, "Given Velocity Vectors for control points:\n");
      for (i=0; i < opt->N_ctrl_points; ++i) {
         if (opt->dim == 2) {
            fprintf(SUMA_STDERR, "  %.18f    %.18f \n",
                                 Vv.elts[idm  ], Vv.elts[idm+1]); 
         } else {
            if(opt->dot ) { 
               idm = (opt->dim + opt->N_ctrl_points)*i; 
               fprintf(SUMA_STDERR, "  V(%d) = [ %f;   %f;   %f;    %f;    %f ] \n",
                                    i, Vv.elts[idm  ], Vv.elts[idm+1], Vv.elts[idm+2], Vv.elts[idm+3], Vv.elts[idm+4] );   
            } else { 
               idm = opt->dim*i; 
               fprintf(SUMA_STDERR, "  V(%d) = [ %f;   %f;   %f ] \n",
                                    i, Vv.elts[idm  ], Vv.elts[idm+1], Vv.elts[idm+2] ); 
            }
         }
      }
   }
   
   if( LocalHead ) {
      fprintf(SUMA_STDERR, "Dot product with radius:\n");
      for (i=0; i < opt->N_ctrl_points; ++i) {
         if(opt->dot ) { idm = (opt->dim + opt->N_ctrl_points)*i; }
         else { idm = opt->dim*i; } 
         fprintf(SUMA_STDERR, "  Dot(%d) = %.12f\n", i, SUMA_MT_DOT( (&(Vv.elts[idm])), (&(C->NewNodeList[ 3*opt->CtrlPts_iim[i]]))) );
      }
   }
 
   if( opt->dot ) { nr = opt->N_ctrl_points * (opt->dim + opt->N_ctrl_points); } /* Need extra rows for dot product. */
      else { nr = opt->dim * opt->N_ctrl_points; } /* number of rows */
   nc = opt->dim * opt->N_ctrl_points; /* number of columns */
   matrix_initialize(&M);
   matrix_create(nr, nc, &M);
   
   if (!M.elts) {
      SUMA_S_Crit("Failed to allocate");
      exit(1);
   }
   
   if( !opt->dot ) {
      for(r=0; r<opt->N_ctrl_points; ++r) {        /* r for row. For the first three rows, r represents the first control point. */
            for(c=0; c<opt->N_ctrl_points; ++c) {  /* c for column. While r is held constant, c cycles through all control points
                                                      so that for the first 3 rows (first 3 C.P.) are compared to all the rest.  */
               c3 = 3*c; r3 = 3*r;

               /* Create the rotation matrix. */
               SUMA_3D_Rotation_Matrix( (&(opt->CtrlPts_i[c3])), (&(opt->CtrlPts_i[r3])), Mcr, t_cr, nrm_cr);
          
               expand_cr = exp(-0.5*t_cr*t_cr);    
               
               /* Assemble the matrix and include the expansion factor. */
               #if 0
               if(LocalHead) {
                  if( r == 0 || r == 1 ) {
                     fprintf(SUMA_STDERR, "ROTATION MATRIX: \n"
                                          "  p1 = [ %f      %f    %f ] \n"
                                          "  p2 = [ %f      %f    %f ] \n"
                                          "     alpha(%d, %d) = %.12f  \n"
                                          "     u (%d, %d) = [ %.12f    %.12f    %.12f ] \n"
                                          "     expansion = %f \n",
                                          opt->CtrlPts_i[c3  ], opt->CtrlPts_i[c3+1], opt->CtrlPts_i[c3+2],
                                          opt->CtrlPts_i[r3  ], opt->CtrlPts_i[r3+1], opt->CtrlPts_i[r3+2],                                
                                          r, c, t_cr, 
                                          r, c, nrm_cr[0], nrm_cr[1], nrm_cr[2],
                                          expand_cr ); } }
               #endif 
              
               if( opt->sigma && r == c) {   /* Need to add sigma squared times the identity matrix to each 3X3. */
                  M.elts[ (r3  ) ][ (c3  ) ] = expand_cr * Mcr[0][0] + (opt->sigma)*(opt->sigma);
                  M.elts[ (r3  ) ][ (c3+1) ] = expand_cr * Mcr[0][1];
                  M.elts[ (r3  ) ][ (c3+2) ] = expand_cr * Mcr[0][2];
                  M.elts[ (r3+1) ][ (c3  ) ] = expand_cr * Mcr[1][0];
                  M.elts[ (r3+1) ][ (c3+1) ] = expand_cr * Mcr[1][1] + (opt->sigma)*(opt->sigma);
                  M.elts[ (r3+1) ][ (c3+2) ] = expand_cr * Mcr[1][2];
                  M.elts[ (r3+2) ][ (c3  ) ] = expand_cr * Mcr[2][0];
                  M.elts[ (r3+2) ][ (c3+1) ] = expand_cr * Mcr[2][1];
                  M.elts[ (r3+2) ][ (c3+2) ] = expand_cr * Mcr[2][2] + (opt->sigma)*(opt->sigma); 
               } else {
                  M.elts[ (r3  ) ][ (c3  ) ] = expand_cr * Mcr[0][0];
                  M.elts[ (r3  ) ][ (c3+1) ] = expand_cr * Mcr[0][1];
                  M.elts[ (r3  ) ][ (c3+2) ] = expand_cr * Mcr[0][2];
                  M.elts[ (r3+1) ][ (c3  ) ] = expand_cr * Mcr[1][0];
                  M.elts[ (r3+1) ][ (c3+1) ] = expand_cr * Mcr[1][1];
                  M.elts[ (r3+1) ][ (c3+2) ] = expand_cr * Mcr[1][2];
                  M.elts[ (r3+2) ][ (c3  ) ] = expand_cr * Mcr[2][0];
                  M.elts[ (r3+2) ][ (c3+1) ] = expand_cr * Mcr[2][1];
                  M.elts[ (r3+2) ][ (c3+2) ] = expand_cr * Mcr[2][2]; 
               }         
            }
         }
      }
   
   if( opt->dot ) {
      for(r=0; r<opt->N_ctrl_points; ++r) {     /* r for row.  For the first three rows, r represents the first control point.*/
         for(c=0; c<opt->N_ctrl_points; ++c) {  /* c for column. While r is held constant, c cycles through all control points
                                                   so that for the first 3 rows (first 3 C.P.) are compared to all the rest.  */
            row = (3 + opt->N_ctrl_points)*r;   /* Need extra rows for dot product restriction. */
            c3 = 3*c; r3 = 3*r;
            
            /* Create the rotation matrix. */
            SUMA_3D_Rotation_Matrix( (&(opt->CtrlPts_i[c3])), (&(opt->CtrlPts_i[r3])), Mcr, t_cr, nrm_cr );
            
            expand_cr = exp( -0.5*t_cr*t_cr ); 
            
            /* Assemble the matrix and include the expansion factor. */
            #if 0
            if(LocalHead) {
               if( r == 0 ) {
                  fprintf(SUMA_STDERR, "ROTATION MATRIX: \n"
                                       "  p1 = [ %f      %f    %f ] \n"
                                       "  p2 = [ %f      %f    %f ] \n"
                                       "     alpha(%d, %d) = %.12f  \n"
                                       "     u (%d, %d) = [ %.12f    %.12f    %.12f ] \n"
                                       "     expansion = %f \n",
                                       opt->CtrlPts_i[c3  ], opt->CtrlPts_i[c3+1], opt->CtrlPts_i[c3+2],
                                       opt->CtrlPts_i[r3  ], opt->CtrlPts_i[r3+1], opt->CtrlPts_i[r3+2],                                
                                       r, c, t_cr, 
                                       r, c, nrm_cr[0], nrm_cr[1], nrm_cr[2],
                                       expand_cr ); } }
            #endif
            
            M.elts[ (row  ) ][ (c3  ) ] = expand_cr * Mcr[0][0];
            M.elts[ (row  ) ][ (c3+1) ] = expand_cr * Mcr[0][1];
            M.elts[ (row  ) ][ (c3+2) ] = expand_cr * Mcr[0][2];
            M.elts[ (row+1) ][ (c3  ) ] = expand_cr * Mcr[1][0];
            M.elts[ (row+1) ][ (c3+1) ] = expand_cr * Mcr[1][1];
            M.elts[ (row+1) ][ (c3+2) ] = expand_cr * Mcr[1][2];
            M.elts[ (row+2) ][ (c3  ) ] = expand_cr * Mcr[2][0];
            M.elts[ (row+2) ][ (c3+1) ] = expand_cr * Mcr[2][1];
            M.elts[ (row+2) ][ (c3+2) ] = expand_cr * Mcr[2][2]; 
            
            for(k=3; k < (3 + opt->N_ctrl_points); ++k) {
               if( k-3 == c && k-c == 3) {
                  M.elts[ (row+k) ][ (c3  ) ] = (opt->CtrlPts_i[r3  ] * (expand_cr * Mcr[0][0]) )
                                              + (opt->CtrlPts_i[r3+1] * (expand_cr * Mcr[1][0]) )
                                              + (opt->CtrlPts_i[r3+2] * (expand_cr * Mcr[2][0]) ); 
                  M.elts[ (row+k) ][ (c3+1) ] = (opt->CtrlPts_i[r3  ] * (expand_cr * Mcr[0][1]) )
                                              + (opt->CtrlPts_i[r3+1] * (expand_cr * Mcr[1][1]) )
                                              + (opt->CtrlPts_i[r3+2] * (expand_cr * Mcr[2][1]) );
                  M.elts[ (row+k) ][ (c3+2) ] = (opt->CtrlPts_i[r3  ] * (expand_cr * Mcr[0][2]) )
                                              + (opt->CtrlPts_i[r3+1] * (expand_cr * Mcr[1][2]) )
                                              + (opt->CtrlPts_i[r3+2] * (expand_cr * Mcr[2][2]) ); } 
               else {
                  M.elts[ (row+k) ][ (c3  ) ] = 0.0;
                  M.elts[ (row+k) ][ (c3+1) ] = 0.0;
                  M.elts[ (row+k) ][ (c3+2) ] = 0.0; }
            }   
         }
      }  
   }
   
   if( LocalHead ) Debug_Weights(C, opt, M, Mi, Vv);
 
   SUMA_LH("Calculating inverse...");
   matrix_initialize(&Mi);
   matrix_create(nr, nc, &Mi);
   matrix_psinv (M, nullptr, &Mi); 
   SUMA_LH("   Done."); 

   if( LocalHead ) Debug_Weights(C, opt, M, Mi, Vv);
   fprintf(SUMA_STDERR, "Done printing inverse matrix.");

   SUMA_LH("Calculating weights...");
   vector_initialize(&(C->Wv));
   vector_multiply(Mi, Vv, &(C->Wv));
   SUMA_LH("   Done."); 
   
   if( LocalHead ) Debug_Weights(C, opt, M, Mi, Vv);
   
   if (t) SUMA_free(t); t=NULL;
   matrix_destroy (&M);
   matrix_destroy (&Mi);
   vector_destroy (&Vv);
   
   opt->Dtheta = NULL;
      
   SUMA_RETURN(YUP);   
}

SUMA_Boolean Velocity( MyCircle *C, MyCircleOpt *opt) 
{
   static char FuncName[]={"Velocity"};
   byte repeat;   
   int i, i3, j, j3, r;
   static double v_alpha = 0.0, v_cr[3]={0.0,0.0,0.0}, v_expand, cr_mag; 
   vector Wr;  /*W rotated*/
   static double v_M[3][3];
 
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   /* Compute velocity at each node using contributions from the control points. */
   vector_initialize (&Wr);
   vector_create (3*opt->N_ctrl_points, &Wr);
   
   if (LocalHead) { 
      fprintf(SUMA_STDERR, "******************************************************\n"
                           "%s: CHECKING THE FUNCTION, First control point: \n", FuncName); }
   
   for (i=0; i< C->N_Node; ++i) {  /* i for all points, j for the control points. */
      i3 = 3*i;

      for (j=0; j<opt->N_ctrl_points; ++j){  
         j3 = 3*j;
        
         #if 0
         /* Need to comment out when using SUMA_3D_Rotation_Matrix to calculate initial velocity field. */
         SUMA_ANGLE_DIST_NC( (&(C->NewNodeList[i3])), (&(opt->CtrlPts_i[j3])), v_alpha, v_cr );
        
         cr_mag = sqrt( v_cr[0]*v_cr[0] + v_cr[1]*v_cr[1] + v_cr[2]*v_cr[2]); 
         if (LocalHead) { 
            if( i == opt->CtrlPts_iim[0] ) { fprintf(SUMA_STDERR, "Magnitude of Cross Product = %.12f \n", cr_mag); }} 
   
         if(cr_mag > 0.000000001){ v_cr[0] = v_cr[0]/cr_mag; v_cr[1] = v_cr[1]/cr_mag; v_cr[2] = v_cr[2]/cr_mag; }  
         
         if (LocalHead) {
            if (i == opt->CtrlPts_iim[0]) {
               fprintf(SUMA_STDERR, "Using SUMA_ROTATE_ABOUT AXIS to calculate velocity field.\n" ); } }
         
         /* ROTATE THE WEIGHTS USING DISPLACEMENT ANGLE AND AXIS OF ROTATION CALCULATED ABOVE. */
         SUMA_ROTATE_ABOUT_AXIS( (&(C->Wv.elts[j3])), v_cr, v_alpha, (&(Wr.elts[j3])) );       
                            
         v_expand = exp( -0.5*v_alpha*v_alpha ); 
         #endif
        
         SUMA_3D_Rotation_Matrix( (&(opt->CtrlPts_i[j3])), (&(C->NewNodeList[i3])), v_M, v_alpha, v_cr);
         
         v_expand = exp( -0.5*v_alpha*v_alpha );   
       
         if(LocalHead) { 
            if (i == opt->CtrlPts_iim[0]) {
               fprintf(SUMA_STDERR, "AFTER ROTATION MATRIX CALCULATED: \n"
                                    "  v_alpha = %f\n"
                                    "  expansion = %.5f\n"
                                    "  p1 = [ %f     %f    %f ]; \n"
                                    "  p2 = [ %f     %f    %f ]; \n"
                                    "  Angle to be rotated = %f \n"
                                    "  Axis of rotation: \n"
                                    "     %.12f    %.12f    %.12f \n"
                                    "  Weight = %f %f %f\n", 
                                    v_alpha, v_expand,
                                    opt->CtrlPts_i[j3  ], opt->CtrlPts_i[j3+1], opt->CtrlPts_i[j3+2],
                                    C->NewNodeList[i3  ], C->NewNodeList[i3+1], C->NewNodeList[i3+2],
                                    v_alpha, 
                                    v_cr[0], v_cr[1], v_cr[2],
                                    C->Wv.elts[j3], C->Wv.elts[j3+1], C->Wv.elts[j3+2]); 
            }
            if(i == opt->CtrlPts_iim[0]) {
               fprintf(SUMA_STDERR, "%s:\n"
                                    "v_M = [\n", FuncName);
               for (r=0; r<3; ++r) {  
                     fprintf (SUMA_STDERR,"  %.5f   %.5f   %.5f \n", 
                                             v_expand*v_M [ (r) ][ (0) ],
                                             v_expand*v_M [ (r) ][ (1) ],
                                             v_expand*v_M [ (r) ][ (2) ]);
               }
               fprintf(SUMA_STDERR, "];\n");
            }
         }
          
         Wr.elts[j3  ] = (v_M[0][0]*C->Wv.elts[j3  ] + v_M[0][1]*C->Wv.elts[j3+1] + v_M[0][2]*C->Wv.elts[j3+2]);
         Wr.elts[j3+1] = (v_M[1][0]*C->Wv.elts[j3  ] + v_M[1][1]*C->Wv.elts[j3+1] + v_M[1][2]*C->Wv.elts[j3+2]);
         Wr.elts[j3+2] = (v_M[2][0]*C->Wv.elts[j3  ] + v_M[2][1]*C->Wv.elts[j3+1] + v_M[2][2]*C->Wv.elts[j3+2]); 

         /* Check rotated weights. */
         if(LocalHead) { 
            if(i == opt->CtrlPts_iim[0]) {
               fprintf(SUMA_STDERR, "%s:  \n"
                                    "  W rotated = %f     %f    %f \n"
                                    "  expansion factor = %f \n", 
                                    FuncName, Wr.elts[j3], Wr.elts[j3+1], Wr.elts[j3+2], v_expand); } 
            if(i == opt->CtrlPts_iim[0]) {
               fprintf(SUMA_STDERR, "  Dot Product of Rotated Weights with Radius = %.12f \n"
                                    "  Mag_Wv_rotated = %f \n", 
                                    SUMA_MT_DOT( (&(Wr.elts[j3])), (&(C->NewNodeList[i3])) ), 
                                    sqrt(   SUMA_POW2(C->Wv.elts[j3  ]) 
                                          + SUMA_POW2(C->Wv.elts[j3+1]) 
                                          + SUMA_POW2(C->Wv.elts[j3+2]) ) ); 
            }
         }
         
         Wr.elts[j3  ] *= v_expand;
         Wr.elts[j3+1] *= v_expand;
         Wr.elts[j3+2] *= v_expand;
         
         if(opt->sigma) { 
            Wr.elts[j3  ] += SUMA_POW2( opt->sigma )*C->Wv.elts[j3  ];
            Wr.elts[j3+1] += SUMA_POW2( opt->sigma )*C->Wv.elts[j3+1];
            Wr.elts[j3+2] += SUMA_POW2( opt->sigma )*C->Wv.elts[j3+2];
         }
         
         if(LocalHead) {
            if(i == opt->CtrlPts_iim[0]) {
               fprintf(SUMA_STDERR, "%s:\n"
                                    "v_M = [\n", FuncName);
               for (r=0; r<3; ++r) {  
                     fprintf (SUMA_STDERR,"  %.5f   %.5f   %.5f \n", 
                                             v_expand*v_M [ (r) ][ (0) ],
                                             v_expand*v_M [ (r) ][ (1) ],
                                             v_expand*v_M [ (r) ][ (2) ]);
               }
               fprintf(SUMA_STDERR, "];\n");
            }
         }
         
         if(LocalHead) { 
            if( i == opt->CtrlPts_iim[0] ) {
               fprintf(SUMA_STDERR, "Velocity Field Components as stored in Wr.elts: \n"
                                    "     %f    %f    %f \n",
                                   Wr.elts[j3  ], Wr.elts[j3+1], Wr.elts[j3+2]); } 
         }
   
         C->VelocityField[i3  ] += Wr.elts[j3  ];
         C->VelocityField[i3+1] += Wr.elts[j3+1];
         C->VelocityField[i3+2] += Wr.elts[j3+2]; 
      }
   
      if(LocalHead) { 
         if(i == opt->CtrlPts_iim[0]) {
            fprintf(SUMA_STDERR, "%s: Initial Velocity Field, no adjustment: \n"
                                 "     %f    %f    %f \n",
                                 FuncName, C->VelocityField[i3  ], C->VelocityField[i3+1], C->VelocityField[i3+2]); }

         if(i == opt->CtrlPts_iim[0]) {
               fprintf(SUMA_STDERR, "  Dot Product of Velocity Field with Radius = %.12f \n",
                                    SUMA_MT_DOT( (&(C->VelocityField[i3])), (&(C->NewNodeList[i3])) ) ); }
      }
   }
      
   vector_destroy (&Wr);

   SUMA_RETURN(YUP);
}

SUMA_Boolean Debug_Move( MyCircle *C, MyCircleOpt *opt, SUMA_SurfaceObject *SO, double dt, int niter, int a_niter, int first_bad_niter) 
{
   static char FuncName[]={"Debug_Move"};
   int i, i3, j;
   char outfile[] = {"Coords_0.txt"}, outfile_speed[] = {"Plot_Speed0.txt"}, outfile_test[50]; 
   char outfile_segments[50], outfile_Vmag[50], outfile_tri_area[50];
   static double sideA[3], sideB[3], sideC[3], height[3], side[3], t_area;
   static int f, g, h, f3, g3, h3;
   static double Vf_segment[3];
   static float Point_at_Distance[2][3] = { {0.0, 0.0, 0.0},{ 0.0, 0.0, 0.0} }, V_Mag = 0.0;
   
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   if(LocalHead) { fprintf(stderr,"%s: niter = %d, dt = %.3f.\n", FuncName, niter, dt); }
   
   /* Test_move.1D -- Write nodelist and velocity field to file. */
   if(!first_bad_niter || niter < first_bad_niter+5){ 
      if(!a_niter) { /* For now, only write to file when niter changes, not when a_niter changes. */
         FILE *test = NULL;
         sprintf(outfile_test, "%s%d.1D", opt->outfile, (niter));
         test = fopen (outfile_test, "w"); 
         fprintf (test, "col#0: Node Index\n"
                        "col#1,2,3: Node Coordinates at Beginning of Iteration.\n"
                        "col#4,5,6: Calculated Velocity Field to be Used in this Iteration.\n"
                        "col#7: Step Size Used in the Move. (magnitude of velocity*dt)\n"
                        "     dt = %f     Niter = %d\n", dt, niter );  
         for (i = 0; i < C->N_Node; ++i) {
            i3 = 3*i;
            /* Calculate magnitude of step size.  Storing in C->Theta[i3+2] because this array has already been created
               and is not being used for anything. Written to fourth column of test_move.1D file. */
            C->Theta[i3+2] = sqrt(  SUMA_POW2(dt*C->VelocityField[i3  ]) + 
                                    SUMA_POW2(dt*C->VelocityField[i3+1]) +                  
                                    SUMA_POW2(dt*C->VelocityField[i3+2]) ); 
            fprintf (test, "%d   %11.8f  %11.8f  %11.8f  %11.8f  %11.8f  %11.8f  %11.8f\n", 
               i, C->NewNodeList[i3  ], C->NewNodeList[i3+1], C->NewNodeList[i3+2], 
               C->VelocityField[i3  ], C->VelocityField[i3+1], C->VelocityField[i3+2], C->Theta[i3+2]);
         } 
         fclose (test); test = NULL;   
      } 
  
      /* For calculating base and height of each facet and step size.  Base and Height of the triangle
         are the minimum distances when considering the movement of the nodes that form the vertices of the triangle.
         Need triangles to not flip, so must check to see that move (step size) is not greater than the 
         distance across the triangle, which is at least the length of the base or height. */
      if(!first_bad_niter || niter < first_bad_niter+5){ 
         FILE *tri_area = NULL;
         sprintf(outfile_tri_area, "tri_area%d.1D", niter);
         tri_area = fopen (outfile_tri_area, "w"); 
         fprintf (tri_area, "col#0: Facet Index\n"
                        "col#1: Triangle base.\n"
                        "col#2: Triangle height.\n"
                        "col#3: Move distance/size of step for the three nodes of the facet.\n"
                        "       Nodes of facet listed in random order.\n"
                        "col#4: Triangle area.\n"
                        "dt = %11f\n", dt);
         for (i = 0; i < SO->N_FaceSet; ++i) {
            i3 = 3*i;
            f = SO->FaceSetList[i3  ];
            g = SO->FaceSetList[i3+1];
            h = SO->FaceSetList[i3+2];
            f3 = 3*f; g3 = 3*g, h3 = 3*h; 

            SUMA_TRI_AREA( (&(C->NewNodeList[f3])), (&(C->NewNodeList[g3])), (&(C->NewNodeList[h3])), t_area );

            /* Determine the vectors that makes up the sides of the triangle by subtracting node locations.*/
            SUMA_MT_SUB( sideA, (&(C->NewNodeList[f3])), (&(C->NewNodeList[g3])) );
            SUMA_MT_SUB( sideB, (&(C->NewNodeList[f3])), (&(C->NewNodeList[h3])) );
            SUMA_MT_SUB( sideC, (&(C->NewNodeList[g3])), (&(C->NewNodeList[h3])) );

            /* Find the length of the side by finding the magnitude of the side vectors. 
               These values represent the possible bases for calculating the area of a triangle 
                  if area = 0.5*base*height. */
            side[0] = sqrt( SUMA_POW2(sideA[0]) + SUMA_POW2(sideA[1]) + SUMA_POW2(sideA[2]) );
            side[1] = sqrt( SUMA_POW2(sideB[0]) + SUMA_POW2(sideB[1]) + SUMA_POW2(sideB[2]) );
            side[2] = sqrt( SUMA_POW2(sideC[0]) + SUMA_POW2(sideC[1]) + SUMA_POW2(sideC[2]) );

            /* Calculate the possible heights; the perpendicular bisectors of the triangle; 
               the minimal distance across the triangle. */             
            height[0] = (2*t_area) / side[0];
            height[1] = (2*t_area) / side[1];
            height[2] = (2*t_area) / side[2];

            fprintf(tri_area, "%d   %f    %f    %f    %f\n"
                              "     %f    %f    %f\n"
                              "     %f    %f    %f\n", i,
                              side[0], height[0], C->Theta[f3+2], t_area, 
                              side[1], height[1], C->Theta[g3+2], 
                              side[2], height[2], C->Theta[h3+2] );
         }
         fclose(tri_area); tri_area = NULL;
      } 

      /* TO PLOT VELOCITY FIELD AT ANY ITERATION. */
      /* Write oriented segment file for plotting in SUMA. */
      if(!first_bad_niter || niter < first_bad_niter+5) { 
         FILE *plot_segments = NULL;
         sprintf(outfile_segments, "SUMA_segments%d.1D.dset", niter); 
         plot_segments = fopen (outfile_segments, "w"); 
         fprintf (plot_segments, "#oriented_segments\n");
         for (i = 0; i < C->N_Node; ++i) {
            i3 = 3*i;
            /* To plot end of segment, must calculate the location of the point of the velocity vector. 
               This is done by adding the position vector of the node to the velocity vector at that node. */
            Vf_segment[0] = C->VelocityField[i3  ];
            Vf_segment[1] = C->VelocityField[i3+1];
            Vf_segment[2] = C->VelocityField[i3+2];

            Vf_segment[0] = C->NewNodeList[i3  ] + 0.05*Vf_segment[0];
            Vf_segment[1] = C->NewNodeList[i3+1] + 0.05*Vf_segment[1];
            Vf_segment[2] = C->NewNodeList[i3+2] + 0.05*Vf_segment[2];

            fprintf (plot_segments, "%11.8f  %11.8f  %11.8f  %11.8f  %11.8f  %11.8f 0.0  0.0  1.0  1.0 1.5\n",
                                    C->NewNodeList[i3], C->NewNodeList[i3+1], C->NewNodeList[i3+2], 
                                    Vf_segment[0], Vf_segment[1], Vf_segment[2] );
         }
         fclose (plot_segments); plot_segments = NULL; 
      } 

      /* Write velocity magnitudes to file for plotting in SUMA. */  
      FILE *plot_Vmag = NULL;
      sprintf(outfile_Vmag, "SUMA_Vmag%d.1D.dset", niter); 
      plot_Vmag = fopen (outfile_Vmag, "w"); 
      for (i = 0; i < C->N_Node; ++i) {
         i3 = 3*i;
         fprintf (plot_Vmag, "%11.8f\n", sqrt(  SUMA_POW2(C->VelocityField[i3  ]) + 
                                                SUMA_POW2(C->VelocityField[i3+1]) + 
                                                SUMA_POW2(C->VelocityField[i3+2]) ));
      }
      fclose (plot_Vmag); plot_Vmag = NULL;

   }
   
   /* Send output to files for graphing in matlab. */
   if(LocalHead) {
      if(opt->dom_dim == 2) {
         FILE *plot_circle = NULL; 
            sprintf(outfile, "Coords_%d.txt", (niter+1));
            plot_circle = fopen (outfile, "w");   
         for (i = 0; i < C->N_Node; ++i) {
            i3 = 3*i;
            fprintf (plot_circle, "%11.8f  %11.8f  %11.8f\n", C->NewNodeList[i3], C->NewNodeList[i3+1], C->NewNodeList[i3+2]);
         }
         fclose (plot_circle); plot_circle = NULL; 
      
         /* Create file for plotting magnitude of velocity field around the circle. */
         FILE *plot_speed = NULL; 
         sprintf(outfile_speed, "Plot_Speed%d.txt", (niter+1));
         plot_speed = fopen(outfile_speed, "w");
         for (i = 0; i < C->N_Node; ++i) {
            i3 = 3*i;
            V_Mag = 0.5*sqrt(  SUMA_POW2(C->VelocityField[i3  ]) +  
                               SUMA_POW2(C->VelocityField[i3+1]) + 
                               SUMA_POW2(C->VelocityField[i3+2])  );
            /* Using half the magnitude to make matlab plot easier to see. */
            SUMA_POINT_AT_DISTANCE_NORM( (&(C->NewNodeList[i3])), (&(C->NewNodeList[i3])), V_Mag, Point_at_Distance );
            fprintf(plot_speed, "%11.8f   %11.8f   %11.8f \n", 
                    Point_at_Distance[0][0], Point_at_Distance[0][1], Point_at_Distance[0][2]);
         }
         fclose(plot_speed); plot_speed = NULL;    
      }
   }
  
   SUMA_RETURN(YUP);
}
 
SUMA_Boolean Neighbor( MyCircle *C, MyCircleOpt *opt, SUMA_SurfaceObject *SO, int niter, int a_niter) 
{
   static char FuncName[]={"Neighbor"};
   char outfile_neighb[50];
   int k, i, j, s, i3, j3, s4;
   static double distance = 0.0;
   int close_neighb = 0;
   
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY; 

   if(LocalHead) fprintf(SUMA_STDERR,"%s: Begin loop for checking neighbors.\n", FuncName);
   
   FILE *neighb = NULL;
   if(opt->neighb_adjust) { sprintf(outfile_neighb, "Neighbor_Check%d_%d.txt", niter, a_niter); }
   else  { sprintf(outfile_neighb, "Neighbor_Check%d.txt", niter); }
   neighb = fopen (outfile_neighb, "w");  

   for(i = 0; i < C->N_Node; ++i) {  
      s4 = 4*i;
      fprintf(neighb, "Node = %d, Stepsize = %f\n", i, C->Vf_Step[s4+3]);
      for(k=0; k < SO->FN->N_Neighb[i]; ++k) {   
         j = SO->FN->FirstNeighb[i][k];  /*Index of node neighbor.*/
         i3 = 3*i;
         j3 = 3*j;
         distance = sqrt(  SUMA_POW2(C->NewNodeList[j3  ] - C->NewNodeList[i3  ]) + 
                           SUMA_POW2(C->NewNodeList[j3+1] - C->NewNodeList[i3+1]) + 
                           SUMA_POW2(C->NewNodeList[j3+2] - C->NewNodeList[i3+2]) ); 
         /* Consider keeping steps smaller than one half of the distance. */
         distance = 0.5*distance; 

         fprintf(neighb,"     Neighbor(%d) = %f\n", j, distance); 
         if(C->Vf_Step[s4+3] > distance) {
            fprintf(neighb,"     Stepsize too big! Need to adjust.\n");
            if(!close_neighb) close_neighb = i;
            if(LocalHead) {
               fprintf(SUMA_STDERR, "%s:  Stepsize too big! Node = %d, Neighbor = %d\n"
                                    "     distance to nearest node = %f\n"
                                    "     step size = %f\n",
                                    FuncName, i, j, distance, C->Vf_Step[s4+3] ); 
            }
         }
         
         if(opt->neighb_adjust) {
            if(C->Vf_Step[s4+3] > distance)  close_neighb = i; break; 
         }
      }
      if(opt->neighb_check) fprintf(neighb,"**********************************\n");
      if(opt->neighb_adjust && close_neighb) break;
   }
   fclose (neighb); neighb = NULL;

   if(LocalHead) fprintf(SUMA_STDERR,"%s: End loop for checking neighbors.\n", FuncName);

   if(opt->neighb_adjust) SUMA_RETURN(close_neighb);
   else SUMA_RETURN(YUP);
}

SUMA_Boolean Calculate_Step (MyCircle *C, MyCircleOpt *opt, double dt) 
{
   static char FuncName[]={"Calculate_Step"};
   static int i, i3, s, s4;
   static double um;
   
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY; 

   /* Calculate step size using dt set at beginning of this loop and make step magnitude adjustment. */
   for (i = 0; i < C->N_Node; ++i) {  
      i3 = i*3;
      s4 = i*4; 

      C->Vf_Step[s4  ] = 0.0;
      C->Vf_Step[s4+1] = 0.0;
      C->Vf_Step[s4+2] = 0.0;

      C->Vf_Step[s4  ] = C->VelocityField[i3  ] * dt; 
      C->Vf_Step[s4+1] = C->VelocityField[i3+1] * dt; 
      C->Vf_Step[s4+2] = C->VelocityField[i3+2] * dt;

      if(LocalHead) {
         if(i == opt->CtrlPts_iim[0]) { /*  debug when node is 1st control point */
            fprintf(SUMA_STDERR, "%s, first control point,before adjustment: \n"
                                 "   Node = %d\n"
                                 "   dot(u,rad)  = [%.18f]\n"
                                 "   u(%d) = Vf*dt = [%f  %f   %f]\n"
                                 "   mag(u) = %.8f\n", 
                                 FuncName, i, SUMA_MT_DOT( (&(C->Vf_Step[s4])),(&(C->NewNodeList[i3]))), 
                                 i, C->Vf_Step[s4  ], C->Vf_Step[s4+1], C->Vf_Step[s4+2],
                                 sqrt( SUMA_POW2(C->Vf_Step[s4  ])+SUMA_POW2(C->Vf_Step[s4+1])+SUMA_POW2(C->Vf_Step[s4+2]) ) ); 
         }
      }                                     

      if (opt->adjust) {
         /* must turn the magnitude of u to that of uc, where |uc| / |u| = tan(a) / a; 
            since for unit circle/sphere a = |u|, |uc| = tan(a) */
         /* um = |u| */
         um = sqrt(  SUMA_POW2(C->Vf_Step[s4  ]) + 
                     SUMA_POW2(C->Vf_Step[s4+1]) + 
                     SUMA_POW2(C->Vf_Step[s4+2]));
         /* rescale |u| to make it |uc| */
         if (um > 0.0000001){
            C->Vf_Step[s4  ] *=  tan(um)/um;
            C->Vf_Step[s4+1] *=  tan(um)/um;            
            C->Vf_Step[s4+2] *=  tan(um)/um;            
         }             
      }

      /* Find the magnitude of the step.  This is the step size. */
      C->Vf_Step[s4+3] = sqrt(   SUMA_POW2(C->Vf_Step[s4  ]) + 
                                 SUMA_POW2(C->Vf_Step[s4+1]) + 
                                 SUMA_POW2(C->Vf_Step[s4+2]) );
   }
   
   SUMA_RETURN(YUP);
}

SUMA_Boolean Move_Points (MyCircle *C, MyCircleOpt *opt) 
{
   static char FuncName[]={"Move_Points"};
   static int i, i3, s, s4;
   static double mv_mag, mv_alpha, mv_nrm[3], mv_nrm_mag, newnode_mag;
   
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY; 
   
   /* MOVE THE POINTS USING ROTATION MACRO. */
   for (i = 0; i < C->N_Node; ++i) {  
      s4 = 4*i;
      i3 = 3*i;
      mv_mag = sqrt( SUMA_POW2(C->Vf_Step[s4  ]) +
                     SUMA_POW2(C->Vf_Step[s4+1]) +
                     SUMA_POW2(C->Vf_Step[s4+2]) );
      if( mv_mag > 0.000000001) { mv_alpha = atan( mv_mag ); }
      else { mv_alpha = 0.0; }
      SUMA_MT_CROSS ( mv_nrm,(&(C->NewNodeList[i3])), (&(C->Vf_Step[s4])) ); 
      mv_nrm_mag = sqrt( mv_nrm[0]*mv_nrm[0] + mv_nrm[1]*mv_nrm[1] +  mv_nrm[2]*mv_nrm[2] );
      if (mv_nrm_mag > 0.000000001) {
         mv_nrm[0] = mv_nrm[0]/mv_nrm_mag; mv_nrm[1] = mv_nrm[1]/mv_nrm_mag;  mv_nrm[2] = mv_nrm[2]/mv_nrm_mag; }

      /* Move the points a small step using the Rotation macro. */
      SUMA_ROTATE_ABOUT_AXIS( (&(C->NewNodeList[i3])), (&(mv_nrm[0])), mv_alpha, (&(C->NewNodeList[i3])) );

      /* Project point back onto the circle. */
      newnode_mag = sqrt(  SUMA_POW2(C->NewNodeList[i3  ]) + 
                           SUMA_POW2(C->NewNodeList[i3+1]) + 
                           SUMA_POW2(C->NewNodeList[i3+2]) );
      if (newnode_mag > 0.000000001) {
         C->NewNodeList[i3  ] = opt->Radius*(C->NewNodeList[i3  ])/( newnode_mag );
         C->NewNodeList[i3+1] = opt->Radius*(C->NewNodeList[i3+1])/( newnode_mag );
         C->NewNodeList[i3+2] = opt->Radius*(C->NewNodeList[i3+2])/( newnode_mag ); }
   }
   /* END OF MOVE. */
      
   #if 0      
   /* OLD MOVE METHOD. MOVE ALONG VELOCITY VECTOR. */ 
   for (i = 0; i < C->N_Node; ++i) {  
      i3 = 3*i;  
      C->NewNodeList[i3  ] = C->NewNodeList[i3  ] + C->Vf_Step[s4  ];
      C->NewNodeList[i3+1] = C->NewNodeList[i3+1] + C->Vf_Step[s4+1];
      C->NewNodeList[i3+2] = C->NewNodeList[i3+2] + C->Vf_Step[s4+2];

      newnode_mag = sqrt(  SUMA_POW2(C->NewNodeList[i3  ]) + 
                           SUMA_POW2(C->NewNodeList[i3+1]) + 
                           SUMA_POW2(C->NewNodeList[i3+2]) );

      /*if (opt->dbg_flag) { fprintf(stderr,"mag initial: %f\n", mag); } */
      if ( newnode_mag > 0.000000001 ) {
         C->NewNodeList[i3  ] = opt->Radius * (C->NewNodeList[i3  ])/( newnode_mag );
         C->NewNodeList[i3+1] = opt->Radius * (C->NewNodeList[i3+1])/( newnode_mag );
         C->NewNodeList[i3+2] = opt->Radius * (C->NewNodeList[i3+2])/( newnode_mag ); }  
   } 
   /* END OLD METHOD. */ 
   #endif
   
   SUMA_RETURN(YUP);
}
      
int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"toy_circle"}; 
   char outfile_SphereQuality[50];
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt;  
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   MyCircleOpt myopt, *opt = NULL;
   int i, i3, idm, j, j3, k, adj_factor = 10, s4;
   double dt;    
   double oxyz[3]={0.0, 0.0, 0.0};
   double oda, faa, error, dtheta=0.0, nrmi[3]={0.0, 0.0, 0.0}, nrmf[3]={0.0, 0.0, 0.0};
   int niter=0, a_niter, first_bad_niter = 0;
   SUMA_SurfaceObject *SO = NULL;
   void * SO_name;
   char *shist=NULL;
   int nbad;
   int too_close = 0, need_neighb_adjust = 0, need_more_adjustment = 0;
   static double energy, energy_sum, time_elapsed;
   SUMA_Boolean exists;
      
   MyCircle *Ci = NULL;
   vector Wv;
   
   SUMA_Boolean LocalHead = NOPE;

	SUMA_mainENTRY;
   SUMA_STANDALONE_INIT;

   /* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "-o;-talk;");
   
   if (argc < 2) {
      usage_toy_circle(ps);
      exit (1);
   }
   
   Opt = SUMA_toy_circle_ParseInput (argv, argc, ps, &myopt);
   opt = (MyCircleOpt *)Opt->popt;
  
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

   /* Initialize renewable initial control points (for error computations). */  
   for (i = 0; i < 3*opt->N_ctrl_points; ++i) opt->CtrlPts_i[i] = opt->CtrlPts_I[i]; 
   
   /* Initialize NewNodelist and Velocity Field. */
   for (i = 0; i < 3*Ci->N_Node; ++i) {
      Ci->NewNodeList[i] = 0.0;
      Ci->NewNodeList[i] = Ci->NodeList[i]; 
      Ci->VelocityField[i] = 0.0; 
   }  
   
   /* Calculate spline weights to fit velocity field. */   
   if (!FindSplineWeights (Ci, opt)) {
      SUMA_S_Err("Failed in FindSplineWeights");
      exit(1);
   }
   
   if (LocalHead) { fprintf(stderr,"%s: Calculating initial velocity field.\n", FuncName); }
   
	/* Calculate initial velocity field.*/
   if (!Velocity(Ci, opt)) {
      SUMA_S_Err("Failed while calculating velocity field");
      exit(1);
   }

   if( LocalHead ) {
      fprintf(SUMA_STDERR, "MAIN: Calculated Initial Velocity Field at Control Points:\n"
                           "V = [ \n"); 
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
   
   /* For plotting energy over time.  Energy over the whole move, initial to final. */
   FILE *energy_graph = NULL;
   energy_graph = fopen("plot_energy.1D", "w");
   
   niter = 0;
   a_niter = 0; 
   do { 
      
      if(opt->neighb_adjust) {
         if(a_niter) { dt = 1.0/(adj_factor*(opt->N_step - niter) - a_niter); }
         else { dt = 1.0/(opt->N_step - niter); } 
      }else { dt = 1.0/(opt->N_step - niter); }
     
      Debug_Move (Ci, opt, SO, dt, niter, a_niter, first_bad_niter);
      
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
      
      /* Check Sphere Quality.  Since want to check at the end of each iteration, need to start at niter = 1. */
      if(!first_bad_niter || niter < first_bad_niter+5){ 
         if (opt->neighb_adjust) { 
            fprintf(SUMA_STDERR, "\nNITER = %d, a_niter = %d", niter, a_niter);
            sprintf(outfile_SphereQuality, "SphereQuality_%d_%d", niter, a_niter);
         } else {         
            fprintf(SUMA_STDERR, "\nNITER = %d", niter);
            sprintf(outfile_SphereQuality, "SphereQuality_%d", niter);
         } 
         SO_name = SUMA_Prefix2SurfaceName (outfile_SphereQuality, NULL, NULL, SUMA_VEC, &exists); 
         for (i3=0; i3<3*SO->N_Node; ++i3) SO->NodeList[i3] = Ci->NewNodeList[i3];
         SUMA_RECOMPUTE_NORMALS(SO);
         shist = SUMA_HistString (NULL, argc, argv, NULL);
         nbad = SUMA_SphereQuality(SO, outfile_SphereQuality , shist);
         if(nbad && !first_bad_niter){ first_bad_niter = niter; }
         if (shist) SUMA_free(shist); shist = NULL;
         SUMA_Save_Surface_Object(SO_name, SO, SUMA_VEC, SUMA_ASCII, NULL);
      }
 
      /* Call spline weight function to recalculate spline weights for renew_weights option. */
      if (opt->renew_weights) { 
         if(LocalHead) {
            fprintf( SUMA_STDERR, "%s: RENEW_WEIGHTS OPTION -- Check renewed control points: \n", FuncName ); }
         for(i=0; i < opt->N_ctrl_points; ++i) {
            i3 = 3*i;
            opt->CtrlPts_i[i3  ] = Ci->NewNodeList[3*(opt->CtrlPts_iim[i])  ];
            opt->CtrlPts_i[i3+1] = Ci->NewNodeList[3*(opt->CtrlPts_iim[i])+1];
            opt->CtrlPts_i[i3+2] = Ci->NewNodeList[3*(opt->CtrlPts_iim[i])+2];
            
            Ci->Wv.elts[i3  ] = 0.0; 
            Ci->Wv.elts[i3+1] = 0.0; 
            Ci->Wv.elts[i3+2] = 0.0; 
         if(opt->dbg_flag) {
            fprintf( SUMA_STDERR, "%s: Control Point(%d) = [%11.8f;  %11.8f;   %11.8f] \n", FuncName, i, 
                                    opt->CtrlPts_i[i3  ], opt->CtrlPts_i[i3+1], opt->CtrlPts_i[i3+2] ); }
         }
         FindSplineWeights(Ci, opt); 
      } 
        
      /* Reset Velocity Field. */
      for (i = 0; i < 3*Ci->N_Node; ++i) Ci->VelocityField[i] = 0.0;  
      
      Velocity(Ci, opt); 
      
      if(LocalHead) {
         fprintf(SUMA_STDERR, "%s: VelocityField at Control Points: \n", FuncName);
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
   
   fclose(energy_graph); energy_graph = NULL;
    
   for (i=0; i<opt->N_ctrl_points; ++i) {
      i3 = 3 * i;
      fprintf(SUMA_STDERR,"%s: Angular error reports where control points coincide with nodes:\n"
                          "Niter = %d, last dt used= %f\n"
                          "#Col. 0: Ctrl_Node\n"
                          "#Col. 1: Original Desired Angle \n"
                          "#Col. 2: Final Achieved Angle  \n"
                          "#Col. 3: Error (ODA-FAA) in rad.\n"
                          "#Col. 4: Error (ODA-FAA) in deg.\n", FuncName, niter, dt);
      if (opt->CtrlPts_iim[i] >= 0) {
         SUMA_ANGLE_DIST_NC( (&(opt->CtrlPts_f[i3])), (&(opt->CtrlPts_I[i3])), oda, nrmi); /* original desired angle */ 
         SUMA_ANGLE_DIST_NC( (&(Ci->NewNodeList[3*opt->CtrlPts_iim[i]])), (&(opt->CtrlPts_I[i3])), faa, nrmf); /* final achieved angle */ 
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
   if (opt->CtrlPts_i) SUMA_free(opt->CtrlPts_i); opt->CtrlPts_i = NULL;
   if (opt->CtrlPts_I) SUMA_free(opt->CtrlPts_I); opt->CtrlPts_I = NULL;
   if (opt->CtrlPts_f) SUMA_free(opt->CtrlPts_f); opt->CtrlPts_f = NULL;
   if (opt->Dtheta) SUMA_free(opt->Dtheta); opt->Dtheta = NULL;
   if (opt->Nrm) SUMA_free(opt->Nrm); opt->Nrm = NULL;
   if (opt->ctrl) opt->ctrl = NULL; /* pointer from argv, do not free */
   if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   exit(0);
}
