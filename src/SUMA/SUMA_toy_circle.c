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
      int N_sub; /* number of subdivisions (for 2D objects, it is the number of nodes) */
      int N_step; /* Number of steps.  Related to dt.  This is the inverse of dt. */
      int dom_dim;
      double dt; 
      char *ctrl; /* a pointer copy of argv containing filename, do not free */
      int N_ctrl_points;
      int *CtrlPts_iim; /* Index of a particular ctrl point in the mesh. It's not necessary
                           for calculations but useful for debugging. An index of -1 is used
                           if ctrl point does not overlap with a node */
      double *CtrlPts_i;
      double *CtrlPts_I;   /*given initial, will not change*/
      double *CtrlPts_f;
      double *Dtheta;
      double *Nrm;         /*axis of rotation used for calculated given velocity*/
      double Center[3];
      double Radius;
      int renew_weights;
      int adjust; 
      int dim;
      int dot;
      int half_kernel;     /* Set flag to 1 if want to use half the kernel weight instead of going all the way 
                           around the circle or sphere. */
      char outfile[500];
   } MyCircleOpt;

typedef struct
   {
      int N_Node;
      double *NodeList;
      double *VelocityField;
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
               "  toy_circle [-ctrl CTRL_FILE] [-dom_dim DOM_DIM] [-dim DIM] [-N_sub N_SUB] [-N_step N_STEP]\n"
               "                    [-renew_weights] [-adjust] [-debug DEBUG] [-ouput OUTPUT]\n"
               "  Must specify control file.  -ctrl is a mandatory option.\n"
               " \n"
               "  -ctrl CTRLFILE:   Control nodes 1D file.\n"
               "                    Each row is for one node's intial and final XYZ.\n" 
               "  -dom_dim DOM_DIM: Set domain dimension.  Default is 2 for the circle.\n"
               "                    Must choose dom_dim = 3 for a sphere.\n" 
               "  -dim DIM:         Set the dimension.  Default is 2D.\n"
               "  -N_sub N_SUB:     Set the number of subdivisions on the circle.\n"
               "                    Approximate the number of subdivisions on the icosahedron.\n"
               "                    Default N_sub is 100.\n"
               "  -N_step N_STEP:   Set the number of steps (inverse dt).\n"
               /*"  -dt DT: Choose time step for moving points to new locations.\n"
               "                 Default DT is 0.001.\n" */
               "  -renew_weights:   Choose to recalculate spline weights after every step.\n"
               "                    Default does not renew the weights.\n"
               "  -adjust:          Choose to scale displacement.\n"
               "                    Default is no adjustment.\n"  
               "  -debug DEBUG:     Choose to turn debugging on.  Default is no debugging.\n"                        
               "  -output OUTPUT:   Name output file. Default OUTPUT is test_move.\n"
               " \n"
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
   popt->half_kernel = 0;
   snprintf(popt->outfile, 499*sizeof(char),"test_move");
   kar = 1;
   brk = NOPE;
	while (kar < argc) { /* loop accross command line options */
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
      
      if (!brk && (strcmp(argv[kar], "-half_kernel") == 0)) {
         popt->half_kernel = 1;
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
   
   /* read the ctrl file */
   
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


SUMA_Boolean FindSplineWeights (MyCircle *C, MyCircleOpt *opt)
{
   static char FuncName[]={"FindSplineWeights"};
   vector Vv;
   static matrix M, Mi;
   static double Mcr[3][3], nrm_cr[3] = {0.0, 0.0, 0.0};
   static double check_a, check_cr[3] = {0.0, 0.0, 0.0}, tan_v_mag;
   int i, idm=0.0, k, r, c, nr, nc, i3, c3, r3, r4, f, row;
   static int sinc_kernel = 0;
   double V_row1[3], V_row2[3], V_row3[3], nrm[3]={0.0,0.0,0.0};
   double  *t=NULL;
   double t_rc = 0.0, nrm_rc[3]={0.0,0.0,0.0};
   double tan_v[3]={0.0,0.0,0.0};  /* Meaning tangent velocity vector. Stores cross product when calculating given velocity. */
   double Vv_mag = 0.0, t_cr, expand_cr; 
   static matrix *nullptr = NULL;
   FILE *output_matrix = NULL;  /*for sending the matrix to a file to be read by matlab. */
   
   SUMA_Boolean LocalHead = NOPE;
   sinc_kernel = 0;     /* Set flag to 1 if want to use sinc kernel weight instead of exponential kernel weight. */
          
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
      opt->Dtheta = (double *)SUMA_calloc(opt->N_ctrl_points , sizeof (double));       /* angular displacement between t = 0 and t = 1 for each node.
                                                                                       One does not need to store for the algorithm but it makes
                                                                                       debugging easier. */
   }
   
   opt->Nrm = (double *)SUMA_calloc(opt->N_ctrl_points * 3 , sizeof (double));

   /* CALCULATE GIVEN VELOCITY AT CONTROL POINTS USING DESIRED ANGLE AND AXIS OF ROTATION. */
   vector_initialize (&Vv);
   if ( opt->dot ) { vector_create ( (opt->dim*opt->N_ctrl_points + opt->N_ctrl_points*opt->N_ctrl_points), &Vv); }
      else { vector_create (opt->dim*opt->N_ctrl_points, &Vv); }
   
   for (i=0; i < opt->N_ctrl_points; ++i) {
      if( opt->dot ) { idm = (opt->dim + opt->N_ctrl_points)*i; }
         else { idm = opt->dim*i; }
      
      i3 = 3*i;

      /* Distance between control point and desired destination of control point */ 
      /* nrm is the cross product, called nrm because normal means perpendicular. nrm has 3 entries for each control point */ 
      /* nrm is the axis of rotation from intial control point location to final. */       
      /* BEWARE: USING _NC HERE, MEANING NO CENTER.  WILL THE CENTER ALWAYS BE THE ORIGIN? */
      SUMA_ANGLE_DIST_NC((&(opt->CtrlPts_f[i3])), (&(opt->CtrlPts_i[i3])), opt->Dtheta[i], (&(opt->Nrm[i3])) );

      if (LocalHead) {
         fprintf(SUMA_STDERR, "%s: Point %d, Dtheta = %.12f rad (%.12f deg)\n",
                                 FuncName, i, opt->Dtheta[i], SUMA_R2D(opt->Dtheta[i]));                            
      }

      SUMA_MT_CROSS(tan_v, (&(opt->Nrm[i3])), (&(opt->CtrlPts_i[i3])) );
      
      /*Check to make see cross product is perpendicular to plane of the node vector. */
      if (LocalHead) {
         fprintf(SUMA_STDERR, "Dot product( axis of rotation, control point) = %.12f\n"
                              "Dot product( velocity vector, control point) = %.12f\n"
                              "tan_v = [%.12f; %.12f;  %.12f; ]\n"
                              "  axis of rotation = %f   %f    %f\n",
                              SUMA_MT_DOT((&(opt->Nrm[i3])), (&(opt->CtrlPts_i[i3])) ), 
                              SUMA_MT_DOT(tan_v, (&(opt->CtrlPts_i[i3])) ),
                              tan_v[0], tan_v[1], tan_v[2],
                              opt->Nrm[0], opt->Nrm[1], opt->Nrm[2] ); }
                             
      
      /* Need to normalize because using the direction (unit vector) of this tangent vector in the velocity calculation. */
      tan_v_mag = sqrt( tan_v[0]*tan_v[0] + tan_v[1]*tan_v[1] + tan_v[2]*tan_v[2] );
      if ( tan_v_mag > 0.000000001) {
         tan_v[0] = tan_v[0]/tan_v_mag;
         tan_v[1] = tan_v[1]/tan_v_mag;
         tan_v[2] = tan_v[2]/tan_v_mag; }
         
      if (LocalHead) {
         fprintf(SUMA_STDERR, "  TAN_V normalized = [ %.12f;    %.12f;    %.12f ]\n"
                              "  Dot product( tan_v, control point) = %.12f\n"
                              "  Dtheta = %f\n", 
                              tan_v[0], tan_v[1], tan_v[2],
                              SUMA_MT_DOT(tan_v, (&(opt->CtrlPts_i[i3])) ),
                              opt->Dtheta[i] ); }
      
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
 
   if( opt->dot ) { nr = opt->N_ctrl_points * (opt->dim + opt->N_ctrl_points); } /*Need extra rows for dot product.*/
      else { nr = opt->dim * opt->N_ctrl_points; } /* number of rows */
   nc = opt->dim * opt->N_ctrl_points; /* number of columns */
   matrix_initialize(&M);
   matrix_create(nr, nc, &M);
   
   if (!M.elts) {
      SUMA_S_Crit("Failed to allocate");
      exit(1);
   }
   
   if( !opt->dot ) {
      for(r=0; r<opt->N_ctrl_points; ++r) { /*r for row, for the first three rows, r represents the first control point*/
            for(c=0; c<opt->N_ctrl_points; ++c) { /*c for column, while r is held constant, c cycles through all control points
                                                    so that for the first 3 rows (first 3 C.P.) are compared to all the rest.*/
               c3 = 3*c; r3 = 3*r;

               /* Create the rotation matrix. */
               SUMA_3D_Rotation_Matrix( (&(opt->CtrlPts_i[c3])), (&(opt->CtrlPts_i[r3])), Mcr, t_cr, nrm_cr);
             
               /* t_cr_new = t_cr;
               if( t_cr_new > SUMA_PI ) { t_cr_new = t_cr_new - 2*SUMA_PI; } */
               
               if ( sinc_kernel ) { 
                  SUMA_SINC( t_cr, expand_cr );
                  expand_cr = SUMA_POW2( expand_cr );
               } else { 
                  if( opt->half_kernel && t_cr > SUMA_PI/2.0 ) { expand_cr = 0.0; }
                  else { expand_cr = exp(-0.5*t_cr*t_cr); }   
               } 

               /* Assemble the matrix and include the expansion factor. */
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
   
   if( opt->dot ) {
      for(r=0; r<opt->N_ctrl_points; ++r) { /*r for row, for the first three rows, r represents the first control point*/
         for(c=0; c<opt->N_ctrl_points; ++c) { /*c for column, while r is held constant, c cycles through all control points
                                                 so that for the first 3 rows (first 3 C.P.) are compared to all the rest.*/
            row = (3 + opt->N_ctrl_points)*r; /* Need extra rows for dot product restriction. */
            c3 = 3*c; r3 = 3*r;
            
            /* Create the rotation matrix. */
            SUMA_3D_Rotation_Matrix( (&(opt->CtrlPts_i[c3])), (&(opt->CtrlPts_i[r3])), Mcr, t_cr, nrm_cr );
            
            if ( sinc_kernel ) {
               SUMA_SINC( t_cr, expand_cr ); 
               expand_cr = SUMA_POW2( expand_cr );
            } else {
               if( opt->half_kernel && t_cr > SUMA_PI/2.0 ) { expand_cr = 0.0; }
               else { expand_cr = exp( -0.5*t_cr*t_cr ); }
            }
 
            /* Assemble the matrix and include the expansion factor. */
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
  
   if(LocalHead) {  
      if (opt->dot) {
         fprintf(SUMA_STDERR, "%s:\n"
                              "M = [\n", FuncName);
         for (r=0; r<opt->N_ctrl_points; ++r) {
            for (k=0; k<(3+opt->N_ctrl_points); ++k) {
               for(c=0; c<opt->N_ctrl_points; ++c) { 
                  fprintf (SUMA_STDERR,"%.8f   %.8f   %.8f   ", 
                                          M.elts [ ( (3+opt->N_ctrl_points) *r+k) ][ (3*c  ) ],
                                          M.elts [ ( (3+opt->N_ctrl_points) *r+k) ][ (3*c+1) ],
                                          M.elts [ ( (3+opt->N_ctrl_points) *r+k) ][ (3*c+2) ]);
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
   } 
   
   /* Write matrix to a file so can be read by matlab. */
   output_matrix = fopen( "output_matrix.m", "w" );
   
   if (opt->dot) {
      fprintf(output_matrix, "M = [\n");
         for (r=0; r<opt->N_ctrl_points; ++r) {
            for (k=0; k<(3+opt->N_ctrl_points); ++k) {
               for(c=0; c<opt->N_ctrl_points; ++c) { 
                  fprintf (output_matrix,"%11.8f   %11.8f   %11.8f   ", 
                                          M.elts [ ( (3+opt->N_ctrl_points) *r+k) ][ (3*c  ) ],
                                          M.elts [ ( (3+opt->N_ctrl_points) *r+k) ][ (3*c+1) ],
                                          M.elts [ ( (3+opt->N_ctrl_points) *r+k) ][ (3*c+2) ]);
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
 
   SUMA_LH("Calculating inverse...");
   matrix_initialize(&Mi);
   if( opt->dot ) { 
      matrix_create(nc, nr, &Mi);
      matrix_psinv (M, nullptr, &Mi); }
   else { 
      matrix_create(nr, nc, &Mi);
      matrix_psinv (M, nullptr, &Mi);
      /*matrix_inverse_dsc ( M, &Mi); */} 
   SUMA_LH("   Done."); 
  
   /*Print the Inverse Coefficient Matrix.*/
   if( opt->dot ) {   
      if (LocalHead) { 
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
      }
   } else {   
      if (LocalHead) { 
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
   if( LocalHead ) {
      fprintf(SUMA_STDERR, "V = [ \n" );
      for (i=0; i < opt->N_ctrl_points; ++i) {
         if( opt->dot ) { 
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
   }   

   /* Send velocity vector to output file for checking math in matlab. */
   if( LocalHead ) {
      fprintf(output_matrix, "V = [ \n" );
      for (i=0; i < opt->N_ctrl_points; ++i) {
         if( opt->dot ) { 
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
   }

   SUMA_LH("Calculating weights...");
   vector_initialize(&(C->Wv));
   vector_multiply(Mi, Vv, &(C->Wv));
   SUMA_LH("   Done."); 
   
   if( LocalHead ) {   
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
 
   if (t) SUMA_free(t); t=NULL;
   matrix_destroy (&M);
   matrix_destroy (&Mi);
   vector_destroy (&Vv);
   
   opt->Dtheta = NULL;
   fclose (output_matrix); output_matrix = NULL;
      
   SUMA_RETURN(YUP);   
}

SUMA_Boolean Velocity( MyCircle *C, MyCircleOpt *opt) 
{
   static char FuncName[]={"Velocity"};
   static int ncall = 0, sinc_kernel = 0;
   byte repeat;   
   int i, i3, j, j3, jdm, r, c;
   double Wax, Way, Waz, AS, cas, sas, dv[3],was, *xyz_i, *xyz_j, vfield_mag = 0.0, scale, nrm[3]={0.0,0.0,0.0};
   static double v_alpha = 0.0, v_alpha_check, v_cr[3]={0.0,0.0,0.0}, v_expand, cr_mag; 
   vector Wr;  /*W rotated*/
   static double v_M[3][3];
   
   SUMA_Boolean LocalHead = NOPE;
   sinc_kernel = 0;
   
   SUMA_ENTRY;

   /* Compute field at each node */
   
   vector_initialize (&Wr);
   vector_create (3*opt->N_ctrl_points, &Wr);
   
   if (LocalHead) { 
      fprintf(SUMA_STDERR, "******************************************************\n"
                           "VELOCITY: CHECKING THE FUNCTION, First control point: \n"); }

   for (i=0; i< C->N_Node; ++i) {  /* i for all points, j for the control points. */
      i3 = 3*i;

      for (j=0; j<opt->N_ctrl_points; ++j){  
         j3 = 3*j;
         
         /* CALCULATE ROTATED WEIGHTS, TO BE PART OF SUM THAT IS VELOCITY AT EACH POINT. */ 
         /* FIND ANGULAR DISTANCE AND AXIS OF ROTATION FOR ROTATING THE VELOCITY CONTRIBUTIONS. */ 
         /* ONLY NEED THESE PARAMETERS WHEN USING SUMA_ROTATE_ABOUT_AXIS */ 
         
         /*Need to comment out when  using SUMA_3D_Rotation_Matrix to calculate initial velocity field. */
         SUMA_ANGLE_DIST_NC( (&(C->NewNodeList[i3])), (&(opt->CtrlPts_i[j3])), v_alpha, v_cr );
         if (LocalHead) { 
            if( i == opt->CtrlPts_iim[0] ) { fprintf(SUMA_STDERR, "Cross product, not normalized = \n"
                                                                  "     %.12f    %.12f    %.12f \n",
                                                                  v_cr[0], v_cr[1], v_cr[2]); } }
         
         /*fprintf( SUMA_STDERR, "HELP: v_alpha = %f\n", v_alpha);   */ 
         
         cr_mag = sqrt( v_cr[0]*v_cr[0] + v_cr[1]*v_cr[1] + v_cr[2]*v_cr[2]); 
         if (LocalHead) { 
            if( i == opt->CtrlPts_iim[0] ) { fprintf(SUMA_STDERR, "Magnitude of Cross Product = %.12f \n", cr_mag); }} 
   
         if(cr_mag > 0.000000001){ v_cr[0] = v_cr[0]/cr_mag; v_cr[1] = v_cr[1]/cr_mag; v_cr[2] = v_cr[2]/cr_mag; }  
         
          if (LocalHead) { 
            if( i == opt->CtrlPts_iim[0] ) { fprintf(SUMA_STDERR, " Control Point: %d to Node: %d \n"
                                                                  "p1 = [ %f     %f    %f ]; \n"
                                                                  "p2 = [ %f     %f    %f ]; \n"
                                                                  "  Angle to be rotated = %f \n"
                                                                  "  Axis of rotation: \n"
                                                                  "     %.12f    %.12f    %.12f \n"
                                                                  "  Weight = %f %f %f\n", 
                                                                  j, i, 
                                                                  opt->CtrlPts_i[j3  ], opt->CtrlPts_i[j3+1], opt->CtrlPts_i[j3+2],
                                                                  C->NewNodeList[i3  ], C->NewNodeList[i3+1], C->NewNodeList[i3+2],
                                                                  v_alpha, 
                                                                  v_cr[0], v_cr[1], v_cr[2],
                                                                  C->Wv.elts[j3], C->Wv.elts[j3+1], C->Wv.elts[j3+2]); } }
         
         
         /*if( i == opt->CtrlPts_iim[0] ) {
            fprintf(SUMA_STDERR, "Cross product, non normalized: \n"
                              "  %f    %f    %f \n"
                              "  mag of cross product = %f \n"
                              "Points used in cross product calculation:\n"
                              "  P1 = %f     %f    %f\n"
                              "  P2 = %f     %f    %f\n", 
                              v_cr[0], v_cr[1], v_cr[2], cr_mag,
                              C->NewNodeList[i3  ], C->NewNodeList[i3+1], C->NewNodeList[i3+2],
                              opt->CtrlPts_i[j3  ], opt->CtrlPts_i[j3+1], opt->CtrlPts_i[j3+2]);  } 
                         
         if( i == opt->CtrlPts_iim[1] ) {
            fprintf(SUMA_STDERR, "Cross product, non normalized: \n"
                              "  %f    %f    %f \n"
                              "  mag of cross product = %f \n"
                              "Points used in cross product calculation:\n"
                              "  P1 = %f     %f    %f \n"
                              "  P2 = %f     %f    %f \n", 
                              v_cr[0], v_cr[1], v_cr[2], cr_mag, 
                              C->NewNodeList[i3  ], C->NewNodeList[i3+1], C->NewNodeList[i3+2],
                              opt->CtrlPts_i[j3  ], opt->CtrlPts_i[j3+1], opt->CtrlPts_i[j3+2]);  } */
         if (LocalHead) {
            if (i == opt->CtrlPts_iim[0]) {
               fprintf(SUMA_STDERR, "Using SUMA_ROTATE_ABOUT AXIS to calculate velocity field.\n" ); } }
         
         /* ROTATE THE WEIGHTS USING DISPLACEMENT ANGLE AND AXIS OF ROTATION CALCULATED ABOVE. */
         SUMA_ROTATE_ABOUT_AXIS( (&(C->Wv.elts[j3])), v_cr, v_alpha, (&(Wr.elts[j3])) );       

         if( sinc_kernel ) {
            SUMA_SINC( v_alpha, v_expand );
            v_expand = SUMA_POW2( v_expand );
         } else { 
            if( opt->half_kernel && v_alpha > SUMA_PI/2.0 ) { v_expand = 0.0; }
            else { v_expand = exp( -0.5*v_alpha*v_alpha ); }
         }
    
         #if 0
         SUMA_3D_Rotation_Matrix( (&(opt->CtrlPts_i[j3])), (&(C->NewNodeList[i3])), v_M, v_alpha, v_cr);
         
         if( sinc_kernel ) {
            SUMA_SINC( v_alpha, v_expand ); 
            v_expand = SUMA_POW2( v_expand );
         } else {
            if( opt->half_kernel && v_alpha > SUMA_PI/2.0 ) { v_expand = 0.0; }
            else { v_expand = exp( -0.5*v_alpha*v_alpha ); }  
         }
         
         if (LocalHead) { 
            if (i == opt->CtrlPts_iim[0]) {
               fprintf(SUMA_STDERR, "AFTER ROTATION MATRIX CALCULATED: \n"
                                    "  v_alpha = %f\n"
                                    "  expansion = %.5f\n", v_alpha, v_expand); } }
         if (LocalHead) { 
            if( i == opt->CtrlPts_iim[0] ) { fprintf(SUMA_STDERR, "  p1 = [ %f     %f    %f ]; \n"
                                                                  "  p2 = [ %f     %f    %f ]; \n"
                                                                  "  Angle to be rotated = %f \n"
                                                                  "  Axis of rotation: \n"
                                                                  "     %.12f    %.12f    %.12f \n"
                                                                  "  Weight = %f %f %f\n", 
                                                                  opt->CtrlPts_i[j3  ], opt->CtrlPts_i[j3+1], opt->CtrlPts_i[j3+2],
                                                                  C->NewNodeList[i3  ], C->NewNodeList[i3+1], C->NewNodeList[i3+2],
                                                                  v_alpha, 
                                                                  v_cr[0], v_cr[1], v_cr[2],
                                                                  C->Wv.elts[j3], C->Wv.elts[j3+1], C->Wv.elts[j3+2]); } }
                                                                  
         /*Check rotation matrix. */
         if (LocalHead) { 
            if (i == opt->CtrlPts_iim[0]) {
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
         #endif
         
         /* Check rotated weights. */
         if (LocalHead) { 
            if( i == opt->CtrlPts_iim[0] ) {
               fprintf(SUMA_STDERR, "  W rotated = %f     %f    %f \n"
                                    "  expansion factor = %f \n", 
                                    Wr.elts[j3], Wr.elts[j3+1], Wr.elts[j3+2], v_expand); } 
            if( i == opt->CtrlPts_iim[0] ) {
               fprintf(SUMA_STDERR, "  Dot Product of Rotated Weights with Radius = %.12f \n"
                                    "  Mag_Wv_rotated = %f \n", 
                                    SUMA_MT_DOT( (&(Wr.elts[j3])), (&(C->NewNodeList[i3])) ), 
                                    sqrt(   SUMA_POW2(C->Wv.elts[j3  ]) 
                                          + SUMA_POW2(C->Wv.elts[j3+1]) 
                                          + SUMA_POW2(C->Wv.elts[j3+2]) ) ); 
            }
         }
         
         /* fprintf(SUMA_STDERR, "HELP: alpha(%d) = %f, expansion factor(%d) = %f \n", i, v_alpha, i, v_expand); */
         
         
         Wr.elts[j3  ] *= v_expand;
         Wr.elts[j3+1] *= v_expand;
         Wr.elts[j3+2] *= v_expand;
         
         if (LocalHead) { 
            if( i == opt->CtrlPts_iim[0] ) {
               fprintf(SUMA_STDERR, "Velocity Field Components as stored in Wr.elts: \n"
                                    "     %f    %f    %f \n",
                                    Wr.elts[j3  ], Wr.elts[j3+1], Wr.elts[j3+2]); } }
   
         C->VelocityField[i3  ] += Wr.elts[j3  ];
         C->VelocityField[i3+1] += Wr.elts[j3+1];
         C->VelocityField[i3+2] += Wr.elts[j3+2]; 
  
      } 
      
      if (LocalHead) { 
         if( i == opt->CtrlPts_iim[0] ) {
            fprintf(SUMA_STDERR, "VELOCITY (calculated): Initial Velocity Field, no adjustment: \n"
                                 "     %f    %f    %f \n",
                                 C->VelocityField[i3  ], C->VelocityField[i3+1], C->VelocityField[i3+2]); }

         if( i == opt->CtrlPts_iim[0] ) {
               fprintf(SUMA_STDERR, "  Dot Product of Calculated Velocity Field with Radius = %.12f \n",
                                    SUMA_MT_DOT( (&(C->VelocityField[i3])), (&(C->NewNodeList[i3])) ) ); }
      } 
   } 
                                
#if 0   
   if (ncall == 0) { /* at the first call, calculate magnitudes of velocities */
      for (i = 0; i < C->N_Node; ++i) {  
         i3 = i*3;
         C->VelocityMagnitude[i] = sqrt ( SUMA_POW2 (C->VelocityField[i3  ]) +
                                          SUMA_POW2 (C->VelocityField[i3+1]) +
                                          SUMA_POW2 (C->VelocityField[i3+2]) );
         if (LocalHead) {  
            if( i == opt->CtrlPts_iim[0] ) {
               i3 = 3*i;
               fprintf(SUMA_STDERR, "VELOCITY: Initial Magnitude to which all are scaled = %f\n",
                                    C->VelocityMagnitude[i] ); 
            } 
         }
      }
   } 
   else {
      /* second time around, rescale vectors to original velocity magnitude */
      /*    Vectors are rescaled by first normalizing them (dividing by their magnitude) */
      /*       so that they become a unit vector and then multiplying by the velocity magnitude */
      /*       (the magnitude of the original vector).  Scale does these two things in one step. */
      
      for (i = 0; i < C->N_Node; ++i) {  
         i3 = i*3;
         vfield_mag = 0.0;
         vfield_mag = sqrt (  SUMA_POW2 (C->VelocityField[i3  ]) +
                              SUMA_POW2 (C->VelocityField[i3+1]) +
                              SUMA_POW2 (C->VelocityField[i3+2]) );
         if (vfield_mag > 0.000000001) {
            scale = C->VelocityMagnitude[i] / vfield_mag;
            C->VelocityField[i3  ] *= scale;
            C->VelocityField[i3+1] *= scale;
            C->VelocityField[i3+2] *= scale; }
   
        
   } }
#endif         
         
   ++ncall; 
      
   vector_destroy (&Wr);

   SUMA_RETURN(YUP);
}

/* 
   Code for 3D wherever you can, 
   Flag instances where method is 2D only
   Comments, debug messages, align millipede equations
   Avoid large scope single char variables
*/
int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"toy_circle"}; 
   char outfile[] = {"Coords_0.txt"}, outfile_speed[] = {"Plot_Speed0.txt"}, outfile_test[50]; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt;  
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   MyCircleOpt myopt, *opt = NULL;
   int i, i3, idm, j;
   double dt;    /*dt2, te; */
   double u[3], oxyz[3]={0.0, 0.0, 0.0};
   double um=-1.0, oda, faa, error, dtheta=0.0, nrm[3]={0.0, 0.0, 0.0}, nrmi[3]={0.0, 0.0, 0.0}, nrmf[3]={0.0, 0.0, 0.0};
   int niter=0;
   SUMA_SurfaceObject *SO = NULL;
   static double mv_mag = 0.0, mv_alpha, mv_nrm_mag, mv_nrm[3], newnode_mag = 0.0;  
   static double Dot_v[2];
   static float Point_at_Distance[2][3] = { {0.0, 0.0, 0.0},{ 0.0, 0.0, 0.0} }, V_Mag = 0.0;
   void * SO_name;
   char *shist=NULL;
   int nbad = 0;
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
   Ci->NodeList = Ci->VelocityField = Ci->NewNodeList = Ci->VelocityMagnitude = Ci->Theta = NULL;
   Ci->NodeList = (double *)SUMA_malloc(Ci->N_Node * 3 * sizeof (double));
   Ci->VelocityField = (double *)SUMA_malloc(Ci->N_Node * 3 * sizeof (double));
   Ci->VelocityMagnitude = (double *)SUMA_malloc(Ci->N_Node * sizeof (double));
   Ci->NewNodeList = (double *)SUMA_malloc(Ci->N_Node * 3 * sizeof (double));
   Ci->Theta = (double *)SUMA_malloc(Ci->N_Node * 3 * sizeof (double));
   
   /*LOOP TO CREATE THE CIRCLE OF N_NODES STARTING AT THE POSITIVE X-AXIS
      AND DRAWING COUNTER CLOCKWISE.*/  
    
   if( opt->dot == 1) { fprintf( stderr, "USING DOT PRODUCT RESTRICTION.\n"); } 
      
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
      /* make the idcode_str depend on the Label, it is convenient to
      send the same surface all the time to SUMA */
      if (SO->Label) { if (SO->idcode_str) SUMA_free(SO->idcode_str); SO->idcode_str = NULL; SUMA_NEW_ID(SO->idcode_str, SO->Label); }
 
      SUMA_Save_Surface_Object(SO_name, SO, SUMA_VEC, SUMA_ASCII, NULL);
      
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
   
   /* Send nodelist to file to be read and graphed by matlab. */
   if( LocalHead ) {
      if( opt->dom_dim == 2 ) { 
         FILE *plot_circle = NULL; 
         plot_circle = fopen ( "Coords_0.txt", "w");   

         for ( i = 0; i < Ci->N_Node; ++i) {
            i3 = 3*i;
            fprintf ( plot_circle, "%11.8f  %11.8f  %11.8f\n", Ci->NodeList[i3], Ci->NodeList[i3+1], Ci->NodeList[i3+2] );
         }
         fclose (plot_circle); plot_circle = NULL; 
      }
   }
    
   /* initialize renewable initial control points - needed for error computations*/
       
   for (i = 0; i < opt->N_ctrl_points; ++i){
      i3 = 3*i;
      opt->CtrlPts_i[i3  ] =  opt->CtrlPts_I[i3  ]; 
      opt->CtrlPts_i[i3+1] =  opt->CtrlPts_I[i3+1];
      opt->CtrlPts_i[i3+2] =  opt->CtrlPts_I[i3+2];
   }
   
   /* Initialize newnodelist */
   for (i = 0; i < 3*Ci->N_Node; ++i)  Ci->NewNodeList[i] = Ci->NodeList[i];    
   
   /*Calculate spline weights to fit velocity field*/   
   if (!FindSplineWeights (Ci, opt)) {
      SUMA_S_Err("Failed in FindSplineWeights");
      exit(1);
   }
   
   if ( LocalHead ) { fprintf(stderr,"%s: Calculating initial velocity field.\n", FuncName); }
   
   /* initialize Velocity Field. */
   for (i = 0; i < Ci->N_Node; ++i){
      i3 = 3*i;
      Ci->VelocityField[i3  ] = 0.0; 
      Ci->VelocityField[i3+1] = 0.0; 
      Ci->VelocityField[i3+2] = 0.0; 
   } 
   
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
  
   /* Send Velocity Magnitudes to File for plotting in Matlab. */
   if( LocalHead ) {
      if( opt->dom_dim == 2) {
         FILE *plot_speed = NULL; 
         plot_speed = fopen(outfile_speed, "w");
         for ( i = 0; i < Ci->N_Node; ++i) {
            i3 = 3*i;
            V_Mag = 0.5*sqrt(  SUMA_POW2(Ci->VelocityField[i3  ]) +  /*Using half the magnitude to make Matlab plot easier to look at.*/
                               SUMA_POW2(Ci->VelocityField[i3+1]) + 
                               SUMA_POW2(Ci->VelocityField[i3+2])  );
            SUMA_POINT_AT_DISTANCE_NORM( (&(Ci->NewNodeList[i3])), (&(Ci->NewNodeList[i3])), V_Mag, Point_at_Distance );
            fprintf( plot_speed, "%11.8f   %11.8f   %11.8f  \n", 
                     Point_at_Distance[0][0], Point_at_Distance[0][1], Point_at_Distance[0][2]);
         }
         fclose(plot_speed); plot_speed = NULL;  
      }
   }  
   
   if( LocalHead ) {
      FILE *test = NULL;
      sprintf( outfile_test, "%s0.1D", opt->outfile );
      test = fopen (outfile_test, "w");   
      for ( i = 0; i < Ci->N_Node; ++i) {
         i3 = 3*i;
         fprintf (test, "%11.8f  %11.8f  %11.8f  %11.8f  %11.8f  %11.8f  \n", 
            Ci->NewNodeList[i3], Ci->NewNodeList[i3+1], Ci->NewNodeList[i3+2], 
            Ci->VelocityField[i3], Ci->VelocityField[i3+1], Ci->VelocityField[i3+2]);
      } 
      fclose (test); test = NULL;
   }
   
   /* loop until time is 1 */
   if (LocalHead) {
      fprintf(SUMA_STDERR,"%s: About to enter main loop:\n"
                          "adjust shorty     = %d\n", 
                          FuncName, 
                          opt->adjust );
   } 
   
   if (opt->dbg_flag) 
   { fprintf(stderr,"%s: Moving the points. (N_Node = %d)\n", FuncName, Ci->N_Node); }
   
   if (opt->dbg_flag) {
      if (opt->adjust == 0) fprintf(stderr,"%s: Moving the points, no adjustment. (N_Node = %d)\n", FuncName, Ci->N_Node); 
      else fprintf(stderr,"%s: Moving the points, adjusted. (N_Node = %d)\n", FuncName, Ci->N_Node); 
   }
   
   /* te=0.0;
      dt = opt->dt; 
      dt2 = dt / 2.0; */
   niter = 0;
   do { 
      
      dt = 1.0/(opt->N_step - niter); 
   
      if (opt->dbg_flag) { fprintf(stderr,"%s: niter = %d, dt = %.3f.\n", FuncName, niter, dt); }
      
      for (i = 0; i < Ci->N_Node; ++i) 
      {  
         i3 = i*3;  
 
         /* See figure ZSS, NIH-4, p 61 */
         u[0] = Ci->VelocityField[i3  ] * dt; 
         u[1] = Ci->VelocityField[i3+1] * dt; 
         u[2] = Ci->VelocityField[i3+2] * dt;

      
         if (opt->dbg_flag && i == opt->CtrlPts_iim[0]) { /* debug when node is 1st control point */
         fprintf(SUMA_STDERR, "MAIN: \n"
                              "DotProduct of u=Vf*dt before adjustment: \n"
                              "   udotrad  = [%.18f]\n"
                              "   u = Vf*dt = [%f  %f   %f]\n", 
                              SUMA_MT_DOT( (&(u[0])),(&(Ci->NewNodeList[i3]))),
                              u[0], u[1], u[2]); } 
         

         if (opt->adjust) {
            /* must turn the magnitude of u to that of uc, where |uc| / |u| = tan(a) / a; 
               since for unit circle/sphere a = |u|, |uc| = tan(a) */
            /* um = |u| */
            um = sqrt(  SUMA_POW2(u[0]) + 
                        SUMA_POW2(u[1]) + 
                        SUMA_POW2(u[2]));
            /* rescale |u| to make it |uc| */
            if (um){
               u[0] *=  tan(um)/um;
               u[1] *=  tan(um)/um;            
               u[2] *=  tan(um)/um;            
            }            
         }
         
         
         if (opt->dbg_flag && i == opt->CtrlPts_iim[0]) { 
         fprintf(SUMA_STDERR, "MAIN: \n"
                              "DotProduct of u=Vf*dt after adjustment: \n"
                              "   udotrad  = [%.18f]\n"
                              "   u = Vf*dt = [%f  %f   %f]\n", 
                              SUMA_MT_DOT( (&(u[0])),(&(Ci->NewNodeList[i3]))),
                              u[0], u[1], u[2] ); } 
         

         if (opt->dbg_flag && i == opt->CtrlPts_iim[0]) { /* debug when node is 1st control point */
            fprintf(SUMA_STDERR, "********************************************\n"
                                 "Iter %d: debug for ctrl point 0 == node %d\n"
                                 "dt = %f\n"
                                 "u        = [%.8f %.8f %.8f], um = %.8f\n"
                                 "udotrad  = [%.18f]\n" 
                                 "old[XYZ] = [%.8f %.8f %.8f]\n", 
                                 niter, i,
                                 dt, 
                                 u[0], u[1], u[2], um,
                                 SUMA_MT_DOT( (&(u[0])),(&(Ci->NewNodeList[i3  ]))), 
                                 Ci->NewNodeList[i3  ], Ci->NewNodeList[i3+1], Ci->NewNodeList[i3+2]);
            oxyz[0] = Ci->NewNodeList[i3  ];
            oxyz[1] = Ci->NewNodeList[i3+1];
            oxyz[2] = Ci->NewNodeList[i3+2];
         }
 
         
         /* BEGIN NEW METHOD -- USES SUMA_ROTATE_ABOUT_AXIS */
         /* Find the parameters needed to move the points.  Here mv stands for move. */
         mv_mag = sqrt( u[0]*u[0] + u[1]*u[1] + u[2]*u[2]);
         if( mv_mag > 0.000000001) { mv_alpha = atan( mv_mag ); }
         SUMA_MT_CROSS ( mv_nrm,(&(Ci->NewNodeList[i3])), (&(u[0]))); 
         mv_nrm_mag = sqrt( mv_nrm[0]*mv_nrm[0] + mv_nrm[1]*mv_nrm[1] +  mv_nrm[2]*mv_nrm[2] );
         if (mv_nrm_mag > 0.000000001) {
            mv_nrm[0] = mv_nrm[0]/mv_nrm_mag; mv_nrm[1] = mv_nrm[1]/mv_nrm_mag;  mv_nrm[2] = mv_nrm[2]/mv_nrm_mag; }

         /* Move the points a small step using the Rotation macro. */
         SUMA_ROTATE_ABOUT_AXIS( (&(Ci->NewNodeList[i3])), (&(mv_nrm[0])), mv_alpha, (&(Ci->NewNodeList[i3])) );
         
         /* Project point back onto the circle. */
         newnode_mag = sqrt(  SUMA_POW2(Ci->NewNodeList[i3  ]) + 
                              SUMA_POW2(Ci->NewNodeList[i3+1]) + 
                              SUMA_POW2(Ci->NewNodeList[i3+2]) );
         
         /*if (opt->dbg_flag) { fprintf(stderr,"mag initial: %f\n", mag); } */
         if (newnode_mag > 0.000000001) {
            Ci->NewNodeList[i3  ] = opt->Radius*(Ci->NewNodeList[i3  ])/( newnode_mag );
            Ci->NewNodeList[i3+1] = opt->Radius*(Ci->NewNodeList[i3+1])/( newnode_mag );
            Ci->NewNodeList[i3+2] = opt->Radius*(Ci->NewNodeList[i3+2])/( newnode_mag ); }
         /* END NEW METHOD THAT USES SUMA_ROTATE_ABOUT_AXIS */
         
         
         #if 0
         /* OLD METHOD. */   
         Ci->NewNodeList[i3  ] = Ci->NewNodeList[i3  ] + u[0];
         Ci->NewNodeList[i3+1] = Ci->NewNodeList[i3+1] + u[1];
         Ci->NewNodeList[i3+2] = Ci->NewNodeList[i3+2] + u[2];
         if (opt->dbg_flag && i == opt->CtrlPts_iim[0]) { /* debug when node is 1st control point */
            fprintf(SUMA_STDERR, "par[XYZ] = [%.8f %.8f %.8f]\n", 
                                 Ci->NewNodeList[i3  ], Ci->NewNodeList[i3+1], Ci->NewNodeList[i3+2]); }
   
         newnode_mag = sqrt(  SUMA_POW2(Ci->NewNodeList[i3  ]) + 
                              SUMA_POW2(Ci->NewNodeList[i3+1]) + 
                              SUMA_POW2(Ci->NewNodeList[i3+2]) );
                              
         /*if (opt->dbg_flag) { fprintf(stderr,"mag initial: %f\n", mag); } */
         if ( newnode_mag > 0.000000001 ) {
            Ci->NewNodeList[i3  ] = opt->Radius * (Ci->NewNodeList[i3  ])/( newnode_mag );
            Ci->NewNodeList[i3+1] = opt->Radius * (Ci->NewNodeList[i3+1])/( newnode_mag );
            Ci->NewNodeList[i3+2] = opt->Radius * (Ci->NewNodeList[i3+2])/( newnode_mag ); }   
         /* END OLD METHOD. */ 
         #endif
            
         /* THIS DTHETA IS AN ABSOLUTE ANGLE THAT IS ALWAYS POSITIVE. */
         if (opt->dbg_flag && i == opt->CtrlPts_iim[0]) { /* debug when node is 1st control point */
            SUMA_ANGLE_DIST_NC( (&(oxyz[0])), (&(Ci->NewNodeList[i3])), dtheta, nrm);

            fprintf(SUMA_STDERR, "new[XYZ] = [%.8f %.8f %.8f]\n"
                                 "Dtheta = %.18f rad, (%.18f deg.)\n",
                                 Ci->NewNodeList[i3  ], Ci->NewNodeList[i3+1], Ci->NewNodeList[i3+2],
                                 dtheta, SUMA_R2D(dtheta));
         }
         
         /* Set u vector back to zero because using u with different values when loop repeats. */
         /* This might be a little much. */
         u[0] = 0.0; u[1] = 0.0; u[2] = 0.0;    
      }
  
      /* recalculate surface normals */
      if (ps->cs->talk_suma) {
         for (i3=0; i3<3*SO->N_Node; ++i3) SO->NodeList[i3] = Ci->NewNodeList[i3];
         SUMA_RECOMPUTE_NORMALS(SO); 
         if (ps->cs->Send) {
            if (!SUMA_SendToSuma (SO, ps->cs, (void *)SO->NodeList, SUMA_NODE_XYZ, 1)) {
               SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
            }
         }
      }
     
      if (opt->dbg_flag) { fprintf(stderr,"%s: Writing results to %s\n", FuncName, opt->outfile); }
  
      /*Call spline weight function to recalculate spline weights for renew_weights option.*/
      if (opt->renew_weights) { 
         if( LocalHead ) {
            fprintf( SUMA_STDERR, "RENEW_WEIGHTS OPTION -- Check renewed control points: \n" ); }
         for (i=0; i < opt->N_ctrl_points; ++i) {
            i3 = 3*i;
            opt->CtrlPts_i[i3  ] = Ci->NewNodeList[3*(opt->CtrlPts_iim[i])  ];
            opt->CtrlPts_i[i3+1] = Ci->NewNodeList[3*(opt->CtrlPts_iim[i])+1];
            opt->CtrlPts_i[i3+2] = Ci->NewNodeList[3*(opt->CtrlPts_iim[i])+2];
            
            Ci->Wv.elts[i3  ] = 0.0; 
            Ci->Wv.elts[i3+1] = 0.0; 
            Ci->Wv.elts[i3+2] = 0.0; 
         if( LocalHead ) {
            fprintf( SUMA_STDERR, "Control Point(%d) = [%11.8f;  %11.8f;   %11.8f] \n", i, 
                                    opt->CtrlPts_i[i3  ], opt->CtrlPts_i[i3+1], opt->CtrlPts_i[i3+2] ); }
         }
         FindSplineWeights(Ci, opt); 
      }          
      
      /* Reset Velocity Field. */
      for (i = 0; i < Ci->N_Node; ++i){
         i3 = 3*i;
         Ci->VelocityField[i3  ] = 0.0; 
         Ci->VelocityField[i3+1] = 0.0; 
         Ci->VelocityField[i3+2] = 0.0; 
      }
        
      Velocity(Ci, opt); 
      
      /* Send output to files for graphing in matlab. */
      if( LocalHead ) {
         if( opt->dom_dim == 2 ) {
            FILE *plot_circle = NULL; 
               sprintf( outfile, "Coords_%d.txt", (niter+1) );
               plot_circle = fopen ( outfile, "w");   
            for ( i = 0; i < Ci->N_Node; ++i) {
               i3 = 3*i;
               fprintf ( plot_circle, "%11.8f  %11.8f  %11.8f\n", Ci->NewNodeList[i3], Ci->NewNodeList[i3+1], Ci->NewNodeList[i3+2] );
            }
            fclose (plot_circle); plot_circle = NULL; 
         }
      }

      /* Create file for plotting magnitude of velocity field around the circle. */
      if( LocalHead ) {
         if( opt->dom_dim == 2 ) { 
            FILE *plot_speed = NULL; 
            sprintf( outfile_speed, "Plot_Speed%d.txt", (niter+1) );
            plot_speed = fopen(outfile_speed, "w");
            for ( i = 0; i < Ci->N_Node; ++i) {
               i3 = 3*i;
               V_Mag = 0.5*sqrt(  SUMA_POW2(Ci->VelocityField[i3  ]) +  /*Using half the magnitude to make matlab plot easier to look at.*/
                                  SUMA_POW2(Ci->VelocityField[i3+1]) + 
                                  SUMA_POW2(Ci->VelocityField[i3+2])  );
               SUMA_POINT_AT_DISTANCE_NORM( (&(Ci->NewNodeList[i3])), (&(Ci->NewNodeList[i3])), V_Mag, Point_at_Distance );
               fprintf( plot_speed, "%11.8f   %11.8f   %11.8f \n", 
                        Point_at_Distance[0][0], Point_at_Distance[0][1], Point_at_Distance[0][2]);
            }
            fclose(plot_speed); plot_speed = NULL;    
         }
      }
      
      if( LocalHead ) {
         FILE *test = NULL;
         sprintf( outfile_test, "%s%d.1D", opt->outfile, (niter+1) );
         test = fopen (outfile_test, "w");   
         for ( i = 0; i < Ci->N_Node; ++i) {
            i3 = 3*i;
            fprintf (test, "%11.8f  %11.8f  %11.8f  %11.8f  %11.8f  %11.8f  \n", 
               Ci->NewNodeList[i3], Ci->NewNodeList[i3+1], Ci->NewNodeList[i3+2], 
               Ci->VelocityField[i3], Ci->VelocityField[i3+1], Ci->VelocityField[i3+2]);
         } 
         fclose (test); test = NULL; 
      }
 
      if( LocalHead ) {
         fprintf(SUMA_STDERR, "MAIN: VelocityField at Control Points: \n");
         for(j=0; j < opt->N_ctrl_points; ++j){ 
            i = opt->CtrlPts_iim[j];
            i3 = i*3;
            fprintf(SUMA_STDERR, "%f   %f    %f \n"
                                 "  Magnitude = %f \n"
                                 "  Dot_v = %.12f \n",
                                 Ci->VelocityField[i3  ], Ci->VelocityField[i3+1], Ci->VelocityField[i3+2], 
                                 sqrt( Ci->VelocityField[i3  ]*Ci->VelocityField[i3  ] + 
                                       Ci->VelocityField[i3+1]*Ci->VelocityField[i3+1] +
                                       Ci->VelocityField[i3+2]*Ci->VelocityField[i3+2] ), 
                                 SUMA_MT_DOT( (&(Ci->VelocityField[i3])), (&(Ci->NewNodeList[i3])) ) ); }  
      }
      
      ++niter;
   }  while( niter < opt->N_step );       
 
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

         /*fprintf(SUMA_STDERR,  "  ERROR REPORTS:\n"
                                 "     original desired axis of rotation = %f    %f    %f \n"
                                 "     final achieved axis of rotation =   %f    %f    %f \n", 
                                 nrmi[0], nrmi[1], nrmi[2], nrmf[0], nrmf[1], nrmf[2]); */
      }
   }
   
   /* Compute the distance between the nodes on the surface and the center of the sphere. */
   if( LocalHead ) {
      FILE *sphere_radius = NULL;
      sphere_radius = fopen ("sphere_radius.1D", "w");
      for( i = 0; i < Ci->N_Node; ++i) 
      {
         i3 = 3*i;
         fprintf (sphere_radius, "%d   %11.8f\n", i, sqrt( SUMA_POW2 (Ci->NewNodeList[i3  ]) +
                                                           SUMA_POW2 (Ci->NewNodeList[i3+1]) + 
                                                           SUMA_POW2 (Ci->NewNodeList[i3+2]) ) );
      }  
      fclose (sphere_radius); sphere_radius = NULL;
   }
 
   /*
   FILE *test = NULL;
   if (opt->dbg_flag) { fprintf(stderr,"%s: Writing results to %s\n", FuncName, opt->outfile); }
   test = fopen ("test_move.1D", "w"); 
   for ( i = 0; i < Ci->N_Node; ++i) 
   {
      i3 = 3*i;
      fprintf (test, "%11.8f  %11.8f  %11.8f  %11.8f  %11.8f  %11.10f  %11.12f  %11.12f  %11.8f  %11.8f  %11.8f\n", 
         Ci->NodeList[i3], Ci->NodeList[i3+1], Ci->NodeList[i3+2],  Ci->VelocityField[i3], Ci->VelocityField[i3+1], 
         Ci->VelocityField[i3+2], Ci->NewNodeList[i3], Ci->NewNodeList[i3+1], Ci->NewNodeList[i3+2], Ci->Theta[i3], Ci->Theta[i3+1]);
   } 
   fclose (test); test = NULL;
   */
   
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
   nbad = SUMA_SphereQuality(SO, opt->outfile , shist);
   if (nbad) {
      fprintf(SUMA_STDERR,"Shist %s!:\n you have %d bad points!\n", FuncName, nbad);
   } else {
      fprintf(SUMA_STDERR,"%s: Happy pretend Valentine!\n"
                          "   You have no errors when checking node normals!\n" , FuncName);
   }
   if (shist) SUMA_free(shist); shist = NULL;

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
