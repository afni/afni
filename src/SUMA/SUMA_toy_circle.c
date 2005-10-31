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
      int N_node;
      double dt;
      char *ctrl; /* a pointer copy of argv containing filename, do not free */
      int N_ctrl_points;
      int *CtrlPts_iim; /* index of a particular ctrl point in the mesh. That's not necessary
                           for calculations but useful for debugging. An index of -1 is used
                           if ctrl point does not overlap with a node */
      double *CtrlPts_i;
      double *CtrlPts_f;
      double *Dtheta;
      double Center[3];
      double Radius;
      int cons_velocity;
      int adjust;
      char outfile[500];
   } MyCircleOpt;

typedef struct
   {
      int N_Node;
      double *NodeList;
      double *VelocityField;
      double *NewNodeList;
      double *Theta;
      vector Wv;
      int Initial;
   } MyCircle;

/*FUNCTION TO CALCULATE VELOCITY FIELD*/
void usage_toy_circle (SUMA_GENERIC_ARGV_PARSE *ps)
{
      static char FuncName[]={"usage_toy_circle"};
      char * s = NULL, *sio=NULL, *st = NULL, *sts = NULL;
      int i;
      s = SUMA_help_basics();
      sio  = SUMA_help_IO_Args(ps);
      printf ( "\n"
               "usage:\n"
               "  toy_circle [-dbg DBG] [-N_node N_NODE] [-dt DT] <-ctrl CTRL_FILE>\n"
               "                             [-cons_velocity CONS_VELOCITY] [-ouput OUTPUT]\n"
               "  -dbg DBG: Choose the debug level, default is 0\n"
               "  -N_node N_NODE: Set the number of nodes forming circle.\n"
               "                 Default N_NODE is 100.\n"
               "  -dt DT: Choose time step for moving points to new locations.\n"
               "                 Default DT is 0.001.\n"
               "  -ctrl CTRLFILE: Control nodes 1D file.\n"
               "                  Each row is for one node's intial and final XYZ.\n"
               "  -cons_velocity CONS_VELOCITY: Choose whether to use constant velocity.\n"
               "                 Default is constant velocity, CONS_VELOCITY=1.\n"
               "  -adjust ADJUST: Choose to scale displacement.\n"
               "                 Default is no adjustment, ADJUST = 0.\n"                            
               "  -output OUTPUT: Name output file. Default OUTPUT is test_move.1D.\n"
               " \n"
               "%s"
               "%s"
               "\n", sio,  s);
      SUMA_free(s); s = NULL; SUMA_free(st); st = NULL; SUMA_free(sio); sio = NULL;       
      s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
      printf("       Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov     \n");
      exit(0);
}

SUMA_GENERIC_PROG_OPTIONS_STRUCT *SUMA_toy_circle_ParseInput(char *argv[], int argc, SUMA_GENERIC_ARGV_PARSE *ps, MyCircleOpt *popt)
{
   static char FuncName[]={"SUMA_BrainWrap_ParseInput"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt=NULL;
   int kar, shft;
   SUMA_Boolean brk;
   SUMA_Boolean LocalHead = YUP;

   SUMA_ENTRY;
   
   Opt = SUMA_Alloc_Generic_Prog_Options_Struct();
   
   popt->dbg_flag = 0;
   popt->N_node = 100;
   popt->dt = 0.001;
   popt->ctrl = NULL;
   popt->N_ctrl_points = 1;
   popt->cons_velocity = 1;
   popt->CtrlPts_iim = NULL;
   popt->CtrlPts_i = NULL;
   popt->CtrlPts_f = NULL;
   popt->Dtheta = NULL;
   popt->adjust = 0;
   snprintf(popt->outfile, 499*sizeof(char),"test_move.1D");
   kar = 1;
   brk = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_toy_circle(ps);
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
         popt->dbg_flag = Opt->debug;
         fprintf(stderr, "%s: dbg-flag = %d\n", FuncName, popt->dbg_flag); 
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-N_node") == 0)) {
         kar ++;
         if (kar >= argc)  
         {
           fprintf (stderr, "need argument after -N_node \n");
           exit (1);
         }
         popt->N_node = atoi(argv[kar]);
         brk = 1;
      }
      
      if (!brk && (strcmp(argv[kar], "-dt") == 0)) {
         kar ++;
         if (kar >= argc)  
         {
            fprintf (stderr, "need argument after -dt \n");
            exit (1);
         }
         popt->dt = (double)atof(argv[kar]);               
         brk = 1;
      }
      
      if (!brk && (strcmp(argv[kar], "-ctrl") == 0)) {
         kar ++;
         if (kar >= argc)  
         {
           fprintf (stderr, "need filename after -N_ctrl \n");
           exit (1);
         }
         popt->ctrl = argv[kar];
         if (!SUMA_filexists(popt->ctrl)) {
            SUMA_S_Err("ctrl file not found");
            exit(1);
         }
         brk = 1;
      }
      
      if (!brk && (strcmp(argv[kar], "-cons_velocity") == 0)) {
         kar ++;
         if (kar >= argc)  
         {
           fprintf (stderr, "need argument after -cons_velocity \n");
           exit (1);
         }
         popt->cons_velocity = atoi(argv[kar]);
         brk = 1;
      }      
      
      if (!brk && (strcmp(argv[kar], "-adjust") == 0)) {
         kar ++;
         if (kar >= argc)  
         {
           fprintf (stderr, "need argument after -adjust \n");
           exit (1);
         }
         popt->adjust = atoi(argv[kar]);
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
			fprintf (SUMA_STDERR,"Error %s:\nOption %s not understood. Try -help for usage\n", FuncName, argv[kar]);
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
      int i, i3, co[6];
      
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

      /* column offsets */
      for (i=0; i<6; ++i) { co[i] = i*popt->N_ctrl_points+shft; }
      /* load up the points */
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


SUMA_Boolean FindSplineWeights (MyCircle *C, MyCircleOpt *opt)
{
   static char FuncName[]={"FindSplineWeights"};
   vector Vv;
   static matrix M, Mi;
   int i, i3, k, r, c, nr, nc;
   double V_row1[3], V_row2[3], V_row3[3];
   double  *t=NULL;
   SUMA_Boolean LocalHead = YUP;
   
   if (LocalHead) {
      fprintf(SUMA_STDERR, "%s:\n"
                           "Number of control points: %d\n" 
                           , FuncName, opt->N_ctrl_points);
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
   
   vector_initialize (&Vv);
   vector_create (3*opt->N_ctrl_points, &Vv);
   for (i=0; i < opt->N_ctrl_points; ++i) {
      i3 = 3*i;
      t[i] = atan2 ( opt->CtrlPts_i[i3+1], opt->CtrlPts_i[i3  ] );
      opt->Dtheta[i] = atan2 ( opt->CtrlPts_f[i3+1], opt->CtrlPts_f[i3  ] ) - t[i]; 
      if (LocalHead) {
         double Dtheta_new;
         SUMA_ANGLE_DIST_NC((&(opt->CtrlPts_f[i3])), (&(opt->CtrlPts_i[i3])), Dtheta_new);
         fprintf(SUMA_STDERR, "%s: Point %d, Dtheta = %.9f rad (%.9f deg)\n"
                              "New meth: Dtheta = %.9f rad (%.9f deg)\n", 
                                 FuncName, i, opt->Dtheta[i], SUMA_R2D(opt->Dtheta[i]),
                                 Dtheta_new, SUMA_R2D(Dtheta_new) ); 
      }
      Vv.elts[i3  ] = opt->Dtheta[i] * - opt->CtrlPts_i[i3+1];
      Vv.elts[i3+1] = opt->Dtheta[i] *   opt->CtrlPts_i[i3  ];
      Vv.elts[i3+2] = 0.0;
   }
   
   nr = 3 * opt->N_ctrl_points; /* number of rows  */
   nc = 3 * opt->N_ctrl_points; /* number of columns */
   matrix_initialize(&M);
   matrix_create(nr, nc, &M);
   if (!M.elts) {
      SUMA_S_Crit("Failed to allocate");
      exit(1);
   }
   for(r=0; r<opt->N_ctrl_points; ++r) { /* r is j in Julia's matlab function, c is i */
      for(c=0; c<opt->N_ctrl_points; ++c) { /* m(i,j) = m(i+j*Ni) */
         V_row1[0] = 0.5 * (cos(t[r] - t[c]) + 1.0) *  cos(t[r] - t[c]);
         V_row1[1] = 0.5 * (cos(t[r] - t[c]) + 1.0) * -sin(t[r] - t[c]);
         V_row1[2] = 0.0;
         V_row2[0] = 0.5 * (cos(t[r] - t[c]) + 1.0) *  sin(t[r] - t[c]);
         V_row2[1] = 0.5 * (cos(t[r] - t[c]) + 1.0) *  cos(t[r] - t[c]);
         V_row2[2] = 0.0; 
         V_row3[0] = 0.0;
         V_row3[1] = 0.0;
         V_row3[2] = 0.0;
         
         M.elts[ (3*r  ) ][ (3*c  ) ] = V_row1[0];
         M.elts[ (3*r  ) ][ (3*c+1) ] = V_row1[1];
         M.elts[ (3*r  ) ][ (3*c+2) ] = V_row1[2];
         M.elts[ (3*r+1) ][ (3*c  ) ] = V_row2[0];
         M.elts[ (3*r+1) ][ (3*c+1) ] = V_row2[1];
         M.elts[ (3*r+1) ][ (3*c+2) ] = V_row2[2];
         M.elts[ (3*r+2) ][ (3*c  ) ] = V_row3[0];
         M.elts[ (3*r+2) ][ (3*c+1) ] = V_row3[1];
         M.elts[ (3*r+2) ][ (3*c+2) ] = V_row3[2];
      }
   }
   if (LocalHead) { 
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
   
   SUMA_LH("Calculating inverse...");
   matrix_initialize(&Mi);
   matrix_psinv (M, &Mi, NULL);
   SUMA_LH("   Done."); 
   
   SUMA_LH("Calculating weights...");
   vector_initialize(&(C->Wv));
   vector_multiply(Mi, Vv, &(C->Wv));
   SUMA_LH("   Done."); 
   
   if (LocalHead) { 
      fprintf(SUMA_STDERR, "%s:\n"
                           "Wv = [\n", FuncName);
      for (r=0; r<opt->N_ctrl_points; ++r) {
         fprintf (SUMA_STDERR,"%.24f   %.24f   %.24f   \n", C->Wv.elts[3*r], C->Wv.elts[3*r+1], C->Wv.elts[3*r+2]);   
      }
      fprintf (SUMA_STDERR,"];\n");
   }
   
   if (t) SUMA_free(t); t=NULL;
   matrix_destroy (&M);
   matrix_destroy (&Mi);
   vector_destroy (&Vv);
      
   SUMA_RETURN(YUP);   
}

SUMA_Boolean Velocity( MyCircle *C, MyCircleOpt *opt) 
{
   static char FuncName[]={"Velocity"};
   int i, i3, j, j3;
   double Wax, Way, Waz, AS, cas, sas, was, *xyz_i, *xyz_j;
   SUMA_Boolean LocalHead = NOPE;
     
   /* reset velocity field */
   for (i = 0; i < C->N_Node; ++i) {  
      i3 = i*3;
      C->VelocityField[i3  ] = 0.0;
      C->VelocityField[i3+1] = 0.0;
      C->VelocityField[i3+2] = 0.0;
   }
   
   /* Compute field at each node */
   for (j=0; j<opt->N_ctrl_points; ++j) {
      j3 = 3*j;
      /* The weights for ctrl point j */
      Wax = C->Wv.elts[3*j]; Way = C->Wv.elts[3*j+1]; Waz = C->Wv.elts[3*j+2];
      xyz_j = &(opt->CtrlPts_i[j3]); /* pointer to XYZ of point j */
      for (i = 0; i < C->N_Node; ++i) {  
         i3 = i*3;
         /* Calculate the angle AS between node i and ctrl pt j. Assuming sphere of rad 1, centered at 0 0 0*/
         xyz_i = &(C->NodeList[i3]);
         SUMA_ANGLE_DIST_NC(xyz_i, xyz_j, AS);
         /* some frequent flyer variables */
         cas = cos(AS); sas = sin(AS); was = 0.5*(cas + 1.0);
         /* add contribution of point j to velocity field at node i. Note that equations are for 2D */
         C->VelocityField[i3  ] += was * ( cas*Wax - sas*Way );
         C->VelocityField[i3+1] += was * ( sas*Wax - cas*Way );
         C->VelocityField[i3+2] += 0.0;
      }      
   }
   
#if 0
{
   int r;
   double Wbx, Wby, Wcx, Wcy, BS, CS;
   
   if(opt->N_ctrl_points == 1)  
   {
      for (i = 0; i < C->N_Node; ++i) {  
         i3 = i*3;
         
         /*if( C->Initial ) { */
            Wax = 0.1217157;  /* 0.01195132865896622;   0.02390265731793244;*/
            Way = -0.03125129;  /*-0.003883222077450966;  -0.0077664441549018;*/ 
            /*Use contribution of node calculated by matlab.*/
         
         /*if( C->Initial == 0 ) {
            Wax = 0.05975664329483120;
            Way = -0.01941611038725369; }  */
         
         a = 21*SUMA_PI/50;        /*C->Theta[63];*/
         AS = C->Theta[i3] - a; 
           
         C->VelocityField [i3] =   (((1.0/2)*(cos(AS) + 1)) * (cos(AS)*Wax - sin(AS)*Way));
         C->VelocityField [i3+1] = (((1.0/2)*(cos(AS) + 1)) * (sin(AS)*Wax + cos(AS)*Way));
      }
   }
   
   
   if (opt->N_ctrl_points == 2)
   {
      for (i = 0; i < C->N_Node; ++i) {  
         i3 = i*3;
         a = SUMA_PI/2;
         b = 0.0;
         Wax = 0.4188;
         Way = 0.0;
         Wbx = 0.0;
         Wby = -1.0473;
         
         AS = C->Theta[i3] - a;  
         BS = C->Theta[i3] - b;   
      
         C->VelocityField [i3] = (((1.0/2)*(cos(AS) + 1)) * (cos(AS)*Wax - sin(AS)*Way) + ((1.0/2)*(cos(BS) + 1)) * (cos(BS)*Wbx - sin(BS)*Wby));
         C->VelocityField [i3+1] = (((1.0/2)*(cos(AS) + 1)) * (sin(AS)*Wax + cos(AS)*Way) + ((1.0/2)*(cos(BS) + 1)) * (sin(BS)*Wbx + cos(BS)*Wby));
      }
    }
    
    
    if (opt->N_ctrl_points == 3)
   {
      for (i = 0; i < C->N_Node; ++i) {  
         i3 = i*3;
         a = SUMA_PI/2;
         b = 0.0;
         c = SUMA_PI;
         Wax = 0.2355;
         Way = 0.0;
         Wbx = 0.0;
         Wby = -1.1389;
         Wcx = 0.0;
         Wcy = 0.2749;
         
         AS = C->Theta[i3] - a;  
         BS = C->Theta[i3] - b;   
         CS = C->Theta[i3] - c;
      
         C->VelocityField [i3] = (((1.0/2)*(cos(AS) + 1)) * (cos(AS)*Wax - sin(AS)*Way) + ((1.0/2)*(cos(BS) + 1)) * (cos(BS)*Wbx - sin(BS)*Wby)
                                 + ((1.0/2)*(cos(CS) + 1)) * (cos(CS)*Wcx - sin(CS)*Wcy));
         C->VelocityField [i3+1] = (((1.0/2)*(cos(AS) + 1)) * (sin(AS)*Wax + cos(AS)*Way) + ((1.0/2)*(cos(BS) + 1)) * (sin(BS)*Wbx + cos(BS)*Wby)
                                 + (1.0/2)*(cos(CS) + 1) * (sin(CS)*Wcx + cos(CS)*Wcy));
      }
    }
}
#endif

    return(YUP);
}

/*! 
   Code for 3D wherever you can, 
   Flag instances where method is 2D only
   Comments, debug messages, align millipede equations
   Avoid large scope single char variables
*/
int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"toy_circle"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt;  
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   MyCircleOpt myopt, *opt = NULL;
   int i, i3;
   double  t, dt, te, Wax, mag, scale = 0, talpha = 0, u[3], um, oda, faa, error;
   int niter=0;
   MyCircle *Ci = NULL;
   vector Wv;
   FILE *test=NULL;
   
   SUMA_Boolean LocalHead = YUP;

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
   
   if (opt->dbg_flag > 0) { fprintf(stderr,"%s: About to allocate.\n", FuncName); }
   
   Ci = (MyCircle *)SUMA_malloc(sizeof (MyCircle));
   Ci->N_Node = opt->N_node;
   Ci->NodeList = Ci->VelocityField = Ci->NewNodeList = NULL;
   Ci->NodeList = (double *)SUMA_malloc(Ci->N_Node * 3 * sizeof (double));
   Ci->VelocityField = (double *)SUMA_malloc(Ci->N_Node * 3 * sizeof (double));
   Ci->NewNodeList = (double *)SUMA_malloc(Ci->N_Node * 3 * sizeof (double));
   Ci->Theta = (double *)SUMA_malloc(Ci->N_Node * 3 * sizeof (double));
   Ci->Initial = 1;
   
   
   /*LOOP TO CREATE THE CIRCLE OF N_NODES STARTING AT THE POSITIVE X-AXIS
      AND DRAWING COUNTER CLOCKWISE.*/  
      
   if (opt->dbg_flag > 0) { fprintf(stderr,"%s: Creating circle.\n", FuncName); }
   
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
   
   if (opt->dbg_flag > 0 || LocalHead) {  
      FILE *cout=fopen("circleiXYZ.1D","w");
      if (cout) {
         for (i = 0; i < Ci->N_Node; ++i) {
            i3 = i*3;
            fprintf(cout,"%d   %.5f   %.5f   %.5f\n", i, Ci->NodeList [i3], Ci->NodeList [i3+1], Ci->NodeList [i3+2]);
         }
         fclose (cout);
      } else {
         SUMA_S_Err("Failted to open file for debug writing");
      }
   }
   
   /*Calculate spline weights to fit velocity field*/   
   if (!FindSplineWeights (Ci, opt)) {
      SUMA_S_Err("Failed in FindSplineWeights");
      exit(1);
   }

   if (opt->dbg_flag > 0) { fprintf(stderr,"%s: Calculating initial velocity field.\n", FuncName); }
   
   if (!Velocity(Ci, opt)) {
      SUMA_S_Err("Failed while calculating velocity field");
      exit(1);
   }  


   /*MOVE POINTS.*/
   
   if (opt->dbg_flag > 0) 
   {
      if (opt->cons_velocity) fprintf(stderr,"%s: Moving the points, constant velocity. (N_Node = %d)\n", FuncName, Ci->N_Node); 
      else fprintf(stderr,"%s: Moving the points, changing velocity. (N_Node = %d)\n", FuncName, Ci->N_Node); 
   }
   
   if (opt->dbg_flag > 0) 
   {
      if (opt->adjust == 0) fprintf(stderr,"%s: Moving the points, no adjustment. (N_Node = %d)\n", FuncName, Ci->N_Node); 
      else fprintf(stderr,"%s: Moving the points, adjusted. (N_Node = %d)\n", FuncName, Ci->N_Node); 
   }
   
   /* initialize newnodelist */
   for (i = 0; i < 3*Ci->N_Node; ++i)  Ci->NewNodeList[i] = Ci->NodeList[i];
   
   /* loop until time is 1 */
   if (LocalHead) {
      fprintf(SUMA_STDERR,"%s: About to enter main loop:\n"
                          "constant velocity = %d\n"
                          "adjust shorty     = %d\n", 
                          FuncName, 
                          opt->cons_velocity,
                          opt->adjust );
   }
   
   te=0.0;
   niter = 0;
   dt = opt->dt;
   
   do {  
      if (opt->dbg_flag > 0) { if (!(niter%100)) fprintf(stderr,"%s: te = %.3f.\n", FuncName, te); }
      for (i = 0; i < Ci->N_Node; ++i) 
      {  
         i3 = i*3;  
         
         u[0] = Ci->VelocityField[i3  ]; 
         u[1] = Ci->VelocityField[i3+1]; 
         u[2] = Ci->VelocityField[i3+2];
          
         um = sqrt(  SUMA_POW2(Ci->VelocityField[i3  ]) + 
                     SUMA_POW2(Ci->VelocityField[i3+1]) + 
                     SUMA_POW2(Ci->VelocityField[i3+2]));
         
         if(opt->adjust == 0)  { talpha = ( dt );} 
         else { /* if(opt->adjust == 1)  { */ 
            talpha = tan( dt * um ); 
            /* I FORGOT WHY U IS SCALED HERE */
            u[0] = u[0] / um;
            u[1] = u[1] / um; 
            u[2] = u[2] / um;
         }

         if (i == opt->CtrlPts_iim[0]) { /* debug when node is 1st control point */
            fprintf(SUMA_STDERR, "*******debug for ctrl point 0 == node %d**********\n"
                                 "te = %f, talpha = %f, tan(talpha)/talpha = %f \n"
                                 "u        = [%.8f %.8f %.8f], um = %.8f\n" 
                                 "old[XYZ] = [%.8f %.8f %.8f]\n", 
                                 i,
                                 te, talpha, tan(talpha)/talpha,
                                 u[0], u[1], u[2], um,
                                 Ci->NewNodeList[i3  ], Ci->NewNodeList[i3+1], Ci->NewNodeList[i3+2]);
         }
         
         Ci->NewNodeList[i3  ] = Ci->NewNodeList[i3  ] + talpha * u[0];
         Ci->NewNodeList[i3+1] = Ci->NewNodeList[i3+1] + talpha * u[1];
         Ci->NewNodeList[i3+2] = Ci->NewNodeList[i3+2] + talpha * u[2];
         if (i == opt->CtrlPts_iim[0]) { /* debug when node is 1st control point */
            fprintf(SUMA_STDERR, "par[XYZ] = [%.8f %.8f %.8f]\n", 
                                 Ci->NewNodeList[i3  ], Ci->NewNodeList[i3+1], Ci->NewNodeList[i3+2]);
         }
         mag = sqrt( SUMA_POW2(Ci->NewNodeList[i3  ]) + 
                     SUMA_POW2(Ci->NewNodeList[i3+1]) + 
                     SUMA_POW2(Ci->NewNodeList[i3+2]) ) * opt->Radius ;
         /*if (opt->dbg_flag > 0) { fprintf(stderr,"mag initial: %f\n", mag); } */
         Ci->NewNodeList[i3  ] = (Ci->NewNodeList[i3  ])/( mag );
         Ci->NewNodeList[i3+1] = (Ci->NewNodeList[i3+1])/( mag );
         Ci->NewNodeList[i3+2] = (Ci->NewNodeList[i3+2])/( mag );
         if (i == opt->CtrlPts_iim[0]) { /* debug when node is 1st control point */
            fprintf(SUMA_STDERR, "new[XYZ] = [%.8f %.8f %.8f]\n", 
                                 Ci->NewNodeList[i3  ], Ci->NewNodeList[i3+1], Ci->NewNodeList[i3+2]);
         }
         /*if (opt->dbg_flag > 0) { 
            fprintf(stderr,"mag final  : %f\n", mag);} */
         
         /* Ci->Theta is no longer needed a more flexible version that works in 3D
           is calculated on the fly later on, eventually we'll cut this one out...*/
         Ci->Theta[i3] = atan2(Ci->NewNodeList[i3+1], Ci->NewNodeList[i3]);
      }
     
      if ( opt->cons_velocity == 0) { Velocity(Ci, opt); /*Ci->Initial = 0;*/} 
      
      te = te + dt; 
      ++niter;
   }  while (te < 1.0);
   
   /*Check magnitude of velocity vectors.*/
   
   for (i = 0; i < Ci->N_Node; ++i) 
   {  
      i3 = i*3;  
      Ci->Theta[i3+2] = sqrt ( SUMA_POW2(Ci->VelocityField[i3]) + SUMA_POW2(Ci->VelocityField[i3+1]) + SUMA_POW2(Ci->VelocityField[i3+2]));
   } 
      

   if (opt->dbg_flag > 0) { fprintf(stderr,"%s: Writing results to %s\n", FuncName, opt->outfile); }
   test = fopen (opt->outfile, "w");
   
   /* Print difference between desired and calculated locations, negative means not far enough. */
   for (i=0; i<opt->N_ctrl_points; ++i) {
      i3 = 3 * i;
      fprintf(SUMA_STDERR,"%s: Angular error reports where control points coincide with nodes:\n"
                          "Niter = %d, last te used= %f\n"
                          "#Col. 0: Ctrl_Node\n"
                          "#Col. 1: Original Desired Angle \n"
                          "#Col. 2: Final Achieved Angle  \n"
                          "#Col. 3: Error (ODA-FAA) in rad.\n"
                          "#Col. 4: Error (ODA-FAA) in deg.\n", FuncName, niter, te - dt);
      if (opt->CtrlPts_iim[i] >= 0) {
         SUMA_ANGLE_DIST_NC( (&(opt->CtrlPts_f[i3])), (&(opt->CtrlPts_i[i3])), oda); /* original desired angle */ 
         SUMA_ANGLE_DIST_NC( (&(Ci->NewNodeList[3*opt->CtrlPts_iim[i]])), (&(opt->CtrlPts_i[i3])), faa ); /* final achieved angle */ 
         error = oda - faa;
         fprintf(SUMA_STDERR,"%d   %.5f   %.5f   %.15f   %.15f\n", opt->CtrlPts_iim[i], oda, faa, error, SUMA_R2D(error)); 
      }
   }

   for ( i = 0; i < Ci->N_Node; ++i) 
   {
      i3 = 3*i;
      fprintf (test, "%11.8f  %11.8f  %11.8f  %11.8f  %11.8f  %11.10f  %11.12f  %11.12f  %11.8f  %11.8f  %11.8f\n", 
         Ci->NodeList[i3], Ci->NodeList[i3+1], Ci->NodeList[i3+2],  Ci->VelocityField[i3], Ci->VelocityField[i3+1], 
         Ci->Theta[i3+2], Ci->NewNodeList[i3], Ci->NewNodeList[i3+1], Ci->Theta[i3], Ci->Theta[i3+1], Ci->Theta[i3+2]);
   }
   
   fclose (test); test = NULL;

      
   /*Cleanup*/
   
   vector_destroy(&(Ci->Wv));
   if(Ci->NodeList) SUMA_free(Ci->NodeList); Ci ->NodeList = NULL;
   if(Ci->VelocityField) SUMA_free(Ci->VelocityField); Ci ->VelocityField = NULL;
   if(Ci->NewNodeList) SUMA_free(Ci->NewNodeList); Ci ->NewNodeList = NULL;  
   if(Ci) SUMA_free(Ci); Ci = NULL;
   if (Opt->debug > 2) LocalHead = YUP;
   if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   if (Opt) Opt = SUMA_Free_Generic_Prog_Options_Struct(Opt);
   if (opt->CtrlPts_iim) SUMA_free(opt->CtrlPts_iim); opt->CtrlPts_iim = NULL;
   if (opt->CtrlPts_i) SUMA_free(opt->CtrlPts_i); opt->CtrlPts_i = NULL;
   if (opt->CtrlPts_f) SUMA_free(opt->CtrlPts_f); opt->CtrlPts_f = NULL;
   if (opt->Dtheta) SUMA_free(opt->Dtheta); opt->Dtheta = NULL;
   if (opt->ctrl) opt->ctrl = NULL; /* pointer from argv, do not free */
   if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   exit(0);
   
} 
