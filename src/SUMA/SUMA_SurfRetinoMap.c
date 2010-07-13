/*USE This sample to start writing standalone programs.
Change RetinoMap to the program name of your choosing.
*/
#include "SUMA_suma.h"

SUMA_SurfaceViewer *SUMAg_cSV = NULL; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
SUMA_DO *SUMAg_DOv = NULL;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF = NULL; /*!< Global pointer to structure containing info common to all viewers */

 
float SUMA_LocalRetinoGrad (SUMA_SurfaceObject *SO,
                                   int node, 
                                   SUMA_DSET *decc,
                                   SUMA_DSET *dpol, 
                                   float *pNodeList, 
                                   float Gre[2],
                                   float Grp[2],
                                   int NodeDBG) 
{
   static char FuncName[]={"SUMA_LocalRetinoGrad"};
   matrix X, XtXiXt;
   vector Y, B;
   SUMA_DSET *dset=NULL;
   int in=0, in3=0, k;
   float vfr = 0.0;
   char *tp="not set";
   
   
   /* phi(x,y) = b0 x + b1 y + const for each of the dsets */
   /* PHI = X B + e */
   matrix_initialize(&X);
   matrix_create(SO->FN->N_Neighb[node]+1, 3, &X);
   matrix_initialize(&XtXiXt);
   matrix_create(SO->FN->N_Neighb[node]+1, 3, &XtXiXt);
   vector_initialize(&Y);
   vector_create(SO->FN->N_Neighb[node]+1, &Y);  
   vector_initialize(&B);
   vector_create(3, &B);  
   for (in=0; in<=SO->FN->N_Neighb[node]; ++in) {
     in3=3*in;
     X.elts[in][0] = pNodeList[in3  ]; 
     X.elts[in][1] = pNodeList[in3+1]; 
     X.elts[in][2] = 1.0;
   }
   
   for (k=0; k<2; ++k) {
      if (k==0) {
         dset = decc;
         tp = "eccentricity";
      } else {
         dset = dpol;
         tp = "polar";
      }
      for (in=0; in<SO->FN->N_Neighb[node]; ++in) {  
         Y.elts[in] = SUMA_GetDsetNodeValInCol2(dset, 0, 
                                 SO->FN->FirstNeighb[node][in], -1);
      }
         Y.elts[SO->FN->N_Neighb[node]] 
                     = SUMA_GetDsetNodeValInCol2(dset, 0, node, -1);

      matrix_psinv(X, NULL, &XtXiXt);
      vector_multiply(XtXiXt, Y, &B); 

      if (node == NodeDBG) {
         SUMA_S_Notev("Gradient computations for node %d, dset %d/2 (%s)\n", 
                     NodeDBG, k+1, k ? "pol":"ecc");
         SUMA_S_Note("Nodes");
         for (in=0; in<SO->FN->N_Neighb[node]; ++in) 
            fprintf(stderr,"%d\n", SO->FN->FirstNeighb[node][in]);
         fprintf(stderr,"%d\n",node);
         SUMA_S_Note("X");
         matrix_print(X);
         SUMA_S_Note("XtXiXt");
         matrix_print(XtXiXt);
         SUMA_S_Notev("Y, %s\n", tp);
         vector_print(Y);
         SUMA_S_Notev("B, %s (degrees/mm)\n", tp);
         vector_print(B);
      }
   
      if (k==0) {
         Gre[0] = B.elts[0];
         Gre[1] = B.elts[1];
      } else {
         Grp[0] = B.elts[0];
         Grp[1] = B.elts[1];
      }
   }
   
   matrix_destroy(&X); 
   matrix_destroy(&XtXiXt); 
   vector_destroy(&Y);
   vector_destroy(&B);
   
   /* the VFR */
   
   vfr = Grp[0]*Gre[1]-Grp[1]*Gre[0];
   
   /* block ridiculously large values */
   if (vfr < -100) vfr = -100.0;
   else if (vfr > 100) vfr = 100.0;
   
   SUMA_RETURN(vfr);
}

SUMA_DSET * SUMA_RetinoMap (SUMA_SurfaceObject *SO, 
                            SUMA_DSET * decc, SUMA_DSET *dpol, 
                            byte *nmask, int NodeDBG, char *pref) 
{
   static char FuncName[]={"SUMA_RetinoMap"};
   SUMA_DSET *dout=NULL;
   int i=0, i3=0, in=0, neigh3, in3, j;
   float *pNodeList=NULL, fv[3], **fm=NULL, *vfr=NULL;
   float Gp[2]={0.0, 0.0}, Ge[2]={0.0, 0.0};
   double Eq[4], proj[3], U[3], d, 
          P2[2][3]={ {0.0, 0.0, 0.0}, {0.0, 0.0, 0.0} } ;
   char *prefix=NULL;
   FILE *ffgp=NULL, *ffge=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SO || !SO->NodeNormList || !SO->FN) {
      SUMA_S_Err("Surface null or not ready for prime time");
      goto CLEANOUT;
   }
   if (!dpol || !decc) {
      SUMA_S_Err("NULL input");
      goto CLEANOUT;
   }
   
   if (!(pNodeList =  (float *)
            SUMA_calloc((SO->FN->N_Neighb_max+1)*SO->NodeDim, sizeof(float))) ||
       !(fm = (float **)SUMA_allocate2D(4,4,sizeof(float))) ||
       !(vfr = (float *)SUMA_calloc(SO->N_Node, sizeof(float))) ){
      SUMA_S_Err("Failed to allocate"); 
      goto CLEANOUT;
   }
   
   if (pref) {
      prefix = SUMA_RemoveDsetExtension_s(pref,SUMA_NO_DSET_FORMAT);
      prefix = SUMA_append_replace_string(prefix, "",".",1);
   } else {
      prefix = SUMA_copy_string("");
   }
   
   for (i=0; i<SO->N_Node; ++i) {   if (!nmask || nmask[i]) {
      if (!(i % 1000)) fprintf(SUMA_STDERR,"."); 
      i3 = SO->NodeDim *i;
      /* tangent plane at node */
      SUMA_PLANE_NORMAL_POINT((SO->NodeNormList+i3),
                              (SO->NodeList+i3),
                              Eq);
      for (in=0; in<SO->FN->N_Neighb[i]; ++in) {
         in3 = 3*in;
         neigh3 = SO->NodeDim *SO->FN->FirstNeighb[i][in];
         SUMA_PROJECT_ONTO_PLANE(Eq, (SO->NodeList+neigh3), proj);
         /* move projection so that it is equidistant from origin */
         d = 0.0;
         for (j=0; j<SO->NodeDim; ++j) {
            U[j] = proj[j] - SO->NodeList[i3+j];
            d += U[j]*U[j];
         }
         d = sqrt(d);
         if (d != 0.0f) {
            for (j=0; j<SO->NodeDim; ++j) U[j] /= d;
         } else {
            SUMA_S_Warn("zero edge length!");
         }
         SUMA_POINT_AT_DISTANCE_NORM(U, (SO->NodeList+i3), d, P2);
         for (j=0; j<SO->NodeDim; ++j) pNodeList[in3+j] = P2[0][j];
      }
      /* add the dear node */
      pNodeList[3*SO->FN->N_Neighb[i]  ] = SO->NodeList[i3  ];
      pNodeList[3*SO->FN->N_Neighb[i]+1] = SO->NodeList[i3+1];
      pNodeList[3*SO->FN->N_Neighb[i]+2] = SO->NodeList[i3+2];
      
      if (i == NodeDBG) {
         FILE *ff=NULL;
         char stmp[256];
         SUMA_S_Notev("Debug files will be written for node %d\n",i);
         sprintf(stmp,"%snorm.%d.1D.do", prefix, i);
         ff = fopen(stmp,"w");
         fprintf(ff,"#node-based_vectors\n");
         fprintf(ff,"%d %f %f %f 1 0 0 1\n",
                     i, SO->EL->AvgLe*SO->NodeNormList[i3],
                        SO->EL->AvgLe*SO->NodeNormList[i3+1],
                        SO->EL->AvgLe*SO->NodeNormList[i3+2]);
         fclose (ff); 
         sprintf(stmp,"%sinit.%d.1D.do", prefix,i);
         ff = fopen(stmp,"w");
         fprintf(SUMA_STDOUT,"Story for node %d \n", i);
         fprintf(ff,"#spheres\n");
         for (in=0; in<SO->FN->N_Neighb[i]; ++in) {
            neigh3 = SO->NodeDim *SO->FN->FirstNeighb[i][in];
            fprintf(ff,"#%d/%d Neighb[%d]=%d, projected to:\n", 
                        in,SO->FN->N_Neighb[i], i,
                        SO->FN->FirstNeighb[i][in]);
             fprintf(ff,"%f %f %f 1 0 0 1\n", 
                    SO->NodeList[neigh3  ],
                    SO->NodeList[neigh3+1],
                    SO->NodeList[neigh3+2]);
         }
         fclose (ff);
         sprintf(stmp,"%sproj.%d.1D.do", prefix,i);
         ff = fopen(stmp,"w");
         fprintf(ff,"#spheres\n");
         for (in=0; in<SO->FN->N_Neighb[i]; ++in) {
            in3=3*in;
            fprintf(ff,"#%d/%d Neighb[%d]=%d, projected to:\n", 
                        in,SO->FN->N_Neighb[i], i,
                        SO->FN->FirstNeighb[i][in]);
            fprintf(ff,"%f %f %f 0 1 0 1\n", 
                    pNodeList[in3],pNodeList[in3+1],pNodeList[in3+2]);
         }
         fclose (ff);
      }
      /* pNodeList contains all the neighbors projected onto the tangent plane */
      /* Next, we rotate that plane so that it is parallel to the horiz. plane */
      fv[0]=fv[1]=0.0; fv[2]=1.0; /* Z axis */
      if (!SUMA_FromToRotation ((SO->NodeNormList+i3),fv,  fm)) {
            SUMA_S_Err("Failed to get rotation");
         goto CLEANOUT;
      }
      /* Apply rotation to all elements, including the dear node */
      for (in=0; in<=SO->FN->N_Neighb[i]; ++in) {
         in3=3*in;
         proj[0] = pNodeList[in3  ]*fm[0][0] + 
                   pNodeList[in3+1]*fm[0][1] +
                   pNodeList[in3+2]*fm[0][2] ;
         proj[1] = pNodeList[in3  ]*fm[1][0] + 
                   pNodeList[in3+1]*fm[1][1] +
                   pNodeList[in3+2]*fm[1][2] ;
         proj[2] = pNodeList[in3  ]*fm[2][0] + 
                   pNodeList[in3+1]*fm[2][1] +
                   pNodeList[in3+2]*fm[2][2] ;
         pNodeList[in3  ] = proj[0];
         pNodeList[in3+1] = proj[1];
         pNodeList[in3+2] = proj[2];
      }
      if (i == NodeDBG) {
         FILE *ff=NULL;
         char stmp[256];
         sprintf(stmp,"%srotmat.%d.1D", prefix,  i);
         ff = fopen(stmp,"w");
         for (in=0; in<3; ++in) 
            fprintf(ff,"%f %f %f\n",fm[in][0], fm[in][1], fm[in][2]);
         fclose (ff);
         sprintf(stmp,"%srotproj.%d.1D.do", prefix, i);
         ff = fopen(stmp,"w");
         fprintf(ff,"#spheres\n");
         for (in=0; in<=SO->FN->N_Neighb[i]; ++in) {
            in3=3*in;
            fprintf(ff,"#%d/%d Neighb[%d]=%d, rotation of projection:\n", 
                        in,SO->FN->N_Neighb[i], i,
                        SO->FN->FirstNeighb[i][in]);
            fprintf(ff,"%f %f %f 0 0 1 1\n", 
                    pNodeList[in3],pNodeList[in3+1],pNodeList[in3+2]);
         }
         fclose (ff);
      }
      /* Now you want to get the local gradients */
      vfr[i] = SUMA_LocalRetinoGrad(SO, i, decc, dpol, pNodeList, 
                                 Ge, Gp, NodeDBG);
      
      if (i==NodeDBG) { 
         SUMA_S_Notev("Node %d: Ge = [%f %f], Gp = [%f %f], VFR=%f\n",
                        i, Ge[0], Ge[1], Gp[0], Gp[1], vfr[i]);
      }
      
      #if 0 /* extreme debugging, output gradient estimates into dset */
         if (i==0) {
            char stmp[256];
            SUMA_S_Note("DBG Stuff");
            sprintf(stmp,"%sgradpol.1D.dset", prefix);
            ffgp = fopen(stmp, "w");
            sprintf(stmp,"%sgradecc.1D.dset", prefix);
            ffge = fopen(stmp, "w");
         } 
         fprintf(ffgp, "%f %f %f\n", 
               Gp[0], Gp[1], 
               sqrtf(Gp[0]*Gp[0]+Gp[1]*Gp[1]));
         fprintf(ffge, "%f %f %f\n", 
               Ge[0], Ge[1], 
               sqrtf(Ge[0]*Ge[0]+Ge[1]*Ge[1]));
         if (i==SO->N_Node) {
            fclose(ffgp); ffgp=NULL;
            fclose(ffge); ffge=NULL;
         }
      #endif
   
   } }
   
   SUMA_LH("Hard work over");
   if (!(dout = SUMA_CreateDsetPointer( 
                  "dodo", SUMA_NODE_BUCKET, NULL, 
                  SDSET_IDMDOM(decc),
                  SO->N_Node  ) ) ){
      SUMA_S_Err("Failed to create dout");
      goto CLEANOUT;
   }else if (!SUMA_PopulateDsetNodeIndexNel(dout,0)) {
      SUMA_S_Err("Failed populating node indices");
      goto CLEANOUT;
   }
   
   if (!SUMA_AddDsetNelCol(dout, "VFR", SUMA_NODE_FLOAT, vfr, NULL, 1)) {
      SUMA_S_Err("Failed to add column");
      SUMA_FreeDset(dout); dout=NULL;
      goto CLEANOUT;
   }
   
   CLEANOUT:
   SUMA_LH("CLEANUP");
   if (prefix) SUMA_free(prefix); prefix=NULL;
   if (pNodeList) SUMA_free(pNodeList); pNodeList=NULL;
   if (fm) SUMA_free2D((char **)fm, 2); fm = NULL;
   if (vfr) SUMA_free(vfr); vfr = NULL;
   
   SUMA_LH("ADIOS"); 
   SUMA_RETURN(dout);
}

void usage_RetinoMap (SUMA_GENERIC_ARGV_PARSE *ps)
{
      static char FuncName[]={"usage_RetinoMap"};
      char * s = NULL, *sio=NULL, *st = NULL, *sts = NULL;
      int i;
      s = SUMA_help_basics();
      sio  = SUMA_help_IO_Args(ps);
      printf ( "\n"
"Usage: RetinoMap <SURFACE> <-input POLAR ECCENTRICITY>\n"
"                 [<-prefix PREFIX>] [<-node_dbg NODE>]\n"
"       A template code for writing SUMA programs.\n"
"  <SURFACE> : Surface on which distances are computed.\n"
"              (For option's syntax, see \n"
"              'Specifying input surfaces' section below).\n"
"  <-input POLAR ECCENTRICITY>: Retinotopic datasets.\n"
"              POLAR is the polar angle dataset.\n"
"              ECCENTRICITY is the eccentricity angle dataset.\n"
"  [<-node_dbg NODE>]: Index of node number for which debugging\n"
"                    information is output.\n"
"  [<-prefix PREFIX>]: Prefix for output datasets.\n"
"                      The program outputs the Visual Field Ratio (VFR),\n"
"                      the sign of which is used to differentiate between\n"
"                      adjacent areas. \n"
"                      Based on paper by Warnking et al. Neuroimage 17, (2002)\n"
"                      'FMRI Retinotopic Mapping - Step by Step\n"
" \n"
"%s"
"%s"
               "\n", 
               ps->hverb ? sio:"Use -help for more detail.\n", 
               ps->hverb ? s:"");
      if (s) SUMA_free(s); s = NULL; 
      if (st) SUMA_free(st); st = NULL; 
      if (sio) SUMA_free(sio); sio = NULL;       
      s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
      printf("       Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov     \n");
      exit(0);
}

SUMA_GENERIC_PROG_OPTIONS_STRUCT *
      SUMA_RetinoMap_ParseInput(
                     char *argv[], int argc, SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_RetinoMap_ParseInput"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt=NULL;
   int kar;
   SUMA_Boolean brk;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   Opt = SUMA_Alloc_Generic_Prog_Options_Struct(); 
   Opt->ps = ps;  /* just hold it there for convenience */
   Opt->NodeDbg = -1;
   Opt->out_prefix = NULL;
   kar = 1;
   brk = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 ps->hverb = 1;
          usage_RetinoMap(ps);
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
            
      if (!brk && (  (strcmp(argv[kar], "-node_dbg") == 0) || 
                     (strcmp(argv[kar], "-node_debug") == 0)) ) {
         kar ++;
			if (kar >= argc)  {
		  		fprintf (SUMA_STDERR, "need argument after -node_debug \n");
				exit (1);
			}
			Opt->NodeDbg = atoi(argv[kar]);
         if (Opt->NodeDbg < 0) {
            fprintf (SUMA_STDERR, "%d is a bad node number.\n", Opt->NodeDbg);
				exit (1);
         }
         brk = YUP;
		}
      
      if (!brk && !ps->arg_checked[kar]) {
			SUMA_S_Errv("Option %s not understood.\n"
                     "Try -help for usage\n", 
                     argv[kar]);
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
   static char FuncName[]={"RetinoMap"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt;  
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   SUMA_DSET_FORMAT iform = SUMA_NO_DSET_FORMAT;
   SUMA_DSET *dpol=NULL, *decc=NULL, *dout=NULL;
   SUMA_SurfSpecFile *Spec = NULL;
   int ii, N_Spec, N_inmask = -1;
   int oform= SUMA_NO_DSET_FORMAT;
   SUMA_SurfaceObject *SO=NULL;
   char *s=NULL;
   SUMA_Boolean LocalHead = YUP;
   
   SUMA_STANDALONE_INIT;
	SUMA_mainENTRY;

   /* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "-i;-spec;-s;-o;-talk;-mask;-dset;");
   
   if (argc < 2) {
      usage_RetinoMap(ps);
      exit (1);
   }
   
   Opt = SUMA_RetinoMap_ParseInput (argv, argc, ps);

   if (Opt->debug > 2) LocalHead = YUP;
   if (Opt->ps->N_dsetname != 2) {
      SUMA_S_Errv("Need two and only two dsets please.\n"
                  "Have %d on command line.\n", 
                  Opt->ps->N_dsetname);
      exit(1);
   }
   if (!(dpol = SUMA_LoadDset_s (Opt->ps->dsetname[0], &iform, 0))) {
      SUMA_S_Errv("Failed to load dpol named %s\n", Opt->ps->dsetname[0]);
      exit(1);
   }
   if (!SUMA_PopulateDsetNodeIndexNel(dpol,0)) {
      SUMA_S_Err("Failed populating node indices");
      exit(1);
   }
   if (!(decc = SUMA_LoadDset_s (Opt->ps->dsetname[1], &iform, 0))) {
      SUMA_S_Errv("Failed to load decc named %s\n", Opt->ps->dsetname[1]);
      exit(1);
   }
   if (!SUMA_PopulateDsetNodeIndexNel(decc,0)) {
      SUMA_S_Err("Failed populating node indices");
      exit(1);
   }
   
   Spec = SUMA_IO_args_2_spec(ps, &N_Spec);
   if (N_Spec == 0) {
      SUMA_S_Err("No surfaces found.");
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
   if (!SUMA_SurfaceMetrics_eng(SO, "EdgeList|PolyArea|",
                                 NULL, Opt->debug, SUMAg_CF->DsetList)) {
      SUMA_S_Err("Failed in SUMA_SurfaceMetrics.\n");
      exit(1);
   }
   
   if (!(Opt->nmask = SUMA_load_all_command_masks(
                        Opt->ps->bmaskname, Opt->ps->nmaskname, Opt->ps->cmask,
                        SO->N_Node, &N_inmask)) 
         && N_inmask < 0) {
         SUMA_S_Err("Failed loading mask");
         exit(1);
   }
   if (Opt->nmask) {
      SUMA_LHv("%d nodes in mask for SO of %d nodes (%.2f perc. of surface)\n", 
               N_inmask, SO->N_Node, 100.0*N_inmask/SO->N_Node);
   } else {
      SUMA_LH("no mask");
   }
   if (!(dout = SUMA_RetinoMap (SO, decc, dpol, Opt->nmask, 
                                 Opt->NodeDbg, Opt->out_prefix))) {
      SUMA_S_Err("Failed in RetinoMap");
      exit(1);
   }
   
   if (Opt->out_prefix) {
      s = SUMA_WriteDset_s (Opt->out_prefix, dout, oform, THD_ok_overwrite(), 0);
   } else {
      s = SUMA_WriteDset_s ("RetinoMap", dout, SUMA_ASCII_NIML, 1, 1);
   }
   SUMA_free(s);
   
   if (!SUMA_FreeSpecFields(Spec)) {
      SUMA_S_Err("Failed to free Spec fields");
   } SUMA_free(Spec); Spec = NULL;
   if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   if (Opt) Opt = SUMA_Free_Generic_Prog_Options_Struct(Opt);
   if (!SUMA_Free_CommonFields(SUMAg_CF)) 
      SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   
   exit(0);
   
} 
